{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Translate process-compose state events into GitHub commit-status posts.
-- This module owns CI's policy across the commit-status lifecycle:
--
--   * The context-name convention (@\<recipe\>\@\<platform\>@,
--     derived from 'NodeId').
--   * The @startup → terminal@ state transitions —
--     'seedPending' fans out @Pending@ posts at the top of a run so
--     every expected check appears at once, and 'postStatusFor'
--     translates each in-flight 'ProcessState' event into the
--     matching @Pending@ / @Success@ / @Failure@ update.
--   * The setup-node filter — internal plumbing nodes ('SetupNode')
--     are excluded from both the seed and the per-event posts via a
--     pattern match on 'NodeId', matching the same filter
--     'JustCI.Verdict.verdictSummary' applies so the PR checks page and
--     the local CLI summary agree on what counts as user-facing.
--   * The human-readable description per state (suffixed with the
--     log path so the GitHub UI carries a navigable pointer).
--   * 'terminalToCommitStatus' — the wire-side half of the
--     cross-module agreement with 'JustCI.Verdict.terminalToOutcome':
--     both consumers of 'TerminalStatus' derive from this one
--     mapping so the GH check page and the local exit code never
--     disagree about which terminal classification counts as
--     success.
--
-- The endpoint URL, the wire encoding of each state, and the
-- form-field names are gh-API details owned by "JustCI.Gh".
module JustCI.CommitStatus
  ( -- * Posting
    postStatusFor,
    seedPending,

    -- * Per-node timing
    Timings,
    newTimings,

    -- * Naming convention
    contextForNode,
    isPostable,
    isBodyBearing,

    -- * === Internal (test surface) ===
    terminalToCommitStatus,
    -- ^ Exposed only for "test.JustCI.VerdictSpec"'s cross-module
    -- agreement check against 'JustCI.Verdict.terminalToOutcome' —
    -- production code reaches this mapping through 'postStatusFor'.
    describePost,
    -- ^ Exposed for "test.JustCI.CommitStatusSpec" so the wire-status
    -- → @(CommitStatus, description)@ classifier's branches stay locked,
    -- including the path-omitting did-not-run cases (issue #26).
    formatElapsed,
    -- ^ Exposed for "test.JustCI.CommitStatusSpec" so the human-readable
    -- duration formatter's branches stay locked.
  )
where

import Control.Concurrent.Async (forConcurrently_)
import Control.Monad (when)
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (display)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import JustCI.Gh (CommitStatus (..), CommitStatusPost (..), Context, Repo, contextFrom, postCommitStatus)
import JustCI.Git (Sha)
import JustCI.Justfile (Recipe, RecipeName, hasBody)
import JustCI.LogPath (logPathFor)
import JustCI.Node (NodeId (..))
import JustCI.ProcessCompose.Events (ProcessState (..), ProcessStatus (..), TerminalStatus (..))
import System.IO (hPutStrLn, stderr)

-- | Given a process-compose state event for an already-parsed
-- 'NodeId', post the corresponding GitHub commit status under the
-- @\<recipe\>\@\<platform\>@ context. Non-terminal states ('PsOther')
-- drop on the floor.
--
-- The caller ('JustCI.Pipeline') has the single 'parseNodeId' site, so
-- "is this event for a node we scheduled?" is decided once, not
-- re-decided here and again in 'JustCI.Verdict.recordOutcome'.
--
-- The status @description@ embeds the path to the node's per-run log
-- (@\<logDir\>\/\<platform\>\/\<recipe\>.log@) so a red check in the
-- GitHub UI carries a navigable pointer to the failing output. The
-- same path is set as the process's @log_location@ in the
-- process-compose YAML, so the file on disk and the path in the
-- status agree by construction. The two did-not-run terminal states
-- ('PsSkipped', 'PsErrored') suppress the path — those nodes never
-- wrote a log file, and a pointer to a missing file made every
-- cascaded check after a failed setup misleading (issue #26).
--
-- Synchronous: each post blocks the subscription loop in
-- 'JustCI.ProcessCompose.Events.subscribeStates' until @gh api@ returns. This
-- is deliberate — forking the post without tracking the resulting
-- threads loses terminal status posts at process exit (the forked @gh@
-- calls get killed before completing). At our scale (~10 events per run,
-- ~hundreds of ms per post) the brief pause is acceptable; if @gh@
-- hangs ever becomes a real problem, the right fix is a timeout on the
-- post, not a fire-and-forget fork.
--
-- Posting failures are logged to stderr with a @gh:@ prefix and
-- swallowed — the node's exit code must not depend on whether a
-- status post succeeded.
postStatusFor :: Timings -> Repo -> Sha -> FilePath -> Map RecipeName Recipe -> NodeId -> ProcessState -> IO ()
postStatusFor timings repo sha logDir recipes node ps
  | not (isPostable recipes node) = pure ()
  | otherwise = do
      when (ps.status == PsRunning) (markStart timings node)
      mElapsed <- elapsedSince timings node
      for_ (describePost ps mElapsed (logPathFor logDir node)) $ \(cs, desc) ->
        postOne repo sha node cs desc

-- | Pre-seed every node with a 'Pending' commit status at startup —
-- one parallel @gh api@ POST per node, all joined before this returns.
-- The PR's checks panel shows the full set of expected checks the moment
-- the pipeline begins, instead of materializing them one at a time as
-- nodes start. Skipped nodes (whose upstream dep failed) get their
-- @pending@ overwritten by @failure@ when 'postStatusFor' fires; nodes
-- that never produce any event at all stay at @pending@, which
-- surfaces as a visible "why is this still pending?" signal rather
-- than silent absence.
--
-- GitHub has no batch endpoint for commit statuses
-- (see <https://docs.github.com/en/rest/commits/statuses>), so this is
-- N parallel single-status POSTs. 'forConcurrently_' joins all of them
-- before returning, so the caller can rely on every seed being in place
-- before the pipeline kicks off.
seedPending :: Repo -> Sha -> FilePath -> Map RecipeName Recipe -> [NodeId] -> IO ()
seedPending repo sha logDir recipes nodes =
  forConcurrently_ (filter (isPostable recipes) nodes) $ \n ->
    postOne repo sha n Pending $ seedDescription $ logPathFor logDir n

-- | Whether a 'NodeId' belongs on the PR's user-facing checks page.
-- The single source of truth for "post a GH commit status for this
-- node?", consumed by both 'seedPending' and 'postStatusFor' and by
-- 'JustCI.Pipeline.runProtect' when assembling the branch-protection
-- required-checks list.
--
-- Two axes of exclusion, both rejected:
--
--   * @SetupNode@s — internal plumbing (bundle ship, drv copy) that
--     never represented user work in the first place; structurally
--     unfit for a PR-visible check.
--
--   * @RecipeNode@s whose 'Recipe' has an empty 'hasBody' — pure
--     dependency aggregators (e.g. @default: checks run-check@). Their
--     state is fully derivative of their leaves, so a check posted for
--     them is a denormalised duplicate that diverges from reality the
--     moment a downstream leaf is retried individually. The retry
--     overwrites the leaf's check to green but the aggregator's
--     'Skipped (upstream failed)' was never tied to a re-runnable
--     process, so it sticks at red and the PR can't merge. Removing
--     aggregators from the surface entirely means the required checks
--     are exactly the recipes that do real work.
isPostable :: Map RecipeName Recipe -> NodeId -> Bool
isPostable _ (SetupNode _) = False
isPostable recipes n@(RecipeNode _ _) = isBodyBearing recipes n

-- | Whether @node@ is body-bearing — the narrower axis 'isPostable'
-- delegates its recipe-side check to. Vacuously 'True' for
-- 'SetupNode' (it isn't a recipe, so the axis doesn't apply); for
-- 'RecipeNode' it looks the recipe up in @recipes@ and asks 'hasBody'.
-- Exists alongside 'isPostable' so the local verdict surface
-- ('JustCI.Verdict.verdictSummary'), which keeps 'SetupNode's in its
-- dedicated @Setup@ section but wants the same aggregator drop as
-- the GH surface, can seed its outcomes map by this predicate. The
-- 'SetupNode' clause's 'True' is the right vacuous value because
-- callers that want to exclude setup nodes (i.e. 'isPostable')
-- handle that case structurally before delegating here.
--
-- Like 'isPostable', a missing 'RecipeName' is a runner-internal
-- contract violation, not a routine absent condition — every
-- 'RecipeNode' in the fanned graph originates from the recipe map
-- 'JustCI.Pipeline.buildNodeGraph' returned.
isBodyBearing :: Map RecipeName Recipe -> NodeId -> Bool
isBodyBearing _ (SetupNode _) = True
isBodyBearing recipes (RecipeNode r _) = case Map.lookup r recipes of
  Just recipe -> hasBody recipe
  Nothing -> error $ "internal error: RecipeNode " <> show r <> " missing from recipe map (buildNodeGraph contract violated)"

-- | Issue one commit-status POST with a caller-supplied description and
-- log the outcome.
postOne :: Repo -> Sha -> NodeId -> CommitStatus -> Text -> IO ()
postOne repo sha node cs desc = do
  let ctx = contextForNode node
      post = CommitStatusPost {state = cs, context = ctx, description = desc}
  result <- postCommitStatus repo sha post
  let line = "gh: " <> T.unpack (display ctx) <> " " <> T.unpack (display cs)
  case result of
    Right () -> hPutStrLn stderr line
    Left e -> hPutStrLn stderr $ line <> " FAILED: " <> T.unpack (display e)

-- | The single source of truth for status-check context names: a
-- 'NodeId' rendered as @\<recipe\>\@\<platform\>@. Named (rather
-- than a @Display a =>@ helper) so the YAML map key and the GH
-- context can evolve independently — both currently render the
-- same 'NodeId' identically, but the agreement is by intent, not
-- typeclass coincidence.
contextForNode :: NodeId -> Context
contextForNode = contextFrom . display

-- | Classify a process-compose state event into the @(commit-status,
-- description)@ pair the per-event poster sends to GitHub. 'Nothing'
-- for events the poster drops on the floor (non-terminal 'PsOther').
--
-- The description's two shapes:
--
--   * /Ran-or-running/ ('PsRunning', 'PsCompleted') — embeds the
--     recipe's per-run log path so a click on the GitHub check
--     lands on the matching file under @.ci\/\<sha\>\/@. The
--     'PsRunning' transition fires at start so its elapsed would
--     be zero (pointless to display); 'PsCompleted' carries the
--     caller's measured elapsed in a @(\<elapsed\>)@ annotation.
--
--   * /Did-not-run/ ('PsSkipped', 'PsErrored') — drops the log
--     path entirely (the file doesn't exist on disk; the cascade
--     left no output) and the elapsed (no meaningful runtime), in
--     favour of a short standalone label that names the failure
--     mode. Prior to issue #26 these embedded the recipe's log
--     path unconditionally, so a failed setup turned every
--     downstream check into a broken pointer on the PR.
--
-- Both shapes stay well under GitHub's 140-char description budget
-- at typical recipe-name lengths.
describePost :: ProcessState -> Maybe NominalDiffTime -> FilePath -> Maybe (CommitStatus, Text)
describePost ps mElapsed logPath = case ps.status of
  PsRunning -> Just (Pending, ranLabel "Running" Nothing)
  PsCompleted
    | ps.exit_code == 0 -> Just (Success, ranLabel "Succeeded" mElapsed)
    | otherwise -> Just (Failure, ranLabel "Failed" mElapsed)
  PsSkipped -> Just (Failure, "Skipped (upstream failed)")
  PsErrored -> Just (Failure, "Errored (did not start)")
  PsOther _ -> Nothing
  where
    ranLabel label elapsed = label <> elapsedSuffix elapsed <> ": " <> T.pack logPath
    elapsedSuffix Nothing = ""
    elapsedSuffix (Just dt) = " (" <> formatElapsed dt <> ")"

-- | Description for a 'seedPending' post: @"Queued: \<logPath\>"@.
-- The seed runs once per node at startup before any state event
-- arrives, so it has its own format — the per-event 'describePost'
-- doesn't see this lifecycle stage.
seedDescription :: FilePath -> Text
seedDescription logPath = "Queued: " <> T.pack logPath

-- | Compact human-readable rendering of a 'NominalDiffTime': @\<n\>s@
-- under a minute, @\<n\>m\<n\>s@ under an hour, @\<n\>h\<n\>m@ otherwise.
-- Sub-second durations round down to @0s@. The format is fixed-width-ish
-- (≤ 6 chars typical) so the @(\<elapsed\>)@ annotation in 'describePost' fits
-- comfortably inside GitHub's 140-char description budget alongside the
-- state label and log path.
formatElapsed :: NominalDiffTime -> Text
formatElapsed dt
  | h > 0 = ts h <> "h" <> ts m <> "m"
  | m > 0 = ts m <> "m" <> ts s <> "s"
  | otherwise = ts s <> "s"
  where
    totalSec = max 0 (floor dt :: Int)
    h = totalSec `div` 3600
    m = (totalSec `mod` 3600) `div` 60
    s = totalSec `mod` 60
    ts = T.pack . show

-- | Per-node start-time store. Populated by 'postStatusFor' when a
-- node first transitions to @PsRunning@; read on the terminal
-- transition to compute elapsed duration. One instance per pipeline
-- run, allocated by the caller via 'newTimings' (typically in
-- 'JustCI.Pipeline.runStrict' alongside the outcome accumulator).
--
-- Single-threaded by construction — the WebSocket observer loop
-- ('JustCI.ProcessCompose.Events.subscribeStates') invokes 'postStatusFor'
-- synchronously per event — so atomicModifyIORef' is overkill but
-- cheap; the alternative would force a TVar+STM rig for an access
-- pattern that's already serial.
newtype Timings = Timings (IORef (Map NodeId UTCTime))

-- | Allocate a fresh, empty 'Timings'.
newTimings :: IO Timings
newTimings = Timings <$> newIORef Map.empty

-- | Stamp the current wall-clock time as the start of @node@. Called
-- once per node (on its first @PsRunning@ event); a duplicate
-- @PsRunning@ would overwrite the prior stamp, but process-compose
-- only emits @PsRunning@ once per node lifecycle.
markStart :: Timings -> NodeId -> IO ()
markStart (Timings ref) node = do
  now <- getCurrentTime
  atomicModifyIORef' ref $ \m -> (Map.insert node now m, ())

-- | Wall-clock time elapsed since 'markStart' for this node, or
-- 'Nothing' if no start was recorded. A missing entry usually means
-- the node went straight from @ready@ to a terminal state without
-- emitting a @PsRunning@ event (cache-hit / no-op recipes); the
-- caller falls back to omitting the duration from the description.
elapsedSince :: Timings -> NodeId -> IO (Maybe NominalDiffTime)
elapsedSince (Timings ref) node = do
  m <- readIORef ref
  case Map.lookup node m of
    Nothing -> pure Nothing
    Just start -> do
      now <- getCurrentTime
      pure (Just (diffUTCTime now start))

-- | GitHub-side mapping for the two terminal classifications.
-- Production code now classifies each wire event through 'describePost'
-- (which distinguishes 'PsSkipped' \/ 'PsErrored' from 'PsCompleted'
-- exit-non-zero so it can drop the log path for nodes that never ran —
-- issue #26), but the binary "did it succeed?" projection still lives
-- here for "JustCI.Verdict.terminalToOutcome" to align against. The
-- cross-module agreement test in @VerdictSpec@ relies on this seam.
terminalToCommitStatus :: TerminalStatus -> CommitStatus
terminalToCommitStatus TsSucceeded = Success
terminalToCommitStatus TsFailed = Failure
