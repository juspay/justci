{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Translate process-compose state events into GitHub commit-status posts.
-- This module owns CI's policy across the commit-status lifecycle:
--
--   * The context-name convention (@\<recipe\>\@\<platform\>@,
--     derived from 'NodeId').
--   * The @running → terminal@ state transitions — 'postStatusFor'
--     translates each 'ProcessState' event into the matching
--     @Pending@ / @Success@ / @Failure@ post. The first @Pending@
--     for a node fires on its @PsRunning@ transition, /just prior
--     to/ the step actually running — there is no pre-run seed.
--     Cascade-skipped nodes ('PsSkipped') don't post anything; their
--     required-check row is supplied by GitHub's own
--     "Expected — Waiting for status to be reported" placeholder
--     (driven by 'JustCI.Pipeline.runProtect's required-checks list),
--     which is the canonical encapsulation of "required but
--     unreported." Clearing the gate requires re-running the failed
--     root, not the skipped recipe itself.
--   * Two predicates with distinct policy meanings:
--     'shouldPostStatus' (does this node emit a commit status?) —
--     true for setup nodes too, so a setup failure surfaces as a
--     red row instead of leaving the user staring at a wall of
--     "Expected — Waiting" downstream; and 'isRequiredCheck' (does
--     this node belong on the branch-protection required list?) —
--     false for setup, so local-only runs don't permanently block
--     merge on a setup row that was never scheduled.
--   * The human-readable description per state (suffixed with the
--     log path so the GitHub UI carries a navigable pointer).
--   * 'terminalToCommitStatus' — the wire-side half of the
--     cross-module agreement with 'JustCI.Verdict.terminalToOutcome'.
--     Both consumers of 'TerminalStatus' project from this one
--     mapping. Note that 'describePost' no longer reaches the
--     @TsSkipped@ arm — the @PsSkipped → Nothing@ short-circuit
--     above handles that case before the seam — so the projection is
--     kept for the cross-module agreement contract, not for any
--     live posting path.
--
-- The endpoint URL, the wire encoding of each state, and the
-- form-field names are gh-API details owned by "JustCI.Gh".
module JustCI.CommitStatus
  ( -- * Posting
    postStatusFor,

    -- * Per-node timing
    Timings,
    newTimings,

    -- * Naming convention
    contextForNode,
    isRequiredCheck,
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
    shouldPostStatus,
    -- ^ Exposed for "test.JustCI.CommitStatusSpec" so the
    -- posting-policy predicate's branches stay locked. Production
    -- code consults this via 'postStatusFor''s internal guard;
    -- no external module imports it directly.
    formatElapsed,
    -- ^ Exposed for "test.JustCI.CommitStatusSpec" so the human-readable
    -- duration formatter's branches stay locked.
  )
where

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
-- and cascade-skipped events ('PsSkipped') drop on the floor — see
-- 'describePost' for the per-status routing.
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
-- status agree by construction. 'PsErrored' (launch failure)
-- suppresses the path — those nodes never wrote a log file, and a
-- pointer to a missing file is worse than no pointer (issue #26) —
-- and posts @Failure@+@"Errored (did not start)"@.
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
  | not (shouldPostStatus recipes node) = pure ()
  | otherwise = do
      when (ps.status == PsRunning) (markStart timings node)
      mElapsed <- elapsedSince timings node
      for_ (describePost ps mElapsed (logPathFor logDir node)) $ \(cs, desc) ->
        postOne repo sha node cs desc

-- | Does this node emit a GitHub commit status as it transitions
-- through process-compose states? True for body-bearing recipe nodes
-- /and/ for setup nodes — the latter so that a remote-platform setup
-- failure (bundle ship, drv copy, SSH plumbing) surfaces as one
-- visible @ci::_ci-setup@\<platform\>@ row instead of leaving the user
-- staring at a wall of "Expected — Waiting" on downstream recipes
-- with no posted cause.
--
-- Distinct from 'isRequiredCheck': setup nodes are posted but never
-- required-merge-blocking. Local-only runs (no remote platforms) do
-- not schedule any 'SetupNode', so registering one as a required
-- check would permanently block merge on a row that was never going
-- to receive a status — see 'isRequiredCheck' for the matching
-- exclusion.
--
-- Pure-aggregator recipes (empty 'hasBody', e.g. @default: checks
-- run-check@) are excluded on a different axis: their state is fully
-- derivative of their leaves, so a check posted for them is a
-- denormalised duplicate. The wedge case is downstream retries —
-- re-running a single failed leaf overwrites the leaf's check to
-- green, but the aggregator was never tied to a re-runnable process,
-- so it'd stick at its prior state and lie about reality.
shouldPostStatus :: Map RecipeName Recipe -> NodeId -> Bool
shouldPostStatus _ (SetupNode _) = True
shouldPostStatus recipes n@(RecipeNode _ _) = isBodyBearing recipes n

-- | Does this node belong on the branch-protection required-checks
-- list assembled by 'JustCI.Pipeline.runProtect'? True for body-bearing
-- recipes only.
--
-- Setup nodes are excluded for liveness: a remote-platform setup is
-- internal plumbing, not user work, and a recipe whose setup is
-- handled implicitly (the local platform) has no 'SetupNode' in the
-- graph at all. Requiring @_ci-setup@\<platform\>@ as a check would
-- mean local-only runs permanently block merge on a row that was
-- never scheduled.
--
-- For cascade-skipped recipes, the required-check row is supplied
-- by GitHub's own "Expected — Waiting for status to be reported"
-- placeholder once branch protection is configured — that's the
-- canonical encapsulation of "required but unreported", and
-- 'describePost' returning 'Nothing' on 'PsSkipped' is what defers
-- to it instead of fabricating a parallel @Pending@+@"Skipped"@
-- post. Clearing the gate requires re-running the failed root.
isRequiredCheck :: Map RecipeName Recipe -> NodeId -> Bool
isRequiredCheck _ (SetupNode _) = False
isRequiredCheck recipes n@(RecipeNode _ _) = isBodyBearing recipes n

-- | Whether @node@ is body-bearing — the narrower axis the
-- recipe-side cases of 'shouldPostStatus' and 'isRequiredCheck'
-- delegate to. Vacuously 'True' for 'SetupNode' (it isn't a recipe,
-- so the axis doesn't apply); for 'RecipeNode' it looks the recipe
-- up in @recipes@ and asks 'hasBody'. Exists alongside the policy
-- predicates so the local verdict surface
-- ('JustCI.Verdict.verdictSummary'), which keeps 'SetupNode's in its
-- dedicated @Setup@ section but wants the same aggregator drop as
-- the GH surface, can seed its outcomes map by this predicate. The
-- 'SetupNode' clause's 'True' is the right vacuous value because
-- callers that want a setup-specific decision (e.g. 'isRequiredCheck')
-- handle that case structurally before delegating here.
--
-- A missing 'RecipeName' is a runner-internal contract violation,
-- not a routine absent condition — every 'RecipeNode' in the
-- fanned graph originates from the recipe map
-- 'JustCI.Pipeline.buildNodeGraph' returned.
isBodyBearing :: Map RecipeName Recipe -> NodeId -> Bool
isBodyBearing _ (SetupNode _) = True
isBodyBearing recipes (RecipeNode r _) = case Map.lookup r recipes of
  Just recipe -> hasBody recipe
  Nothing -> error $ "internal error: RecipeNode " <> T.unpack (display r) <> " missing from recipe map (buildNodeGraph contract violated)"

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
-- The @CommitStatus@ half of every terminal arm is derived from
-- 'terminalToCommitStatus' rather than inlined, so the wire→GH-state
-- mapping has one definition site: a future change to (say)
-- @terminalToCommitStatus TsSkipped@ propagates to every arm without
-- the silent-drift risk of two parallel literal tables. The
-- description half stays local because it's a presentation choice
-- with no agreement obligation to the verdict side ('PsRunning' has
-- no terminal-status at all and 'PsErrored' wants its own wording).
--
-- The description's two shapes:
--
--   * /Ran-or-running/ ('PsRunning', 'PsCompleted') — embeds the
--     recipe's per-run log path so a click on the GitHub check
--     lands on the matching file under @.ci\/\<sha\>\/@. The
--     'PsRunning' transition fires at start so its elapsed would
--     be zero (pointless to display); 'PsCompleted' carries the
--     caller's measured elapsed in a @(\<elapsed\>)@ annotation.
--     This is the first post for a node — there is no pre-run
--     seed, so the @Pending@+@"Running"@ row appearing on the PR
--     marks the moment the step actually starts executing.
--
--   * /Did-not-run/ ('PsErrored') — drops the log path entirely
--     (the file doesn't exist on disk; the launch left no output)
--     and the elapsed (no meaningful runtime), in favour of a
--     short standalone label that names the case. Prior to issue
--     \#26 this embedded the recipe's log path unconditionally, so
--     a failed setup turned every downstream check into a broken
--     pointer on the PR. 'PsErrored' is a real launch failure (pc
--     couldn't start the process) and posts
--     @Failure@+@"Errored (did not start)"@.
--
-- 'PsSkipped' returns 'Nothing' — cascade-skipped recipes (an
-- upstream dep failed and pc never started this one) do not post a
-- justci status. The required-check row on the PR comes from
-- GitHub's own "Expected — Waiting for status to be reported"
-- placeholder, registered by 'JustCI.Pipeline.runProtect'. Posting a
-- @Pending@+@"Skipped"@ row would duplicate that encapsulation and
-- — worse — overwrite a prior @Success@ on a partial re-run, since
-- process-compose re-emits 'PsSkipped' for downstreams of any
-- re-running failed leaf. Clearing the gate requires re-running
-- the failed root, not the cascaded recipe.
--
-- Both posted shapes stay well under GitHub's 140-char description
-- budget at typical recipe-name lengths.
describePost :: ProcessState -> Maybe NominalDiffTime -> FilePath -> Maybe (CommitStatus, Text)
describePost ps mElapsed logPath = case ps.status of
  PsRunning -> Just (Pending, ranLabel "Running" Nothing)
  PsCompleted
    | ps.exit_code == 0 -> Just (terminalToCommitStatus TsSucceeded, ranLabel "Succeeded" mElapsed)
    | otherwise -> Just (terminalToCommitStatus TsFailed, ranLabel "Failed" mElapsed)
  -- Cascade-skipped: defer to branch-protection's "Expected — Waiting"
  -- placeholder (see haddock above for the partial-re-run wedge case).
  PsSkipped -> Nothing
  PsErrored -> Just (terminalToCommitStatus TsFailed, "Errored (did not start)")
  PsOther _ -> Nothing
  where
    ranLabel label elapsed = label <> elapsedSuffix elapsed <> ": " <> T.pack logPath
    elapsedSuffix Nothing = ""
    elapsedSuffix (Just dt) = " (" <> formatElapsed dt <> ")"

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

-- | GitHub-side mapping for the three terminal classifications.
-- Production code classifies each wire event through 'describePost'
-- (which carries the description too — log path on ran nodes, short
-- label on did-not-run nodes per issue #26), but the state-only
-- projection lives here so "JustCI.Verdict.terminalToOutcome" has a
-- single seam to align against. The cross-module agreement test in
-- @VerdictSpec@ iterates @[minBound..maxBound]@ and checks that
-- every 'TerminalStatus' projects to exactly one 'CommitStatus' here
-- and exactly one 'JustCI.Verdict.RecipeOutcome' there, so adding a
-- fourth wire-state constructor would surface as an exhaustiveness
-- warning in both modules instead of a silent drift.
--
-- The 'TsSkipped' arm is no longer reached from the live posting
-- path — 'describePost' short-circuits 'PsSkipped' to 'Nothing'
-- before the projection is consulted — but the arm is kept so the
-- function stays total against @TerminalStatus@'s
-- @[minBound..maxBound]@ contract. Its value (@Pending@) records
-- what GitHub would render if a cascade-skip ever did need a
-- posted status; in current use, branch protection's
-- "Expected — Waiting" placeholder supplies the same yellow-row
-- semantics without justci having to post.
terminalToCommitStatus :: TerminalStatus -> CommitStatus
terminalToCommitStatus TsSucceeded = Success
terminalToCommitStatus TsFailed = Failure
terminalToCommitStatus TsSkipped = Pending
