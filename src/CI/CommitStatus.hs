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
--     matching @Pending@ / @Success@ / @Failure@ / @Error@ update.
--   * The setup-node filter — internal plumbing nodes ('SetupNode')
--     are excluded from both the seed and the per-event posts via a
--     pattern match on 'NodeId', matching the same filter
--     'CI.Verdict.verdictSummary' applies so the PR checks page and
--     the local CLI summary agree on what counts as user-facing.
--   * The human-readable description per state (suffixed with the
--     log path so the GitHub UI carries a navigable pointer).
--   * 'terminalToCommitStatus' — the wire-side half of the
--     cross-module agreement with 'CI.Verdict.terminalToOutcome':
--     both consumers of 'TerminalStatus' derive from this one
--     mapping so the GH check page and the local exit code never
--     disagree about which terminal classification counts as
--     success.
--
-- The endpoint URL, the wire encoding of each state, and the
-- form-field names are gh-API details owned by "CI.Gh".
module CI.CommitStatus
  ( -- * Posting
    postStatusFor,
    seedPending,

    -- * Per-node timing
    Timings,
    newTimings,

    -- * Naming convention
    contextForNode,
    isUserVisible,

    -- * === Internal (test surface) ===
    terminalToCommitStatus,
    -- ^ Exposed only for "test.CI.VerdictSpec"'s cross-module
    -- agreement check against 'CI.Verdict.terminalToOutcome' —
    -- production code reaches this mapping through 'postStatusFor'.
    formatElapsed,
    -- ^ Exposed for "test.CI.CommitStatusSpec" so the human-readable
    -- duration formatter's branches stay locked.
  )
where

import CI.Gh (CommitStatus (..), CommitStatusPost (..), Context, Repo, contextFrom, postCommitStatus)
import CI.Git (Sha)
import CI.LogPath (logPathFor)
import CI.Node (NodeId (..))
import CI.ProcessCompose.Events (ProcessState (..), ProcessStatus (..), TerminalStatus (..), psToTerminalStatus)
import Control.Concurrent.Async (forConcurrently_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (display)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import System.IO (hPutStrLn, stderr)

-- | Given a process-compose state event for an already-parsed
-- 'NodeId', post the corresponding GitHub commit status under the
-- @\<recipe\>\@\<platform\>@ context. Non-terminal states ('PsOther')
-- drop on the floor.
--
-- The caller ('CI.Pipeline') has the single 'parseNodeId' site, so
-- "is this event for a node we scheduled?" is decided once, not
-- re-decided here and again in 'CI.Verdict.recordOutcome'.
--
-- The status @description@ embeds the path to the node's per-run log
-- (@\<logDir\>\/\<platform\>\/\<recipe\>.log@) so a red check in the
-- GitHub UI carries a navigable pointer to the failing output. The
-- same path is set as the process's @log_location@ in the
-- process-compose YAML, so the file on disk and the path in the
-- status agree by construction.
--
-- Synchronous: each post blocks the subscription loop in
-- 'CI.ProcessCompose.Events.subscribeStates' until @gh api@ returns. This
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
postStatusFor :: Timings -> Repo -> Sha -> FilePath -> NodeId -> ProcessState -> IO ()
postStatusFor timings repo sha logDir node ps
  | not (isUserVisible node) = pure ()
  | otherwise = case ps.status of
      PsRunning -> do
        markStart timings node
        postOne repo sha node Pending (describe Pending Nothing (logPathFor logDir node))
      _ -> case psToTerminalStatus ps of
        Nothing -> pure ()
        Just ts -> do
          mElapsed <- elapsedSince timings node
          let cs = terminalToCommitStatus ts
          postOne repo sha node cs (describe cs mElapsed (logPathFor logDir node))

-- | Pre-seed every node with a 'Pending' commit status at startup —
-- one parallel @gh api@ POST per node, all joined before this returns.
-- The PR's checks panel shows the full set of expected checks the moment
-- the pipeline begins, instead of materializing them one at a time as
-- nodes start. Skipped nodes (whose dep failed) get their @pending@
-- overwritten by @error@ when 'postStatusFor' fires; nodes that never
-- run at all stay at @pending@, which surfaces as a visible "why is
-- this still pending?" signal rather than silent absence.
--
-- GitHub has no batch endpoint for commit statuses
-- (see <https://docs.github.com/en/rest/commits/statuses>), so this is
-- N parallel single-status POSTs. 'forConcurrently_' joins all of them
-- before returning, so the caller can rely on every seed being in place
-- before the pipeline kicks off.
seedPending :: Repo -> Sha -> FilePath -> [NodeId] -> IO ()
seedPending repo sha logDir nodes =
  forConcurrently_ (filter isUserVisible nodes) $ \n ->
    postOne repo sha n Pending $ seedDescription $ logPathFor logDir n

-- | Whether a 'NodeId' belongs on the PR's user-facing checks page.
-- Setup nodes are internal plumbing (bundle ship, drv copy) and
-- never get GH posts; recipe nodes are the user's work and always
-- do. The single source of truth for "is this user-facing?",
-- consumed by both 'seedPending' and 'postStatusFor'.
isUserVisible :: NodeId -> Bool
isUserVisible (RecipeNode _ _) = True
isUserVisible (SetupNode _) = False

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

-- | CI's human-readable label per state, optionally annotated with the
-- elapsed time the node spent running, suffixed with the recipe's log
-- path so the GitHub UI's 140-char description carries a one-click
-- pointer to the matching file under @.ci\/\<sha\>\/@. Path stays
-- under ~80 chars at typical recipe-name lengths, leaving room for
-- the state prose without truncation.
--
-- Elapsed time is only present on terminal posts (Success/Failure/Error);
-- the @PsRunning@ → @Pending@ transition fires at start, so its elapsed
-- would be zero — pointless to display. Pending posts get @Nothing@.
describe :: CommitStatus -> Maybe NominalDiffTime -> FilePath -> Text
describe cs mElapsed = withLogPath (stateLabel cs <> elapsedSuffix mElapsed)
  where
    stateLabel Pending = "Running"
    stateLabel Success = "Succeeded"
    stateLabel Failure = "Failed"
    stateLabel Error = "Errored"
    elapsedSuffix Nothing = ""
    elapsedSuffix (Just dt) = " (" <> formatElapsed dt <> ")"

-- | Description for a 'seedPending' post, formatted the same way as
-- 'describe' so the seed and transition lifecycles share one path-bearing
-- shape. If the description format ever changes (e.g. path moves to a
-- @target_url@ field), 'withLogPath' is the single edit site.
seedDescription :: FilePath -> Text
seedDescription = withLogPath "Queued"

-- | Internal: @\<label\>: \<logPath\>@. Owns the description shape so
-- every status post under the same SHA + context carries the same
-- format across its lifecycle.
withLogPath :: Text -> FilePath -> Text
withLogPath label logPath = label <> ": " <> T.pack logPath

-- | Compact human-readable rendering of a 'NominalDiffTime': @\<n\>s@
-- under a minute, @\<n\>m\<n\>s@ under an hour, @\<n\>h\<n\>m@ otherwise.
-- Sub-second durations round down to @0s@. The format is fixed-width-ish
-- (≤ 6 chars typical) so the @(\<elapsed\>)@ annotation in 'describe' fits
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
-- 'CI.Pipeline.runStrict' alongside the outcome accumulator).
--
-- Single-threaded by construction — the WebSocket observer loop
-- ('CI.ProcessCompose.Events.subscribeStates') invokes 'postStatusFor'
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

-- | GitHub-side mapping for the two terminal classifications. The
-- wire-layer 'PsSkipped' / 'PsErrored' have already been folded into
-- 'TsFailed' by 'CI.ProcessCompose.Events.psToTerminalStatus', so the
-- @Error@-vs-@Failure@ distinction we used to make for "upstream
-- cascaded" doesn't exist here — every non-success surfaces as
-- @Failure@. The cascade story is reconstructed elsewhere from the
-- dep graph + outcome map.
terminalToCommitStatus :: TerminalStatus -> CommitStatus
terminalToCommitStatus TsSucceeded = Success
terminalToCommitStatus TsFailed = Failure
