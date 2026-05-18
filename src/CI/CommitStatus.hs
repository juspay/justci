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

    -- * === Internal (test surface) ===
    terminalToCommitStatus,
    -- ^ Exposed only for "test.CI.VerdictSpec"'s cross-module
    -- agreement check against 'CI.Verdict.terminalToOutcome' —
    -- production code reaches this mapping through 'postStatusFor'.
  )
where

import CI.Gh (CommitStatus (..), CommitStatusPost (..), Context, Repo, contextFrom, postCommitStatus)
import CI.Git (Sha)
import CI.LogPath (logPathFor)
import CI.Node (NodeId (..))
import CI.ProcessCompose.Events (ProcessState (..), ProcessStatus (..), TerminalStatus (..), psToTerminalStatus)
import Control.Concurrent.Async (forConcurrently_)
import Data.Foldable (for_)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (display)
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
postStatusFor :: Repo -> Sha -> FilePath -> NodeId -> ProcessState -> IO ()
postStatusFor repo sha logDir node ps
  | isUserVisible node =
      for_ (psToCommitStatus ps) $ \cs ->
        postOne repo sha node cs $ describe cs $ logPathFor logDir node
  | otherwise = pure ()

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

-- | CI's human-readable label per state, suffixed with the recipe's log
-- path so the GitHub UI's 140-char description carries a one-click
-- pointer to the matching file under @.ci\/\<sha\>\/@. Path stays under
-- ~80 chars at typical recipe-name lengths, leaving room for the state
-- prose without truncation.
describe :: CommitStatus -> FilePath -> Text
describe cs = withLogPath $ stateLabel cs
  where
    stateLabel Pending = "Running"
    stateLabel Success = "Succeeded"
    stateLabel Failure = "Failed"
    stateLabel Error = "Errored"

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

-- | Translate one process-compose 'ProcessState' event into the
-- 'CommitStatus' it surfaces under. Non-terminal states ('PsOther')
-- return 'Nothing' so consumers can drop them. The terminal cases
-- delegate to 'psToTerminalStatus' (the project-wide ground-truth
-- predicate) and add the GitHub-specific @PsRunning -> Pending@
-- transition on top — the verdict accumulator in "CI.Verdict" reuses
-- the same base classifier directly, so the GH check page and the
-- local exit code stay in agreement without "CI.Verdict" having to
-- depend on this module.
psToCommitStatus :: ProcessState -> Maybe CommitStatus
psToCommitStatus ps = case ps.status of
  PsRunning -> Just Pending
  _ -> terminalToCommitStatus <$> psToTerminalStatus ps

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
