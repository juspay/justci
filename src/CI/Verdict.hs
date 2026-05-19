{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Per-run outcome accumulator and end-of-run verdict. The observer
-- thread folds every terminal 'ProcessState' into an in-memory
-- 'Outcomes' map (keyed by 'NodeId' — recipe paired with platform);
-- after process-compose exits, the orchestrator calls 'verdictCode'
-- on the map for the pipeline's overall 'ExitCode' and 'verdictSummary'
-- for the printable summary.
--
-- 'RecipeOutcome' is the verdict's vocabulary: two terminal cases.
-- Absence of a node from the event map (encoded as 'Nothing' in the
-- pre-seeded map below) means "the observer never saw a terminal
-- state for this node" — a non-success that flows into 'verdictCode'
-- without needing its own 'RecipeOutcome' constructor.
module CI.Verdict
  ( -- * Outcome values
    RecipeOutcome (..),
    Outcomes,

    -- * Per-event accumulator
    newOutcomes,
    recordOutcome,
    readOutcomes,

    -- * End-of-run summary
    verdictCode,
    verdictSummary,
    exitWithVerdict,

    -- * === Internal (test surface) ===
    terminalToOutcome,
    -- ^ Exposed only for "test.CI.VerdictSpec"'s cross-module
    -- agreement check against 'CI.CommitStatus.terminalToCommitStatus'
    -- — production code reaches the mapping through 'recordOutcome'.
  )
where

import CI.Justfile (RecipeName)
import CI.Node (NodeId (..))
import CI.Platform (Platform)
import CI.ProcessCompose.Events (ProcessState (..), TerminalStatus (..), psToTerminalStatus)
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (Display (..), display)
import qualified Data.Text.IO as TIO
import System.Exit (ExitCode (..), exitWith)

-- | The terminal outcome of one node: ran-and-succeeded, or didn't.
-- "Didn't reach a terminal event at all" (pc crashed before
-- scheduling, network drop on the observer) is *not* a 'RecipeOutcome'
-- constructor — it's the absence of a fold value in the per-node
-- 'Maybe' slot of the 'Outcomes' map. "Skipped because the upstream
-- failed" isn't a constructor either — that's a graph property of
-- the dep map combined with the outcomes, not a per-node primitive.
data RecipeOutcome = Succeeded | Failed
  deriving stock (Show, Eq)

instance Display RecipeOutcome where
  displayBuilder Succeeded = "succeeded"
  displayBuilder Failed = "failed"

-- | The mutable per-run state the observer folds into. Each scheduled
-- 'NodeId' starts with 'Nothing' (no terminal event yet) and flips to
-- @Just outcome@ when 'recordOutcome' fires. Keyed by the typed 'NodeId'
-- so the seed identity and the event-side identity are the same value
-- (no implicit @Text@ convention to drift). Opaque; minted only by
-- 'newOutcomes' and read only by 'readOutcomes'.
newtype Outcomes = Outcomes (IORef (Map NodeId (Maybe RecipeOutcome)))

-- | Pre-populate the outcome map with 'Nothing' for every node in
-- the lowered pipeline. Without this, a node that pc never emits a
-- state event for (e.g. pc crashed before scheduling it) would be
-- absent from the final map entirely; with it, missing-from-pc
-- surfaces as a 'Nothing', which 'verdictCode' treats as a
-- non-success and rolls into a non-zero exit.
newOutcomes :: [NodeId] -> IO Outcomes
newOutcomes nodes =
  Outcomes <$> newIORef (Map.fromList [(n, Nothing) | n <- nodes])

-- | Fold one 'ProcessState' event for an already-parsed 'NodeId' into
-- the outcome map. Routes through 'psToTerminalStatus' — the
-- project-wide ground-truth classifier of process-compose's terminal
-- states — and adopts its outcome under the verdict's own
-- vocabulary. Non-terminal events ('PsRunning', 'PsOther') are
-- dropped; the seed 'Nothing' stays in place until a real terminal
-- event arrives.
--
-- The 'NodeId' is parsed once at the composition site in
-- 'CI.Pipeline'; this module no longer does its own 'parseNodeId'
-- call. That keeps the two consumers of the state stream ('CI.CommitStatus'
-- and this one) from independently deciding whether to drop an event
-- with an unparseable name — there is one parse, one drop decision,
-- and both downstreams agree by construction.
--
-- Safe to call from any thread; the underlying 'atomicModifyIORef''
-- serializes concurrent writes. In practice only the observer thread
-- writes.
recordOutcome :: Outcomes -> NodeId -> ProcessState -> IO ()
recordOutcome (Outcomes ref) node ps =
  for_ (terminalToOutcome <$> psToTerminalStatus ps) $ \o ->
    -- 'Map.adjust' silently drops events for nodes the seed doesn't
    -- already know about. That's the right policy: every legitimate
    -- node was seeded by 'newOutcomes', so an unknown key means pc
    -- emitted a state for something we didn't ask it to schedule
    -- (which shouldn't happen, and adding ghost entries to the map
    -- would only confuse the summary).
    atomicModifyIORef' ref (\m -> (Map.adjust (const (Just o)) node m, ()))

-- | Verdict-side relabeling of the two terminal classifications.
terminalToOutcome :: TerminalStatus -> RecipeOutcome
terminalToOutcome TsSucceeded = Succeeded
terminalToOutcome TsFailed = Failed

-- | End-of-run convenience: snapshot the accumulator, print the
-- per-recipe summary to stdout, and exit with the derived code.
-- Glue around 'verdictSummary' + 'verdictCode' for the orchestrator's
-- one common shape. The pure functions remain the seam for any
-- caller that wants the summary without exiting (e.g. a future MCP
-- server or HTTP handler).
--
-- @mkHost@ resolves the host each node ran on — the orchestrator
-- threads this in from the loaded 'CI.Hosts.Hosts' so the verdict
-- module doesn't take a direct dependency on the hosts vocabulary.
exitWithVerdict :: (NodeId -> Text) -> Outcomes -> IO ()
exitWithVerdict mkHost outcomes = do
  o <- readOutcomes outcomes
  mapM_ TIO.putStrLn (verdictSummary mkHost o)
  exitWith (verdictCode o)

-- | Snapshot the accumulator. Call once, after the observer subscription
-- has closed (the WebSocket closes when process-compose exits, so by
-- this point every terminal event has been folded in).
readOutcomes :: Outcomes -> IO (Map NodeId (Maybe RecipeOutcome))
readOutcomes (Outcomes ref) = readIORef ref

-- | The pipeline's exit code: 'ExitSuccess' iff every node in the
-- snapshot finished @'Just' 'Succeeded'@; anything else — @Just Failed@
-- or 'Nothing' (no terminal event) — flips it to 'ExitFailure' 1.
-- Pure; trivial to test against handcrafted maps.
verdictCode :: Map NodeId (Maybe RecipeOutcome) -> ExitCode
verdictCode outcomes
  | all (== Just Succeeded) (Map.elems outcomes) = ExitSuccess
  | otherwise = ExitFailure 1

-- | The pipeline's printable summary, structured in two sections:
--
--   * @Setup@ — one line per remote 'SetupNode' showing host and
--     outcome. Surfaces provisioning failures explicitly so a failed
--     bundle/drv ship doesn't hide behind cascading recipe lines.
--     Omitted when there are no setup nodes (local-only runs).
--   * @Recipes@ — recipes grouped by platform lane. A lane whose
--     setup failed (or never reached terminal) collapses to a single
--     @"not scheduled (setup failed)"@ line instead of repeating
--     @"did not run"@ per recipe; lanes whose setup succeeded (or
--     had no setup at all) list each recipe's outcome.
--
-- Pure; companion to 'verdictCode' over the same snapshot.
--
-- The host column shows where each node ran ("local" for the
-- orchestrator-local lane, the SSH host name otherwise). Caller
-- supplies the resolver so this module doesn't depend on the
-- "CI.Hosts" vocabulary directly.
--
-- A 'Nothing' outcome on a recipe — scheduled but no terminal event
-- reached us — renders as @"did not run"@ when the lane's setup
-- succeeded; if the setup itself failed, the whole lane folds into
-- the @"not scheduled"@ summary and the recipe's individual outcome
-- is suppressed.
verdictSummary :: (NodeId -> Text) -> Map NodeId (Maybe RecipeOutcome) -> [Text]
verdictSummary mkHost outcomes =
  ["── ci run summary ─────────────────────────────"]
    <> setupSection
    <> recipeSection
    <> ["───────────────────────────────────────────────", verdictLine]
  where
    -- Split by kind so the two sections render from disjoint inputs.
    setupOutcomes = [(p, o) | (SetupNode p, o) <- Map.toAscList outcomes]
    recipeOutcomes = [(r, p, o) | (RecipeNode r p, o) <- Map.toAscList outcomes]

    -- Platforms whose setup failed (or never reached terminal). Used
    -- to decide whether a lane's recipes get per-recipe lines or one
    -- "not scheduled" rollup.
    setupFailed p = case lookup p setupOutcomes of
      Just (Just Succeeded) -> False
      Just _ -> True
      Nothing -> False -- no setup node at all (local lane): recipes run as normal

    -- Recipes grouped by platform, preserving Map's ascending order
    -- for stable rendering across runs.
    recipesByPlatform :: Map.Map Platform [(RecipeName, Maybe RecipeOutcome)]
    recipesByPlatform =
      Map.fromListWith (flip (<>)) [(p, [(r, o)]) | (r, p, o) <- recipeOutcomes]

    -- Setup section: omitted if no setup nodes exist.
    setupSection
      | null setupOutcomes = []
      | otherwise =
          ["  Setup"]
            <> [ "    " <> pad setupPlatWidth (display p) <> "  " <> pad setupHostWidth (mkHost (SetupNode p)) <> "  " <> renderOutcome o
               | (p, o) <- setupOutcomes
               ]
            <> [""]

    -- Recipes section: per-lane sub-blocks.
    recipeSection
      | null recipeOutcomes = []
      | otherwise = ["  Recipes"] <> concatMap renderLane (Map.toAscList recipesByPlatform)

    renderLane (plat, recipes)
      | setupFailed plat =
          ["    " <> display plat <> " (" <> mkHost (SetupNode plat) <> "):"]
            <> ["      not scheduled (setup failed)"]
      | otherwise =
          ["    " <> display plat <> " (" <> mkHost (firstNode recipes plat) <> "):"]
            <> [ "      " <> pad (recipeNameWidth recipes) (display r) <> "  " <> renderOutcome o
               | (r, o) <- recipes
               ]
      where
        firstNode ((r, _) : _) p = RecipeNode r p
        firstNode [] p = SetupNode p -- defensive; empty lane shouldn't happen

    -- Column-width helpers.
    setupPlatWidth = maximum (0 : [T.length (display p) | (p, _) <- setupOutcomes])
    setupHostWidth = maximum (0 : [T.length (mkHost (SetupNode p)) | (p, _) <- setupOutcomes])
    recipeNameWidth rs = maximum (0 : [T.length (display r) | (r, _) <- rs])
    pad w t = t <> T.replicate (w - T.length t) " "

    renderOutcome (Just o) = display o
    renderOutcome Nothing = "did not run"

    -- Bottom-line tally counts every scheduled node (setup + recipes)
    -- that didn't reach 'Just Succeeded'.
    totalCount = Map.size outcomes
    failedCount = Map.size (Map.filter (/= Just Succeeded) outcomes)
    verdictLine
      | failedCount == 0 = "all " <> tshow totalCount <> " nodes succeeded"
      | otherwise =
          tshow failedCount
            <> " of "
            <> tshow totalCount
            <> " nodes did not succeed"
    tshow = T.pack . show
