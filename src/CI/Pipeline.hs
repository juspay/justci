{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Orchestration for the per-subcommand entry points
-- ('runLocal', 'runStrict', 'runGraph', 'runDumpYaml', 'runProtect')
-- and the runtime artifact layout under @\$PWD\/.ci\/@. "Main" is the
-- dispatch layer; everything mode-specific lives here. The pure graph
-- shape change (recipe DAG → platform-fanned NodeId DAG, plus the
-- user-selector filter) lives in "CI.Fanout"; this module composes
-- 'CI.Fanout' with @just --dump@ → root resolution → process-compose
-- YAML rendering.
module CI.Pipeline
  ( -- * Runtime artifact layout
    RunDir (..),
    ensureRunDir,

    -- * Run modes
    runLocal,
    runStrict,
    runMcp,
    runGraph,
    runDumpYaml,
    runProtect,

    -- * Pipeline assembly
    RunMode (..),
    BuildGraphError,
    buildNodeGraph,
    buildProcessCompose,
  )
where

import qualified Algebra.Graph.AdjacencyMap as G
import CI.CLI (ProtectOpts (..), RunOpts (..))
import CI.CommitStatus (contextForNode, isUserVisible, newTimings, postStatusFor, seedPending)
import CI.Fanout (applySelectors, fanOut, isRemote, pipelinePlatformsFor, rootOsFamilies)
import CI.Gh (setRequiredChecks, viewDefaultBranch, viewRepo)
import CI.Git (Sha, ensureCleanTree, resolveSha, shaPlaceholder, withSnapshotWorktree)
import CI.Graph (lowerToRunnerGraph, reachableSubgraph)
import CI.Hosts (Hosts, loadHosts, lookupHost, mergeHostOverrides)
import CI.Justfile (RecipeName, fetchDump, recipeCommand)
import qualified CI.Justfile as J
import CI.LogPath (logDirFor, logPathFor, platformDir)
import CI.Node (DagSelection (..), NodeId (..), defaultDagSelection, nodePlatform, parseNodeId, toMermaid)
import CI.Platform (Platform, localPlatform)
import CI.ProcessCompose (ProcessCompose, UpInvocation (..), disableAllProcesses, processGraph, processNames, runProcessCompose, stdioMcp, toProcessCompose, withMcpServer)
import CI.ProcessCompose.Events (ProcessState (..), subscribeStates)
import CI.Root (findRoot)
import CI.Transport (sshRecipeCommand, sshSetupCommand)
import CI.Verdict (exitWithVerdict, newOutcomes, recordOutcome)
import Control.Concurrent.Async (link, wait, withAsync)
import Control.Monad (void)
import qualified Data.ByteString as BS
import Data.Foldable (for_)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text.Display (Display (..), display)
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Y
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.Exit (die, exitWith)
import System.FilePath ((</>))

-- | The runtime artifact paths under @\$PWD\/.ci\/@. Built once at the top
-- of a run so 'runLocal' and 'runStrict' both reference the same
-- convention instead of hand-rolling @runDir \<\/\> "pc.log"@ at each
-- call site.
data RunDir = RunDir
  { worktreePath :: FilePath,
    sock :: FilePath,
    pcLog :: FilePath,
    pcYaml :: FilePath
  }

-- | Create @\$PWD\/.ci\/@ (if missing) and return the canonical sub-paths.
-- Everything we write at runtime lives here so the user gitignores
-- @\/.ci\/@ once and forgets about it.
ensureRunDir :: IO RunDir
ensureRunDir = do
  cwd <- getCurrentDirectory
  let dir = cwd </> ".ci"
  createDirectoryIfMissing True dir
  pure
    RunDir
      { worktreePath = dir </> "worktree",
        sock = dir </> "pc.sock",
        pcLog = dir </> "pc.log",
        pcYaml = dir </> "pc.yaml"
      }

-- | Local mode: live working tree, no GitHub status posts, no per-recipe
-- log routing. The observer still runs — its only consumer is the
-- verdict accumulator, which gives developer runs the same end-of-run
-- summary strict mode produces. Process-compose's log goes to
-- @.ci\/pc.log@ so even local runs don't leak into @\$TMPDIR@; the same
-- UDS at @.ci\/pc.sock@ is bound so the API surface is available for
-- future consumers (e.g. an MCP server).
--
-- SSH lanes are supported in local mode too: any non-local platform
-- in the pipeline requires a @~\/.config\/ci\/hosts.json@ entry (the
-- user opts in by editing the file; missing entries are excluded
-- from the fanout by 'pipelinePlatformsFor'). Each remote lane gets
-- an SSH-shaped @command@ that bundles @HEAD@ across rather than
-- the dirty live tree — the dev's uncommitted work is intentionally
-- invisible to remote lanes; the bundle reflects committed history
-- only.
runLocal :: RunOpts -> [String] -> RunDir -> IO ()
runLocal opts passthrough dirs = do
  hosts <- mergeHostOverrides opts.hostOverrides <$> (dieOnLeft =<< loadHosts)
  pc <- attachMcp opts <$> (dieOnLeft =<< buildProcessCompose hosts opts.dagSelection LocalRun)
  outcomes <- newOutcomes (processNames pc)
  let onState ps = withParsedNode ps $ \node -> recordOutcome outcomes node ps
  withObserver dirs.sock onState $
    void $
      runProcessCompose (UpInvocation dirs.sock dirs.pcLog dirs.pcYaml opts.tui passthrough) pc
  exitWithVerdict (hostFor hosts) outcomes

-- | Strict mode: clean-tree refuse → resolve repo + SHA → snapshot HEAD
-- via @git worktree@ at @.ci\/worktree@ → start process-compose with its
-- API on @.ci\/pc.sock@ → subscribe to state events, post commit
-- statuses, and accumulate the per-node outcome map concurrently with
-- the pipeline run.
--
-- Per-node stdout/stderr is split into
-- @.ci\/\<sha\>\/\<platform\>\/\<recipe\>.log@ (created here before
-- process-compose spawns) so each GitHub commit status can carry a
-- navigable path to the matching log. The SHA-keyed directory keeps
-- history across runs: a green-then-red sequence on the same checkout
-- leaves both runs' logs side-by-side under @.ci\/@.
--
-- The two consumers of the state stream — 'postStatusFor' (GitHub
-- write) and 'recordOutcome' (local accumulator) — are composed at
-- this single call site rather than entangled inside the observer or
-- the GH-posting code. Both share
-- 'CI.ProcessCompose.Events.psToTerminalStatus' as the underlying
-- terminal-state classifier, so the GH check page and the local
-- verdict agree on which nodes succeeded.
--
-- Process-compose's own exit code is intentionally ignored — with
-- @restart: no@ on every process it no longer reflects pipeline
-- outcome (a failed node leaves pc exiting 0). The accumulated
-- outcome map is the source of truth; 'exitWithVerdict' derives the
-- final 'ExitCode' from it.
runStrict :: RunOpts -> [String] -> RunDir -> IO ()
runStrict opts passthrough dirs = do
  dieOnLeft =<< ensureCleanTree
  repo <- dieOnLeft =<< viewRepo
  sha <- dieOnLeft =<< resolveSha
  hosts <- mergeHostOverrides opts.hostOverrides <$> (dieOnLeft =<< loadHosts)
  let logDir = logDirFor sha
  withSnapshotWorktree dirs.worktreePath $ do
    pc <- attachMcp opts <$> (dieOnLeft =<< buildProcessCompose hosts opts.dagSelection (StrictRun dirs.worktreePath logDir))
    let nodes = processNames pc
    createPlatformDirs logDir nodes
    seedPending repo sha logDir nodes
    outcomes <- newOutcomes nodes
    timings <- newTimings
    let onState ps = withParsedNode ps $ \node ->
          postStatusFor timings repo sha logDir node ps
            >> recordOutcome outcomes node ps
    withObserver dirs.sock onState $
      void $
        runProcessCompose (UpInvocation dirs.sock dirs.pcLog dirs.pcYaml opts.tui passthrough) pc
    exitWithVerdict (hostFor hosts) outcomes

-- | MCP-server mode. Spawn pc with @mcp_server: { transport: stdio }@
-- and every process @disabled: true@ — pc runs as a JSON-RPC host on
-- stdin/stdout, the MCP tools list the registered pipeline, and the
-- attached agent decides what to execute via @pc_process_start@.
--
-- No observer, no outcome accumulator, no verdict summary: this
-- isn't a CI run, it's an interactive session. pc's own exit code
-- is forwarded as the runner's exit code; @--keep-project@ is
-- injected so pc stays alive while the MCP session is open even if
-- the agent never starts a process (or starts then-terminates all
-- of them).
--
-- @--tui@ is forced off — pc auto-disables it anyway under stdio
-- transport, but spelling it explicitly here documents the
-- incompatibility at the call site.
--
-- The SHA baked into the YAML (for remote @ssh@ commands) is
-- frozen at the moment @ci run --mcp@ launches. Working-tree edits
-- to local lanes still take effect on each subsequent
-- @pc_process_start@, but remote lanes always operate against the
-- pinned SHA; to follow a new commit, the user restarts the MCP
-- session.
runMcp :: RunOpts -> [String] -> RunDir -> IO ()
runMcp opts passthrough dirs = do
  hosts <- mergeHostOverrides opts.hostOverrides <$> (dieOnLeft =<< loadHosts)
  pc <- attachMcp opts <$> (dieOnLeft =<< buildProcessCompose hosts opts.dagSelection LocalRun)
  let mcpPassthrough = "--keep-project" : passthrough
      up = UpInvocation dirs.sock dirs.pcLog dirs.pcYaml False mcpPassthrough
  exitCode <- runProcessCompose up pc
  exitWith exitCode

-- | Print the assembled pipeline's dependency graph to stdout in
-- Mermaid @flowchart@ syntax. Uses the same 'DumpRun' shape
-- @dump-yaml@ uses, so the rendered graph reflects the full fanout
-- without prompting for hosts or shelling out to git.
--
-- Pc's own @process-compose graph@ subcommand is server-only — it
-- queries a running pc instance over HTTP/UDS rather than reading a
-- YAML file — so it can't render the graph standalone. Emitting
-- Mermaid here keeps the command useful outside a live run; pipe
-- through @mermaid-ascii@ (or paste into <https://mermaid.live>) to
-- visualise.
--
-- Read-only: 'ci graph' must not create @.ci\/@ as a side effect.
-- That's why this takes no 'RunDir' — only 'runLocal' and
-- 'runStrict' need the runtime-artifact paths.
runGraph :: IO ()
runGraph = do
  hosts <- dieOnLeft =<< loadHosts
  pc <- dieOnLeft =<< buildProcessCompose hosts defaultDagSelection DumpRun
  TIO.putStrLn (toMermaid (processGraph pc))

-- | Emit the assembled process-compose YAML to stdout. Uses 'DumpRun'
-- mode so no host resolution side-effects occur — safe to invoke
-- offline, outside a git checkout, or from a remote whose
-- @hosts.json@ has no entry for the other platform.
runDumpYaml :: IO ()
runDumpYaml = do
  hosts <- dieOnLeft =<< loadHosts
  pc <- dieOnLeft =<< buildProcessCompose hosts defaultDagSelection DumpRun
  BS.putStr (Y.encode pc)

-- | Branch-protection helper: read the canonical DAG, extract the
-- @(recipe, platform)@ context for every user-facing node, and PATCH
-- the GitHub branch protection's required-status-checks list to
-- exactly that set. The user-facing filter is the same one
-- 'CI.CommitStatus.postStatusFor' applies — setup nodes never post
-- statuses, so they never show up as required checks either.
--
-- The DAG comes from the canonical @[metadata("ci")]@ root: there's
-- no @--root@ on @protect@ because the required-check list is the
-- source of truth for "what statuses must exist on every PR", not a
-- partial / leaf re-run knob (per the discussion in #20).
--
-- 'opts.branchOverride' selects which branch to protect; absent, we
-- look up the repo's default branch via @gh repo view@.
-- 'opts.dryRun' prints the contexts and exits without touching the
-- API, for inspection before flipping the live ruleset.
--
-- @gh@ returns 404 if branch protection isn't enabled on the
-- target branch — that's a one-time toggle the repo owner does in
-- the GitHub UI before @ci protect@ is meaningful.
runProtect :: ProtectOpts -> IO ()
runProtect opts = do
  hosts <- dieOnLeft =<< loadHosts
  (nodeGraph, _) <- dieOnLeft =<< buildNodeGraph hosts defaultDagSelection
  let contexts = contextForNode <$> filter isUserVisible (G.vertexList nodeGraph)
  case contexts of
    [] -> die "no recipe nodes in the DAG — nothing to require"
    _ -> pure ()
  let nCtx = T.pack (show (length contexts))
  if opts.dryRun
    then do
      TIO.putStrLn $ "would PATCH required_status_checks (" <> nCtx <> " contexts):"
      for_ contexts $ \c -> TIO.putStrLn $ "  " <> display c
    else do
      repo <- dieOnLeft =<< viewRepo
      branch <- case opts.branchOverride of
        Just b -> pure b
        Nothing -> dieOnLeft =<< viewDefaultBranch
      dieOnLeft =<< setRequiredChecks repo branch contexts
      TIO.putStrLn $ "updated required_status_checks on " <> display branch <> " (" <> nCtx <> " contexts)"

-- | Materialise every @.ci\/\<sha\>\/\<platform\>\/@ subdirectory the
-- pipeline will route logs to, before process-compose spawns. pc
-- creates the per-recipe log *file* itself but won't create
-- intermediate directories — without this the first event for a
-- platform whose subdir doesn't exist fails the spawn.
createPlatformDirs :: FilePath -> [NodeId] -> IO ()
createPlatformDirs logDir nodes =
  mapM_ (createDirectoryIfMissing True . platformDir logDir) (nub (nodePlatform <$> nodes))

-- | Enforce the wire-event-identity invariant at the single site that
-- owns it: parse @ps.name@ as a 'NodeId' and run @action@ only if it
-- names a node we scheduled. Both observers ('postStatusFor' and
-- 'recordOutcome') consume the resulting parsed 'NodeId', so the
-- drop-on-unparseable policy is decided once here instead of being
-- re-decided in each downstream module. The name signals the
-- parse/filter responsibility — this is the gate, not a bare iteration.
withParsedNode :: ProcessState -> (NodeId -> IO ()) -> IO ()
withParsedNode ps action = for_ (parseNodeId ps.name) action

-- | Apply the @--mcp@ overlay to an assembled YAML: attach
-- 'CI.ProcessCompose.stdioMcp' /and/ mark every process @disabled@.
-- pc spawns, the MCP server is reachable on stdio, the project
-- registers all 14-or-so nodes, but /none/ of them auto-start.
-- The attached agent decides what to run via @pc_process_start@
-- — the runner is purely a host for pc; it doesn't drive
-- execution itself when MCP is on. Without @--mcp@, leave the YAML
-- exactly as 'toProcessCompose' built it.
attachMcp :: RunOpts -> ProcessCompose -> ProcessCompose
attachMcp opts pc
  | opts.mcp = disableAllProcesses (withMcpServer stdioMcp pc)
  | otherwise = pc

-- | Bracket @body@ between a 'subscribeStates' subscription on @sock@
-- and a clean @wait@ on it: spawn the observer, 'link' so its crash
-- aborts the caller, run @body@, then 'wait' for the WebSocket to
-- close (which it does when process-compose exits). The
-- async-lifecycle scaffold lives here so 'runLocal' and 'runStrict'
-- vary only in their @onState@ callback and the body they pass.
withObserver :: FilePath -> (ProcessState -> IO ()) -> IO a -> IO a
withObserver sockP onState body =
  withAsync (subscribeStates sockP onState) $ \obs -> do
    link obs
    result <- body
    wait obs
    pure result

-- | The two pipeline-build modes. 'LocalRun' is the @dev@ / @dump-yaml@
-- shape: no worktree pin, no per-recipe log routing. 'StrictRun'
-- carries the two paths that always travel together — the @git
-- worktree@ snapshot every local recipe @chdir@s into, and the
-- @.ci\/\<sha\>\/@ log directory the YAML emitter routes each
-- process's stdout/stderr to. A sum type instead of two parallel
-- @Maybe FilePath@s rules out the mixed @(Just, Nothing)@ /
-- @(Nothing, Just)@ states that produce logically inconsistent YAML.
data RunMode
  = LocalRun
  | -- | @StrictRun worktreeDir logDir@.
    StrictRun FilePath FilePath
  | -- | YAML-inspection mode for @dump-yaml@: no working dir, no log
    --       routing, and (importantly) no host resolution side effects.
    --       Missing 'CI.Hosts.Host' entries are tolerated; SSH-lane
    --       commands render with a placeholder so the structural keys
    --       (process names, depends_on edges) still reflect the real
    --       fanout. Used by the macos remote's smoke test where stdin is
    --       closed and prompting would deadlock.
    DumpRun

-- | The two YAML-shape projections of 'RunMode': the per-node working
-- directory every recipe @chdir@s into, and the per-node log location
-- the YAML emitter routes stdout/stderr to. Both vary together across
-- modes — 'StrictRun' supplies both; 'LocalRun' and 'DumpRun' supply
-- neither — so they live in a single 'RunMode'-pattern-match rather
-- than two parallel where-clauses that have to stay in lockstep
-- across future 'RunMode' constructors.
--
-- The working-dir callback opts setup nodes out of the worktree pin:
-- they're @ssh -T \<host\>@ launcher processes whose local cwd is
-- ignored by ssh, so 'Just worktreePath' would be a misleading no-op
-- in the emitted YAML. The setup-vs-recipe choice is now structural
-- ('NodeId' pattern match), not a name-based predicate.
--
-- The log-location callback intentionally does *not* skip setup nodes
-- the way 'CI.CommitStatus' and 'CI.Verdict' do for their user-facing
-- surfaces. The reporting filter exists so the PR author doesn't see
-- internal plumbing on their checks page or in the summary line; the
-- log file exists for debugging when setup *fails*. Hiding setup-node
-- output would leave a failed bundle ship or drv copy with nowhere to
-- look. Same predicate, different consumers, different visibility
-- goals.
yamlPathsFor :: RunMode -> (NodeId -> Maybe FilePath, NodeId -> Maybe FilePath)
yamlPathsFor (StrictRun wt ld) = (workingDirFor wt, Just . logPathFor ld)
  where
    workingDirFor _ (SetupNode _) = Nothing
    workingDirFor w (RecipeNode _ _) = Just w
yamlPathsFor LocalRun = (const Nothing, const Nothing)
yamlPathsFor DumpRun = (const Nothing, const Nothing)

-- | A user-recoverable failure during graph construction. Surfaced
-- through @Either@ rather than 'die' so callers ('runLocal',
-- 'runStrict', 'runGraph', 'runDumpYaml', 'runProtect') own the
-- die-vs-respond boundary at one place. Other failures inside
-- 'buildNodeGraph' (justfile parse, recipe ordering cycles, reachability
-- on a missing recipe, local-system classification) already flow
-- through their own 'Either' types and are funneled here via
-- 'dieOnLeft' at the same boundary.
data BuildGraphError
  = -- | @--root \<r\>@ named a recipe that isn't in the justfile.
    --     Carries the bad name so the display rendering can echo it
    --     back to the user.
    BadRoot RecipeName
  | -- | The root recipe declares OS attrs (e.g. @[linux] [macos]@),
    --     but none of those families have a matching system configured
    --     — neither @localPlatform@ nor any host in @hosts.json@.
    --     Carries the root name + the unsatisfied OS families so the
    --     display rendering names both.
    EmptyFanout RecipeName [J.Os]
  deriving stock (Show)

instance Display BuildGraphError where
  displayBuilder (BadRoot r) =
    "--root " <> displayBuilder r <> " is not a recipe in the justfile"
  displayBuilder (EmptyFanout rootName oss) =
    "root recipe declares OS attrs but no matching system is configured. "
      <> "Either remove the OS attrs from "
      <> displayBuilder rootName
      <> " or add an entry to ~/.config/ci/hosts.json for one of: "
      <> displayBuilder (T.pack (unwords (show <$> oss)))

-- | Walk @just --dump@ → root → reachable subgraph → topologically
-- lowered DAG → fan out across the pipeline's platform set → filter
-- by the user's 'DagSelection'. The graph-construction half of the
-- pipeline; 'buildProcessCompose' adds the transport + YAML render
-- pass on top.
--
--  * Pipeline platforms come from the root recipe's OS attributes
--    (@[linux] [macos] [metadata(\"ci\")] root:@). A root with no
--    OS attrs defaults to the local platform only.
--
--  * Host resolution loads @~\/.config\/ci\/hosts.json@ once.
--    'pipelinePlatformsFor' silently excludes platforms with no
--    entry from the fanout, so a missing host is never a runtime
--    failure — the user opts in by editing the file.
--
-- Exposed separately from 'buildProcessCompose' because 'runProtect'
-- and any future graph-only consumer (e.g. a "what would this run?"
-- query) genuinely only need the node set — they don't want to pay
-- the cost of SHA resolution + YAML assembly that
-- 'buildProcessCompose' adds on top. The two phases sit on different
-- volatility axes anyway: DAG shape changes with the recipe graph
-- and the fanout policy; YAML field names / dep-edge syntax change
-- with process-compose's schema.
--
-- Returns the filtered fanned-out graph plus the local platform —
-- the latter is needed downstream for transport selection
-- ('commandForNode') and is computed here anyway as part of fanout.
buildNodeGraph :: Hosts -> DagSelection -> IO (Either BuildGraphError (G.AdjacencyMap NodeId, Platform))
buildNodeGraph hosts sel = do
  recipes <- dieOnLeft =<< fetchDump
  rootResult <- case sel.rootOverride of
    Just r
      | Map.member r recipes -> pure (Right r)
      | otherwise -> pure (Left (BadRoot r))
    Nothing -> Right <$> dieOnLeft (findRoot recipes)
  case rootResult of
    Left err -> pure (Left err)
    Right rootName -> do
      rootRecipe <- case Map.lookup rootName recipes of
        Just r -> pure r
        -- The root-resolution above guarantees membership; defensive only.
        Nothing -> die $ "internal error: root " <> T.unpack (display rootName) <> " missing from recipe map"
      reachable <- dieOnLeft $ reachableSubgraph rootName recipes
      recipeGraph <- dieOnLeft $ lowerToRunnerGraph reachable
      localPlat <- dieOnLeft localPlatform
      let pipelinePlatforms = pipelinePlatformsFor rootRecipe localPlat hosts
      case pipelinePlatforms of
        [] -> pure (Left (EmptyFanout rootName (rootOsFamilies rootRecipe)))
        _ -> do
          let unfilteredNodeGraph = fanOut localPlat hosts pipelinePlatforms recipeGraph
          nodeGraph <- dieOnLeft $ applySelectors sel.selectorMode pipelinePlatforms unfilteredNodeGraph
          pure (Right (nodeGraph, localPlat))

-- | Build the full 'ProcessCompose' YAML: extends 'buildNodeGraph'
-- with SHA resolution, transport command rendering, and the
-- per-mode YAML projections ('yamlPathsFor'). Used by every
-- subcommand that actually drives process-compose ('runLocal',
-- 'runStrict') or emits its YAML/graph form ('runDumpYaml',
-- 'runGraph').
--
-- Each fanned-out 'NodeId' gets a local or @ssh host@ command
-- depending on whether its platform matches the runner's; the
-- 'CI.Transport' builders are the only site that know SSH command
-- shapes.
buildProcessCompose :: Hosts -> DagSelection -> RunMode -> IO (Either BuildGraphError ProcessCompose)
buildProcessCompose hosts sel mode = do
  result <- buildNodeGraph hosts sel
  case result of
    Left err -> pure (Left err)
    Right (nodeGraph, localPlat) -> do
      -- Same predicate 'fanOut' uses to decide where to emit setup
      -- nodes — sourcing both from one definition avoids the dormant
      -- divergence risk of two near-identical "is this platform
      -- remote?" predicates. Computed over the *filtered* node set so
      -- a partial run that excluded every remote lane doesn't ask for
      -- a SHA it doesn't need.
      let hasRemote = any (\node -> isRemote (nodePlatform node) (localPlat, hosts)) (G.vertexList nodeGraph)
      -- A Sha is needed iff at least one remote lane is fanned out
      -- (setup nodes ship a bundle that gets @git checkout@'d on the
      -- remote at this SHA). @DumpRun@ uses 'shaPlaceholder' so
      -- inspection works outside a git checkout; non-remote local runs
      -- also use the placeholder (never consumed — the graph has no
      -- nodes that read it).
      sha <- case mode of
        DumpRun -> pure shaPlaceholder
        _ | hasRemote -> dieOnLeft =<< resolveSha
        _ -> pure shaPlaceholder
      let mkCommand = commandForNode sha localPlat hosts
          (yamlWorkingDir, yamlLogLocation) = yamlPathsFor mode
      pure (Right (toProcessCompose mkCommand yamlWorkingDir yamlLogLocation nodeGraph))

-- | Per-node command construction. Dispatches over (host lookup,
-- node kind) and picks one of the three valid command builders in
-- 'CI.Transport'. The "(Local, SetupNode)" combination is
-- structurally absent — there is no @localSetupCommand@ — so the
-- match is total over the cases the fanout actually produces:
--
--   * @(RecipeNode, no host, local platform)@ → 'CI.Justfile.recipeCommand'
--   * @(RecipeNode, host)@                    → 'sshRecipeCommand'
--   * @(SetupNode, host)@                     → 'sshSetupCommand'
--
-- @sha@ is consumed only by the SSH builders. Local-mode runs
-- without remote lanes pass 'shaPlaceholder' as a no-op (never
-- read — the graph has no SSH nodes).
commandForNode :: Sha -> Platform -> Hosts -> NodeId -> T.Text
commandForNode sha localPlat hosts node = case (node, lookupHost plat hosts) of
  (RecipeNode r _, Nothing)
    | plat == localPlat -> recipeCommand r
    | otherwise -> hostContractError
  (RecipeNode r _, Just h) -> sshRecipeCommand h sha plat r
  (SetupNode _, Just h) -> sshSetupCommand h sha plat
  -- 'fanOut' emits setup nodes only for platforms with a hosts entry,
  -- so this branch is unreachable given the invariants. Surface a
  -- contract error rather than make it a 'commandFor' input shape.
  (SetupNode _, Nothing) -> setupOnLocalError
  where
    plat = nodePlatform node
    hostContractError =
      error $
        "internal error: no SSH host for "
          <> T.unpack (display plat)
          <> " (pipelinePlatformsFor should have excluded this)"
    setupOnLocalError =
      error $
        "internal error: SetupNode for "
          <> T.unpack (display plat)
          <> " with no hosts entry (fanOut emits setup only for remote platforms)"

-- | The @NodeId -> host-label@ resolver the verdict summary prints.
-- Pure: closes over an already-loaded 'Hosts' so the caller controls
-- how many times the JSON is parsed (once, at the top of
-- 'runLocal' / 'runStrict' / 'runGraph'). Nodes whose platform has
-- an entry render as that host; nodes without an entry render as
-- @"local"@ (the orchestrator-local lane that ran inline).
--
-- 'CI.Verdict' still receives an opaque @NodeId -> Text@ resolver,
-- so its independence from the 'CI.Hosts' vocabulary is preserved
-- without the cost of a second 'loadHosts' call.
hostFor :: Hosts -> NodeId -> T.Text
hostFor hosts n = case lookupHost (nodePlatform n) hosts of
  Just h -> display h
  Nothing -> "local"

-- | The single 'die' site in the project: every recoverable failure
-- mode threads up through @Either e a@ to this boundary, where the
-- structured error's 'Display' rendering becomes the exit message.
--
-- Shape note: takes @Either e a@ rather than @IO (Either e a)@ so the
-- same helper works for both pure Eithers (@dieOnLeft $ findRoot
-- recipes@) and IO ones (@dieOnLeft =<< ensureCleanTree@). A helper
-- typed to @IO (Either e a) -> IO a@ would force every pure call site
-- to add a @pure@.
dieOnLeft :: Display e => Either e a -> IO a
dieOnLeft = either (die . T.unpack . display) pure
