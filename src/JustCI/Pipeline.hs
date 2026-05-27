{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Orchestration for the per-subcommand entry points
-- ('runLocal', 'runStrict', 'runGraph', 'runDumpYaml', 'runProtect')
-- and the runtime artifact layout under @\$PWD\/.ci\/@. "Main" is the
-- dispatch layer; everything mode-specific lives here. The pure graph
-- shape change (recipe DAG → platform-fanned NodeId DAG, plus the
-- user-selector filter) lives in "JustCI.Fanout"; this module composes
-- 'JustCI.Fanout' with @just --dump@ → root resolution → process-compose
-- YAML rendering.
module JustCI.Pipeline
  ( -- * Runtime artifact layout
    RunDir (..),
    resolveRunDir,
    ensureRunDir,

    -- * Run modes
    runLocal,
    runStrict,
    runGraph,
    runDumpYaml,
    runProtect,
    runPcPassthrough,

    -- * Pipeline assembly
    RunMode (..),
    BuildGraphError,
    buildNodeGraph,
    buildProcessCompose,
  )
where

import qualified Algebra.Graph.AdjacencyMap as G
import Control.Concurrent.Async (link, wait, withAsync)
import Control.Exception (catch, throwIO)
import Control.Monad (unless, void)
import qualified Data.ByteString as BS
import Data.Foldable (for_)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text.Display (Display (..), display)
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Y
import GHC.IO.Handle.Lock (LockMode (..), hTryLock)
import JustCI.CLI (PcVerb, ProtectOpts (..), RunOpts (..), defaultCacheTtlHours, pcVerbArg)
import JustCI.CommitStatus (contextForNode, isBodyBearing, isRequiredCheck, newTimings, postStatusFor)
import JustCI.Fanout (applySelectors, fanOut, isRemote, pipelinePlatformsFor, rootOsFamilies)
import JustCI.Gh (setRequiredChecks, viewDefaultBranch, viewRepo)
import JustCI.Git (Sha, ensureCleanTree, resolveSha, shaPlaceholder, withSnapshotWorktree)
import JustCI.Graph (lowerToRunnerGraph, reachableSubgraph)
import JustCI.Hosts (Hosts, loadHosts, lookupHost, mergeHostOverrides)
import JustCI.Justfile (Recipe, RecipeName, fetchDump, recipeCommand)
import qualified JustCI.Justfile as J
import JustCI.LogPath (logDirFor, logPathFor, platformDir)
import JustCI.Node (DagSelection (..), NodeId (..), defaultDagSelection, nodePlatform, parseNodeId, toMermaid)
import JustCI.Platform (Platform, localPlatform)
import JustCI.ProcessCompose (ProcessCompose, UpInvocation (..), processGraph, processNames, runProcessCompose, runProcessComposeClient, toProcessCompose)
import JustCI.ProcessCompose.Events (ProcessState (..), subscribeStates)
import JustCI.Root (findRoot)
import JustCI.Transport (sshRecipeCommand, sshSetupCommand)
import JustCI.Verdict (exitWithVerdict, newOutcomes, recordOutcome)
import System.Directory (createDirectoryIfMissing, doesPathExist, getCurrentDirectory, removeFile)
import System.Exit (ExitCode, die)
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode (..), withFile)
import System.IO.Error (isDoesNotExistError)

-- | The runtime artifact paths under @\$PWD\/.ci\/@. Built once at the top
-- of a run so 'runLocal' and 'runStrict' both reference the same
-- convention instead of hand-rolling @runDir \<\/\> "pc.log"@ at each
-- call site.
data RunDir = RunDir
  { worktreePath :: FilePath,
    sock :: FilePath,
    pcLog :: FilePath,
    pcYaml :: FilePath,
    lock :: FilePath
  }

-- | Compute the canonical @\$PWD\/.ci\/@ sub-paths without touching the
-- filesystem. Used by read-only client subcommands ('runPcPassthrough'
-- against a live socket) that must not silently create @.ci\/@ on disk
-- in checkouts that have never run a pipeline. 'ensureRunDir' is the
-- write-mode wrapper that calls this then creates the directory.
--
-- Single source of truth for the artifact layout: future changes to the
-- @.ci\/@ shape (e.g. moving to @\$XDG_RUNTIME_DIR\/justci\/@) edit only
-- this function — 'ensureRunDir' inherits via composition.
resolveRunDir :: IO RunDir
resolveRunDir = do
  cwd <- getCurrentDirectory
  let dir = cwd </> ".ci"
  pure
    RunDir
      { worktreePath = dir </> "worktree",
        sock = dir </> "pc.sock",
        pcLog = dir </> "pc.log",
        pcYaml = dir </> "pc.yaml",
        lock = dir </> "lock"
      }

-- | Create @\$PWD\/.ci\/@ (if missing) and return the canonical sub-paths.
-- Everything we write at runtime lives here so the user gitignores
-- @\/.ci\/@ once and forgets about it. Composes 'resolveRunDir' (paths)
-- with 'createDirectoryIfMissing' (creation) so the layout is not
-- duplicated across the read-only and write modes.
ensureRunDir :: IO RunDir
ensureRunDir = do
  dirs <- resolveRunDir
  createDirectoryIfMissing True (takeDirectory dirs.sock)
  pure dirs

-- | Take an exclusive kernel file lock on @lockPath@, unlink any stale
-- @sockFile@ left behind by a crashed prior run, and run @action@. The
-- lock is released when the file handle closes (clean exit, signal,
-- @kill -9@, segfault — all covered by 'withFile'+kernel-flock
-- semantics). Refuses fast (via 'die') if another justci already holds
-- the lock in this checkout.
--
-- Lock and unlink fused into one bracket so the "cleanup happens under
-- the lock" invariant lives in the signature, not in a call-site
-- comment. Splitting them — even as two helpers called back-to-back in
-- every run mode — drifts: a future @runFoo@ that adopts the lock and
-- forgets the companion unlink silently reintroduces juspay\/justci#10.
--
-- Why a kernel lock and not "probe the UDS to see if pc is alive": the
-- probe-then-act dance is racy (TOCTOU between liveness check and bind,
-- two concurrent justcis both seeing "stale" and deleting each other's
-- live socket). 'hTryLock' is atomic at the syscall layer — no window,
-- no manual cleanup. Ships with @base@ via "GHC.IO.Handle.Lock", no new
-- dep.
--
-- The unlink is safe because holding the lock means no live pc owns
-- @sockFile@: any prior owner released the kernel lock by dying, so
-- whatever file sits on disk is unconditionally stale. Without the
-- unlink, pc's bind would fail with @address already in use@ and the
-- new orchestrator would silently attach to the surviving old pc.
--
-- See juspay\/justci#10 for the full design and the prior always-UDS
-- attempt that this replaces.
withCiLock :: FilePath -> FilePath -> IO a -> IO a
withCiLock lockPath sockFile action =
  withFile lockPath ReadWriteMode $ \h -> do
    acquired <- hTryLock h ExclusiveLock
    if acquired
      then do
        removeFile sockFile `catch` \e ->
          unless (isDoesNotExistError e) (throwIO e)
        action
      else die $ "another justci run is in progress (lock held on " <> lockPath <> ")"

-- | Local mode: live working tree, no GitHub status posts, no per-recipe
-- log routing. The observer still runs — its only consumer is the
-- verdict accumulator, which gives developer runs the same end-of-run
-- summary strict mode produces. Process-compose's log goes to
-- @.ci\/pc.log@ so even local runs don't leak into @\$TMPDIR@; the same
-- UDS at @.ci\/pc.sock@ is bound so the API surface is available for
-- future consumers (e.g. an MCP server).
--
-- SSH lanes are supported in local mode too: any non-local platform
-- in the pipeline requires a @~\/.config\/justci\/hosts.json@ entry (the
-- user opts in by editing the file; missing entries are excluded
-- from the fanout by 'pipelinePlatformsFor'). Each remote lane gets
-- an SSH-shaped @command@ that bundles @HEAD@ across rather than
-- the dirty live tree — the dev's uncommitted work is intentionally
-- invisible to remote lanes; the bundle reflects committed history
-- only.
runLocal :: RunOpts -> [String] -> RunDir -> IO ()
runLocal opts passthrough dirs = withCiLock dirs.lock dirs.sock $ do
  hosts <- mergeHostOverrides opts.hostOverrides <$> (dieOnLeft =<< loadHosts)
  (pc, recipes) <- dieOnLeft =<< buildProcessCompose hosts opts.dagSelection LocalRun opts.cacheTtlHours
  outcomes <- newOutcomes (filter (isBodyBearing recipes) (processNames pc))
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
-- 'JustCI.ProcessCompose.Events.psToTerminalStatus' as the underlying
-- terminal-state classifier, so the GH check page and the local
-- verdict agree on which nodes succeeded.
--
-- Process-compose's own exit code is intentionally ignored — with
-- @restart: no@ on every process it no longer reflects pipeline
-- outcome (a failed node leaves pc exiting 0). The accumulated
-- outcome map is the source of truth; 'exitWithVerdict' derives the
-- final 'ExitCode' from it.
runStrict :: RunOpts -> [String] -> RunDir -> IO ()
runStrict opts passthrough dirs = withCiLock dirs.lock dirs.sock $ do
  dieOnLeft =<< ensureCleanTree
  repo <- dieOnLeft =<< viewRepo
  sha <- dieOnLeft =<< resolveSha
  hosts <- mergeHostOverrides opts.hostOverrides <$> (dieOnLeft =<< loadHosts)
  let logDir = logDirFor sha
  withSnapshotWorktree dirs.worktreePath $ do
    (pc, recipes) <- dieOnLeft =<< buildProcessCompose hosts opts.dagSelection (StrictRun dirs.worktreePath logDir) opts.cacheTtlHours
    let nodes = processNames pc
    createPlatformDirs logDir nodes
    outcomes <- newOutcomes (filter (isBodyBearing recipes) nodes)
    timings <- newTimings
    let onState ps = withParsedNode ps $ \node ->
          postStatusFor timings repo sha logDir recipes node ps
            >> recordOutcome outcomes node ps
    withObserver dirs.sock onState $
      void $
        runProcessCompose (UpInvocation dirs.sock dirs.pcLog dirs.pcYaml opts.tui passthrough) pc
    exitWithVerdict (hostFor hosts) outcomes

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
-- Read-only: 'justci graph' must not create @.ci\/@ as a side effect.
-- That's why this takes no 'RunDir' — only 'runLocal' and
-- 'runStrict' need the runtime-artifact paths.
runGraph :: IO ()
runGraph = do
  hosts <- dieOnLeft =<< loadHosts
  -- DumpRun emits structure-only output; the TTL is a body field on
  -- the rendered SSH command, irrelevant to graph topology. Pass the
  -- shared default so the embedded number matches what the real run
  -- would use by default (operators override per-invocation).
  (pc, _) <- dieOnLeft =<< buildProcessCompose hosts defaultDagSelection DumpRun defaultCacheTtlHours
  TIO.putStrLn (toMermaid (processGraph pc))

-- | Emit the assembled process-compose YAML to stdout. Uses 'DumpRun'
-- mode so no host resolution side-effects occur — safe to invoke
-- offline, outside a git checkout, or from a remote whose
-- @hosts.json@ has no entry for the other platform.
runDumpYaml :: IO ()
runDumpYaml = do
  hosts <- dieOnLeft =<< loadHosts
  -- See 'runGraph' for why the default is fine here: the dumped YAML
  -- embeds the TTL number for inspection; the actual run uses
  -- whatever @--cache-ttl-hours@ resolves to.
  (pc, _) <- dieOnLeft =<< buildProcessCompose hosts defaultDagSelection DumpRun defaultCacheTtlHours
  BS.putStr (Y.encode pc)

-- | Branch-protection helper: read the canonical DAG, extract the
-- @(recipe, platform)@ context for every user-facing node, and PATCH
-- the GitHub branch protection's required-status-checks list to
-- exactly that set. The filter is 'isRequiredCheck' — body-bearing
-- recipes only. Setup nodes are excluded for liveness (local-only
-- runs never schedule them), and pure-aggregator recipes are
-- excluded because their state is fully derivative of their
-- leaves. Note this is /narrower/ than 'shouldPostStatus', which
-- includes setup nodes so setup failures surface on the PR — only
-- some posted statuses are merge-blocking.
--
-- For cascade-skipped recipes (whose required-check row is supplied
-- by GitHub's "Expected — Waiting for status to be reported"
-- placeholder once branch protection is configured), this list is
-- the only thing keeping the PR un-mergeable: 'postStatusFor' no
-- longer posts a parallel @Pending@+@"Skipped"@ row. The placeholder
-- is the canonical encapsulation of "required but unreported."
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
-- the GitHub UI before @justci protect@ is meaningful.
runProtect :: ProtectOpts -> IO ()
runProtect opts = do
  hosts <- dieOnLeft =<< loadHosts
  (nodeGraph, _, recipes) <- dieOnLeft =<< buildNodeGraph hosts defaultDagSelection
  let contexts = contextForNode <$> filter (isRequiredCheck recipes) (G.vertexList nodeGraph)
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

-- | Dispatch a live-introspection subcommand (@justci status@ / @logs@ /
-- @monitor@) against the currently-running pipeline. Shells out to the
-- baked 'JustCI.ProcessCompose.processComposeBin' so the client version
-- pins to whatever justci itself was built with — agents that pinned a
-- tag of @juspay/justci@ get exactly that pc client talking to exactly
-- that pc server, no nixpkgs-drift skew.
--
-- Takes the socket path directly (not a 'RunDir') because the
-- read-only client side has no business with the rest of the run-dir
-- bundle (log file, yaml file, lock file). The caller in @app/Main.hs@
-- resolves the path via 'resolveRunDir' and passes 'dirs.sock' —
-- importantly, *without* 'ensureRunDir', so @justci status@ in a fresh
-- checkout doesn't leave behind an empty @.ci\/@ directory.
--
-- The 'doesPathExist' check is a courtesy: it converts the absent-socket
-- case into a clear "no run in progress" message instead of pc's own
-- @"connection refused"@. A stale socket (file present, pc dead) still
-- falls through to pc, which reports its own connect failure — same
-- failure shape as @ci@'s state-event observer hits in that scenario, so
-- the error vocabulary is already consistent.
runPcPassthrough :: PcVerb -> [String] -> FilePath -> IO ExitCode
runPcPassthrough verb args sock = do
  alive <- doesPathExist sock
  unless alive $
    die $
      "no justci run in progress in this checkout (no socket at " <> sock <> ")"
  runProcessComposeClient sock (pcVerbArg verb) args

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
    --       Missing 'JustCI.Hosts.Host' entries are tolerated; SSH-lane
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
-- the way 'JustCI.CommitStatus' and 'JustCI.Verdict' do for their user-facing
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
      <> " or add an entry to ~/.config/justci/hosts.json for one of: "
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
--  * Host resolution loads @~\/.config\/justci\/hosts.json@ once.
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
-- Returns the filtered fanned-out graph, the local platform, and the
-- full justfile recipe map (keyed by 'RecipeName'). The platform is
-- needed downstream for transport selection ('commandForNode'); the
-- recipe map is needed by 'JustCI.CommitStatus.shouldPostStatus' and
-- 'JustCI.CommitStatus.isRequiredCheck' so the GH-status and
-- branch-protection filters can drop pure aggregators.
-- Both are computed here anyway as part of fanout — exposing them
-- avoids shelling out to @just --dump@ a second time and avoids the
-- dormant divergence risk of two parses going out of sync mid-run.
buildNodeGraph :: Hosts -> DagSelection -> IO (Either BuildGraphError (G.AdjacencyMap NodeId, Platform, Map.Map RecipeName Recipe))
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
          pure (Right (nodeGraph, localPlat, recipes))

-- | Build the full 'ProcessCompose' YAML: extends 'buildNodeGraph'
-- with SHA resolution, transport command rendering, and the
-- per-mode YAML projections ('yamlPathsFor'). Used by every
-- subcommand that actually drives process-compose ('runLocal',
-- 'runStrict') or emits its YAML/graph form ('runDumpYaml',
-- 'runGraph').
--
-- Each fanned-out 'NodeId' gets a local or @ssh host@ command
-- depending on whether its platform matches the runner's; the
-- 'JustCI.Transport' builders are the only site that know SSH command
-- shapes.
buildProcessCompose :: Hosts -> DagSelection -> RunMode -> Int -> IO (Either BuildGraphError (ProcessCompose, Map.Map RecipeName Recipe))
buildProcessCompose hosts sel mode cacheTtlHours = do
  result <- buildNodeGraph hosts sel
  case result of
    Left err -> pure (Left err)
    Right (nodeGraph, localPlat, recipes) -> do
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
      let mkCommand = commandForNode sha localPlat hosts cacheTtlHours
          (yamlWorkingDir, yamlLogLocation) = yamlPathsFor mode
      pure (Right (toProcessCompose mkCommand yamlWorkingDir yamlLogLocation nodeGraph, recipes))

-- | Per-node command construction. Dispatches over (host lookup,
-- node kind) and picks one of the three valid command builders in
-- 'JustCI.Transport'. The "(Local, SetupNode)" combination is
-- structurally absent — there is no @localSetupCommand@ — so the
-- match is total over the cases the fanout actually produces:
--
--   * @(RecipeNode, no host, local platform)@ → 'JustCI.Justfile.recipeCommand'
--   * @(RecipeNode, host)@                    → 'sshRecipeCommand'
--   * @(SetupNode, host)@                     → 'sshSetupCommand'
--
-- @sha@ is consumed only by the SSH builders. Local-mode runs
-- without remote lanes pass 'shaPlaceholder' as a no-op (never
-- read — the graph has no SSH nodes).
commandForNode :: Sha -> Platform -> Hosts -> Int -> NodeId -> T.Text
commandForNode sha localPlat hosts cacheTtlHours node = case (node, lookupHost plat hosts) of
  (RecipeNode r _, Nothing)
    | plat == localPlat -> recipeCommand r
    | otherwise -> hostContractError
  (RecipeNode r _, Just h) -> sshRecipeCommand h sha plat r
  (SetupNode _, Just h) -> sshSetupCommand h sha plat cacheTtlHours
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
-- 'JustCI.Verdict' still receives an opaque @NodeId -> Text@ resolver,
-- so its independence from the 'JustCI.Hosts' vocabulary is preserved
-- without the cost of a second 'loadHosts' call.
hostFor :: Hosts -> NodeId -> T.Text
hostFor hosts n = case lookupHost (nodePlatform n) hosts of
  Just h -> display h
  Nothing -> "local"

-- | The canonical exit point for recoverable failures: every @Either@-typed
-- failure mode threads up to this boundary, where the structured error's
-- 'Display' rendering becomes the exit message. Direct 'die' calls elsewhere
-- are reserved for invariant violations (internal errors) or non-recoverable
-- mutex failures ('withCiLock') where an @Either@ return would be meaningless.
--
-- Shape note: takes @Either e a@ rather than @IO (Either e a)@ so the
-- same helper works for both pure Eithers (@dieOnLeft $ findRoot
-- recipes@) and IO ones (@dieOnLeft =<< ensureCleanTree@). A helper
-- typed to @IO (Either e a) -> IO a@ would force every pure call site
-- to add a @pure@.
dieOnLeft :: Display e => Either e a -> IO a
dieOnLeft = either (die . T.unpack . display) pure
