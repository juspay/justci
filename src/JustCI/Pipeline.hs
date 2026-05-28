{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Orchestration for the per-subcommand entry points
-- ('runPipeline', 'runGraph', 'runDumpYaml', 'runProtect')
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
    runPipeline,
    runGraph,
    runDumpYaml,
    runProtect,
    runPcPassthrough,

    -- * Pipeline assembly
    RunMode (..),
    RunPolicy (..),
    SnapshotPolicy (..),
    PolicyShape (..),
    policyShape,
    resolveRunPolicy,
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
import JustCI.CLI (PcVerb, ProtectOpts (..), RunOpts (..), pcVerbArg)
import JustCI.CommitStatus (contextForNode, isBodyBearing, isRequiredCheck, newTimings, postStatusFor)
import JustCI.Fanout (EmptyFanoutCause (..), applySelectors, fanOut, isRemote, pipelinePlatformsFor, rootOsFamilies)
import JustCI.Gh (Repo, setRequiredChecks, viewDefaultBranch, viewRepo)
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
import JustCI.Transport (defaultCacheTtlHours, sshRecipeCommand, sshSetupCommand)
import JustCI.Verdict (Outcomes, exitWithVerdict, newOutcomes, recordOutcome)
import System.Directory (createDirectoryIfMissing, doesPathExist, getCurrentDirectory, removeFile)
import System.Exit (ExitCode, die)
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode (..), withFile)
import System.IO.Error (isDoesNotExistError)

-- | The runtime artifact paths under @\$PWD\/.ci\/@. Built once at the top
-- of a run so every subcommand references the same convention
-- instead of hand-rolling @runDir \<\/\> "pc.log"@ at each call site.
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

-- | Resolved policy for one pipeline run. A three-constructor sum
-- over the valid policy states — the structurally incoherent
-- combination "post enabled without a snapshot" is unrepresentable
-- by construction (rather than ruled out at the value layer by a
-- separate enforcement step).
--
--   * 'NoSnapshot' — live working tree, no GH posts. The dev arm:
--     @--no-strict@ or @--no-snapshot@. No pre-flight IO incurred.
--   * 'SnapshotOnly' — clean-tree refuse + HEAD @git worktree@ pin
--     + SHA-keyed log routing, but /no/ GH commit-status posts.
--     The @--no-post@ arm: for non-github strict consumers and for
--     debugging strict runs without writing to the PR's checks
--     list.
--   * 'SnapshotAndPost' — same plus GH posts. Default of
--     @nix run . -- run@.
--
-- 'SnapshotPolicy' carries the artefacts both snapshot arms need
-- (repo, SHA, worktree dir, log dir); the post-vs-no-post decision
-- lives in the constructor name, not as a field inside the
-- artefacts record. Keeps 'SnapshotPolicy' purely descriptive.
data RunPolicy
  = NoSnapshot
  | SnapshotOnly SnapshotPolicy
  | SnapshotAndPost SnapshotPolicy

-- | The artefacts a snapshotted run needs, bundled at one populate
-- site ('resolveRunPolicy'). 'snapRepo' + 'snapSha' feed
-- 'postStatusFor' on the 'SnapshotAndPost' arm (the SHA is also
-- threaded into the SSH bundle for remote lanes via
-- 'JustCI.Transport'); 'snapWorktreeDir' is the @git worktree@ root
-- every local recipe @chdir@s into; 'snapLogDir' is the SHA-keyed
-- log root the YAML emitter routes per-recipe stdout/stderr to.
--
-- Holds no policy flags — the post-vs-no-post choice rides on the
-- 'RunPolicy' constructor instead, so this record stays a pure
-- "facts about the snapshot the resolver discovered" carrier.
data SnapshotPolicy = SnapshotPolicy
  { snapRepo :: Repo,
    snapSha :: Sha,
    snapWorktreeDir :: FilePath,
    snapLogDir :: FilePath
  }

-- | Pure projection of the strict-vs-dev flag triple
-- (@--no-strict@, @--no-snapshot@, @--no-post@) onto the three
-- valid resolved-policy states. A sum with one constructor per
-- valid state — rather than a 'Bool'-pair record — so the
-- "@post@ without snapshot" combination is unrepresentable at
-- the type level rather than enforced at the value layer.
--
--   * 'DevMode' — live working tree, no GH posts. The opt-out
--     arm: @--no-strict@ (the shortcut) or @--no-snapshot@.
--   * 'StrictNoPost' — snapshot engaged (clean-tree refuse + HEAD
--     pin) but GH posts skipped. The @--no-post@ arm.
--   * 'FullStrict' — snapshot engaged + GH posts. The default of
--     @nix run . -- run@ (no opt-out flags).
--
-- Flag interactions fold into the resolution at one site
-- ('policyShape') rather than at the CLI parser layer — every
-- 'RunOpts' value lands on exactly one constructor:
--
--   * @--no-strict@ is the dev-mode shortcut: equivalent to passing
--     both @--no-snapshot@ and @--no-post@.
--   * @--no-snapshot@ subsumes @--no-post@ — a SHA-tagged status
--     posted against bytes that aren't @HEAD@ violates the
--     "SHA matches tested bytes" invariant, so any @--no-snapshot@
--     run lands on 'DevMode' regardless of @--no-post@'s value.
--   * @--no-post@ alone keeps snapshot engaged: for non-github
--     strict consumers and for debugging strict runs without
--     writing to the PR's checks list.
--
-- Separated from 'resolveRunPolicy' so the boolean-folding rules
-- are unit-testable without faking @gh@\/@git@ subprocesses.
data PolicyShape
  = DevMode
  | StrictNoPost
  | FullStrict
  deriving stock (Eq, Show)

-- | Reduce the user's three opt-out flags to a 'PolicyShape'. See
-- the 'PolicyShape' haddock for the resolution rules and the
-- mapping from constructor → strict-mode side effects.
policyShape :: RunOpts -> PolicyShape
policyShape opts
  | opts.noStrict || opts.noSnapshot = DevMode
  | opts.noPost = StrictNoPost
  | otherwise = FullStrict

-- | Resolve the user's flags into a 'RunPolicy', performing every
-- fail-fast check before returning.
--
-- Dirty-tree refuse, @gh repo view@ (for the repo lookup), and
-- @git rev-parse HEAD@ (for the SHA) all run here — /before/
-- 'withCiLock' takes the lock, /before/ @process-compose@ starts,
-- /before/ any pipeline machinery boots. The contract is "failure
-- before CI even starts": a misconfigured environment (dirty tree,
-- no @gh@ auth, no github remote) halts at the front door, not
-- mid-run.
--
-- The flag-to-decision projection lives in 'policyShape' (pure,
-- unit-tested). This resolver only owns the IO side: given the
-- shape's verdict, fetch the artefacts the snapshot arm needs.
resolveRunPolicy :: RunOpts -> RunDir -> IO RunPolicy
resolveRunPolicy opts dirs = case policyShape opts of
  DevMode -> pure NoSnapshot
  StrictNoPost -> snapshotted SnapshotOnly
  FullStrict -> snapshotted SnapshotAndPost
  where
    snapshotted ctor = do
      dieOnLeft =<< ensureCleanTree
      repo <- dieOnLeft =<< viewRepo
      sha <- dieOnLeft =<< resolveSha
      pure . ctor $
        SnapshotPolicy
          { snapRepo = repo,
            snapSha = sha,
            snapWorktreeDir = dirs.worktreePath,
            snapLogDir = logDirFor sha
          }

-- | The single entry point for @justci run@. Resolves the user's
-- opt-out flags into a 'RunPolicy' via 'resolveRunPolicy' (which
-- performs every fail-fast check before returning), takes the
-- per-checkout lock, then drives the pipeline against the policy
-- the resolver returned.
--
-- The two axes the old @runLocal@\/@runStrict@ split bundled now
-- compose explicitly here:
--
--   * Snapshot ('RunPolicy.snapshot') controls whether the
--     'withSnapshotWorktree' bracket wraps the run, which 'RunMode'
--     the YAML emitter sees ('PinnedRun' vs 'LiveRun'), and whether
--     'createPlatformDirs' materialises the SHA-keyed log
--     directories.
--   * Post ('RunPolicy.post') controls whether the @onState@
--     callback fans into 'postStatusFor' alongside 'recordOutcome',
--     or just 'recordOutcome' on its own. The local verdict
--     accumulator runs unconditionally so every mode produces the
--     same @── ci run summary ──@ tail.
--
-- Process-compose's own exit code is intentionally ignored — with
-- @restart: no@ on every process it no longer reflects pipeline
-- outcome (a failed node leaves pc exiting 0). The accumulated
-- outcome map is the source of truth; 'exitWithVerdict' derives
-- the final 'ExitCode' from it.
--
-- SSH lanes are supported in every mode. Each non-local platform
-- in the pipeline needs a @~\/.config\/justci\/hosts.json@ entry
-- (the user opts in by editing the file; missing entries are
-- excluded from the fanout by 'pipelinePlatformsFor'). Remote lanes
-- always run against a @git bundle@ of @HEAD@ — uncommitted work
-- is intentionally invisible to remote recipes, regardless of
-- @--no-snapshot@.
runPipeline :: RunOpts -> [String] -> RunDir -> IO ()
runPipeline opts passthrough dirs = do
  policy <- resolveRunPolicy opts dirs
  withCiLock dirs.lock dirs.sock $ do
    hosts <- mergeHostOverrides opts.hostOverrides <$> (dieOnLeft =<< loadHosts)
    let (mode, withMaybeSnapshot, mLogDir) = case snapshotOf policy of
          Just s -> (PinnedRun s.snapWorktreeDir s.snapLogDir, withSnapshotWorktree s.snapWorktreeDir, Just s.snapLogDir)
          Nothing -> (LiveRun, id, Nothing)
    withMaybeSnapshot $ do
      (pc, recipes) <- dieOnLeft =<< buildProcessCompose hosts opts.dagSelection mode opts.cacheTtlHours
      let nodes = processNames pc
      for_ mLogDir $ \ld -> createPlatformDirs ld nodes
      outcomes <- newOutcomes (filter (isBodyBearing recipes) nodes)
      onState <- buildOnState policy recipes outcomes
      withObserver dirs.sock onState $
        void $
          runProcessCompose (UpInvocation dirs.sock dirs.pcLog dirs.pcYaml opts.tui passthrough) pc
      exitWithVerdict (hostFor hosts) outcomes

-- | Project the 'RunPolicy' sum to its inner 'SnapshotPolicy', if
-- any. Used by 'runPipeline' to share the (worktree, log dir, mode)
-- choices across both snapshot arms ('SnapshotOnly' and
-- 'SnapshotAndPost'): they make identical decisions about
-- @withSnapshotWorktree@, the YAML mode, and the platform-dir
-- materialisation; only the @onState@ fan differs.
snapshotOf :: RunPolicy -> Maybe SnapshotPolicy
snapshotOf NoSnapshot = Nothing
snapshotOf (SnapshotOnly s) = Just s
snapshotOf (SnapshotAndPost s) = Just s

-- | Build the @onState@ callback the observer subscribes to. The
-- local outcome accumulator runs unconditionally; the GH-status
-- poster is composed on top only on the 'SnapshotAndPost' arm —
-- pattern matching on the 'RunPolicy' sum makes the three-state
-- dispatch flat (no nested @Maybe@-unwrap + @Bool@-test).
buildOnState ::
  RunPolicy ->
  Map.Map RecipeName Recipe ->
  -- | Local outcome accumulator (from 'newOutcomes').
  Outcomes ->
  IO (ProcessState -> IO ())
buildOnState policy recipes outcomes = case policy of
  SnapshotAndPost s -> do
    timings <- newTimings
    pure $ \ps -> withParsedNode ps $ \node ->
      postStatusFor timings s.snapRepo s.snapSha s.snapLogDir recipes node ps
        >> recordOutcome outcomes node ps
  _ ->
    pure $ \ps -> withParsedNode ps $ \node -> recordOutcome outcomes node ps

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
-- That's why this takes no 'RunDir' — only 'runPipeline' needs
-- the runtime-artifact paths.
runGraph :: IO ()
runGraph = do
  hosts <- dieOnLeft =<< loadHosts
  -- DumpRun emits structure-only output; the TTL is a body field on
  -- the rendered SSH command, irrelevant to graph topology. Pass the
  -- shared default so the embedded number matches what the real run
  -- would use by default (operators override per-invocation).
  -- 'defaultDagSelection' carries an empty 'platformFilter', so the
  -- rendered graph reflects the full canonical fanout regardless of
  -- the @--platform@ knob on @run@.
  (pc, _) <- dieOnLeft =<< buildProcessCompose hosts defaultDagSelection DumpRun defaultCacheTtlHours
  TIO.putStrLn (toMermaid (processGraph pc))

-- | Emit the assembled process-compose YAML to stdout. Uses 'DumpRun'
-- mode so no host resolution side-effects occur — safe to invoke
-- offline, outside a git checkout, or from a remote whose
-- @hosts.json@ has no entry for the other platform.
runDumpYaml :: IO ()
runDumpYaml = do
  hosts <- dieOnLeft =<< loadHosts
  -- See 'runGraph' for why the defaults are fine here: the dumped
  -- YAML embeds the TTL number for inspection; the actual run uses
  -- whatever @--cache-ttl-hours@ resolves to. 'defaultDagSelection'
  -- pins @platformFilter = []@ so the YAML reflects the canonical
  -- fanout regardless of the @--platform@ knob on @run@.
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
-- async-lifecycle scaffold lives here so 'runPipeline' can vary
-- only in the @onState@ callback 'buildOnState' returns and the
-- body it passes.
withObserver :: FilePath -> (ProcessState -> IO ()) -> IO a -> IO a
withObserver sockP onState body =
  withAsync (subscribeStates sockP onState) $ \obs -> do
    link obs
    result <- body
    wait obs
    pure result

-- | The three YAML-emission shapes 'buildProcessCompose' produces.
-- The constructors name the YAML-path axis they actually
-- encapsulate — whether per-node @working_dir@ / log-file paths
-- get pinned into the emitted YAML — and not the user-visible
-- run-mode they originally tracked:
--
--   * 'LiveRun' — no path pinning: recipes run in the inherited cwd,
--     logs go to process-compose's default. Used by 'runPipeline'
--     when 'RunPolicy.snapshot' is 'Nothing'.
--   * 'PinnedRun' — both paths injected. Local recipes @chdir@ into
--     the @git worktree@ root; per-recipe stdout/stderr lands under
--     @.ci\/\<sha\>\/\<platform\>\/\<recipe\>.log@. Used by
--     'runPipeline' under a 'SnapshotPolicy'.
--   * 'DumpRun' — no path pinning /and/ no host-resolution side
--     effects. Missing 'JustCI.Hosts.Host' entries are tolerated;
--     SSH-lane commands render with a placeholder so the structural
--     keys (process names, depends_on edges) still reflect the real
--     fanout. Used by 'runDumpYaml' \/ 'runGraph' \/ 'runProtect'
--     so they work offline, outside a git checkout, and on the
--     macos remote's smoke test where stdin is closed and prompting
--     would deadlock.
--
-- A sum type instead of two parallel @Maybe FilePath@s rules out
-- the mixed @(Just, Nothing)@ \/ @(Nothing, Just)@ states that
-- produce logically inconsistent YAML, and gives 'DumpRun' a slot
-- distinct from 'LiveRun' even though both project to the same
-- @yamlPathsFor@ result — they differ in the host-resolution +
-- SHA-placeholder behaviour 'buildProcessCompose' branches on.
data RunMode
  = LiveRun
  | -- | @PinnedRun worktreeDir logDir@.
    PinnedRun FilePath FilePath
  | DumpRun

-- | The two YAML-shape projections of 'RunMode': the per-node working
-- directory every recipe @chdir@s into, and the per-node log location
-- the YAML emitter routes stdout/stderr to. Both vary together across
-- modes — 'PinnedRun' supplies both; 'LiveRun' and 'DumpRun' supply
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
yamlPathsFor (PinnedRun wt ld) = (workingDirFor wt, Just . logPathFor ld)
  where
    workingDirFor _ (SetupNode _) = Nothing
    workingDirFor w (RecipeNode _ _) = Just w
yamlPathsFor LiveRun = (const Nothing, const Nothing)
yamlPathsFor DumpRun = (const Nothing, const Nothing)

-- | A user-recoverable failure during graph construction. Surfaced
-- through @Either@ rather than 'die' so callers ('runPipeline',
-- 'runGraph', 'runDumpYaml', 'runProtect') own the die-vs-respond
-- boundary at one place. Other failures inside
-- 'buildNodeGraph' (justfile parse, recipe ordering cycles, reachability
-- on a missing recipe, local-system classification) already flow
-- through their own 'Either' types and are funneled here via
-- 'dieOnLeft' at the same boundary.
data BuildGraphError
  = -- | @--root \<r\>@ named a recipe that isn't in the justfile.
    --     Carries the bad name so the display rendering can echo it
    --     back to the user.
    BadRoot RecipeName
  | -- | The platform fanout for this root collapsed to the empty
    --     set. Carries the root name, the root's declared OS
    --     families, the structural reason from 'pipelinePlatformsFor'
    --     ('EmptyNaturalFanout' vs 'FilterExcludedAll'), and the
    --     user's @--platform@ list — the last just to echo the
    --     offending tokens back in the 'FilterExcludedAll' message.
    --     The 'Display' instance pattern-matches on the cause, not on
    --     the shape of the filter list, so the attribution can't drift
    --     when both the natural fanout and the filter happen to be
    --     empty.
    EmptyFanout RecipeName [J.Os] EmptyFanoutCause [Platform]
  deriving stock (Show)

instance Display BuildGraphError where
  displayBuilder (BadRoot r) =
    "--root " <> displayBuilder r <> " is not a recipe in the justfile"
  displayBuilder (EmptyFanout rootName oss EmptyNaturalFanout _) =
    "root recipe declares OS attrs but no matching system is configured. "
      <> "Either remove the OS attrs from "
      <> displayBuilder rootName
      <> " or add an entry to ~/.config/justci/hosts.json for one of: "
      <> displayBuilder (T.pack (unwords (show <$> oss)))
  displayBuilder (EmptyFanout rootName oss FilterExcludedAll filt) =
    "--platform "
      <> displayBuilder (T.intercalate ", " $ display <$> filt)
      <> " excluded every platform for "
      <> displayBuilder rootName
      <> case oss of
        [] -> ". That recipe has no OS-family attributes — it runs on the local platform only."
        _ ->
          "'s OS attrs ("
            <> displayBuilder (T.pack $ unwords $ show <$> oss)
            <> "). Drop the override or pick one that matches."

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
      case pipelinePlatformsFor sel.platformFilter rootRecipe localPlat hosts of
        Left cause -> pure (Left (EmptyFanout rootName (rootOsFamilies rootRecipe) cause sel.platformFilter))
        Right pipelinePlatforms -> do
          let unfilteredNodeGraph = fanOut localPlat hosts pipelinePlatforms recipeGraph
          nodeGraph <- dieOnLeft $ applySelectors sel.selectorMode pipelinePlatforms unfilteredNodeGraph
          pure (Right (nodeGraph, localPlat, recipes))

-- | Build the full 'ProcessCompose' YAML: extends 'buildNodeGraph'
-- with SHA resolution, transport command rendering, and the
-- per-mode YAML projections ('yamlPathsFor'). Used by every
-- subcommand that actually drives process-compose ('runPipeline')
-- or emits its YAML/graph form ('runDumpYaml', 'runGraph').
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
-- 'runPipeline' / 'runGraph'). Nodes whose platform has
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
