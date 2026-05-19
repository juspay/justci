{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The config-and-invocation half of the process-compose interface: the
-- YAML schema we emit, the @up@-invocation argv, and the spawn-and-wait
-- operation. The event-stream half (WS-over-UDS subscription, typed
-- @ProcessState@/@ProcessStatus@) lives in "CI.ProcessCompose.Events" —
-- those wire vocabularies change on a different axis from the YAML
-- config.
module CI.ProcessCompose
  ( -- * Output schema (YAML config)
    ProcessCompose,
    toProcessCompose,
    processNames,
    processGraph,

    -- * MCP server config
    McpServerConfig (..),
    McpTransport (..),
    stdioMcp,
    withMcpServer,
    disableAllProcesses,

    -- * Invocation
    UpInvocation (..),
    runProcessCompose,
  )
where

import qualified Algebra.Graph.AdjacencyMap as G
import CI.Node (NodeId (..))
import Data.Aeson (ToJSON (..), camelTo2, defaultOptions, genericToJSON)
import Data.Aeson.Types (Options (..))
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Yaml as Y
import GHC.Generics (Generic)
import System.Exit (ExitCode)
import System.Process (proc, waitForProcess, withCreateProcess)
import System.Which (staticWhich)

-- | Absolute path to the @process-compose@ binary, baked in at compile time
-- via Nix (see @settings.ci.extraBuildTools@ in @flake.nix@). Not exported:
-- the only spawn site is 'runProcessCompose' below.
processComposeBin :: FilePath
processComposeBin = $(staticWhich "process-compose")

-- | Aeson 'Options' that translate @CamelCase@ constructor tags into
-- @snake_case@, matching process-compose's wire conventions
-- (@ExitOnFailure@ → @"exit_on_failure"@). 'tagSingleConstructors' is on
-- so single-constructor nullary sums still go through the sum encoding
-- and emit their tag as a string rather than aeson's default empty array.
snakeCaseTag :: Options
snakeCaseTag =
  defaultOptions
    { constructorTagModifier = camelTo2 '_',
      tagSingleConstructors = True
    }

-- | Top-level @process-compose.yaml@: a map from process name to spec,
-- plus an optional 'mcp_server' block that turns the spawned pc into
-- an MCP server (introspection + control over the running pipeline).
data ProcessCompose = ProcessCompose
  { processes :: Map.Map NodeId Process,
    mcp_server :: Maybe McpServerConfig
  }
  deriving stock (Generic)

-- Custom instance so 'Nothing' in 'mcp_server' drops the field
-- entirely — the default project YAML stays exactly as it was before
-- MCP support landed.
instance ToJSON ProcessCompose where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

-- | Mark every process as @disabled: true@. Used together with
-- 'withMcpServer' in @ci run --mcp@ so pc spawns, the MCP server
-- is available on stdio, and pc registers every recipe + setup
-- node in the project — but auto-starts /none/ of them. The
-- attached agent drives execution explicitly via @pc_process_start@,
-- which is the whole point of agent-mediated runs.
disableAllProcesses :: ProcessCompose -> ProcessCompose
disableAllProcesses pc =
  pc {processes = Map.map disable pc.processes}
  where
    disable p = p {disabled = Just True}

-- | Attach an 'McpServerConfig' to an already-assembled 'ProcessCompose'.
-- Returns a fresh record with @mcp_server@ populated; the caller is
-- typically @CI.Pipeline.runLocal@ / @runStrict@ when @ci run --mcp@
-- is set, doing the override after 'toProcessCompose' built the
-- baseline graph.
withMcpServer :: McpServerConfig -> ProcessCompose -> ProcessCompose
withMcpServer cfg pc = pc {mcp_server = Just cfg}

-- | Pc's MCP transport. SSE is the HTTP-based default; stdio takes
-- over pc's own stdin\/stdout (which auto-disables the TUI). Today
-- only the stdio path has a CLI hook ('CI.CLI.RunOpts.mcp' + 'stdioMcp');
-- 'Sse' is named here so the wire vocabulary is complete and adding
-- an SSE flag later is a one-line addition.
data McpTransport = Sse | Stdio
  deriving stock (Show, Eq, Generic)

instance ToJSON McpTransport where
  toJSON Sse = "sse"
  toJSON Stdio = "stdio"

-- | The @mcp_server@ YAML block. Field names match pc's keys
-- verbatim. 'host' and 'port' are SSE-only; pc tolerates their
-- presence under stdio transport and ignores them.
--
-- @expose_control_tools@ governs whether MCP clients can drive pc
-- (start\/stop\/restart processes) on top of read-only introspection.
-- 'stdioMcp' sets it to 'True' so an agent attached over stdio can
-- recover from a failed recipe by restarting it rather than re-running
-- the whole pipeline.
data McpServerConfig = McpServerConfig
  { host :: Text,
    port :: Int,
    transport :: McpTransport,
    expose_control_tools :: Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

-- | The canonical stdio-transport MCP config: @transport: stdio@
-- with @expose_control_tools: true@. @host@\/@port@ are populated
-- with placeholders (pc ignores them under stdio).
stdioMcp :: McpServerConfig
stdioMcp =
  McpServerConfig
    { host = "localhost",
      port = 0,
      transport = Stdio,
      expose_control_tools = True
    }

-- | One @processes.<name>@ entry. Field names match @process-compose@'s YAML keys.
data Process = Process
  { command :: Text,
    -- | Which group this process belongs to in pc's typed
    --     @namespace@ vocabulary. Either @"setup"@ (internal
    --     plumbing — bundle ship, drv copy) or @"recipes"@ (user
    --     work). The kind is derived structurally from the 'NodeId'
    --     sum at emission time, so the namespace label and the
    --     'NodeId' constructor agree by construction. The label is
    --     the pc-side seam that replaces the historical
    --     name-prefix sniff (@_ci-setup@) for the setup/recipe
    --     distinction at the wire layer.
    namespace :: Text,
    depends_on :: Map.Map NodeId Dependency,
    availability :: Availability,
    -- | When set, process-compose @chdir@s the spawned process into this
    --     directory before executing 'command'. Used in strict mode to pin
    --     every local recipe to an immutable @git worktree@ snapshot of
    --     HEAD. 'Nothing' omits the field from the YAML — both for dev runs
    --     (which run against the live tree) and for setup-node processes
    --     (which are @ssh -T ...@ launchers whose local cwd is ignored).
    working_dir :: Maybe FilePath,
    -- | When set, process-compose routes this process's stdout/stderr to
    --     the given file instead of the global @-L@ log. Used in strict mode
    --     to split per-recipe output into @.ci\/\<sha\>\/\<recipe\>.log@ so
    --     the GitHub commit status can embed a navigable path to the failing
    --     log. 'Nothing' falls back to the global log.
    log_location :: Maybe FilePath,
    -- | When 'Just True', process-compose registers the process in the
    --     project (so MCP introspection sees it) but does not auto-start
    --     it on @up@. An attached agent calls @pc_process_start \<name\>@
    --     to launch it on demand. 'Nothing' omits the field — pc's default
    --     auto-starts the process.
    disabled :: Maybe Bool
  }
  deriving stock (Generic)

-- Custom instance so 'Nothing' in 'working_dir' drops the field entirely
-- rather than emitting @working_dir: null@.
instance ToJSON Process where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

-- | One entry inside @depends_on@: the condition under which the named
-- dependency is considered satisfied.
data Dependency = Dependency
  { condition :: Condition
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

-- | Process-compose's set of dep-edge conditions. We only emit
-- 'ProcessCompletedSuccessfully' today; the closed sum names every value the
-- wire format admits so the choice stays type-safe at every emission site.
data Condition
  = ProcessCompletedSuccessfully
  deriving stock (Generic)

instance ToJSON Condition where
  toJSON = genericToJSON snakeCaseTag

-- | Per-process failure policy. Both knobs are set explicitly so the
-- value at every emission site shows the policy in full: 'RestartNo'
-- lets a failed recipe stay failed without tearing the project down,
-- and @exit_on_skipped = False@ keeps the same composure when a
-- downstream recipe is skipped because its dep failed. The
-- combination is what gives sibling lanes the freedom to keep running
-- after one fails — verifying the cross-lane outcome is then the job
-- of "CI.Verdict", not of process-compose's own exit code.
data Availability = Availability
  { restart :: RestartPolicy,
    exit_on_skipped :: Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

-- | Restart strategy for a process. We only emit 'No' today — "let the
-- failure stick, surface it in the verdict" — but the closed sum
-- names every value the wire format admits ('ExitOnFailure' would
-- shut the whole project down on the first failure) so the choice
-- stays type-safe at the emission site.
data RestartPolicy = No | ExitOnFailure
  deriving stock (Generic)

instance ToJSON RestartPolicy where
  toJSON = genericToJSON snakeCaseTag

-- | Assemble a @process-compose@ config from a pre-validated execution
-- graph. The caller supplies three per-vertex callbacks:
--
--   * @mkCommand@ — the shell command emitted for each vertex.
--   * @mkWorkingDir@ — the directory the process is @chdir@'d into,
--     or 'Nothing' to leave it unset. Per-node (not uniform) so
--     e.g. SSH-launcher processes can opt out of the local worktree
--     pin (their cwd is ignored once the @ssh@ tokens take over).
--   * @mkLogLocation@ — the per-process log path, or 'Nothing' to
--     fall back to the global log.
--
-- Each outgoing edge becomes a @depends_on@ entry. Keeping these
-- policy decisions out of this module lets callers vary how
-- vertices are invoked, where they execute, and where their output
-- lands without the YAML emitter knowing about any of those choices.
toProcessCompose ::
  (NodeId -> Text) ->
  (NodeId -> Maybe FilePath) ->
  (NodeId -> Maybe FilePath) ->
  G.AdjacencyMap NodeId ->
  ProcessCompose
toProcessCompose mkCommand mkWorkingDir mkLogLocation g =
  ProcessCompose
    { processes = Map.fromSet mkProcess (G.vertexSet g),
      mcp_server = Nothing
    }
  where
    mkProcess node =
      Process
        { command = mkCommand node,
          namespace = namespaceFor node,
          depends_on = Map.fromSet (const (Dependency ProcessCompletedSuccessfully)) (G.postSet node g),
          availability = Availability {restart = No, exit_on_skipped = False},
          working_dir = mkWorkingDir node,
          log_location = mkLogLocation node,
          disabled = Nothing
        }

-- | The pc namespace label for a 'NodeId'. Derived structurally from
-- the closed sum so the label and the constructor can never disagree.
-- Unexported — used only inside 'toProcessCompose'.
namespaceFor :: NodeId -> Text
namespaceFor (SetupNode _) = "setup"
namespaceFor (RecipeNode _ _) = "recipes"

-- | The set of node identities in a 'ProcessCompose'. Returned in
-- 'Map' key order so iteration is stable. Useful for pre-seeding
-- per-node state at startup (e.g. posting @pending@ commit statuses
-- for every @(recipe, platform)@ before process-compose has begun
-- scheduling them).
processNames :: ProcessCompose -> [NodeId]
processNames pc = Map.keys pc.processes

-- | All inputs that shape a @process-compose up@ invocation. The YAML
-- config is materialised to @yamlPath@ before the spawn (rather than
-- piped through stdin) so process-compose's TUI mode — which needs the
-- parent's tty on stdin for keyboard input — works as a drop-in toggle.
-- Process-compose always binds its API to a UDS at @sockPath@ — that's
-- both the 'CI.ProcessCompose.Events.subscribeStates' attachment point
-- and the de-facto mutex for "is a ci run in progress in this checkout."
data UpInvocation = UpInvocation
  { sockPath :: FilePath,
    logFile :: FilePath,
    -- | Where to write the YAML before spawning pc. Passed via @-f@.
    yamlPath :: FilePath,
    -- | Drive process-compose's TUI (@-t=true@) instead of headless
    --     (@-t=false@). Only meaningful in 'CI.Pipeline.runLocal'; CI
    --     mode normally wants headless, but the flag itself is mode-
    --     agnostic at this layer.
    tui :: Bool,
    -- | Caller-supplied args appended verbatim after the canned
    --     baseline; the @ci run -- ...@ passthrough lands here.
    passthroughArgs :: [String]
  }

-- | Translate an 'UpInvocation' into the argv vector for @process-compose@.
-- The YAML config is read from @yamlPath@ via @-f@. @-t=true@ enables the
-- TUI, @-t=false@ keeps it headless; the flag is always emitted explicitly
-- so the chosen mode is visible at the call site.
toUpArgs :: UpInvocation -> [String]
toUpArgs up =
  ["up", "-f", up.yamlPath, tFlag, "-L", up.logFile, "-U", "-u", up.sockPath] <> up.passthroughArgs
  where
    tFlag = if up.tui then "-t=true" else "-t=false"

-- | Spawn @process-compose up@ from the 'UpInvocation'. The YAML is
-- materialised at 'yamlPath' first (overwriting any prior content); the
-- subprocess then reads it via @-f@. Stdin/stdout/stderr inherit from
-- the parent so TUI mode (when enabled) has the user's tty for input
-- and headless mode still shows pc's own progress lines.
runProcessCompose :: UpInvocation -> ProcessCompose -> IO ExitCode
runProcessCompose up pc = do
  BS.writeFile up.yamlPath (Y.encode pc)
  withCreateProcess cp $ \_ _ _ ph -> waitForProcess ph
  where
    cp = proc processComposeBin (toUpArgs up)

-- | Recover the dependency graph from an assembled 'ProcessCompose'.
-- Useful for renderers that want the typed adjacency map back ('CI.Pipeline.runGraph'
-- emits mermaid syntax from it) — pc's own @graph@ subcommand is
-- server-only (it hits a running pc's HTTP API rather than reading
-- a YAML file), so re-deriving the structure here is the path
-- that works without a live run.
processGraph :: ProcessCompose -> G.AdjacencyMap NodeId
processGraph pc =
  G.vertices (Map.keys ps)
    `G.overlay` G.edges
      [(name, dep) | (name, p) <- Map.toList ps, dep <- Map.keys p.depends_on]
  where
    ps = pc.processes
