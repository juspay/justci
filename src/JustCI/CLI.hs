{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | argv parser for the @ci@ executable. Three subcommands —
-- @run [OPTIONS] [-- ARGS...]@ (default), @dump-yaml@, @graph@ — map
-- to 'Command' constructors; "Main" dispatches each to a handler in
-- "JustCI.Pipeline". All knobs (@--tui@, @--host@, …) are subcommand-level
-- options under @run@: they only make sense when executing the pipeline,
-- not for the inspection subcommands.
module JustCI.CLI
  ( -- * Parsed argv
    Args (..),
    Command (..),
    PcVerb (..),
    pcVerbArg,
    RunOpts (..),
    ProtectOpts (..),

    -- * Entry point
    parseCli,
  )
where

import Control.Applicative (many, optional, (<|>))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import JustCI.Gh (BranchName)
import JustCI.Hosts (Host, hostFromText)
import JustCI.Justfile (RecipeName, recipeNameFromText)
import JustCI.Node (DagSelection (..), DepsMode (..), NodeSelector, SelectorMode (..), parseSelector)
import JustCI.Platform (Platform, parsePlatform, supportedPlatformsLabel)
import JustCI.Transport (defaultCacheTtlHours)
import Options.Applicative
  ( Parser,
    ParserInfo,
    ParserResult,
    ReadM,
    argument,
    auto,
    defaultPrefs,
    eitherReader,
    execParserPure,
    forwardOptions,
    fullDesc,
    handleParseResult,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    progDesc,
    showDefault,
    str,
    strOption,
    subparser,
    switch,
    value,
    (<**>),
  )
import qualified Options.Applicative as O (command)
import System.Environment (getArgs)

-- | Parsed argv: just the chosen subcommand. All per-mode knobs live
-- inside their subcommand's option record ('RunOpts' for @run@) —
-- there are no global flags at this layer.
newtype Args = Args {cmd :: Command}

-- | The parsed subcommand. 'Run' and 'Protect' carry their own option
-- records; 'DumpYaml' and 'Graph' are pure inspection modes with no
-- options (the graph is always emitted as Mermaid @flowchart TD@
-- syntax); 'PcPassthrough' tags one of three live-introspection verbs
-- and carries the user-supplied tail to forward to @process-compose@.
data Command
  = Run RunOpts
  | DumpYaml
  | Graph
  | Protect ProtectOpts
  | PcPassthrough PcVerb [String]

-- | The three live-introspection subcommands all share one implementation
-- — shell out to the baked @process-compose@ binary against the running
-- pipeline's UDS. This tag picks which @process \<verb\>@ to forward.
--
-- Phrased as a sum (rather than three top-level 'Command' constructors)
-- so the dispatch arm in @app/Main.hs@ is a single case branch and the
-- pc-verb mapping lives in exactly one place ('pcVerbArg'). Adding a
-- fourth live verb later is a one-line constructor + one-line mapping.
data PcVerb = PcStatus | PcLogs | PcMonitor
  deriving stock (Eq, Show)

-- | Map a justci-facing 'PcVerb' to the @process-compose process \<verb\>@
-- token. The one rename — 'PcStatus' → @"list"@ — gives the user-facing
-- subcommand a clearer name (@justci status@ matches @git status@ /
-- @systemctl status@ idiom) without dragging pc's slightly-awkward
-- @"list"@ noun into the justci CLI surface.
pcVerbArg :: PcVerb -> String
pcVerbArg PcStatus = "list"
pcVerbArg PcLogs = "logs"
pcVerbArg PcMonitor = "monitor"

-- | Options that only apply to @justci protect@.
--
--   * @branchOverride@: which branch's protection ruleset to update.
--     'Nothing' = resolve the repo's default branch via
--     @gh repo view --json defaultBranchRef@.
--   * @dryRun@: print the contexts that /would/ be PATCHed and exit
--     without touching the GitHub API.
data ProtectOpts = ProtectOpts
  { branchOverride :: Maybe BranchName,
    dryRun :: Bool
  }

-- | Options that only apply to @justci run@.
--
--   * @tui@: drive process-compose's TUI instead of its headless logger.
--   * @hostOverrides@: overlay onto @~\/.config\/justci\/hosts.json@ via
--     'JustCI.Hosts.mergeHostOverrides'; CLI entries win on collision.
--   * @dagSelection@: the DAG-shape choice — @--root@ override, the
--     @--platform@ pre-fanout filter, the positional leaf selectors,
--     and the @--no-deps@ flag — bundled into one value so
--     'JustCI.Pipeline.buildProcessCompose' takes a single
--     'JustCI.Node.DagSelection' instead of four positional knobs
--     (and the @"--no-deps without selectors"@ illegal combination is
--     unrepresentable at this layer).
--   * @noStrict@ \/ @noSnapshot@ \/ @noPost@: three opt-outs the user
--     can dial from full strict-by-default down to a no-frills dev
--     run. Each names one axis ('JustCI.Pipeline.resolveRunPolicy'
--     resolves the trio to a 'JustCI.Pipeline.RunPolicy'):
--
--       * @noPost@: skip GitHub commit-status posts. Clean-tree refuse
--         and the HEAD worktree pin still apply.
--       * @noSnapshot@: skip the clean-tree refuse and HEAD worktree
--         pin. Implies @noPost@ — posting a SHA-tagged status against
--         bytes that aren't @HEAD@ violates the "SHA matches tested
--         bytes" invariant.
--       * @noStrict@: meta — equivalent to @noSnapshot && noPost@. The
--         one-flag dev-mode opt-out matching the common case where
--         the user wants every strict-mode side effect disabled.
--
-- The @--@-passthrough tail is /not/ a field on 'RunOpts': it lives
-- outside the optparse-parsed structure entirely, returned alongside
-- 'Args' by 'parseCli'. See that function's haddock for why.
data RunOpts = RunOpts
  { tui :: Bool,
    hostOverrides :: [(Platform, Host)],
    dagSelection :: DagSelection,
    -- | TTL in hours for per-SHA dirs under the remote cache root.
    -- @0@ disables eviction; the current run's dir is never evicted.
    -- Threaded to 'JustCI.Transport.remoteEvictCacheShell' via
    -- 'JustCI.Pipeline.buildProcessCompose'. See juspay\/justci#39.
    cacheTtlHours :: Int,
    noStrict :: Bool,
    noSnapshot :: Bool,
    noPost :: Bool
  }

-- | Parse argv and return the structured 'Args' alongside the raw
-- post-@--@ argv tail. Bad flags and @--help@ exit the process via
-- optparse-applicative's standard handler — callers see only a
-- successful parse.
--
-- The argv is split around the first @--@ before optparse sees it:
-- pre-@--@ tokens go through the parser as flags + positional leaf
-- selectors; post-@--@ tokens are returned in the second tuple
-- element. The tail is only meaningful for @justci run@ (forwarded to
-- @process-compose up@) and ignored on the inspection subcommands —
-- the caller in "Main" pattern-matches on the subcommand and routes
-- accordingly. Returning it explicitly rather than threading it
-- through 'RunOpts' avoids the two-phase parse-then-rewrite pattern
-- where the parser would otherwise have to write a known-wrong
-- placeholder that a downstream pass corrects.
parseCli :: IO (Args, [String])
parseCli = do
  raw <- getArgs
  let (pre, post) = case break (== "--") raw of
        (xs, "--" : ys) -> (xs, ys)
        _ -> (raw, [])
  args <- handleParseResult (execParserPure defaultPrefs parserInfo pre :: ParserResult Args)
  pure (args, post)

parserInfo :: ParserInfo Args
parserInfo =
  info
    (argsParser <**> helper)
    (fullDesc <> progDesc "Drive CI by translating the just recipe graph into process-compose")

argsParser :: Parser Args
argsParser = Args <$> commandParser

commandParser :: Parser Command
commandParser =
  subparser
    ( O.command "run" (info (Run <$> runOptsParser) (progDesc "Execute the CI pipeline via process-compose (default). Args after -- are passed through."))
        <> O.command "dump-yaml" (info (pure DumpYaml) (progDesc "Print the process-compose YAML to stdout"))
        <> O.command "graph" (info (pure Graph) (progDesc "Print the process dependency graph in Mermaid flowchart syntax"))
        <> O.command "protect" (info (Protect <$> protectOptsParser) (progDesc "Set GitHub branch-protection required_status_checks to the (recipe, platform) contexts the canonical DAG produces."))
        <> O.command "status" (pcPassthroughInfo PcStatus "Snapshot every node's state in the running pipeline. Forwards to `process-compose process list` against $PWD/.ci/pc.sock. Pass `-o json` for jq-able output; other flags pass through verbatim.")
        <> O.command "logs" (pcPassthroughInfo PcLogs "Tail or follow one node's logs in the running pipeline. Positional argument is the process name (`<recipe>@<platform>`); `-f` follows. Forwards to `process-compose process logs`.")
        <> O.command "monitor" (pcPassthroughInfo PcMonitor "Live state-transition stream for every node in the running pipeline. Forwards to `process-compose process monitor`. Pass `-o json` for one JSON event per line.")
    )
    <|> Run <$> runOptsParser

-- | 'ParserInfo' for a live-introspection subcommand. 'forwardOptions'
-- lets pc's own flags (@-f@, @-o json@, @--no-snapshot@) reach the
-- subprocess without justci having to re-declare them; 'many str'
-- collects positionals (process names) plus any forwarded flags.
pcPassthroughInfo :: PcVerb -> String -> ParserInfo Command
pcPassthroughInfo verb desc =
  info
    (PcPassthrough verb <$> many (argument str (metavar "ARGS...")))
    (forwardOptions <> progDesc desc)

protectOptsParser :: Parser ProtectOpts
protectOptsParser =
  ProtectOpts
    <$> optional
      ( strOption
          ( long "branch"
              <> metavar "BRANCH"
              <> help "Which branch to update protection for. Defaults to the repo's default branch (queried via `gh repo view`)."
          )
      )
    <*> switch
      ( long "dry-run"
          <> help "Print the contexts that would be PATCHed and exit, without touching the GitHub API."
      )

runOptsParser :: Parser RunOpts
runOptsParser =
  RunOpts
    <$> switch (long "tui" <> help "Drive process-compose's TUI instead of its headless logger.")
    <*> many
      ( option
          hostOverrideReader
          ( long "host"
              <> metavar "PLATFORM=ADDR"
              <> help "Override the ~/.config/justci/hosts.json mapping for this run. Repeatable; e.g. --host x86_64-linux=root@lxc-foo. CLI overrides win over the file; platforms not named here still consult the file."
          )
      )
    <*> dagSelectionParser
    <*> option
      auto
      ( long "cache-ttl-hours"
          <> metavar "N"
          <> value defaultCacheTtlHours
          <> showDefault
          <> help "On every remote setup, prune per-SHA cache dirs under $JUSTCI_CACHE_DIR (~/.local/state/justci by default) older than N hours. 0 disables eviction. The current run's dir is never evicted. See juspay/justci#39."
      )
    <*> switch
      ( long "no-strict"
          <> help "Opt out of every strict-mode side effect: run against the live working tree (no clean-tree refuse, no HEAD worktree pin) and skip GitHub commit-status posts. The dev-mode shortcut, equivalent to `--no-snapshot --no-post`."
      )
    <*> switch
      ( long "no-snapshot"
          <> help "Run against the live working tree (skip the clean-tree refuse and the HEAD `git worktree` pin). Implies `--no-post` — a SHA-tagged GitHub status against unpinned bytes violates the reproducibility invariant. Distinct from process-compose's own `--no-snapshot` flag, which is forwarded after `--` if you need it."
      )
    <*> switch
      ( long "no-post"
          <> help "Skip GitHub commit-status posts. Clean-tree refuse and HEAD worktree pin still apply; useful for non-github strict consumers and for debugging strict runs without writing to the PR's checks list."
      )

-- | Parse @--root@ + @--platform@ + positional selectors + @--no-deps@
-- into a single 'DagSelection'. The empty-selectors case collapses to
-- 'AllNodes' so the @--no-deps@ flag silently has no effect without
-- selectors — matching @just@'s behaviour for the same flag, and
-- making the @"--no-deps without selectors"@ state unrepresentable in
-- 'SelectorMode' rather than relying on docstring discipline.
dagSelectionParser :: Parser DagSelection
dagSelectionParser =
  DagSelection
    <$> optional
      ( option
          recipeNameReader
          ( long "root"
              <> metavar "RECIPE"
              <> help "Use RECIPE as the DAG root instead of whichever recipe carries [metadata(\"ci\")]. The pipeline fans out across its OS attributes as usual."
          )
      )
    <*> many
      ( option
          platformReader
          ( long "platform"
              <> metavar "PLATFORM"
              <> help "Restrict the run to this platform; repeatable to opt into a subset (e.g. --platform x86_64-linux --platform aarch64-darwin). Without --platform, the pipeline fans out across every platform the root recipe's OS attributes permit. Intersected with the natural fanout — requested platforms outside it are silently dropped; an empty intersection errors. No effect on `dump-yaml`/`graph`/`protect`."
          )
      )
    <*> selectorModeParser

selectorModeParser :: Parser SelectorMode
selectorModeParser =
  combine
    <$> many
      ( argument
          selectorReader
          ( metavar "RECIPE[@PLATFORM]..."
              <> help "Restrict the run to these recipes and their dependencies. Each positional is either a bare recipe (fans out across every pipeline platform) or RECIPE@PLATFORM (pin to one platform). Use --no-deps to skip the transitive expansion. Setup nodes are auto-included for every remote platform a selected recipe lands on."
          )
      )
    <*> switch
      ( long "no-deps"
          <> help "With positional RECIPE[@PLATFORM] selectors, run only the named nodes — do not transitively expand their dependencies. Mirrors `just --no-deps`. Silently ignored when no selectors are given."
      )
  where
    combine [] _ = AllNodes
    combine (x : xs) skipDeps =
      SelectedLeaves (x NE.:| xs) (if skipDeps then NoDeps else WithDeps)

-- | Parse a single positional @RECIPE[\@PLATFORM]@ selector. Delegates
-- to 'parseSelector' in "JustCI.Node" — the same parsing rule the
-- 'NodeSelector' wire form documents.
selectorReader :: ReadM NodeSelector
selectorReader = eitherReader (parseSelector . T.pack)

-- | Parse a single recipe name for @--root@. 'recipeNameFromText' is
-- total, so the only failure mode here is the empty string.
recipeNameReader :: ReadM RecipeName
recipeNameReader = eitherReader $ \s ->
  if null s
    then Left "empty recipe name in --root"
    else Right (recipeNameFromText (T.pack s))

-- | Parse a single @PLATFORM@ argument into a typed 'Platform'.
-- The accepted vocabulary is the same as 'hostOverrideReader' — one
-- of the 'JustCI.Platform.Platform' display renderings. Used by
-- @--platform@ for the pre-fanout user filter; on parse failure the
-- error names the supported set verbatim.
platformReader :: ReadM Platform
platformReader = eitherReader $ \s -> case parsePlatform (T.pack s) of
  Just p -> Right p
  Nothing ->
    Left $
      "unknown platform " <> s <> " in --platform (expected one of: " <> T.unpack supportedPlatformsLabel <> ")"

-- | Parse a single @PLATFORM=ADDR@ argument into a typed pair. The
-- platform must be one of the 'Platform' constructors'
-- 'Data.Text.Display.Display' renderings (e.g. @x86_64-linux@); the
-- address is taken verbatim as the @ssh@ destination. Empty addresses
-- and unknown platforms produce an @optparse@-style error so the
-- failure surfaces before any host loading happens.
hostOverrideReader :: ReadM (Platform, Host)
hostOverrideReader = eitherReader $ \s -> case break (== '=') s of
  (platStr, '=' : addr)
    | not (null addr) -> case parsePlatform (T.pack platStr) of
        Just p -> Right (p, hostFromText (T.pack addr))
        Nothing ->
          Left $
            "unknown platform " <> platStr <> " in --host (expected one of: " <> T.unpack supportedPlatformsLabel <> ")"
  _ -> Left $ "expected PLATFORM=ADDR in --host, got: " <> s
