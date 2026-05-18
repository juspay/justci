{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | argv parser for the @ci@ executable. Three subcommands —
-- @run [OPTIONS] [-- ARGS...]@ (default), @dump-yaml@, @graph@ — map
-- to 'Command' constructors; "Main" dispatches each to a handler in
-- "CI.Pipeline". All knobs (@--tui@, @--host@, …) are subcommand-level
-- options under @run@: they only make sense when executing the pipeline,
-- not for the inspection subcommands.
module CI.CLI
  ( -- * Parsed argv
    Args (..),
    Command (..),
    RunOpts (..),
    ProtectOpts (..),

    -- * Entry point
    parseCli,
  )
where

import CI.Hosts (Host, hostFromText)
import CI.Justfile (RecipeName, recipeNameFromText)
import CI.Node (DagSelection (..), DepsMode (..), NodeSelector, SelectorMode (..), parseSelector)
import CI.Platform (Platform, parsePlatform, supportedPlatformsLabel)
import Control.Applicative (many, optional, (<|>))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Text (Text)
import Options.Applicative
  ( Parser,
    ParserInfo,
    ParserResult,
    ReadM,
    argument,
    defaultPrefs,
    eitherReader,
    execParserPure,
    fullDesc,
    handleParseResult,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    progDesc,
    strOption,
    subparser,
    switch,
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
-- syntax).
data Command
  = Run RunOpts
  | DumpYaml
  | Graph
  | Protect ProtectOpts

-- | Options that only apply to @ci protect@.
--
--   * @branchOverride@: which branch's protection ruleset to update.
--     'Nothing' = resolve the repo's default branch via
--     @gh repo view --json defaultBranchRef@.
--   * @dryRun@: print the contexts that /would/ be PATCHed and exit
--     without touching the GitHub API.
data ProtectOpts = ProtectOpts
  { branchOverride :: Maybe Text,
    dryRun :: Bool
  }

-- | Options that only apply to @ci run@.
--
--   * @tui@: drive process-compose's TUI instead of its headless logger.
--   * @hostOverrides@: overlay onto @~\/.config\/ci\/hosts.json@ via
--     'CI.Hosts.mergeHostOverrides'; CLI entries win on collision.
--   * @dagSelection@: the DAG-shape choice — @--root@ override plus the
--     positional leaf selectors and @--no-deps@ flag, bundled into one
--     value so 'CI.Pipeline.buildProcessCompose' takes a single
--     'CI.Node.DagSelection' instead of three positional knobs (and
--     the @"--no-deps without selectors"@ illegal combination is
--     unrepresentable at this layer).
--
-- The @--@-passthrough tail is /not/ a field on 'RunOpts': it lives
-- outside the optparse-parsed structure entirely, returned alongside
-- 'Args' by 'parseCli'. See that function's haddock for why.
data RunOpts = RunOpts
  { tui :: Bool,
    hostOverrides :: [(Platform, Host)],
    dagSelection :: DagSelection
  }

-- | Parse argv and return the structured 'Args' alongside the raw
-- post-@--@ argv tail. Bad flags and @--help@ exit the process via
-- optparse-applicative's standard handler — callers see only a
-- successful parse.
--
-- The argv is split around the first @--@ before optparse sees it:
-- pre-@--@ tokens go through the parser as flags + positional leaf
-- selectors; post-@--@ tokens are returned in the second tuple
-- element. The tail is only meaningful for @ci run@ (forwarded to
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
    )
    <|> (Run <$> runOptsParser)

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
              <> help "Override the ~/.config/ci/hosts.json mapping for this run. Repeatable; e.g. --host x86_64-linux=root@lxc-foo. CLI overrides win over the file; platforms not named here still consult the file."
          )
      )
    <*> dagSelectionParser

-- | Parse @--root@ + positional selectors + @--no-deps@ into a single
-- 'DagSelection'. The empty-selectors case collapses to 'AllNodes' so
-- the @--no-deps@ flag silently has no effect without selectors —
-- matching @just@'s behaviour for the same flag, and making the
-- @"--no-deps without selectors"@ state unrepresentable in
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
-- to 'parseSelector' in "CI.Node" — the same parsing rule the
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
