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

    -- * Entry point
    parseCli,
  )
where

import CI.Hosts (Host, hostFromText)
import CI.Platform (Platform, parsePlatform)
import Control.Applicative (many, (<|>))
import qualified Data.Text as T
import Options.Applicative
  ( Parser,
    ParserInfo,
    ReadM,
    eitherReader,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    progDesc,
    strArgument,
    subparser,
    switch,
    (<**>),
  )
import qualified Options.Applicative as O (command)

-- | Parsed argv: just the chosen subcommand. All per-mode knobs live
-- inside their subcommand's option record ('RunOpts' for @run@) —
-- there are no global flags at this layer.
newtype Args = Args {cmd :: Command}

-- | The parsed subcommand. 'Run' carries its own option record;
-- 'DumpYaml' and 'Graph' are pure inspection modes with no options
-- (the graph is always emitted as Mermaid @flowchart TD@ syntax).
data Command
  = Run RunOpts
  | DumpYaml
  | Graph

-- | Options that only apply to @ci run@.
--
--   * @tui@: drive process-compose's TUI instead of its headless logger.
--   * @hostOverrides@: overlay onto @~\/.config\/ci\/hosts.json@ via
--     'CI.Hosts.mergeHostOverrides'; CLI entries win on collision.
--   * @passthroughArgs@: everything after @--@; forwarded verbatim to
--     @process-compose up@.
data RunOpts = RunOpts
  { tui :: Bool,
    hostOverrides :: [(Platform, Host)],
    passthroughArgs :: [String]
  }

-- | Parse argv and return the structured 'Args'. Bad flags and
-- @--help@ exit the process via optparse-applicative's standard
-- handler — callers see only a successful parse.
parseCli :: IO Args
parseCli = execParser parserInfo

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
    )
    <|> (Run <$> runOptsParser)

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
    <*> many (strArgument (metavar "-- ARGS..."))

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
            "unknown platform " <> platStr <> " in --host (expected one of: x86_64-linux, aarch64-linux, aarch64-darwin)"
  _ -> Left $ "expected PLATFORM=ADDR in --host, got: " <> s
