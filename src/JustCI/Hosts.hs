{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | Persistent map from 'JustCI.Platform.Platform' to the SSH 'Host' the
-- runner should use for that lane. Two file layers, identical JSON
-- schema:
--
--   * @~\/.config\/justci\/hosts.json@ — global, per-user, shared across
--     every repo on this machine. Same convention kolu uses.
--   * @\$PWD\/.justci\/hosts.json@ — optional per-repo override,
--     checked in alongside the rest of the project. Entries here win
--     over the global file on collision; missing keys still fall
--     through to global.
--
-- Read-only from the runner's perspective: the user edits the JSON
-- file(s) by hand. 'loadGlobalHosts' / 'loadRepoHosts' read each layer
-- (dropping unknown keys); 'resolveHosts' composes the two with the
-- CLI @--host@ overlay on top. 'lookupHost' /
-- 'hostsPlatforms' query the resolved result. Missing entries are
-- not an error — 'JustCI.Pipeline.pipelinePlatformsFor' silently
-- excludes platforms with no entry from the fanout, so the user
-- opts in to a remote lane by adding its hosts.json key.
module JustCI.Hosts
  ( -- * Types
    Host,
    Hosts,
    HostsLoadError,

    -- * Construction
    hostFromText,
    mergeHostOverrides,

    -- * Loading + lookup
    resolveHosts,
    loadHostsFrom,
    lookupHost,
    hostsPlatforms,
    hostsToList,
  )
where

import Control.Exception (IOException, try)
import Data.Aeson (eitherDecodeStrict)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (Display (..))
import JustCI.Platform (Platform, parsePlatform)
import System.Directory (getCurrentDirectory, getHomeDirectory)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)

-- | An SSH destination — anything @ssh@ accepts as @[user@]host[:port]@.
-- Opaque; minted only by 'hostFromText' or via JSON decode in 'loadHostsFrom'.
newtype Host = Host Text
  deriving stock (Show, Eq, Ord)
  deriving newtype (Display)

-- | Smart constructor — named so every minting site is searchable. No
-- validation today; the @ssh@ subprocess is the source of truth on
-- whether the string is a valid destination. Two production minting
-- sites: 'loadHostsFrom' (JSON decode) and "JustCI.CLI" (the @--host PLATFORM=ADDR@
-- override flag).
hostFromText :: Text -> Host
hostFromText = Host

-- | Overlay an association-list layer onto a 'Hosts' map; the overlay
-- wins on collision. The single layer-merge receptacle: 'resolveHosts'
-- uses it twice — once with the per-repo layer (flattened via
-- 'hostsToList') on top of the global layer, then with the CLI
-- @--host@ list on top of that — so the three-source precedence
-- (global ◁ repo ◁ CLI) collapses to two folds of the same
-- function, no parallel @Hosts -> Hosts -> Hosts@ needed.
--
-- The CLI use case is a one-shot redirect to a throwaway target
-- (e.g. an LXC container) without editing any on-disk config; the
-- per-repo use case is a checked-in override of the global file.
-- Platforms not named in any source still route inline when they
-- match 'JustCI.Platform.localPlatform', and get filtered out of the
-- fanout by 'JustCI.Pipeline.pipelinePlatformsFor' otherwise.
mergeHostOverrides :: [(Platform, Host)] -> Hosts -> Hosts
mergeHostOverrides overrides (Hosts m) =
  Hosts (Map.union (Map.fromList overrides) m)

-- | A loaded view of @~\/.config\/justci\/hosts.json@. Newtype around the
-- underlying map so 'lookupHost' and 'hostsPlatforms' are the only
-- access points (no module-external pattern matching on the map shape).
newtype Hosts = Hosts (Map Platform Host)
  deriving stock (Show)

-- | Absolute path to the global hosts config: @\$HOME\/.config\/justci\/hosts.json@.
-- Computed once per process. The runner only reads the file; the
-- user creates it.
globalHostsPath :: IO FilePath
globalHostsPath = do
  home <- getHomeDirectory
  pure (home </> ".config" </> "justci" </> "hosts.json")

-- | Absolute path to the per-repo hosts override: @\$PWD\/.justci\/hosts.json@.
-- Resolved through 'getCurrentDirectory' so error messages render an
-- absolute path consistent with 'globalHostsPath'. Strict mode's
-- @ensureCleanTree@ + non-cd-ing @withSnapshotWorktree@ keep $PWD
-- equal to HEAD on disk, so reading from $PWD reads what HEAD
-- committed.
repoHostsPath :: IO FilePath
repoHostsPath = do
  cwd <- getCurrentDirectory
  pure (cwd </> ".justci" </> "hosts.json")

-- | Why 'loadHostsFrom' couldn't return a 'Hosts' value. Both failure
-- modes carry the hosts.json path so the display rendering points
-- the user at the file they need to fix.
data HostsLoadError
  = -- | The config file exists but couldn't be read (permissions,
    -- disk error). The 'IOException' message is preserved for the
    -- @Display@ rendering.
    HostsReadError FilePath IOException
  | -- | Aeson couldn't decode the bytes as the expected
    -- @Map Text Text@ shape (a syntax error, wrong top-level type,
    -- etc.). The aeson message names the position.
    HostsDecodeError FilePath String
  deriving stock (Show)

instance Display HostsLoadError where
  displayBuilder (HostsReadError path e) =
    "cannot read " <> displayBuilder (T.pack path) <> ": " <> displayBuilder (T.pack (show e))
  displayBuilder (HostsDecodeError path err) =
    "malformed " <> displayBuilder (T.pack path) <> ": " <> displayBuilder (T.pack err)

-- | Read a hosts config from the given path. Missing file → empty map
-- (a fresh user has no hosts yet, and that's not an error —
-- 'JustCI.Pipeline.pipelinePlatformsFor' filters non-local platforms
-- without entries out of the fanout). Other IO errors and malformed
-- JSON surface as 'HostsLoadError' through @Either@, carrying the
-- failing path so the user sees which file to fix; the orchestrator's
-- 'dieOnLeft' renders the structured error rather than catching an
-- opaque 'IOException'.
--
-- Unknown platform keys are dropped silently — a future addition to
-- the 'Platform' enum shouldn't reject older configs, and an
-- already-deleted constructor in an older config shouldn't reject
-- newer binaries.
loadHostsFrom :: FilePath -> IO (Either HostsLoadError Hosts)
loadHostsFrom path = do
  result <- try @IOException $ BS.readFile path
  case result of
    Left e | isDoesNotExistError e -> pure (Right (Hosts Map.empty))
    Left e -> pure (Left (HostsReadError path e))
    Right bs ->
      case eitherDecodeStrict @(Map Text Text) bs of
        Left err -> pure (Left (HostsDecodeError path err))
        Right raw ->
          pure . Right . Hosts . Map.fromList $
            mapMaybe (\(k, v) -> (,Host v) <$> parsePlatform k) (Map.toList raw)

-- | Read the global hosts config at 'globalHostsPath'. See
-- 'loadHostsFrom' for missing-file / error semantics.
loadGlobalHosts :: IO (Either HostsLoadError Hosts)
loadGlobalHosts = globalHostsPath >>= loadHostsFrom

-- | Read the per-repo hosts override at 'repoHostsPath'. See
-- 'loadHostsFrom' for missing-file / error semantics — most repos
-- have no @.justci\/hosts.json@ and that returns an empty 'Hosts'
-- like a missing global file does.
loadRepoHosts :: IO (Either HostsLoadError Hosts)
loadRepoHosts = repoHostsPath >>= loadHostsFrom

-- | Pure lookup.
lookupHost :: Platform -> Hosts -> Maybe Host
lookupHost p (Hosts m) = Map.lookup p m

-- | Every 'Platform' with a configured host entry. The pipeline
-- fanout in 'JustCI.Pipeline' intersects this set with the root recipe's
-- declared OS families to decide which Nix systems to target — so a
-- platform without a hosts.json entry doesn't appear in the fanout
-- at all (no prompt-on-miss, no fail-fast: the user explicitly opts
-- in by writing the file).
hostsPlatforms :: Hosts -> [Platform]
hostsPlatforms (Hosts m) = Map.keys m

-- | Flatten a 'Hosts' value to an association list. Inverse of the
-- 'Map.fromList' inside 'loadHostsFrom'. The only production caller
-- is 'resolveHosts', which uses it to feed a loaded per-repo layer
-- through 'mergeHostOverrides' on top of the global layer — reusing
-- the existing left-biased merge instead of introducing a parallel
-- @Hosts -> Hosts -> Hosts@ function.
hostsToList :: Hosts -> [(Platform, Host)]
hostsToList (Hosts m) = Map.toList m

-- | Resolve the layered host config used by every pipeline entry
-- point: global @~\/.config\/justci\/hosts.json@ ◁ per-repo
-- @\$PWD\/.justci\/hosts.json@ ◁ CLI @--host@ overrides. Rightmost
-- wins. Each file layer surfaces its own 'HostsLoadError' (carrying
-- the offending path) so a malformed repo file points at the repo
-- file in the error.
--
-- Every "JustCI.Pipeline" entry point routes through this — including
-- @runGraph@, @runDumpYaml@, and @runProtect@ — so the documented
-- invariant "dump-yaml shows what run does" holds when a repo file
-- is committed. The non-run entries pass @[]@ for the CLI layer;
-- @--host@ is intentionally ephemeral and shouldn't affect
-- @justci protect@'s required-checks list.
resolveHosts :: [(Platform, Host)] -> IO (Either HostsLoadError Hosts)
resolveHosts cliOverrides =
  loadGlobalHosts >>= \case
    Left err -> pure (Left err)
    Right global ->
      loadRepoHosts >>= \case
        Left err -> pure (Left err)
        Right repo ->
          pure
            . Right
            . mergeHostOverrides cliOverrides
            . mergeHostOverrides (hostsToList repo)
            $ global
