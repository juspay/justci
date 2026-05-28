{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | Persistent map from 'JustCI.Platform.Platform' to the SSH 'Host' the
-- runner should use for that lane. Lives at
-- @~\/.config\/justci\/hosts.json@ — one global file per user, shared
-- across every repo on this machine. Same convention kolu uses.
--
-- Read-only from the runner's perspective: the user edits the JSON
-- file by hand. 'loadHosts' reads the file (dropping unknown keys),
-- 'lookupHost' / 'hostsPlatforms' query the result. Missing entries
-- are not an error — 'JustCI.Fanout.pipelinePlatformsFor' silently
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
    loadHosts,
    lookupHost,
    hostsPlatforms,

    -- * Internal (exposed for tests)
    hostsFromList,
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
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)

-- | An SSH destination — anything @ssh@ accepts as @[user@]host[:port]@.
-- Opaque; minted only by 'hostFromText' or via JSON decode in 'loadHosts'.
newtype Host = Host Text
  deriving stock (Show, Eq, Ord)
  deriving newtype (Display)

-- | Smart constructor — named so every minting site is searchable. No
-- validation today; the @ssh@ subprocess is the source of truth on
-- whether the string is a valid destination. Two production minting
-- sites: 'loadHosts' (JSON decode) and "JustCI.CLI" (the @--host PLATFORM=ADDR@
-- override flag).
hostFromText :: Text -> Host
hostFromText = Host

-- | Internal smart constructor — currently the only consumer is
-- 'JustCI.FanoutSpec', so the test suite can build a 'Hosts' without
-- exposing the newtype constructor. Last entry wins on duplicate keys
-- ('Map.fromList' semantics). Production paths use 'loadHosts'.
hostsFromList :: [(Platform, Host)] -> Hosts
hostsFromList = Hosts . Map.fromList

-- | Overlay caller-supplied @(Platform, Host)@ overrides onto a 'Hosts'
-- map. Used by the CLI's @--host@ flag for one-shot redirects to a
-- throwaway target (e.g. an LXC container) without editing
-- @~\/.config\/justci\/hosts.json@. CLI overrides win over the loaded map
-- on collision; platforms not named by either source still route
-- inline when they match 'JustCI.Platform.localPlatform', and get
-- filtered out of the fanout by 'JustCI.Fanout.pipelinePlatformsFor'
-- otherwise.
mergeHostOverrides :: [(Platform, Host)] -> Hosts -> Hosts
mergeHostOverrides overrides (Hosts m) =
  Hosts (Map.union (Map.fromList overrides) m)

-- | A loaded view of @~\/.config\/justci\/hosts.json@. Newtype around the
-- underlying map so 'lookupHost' and 'hostsPlatforms' are the only
-- access points (no module-external pattern matching on the map shape).
newtype Hosts = Hosts (Map Platform Host)
  deriving stock (Show)

-- | Absolute path to the hosts config: @\$HOME\/.config\/justci\/hosts.json@.
-- Computed once per process. The runner only reads the file; the
-- user creates it.
hostsPath :: IO FilePath
hostsPath = do
  home <- getHomeDirectory
  pure (home </> ".config" </> "justci" </> "hosts.json")

-- | Why 'loadHosts' couldn't return a 'Hosts' value. Both failure
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

-- | Read the config file. Missing file → empty map (a fresh user has
-- no hosts yet, and that's not an error — 'pipelinePlatformsFor'
-- filters non-local platforms without entries out of the fanout).
-- Other IO errors and malformed JSON surface as 'HostsLoadError'
-- through @Either@; the orchestrator's 'dieOnLeft' renders the
-- structured error rather than catching an opaque 'IOException'.
--
-- Unknown platform keys are dropped silently — a future addition to
-- the 'Platform' enum shouldn't reject older configs, and an
-- already-deleted constructor in an older config shouldn't reject
-- newer binaries.
loadHosts :: IO (Either HostsLoadError Hosts)
loadHosts = do
  path <- hostsPath
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

-- | Pure lookup.
lookupHost :: Platform -> Hosts -> Maybe Host
lookupHost p (Hosts m) = Map.lookup p m

-- | Every 'Platform' with a configured host entry. The pipeline
-- fanout in 'JustCI.Fanout' intersects this set with the root recipe's
-- declared OS families to decide which Nix systems to target — so a
-- platform without a hosts.json entry doesn't appear in the fanout
-- at all (no prompt-on-miss, no fail-fast: the user explicitly opts
-- in by writing the file).
hostsPlatforms :: Hosts -> [Platform]
hostsPlatforms (Hosts m) = Map.keys m
