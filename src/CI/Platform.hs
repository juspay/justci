{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The fanout platform vocabulary — Nix system tuples
-- (@x86_64-linux@, @aarch64-darwin@, …). Closed sum: each constructor
-- maps to one env-var-injected @just@ @.drv@ path that the runner
-- ships to remotes via @nix-store --export | --import@ and realises
-- on-site. The three supported systems are nixpkgs's current tier-1
-- set (@x86_64-darwin@ was dropped after upstream's 26.05 sunset).
--
-- Distinct from 'CI.Justfile.Os' — that's just's host-OS-gate
-- vocabulary (Linux/Macos/BSD/Windows); 'Platform' is the strictly
-- smaller, Nix-aware set we route to. 'osToPlatforms' bridges between
-- them: a recipe-level @[linux]@ gate matches both @x86_64-linux@ and
-- @aarch64-linux@.
module CI.Platform
  ( -- * Platform values
    Platform (..),
    allPlatforms,
    supportedPlatformsLabel,

    -- * Wire round-trip
    parsePlatform,

    -- * Recipe-OS bridge
    platformOs,
    osToPlatforms,

    -- * Local detection
    localPlatform,
  )
where

import qualified CI.Justfile as J
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (Display (..), display)
import qualified System.Info

-- | The fanout platform set. Closed sum so adding a new system
-- requires both a constructor and a matching env-var in @flake.nix@.
-- The three Nix systems with @nixpkgs@ tier-1 coverage today are
-- supported; @x86_64-darwin@ was dropped (upstream sunsetting after
-- nixpkgs 26.05). @riscv64-linux@, @armv7l-linux@, etc. can be added
-- by extending this type and mirroring the @CI_JUST_DRV_*@ entry in
-- the flake.
data Platform
  = X86_64Linux
  | Aarch64Linux
  | Aarch64Darwin
  deriving stock (Show, Eq, Ord, Bounded, Enum)

-- | Standard Nix system tuple — what @builtins.currentSystem@
-- returns and what @flake.nix@'s @legacyPackages@ keys by.
instance Display Platform where
  displayBuilder X86_64Linux = "x86_64-linux"
  displayBuilder Aarch64Linux = "aarch64-linux"
  displayBuilder Aarch64Darwin = "aarch64-darwin"

-- | Every 'Platform'.
allPlatforms :: [Platform]
allPlatforms = [minBound .. maxBound]

-- | The supported platforms as a comma-separated label
-- (e.g. @"x86_64-linux, aarch64-linux, aarch64-darwin"@). The single
-- source of truth for the platform-enum prose that appears in user-facing
-- error messages — anywhere a "supported: …" / "expected one of: …"
-- list of platforms would otherwise be hardcoded reaches for this so
-- the prose tracks 'allPlatforms' mechanically (adding or dropping a
-- 'Platform' constructor doesn't fan out to error-message string
-- literals scattered across the codebase).
supportedPlatformsLabel :: Text
supportedPlatformsLabel = T.intercalate ", " (display <$> allPlatforms)

-- | Inverse of 'display' over the closed set. 'Nothing' on anything
-- else — callers (host-config loader, 'CI.Node.parseNodeId') tolerate
-- the failure rather than dying.
parsePlatform :: Text -> Maybe Platform
parsePlatform t = case T.toLower t of
  "x86_64-linux" -> Just X86_64Linux
  "aarch64-linux" -> Just Aarch64Linux
  "aarch64-darwin" -> Just Aarch64Darwin
  _ -> Nothing

-- | The host-OS family this platform belongs to. Used to match a
-- fanout candidate against recipe-level @[linux]/[macos]@ attributes
-- emitted by @just@.
platformOs :: Platform -> J.Os
platformOs X86_64Linux = J.Linux
platformOs Aarch64Linux = J.Linux
platformOs Aarch64Darwin = J.Macos

-- | Bridge from just's host-OS-gate vocabulary to the set of Nix
-- systems that satisfy it. @[linux]@ matches both linux variants;
-- @[macos]@ matches the supported darwin variant ('Aarch64Darwin').
-- Other 'J.Os' gates ('J.Unix', 'J.Windows', the BSDs) don't identify
-- a CI lane target and return @[]@ — those stay host-OS gates only.
osToPlatforms :: J.Os -> [Platform]
osToPlatforms J.Linux = [X86_64Linux, Aarch64Linux]
osToPlatforms J.Macos = [Aarch64Darwin]
osToPlatforms _ = []

-- | The host wasn't a 'Platform' we know how to route to. Today
-- the supported set is the three Nix systems with current upstream
-- coverage (@x86_64-linux@, @aarch64-linux@, @aarch64-darwin@);
-- anything else (including @x86_64-darwin@, which nixpkgs is
-- sunsetting after 26.05) fails fast rather than silently defaulting
-- to a wrong lane.
newtype LocalPlatformError = LocalPlatformError {tuple :: String}
  deriving stock (Show)

instance Display LocalPlatformError where
  displayBuilder e =
    "unsupported local Nix system: "
      <> displayBuilder (T.pack e.tuple)
      <> " (supported: "
      <> displayBuilder supportedPlatformsLabel
      <> ")"

-- | Classify the running host into a 'Platform'. Reads
-- 'System.Info.os' + 'System.Info.arch' — both compile-time
-- constants baked in by GHC, so this is pure. Two invocations on
-- the same binary always agree.
localPlatform :: Either LocalPlatformError Platform
localPlatform = case (System.Info.os, System.Info.arch) of
  ("linux", "x86_64") -> Right X86_64Linux
  ("linux", "aarch64") -> Right Aarch64Linux
  ("darwin", "aarch64") -> Right Aarch64Darwin
  (o, a) -> Left $ LocalPlatformError $ o <> "/" <> a
