{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Per-node shell-command builders for remote SSH lanes:
--
--   * 'sshRecipeCommand' — recipe in a remote's cached checkout.
--   * 'sshSetupCommand' — the per-platform drv-copy + bundle + clone.
--
-- Local recipes use 'CI.Justfile.recipeCommand' directly (no SSH
-- plumbing needed — process-compose's @working_dir@ already pins the
-- snapshot). The caller ('CI.Pipeline.commandForNode') dispatches by
-- pattern match on 'NodeId' + the host lookup it already performed.
--
-- Remote setup nodes ship the @just@ derivation, bundle @HEAD@
-- across, and clone into the remote-side run dir
-- (@${CI_CACHE_DIR:-${XDG_STATE_HOME:-$HOME/.local/state}/ci}/\<short-sha\>/\<platform\>/src@).
-- Idempotent: same-SHA reruns hit the cached directory and skip the
-- bundle. Remote recipe nodes @cd@ into the same shared cached
-- directory (which their @depends_on@ setup node has already
-- populated) and run the realised @just --no-deps \<recipe\>@.
--
-- The split collapses N bundle transfers (one per recipe per
-- platform) down to one per remote per run, and to zero on cache
-- hits.
--
-- Every remote command runs over plain @ssh -T \<host\>@. Anything
-- the local @ssh@ config knows how to dial works as the host
-- string — bare hostnames, @user\@host@, aliases from
-- @~\/.ssh\/config@ (incus instances are reached via an ssh alias).
module CI.Transport
  ( -- * Command builders
    sshSetupCommand,
    sshRecipeCommand,

    -- * SSH prefix
    remoteRunner,
  )
where

import CI.Git (Sha)
import CI.Hosts (Host)
import CI.Justfile (RecipeName)
import CI.LogPath (shortShaLen)
import CI.Nix (realisedJust, shipJustDrv)
import CI.Platform (Platform)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (display)

-- | Setup-node command. Ships the @just@ derivation, then bundles
-- @HEAD@ across and clones into 'cachedRunDir'. Idempotent: if the
-- target @src\/@ already exists (cache hit on same-SHA rerun), the
-- incoming bundle bytes are discarded and the setup exits 0
-- immediately.
--
-- The bundle is always piped over the wire (we don't probe-then-ship
-- because that'd add a round trip on every run). On a cache hit
-- the wasted bandwidth is a few MB of bundle bytes discarded into
-- @/dev/null@ on the remote — fine.
sshSetupCommand :: Host -> Sha -> Platform -> Text
sshSetupCommand host sha targetPlat =
  shipJustDrv r targetPlat
    <> " && git bundle create - --all 2>/dev/null | "
    <> r
    <> " '"
    <> remoteSetupShell sha targetPlat
    <> "'"
  where
    r = remoteRunner host

-- | Per-recipe remote command. The corresponding setup node has
-- already provisioned the cached checkout (process-compose's
-- @depends_on@ enforces ordering); the recipe just @cd@s into it
-- and runs the realised @just@.
sshRecipeCommand :: Host -> Sha -> Platform -> RecipeName -> Text
sshRecipeCommand host sha targetPlat r' =
  runner
    <> " 'cd "
    <> cachedRunDir sha targetPlat
    <> "/src && "
    <> realisedJust targetPlat r'
    <> "'"
  where
    runner = remoteRunner host

-- | The shell-tokens prefix that runs a command on this 'Host':
-- @ssh -T \<host\>@. @-T@ suppresses TTY allocation so binary stdin
-- (the @git bundle@ stream) survives unmolested. Anything the local
-- @ssh@ config knows how to dial — bare @hostname@, @user\@host@,
-- an alias from @~\/.ssh\/config@ — works as the host string.
remoteRunner :: Host -> Text
remoteRunner host = "ssh -T " <> display host

-- | The shared checkout path on the remote, deterministic from
-- @(short-sha, platform)@. Resolved at the *remote* shell:
--
-- @
-- ${CI_CACHE_DIR:-${XDG_STATE_HOME:-$HOME/.local/state}/ci}/\<short-sha\>/\<platform\>
-- @
--
-- The setup node clones into @\<cachedRunDir\>\/src@; recipe nodes
-- @cd@ into the same path.
--
-- Why not @~\/.cache\/ci\/...@ — biome's project scanner has a
-- case-sensitive @.cache@ filter on /any/ ancestor directory, so a
-- checkout below @~\/.cache@ poisons every @biome lint@ invocation
-- with phantom @noUndeclaredDependencies@ errors. Despite the name
-- (which matches the CLI override env-var), the directory is
-- /state/ in the XDG sense: SHA-pinned, runner-managed, intentionally
-- outlives a single run. @\$XDG_STATE_HOME@ is the conformant home
-- ('\$HOME/.local/state' default). The two-level override —
-- @CI_CACHE_DIR@ → @XDG_STATE_HOME@ → @\$HOME/.local/state@ — lets
-- runners with restricted writable homes opt in explicitly without
-- having to set XDG vars project-wide. See juspay\/ci#21.
--
-- Across runs against the same SHA the directory persists, so
-- re-runs (e.g. @ci run e2e@ after fixing a flake) skip the
-- bundle+clone entirely. Garbage collection is the user's job —
-- @rm -rf ~\/.local\/state\/ci@ (or whatever override resolves to)
-- when disk pressure warrants.
cachedRunDir :: Sha -> Platform -> Text
cachedRunDir sha targetPlat =
  "${CI_CACHE_DIR:-${XDG_STATE_HOME:-$HOME/.local/state}/ci}/"
    <> T.take shortShaLen (display sha)
    <> "/"
    <> display targetPlat

-- | The remote-side shell snippet the setup node sends over ssh.
-- Single-quoted on the way through so the local shell leaves @$DIR@
-- and friends alone; the remote shell expands them. Cache-hit path
-- short-circuits with @cat > /dev/null@ to consume the bundle bytes
-- the local side is already piping.
remoteSetupShell :: Sha -> Platform -> Text
remoteSetupShell sha targetPlat =
  T.intercalate "; " $
    [ "set -e",
      "DIR=" <> cachedRunDir sha targetPlat
    ]
      <> [ "if [ -d \"$DIR/src\" ]; then cat > /dev/null; exit 0; fi",
           "mkdir -p \"$DIR\"",
           "cd \"$DIR\"",
           "cat > repo.bundle",
           "git clone --quiet repo.bundle src",
           "cd src",
           "git -c advice.detachedHead=false checkout --quiet " <> display sha
         ]
