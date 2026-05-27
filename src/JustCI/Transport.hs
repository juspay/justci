{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Per-node shell-command builders for remote SSH lanes:
--
--   * 'sshRecipeCommand' — recipe in a remote's cached checkout.
--   * 'sshSetupCommand' — the per-platform drv-copy + bundle + clone.
--
-- Local recipes use 'JustCI.Justfile.recipeCommand' directly (no SSH
-- plumbing needed — process-compose's @working_dir@ already pins the
-- snapshot). The caller ('JustCI.Pipeline.commandForNode') dispatches by
-- pattern match on 'NodeId' + the host lookup it already performed.
--
-- Remote setup nodes ship the @just@ derivation, bundle @HEAD@
-- across, and clone into the remote-side run dir
-- (@${JUSTCI_CACHE_DIR:-${XDG_STATE_HOME:-$HOME/.local/state}/justci}/\<short-sha\>/\<platform\>/src@).
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
module JustCI.Transport
  ( -- * Command builders
    sshSetupCommand,
    sshRecipeCommand,

    -- * SSH prefix
    remoteRunner,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (display)
import JustCI.Git (Sha)
import JustCI.Hosts (Host)
import JustCI.Justfile (RecipeName)
import JustCI.LogPath (shortShaLen)
import JustCI.Nix (realisedJust, shipJustDrv)
import JustCI.Platform (Platform)

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
--
-- @ttlHours@ is interpolated into a prepended eviction snippet
-- ('remoteEvictCacheShell') so the remote prunes stale per-SHA cache
-- dirs on every setup. Composed into the same SSH call to avoid a
-- second round-trip; @ttlHours == 0@ makes the eviction a no-op.
sshSetupCommand :: Host -> Sha -> Platform -> Int -> Text
sshSetupCommand host sha targetPlat ttlHours =
  shipJustDrv r targetPlat
    <> " && git bundle create - --all 2>/dev/null | "
    <> r
    <> " '"
    <> remoteEvictCacheShell ttlHours sha
    <> " ; "
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

-- ${JUSTCI_CACHE_DIR:-${XDG_STATE_HOME:-$HOME/.local/state}/justci}/\<short-sha\>/\<platform\>
-- @
--
-- The setup node clones into @\<cachedRunDir\>\/src@; recipe nodes
-- @cd@ into the same path.
--
-- Why not @~\/.cache\/justci\/...@ — biome's project scanner has a
-- case-sensitive @.cache@ filter on /any/ ancestor directory, so a
-- checkout below @~\/.cache@ poisons every @biome lint@ invocation
-- with phantom @noUndeclaredDependencies@ errors. Despite the name
-- (which matches the CLI override env-var), the directory is
-- /state/ in the XDG sense: SHA-pinned, runner-managed, intentionally
-- outlives a single run. @\$XDG_STATE_HOME@ is the conformant home
-- ('\$HOME/.local/state' default). The two-level override —
-- @JUSTCI_CACHE_DIR@ → @XDG_STATE_HOME@ → @\$HOME/.local/state@ — lets
-- runners with restricted writable homes opt in explicitly without
-- having to set XDG vars project-wide. See juspay\/justci#21.
--
-- Across runs against the same SHA the directory persists, so
-- re-runs (e.g. @justci run e2e@ after fixing a flake) skip the
-- bundle+clone entirely. Stale per-SHA dirs are pruned by
-- 'remoteEvictCacheShell' on the next setup (defaults to a 48-hour
-- TTL via @--cache-ttl-hours@); see juspay\/justci#39.

-- | The remote cache prefix — the directory holding every
-- @\<short-sha\>\/\<platform\>\/@ dir. Single source of truth for the
-- path expansion: 'cachedRunDir' joins per-run segments onto it, and
-- 'remoteEvictCacheShell' uses it as the prune scope. If the prefix
-- ever gains a segment, both call sites move together.
cacheRoot :: Text
cacheRoot = "${JUSTCI_CACHE_DIR:-${XDG_STATE_HOME:-$HOME/.local/state}/justci}"

cachedRunDir :: Sha -> Platform -> Text
cachedRunDir sha targetPlat =
  cacheRoot
    <> "/"
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

-- | Prune per-SHA cache dirs under 'cacheRoot' whose mtime is older
-- than @ttlHours@. Composed before 'remoteSetupShell' in the same SSH
-- call so eviction always runs, including on cache-hit reruns (the
-- setup snippet would otherwise short-circuit before any cleanup).
--
-- The current run's @\<short-sha\>\/@ dir is excluded by @! -path@ so
-- a concurrent setup from a separate orchestrator targeting the same
-- remote can't delete the clone the other is populating. Within one
-- pipeline this race can't happen — 'JustCI.Fanout.fanOut' emits
-- exactly one setup node per remote — but across distinct @justci run@
-- invocations on different checkouts (or different machines) hitting
-- one shared remote, the exclusion is the only thing preventing a
-- TTL-past dir from being torn out from under a replay.
--
-- @ttlHours <= 0@ disables eviction. The shell-level @[ -gt 0 ]@
-- guard also rejects non-numeric env-var injection if the operator
-- ever wraps this with a wrapper script.
--
-- Portability: @-mmin@, @-mindepth@, @-maxdepth@, @-path@, @!@, and
-- @-exec ... {} +@ are all in POSIX/BSD find (verified against
-- FreeBSD's find(1)); macOS remotes are covered.
remoteEvictCacheShell :: Int -> Sha -> Text
remoteEvictCacheShell ttlHours sha =
  T.intercalate
    "; "
    [ "set -e",
      "ROOT=" <> cacheRoot,
      "CURRENT=\"$ROOT/" <> T.take shortShaLen (display sha) <> "\"",
      "HOURS=" <> T.pack (show ttlHours),
      "if [ \"$HOURS\" -gt 0 ] && [ -d \"$ROOT\" ]; then "
        <> "find \"$ROOT\" -mindepth 1 -maxdepth 1 -type d "
        <> "-mmin +$((HOURS * 60)) "
        <> "! -path \"$CURRENT\" "
        <> "-exec rm -rf -- {} + ; "
        <> "fi"
    ]
