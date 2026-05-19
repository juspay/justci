{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Compile-time Nix-derivation references and the shell snippets
-- that ship them across.
--
-- 'justDrvFor' is the analogue of 'System.Which.staticWhich' for a
-- @.drv@ path (rather than an output path) — one per supported
-- 'JustCI.Platform.Platform', baked in at TH-splice time from the
-- @JUSTCI_JUST_DRV_\<system\>@ env vars the flake injects. Lookup uses
-- 'Language.Haskell.TH.Env.envQ'' (which fails the build if the var
-- isn't set), so building outside @nix develop@ (or without the
-- flake's env-var plumbing) errors immediately rather than silently
-- embedding a placeholder.
--
-- Two consumers, both in 'JustCI.Transport':
--
--   * 'shipJustDrv' — the @nix-store --export ... | <runner>
--     nix-store --import@ step that pushes the closure of the just
--     derivation for a target platform to a remote.
--
--   * 'realisedJust' — the @\\$(nix-store --realise <drv>!out)@
--     expansion that, on the remote, fetches/builds the native
--     binary for that platform and yields the @bin/just@ prefix.
--
-- Keeping both shell snippets here means every concrete Nix-CLI
-- invocation (export, import, realise, output-selector syntax) is
-- in one module instead of woven into the bundle+ssh choreography
-- of 'JustCI.Transport'.
module JustCI.Nix (shipJustDrv, realisedJust) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (display)
import JustCI.Justfile (RecipeName)
import JustCI.Platform (Platform (..))
import Language.Haskell.TH.Env (envQ')

-- | Drv path for @just@ on the given target platform — the recipe
-- shipped to that remote, realised on-site to produce a natively
-- executable binary regardless of the runner's own arch. The splice
-- per branch is baked in from the @JUSTCI_JUST_DRV_\<system\>@ env vars
-- the flake injects (see module-level doc).
justDrvFor :: Platform -> FilePath
justDrvFor X86_64Linux = $$(envQ' "JUSTCI_JUST_DRV_X86_64_LINUX")
justDrvFor Aarch64Linux = $$(envQ' "JUSTCI_JUST_DRV_AARCH64_LINUX")
justDrvFor Aarch64Darwin = $$(envQ' "JUSTCI_JUST_DRV_AARCH64_DARWIN")

-- | The @.drv@ store path as 'Text', ready for embedding in shell
-- snippets. Both 'shipJustDrv' and 'realisedJust' need this; the
-- conversion is centralised here so neither has a @T.pack@ call.
drvText :: Platform -> Text
drvText = T.pack . justDrvFor

-- | Shell snippet that copies the @just@ derivation for the given
-- target platform to a remote, via the runner-command prefix (e.g.
-- @ssh -T hostname@).
--
-- @nix-store --export $(...closure...) | \<runner\> nix-store --import@
-- ships the drv file plus its closure; the remote can then
-- @--realise@ it. Output is redirected to @/dev/null@ since
-- @nix-store --import@ prints every imported path on its own line
-- and we don't need that noise in the per-node log.
shipJustDrv :: Text -> Platform -> Text
shipJustDrv runner targetPlat =
  "nix-store --export $(nix-store --query --requisites "
    <> drvText targetPlat
    <> ") | "
    <> runner
    <> " nix-store --import > /dev/null"

-- | The bash sub-expression that yields the @just --no-deps
-- \<recipe\>@ invocation on the remote, with @just@ provided by
-- realising the platform-specific drv.
--
-- @nix-store --realise <drv>!out@ selects the @out@ output of a
-- multi-output derivation (just has @out@, @man@, @doc@) so the
-- expansion is exactly one store path, regardless of how many
-- outputs the derivation declares.
--
-- Intended to sit inside a *single-quoted* shell argument to ssh so
-- the @$()@ subshell is evaluated by the remote shell rather than
-- the local one. Callers (in 'JustCI.Transport') are responsible for the
-- outer quoting; this function only emits the inner expansion.
realisedJust :: Platform -> RecipeName -> Text
realisedJust targetPlat recipe =
  "$(nix-store --realise "
    <> drvText targetPlat
    <> "!out)/bin/just --no-deps "
    <> display recipe
