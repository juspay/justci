{-# LANGUAGE OverloadedStrings #-}

-- | Tests for "JustCI.Transport"'s SSH command builders and runner prefix.
-- 'JustCI.Justfile.recipeCommand' covers local recipe commands (no SSH plumbing).
-- The end-to-end bundle+clone+run path is exercised by the
-- @ci::run-check@ smoke test in @justci.just@; this spec locks down the
-- structural choices in isolation.
module JustCI.TransportSpec (spec) where

import qualified Data.Text as T
import JustCI.Git (shaPlaceholder)
import JustCI.Hosts (hostFromText)
import JustCI.Justfile (recipeCommand)
import JustCI.Platform (Platform (..))
import JustCI.Transport (remoteRunner, sshRecipeCommand, sshSetupCommand)
import Test.Hspec

spec :: Spec
spec = do
  describe "remoteRunner" $ do
    it "wraps a plain hostname in `ssh -T`" $
      remoteRunner (hostFromText "sincereintent") `shouldBe` "ssh -T sincereintent"

    it "wraps a user@host form in `ssh -T`" $
      remoteRunner (hostFromText "srid@builder.example.com") `shouldBe` "ssh -T srid@builder.example.com"

    it "treats an ssh-config alias the same — anything ssh dials works" $
      remoteRunner (hostFromText "srid1") `shouldBe` "ssh -T srid1"

  describe "recipeCommand" $ do
    it "emits a bare just --no-deps invocation (pc working_dir handles the cwd)" $
      ("--no-deps ci::build" `T.isInfixOf` recipeCommand "ci::build") `shouldBe` True

  describe "sshSetupCommand" $ do
    let host = hostFromText "remote.example.com"
        sha = shaPlaceholder
        cmd = sshSetupCommand host sha Aarch64Darwin 48

    it "ships the just derivation first" $
      ("nix-store --export" `T.isInfixOf` cmd) `shouldBe` True

    it "bundles HEAD into the remote cache dir" $
      ("git bundle create" `T.isInfixOf` cmd) `shouldBe` True

    it "clones into the per-(sha,platform) cached run dir on the remote" $
      ("${JUSTCI_CACHE_DIR:-${XDG_STATE_HOME:-$HOME/.local/state}/justci}/0000000/aarch64-darwin" `T.isInfixOf` cmd) `shouldBe` True

    it "never puts the checkout below ~/.cache (biome scanner trips on that, see #21)" $
      (".cache/justci/" `T.isInfixOf` cmd) `shouldBe` False

    it "skips bundle+clone on cache hit" $
      ("cat > /dev/null; exit 0" `T.isInfixOf` cmd) `shouldBe` True

    -- Cache eviction (juspay/justci#39): the snippet prepended before the
    -- setup shell prunes per-SHA dirs older than the configured TTL.
    it "interpolates the configured TTL into the eviction snippet" $
      ("HOURS=48" `T.isInfixOf` cmd) `shouldBe` True

    it "scopes eviction to the cache root, not the per-run dir" $
      ("ROOT=${JUSTCI_CACHE_DIR:-${XDG_STATE_HOME:-$HOME/.local/state}/justci}" `T.isInfixOf` cmd) `shouldBe` True

    it "excludes the current short-sha dir so concurrent setups can't evict each other" $
      ("! -path \"$CURRENT\"" `T.isInfixOf` cmd) `shouldBe` True

    it "guards eviction behind an explicit positive-TTL check (0 disables, bad input fails loud)" $
      ("[ \"$HOURS\" -gt 0 ]" `T.isInfixOf` cmd) `shouldBe` True

    it "uses portable -mmin (POSIX/BSD find), not a GNU-only -mtime suffix" $
      ("-mmin +$((HOURS * 60))" `T.isInfixOf` cmd) `shouldBe` True

    it "TTL=0 still emits the snippet but the guard short-circuits before find runs" $
      let cmd0 = sshSetupCommand host sha Aarch64Darwin 0
       in ("HOURS=0" `T.isInfixOf` cmd0) `shouldBe` True

    -- Eviction must precede the setup snippet — setup short-circuits with
    -- `exit 0` on cache hit, so a swapped order would skip eviction
    -- forever on warm caches. `ROOT=` is unique to the eviction snippet
    -- and `DIR=` is unique to setup, so a prefix check pins the order
    -- without depending on whitespace or other formatting.
    it "renders eviction before setup so cache-hit short-circuit doesn't skip the prune" $
      let (before, _) = T.breakOn "DIR=" cmd
       in ("ROOT=" `T.isInfixOf` before) `shouldBe` True

  describe "sshRecipeCommand" $ do
    let host = hostFromText "remote.example.com"
        sha = shaPlaceholder
        cmd = sshRecipeCommand host sha Aarch64Darwin "ci::build"

    it "cd's into the per-(sha,platform) cached run dir set up by the setup node" $
      ("cd ${JUSTCI_CACHE_DIR:-${XDG_STATE_HOME:-$HOME/.local/state}/justci}/0000000/aarch64-darwin/src" `T.isInfixOf` cmd) `shouldBe` True

    it "realises the drv on the remote and invokes /bin/just" $
      ("$(nix-store --realise" `T.isInfixOf` cmd) `shouldBe` True

    it "ends with --no-deps + the recipe" $
      ("/bin/just --no-deps ci::build" `T.isInfixOf` cmd) `shouldBe` True

    it "does not re-bundle (setup did that)" $
      ("git bundle" `T.isInfixOf` cmd) `shouldBe` False
