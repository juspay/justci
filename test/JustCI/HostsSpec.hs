{-# LANGUAGE OverloadedStrings #-}

-- | Tests for "JustCI.Hosts"'s file-load surface and the layered
-- composition that 'JustCI.Hosts.resolveHosts' uses. The IO half
-- ('loadHostsFrom') exercises the JSON-decode path against fixture
-- files written to a tempdir; the pure half ('mergeHostOverrides'
-- composed with 'hostsToList') exercises the layering precedence
-- (global ◁ repo ◁ CLI) without involving 'loadGlobalHosts' /
-- 'loadRepoHosts' (those just feed paths into 'loadHostsFrom').
module JustCI.HostsSpec (spec) where

import Control.Exception (bracket)
import qualified Data.ByteString as BS
import Data.List (isInfixOf)
import Data.Text.Display (display)
import JustCI.Hosts
  ( hostFromText,
    hostsPlatforms,
    hostsToList,
    loadHostsFrom,
    lookupHost,
    mergeHostOverrides,
  )
import JustCI.Platform (Platform (..))
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openBinaryTempFile)
import Test.Hspec

spec :: Spec
spec = do
  describe "loadHostsFrom" $ do
    it "treats a missing file as empty (no error)" $ do
      result <- loadHostsFrom "/this/path/does/not/exist/hosts.json"
      case result of
        Right hs -> hostsPlatforms hs `shouldBe` []
        Left e -> expectationFailure $ "expected Right, got Left: " <> show e

    it "decodes a well-formed file" $ withTempHosts wellFormedJson $ \path -> do
      result <- loadHostsFrom path
      case result of
        Right hs -> do
          lookupHost X86_64Linux hs `shouldBe` Just (hostFromText "builder.example.com")
          lookupHost Aarch64Darwin hs `shouldBe` Just (hostFromText "mac-runner.example.com")
          lookupHost Aarch64Linux hs `shouldBe` Nothing
        Left e -> expectationFailure $ "expected Right, got Left: " <> show e

    it "drops unknown platform keys silently" $ withTempHosts withUnknownKeyJson $ \path -> do
      result <- loadHostsFrom path
      case result of
        Right hs -> hostsPlatforms hs `shouldBe` [X86_64Linux]
        Left e -> expectationFailure $ "expected Right, got Left: " <> show e

    it "surfaces a decode error naming the file" $ withTempHosts "{not json" $ \path -> do
      result <- loadHostsFrom path
      case result of
        Left e -> show e `shouldSatisfy` (path `isInfixOf`)
        Right _ -> expectationFailure "expected Left HostsDecodeError"

  describe "layered composition (global ◁ repo ◁ CLI)" $ do
    -- The composition under test is what 'resolveHosts' does in IO:
    --   mergeHostOverrides cli . mergeHostOverrides (hostsToList repo) $ global
    -- All inputs are loaded via 'loadHostsFrom' so the layering test
    -- runs against the same JSON pipeline production uses.
    it "repo file overrides the global file" $
      withTempHosts globalJson $ \globalPath ->
        withTempHosts repoJson $ \repoPath -> do
          Right global <- loadHostsFrom globalPath
          Right repo <- loadHostsFrom repoPath
          let merged = mergeHostOverrides (hostsToList repo) global
          display <$> lookupHost X86_64Linux merged `shouldBe` Just "repo-linux"
          display <$> lookupHost Aarch64Darwin merged `shouldBe` Just "global-mac"

    it "CLI overrides win over both file layers" $
      withTempHosts globalJson $ \globalPath ->
        withTempHosts repoJson $ \repoPath -> do
          Right global <- loadHostsFrom globalPath
          Right repo <- loadHostsFrom repoPath
          let cli = [(X86_64Linux, hostFromText "cli-linux")]
          let merged = mergeHostOverrides cli . mergeHostOverrides (hostsToList repo) $ global
          display <$> lookupHost X86_64Linux merged `shouldBe` Just "cli-linux"
          display <$> lookupHost Aarch64Darwin merged `shouldBe` Just "global-mac"

    it "platforms only in the repo file appear in the merged result" $
      withTempHosts globalOnlyLinuxJson $ \globalPath ->
        withTempHosts repoOnlyDarwinJson $ \repoPath -> do
          Right global <- loadHostsFrom globalPath
          Right repo <- loadHostsFrom repoPath
          let merged = mergeHostOverrides (hostsToList repo) global
          display <$> lookupHost X86_64Linux merged `shouldBe` Just "global-linux"
          display <$> lookupHost Aarch64Darwin merged `shouldBe` Just "repo-mac"

  describe "hostsToList" $
    it "round-trips through loadHostsFrom + mergeHostOverrides" $
      withTempHosts wellFormedJson $ \path -> do
        Right hs <- loadHostsFrom path
        -- Re-overlaying a Hosts on top of itself is a no-op (left-biased
        -- union of identical keys), so the resulting hostsPlatforms set
        -- is unchanged.
        let reflowed = mergeHostOverrides (hostsToList hs) hs
        hostsPlatforms reflowed `shouldMatchList` hostsPlatforms hs

-- | Write @bs@ to a fresh file in the system tempdir, run @k@ with the
-- path, then remove the file. Used to feed 'loadHostsFrom' fixture
-- content without relying on an external @test/fixtures/@ tree (the
-- fixtures are tiny and inline visibility beats indirection here).
withTempHosts :: BS.ByteString -> (FilePath -> IO a) -> IO a
withTempHosts bs k = do
  tmpDir <- getTemporaryDirectory
  bracket
    ( do
        (path, h) <- openBinaryTempFile tmpDir "justci-hosts.json"
        BS.hPut h bs
        hClose h
        pure path
    )
    removeFile
    k

wellFormedJson :: BS.ByteString
wellFormedJson =
  "{ \"x86_64-linux\": \"builder.example.com\", \"aarch64-darwin\": \"mac-runner.example.com\" }"

withUnknownKeyJson :: BS.ByteString
withUnknownKeyJson =
  "{ \"x86_64-linux\": \"builder.example.com\", \"riscv64-linux\": \"future.example.com\" }"

globalJson :: BS.ByteString
globalJson =
  "{ \"x86_64-linux\": \"global-linux\", \"aarch64-darwin\": \"global-mac\" }"

repoJson :: BS.ByteString
repoJson =
  "{ \"x86_64-linux\": \"repo-linux\" }"

globalOnlyLinuxJson :: BS.ByteString
globalOnlyLinuxJson =
  "{ \"x86_64-linux\": \"global-linux\" }"

repoOnlyDarwinJson :: BS.ByteString
repoOnlyDarwinJson =
  "{ \"aarch64-darwin\": \"repo-mac\" }"
