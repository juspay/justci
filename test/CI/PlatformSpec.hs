{-# LANGUAGE OverloadedStrings #-}

-- | Tests for "CI.Platform"'s 'Display' / 'parsePlatform' surface
-- and the just-OS → 'Platform' bridge. 'localPlatform' isn't
-- exercised here (its 'System.Info.os' / 'System.Info.arch' input is
-- compiled-in, not parameterizable) — the run-check smoke test in
-- @ci.just@ covers it end-to-end.
module CI.PlatformSpec (spec) where

import qualified CI.Justfile as J
import CI.Platform (Platform (..), allPlatforms, osToPlatforms, parsePlatform, platformOs)
import Data.Text.Display (display)
import Test.Hspec

spec :: Spec
spec = do
  describe "parsePlatform" $ do
    it "parses x86_64-linux" $ parsePlatform "x86_64-linux" `shouldBe` Just X86_64Linux
    it "parses aarch64-linux" $ parsePlatform "aarch64-linux" `shouldBe` Just Aarch64Linux
    it "parses aarch64-darwin" $ parsePlatform "aarch64-darwin" `shouldBe` Just Aarch64Darwin
    it "rejects x86_64-darwin (dropped after nixpkgs 26.05)" $ parsePlatform "x86_64-darwin" `shouldBe` Nothing
    it "is case-insensitive" $ parsePlatform "AARCH64-DARWIN" `shouldBe` Just Aarch64Darwin
    it "rejects bare linux (must be a full tuple)" $ parsePlatform "linux" `shouldBe` Nothing
    it "rejects empty" $ parsePlatform "" `shouldBe` Nothing

  describe "Display Platform" $ do
    it "renders x86_64-linux" $ display X86_64Linux `shouldBe` "x86_64-linux"
    it "renders aarch64-darwin" $ display Aarch64Darwin `shouldBe` "aarch64-darwin"

  describe "allPlatforms" $
    it "enumerates every constructor" $
      allPlatforms `shouldBe` [X86_64Linux, Aarch64Linux, Aarch64Darwin]

  describe "platformOs" $ do
    it "maps linux variants to Linux" $ do
      platformOs X86_64Linux `shouldBe` J.Linux
      platformOs Aarch64Linux `shouldBe` J.Linux
    it "maps the supported darwin variant to Macos" $
      platformOs Aarch64Darwin `shouldBe` J.Macos

  describe "osToPlatforms" $ do
    it "expands [linux] to both linux systems" $
      osToPlatforms J.Linux `shouldBe` [X86_64Linux, Aarch64Linux]
    it "expands [macos] to the supported darwin system" $
      osToPlatforms J.Macos `shouldBe` [Aarch64Darwin]
    it "rejects Unix (not a fanout target)" $
      osToPlatforms J.Unix `shouldBe` []
    it "rejects Windows" $
      osToPlatforms J.Windows `shouldBe` []
    it "rejects BSDs" $ do
      osToPlatforms J.Freebsd `shouldBe` []
      osToPlatforms J.Openbsd `shouldBe` []
      osToPlatforms J.Netbsd `shouldBe` []
      osToPlatforms J.Dragonfly `shouldBe` []
