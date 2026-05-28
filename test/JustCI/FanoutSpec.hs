{-# LANGUAGE OverloadedStrings #-}

-- | Tests for "JustCI.Fanout"'s 'pipelinePlatformsFor' — the
-- three-way intersection ("root OS families ∩ configured systems ∩
-- user @--platform@ filter") that decides which platforms the
-- pipeline fans out to. Recipe construction mirrors the shape
-- 'JustCI.CommitStatusSpec' uses; 'Hosts' construction goes through
-- the public 'hostsFromList' so the newtype constructor stays
-- module-internal.
module JustCI.FanoutSpec (spec) where

import JustCI.Fanout (pipelinePlatformsFor)
import JustCI.Hosts (hostFromText, hostsFromList)
import JustCI.Justfile (Recipe (..))
import qualified JustCI.Justfile as J
import JustCI.Platform (Platform (..))
import Test.Hspec

spec :: Spec
spec = describe "pipelinePlatformsFor" $ do
  let mkRecipe attrs = Recipe {namepath = "ci", dependencies = [], parameters = [], attributes = attrs, body = []}
      noAttrs = mkRecipe []
      linuxRoot = mkRecipe [J.Os J.Linux]
      crossRoot = mkRecipe [J.Os J.Linux, J.Os J.Macos]
      noHosts = hostsFromList []
      darwinHost = hostsFromList [(Aarch64Darwin, hostFromText "mac")]

  describe "natural fanout (no --platform)" $ do
    it "root with no OS attrs collapses to the local platform" $
      pipelinePlatformsFor [] noAttrs X86_64Linux noHosts `shouldBe` [X86_64Linux]

    it "root with [linux] on a linux host with no hosts.json stays single-lane" $
      pipelinePlatformsFor [] linuxRoot X86_64Linux noHosts `shouldBe` [X86_64Linux]

    it "root with [linux] [macos] + darwin host fans out to both lanes" $
      pipelinePlatformsFor [] crossRoot X86_64Linux darwinHost `shouldBe` [X86_64Linux, Aarch64Darwin]

    it "root with [macos] only on a linux host with no darwin entry is empty" $
      pipelinePlatformsFor [] (mkRecipe [J.Os J.Macos]) X86_64Linux noHosts `shouldBe` []

  describe "user --platform filter" $ do
    it "[] is identity (matches the natural fanout exactly)" $
      pipelinePlatformsFor [] crossRoot X86_64Linux darwinHost
        `shouldBe` pipelinePlatformsFor [] crossRoot X86_64Linux darwinHost

    it "singleton picks one lane out of a multi-lane fanout" $
      pipelinePlatformsFor [X86_64Linux] crossRoot X86_64Linux darwinHost `shouldBe` [X86_64Linux]

    it "subset preserves the natural fanout's order" $
      -- crossRoot + darwinHost naturally fans to [X86_64Linux, Aarch64Darwin];
      -- a subset request keeps that order regardless of how the user spelled it.
      pipelinePlatformsFor [Aarch64Darwin, X86_64Linux] crossRoot X86_64Linux darwinHost
        `shouldBe` [X86_64Linux, Aarch64Darwin]

    it "partial drop: requesting a platform outside the natural fanout silently keeps the valid subset" $
      pipelinePlatformsFor [X86_64Linux, Aarch64Darwin] linuxRoot X86_64Linux noHosts
        `shouldBe` [X86_64Linux]

    it "full empty: requesting only platforms outside the natural fanout yields []" $
      pipelinePlatformsFor [Aarch64Darwin] linuxRoot X86_64Linux noHosts `shouldBe` []

    it "filter applies even when root has no OS attrs (locks to localPlat or excludes it)" $ do
      pipelinePlatformsFor [X86_64Linux] noAttrs X86_64Linux noHosts `shouldBe` [X86_64Linux]
      pipelinePlatformsFor [Aarch64Darwin] noAttrs X86_64Linux noHosts `shouldBe` []
