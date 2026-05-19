{-# LANGUAGE OverloadedStrings #-}

-- | Tests for "JustCI.Node"'s 'Display' / 'parseNodeId' round-trip.
-- This is the single wire-to-domain seam for process-compose's
-- process-name strings, so the contract is locked here: every
-- displayable 'NodeId' round-trips, the wire form of the setup
-- prefix routes back to a 'SetupNode' constructor, and
-- unparseable input drops silently (a @Nothing@) rather than
-- crashing the consumer.
module JustCI.NodeSpec (spec) where

import Data.Text.Display (display)
import JustCI.Node (NodeId (..), NodeSelector (..), parseNodeId, parseSelector)
import JustCI.Platform (Platform (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "parseNodeId" $ do
    it "parses <recipe>@x86_64-linux into a RecipeNode" $
      parseNodeId "build@x86_64-linux" `shouldBe` Just (RecipeNode "build" X86_64Linux)

    it "parses <recipe>@aarch64-darwin into a RecipeNode" $
      parseNodeId "build@aarch64-darwin" `shouldBe` Just (RecipeNode "build" Aarch64Darwin)

    it "preserves :: in recipe FQNs" $
      parseNodeId "sub::build@x86_64-linux" `shouldBe` Just (RecipeNode "sub::build" X86_64Linux)

    it "splits on the last @ (recipes never contain @, platforms never contain ::)" $
      parseNodeId "a::b::c@aarch64-darwin" `shouldBe` Just (RecipeNode "a::b::c" Aarch64Darwin)

    it "splits on the LAST @ even when the recipe name itself contains @" $
      parseNodeId "weird@name@x86_64-linux" `shouldBe` Just (RecipeNode "weird@name" X86_64Linux)

    it "recognises the reserved setup-node wire name" $
      parseNodeId "_ci-setup@x86_64-linux" `shouldBe` Just (SetupNode X86_64Linux)

    it "returns Nothing on missing platform suffix" $
      parseNodeId "build" `shouldBe` Nothing

    it "returns Nothing on unknown platform" $
      parseNodeId "build@windows" `shouldBe` Nothing

    it "returns Nothing on empty recipe name" $
      parseNodeId "@x86_64-linux" `shouldBe` Nothing

  describe "Display NodeId" $ do
    it "emits <recipe>@<platform> for recipe nodes" $
      display (RecipeNode "build" X86_64Linux) `shouldBe` "build@x86_64-linux"

    it "preserves :: in recipe FQNs" $
      display (RecipeNode "sub::build" Aarch64Darwin) `shouldBe` "sub::build@aarch64-darwin"

    it "emits the reserved name for setup nodes" $
      display (SetupNode X86_64Linux) `shouldBe` "_ci-setup@x86_64-linux"

  describe "parseSelector" $ do
    it "parses a bare recipe name as SelRecipe" $
      parseSelector "build" `shouldBe` Right (SelRecipe "build")

    it "preserves :: in recipe FQNs" $
      parseSelector "ci::default" `shouldBe` Right (SelRecipe "ci::default")

    it "parses RECIPE@PLATFORM as SelRecipePlatform" $
      parseSelector "build@x86_64-linux" `shouldBe` Right (SelRecipePlatform "build" X86_64Linux)

    it "preserves :: in the recipe part of RECIPE@PLATFORM" $
      parseSelector "ci::build@aarch64-darwin" `shouldBe` Right (SelRecipePlatform "ci::build" Aarch64Darwin)

    it "rejects @ with an unknown platform suffix" $
      case parseSelector "build@windows" of
        Left msg -> msg `shouldContain` "no known platform suffix"
        Right _ -> expectationFailure "expected Left"

    it "rejects an empty selector" $
      case parseSelector "" of
        Left msg -> msg `shouldContain` "empty"
        Right _ -> expectationFailure "expected Left"

    it "rejects @PLATFORM with empty recipe part" $
      case parseSelector "@x86_64-linux" of
        Left msg -> msg `shouldContain` "empty recipe"
        Right _ -> expectationFailure "expected Left"

  describe "round-trip" $ do
    let cases =
          [ RecipeNode "build" X86_64Linux,
            RecipeNode "build" Aarch64Darwin,
            RecipeNode "sub::nested::recipe" X86_64Linux,
            RecipeNode "a-recipe-with-dashes" Aarch64Darwin,
            SetupNode X86_64Linux,
            SetupNode Aarch64Darwin
          ]
    it "parseNodeId . display is Just for every NodeId" $
      mapM_
        (\n -> parseNodeId (display n) `shouldBe` Just n)
        cases
