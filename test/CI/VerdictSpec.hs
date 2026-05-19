{-# LANGUAGE OverloadedStrings #-}

-- | Tests for "CI.Verdict"'s pure verdict logic. The 'recordOutcome'
-- side of the module is exercised end-to-end by the smoke test in
-- @ci.just@; this spec covers 'verdictCode' and 'verdictSummary'
-- against handcrafted outcome maps so the exit-code + summary-line
-- contracts are locked down without spinning up process-compose.
module CI.VerdictSpec (spec) where

import CI.CommitStatus (terminalToCommitStatus)
import CI.Gh (CommitStatus (Success))
import CI.Justfile (RecipeName)
import CI.Node (NodeId (..))
import CI.Platform (Platform (..))
import CI.ProcessCompose.Events (TerminalStatus)
import CI.Verdict (RecipeOutcome (..), terminalToOutcome, verdictCode, verdictSummary)
import Data.Foldable (for_)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text.Display (display)
import System.Exit (ExitCode (..))
import Test.Hspec

-- | Convenience: build a X86_64Linux-lane 'RecipeNode' from a bare
-- recipe-name string literal.
nodeLinux :: RecipeName -> NodeId
nodeLinux r = RecipeNode r X86_64Linux

spec :: Spec
spec = do
  describe "verdictCode" $ do
    it "is ExitSuccess when every node succeeded" $
      verdictCode (Map.fromList [(nodeLinux "a", Just Succeeded), (nodeLinux "b", Just Succeeded)])
        `shouldBe` ExitSuccess

    it "is ExitFailure 1 when any node failed" $
      verdictCode (Map.fromList [(nodeLinux "a", Just Succeeded), (nodeLinux "b", Just Failed)])
        `shouldBe` ExitFailure 1

    it "is ExitFailure 1 when any node never reached terminal (Nothing)" $
      verdictCode (Map.fromList [(nodeLinux "a", Just Succeeded), (nodeLinux "b", Nothing)])
        `shouldBe` ExitFailure 1

    it "is ExitSuccess for the empty map" $
      verdictCode Map.empty `shouldBe` ExitSuccess

  describe "verdictSummary" $ do
    let summary = T.unlines . verdictSummary (const "local") . Map.fromList

    it "lists every recipe in the summary" $ do
      let nodes = [(nodeLinux "alpha", Just Succeeded), (nodeLinux "beta", Just Failed)]
          joined = summary nodes
      for_ nodes $ \(RecipeNode r _, _) ->
        (display r `T.isInfixOf` joined) `shouldBe` True

    it "renders Nothing as 'did not run' when the lane's setup is fine" $ do
      let joined = summary [(nodeLinux "alpha", Nothing)]
      ("did not run" `T.isInfixOf` joined) `shouldBe` True

    it "groups recipes by platform lane" $ do
      let nodes = [(RecipeNode "alpha" X86_64Linux, Just Succeeded), (RecipeNode "alpha" Aarch64Darwin, Just Failed)]
          joined = summary nodes
      ("x86_64-linux (local)" `T.isInfixOf` joined) `shouldBe` True
      ("aarch64-darwin (local)" `T.isInfixOf` joined) `shouldBe` True

    it "shows setup nodes in their own Setup section" $ do
      let nodes =
            [ (SetupNode X86_64Linux, Just Succeeded),
              (RecipeNode "build" X86_64Linux, Just Succeeded)
            ]
          joined = summary nodes
      ("Setup" `T.isInfixOf` joined) `shouldBe` True
      ("x86_64-linux" `T.isInfixOf` joined) `shouldBe` True

    it "omits the Setup section when there are no setup nodes" $ do
      let joined = summary [(nodeLinux "build", Just Succeeded)]
      ("Setup" `T.isInfixOf` joined) `shouldBe` False

    it "collapses a lane to 'not scheduled (setup failed)' when its setup failed" $ do
      let nodes =
            [ (SetupNode X86_64Linux, Just Failed),
              (RecipeNode "build" X86_64Linux, Nothing),
              (RecipeNode "test" X86_64Linux, Nothing)
            ]
          joined = summary nodes
      ("not scheduled (setup failed)" `T.isInfixOf` joined) `shouldBe` True
      -- Individual recipe outcomes are suppressed under a failed lane.
      ("build  did not run" `T.isInfixOf` joined) `shouldBe` False

    it "renders per-recipe outcomes for lanes whose setup succeeded" $ do
      let nodes =
            [ (SetupNode Aarch64Darwin, Just Succeeded),
              (RecipeNode "build" Aarch64Darwin, Just Succeeded),
              (RecipeNode "test" Aarch64Darwin, Just Failed)
            ]
          joined = summary nodes
      ("build" `T.isInfixOf` joined) `shouldBe` True
      ("succeeded" `T.isInfixOf` joined) `shouldBe` True
      ("failed" `T.isInfixOf` joined) `shouldBe` True
      ("not scheduled" `T.isInfixOf` joined) `shouldBe` False

    it "counts every scheduled node (setup + recipes) in the bottom tally" $ do
      let nodes =
            [ (SetupNode X86_64Linux, Just Succeeded),
              (RecipeNode "build" X86_64Linux, Just Succeeded),
              (RecipeNode "test" X86_64Linux, Just Succeeded)
            ]
          joined = summary nodes
      ("all 3 nodes succeeded" `T.isInfixOf` joined) `shouldBe` True

  -- Cross-module invariant: the two consumers of 'TerminalStatus'
  -- ('terminalToOutcome' in CI.Verdict, 'terminalToCommitStatus' in
  -- CI.CommitStatus) must agree on which terminal classification
  -- counts as "success".
  describe "TerminalStatus" $
    it "terminalToOutcome and terminalToCommitStatus agree on the success case" $
      for_ [minBound .. maxBound :: TerminalStatus] $ \ts ->
        (terminalToOutcome ts == Succeeded)
          `shouldBe` (terminalToCommitStatus ts == Success)
