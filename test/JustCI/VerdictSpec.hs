{-# LANGUAGE OverloadedStrings #-}

-- | Tests for "JustCI.Verdict"'s pure verdict logic. The 'recordOutcome'
-- side of the module is exercised end-to-end by the smoke test in
-- @ci.just@; this spec covers 'verdictCode' and 'verdictSummary'
-- against handcrafted outcome maps so the exit-code + summary-line
-- contracts are locked down without spinning up process-compose.
module JustCI.VerdictSpec (spec) where

import Data.Foldable (for_)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text.Display (display)
import JustCI.CommitStatus (terminalToCommitStatus)
import JustCI.Gh (CommitStatus (Failure, Pending, Success))
import JustCI.Justfile (RecipeName)
import JustCI.Node (NodeId (..))
import JustCI.Platform (Platform (..))
import JustCI.ProcessCompose.Events (TerminalStatus (..))
import JustCI.Verdict (RecipeOutcome (..), terminalToOutcome, verdictCode, verdictSummary)
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

    it "is ExitFailure 1 when any node was skipped (upstream cascade)" $
      -- Mirrors GitHub's required-check semantics: a Pending check
      -- blocks merge, so the local exit code must agree and refuse
      -- to call the run successful while a skipped recipe is on
      -- the books.
      verdictCode (Map.fromList [(nodeLinux "a", Just Succeeded), (nodeLinux "b", Just Skipped)])
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

    it "renders Just Skipped as 'skipped' (cascade — distinct from 'failed')" $ do
      -- Same vocabulary the GH commit status uses for the cascade
      -- case. Failed recipes still render as "failed".
      let joined = summary [(nodeLinux "alpha", Just Skipped), (nodeLinux "beta", Just Failed)]
      ("alpha" `T.isInfixOf` joined && "skipped" `T.isInfixOf` joined) `shouldBe` True
      ("beta" `T.isInfixOf` joined && "failed" `T.isInfixOf` joined) `shouldBe` True

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
  -- ('terminalToOutcome' in JustCI.Verdict, 'terminalToCommitStatus'
  -- in JustCI.CommitStatus) must agree on the per-constructor
  -- projection — every 'TerminalStatus' value maps to exactly one
  -- 'CommitStatus' and one 'RecipeOutcome', and the two consumers'
  -- semantics line up. Iterating @[minBound..maxBound]@ traps any
  -- future drift: adding a fourth 'TerminalStatus' constructor would
  -- force an update on this table or surface as a non-exhaustive
  -- pattern in one of the projection functions.
  describe "TerminalStatus" $ do
    it "TsSucceeded projects to Succeeded / Success" $ do
      terminalToOutcome TsSucceeded `shouldBe` Succeeded
      terminalToCommitStatus TsSucceeded `shouldBe` Success

    it "TsFailed projects to Failed / Failure" $ do
      terminalToOutcome TsFailed `shouldBe` Failed
      terminalToCommitStatus TsFailed `shouldBe` Failure

    it "TsSkipped projects to Skipped / Pending" $ do
      -- The cascade case: GH stays Pending (not Failure) so the
      -- check is visibly "not yet met" rather than red; the CLI
      -- summary calls it "skipped" so the two surfaces describe
      -- the same wire-state with consistent vocabulary.
      terminalToOutcome TsSkipped `shouldBe` Skipped
      terminalToCommitStatus TsSkipped `shouldBe` Pending

    it "every TerminalStatus value has a defined projection on both sides" $
      -- Catches the "added a constructor, forgot one side" mistake.
      -- The bodies trigger pattern-match exhaustiveness; the
      -- iteration confirms both functions are total.
      for_ [minBound .. maxBound :: TerminalStatus] $ \ts -> do
        terminalToOutcome ts `shouldSatisfy` (`elem` [Succeeded, Failed, Skipped])
        terminalToCommitStatus ts `shouldSatisfy` (`elem` [Success, Failure, Pending])
