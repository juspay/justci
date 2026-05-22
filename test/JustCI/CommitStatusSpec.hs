{-# LANGUAGE OverloadedStrings #-}

-- | Tests for "JustCI.CommitStatus"'s pure description-building helpers:
-- 'formatElapsed' (human-readable durations) and 'describePost'
-- (the wire-status → @(commit-status, description)@ classifier the
-- per-event poster routes through). The actual posting path
-- ('postStatusFor', 'seedPending') talks to the GitHub API and isn't
-- exercised here; this spec locks down the pure formatting that the
-- description field embeds.
module JustCI.CommitStatusSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import JustCI.CommitStatus (describePost, formatElapsed, isBodyBearing, isPostable)
import JustCI.Gh (CommitStatus (..))
import JustCI.Justfile (Recipe (..))
import JustCI.Node (NodeId (..))
import JustCI.Platform (Platform (..))
import JustCI.ProcessCompose.Events (ProcessState (..), ProcessStatus (..))
import Test.Hspec

ps :: ProcessStatus -> Int -> ProcessState
ps s code = ProcessState {name = "build@x86_64-linux", status = s, exit_code = code}

logP :: FilePath
logP = ".ci/abc1234/x86_64-linux/build.log"

logPT :: T.Text
logPT = T.pack logP

spec :: Spec
spec = do
  describe "formatElapsed" $ do
    it "renders sub-minute durations in seconds" $ do
      formatElapsed 0 `shouldBe` "0s"
      formatElapsed 12 `shouldBe` "12s"
      formatElapsed 59 `shouldBe` "59s"

    it "rounds sub-second durations down to 0s" $
      formatElapsed 0.4 `shouldBe` "0s"

    it "renders minute-scale durations as <m>m<s>s" $ do
      formatElapsed 60 `shouldBe` "1m0s"
      formatElapsed 125 `shouldBe` "2m5s"
      formatElapsed (45 * 60 + 30) `shouldBe` "45m30s"

    it "renders hour-scale durations as <h>h<m>m (seconds dropped)" $ do
      formatElapsed 3600 `shouldBe` "1h0m"
      formatElapsed (3600 + 12 * 60 + 30) `shouldBe` "1h12m"
      formatElapsed (2 * 3600 + 5 * 60) `shouldBe` "2h5m"

  describe "describePost" $ do
    it "drops non-terminal PsOther events" $
      describePost (ps (PsOther "Pending") 0) Nothing logP `shouldBe` Nothing

    it "posts Pending with the recipe's log path for PsRunning" $
      describePost (ps PsRunning 0) Nothing logP
        `shouldBe` Just (Pending, "Running: " <> logPT)

    it "posts Success with elapsed and the recipe's log path for PsCompleted exit=0" $
      describePost (ps PsCompleted 0) (Just 12) logP
        `shouldBe` Just (Success, "Succeeded (12s): " <> logPT)

    it "posts Failure with elapsed and the recipe's log path for PsCompleted non-zero" $
      describePost (ps PsCompleted 1) (Just 5) logP
        `shouldBe` Just (Failure, "Failed (5s): " <> logPT)

    it "posts Failure with a path-free 'Skipped (upstream failed)' for PsSkipped" $
      -- Regression for issue #26: previously this embedded the recipe's
      -- log path even though the recipe never ran and the file never
      -- existed on disk.
      describePost (ps PsSkipped 0) Nothing logP
        `shouldBe` Just (Failure, "Skipped (upstream failed)")

    it "posts Failure with a path-free 'Errored (did not start)' for PsErrored" $
      describePost (ps PsErrored 0) Nothing logP
        `shouldBe` Just (Failure, "Errored (did not start)")

    it "omits the elapsed suffix from skipped/errored posts (recipe never ran)" $ do
      -- Even if the caller passes an elapsed value, did-not-run states
      -- suppress it — there's no meaningful runtime to report.
      describePost (ps PsSkipped 0) (Just 99) logP
        `shouldBe` Just (Failure, "Skipped (upstream failed)")
      describePost (ps PsErrored 0) (Just 99) logP
        `shouldBe` Just (Failure, "Errored (did not start)")

  -- Pair of NodeId filters serving the GH-status surface and the
  -- local verdict-summary surface respectively. Both consult the
  -- recipe map; they differ only on whether SetupNodes pass.
  describe "isPostable / isBodyBearing" $ do
    let mkRecipe nm bdy = Recipe {namepath = nm, dependencies = [], parameters = [], attributes = [], body = bdy}
        recipes =
          Map.fromList
            [ ("work", mkRecipe "work" [["echo hi"]]),
              ("agg", mkRecipe "agg" [])
            ]
        workNode = RecipeNode "work" X86_64Linux
        aggNode = RecipeNode "agg" X86_64Linux
        setupNode = SetupNode X86_64Linux

    it "isPostable keeps body-bearing recipe nodes" $
      isPostable recipes workNode `shouldBe` True

    it "isPostable drops pure-aggregator recipe nodes" $
      isPostable recipes aggNode `shouldBe` False

    it "isPostable drops setup nodes (internal plumbing)" $
      isPostable recipes setupNode `shouldBe` False

    it "isBodyBearing matches isPostable on the two recipe-node cases" $ do
      isBodyBearing recipes workNode `shouldBe` True
      isBodyBearing recipes aggNode `shouldBe` False

    it "isBodyBearing keeps setup nodes — they pass the body axis vacuously" $
      -- The verdict surface keeps SetupNodes (they're rendered in their
      -- own Setup section); isBodyBearing's vacuous True for them is
      -- what lets the same predicate seed the verdict outcomes map.
      isBodyBearing recipes setupNode `shouldBe` True
