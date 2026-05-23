{-# LANGUAGE OverloadedStrings #-}

-- | Tests for "JustCI.CommitStatus"'s pure description-building helpers:
-- 'formatElapsed' (human-readable durations) and 'describePost'
-- (the wire-status → @(commit-status, description)@ classifier the
-- per-event poster routes through). The actual posting path
-- ('postStatusFor') talks to the GitHub API and isn't exercised
-- here; this spec locks down the pure formatting that the
-- description field embeds and the two policy predicates that
-- decide which nodes flow through it.
module JustCI.CommitStatusSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import JustCI.CommitStatus (describePost, formatElapsed, isBodyBearing, isRequiredCheck, shouldPostStatus)
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

    it "drops PsSkipped events (no post — defers to branch-protection placeholder)" $
      -- Cascade case: this recipe never ran because an upstream dep
      -- failed. We do not post; GitHub's own "Expected — Waiting for
      -- status to be reported" placeholder (driven by the
      -- required-checks list 'runProtect' configured) is what keeps
      -- the row visible and the merge gate closed. Posting our own
      -- @Pending@+@"Skipped"@ row would overwrite a prior @Success@
      -- on a partial re-run, since pc re-emits PsSkipped for
      -- downstreams of any re-running failed leaf.
      describePost (ps PsSkipped 0) Nothing logP `shouldBe` Nothing

    it "drops PsSkipped even when an elapsed value is supplied" $
      describePost (ps PsSkipped 0) (Just 99) logP `shouldBe` Nothing

    it "posts Failure with a path-free 'Errored (did not start)' for PsErrored" $
      -- Launch failure (pc tried to start the process and couldn't) —
      -- this *is* a defect in the recipe's launch path, not a
      -- cascade, so it stays red. Asymmetric with PsSkipped on
      -- purpose.
      describePost (ps PsErrored 0) Nothing logP
        `shouldBe` Just (Failure, "Errored (did not start)")

    it "omits the elapsed suffix from errored posts (recipe never ran)" $
      -- Even if the caller passes an elapsed value, did-not-run states
      -- suppress it — there's no meaningful runtime to report.
      describePost (ps PsErrored 0) (Just 99) logP
        `shouldBe` Just (Failure, "Errored (did not start)")

  -- Two policy predicates with distinct meanings, plus the shared
  -- body-axis helper. The pair (shouldPostStatus, isRequiredCheck)
  -- differs only on setup nodes — setup nodes /post/ statuses (so a
  -- setup failure shows up on the PR) but are /not/ required checks
  -- (local-only runs would otherwise block merge on a row that was
  -- never scheduled).
  describe "policy predicates" $ do
    let mkRecipe nm bdy = Recipe {namepath = nm, dependencies = [], parameters = [], attributes = [], body = bdy}
        recipes =
          Map.fromList
            [ ("work", mkRecipe "work" [["echo hi"]]),
              ("agg", mkRecipe "agg" [])
            ]
        workNode = RecipeNode "work" X86_64Linux
        aggNode = RecipeNode "agg" X86_64Linux
        setupNode = SetupNode X86_64Linux

    it "shouldPostStatus keeps body-bearing recipe nodes" $
      shouldPostStatus recipes workNode `shouldBe` True

    it "shouldPostStatus drops pure-aggregator recipe nodes" $
      shouldPostStatus recipes aggNode `shouldBe` False

    it "shouldPostStatus keeps setup nodes — failures surface on the PR" $
      shouldPostStatus recipes setupNode `shouldBe` True

    it "isRequiredCheck keeps body-bearing recipe nodes" $
      isRequiredCheck recipes workNode `shouldBe` True

    it "isRequiredCheck drops pure-aggregator recipe nodes" $
      isRequiredCheck recipes aggNode `shouldBe` False

    it "isRequiredCheck drops setup nodes — local-only runs never schedule them" $
      -- The asymmetry against shouldPostStatus: if setup were a
      -- required check, a local-only run (no remote platforms, no
      -- SetupNode in the graph) would permanently block merge on a
      -- row that never received a status.
      isRequiredCheck recipes setupNode `shouldBe` False

    it "isBodyBearing matches isRequiredCheck on the two recipe-node cases" $ do
      isBodyBearing recipes workNode `shouldBe` True
      isBodyBearing recipes aggNode `shouldBe` False

    it "isBodyBearing keeps setup nodes — they pass the body axis vacuously" $
      -- The verdict surface keeps SetupNodes (they're rendered in their
      -- own Setup section); isBodyBearing's vacuous True for them is
      -- what lets the same predicate seed the verdict outcomes map.
      isBodyBearing recipes setupNode `shouldBe` True
