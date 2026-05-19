{-# LANGUAGE OverloadedStrings #-}

-- | Tests for "JustCI.CommitStatus"'s human-readable duration formatter.
-- The actual posting path ('postStatusFor', 'seedPending') talks to
-- the GitHub API and isn't exercised here; this spec locks down the
-- pure formatting that the description field embeds.
module JustCI.CommitStatusSpec (spec) where

import JustCI.CommitStatus (formatElapsed)
import Test.Hspec

spec :: Spec
spec = describe "formatElapsed" $ do
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
