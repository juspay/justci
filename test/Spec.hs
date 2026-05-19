-- | Test-suite entry point: composes the per-module hspec specs into a
-- single run.
module Main (main) where

import qualified CI.CommitStatusSpec
import qualified CI.JustfileSpec
import qualified CI.NodeSpec
import qualified CI.PlatformSpec
import qualified CI.ProcessComposeSpec
import qualified CI.TransportSpec
import qualified CI.VerdictSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  CI.CommitStatusSpec.spec
  CI.JustfileSpec.spec
  CI.NodeSpec.spec
  CI.PlatformSpec.spec
  CI.ProcessComposeSpec.spec
  CI.TransportSpec.spec
  CI.VerdictSpec.spec
