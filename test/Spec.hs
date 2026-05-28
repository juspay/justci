-- | Test-suite entry point: composes the per-module hspec specs into a
-- single run.
module Main (main) where

import qualified JustCI.CommitStatusSpec
import qualified JustCI.FanoutSpec
import qualified JustCI.JustfileSpec
import qualified JustCI.NodeSpec
import qualified JustCI.PlatformSpec
import qualified JustCI.ProcessComposeSpec
import qualified JustCI.TransportSpec
import qualified JustCI.VerdictSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  JustCI.CommitStatusSpec.spec
  JustCI.FanoutSpec.spec
  JustCI.JustfileSpec.spec
  JustCI.NodeSpec.spec
  JustCI.PlatformSpec.spec
  JustCI.ProcessComposeSpec.spec
  JustCI.TransportSpec.spec
  JustCI.VerdictSpec.spec
