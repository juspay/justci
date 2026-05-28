-- | Tests for "JustCI.Pipeline"'s 'policyShape' — the pure flag-folding
-- step that decides, given the user's three opt-out flags, which of
-- the three valid 'PolicyShape' constructors the run lands on
-- ('DevMode', 'StrictNoPost', 'FullStrict'). Owns the contract the
-- strict-by-default flip introduced (default → 'FullStrict';
-- @--no-strict@ \/ @--no-snapshot@ → 'DevMode'; @--no-post@ alone
-- → 'StrictNoPost').
--
-- 'resolveRunPolicy' itself stays untested at this layer — its only
-- behaviour past the shape verdict is calling @gh@\/@git@ subprocesses,
-- which 'policyShape' factors out so the boolean rules can be
-- exercised without any IO faking.
module JustCI.PipelineSpec (spec) where

import JustCI.CLI (RunOpts (..))
import JustCI.Node (defaultDagSelection)
import JustCI.Pipeline (PolicyShape (..), policyShape)
import JustCI.Transport (defaultCacheTtlHours)
import Test.Hspec

-- | A 'RunOpts' value with the three policy flags pinned; the other
-- fields take harmless defaults that 'policyShape' never consults.
-- Constructed here rather than via a smart constructor in the
-- library because the test is the only consumer of the
-- "everything-else-defaulted" shape.
mkOpts :: Bool -> Bool -> Bool -> RunOpts
mkOpts ns nss np =
  RunOpts
    { tui = False,
      hostOverrides = [],
      dagSelection = defaultDagSelection,
      cacheTtlHours = defaultCacheTtlHours,
      noStrict = ns,
      noSnapshot = nss,
      noPost = np
    }

spec :: Spec
spec = describe "policyShape" $ do
  it "default (no flags) is FullStrict — snapshot + GH posts" $
    policyShape (mkOpts False False False) `shouldBe` FullStrict

  it "--no-post alone is StrictNoPost — snapshot, no GH posts" $
    policyShape (mkOpts False False True) `shouldBe` StrictNoPost

  it "--no-snapshot is DevMode (subsumes --no-post — post without snapshot violates the SHA-matches-tested-bytes invariant)" $
    policyShape (mkOpts False True False) `shouldBe` DevMode

  it "--no-snapshot + --no-post is the same as --no-snapshot alone (--no-post is redundant)" $
    policyShape (mkOpts False True True) `shouldBe` DevMode

  it "--no-strict is the dev-mode meta — equivalent to --no-snapshot + --no-post" $
    policyShape (mkOpts True False False) `shouldBe` DevMode

  it "--no-strict subsumes the other two flags regardless of their values" $ do
    policyShape (mkOpts True True False) `shouldBe` DevMode
    policyShape (mkOpts True False True) `shouldBe` DevMode
    policyShape (mkOpts True True True) `shouldBe` DevMode

  it "only ever resolves to one of the three valid constructors (no incoherent fourth state, by construction)" $ do
    let allFlagCombos = [(ns, nss, np) | ns <- [False, True], nss <- [False, True], np <- [False, True]]
        landsOnValidConstructor flags =
          policyShape (uncurry3 mkOpts flags) `elem` [DevMode, StrictNoPost, FullStrict]
        uncurry3 f (a, b, c) = f a b c
    mapM_ (`shouldSatisfy` landsOnValidConstructor) allFlagCombos
