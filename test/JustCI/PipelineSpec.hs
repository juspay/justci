{-# LANGUAGE OverloadedRecordDot #-}

-- | Tests for "JustCI.Pipeline"'s 'policyShape' — the pure flag-folding
-- step that decides, given the user's three opt-out flags, whether
-- the pre-flight (clean-tree + repo + SHA) needs to run and whether
-- the @onState@ callback should fan into 'postStatusFor'. Owns the
-- contract the strict-by-default flip introduced (default mode is
-- @snapshot + post@; @--no-strict@ collapses both; @--no-snapshot@
-- implies @--no-post@; @--no-post@ alone keeps snapshot).
--
-- 'resolveRunPolicy' itself stays untested at this layer — its only
-- behaviour past the shape verdict is calling @gh@/@git@ subprocesses,
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
  it "default (no flags) is full strict: snapshot + post" $
    policyShape (mkOpts False False False)
      `shouldBe` PolicyShape {wantSnapshot = True, wantPost = True}

  it "--no-post alone keeps snapshot but suppresses posts" $
    policyShape (mkOpts False False True)
      `shouldBe` PolicyShape {wantSnapshot = True, wantPost = False}

  it "--no-snapshot drops snapshot AND posts (post without snapshot violates the SHA-matches-tested-bytes invariant)" $
    policyShape (mkOpts False True False)
      `shouldBe` PolicyShape {wantSnapshot = False, wantPost = False}

  it "--no-snapshot + --no-post is the same as --no-snapshot alone (--no-post is redundant)" $
    policyShape (mkOpts False True True)
      `shouldBe` PolicyShape {wantSnapshot = False, wantPost = False}

  it "--no-strict is the dev-mode meta: equivalent to --no-snapshot + --no-post" $
    policyShape (mkOpts True False False)
      `shouldBe` PolicyShape {wantSnapshot = False, wantPost = False}

  it "--no-strict subsumes the other two flags regardless of their values" $ do
    policyShape (mkOpts True True False)
      `shouldBe` PolicyShape {wantSnapshot = False, wantPost = False}
    policyShape (mkOpts True False True)
      `shouldBe` PolicyShape {wantSnapshot = False, wantPost = False}
    policyShape (mkOpts True True True)
      `shouldBe` PolicyShape {wantSnapshot = False, wantPost = False}

  it "preserves the invariant: wantSnapshot=False implies wantPost=False (every case)" $ do
    let allFlagCombos = [(ns, nss, np) | ns <- [False, True], nss <- [False, True], np <- [False, True]]
        invariantHolds (ns, nss, np) =
          let s = policyShape (mkOpts ns nss np)
           in s.wantSnapshot || not s.wantPost
    mapM_ (`shouldSatisfy` invariantHolds) allFlagCombos
