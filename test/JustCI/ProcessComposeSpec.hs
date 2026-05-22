{-# LANGUAGE OverloadedStrings #-}

-- | Tests for "JustCI.ProcessCompose": the argv shape produced by
-- 'pcClientArgs' (pinned here so a mis-order surfaces as a test failure
-- rather than a mystery flag error at runtime) and the YAML emission
-- from 'toProcessCompose' (covering @log_location@, process keying, and
-- cross-platform fanout wiring).
module JustCI.ProcessComposeSpec (spec) where

import qualified Algebra.Graph.AdjacencyMap as G
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Yaml as Y
import JustCI.Node (NodeId (..))
import JustCI.Platform (Platform (..))
import JustCI.ProcessCompose (pcClientArgs, toProcessCompose)
import Test.Hspec

spec :: Spec
spec = do
  describe "pcClientArgs" $ do
    -- The pc-client passthrough is the only chance to silently mis-order
    -- argv for the new @justci status / logs / monitor@ subcommands —
    -- the runtime side is a fire-and-forget shell-out, so a typo would
    -- only surface as a downstream agent's "process-compose: unknown
    -- flag" mystery. Pin the argv shape here.
    it "places the canonical socket flags between the pc subcommand and user args" $
      pcClientArgs ".ci/pc.sock" "list" ["-o", "json"]
        `shouldBe` ["process", "list", "-U", "-u", ".ci/pc.sock", "-o", "json"]

    it "forwards a positional process name verbatim after the socket flags" $
      pcClientArgs "/tmp/pc.sock" "logs" ["-f", "ci::e2e@x86_64-linux"]
        `shouldBe` ["process", "logs", "-U", "-u", "/tmp/pc.sock", "-f", "ci::e2e@x86_64-linux"]

    it "handles an empty user-arg list (e.g. @justci monitor@ with no flags)" $
      pcClientArgs ".ci/pc.sock" "monitor" []
        `shouldBe` ["process", "monitor", "-U", "-u", ".ci/pc.sock"]

  describe "toProcessCompose" $ do
    it "emits log_location when the per-node lookup returns Just" $ do
      let yaml = encodeYaml (const (Just ".ci/abc/linux/r.log"))
      yaml `shouldContain` "log_location: .ci/abc/linux/r.log"

    it "omits log_location when the per-node lookup returns Nothing" $ do
      let yaml = encodeYaml (const Nothing)
      yaml `shouldNotContain` "log_location"

    it "keys processes by <recipe>@<platform>" $ do
      let yaml = encodeYaml (const Nothing)
      yaml `shouldContain` "r@x86_64-linux"

    it "emits one process per (recipe, platform) when a recipe is fanned out" $ do
      let g =
            G.vertices
              [RecipeNode "build" X86_64Linux, RecipeNode "build" Aarch64Darwin]
          yaml = encodeMulti (const Nothing) g
      yaml `shouldContain` "build@x86_64-linux"
      yaml `shouldContain` "build@aarch64-darwin"

    it "wires depends_on within a platform lane, never across" $ do
      -- Two lanes, each with build←root: root depends on build. The
      -- linux lane's root must depend_on build@x86_64-linux, not build@aarch64-darwin
      -- (and vice versa). Cross-platform edges are a fanout bug.
      let g =
            G.edges
              [ (RecipeNode "root" X86_64Linux, RecipeNode "build" X86_64Linux),
                (RecipeNode "root" Aarch64Darwin, RecipeNode "build" Aarch64Darwin)
              ]
          yaml = encodeMulti (const Nothing) g
      -- root@x86_64-linux's depends_on block contains build@x86_64-linux
      yaml `shouldContain` "root@x86_64-linux"
      yaml `shouldContain` "build@x86_64-linux"
      yaml `shouldContain` "root@aarch64-darwin"
      yaml `shouldContain` "build@aarch64-darwin"
      -- No cross-lane edges should be emitted
      yaml `shouldNotContain` "build@aarch64-darwin:\n          condition"

-- | Encode a single-vertex 'ProcessCompose' to YAML as a String. One
-- vertex is enough: the per-process @log_location@ field is set
-- vertex-by-vertex, and adding more vertices would just multiply the
-- assertion surface without exercising any new code path.
encodeYaml :: (NodeId -> Maybe FilePath) -> String
encodeYaml mkLog =
  BS8.unpack . Y.encode $
    toProcessCompose (const "echo hi") (const Nothing) mkLog graph
  where
    graph = G.vertex (RecipeNode "r" X86_64Linux)

-- | Variant of 'encodeYaml' that takes a caller-supplied graph so a
-- test can exercise multi-platform fanout shapes.
encodeMulti :: (NodeId -> Maybe FilePath) -> G.AdjacencyMap NodeId -> String
encodeMulti mkLog g =
  BS8.unpack . Y.encode $ toProcessCompose (const "echo hi") (const Nothing) mkLog g
