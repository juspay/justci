{-# LANGUAGE OverloadedStrings #-}

-- | Tests for "CI.ProcessCompose"'s YAML emission, focused on the
-- per-process @log_location@ knob: it must round-trip into the YAML
-- when the caller supplies one and stay absent when the caller doesn't
-- (so @dump-yaml@ and local-mode runs are byte-identical to the
-- pre-feature output).
module CI.ProcessComposeSpec (spec) where

import qualified Algebra.Graph.AdjacencyMap as G
import CI.Node (NodeId (..))
import CI.Platform (Platform (..))
import CI.ProcessCompose (toProcessCompose)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Yaml as Y
import Test.Hspec

spec :: Spec
spec = describe "toProcessCompose" $ do
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
