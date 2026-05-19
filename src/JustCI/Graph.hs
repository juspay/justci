{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Recipe-graph computations. Two responsibilities live here:
--
--   * 'reachableSubgraph' — a stable, pure graph operation: restrict a
--     recipe map to those reachable from a root.
--
--   * 'lowerToRunnerGraph' — the cross-system translation axis: encode
--     @just@'s sequential-vs-parallel dependency semantics as explicit
--     edges in a runner-friendly DAG. This is where any future change of
--     orchestrator-shaped semantics (today: process-compose-style
--     per-node @depends_on@) lands.
module JustCI.Graph
  ( -- * Reachability
    reachableSubgraph,
    ReachError,

    -- * just → runner-graph translation
    lowerToRunnerGraph,
    OrderingConflict,
  )
where

import qualified Algebra.Graph.AdjacencyMap as G
import qualified Algebra.Graph.AdjacencyMap.Algorithm as G
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Display (Display (..), display)
import JustCI.Justfile (Attribute (..), Dep (..), Recipe (..), RecipeName)

-- | Failures from 'reachableSubgraph'.
data ReachError = MissingRecipe RecipeName
  deriving stock (Show)

instance Display ReachError where
  displayBuilder (MissingRecipe r) =
    "recipe " <> displayBuilder r <> " not found in justfile"

-- | The recipes reachable from @root@ along their declared dependencies. Returns 'Left' if @root@ isn't a key of the input map.
reachableSubgraph :: RecipeName -> Map.Map RecipeName Recipe -> Either ReachError (Map.Map RecipeName Recipe)
reachableSubgraph root g
  -- Reject missing roots up front; G.reachable on an absent vertex
  -- silently returns [root], which would yield a one-vertex graph.
  | Map.notMember root g = Left $ MissingRecipe root
  | otherwise = Right $ Map.restrictKeys g keep
  where
    recipeGraph =
      G.stars
        [ (name, [d.recipe | d <- r.dependencies])
        | (name, r) <- Map.toList g
        ]
    keep = Set.fromList $ G.reachable recipeGraph root

-- | The recipes cannot be linearized: their dependencies form a cycle.
-- Carries the cycling recipes in the order @topSort@ returned them.
newtype OrderingConflict = OrderingConflict {cycleNodes :: NE.NonEmpty RecipeName}
  deriving stock (Show)

instance Display OrderingConflict where
  displayBuilder (OrderingConflict c) =
    "recipe dependencies form a cycle: "
      <> displayBuilder (T.intercalate " -> " (display <$> NE.toList c))

-- | Lower a recipe map to a runner-friendly DAG, translating @just@'s
-- ordering semantics into explicit edges the orchestrator can consume.
--
-- Per recipe @R@ with deps @[d1..dn]@:
--
--   * If @R@ has @[parallel]@ (or has ≤ 1 dep): @R@ depends on every dep
--     directly; siblings are unconstrained, so the runner can fan out.
--
--   * Otherwise (sequential semantics, which is just's default): @R@ depends
--     only on the last dep; each later dep is augmented to depend on its
--     predecessor, forming a chain @d1 ← d2 ← … ← dn ← R@. Augmenting
--     /shared/ dep nodes is the only way to express caller-side ordering when
--     the downstream runner only understands per-node @depends_on@; the
--     cycle check guards against contradictory chains from different callers.
lowerToRunnerGraph ::
  Map.Map RecipeName Recipe ->
  Either OrderingConflict (G.AdjacencyMap RecipeName)
lowerToRunnerGraph recipes =
  case G.topSort g of
    Left c -> Left (OrderingConflict c)
    Right _ -> Right g
  where
    g = G.vertices (Map.keys recipes) `G.overlay` G.edges (concatMap callerEdges $ Map.toList recipes)

callerEdges :: (RecipeName, Recipe) -> [(RecipeName, RecipeName)]
callerEdges (name, r) =
  case NE.nonEmpty [d.recipe | d <- r.dependencies] of
    Nothing -> []
    Just deps
      | isParallel r.attributes || isSingleton deps ->
          [(name, d) | d <- NE.toList deps]
      | otherwise ->
          let depList = NE.toList deps
              chain = zip (drop 1 depList) depList
           in (name, NE.last deps) : chain
  where
    isSingleton (_ NE.:| rest) = null rest

isParallel :: [Attribute] -> Bool
isParallel = any (\case Parallel -> True; _ -> False)
