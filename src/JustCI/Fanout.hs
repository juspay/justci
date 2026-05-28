{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Recipe-graph fanout + user-selector filtering. Owns the pure
-- shape change from a recipe DAG ('AdjacencyMap RecipeName') to a
-- per-platform node DAG ('AdjacencyMap NodeId'), plus the optional
-- restriction down to a user-chosen subset.
--
-- Three responsibilities, all on the same volatility axis ("how do
-- we lay out the runner DAG once we know which recipes + platforms +
-- hosts to combine"):
--
--   * 'pipelinePlatformsFor' — derive the set of target platforms
--     from the root recipe's OS attributes intersected with the
--     hosts.json config.
--   * 'fanOut' — cross-product the recipe graph with the platform
--     set; add per-remote-platform setup nodes and their incoming
--     dependencies.
--   * 'applySelectors' — restrict the fanned-out graph to the user's
--     @ci run RECIPE[\@PLATFORM]...@ + @--no-deps@ selectors.
--
-- Pure functions throughout — no IO, no @die@. Selector-resolution
-- failures surface as 'SelectionError' through @Either@.
module JustCI.Fanout
  ( -- * Platform-set derivation
    pipelinePlatformsFor,
    rootOsFamilies,

    -- * Recipe → node-id fanout
    fanOut,
    isRemote,

    -- * Selector filtering
    SelectionError,
    applySelectors,
  )
where

import qualified Algebra.Graph.AdjacencyMap as G
import qualified Algebra.Graph.AdjacencyMap.Algorithm as G
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Text.Display (Display (..))
import JustCI.Hosts (Hosts, hostsPlatforms, lookupHost)
import JustCI.Justfile (Attribute (..), Recipe (..), RecipeName)
import qualified JustCI.Justfile as J
import JustCI.Node (DepsMode (..), NodeId (..), NodeSelector (..), SelectorMode (..))
import JustCI.Platform (Platform, platformOs)

-- | The pipeline's platform set: the intersection of (the root
-- recipe's declared OS families) ∩ (the systems we have either a
-- hosts.json entry for OR are running on locally) ∩ (the user's
-- @--platform@ subset, when non-empty).
--
--   * Root @[linux] [macos]@ + local @x86_64-linux@ + hosts.json
--     @{aarch64-darwin: ...}@ → @{x86_64-linux, aarch64-darwin}@.
--   * Root @[linux]@ + local @x86_64-linux@ + empty hosts.json →
--     @{x86_64-linux}@ (local-only, no macos).
--   * Root with no OS attrs → @{localPlat}@ (single-host shape,
--     same as before fanout existed).
--   * Same fanout + @--platform x86_64-linux@ → @{x86_64-linux}@
--     (user-driven subset; partial drop is silent, full empty
--     surfaces as 'JustCI.Pipeline.EmptyFanout' with the override
--     named in the message).
--
-- A system in @hosts.json@ whose OS family doesn't appear in the root
-- attributes is silently ignored — the user opts in by adding the OS
-- attribute to the root. Symmetrically, an OS family in the root that
-- matches no configured system is silently empty for that family —
-- the user opts in by adding an entry to @hosts.json@. The user's
-- @--platform@ list follows the same silent-drop rule: requested
-- platforms outside the natural fanout don't error here; the run
-- proceeds with whatever subset of the request survives the
-- intersection (or fails through 'EmptyFanout' if nothing does).
pipelinePlatformsFor :: [Platform] -> Recipe -> Platform -> Hosts -> [Platform]
pipelinePlatformsFor userFilter root localPlat hosts =
  let configured = nub (localPlat : hostsPlatforms hosts)
      natural = case rootOsFamilies root of
        [] -> [localPlat]
        oss -> filter (\p -> platformOs p `elem` oss) configured
   in case userFilter of
        [] -> natural
        xs -> filter (`elem` xs) natural

-- | The OS-family attributes declared on a recipe ('[linux]',
-- '[macos]', etc.), as a plain list. Used both by
-- 'pipelinePlatformsFor' (to compute the fanout set) and by the
-- empty-fanout error in "JustCI.Pipeline" (to tell the user which
-- families couldn't be satisfied).
rootOsFamilies :: Recipe -> [J.Os]
rootOsFamilies r = [o | Os o <- r.attributes]

-- | Cross-product the recipe DAG with the pipeline's platform set:
-- one 'NodeId' per @(recipe, platform)@, edges replicated
-- lane-by-lane with no cross-platform connections. Each remote
-- platform also gets a synthetic @_ci-setup\@\<platform\>@ node;
-- every recipe node on that platform @depends_on@ it. The setup
-- node ships the @just@ derivation + a fresh @git bundle@ once per
-- remote per run; recipe nodes reuse the cached checkout.
--
-- Platforms that route inline ('localPlat' with no hosts entry)
-- don't need a setup node — there's no bundle to ship, no remote
-- clone to coordinate.
--
-- Lanes run independently; a failure on linux doesn't block macos
-- and vice versa (and the cross-lane failure tolerance
-- @restart: no@ / @exit_on_skipped: false@ in 'JustCI.ProcessCompose'
-- carries that through).
fanOut :: Platform -> Hosts -> [Platform] -> G.AdjacencyMap RecipeName -> G.AdjacencyMap NodeId
fanOut localPlat hosts platforms g =
  recipeVertices
    `G.overlay` G.edges recipeEdges
    `G.overlay` G.vertices setupVertices
    `G.overlay` G.edges setupEdges
  where
    recipeVertices = G.vertices [RecipeNode r p | r <- G.vertexList g, p <- platforms]
    recipeEdges = [(RecipeNode r p, RecipeNode d p) | (r, d) <- G.edgeList g, p <- platforms]
    -- Remote platforms: anything with a hosts entry runs over SSH.
    -- A local platform with a hosts entry counts as remote (the
    -- host-override case).
    remotePlatforms = filter (`isRemote` (localPlat, hosts)) platforms
    setupVertices = [SetupNode p | p <- remotePlatforms]
    -- Every recipe node on a remote platform depends on that
    -- platform's setup node.
    setupEdges =
      [ (RecipeNode r p, SetupNode p)
      | r <- G.vertexList g,
        p <- remotePlatforms
      ]

-- | A platform routes through SSH if it has a hosts.json entry, or
-- if it isn't the local platform (the latter shouldn't happen post-
-- 'pipelinePlatformsFor' filtering, but the check is cheap and
-- explicit).
isRemote :: Platform -> (Platform, Hosts) -> Bool
isRemote p (localPlat, hosts) =
  isJust (lookupHost p hosts) || p /= localPlat

-- | A user-supplied 'NodeSelector' that doesn't resolve to any node
-- in the fanned-out graph. Carries the selector verbatim so the
-- error message echoes the exact token from argv.
newtype SelectionError = SelectorNotInPipeline NodeSelector
  deriving stock (Show)

instance Display SelectionError where
  displayBuilder (SelectorNotInPipeline s) =
    "selector "
      <> displayBuilder s
      <> " did not match any node in the pipeline DAG (check the root, OS attributes, and hosts.json)"

-- | Restrict the fanned-out graph to the user's selector mode.
--
--   * 'AllNodes' → identity. The pipeline runs the full DAG.
--   * 'SelectedLeaves' with 'WithDeps': keep each seed plus everything
--     reachable from it along the runner DAG's @depends_on@ edges. On
--     remote platforms this auto-includes the 'SetupNode' because
--     every recipe node depends on it.
--   * 'SelectedLeaves' with 'NoDeps': keep only the seeds. The
--     setup-node auto-include still applies — running a remote recipe
--     without its setup would emit a dangling @depends_on@ in the YAML.
--
-- @SelRecipe@ seeds expand to every @(recipe, platform)@ pair in the
-- pipeline's platform set; @SelRecipePlatform@ seeds pin to one.
applySelectors ::
  SelectorMode ->
  [Platform] ->
  G.AdjacencyMap NodeId ->
  Either SelectionError (G.AdjacencyMap NodeId)
applySelectors AllNodes _ g = Right g
applySelectors (SelectedLeaves selectors depsMode) platforms g = do
  seeds <- nub . concat <$> traverse (resolveSelector allNodes platforms) (NE.toList selectors)
  let baseKeep = case depsMode of
        NoDeps -> Set.fromList seeds
        WithDeps -> Set.fromList (concatMap (G.reachable g) seeds)
      -- Auto-include the SetupNode for any selected remote-platform
      -- recipe even under 'NoDeps', so the emitted YAML never
      -- references a setup node we dropped.
      requiredSetup =
        Set.fromList
          [ SetupNode p
          | RecipeNode _ p <- Set.toList baseKeep,
            SetupNode p `Set.member` allNodes
          ]
      keep = baseKeep `Set.union` requiredSetup
  pure (G.induce (`Set.member` keep) g)
  where
    allNodes = G.vertexSet g

-- | Map one 'NodeSelector' onto the matching 'NodeId's in the
-- fanned-out graph, failing with 'SelectorNotInPipeline' if the
-- selector resolves to nothing. A bare 'SelRecipe' fans out across
-- every pipeline platform present in the graph; a
-- 'SelRecipePlatform' pins to the exact pair. Internal to this
-- module — 'applySelectors' is the single entry point.
resolveSelector ::
  Set.Set NodeId ->
  [Platform] ->
  NodeSelector ->
  Either SelectionError [NodeId]
resolveSelector allNodes platforms = \case
  SelRecipe r ->
    let candidates = [RecipeNode r p | p <- platforms]
        present = filter (`Set.member` allNodes) candidates
     in if null present
          then Left (SelectorNotInPipeline (SelRecipe r))
          else Right present
  s@(SelRecipePlatform r p) ->
    if RecipeNode r p `Set.member` allNodes
      then Right [RecipeNode r p]
      else Left (SelectorNotInPipeline s)
