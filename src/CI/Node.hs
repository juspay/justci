{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The runner DAG's node identity, as a closed sum over the two kinds
-- of nodes the orchestrator ever schedules:
--
--   * 'SetupNode' — per-platform internal plumbing (the once-per-remote
--     bundle ship + drv copy).
--   * 'RecipeNode' — a user recipe paired with the target platform.
--
-- The kind is *structural*, not name-derived: no consumer infers it
-- from a magic recipe-name prefix, and pattern matches on the sum get
-- '-Wincomplete-patterns' coverage. The one place where the
-- @_ci-setup@ wire-name lives is this module — inside 'Display' and
-- 'parseNodeId'.
--
-- The @\<recipe\>\@\<platform\>@ separator is chosen because recipe
-- FQNs use @::@ (so collisions are impossible) and @\@@ needs no
-- shell quoting in any consumer. Kolu uses the same convention for
-- its GitHub commit-status contexts.
module CI.Node
  ( -- * Identity
    NodeId (..),
    nodePlatform,
    nodeName,

    -- * Wire round-trip
    parseNodeId,

    -- * User-input selectors
    NodeSelector (..),
    parseSelector,

    -- * Graph rendering
    toMermaid,
  )
where

import qualified Algebra.Graph.AdjacencyMap as G
import CI.Justfile (RecipeName, recipeNameFromText)
import CI.Platform (Platform, parsePlatform)
import Data.Aeson (ToJSON (..), ToJSONKey (..))
import Data.Aeson.Types (toJSONKeyText)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (Display (..), display)

-- | A scheduled DAG node. Setup and recipe nodes share the per-platform
-- attribute (both fan out across the pipeline's platform set) but only
-- recipe nodes carry a 'RecipeName' — setup nodes are uniquely
-- identified by their platform. Pulling them apart removes the
-- "is this a setup recipe by name?" runtime check and pushes the
-- distinction into pattern-match exhaustiveness.
data NodeId
  = SetupNode Platform
  | RecipeNode RecipeName Platform
  deriving stock (Show, Eq, Ord)

-- | The platform a node targets. Both constructors carry one; this
-- helper saves callers a four-line pattern match when they only
-- need that field.
nodePlatform :: NodeId -> Platform
nodePlatform = \case
  SetupNode p -> p
  RecipeNode _ p -> p

-- | The recipe-name portion of the wire identity (no @\@\<platform\>@
-- suffix). Setup nodes return the reserved 'setupNodeName' constant;
-- recipe nodes return their 'RecipeName' rendered as text. Used by
-- on-disk log-path construction in 'CI.LogPath' where the platform
-- already lives in the directory component.
nodeName :: NodeId -> Text
nodeName = \case
  SetupNode _ -> setupNodeName
  RecipeNode r _ -> display r

-- | The wire name reserved for setup nodes. Only used inside this
-- module — by 'Display' (to render the @\<name\>\@\<platform\>@ form)
-- and by 'parseNodeId' (to recognise setup-node wire inputs). Every
-- other module consumes the kind via pattern matching on 'NodeId',
-- not by string comparison.
setupNodeName :: Text
setupNodeName = "_ci-setup"

-- | The canonical wire form: @\<name\>\@\<platform\>@, where @name@ is
-- 'setupNodeName' for setup nodes and the recipe's display name for
-- recipe nodes. This is the string process-compose sees as a
-- process name and the context name on GitHub commit statuses.
instance Display NodeId where
  displayBuilder n = displayBuilder (nodeName n) <> "@" <> displayBuilder (nodePlatform n)

-- | YAML/JSON key encoding for the process-compose @processes@ map.
-- Routes through 'Display' so the wire form ('@'-separated) is the
-- *only* serialization; no parallel JSON-specific shape can drift.
instance ToJSON NodeId where
  toJSON = toJSON . display

instance ToJSONKey NodeId where
  toJSONKey = toJSONKeyText display

-- | Inverse of 'display'. The wire-side observer
-- ('CI.ProcessCompose.Events.subscribeStates') hands us a raw 'Text'
-- and we recover the typed value here. Splits on the *last* @\@@ so
-- a recipe FQN containing no @\@@ (the usual case) and the platform
-- suffix are unambiguous; 'Nothing' on any unparseable input. The
-- 'setupNodeName' constant is the single load-bearing string seam
-- between the wire and the closed sum.
--
-- Failure mode is silent-drop at the call site (see
-- 'CI.Pipeline.withParsedNode'): an unknown wire name means the run
-- emitted a process we didn't schedule, which is a contract
-- violation we surface elsewhere rather than crash on here.
parseNodeId :: Text -> Maybe NodeId
parseNodeId t = case T.breakOnEnd "@" t of
  ("", _) -> Nothing
  (prefixWithSep, platformText) -> do
    let nameText = T.dropEnd 1 prefixWithSep
    p <- parsePlatform platformText
    case nameText of
      "" -> Nothing
      n | n == setupNodeName -> Just (SetupNode p)
      _ -> Just (RecipeNode (recipeNameFromText nameText) p)

-- | A user-supplied DAG filter — what @ci run e2e@ or
-- @ci run e2e\@x86_64-linux@ resolves to before the pipeline restricts
-- the fanned-out graph to it. Strictly less informative than 'NodeId':
-- 'SelRecipe' fans out across every pipeline platform; 'SelRecipePlatform'
-- pins to one. Setup nodes are never user-selectable — they ride along
-- automatically because every remote recipe depends on them.
data NodeSelector
  = SelRecipe RecipeName
  | SelRecipePlatform RecipeName Platform
  deriving stock (Show, Eq, Ord)

-- | Render in the same @\<recipe\>\[\@\<platform\>\]@ shape the user
-- typed it in. Used in error messages so an "unknown selector"
-- complaint echoes the exact token from argv.
instance Display NodeSelector where
  displayBuilder (SelRecipe r) = displayBuilder r
  displayBuilder (SelRecipePlatform r p) = displayBuilder r <> "@" <> displayBuilder p

-- | Parse a positional CLI selector @RECIPE[\@PLATFORM]@. An @\@@ is
-- mandatory if a platform is intended; the suffix must then be a
-- known 'Platform'. A bare token with no @\@@ becomes 'SelRecipe';
-- a token with @\@@ whose suffix is /not/ a valid platform is rejected
-- (the user almost certainly typo'd a platform, not authored a
-- recipe with @\@@ in its name).
parseSelector :: Text -> Either String NodeSelector
parseSelector t
  | T.null t = Left "empty selector"
  | otherwise = case T.breakOnEnd "@" t of
      ("", _) -> Right (SelRecipe (recipeNameFromText t))
      (prefixWithSep, suffix) -> case parsePlatform suffix of
        Just p ->
          let nameText = T.dropEnd 1 prefixWithSep
           in if T.null nameText
                then Left $ "selector " <> T.unpack t <> " has empty recipe part"
                else Right (SelRecipePlatform (recipeNameFromText nameText) p)
        Nothing ->
          Left $
            "selector " <> T.unpack t <> " has @ but no known platform suffix (expected x86_64-linux, aarch64-linux, or aarch64-darwin)"

-- | Render an adjacency map of 'NodeId's as Mermaid @flowchart TD@.
-- Vertex IDs are sanitized to mermaid-safe alphanumeric+underscore;
-- the @\<name\>\@\<platform\>@ display form is preserved verbatim in
-- the quoted label so the rendering reads the same as every other
-- consumer of 'Display'.
--
-- Lives here (rather than in "CI.ProcessCompose") because the
-- rendering volatility is "graph output format" — independent of the
-- pc YAML schema — and the only knowledge needed is how to display
-- a 'NodeId', which this module already owns.
toMermaid :: G.AdjacencyMap NodeId -> Text
toMermaid g =
  T.intercalate "\n" $
    "flowchart TD"
      : [nodeLine n | n <- G.vertexList g]
        <> [edgeLine a b | (a, b) <- G.edgeList g]
  where
    sanitize c
      | c == '@' || c == ':' || c == '-' || c == '.' = '_'
      | otherwise = c
    nodeId n = T.map sanitize (display n)
    nodeLine n = "  " <> nodeId n <> "[\"" <> display n <> "\"]"
    edgeLine a b = "  " <> nodeId a <> " --> " <> nodeId b
