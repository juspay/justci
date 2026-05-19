{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Everything tied to the @just@ data model: the @just@ CLI and its
-- @--dump --dump-format json@ schema. Discovery policy (which decoded
-- recipe is the pipeline root) lives in @CI.Root@ — that axis of change is
-- independent of the wire format and shouldn't ride along here.
module CI.Justfile
  ( -- * Schema
    RecipeName,
    recipeNameFromText,
    Recipe (..),
    Dep (..),
    Attribute (..),
    Os (..),

    -- * Operations
    FetchError,
    ParseError,
    fetchDump,
    parseDump,
    recipeCommand,
  )
where

import CI.Subprocess (SubprocessError, runSubprocess)
import Data.Aeson (FromJSON (parseJSON), FromJSONKey, Options (..), ToJSON, ToJSONKey, Value (Object, String), defaultOptions, eitherDecodeStrict, genericParseJSON)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Bifunctor (bimap, first)
import qualified Data.ByteString as BS
import Data.List (dropWhileEnd)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (Display (..))
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import System.Which (staticWhich)

-- | Absolute path to the @just@ binary, baked in at compile time via Nix.
-- Not exported: every just shell-out in the project goes through one of
-- the typed operations below ('recipeCommand', or 'CI.Nix.realisedJust'
-- for remote lanes).
justBin :: FilePath
justBin = $(staticWhich "just")

-- | The shell command process-compose runs per recipe: the absolute
-- @just@ path with @--no-deps@ (process-compose itself owns scheduling,
-- so just must not re-resolve dependencies) and the recipe name. Absolute
-- path so process-compose's spawned shell finds @just@ regardless of PATH.
recipeCommand :: RecipeName -> Text
recipeCommand (RecipeName n) = T.pack justBin <> " --no-deps " <> n

-- | The identifier of a recipe — its fully-qualified name (just's own
-- @namepath@: bare for top-level recipes, @mod::name@ for submodule
-- ones). The key under which a recipe lives in the flat map 'fetchDump'
-- returns and the argument @just --no-deps@ accepts.
newtype RecipeName = RecipeName Text
  deriving newtype (Show, Eq, Ord, IsString, Display, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

-- | The single named entry point for callers that hold a 'Text' recipe
-- identifier (e.g. the @name@ field of a 'CI.ProcessCompose.Events.ProcessState')
-- and need to look it up in a map keyed by 'RecipeName'. Validation
-- would live here if we added any — the round-trip via 'fromString' +
-- 'T.unpack' that callers would otherwise reach for is both ugly and
-- ambiguous about whether the conversion is total.
recipeNameFromText :: Text -> RecipeName
recipeNameFromText = RecipeName

-- | One entry in a recipe's @dependencies@ array: the dep's target name
-- plus any arguments passed at this call site (only non-empty when the
-- target is parameterized).
--
-- The 'recipe' field always holds a fully-qualified name in any value
-- that reaches the outside of this module. just's raw dump emits
-- siblings of submodule recipes in source form (@a@, @b@) rather than
-- qualified form, but 'parseDump' rewrites those to @mod::a@ /
-- @mod::b@ before returning, so every 'Dep.recipe' anywhere in the
-- returned map is a key in that same map.
data Dep = Dep
  { recipe :: RecipeName,
    arguments :: [Text]
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

-- | One entry in a recipe's @parameters@ array: a formal parameter the recipe declares. Mirrors the nine fields @just@ emits per parameter.
data Parameter = Parameter
  { name :: Text,
    default_ :: Maybe Text,
    export :: Bool,
    help :: Maybe Text,
    kind :: Text,
    long :: Maybe Text,
    pattern :: Maybe Text,
    short :: Maybe Text,
    value :: Maybe Text
  }
  deriving stock (Generic)

-- Custom because @default@ is a Haskell keyword; the field is @default_@ here
-- and stripped back to @default@ for the JSON.
instance FromJSON Parameter where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = dropWhileEnd (== '_')}

-- | A recipe-level attribute. Named cases cover the attributes this runner interprets today; everything else (including future attributes just may add) is preserved opaquely as 'Other'. JSON shapes mirror just's own encoding: flag attributes are bare strings (@"parallel"@, @"linux"@); parameterized ones are single-key objects (@{"metadata": ["..."]}@, @{"group": "..."}@). Decode-only — there is no @ToJSON@.
data Attribute
  = Parallel
  | Metadata [Text]
  | Os Os
  | Other Value
  deriving stock (Generic, Show)

-- | Host-OS gate. A recipe marked with one of these is only enabled on the matching host; multiple gates widen the disjunction (@[linux] [macos] foo:@ enables @foo@ on either). @Unix@ subsumes the BSDs and macOS, per just's own [conditional attribute](https://github.com/casey/just/blob/1.49.0/README.md) table.
data Os
  = Linux
  | Macos
  | Windows
  | Unix
  | Freebsd
  | Openbsd
  | Netbsd
  | Dragonfly
  deriving stock (Generic, Show, Eq, Ord, Bounded, Enum)

osFromText :: Text -> Maybe Os
osFromText = flip lookup osTable
  where
    osTable :: [(Text, Os)]
    osTable =
      [ ("linux", Linux),
        ("macos", Macos),
        ("windows", Windows),
        ("unix", Unix),
        ("freebsd", Freebsd),
        ("openbsd", Openbsd),
        ("netbsd", Netbsd),
        ("dragonfly", Dragonfly)
      ]

-- | Hand-rolled because aeson's default externally-tagged encoding can't model just's
-- mixed bare-string-and-single-key-object @attributes@ array (we want @'Os' 'Linux'@ to
-- decode from the flattened @"linux"@, not @{"os":"linux"}@). Equations dispatch
-- concrete to general: literal @"parallel"@, then OS strings via 'osFromText', then the
-- @metadata@ single-key object; the final wildcard preserves anything unknown as
-- 'Other' rather than rejecting it — open-world by design, see 'Attribute'.
instance FromJSON Attribute where
  parseJSON (String "parallel") = pure Parallel
  parseJSON (String t) | Just os <- osFromText t = pure (Os os)
  parseJSON v@(Object o)
    | Just metas <- KeyMap.lookup "metadata" o = Metadata <$> parseJSON metas
    | otherwise = pure (Other v)
  parseJSON v = pure (Other v)

-- | A parsed recipe: its fully-qualified name (just's own @namepath@: bare
-- for top-level recipes, @mod::name@ for submodule ones), its declared
-- dependencies, formal parameters, and recipe-level attributes.
data Recipe = Recipe
  { namepath :: RecipeName,
    dependencies :: [Dep],
    parameters :: [Parameter],
    attributes :: [Attribute]
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

-- | The @just --dump@ object, used recursively for both the top level and
-- each submodule (just emits the same shape at both levels — same
-- @recipes@ map plus a nested @modules@ map). We model the two fields the
-- runner needs; aeson ignores the rest. Internal-only: 'fetchDump'
-- flattens this tree into a single 'Map' before returning, so no consumer
-- outside this module sees the nested shape.
data Dump = Dump
  { recipes :: Map RecipeName Recipe,
    modules :: Map Text Dump
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

-- | Failures from 'fetchDump' — either the subprocess died or its
-- output didn't decode. 'FetchParseError' carries a 'ParseError' so
-- callers that only ran the pure 'parseDump' don't pattern-match a
-- constructor that path can't produce.
data FetchError
  = -- | The @just@ subprocess exited non-zero.
    FetchProcessError SubprocessError
  | -- | The @just@ subprocess succeeded but its JSON output didn't decode.
    FetchParseError ParseError
  deriving stock (Show)

-- | Failures from 'parseDump'. Decode-only — there is no subprocess
-- failure mode on this path. Carries aeson's underlying message.
newtype ParseError = ParseError {message :: String}
  deriving stock (Show)

instance Display FetchError where
  displayBuilder (FetchProcessError e) = displayBuilder e
  displayBuilder (FetchParseError e) = displayBuilder e

instance Display ParseError where
  displayBuilder e =
    "failed to decode just dump: " <> displayBuilder (T.pack e.message)

-- | Invoke @just --dump --dump-format json@ and return a single flat
-- recipe map keyed by fully-qualified name. Pipeline-shaped: subprocess →
-- 'parseDump'. Process failures and JSON parse failures are both surfaced
-- as 'FetchError'; no exception escapes.
fetchDump :: IO (Either FetchError (Map RecipeName Recipe))
fetchDump = do
  result <- runSubprocess "just --dump --dump-format json" justBin ["--dump", "--dump-format", "json"] ""
  pure $ case result of
    Left e -> Left (FetchProcessError e)
    Right stdout -> first FetchParseError $ parseDump $ TE.encodeUtf8 $ T.pack stdout

-- | Decode a @just --dump --dump-format json@ payload into the flat,
-- qualified recipe map. The 'just' top-level + submodule tree is
-- collapsed (via 'flattenDump') into one map keyed by each recipe's
-- emitted @namepath@; a second pass (via 'qualifyDeps') rewrites each
-- recipe's unqualified sibling-dep references into the same
-- fully-qualified form, so the map is internally consistent before it
-- leaves this module. Pure: separated from 'fetchDump' so tests can
-- exercise the schema + flatten + qualify pipeline without invoking the
-- @just@ subprocess.
parseDump :: BS.ByteString -> Either ParseError (Map RecipeName Recipe)
parseDump bs = bimap ParseError (qualifyDeps . flattenDump) (eitherDecodeStrict @Dump bs)

-- | Walk the @Dump@ tree and produce a single map keyed by each recipe's
-- emitted @namepath@. The keys in the input map (the short recipe names
-- just uses inside its tree) are dropped in favour of the FQN — top-level
-- recipes remain bare (@default@), submodule recipes become @mod::name@.
-- Pure structural pass; does not touch deps.
--
-- @just@ guarantees FQN uniqueness across the whole tree (the CLI
-- rejects collisions at load time), so the @keepFirst@ merge policy
-- below is defensive: it names the invariant rather than silently
-- relying on @Map@\'s default left-bias.
flattenDump :: Dump -> Map RecipeName Recipe
flattenDump d = Map.unionsWith keepFirst (top d : (flattenDump <$> Map.elems d.modules))
  where
    top :: Dump -> Map RecipeName Recipe
    top dump = Map.fromList [(r.namepath, r) | r <- Map.elems dump.recipes]
    keepFirst :: a -> a -> a
    keepFirst x _ = x

-- | Rewrite each recipe's dep list so every 'Dep' refers to its target by
-- fully-qualified name. Dep strings already containing @::@ are trusted
-- verbatim (just emits qualified deps in source form when the source
-- crosses module boundaries); bare names are taken as siblings of the
-- owning recipe and prefixed with that recipe's module path. Operates
-- over the complete flat map so no temporal coupling exists with the
-- order recipes were inserted.
qualifyDeps :: Map RecipeName Recipe -> Map RecipeName Recipe
qualifyDeps = fmap qualifyOne
  where
    qualifyOne r = r {dependencies = map (qualifyDep (modulePath r.namepath)) r.dependencies}

-- | Strip a recipe's trailing @::name@ segment to yield the path of its
-- enclosing module — 'Nothing' for top-level recipes, @Just "ci"@ for
-- @ci::e2e@, @Just "ci::sub"@ for @ci::sub::recipe@.
modulePath :: RecipeName -> Maybe Text
modulePath (RecipeName np) = case T.breakOnEnd "::" np of
  ("", _) -> Nothing
  (prefix, _) -> Just (T.dropEnd 2 prefix)

-- | Resolve a single 'Dep' against the module path of the recipe that
-- owns it. Three branches:
--
--   * @::@-bearing deps are trusted verbatim — just emits already-
--     qualified deps in source form when the source crosses module
--     boundaries.
--
--   * 'Nothing' owner module (top-level recipe) means there is no
--     module to prefix with; the dep is already in its final form.
--
--   * @Just m@ owner: the dep is a bare sibling inside that submodule
--     and gets prefixed with @m@.
--
-- Lifted out of 'qualifyDeps' so the rule stays one named decision
-- rather than an inline branch in a traversal.
qualifyDep :: Maybe Text -> Dep -> Dep
qualifyDep mOwnerMod d
  | "::" `T.isInfixOf` rawName = d
  | otherwise = case mOwnerMod of
      Nothing -> d
      Just m -> d {recipe = RecipeName (m <> "::" <> rawName)}
  where
    rawName = case d.recipe of RecipeName n -> n
