{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for "JustCI.Justfile"'s public surface. The focus is 'parseDump' —
-- the pure entry point that runs decode → flatten → qualify over a
-- @just --dump --dump-format json@ payload — and 'recipeCommand', the
-- one-line invocation builder. 'fetchDump' itself (subprocess + parse)
-- is covered end-to-end by the justfile's @run-check@ smoke test.
module JustCI.JustfileSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import JustCI.Justfile (Attribute (..), Dep (..), Recipe (..), RecipeName, hasBody, parseDump, recipeCommand)
import Test.Hspec

spec :: Spec
spec = do
  describe "recipeCommand" $ do
    it "emits --no-deps + the bare recipe name for a top-level recipe" $
      recipeCommand "default" `shouldSatisfy` T.isInfixOf " --no-deps default"
    it "emits --no-deps + the fully-qualified name for a submodule recipe" $
      recipeCommand "sub::entry" `shouldSatisfy` T.isInfixOf " --no-deps sub::entry"

  describe "parseDump" $ do
    context "top-level only" $ do
      it "decodes a single top-level recipe with no deps" $ do
        recipes <- decodeOrFail topLevelOnlyJson
        Map.keys recipes `shouldBe` ["solo"]
        solo <- requireRecipe recipes "solo"
        depNames solo `shouldBe` []

    context "submodule recipes" $ do
      it "keys every recipe by its fully-qualified namepath" $ do
        recipes <- decodeOrFail submoduleFixtureJson
        Map.keys recipes
          `shouldMatchList` ["default", "sub::a", "sub::b", "sub::entry", "sub::fan", "sub::shared"]

      it "leaves top-level recipe deps untouched" $ do
        recipes <- decodeOrFail submoduleFixtureJson
        top <- requireRecipe recipes "default"
        depNames top `shouldBe` []

      it "qualifies an unqualified sibling dep with the owner's module path" $ do
        -- sub::a's source dep is bare 'shared'; should become 'sub::shared'
        recipes <- decodeOrFail submoduleFixtureJson
        a <- requireRecipe recipes "sub::a"
        depNames a `shouldBe` ["sub::shared"]

      it "qualifies multiple sibling deps on the same recipe" $ do
        recipes <- decodeOrFail submoduleFixtureJson
        entry <- requireRecipe recipes "sub::entry"
        depNames entry `shouldMatchList` ["sub::a", "sub::b"]

      it "preserves the [parallel] attribute alongside qualified deps" $ do
        recipes <- decodeOrFail submoduleFixtureJson
        fan <- requireRecipe recipes "sub::fan"
        fan.attributes `shouldSatisfy` any isParallel
        depNames fan `shouldMatchList` ["sub::a", "sub::b"]

    context "already-qualified deps" $ do
      it "trusts a dep that already contains :: verbatim" $ do
        recipes <- decodeOrFail crossModuleDepJson
        top <- requireRecipe recipes "default"
        depNames top `shouldBe` ["sub::leaf"]

    context "variable-substitution body tokens" $ do
      -- Regression for juspay/justci#32: just emits each {{ var }}
      -- substitution as a nested JSON array (e.g. [["variable","name"]])
      -- inside the recipe body alongside literal-string tokens. The
      -- decoder must accept either shape per-token or it'll reject every
      -- justfile that uses variable substitution.
      it "decodes a recipe body containing a variable-reference token" $ do
        recipes <- decodeOrFail variableTokenJson
        r <- requireRecipe recipes "run"
        hasBody r `shouldBe` True

    context "errors" $ do
      it "returns ParseError on malformed JSON" $
        case parseDump "{ not valid json" of
          Left _ -> pure ()
          Right _ -> expectationFailure "expected Left on malformed JSON"

  describe "hasBody" $ do
    it "is False for a pure aggregator (empty body)" $ do
      recipes <- decodeOrFail submoduleFixtureJson
      entry <- requireRecipe recipes "sub::entry"
      hasBody entry `shouldBe` False

    it "is True for a recipe with shell lines" $ do
      recipes <- decodeOrFail submoduleFixtureJson
      shared <- requireRecipe recipes "sub::shared"
      hasBody shared `shouldBe` True

-- | Decode a fixture into the recipe map; 'fail' in 'IO' raises an
-- exception hspec catches and reports with the supplied message.
decodeOrFail :: BS.ByteString -> IO (Map.Map RecipeName Recipe)
decodeOrFail bs = case parseDump bs of
  Right m -> pure m
  Left e -> fail ("parseDump failed: " <> show e)

requireRecipe :: Map.Map RecipeName Recipe -> RecipeName -> IO Recipe
requireRecipe recipes k = case Map.lookup k recipes of
  Just r -> pure r
  Nothing -> fail ("missing recipe: " <> show k)

depNames :: Recipe -> [RecipeName]
depNames r = map (\d -> d.recipe) r.dependencies

isParallel :: Attribute -> Bool
isParallel Parallel = True
isParallel _ = False

-- | One top-level recipe, no submodules. Smallest input that parseDump
-- can succeed on — exercises the no-flattening, no-qualifying path.
topLevelOnlyJson :: BS.ByteString
topLevelOnlyJson =
  "{\"recipes\":{\"solo\":{\"namepath\":\"solo\",\"dependencies\":[],\"parameters\":[],\"attributes\":[],\"body\":[[\"echo solo\"]]}},\"modules\":{}}"

-- | A recipe whose body contains a {{ variable }} substitution. just
-- emits each substitution as a nested JSON array of the form
-- @[["variable", "<name>"]]@ in place of a literal-string token, so
-- @body[0]@ here is a one-line list with two tokens: the substitution
-- array and a trailing string literal. Reproduces juspay/justci#32.
variableTokenJson :: BS.ByteString
variableTokenJson =
  "{\"recipes\":{\"run\":{\"namepath\":\"run\",\"dependencies\":[],\"parameters\":[],\"attributes\":[],\"body\":[[[[\"variable\",\"nix_shell\"]],\" sh -c 'echo done'\"]]}},\"modules\":{}}"

-- | A top-level recipe whose dep is already qualified to a submodule
-- recipe (e.g. a top-level @ci: sub::leaf@). Qualification must trust
-- the @::@ verbatim and not double-prefix.
crossModuleDepJson :: BS.ByteString
crossModuleDepJson =
  "{\"recipes\":{\"default\":{\"namepath\":\"default\",\"dependencies\":[{\"recipe\":\"sub::leaf\",\"arguments\":[]}],\"parameters\":[],\"attributes\":[],\"body\":[]}},\"modules\":{\"sub\":{\"recipes\":{\"leaf\":{\"namepath\":\"sub::leaf\",\"dependencies\":[],\"parameters\":[],\"attributes\":[],\"body\":[[\"echo leaf\"]]}},\"modules\":{}}}}"

-- | The shape the @test/fixtures/with-module@ justfile produces: a
-- bare top-level @default@ plus a @sub@ module with five recipes —
-- @entry@ (tagged @[metadata("ci")]@) depending on @a@ and @b@,
-- @fan@ (tagged @[parallel]@) depending on the same two, both @a@
-- and @b@ depending on a shared upstream @shared@, and @shared@
-- itself with no deps. Mirrors the json captured by running
-- @just --dump --dump-format json@ in that fixture directory.
-- @entry@ and @fan@ are pure aggregators (empty @body@); the
-- remaining recipes carry one-line bodies so 'hasBody' tests have
-- both shapes to exercise.
submoduleFixtureJson :: BS.ByteString
submoduleFixtureJson =
  "{\"recipes\":{\"default\":{\"namepath\":\"default\",\"dependencies\":[],\"parameters\":[],\"attributes\":[],\"body\":[]}},\
  \\"modules\":{\"sub\":{\"recipes\":{\
  \\"a\":{\"namepath\":\"sub::a\",\"dependencies\":[{\"recipe\":\"shared\",\"arguments\":[]}],\"parameters\":[],\"attributes\":[],\"body\":[[\"echo a\"]]},\
  \\"b\":{\"namepath\":\"sub::b\",\"dependencies\":[{\"recipe\":\"shared\",\"arguments\":[]}],\"parameters\":[],\"attributes\":[],\"body\":[[\"echo b\"]]},\
  \\"entry\":{\"namepath\":\"sub::entry\",\"dependencies\":[{\"recipe\":\"a\",\"arguments\":[]},{\"recipe\":\"b\",\"arguments\":[]}],\"parameters\":[],\"attributes\":[{\"metadata\":[\"ci\"]}],\"body\":[]},\
  \\"fan\":{\"namepath\":\"sub::fan\",\"dependencies\":[{\"recipe\":\"a\",\"arguments\":[]},{\"recipe\":\"b\",\"arguments\":[]}],\"parameters\":[],\"attributes\":[\"parallel\"],\"body\":[]},\
  \\"shared\":{\"namepath\":\"sub::shared\",\"dependencies\":[],\"parameters\":[],\"attributes\":[],\"body\":[[\"echo shared\"]]}\
  \},\"modules\":{}}}}"
