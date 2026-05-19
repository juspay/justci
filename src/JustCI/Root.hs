{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | CI-root discovery policy.
--
-- This lives in its own module because the axis of change is independent of
-- @just@'s wire format (which 'JustCI.Justfile' encapsulates): the policy is
-- which recipe in a decoded recipe map is treated as the pipeline's root,
-- and that can evolve (e.g. CLI override, config file, well-known name)
-- without any corresponding change to how @just --dump@ output is parsed.
module JustCI.Root
  ( RootError,
    findRoot,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text.Display (Display (..), display)
import JustCI.Justfile (Attribute (..), Recipe (..), RecipeName)

-- | Failures from 'findRoot'.
data RootError
  = -- | No recipe carries the @[metadata(\"ci\")]@ tag.
    NoRoot
  | -- | More than one recipe carries it; the runner refuses to guess.
    MultipleRoots [RecipeName]
  deriving stock (Show)

instance Display RootError where
  displayBuilder NoRoot =
    "no recipe is tagged [metadata(\"ci\")]; mark exactly one recipe as the pipeline root"
  displayBuilder (MultipleRoots rs) =
    "multiple recipes are tagged [metadata(\"ci\")]: "
      <> displayBuilder (T.intercalate ", " (display <$> rs))

-- | Find the single recipe tagged with @[metadata(\"ci\")]@. Refuses to
-- silently pick one when more than one is tagged.
findRoot :: Map.Map RecipeName Recipe -> Either RootError RecipeName
findRoot recipes =
  case [name | (name, r) <- Map.toList recipes, any isRoot r.attributes] of
    [] -> Left NoRoot
    [name] -> Right name
    xs -> Left (MultipleRoots xs)
  where
    isRoot (Metadata ms) = "ci" `elem` ms
    isRoot _ = False
