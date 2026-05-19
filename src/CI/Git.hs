{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Git-tool operations the pipeline needs: a HEAD SHA, a clean-tree
-- precondition, and a bracketed worktree snapshot. All shell out to
-- @git@; the binary path lives here as 'gitBin'.
module CI.Git
  ( -- * Values
    Sha,
    shaPlaceholder,

    -- * Operations
    CleanTreeError,
    ensureCleanTree,
    resolveSha,
    withSnapshotWorktree,
  )
where

import CI.Subprocess (SubprocessError, runSubprocess)
import Control.Exception (SomeException, bracket_, catch)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (Display (..))
import System.Process (callProcess)
import System.Which (staticWhich)

-- | Absolute path to the @git@ binary, baked in at compile time via Nix.
-- Not exported: every git shell-out in the project goes through one of
-- the typed operations below.
gitBin :: FilePath
gitBin = $(staticWhich "git")

-- | A git commit SHA — what 'CI.CommitStatus.postConsumer' attaches each
-- status check to. Opaque: minted only by 'resolveSha'; rendered to Text
-- via 'display'.
newtype Sha = Sha Text
  deriving stock (Show, Eq)
  deriving newtype (Display)

-- | A visibly-fake 'Sha' used by 'CI.Pipeline' in dump-yaml mode to fill
-- the SSH command shape when there's no real SHA to clone. Never
-- written to GitHub, never read back: the rendered form just stands out
-- in the inspected YAML.
shaPlaceholder :: Sha
shaPlaceholder = Sha "0000000-dump-yaml-placeholder"

-- | The two ways 'ensureCleanTree' can fail. Subprocess failures wrap
-- 'SubprocessError' so the @binary failed (N): stderr@ formatting lives
-- in one place; 'DirtyTree' carries the porcelain-parsed paths so the
-- user can see what to commit or stash.
data CleanTreeError
  = CleanTreeSubprocess SubprocessError
  | DirtyTree [FilePath]
  deriving stock (Show)

instance Display CleanTreeError where
  displayBuilder (CleanTreeSubprocess e) = displayBuilder e
  displayBuilder (DirtyTree paths) =
    "working tree is dirty (CI=true requires a clean tree); commit or stash first:\n"
      <> mconcat ["  " <> displayBuilder (T.pack p) <> "\n" | p <- paths]

-- | Refuse the run if the working tree has uncommitted changes. Strict mode
-- demands the SHA on the green check exactly match the bytes that were
-- tested; a dirty tree breaks that invariant by definition.
ensureCleanTree :: IO (Either CleanTreeError ())
ensureCleanTree = do
  result <- runSubprocess "git status --porcelain" gitBin ["status", "--porcelain"] ""
  pure $ case result of
    Left e -> Left (CleanTreeSubprocess e)
    Right out -> case mapMaybe porcelainPath (lines out) of
      [] -> Right ()
      dirty -> Left (DirtyTree dirty)
  where
    -- Each non-empty porcelain v1 line is @XY path@: two status chars, a
    -- space, then the path. Renames and copies are @XY orig -> new@ —
    -- keep only the rename target for the user-facing list.
    porcelainPath line = case drop 3 line of
      "" -> Nothing
      rest -> Just (renameTarget rest)
    renameTarget s = case T.breakOn " -> " (T.pack s) of
      (_, arrowOnwards) | not (T.null arrowOnwards) -> T.unpack (T.drop 4 arrowOnwards)
      _ -> s

-- | Resolve the current HEAD SHA via @git rev-parse HEAD@. Used once at
-- strict-mode startup so every commit-status post against this run targets
-- the same commit. The only failure mode is a subprocess failure, so the
-- return type carries 'SubprocessError' directly — no module-level error
-- wrapper.
resolveSha :: IO (Either SubprocessError Sha)
resolveSha = do
  result <- runSubprocess "git rev-parse HEAD" gitBin ["rev-parse", "HEAD"] ""
  pure $ Sha . T.strip . T.pack <$> result

-- | Create a @git worktree@ at HEAD pinned at @path@, run the action,
-- and remove the worktree on any exit path (success, failure, or
-- exception, including 'exitWith'). On the happy path this is one @git
-- worktree add@ invocation; only if @add@ fails (typically because a
-- previous run crashed and left a stale worktree at @path@) do we
-- attempt a @worktree remove --force@ and retry — saves a fork+exec on
-- every normal run.
--
-- Strict-mode CI runs the whole pipeline inside this worktree so that
-- (a) edits to the original working tree mid-pipeline can't leak into
-- later recipes, and (b) @git rev-parse HEAD@ from inside any recipe
-- always returns the same pinned SHA.
withSnapshotWorktree :: FilePath -> IO a -> IO a
withSnapshotWorktree path action =
  bracket_
    (tryAdd `catch` \(_ :: SomeException) -> cleanup >> tryAdd)
    cleanup
    action
  where
    tryAdd = callProcess gitBin ["worktree", "add", "--detach", path, "HEAD"]
    cleanup = callProcess gitBin ["worktree", "remove", "--force", path]
