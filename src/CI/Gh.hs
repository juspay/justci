{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | GitHub CLI (@gh@) operations. Owns the binary path, every shell-out to
-- @gh@ in the project, and the GitHub API details those shell-outs encode:
-- endpoint URLs, wire values for typed enums, form-field names. Callers
-- reach for typed operations ('viewRepo', 'postCommitStatus') with typed
-- arguments — no raw endpoints, no @-f key=value@ pairs.
module CI.Gh
  ( -- * Values
    Owner,
    RepoName,
    Repo,
    BranchName,
    CommitStatus (..),
    Context,
    CommitStatusPost (..),

    -- * Errors
    GhError,

    -- * Operations
    viewRepo,
    viewDefaultBranch,
    contextFrom,
    postCommitStatus,
    setRequiredChecks,
  )
where

import CI.Git (Sha)
import CI.Subprocess (SubprocessError, runSubprocess)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (Display (..), display)
import System.Which (staticWhich)

-- | Absolute path to the @gh@ binary, baked in at compile time via Nix.
-- Not exported: every gh shell-out in the project goes through one of the
-- typed operations below.
ghBin :: FilePath
ghBin = $(staticWhich "gh")

-- | A GitHub owner login (user or org). Opaque; minted only by 'viewRepo'.
newtype Owner = Owner Text
  deriving stock (Show, Eq)
  deriving newtype (Display)

-- | A GitHub repository name (the @name@ half of @nameWithOwner@). Opaque;
-- minted only by 'viewRepo'.
newtype RepoName = RepoName Text
  deriving stock (Show, Eq)
  deriving newtype (Display)

-- | A GitHub repository, matching @gh@'s vocabulary: an owner login plus a
-- repository name — the two halves of @nameWithOwner@. Resolved once from
-- @gh repo view@ and threaded into 'CI.CommitStatus.postConsumer'
-- alongside the 'CI.Git.Sha'.
data Repo = Repo {owner :: Owner, name :: RepoName}
  deriving stock (Show, Eq)

-- | A GitHub branch name (e.g. @main@, @master@, @develop@). Opaque
-- per @prefer-newtype-over-string@: minted either by 'viewDefaultBranch'
-- (resolved from @gh repo view@) or via 'OverloadedStrings' literals
-- ('IsString'). 'Display' is the canonical destructor — consumers
-- never pattern-match the constructor.
newtype BranchName = BranchName Text
  deriving stock (Show, Eq)
  deriving newtype (Display, IsString)

-- | Failures from the gh operations in this module.
data GhError
  = GhSubprocess SubprocessError
  | UnexpectedNameWithOwner String
  | UnexpectedDefaultBranch String
  deriving stock (Show)

instance Display GhError where
  displayBuilder (GhSubprocess e) = displayBuilder e
  displayBuilder (UnexpectedNameWithOwner out) =
    "unexpected nameWithOwner from gh: " <> displayBuilder (T.pack out)
  displayBuilder (UnexpectedDefaultBranch out) =
    "unexpected defaultBranchRef from gh: " <> displayBuilder (T.pack out)

-- | Run @gh repo view --json nameWithOwner@ and split the result into a
-- typed 'Repo' so callers never see a slash-separated string.
viewRepo :: IO (Either GhError Repo)
viewRepo = do
  result <-
    runSubprocess
      "gh repo view"
      ghBin
      ["repo", "view", "--json", "nameWithOwner", "-q", ".nameWithOwner"]
      ""
  pure $ case result of
    Left e -> Left (GhSubprocess e)
    Right out -> case T.splitOn "/" (T.strip (T.pack out)) of
      [o, n] | not (T.null o), not (T.null n) -> Right (Repo (Owner o) (RepoName n))
      _ -> Left (UnexpectedNameWithOwner out)

-- | Resolve the repo's default branch name (e.g. @main@, @master@) via
-- @gh repo view --json defaultBranchRef@. Used by "CI.Pipeline.runProtect"
-- when the user hasn't passed an explicit @--branch@.
viewDefaultBranch :: IO (Either GhError BranchName)
viewDefaultBranch = do
  result <-
    runSubprocess
      "gh repo view (defaultBranchRef)"
      ghBin
      ["repo", "view", "--json", "defaultBranchRef", "-q", ".defaultBranchRef.name"]
      ""
  pure $ case result of
    Left e -> Left (GhSubprocess e)
    Right out ->
      let b = T.strip (T.pack out)
       in if T.null b
            then Left (UnexpectedDefaultBranch out)
            else Right (BranchName b)

-- | The four GitHub-defined commit-status states. See
-- <https://docs.github.com/en/rest/commits/statuses>. The 'Display'
-- instance emits the lowercase wire form GitHub accepts.
data CommitStatus = Pending | Success | Failure | Error
  deriving stock (Show, Eq)

instance Display CommitStatus where
  displayBuilder Pending = "pending"
  displayBuilder Success = "success"
  displayBuilder Failure = "failure"
  displayBuilder Error = "error"

-- | A status-check context: the unique label that groups posts of the same
-- check on a PR. Opaque; minted only via 'contextFrom'. GitHub treats the
-- value itself as free-form text — the @ci/\<recipe\>@ naming convention
-- is CI policy, owned by "CI.CommitStatus".
newtype Context = Context Text
  deriving stock (Show)
  deriving newtype (Display)

-- | The smart constructor for 'Context'. The value is opaque to GitHub, so
-- this is just @Context@ today — the named entry point makes every
-- minting site searchable and gives one place to add validation later
-- without touching call sites.
contextFrom :: Text -> Context
contextFrom = Context

-- | The fields the @Create-a-commit-status@ endpoint expects, grouped so
-- callers don't pass three positional values. @description@ is free-form
-- caller-supplied prose.
data CommitStatusPost = CommitStatusPost
  { state :: CommitStatus,
    context :: Context,
    description :: Text
  }

-- | POST @\/repos\/{owner}\/{repo}\/statuses\/{sha}@ with the given status
-- post. The endpoint URL and wire encoding of 'CommitStatus' are gh-API
-- details owned here; the caller passes only typed values.
postCommitStatus :: Repo -> Sha -> CommitStatusPost -> IO (Either SubprocessError ())
postCommitStatus repo sha post = apiPost endpoint fields
  where
    endpoint = "/repos/" <> display repo.owner <> "/" <> display repo.name <> "/statuses/" <> display sha
    fields =
      [ ("state", display post.state),
        ("context", display post.context),
        ("description", post.description)
      ]

-- | Internal helper: @gh api -X POST \<endpoint\> -f k=v ...@ over the
-- form fields. Not exported — callers use endpoint-specific typed
-- operations (e.g. 'postCommitStatus'). Reusable by future POST-shaped
-- operations without re-deriving the argv layout.
apiPost :: Text -> [(Text, Text)] -> IO (Either SubprocessError ())
apiPost endpoint fields = do
  result <- runSubprocess ("gh api POST " <> endpoint) ghBin args ""
  pure (() <$ result)
  where
    args = ["api", "-X", "POST", T.unpack endpoint] ++ concatMap formArg fields
    formArg (k, v) = ["-f", T.unpack k <> "=" <> T.unpack v]

-- | Replace the required status checks on a branch's protection ruleset.
-- PATCHes
-- @\/repos\/{owner}\/{repo}\/branches\/{branch}\/protection\/required_status_checks@
-- with @{"strict": false, "contexts": [...]}@.
--
-- @strict = false@ is deliberate: requiring branches to be up-to-date
-- with base before merge is a separate decision (it interacts with
-- mergeability and review semantics) that the repo owner can flip in
-- the GH UI without re-running @ci protect@.
--
-- Requires the branch to already have protection enabled — the
-- endpoint is a subresource of an existing protection ruleset, not a
-- creator. If protection isn't enabled gh returns a 404 which surfaces
-- through 'SubprocessError'; the user enables protection once via the
-- GH UI, then re-runs @ci protect@.
setRequiredChecks :: Repo -> BranchName -> [Context] -> IO (Either SubprocessError ())
setRequiredChecks repo branch contexts = apiPatchJson endpoint body
  where
    endpoint =
      "/repos/"
        <> display repo.owner
        <> "/"
        <> display repo.name
        <> "/branches/"
        <> display branch
        <> "/protection/required_status_checks"
    body =
      A.object
        [ "strict" A..= False,
          "contexts" A..= [display c | c <- contexts]
        ]

-- | Internal helper: @gh api -X PATCH \<endpoint\> --input -@ with a
-- JSON body piped on stdin. The form-field shape ('apiPost') can't
-- express GitHub's nested JSON requests (arrays of objects, mixed
-- types); a literal JSON body is the simplest equivalent and lets
-- aeson handle the encoding.
apiPatchJson :: Text -> A.Value -> IO (Either SubprocessError ())
apiPatchJson endpoint payload = do
  result <-
    runSubprocess
      ("gh api PATCH " <> endpoint)
      ghBin
      ["api", "-X", "PATCH", T.unpack endpoint, "--input", "-"]
      (BL.unpack (A.encode payload))
  pure (() <$ result)
