{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The exit-code-capture shell-out helper. Wraps
-- 'System.Process.readProcessWithExitCode' so non-zero exits become a
-- structured 'SubprocessError' (description + code + captured stderr)
-- routed through one Display instance, instead of each module repeating
-- @case ec of ExitFailure n -> Left (FooFailed n err)@.
--
-- Scoped narrowly: only this one shell-out pattern. Other
-- 'System.Process' shapes (fire-and-forget 'callProcess', bracketed
-- 'withCreateProcess' with custom stdin handling) are not part of this
-- module's claim — they live in the tool wrapper that needs them.
module JustCI.Subprocess
  ( SubprocessError (..),
    runSubprocess,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (Display (..))
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

-- | A subprocess that exited non-zero. 'description' is a human-readable
-- name for the invocation (e.g. @"git rev-parse HEAD"@) used in error
-- display; 'code' is the exit code; 'stderr' is captured standard error.
data SubprocessError = SubprocessError
  { description :: Text,
    code :: Int,
    stderr :: String
  }
  deriving stock (Show)

instance Display SubprocessError where
  displayBuilder e =
    displayBuilder e.description
      <> " failed ("
      <> displayBuilder e.code
      <> "): "
      <> displayBuilder (T.pack e.stderr)

-- | Run a subprocess, returning the captured stdout on success or a
-- 'SubprocessError' on non-zero exit. @description@ identifies the
-- invocation in error messages; @bin@ is the absolute path to the binary
-- (typically a @\$(staticWhich ...)@ splice); @args@ are the invocation
-- args; @stdin@ is piped to the subprocess.
runSubprocess :: Text -> FilePath -> [String] -> String -> IO (Either SubprocessError String)
runSubprocess description bin args input = do
  (ec, out, err) <- readProcessWithExitCode bin args input
  pure $ case ec of
    ExitSuccess -> Right out
    ExitFailure n -> Left (SubprocessError description n err)
