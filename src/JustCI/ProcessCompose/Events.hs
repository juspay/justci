{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The event-stream half of the process-compose interface. Encapsulates
-- the WebSocket-over-UDS subscription, the typed @ProcessState@ /
-- @ProcessStatus@ vocabulary, and the connect-retry + decode-budget
-- policy. Volatility axis: the @\/process\/states\/ws@ protocol and event
-- schema, which changes independently of the YAML config schema (which
-- lives in "JustCI.ProcessCompose") and the invocation argv.
module JustCI.ProcessCompose.Events
  ( -- * Wire vocabulary
    ProcessState (..),
    ProcessStatus (..),

    -- * Terminal classification
    TerminalStatus (..),
    psToTerminalStatus,

    -- * Subscription
    subscribeStates,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, bracketOnError, try)
import Data.Aeson (FromJSON (parseJSON), eitherDecodeStrict, withText)
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Network.Socket as S
import qualified Network.WebSockets as WS
import System.IO (hPutStrLn, stderr)

-- | The four process-compose states that map onto a GitHub commit status.
-- Everything else (Pending, Launching, Restarting, …) lands in 'PsOther'
-- and is silently dropped by consumers that only care about the four.
-- Typed (rather than left as a raw 'Text') so a typo or an upstream rename
-- becomes a pattern-match exhaustiveness warning instead of a silent miss.
data ProcessStatus
  = PsRunning
  | PsCompleted
  | PsSkipped
  | PsErrored
  | PsOther Text
  deriving stock (Show, Eq)

instance FromJSON ProcessStatus where
  parseJSON = withText "ProcessStatus" $ \t -> pure $ case t of
    "Running" -> PsRunning
    "Completed" -> PsCompleted
    "Skipped" -> PsSkipped
    "Error" -> PsErrored
    other -> PsOther other

-- | Subset of process-compose's @ProcessState@ (per
-- @src\/types\/process.go@) we care about. Aeson ignores extra fields.
data ProcessState = ProcessState
  { name :: Text,
    status :: ProcessStatus,
    exit_code :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

-- | The three terminal outcomes we distinguish at the project's
-- vocabulary layer: a node either ran-and-succeeded ('TsSucceeded'),
-- ran-and-failed ('TsFailed'), or never ran because an upstream dep
-- failed ('TsSkipped'). pc's wire has four terminal shapes
-- (completed-with-zero, completed-with-nonzero, skipped, errored);
-- 'psToTerminalStatus' folds completed-non-zero and errored into
-- 'TsFailed' (both are this-node-broke) and keeps skipped separate as
-- 'TsSkipped' (this-node-didn't-run-because-of-someone-else).
--
-- Owned here (rather than in "JustCI.CommitStatus" or "JustCI.Verdict")
-- so both downstreams derive their own surface vocabulary from the
-- same base predicate: the local CLI verdict projects 'TsSkipped' to
-- a @Skipped@ 'JustCI.Verdict.RecipeOutcome', and the GH commit-status
-- surface posts /nothing/ for 'TsSkipped' — branch protection's
-- "Expected — Waiting for status to be reported" placeholder is the
-- canonical receptacle for "required but unreported," and the
-- 'JustCI.CommitStatus.describePost' path short-circuits to 'Nothing'
-- on 'PsSkipped' rather than fabricating a parallel @Pending@+"Skipped"
-- row. The cross-module invariant — every 'TerminalStatus' value maps
-- to exactly one 'CommitStatus' and one 'RecipeOutcome' — is locked
-- down by the agreement test in @VerdictSpec@; the 'TsSkipped' arm of
-- 'JustCI.CommitStatus.terminalToCommitStatus' is preserved for that
-- totality contract even though no production path reaches it.
data TerminalStatus = TsSucceeded | TsFailed | TsSkipped
  deriving stock (Show, Eq, Bounded, Enum)

-- | The single ground-truth classifier of a 'ProcessState' event into
-- a terminal outcome. Non-terminal events ('PsRunning', 'PsOther')
-- return 'Nothing'; downstreams add their own non-terminal handling
-- ('JustCI.CommitStatus' posts @Pending@ on 'PsRunning', for example).
--
-- 'PsSkipped' and 'PsErrored' are kept asymmetric on purpose. Both
-- mean "this node didn't run," but the reason differs: 'PsSkipped'
-- is a cascade (some upstream dep failed and pc decided not to
-- start this one; the recipe itself is probably fine), while
-- 'PsErrored' is a launch failure (pc tried to start the process
-- and the start itself broke — a real defect in this recipe's
-- launch path). The two projections downstream reflect that:
-- 'TsSkipped' is rendered on the GH side by branch protection's
-- "Expected — Waiting" placeholder (no justci-posted row) and on
-- the CLI side as @Skipped@, while 'TsFailed' (from 'PsErrored')
-- posts @Failure@+"Errored (did not start)" and renders as @Failed@.
psToTerminalStatus :: ProcessState -> Maybe TerminalStatus
psToTerminalStatus ps = case (ps.status, ps.exit_code) of
  (PsCompleted, 0) -> Just TsSucceeded
  (PsCompleted, _) -> Just TsFailed
  (PsSkipped, _) -> Just TsSkipped
  (PsErrored, _) -> Just TsFailed
  _ -> Nothing

-- | Mirrors process-compose's @ProcessStateEvent@ wire type. We model only
-- the @state@ field; the @snapshot@ flag (true on initial replay, omitted
-- on live transitions) is ignored — aeson drops unknown keys by default,
-- and consumers treat both kinds identically.
newtype ProcessStateEvent = ProcessStateEvent {state :: ProcessState}
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

-- | Subscribe to a running process-compose's @\/process\/states\/ws@
-- WebSocket stream over the UDS at @sockPath@. Decode each frame and
-- hand the inner 'ProcessState' to the callback. Blocks until the
-- WebSocket closes (i.e. process-compose exits). Retries the connect up
-- to ~10s so callers can launch this concurrently with the
-- process-compose spawn without ordering the spawn against socket
-- creation.
--
-- The callback runs in the event-loop thread; fork inside if you need
-- to do slow work. A clean WebSocket close ('WS.ConnectionException')
-- logs and returns; any other exception propagates so 'link' on the
-- subscriber thread aborts the pipeline. Decode failures are logged and
-- tolerated up to 'maxConsecutiveDecodeFailures' in a row — past that,
-- the wire format has likely drifted and we throw rather than silently
-- drop every event.
subscribeStates :: FilePath -> (ProcessState -> IO ()) -> IO ()
subscribeStates sockPath onState = do
  hPutStrLn stderr $ "observer: connecting to " <> sockPath
  sock <- connectWithRetry sockPath
  WS.runClientWithSocket sock "localhost" "/process/states/ws" WS.defaultConnectionOptions [] $ \conn -> do
    hPutStrLn stderr "observer: connected, streaming events"
    loop conn maxConsecutiveDecodeFailures
  where
    loop conn budget = do
      result <- try (WS.receiveData conn)
      case result of
        Left (e :: WS.ConnectionException) ->
          hPutStrLn stderr $ "observer: stream closed (" <> show e <> ")"
        Right (bs :: BSL.ByteString) -> case eitherDecodeStrict @ProcessStateEvent (BSL.toStrict bs) of
          Left err
            | budget > 1 -> do
                hPutStrLn stderr $ "observer: decode error: " <> err
                loop conn (budget - 1)
            | otherwise ->
                fail $
                  "observer: "
                    <> show maxConsecutiveDecodeFailures
                    <> " consecutive decode failures; wire format drifted? last error: "
                    <> err
          Right ev -> do
            onState ev.state
            loop conn maxConsecutiveDecodeFailures

-- | Tolerate up to this many decode failures in a row before tearing the
-- subscription down. Low enough that a real wire-format drift surfaces
-- promptly, high enough that one bad frame doesn't kill the run.
maxConsecutiveDecodeFailures :: Int
maxConsecutiveDecodeFailures = 5

-- | Connect to a UDS, retrying with 100ms backoff up to 100 attempts
-- (~10s ceiling). The socket may not exist yet at startup
-- (process-compose creates it a beat after spawn); any 'IOException' from
-- @connect@ — including ENOENT for the not-yet-created path — triggers a
-- retry. The final attempt's exception propagates so the caller (and
-- 'link') sees it. 'bracketOnError' ensures the half-open socket fd is
-- closed before each retry — without it, 100 retries leak 100 fds.
connectWithRetry :: FilePath -> IO S.Socket
connectWithRetry sockPath = go (100 :: Int)
  where
    go 1 = attempt
    go n =
      try attempt >>= \case
        Right sock -> pure sock
        Left (_ :: IOException) -> threadDelay 100_000 >> go (n - 1)
    attempt =
      bracketOnError
        (S.socket S.AF_UNIX S.Stream S.defaultProtocol)
        S.close
        ( \sock -> do
            S.connect sock (S.SockAddrUnix sockPath)
            pure sock
        )
