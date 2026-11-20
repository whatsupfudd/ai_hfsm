-- | Focused executable regression for the new one-turn persistence contract.
-- Run with: stack exec -- runghc -isrc src/Demo/CommitTurnRegression.hs
module Demo.CommitTurnRegression (regression) where

import Control.Monad (unless)
import Data.Aeson (Value(..))
import qualified Data.Aeson.KeyMap as KM
import Data.String (fromString)
import qualified Data.Text
import Data.Time.Clock (getCurrentTime)
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U4
import qualified Data.Vector as V

import Demo.ApprovalHfsm (approvalSpec, Signal(..))
import Demo.ApprovalRuntime (initialCtx)
import Demo.MemoryStore (createInstance, enqueueSignal, loadInstanceView, newMemoryStore, storeOf)
import Hfsm.Compile (compile)
import Hfsm.Runtime.Engine.Step (StepInput(..), StepPlan(..), EngineErr(..), commitStep, planStep)
import Hfsm.Runtime.Model (InstanceRw(..), SignalRw(..), SignalStatus(..), SnapshotRw(..), StepRw(..))
import Hfsm.Runtime.Registry (machineKeyOf)
import Hfsm.Store.Class (AckRq(..), CommitRq(..), CommitRez(..), LeaseRq(..), LeaseRez(..), PollRq(..), Store(..))


regression :: IO ()
regression = do
  spec <- either (fail . show) pure approvalSpec
  compiled <- either (fail . show) pure (compile spec)
  store <- newMemoryStore
  let api = storeOf store
  instId <- createInstance store compiled initialCtx
  _ <- enqueueSignal store instId (SubmitSg "alice")
  workerId <- U4.nextRandom
  now <- getCurrentTime
  leased <- api.leaseSignal LeaseRq
    { node = workerId, batchSize = 1, leaseSeconds = 60, now = now }
  sig <- case leased of
    LeaseSomeLz [x] -> pure x
    other -> fail ("expected one leased signal: " <> show other)
  leaseId <- maybe (fail "missing lease token") pure sig.lease
  Just inst0 <- api.loadInstance instId
  Just snap0 <- api.loadSnapshot inst0.snap
  let input = StepInput
        { machine = machineKeyOf compiled
        , instance_ = inst0, snapshot = snap0, signal = sig }
  plan <- either (fail . show) pure (planStep compiled input)

  -- Deliberate CAS mismatch: NO step, snapshot, side effect, ACK or projection
  -- may be persisted. This must also preserve the live lease for retry.
  let request = CommitRq
        { instanceId = instId
        , expectedSnapshot = U.nil
        , expectedRevision = inst0.revision
        , signalId = sig.uuid
        , signalOrdinal = sig.ordinal
        , leaseId = leaseId
        , step = plan.step
        , snapshot = plan.snapshot
        , nextStatus = plan.nextStatus
        , outbox = plan.outbox
        , child = plan.child
        , breaks = plan.breaks
        , proj = plan.proj }
  bad <- api.commitTurn request
  case bad of
    Left _ -> pure ()
    Right _ -> fail "stale CAS unexpectedly committed"
  Just instAfterBad <- api.loadInstance instId
  assert (instAfterBad.snap == inst0.snap) "CAS failure mutated instance"
  viewBad <- loadInstanceView store instId
  assert (arrayLength "steps" viewBad == 0) "failed transaction created a step"
  assert (arrayLength "outbox" viewBad == 0) "failed transaction created an effect"
  assert (nestedArrayLength "projections" "trace" viewBad == 0) "failed transaction changed projections"
  leasedAfterBad <- api.listSignals PollRq { status = [LeasedSs], limit = 10, now = now }
  assert (length leasedAfterBad == 1) "failed transaction consumed its leased input"

  good <- commitStep api input plan
  applied <- either (fail . show) pure good
  assert (applied.step.uuid == plan.step.uuid) "committed step UUID changed"
  assert (applied.snapshot.uuid == plan.snapshot.uuid) "committed snapshot UUID changed"
  Just instAfterGood <- api.loadInstance instId
  assert (instAfterGood.snap == applied.snapshot.uuid) "snapshot pointer not updated"
  viewGood <- loadInstanceView store instId
  assert (arrayLength "steps" viewGood == 1) "expected exactly one committed step"
  assert (arrayLength "outbox" viewGood == 1) "expected the Submit notification command"
  assert (nestedArrayLength "projections" "trace" viewGood == 1) "expected one trace projection"
  assert (firstOutboxField "inst" viewGood == Just (U.toText instId)) "outbox is not bound to instance"
  assert (firstOutboxField "step" viewGood == Just (U.toText applied.step.uuid)) "outbox is not bound to committed step"
  statuses <- api.listSignals PollRq { status = [AppliedSs], limit = 10, now = now }
  assert (length statuses == 1) "expected exactly one applied signal"
  assert (all (\row -> row.status == AppliedSs) statuses) "success signal not ACKed in transaction"

  -- A second commit from the same planned/leased input must fail without
  -- adding anything. Success has already released/consumed the input lease.
  duplicate <- commitStep api input plan
  case duplicate of
    Left (StoreCommitEr _) -> pure ()
    other -> fail ("duplicate commit not rejected: " <> show other)
  viewDuplicate <- loadInstanceView store instId
  assert (arrayLength "steps" viewDuplicate == 1) "duplicate commit created another step"
  failedAck <- api.ackSignal AckRq
    { signal = sig.uuid, lease = leaseId, status = FailedSs
    , note = Nothing, now = now }
  case failedAck of
    Left _ -> pure ()
    Right () -> fail "post-commit failure ACK incorrectly changed an applied signal"
  putStrLn "CommitTurnRegression: PASS"

assert :: Bool -> String -> IO ()
assert condition message = unless condition (fail message)

arrayLength :: String -> Value -> Int
arrayLength key val = case val of
  Object obj -> case KM.lookup (fromString key) obj of
    Just (Array xs) -> V.length xs
    _ -> 0
  _ -> 0

-- | Read a nested projection array in the JSON debug view.
nestedArrayLength :: String -> String -> Value -> Int
nestedArrayLength parent child val = case val of
  Object obj -> case KM.lookup (fromString parent) obj of
    Just (Object inner) -> case KM.lookup (fromString child) inner of
      Just (Array xs) -> V.length xs
      _ -> 0
    _ -> 0
  _ -> 0

firstOutboxField :: String -> Value -> Maybe Data.Text.Text
firstOutboxField key val = case val of
  Object obj -> case KM.lookup "outbox" obj of
    Just (Array xs) -> case V.toList xs of
      (Object row : _) -> case KM.lookup (fromString key) row of
        Just (String txt) -> Just txt
        _ -> Nothing
      _ -> Nothing
    _ -> Nothing
  _ -> Nothing
