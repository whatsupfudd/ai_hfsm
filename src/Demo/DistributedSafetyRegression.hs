-- | Regression for revision CAS, FIFO admission, fencing and deterministic effects.
module Demo.DistributedSafetyRegression (regression) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)

import qualified Data.Vector as V
import Data.Time.Clock (getCurrentTime)
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U4

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM

import Demo.ApprovalHfsm (approvalSpec, Signal(..))
import Demo.ApprovalRuntime (initialCtx)
import Demo.MemoryStore (createInstance, enqueueSignal, loadInstanceView, newMemoryStore, storeOf)
import Hfsm.Compile (compile)
import Hfsm.Runtime.Engine.Step (StepInput(..), StepPlan(..), planStep, commitStep)
import Hfsm.Runtime.Model (InstanceRw(..), InstanceStatus(..), SignalRw(..), SignalStatus(..), SnapshotRw(..), StepRw(..))
import Hfsm.Runtime.Registry (machineKeyOf)
import Hfsm.Store.Class (Store(..), CommitRq(..), LeaseRq(..), LeaseRez(..), RenewRq(..), StoreErr(..))


assert :: Bool -> String -> IO ()
assert p msg = unless p (fail msg)

only :: LeaseRez -> IO SignalRw
only (LeaseSomeLz [row]) = pure row
only other = fail ("expected one leased signal, got " <> show other)

regression :: IO ()
regression = do
  machine <- either (fail . show) pure approvalSpec
  compiled <- either (fail . show) pure (compile machine)
  store <- newMemoryStore
  let api = storeOf store
  inst <- createInstance store compiled initialCtx
  _ <- enqueueSignal store inst (SubmitSg "alice")
  _ <- enqueueSignal store inst (ApproveSg "carol")
  a <- U4.nextRandom
  b <- U4.nextRandom
  now <- getCurrentTime
  first <- api.leaseSignal LeaseRq
    { node = a, batchSize = 1, leaseSeconds = 30, now = now } >>= only
  assert (first.ordinal == 1) "first ordinal must be 1"
  assert (first.leaseOwner == Just a) "owner must be A"
  blocked <- api.leaseSignal LeaseRq
    { node = b, batchSize = 1, leaseSeconds = 30, now = now }
  assert (blocked == LeaseNoneLz) "worker B leased a concurrent signal on the same instance"
  lease <- maybe (fail "missing token") pure first.lease
  renewed <- api.renewLease RenewRq
    { signal = first.uuid, lease = lease, extendSeconds = 30, now = now }
  case renewed of
    Left e -> fail ("renewLease failed: " <> show e)
    Right _ -> pure ()
  Just ir <- api.loadInstance inst
  assert (ir.revision == 0) "new instances must start at revision zero"
  Just snapshot <- api.loadSnapshot ir.snap
  let input = StepInput { machine = machineKeyOf compiled
                        , instance_ = ir, snapshot = snapshot, signal = first }
  plan <- either (fail . show) pure (planStep compiled input)
  let rq = CommitRq
        { instanceId = inst, expectedSnapshot = snapshot.uuid
        , expectedRevision = ir.revision, signalId = first.uuid
        , signalOrdinal = first.ordinal, leaseId = lease
        , step = plan.step, snapshot = plan.snapshot
        , nextStatus = plan.nextStatus, outbox = plan.outbox
        , child = plan.child, breaks = plan.breaks, proj = plan.proj }
  bad <- api.commitTurn (rq { expectedRevision = 1 })
  case bad of
    Left (ConflictEr _) -> pure ()
    other -> fail ("wrong revision wasn't fenced: " <> show other)
  ok <- commitStep api input plan
  either (fail . show) (const (pure ())) ok
  Just ir1 <- api.loadInstance inst
  assert (ir1.revision == 1) "commit must increment instance revision"
  twice <- api.commitTurn rq
  case twice of
    Left _ -> pure ()
    Right _ -> fail "duplicate commit succeeded"
  view <- loadInstanceView store inst
  let outbox = case view of
        A.Object fields -> case KM.lookup "outbox" fields of
          Just (A.Array rows) -> V.toList rows
          _ -> []
        _ -> []
  assert (length outbox == 1) "must have exactly one durable effect"
  let stable = case outbox of
        [A.Object row] -> KM.lookup "effectKey" row
        _ -> Nothing
  assert (stable == Just (A.String (U.toText plan.step.uuid <> ":0")))
    "effect idempotency key must be stable and based on step UUID + ordinal"

  second <- api.leaseSignal LeaseRq
    { node = b, batchSize = 1, leaseSeconds = 30, now = now } >>= only
  assert (second.ordinal == 2) "second instance-local ordinal must be 2"
  Just ir2 <- api.loadInstance inst
  Just snap2 <- api.loadSnapshot ir2.snap
  next <- either (fail . show) pure (planStep compiled StepInput
    { machine = machineKeyOf compiled, instance_ = ir2
    , snapshot = snap2, signal = second })
  token2 <- maybe (fail "missing second token") pure second.lease
  committed <- api.commitTurn CommitRq
    { instanceId = inst, expectedSnapshot = snap2.uuid
    , expectedRevision = ir2.revision, signalId = second.uuid
    , signalOrdinal = second.ordinal, leaseId = token2
    , step = next.step, snapshot = next.snapshot, nextStatus = next.nextStatus
    , outbox = next.outbox, child = next.child, breaks = next.breaks, proj = next.proj }
  either (fail . show) (const (pure ())) committed
  Just finished <- api.loadInstance inst
  assert (finished.revision == 2) "revision must advance to 2"
  assert (finished.status == DoneIs) "root terminal state must end the instance"

  -- Lost lease after expiration: use real time, not untrusted request timestamps.
  otherStore <- newMemoryStore
  let api2 = storeOf otherStore
  inst2 <- createInstance otherStore compiled initialCtx
  _ <- enqueueSignal otherStore inst2 (SubmitSg "alice")
  t0 <- getCurrentTime
  old <- api2.leaseSignal LeaseRq
    { node = a, batchSize = 1, leaseSeconds = 1, now = t0 } >>= only
  threadDelay 1300000
  t1 <- getCurrentTime
  reassigned <- api2.leaseSignal LeaseRq
    { node = b, batchSize = 1, leaseSeconds = 30, now = t1 } >>= only
  assert (reassigned.uuid == old.uuid) "reclaim must lease the SAME signal"
  assert (reassigned.lease /= old.lease) "reclaim must issue a fresh token"
  assert (reassigned.attempts == old.attempts + 1) "reclaim must increment attempts"
  Just start <- api2.loadInstance inst2
  Just ss <- api2.loadSnapshot start.snap
  oldPlan <- either (fail . show) pure (planStep compiled StepInput
    { machine = machineKeyOf compiled, instance_ = start
    , snapshot = ss, signal = old })
  oldToken <- maybe (fail "missing expired token") pure old.lease
  late <- api2.commitTurn CommitRq
    { instanceId = inst2, expectedSnapshot = ss.uuid
    , expectedRevision = start.revision, signalId = old.uuid
    , signalOrdinal = old.ordinal, leaseId = oldToken
    , step = oldPlan.step, snapshot = oldPlan.snapshot
    , nextStatus = oldPlan.nextStatus, outbox = oldPlan.outbox
    , child = oldPlan.child, breaks = oldPlan.breaks, proj = oldPlan.proj }
  case late of
    Left (LeaseLostEr _) -> pure ()
    other -> fail ("expired worker committed: " <> show other)
  Just untouched <- api2.loadInstance inst2
  assert (untouched.revision == 0) "stale worker changed revision"
  putStrLn "DistributedSafetyRegression: PASS"
