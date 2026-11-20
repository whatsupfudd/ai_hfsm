{-# LANGUAGE NoFieldSelectors #-}

-- | Transaction plans for CT-02A. Each operation is one Hasql.Transaction;
-- failures are condemned and so cannot commit partial updates.
module Hfsm.Store.Postgres.Transaction
  ( commitTurnTx, ackFailureTx, renewLeaseTx, claimSignalsTx
  , registerReleaseTx, createInstanceTx, enqueueTx
  ) where

import Control.Monad (unless)
import Data.Aeson (Value, toJSON)
import Data.Int (Int32, Int64)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Time (UTCTime, addUTCTime)
import Data.UUID (UUID)
import qualified Data.UUID as U
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Hasql.Transaction as Tx

import Hfsm.Core.Digest (specDigestText)
import Hfsm.Core.Name (machineNameText)
import Hfsm.Core.Version (machineVersionText)
import qualified Hfsm.Core.Ref as Ref
import Hfsm.Runtime.Registry (MachineKey(..))
import Hfsm.Runtime.Model
  ( InstanceStatus(..), SignalStatus(..), StepRw(..), SnapshotRw(..)
  , InstanceRw(..), SignalRw(..) )
import Hfsm.Runtime.Model.Instance (instanceStatusText)
import Hfsm.Store.Class
  ( StoreErr(..), CommitRq(..), CommitRez(..), AckRq(..), RenewRq(..)
  , OutboxAppend(..), ChildAppend(..), BreakAppend(..) )
import Hfsm.Store.Postgres.Codec
  ( ref32, caseRef32, stepKindSql, signalStatusSql, projectionValues
  , outboxValues, childValues, breakValues )
import qualified Hfsm.Store.Postgres.Statements as S
import Hfsm.Store.Postgres.Types (NewInstance(..), EnqueueRq(..))

-- The tuple is emitted by lockInstance; updating this requires changing its SQL.
type LockedInstance =
  (UUID, Int64, Maybe UUID, Maybe UUID, Maybe UTCTime,
   Text, Text, Text, Int64, Value, Text)

type LockedSignal = (UUID, Text, Maybe UUID, Maybe UTCTime, Int64)

abort :: StoreErr -> Tx.Transaction (Either StoreErr a)
abort e = Tx.condemn *> pure (Left e)

commitTurnTx :: CommitRq -> Tx.Transaction (Either StoreErr CommitRez)
commitTurnTx rq = do
  mi <- Tx.statement rq.instanceId S.lockInstance
  case mi of
    Nothing -> abort (NotFoundEr "commitTurn: instance does not exist")
    Just inst -> do
      ms <- Tx.statement rq.signalId S.lockSignal
      now <- Tx.statement () S.serverNow
      case ms of
        Nothing -> abort (NotFoundEr "commitTurn: signal does not exist")
        Just sig -> case validateTurn rq inst sig now of
          Left e -> abort e
          Right revision -> do
            let step = rq.step
                snap = rq.snapshot
                instId = rq.instanceId
                stepId = step.uuid
                snapId = snap.uuid
                nextRev = revision + 1
            stepUid <- Tx.statement
              (stepId, instId, nextRev, rq.signalId, rq.expectedSnapshot,
               stepKindSql step.kind, ref32 step.from, ref32 step.to,
               fmap caseRef32 step.caseRef, fromIntegral (Ref.routeRefWord32 step.route),
               step.note, step.createdAt) S.insertStep
            snapUid <- Tx.statement
              (snapId, instId, nextRev, stepId, ref32 snap.state,
               toJSON snap.path, snap.ctx, snap.wait, snap.child,
               machineVersionText snap.version, specDigestText snap.digest,
               snap.createdAt) S.insertTurnSnapshot
            unless (null rq.outbox) $
              Tx.statement (instId, stepId, step.createdAt, outboxValues rq.outbox) S.insertOutboxBatch
            unless (null rq.child) $
              Tx.statement (instId, stepId, step.createdAt, childValues rq.child) S.insertChildBatch
            unless (null rq.breaks) $
              Tx.statement (instId, stepId, step.createdAt, breakValues rq.breaks) S.insertBreakBatch
            let pv = projectionValues rq.proj
            Tx.statement (instId, stepId, nextRev, pv) S.insertProjectionBatch
            advanced <- Tx.statement
              (instId, rq.expectedSnapshot, revision, snapId,
               rq.signalId, ref32 snap.state, toJSON snap.path,
               instanceStatusText rq.nextStatus, rq.leaseId) S.advanceInstance
            case advanced of
              Nothing -> abort (ConflictEr "commitTurn: instance CAS/lease changed")
              Just _ -> do
                applied <- Tx.statement (rq.signalId, rq.leaseId) S.applySignal
                case applied of
                  Nothing -> abort (LeaseLostEr "commitTurn: signal lease expired or changed before ACK")
                  Just _ -> pure (Right (CommitRez
                    { step = step { uid = stepUid }
                    , snapshot = snap { uid = snapUid } }))

validateTurn :: CommitRq -> LockedInstance -> LockedSignal -> UTCTime -> Either StoreErr Int64
validateTurn rq (snapshotId, revision, inSig, inLease, inUntil,
                  _mName, mVersion, mDigest, stateRef, _pathVal, instStatus)
                (sigInst, sigStatus, sigLease, sigUntil, sigOrdinal) now
  | instStatus `notElem` ["running", "waiting", "paused"] = Left (ConflictEr "commitTurn: instance terminal")
  | revision == maxBound = Left (ConflictEr "commitTurn: instance revision exhausted")
  | rq.expectedRevision /= revision = Left (ConflictEr "commitTurn: stale expected revision")
  | rq.expectedSnapshot /= snapshotId = Left (ConflictEr "commitTurn: stale expected snapshot")
  | rq.signalId /= inSigId = Left (LeaseLostEr "commitTurn: different in-flight signal")
  | rq.leaseId /= inLeaseId = Left (LeaseLostEr "commitTurn: wrong instance lease token")
  | not (validUntil inUntil) = Left (LeaseLostEr "commitTurn: instance lease expired")
  | sigOrdinal /= rq.signalOrdinal = Left (ConflictEr "commitTurn: wrong instance-local signal ordinal")
  | sigInst /= rq.instanceId = Left (ConflictEr "commitTurn: signal belongs to another instance")
  | sigStatus /= "leased" || sigLease /= Just rq.leaseId = Left (LeaseLostEr "commitTurn: signal not leased by caller")
  | not (validUntil sigUntil) = Left (LeaseLostEr "commitTurn: signal lease expired")
  | rq.step.uuid == U.nil || rq.snapshot.uuid == U.nil = Left (ConflictEr "commitTurn: nil planned UUID")
  | rq.step.inst /= rq.instanceId || rq.snapshot.inst /= rq.instanceId = Left (ConflictEr "commitTurn: step/snapshot instance mismatch")
  | rq.step.signal /= Just rq.signalId = Left (ConflictEr "commitTurn: step/signal mismatch")
  | ref32 rq.step.from /= stateRef = Left (ConflictEr "commitTurn: from-state mismatch")
  | ref32 rq.step.to /= ref32 rq.snapshot.state = Left (ConflictEr "commitTurn: step target mismatch")
  | mVersion /= machineVersionText rq.snapshot.version || mDigest /= specDigestText rq.snapshot.digest =
      Left (ConflictEr "commitTurn: snapshot machine release mismatch")
  | any (\o -> o.inst /= rq.instanceId || o.step /= rq.step.uuid) rq.outbox =
      Left (ConflictEr "commitTurn: outbox unbound to turn")
  | any (\c -> c.parent /= rq.instanceId) rq.child = Left (ConflictEr "commitTurn: child wrong parent")
  | any (\b -> b.inst /= Just rq.instanceId) rq.breaks = Left (ConflictEr "commitTurn: break wrong instance")
  | otherwise = Right revision
 where
  inSigId = maybe U.nil id inSig
  inLeaseId = maybe U.nil id inLease
  validUntil = maybe False (> now)

-- | Failure ACK is independent of successful turn commits, but still atomically
-- clears both signal and per-instance admission slot under the same fence.
ackFailureTx :: AckRq -> Tx.Transaction (Either StoreErr ())
ackFailureTx rq
  | rq.status `notElem` [EnteredSs, RejectedSs, FailedSs] =
      abort (ConflictEr "ackSignal cannot mark a successful turn")
  | otherwise = do
      -- The first read does not lock; it only discovers the parent UUID.
      parent <- Tx.statement rq.signal S.lookupSignalParent
      case parent of
        Nothing -> abort (NotFoundEr "ackSignal: signal missing")
        Just instId -> do
          mi <- Tx.statement instId S.lockInstance
          case mi of
            Nothing -> abort (NotFoundEr "ackSignal: instance missing")
            Just (_,_,inSig,inLease,until_,_,_,_,_,_,_) -> do
              ms <- Tx.statement rq.signal S.lockSignal
              now <- Tx.statement () S.serverNow
              case ms of
                Nothing -> abort (NotFoundEr "ackSignal: signal missing after lock")
                Just (parentId, status, leaseId, leaseUntil, _sigOrdinal)
                  | parentId /= instId || inSig /= Just rq.signal || inLease /= Just rq.lease ->
                      abort (LeaseLostEr "ackSignal: instance fence changed")
                  | status /= "leased" || leaseId /= Just rq.lease ->
                      abort (LeaseLostEr "ackSignal: signal fence changed")
                  | maybe True (<= now) until_ || maybe True (<= now) leaseUntil ->
                      abort (LeaseLostEr "ackSignal: expired lease")
                  | otherwise -> do
                      -- Mutation order doesn't change the lock order.
                      cleared <- Tx.statement (instId, rq.signal, rq.lease) S.clearAdmission
                      released <- Tx.statement (rq.signal, rq.lease,
                                 signalStatusSql rq.status, rq.note) S.releaseSignal
                      if isJust cleared && isJust released
                        then pure (Right ())
                        else abort (LeaseLostEr "ackSignal: fence changed during release")

-- | Extend a still-owned lease using server time and both fencing predicates.
-- Lock order matches commitTurn: instance then signal. Any failed mutation
-- condemns the full transaction, including a provisional signal extension.
renewLeaseTx :: RenewRq -> Tx.Transaction (Either StoreErr UTCTime)
renewLeaseTx rq = do
  parent <- Tx.statement rq.signal S.lookupSignalParent
  case parent of
    Nothing -> abort (NotFoundEr "renewLease: signal missing")
    Just instId -> do
      mi <- Tx.statement instId S.lockInstance
      case mi of
        Nothing -> abort (NotFoundEr "renewLease: instance missing")
        Just (_,_,inSig,inLease,inUntil,_,_,_,_,_,_) -> do
          ms <- Tx.statement rq.signal S.lockSignal
          now <- Tx.statement () S.serverNow
          case ms of
            Just (sigInst, "leased", Just token, Just sigUntil, _ord)
              | sigInst == instId && inSig == Just rq.signal
                  && inLease == Just rq.lease && token == rq.lease
                  && maybe False (> now) inUntil && sigUntil > now -> do
                  let seconds = max 1 (min 3600 rq.extendSeconds)
                      wanted = addUTCTime (fromIntegral seconds) now
                  extended <- Tx.statement (rq.signal, rq.lease, wanted) S.renewSignalLease
                  case extended of
                    Nothing -> abort (LeaseLostEr "renewLease: signal fencing failed")
                    Just until' -> do
                      updated <- Tx.statement (instId, rq.signal, rq.lease, until') S.renewInstanceLease
                      case updated of
                        Nothing -> abort (LeaseLostEr "renewLease: instance fencing failed")
                        Just finalUntil -> pure (Right finalUntil)
            _ -> abort (LeaseLostEr "renewLease: expired/stale lease")


claimSignalsTx :: Int32 -> UUID -> Int32 -> Tx.Transaction (Vector Value)
claimSignalsTx amount node seconds =
  Tx.statement (amount, node, seconds) S.claimSignals

registerReleaseTx :: MachineKey -> Tx.Transaction ()
registerReleaseTx key = Tx.statement
  (machineNameText key.name, machineVersionText key.version,
   specDigestText key.digest) S.registerRelease

-- | An initial instance and its revision-0 snapshot are created together.
-- The deferred instance_current_snapshot_fk permits this FK cycle.
createInstanceTx :: NewInstance -> Tx.Transaction (Either StoreErr UUID)
createInstanceTx new = do
  let key = new.machine
      vers = machineVersionText key.version
      dig = specDigestText key.digest
      instId = new.instanceId
  _ <- Tx.statement
    (instId, machineNameText key.name, vers, dig,
     instanceStatusText new.status, ref32 new.state, toJSON new.path,
     new.snapshotId, new.parent, new.createdAt) S.insertInstance
  _ <- Tx.statement
    (new.snapshotId, instId, ref32 new.state, toJSON new.path,
     new.ctx, new.wait, new.child, vers, dig, new.createdAt) S.insertInitialSnapshot
  pure (Right instId)

-- | Dedupe check + ordinal allocation + insert in a single transaction.
-- Acquires instance lock first; retries with a dedupe key return original ID.
enqueueTx :: EnqueueRq -> Tx.Transaction (Either StoreErr UUID)
enqueueTx rq = do
  mi <- Tx.statement rq.instanceId S.lockInstance
  case mi of
    Nothing -> abort (NotFoundEr "enqueue: missing instance")
    Just _ -> do
      prior <- case rq.dedupeKey of
        Nothing -> pure Nothing
        Just key -> Tx.statement (rq.instanceId, key) S.lookupDedupe
      case prior of
        Just old -> pure (Right old)
        Nothing -> do
          ord <- Tx.statement rq.instanceId S.advanceSignalOrdinal
          case ord of
            Nothing -> abort (NotFoundEr "enqueue: instance disappeared")
            Just n -> do
              Tx.statement
                (rq.signalId, rq.instanceId, n, rq.payload, rq.cause,
                 rq.dedupeKey, rq.readyAt) S.insertSignal
              pure (Right rq.signalId)
