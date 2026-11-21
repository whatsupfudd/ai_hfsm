{-# LANGUAGE NoFieldSelectors #-}

-- | Atomic, CAS-guarded explicit migration. Incompatible signals/effects
-- must be drained first; migration never runs arbitrary Haskell callbacks
-- inside SQL, nor mutates any prior snapshot or step.
module Hfsm.Migration.Postgres (applyMigration) where

import Data.Aeson (Value, object, toJSON, (.=))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as TS
import qualified Hasql.Session as Session

import Hfsm.Core.Digest (specDigestText)
import Hfsm.Core.Name (machineNameText)
import Hfsm.Core.Ref (stateRefWord32)
import Hfsm.Core.Version (machineVersionText)
import Hfsm.Runtime.Model (InstanceStatus(..))
import Hfsm.Runtime.Model.Instance (instanceStatusText)
import Hfsm.Runtime.Registry (MachineKey(..))
import Hfsm.Store.Postgres.Types (Runner(..))
import Hfsm.Migration.Types (MigrationErr(..), MigrationRequest(..), MigrationReceipt(..))
import qualified Hfsm.Store.Postgres.Statements as S
import qualified Hfsm.Migration.Statements as M

applyMigration :: Runner -> MigrationRequest
               -> IO (Either MigrationErr MigrationReceipt)
applyMigration runner rq = do
  result <- runSession runner $
    TS.transactionNoRetry TS.ReadCommitted TS.Write (migrateTx rq)
  pure $ case result of
    Left errorValue -> Left (MigrationStorageError (T.pack (show errorValue)))
    Right value -> value

abort :: MigrationErr -> Tx.Transaction (Either MigrationErr a)
abort err = Tx.condemn *> pure (Left err)

migrateTx :: MigrationRequest
          -> Tx.Transaction (Either MigrationErr MigrationReceipt)
migrateTx rq = do
  mi <- Tx.statement rq.instanceId S.lockInstance
  case mi of
    Nothing -> abort (MigrationConflict "instance not found")
    Just (snap,rev,inSig,_inLease,_until,name,version,digest,state,_path,status) -> do
      earlier <- Tx.statement rq.operationId M.lookupOperation
      case earlier of
        Just (existingInst,existingRev,existingStep,existingSnap,existingVersion,existingDigest,existingMapping)
          | existingInst == rq.instanceId && existingStep == rq.stepId &&
            existingSnap == rq.snapshotId && existingRev == rq.expectedRevision+1 &&
            existingVersion == machineVersionText rq.targetKey.version &&
            existingDigest == specDigestText rq.targetKey.digest &&
            existingMapping == rq.mappingName ->
              pure (Right (MigrationReceipt rq.operationId rq.instanceId existingRev existingStep existingSnap))
          | otherwise -> abort (MigrationConflict "operation ID reused with different migration")
        Nothing -> do
          outstanding <- Tx.statement rq.instanceId M.pendingWork
          case validateMigration rq snap rev inSig name version digest state status outstanding of
            Left err -> abort err
            Right nextRev -> do
              let note = migrationNote rq
                  fromRef = fromIntegral (stateRefWord32 rq.fromState) :: Int64
                  toRef = fromIntegral (stateRefWord32 rq.toState) :: Int64
                  nextVersion = machineVersionText rq.targetKey.version
                  nextDigest = specDigestText rq.targetKey.digest
              _ <- Tx.statement
                (rq.stepId,rq.instanceId,nextRev,rq.expectedSnapshot,
                 fromRef,toRef,note,rq.createdAt)
                M.insertMigrationStep
              _ <- Tx.statement
                (rq.snapshotId,rq.instanceId,nextRev,rq.stepId,toRef,
                 toJSON rq.toPath,rq.newCtx,rq.newWait,rq.newChild,
                 nextVersion,nextDigest,rq.createdAt)
                S.insertTurnSnapshot
              advanced <- Tx.statement
                (rq.instanceId,rq.expectedSnapshot,rq.expectedRevision,
                 machineVersionText rq.sourceKey.version,
                 specDigestText rq.sourceKey.digest,
                 nextVersion,nextDigest,rq.snapshotId,toRef,
                 toJSON rq.toPath,instanceStatusText rq.nextStatus)
                M.advanceMigration
              case advanced of
                Nothing -> abort (MigrationConflict "migration CAS or outstanding work changed")
                Just persistedRevision -> do
                  Tx.statement
                    (rq.operationId,rq.instanceId,nextRev,rq.stepId,
                     rq.expectedSnapshot,rq.snapshotId,
                     toJSON rq.sourceKey,toJSON rq.targetKey,
                     rq.mappingName,rq.oldCtx,rq.newCtx,rq.createdAt)
                    M.insertEvent
                  Tx.statement (rq.instanceId,rq.stepId,nextRev,
                    activeProjection rq,traceProjection rq note) M.insertProjections
                  pure (Right (MigrationReceipt rq.operationId rq.instanceId persistedRevision rq.stepId rq.snapshotId))

validateMigration
  :: MigrationRequest
  -> UUID.UUID -> Int64 -> Maybe UUID.UUID -> Text -> Text -> Text
  -> Int64 -> Text -> (Int64,Int64,Int64)
  -> Either MigrationErr Int64
validateMigration rq snapshotId rev inSig name version digest state status (signals,effects,children)
  | rev == maxBound = bad "instance revision exhausted"
  | rq.expectedRevision /= rev = bad "stale expected revision"
  | rq.expectedSnapshot /= snapshotId = bad "stale snapshot"
  | name /= machineNameText rq.sourceKey.name ||
    version /= machineVersionText rq.sourceKey.version ||
    digest /= specDigestText rq.sourceKey.digest = bad "instance pinned to different release"
  | rq.targetKey.name /= rq.sourceKey.name || rq.targetKey == rq.sourceKey =
      bad "invalid target release identity"
  | fromIntegral (stateRefWord32 rq.fromState) /= state = bad "old state changed"
  | inSig /= Nothing = bad "instance has an active admission lease"
  | status `notElem` ["running","waiting","paused"] = bad "terminal instance"
  | signals /= 0 || effects /= 0 || children /= 0 =
      bad "pending signals, outbox deliveries or child runs must be drained first"
  | rq.instanceId == UUID.nil || rq.stepId == UUID.nil || rq.snapshotId == UUID.nil =
      bad "nil migration UUID"
  | otherwise = Right (rev + 1)
 where bad = Left . MigrationConflict

migrationNote :: MigrationRequest -> Value
migrationNote rq = object
  [ "kind" .= ("migrate" :: Text)
  , "operationId" .= rq.operationId
  , "mappingName" .= rq.mappingName
  , "sourceKey" .= rq.sourceKey
  , "targetKey" .= rq.targetKey
  , "fromState" .= rq.fromState
  , "toState" .= rq.toState
  , "status" .= instanceStatusText rq.nextStatus
  , "replayable" .= False
  ]

activeProjection :: MigrationRequest -> Value
activeProjection rq = object
  [ "instance_" .= rq.instanceId
  , "machine" .= machineNameText rq.targetKey.name
  , "version" .= machineVersionText rq.targetKey.version
  , "status" .= instanceStatusText rq.nextStatus
  , "state" .= rq.toState, "path" .= rq.toPath
  , "wait" .= rq.newWait
  , "blockedOn" .= (Nothing :: Maybe Text)
  , "updatedAt" .= rq.createdAt ]

traceProjection :: MigrationRequest -> Value -> Value
traceProjection rq note = object
  [ "inst" .= rq.instanceId, "step" .= rq.stepId
  , "signal" .= (Nothing :: Maybe UUID.UUID)
  , "from" .= rq.fromState, "to" .= rq.toState
  , "route" .= (0 :: Int), "note" .= note
  , "createdAt" .= rq.createdAt ]
