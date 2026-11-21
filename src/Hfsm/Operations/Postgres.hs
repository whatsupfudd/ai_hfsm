{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- | CT-06 production access. Callers must use these constructors instead of
-- Pg.postgresStore/PgActivity.activityStore to obtain admission guarantees.
-- The inherited CT03/CT04 commit, ACK, and renewal methods remain unchanged.
module Hfsm.Operations.Postgres
  ( registerWorker, heartbeatWorker, setWorkerState
  , productionStore, productionStoreForInstance, productionActivityStore
  , enqueueWithPolicy, createTenant, createTenantInstance
  , cancelInstance, projectInstance, resetProjection, loadProjection
  , deadLetters, redriveSignal, setGroupLimits
  ) where

import Control.Exception (throwIO)
import Data.Aeson (Value, FromJSON, fromJSON, Result(..))
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as U
import qualified Data.Vector as V
import qualified Hasql.Session as Session
import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as TS

import Hfsm.Activity.Types (ActivityStore(..), ActivityLease, ActivityErr(..), ClaimRq(..))
import Hfsm.Operations.Types
  ( OpErr(..), WorkerRegistration(..), EnqueueOptions(..), CancelRequest(..),
    CancelResult(..), DeadLetter )
import Hfsm.Store.Class (Store(..), LeaseRq(..), LeaseRez(..))
import Hfsm.Store.Postgres.Types (Runner(..), NewInstance, EnqueueRq(..))
import qualified Hfsm.Store.Postgres as Pg
import qualified Hfsm.Store.Postgres.Transaction as HfsmTx
import qualified Hfsm.Store.Postgres.Activity as PgActivity
import qualified Hfsm.Store.Postgres.Activity.Statements as ActivityS
import qualified Hfsm.Operations.Postgres.Statements as S

writeTx :: Tx.Transaction a -> Session.Session a
writeTx = TS.transactionNoRetry TS.ReadCommitted TS.Write

exec :: Runner -> Session.Session a -> IO (Either OpErr a)
exec runner action = do
  result <- runSession runner action
  pure $ either (Left . OpBackend . T.pack . show) Right result

registerWorker :: Runner -> WorkerRegistration -> IO (Either OpErr UUID)
registerWorker runner w = exec runner $ Session.statement
  (w.workerId,V.fromList w.capabilities, clamp 2 3600 w.heartbeatTtlSeconds,
   clamp 1 1000 w.maxSignals,clamp 1 1000 w.maxActivities) S.registerWorker

heartbeatWorker :: Runner -> UUID -> UUID -> IO (Either OpErr Bool)
heartbeatWorker runner wid generation =
  exec runner (Session.statement (wid,generation) S.heartbeatWorker)

setWorkerState :: Runner -> UUID -> UUID -> Text -> IO (Either OpErr Bool)
setWorkerState runner wid gen state =
  fmap (fmap (maybe False id)) $ exec runner $
    Session.statement (wid,gen,state) S.setWorkerState

productionStore :: Runner -> Store IO
productionStore runner = productionStoreMaybe runner Nothing

productionStoreForInstance :: Runner -> UUID -> Store IO
productionStoreForInstance runner ident = productionStoreMaybe runner (Just ident)

productionStoreMaybe :: Runner -> Maybe UUID -> Store IO
productionStoreMaybe runner selected =
  let base = maybe (Pg.postgresStore runner) (Pg.postgresStoreForInstance runner) selected
  in base { leaseSignal = \req -> do
       result <- exec runner $ writeTx $ Tx.statement
         (req.node,clamp 1 100 req.batchSize,clamp 1 3600 req.leaseSeconds,selected)
         S.claimSignals
       case result of
         Left err -> throwIO (userError (show err))
         Right raw -> case traverse parse (V.toList raw) of
           Left err -> throwIO (userError (show err))
           Right [] -> pure LeaseNoneLz
           Right rows -> pure (LeaseSomeLz rows)
     }

productionActivityStore :: Runner -> ActivityStore IO
productionActivityStore runner =
  let base = PgActivity.activityStore runner
  in base { claim = \req -> do
       rez <- exec runner $ writeTx $ do
         -- Keep the CT04 expiry/unknown-outcome policy before admission.
         Tx.statement req.instanceFilter ActivityS.reapExhausted
         Tx.statement
           (req.node,clamp 1 100 req.batchSize,clamp 1 3600 req.leaseSeconds,
            req.instanceFilter,V.fromList req.safeKinds,V.fromList req.supportedKinds)
           S.claimActivities
       pure $ case rez of
         Left err -> Left (ActivityBackend (T.pack (show err)))
         Right raw -> either (Left . ActivityDecode . T.pack . show) Right
                      (traverse parse (V.toList raw))
     }

enqueueWithPolicy :: Runner -> EnqueueRq -> EnqueueOptions -> IO (Either OpErr UUID)
enqueueWithPolicy runner rq opts = exec runner $ writeTx $ Tx.statement
  (rq.instanceId,rq.signalId,rq.payload,rq.cause,rq.dedupeKey,
   opts.priority,opts.correlationId,opts.causationId,opts.traceparent,
   opts.expectedRelease,rq.readyAt) S.enqueueManaged

-- | Creates tenant and its required default group atomically.
createTenant :: Runner -> UUID -> Text -> Int -> Int -> Int -> Int -> Int
             -> IO (Either OpErr ())
createTenant runner tid name instLimit backlogLimit sigLimit actLimit attempts =
  exec runner $ writeTx $ do
    Tx.statement (tid,name,clamp 1 1000000 instLimit,clamp 1 10000000 backlogLimit,
        clamp 1 100000 sigLimit,clamp 1 100000 actLimit,clamp 1 1000 attempts) S.createTenant
    Tx.statement tid S.createDefaultGroup

-- | Tenant identity and group metadata apply only in this same SQL transaction.
-- Keeps the original CT03 atomic initial snapshot/instance creation logic.
createTenantInstance :: Runner -> UUID -> Text -> NewInstance -> IO (Either OpErr UUID)
createTenantInstance runner tid groupKey new = do
  outer <- exec runner $ writeTx $ do
    _ <- Tx.statement ("hfsm.tenant_id", U.toText tid) S.setLocalConfig
    _ <- Tx.statement ("hfsm.group_key", groupKey) S.setLocalConfig
    HfsmTx.createInstanceTx new
  pure $ case outer of
    Left err -> Left err
    Right (Left err) -> Left (OpRejected (T.pack (show err)))
    Right (Right ident) -> Right ident

cancelInstance :: Runner -> CancelRequest -> IO (Either OpErr CancelResult)
cancelInstance runner rq = do
  result <- exec runner $ writeTx $ Tx.statement
    (rq.instanceId,rq.expectedRevision,rq.operationId,rq.actor,rq.reason)
    S.cancelInstance
  pure $ result >>= \status -> case status of
    "cancelled" -> Right Cancelled
    "already_cancelled" -> Right AlreadyCancelled
    "conflict" -> Right CancelConflict
    "terminal" -> Right CancelTerminal
    "not_found" -> Right CancelMissing
    other -> Left (OpRejected ("unknown cancel outcome: " <> other))

projectInstance :: Runner -> Text -> UUID -> Int -> IO (Either OpErr Int)
projectInstance runner consumer ident page =
  fmap (fmap fromIntegral) $ exec runner $ writeTx $
    Tx.statement (consumer,ident,clamp 1 1000 page) S.projectInstance

-- | Delete only derived views. Must be run by an authorized administrative DB role.
resetProjection :: Runner -> Text -> UUID -> IO (Either OpErr ())
resetProjection runner consumer ident =
  fmap (fmap (const ())) $ exec runner $ writeTx $
    Tx.statement (consumer,ident) S.resetProjection

loadProjection :: Runner -> Text -> UUID -> IO (Either OpErr (Maybe Value))
loadProjection runner consumer ident =
  exec runner $ Session.statement (consumer,ident) S.readActive

deadLetters :: Runner -> UUID -> Int -> IO (Either OpErr [DeadLetter])
deadLetters runner tenant page = do
  raw <- exec runner $ Session.statement (tenant,clamp 1 500 page) S.readDeadLetters
  pure (raw >>= traverse parse . V.toList)

parse :: FromJSON a => Value -> Either OpErr a
parse value = case fromJSON value of
  Error message -> Left (OpDecode (T.pack message))
  Success decoded -> Right decoded

clamp :: Int32 -> Int32 -> Int -> Int32
clamp low high value =
  fromIntegral (max (fromIntegral low :: Int)
                   (min (fromIntegral high :: Int) value))

-- | Redrive is allowed only for a failed SIGNAL. It creates a new sequenced
-- input and appends an operator audit record; effect retries require separate
-- reconciliation, since external calls may already have happened.
redriveSignal :: Runner -> Int64 -> UUID -> UUID -> Text -> Text -> Maybe Text
              -> IO (Either OpErr UUID)
redriveSignal runner deadLetterId newSignalId operationId actor reason expectedRelease =
  exec runner $ writeTx $ Tx.statement
    (deadLetterId,newSignalId,operationId,actor,reason,expectedRelease)
    S.redriveSignal

-- | Limits are changed under the same admission gate used by CT06 claimers.
setGroupLimits :: Runner -> UUID -> Text -> Int -> Int -> IO (Either OpErr Bool)
setGroupLimits runner tenant groupKey signals activities =
  exec runner $ writeTx $ Tx.statement
    (tenant,groupKey,clamp 1 100000 signals,clamp 1 100000 activities)
    S.setGroupLimits
