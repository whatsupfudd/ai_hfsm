{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- | DF-01 PostgreSQL adapter. Each mutation is one SQL transaction; the
-- PostgreSQL functions implement atomic token emission, fencing and quotas.
module Hfsm.Flow.Postgres
  ( registerDefinition, startRun, publishArtifact, registerResources
  , flowStore, flowStoreForRun, cancelRun, deliverCompletion, pendingCompletions
  , loadRunSummary, loadRunEvents, loadRunOutputs
  ) where

import Data.Aeson (FromJSON, Result(..), Value, fromJSON, toJSON, object, (.=))
import qualified Data.Aeson.Key as K
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID.V4 as U4
import qualified Data.Vector as V
import qualified Hasql.Session as Session
import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as TS

import Hfsm.Flow.Spec (FlowSpec, validateFlowSpec)
import Hfsm.Flow.Types
  ( FlowErr(..), FlowStore(..), FlowTaskLease(..), PublishArtifact(..)
  , StartRun(..), TaskOutputs(..), TaskFailure(..), WorkerResources(..) )
import Hfsm.Store.Postgres.Types (Runner(..))
import qualified Hfsm.Flow.Postgres.Statements as S

writeTx :: Tx.Transaction a -> Session.Session a
writeTx = TS.transactionNoRetry TS.ReadCommitted TS.Write

call :: Runner -> Session.Session a -> IO (Either FlowErr a)
call runner action = do
  result <- runSession runner action
  pure $ either (Left . FlowBackend . T.pack . show) Right result

parse :: FromJSON a => Value -> Either FlowErr a
parse raw = case fromJSON raw of
  Error msg -> Left (FlowDecode (T.pack msg))
  Success x -> Right x

registerDefinition :: Runner -> UUID -> UUID -> Text -> Text -> FlowSpec
                   -> IO (Either FlowErr UUID)
registerDefinition runner ident tenant name version spec =
  case validateFlowSpec spec of
    Left err -> pure (Left (FlowInvalid (T.pack (show err))))
    Right () -> call runner (Session.statement
      (ident,tenant,name,version,toJSON spec) S.registerDefinition)

registerResources :: Runner -> WorkerResources -> IO (Either FlowErr Bool)
registerResources runner resources = call runner $ Session.statement
  ( resources.workerId
  , fromIntegral resources.cpuUnits :: Int32
  , fromIntegral resources.memoryMb :: Int32
  , fromIntegral resources.maxTasks :: Int32) S.registerResources

startRun :: Runner -> StartRun -> IO (Either FlowErr UUID)
startRun runner rq = call runner $ Session.statement
  (rq.runId,rq.definitionId,rq.sourceEffectKey,rq.sourceInstance,
   rq.sourceRelease,rq.runInput,rq.successSignal,rq.failureSignal) S.startRun

publishArtifact :: Runner -> PublishArtifact -> IO (Either FlowErr UUID)
publishArtifact runner a = call runner $ Session.statement
  (a.artifactId,a.tenantId,a.digest,a.sizeBytes,a.storageUri,a.mediaType,a.metadata)
  S.publishArtifact

flowStore :: Runner -> FlowStore IO
flowStore runner = flowStoreScoped runner Nothing

-- | Regression/dev-only scoped claims. Production must use flowStore.
flowStoreForRun :: Runner -> UUID -> FlowStore IO
flowStoreForRun runner ident = flowStoreScoped runner (Just ident)

flowStoreScoped :: Runner -> Maybe UUID -> FlowStore IO
flowStoreScoped runner selected = FlowStore
  { claim = \worker count seconds -> do
      rez <- call runner $ Session.statement
        (worker,fromIntegral (max 1 (min 100 count)) :: Int32,
         fromIntegral (max 1 (min 3600 seconds)) :: Int32,selected) S.claimTasks
      pure (rez >>= traverse parse . V.toList)
  , renew = \lease seconds -> do
      rez <- call runner $ Session.statement
        (lease.runId,lease.taskKey,lease.leaseId,lease.workerId,
         fromIntegral (max 1 (min 3600 seconds)) :: Int32) S.renewTask
      pure $ case rez of
        Right Nothing -> Left (FlowLeaseLost "fenced lease expired or replaced")
        Right (Just expiry) -> Right expiry
        Left err -> Left err
  , finish = \rq -> do
      let values = object [K.fromText port .= artifact | (port,artifact) <- rq.outputs]
      call runner $ Session.statement
        (rq.runId,rq.taskKey,rq.leaseId,rq.workerId,
         fromIntegral rq.attempt :: Int32,values) S.finishTask
  , failTask = \rq -> call runner $ Session.statement
      (rq.runId,rq.taskKey,rq.leaseId,rq.workerId,
       fromIntegral rq.attempt :: Int32,rq.reason,rq.retryable) S.failTask
  }

cancelRun :: Runner -> UUID -> Text -> IO (Either FlowErr Bool)
cancelRun runner ident reason = call runner (Session.statement (ident,reason) S.cancelRun)

deliverCompletion :: Runner -> UUID -> IO (Either FlowErr (Maybe UUID))
deliverCompletion runner ident = do
  candidate <- U4.nextRandom
  call runner (Session.statement (ident,candidate) S.deliverCompletion)

pendingCompletions :: Runner -> Int -> IO (Either FlowErr [UUID])
pendingCompletions runner limit = do
  rez <- call runner $ Session.statement
    (fromIntegral (max 1 (min 1000 limit)) :: Int32) S.pendingCompletions
  pure (V.toList <$> rez)

loadRunSummary :: Runner -> UUID -> IO (Either FlowErr (Maybe Value))
loadRunSummary runner ident = call runner (Session.statement ident S.runSummary)

loadRunEvents :: Runner -> UUID -> IO (Either FlowErr [Value])
loadRunEvents runner ident =
  fmap (fmap V.toList) (call runner (Session.statement ident S.runEvents))

loadRunOutputs :: Runner -> UUID -> IO (Either FlowErr [Value])
loadRunOutputs runner ident =
  fmap (fmap V.toList) (call runner (Session.statement ident S.runOutputs))
