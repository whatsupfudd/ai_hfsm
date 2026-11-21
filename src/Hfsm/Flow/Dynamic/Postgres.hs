{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | DF-02 attaches bounded scatter/gather to existing DF-01 task claims.
-- No new scheduler: CT-06 quotas/resources/worker heartbeats and CT-03 fences
-- remain the only authorities for admission and successful task settlement.
module Hfsm.Flow.Dynamic.Postgres
  ( expandScatter, loadPartitionBinding, loadScatterSummary
  , dynamicFlowStore, dynamicFlowStoreForRun ) where

import Data.Aeson (FromJSON, Result(..), Value, fromJSON, toJSON)
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID.V4 as U4
import qualified Hasql.Session as Session

import Hfsm.Flow.Dynamic.Spec (ScatterPlan, planExpansion)
import Hfsm.Flow.Dynamic.Types (PartitionBinding(..), ScatterSummary)
import Hfsm.Flow.Types (FlowErr(..), FlowStore(..), FlowTaskLease(..),
                        FlowInput(..), TaskOutputs(..))
import qualified Hfsm.Flow.Types as Flow
import Hfsm.Store.Postgres.Types (Runner(..))
import qualified Hfsm.Flow.Postgres as Base
import qualified Hfsm.Flow.Dynamic.Postgres.Statements as Statements

call :: Runner -> Session.Session result -> IO (Either FlowErr result)
call runner session = do
  answer <- runSession runner session
  pure $ either (Left . FlowBackend . T.pack . show) Right answer

parse :: FromJSON value => Value -> Either FlowErr value
parse value = case fromJSON value of
  Error message -> Left (FlowDecode (T.pack message))
  Success result -> Right result

expandScatter :: Runner -> FlowTaskLease -> ScatterPlan -> IO (Either FlowErr UUID)
expandScatter runner lease scatter =
  case planExpansion scatter of
    Left err -> pure (Left (FlowInvalid (T.pack (show err))))
    Right _ -> do
      proposedGroup <- U4.nextRandom
      call runner $ Session.statement
        ( lease.runId, lease.taskKey, lease.leaseId, lease.workerId
        , fromIntegral lease.attempt :: Int32, proposedGroup, toJSON scatter)
        Statements.expandScatter

loadPartitionBinding :: Runner -> UUID -> Text
                     -> IO (Either FlowErr (Maybe PartitionBinding))
loadPartitionBinding runner runId taskKey = do
  answer <- call runner $ Session.statement (runId,taskKey) Statements.partitionInput
  pure (answer >>= traverse parse)

loadScatterSummary :: Runner -> UUID -> IO (Either FlowErr (Maybe ScatterSummary))
loadScatterSummary runner runId = do
  answer <- call runner $ Session.statement runId Statements.scatterSummary
  pure (answer >>= traverse parse)

-- | Production-safe extension of DF-01. Dynamic leaf tasks get an additional
-- "item" input from their immutable partition manifest. Reduction tasks get
-- their ordinary, ordered i000001..in tokens through DF-01's claim path.
dynamicFlowStore :: Runner -> FlowStore IO
dynamicFlowStore runner = wrapStore runner (Base.flowStore runner)

dynamicFlowStoreForRun :: Runner -> UUID -> FlowStore IO
dynamicFlowStoreForRun runner runId =
  wrapStore runner (Base.flowStoreForRun runner runId)

wrapStore :: Runner -> FlowStore IO -> FlowStore IO
wrapStore runner base = FlowStore
  { claim = \worker count seconds -> do
      claimed <- base.claim worker count seconds
      case claimed of
        Left failure -> pure (Left failure)
        Right leases -> do
          enriched <- traverse (enrich runner) leases
          pure (sequence enriched)
  , renew = base.renew
  , finish = \rq -> do
      -- expandScatter already completed its source atomically. The ordinary
      -- DF-01 dispatcher calls finish once more; accept only the same fence.
      recognized <- call runner $ Session.statement
        (rq.runId,rq.taskKey,rq.leaseId,rq.workerId,
         fromIntegral rq.attempt :: Int32) Statements.ackScatter
      case recognized of
        Left err -> pure (Left err)
        Right True
          | null rq.outputs -> pure (Right True)
          | otherwise -> pure (Left (FlowInvalid "scatter must have no output ports"))
        Right False -> base.finish rq
  , failTask = base.failTask
  }

enrich :: Runner -> FlowTaskLease -> IO (Either FlowErr FlowTaskLease)
enrich runner lease = do
  answer <- loadPartitionBinding runner lease.runId lease.taskKey
  pure $ case answer of
    Left err -> Left err
    Right Nothing -> Right lease
    Right (Just binding) ->
      Right (lease { Flow.inputs = lease.inputs <>
          [FlowInput {port="item",artifact=binding.artifact}] })
