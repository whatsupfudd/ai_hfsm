{-# LANGUAGE NoFieldSelectors #-}

-- | Typed DF-02 providers running under the existing DF-01 worker. Providers
-- must produce deterministic CAS artifacts; external business effects belong
-- in CT-04 outbox handlers, not in a retryable dataflow callback.
module Hfsm.Flow.Dynamic.Provider
  ( newScatterProvider, newPartitionProvider, newOrderedReducerProvider ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import Hfsm.Flow.Dynamic.Spec (ScatterPlan)
import Hfsm.Flow.Dynamic.Types (PartitionBinding(..))
import qualified Hfsm.Flow.Dynamic.Postgres as Dynamic
import Hfsm.Flow.Worker (TaskProvider(..), TaskError(..))
import Hfsm.Flow.Types
  ( FlowTaskLease(..), FlowInput(..), ArtifactRef(..), FlowErr(..))
import Hfsm.Store.Postgres.Types (Runner)

-- | Compute a finite immutable plan from the exact leased source. The
-- database atomically materializes the expansion and completes that source.
-- The DF-02 FlowStore acknowledges the worker's subsequent finish call.
newScatterProvider :: Runner
  -> (FlowTaskLease -> IO (Either TaskError ScatterPlan))
  -> TaskProvider
newScatterProvider runner build = TaskProvider
  { executor = "df2.scatter"
  , execute = \lease -> do
      computed <- build lease
      case computed of
        Left problem -> pure (Left problem)
        Right plan -> do
          answer <- Dynamic.expandScatter runner lease plan
          pure $ case answer of
            Right _ -> Right []
            Left err -> Left (asError err)
  }

-- | The persisted partition key and ordinal are supplied explicitly. The
-- immutable "item" reference injected into the lease must match the binding.
newPartitionProvider :: Runner -> Text
  -> (FlowTaskLease -> PartitionBinding -> IO (Either TaskError UUID))
  -> TaskProvider
newPartitionProvider runner executorName calculate = TaskProvider
  { executor = executorName
  , execute = \lease ->
      case (lease.inputs,lease.outputs) of
        ([itemInput],["value"]) | itemInput.port=="item" -> do
          loaded <- Dynamic.loadPartitionBinding runner lease.runId lease.taskKey
          case loaded of
            Left err -> pure (Left (asError err))
            Right Nothing -> pure (Left (invalid "partition binding missing"))
            Right (Just binding)
              | binding.artifact /= itemInput.artifact ->
                  pure (Left (invalid "partition claim/binding mismatch"))
              | otherwise -> fmap (fmap (\artifact -> [("value",artifact)]))
                                  (calculate lease binding)
        _ -> pure (Left (invalid "partition requires one item input and one value output"))
  }

-- | A reducer sees its inputs in MANIFEST order, not task completion order.
-- The callback implements a deterministic ordered fold and returns a CAS
-- manifest ID. The final reducer outputs "result"; intermediate levels "value".
newOrderedReducerProvider :: Text
  -> (FlowTaskLease -> [ArtifactRef] -> IO (Either TaskError UUID))
  -> TaskProvider
newOrderedReducerProvider executorName reduce = TaskProvider
  { executor=executorName
  , execute = \lease -> do
      let ports = map (.port) lease.inputs
          canonical = ["i" <> T.justifyRight 6 '0' (T.pack (show index)) | index <- [1..length ports]]
      case lease.outputs of
        [outputName]
          | outputName `elem` ["value","result"] &&
            ports==canonical && length ports<=64 ->
              fmap (fmap (\artifact -> [(outputName,artifact)]))
                (reduce lease (map (.artifact) lease.inputs))
        _ -> pure (Left (invalid "reducer inputs out of order or output contract invalid"))
  }

asError :: FlowErr -> TaskError
asError err = TaskError
  { reason=T.pack (show err)
  , retryable=case err of
       FlowBackend _ -> True -- a committed result may have lost its ACK
       _ -> False }

invalid :: Text -> TaskError
invalid message = TaskError {reason=message,retryable=False}
