{-# LANGUAGE NoFieldSelectors #-}

-- | Production CT-06 registration and resource supervision for DF-02.
-- Use this instead of DF-01's runManagedFlowWorker when dynamic partitions
-- need their persisted "item" input injected into FlowTaskLease.
module Hfsm.Flow.Dynamic.Runtime
  ( runManagedDynamicWorker ) where

import Hfsm.Operations.Types (WorkerRegistration(..))
import Hfsm.Operations.Runtime (withRegisteredWorker)
import Hfsm.Flow.Types (WorkerResources(..))
import Hfsm.Flow.Worker (FlowWorkerCfg(..), TaskProvider)
import qualified Hfsm.Flow.Worker as Worker
import qualified Hfsm.Flow.Postgres as Flow
import qualified Hfsm.Flow.Dynamic.Postgres as Dynamic
import Hfsm.Store.Postgres.Types (Runner)

runManagedDynamicWorker :: Runner -> WorkerRegistration -> WorkerResources
                        -> FlowWorkerCfg -> [TaskProvider] -> IO ()
runManagedDynamicWorker runner registration resources config providers
  | registration.workerId /= resources.workerId ||
    registration.workerId /= config.workerId =
      fail "DF-02 worker identity must match CT-06 registration and resource reservation"
  | otherwise = withRegisteredWorker runner registration $ do
      registered <- Flow.registerResources runner resources
      case registered of
        Left err -> fail ("DF-02 resource registration failed: " <> show err)
        Right False -> fail "DF-02 resource reservation refused"
        Right True -> Worker.runWorker config (Dynamic.dynamicFlowStore runner) providers
