{-# LANGUAGE NoFieldSelectors #-}
-- | CT06 crash-before/after boundary matrix on actual PostgreSQL transactions.
-- Uses simulated exceptions, not OS process death. See docs for SIGKILL suite.
module Demo.OperationsChaosRegression (regressionPg) where

import Control.Concurrent (forkFinally, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, when)
import Control.Monad.Trans.Cont (runContT)
import qualified Data.Text as T
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U4
import Data.UUID (UUID)
import Data.Time.Clock (getCurrentTime)

import qualified DB.Connect as Db
import Demo.ApprovalHfsm (approvalSpec, Signal(..))
import Demo.ApprovalRuntime (initialCtx)
import Hfsm.Compile (compile)
import qualified Hfsm.Runtime.Engine.Worker as Engine
import Hfsm.Runtime.Model (InstanceRw(..), SignalRw(..), SignalStatus(..))
import Hfsm.Runtime.Registry (emptyRegistry, machineKeyOf, register)
import Hfsm.Store.Class (Store(..), LeaseRq(..), LeaseRez(..), AckRq(..))
import qualified Hfsm.Store.Postgres as Pg
import Hfsm.Store.Postgres.Bootstrap (enqueueFor, newInstanceFor)
import Hfsm.Operations.Fault (Boundary(..), newFaultHook, faultedStore)
import Hfsm.Operations.Types (WorkerRegistration(..), EnqueueOptions(..))
import qualified Hfsm.Operations.Postgres as Ops

assert :: Bool -> String -> IO ()
assert ok msg = unless ok (fail msg)

required :: Show e => String -> Either e a -> IO a
required what = either (fail . ((what<>": ")<>) . show) pure

regressionPg :: Db.PgDbConfig -> IO ()
regressionPg db = runContT (Db.startPg db) $ \_pool -> do
  -- Separate Runner scope is taken from the SAME pool, so admission is tested
  -- across independently borrowed connections.
  let runner = Pg.runWithPool _pool
  tenant <- U4.nextRandom
  _ <- required "chaos tenant" =<< Ops.createTenant runner tenant
       ("ct06-chaos-"<>U.toText tenant) 100 100 2 4 10
  a <- U4.nextRandom
  b <- U4.nextRandom
  let worker wid = WorkerRegistration
        { workerId=wid,capabilities=["hfsm:signal"],heartbeatTtlSeconds=120,
          maxSignals=10,maxActivities=1 }
  _ <- required "worker a" =<< Ops.registerWorker runner (worker a)
  _ <- required "worker b" =<< Ops.registerWorker runner (worker b)
  spec <- required "approval spec" approvalSpec
  compiled <- required "compile" (compile spec)
  registry <- required "registry" (register compiled emptyRegistry)
  required "register release" =<< Pg.registerMachine runner (machineKeyOf compiled)
  let make = do
        initial <- required "initial" =<< newInstanceFor compiled initialCtx Nothing
        ident <- required "tenant instance" =<< Ops.createTenantInstance runner tenant "default" initial
        rq <- enqueueFor compiled ident (SubmitSg "alice") Nothing
        _ <- required "enqueue" =<< Ops.enqueueWithPolicy runner rq EnqueueOptions
          { priority=0,correlationId=Just rq.signalId,causationId=Nothing,
            traceparent=Nothing,expectedRelease=Nothing }
        pure ident
      cfg wid = Engine.WorkerCfg
        {node=wid,batchSize=1,leaseSeconds=1,pollDelayMs=0}
      readRev ident = do
        let scoped = Ops.productionStoreForInstance runner ident
        row <- scoped.loadInstance ident
        maybe (fail "instance missing") (pure . (.revision)) row

  -- Two processes race for precisely the SAME instance, not merely the same DB.
  shared <- make
  let scoped = Ops.productionStoreForInstance runner shared
  ma <- newEmptyMVar
  mb <- newEmptyMVar
  now <- getCurrentTime
  let request wid = LeaseRq {node=wid,batchSize=1,leaseSeconds=2,now=now}
  _ <- forkFinally (scoped.leaseSignal (request a)) (putMVar ma)
  _ <- forkFinally (scoped.leaseSignal (request b)) (putMVar mb)
  ea <- takeMVar ma
  eb <- takeMVar mb
  let unwrap (Left err) = fail (show err)
      unwrap (Right LeaseNoneLz) = pure []
      unwrap (Right (LeaseSomeLz rows)) = pure rows
  claimsA <- unwrap ea
  claimsB <- unwrap eb
  let both=claimsA<>claimsB
  assert (length both==1) "both workers claimed the same instance"
  let winner=head both
  token <- maybe (fail "missing lease token") pure winner.lease
  _ <- required "cleanup claimed signal" =<< scoped.ackSignal AckRq
     {signal=winner.uuid,lease=token,status=FailedSs,note=Just "chaos fixture",
      now=now}

  -- A fresh instance is created for each boundary so no previous work leaks.
  forM_ [BeforeClaim,AfterClaim,BeforeLoadInstance,AfterLoadInstance,
         BeforeLoadSnapshot,AfterLoadSnapshot,BeforeCommit,AfterCommit] $ \boundary -> do
    instanceId <- make
    fault <- newFaultHook [(boundary,1)]
    failed <- try (Engine.runTick (cfg a)
       (faultedStore fault (Ops.productionStoreForInstance runner instanceId)) registry)
       :: IO (Either SomeException ())
    case failed of
      Left _ -> pure ()
      Right () -> fail ("fault not triggered at "<>show boundary)
    -- Reclaim after the 1s lease, unless the previous commit succeeded.
    if boundary==BeforeClaim || boundary==AfterCommit
      then pure ()
      else threadDelay 1300000
    Engine.runTick (cfg b) (Ops.productionStoreForInstance runner instanceId) registry
    revision <- readRev instanceId
    assert (revision==1) ("duplicate/lost revision at "<>show boundary)
  -- Lease expires during *pure planning* before commit. The original
  -- worker cannot change the instance; the replacement reclaims it.
  expiring <- make
  let delayed BeforeCommit = threadDelay 1500000
      delayed _ = pure ()
  Engine.runTick (cfg a)
    (faultedStore delayed (Ops.productionStoreForInstance runner expiring)) registry
  staleRevision <- readRev expiring
  assert (staleRevision==0) "expired planner committed despite lost fence"
  Engine.runTick (cfg b) (Ops.productionStoreForInstance runner expiring) registry
  newRevision <- readRev expiring
  assert (newRevision==1) "expired planner was not recoverable"
  putStrLn "OperationsChaosRegression (simulated PostgreSQL crashes): PASS"
