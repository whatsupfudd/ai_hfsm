{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}
-- | This command is intended to be launched by a separate subprocess.
-- It can SIGKILL itself at a selected persistence boundary; NEVER call its
-- chaosWorkerDb test in the same process as a broader test suite.
module Demo.OperationsChaosProcess
  ( createFixtureDb, chaosWorkerDb, inspectFixtureDb ) where

import Control.Exception (catch, throwIO)
import Control.Monad (unless)
import Control.Monad.Trans.Cont (runContT)
import Data.Int (Int32)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U4
import qualified Hasql.Session as Session
import Hasql.Statement (Statement)
import Hasql.TH (singletonStatement)
import System.Environment (lookupEnv)
import System.Posix.Signals (raiseSignal, sigKILL)
import Text.Read (readMaybe)

import qualified DB.Connect as Db
import Demo.ApprovalHfsm (approvalSpec, Signal(..))
import Demo.ApprovalRuntime (initialCtx)
import Hfsm.Compile (compile)
import qualified Hfsm.Runtime.Engine.Worker as Engine
import Hfsm.Runtime.Registry (emptyRegistry, machineKeyOf, register)
import Hfsm.Store.Class (Store(..))
import qualified Hfsm.Store.Postgres as Pg
import Hfsm.Store.Postgres.Types (Runner(..))
import Hfsm.Store.Postgres.Bootstrap (enqueueFor, newInstanceFor)
import Hfsm.Operations.Fault
  (Boundary,SimulatedCrash(..),newFaultHook,faultedStore)
import Hfsm.Operations.Types (WorkerRegistration(..))
import qualified Hfsm.Operations.Postgres as Ops

required :: Show e => String -> Either e a -> IO a
required tag = either (fail . ((tag<> ": ") <>) . show) pure

fixtureId :: IO UUID
fixtureId = do
  raw <- lookupEnv "HFSM_CHAOS_INSTANCE_UUID"
  case raw >>= (U.fromText . T.pack) of
    Just ident -> pure ident
    Nothing -> fail "set HFSM_CHAOS_INSTANCE_UUID"

-- Prints only the instance UUID on the last line, for harness capture.
createFixtureDb :: Db.PgDbConfig -> IO ()
createFixtureDb config = runContT (Db.startPg config) $ \pool -> do
  let runner=Pg.runWithPool pool
  spec <- required "approval spec" approvalSpec
  compiled <- required "compile" (compile spec)
  required "register release" =<< Pg.registerMachine runner (machineKeyOf compiled)
  initialized <- required "initial state" =<< newInstanceFor compiled initialCtx Nothing
  ident <- required "instance" =<< Pg.initializeInstance runner initialized
  req <- enqueueFor compiled ident (SubmitSg "chaos") Nothing
  _ <- required "enqueue" =<< Pg.enqueue runner req
  putStrLn (T.unpack (U.toText ident))

chaosWorkerDb :: Db.PgDbConfig -> IO ()
chaosWorkerDb config = do
  ident <- fixtureId
  target <- lookupEnv "HFSM_FAULT_BOUNDARY"
  mode <- lookupEnv "HFSM_FAULT_MODE"
  fault <- case target >>= readMaybe of
    Nothing -> newFaultHook []
    Just boundary -> newFaultHook [(boundary,1)]
  let hooked boundary = fault boundary `catch` \(SimulatedCrash which) ->
        if mode==Just "kill" then raiseSignal sigKILL
        else throwIO (SimulatedCrash which)
  runContT (Db.startPg config) $ \pool -> do
    let runner=Pg.runWithPool pool
    spec <- required "approval spec" approvalSpec
    compiled <- required "compile" (compile spec)
    registry <- required "registry" (register compiled emptyRegistry)
    node <- U4.nextRandom
    _ <- required "worker registration" =<< Ops.registerWorker runner WorkerRegistration
      { workerId=node,capabilities=["hfsm:signal"],heartbeatTtlSeconds=30,
        maxSignals=2,maxActivities=1 }
    let cfg = Engine.WorkerCfg
          {node=node,batchSize=1,leaseSeconds=2,pollDelayMs=0}
    Engine.runTick cfg (faultedStore hooked
      (Ops.productionStoreForInstance runner ident)) registry

inspectFixtureDb :: Db.PgDbConfig -> IO ()
inspectFixtureDb config = do
  ident <- fixtureId
  runContT (Db.startPg config) $ \pool -> do
    counts <- required "fixture counts" =<<
      (runSession (Pg.runWithPool pool))
         (Session.statement ident fixtureCounts)
    let (revision, steps, effects)=counts
    unless (revision==1 && steps==1 && effects==1) $
      fail ("CT06 crash fixture inconsistent: "<>show counts)
    putStrLn "CT06 process crash fixture: PASS"

fixtureCounts :: Statement UUID (Int32,Int32,Int32)
fixtureCounts = [singletonStatement|
 SELECT i.revision::int4,
    (SELECT count(*)::int4 FROM hfsm.step s WHERE s.instance_id=i.id)::int4,
    (SELECT count(*)::int4 FROM hfsm.outbox o WHERE o.instance_id=i.id)::int4
 FROM hfsm.instance i WHERE i.id=$1 :: uuid
|]
