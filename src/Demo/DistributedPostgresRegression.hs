{-# LANGUAGE QuasiQuotes #-}

-- | PostgreSQL integration regression for TWO competing Hasql.Pool users.
-- Each invocation creates isolated instances; it does not purge shared data.
module Demo.DistributedPostgresRegression (regressionPg) where

import Control.Concurrent (forkFinally, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception (SomeException)
import Control.Monad (unless)
import Control.Monad.Trans.Cont (runContT)
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U4
import qualified Data.Vector as V
import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Session
import Hasql.Statement (Statement)
import Hasql.TH (vectorStatement, singletonStatement)

import qualified DB.Connect as Db
import Demo.ApprovalHfsm (approvalSpec, Signal(..))
import Demo.ApprovalRuntime (initialCtx)
import Hfsm.Compile (compile)
import Hfsm.Runtime.Engine.Step (StepInput(..), StepPlan(..), planStep, commitStep)
import Hfsm.Runtime.Model (InstanceRw(..), InstanceStatus(..), SignalRw(..), SnapshotRw(..), SignalStatus(..), StepRw(..))
import Hfsm.Runtime.Registry (machineKeyOf)
import Hfsm.Store.Class (Store(..), CommitRq(..), LeaseRq(..), LeaseRez(..), StoreErr(..))
import qualified Hfsm.Store.Postgres as Pg
import Hfsm.Store.Postgres.Bootstrap (enqueueFor, newInstanceFor)


assert :: Bool -> String -> IO ()
assert p msg = unless p (fail msg)

required :: Show err => String -> Either err a -> IO a
required name = either (fail . ((name <> ": ") <>) . show) pure

just :: String -> Maybe a -> IO a
just msg = maybe (fail msg) pure

only :: LeaseRez -> IO SignalRw
only (LeaseSomeLz [s]) = pure s
only other = fail ("expected exactly one lease, got " <> show other)

-- The two forked claim calls race over different pooled connections.
competingClaims :: Store IO -> LeaseRq -> LeaseRq -> IO [SignalRw]
competingClaims api ra rb = do
  ma <- newEmptyMVar
  mb <- newEmptyMVar
  _ <- forkFinally (api.leaseSignal ra) (putMVar ma)
  _ <- forkFinally (api.leaseSignal rb) (putMVar mb)
  ea <- takeMVar ma
  eb <- takeMVar mb
  let unwrap :: Either SomeException LeaseRez -> IO [SignalRw]
      unwrap value = case value of
        Left err -> fail (show err)
        Right LeaseNoneLz -> pure []
        Right (LeaseSomeLz xs) -> pure xs
  (++) <$> unwrap ea <*> unwrap eb

regressionPg :: Db.PgDbConfig -> IO ()
regressionPg config = runContT (Db.startPg config) $ \pool -> do
  spec <- required "approvalSpec" approvalSpec
  compiled <- required "compile" (compile spec)
  let runner = Pg.runWithPool pool
      api = Pg.postgresStore runner
  required "register release" =<< Pg.registerMachine runner (machineKeyOf compiled)
  new <- required "initial machine" =<< newInstanceFor compiled initialCtx Nothing
  inst <- required "initialize instance" =<< Pg.initializeInstance runner new
  firstRq <- enqueueFor compiled inst (SubmitSg "alice") (Just "ct03/submit")
  secondRq <- enqueueFor compiled inst (ApproveSg "carol") (Just "ct03/approve")
  _ <- required "enqueue first" =<< Pg.enqueue runner firstRq
  _ <- required "enqueue second" =<< Pg.enqueue runner secondRq
  a <- U4.nextRandom
  b <- U4.nextRandom
  now <- getCurrentTime

  let
    claim node = LeaseRq { node = node, batchSize = 1, leaseSeconds = 30, now = now }
    testApi = Pg.postgresStoreForInstance runner inst

  claims <- competingClaims api (claim a) (claim b)

  putStrLn ("Expected instance: " <> show inst)
  mapM_ (\s -> putStrLn
      ("Claimed instance=" <> show s.inst
        <> " signal=" <> show s.uuid
        <> " ordinal=" <> show s.ordinal
        <> " lease=" <> show s.lease)
    ) claims

  assert (length claims == 1) ("Many workers leased the same instance concurrently: " <> (show. length $ claims))
  let sig = head claims
  assert (sig.ordinal == 1) "Wrong FIFO ordinal admitted"
  token <- just "missing lease token" sig.lease
  ir <- just "missing instance" =<< api.loadInstance inst
  snap <- just "missing snapshot" =<< api.loadSnapshot ir.snap
  assert (ir.revision == 0) "Instance must begin at revision zero"
  let input = StepInput { machine = machineKeyOf compiled
                        , instance_ = ir, snapshot = snap, signal = sig }
  plan <- required "planStep" (planStep compiled input)
  let rq = CommitRq
        { instanceId = inst, expectedSnapshot = snap.uuid
        , expectedRevision = ir.revision, signalId = sig.uuid
        , signalOrdinal = sig.ordinal, leaseId = token
        , step = plan.step, snapshot = plan.snapshot, nextStatus = plan.nextStatus
        , outbox = plan.outbox, child = plan.child, breaks = plan.breaks, proj = plan.proj }
  stale <- api.commitTurn (rq { expectedRevision = ir.revision + 1 })
  case stale of
    Left (ConflictEr _) -> pure ()
    other -> fail ("Expected stale revision conflict: " <> show other)
  required "commitTurn" =<< api.commitTurn rq
  duplicated <- api.commitTurn rq
  case duplicated of
    Left _ -> pure ()
    Right _ -> fail "Duplicate turn committed"
  ir1 <- just "missing instance after commit" =<< api.loadInstance inst
  assert (ir1.revision == 1) "Instance revision did not increment once"
  ekeys <- required "effect keys" =<< Pool.use pool (Session.statement inst readEffectKeys)
  assert (ekeys == V.singleton (U.toText plan.step.uuid <> ":0"))
    "Outbox idempotency key is not stable"

  next <- api.leaseSignal (claim b) >>= only
  assert (next.ordinal == 2) "Second admitted signal is not sequence 2"
  ir2 <- just "missing current instance" =<< api.loadInstance inst
  sn2 <- just "missing current snapshot" =<< api.loadSnapshot ir2.snap
  plan2 <- required "plan second" (planStep compiled StepInput
    { machine = machineKeyOf compiled, instance_ = ir2
    , snapshot = sn2, signal = next })
  key2 <- just "missing second fence" next.lease
  required "second commit" =<< api.commitTurn CommitRq
    { instanceId = inst, expectedSnapshot = sn2.uuid
    , expectedRevision = ir2.revision, signalId = next.uuid
    , signalOrdinal = next.ordinal, leaseId = key2
    , step = plan2.step, snapshot = plan2.snapshot
    , nextStatus = plan2.nextStatus, outbox = plan2.outbox
    , child = plan2.child, breaks = plan2.breaks, proj = plan2.proj }
  irDone <- just "missing completed instance" =<< api.loadInstance inst
  assert (irDone.revision == 2) "Final revision must be 2"
  assert (irDone.status == DoneIs) "root terminal state must end the instance"

  -- The database server, not a caller-supplied clock, determines expiration.
  staleInstRq <- newInstanceFor compiled initialCtx Nothing >>= required "stale instance"
  staleInst <- required "initialize stale instance" =<< Pg.initializeInstance runner staleInstRq
  enqueueRq <- enqueueFor compiled staleInst (SubmitSg "alice") (Just "ct03/stale")
  _ <- required "enqueue stale" =<< Pg.enqueue runner enqueueRq
  old <- api.leaseSignal LeaseRq
    { node = a, batchSize = 1, leaseSeconds = 1, now = now } >>= only
  assert (old.inst == staleInst) "Wrong instance claimed"
  threadDelay 1300000
  newLease <- api.leaseSignal (claim b) >>= only
  assert (newLease.uuid == old.uuid) "Reclaim advanced past an expired signal"
  assert (newLease.lease /= old.lease) "Reclaim reused a stale fencing token"
  assert (newLease.attempts == old.attempts + 1) "Reclaim did not increment attempts"
  irOld <- just "missing expired instance" =<< api.loadInstance staleInst
  snOld <- just "missing expired snapshot" =<< api.loadSnapshot irOld.snap
  oldPlan <- required "stale plan" (planStep compiled StepInput
    { machine = machineKeyOf compiled, instance_ = irOld
    , snapshot = snOld, signal = old })
  oldToken <- just "missing old token" old.lease
  rejected <- api.commitTurn CommitRq
    { instanceId = staleInst, expectedSnapshot = snOld.uuid
    , expectedRevision = irOld.revision, signalId = old.uuid
    , signalOrdinal = old.ordinal, leaseId = oldToken
    , step = oldPlan.step, snapshot = oldPlan.snapshot
    , nextStatus = oldPlan.nextStatus, outbox = oldPlan.outbox
    , child = oldPlan.child, breaks = oldPlan.breaks, proj = oldPlan.proj }
  case rejected of
    Left (LeaseLostEr _) -> pure ()
    other -> fail ("Expired owner committed: " <> show other)
  unaffected <- just "missing stale instance" =<< api.loadInstance staleInst
  assert (unaffected.revision == 0) "Expired owner advanced the revision"
  putStrLn "DistributedPostgresRegression: PASS"

readEffectKeys :: Statement UUID (V.Vector T.Text)
readEffectKeys = [vectorStatement|
  SELECT effect_key :: text FROM hfsm.outbox
  WHERE instance_id = $1 :: uuid ORDER BY ordinal
|]
