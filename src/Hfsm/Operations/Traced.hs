{-# LANGUAGE NoFieldSelectors #-}
-- | Optional per-operation OpenTelemetry-style instrumentation. The trace
-- context was persisted with the signal/outbox, never reconstructed from a
-- mutable process-local cache. Export is best-effort and outside SQL TXs.
module Hfsm.Operations.Traced
  ( productionStoreTraced, productionActivityStoreTraced ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as U
import qualified Hasql.Session as Session
import Hfsm.Store.Class (Store(..), CommitRq(..), LeaseRq(..), AckRq(..))
import Hfsm.Activity.Types (ActivityStore(..), FinishRq(..), ClaimRq(..))
import Hfsm.Operations.Trace (SpanSink, parseTraceparent, withSpan)
import qualified Hfsm.Operations.Postgres as Ops
import qualified Hfsm.Operations.Postgres.Statements as S
import Hfsm.Store.Postgres.Types (Runner(..))

productionStoreTraced :: SpanSink -> Runner -> Store IO
productionStoreTraced sink runner =
  let api = Ops.productionStore runner
  in api
     { leaseSignal = \rq ->
         withSpan sink Nothing "hfsm.signal.claim"
           [("worker.id",U.toText rq.node)] (api.leaseSignal rq)
     , commitTurn = \rq -> do
         context <- signalTraceparent runner rq.signalId
         withSpan sink (context >>= parseTraceparent) "hfsm.turn.commit"
           [("hfsm.instance.id",U.toText rq.instanceId),
            ("hfsm.signal.id",U.toText rq.signalId),
            ("hfsm.expected_revision",T.pack (show rq.expectedRevision))]
           (api.commitTurn rq)
     , ackSignal = \rq -> do
         context <- signalTraceparent runner rq.signal
         withSpan sink (context >>= parseTraceparent) "hfsm.signal.ack"
           [("hfsm.signal.id",U.toText rq.signal)] (api.ackSignal rq)
     }

productionActivityStoreTraced :: SpanSink -> Runner -> ActivityStore IO
productionActivityStoreTraced sink runner =
  let api = Ops.productionActivityStore runner
  in api
     { claim = \rq -> withSpan sink Nothing "hfsm.activity.claim"
         [("worker.id",U.toText rq.node)] (api.claim rq)
     , finish = \rq -> do
         context <- effectTraceparent runner rq.effectId
         withSpan sink (context >>= parseTraceparent) "hfsm.activity.finish"
           [("hfsm.effect.id",U.toText rq.effectId),
            ("hfsm.activity.attempt",T.pack (show rq.attempt))]
           (api.finish rq)
     }

signalTraceparent :: Runner -> U.UUID -> IO (Maybe Text)
signalTraceparent runner sid = do
  result <- runSession runner (Session.statement sid S.readSignalTraceparent)
  pure (either (const Nothing) id result)

effectTraceparent :: Runner -> U.UUID -> IO (Maybe Text)
effectTraceparent runner eid = do
  result <- runSession runner (Session.statement eid S.readEffectTraceparent)
  pure (either (const Nothing) id result)
