{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoFieldSelectors #-}
-- | Deterministic IO exception injection at persistence boundaries.
-- An exception simulates the observable result of a crashed worker; the
-- real SIGKILL harness in docs must separately verify process death paths.
module Hfsm.Operations.Fault
  ( Boundary(..), SimulatedCrash(..), FaultHook, newFaultHook
  , faultedStore, faultedActivityStore ) where

import Control.Exception (Exception, throwIO)
import Data.IORef (newIORef, atomicModifyIORef')
import Data.Typeable (Typeable)
import Hfsm.Store.Class (Store(..))
import Hfsm.Activity.Types (ActivityStore(..))

data Boundary
  = BeforeClaim | AfterClaim | BeforeLoadInstance | AfterLoadInstance
  | BeforeLoadSnapshot | AfterLoadSnapshot | BeforeCommit | AfterCommit
  | BeforeAck | AfterAck | BeforeLeaseRenew | AfterLeaseRenew
  | BeforeActivityClaim | AfterActivityClaim
  | BeforeActivityFinish | AfterActivityFinish
  deriving (Eq,Ord,Show,Read,Enum,Bounded)

data SimulatedCrash = SimulatedCrash Boundary
  deriving (Show,Typeable)
instance Exception SimulatedCrash

type FaultHook = Boundary -> IO ()


-- | Each (boundary, occurrence) means throw after that many visits;
-- occurrence 1 fires immediately. Faults are consumed exactly once.
newFaultHook :: [(Boundary,Int)] -> IO FaultHook
newFaultHook rules = do
  ref <- newIORef rules
  pure $ \boundary -> do
    explode <- atomicModifyIORef' ref $ \entries ->
      let step [] = ([],False)
          step ((key,n):rest)
            | key/=boundary = let (others,found)=step rest in ((key,n):others,found)
            | n<=1 = (rest,True)
            | otherwise = ((key,n-1):rest,False)
      in step entries
    if explode then throwIO (SimulatedCrash boundary) else pure ()


faultedStore :: FaultHook -> Store IO -> Store IO
faultedStore hook api = Store {
    leaseSignal = \rq -> wrapped BeforeClaim AfterClaim (api.leaseSignal rq)
  , loadInstance = \ident -> wrapped BeforeLoadInstance AfterLoadInstance (api.loadInstance ident)
  , loadSnapshot = \ident -> wrapped BeforeLoadSnapshot AfterLoadSnapshot (api.loadSnapshot ident)
  , commitTurn = \rq -> wrapped BeforeCommit AfterCommit (api.commitTurn rq)
  , ackSignal = \rq -> wrapped BeforeAck AfterAck (api.ackSignal rq)
  , renewLease = \rq -> wrapped BeforeLeaseRenew AfterLeaseRenew (api.renewLease rq)
  , listSignals = api.listSignals }
 where
  wrapped before after action = hook before >> action >>= \value -> hook after >> pure value


faultedActivityStore :: FaultHook -> ActivityStore IO -> ActivityStore IO
faultedActivityStore hook api = ActivityStore {
    claim = \rq -> wrapped BeforeActivityClaim AfterActivityClaim (api.claim rq)
  , renew = api.renew
  , finish = \rq -> wrapped BeforeActivityFinish AfterActivityFinish (api.finish rq) }
  where
  wrapped before after action = hook before >> action >>= \value -> hook after >> pure value
