-- | CT-04 activity Store over the existing CT-02 Hasql.Pool Runner.
-- Does not alter the successful HFSM commitTurn transaction.
module Hfsm.Store.Postgres.Activity
  ( activityStore ) where

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Hasql.Pool as Hp
import qualified Hasql.Session as Hs
import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as TS

import Hfsm.Activity.Types (ActivityStore(..), ActivityErr(..))
import Hfsm.Store.Postgres.Codec (decodeValue)
import Hfsm.Store.Postgres.Types (Runner(..))
import qualified Hfsm.Store.Postgres.Activity.Transaction as X

activityStore :: Runner -> ActivityStore IO
activityStore runner = ActivityStore
  { claim = \rq -> do
      result <- (runSession runner) (writeTx (X.claimTx rq))
      pure $ case result of
        Left err -> Left (ActivityBackend (T.pack (show err)))
        Right vals -> case traverse decodeValue (V.toList vals) of
          Left errorValue -> Left (ActivityDecode (T.pack (show errorValue)))
          Right leases -> Right leases
  , renew = \rq -> runResult runner (writeTx (X.renewTx rq))
  , finish = \rq -> runResult runner (writeTx (X.finishTx rq))
  }

writeTx :: Tx.Transaction a -> Hs.Session a
writeTx = TS.transactionNoRetry TS.ReadCommitted TS.Write

runResult :: Runner -> Hs.Session (Either ActivityErr a) -> IO (Either ActivityErr a)
runResult runner query = do
  result <- (runSession runner) query
  pure $ case result of
    Left err -> Left (ActivityBackend (T.pack (show err)))
    Right value -> value
