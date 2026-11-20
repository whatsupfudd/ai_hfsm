-- | Each operation is a single Postgres transaction on a pooled connection.
module Hfsm.Store.Postgres.Activity.Transaction
  ( claimTx, renewTx, finishTx ) where

import Data.Aeson (Value)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import qualified Data.Vector as V
import qualified Hasql.Transaction as Tx

import Hfsm.Activity.Types
  ( ActivityErr(..), ClaimRq(..), RenewRq(..), FinishRq(..)
  , FinishDecision(..) )
import qualified Hfsm.Store.Postgres.Activity.Statements as S

abort :: ActivityErr -> Tx.Transaction (Either ActivityErr a)
abort errorValue = Tx.condemn *> pure (Left errorValue)

claimTx :: ClaimRq -> Tx.Transaction (V.Vector Value)
claimTx rq = do
  -- Sweeping happens even when no new item is available. It is idempotent.
  Tx.statement rq.instanceFilter S.reapExhausted
  Tx.statement
    ( fromIntegral (max 1 (min 100 rq.batchSize)) :: Int32
    , rq.node
    , fromIntegral (max 1 (min 3600 rq.leaseSeconds)) :: Int32
    , rq.instanceFilter
    , V.fromList rq.safeKinds
    , V.fromList rq.supportedKinds
    ) S.claimEffects

renewTx :: RenewRq -> Tx.Transaction (Either ActivityErr UTCTime)
renewTx rq = do
  result <- Tx.statement
    ( rq.effectId, rq.leaseId, rq.node
    , fromIntegral (max 1 (min 3600 rq.leaseSeconds)) :: Int32
    ) S.renewEffect
  case result of
    Just expiry -> pure (Right expiry)
    Nothing -> abort (ActivityLeaseLost "Activity renewal: missing/expired fence")

finishTx :: FinishRq -> Tx.Transaction (Either ActivityErr ())
finishTx rq = do
  let (dbStatus, receipt, errText, delaySeconds, failureClass, historyStatus) =
        case rq.decision of
          Delivered value ->
            ("done", Just value, Nothing, 0, Nothing, "succeeded")
          RetryAfter explanation delay ->
            ("ready", Nothing, Just explanation, max 0 delay, Nothing, "retry")
          PermanentFailure failure explanation ->
            ("failed", Nothing, Just explanation, 0, Just failure, "failed")
  updated <- Tx.statement
    ( rq.effectId, rq.leaseId, rq.node, fromIntegral rq.attempt :: Int32
    , dbStatus :: Text, receipt :: Maybe Value, errText :: Maybe Text
    , fromIntegral (min 86400 delaySeconds) :: Int32, failureClass :: Maybe Text
    ) S.finishEffect
  case updated of
    Nothing -> abort (ActivityLeaseLost "Activity finish: stale lease or exhausted retry")
    Just _ -> do
      history <- Tx.statement
        (rq.effectId, rq.leaseId, fromIntegral rq.attempt :: Int32,
         historyStatus :: Text, errText :: Maybe Text, receipt :: Maybe Value)
        S.finishAttempt
      case history of
        Nothing -> abort (ActivityConflict "Missing leased attempt history")
        Just _ -> pure (Right ())
