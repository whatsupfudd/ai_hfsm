-- | Durable internal completion/timer provider: translate a completed
-- outbox intent into exactly one enqueued HFSM signal, even after a crash
-- between enqueueing the signal and acknowledging the outbox effect.
module Hfsm.Activity.Provider.Enqueue
  ( newEnqueueProvider ) where

import Data.Aeson (Value)
import qualified Data.Aeson as Ae
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import qualified Data.UUID.V4 as U4

import Hfsm.Activity.Types (ActivityLease(..), Provider(..), ProviderFailure(..))
import Hfsm.Store.Postgres.Types (Runner(..), EnqueueRq(..))
import qualified Hfsm.Store.Postgres as Pg
import Hfsm.Store.Class (StoreErr(..))

-- | The mapper must produce the same application signal (and cause) for
-- every retry of the same leased effect. It is responsible for choosing
-- the actual application signal codec; no arbitrary JSON is inserted into
-- an application-specific machine without its approval.
newEnqueueProvider
  :: Runner
  -> Text  -- ^ "timer", "cmd", "spawn", or another registered kind
  -> (ActivityLease -> IO (Either ProviderFailure (Value, Maybe Value)))
  -> Provider
newEnqueueProvider runner effectKind mapper = Provider
  { kind = effectKind
  , supportsIdempotency = True -- PostgreSQL dedupe key is stable for each effect
  , deliver = \effect -> do
      mapped <- mapper effect
      case mapped of
        Left err -> pure (Left err)
        Right (signalPayload, causeValue) -> do
          signalId <- U4.nextRandom
          now <- getCurrentTime
          let request = EnqueueRq
                { instanceId=effect.instanceId, signalId=signalId
                , payload=signalPayload, cause=causeValue
                , dedupeKey=Just ("hfsm/activity/" <> effect.effectKey)
                , readyAt=now }
          delivered <- Pg.enqueue runner request
          pure $ case delivered of
            Right persistedSignalId -> Right (toReceipt persistedSignalId)
            Left err -> Left (toProviderFailure err)
  }
 where
  toReceipt idValue =
    -- Returning a JSON receipt does not advance the HFSM itself; the
    -- persisted signal is processed by the ordinary fenced HFSM worker.
    Ae.toJSON idValue

  toProviderFailure err = case err of
    BackendEr msg -> ProviderFailure
      {message=msg, retryable=True, ambiguous=True}
    ConflictEr msg -> ProviderFailure
      {message=msg, retryable=False, ambiguous=False}
    LeaseLostEr msg -> ProviderFailure
      {message=msg, retryable=False, ambiguous=False}
    NotFoundEr msg -> ProviderFailure
      {message=msg, retryable=False, ambiguous=False}
    DecodeEr msg -> ProviderFailure
      {message=msg, retryable=False, ambiguous=False}
