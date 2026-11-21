{-# LANGUAGE NoFieldSelectors #-}

-- | CT-05: internal outbox->signal provider. For migrated instances, the
-- mapper MUST return an exact target MachineKey and the release-aware path
-- prevents old codec payloads from entering the upgraded instance.
module Hfsm.Activity.Provider.Enqueue
  ( newEnqueueProvider, newReleaseAwareEnqueueProvider ) where

import Data.Aeson (Value)
import qualified Data.Aeson as Ae
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import qualified Data.UUID.V4 as U4

import Hfsm.Activity.Types (ActivityLease(..), Provider(..), ProviderFailure(..))
import Hfsm.Runtime.Registry (MachineKey)
import Hfsm.Store.Postgres.Types (Runner(..), EnqueueRq(..))
import qualified Hfsm.Store.Postgres as Pg
import Hfsm.Store.Class (StoreErr(..))
import qualified Hfsm.Migration.PinnedEnqueue as Pinned

-- | Backward-compatible provider for instances that have never migrated.
-- The CT-05 SQL guard deliberately rejects this path after migration.
newEnqueueProvider
  :: Runner -> Text
  -> (ActivityLease -> IO (Either ProviderFailure (Value, Maybe Value)))
  -> Provider
newEnqueueProvider runner kind mapper =
  mkProvider runner kind $ \effect -> do
    result <- mapper effect
    pure $ fmap (\(payload,cause) -> (Nothing,payload,cause)) result

-- | Production version: choose the exact compiled machine release for EACH
-- effect, then use fenced/serialized enqueuePinned. A stable effectKey is
-- still used as the signal dedupe key across worker crashes.
newReleaseAwareEnqueueProvider
  :: Runner -> Text
  -> (ActivityLease -> IO (Either ProviderFailure (MachineKey,Value,Maybe Value)))
  -> Provider
newReleaseAwareEnqueueProvider runner kind mapper =
  mkProvider runner kind $ \effect -> do
    result <- mapper effect
    pure $ fmap (\(key,payload,cause) -> (Just key,payload,cause)) result

mkProvider
  :: Runner -> Text
  -> (ActivityLease -> IO (Either ProviderFailure (Maybe MachineKey,Value,Maybe Value)))
  -> Provider
mkProvider runner effectKind mapper = Provider
  { kind = effectKind
  , supportsIdempotency = True
  , deliver = \effect -> do
      mapped <- mapper effect
      case mapped of
        Left err -> pure (Left err)
        Right (release,signalPayload,causeValue) -> do
          signalId <- U4.nextRandom
          now <- getCurrentTime
          let request = EnqueueRq
                { instanceId=effect.instanceId,signalId=signalId
                , payload=signalPayload,cause=causeValue
                , dedupeKey=Just ("hfsm/activity/" <> effect.effectKey)
                , readyAt=now }
          persisted <- case release of
            Nothing -> Pg.enqueue runner request
            Just key -> Pinned.enqueuePinned runner key request
          pure $ case persisted of
            Right signalUuid -> Right (Ae.toJSON signalUuid)
            Left err -> Left (toProviderFailure err)
  }

toProviderFailure :: StoreErr -> ProviderFailure
toProviderFailure err = case err of
  BackendEr msg -> ProviderFailure {message=msg,retryable=True,ambiguous=True}
  ConflictEr msg -> ProviderFailure {message=msg,retryable=False,ambiguous=False}
  LeaseLostEr msg -> ProviderFailure {message=msg,retryable=False,ambiguous=False}
  NotFoundEr msg -> ProviderFailure {message=msg,retryable=False,ambiguous=False}
  DecodeEr msg -> ProviderFailure {message=msg,retryable=False,ambiguous=False}
