{-# LANGUAGE NoFieldSelectors #-}
-- | Durable correlated outbox-to-signal delivery. This is the CT06 variant
-- of CT05 newReleaseAwareEnqueueProvider. Uses the same deterministic dedupe
-- key while carrying correlation/causation and W3C traceparent metadata.
module Hfsm.Operations.Provider.Enqueue (correlatedEnqueueProvider) where

import Data.Aeson (Value, toJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import qualified Data.UUID.V4 as U4
import qualified Hasql.Session as Session

import Hfsm.Activity.Types
  (ActivityLease(..), Provider(..), ProviderFailure(..))
import Hfsm.Core.Name (machineNameText)
import Hfsm.Core.Version (machineVersionText)
import Hfsm.Core.Digest (specDigestText)
import Hfsm.Runtime.Registry (MachineKey(..))
import Hfsm.Store.Postgres.Types (Runner(..), EnqueueRq(..))
import Hfsm.Operations.Types (EnqueueOptions(..), OpErr(..))
import qualified Hfsm.Operations.Postgres.Statements as S
import qualified Hfsm.Operations.Postgres as Ops

correlatedEnqueueProvider
  :: Runner -> Text
  -> (ActivityLease -> IO (Either ProviderFailure (MachineKey,Value,Maybe Value)))
  -> Provider
correlatedEnqueueProvider runner providerKind mapper = Provider
  { kind=providerKind
  , supportsIdempotency=True
  , deliver = \effect -> do
      mapped <- mapper effect
      case mapped of
        Left err -> pure (Left err)
        Right (key,signalValue,causeValue) -> do
          correlation <- runSession runner
            (Session.statement effect.effectId S.readEffectCorrelation)
          case correlation of
            Left err -> pure (Left ProviderFailure
              {message=T.pack (show err),retryable=True,ambiguous=True})
            Right (correlationId,traceparent) -> do
              sid <- U4.nextRandom
              now <- getCurrentTime
              let request=EnqueueRq
                    {instanceId=effect.instanceId,signalId=sid,payload=signalValue,
                     cause=causeValue,dedupeKey=Just ("hfsm/activity/"<>effect.effectKey),
                     readyAt=now}
                  release=machineNameText key.name<>"@"<>
                    machineVersionText key.version<>"#"<>specDigestText key.digest
                  opts=EnqueueOptions
                    {priority=0,correlationId=correlationId,
                     causationId=Just effect.effectId,traceparent=traceparent,
                     expectedRelease=Just release}
              outcome <- Ops.enqueueWithPolicy runner request opts
              pure $ case outcome of
                Right signalId -> Right (toJSON signalId)
                Left (OpBackend errorText) -> Left ProviderFailure
                  {message=errorText,retryable=True,ambiguous=True}
                Left other -> Left ProviderFailure
                  {message=T.pack (show other),retryable=False,ambiguous=False}
  }
