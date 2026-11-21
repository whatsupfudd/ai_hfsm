{-# LANGUAGE NoFieldSelectors #-}
-- | HFSM -> Flow boundary: a CT-04 outbox activity starts a flow exactly
-- once, even if an activity worker crashes after committing the start.
module Hfsm.Flow.Provider.Start (FlowStartPlan(..), newFlowStartProvider) where

import Data.Aeson (Value, toJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID.V4 as U4

import Hfsm.Activity.Types
  ( ActivityLease(..), Provider(..), ProviderFailure(..) )
import Hfsm.Flow.Types (StartRun(..), FlowErr(..))
import qualified Hfsm.Flow.Postgres as Flow
import Hfsm.Store.Postgres.Types (Runner)

data FlowStartPlan = FlowStartPlan
  { definitionId :: UUID
  , sourceRelease :: Text
  , runInput :: Value
  , successSignal :: Value
  , failureSignal :: Value
  }

-- | The mapper MUST encode success/failure with the exact pinned HFSM signal
-- codec and choose the intended DAG definition. Not a generic JSON cast.
newFlowStartProvider
  :: Runner
  -> Text -- ^ Outbox kind; current HFSM planner emits "cmd" for commands.
  -> (ActivityLease -> IO (Either ProviderFailure FlowStartPlan))
  -> Provider
newFlowStartProvider runner effectKind mapper = Provider
  { kind = effectKind
  , supportsIdempotency = True
  , deliver = \effect -> do
      planned <- mapper effect
      case planned of
        Left err -> pure (Left err)
        Right plan -> do
          proposedId <- U4.nextRandom
          result <- Flow.startRun runner StartRun
            { runId=proposedId,definitionId=plan.definitionId
            , sourceEffectKey=effect.effectKey
            , sourceInstance=Just effect.instanceId
            , sourceRelease=Just plan.sourceRelease
            , runInput=plan.runInput
            , successSignal=Just plan.successSignal
            , failureSignal=Just plan.failureSignal }
          pure $ case result of
            Right runUuid -> Right (toJSON runUuid)
            Left errorValue -> Left (asFailure errorValue)
  }
 where
  asFailure errorValue = case errorValue of
    FlowBackend msg -> ProviderFailure
      {message=msg,retryable=True,ambiguous=True}
    FlowDecode msg -> ProviderFailure
      {message=msg,retryable=False,ambiguous=False}
    FlowInvalid msg -> ProviderFailure
      {message=msg,retryable=False,ambiguous=False}
    FlowLeaseLost msg -> ProviderFailure
      {message=msg,retryable=False,ambiguous=False}
