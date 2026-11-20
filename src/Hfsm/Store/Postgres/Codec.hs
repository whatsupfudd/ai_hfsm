-- | CT-01 model <-> PostgreSQL row representations. No implicit/show-based
-- status codec: the SQL schema is an external, stable serialization format.
module Hfsm.Store.Postgres.Codec
  ( decodeValue, ref32, caseRef32, joinRef32, stepKindSql
  , signalStatusSql, childStatusSql, projectionValues
  , outboxValues, childValues, breakValues
  ) where

import Data.Aeson (FromJSON, Value, fromJSON, Result(..), object, toJSON, (.=))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Hfsm.Core.Ref (StateRef, CaseRef, JoinRef, stateRefWord32, caseRefWord32, joinRefWord32)
import Hfsm.Runtime.Model (SignalStatus(..), StepKind(..), ChildStatus(..))
import Hfsm.Runtime.Model.Child (renderChildStatus)
import Hfsm.Runtime.Model.Step (renderStepKind)
import Hfsm.Store.Class (StoreErr(..), ProjBatch(..), OutboxAppend(..), ChildAppend(..), BreakAppend(..))

decodeValue :: FromJSON a => Value -> Either StoreErr a
decodeValue v = case fromJSON v of
  Error msg -> Left (DecodeEr (T.pack msg))
  Success a -> Right a

ref32 :: StateRef -> Int64
ref32 = fromIntegral . stateRefWord32
caseRef32 :: CaseRef -> Int64
caseRef32 = fromIntegral . caseRefWord32
joinRef32 :: JoinRef -> Int64
joinRef32 = fromIntegral . joinRefWord32

stepKindSql :: StepKind -> Text
stepKindSql = renderStepKind

signalStatusSql :: SignalStatus -> Text
signalStatusSql x = case x of
  EnteredSs -> "entered"
  LeasedSs -> "leased"
  AppliedSs -> "applied"
  RejectedSs -> "rejected"
  FailedSs -> "failed"

childStatusSql :: ChildStatus -> Text
childStatusSql = renderChildStatus

-- | All values are part of the same SQL transaction as their producing step.
-- Generated ordinal values are stable within each projection kind.
projectionValues :: ProjBatch -> Value
projectionValues batch = toJSON $ concat
  [ entries "active" batch.active, entries "trace" batch.trace
  , entries "queue" batch.queue, entries "tree" batch.tree ]
 where
  entries :: Text -> [Value] -> [Value]
  entries kind xs = zipWith (\n payload -> object
    [ "kind" .= kind, "ordinal" .= (n :: Int), "payload" .= payload ]) [0..] xs

-- | Append records are validated before encoding. The step and instance IDs
-- come from the transaction, not from untrusted JSON.
outboxValues :: [OutboxAppend] -> Value
outboxValues = toJSON . map (.payload)

childValues :: [ChildAppend] -> Value
childValues = toJSON . map (\x -> object
  [ "child" .= x.child
  , "key" .= x.key
  , "status" .= childStatusSql x.status
  , "join" .= fmap joinRef32 x.join
  ])

breakValues :: [BreakAppend] -> Value
breakValues = toJSON . map (\x -> object
  [ "kind" .= T.pack (show x.kind)
  , "state" .= fmap ref32 x.state
  , "status" .= T.pack (show x.status)
  , "note" .= x.note
  ])
