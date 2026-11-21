{-# LANGUAGE QuasiQuotes #-}

-- | Prepared SQL contract for 006_hfsm_dynamic_scatter_aggregation.sql.
module Hfsm.Flow.Dynamic.Postgres.Statements where

import Data.Aeson (Value)
import Data.Int (Int32)
import Data.Text (Text)
import Data.UUID (UUID)
import Hasql.Statement (Statement)
import Hasql.TH (singletonStatement, maybeStatement)

expandScatter :: Statement (UUID, Text, UUID, UUID, Int32, UUID, Value) UUID
expandScatter = [singletonStatement|
  SELECT hfsm.flow_expand_scatter(
    $1 :: uuid, $2 :: text, $3 :: uuid, $4 :: uuid,
    $5 :: int4, $6 :: uuid, $7 :: jsonb) :: uuid
|]

ackScatter :: Statement (UUID, Text, UUID, UUID, Int32) Bool
ackScatter = [singletonStatement|
  SELECT hfsm.flow_scatter_ack(
    $1 :: uuid, $2 :: text, $3 :: uuid, $4 :: uuid, $5 :: int4) :: bool
|]

partitionInput :: Statement (UUID, Text) (Maybe Value)
partitionInput = [singletonStatement|
  SELECT hfsm.flow_partition_input($1 :: uuid, $2 :: text) :: jsonb?
|]

scatterSummary :: Statement UUID (Maybe Value)
scatterSummary = [singletonStatement|
  SELECT hfsm.flow_scatter_summary($1 :: uuid) :: jsonb?
|]
