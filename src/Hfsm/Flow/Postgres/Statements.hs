{-# LANGUAGE QuasiQuotes #-}
-- | DF-01 prepared Hasql statements. Install Support/005 before compiling.
module Hfsm.Flow.Postgres.Statements where

import Data.Aeson (Value)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Hasql.Statement (Statement)
import Hasql.TH (singletonStatement, maybeStatement, vectorStatement)

registerDefinition :: Statement (UUID, UUID, Text, Text, Value) UUID
registerDefinition = [singletonStatement|
  SELECT hfsm.flow_register_definition($1 :: uuid,$2 :: uuid,$3 :: text,
     $4 :: text,$5 :: jsonb) :: uuid
|]

registerResources :: Statement (UUID, Int32, Int32, Int32) Bool
registerResources = [singletonStatement|
  SELECT hfsm.flow_register_worker($1 :: uuid,$2 :: int4,$3 :: int4,$4 :: int4) :: bool
|]

startRun :: Statement (UUID, UUID, Text, Maybe UUID, Maybe Text, Value, Maybe Value, Maybe Value) UUID
startRun = [singletonStatement|
 SELECT hfsm.flow_start_run($1 :: uuid,$2 :: uuid,$3 :: text,$4 :: uuid?,
   $5 :: text?,$6 :: jsonb,$7 :: jsonb?,$8 :: jsonb?) :: uuid
|]

publishArtifact :: Statement (UUID, UUID, Text, Int64, Text, Text, Value) UUID
publishArtifact = [singletonStatement|
 SELECT hfsm.flow_publish_artifact($1 :: uuid,$2 :: uuid,$3 :: text,
   $4 :: int8,$5 :: text,$6 :: text,$7 :: jsonb) :: uuid
|]

claimTasks :: Statement (UUID, Int32, Int32, Maybe UUID) (Vector Value)
claimTasks = [vectorStatement|
 SELECT result :: jsonb FROM hfsm.flow_claim_tasks($1 :: uuid,$2 :: int4,$3 :: int4,$4 :: uuid?) AS result
|]

renewTask :: Statement (UUID, Text, UUID, UUID, Int32) (Maybe UTCTime)
renewTask = [singletonStatement|
 SELECT hfsm.flow_renew_task(
    $1 :: uuid,$2 :: text,$3 :: uuid,$4 :: uuid,$5 :: int4) :: timestamptz?
|]

finishTask :: Statement (UUID, Text, UUID, UUID, Int32, Value) Bool
finishTask = [singletonStatement|
 SELECT hfsm.flow_finish_task($1 :: uuid,$2 :: text,$3 :: uuid,
    $4 :: uuid,$5 :: int4,$6 :: jsonb) :: bool
|]

failTask :: Statement (UUID, Text, UUID, UUID, Int32, Text, Bool) Bool
failTask = [singletonStatement|
 SELECT hfsm.flow_fail_task($1 :: uuid,$2 :: text,$3 :: uuid,$4 :: uuid,
   $5 :: int4,$6 :: text,$7 :: bool) :: bool
|]

deliverCompletion :: Statement (UUID, UUID) (Maybe UUID)
deliverCompletion = [singletonStatement|
 SELECT hfsm.flow_deliver_completion($1 :: uuid,$2 :: uuid) :: uuid?
|]

cancelRun :: Statement (UUID, Text) Bool
cancelRun = [singletonStatement|
 SELECT hfsm.flow_cancel_run($1 :: uuid,$2 :: text) :: bool
|]

runSummary :: Statement UUID (Maybe Value)
runSummary = [maybeStatement|
 SELECT hfsm.flow_run_summary($1 :: uuid) :: jsonb
 FROM hfsm.flow_run WHERE id=$1 :: uuid
|]

pendingCompletions :: Statement Int32 (Vector UUID)
pendingCompletions = [vectorStatement|
 SELECT run_id :: uuid FROM hfsm.flow_completion
 WHERE status='ready' ORDER BY created_at,run_id LIMIT $1 :: int4
|]

-- Read-only diagnostic: immutable, contiguous per-run events ordered by id.
runEvents :: Statement UUID (Vector Value)
runEvents = [vectorStatement|
 SELECT jsonb_build_object('id',id,'runId',run_id,'taskKey',task_key,
   'kind',kind,'detail',detail,'recordedAt',recorded_at) :: jsonb
 FROM hfsm.flow_event WHERE run_id=$1 :: uuid ORDER BY id
|]

-- | Published output manifest for each static task output. Mutable lease
-- fields are intentionally absent; artifact metadata is immutable.
runOutputs :: Statement UUID (Vector Value)
runOutputs = [vectorStatement|
 SELECT jsonb_build_object('taskKey',o.task_key,'port',o.port,
    'artifact',jsonb_build_object('artifactId',a.id,'digest',a.digest,
    'sizeBytes',a.size_bytes,'storageUri',a.storage_uri,
    'mediaType',a.media_type,'metadata',a.metadata)) :: jsonb
 FROM hfsm.flow_task_output o JOIN hfsm.flow_artifact a ON a.id=o.artifact_id
 WHERE o.run_id=$1 :: uuid ORDER BY o.task_key,o.port
|]
