-- DF-02: bounded, one-shot dynamic scatter/gather plus ordered partitioned
-- aggregation. Requires migrations 001..005. PostgreSQL 16+.
--
-- Apply in ONE transaction. New functions use descriptive PL/pgSQL variable
-- names deliberately disjoint from every SQL range-table alias.
--
-- Execution is conservative: all graph changes and task settlement take the
-- existing CT-06 admission_gate lock. No dynamic graph mutation after commit.

CREATE TABLE hfsm.flow_scatter_group (
  id uuid PRIMARY KEY,
  run_id uuid NOT NULL UNIQUE REFERENCES hfsm.flow_run(id) ON DELETE RESTRICT,
  source_task text NOT NULL,
  group_key text NOT NULL CHECK(group_key ~ '^[A-Za-z0-9_.:-]{1,24}$'),
  root_task text NOT NULL,
  partition_count integer NOT NULL CHECK(partition_count BETWEEN 0 AND 512),
  fan_in integer NOT NULL CHECK(fan_in BETWEEN 2 AND 64),
  plan jsonb NOT NULL CHECK(jsonb_typeof(plan)='object'),
  plan_digest hfsm.digest64 NOT NULL,
  source_lease_id uuid NOT NULL,
  source_worker_id uuid NOT NULL,
  source_attempt integer NOT NULL CHECK(source_attempt>=1),
  created_at timestamptz NOT NULL DEFAULT clock_timestamp(),
  FOREIGN KEY(run_id,source_task) REFERENCES hfsm.flow_task(run_id,task_key),
  FOREIGN KEY(run_id,root_task) REFERENCES hfsm.flow_task(run_id,task_key),
  CONSTRAINT flow_scatter_digest_ck CHECK
    (plan_digest=encode(sha256(convert_to(plan::text,'UTF8')),'hex')),
  UNIQUE(run_id,group_key)
);
CREATE TRIGGER flow_scatter_group_immutable
  BEFORE UPDATE OR DELETE ON hfsm.flow_scatter_group
  FOR EACH ROW EXECUTE FUNCTION hfsm.reject_history_mutation();

-- Partition inputs reference previously published, tenant-scoped CAS manifests.
-- The input does NOT pretend to be a token produced by a synthetic graph edge;
-- its immutable lineage is (scatter source, manifest ordinal, artifact ID).
CREATE TABLE hfsm.flow_partition_binding (
  run_id uuid NOT NULL,
  group_key text NOT NULL,
  task_key text NOT NULL,
  tenant_id uuid NOT NULL,
  partition_ordinal integer NOT NULL CHECK(partition_ordinal BETWEEN 1 AND 512),
  partition_key text NOT NULL CHECK(length(partition_key) BETWEEN 1 AND 128),
  artifact_id uuid NOT NULL,
  created_at timestamptz NOT NULL DEFAULT clock_timestamp(),
  PRIMARY KEY(run_id,task_key),
  UNIQUE(run_id,group_key,partition_ordinal),
  UNIQUE(run_id,group_key,partition_key),
  FOREIGN KEY(run_id,group_key) REFERENCES hfsm.flow_scatter_group(run_id,group_key),
  FOREIGN KEY(run_id,task_key) REFERENCES hfsm.flow_task(run_id,task_key),
  FOREIGN KEY(tenant_id,run_id) REFERENCES hfsm.flow_run(tenant_id,id),
  FOREIGN KEY(tenant_id,artifact_id) REFERENCES hfsm.flow_artifact(tenant_id,id)
);
CREATE INDEX flow_partition_binding_group_idx
  ON hfsm.flow_partition_binding(run_id,group_key,partition_ordinal);
CREATE TRIGGER flow_partition_binding_immutable
  BEFORE UPDATE OR DELETE ON hfsm.flow_partition_binding
  FOR EACH ROW EXECUTE FUNCTION hfsm.reject_history_mutation();

-- The reserved scatter executor is a control operation. A caller using the
-- legacy DF-01 finish path must not be able to bypass materialization.
CREATE FUNCTION hfsm.flow_require_scatter_expansion()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF OLD.executor='df2.scatter' AND OLD.status <> 'done' AND NEW.status='done'
     AND NOT EXISTS(
       SELECT 1 FROM hfsm.flow_scatter_group AS scatter_record
       WHERE scatter_record.run_id=NEW.run_id
         AND scatter_record.source_task=NEW.task_key
     ) THEN
    RAISE EXCEPTION 'df2.scatter requires atomic expansion before completion'
      USING ERRCODE='23514';
  END IF;
  RETURN NEW;
END;
$$;
CREATE TRIGGER flow_scatter_expansion_required
  BEFORE UPDATE OF status ON hfsm.flow_task
  FOR EACH ROW EXECUTE FUNCTION hfsm.flow_require_scatter_expansion();

-- Expand ONLY a currently leased static sink with zero output ports.
-- The function is idempotent for the same run/source/plan. The entire graph,
-- its source completion and all event evidence commit as a single transaction.
CREATE FUNCTION hfsm.flow_expand_scatter(
  p_run uuid, p_source_task text, p_fence uuid, p_worker uuid,
  p_attempt integer, p_group_id uuid, p_plan jsonb
) RETURNS uuid LANGUAGE plpgsql AS $$
DECLARE
  run_record hfsm.flow_run%ROWTYPE;
  source_record hfsm.flow_task%ROWTYPE;
  prior_group hfsm.flow_scatter_group%ROWTYPE;
  manifest_entry jsonb;
  partition_profile jsonb;
  aggregate_profile jsonb;
  manifest_count integer;
  grouping_factor integer;
  profile_cpu integer;
  profile_memory integer;
  profile_attempts integer;
  profile_timeout integer;
  profile_priority integer;
  profile_caps text[];
  profile_document jsonb;
  partition_index integer;
  aggregate_index integer;
  depth_index integer;
  first_child_index integer;
  final_child_index integer;
  child_index integer;
  previous_count integer;
  current_count integer;
  next_count integer;
  total_new_tasks integer;
  final_reduction boolean;
  group_name text;
  source_name text;
  partition_name text;
  aggregate_executor_name text;
  aggregate_task_key text;
  child_name text;
  root_name text;
  reduction_input_name text;
  expected_artifact uuid;
  previous_level text[];
  next_level text[];
  finished_source boolean;
BEGIN
  PERFORM 1 FROM hfsm.admission_gate WHERE id=1 FOR UPDATE;
  IF p_plan IS NULL OR jsonb_typeof(p_plan)<>'object' THEN
    RAISE EXCEPTION 'scatter plan must be a JSON object' USING ERRCODE='23514';
  END IF;
  -- A previous successful call may have lost its client-side ACK. Return
  -- the old group ONLY for an identical immutable plan and source task.
  SELECT * INTO prior_group FROM hfsm.flow_scatter_group
  WHERE run_id=p_run;
  IF FOUND THEN
    IF prior_group.source_task=p_source_task AND prior_group.plan=p_plan THEN
      RETURN prior_group.id;
    END IF;
    RAISE EXCEPTION 'scatter group already materialized with a different contract'
      USING ERRCODE='23514';
  END IF;

  SELECT * INTO run_record FROM hfsm.flow_run WHERE id=p_run FOR UPDATE;
  IF NOT FOUND OR run_record.status <> 'running' THEN
    RAISE EXCEPTION 'scatter run missing or terminal' USING ERRCODE='23514';
  END IF;
  SELECT * INTO source_record FROM hfsm.flow_task
  WHERE run_id=p_run AND task_key=p_source_task FOR UPDATE;
  IF NOT FOUND OR source_record.executor <> 'df2.scatter'
     OR source_record.status <> 'leased'
     OR source_record.lease_id IS DISTINCT FROM p_fence
     OR source_record.lease_owner IS DISTINCT FROM p_worker
     OR source_record.attempts <> p_attempt
     OR source_record.lease_until <= clock_timestamp() THEN
    RAISE EXCEPTION 'scatter source lacks a live, matching fence'
      USING ERRCODE='23514';
  END IF;
  IF source_record.output_ports <> '[]'::jsonb OR EXISTS (
     SELECT 1 FROM hfsm.flow_edge AS outgoing_edge
      WHERE outgoing_edge.run_id=p_run AND outgoing_edge.from_task=p_source_task
    ) THEN
    RAISE EXCEPTION 'scatter source must be a sink with zero declared outputs'
      USING ERRCODE='23514';
  END IF;

  group_name := p_plan->>'groupKey';
  source_name := p_plan->>'partitionExecutor';
  aggregate_executor_name := p_plan->>'aggregateExecutor';
  IF group_name IS NULL OR group_name !~ '^[A-Za-z0-9_.:-]{1,24}$'
     OR source_name IS NULL OR source_name !~ '^[A-Za-z0-9_.:-]{1,128}$'
     OR aggregate_executor_name IS NULL OR aggregate_executor_name !~ '^[A-Za-z0-9_.:-]{1,128}$'
     OR source_name='df2.scatter' OR aggregate_executor_name='df2.scatter'
     OR jsonb_typeof(p_plan->'partitions') IS DISTINCT FROM 'array'
     OR jsonb_typeof(p_plan->'partitionProfile') IS DISTINCT FROM 'object'
     OR jsonb_typeof(p_plan->'aggregateProfile') IS DISTINCT FROM 'object' THEN
    RAISE EXCEPTION 'invalid scatter group/executors/profiles'
      USING ERRCODE='23514';
  END IF;
  manifest_count := jsonb_array_length(p_plan->'partitions');
  grouping_factor := (p_plan->>'fanIn')::integer;
  IF manifest_count > 512 OR grouping_factor NOT BETWEEN 2 AND 64 THEN
    RAISE EXCEPTION 'scatter exceeds partition or fan-in bounds'
      USING ERRCODE='23514';
  END IF;
  IF (SELECT count(DISTINCT partition_value->>'key')
      FROM jsonb_array_elements(p_plan->'partitions') AS partition_array(partition_value))
       <> manifest_count
    OR EXISTS (
      SELECT 1 FROM jsonb_array_elements(p_plan->'partitions') AS partition_array(partition_value)
      WHERE jsonb_typeof(partition_value)<>'object'
         OR coalesce(partition_value->>'key','') !~ '^[A-Za-z0-9_.:-]{1,128}$'
         OR coalesce(partition_value->>'artifactId','') !~
              '^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$'
    ) THEN
      RAISE EXCEPTION 'invalid or repeated partition keys or artifact UUIDs'
        USING ERRCODE='23514';
  END IF;

  -- Validate all resource profiles BEFORE inserting any task. DB constraints
  -- remain authoritative, but this yields actionable diagnostic messages.
  FOREACH profile_document IN ARRAY ARRAY[
    p_plan->'partitionProfile', p_plan->'aggregateProfile']
  LOOP
    profile_cpu := (profile_document->>'cpu')::integer;
    profile_memory := (profile_document->>'memoryMb')::integer;
    profile_attempts := (profile_document->>'maxAttempts')::integer;
    profile_timeout := (profile_document->>'timeoutSeconds')::integer;
    profile_priority := (profile_document->>'priority')::integer;
    IF coalesce(profile_cpu,0) NOT BETWEEN 1 AND 1024
       OR coalesce(profile_memory,0) NOT BETWEEN 1 AND 1048576
       OR coalesce(profile_attempts,0) NOT BETWEEN 1 AND 100
       OR coalesce(profile_timeout,0) NOT BETWEEN 1 AND 86400
       OR coalesce(profile_priority,999) NOT BETWEEN -20 AND 20
       OR jsonb_typeof(profile_document->'capabilities') IS DISTINCT FROM 'array'
       OR jsonb_array_length(profile_document->'capabilities') > 32
       OR (SELECT count(DISTINCT capability_name)
           FROM jsonb_array_elements_text(profile_document->'capabilities')
             AS capability_array(capability_name))
           <> jsonb_array_length(profile_document->'capabilities')
       OR EXISTS (
         SELECT 1 FROM jsonb_array_elements_text(profile_document->'capabilities')
           AS capability_array(capability_name)
         WHERE capability_name !~ '^[A-Za-z0-9_.:-]{1,128}$'
       ) THEN
      RAISE EXCEPTION 'invalid partition or reducer resource policy'
        USING ERRCODE='23514';
    END IF;
  END LOOP;

  -- Calculate the exact finite size before inserting the first row.
  total_new_tasks := manifest_count;
  current_count := manifest_count;
  LOOP
    next_count := GREATEST(1, (current_count + grouping_factor - 1) / grouping_factor);
    total_new_tasks := total_new_tasks + next_count;
    EXIT WHEN next_count=1;
    current_count := next_count;
  END LOOP;
  IF (SELECT count(*) FROM hfsm.flow_task WHERE run_id=p_run) + total_new_tasks > 4096 THEN
    RAISE EXCEPTION 'dynamic run would exceed 4096 total tasks'
      USING ERRCODE='23514';
  END IF;

  -- Partition tasks are ready immediately. Their input lineage is in
  -- flow_partition_binding; the DF-02 adapter adds it as the "item" input.
  partition_profile := p_plan->'partitionProfile';
  profile_caps := ARRAY(
    SELECT capability_name
    FROM jsonb_array_elements_text(partition_profile->'capabilities')
      AS capability_array(capability_name));
  previous_level := ARRAY[]::text[];
  FOR partition_index IN 1..manifest_count LOOP
    manifest_entry := (p_plan->'partitions')->(partition_index-1);
    expected_artifact := (manifest_entry->>'artifactId')::uuid;
    IF NOT EXISTS (
       SELECT 1 FROM hfsm.flow_artifact AS artifact_record
       WHERE artifact_record.id=expected_artifact
         AND artifact_record.tenant_id=run_record.tenant_id) THEN
      RAISE EXCEPTION 'partition % references unpublished/cross-tenant artifact',partition_index
        USING ERRCODE='23514';
    END IF;
    partition_name := 'df2:'||group_name||':p:'||lpad(partition_index::text,6,'0');
    INSERT INTO hfsm.flow_task (
      run_id, task_key, executor, required_inputs, output_ports,
      cpu_units,memory_mb,required_caps,priority,max_attempts,timeout_seconds,status
    ) VALUES (
      p_run,partition_name,source_name,0,'["value"]'::jsonb,
      (partition_profile->>'cpu')::integer,(partition_profile->>'memoryMb')::integer,
      profile_caps,(partition_profile->>'priority')::integer,
      (partition_profile->>'maxAttempts')::integer,
      (partition_profile->>'timeoutSeconds')::integer,'ready'
    );
    previous_level := array_append(previous_level,partition_name);
  END LOOP;

  aggregate_profile := p_plan->'aggregateProfile';
  profile_caps := ARRAY(
    SELECT capability_name
    FROM jsonb_array_elements_text(aggregate_profile->'capabilities')
      AS capability_array(capability_name));
  depth_index := 0;
  previous_count := manifest_count;
  LOOP
    next_count := GREATEST(1, (previous_count + grouping_factor - 1) / grouping_factor);
    final_reduction := next_count=1;
    next_level := ARRAY[]::text[];
    FOR aggregate_index IN 1..next_count LOOP
      aggregate_task_key := 'df2:'||group_name||':r:'||depth_index::text||':'||lpad(aggregate_index::text,6,'0');
      first_child_index := (aggregate_index-1)*grouping_factor+1;
      final_child_index := LEAST(aggregate_index*grouping_factor,previous_count);
      INSERT INTO hfsm.flow_task (
        run_id,task_key,executor,required_inputs,output_ports,
        cpu_units,memory_mb,required_caps,priority,max_attempts,timeout_seconds,status
      ) VALUES (
        p_run,aggregate_task_key,aggregate_executor_name,
        GREATEST(0,final_child_index-first_child_index+1),
        CASE WHEN final_reduction THEN '["result"]'::jsonb ELSE '["value"]'::jsonb END,
        (aggregate_profile->>'cpu')::integer,(aggregate_profile->>'memoryMb')::integer,
        profile_caps,(aggregate_profile->>'priority')::integer,
        (aggregate_profile->>'maxAttempts')::integer,
        (aggregate_profile->>'timeoutSeconds')::integer,
        CASE WHEN previous_count=0 THEN 'ready' ELSE 'blocked' END
      );
      IF previous_count>0 THEN
        FOR child_index IN first_child_index..final_child_index LOOP
          child_name := previous_level[child_index];
          reduction_input_name := 'i'||lpad((child_index-first_child_index+1)::text,6,'0');
          INSERT INTO hfsm.flow_edge(run_id,from_task,from_port,to_task,to_port)
          VALUES(p_run,child_name,'value',aggregate_task_key,reduction_input_name);
        END LOOP;
      END IF;
      next_level := array_append(next_level,aggregate_task_key);
    END LOOP;
    previous_level := next_level;
    previous_count := next_count;
    EXIT WHEN final_reduction;
    depth_index := depth_index+1;
  END LOOP;
  root_name := previous_level[1];

  INSERT INTO hfsm.flow_scatter_group (
    id,run_id,source_task,group_key,root_task,partition_count,fan_in,plan,
    plan_digest,source_lease_id,source_worker_id,source_attempt
  ) VALUES (
    p_group_id,p_run,p_source_task,group_name,root_name,manifest_count,grouping_factor,
    p_plan,encode(sha256(convert_to(p_plan::text,'UTF8')),'hex'),
    p_fence,p_worker,p_attempt
  );
  FOR partition_index IN 1..manifest_count LOOP
    manifest_entry := (p_plan->'partitions')->(partition_index-1);
    partition_name := 'df2:'||group_name||':p:'||lpad(partition_index::text,6,'0');
    INSERT INTO hfsm.flow_partition_binding (
      run_id,group_key,task_key,tenant_id,partition_ordinal,partition_key,artifact_id
    ) VALUES (
      p_run,group_name,partition_name,run_record.tenant_id,
      partition_index,manifest_entry->>'key',(manifest_entry->>'artifactId')::uuid
    );
  END LOOP;

  -- The pre-existing DF-01 function is still the ONLY authority to settle a
  -- successful leased task. Because all new tasks already exist, it cannot
  -- prematurely declare the run succeeded. Any failure must RAISE to roll
  -- back the entire function call, not return false after partial writes.
  finished_source := hfsm.flow_finish_task(
    p_run,p_source_task,p_fence,p_worker,p_attempt,'{}'::jsonb);
  IF NOT finished_source THEN
    RAISE EXCEPTION 'scatter lease expired during expansion; entire graph rolled back'
      USING ERRCODE='23514';
  END IF;
  INSERT INTO hfsm.flow_event(run_id,task_key,kind,detail) VALUES(
    p_run,p_source_task,'scatter_expanded',jsonb_build_object(
      'groupId',p_group_id,'groupKey',group_name,'rootTask',root_name,
      'partitionCount',manifest_count,'fanIn',grouping_factor,
      'planDigest',encode(sha256(convert_to(p_plan::text,'UTF8')),'hex')));
  RETURN p_group_id;
END;
$$;

-- An idempotent acknowledgement for the DF-01 worker's second finish call.
-- It never permits a new or stale owner to commit a different turn.
CREATE FUNCTION hfsm.flow_scatter_ack(
  p_run uuid,p_task text,p_fence uuid,p_worker uuid,p_attempt integer
) RETURNS boolean LANGUAGE sql STABLE AS $$
  SELECT EXISTS (
    SELECT 1 FROM hfsm.flow_scatter_group AS scatter_record
    JOIN hfsm.flow_task AS task_record
      ON task_record.run_id=scatter_record.run_id
       AND task_record.task_key=scatter_record.source_task
    WHERE scatter_record.run_id=p_run
      AND scatter_record.source_task=p_task
      AND scatter_record.source_lease_id=p_fence
      AND scatter_record.source_worker_id=p_worker
      AND scatter_record.source_attempt=p_attempt
      AND task_record.status='done'
  );
$$;

CREATE FUNCTION hfsm.flow_partition_input(p_run uuid,p_task text)
RETURNS jsonb LANGUAGE sql STABLE AS $$
 SELECT jsonb_build_object(
   'groupKey',binding_record.group_key,
   'partitionKey',binding_record.partition_key,
   'ordinal',binding_record.partition_ordinal,
   'artifact',jsonb_build_object(
     'artifactId',artifact_record.id,'digest',artifact_record.digest,
     'sizeBytes',artifact_record.size_bytes,
     'storageUri',artifact_record.storage_uri,
     'mediaType',artifact_record.media_type,'metadata',artifact_record.metadata))
 FROM hfsm.flow_partition_binding AS binding_record
 JOIN hfsm.flow_artifact AS artifact_record
   ON artifact_record.id=binding_record.artifact_id
 WHERE binding_record.run_id=p_run AND binding_record.task_key=p_task;
$$;

CREATE FUNCTION hfsm.flow_scatter_summary(p_run uuid)
RETURNS jsonb LANGUAGE sql STABLE AS $$
 SELECT jsonb_build_object(
   'groupId',scatter_record.id,'groupKey',scatter_record.group_key,
   'sourceTask',scatter_record.source_task,
   'rootTask',scatter_record.root_task,
   'partitionCount',scatter_record.partition_count,
   'fanIn',scatter_record.fan_in,'planDigest',scatter_record.plan_digest,
   'rootStatus',root_task.status,
   'rootResult',result_record.artifact_id)
 FROM hfsm.flow_scatter_group AS scatter_record
 JOIN hfsm.flow_task AS root_task ON root_task.run_id=scatter_record.run_id
   AND root_task.task_key=scatter_record.root_task
 LEFT JOIN hfsm.flow_task_output AS result_record
   ON result_record.run_id=scatter_record.run_id
    AND result_record.task_key=scatter_record.root_task AND result_record.port='result'
 WHERE scatter_record.run_id=p_run;
$$;

COMMENT ON TABLE hfsm.flow_scatter_group IS
  'Immutable one-shot expansion contract for a leased DF-01 df2.scatter sink';
COMMENT ON TABLE hfsm.flow_partition_binding IS
  'Append-only, tenant-scoped partition artifact input lineage; enriched as task input item';
