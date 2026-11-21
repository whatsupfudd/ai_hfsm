-- DF-01: bounded acyclic dataflow for finite FUDD computations.
-- Requires HFSM migrations 001..004. PostgreSQL 16+. Apply once, in a migration TX.
-- No changes to CT-03 commitTurn, CT-04 outbox or CT-05 history.

ALTER TABLE hfsm.tenant
  ADD COLUMN max_flow_runs integer NOT NULL DEFAULT 1000 CHECK(max_flow_runs > 0),
  ADD COLUMN max_flow_inflight integer NOT NULL DEFAULT 100 CHECK(max_flow_inflight > 0);
ALTER TABLE hfsm.concurrency_group
  ADD COLUMN max_flow_tasks integer NOT NULL DEFAULT 100 CHECK(max_flow_tasks > 0);

CREATE TABLE hfsm.flow_definition (
 id uuid PRIMARY KEY,
 tenant_id uuid NOT NULL REFERENCES hfsm.tenant(id),
 name text NOT NULL CHECK(length(name) BETWEEN 1 AND 128),
 version text NOT NULL CHECK(length(version) BETWEEN 1 AND 128),
 digest hfsm.digest64 NOT NULL,
 spec jsonb NOT NULL CHECK(jsonb_typeof(spec)='object'),
 CONSTRAINT flow_definition_digest_ck CHECK(digest=encode(sha256(convert_to(spec::text,'UTF8')),'hex')),
 created_at timestamptz NOT NULL DEFAULT clock_timestamp(),
 UNIQUE(tenant_id,name,version), UNIQUE(tenant_id,id)
);
CREATE TRIGGER flow_definition_immutable BEFORE UPDATE OR DELETE ON hfsm.flow_definition
 FOR EACH ROW EXECUTE FUNCTION hfsm.reject_history_mutation();

CREATE TABLE hfsm.flow_run (
 id uuid PRIMARY KEY,
 tenant_id uuid NOT NULL REFERENCES hfsm.tenant(id),
 definition_id uuid NOT NULL,
 definition_digest hfsm.digest64 NOT NULL,
 source_effect_key text NOT NULL CHECK(length(source_effect_key)>0),
 source_instance_id uuid REFERENCES hfsm.instance(id),
 source_release text,
 correlation_id uuid,
 traceparent text,
 input_value jsonb NOT NULL,
 success_signal jsonb,
 failure_signal jsonb,
 status text NOT NULL DEFAULT 'running' CHECK(status IN('running','succeeded','failed','cancelled')),
 created_at timestamptz NOT NULL DEFAULT clock_timestamp(),
 updated_at timestamptz NOT NULL DEFAULT clock_timestamp(),
 finished_at timestamptz,
 FOREIGN KEY(tenant_id,definition_id) REFERENCES hfsm.flow_definition(tenant_id,id),
 UNIQUE(tenant_id,source_effect_key), UNIQUE(tenant_id,id)
);
CREATE INDEX flow_run_tenant_status_idx ON hfsm.flow_run(tenant_id,status,created_at);

CREATE TABLE hfsm.flow_task (
 run_id uuid NOT NULL REFERENCES hfsm.flow_run(id),
 task_key text NOT NULL CHECK(length(task_key) BETWEEN 1 AND 128),
 executor text NOT NULL CHECK(length(executor) BETWEEN 1 AND 128),
 required_inputs integer NOT NULL CHECK(required_inputs BETWEEN 0 AND 64),
 received_inputs integer NOT NULL DEFAULT 0 CHECK(received_inputs BETWEEN 0 AND 64),
 output_ports jsonb NOT NULL CHECK(jsonb_typeof(output_ports)='array'),
 cpu_units integer NOT NULL CHECK(cpu_units BETWEEN 1 AND 1024),
 memory_mb integer NOT NULL CHECK(memory_mb BETWEEN 1 AND 1048576),
 required_caps text[] NOT NULL DEFAULT '{}',
 priority integer NOT NULL DEFAULT 0 CHECK(priority BETWEEN -20 AND 20),
 max_attempts integer NOT NULL CHECK(max_attempts BETWEEN 1 AND 100),
 timeout_seconds integer NOT NULL CHECK(timeout_seconds BETWEEN 1 AND 86400),
 status text NOT NULL CHECK(status IN('blocked','ready','leased','done','failed','cancelled')),
 attempts integer NOT NULL DEFAULT 0 CHECK(attempts BETWEEN 0 AND 100),
 lease_id uuid, lease_owner uuid, lease_until timestamptz,
 available_at timestamptz NOT NULL DEFAULT clock_timestamp(),
 last_error text,
 created_at timestamptz NOT NULL DEFAULT clock_timestamp(),
 finished_at timestamptz,
 PRIMARY KEY(run_id,task_key),
 CONSTRAINT flow_task_readiness_ck CHECK(received_inputs<=required_inputs),
 CONSTRAINT flow_task_ready_before_claim_ck CHECK(status NOT IN('leased','done') OR received_inputs=required_inputs),
 CONSTRAINT flow_task_lease_ck CHECK(
    (status='leased' AND lease_id IS NOT NULL AND lease_owner IS NOT NULL AND lease_until IS NOT NULL)
    OR (status<>'leased' AND lease_id IS NULL AND lease_owner IS NULL AND lease_until IS NULL))
);
CREATE INDEX flow_task_ready_idx ON hfsm.flow_task(status,available_at,priority DESC)
 WHERE status IN ('ready','leased');
CREATE INDEX flow_task_owner_idx ON hfsm.flow_task(lease_owner,lease_until) WHERE status='leased';
CREATE INDEX flow_task_run_idx ON hfsm.flow_task(run_id,status);

CREATE TABLE hfsm.flow_edge (
 run_id uuid NOT NULL REFERENCES hfsm.flow_run(id),
 from_task text NOT NULL, from_port text NOT NULL,
 to_task text NOT NULL, to_port text NOT NULL,
 PRIMARY KEY(run_id,from_task,from_port,to_task,to_port),
 UNIQUE(run_id,to_task,to_port),
 FOREIGN KEY(run_id,from_task) REFERENCES hfsm.flow_task(run_id,task_key),
 FOREIGN KEY(run_id,to_task) REFERENCES hfsm.flow_task(run_id,task_key)
);
CREATE TRIGGER flow_edge_immutable BEFORE UPDATE OR DELETE ON hfsm.flow_edge
 FOR EACH ROW EXECUTE FUNCTION hfsm.reject_history_mutation();

CREATE TABLE hfsm.flow_artifact (
 id uuid PRIMARY KEY,
 tenant_id uuid NOT NULL REFERENCES hfsm.tenant(id),
 digest hfsm.digest64 NOT NULL,
 size_bytes bigint NOT NULL CHECK(size_bytes>=0),
 storage_uri text NOT NULL CHECK(length(storage_uri)>0),
 media_type text NOT NULL CHECK(length(media_type)>0),
 metadata jsonb NOT NULL DEFAULT '{}'::jsonb,
 created_at timestamptz NOT NULL DEFAULT clock_timestamp(),
 UNIQUE(tenant_id,digest), UNIQUE(tenant_id,id)
);
CREATE TRIGGER flow_artifact_immutable BEFORE UPDATE OR DELETE ON hfsm.flow_artifact
 FOR EACH ROW EXECUTE FUNCTION hfsm.reject_history_mutation();

CREATE TABLE hfsm.flow_task_output (
 run_id uuid NOT NULL, task_key text NOT NULL, port text NOT NULL,
 artifact_id uuid NOT NULL,
 PRIMARY KEY(run_id,task_key,port),
 UNIQUE(run_id,task_key,port,artifact_id),
 FOREIGN KEY(run_id,task_key) REFERENCES hfsm.flow_task(run_id,task_key),
 FOREIGN KEY(artifact_id) REFERENCES hfsm.flow_artifact(id)
);
CREATE TRIGGER flow_task_output_immutable BEFORE UPDATE OR DELETE ON hfsm.flow_task_output
 FOR EACH ROW EXECUTE FUNCTION hfsm.reject_history_mutation();

-- Each dependency creates exactly one token, and only after its producer's
-- materialized artifact has been published. Tokens are append-only.
CREATE TABLE hfsm.flow_token (
 run_id uuid NOT NULL,
 from_task text NOT NULL, from_port text NOT NULL,
 to_task text NOT NULL, to_port text NOT NULL,
 artifact_id uuid NOT NULL,
 created_at timestamptz NOT NULL DEFAULT clock_timestamp(),
 PRIMARY KEY(run_id,to_task,to_port),
 FOREIGN KEY(run_id,from_task,from_port,to_task,to_port)
   REFERENCES hfsm.flow_edge(run_id,from_task,from_port,to_task,to_port),
 FOREIGN KEY(run_id,from_task,from_port,artifact_id)
   REFERENCES hfsm.flow_task_output(run_id,task_key,port,artifact_id)
);
CREATE TRIGGER flow_token_immutable BEFORE UPDATE OR DELETE ON hfsm.flow_token
 FOR EACH ROW EXECUTE FUNCTION hfsm.reject_history_mutation();

CREATE TABLE hfsm.flow_task_attempt (
 run_id uuid NOT NULL, task_key text NOT NULL, attempt_no integer NOT NULL,
 lease_id uuid NOT NULL UNIQUE, worker_id uuid NOT NULL,
 started_at timestamptz NOT NULL DEFAULT clock_timestamp(), finished_at timestamptz,
 outcome text NOT NULL CHECK(outcome IN('leased','succeeded','failed','expired','cancelled')),
 error_text text,
 PRIMARY KEY(run_id,task_key,attempt_no),
 FOREIGN KEY(run_id,task_key) REFERENCES hfsm.flow_task(run_id,task_key)
);

CREATE TABLE hfsm.flow_dead_letter (
 run_id uuid NOT NULL, task_key text NOT NULL,
 attempts integer NOT NULL, reason text NOT NULL,
 recorded_at timestamptz NOT NULL DEFAULT clock_timestamp(),
 PRIMARY KEY(run_id,task_key),
 FOREIGN KEY(run_id,task_key) REFERENCES hfsm.flow_task(run_id,task_key)
);
CREATE TRIGGER flow_dead_letter_immutable BEFORE UPDATE OR DELETE ON hfsm.flow_dead_letter
 FOR EACH ROW EXECUTE FUNCTION hfsm.reject_history_mutation();

CREATE TABLE hfsm.flow_event (
 id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
 run_id uuid NOT NULL REFERENCES hfsm.flow_run(id),
 task_key text,
 kind text NOT NULL CHECK(length(kind)>0),
 detail jsonb NOT NULL DEFAULT '{}'::jsonb,
 recorded_at timestamptz NOT NULL DEFAULT clock_timestamp()
);
CREATE INDEX flow_event_run_idx ON hfsm.flow_event(run_id,id);
CREATE TRIGGER flow_event_immutable BEFORE UPDATE OR DELETE ON hfsm.flow_event
 FOR EACH ROW EXECUTE FUNCTION hfsm.reject_history_mutation();

CREATE TABLE hfsm.flow_completion (
 run_id uuid PRIMARY KEY REFERENCES hfsm.flow_run(id),
 status text NOT NULL DEFAULT 'ready' CHECK(status IN('ready','delivered')),
 signal_id uuid REFERENCES hfsm.signal(id),
 created_at timestamptz NOT NULL DEFAULT clock_timestamp(),
 delivered_at timestamptz
);

-- Reuses the CT-06 worker heartbeat/generation identity; resource reservations
-- are computed from live fenced leases inside one serialized admission TX.
CREATE TABLE hfsm.flow_worker_resource (
 worker_id uuid PRIMARY KEY REFERENCES hfsm.worker_registry(id),
 cpu_units integer NOT NULL CHECK(cpu_units BETWEEN 1 AND 1000000),
 memory_mb integer NOT NULL CHECK(memory_mb BETWEEN 1 AND 100000000),
 max_tasks integer NOT NULL CHECK(max_tasks BETWEEN 1 AND 100000)
);

-- Operational state is mutable, but graph identity, inputs and retry policy
-- are not. This prevents silent changes to a pinned run or its task contract.
CREATE FUNCTION hfsm.flow_guard_run() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
 IF (OLD.id,OLD.tenant_id,OLD.definition_id,OLD.definition_digest,OLD.source_effect_key,
     OLD.source_instance_id,OLD.source_release,OLD.correlation_id,OLD.traceparent,
     OLD.input_value,OLD.success_signal,OLD.failure_signal,OLD.created_at)
  IS DISTINCT FROM
    (NEW.id,NEW.tenant_id,NEW.definition_id,NEW.definition_digest,NEW.source_effect_key,
     NEW.source_instance_id,NEW.source_release,NEW.correlation_id,NEW.traceparent,
     NEW.input_value,NEW.success_signal,NEW.failure_signal,NEW.created_at)
 THEN RAISE EXCEPTION 'immutable flow run identity/input' USING ERRCODE='23514'; END IF;
 IF OLD.status IN('succeeded','failed','cancelled') AND NEW.status<>OLD.status THEN
   RAISE EXCEPTION 'terminal flow run status cannot change' USING ERRCODE='23514'; END IF;
 IF NEW.status='succeeded' AND OLD.status<>'succeeded' AND EXISTS
     (SELECT 1 FROM hfsm.flow_task WHERE run_id=NEW.id AND status<>'done') THEN
   RAISE EXCEPTION 'flow cannot succeed with unfinished tasks' USING ERRCODE='23514'; END IF;
 RETURN NEW;
END $$;
CREATE TRIGGER flow_guard_run BEFORE UPDATE ON hfsm.flow_run
 FOR EACH ROW EXECUTE FUNCTION hfsm.flow_guard_run();

CREATE FUNCTION hfsm.flow_guard_task() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
 IF (OLD.run_id,OLD.task_key,OLD.executor,OLD.required_inputs,OLD.output_ports,
     OLD.cpu_units,OLD.memory_mb,OLD.required_caps,OLD.priority,OLD.max_attempts,
     OLD.timeout_seconds,OLD.created_at)
  IS DISTINCT FROM
    (NEW.run_id,NEW.task_key,NEW.executor,NEW.required_inputs,NEW.output_ports,
     NEW.cpu_units,NEW.memory_mb,NEW.required_caps,NEW.priority,NEW.max_attempts,
     NEW.timeout_seconds,NEW.created_at)
 THEN RAISE EXCEPTION 'immutable flow task definition' USING ERRCODE='23514'; END IF;
 IF OLD.status IN('done','failed','cancelled') AND NEW.status<>OLD.status THEN
    RAISE EXCEPTION 'terminal flow task cannot change state' USING ERRCODE='23514'; END IF;
 RETURN NEW;
END $$;
CREATE TRIGGER flow_guard_task BEFORE UPDATE ON hfsm.flow_task
 FOR EACH ROW EXECUTE FUNCTION hfsm.flow_guard_task();

-- SQL registration validates the same bounded acyclic shape as Flow.Spec.
CREATE FUNCTION hfsm.flow_register_definition(
    p_id uuid, p_tenant uuid, p_name text, p_version text, p_spec jsonb
  )
  RETURNS uuid LANGUAGE plpgsql AS $$
DECLARE
  tv jsonb; ev jsonb; kv text; prev uuid; prior_spec jsonb;
  task_count integer; edge_count integer; remaining text[]; i integer;
  incoming integer; ports integer;
BEGIN
  IF coalesce(jsonb_typeof(p_spec),'null') <> 'object'
    OR coalesce(jsonb_typeof(p_spec->'tasks'),'null') <> 'array'
    OR coalesce(jsonb_typeof(p_spec->'edges'),'null') <> 'array' THEN
      RAISE EXCEPTION 'invalid DAG specification' USING ERRCODE='23514';
  END IF;
  task_count := jsonb_array_length(p_spec->'tasks');
  edge_count := jsonb_array_length(p_spec->'edges');
  IF task_count NOT BETWEEN 1 AND 256 OR edge_count>4096 THEN
    RAISE EXCEPTION 'DAG exceeds static bounds' USING ERRCODE='23514';
  END IF;
  IF (SELECT count(DISTINCT x->>'key') FROM jsonb_array_elements(p_spec->'tasks') AS tasks_array(x)) <> task_count
    OR EXISTS(
      SELECT 1 FROM jsonb_array_elements(p_spec->'tasks') AS tasks_array(x)
        WHERE nullif(x->>'key','') IS NULL
          OR nullif(x->>'executor','') IS NULL
          OR jsonb_typeof(x->'inputs')<>'array' OR jsonb_typeof(x->'outputs')<>'array'
          OR jsonb_typeof(x->'capabilities')<>'array'
          OR coalesce((x->>'cpu')::integer,0) NOT BETWEEN 1 AND 1024
          OR coalesce((x->>'memoryMb')::integer,0) NOT BETWEEN 1 AND 1048576
          OR coalesce((x->>'maxAttempts')::integer,0) NOT BETWEEN 1 AND 100
          OR coalesce((x->>'timeoutSeconds')::integer,0) NOT BETWEEN 1 AND 86400
          OR coalesce((x->>'priority')::integer,999) NOT BETWEEN -20 AND 20
        )
    THEN
      RAISE EXCEPTION 'invalid task definition' USING ERRCODE='23514';
  END IF;
  FOR tv IN
    SELECT item FROM jsonb_array_elements(p_spec->'tasks') AS tasks_array(item)
  LOOP
    IF jsonb_array_length(tv->'inputs')>64 OR jsonb_array_length(tv->'outputs')>64
      OR (
        SELECT count(DISTINCT v) FROM jsonb_array_elements_text(tv->'inputs') AS port_array(v)
        ) <> jsonb_array_length(tv->'inputs')
      OR (
        SELECT count(DISTINCT v) FROM jsonb_array_elements_text(tv->'outputs') AS port_array(v)
        ) <> jsonb_array_length(tv->'outputs')
    THEN
      RAISE EXCEPTION 'duplicate or oversized ports' USING ERRCODE='23514';
    END IF;
    FOR kv IN
      SELECT port FROM jsonb_array_elements_text(tv->'inputs') AS port_array(port)
    LOOP
      SELECT count(*) INTO incoming FROM jsonb_array_elements(p_spec->'edges') AS edges_array(e)
        WHERE e->>'toTask' = tv->>'key' AND e->>'toPort' = kv;
      IF incoming<>1 THEN
        RAISE EXCEPTION 'each input port must have exactly one producer' USING ERRCODE='23514';
      END IF;
    END LOOP;
  END LOOP;
  FOR ev IN
    SELECT item FROM jsonb_array_elements(p_spec->'edges') AS edges_array(item)
  LOOP
    IF ev->>'fromTask' = ev->>'toTask'
      OR NOT EXISTS(
        SELECT 1 FROM jsonb_array_elements(p_spec->'tasks') AS tasks_array(x)
        WHERE x->>'key' = ev->>'fromTask' AND (x->'outputs') ? (ev->>'fromPort')
      )
      OR NOT EXISTS(
        SELECT 1 FROM jsonb_array_elements(p_spec->'tasks') AS tasks_array(x)
        WHERE x->>'key' = ev->>'toTask' AND (x->'inputs') ? (ev->>'toPort')
      )
    THEN
      RAISE EXCEPTION 'edge references missing task/port or self-loop' USING ERRCODE='23514';
    END IF;
  END LOOP;
  SELECT array_agg(x->>'key') INTO remaining FROM jsonb_array_elements(p_spec->'tasks') AS tasks_array(x);
  FOR i IN 1..task_count
  LOOP
    SELECT x INTO kv FROM unnest(remaining) AS key_array(x) WHERE NOT EXISTS (
      SELECT 1 FROM jsonb_array_elements(p_spec->'edges') AS edges_array(ele)
      WHERE ele->>'toTask' = x AND (ele->>'fromTask') = ANY(remaining))
      ORDER BY x LIMIT 1;
    IF kv IS NULL THEN
      RAISE EXCEPTION 'cyclic DAG' USING ERRCODE='23514';
    END IF;
    remaining := array_remove(remaining,kv);
  END LOOP;
  PERFORM 1 FROM hfsm.tenant WHERE id = p_tenant AND enabled FOR UPDATE;
  IF NOT FOUND THEN
    RAISE EXCEPTION 'missing/disabled tenant' USING ERRCODE='23514';
  END IF;
  SELECT id,spec INTO prev,prior_spec FROM hfsm.flow_definition
    WHERE tenant_id = p_tenant AND name = p_name AND version = p_version;
  IF FOUND THEN
    IF prior_spec <> p_spec THEN
      RAISE EXCEPTION 'immutable flow version differs' USING ERRCODE='23514';
    END IF;
    RETURN prev;
  END IF;
  INSERT INTO hfsm.flow_definition
      (id, tenant_id, name, version, digest, spec)
    VALUES
      (p_id, p_tenant, p_name, p_version, encode(sha256(convert_to(p_spec::text,'UTF8')),'hex'), p_spec);
  RETURN p_id;
END $$;

CREATE FUNCTION hfsm.flow_register_worker(p_worker uuid,p_cpu integer,p_memory integer,p_max integer)
 RETURNS boolean LANGUAGE plpgsql AS $$
BEGIN
 PERFORM 1 FROM hfsm.admission_gate WHERE id=1 FOR UPDATE;
 IF NOT EXISTS(SELECT 1 FROM hfsm.worker_registry WHERE id=p_worker) THEN
  RAISE EXCEPTION 'register CT06 worker first' USING ERRCODE='23514'; END IF;
 INSERT INTO hfsm.flow_worker_resource(worker_id,cpu_units,memory_mb,max_tasks)
 VALUES(p_worker,p_cpu,p_memory,p_max)
 ON CONFLICT(worker_id) DO UPDATE SET cpu_units=excluded.cpu_units,
   memory_mb=excluded.memory_mb,max_tasks=excluded.max_tasks;
 RETURN true;
END $$;


CREATE FUNCTION hfsm.flow_start_run(
    p_run uuid,p_definition uuid,p_effect text,p_source uuid,p_release text,
    p_input jsonb,p_success jsonb,p_failure jsonb
  )
RETURNS uuid LANGUAGE plpgsql AS $$
DECLARE d hfsm.flow_definition%ROWTYPE; old_id uuid; old_def uuid; old_source uuid;
  old_release text; old_input jsonb; old_success jsonb; old_failure jsonb;
  s hfsm.instance%ROWTYPE; tunk jsonb; eunk jsonb; corr uuid; tr text; current_release text;
BEGIN
  PERFORM 1 FROM hfsm.admission_gate WHERE id=1 FOR UPDATE;
  SELECT * INTO d FROM hfsm.flow_definition WHERE id=p_definition;
  IF NOT FOUND THEN
    RAISE EXCEPTION 'flow definition not registered' USING ERRCODE='23503';
  END IF;
  SELECT id, definition_id, source_instance_id, source_release, input_value, success_signal, failure_signal
    INTO old_id, old_def, old_source, old_release, old_input, old_success, old_failure FROM hfsm.flow_run
    WHERE tenant_id = d.tenant_id AND source_effect_key = p_effect;
  IF FOUND THEN
    IF old_def <> p_definition OR old_source IS DISTINCT FROM p_source
      OR old_release IS DISTINCT FROM p_release OR old_input IS DISTINCT FROM p_input
      OR old_success IS DISTINCT FROM p_success OR old_failure IS DISTINCT FROM p_failure
    THEN
      RAISE EXCEPTION 'flow effect key reused with a different immutable start contract' USING ERRCODE='23514';
    END IF;
    RETURN old_id;
  END IF;
  IF p_source IS NOT NULL THEN
    SELECT * INTO s FROM hfsm.instance WHERE id = p_source FOR UPDATE;
    IF NOT FOUND OR s.tenant_id <> d.tenant_id OR s.status IN ('done','failed','cancelled') THEN
      RAISE EXCEPTION 'HFSM source missing, cross-tenant or terminal' USING ERRCODE='23514';
    END IF;
    current_release := s.machine_name || '@' || s.machine_version || '#' || s.spec_digest;
    IF p_release IS DISTINCT FROM current_release OR p_success IS NULL OR p_failure IS NULL THEN
      RAISE EXCEPTION 'HFSM flow boundary requires exact release and both completion payloads'
        USING ERRCODE='23514';
    END IF;
    SELECT correlation_id,traceparent INTO corr,tr
      FROM hfsm.outbox
      WHERE effect_key=p_effect AND instance_id=p_source;
    IF NOT FOUND THEN
      RAISE EXCEPTION 'HFSM flow start must reference a committed outbox intent' USING ERRCODE='23514';
    END IF;
  ELSE
    IF p_release IS NOT NULL OR p_success IS NOT NULL OR p_failure IS NOT NULL THEN
      RAISE EXCEPTION 'standalone flow cannot have HFSM completion contract' USING ERRCODE='23514';
    END IF;
  END IF;
  IF (
      SELECT count(*) FROM hfsm.flow_run WHERE tenant_id = d.tenant_id AND status='running'
    ) >= (
      SELECT max_flow_runs FROM hfsm.tenant WHERE id = d.tenant_id
    )
  THEN
    RAISE EXCEPTION 'tenant flow-run quota exhausted' USING ERRCODE='23514';
  END IF;
  INSERT INTO hfsm.flow_run
    (id, tenant_id, definition_id, definition_digest, source_effect_key,
      source_instance_id, source_release, correlation_id, traceparent, input_value, success_signal, failure_signal
    )
  VALUES
    (p_run, d.tenant_id, d.id, d.digest, p_effect, p_source, p_release, coalesce(corr, p_run), tr,
      p_input,p_success,p_failure
    );
  INSERT INTO hfsm.flow_task
    (run_id,task_key,executor,required_inputs,output_ports,
      cpu_units,memory_mb,required_caps,priority,max_attempts,timeout_seconds,status
    )
  SELECT p_run, t->>'key', t->>'executor', jsonb_array_length(t->'inputs'), t->'outputs',
      (t->>'cpu')::integer, (t->>'memoryMb')::integer,
      ARRAY(SELECT jsonb_array_elements_text(t->'capabilities')),
      (t->>'priority')::integer,(t->>'maxAttempts')::integer,
      (t->>'timeoutSeconds')::integer,
      CASE WHEN jsonb_array_length(t->'inputs') = 0 THEN
        'ready'
      ELSE
        'blocked'
      END
    FROM jsonb_array_elements(d.spec->'tasks') AS tasks_array(t);
  INSERT INTO hfsm.flow_edge
    (run_id,from_task,from_port,to_task,to_port)
    SELECT p_run, e->>'fromTask', e->>'fromPort', e->>'toTask', e->>'toPort'
    FROM jsonb_array_elements(d.spec->'edges') AS edges_array(e);
  INSERT INTO hfsm.flow_event
    (run_id,kind,detail)
  VALUES
    (p_run,'run_started', jsonb_build_object('definitionDigest',d.digest,'sourceEffectKey',p_effect));
  RETURN p_run;
END $$;

-- Artifact publication must happen AFTER durable byte upload & checksum
-- verification, BEFORE the task completion. SQL cannot verify remote bytes.
CREATE FUNCTION hfsm.flow_publish_artifact(
 p_id uuid,p_tenant uuid,p_digest text,p_size bigint,p_uri text,p_media text,p_meta jsonb)
 RETURNS uuid LANGUAGE plpgsql AS $$
DECLARE existing uuid; old_size bigint;
BEGIN
 IF p_digest !~ '^[0-9a-f]{64}$' OR p_size<0 OR length(p_uri)=0 OR length(p_media)=0 THEN
   RAISE EXCEPTION 'invalid immutable artifact manifest' USING ERRCODE='23514'; END IF;
 SELECT id,size_bytes INTO existing,old_size FROM hfsm.flow_artifact
 WHERE tenant_id=p_tenant AND digest=p_digest;
 IF FOUND THEN
   IF old_size<>p_size THEN RAISE EXCEPTION 'same digest has different byte size' USING ERRCODE='23514'; END IF;
   RETURN existing;
 END IF;
 INSERT INTO hfsm.flow_artifact(id,tenant_id,digest,size_bytes,storage_uri,media_type,metadata)
 VALUES(p_id,p_tenant,p_digest,p_size,p_uri,p_media,p_meta)
 ON CONFLICT (tenant_id,digest) DO NOTHING;
 SELECT id,size_bytes INTO existing,old_size FROM hfsm.flow_artifact
 WHERE tenant_id=p_tenant AND digest=p_digest;
 IF old_size<>p_size THEN RAISE EXCEPTION 'concurrent CAS size mismatch' USING ERRCODE='23514'; END IF;
 RETURN existing;
END $$;

CREATE FUNCTION hfsm.flow_claim_tasks(p_worker uuid,p_batch integer,p_seconds integer,p_run uuid)
 RETURNS SETOF jsonb LANGUAGE plpgsql AS $$
DECLARE w hfsm.worker_registry%ROWTYPE; res hfsm.flow_worker_resource%ROWTYPE;
 taskrow record; n integer; busy_cpu bigint; busy_mem bigint; busy_count bigint;
 tid uuid; until_ timestamptz; k integer;
BEGIN
 PERFORM 1 FROM hfsm.admission_gate WHERE id=1 FOR UPDATE;
 SELECT * INTO w FROM hfsm.worker_registry WHERE id=p_worker;
 SELECT * INTO res FROM hfsm.flow_worker_resource WHERE worker_id=p_worker;
 IF NOT FOUND OR w.id IS NULL OR w.state<>'active' OR
    w.heartbeat_at + w.ttl_seconds * interval '1 second' <= clock_timestamp() OR
    NOT ('flow:*'=ANY(w.capabilities) OR 'flow:task'=ANY(w.capabilities)) THEN
    RETURN; END IF;
 -- Exhausted stale leases terminate their runs, but never alter a succeeded task.
 FOR taskrow IN SELECT t.run_id,t.task_key,t.attempts FROM hfsm.flow_task t
    WHERE (p_run IS NULL OR t.run_id=p_run) AND t.status='leased' AND t.lease_until<=clock_timestamp()
      AND t.attempts>=t.max_attempts
    ORDER BY t.lease_until LIMIT 100 FOR UPDATE OF t SKIP LOCKED LOOP
   UPDATE hfsm.flow_task SET status='failed',lease_id=NULL,lease_owner=NULL,
     lease_until=NULL,last_error='lease expired at retry budget',finished_at=clock_timestamp()
   WHERE run_id=taskrow.run_id AND task_key=taskrow.task_key;
   UPDATE hfsm.flow_task_attempt SET outcome='expired',finished_at=clock_timestamp(),
     error_text='final lease expired' WHERE run_id=taskrow.run_id AND task_key=taskrow.task_key
       AND outcome='leased';
   INSERT INTO hfsm.flow_dead_letter(run_id,task_key,attempts,reason)
    VALUES(taskrow.run_id,taskrow.task_key,taskrow.attempts,'lease expired at retry budget')
    ON CONFLICT DO NOTHING;
   UPDATE hfsm.flow_run SET status='failed',finished_at=clock_timestamp(),updated_at=clock_timestamp()
    WHERE id=taskrow.run_id AND status='running';
   UPDATE hfsm.flow_task_attempt SET outcome='cancelled',finished_at=clock_timestamp(),
     error_text='run failed after exhausted lease'
     WHERE run_id=taskrow.run_id AND outcome='leased';
   UPDATE hfsm.flow_task SET status='cancelled',lease_id=NULL,lease_owner=NULL,lease_until=NULL,
     finished_at=clock_timestamp() WHERE run_id=taskrow.run_id
       AND status IN('ready','blocked','leased');
   INSERT INTO hfsm.flow_completion(run_id) SELECT taskrow.run_id
    WHERE EXISTS(SELECT 1 FROM hfsm.flow_run WHERE id=taskrow.run_id AND source_instance_id IS NOT NULL)
    ON CONFLICT DO NOTHING;
 END LOOP;
 FOR k IN 1..LEAST(100,GREATEST(1,p_batch)) LOOP
   SELECT coalesce(sum(t.cpu_units),0),coalesce(sum(t.memory_mb),0),count(*)
    INTO busy_cpu,busy_mem,busy_count FROM hfsm.flow_task t
    WHERE t.lease_owner=p_worker AND t.status='leased' AND t.lease_until>clock_timestamp();
   SELECT t.run_id,t.task_key,t.executor,t.attempts,t.timeout_seconds,t.output_ports,
      t.cpu_units,t.memory_mb,t.lease_id,t.priority,r.tenant_id,r.input_value,
      r.source_effect_key,r.correlation_id,r.traceparent
     INTO taskrow
     FROM hfsm.flow_task t
     JOIN hfsm.flow_run r ON r.id=t.run_id AND r.status='running'
     JOIN hfsm.tenant ten ON ten.id=r.tenant_id AND ten.enabled
     JOIN hfsm.flow_definition d ON d.id=r.definition_id
     LEFT JOIN hfsm.instance parent ON parent.id=r.source_instance_id
     WHERE (p_run IS NULL OR t.run_id=p_run) AND t.status IN('ready','leased')
      AND (t.status='ready' AND t.available_at<=clock_timestamp()
           OR t.status='leased' AND t.lease_until<=clock_timestamp())
      AND t.attempts<t.max_attempts AND t.received_inputs=t.required_inputs
      AND (r.source_instance_id IS NULL OR parent.status NOT IN('done','failed','cancelled'))
      AND (t.required_caps <@ w.capabilities OR 'flow:*'=ANY(w.capabilities))
      AND (('flow:execute:'||t.executor)=ANY(w.capabilities) OR 'flow:*'=ANY(w.capabilities))
      AND busy_count<res.max_tasks AND busy_cpu+t.cpu_units<=res.cpu_units
      AND busy_mem+t.memory_mb<=res.memory_mb
      AND (SELECT count(*) FROM hfsm.flow_task x JOIN hfsm.flow_run rr ON rr.id=x.run_id
         WHERE rr.tenant_id=r.tenant_id AND x.status='leased' AND x.lease_until>clock_timestamp())
          < ten.max_flow_inflight
      AND (SELECT count(*) FROM hfsm.flow_task x JOIN hfsm.flow_run rr ON rr.id=x.run_id
         LEFT JOIN hfsm.instance src ON src.id=rr.source_instance_id
         WHERE rr.tenant_id=r.tenant_id AND coalesce(src.concurrency_group,'default')=coalesce(parent.concurrency_group,'default')
         AND x.status='leased' AND x.lease_until>clock_timestamp())
          < coalesce((SELECT max_flow_tasks FROM hfsm.concurrency_group
             WHERE tenant_id=r.tenant_id AND group_key=coalesce(parent.concurrency_group,'default')),100)
     ORDER BY (t.priority+LEAST(40,GREATEST(0,
                floor(EXTRACT(epoch FROM (clock_timestamp()-t.available_at))/60)::integer))) DESC,
       r.created_at,t.task_key
     LIMIT 1 FOR UPDATE OF t SKIP LOCKED;
   EXIT WHEN NOT FOUND;
   IF taskrow.lease_id IS NOT NULL THEN
      UPDATE hfsm.flow_task_attempt SET outcome='expired',finished_at=clock_timestamp(),
        error_text='lease reclaimed by another worker'
        WHERE run_id=taskrow.run_id AND task_key=taskrow.task_key
          AND lease_id=taskrow.lease_id AND outcome='leased';
   END IF;
   tid:=gen_random_uuid();
   until_:=clock_timestamp()+LEAST(3600,GREATEST(1,p_seconds))*interval '1 second';
   UPDATE hfsm.flow_task SET status='leased',lease_id=tid,lease_owner=p_worker,
      lease_until=until_,attempts=attempts+1
      WHERE run_id=taskrow.run_id AND task_key=taskrow.task_key;
   INSERT INTO hfsm.flow_task_attempt(run_id,task_key,attempt_no,lease_id,worker_id,outcome)
    VALUES(taskrow.run_id,taskrow.task_key,taskrow.attempts+1,tid,p_worker,'leased');
   INSERT INTO hfsm.flow_event(run_id,task_key,kind,detail)
    VALUES(taskrow.run_id,taskrow.task_key,'task_claimed',
      jsonb_build_object('attempt',taskrow.attempts+1,'worker',p_worker));
   RETURN NEXT jsonb_build_object(
     'runId',taskrow.run_id,'tenantId',taskrow.tenant_id,'taskKey',taskrow.task_key,
     'executor',taskrow.executor,'attempt',taskrow.attempts+1,
     'leaseId',tid,'workerId',p_worker,'leaseUntil',until_,
     'timeoutSeconds',taskrow.timeout_seconds,
     'effectKey','hfsm/flow/'||taskrow.run_id::text||'/'||taskrow.task_key,
     'runInput',taskrow.input_value,'correlationId',taskrow.correlation_id,
     'traceparent',taskrow.traceparent,
     'outputs',taskrow.output_ports,
     'inputs',coalesce((SELECT jsonb_agg(jsonb_build_object(
       'port',tok.to_port,'artifact',jsonb_build_object(
        'artifactId',a.id,'digest',a.digest,'sizeBytes',a.size_bytes,
        'storageUri',a.storage_uri,'mediaType',a.media_type,'metadata',a.metadata)) ORDER BY tok.to_port)
       FROM hfsm.flow_token tok JOIN hfsm.flow_artifact a ON a.id=tok.artifact_id
       WHERE tok.run_id=taskrow.run_id AND tok.to_task=taskrow.task_key),'[]'::jsonb));
 END LOOP;
END $$;

CREATE FUNCTION hfsm.flow_renew_task(p_run uuid,p_key text,p_token uuid,p_worker uuid,p_seconds integer)
 RETURNS timestamptz LANGUAGE plpgsql AS $$
DECLARE until_ timestamptz;
BEGIN
 UPDATE hfsm.flow_task SET lease_until=GREATEST(lease_until,
    clock_timestamp()+LEAST(3600,GREATEST(1,p_seconds))*interval '1 second')
 WHERE run_id=p_run AND task_key=p_key AND status='leased'
  AND lease_id=p_token AND lease_owner=p_worker AND lease_until>clock_timestamp()
 RETURNING lease_until INTO until_;
 RETURN until_;
END $$;

-- A fenced, all-or-nothing commit of output manifests + tokens + readiness.
CREATE FUNCTION hfsm.flow_finish_task(p_run uuid,p_key text,p_token uuid,p_worker uuid,
 p_attempt integer,p_outputs jsonb) RETURNS boolean LANGUAGE plpgsql AS $$
DECLARE t hfsm.flow_task%ROWTYPE; r hfsm.flow_run%ROWTYPE;
 port_name text; aid uuid; expected integer; actual integer; e record;
BEGIN
 PERFORM 1 FROM hfsm.admission_gate WHERE id=1 FOR UPDATE;
 SELECT * INTO r FROM hfsm.flow_run WHERE id=p_run FOR UPDATE;
 SELECT * INTO t FROM hfsm.flow_task WHERE run_id=p_run AND task_key=p_key FOR UPDATE;
 IF t.run_id IS NULL OR r.id IS NULL OR t.status<>'leased' OR r.status<>'running' OR
    t.lease_id IS DISTINCT FROM p_token OR t.lease_owner IS DISTINCT FROM p_worker OR
    t.attempts<>p_attempt OR t.lease_until<=clock_timestamp() THEN RETURN false; END IF;
 IF jsonb_typeof(p_outputs)<>'object' THEN RETURN false; END IF;
 expected:=jsonb_array_length(t.output_ports);
 SELECT count(*) INTO actual FROM jsonb_object_keys(p_outputs);
 IF actual<>expected THEN RETURN false; END IF;
 FOR port_name IN SELECT port FROM jsonb_array_elements_text(t.output_ports) AS port_array(port) LOOP
   IF NOT p_outputs ? port_name OR jsonb_typeof(p_outputs->port_name)<>'string' THEN RETURN false; END IF;
   BEGIN aid:=(p_outputs->>port_name)::uuid;
   EXCEPTION WHEN invalid_text_representation THEN RETURN false; END;
   IF NOT EXISTS (SELECT 1 FROM hfsm.flow_artifact WHERE id=aid AND tenant_id=r.tenant_id)
     THEN RETURN false; END IF;
 END LOOP;
 -- Validate the ENTIRE output map before writing any row. Returning false
 -- after a partial INSERT would otherwise commit incomplete output evidence.
 FOR port_name IN SELECT port FROM jsonb_array_elements_text(t.output_ports) AS port_array(port) LOOP
   aid:=(p_outputs->>port_name)::uuid;
   INSERT INTO hfsm.flow_task_output(run_id,task_key,port,artifact_id)
    VALUES(p_run,p_key,port_name,aid);
 END LOOP;
 FOR e IN SELECT x.to_task,x.to_port,x.from_port,o.artifact_id
    FROM hfsm.flow_edge x JOIN hfsm.flow_task_output o
      ON o.run_id=x.run_id AND o.task_key=x.from_task AND o.port=x.from_port
    WHERE x.run_id=p_run AND x.from_task=p_key LOOP
   INSERT INTO hfsm.flow_token(run_id,from_task,from_port,to_task,to_port,artifact_id)
    VALUES(p_run,p_key,e.from_port,e.to_task,e.to_port,e.artifact_id);
   UPDATE hfsm.flow_task SET received_inputs=received_inputs+1,
     status=CASE WHEN received_inputs+1=required_inputs THEN 'ready' ELSE status END
    WHERE run_id=p_run AND task_key=e.to_task AND status IN('ready','blocked');
 END LOOP;
 UPDATE hfsm.flow_task SET status='done',lease_id=NULL,lease_owner=NULL,lease_until=NULL,
    finished_at=clock_timestamp() WHERE run_id=p_run AND task_key=p_key;
 UPDATE hfsm.flow_task_attempt SET outcome='succeeded',finished_at=clock_timestamp()
  WHERE run_id=p_run AND task_key=p_key AND attempt_no=p_attempt AND lease_id=p_token
    AND outcome='leased';
 INSERT INTO hfsm.flow_event(run_id,task_key,kind,detail) VALUES
    (p_run,p_key,'task_succeeded',jsonb_build_object('outputs',p_outputs));
 IF NOT EXISTS(SELECT 1 FROM hfsm.flow_task WHERE run_id=p_run AND status<>'done') THEN
   UPDATE hfsm.flow_run SET status='succeeded',finished_at=clock_timestamp(),updated_at=clock_timestamp()
    WHERE id=p_run AND status='running';
   INSERT INTO hfsm.flow_event(run_id,kind,detail) VALUES(p_run,'run_succeeded','{}'::jsonb);
   IF r.source_instance_id IS NOT NULL THEN
     INSERT INTO hfsm.flow_completion(run_id) VALUES(p_run) ON CONFLICT DO NOTHING;
   END IF;
 END IF;
 RETURN true;
END $$;

CREATE FUNCTION hfsm.flow_fail_task(p_run uuid,p_key text,p_token uuid,p_worker uuid,
 p_attempt integer,p_reason text,p_retryable boolean) RETURNS boolean LANGUAGE plpgsql AS $$
DECLARE t hfsm.flow_task%ROWTYPE; r hfsm.flow_run%ROWTYPE; delay_seconds integer;
BEGIN
 PERFORM 1 FROM hfsm.admission_gate WHERE id=1 FOR UPDATE;
 SELECT * INTO r FROM hfsm.flow_run WHERE id=p_run FOR UPDATE;
 SELECT * INTO t FROM hfsm.flow_task WHERE run_id=p_run AND task_key=p_key FOR UPDATE;
 IF t.run_id IS NULL OR r.id IS NULL OR t.status<>'leased' OR r.status<>'running' OR t.lease_id IS DISTINCT FROM p_token OR
   t.lease_owner IS DISTINCT FROM p_worker OR t.attempts<>p_attempt OR
   t.lease_until<=clock_timestamp() THEN RETURN false; END IF;
 UPDATE hfsm.flow_task_attempt SET outcome='failed',finished_at=clock_timestamp(),error_text=p_reason
  WHERE run_id=p_run AND task_key=p_key AND attempt_no=p_attempt AND lease_id=p_token
   AND outcome='leased';
 IF p_retryable AND t.attempts<t.max_attempts THEN
   delay_seconds:=LEAST(60, (power(2,LEAST(6,t.attempts-1)))::integer);
   UPDATE hfsm.flow_task SET status='ready',lease_id=NULL,lease_owner=NULL,lease_until=NULL,
      available_at=clock_timestamp()+delay_seconds*interval '1 second',last_error=p_reason
    WHERE run_id=p_run AND task_key=p_key;
   INSERT INTO hfsm.flow_event(run_id,task_key,kind,detail)
     VALUES(p_run,p_key,'task_retry',jsonb_build_object('attempt',p_attempt,'delaySeconds',delay_seconds));
 ELSE
   UPDATE hfsm.flow_task SET status='failed',lease_id=NULL,lease_owner=NULL,lease_until=NULL,
      finished_at=clock_timestamp(),last_error=p_reason
    WHERE run_id=p_run AND task_key=p_key;
   INSERT INTO hfsm.flow_dead_letter(run_id,task_key,attempts,reason)
     VALUES(p_run,p_key,p_attempt,p_reason) ON CONFLICT DO NOTHING;
   UPDATE hfsm.flow_run SET status='failed',finished_at=clock_timestamp(),updated_at=clock_timestamp()
    WHERE id=p_run AND status='running';
   UPDATE hfsm.flow_task_attempt SET outcome='cancelled',finished_at=clock_timestamp(),
     error_text='sibling task terminated run' WHERE run_id=p_run AND outcome='leased';
   UPDATE hfsm.flow_task SET status='cancelled',lease_id=NULL,lease_owner=NULL,lease_until=NULL,
    finished_at=clock_timestamp() WHERE run_id=p_run AND status IN('blocked','ready','leased');
   INSERT INTO hfsm.flow_event(run_id,task_key,kind,detail)
     VALUES(p_run,p_key,'task_failed',jsonb_build_object('attempt',p_attempt,'reason',p_reason));
   IF r.source_instance_id IS NOT NULL THEN
    INSERT INTO hfsm.flow_completion(run_id) VALUES(p_run) ON CONFLICT DO NOTHING;
   END IF;
 END IF;
 RETURN true;
END $$;

-- Delivery is a separate transaction from task completion. CT-06 enqueue
-- assigns a unique instance-local ordinal under the instance lock, with a
-- stable dedupe key, so a lost acknowledgement cannot duplicate a signal.
CREATE FUNCTION hfsm.flow_deliver_completion(p_run uuid,p_signal uuid)
 RETURNS uuid LANGUAGE plpgsql AS $$
DECLARE c hfsm.flow_completion%ROWTYPE; r hfsm.flow_run%ROWTYPE;
  payload jsonb; delivered uuid;
BEGIN
 SELECT * INTO c FROM hfsm.flow_completion WHERE run_id=p_run FOR UPDATE;
 IF NOT FOUND THEN RETURN NULL; END IF;
 IF c.status='delivered' THEN RETURN c.signal_id; END IF;
 SELECT * INTO r FROM hfsm.flow_run WHERE id=p_run;
 payload:=CASE r.status WHEN 'succeeded' THEN r.success_signal
                       WHEN 'failed' THEN r.failure_signal ELSE NULL END;
 IF payload IS NULL OR r.source_instance_id IS NULL THEN RETURN NULL; END IF;
 delivered := hfsm.ops_enqueue_signal(
    r.source_instance_id
    , p_signal
    , payload
    , jsonb_build_object('kind','flow_completion','flowRunId',r.id,
          'flowStatus',r.status,'definitionDigest',r.definition_digest
        )
    , 'hfsm/flow/completion/'||r.id::text
    , 0::smallint
    , r.correlation_id
    , r.id
    , r.traceparent
    , r.source_release
    , clock_timestamp());
 UPDATE hfsm.flow_completion SET status='delivered',signal_id=delivered,
   delivered_at=clock_timestamp() WHERE run_id=p_run;
 INSERT INTO hfsm.flow_event(run_id,kind,detail)
   VALUES(p_run,'completion_delivered',jsonb_build_object('signalId',delivered));
 RETURN delivered;
END $$;


CREATE FUNCTION hfsm.flow_cancel_run(p_run uuid,p_reason text)
 RETURNS boolean LANGUAGE plpgsql AS $$
DECLARE r hfsm.flow_run%ROWTYPE;
BEGIN
 PERFORM 1 FROM hfsm.admission_gate WHERE id=1 FOR UPDATE;
 SELECT * INTO r FROM hfsm.flow_run WHERE id=p_run FOR UPDATE;
 IF NOT FOUND OR r.status<>'running' THEN RETURN false; END IF;
 UPDATE hfsm.flow_run SET status='cancelled',finished_at=clock_timestamp(),updated_at=clock_timestamp()
   WHERE id=p_run;
 UPDATE hfsm.flow_task_attempt SET outcome='cancelled',finished_at=clock_timestamp(),
   error_text=p_reason WHERE run_id=p_run AND outcome='leased';
 UPDATE hfsm.flow_task SET status='cancelled',lease_id=NULL,lease_owner=NULL,lease_until=NULL,
   finished_at=clock_timestamp(),last_error=p_reason
   WHERE run_id=p_run AND status IN('blocked','ready','leased');
 INSERT INTO hfsm.flow_event(run_id,kind,detail)
  VALUES(p_run,'run_cancelled',jsonb_build_object('reason',p_reason));
 RETURN true;
END $$;

CREATE FUNCTION hfsm.flow_run_summary(p_run uuid) RETURNS jsonb LANGUAGE sql STABLE AS $$
 SELECT jsonb_build_object('runId',r.id,'tenantId',r.tenant_id,
     'definitionId',r.definition_id,'status',r.status,
     'sourceInstance',r.source_instance_id,'startedAt',r.created_at,
     'taskStatuses',coalesce((SELECT jsonb_object_agg(t.task_key,t.status)
                              FROM hfsm.flow_task t WHERE t.run_id=r.id),'{}'::jsonb),
     'completionStatus',(SELECT c.status FROM hfsm.flow_completion c WHERE c.run_id=r.id))
 FROM hfsm.flow_run r WHERE r.id=p_run;
$$;

COMMENT ON TABLE hfsm.flow_artifact IS
 'Immutable content-addressed manifest; remote bytes must be checksum verified by publisher before registration';
COMMENT ON TABLE hfsm.flow_task IS
 'Finite DF-01 task instance. Exactly one fenced winner per leased attempt';
COMMENT ON TABLE hfsm.flow_token IS
 'Exactly-once materialized input token for a statically declared DAG edge';
COMMENT ON TABLE hfsm.flow_completion IS
 'Durable HFSM boundary: completion signal may be enqueued after run terminal transition';

-- Provision narrow runtime / operator privileges explicitly. These functions
-- assume trusted tenant-aware service authentication; never expose publicly.
REVOKE ALL ON FUNCTION hfsm.flow_register_definition(uuid,uuid,text,text,jsonb) FROM PUBLIC;
REVOKE ALL ON FUNCTION hfsm.flow_start_run(uuid,uuid,text,uuid,text,jsonb,jsonb,jsonb) FROM PUBLIC;
REVOKE ALL ON FUNCTION hfsm.flow_publish_artifact(uuid,uuid,text,bigint,text,text,jsonb) FROM PUBLIC;
REVOKE ALL ON FUNCTION hfsm.flow_claim_tasks(uuid,integer,integer,uuid) FROM PUBLIC;
REVOKE ALL ON FUNCTION hfsm.flow_renew_task(uuid,text,uuid,uuid,integer) FROM PUBLIC;
REVOKE ALL ON FUNCTION hfsm.flow_finish_task(uuid,text,uuid,uuid,integer,jsonb) FROM PUBLIC;
REVOKE ALL ON FUNCTION hfsm.flow_fail_task(uuid,text,uuid,uuid,integer,text,boolean) FROM PUBLIC;
REVOKE ALL ON FUNCTION hfsm.flow_deliver_completion(uuid,uuid) FROM PUBLIC;
REVOKE ALL ON FUNCTION hfsm.flow_cancel_run(uuid,text) FROM PUBLIC;
REVOKE ALL ON FUNCTION hfsm.flow_register_worker(uuid,integer,integer,integer) FROM PUBLIC;
