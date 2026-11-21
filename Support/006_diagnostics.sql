-- DF-02 optional read-only diagnostics. Set the run_id in psql before calling:
--   \set flow_run_id 'xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx'
-- No mutations. All table aliases are descriptive.
SELECT scatter_group.id AS group_id, scatter_group.source_task,
       scatter_group.root_task, scatter_group.partition_count,
       scatter_group.fan_in, scatter_group.plan_digest,
       flow_run.status AS run_status
FROM hfsm.flow_scatter_group AS scatter_group
JOIN hfsm.flow_run AS flow_run ON flow_run.id=scatter_group.run_id
WHERE scatter_group.run_id=:'flow_run_id'::uuid;

SELECT flow_task.status,flow_task.executor,count(*) AS task_count
FROM hfsm.flow_task AS flow_task
WHERE flow_task.run_id=:'flow_run_id'::uuid
GROUP BY flow_task.status,flow_task.executor
ORDER BY flow_task.executor,flow_task.status;

SELECT partition_binding.partition_ordinal,partition_binding.partition_key,
       partition_binding.task_key,partition_binding.artifact_id
FROM hfsm.flow_partition_binding AS partition_binding
WHERE partition_binding.run_id=:'flow_run_id'::uuid
ORDER BY partition_binding.partition_ordinal;

SELECT reduction_task.task_key,reduction_task.required_inputs,
       reduction_task.received_inputs,reduction_task.status,
       reduction_task.output_ports
FROM hfsm.flow_task AS reduction_task
WHERE reduction_task.run_id=:'flow_run_id'::uuid
  AND reduction_task.task_key LIKE 'df2:%:r:%'
ORDER BY reduction_task.task_key;

SELECT flow_event.id,flow_event.task_key,flow_event.kind,flow_event.detail
FROM hfsm.flow_event AS flow_event
WHERE flow_event.run_id=:'flow_run_id'::uuid
ORDER BY flow_event.id;
