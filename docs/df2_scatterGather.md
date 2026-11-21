# DF-02 — Bounded Dynamic Scatter/Gather and Partitioned Aggregation

Source basis: `whatsupfudd/ai_hfsm`, current `main` tree `47929400114f736178c2eb33abc71b5c49ebb846` (2026-11-21). This is an additive, conservative extension of the DF-01 runtime, not a replacement for its scheduler.

## Deliverables and integration

Copy `src/` into the repository's `src/` directory; only `src/Commands/Test.hs` replaces an existing module. All `src/Hfsm/Flow/Dynamic/...` and `src/Demo/FlowDynamic...` modules are new. Apply `Support/006_hfsm_dynamic_scatter_aggregation.sql` after `001`–`005` in a single transaction. No source-editing/patch scripts are included.

```
psql -v ON_ERROR_STOP=1 -1 -f Support/006_hfsm_dynamic_scatter_aggregation.sql
stack build
stack exec -- thfsm test flowDynamic
stack exec -- thfsm test flowDynamicDb --db
stack exec -- thfsm test flowDb --db
stack exec -- thfsm test operationsDb --db
stack exec -- thfsm test activityDb --db
stack exec -- thfsm test historyDb --db
```

`flowDynamicDb` must run in a dedicated test database with migrations 001–006 and no production workers. It uses run-scoped task claiming. **Compilation and PostgreSQL execution were not available in the authoring environment**; all test results must be verified by the implementing team.

## Deliberate boundary and constraints

- **HFSM** remains the authoritative orchestration host. Starting a flow and delivering its completion still use DF-01's `flow_start_run`, `flow_completion` and CT-06 deduplicated enqueue. The database uses a stable `hfsm/flow/completion/<run_id>` key.
- **DF-01** remains authoritative for task claims, tenant quotas, CPU/memory reservations, worker capability/heartbeat checks, task fencing, retry budgets, artifact outputs, tokens, readiness, dead letters and run success/failure.
- **DF-02** introduces a **one-shot bounded graph expansion** by a leased, statically registered `df2.scatter` **sink**. Its source must declare no output ports and no outgoing static edges. It creates immutable partition manifests, dynamically generated finite task rows, and an ordered reduction tree *before* completing the source under the same admission lock and SQL transaction.
- Exactly **one** dynamic group per flow run in DF-02, **0–512** partitions per group, reducer fan-in **2–64**, maximum **4,096 tasks per run**, no recursion, no graph updates after materialization, no streaming or aggregation windows. Separate runs may have different manifests.
- The pure generator produces stable task names: `df2:<groupKey>:p:000001` for leaves, `df2:<groupKey>:r:<level>:000001` for reducers. Their input ports are `i000001`, `i000002`, … assigned by manifest order. A root reducer emits `result`; intermediate reducers and leaves emit `value`.
- A partition may read **one externally published immutable CAS artifact** as the special `item` input. The binding is stored in an append-only table (`flow_partition_binding`); this is intentional **not** an invented `flow_token`, because there is no dataflow producer edge for that pre-published artifact.
- Every reducer input is an ordinary DF-01 token created atomically with its producer's `flow_finish_task`. Reducers wait for all tokens. Task completion order cannot reorder reducer ports.

## Defining the static anchor

Register a normal `Hfsm.Flow.Spec.FlowSpec` containing a task with:

```
TaskDef
  { key="scatter", executor="df2.scatter"
  , inputs=[], outputs=[]
  , cpu=1, memoryMb=64, capabilities=[]
  , maxAttempts=3, timeoutSeconds=60, priority=0 }
```

The scatter task can also have upstream static input dependencies, but must remain a sink. Any attempt to complete it through unextended `flow_finish_task` without expansion is rejected by migration 006's guard trigger.

## Computing a manifest

The application registers:

```
newScatterProvider runner (\sourceLease -> do
  -- Read/verify/publish each partition artifact in durable CAS first.
  -- Return the same immutable plan for the same source lease/effect key.
  pure (Right scatterPlan))
```

`ScatterPlan` contains `groupKey`, `partitions :: [PartitionDef]`, `fanIn`, `partitionExecutor`, `aggregateExecutor`, and `partitionProfile` / `aggregateProfile`. Each `PartitionDef` has a stable `key` and an `artifactId` referencing an already published, tenant-owned DF-01 CAS manifest.

Use **`Hfsm.Flow.Dynamic.Runtime.runManagedDynamicWorker`**, not the DF-01 managed worker helper. It wraps CT-06 registration/resources and passes `Hfsm.Flow.Dynamic.Postgres.dynamicFlowStore` to the existing DF-01 worker. Provide all three providers:

- `newScatterProvider runner <manifest builder>`
- `newPartitionProvider runner "df2.partition" <partition executor>`
- `newOrderedReducerProvider "df2.reduce" <deterministic ordered reducer>`

Register a worker with `flow:task`, `flow:execute:df2.scatter`, `flow:execute:df2.partition`, and `flow:execute:df2.reduce` (or a carefully authorized `flow:*` wildcard), plus any task-specific capabilities. The `FlowWorkerCfg.workerId` and `WorkerResources.workerId` must match its CT-06 registration.

The Haskell reducer receives artifact references in **ascending input-port order**. Its callback chooses deterministic artifact bytes and publishes the resulting manifest to the DF-01 CAS table **before returning the artifact UUID**. For zero input partitions the callback must explicitly implement and publish the reduction identity. Ordering alone does not guarantee algebraic associativity: use a well-defined ordered fold or a deterministic associative operator whose output is independent of the chosen fan-in. Exact global results may otherwise change when the tree shape changes; the immutable plan records the chosen fan-in and partition order.

## Atomicity and idempotence

`flow_expand_scatter(run,source,fence,worker,attempt,newGroupUuid,planJson)` holds the existing CT-06 `admission_gate` lock, checks the live source lease, confirms all referenced CAS manifests have matching tenant IDs, validates the bounded graph and policies, inserts partition/reduction tasks and edges, appends the immutable group and binding records, and calls DF-01 `flow_finish_task` within the **same transaction**. It raises an exception on any failed final settlement; **no partial graph may commit**.

A lost client ACK can repeat the expansion: equal `(run,source,plan)` returns the original group UUID without modifying history. A different plan for the same run is rejected. The DF-02 FlowStore's `finish` checks `flow_scatter_ack` to recognize a second finish attempt from **the exact same fenced scatter owner** and returns success without adding a duplicate step/event.

If a worker crashes **before commit**, the transaction rolls back; after lease expiry a new worker can re-execute the scatter. If it crashes **after commit**, the source is already `done` and only its generated tasks are eligible for execution. If a lease expires *during* the expansion transaction, the final DF-01 fence check rejects the commit and rolls back the inserted graph. Because this check uses database wall clock, ensure the scatter lease covers the maximum single-transaction expansion time; renewals can block on the source row while expansion holds its lock.

Task output artifacts and tokens are guarded by DF-01's immutable tables and CAS tenant checks. Worker crashes after publishing bytes but before settling may leave orphan CAS objects; storage-level garbage collection/reconciliation remains the artifact provider's responsibility. Business side effects **must not** run inside retryable partition or reducer callbacks; use HFSM/CT-04 for those.

## Migration compatibility and SQL readability

The migration is **forward-only**. No DF-01 SQL function is replaced, so existing 005 tests and application contracts are preserved. New PL/pgSQL `DECLARE` identifiers are descriptive (at least two characters), never single letters, and distinct from all SQL range-table aliases; this is audited in packaging. Internal SQL functions remain `SECURITY INVOKER` and should be reachable only through trusted application database roles. Tenant/user authorization is the service layer's responsibility; don't expose raw function execution to arbitrary client roles.

## Deferred from DF-02

Nested or recursively produced scatters, dynamic re-partitioning, multi-group DAG rewriting, distributed streaming, work stealing across reducer levels, materialized input byte verification, partial/approximate aggregates, cross-run reduction barriers, and partition-specific retry policies. Keep this finite version stable before allowing them.

See `docs/DF02_ACCEPTANCE_MATRIX.md` for the automated and external fault cases.
