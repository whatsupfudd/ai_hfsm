# ai_hfsm DF-01 — Finite Bounded DAG Dataflow (2026-11-21)

Source baseline: `https://github.com/whatsupfudd/ai_hfsm`, `main` Git tree
`a0d27ef8f9123e4a0bf68d12d28588fb8af3a1e2` when inspected.

This is the deliberately narrow **first dataflow milestone** after CT-06.
It adds a task/token/artifact subsystem beside the existing HFSM runtime;
**it does not replace `planStep`, `commitTurn`, history/replay, or CT-04 outbox**.

## Package and installation

- **10 complete Haskell modules**: nine new modules and one replacement,
  `src/Commands/Test.hs`. No patch scripts, textual replacements, or generated
  stale core modules.
- New migration `Support/005_hfsm_finite_flow.sql` applied **once** after
  `001_hfsm_runtime.sql`, `002_hfsm_activity.sql`,
  `003_hfsm_history_migration.sql`, `004_hfsm_operations.sql`.
- No additional Haskell dependencies beyond current `package.yaml`.
- In your test database, apply migration 005 **before** running `stack build`:
  Hasql-TH validates prepared SQL against the live schema at compile time.
- Copy the supplied `src/.` into the checkout. Only `Commands.Test.hs` overwrites
  a currently tracked module. Prefer copying rather than in-place patch logic.
- Tests use a dedicated PostgreSQL DB. Avoid global production flow workers
  during testing. `Flow.flowStoreForRun` scopes regression claims to a single run;
  production must use `Flow.flowStore` without a filter.

```sh
# In migration-managed test database (example: direct psql)
psql -v ON_ERROR_STOP=1 -1 -f Support/005_hfsm_finite_flow.sql
stack build
stack exec -- thfsm test flow
stack exec -- thfsm test flowDb --db
# CT-03..06 existing regressions should continue to pass
stack exec -- thfsm test distributedDb --db
stack exec -- thfsm test activityDb --db
stack exec -- thfsm test historyDb --db
stack exec -- thfsm test operationsDb --db
```

**Build/test status:** Source modules and SQL were statically checked and the
archive checksum/CRC verified. The artifact environment has no GHC, Stack,
psql, or running PostgreSQL service. Compilation, Hasql-TH typechecking and
SQL execution remain tasks for your CI/test environment; do not interpret
static checks as a production-ready release.

## 1. Execution boundary

The HFSM remains the **long-lived orchestration and authority**. It decides
whether a computation is needed and emits an ordinary CT-04 outbox command.
The flow subsystem executes a **finite, statically defined, deterministic
DAG**, then enqueues one typed completion signal back to the source HFSM.

```text
HFSM revision n                    DATAFLOW RUN                 HFSM revision n+1
  transition                          (finite DAG)             after completion
      |                                    |                           ^
      +---- CT04 outbox effect ------------+                           |
                  effect_key        flow_start_run                     |
                          |               |                           |
                          |      static tasks & port edges            |
                          |               |                           |
                          |      artifact manifests + tokens         |
                          |               |                           |
                          |      run succeeds / fails                |
                          |               |                           |
                          +------ flow_completion --------------------+
                                   CT06 ops_enqueue_signal
                                   dedupe_key: hfsm/flow/completion/<runId>
```

`Hfsm.Flow.Provider.Start.newFlowStartProvider` connects this to an existing
CT-04 provider dispatch. Its `effectKind` parameter should be `"cmd"` for the
current stock planner's command outbox, where the application dispatcher owns
only flow-start command messages, or a distinct kind if the application
already produces a distinct flow-start outbox intent. A production application
with multiple `cmd` operations needs **one application-level command router**;
registering two providers with the same `kind` does not dispatch correctly.

A `StartRun` linked to an HFSM source requires:
1. A **previously committed** outbox row with the same `effectKey` and source
   instance UUID; source identity and tenant are checked in the DB.
2. Exact source machine release (`name@version#specDigest`) pinned at start.
3. Both application-codec-encoded success/failure signal payloads. SQL cannot
   infer the application signal type and must never perform generic casts.
4. An immutable definition and a unique source effect key. Retries return the
   original run UUID; no second task graph is created.

On terminal flow result, `hfsm.flow_completion` becomes `ready` in the SAME
transaction as the final task commit. `Hfsm.Flow.Worker.runCompletionTick`
calls the CT-06 idempotent enqueue primitive and marks delivery in another
atomic transaction. Losing the acknowledgement cannot create another signal;
the stable key is unique per HFSM instance and signal. An explicit source
release mismatch after HFSM migration is rejected rather than silently
sending a legacy signal into a different decoder. Such cases require operator
reconciliation or an explicit version-compatible translation.

## 2. Static graph/task/token semantics

`Hfsm.Flow.Spec` defines `FlowSpec`, `TaskDef`, and `EdgeDef` and performs
pure DAG validation; registration repeats the essential checks in PostgreSQL:

- Between **1 and 256** tasks, at most **4,096** edges, at most 64 declared
  inputs and 64 outputs per task.
- Every task has a unique key and stable executor kind, bounded CPU/memory,
  retry/timeout policy, priority and optional worker capabilities.
- Each declared input port has **exactly one** incoming edge. Each edge
  connects a declared source output port to a declared target input port.
- Self-edges and directed cycles are forbidden; deterministic topological
  order is computed using Kahn's algorithm.
- Each task exists once at run creation; no task creation after start.
- The validated and database-pinned definition can be reused by many runs;
  the run records its SHA-256 digest and immutable definition ID.
- Root tasks (zero inputs) begin `ready`; others begin `blocked`.
- One successful output binds one immutable artifact ID per declared output
  port. A single output may fan out to multiple edges.
- **One immutable token per (run, destination task, input port)** is created
  atomically with successful task completion. A task becomes `ready` only when
  `received_inputs == required_inputs`.

Task status:

```text
blocked --(all input tokens)--> ready --(fenced claim)--> leased
                                   ^                         |
                                   |---- (bounded retry) ------+
                                                             |
                                         +--- completed ------> done
                                         +--- final error ----> failed
                                         +--- run cancel -----> cancelled
```

Attempt leases use a new opaque UUID on every claim/reclaim, an absolute server
expiry, a worker UUID and monotonically increasing per-task attempt ordinal.
A stale worker's completion is rejected. Each successful task's output
bindings, produced input tokens, newly ready consumers, attempt settlement,
and final-run transition are **one SQL transaction**.

## 3. Artifact boundary

`Hfsm.Flow.Types.PublishArtifact` is an immutable CAS manifest containing:
`tenantId`, `artifactId`, SHA-256 lowercase hex digest, byte size, storage URI,
media type and JSON metadata. The payload bytes are NOT stored in PostgreSQL.

The object-store adapter must first write content to durable storage and
verify its digest and length. Only then should it call
`Hfsm.Flow.Postgres.publishArtifact`. The database checks digest syntax,
nonnegative size and tenant identity. Concurrent registrations of the same
`(tenant,digest)` return the canonical existing artifact UUID; inconsistent
size is an error. **The SQL database does not read or verify remote bytes**.

Outputs are UUID references to previously published manifests. SQL verifies
that each is in the same tenant and that the map has precisely the declared
output port set before inserting ANY output rows. Outputs and emitted tokens
cannot later be changed or deleted. A crash after storing bytes but before
SQL publication may leave a CAS orphan; object-store GC is a later concern.

The DF-01 regression uses `cas://df01-fixture/...` metadata-only fixtures.
These validate relational semantics, not an actual S3/MinIO upload. Before
production connect a real CAS client and verify read-after-write behavior.

## 4. Worker admission and scheduling

- Register with CT-06 `Ops.registerWorker` using a **unique per-process UUID**,
  `flow:task` and `flow:execute:<executor>` capabilities.
- Call `Flow.registerResources` with integer `cpuUnits`, `memoryMb` and
  `maxTasks`; `runManagedFlowWorker` combines this with the CT-06 heartbeat
  loop and `Flow.runWorker`.
- Each claim checks CT-06 worker state, TTL, capabilities, live leases,
  registered CPU/memory and task-count capacities. It also respects tenant
  `max_flow_inflight` and the concurrency group's `max_flow_tasks`.
- Tasks are prioritized across independent runs with bounded age credit.
  Input dependency order is stricter than priority: a blocked task can never
  jump ahead of an uncommitted predecessor.
- A database-wide **admission gate** serializes capacity decisions, claim,
  completion/failure and cancellation for DF-01. This follows CT-06's
  correctness-first philosophy; performance/sharding is intentionally deferred.
- The runner's task callback must be **idempotent and external-effect-free**.
  On retry it receives the same deterministic `effectKey`:
  `hfsm/flow/<run-uuid>/<task-key>`.
- A provider publishes artifacts before returning `[(outputPort,artifactId)]`.
  The worker enforces task timeout, heartbeats the lease, and settles via a
  database fencing predicate. Interrupted/stale workers cannot emit tokens.
- A terminal failure emits a per-run dead letter and cancels unfinished
  dependent tasks; no hidden scatter/gather and no automatic graph expansion.
- Runner cancellation (`Flow.cancelRun`) uses the same admission gate and
  invalidates all in-flight task fences. CT-06 HFSM cancellation does not
  automatically call it: the trusted orchestration service must explicitly
  cancel/reconcile linked runs.

## 5. API modules

| Module | Role |
|---|---|
| `Hfsm.Flow.Spec` | Strict `FlowSpec` types, bounds, port/edge validation, topological order |
| `Hfsm.Flow.Types` | Artifact, lease, start request, resource and store contracts |
| `Hfsm.Flow.Postgres.Statements` | Complete prepared Hasql-TH SQL interface |
| `Hfsm.Flow.Postgres` | Definition, run, CAS manifest, lease, completion and output-lineage inspection adapter |
| `Hfsm.Flow.Worker` | Provider dispatcher, timeouts, heartbeats and completion poller |
| `Hfsm.Flow.Runtime` | CT-06 registration/heartbeat/resource worker composition |
| `Hfsm.Flow.Provider.Start` | CT-04 outbox -> idempotent flow-run start provider |
| `Demo.FlowRegression` | Pure topology, bounds and JSON contract test |
| `Demo.FlowPostgresRegression` | Real PostgreSQL DAG, artifacts, concurrency, reclaim, HFSM completion |
| `Commands.Test` | Complete updated test command dispatch |

## 6. Deliberately excluded from DF-01

**Not implemented:** dynamic scatter/gather, graph rewriting, partitioned
aggregation, streaming tokens, windowing, distributed shuffle, data-locality
heuristics, worker binary/closure attestation, remote CAS byte verification,
artifact GC, active dataflow read-model projections, and performance sharding.

**Security:** Use trusted service accounts and explicit GRANTs; CT-06 tenant
identity/quotas are not customer authentication or full row-level security.
The migration REVOKEs public execution of core flow mutation functions; grant
only the required functions to appropriate application roles. Admin actions
should be authorized by API/DB role policy.

**Operational edge case:** if a source HFSM instance is cancelled or migrates
while its flow is running, new task claims are blocked for terminal sources,
but the existing flow must be explicitly cancelled/reconciled by the operator.
Completion delivery to a terminal or incompatible source fails visibly and
remains pending; SQL never silently drops or recodes its signal. This is an
intentional first-milestone safety boundary, not full compensation semantics.
