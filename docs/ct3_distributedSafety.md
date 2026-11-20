# HFSM CT-03 — Distributed execution safety contract

Implementation set based on the integrated CT-02A contract in `whatsupfudd/ai_hfsm` (20 November 2026).

## Status

Source changes and source-anchor checks prepared. **Compilation and PostgreSQL integration have not been executed in this environment.** Run both included regression tests before deploying. No schema migration is needed: CT-02 already has the columns and constraints used here.

## Invariants

1. An instance's `revision` begins at 0 and increases by exactly one *per successful committed turn*. At commit, `expectedRevision` AND `expectedSnapshot` must match the same locked instance row. PostgreSQL retains the explicit `revision = expected` compare-and-swap in `advanceInstance`.
2. `signal.ordinal` is an instance-local, increasing sequence allocated under that instance's PostgreSQL row lock (`next_signal_ordinal`). The same invariant is implemented in the in-memory reference store with STM.
3. For an instance, only the lowest pending ordinal can be admitted. Only one signal is leased per instance. Expired leases reclaim **the same signal**, increment attempts, and issue a newly generated `lease_id`; they do not skip to the next queued signal.
4. A lease token is not the worker/node UUID. The immutable token returned by claim is required for commit, failure-acknowledgment and renewal. Both the instance admission slot and signal must agree on token and remain unexpired **using PostgreSQL server time**.
5. Renewal is fenced, validated under instance-then-signal lock order, and is atomic. It never shortens the TTL. A late renewal fails rather than resurrecting an expired lease.
6. Each committed outbox effect has the same deterministic key on both backends: lowercase `step_uuid:zero_based_ordinal`. `hfsm.outbox.effect_key` and `(step_id,ordinal)` already have unique constraints.
7. No planning or worker fault can produce a partially committed turn. Every SQL failure/failed storage check prevents the entire transaction from committing.
8. `commitTurn` is the sole successful signal ACK; `ackSignal` remains reserved for failed/rejected/requeued work.

## Changes by module

| Module | Type | Changes |
|---|---|---|
| `Hfsm.Runtime.Model.Instance` | modified | Expose revision and next signal ordinal |
| `Hfsm.Runtime.Model.Signal` | modified | Expose instance-local ordinal, lease owner, expiry, attempts, ready time |
| `Hfsm.Runtime.Model.Outbox` | modified | Expose deterministic effect key |
| `Hfsm.Store.Class` | modified | Add expected revision+ordinal and fenced lease renewal contract |
| `Hfsm.Runtime.Engine.Step` | modified | Forward expectedRevision and signalOrdinal to semantic commit |
| `Demo.MemoryStore` | modified | Revision/ordinal STM invariants, per-instance admission, expiry/reclaim, fenced renewal, effect keys |
| `Hfsm.Store.Postgres.Statements` | modified | Return distributed metadata in decoded rows, check ordinal, two Hasql-TH renewal statements |
| `Hfsm.Store.Postgres.Transaction` | modified | Explicit revision and ordinal validation, atomic renewal transaction |
| `Hfsm.Store.Postgres` | modified | Bind renewLease into Store IO |
| `Demo.CommitTurnRegression` | modified | Update prior regression to new CommitRq fields |
| `Commands.Test` | modified | Wire both new smoke tests to test command |
| `Hfsm.Runtime.Engine.Lease` | **new** | Optional IO heartbeat for longer pure turns |
| `Demo.DistributedSafetyRegression` | **new** | STM reference checks |
| `Demo.DistributedPostgresRegression` | **new** | Real Hasql.Pool multi-worker and timeout checks |

## Why this is a source transformation rather than a replacement tree

The repo has continued to evolve after CT-02A, including a separate fix to root-terminal completion. The installer transforms the **current working-copy modules** in place, checks each exact textual anchor **before writing any file**, and backs up all originals. This avoids overwriting unrelated code with the previous CT-02A archive.

Run `python3 scripts/install.py /path/to/ai_hfsm --check`, optionally `python3 scripts/install.py /path/to/ai_hfsm --export /tmp/hfsm-ct03-source` to inspect all 14 full updated `.hs` files, then `python3 scripts/install.py /path/to/ai_hfsm` to apply. A fresh Git working tree is recommended. The installer does not access the network, alter database DDL, or commit Git changes.

## Verification commands

```shell
stack build
stack exec -- thfsm test commitTurn
stack exec -- thfsm test distributed
stack exec -- thfsm test distributedDb --db
```

The last test requires a functional `hfsm` schema and the PostgreSQL connection configuration already used by the approval demo. It creates isolated test instances and **does not remove data**.

## Operational cautions

- `renewLease` uses server time in PostgreSQL. Callers must not assume a local-clock deadline extends authority.
- The regular generic `runTick` worker remains a bounded per-turn worker. When a custom IO turn may run longer than its lease, use `withLeaseHeartbeat`, or structure the slow computation as a durable outbox activity. Renewal failure does not confer authority; the later commit remains fenced.
- The Hasql adapter **must not** retry a `commitTurn` transparently after indeterminate transaction outcome. Check committed history using the deterministic step/signal IDs before choosing a retry strategy.
- The idempotency key identifies an external operation, but **does not itself guarantee once-only effects**. Dispatchers/providers must accept the key and make repeated attempts safe.
- Failed/rejected terminal signals may block *semantic* business expectations; queue ordering only guarantees order for pending signals, not that every enqueued signal will be processable after a terminal transition.
- These field additions are source-breaking for downstream code manually constructing `InstanceRw`, `SignalRw`, `OutboxRw`, or `CommitRq`; constructors in other applications must be updated accordingly.
- Lease renewal is defined for work that is still leased. It does not alter `instance.revision`, consume a signal, or advance a state.

## Next milestone

Introduce a capability-aware outbox dispatcher and retry policy (with per-effect idempotency propagation) and extend multi-process fault injection. Completion status and human-review waits remain governed by the HFSM engine, not the database adapter.
