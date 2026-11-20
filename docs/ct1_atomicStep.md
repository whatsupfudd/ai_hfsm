# HFSM CT-01 — Semantic `commitTurn` migration

Target: `whatsupfudd/ai_hfsm`, `main` source as inspected on 2026-11-17.
This is **milestone CT-01**, not a PostgreSQL/distributed production release.

## Why the change

Previously `commitStep` invoked `appendStep`, `appendSnapshot`, `appendOutbox`,
`appendChild`, `appendBreak`, and `updateProj` separately. `runStep` and the
worker independently acknowledged a successful signal. This exposed partially
committed turns and double acknowledgements.

The new `Store` API has exactly one successful-turn write operation:

```haskell
commitTurn :: CommitRq -> m (Either StoreErr CommitRez)
```

A `CommitRq` contains:

- instance UUID and **expected snapshot UUID** (optimistic CAS guard);
- signal UUID and **lease UUID** (fencing guard);
- preplanned step UUID and snapshot UUID, final instance status;
- all outbox commands, spawned children, breakpoint rows, projections.

All of these must commit atomically, including acknowledgement of the leased
signal. If any guard fails, the entire operation returns `Left StoreErr`
without advancing state or persisting any side record.

`Store.ackSignal` remains a **separate semantic operation only** for a failed,
rejected, or requeued signal that did not produce a committed turn. It requires
the lease and is forbidden for an already applied signal.

## Modified modules

| Path | Change |
|---|---|
| `src/Hfsm/Store/Class.hs` | Replace write-oriented record fields with `commitTurn`, `CommitRq`, and `Either StoreErr` results. |
| `src/Hfsm/Runtime/Engine/Step.hs` | Construct turn request, planned deterministic row IDs, commit once, no success ACK. |
| `src/Hfsm/Runtime/Engine/Worker.hs` | On success do nothing; failed planning ACK only; requeue on CAS conflict; do not touch lost leases. |
| `src/Demo/MemoryStore.hs` | One `atomically` STM transaction validates and writes all turn records including success ACK. |
| `src/Demo/CommitTurnRegression.hs` | Regression scenario checks failed CAS does not persist, success is atomic, duplicate turn is rejected, late ACK is rejected. |

The package retains the original directory structure. Two large existing
modules (`Step.hs` and `Demo/MemoryStore.hs`) are regenerated in place from the
pinned upstream sources by `scripts/install.py`, preserving all unrelated code.
The archive carries full direct replacements for the other modules.

## Installation

Do **not** use this as a raw unzip overlay. Extract the ZIP to a separate
folder, then run:

```bash
python3 hfsm_turn_upgrade/scripts/install.py --check /path/to/ai_hfsm
python3 hfsm_turn_upgrade/scripts/install.py /path/to/ai_hfsm
cd /path/to/ai_hfsm
stack build
# If stack exposes ghc as expected:
stack ghc -- -isrc -main-is Demo.CommitTurnRegression.main \
  src/Demo/CommitTurnRegression.hs -o /tmp/hfsm-commit-turn-test
/tmp/hfsm-commit-turn-test
git diff --check
git diff -- src/Hfsm/Store/Class.hs src/Hfsm/Runtime/Engine/Step.hs \
  src/Hfsm/Runtime/Engine/Worker.hs src/Demo/MemoryStore.hs
```

`install.py` checks the exact upstream Git blob hashes before writing anything,
checks all textual source anchors, then creates originals under
`.hfsm-commit-turn-backup/<UTC timestamp>/`. If upstream changed, it aborts;
`--allow-drift` is available for a consciously reviewed variant that still
matches every strict source anchor.

## Invariants of the STM reference implementation

1. **No partial writes.** A conflict or missing input returns without changing
   instance, signal, step, snapshot, outbox, child, break or projection TVars.
2. **CAS against expected snapshot.** Only a worker that planned from the
   current instance snapshot can advance it.
3. **Lease fencing.** `LeasedSs` and exact lease UUID are required to commit.
4. **Single acknowledgement.** `commitTurn` moves the input to `AppliedSs` and
   clears the lease within the same STM transaction.
5. **One logical turn, one step.** Step/snapshot UUIDs are planned from
   `(instance UUID, signal UUID)`; duplicate commit is rejected.
6. **Consistent outputs.** Outbox drafts bind to the instance UUID and step
   UUID, including command/timer drafts that previously used nil placeholders.
7. **Instance status advances.** The committed `InstanceRw.status`, state, path
   and snapshot pointer advance together.
8. **Retryable commit outcomes.** Worker releases/requeues on `ConflictEr` or
   `BackendEr`; stale `LeaseLostEr` workers make no further writes.

## What this does *not* implement

- **Crash durability**: the STM backend is process-memory only.
- **Lease expiry/renewal**: the current signal row does not carry `leaseUntil`.
- **Monotonic `InstanceRw.revision`**: the unique snapshot UUID is the present
  optimistic guard. A revision column is the next incremental milestone.
- **Per-instance ordered queue discipline** across workers.
- **Database transaction semantics** or SQL schema; PostgreSQL remains CT-02.
- **Outbox dispatch guarantees / external exactly-once effects**.
- **Queryable operational diagnostics** beyond existing projections.
- **Verified GHC compilation in the generation environment**: the toolchain
  was not available here; source-anchor validation is not compilation.

## Proposed CT-02 contract for PostgreSQL

Implement the same `commitTurn` operation inside a single PostgreSQL
transaction. Lock and verify instance/signal, verify the CAS snapshot pointer,
insert step + new snapshot + side rows, advance instance and ACK signal before
COMMIT. Use a unique `(instance_id, signal_id)` constraint on steps and stable
IDs for effect deduplication. Extend `SignalRw` with lease expiry, and add a
monotonic `InstanceRw.revision` in a separate DB migration with explicit
backward compatibility.

The current interface intentionally leaves `commitTurn` backend-neutral so the
next workstream can focus on SQL transaction design rather than touching the
state-machine semantics again.
