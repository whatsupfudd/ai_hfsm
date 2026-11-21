#!/usr/bin/env bash
# For a dedicated PostgreSQL TEST DB only; requires configured thfsm executable.
# Set HFSM_CHAOS_RUNNER if your command differs from the default.
set -euo pipefail
IFS=' ' read -r -a RUNNER <<< "${HFSM_CHAOS_RUNNER:-stack exec -- thfsm}"
for boundary in BeforeClaim AfterClaim BeforeLoadInstance AfterLoadInstance BeforeLoadSnapshot AfterLoadSnapshot BeforeCommit AfterCommit; do
  echo "CT06 process crash at ${boundary}"
  fixture="$(${RUNNER[@]} test chaosFixtureDb --db | tail -n 1)"
  if [[ ! "$fixture" =~ ^[[:xdigit:]-]{36}$ ]]; then
    echo "could not parse test UUID from fixture output: $fixture" >&2
    exit 1
  fi
  # Kill is confined to the *child* thfsm process. Nonzero exit is expected.
  HFSM_CHAOS_INSTANCE_UUID="$fixture" HFSM_FAULT_BOUNDARY="$boundary" HFSM_FAULT_MODE=kill \
    "${RUNNER[@]}" test chaosWorkerDb --db && { echo "fault did not fire" >&2; exit 1; } || true
  sleep 3
  HFSM_CHAOS_INSTANCE_UUID="$fixture" "${RUNNER[@]}" test chaosWorkerDb --db
  HFSM_CHAOS_INSTANCE_UUID="$fixture" "${RUNNER[@]}" test chaosInspectDb --db
  echo "PASS: $boundary"
done
