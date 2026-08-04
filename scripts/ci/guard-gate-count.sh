#!/usr/bin/env bash
# guard-gate-count — refuses drift between the real on-disk gate-query count
# and the hand-maintained rf:gateCount fact in .specify/repo-facts.ttl.
#
# Root cause this guard closes (retrofit:GeneratedTableDriftManifested,
# mirrored from scripts/ci/guard-pack-count.sh): rf:gateCount declared "9"
# while .specify/gates/*.rq held 10 real files on disk (confirmed by direct
# recount) because nothing ever re-checks the hand-written literal against
# reality. This guard is the actual fix: it does not recompute the number
# for you (that would just move the staleness one step sideways, e.g. into
# whatever regenerates this script) -- it refuses the build the moment the
# two diverge, the same discipline every other guard-*.sh in this pre-commit
# chain already applies to its own fact class.
#
# Exit non-zero with a BUILD_BROKEN: line naming both counts on mismatch;
# print an ALIVE: line otherwise.
set -euo pipefail

REPO_FACTS="${GATE_COUNT_REPO_FACTS:-.specify/repo-facts.ttl}"
GATES_DIR="${GATE_COUNT_GATES_DIR:-.specify/gates}"

if [ ! -f "$REPO_FACTS" ]; then
  echo "BUILD_BROKEN: $REPO_FACTS not found"
  exit 1
fi

if [ ! -d "$GATES_DIR" ]; then
  echo "BUILD_BROKEN: $GATES_DIR directory not found"
  exit 1
fi

declared="$(grep -oE 'rf:gateCount[[:space:]]+"[0-9]+"' "$REPO_FACTS" | grep -oE '[0-9]+' | head -1)"
if [ -z "$declared" ]; then
  echo "BUILD_BROKEN: no rf:gateCount fact found in $REPO_FACTS"
  exit 1
fi

actual="$(find "$GATES_DIR" -mindepth 1 -maxdepth 1 -type f -name '*.rq' | wc -l | tr -d ' ')"

if [ "$declared" != "$actual" ]; then
  echo "BUILD_BROKEN: rf:gateCount declares $declared but $GATES_DIR/ has $actual real *.rq gate files -- update rf:gateCount in $REPO_FACTS (this drifts every time a gate is added or removed without updating this fact; that is exactly what this guard exists to catch)"
  exit 1
fi

echo "ALIVE: rf:gateCount ($declared) matches real gate file count ($actual)"
