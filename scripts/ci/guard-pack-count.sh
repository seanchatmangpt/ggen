#!/usr/bin/env bash
# guard-pack-count — refuses drift between the real on-disk pack count and
# the hand-maintained rf:packCount fact in .specify/repo-facts.ttl.
#
# Root cause this guard closes (retrofit:GeneratedTableDriftManifested,
# .tcps/retrofit/ggen/load-path.ttl): rf:packCount has drifted stale THREE
# times observed this session alone (28-vs-32, 32-vs-33, 32-vs-39) because
# nothing ever re-checks the hand-written literal against reality. A prior
# generation fixed the number twice; neither fix touched the actual cause,
# so it drifted again both times. This guard is the actual fix: it does not
# recompute the number for you (that would just move the staleness one step
# sideways, e.g. into whatever regenerates this script) -- it refuses the
# build the moment the two diverge, the same discipline every other
# guard-*.sh in this pre-commit chain already applies to its own fact class.
#
# Exit non-zero with a BUILD_BROKEN: line naming both counts on mismatch;
# print an ALIVE: line otherwise.
set -euo pipefail

REPO_FACTS="${PACK_COUNT_REPO_FACTS:-.specify/repo-facts.ttl}"
PACKS_DIR="${PACK_COUNT_PACKS_DIR:-packs}"

if [ ! -f "$REPO_FACTS" ]; then
  echo "BUILD_BROKEN: $REPO_FACTS not found"
  exit 1
fi

if [ ! -d "$PACKS_DIR" ]; then
  echo "BUILD_BROKEN: $PACKS_DIR directory not found"
  exit 1
fi

declared="$(grep -oE 'rf:packCount[[:space:]]+"[0-9]+"' "$REPO_FACTS" | grep -oE '[0-9]+' | head -1)"
if [ -z "$declared" ]; then
  echo "BUILD_BROKEN: no rf:packCount fact found in $REPO_FACTS"
  exit 1
fi

actual="$(find "$PACKS_DIR" -mindepth 1 -maxdepth 1 -type d | wc -l | tr -d ' ')"

if [ "$declared" != "$actual" ]; then
  echo "BUILD_BROKEN: rf:packCount declares $declared but $PACKS_DIR/ has $actual real pack directories -- update rf:packCount in $REPO_FACTS (this drifts every time a pack is added or removed without updating this fact; that is exactly what this guard exists to catch)"
  exit 1
fi

echo "ALIVE: rf:packCount ($declared) matches real pack directory count ($actual)"
