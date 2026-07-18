#!/usr/bin/env bash
# guard-publish-standing — validate docs/aps/claims.toml and (in full mode) gate publish.
#
# Modes:
#   --schema-only   validate structure only (used by `just pre-commit`): the file parses,
#                   every claim has id/standing/falsifier/evidence, standings are from the
#                   allowed vocabulary. Publish-gate enforcement is skipped — commits are
#                   not publishes.
#   (default)       full mode, run before any real `cargo publish`: everything above, PLUS
#                   fail if any claim whose gates include "publish" has standing=BLOCKED
#                   without an explicit non-empty exception_admitted_by, and warn (not fail)
#                   on evidence coordinates that don't match the current HEAD prefix.
#
# Vocabulary (this repo's no-overclaiming floor): ALIVE | PARTIAL | BLOCKED | UNVERIFIED.
set -euo pipefail

LEDGER="docs/aps/claims.toml"
MODE="${1:-full}"

if [ ! -f "$LEDGER" ]; then
  echo "BUILD_BROKEN: $LEDGER not found"
  exit 1
fi

HEAD_SHORT="$(git rev-parse --short=9 HEAD 2>/dev/null || echo unknown)"

MODE="$MODE" HEAD_SHORT="$HEAD_SHORT" python3 - "$LEDGER" <<'PY'
import os, sys, tomllib

path = sys.argv[1]
mode = os.environ.get("MODE", "full")
head = os.environ.get("HEAD_SHORT", "unknown")
ALLOWED = {"ALIVE", "PARTIAL", "BLOCKED", "UNVERIFIED"}

with open(path, "rb") as f:
    data = tomllib.load(f)

claims = data.get("claims", [])
errors, warnings = [], []

if not claims:
    errors.append("ledger contains zero [[claims]] entries")

seen_ids = set()
for i, c in enumerate(claims):
    cid = c.get("id", f"<claims[{i}]>")
    if cid in seen_ids:
        errors.append(f"{cid}: duplicate id")
    seen_ids.add(cid)
    for field in ("id", "standing", "falsifier", "evidence"):
        if field not in c:
            errors.append(f"{cid}: missing required field '{field}'")
    st = c.get("standing")
    if st is not None and st not in ALLOWED:
        errors.append(f"{cid}: standing '{st}' not in {sorted(ALLOWED)}")
    ev = c.get("evidence")
    if isinstance(ev, dict):
        for field in ("commit", "date", "method"):
            if field not in ev:
                errors.append(f"{cid}: evidence missing '{field}'")
    elif ev is not None:
        errors.append(f"{cid}: evidence must be an inline table")

if mode != "--schema-only" and mode != "schema-only":
    for c in claims:
        cid = c.get("id", "?")
        gates = c.get("gates", [])
        if "publish" not in gates:
            continue
        if c.get("standing") == "BLOCKED":
            admitted = str(c.get("exception_admitted_by", "")).strip()
            if not admitted:
                errors.append(
                    f"{cid}: publish-gated claim is BLOCKED with no admitted exception "
                    f"(set exception_admitted_by = \"<who>\" to record an explicit risk acceptance)"
                )
        ev = c.get("evidence", {})
        commit = str(ev.get("commit", "")) if isinstance(ev, dict) else ""
        if commit and head != "unknown" and not (head.startswith(commit) or commit.startswith(head)):
            warnings.append(
                f"{cid}: evidence coordinate {commit} != HEAD {head} — stale evidence is a "
                f"warning, not a refutation; re-run the falsifier to refresh"
            )

for w in warnings:
    print(f"WARN: {w}")
if errors:
    for e in errors:
        print(f"BUILD_BROKEN: {e}")
    sys.exit(1)

label = "schema" if mode in ("--schema-only", "schema-only") else "publish-standing"
print(f"ALIVE: {len(claims)} claims validated ({label} mode)")
PY
