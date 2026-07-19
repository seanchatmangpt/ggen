#!/usr/bin/env bash
# capture_and_validate.sh -- L5-push: chains transcript_to_turtle.py
# (capture) and `ggen graph validate --shapes` (admission) into ONE shipped
# pipeline invocation, instead of a human running the Python script by hand
# and separately, optionally, remembering to validate the output.
#
# Matrix 2's Input-acquisition L3 bar: "A shipped capture pipeline turns
# real system events into admitted facts." Before this pass,
# transcript_to_turtle.py existed but was never chained with an admission
# check -- README's own "does NOT prove" section and
# docs/packs/PACK_MATURITY_MODEL.md's scoring both named this exact gap
# ("transcript_to_turtle.py exists but isn't wired"). This script closes
# that specific gap: capture, THEN admit, in one command, failing loudly
# (non-zero exit, real SHACL violation messages) if the captured facts do
# not conform to shapes.ttl -- never silently accepting unvalidated output.
#
# STILL NOT L4/L5 (disclosed, not silently omitted): this is one
# INVOCATION of a pipeline, run by a human or a CI job against one
# transcript file at a time -- not "continuous ... validated (SHACL-gated)
# on ingest" (L4's bar, which needs an actual running capture daemon/watch
# process) and not "capture, admission, and retention are the pack's
# responsibility end to end" (L5's bar, which needs this pack to OWN
# retention policy too, not just chain two existing scripts). See this
# pack's final scoring for the named remaining gap.
#
# USAGE:
#   scripts/capture_and_validate.sh <transcript.jsonl> <out.ttl> [session-id]
#
# Exits non-zero (and prints the real SHACL violation messages) if the
# captured Turtle does not conform to shapes.ttl -- e.g. a transcript whose
# turns somehow lack a required field would be caught HERE, at capture
# time, not silently accepted and only discovered later.

set -euo pipefail

PACK_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
GGEN_BIN="${GGEN_BIN:-/Users/sac/ggen/target/release/ggen}"

TRANSCRIPT="${1:?usage: capture_and_validate.sh <transcript.jsonl> <out.ttl> [session-id]}"
OUT_TTL="${2:?usage: capture_and_validate.sh <transcript.jsonl> <out.ttl> [session-id]}"
SESSION_ID="${3:-}"

echo "== stage 1: capture (transcript_to_turtle.py) ==" >&2
if [ -n "$SESSION_ID" ]; then
  python3 "$PACK_DIR/scripts/transcript_to_turtle.py" \
    --transcript "$TRANSCRIPT" --out "$OUT_TTL" --session-id "$SESSION_ID"
else
  python3 "$PACK_DIR/scripts/transcript_to_turtle.py" \
    --transcript "$TRANSCRIPT" --out "$OUT_TTL"
fi

echo "" >&2
echo "== stage 2: admission (ggen graph validate --shapes) ==" >&2
"$GGEN_BIN" graph validate \
  --files "$OUT_TTL" \
  --shapes "$PACK_DIR/ontology.ttl" \
  --shapes "$PACK_DIR/shapes.ttl" \
  --format json

echo "" >&2
echo "capture + admission succeeded: $OUT_TTL conforms to shapes.ttl" >&2
