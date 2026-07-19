#!/usr/bin/env bash
# dogfood-lifecycle-session-end.sh — Operation Dogfood (v26.7.18) session-end
# validator + hash-chained receipt.
#
# Validates every captured session lifecycle log
# (.cargo-cicd/lifecycle/session-*.ttl, produced by the PostToolUse
# dogfood-lifecycle-capture.sh hook) with `ggen graph validate --files ...
# --shapes ../shapes.ttl` (Turtle parse + real SHACL shape-conformance) and
# appends a hash-chained validation receipt per log to receipts.jsonl.
# Closes the loop: capture -> admit/validate (parse + SHACL) -> receipt.
#
# Invoke manually (`bash .claude/hooks/dogfood-lifecycle-session-end.sh`) or
# wire as a SessionEnd/Stop hook.
#
# PARAMETERIZED TARGET (v26.7.18): resolved via dogfood-lib.sh's
# dogfood_repo_root ($DOGFOOD_REPO_ROOT env var, else derived from this
# script's own location) -- no hardcoded /Users/sac/... path.
#
# GOVERNANCE COVERAGE (v26.7.18): for each session log, compares the
# capture hook's own "invocations seen" counter (dogfood-lib.sh's
# dogfood_bump_invocation_counter, incremented on EVERY tool call the
# PostToolUse hook observed, before the closed-vocabulary filter runs)
# against the number of dfl:ToolEvent nodes actually captured in that log.
# A gap (invocations_seen > events_captured) means SOME tool calls were
# silently dropped -- an unsupported tool name, a jq/b3sum failure, a
# malformed payload -- and is flagged as a receipted anomaly
# (governance_gap: true) rather than passing silently. This directly closes
# the "every capture failure is a silent, unrecorded exit 0" gap named by
# the L5 audit for Governance coverage.
#
# RETENTION / ROTATION (v26.7.18): a session log that has grown past
# $DOGFOOD_MAX_EVENTS (default 5000) tool events is archived (renamed with
# a UTC timestamp suffix under a sibling `archived/` directory) rather than
# left to grow unboundedly forever -- closing the "no retention/rotation
# policy" gap named for Input acquisition. Rotation happens AFTER this run's
# validation + receipt (so the just-validated content is what gets
# archived), and is itself receipted (rotated: true) so the fact that a log
# was rotated is part of the permanent record, not a silent filesystem
# operation.
#
# SCOPE NOTE (v26.7.13, commit 523cc6e4): `ggen graph validate --files X
# --shapes Y` performs real SHACL shape-conformance checking (via
# praxis-graphlaw's GraphLawStore::validate_shacl), not just Turtle PARSE
# validation. This script passes `--shapes shapes.ttl` on every invocation,
# so every session log is checked against dfl:ToolEventShape/dfl:SessionShape
# in `../shapes.ttl`, not merely parsed.
#
# CHAIN NOTE (v26.7.13 receipt-chain upgrade): each appended record now
# carries a genuine two-hop hash chain, not just a flat content digest:
#   payload_hash   = blake3(canonical JSON of {blake3, parse_valid,
#                    session_log, tool_events}, keys sorted -- `jq -ncS`)
#   prev_chain_hash = the previous chained record's chain_hash, or 64 "0"
#                    characters (genesis) for the first chained record
#   chain_hash     = blake3(prev_chain_hash_hex ++ payload_hash_hex)
#                    (ASCII-hex string concatenation, then re-hashed)
# This is the SAME SHAPE as ggen's own .ggen-v2/receipt-log.jsonl chain
# (genesis-seeded; each record's chain_hash feeds the next record's
# prev_chain_hash) but it is NOT byte-compatible with ggen's actual chain
# algorithm: ggen's chain (crates/praxis-core/src/law.rs
# build_admission_frame / chain_from_frame, invoked from
# crates/ggen/src/sync.rs) mixes a 99-byte OcelCausalFrame
# (instruction_id, node_kind, ts_ns, andon, obligation_count, packed
# object refs, ...) through bcinr-powl-receipt's `chain()` call --
# reproducing that exactly needs Rust struct/byte-layout code, not
# bash+jq+b3sum. This script only implements the PRODUCTION side (writing
# a genuinely chained ledger). Trustworthy CONSUMPTION -- a
# `ggen receipt verify`/`ggen receipt history`-equivalent that walks this
# file and fails closed on any break in the chain -- is a named Rust
# follow-up (see README "Scope and named follow-ups"); the sibling
# `dogfood-lifecycle-receipt-spotcheck.sh` in this directory is a bash-only
# recompute smoke test, not that trust boundary.
#
# Pre-existing lines in receipts.jsonl written before this upgrade have no
# chain_hash field (flat content-addressed digests only). The chain starts
# fresh from genesis at the first post-upgrade record; it does not
# retroactively chain through old flat-format lines, and this script never
# rewrites a previously-written line (append-only).

set -uo pipefail
# shellcheck source=./dogfood-lib.sh
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/dogfood-lib.sh"

dir="$(dogfood_lifecycle_dir)"
shapes="$(dogfood_shapes_path)"
max_events="${DOGFOOD_MAX_EVENTS:-5000}"
shopt -s nullglob
files=("$dir"/session-*.ttl)
if [ ${#files[@]} -eq 0 ]; then
  echo "dogfood: no session logs in $dir"
  exit 0
fi

args=()
for f in "${files[@]}"; do args+=(--files "$f"); done
args+=(--shapes "$shapes")

echo "dogfood: validating ${#files[@]} session log(s) via ggen graph validate --files ... --shapes shapes.ttl ..."
if ggen graph validate "${args[@]}" >/dev/null 2>&1; then
  echo "dogfood: VALID — all ${#files[@]} session log(s) parse and conform to shapes.ttl"
  rc=0
else
  echo "dogfood: INVALID — at least one session log failed parse or SHACL shape-conformance validation:"
  ggen graph validate "${args[@]}" 2>&1 | tail -5
  rc=1
fi

recs="$dir/receipts.jsonl"
genesis=$(printf '0%.0s' $(seq 1 64))

# Seed the chain from the last CHAINED record already in receipts.jsonl (a
# record carrying a chain_hash field). Older flat-format lines have no such
# field and are skipped -- the chain begins at genesis the first time this
# upgraded script runs, not retroactively through pre-upgrade history.
prev_chain="$genesis"
if [ -f "$recs" ]; then
  last_line=$(tail -n 1 "$recs" 2>/dev/null || true)
  if [ -n "$last_line" ]; then
    last_chain=$(printf '%s' "$last_line" | jq -r '.chain_hash? // empty' 2>/dev/null || true)
    if [ -n "$last_chain" ]; then
      prev_chain="$last_chain"
    fi
  fi
fi

pv=$([ "$rc" -eq 0 ] && echo true || echo false)
to_rotate=()
for f in "${files[@]}"; do
  h=$(b3sum --no-names "$f" 2>/dev/null | cut -c1-64)
  n=$(grep -c 'a dfl:ToolEvent' "$f" 2>/dev/null || echo 0)

  # GOVERNANCE COVERAGE: cross-check invocations-seen vs events-captured.
  sid=$(basename "$f" .ttl)
  sid=${sid#session-}
  invocations_file="$dir/session-${sid}.invocations"
  invocations_seen=$(cat "$invocations_file" 2>/dev/null || echo "$n")
  gap=false
  if [ "$invocations_seen" -gt "$n" ] 2>/dev/null; then
    gap=true
    echo "dogfood: GOVERNANCE ANOMALY in $f — $invocations_seen invocation(s) seen but only $n dfl:ToolEvent(s) captured (gap=$((invocations_seen - n)))"
  fi

  # Canonical (sorted-key) payload bytes -- the exact bytes payload_hash is
  # computed over, so any independent verifier can rebuild them from the
  # record's own fields without ambiguity.
  payload=$(jq -ncS \
    --arg session_log "$(basename "$f")" \
    --arg blake3 "$h" \
    --argjson tool_events "$n" \
    --argjson parse_valid "$pv" \
    --argjson invocations_seen "${invocations_seen:-$n}" \
    --argjson governance_gap "$gap" \
    '{blake3: $blake3, parse_valid: $parse_valid, session_log: $session_log, tool_events: $tool_events, invocations_seen: $invocations_seen, governance_gap: $governance_gap}')
  payload_hash=$(printf '%s' "$payload" | b3sum --no-names - | cut -c1-64)
  chain_hash=$(printf '%s%s' "$prev_chain" "$payload_hash" | b3sum --no-names - | cut -c1-64)

  jq -nc \
    --arg session_log "$(basename "$f")" \
    --arg blake3 "$h" \
    --argjson tool_events "$n" \
    --argjson parse_valid "$pv" \
    --argjson invocations_seen "${invocations_seen:-$n}" \
    --argjson governance_gap "$gap" \
    --arg payload_hash "$payload_hash" \
    --arg prev_chain_hash "$prev_chain" \
    --arg chain_hash "$chain_hash" \
    '{session_log: $session_log, blake3: $blake3, tool_events: $tool_events, parse_valid: $parse_valid,
      invocations_seen: $invocations_seen, governance_gap: $governance_gap,
      payload_hash: $payload_hash, prev_chain_hash: $prev_chain_hash, chain_hash: $chain_hash}' \
    >> "$recs"

  prev_chain="$chain_hash"

  if [ "$n" -gt "$max_events" ] 2>/dev/null; then
    to_rotate+=("$f")
  fi
done
echo "dogfood: appended ${#files[@]} hash-chained receipt(s) -> $recs (head chain_hash=$prev_chain)"

# RETENTION / ROTATION: archive any log past the configured threshold,
# AFTER it has been validated + receipted above, and receipt the rotation
# itself so it is part of the permanent record rather than a silent mv.
if [ "${#to_rotate[@]}" -gt 0 ]; then
  archive_dir="$dir/archived"
  mkdir -p "$archive_dir" 2>/dev/null || true
  rotate_ts=$(date -u +"%Y%m%dT%H%M%SZ")
  for f in "${to_rotate[@]}"; do
    base=$(basename "$f" .ttl)
    dest="$archive_dir/${base}.${rotate_ts}.ttl"
    if mv "$f" "$dest" 2>/dev/null; then
      echo "dogfood: ROTATED $f -> $dest (exceeded DOGFOOD_MAX_EVENTS=$max_events)"
      jq -nc --arg from "$(basename "$f")" --arg to "$dest" --arg ts "$rotate_ts" \
        '{rotated: true, from: $from, to: $to, rotated_at: $ts}' >> "$recs" 2>/dev/null || true
    fi
  done
fi

exit "$rc"
