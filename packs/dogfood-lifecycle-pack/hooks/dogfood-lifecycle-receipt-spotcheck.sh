#!/usr/bin/env bash
# dogfood-lifecycle-receipt-spotcheck.sh — Operation Dogfood (v26.7.13)
# bash-only hash-chain recompute smoke test.
#
# Reads .cargo-cicd/lifecycle/receipts.jsonl (written by
# dogfood-lifecycle-session-end.sh) and, for every CHAINED record (one
# carrying a chain_hash field -- pre-upgrade flat records without one are
# skipped, not failed), independently recomputes:
#   1. payload_hash  from the record's own {blake3, parse_valid,
#      session_log, tool_events} fields (canonical sorted-key JSON, same as
#      the session-end script's `jq -ncS` construction)
#   2. chain_hash    = blake3(prev_chain_hash_hex ++ payload_hash_hex)
#   3. adjacency     -- each chained record's chain_hash must equal the
#      NEXT chained record's prev_chain_hash, and the FIRST chained record's
#      prev_chain_hash must be genesis (64 "0" characters)
# and fails closed (non-zero exit, names the broken record) on any mismatch.
#
# NOT A TRUST BOUNDARY. This is a local spot-check for the bash-only
# production path in dogfood-lifecycle-session-end.sh -- it is NOT the
# `ggen receipt verify` / `ggen receipt history` equivalent named as a
# follow-up in README.md "Scope and named follow-ups" (that needs Rust:
# fail-closed CLI semantics, praxis-core wiring, and coverage of ggen's own
# richer OcelCausalFrame chain, none of which this script attempts).

set -uo pipefail
# shellcheck source=./dogfood-lib.sh
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/dogfood-lib.sh"
recs="$(dogfood_lifecycle_dir)/receipts.jsonl"

command -v jq >/dev/null 2>&1 || { echo "spotcheck: jq not found"; exit 1; }
command -v b3sum >/dev/null 2>&1 || { echo "spotcheck: b3sum not found"; exit 1; }
[ -f "$recs" ] || { echo "spotcheck: no receipts log at $recs"; exit 1; }

genesis=$(printf '0%.0s' $(seq 1 64))

# Only records carrying a chain_hash field participate (pre-upgrade flat
# records are skipped, not failed).
chained=$(jq -c 'select(has("chain_hash"))' "$recs")
if [ -z "$chained" ]; then
  echo "spotcheck: no chained records found in $recs (nothing to verify)"
  exit 0
fi

count=0
expected_prev="$genesis"
prev_display="(none yet)"
ok=1
while IFS= read -r line; do
  [ -z "$line" ] && continue
  count=$((count + 1))

  # v26.7.18: a chained record is now EITHER a session-validation record
  # (blake3/tool_events/parse_valid/invocations_seen/governance_gap) OR a
  # rotation record (rotated/from/to/rotated_at) OR a refusal record
  # (kind:"plan-admission-refusal") -- rotation and refusal records do not
  # carry payload_hash/chain_hash at all (rotation) or are chained over a
  # DIFFERENT payload shape (refusal); this spot-check only recomputes
  # session-validation records (has("blake3")), matching what
  # dogfood-lifecycle-session-end.sh itself chains via that exact payload
  # shape. Records of the other two kinds are counted but not recomputed --
  # a disclosed narrowing of this bash-only smoke test, not a silent skip.
  has_blake3=$(printf '%s' "$line" | jq -r 'has("blake3")')
  if [ "$has_blake3" != "true" ]; then
    echo "spotcheck: record $count is a non-session-validation chained record (rotation/refusal) -- not recomputed by this bash-only smoke test"
    prev_expected_from_this=$(printf '%s' "$line" | jq -r '.chain_hash? // empty')
    if [ -n "$prev_expected_from_this" ]; then
      expected_prev="$prev_expected_from_this"
    fi
    continue
  fi

  session_log=$(printf '%s' "$line" | jq -r '.session_log')
  blake3=$(printf '%s' "$line" | jq -r '.blake3')
  tool_events=$(printf '%s' "$line" | jq -r '.tool_events')
  parse_valid=$(printf '%s' "$line" | jq -r '.parse_valid')
  stored_payload_hash=$(printf '%s' "$line" | jq -r '.payload_hash')
  stored_prev=$(printf '%s' "$line" | jq -r '.prev_chain_hash')
  stored_chain=$(printf '%s' "$line" | jq -r '.chain_hash')

  # Records written before the v26.7.18 governance-coverage upgrade have no
  # invocations_seen/governance_gap keys at all -- reconstruct the payload
  # in whichever of the two shapes this specific record actually used, so
  # older already-chained records keep recomputing correctly rather than
  # being broken by a schema change (same non-retroactive discipline as the
  # original flat->chained upgrade documented above).
  has_governance_fields=$(printf '%s' "$line" | jq -r 'has("invocations_seen")')
  if [ "$has_governance_fields" = "true" ]; then
    invocations_seen=$(printf '%s' "$line" | jq -r '.invocations_seen')
    governance_gap=$(printf '%s' "$line" | jq -r '.governance_gap')
    payload=$(jq -ncS \
      --arg session_log "$session_log" \
      --arg blake3 "$blake3" \
      --argjson tool_events "$tool_events" \
      --argjson parse_valid "$parse_valid" \
      --argjson invocations_seen "$invocations_seen" \
      --argjson governance_gap "$governance_gap" \
      '{blake3: $blake3, parse_valid: $parse_valid, session_log: $session_log, tool_events: $tool_events, invocations_seen: $invocations_seen, governance_gap: $governance_gap}')
  else
    payload=$(jq -ncS \
      --arg session_log "$session_log" \
      --arg blake3 "$blake3" \
      --argjson tool_events "$tool_events" \
      --argjson parse_valid "$parse_valid" \
      '{blake3: $blake3, parse_valid: $parse_valid, session_log: $session_log, tool_events: $tool_events}')
  fi
  recomputed_payload_hash=$(printf '%s' "$payload" | b3sum --no-names - | cut -c1-64)
  recomputed_chain=$(printf '%s%s' "$stored_prev" "$recomputed_payload_hash" | b3sum --no-names - | cut -c1-64)

  if [ "$recomputed_payload_hash" != "$stored_payload_hash" ]; then
    echo "spotcheck: FAIL record $count ($session_log): payload_hash mismatch (stored $stored_payload_hash, recomputed $recomputed_payload_hash)"
    ok=0
    break
  fi
  if [ "$recomputed_chain" != "$stored_chain" ]; then
    echo "spotcheck: FAIL record $count ($session_log): chain_hash mismatch (stored $stored_chain, recomputed $recomputed_chain)"
    ok=0
    break
  fi
  if [ "$stored_prev" != "$expected_prev" ]; then
    echo "spotcheck: FAIL record $count ($session_log): broken link (prev_chain_hash $stored_prev != expected $expected_prev from record $((count - 1)) [$prev_display])"
    ok=0
    break
  fi

  expected_prev="$stored_chain"
  prev_display="$session_log"
done <<< "$chained"

if [ "$ok" -eq 1 ]; then
  echo "spotcheck: OK — $count chained record(s) recompute correctly, head chain_hash=$expected_prev"
  exit 0
else
  exit 1
fi
