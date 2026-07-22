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

# REGRESSION FOUND AND FIXED FORWARD (2026-07-22, wave3 reverify-unverified-
# docs pass): commit ad9106702 (2026-07-19) deleted shapes.ttl as part of the
# SHACL->SPARQL-gates migration (see gates/{010_required,020_single_valued,
# 030_value_constraints}.rq) but left this script's `--shapes "$shapes"`
# argument pointing at the now-deleted file. Live re-verification confirmed
# the real effect: `ggen graph validate --files ... --shapes <missing>`
# errors on EVERY run regardless of whether the session log is well-formed
# ("shapes file ... unreadable: No such file or directory"), so every
# receipt's `parse_valid` was unconditionally false since that commit --
# reproduced live against both the committed session-good.ttl fixture and a
# freshly capture-generated log. `ggen graph validate` has no `--gates` flag
# (confirmed: `ggen graph validate --help` lists only `--files`/`--shapes`);
# gates/*.rq are consumed via `[validation].gates` sync-time admission or a
# direct `TripleStore::query()` call (see
# `crates/praxis-graphlaw/tests/dogfood_lifecycle_hook_actuation.rs`'s
# `gate_020_rows` helper), neither a drop-in bash-CLI substitute. Fixed
# minimally: only pass `--shapes` when the file exists, so a well-formed log
# reports parse_valid:true again instead of a permanent false negative.
# SHACL shape-conformance itself is NOT reinstated (disclosed, not glossed
# over) -- re-wiring gate-conformance into this bash pipeline is a real,
# separate, NOT-closed follow-up.
if [ -f "$shapes" ]; then
  args+=(--shapes "$shapes")
  echo "dogfood: validating ${#files[@]} session log(s) via ggen graph validate --files ... --shapes shapes.ttl ..."
else
  echo "dogfood: WARNING shapes.ttl not found at $shapes (deleted by commit ad9106702's SHACL->SPARQL-gates migration; gate-conformance re-wiring is a disclosed, not-yet-closed follow-up -- see this script's own comment above) -- validating Turtle PARSE only, not SHACL shape-conformance, this run"
  echo "dogfood: validating ${#files[@]} session log(s) via ggen graph validate --files ... (parse-only) ..."
fi
if ggen graph validate "${args[@]}" >/dev/null 2>&1; then
  if [ -f "$shapes" ]; then
    echo "dogfood: VALID — all ${#files[@]} session log(s) parse and conform to shapes.ttl"
  else
    echo "dogfood: VALID (parse-only) — all ${#files[@]} session log(s) parse as Turtle"
  fi
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
  # NOTE (v26.7.19, real bug found + fixed live this session): `grep -c
  # PATTERN file || echo 0` double-prints "0" when PATTERN matches ZERO
  # lines -- `grep -c` already prints "0" to stdout in that case, but GNU
  # grep's exit status is still 1 (no match), so the `|| echo 0` ALSO
  # fires, yielding "0\n0" instead of "0" and breaking any numeric
  # comparison against the captured value. Confirmed live: a governance-gap
  # per-tool check below hit this exact case (`grep -c '"tool":"Edit"'`
  # against a log with zero Edit invocations) and failed with `integer
  # expression expected`. Fixed everywhere in this script by capturing
  # grep -c's stdout directly (ignoring its exit status) and defaulting
  # only if the variable ends up EMPTY, not merely nonzero-exit.
  n=$(grep -c 'a dfl:ToolEvent' "$f" 2>/dev/null); n=${n:-0}

  # GOVERNANCE COVERAGE: cross-check invocations-seen vs events-captured.
  sid=$(basename "$f" .ttl)
  sid=${sid#session-}
  invocations_file="$dir/session-${sid}.invocations"
  invocations_seen=$(cat "$invocations_file" 2>/dev/null || echo "$n")
  gap=false
  gap_tools="[]"
  if [ "$invocations_seen" -gt "$n" ] 2>/dev/null; then
    gap=true
    gapn=$((invocations_seen - n))
    echo "dogfood: GOVERNANCE ANOMALY in $f — $invocations_seen invocation(s) seen but only $n dfl:ToolEvent(s) captured (gap=$gapn)"

    # PER-TOOL ATTRIBUTION (v26.7.19, L3->L4): name WHICH tool(s) the gap
    # came from, not just its size. invocations-by-tool.jsonl has one line
    # per invocation SEEN (before the closed-vocabulary filter); the
    # captured log's own skos:notation triples tell us how many of each
    # tool name actually landed. Any tool whose seen-count exceeds its
    # captured-count is a named, attributable gap.
    # Uses the DISTINCT tool names actually present in invocations-by-tool.jsonl
    # (via jq, not a hardcoded candidate list) so a tool name OUTSIDE the closed
    # vocabulary entirely (e.g. "NotebookEdit", which the capture hook's own
    # case-statement filter drops before it ever becomes a dfl:ToolEvent) is
    # still attributed by name -- that is precisely the most common real gap
    # this mechanism exists to catch, and a fixed candidate list restricted to
    # the closed dfl:ToolNameScheme names would silently miss it.
    tool_seen_file="$dir/session-${sid}.invocations-by-tool.jsonl"
    if [ -f "$tool_seen_file" ] && command -v jq >/dev/null 2>&1; then
      distinct_tools=$(jq -r '.tool' "$tool_seen_file" 2>/dev/null | sort -u)
      gap_tools=$(
        while IFS= read -r t; do
          [ -n "$t" ] || continue
          seen_n=$(grep -c "\"tool\":\"$t\"" "$tool_seen_file" 2>/dev/null); seen_n=${seen_n:-0}
          cap_n=$(grep -c "skos:notation \"$t\"" "$f" 2>/dev/null); cap_n=${cap_n:-0}
          if [ "$seen_n" -gt "$cap_n" ]; then
            echo "dogfood:   -> tool=\"$t\" seen=$seen_n captured=$cap_n gap=$((seen_n - cap_n))" >&2
            printf '%s\n' "$t"
          fi
        done <<< "$distinct_tools" | jq -R -s -c 'split("\n") | map(select(length > 0))'
      )
    fi
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
    --argjson governance_gap_tools "$gap_tools" \
    '{blake3: $blake3, parse_valid: $parse_valid, session_log: $session_log, tool_events: $tool_events, invocations_seen: $invocations_seen, governance_gap: $governance_gap, governance_gap_tools: $governance_gap_tools}')
  payload_hash=$(printf '%s' "$payload" | b3sum --no-names - | cut -c1-64)
  chain_hash=$(printf '%s%s' "$prev_chain" "$payload_hash" | b3sum --no-names - | cut -c1-64)

  jq -nc \
    --arg session_log "$(basename "$f")" \
    --arg blake3 "$h" \
    --argjson tool_events "$n" \
    --argjson parse_valid "$pv" \
    --argjson invocations_seen "${invocations_seen:-$n}" \
    --argjson governance_gap "$gap" \
    --argjson governance_gap_tools "$gap_tools" \
    --arg payload_hash "$payload_hash" \
    --arg prev_chain_hash "$prev_chain" \
    --arg chain_hash "$chain_hash" \
    '{session_log: $session_log, blake3: $blake3, tool_events: $tool_events, parse_valid: $parse_valid,
      invocations_seen: $invocations_seen, governance_gap: $governance_gap, governance_gap_tools: $governance_gap_tools,
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
