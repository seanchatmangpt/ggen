#!/usr/bin/env bash
# dogfood-lifecycle-capture.sh — Operation Dogfood (v26.7.18) PostToolUse hook.
#
# Appends one dfl:ToolEvent Turtle node per tool event to
# <repo>/.cargo-cicd/lifecycle/session-<id>.ttl, conforming to
# packs/dogfood-lifecycle-pack/{ontology,shapes}.ttl. This is how Multifractal
# Workflow governs its own Claude Code lifecycle: every tool call becomes an
# admitted RDF observation the session-end recipe can validate + receipt.
#
# OBSERVATION ONLY. This hook records what a tool event did; it never asserts
# authority, permission, or actuation. Outcome is the TOOL-level result (Ok
# unless the tool itself errored); a dfl:Blocked outcome is asserted by
# hooks/cng-plan-admission-guard.sh (the PreToolUse guard), not here --
# PostToolUse structurally never fires for a call a PreToolUse hook blocked,
# so this hook's own "Blocked" branch is intentionally unreachable by
# design, not a bug (see hooks/dogfood-lib.sh's header for the full
# disclosure and README.md's "Actuation closure" section).
#
# BEST-EFFORT: any internal failure must never disrupt the session. Every step
# is guarded and the hook always exits 0.
#
# PARAMETERIZED TARGET (v26.7.18): the capture root is resolved via
# dogfood-lib.sh's dogfood_repo_root (honors $DOGFOOD_REPO_ROOT, else
# derives the repo root from this script's own location) -- no hardcoded
# /Users/sac/... path.

# shellcheck source=./dogfood-lib.sh
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/dogfood-lib.sh" 2>/dev/null || exit 0

payload=$(cat 2>/dev/null || true)
{
  command -v jq >/dev/null 2>&1 || exit 0
  command -v b3sum >/dev/null 2>&1 || exit 0

  sid=$(printf '%s' "$payload" | jq -r '.session_id // "unknown"' 2>/dev/null || echo unknown)
  tool=$(printf '%s' "$payload" | jq -r '.tool_name // "unknown"' 2>/dev/null || echo unknown)

  # GOVERNANCE COVERAGE: count this invocation as SEEN before the closed
  # tool-name filter below decides whether it gets an admitted ToolEvent.
  # This is what lets session-end.sh detect a gap between "invocations
  # this hook observed" and "ToolEvent nodes actually captured".
  dogfood_bump_invocation_counter "$sid"

  # Only the closed tool-name set is admitted by dfl:ToolNameScheme / the shape.
  case "$tool" in
    Bash|Edit|Write|Read|Grep|Glob|Task|WebFetch|WebSearch) : ;;
    *) exit 0 ;;
  esac

  # Content-address the input and the result with real blake3 digests.
  in_hash=$(printf '%s' "$payload" | jq -c '.tool_input // {}' 2>/dev/null | b3sum --no-names 2>/dev/null | cut -c1-64)
  out_hash=$(printf '%s' "$payload" | jq -c '.tool_response // {}' 2>/dev/null | b3sum --no-names 2>/dev/null | cut -c1-64)
  [ -n "$in_hash" ] && [ -n "$out_hash" ] || exit 0

  # Tool-level outcome: Error only if the tool itself signalled an error.
  # (Blocked is never assigned here -- see the module header disclosure.)
  outcome=$(printf '%s' "$payload" | jq -r '
    if (.tool_response.is_error // .is_error // false) == true then "Error" else "Ok" end
  ' 2>/dev/null || echo Ok)

  dogfood_append_event "$sid" "$tool" "$outcome" "$in_hash" "$out_hash"

  # ON-INGEST VALIDATION: check this session log's parse+SHACL conformance
  # right now, not only later at session-end. Best-effort, never blocks.
  dogfood_ingest_validate "$(dogfood_lifecycle_dir)/session-${sid}.ttl"
} 2>/dev/null || true

exit 0
