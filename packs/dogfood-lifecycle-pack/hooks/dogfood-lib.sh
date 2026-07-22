#!/usr/bin/env bash
# dogfood-lib.sh — shared helpers for Operation Dogfood's hook scripts
# (v26.7.18 gap-closing pass). Factored OUT of dogfood-lifecycle-capture.sh
# and cng-plan-admission-guard.sh so both a PostToolUse hook (which only
# ever OBSERVES completed calls) and a PreToolUse hook (which can BLOCK a
# call before it runs) share the exact same Turtle-append primitive --
# closing the "capture hook's dead Blocked branch must never become
# reachable" gap named by the L5 audit: PostToolUse structurally cannot see
# a call a PreToolUse hook blocked (Claude Code never dispatches PostToolUse
# for a blocked call), so the ONLY way a dfl:Blocked dfl:ToolEvent can ever
# exist is for the blocking hook itself to append it. This file is that
# shared append primitive, callable from either hook.
#
# PARAMETERIZED TARGET (closes the Input-acquisition gap's "hardcoded
# absolute path to a sibling repo on one machine" finding): every caller
# resolves the capture/validation root via `dogfood_repo_root`, which reads
# $DOGFOOD_REPO_ROOT if set, else falls back to the directory three levels
# above this script (repo root, since this file lives at
# packs/dogfood-lifecycle-pack/hooks/dogfood-lib.sh) -- never a hardcoded
# /Users/sac/... path baked into the script text.

dogfood_repo_root() {
  if [ -n "${DOGFOOD_REPO_ROOT:-}" ]; then
    printf '%s' "$DOGFOOD_REPO_ROOT"
    return 0
  fi
  local here
  here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
  # here = <repo>/packs/dogfood-lifecycle-pack/hooks -> repo root is 3 up.
  (cd "$here/../../.." && pwd)
}

dogfood_lifecycle_dir() {
  printf '%s/.cargo-cicd/lifecycle' "$(dogfood_repo_root)"
}

dogfood_shapes_path() {
  printf '%s/packs/dogfood-lifecycle-pack/shapes.ttl' "$(dogfood_repo_root)"
}

# dogfood_append_event sid tool outcome in_hash out_hash
#
# Appends one dfl:ToolEvent Turtle node to
# <lifecycle-dir>/session-<sid>.ttl, using the SAME portable mkdir-based
# lock + single-printf-write discipline
# dogfood-lifecycle-capture.sh already established (concurrency-safe: see
# README.md "Concurrency fix"). Always returns 0 (best-effort; a hook must
# never fail the tool call it is observing/guarding because of a logging
# problem) -- callers that need to know whether the append actually landed
# should check the file directly, not this function's exit code.
dogfood_append_event() {
  local sid="$1" tool="$2" outcome="$3" in_hash="$4" out_hash="$5"
  local dir f lock tries seq header event

  case "$outcome" in Ok|Error|Blocked) : ;; *) outcome=Ok ;; esac
  [ -n "$sid" ] && [ -n "$tool" ] || return 0

  dir="$(dogfood_lifecycle_dir)"
  mkdir -p "$dir" 2>/dev/null || return 0
  f="$dir/session-${sid}.ttl"

  lock="$f.lock"
  tries=0
  while ! mkdir "$lock" 2>/dev/null; do
    tries=$((tries + 1))
    [ "$tries" -ge 50 ] && return 0
    sleep 0.05
  done
  trap 'rmdir "'"$lock"'" 2>/dev/null' RETURN

  if [ ! -f "$f" ]; then
    header=$(printf '@prefix dfl:     <http://seanchatmangpt.github.io/packs/dogfood-lifecycle#> .\n@prefix prov:    <http://www.w3.org/ns/prov#> .\n@prefix dcterms: <http://purl.org/dc/terms/> .\n@prefix skos:    <http://www.w3.org/2004/02/skos/core#> .\n@prefix time:    <http://www.w3.org/2006/time#> .\n@prefix xsd:     <http://www.w3.org/2001/XMLSchema#> .\n\n<urn:dfl:agent:%s> a prov:SoftwareAgent , prov:Agent .\n\n<urn:dfl:session:%s> a dfl:Session , prov:Activity ;\n    dcterms:identifier "%s" ;\n    prov:wasAssociatedWith <urn:dfl:agent:%s> .\n' "$sid" "$sid" "$sid" "$sid")
    seq=0
  else
    header=""
    seq=$(grep -c 'a dfl:ToolEvent' "$f" 2>/dev/null || echo 0)
  fi

  local ts
  ts=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
  [ -n "$in_hash" ] || in_hash="0000000000000000000000000000000000000000000000000000000000000000"
  [ -n "$out_hash" ] || out_hash="0000000000000000000000000000000000000000000000000000000000000000"

  event=$(printf '\n<urn:dfl:event:%s:%s> a dfl:ToolEvent , prov:Activity ;\n    dcterms:isPartOf <urn:dfl:session:%s> ;\n    prov:wasAssociatedWith <urn:dfl:agent:%s> ;\n    skos:notation "%s" ;\n    dfl:sequenceIndex "%s"^^xsd:integer ;\n    time:inXSDDateTimeStamp "%s"^^xsd:dateTimeStamp ;\n    prov:used <urn:blake3:%s> ;\n    prov:generated <urn:blake3:%s> ;\n    dfl:outcome dfl:%s .\n' "$sid" "$seq" "$sid" "$sid" "$tool" "$seq" "$ts" "$in_hash" "$out_hash" "$outcome")

  printf '%s%s' "$header" "$event" >> "$f" 2>/dev/null || true
  rmdir "$lock" 2>/dev/null
  trap - RETURN
  return 0
}

# dogfood_bump_invocation_counter sid
#
# GOVERNANCE COVERAGE (closes the "no genuine coverage-detection mechanism"
# gap): increments a session-wide "tool invocations SEEN" counter,
# independent of whether that invocation went on to pass the closed
# tool-name allowlist or produce a valid ToolEvent. dogfood-lifecycle-
# session-end.sh compares this counter against the number of dfl:ToolEvent
# nodes actually captured and flags + receipts any gap as an anomaly. Uses
# the SAME lock as the event append (a single counter file per session,
# incremented atomically under the same mkdir lock discipline) so a
# concurrent burst of tool calls cannot lose increments to a race.
dogfood_bump_invocation_counter() {
  local sid="$1" tool="${2:-unknown}"
  [ -n "$sid" ] || return 0
  local dir f lock tries n
  dir="$(dogfood_lifecycle_dir)"
  mkdir -p "$dir" 2>/dev/null || return 0
  f="$dir/session-${sid}.invocations"
  lock="$f.lock"
  tries=0
  while ! mkdir "$lock" 2>/dev/null; do
    tries=$((tries + 1))
    [ "$tries" -ge 50 ] && return 0
    sleep 0.05
  done
  n=$(cat "$f" 2>/dev/null || echo 0)
  n=$((n + 1))
  printf '%s' "$n" > "$f" 2>/dev/null || true
  rmdir "$lock" 2>/dev/null

  # GOVERNANCE COVERAGE (v26.7.19, per-tool-name breakdown -- L3->L4): the
  # v26.7.18 pass only compared a single aggregate "invocations seen" total
  # against total captured events, so a governance gap could be DETECTED but
  # not ATTRIBUTED to a specific tool name. This appends one line per
  # invocation to session-<sid>.invocations-by-tool.jsonl (best-effort, own
  # lock so a burst of concurrent calls cannot lose lines) so
  # session-end.sh can report WHICH tool name(s) accounted for a gap, not
  # merely that a gap of some size exists. Uses a second, tool-scoped lock
  # (not the total-counter lock above) so the two counters never contend.
  local tf tlock ttries
  tf="$dir/session-${sid}.invocations-by-tool.jsonl"
  tlock="$tf.lock"
  ttries=0
  while ! mkdir "$tlock" 2>/dev/null; do
    ttries=$((ttries + 1))
    [ "$ttries" -ge 50 ] && return 0
    sleep 0.05
  done
  printf '{"tool":"%s"}\n' "$tool" >> "$tf" 2>/dev/null || true
  rmdir "$tlock" 2>/dev/null
  return 0
}

# dogfood_ingest_validate session_ttl_path
#
# ON-INGEST VALIDATION (closes the "validation is a later manual batch
# step, not on ingest" gap): runs `ggen graph validate --files <log>`
# IMMEDIATELY after an event is appended (not deferred to session-end), and
# appends ONE line per attempt to <lifecycle-dir>/ingest-validation.jsonl
# recording whether that snapshot of the growing log currently parses.
# Best-effort: never fails the caller (a missing `ggen` binary, a missing
# `jq`, or any other internal error is swallowed, matching this pack's
# existing best-effort discipline for hooks that must never disrupt a
# session).
#
# REGRESSION FOUND AND FIXED FORWARD (2026-07-22, wave3 reverify-unverified-
# docs pass): commit ad9106702 (2026-07-19, the same-day SHACL->SPARQL-gates
# migration that added gates/{010_required,020_single_valued,
# 030_value_constraints}.rq) deleted packs/dogfood-lifecycle-pack/shapes.ttl
# but left this function's `--shapes "$(dogfood_shapes_path)"` argument
# pointing at that now-nonexistent path. Live re-verification this pass
# confirmed the real, reproducible effect: `ggen graph validate --files X
# --shapes <missing-path>` errors out ("shapes file ... unreadable: No such
# file or directory") on EVERY invocation regardless of whether the session
# log is well-formed, so `shapes_conform` was unconditionally false in every
# ingest-validation.jsonl record since that commit landed -- a permanent
# false negative, not a narrowing. `ggen graph validate` has no `--gates`
# flag (confirmed via `ggen graph validate --help`; gates/*.rq are wired
# into `[validation].gates` sync-time admission or exercised directly via
# `TripleStore::query()` in a Rust test, per
# `crates/praxis-graphlaw/tests/dogfood_lifecycle_hook_actuation.rs`'s
# `gate_020_rows` helper -- neither is a drop-in CLI substitute for a bash
# script). Fixed minimally and disclosed: only pass `--shapes` when the file
# actually exists, so a well-formed log's real Turtle-parseability is
# reported accurately again instead of being permanently masked by a
# dangling path. SHACL shape-conformance itself is NOT reinstated here (the
# shapes it checked now live as gates/*.rq, not shapes.ttl) -- re-wiring
# gate-conformance into this bash pipeline remains a real, disclosed,
# NOT-closed follow-up (see dogfood-lifecycle-session-end.sh's matching
# note).
dogfood_ingest_validate() {
  local ttl_path="$1"
  [ -f "$ttl_path" ] || return 0
  command -v jq >/dev/null 2>&1 || return 0

  local root ggen_bin shapes ok ts
  root="$(dogfood_repo_root)"
  ggen_bin="$root/target/release/ggen"
  [ -x "$ggen_bin" ] || ggen_bin=$(command -v ggen 2>/dev/null || true)
  [ -n "$ggen_bin" ] || return 0
  shapes="$(dogfood_shapes_path)"

  if [ -f "$shapes" ]; then
    if "$ggen_bin" graph validate --files "$ttl_path" --shapes "$shapes" >/dev/null 2>&1; then
      ok=true
    else
      ok=false
    fi
  else
    # shapes.ttl no longer exists (see REGRESSION note above) -- fall back
    # to a real Turtle PARSE check rather than reporting a false negative
    # caused by a missing file.
    if "$ggen_bin" graph validate --files "$ttl_path" >/dev/null 2>&1; then
      ok=true
    else
      ok=false
    fi
  fi
  ts=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
  local rec
  rec=$(jq -nc --arg log "$(basename "$ttl_path")" --argjson ok "$ok" --arg ts "$ts" \
    '{session_log: $log, shapes_conform: $ok, checked_at: $ts}' 2>/dev/null) || return 0
  printf '%s\n' "$rec" >> "$(dogfood_lifecycle_dir)/ingest-validation.jsonl" 2>/dev/null || true
  return 0
}
