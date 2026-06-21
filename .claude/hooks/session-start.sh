#!/bin/bash
# Timeout: 15s
# Purpose: Inject live workspace state as structured JSON (additionalContext).
#          Claude Code reads hookSpecificOutput.additionalContext and prepends
#          it to the first system message — gives the agent accurate branch,
#          compile, and rule context before the first prompt.
#
# Phase-shift: detects the [patch.crates-io] path issue (lsp-max sibling dirs
# absent in remote execution) instead of silently failing cargo check --workspace.

set -euo pipefail

main() {
  # ── workspace root ───────────────────────────────────────────────────────
  WORKSPACE_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || true)"
  if [[ -z "$WORKSPACE_ROOT" ]]; then
    jq -n '{"hookSpecificOutput":{"additionalContext":"ERROR: not inside a git repo"}}'
    return 0
  fi
  cd "$WORKSPACE_ROOT"

  # ── branch + changes ─────────────────────────────────────────────────────
  BRANCH="$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo unknown)"
  UNCOMMITTED="$(git status --porcelain 2>/dev/null | wc -l | tr -d ' ' || echo '?')"

  # ── workspace patch health ───────────────────────────────────────────────
  # Remote execution (code.claude.com) clones only this repo; sibling dirs
  # referenced in [patch.crates-io] don't exist, blocking cargo check --workspace.
  MISSING=""
  for P in "../lsp-max" "../wasm4pm-compat" "../wasm4pm"; do
    [[ -d "$P" ]] || MISSING+="$P "
  done
  MISSING="${MISSING% }"

  if [[ -n "$MISSING" ]]; then
    COMPILE_STATE="PATCH_MISSING"
    COMPILE_NOTE="Missing: $MISSING — use per-crate builds (cargo check -p star-toml)"
  else
    OUT="$(cargo check -p star-toml -p ggen-config 2>&1 | tail -3 || true)"
    if echo "$OUT" | grep -qE 'error\[E|^error:'; then
      COMPILE_STATE="ERRORS"
      COMPILE_NOTE="$(echo "$OUT" | grep -m1 'error' | head -c 120)"
    elif echo "$OUT" | grep -qE '^warning:'; then
      COMPILE_STATE="WARNINGS"
      COMPILE_NOTE="see cargo output"
    else
      COMPILE_STATE="CLEAN"
      COMPILE_NOTE="star-toml + ggen-config OK"
    fi
  fi

  # ── dev branch guidance ──────────────────────────────────────────────────
  DEV_NOTE=""
  if echo "$BRANCH" | grep -qE '^claude/'; then
    DEV_NOTE="Dev branch: $BRANCH — push to origin/$BRANCH"
  fi

  # ── truth-gate ───────────────────────────────────────────────────────────
  TG="present"
  [[ -f "$WORKSPACE_ROOT/tools/truth-gate/target/release/truth-gate" ]] || \
    TG="missing (cargo make build-truth-gate)"

  # ── compose context string ───────────────────────────────────────────────
  # jq handles all escaping; we pass each field as a shell variable.
  CONTEXT="$(jq -rn \
    --arg branch "$BRANCH" \
    --arg uncommitted "$UNCOMMITTED" \
    --arg compile "$COMPILE_STATE" \
    --arg note "$COMPILE_NOTE" \
    --arg tg "$TG" \
    --arg dev "$DEV_NOTE" \
    '"=== ggen workspace @ \($branch) ===
changes: \($uncommitted) uncommitted | compile: \($compile)
note: \($note)
truth-gate: \($tg)
\(if $dev != "" then "dev: \($dev)" else "" end)

KEY RULES:
  just <cmd> — never bare cargo
  Chicago TDD: real collaborators, no mocks
  STOP THE LINE on error[E...] or test FAILED
  star_toml: config validation framework (crates/star-toml/)
  Validators: check_non_empty/range/one_of/consistent + with_severity()
  Analytics: fitness() conformance score, variant_id() fingerprint, by_section()
  Evidence-First: OTEL spans prove LLM calls — tests alone are not proof"'
  )"

  # ── emit structured JSON ─────────────────────────────────────────────────
  jq -n \
    --arg ctx "$CONTEXT" \
    --arg title "ggen/$BRANCH" \
    '{"hookSpecificOutput":{"additionalContext":$ctx,"sessionTitle":$title}}'
}

main 2>/dev/null || jq -n '{"hookSpecificOutput":{"additionalContext":"session-start hook errored"}}'
exit 0
