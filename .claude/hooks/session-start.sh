#!/bin/bash
# Timeout: 15s
# Purpose: Inject live workspace state at session start + verify truth-gate binary.
#          Check critical infrastructure, no ceremony.

set -euo pipefail

timeout 15s bash -c '
  # Detect workspace root from git
  WORKSPACE_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || echo "")"
  if [[ -z "$WORKSPACE_ROOT" ]]; then
    echo "ERROR: Not inside a git repository" >&2
    exit 1
  fi

  cd "$WORKSPACE_ROOT"

  # 1. Check truth-gate binary exists (critical infrastructure)
  BINARY="./tools/truth-gate/target/release/truth-gate"
  if [ ! -f "$BINARY" ]; then
    echo "⚠️  truth-gate binary missing. Hooks will not enforce gates." >&2
    echo "Run: cargo make build-truth-gate (or build-all)" >&2
  fi

  # 2. Current branch
  BRANCH="$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "unknown")"

  # 3. Uncommitted changes
  UNCOMMITTED="$(git status --porcelain 2>/dev/null | wc -l | tr -d " ")"
  UNCOMMITTED="${UNCOMMITTED:-0}"

  # 4. Compile state (quick check, last 5 lines)
  COMPILE_OUTPUT="$(cargo check --workspace 2>&1 | tail -5 || true)"
  if echo "$COMPILE_OUTPUT" | grep -qE "error\[E|error:"; then
    COMPILE_STATE="ERRORS"
  elif echo "$COMPILE_OUTPUT" | grep -qE "warning:"; then
    COMPILE_STATE="WARNINGS"
  else
    COMPILE_STATE="CLEAN"
  fi

  # 5. Structured output
  echo "Branch: ${BRANCH} | Changes: ${UNCOMMITTED} uncommitted | Compile: ${COMPILE_STATE}" >&2
  echo "Model: sonnet (default) | Explore: haiku | Hook: truth-gate, pre_tool_use_guard, post_tool_use_emitter" >&2
  echo "Andon: STOP THE LINE on error[E...], FAILED, or clippy::" >&2
  echo "Evidence tier required for completion claims: PROVEN" >&2

  exit 0
'

exit $?
