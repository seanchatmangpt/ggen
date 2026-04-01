#!/bin/bash
# Timeout: 3s
# Purpose: Inject live workspace state at session start.
#          No ceremony -- structured context only.

set -euo pipefail

timeout 3s bash -c '
  # Detect workspace root from git
  WORKSPACE_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || echo "")"
  if [[ -z "$WORKSPACE_ROOT" ]]; then
    echo "ERROR: Not inside a git repository" >&2
    exit 1
  fi

  cd "$WORKSPACE_ROOT"

  # 1. Current branch
  BRANCH="$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "unknown")"

  # 2. Uncommitted changes
  UNCOMMITTED="$(git status --porcelain 2>/dev/null | wc -l | tr -d " ")"
  UNCOMMITTED="${UNCOMMITTED:-0}"

  # 3. Compile state (quick check, last 5 lines)
  COMPILE_OUTPUT="$(cargo check --workspace 2>&1 | tail -5 || true)"
  if echo "$COMPILE_OUTPUT" | grep -qE "error\[E|error:"; then
    COMPILE_STATE="ERRORS"
  elif echo "$COMPILE_OUTPUT" | grep -qE "warning:"; then
    COMPILE_STATE="WARNINGS"
  else
    COMPILE_STATE="CLEAN"
  fi

  # 4. Structured output
  echo "Branch: ${BRANCH} | Changes: ${UNCOMMITTED} uncommitted | Compile: ${COMPILE_STATE}" >&2
  echo "Andon: STOP THE LINE on compiler errors and test failures" >&2
  echo "Evidence tier required for completion claims: PROVEN" >&2

  exit 0
'

exit $?
