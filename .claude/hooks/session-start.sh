#!/bin/bash
# Purpose: Inject live workspace state at session start. No ceremony.

set -uo pipefail

WORKSPACE_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"

# Git state
if [ -d "$WORKSPACE_ROOT/.git" ]; then
  BRANCH=$(git -C "$WORKSPACE_ROOT" rev-parse --abbrev-ref HEAD 2>/dev/null || echo "unknown")
  CHANGED=$(git -C "$WORKSPACE_ROOT" status --porcelain 2>/dev/null | wc -l | tr -d ' ')
  echo "branch=$BRANCH uncommitted=$CHANGED"
fi

# Compile state (best-effort, no timeout since this is session start)
if [ -f "$WORKSPACE_ROOT/Cargo.toml" ]; then
  COMPILE_OUTPUT=$(cargo check --workspace --quiet 2>&1 || true)
  if echo "$COMPILE_OUTPUT" | grep -qE 'error'; then
    ERRORS=$(echo "$COMPILE_OUTPUT" | grep -cE 'error' || true)
    echo "compile=ERRORS($ERRORS)"
  elif echo "$COMPILE_OUTPUT" | grep -qE 'warning'; then
    WARNINGS=$(echo "$COMPILE_OUTPUT" | grep -cE 'warning' || true)
    echo "compile=WARNINGS($WARNINGS)"
  else
    echo "compile=CLEAN"
  fi
fi

exit 0
