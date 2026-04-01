#!/bin/bash
# Timeout: 3s
# Purpose: Block dangerous operations before execution.

set -euo pipefail

COMMAND="${1:-}"

WORKSPACE_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"

timeout 3s bash -c '
  COMMAND="$1"
  WORKSPACE_ROOT="$2"

  if [[ -z "$COMMAND" ]]; then
    exit 0
  fi

  # Block direct cargo commands (must use cargo make)
  if echo "$COMMAND" | grep -qE "^\s*cargo\s+(check|test|build|clippy|fmt|run|bench|clean|doc)"; then
    echo "BLOCKED: Direct cargo commands prohibited. Use: cargo make <target>" >&2
    exit 1
  fi

  # Block force push to main/master
  if echo "$COMMAND" | grep -qE "git\s+push.*--force.*(main|master)"; then
    echo "BLOCKED: Force push to main/master prohibited" >&2
    exit 1
  fi

  # Block dangerous rm -rf patterns
  if echo "$COMMAND" | grep -qE "rm\s+-rf\s+(/\s|/\$|~|\.\s*$|\*\s*$)"; then
    echo "BLOCKED: Dangerous rm -rf pattern detected" >&2
    exit 1
  fi

  # Block git reset --hard
  if echo "$COMMAND" | grep -qE "git\s+reset\s+--hard"; then
    echo "BLOCKED: git reset --hard prohibited. Fix forward only." >&2
    exit 1
  fi

  # Block saving files to repository root
  if echo "$COMMAND" | grep -qE "(echo|cat|tee).*>.*[^/]+\.(rs|toml|md|txt)$"; then
    echo "BLOCKED: Saving files to root folder prohibited" >&2
    echo "  Use: crates/*/src/, tests/, docs/, etc." >&2
    exit 1
  fi

  # Block --no-verify flag
  if echo "$COMMAND" | grep -qE "(--no-verify|--no-gpg-sign)"; then
    echo "BLOCKED: Skipping git hooks prohibited" >&2
    exit 1
  fi

  exit 0
' bash "$COMMAND" "$WORKSPACE_ROOT"

exit $?
