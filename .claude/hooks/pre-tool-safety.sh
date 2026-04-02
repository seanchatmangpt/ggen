#!/bin/bash
# Timeout: 3s
# Purpose: Gate destructive operations and enforce ggen safety rules.
#          Exits 1 on blocks (hard violations), exits 0 on passes.

set -euo pipefail

COMMAND="${1:-}"

timeout 3s bash -c '
  COMMAND="$1"

  # Detect workspace root from git
  WORKSPACE_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || echo "")"

  # Check for empty command
  if [[ -z "$COMMAND" ]]; then
    exit 0
  fi

  # GATE 1: Block direct cargo commands (must use cargo make)
  if echo "$COMMAND" | grep -qE "^\s*(cargo\s+(check|test|build|clippy|fmt|run|bench))"; then
    echo "BLOCKED: Direct cargo commands prohibited. Use: cargo make <target>" >&2
    echo "  Available: cargo make check|test|lint|pre-commit|ci" >&2
    exit 1
  fi

  # GATE 2: Block destructive git operations on main/master
  if echo "$COMMAND" | grep -qE "git\s+push.*--force.*(main|master)"; then
    echo "BLOCKED: Force push to main/master prohibited" >&2
    exit 1
  fi

  # GATE 3: Block dangerous rm patterns
  if echo "$COMMAND" | grep -qE "rm\s+-rf\s+(/\s|/\$|\.|\.\.|\*)"; then
    echo "BLOCKED: Dangerous rm -rf pattern detected" >&2
    exit 1
  fi

  # GATE 4: Block git reset --hard (absolute rule per CLAUDE.md)
  if echo "$COMMAND" | grep -qE "git\s+reset\s+--hard"; then
    echo "BLOCKED: git reset --hard is absolutely forbidden" >&2
    echo "  Rule: Fix forward only. Never undo commits." >&2
    exit 1
  fi

  # GATE 5: Block saving files to workspace root
  if [[ -n "$WORKSPACE_ROOT" ]]; then
    ROOT_SAVE_PATTERN="$(echo "$WORKSPACE_ROOT" | sed "s/[.[\*^$()+?{|\\]/\\\\&/g")/[^/]+\.(rs|toml|md|txt)"
    if echo "$COMMAND" | grep -qE "(touch|echo.*>|cat.*>|write).*${ROOT_SAVE_PATTERN}"; then
      echo "BLOCKED: Saving files to root folder prohibited" >&2
      echo "  Use: crates/*/src/, docs/, tests/, scripts/, etc." >&2
      exit 1
    fi
  fi

  # GATE 6: Block git operations that skip hooks
  if echo "$COMMAND" | grep -qE "git.*(--no-verify|--no-gpg-sign)"; then
    echo "BLOCKED: Skipping git hooks prohibited (--no-verify, --no-gpg-sign)" >&2
    exit 1
  fi

  # GATE 7: Block unwrap/expect in production code paths
  if [[ "$COMMAND" =~ crates/[^/]+/src/ ]] && echo "$COMMAND" | grep -qE "\.(unwrap|expect)\("; then
    echo "BLOCKED: unwrap/expect in production code detected" >&2
    echo "  Use Result<T, E> instead. unwrap() only allowed in tests." >&2
    exit 1
  fi

  # ADVISORY: Warn about unimplemented! in production
  if [[ "$COMMAND" =~ crates/[^/]+/src/ ]] && echo "$COMMAND" | grep -qE "unimplemented!"; then
    echo "WARNING: unimplemented!() detected in production code" >&2
    echo "  Complete implementations required per CLAUDE.md" >&2
  fi

  # ADVISORY: Warn if timeout command missing
  if echo "$COMMAND" | grep -qE "cargo\s+make" && ! command -v timeout &>/dev/null; then
    echo "WARNING: timeout command not found. Run: cargo make timeout-check" >&2
  fi

  # Command passed all gates
  exit 0
' bash "$COMMAND"

exit $?
