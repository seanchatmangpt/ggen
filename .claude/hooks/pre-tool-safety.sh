#!/bin/bash
# Timeout: 3s
# Purpose: Prevent destructive operations and enforce ggen safety rules

set -euo pipefail

COMMAND="${1:-}"

timeout 3s bash -c '
  COMMAND="$1"

  # Check for empty command
  if [[ -z "$COMMAND" ]]; then
    exit 0
  fi

  # CRITICAL: Block direct cargo commands (must use cargo make)
  if echo "$COMMAND" | grep -qE "^\s*(cargo\s+(check|test|build|clippy|fmt|run|bench))"; then
    echo "❌ BLOCKED: Direct cargo commands prohibited. Use: cargo make <target>" >&2
    echo "   Available: cargo make check|test|lint|pre-commit|ci" >&2
    exit 1
  fi

  # CRITICAL: Block destructive git operations on main/master
  if echo "$COMMAND" | grep -qE "git\s+push.*--force.*(main|master)"; then
    echo "❌ BLOCKED: Force push to main/master prohibited" >&2
    exit 1
  fi

  # CRITICAL: Block dangerous rm patterns
  if echo "$COMMAND" | grep -qE "rm\s+-rf\s+(/\s|/\$|\.|\.\.|\*)"; then
    echo "❌ BLOCKED: Dangerous rm -rf pattern detected" >&2
    exit 1
  fi

  # CRITICAL: Block git reset --hard without explicit confirmation
  if echo "$COMMAND" | grep -qE "git\s+reset\s+--hard" && [[ ! "$COMMAND" =~ "confirmed" ]]; then
    echo "⚠️  WARNING: git reset --hard detected (destructive)" >&2
    echo "   Add \"confirmed\" to command if intentional" >&2
    exit 1
  fi

  # CRITICAL: Block saving files to root
  if echo "$COMMAND" | grep -qE "(touch|echo.*>|cat.*>)\s+/home/user/ggen/[^/]+\.(rs|toml|md|txt)"; then
    echo "❌ BLOCKED: Saving files to root folder prohibited" >&2
    echo "   Use: crates/*/src/, docs/, tests/, scripts/, etc." >&2
    exit 1
  fi

  # CRITICAL: Block unwrap/expect in production code (not tests)
  if [[ "$COMMAND" =~ "crates/[^/]+/src/" ]] && echo "$COMMAND" | grep -qE "\.(unwrap|expect)\("; then
    echo "⚠️  WARNING: unwrap/expect in production code detected" >&2
    echo "   Use Result<T, E> instead. unwrap() only allowed in tests." >&2
  fi

  # CRITICAL: Validate cargo make timeout exists
  if echo "$COMMAND" | grep -qE "cargo\s+make" && [[ ! "$COMMAND" =~ "timeout-check" ]]; then
    if ! command -v timeout &> /dev/null; then
      echo "⚠️  WARNING: timeout command not found. Run: cargo make timeout-check" >&2
    fi
  fi

  # Block git operations that skip hooks
  if echo "$COMMAND" | grep -qE "git.*(--no-verify|--no-gpg-sign)"; then
    echo "❌ BLOCKED: Skipping git hooks prohibited (--no-verify, --no-gpg-sign)" >&2
    exit 1
  fi

  # Warn about unimplemented! in production
  if [[ "$COMMAND" =~ "crates/[^/]+/src/" ]] && echo "$COMMAND" | grep -qE "unimplemented!"; then
    echo "⚠️  WARNING: unimplemented!() detected in production code" >&2
    echo "   Complete implementations required per CLAUDE.md" >&2
  fi

  # Success - command is safe
  exit 0
' bash "$COMMAND"

exit $?
