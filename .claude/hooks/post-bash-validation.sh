#!/bin/bash
# Timeout: 3s
# Purpose: Andon signal enforcement. Exit 2 (blocking) on CRITICAL signals.

set -euo pipefail

COMMAND="${1:-}"
EXIT_CODE="${2:-0}"
OUTPUT="${3:-}"

WORKSPACE_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"

timeout 3s bash -c '
  COMMAND="$1"
  EXIT_CODE="$2"
  OUTPUT="$3"

  # CRITICAL ANDON: Compiler errors -> exit 2 (blocking)
  if echo "$OUTPUT" | grep -qE "error(\[E[0-9]+\]|:)"; then
    echo "COMPILER ERROR: STOP THE LINE" >&2
    echo "  Run: cargo make check" >&2
    echo "  Read the error. Fix the code. Do not suppress." >&2
    exit 2
  fi

  # CRITICAL ANDON: Test failures -> exit 2 (blocking)
  if echo "$OUTPUT" | grep -qE "test .* FAILED|failures:|FAILED$|panicked at"; then
    echo "TEST FAILURE: STOP THE LINE" >&2
    echo "  Run: cargo make test" >&2
    echo "  Read the failure. Fix the code. Do not weaken the test." >&2
    exit 2
  fi

  # Command failure (no specific signal detected)
  if [[ "$EXIT_CODE" -ne 0 ]]; then
    echo "Command failed with exit code: $EXIT_CODE" >&2
    echo "  Run: cargo make timeout-check" >&2
    exit 0
  fi

  # HIGH ANDON: Warnings (advisory, not blocking)
  if echo "$OUTPUT" | grep -qE "warning:"; then
    echo "WARNING detected. Stop before merge." >&2
    echo "  Run: cargo make lint" >&2
    exit 0
  fi

  # HIGH ANDON: Clippy issues (advisory, not blocking)
  if echo "$OUTPUT" | grep -qE "clippy::"; then
    echo "CLIPPY issue detected. Stop before merge." >&2
    echo "  Run: cargo make lint" >&2
    exit 0
  fi

  # Success signals
  if echo "$COMMAND" | grep -q "cargo make test"; then
    if echo "$OUTPUT" | grep -qE "test result:.*ok"; then
      TEST_COUNT=$(echo "$OUTPUT" | grep -oE "[0-9]+ passed" | head -1 | grep -oE "[0-9]+")
      echo "Tests passed: ${TEST_COUNT:-unknown}" >&2
    fi
  fi

  exit 0
' bash "$COMMAND" "$EXIT_CODE" "$OUTPUT"

exit $?
