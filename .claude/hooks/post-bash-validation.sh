#!/bin/bash
# Timeout: 3s
# Purpose: Verify bash commands succeeded and check for Andon signals

set -euo pipefail

COMMAND="${1:-}"
EXIT_CODE="${2:-0}"
OUTPUT="${3:-}"

timeout 3s bash -c '
  COMMAND="$1"
  EXIT_CODE="$2"
  OUTPUT="$3"

  # Check if command failed
  if [[ "$EXIT_CODE" -ne 0 ]]; then
    echo "⚠️  Command failed with exit code: $EXIT_CODE" >&2

    # Provide context for common failures
    if echo "$COMMAND" | grep -q "cargo make"; then
      echo "   → Run: cargo make timeout-check" >&2
      echo "   → Check: Andon signals in output" >&2
    fi

    exit 0  # Don'\''t fail the hook, just warn
  fi

  # 🔴 CRITICAL ANDON SIGNALS - Compiler errors
  if echo "$OUTPUT" | grep -qE "error(\[E[0-9]+\]|:)"; then
    echo "🔴 CRITICAL ANDON SIGNAL: Compiler error detected!" >&2
    echo "   → STOP THE LINE - Fix immediately" >&2
    echo "   → Run: cargo make check" >&2
    exit 0
  fi

  # 🔴 CRITICAL ANDON SIGNALS - Test failures
  if echo "$OUTPUT" | grep -qE "test .* FAILED|failures:|FAILED|panicked at"; then
    echo "🔴 CRITICAL ANDON SIGNAL: Test failure detected!" >&2
    echo "   → STOP THE LINE - Fix immediately" >&2
    echo "   → Run: cargo make test" >&2
    exit 0
  fi

  # 🟡 HIGH ANDON SIGNALS - Warnings
  if echo "$OUTPUT" | grep -qE "warning:"; then
    echo "🟡 HIGH ANDON SIGNAL: Warning detected!" >&2
    echo "   → STOP THE LINE before release" >&2
    echo "   → Run: cargo make lint" >&2
    exit 0
  fi

  # 🟡 HIGH ANDON SIGNALS - Clippy errors
  if echo "$OUTPUT" | grep -qE "clippy::"; then
    echo "🟡 HIGH ANDON SIGNAL: Clippy issue detected!" >&2
    echo "   → STOP THE LINE before release" >&2
    echo "   → Run: cargo make lint" >&2
    exit 0
  fi

  # Check for successful cargo make commands
  if echo "$COMMAND" | grep -q "cargo make test"; then
    if echo "$OUTPUT" | grep -qE "test result:.*ok"; then
      TEST_COUNT=$(echo "$OUTPUT" | grep -oE "[0-9]+ passed" | head -1 | grep -oE "[0-9]+")
      echo "✅ Tests passed: ${TEST_COUNT:-unknown} tests" >&2
    fi
  fi

  # Check for successful cargo make check
  if echo "$COMMAND" | grep -q "cargo make check"; then
    if ! echo "$OUTPUT" | grep -qE "error"; then
      echo "✅ Compilation check passed" >&2
    fi
  fi

  # Check for successful cargo make lint
  if echo "$COMMAND" | grep -q "cargo make lint"; then
    if ! echo "$OUTPUT" | grep -qE "(error|warning)"; then
      echo "✅ Linting passed" >&2
    fi
  fi

  # Validate ggen sync operations
  if echo "$COMMAND" | grep -q "ggen sync"; then
    if echo "$OUTPUT" | grep -qE "(✓|success|completed)"; then
      echo "✅ ggen sync completed successfully" >&2
    elif echo "$OUTPUT" | grep -qE "(conflict|error|failed)"; then
      echo "⚠️  ggen sync reported issues - check output" >&2
    fi
  fi

  # Check for timeout issues
  if echo "$OUTPUT" | grep -qE "timed out|timeout"; then
    echo "⚠️  Timeout detected - command may have been interrupted" >&2
    echo "   → Consider increasing timeout or optimizing operation" >&2
  fi

  # Success - validation complete
  exit 0
' bash "$COMMAND" "$EXIT_CODE" "$OUTPUT"

exit $?
