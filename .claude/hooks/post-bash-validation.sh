#!/bin/bash
# Timeout: 3s
# Purpose: GATE on compiler errors and test failures.
#          Exit 2 on critical Andon signals to block operations.
#          Exit 0 on passes and warnings.

set -euo pipefail

COMMAND="${1:-}"
EXIT_CODE="${2:-0}"
OUTPUT="${3:-}"

timeout 3s bash -c '
  COMMAND="$1"
  EXIT_CODE="$2"
  OUTPUT="$3"

  # Handle empty output
  if [[ -z "$OUTPUT" ]]; then
    exit 0
  fi

  # GATE: Compiler errors detected in output (even if exit code was 0)
  if echo "$OUTPUT" | grep -qE "error\[E[0-9]+\]"; then
    echo "COMPILER ERROR: STOP THE LINE" >&2
    echo "  Fix all error[E...] before proceeding" >&2
    echo "  Run: cargo make check" >&2
    exit 2
  fi

  # GATE: Test failures detected in output
  if echo "$OUTPUT" | grep -qE "test .* FAILED|failures:"; then
    echo "TEST FAILURE: STOP THE LINE" >&2
    echo "  Fix all failing tests before proceeding" >&2
    echo "  Run: cargo make test" >&2
    exit 2
  fi

  # GATE: Panic detected
  if echo "$OUTPUT" | grep -qE "panicked at"; then
    echo "PANIC: STOP THE LINE" >&2
    echo "  Process panicked -- investigate and fix root cause" >&2
    exit 2
  fi

  # Non-zero exit code (not a specific pattern match, general failure)
  if [[ "$EXIT_CODE" -ne 0 ]]; then
    echo "Command failed with exit code: $EXIT_CODE" >&2
    if echo "$COMMAND" | grep -q "cargo make"; then
      echo "  Run: cargo make timeout-check" >&2
      echo "  Check: Andon signals in output" >&2
    fi
    exit 0
  fi

  # WARNING: Compiler warnings (non-blocking)
  if echo "$OUTPUT" | grep -qE "warning:"; then
    echo "WARNING: Compiler warning detected" >&2
    echo "  Resolve before release: cargo make lint" >&2
  fi

  # WARNING: Clippy issues (non-blocking)
  if echo "$OUTPUT" | grep -qE "clippy::"; then
    echo "WARNING: Clippy issue detected" >&2
    echo "  Resolve before release: cargo make lint" >&2
  fi

  # INFO: Successful test run
  if echo "$COMMAND" | grep -q "cargo make test"; then
    if echo "$OUTPUT" | grep -qE "test result:.*ok"; then
      TEST_COUNT=$(echo "$OUTPUT" | grep -oE "[0-9]+ passed" | head -1 | grep -oE "[0-9]+" || true)
      echo "Tests passed: ${TEST_COUNT:-unknown} tests" >&2
    fi
  fi

  # INFO: Successful compilation check
  if echo "$COMMAND" | grep -q "cargo make check"; then
    if ! echo "$OUTPUT" | grep -qE "error"; then
      echo "Compilation check passed" >&2
    fi
  fi

  # INFO: Successful lint
  if echo "$COMMAND" | grep -q "cargo make lint"; then
    if ! echo "$OUTPUT" | grep -qE "(error|warning)"; then
      echo "Linting passed" >&2
    fi
  fi

  # INFO: ggen sync
  if echo "$COMMAND" | grep -q "ggen sync"; then
    if echo "$OUTPUT" | grep -qE "(success|completed)"; then
      echo "ggen sync completed successfully" >&2
    elif echo "$OUTPUT" | grep -qE "(conflict|error|failed)"; then
      echo "ggen sync reported issues -- check output" >&2
    fi
  fi

  # WARNING: Timeout detected
  if echo "$OUTPUT" | grep -qE "timed out"; then
    echo "WARNING: Timeout detected -- command may have been interrupted" >&2
    echo "  Consider increasing timeout or optimizing operation" >&2
  fi

  exit 0
' bash "$COMMAND" "$EXIT_CODE" "$OUTPUT"

exit $?
