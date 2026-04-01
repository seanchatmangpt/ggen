#!/bin/bash
# Timeout: 2s
# Purpose: Stop hook -- remind about evidence requirements when uncommitted work exists.
#          Non-blocking (always exit 0) since session history is not available to shell.

set -euo pipefail

timeout 2s bash -c '
  # Check for uncommitted work (staged + unstaged)
  STAGED=$(git diff --cached --stat 2>/dev/null | wc -l | tr -d " " || echo "0")
  UNSTAGED=$(git diff --stat 2>/dev/null | wc -l | tr -d " " || echo "0")
  UNTRACKED=$(git ls-files --others --exclude-standard 2>/dev/null | wc -l | tr -d " " || echo "0")

  TOTAL=$((STAGED + UNSTAGED))
  TOTAL_WITH_UNTRACKED=$((STAGED + UNSTAGED + UNTRACKED))

  if [[ "$TOTAL" -eq 0 ]]; then
    # Clean tree, nothing to remind about
    exit 0
  fi

  echo "" >&2
  echo "SESSION CLOSE: $TOTAL file(s) changed, $UNTRACKED untracked" >&2
  echo "Evidence requirements before claiming completion:" >&2
  echo "" >&2
  echo "  1. Mechanical proof:" >&2
  echo "     cargo make check && cargo make test && cargo make lint" >&2
  echo "" >&2
  echo "  2. For LLM/external service features (PROVEN evidence tier):" >&2
  echo "     OTEL spans must exist: llm.complete, llm.model, llm.total_tokens" >&2
  echo "     Verify: RUST_LOG=trace,ggen_ai=trace cargo test <name> 2>&1 | grep llm" >&2
  echo "" >&2
  echo "  3. Do not claim completion without running checks" >&2
  echo "     Tests passing is NOT sufficient for LLM features" >&2
  echo "" >&2

  if [[ "$UNTRACKED" -gt 0 ]]; then
    echo "  NOTE: $UNTRACKED untracked file(s) detected" >&2
    echo "     Review and commit or gitignore before closing" >&2
    echo "" >&2
  fi

  exit 0
' bash

exit $?
