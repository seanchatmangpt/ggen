#!/bin/bash
# Post-Tool-Use - Andon Signal Detection & Receipt Generation
set -euo pipefail
TOOL_OUTPUT="${1:-}"
FILE_PATH="${2:-}"

# Andon Signal Detection
detect_andon() {
  local out="$1"
  # RED: Stop immediately
  if echo "$out" | grep -qE "error\[E[0-9]+\]|FAILED|panicked"; then
    echo "🔴 ANDON RED: Error detected - STOP"
    return 2
  fi
  # YELLOW: Investigate
  if echo "$out" | grep -qE "warning:|clippy::"; then
    echo "🟡 ANDON YELLOW: Warning - investigate"
  fi
  # GREEN: Success
  if echo "$out" | grep -qE "test result: ok|0 violations"; then
    echo "🟢 ANDON GREEN: Success"
  fi
}

# Auto-format Rust files
if [[ "$FILE_PATH" =~ \.rs$ ]] && command -v rustfmt &>/dev/null; then
  rustfmt --edition 2021 "$FILE_PATH" 2>/dev/null || true
  echo "✓ Auto-formatted: $FILE_PATH"
fi

# Detect andon signals in output
[[ -n "$TOOL_OUTPUT" ]] && detect_andon "$TOOL_OUTPUT"

# Log for audit trail
echo "$(date -Is) POST: $FILE_PATH" >> .claude/audit.log 2>/dev/null || true
exit 0
