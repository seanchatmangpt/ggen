#!/usr/bin/env bash
# CI Check 2: Verify no locks remain
set -euo pipefail

echo "🔍 Checking for remaining WIP locks in .wiplocks/"

# Check if .wiplocks directory exists
if [[ ! -d ".wiplocks" ]]; then
  echo "✅ PASS: .wiplocks directory doesn't exist"
  exit 0
fi

# Count lock directories (excluding .gitignore)
LOCK_COUNT=$(find .wiplocks -mindepth 1 -type d 2>/dev/null | wc -l | tr -d ' ')

if [[ "$LOCK_COUNT" -eq 0 ]]; then
  echo "✅ PASS: No locks found in .wiplocks/"
  exit 0
fi

echo "❌ FAIL: Found ${LOCK_COUNT} remaining lock(s) in .wiplocks/"
echo ""
echo "Remaining locks:"
find .wiplocks -mindepth 1 -type d -exec sh -c 'echo "  - $(basename {}) ($(cat {}/owner 2>/dev/null || echo "no owner"))"' \;
echo ""
echo "Locks must be released before merging. Run:"
echo "  ./scripts/wip-lock release <path>"
exit 1
