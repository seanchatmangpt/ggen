#!/bin/bash
# Timeout: 3s
# Purpose: PostToolUse scan after writing test files.
#          Detects MOCK COMFORT and SHALLOW GREEN patterns.
#          Blocking (exit 2) for mockall imports. Advisory for others.

set +H
set -uo pipefail

# Read JSON to temp file
TMPFILE=$(mktemp)
trap 'rm -f "$TMPFILE"' EXIT
cat > "$TMPFILE"

[ ! -s "$TMPFILE" ] && exit 0

# Extract JSON fields with jq
FILE_PATH=$(jq -r '.file_path // empty' "$TMPFILE" 2>/dev/null)
[ -z "$FILE_PATH" ] && exit 0
echo "$FILE_PATH" | grep -qE '_test\.rs|/tests/|tests\.rs' || exit 0
[ -f "$FILE_PATH" ] || exit 0

FILE_CONTENT=$(cat "$FILE_PATH" 2>/dev/null || true)
[ -z "$FILE_CONTENT" ] && exit 0

# Check 1: Mock imports (exit 2 -- blocking)
if echo "$FILE_CONTENT" | grep -qE 'use[[:space:]]+mockall'; then
  echo "MOCK COMFORT: mockall use statement in $FILE_PATH" >&2
  echo "  Chicago TDD forbids mockall. Use real collaborators." >&2
  exit 2
fi

if echo "$FILE_CONTENT" | grep -qE '#\[automock\]'; then
  echo "MOCK COMFORT: #[automock] attribute in $FILE_PATH" >&2
  exit 2
fi

if echo "$FILE_CONTENT" | grep -qE 'mock![[:space:]]*\{'; then
  echo "MOCK COMFORT: mock! macro in $FILE_PATH" >&2
  exit 2
fi

# Check 2: Mock/Fake struct patterns (advisory)
MOCK_STRUCTS=$(echo "$FILE_CONTENT" | grep -oE 'struct[[:space:]]+(Mock|Fake)[A-Z][a-zA-Z0-9]*' | sed 's/struct[[:space:]]*//' || true)
if [ -n "$MOCK_STRUCTS" ]; then
  echo "MOCK COMFORT: mock/fake struct patterns in $FILE_PATH" >&2
  echo "$MOCK_STRUCTS" | while read -r s; do echo "  - $s" >&2; done
fi

# Check 3: No specific assertions (advisory)
TOTAL=$(echo "$FILE_CONTENT" | grep -oE 'assert!|assert_eq!|assert_ne!|assert_matches!' | wc -l | tr -d ' ' || true)
SPECIFIC=$(echo "$FILE_CONTENT" | grep -oE 'assert_eq!|assert_ne!|assert_matches!' | wc -l | tr -d ' ' || true)
if [ "$TOTAL" -gt 0 ] && [ "$SPECIFIC" -eq 0 ]; then
  echo "SHALLOW GREEN: no specific assertions in $FILE_PATH" >&2
  echo "  $TOTAL assert! calls but none use assert_eq!/assert_ne!/assert_matches!" >&2
fi

# Check 4: Test functions with zero assertions (advisory)
TEST_FNS=$(echo "$FILE_CONTENT" | grep -cE '#\[test\]|#\[tokio::test\]' || true)
if [ "$TEST_FNS" -gt 0 ] && [ "$TOTAL" -eq 0 ]; then
  echo "SHALLOW GREEN: $TEST_FNS test(s) with zero assertions in $FILE_PATH" >&2
fi

exit 0
