#!/bin/bash
# Timeout: 2s
# Purpose: PreToolUse gate for Edit/Write on test files.
#          Detects TEST MURDER, MOCK COMFORT, SHALLOW GREEN.

set +H
set -uo pipefail

# Read JSON to temp file (avoids bash ! escaping issues)
TMPFILE=$(mktemp)
trap 'rm -f "$TMPFILE"' EXIT
cat > "$TMPFILE"

[ ! -s "$TMPFILE" ] && exit 0

# Extract JSON fields with jq
FILE_PATH=$(jq -r '.file_path // empty' "$TMPFILE" 2>/dev/null)
[ -z "$FILE_PATH" ] && exit 0

# Only gate test files
echo "$FILE_PATH" | grep -qE '_test\.rs|/tests/|tests\.rs' || exit 0

# Extract content (Write tool) or new_string/old_string (Edit tool)
CONTENT=$(jq -r '.content // empty' "$TMPFILE" 2>/dev/null)
[ -z "$CONTENT" ] && CONTENT=$(jq -r '.new_string // empty' "$TMPFILE" 2>/dev/null)
OLD_STRING=$(jq -r '.old_string // empty' "$TMPFILE" 2>/dev/null)
[ -z "$CONTENT" ] && [ -z "$OLD_STRING" ] && exit 0

# Check 1: Mock imports (exit 2)
if echo "$CONTENT" | grep -qE 'use[[:space:]]+mockall|#[[automock]]|mock![[:space:]]*\{'; then
  echo "TEST MURDER: mockall import in test file blocked" >&2
  echo "  Chicago TDD requires real collaborators, not mocks" >&2
  echo "  See: .claude/rules/rust/testing-forbidden.md" >&2
  exit 2
fi

# Check 2: Vacuous assertions (exit 2)
VACUOUS=false
if echo "$CONTENT" | grep -qE 'assert!\([^)]+\.is_ok\(\)\)'; then
  VACUOUS=true
fi
if echo "$CONTENT" | grep -qE 'assert!\([^)]+\.is_some\(\)\)'; then
  VACUOUS=true
fi

if [ "$VACUOUS" = true ]; then
  SPECIFIC=$(echo "$CONTENT" | grep -oE 'assert_eq!|assert_ne!|assert_matches!' | wc -l | tr -d ' ' || true)
  if [ "$SPECIFIC" -eq 0 ]; then
    echo "SHALLOW GREEN: vacuous assertion detected" >&2
    echo "  assert!(result.is_ok()) proves nothing about domain behavior" >&2
    echo "  Add assert_eq! or assert_ne! for actual value verification" >&2
    exit 2
  fi
fi

# Check 3: Assertion deletion in edits (exit 2)
if [ -n "$OLD_STRING" ] && [ -n "$CONTENT" ]; then
  OLD_COUNT=$(echo "$OLD_STRING" | grep -oE 'assert_eq!|assert_ne!|assert_matches!' | wc -l | tr -d ' ' || true)
  NEW_COUNT=$(echo "$CONTENT" | grep -oE 'assert_eq!|assert_ne!|assert_matches!' | wc -l | tr -d ' ' || true)
  if [ "$OLD_COUNT" -gt "$NEW_COUNT" ]; then
    echo "TEST MURDER: assertion deletion detected" >&2
    echo "  Old: $OLD_COUNT assertion(s), new: $NEW_COUNT" >&2
    exit 2
  fi
fi

# Check 4: #[ignore] without reason (exit 2)
if echo "$CONTENT" | grep -qE '#\[ignore\]'; then
  if ! echo "$CONTENT" | grep -qE '#\[ignore\][^#]*//'; then
    echo "TEST MURDER: #[ignore] added without reason comment" >&2
    echo "  Every #[ignore] must document WHY the test is skipped" >&2
    exit 2
  fi
fi

exit 0
