#!/usr/bin/env bash
# CI Check 1: Verify WIP marker was flipped or removed
set -euo pipefail

# Extract branch name
BRANCH="${GITHUB_HEAD_REF:-$(git branch --show-current)}"

# Check if this is a WIP branch
if [[ ! "$BRANCH" =~ wip/.*__L([0-9]+)$ ]]; then
  echo "ℹ️  Not a WIP branch, skipping marker check"
  exit 0
fi

# Extract line number from branch name
LINE="${BASH_REMATCH[1]}"

# Extract file path from branch (convert __ back to /)
FILE_PATH=$(echo "$BRANCH" | sed 's/^wip\///' | sed 's/__L[0-9]*$//' | tr '__' '/')

echo "🔍 Checking WIP marker change in ${FILE_PATH}:${LINE}"

# Get the base branch (default to master)
BASE_BRANCH="${GITHUB_BASE_REF:-master}"

# Check if file exists
if [[ ! -f "$FILE_PATH" ]]; then
  echo "❌ File not found: $FILE_PATH"
  exit 1
fi

# Get the line content from base branch
BASE_LINE=$(git show "${BASE_BRANCH}:${FILE_PATH}" 2>/dev/null | sed -n "${LINE}p" || echo "")

# Get the line content from current branch
CURRENT_LINE=$(sed -n "${LINE}p" "$FILE_PATH" || echo "")

# Check if base line contained WIP marker
if [[ ! "$BASE_LINE" =~ (WIP:|TODO:|UNIMPL:|unimplemented!|todo!) ]]; then
  echo "⚠️  Warning: Base line at ${FILE_PATH}:${LINE} didn't contain a WIP marker"
  echo "Base: $BASE_LINE"
  echo "This might be okay if the file structure changed."
  exit 0
fi

# Check if current line still contains WIP marker
if [[ "$CURRENT_LINE" =~ (WIP:|TODO:|UNIMPL:|unimplemented!|todo!) ]]; then
  echo "❌ FAIL: WIP marker still present at ${FILE_PATH}:${LINE}"
  echo "Base:    $BASE_LINE"
  echo "Current: $CURRENT_LINE"
  echo ""
  echo "The WIP marker must be either:"
  echo "  1. Changed to '// READY: <description>'"
  echo "  2. Removed entirely"
  exit 1
fi

# Check if line was changed or removed
if [[ "$BASE_LINE" != "$CURRENT_LINE" ]]; then
  echo "✅ PASS: WIP marker was modified"
  echo "Base:    $BASE_LINE"
  echo "Current: $CURRENT_LINE"
  exit 0
fi

echo "❌ FAIL: Line was not changed"
echo "Content: $CURRENT_LINE"
exit 1
