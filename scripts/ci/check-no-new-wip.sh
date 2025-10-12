#!/usr/bin/env bash
# CI Check 3: Verify no new WIP markers added outside touched files
set -euo pipefail

echo "🔍 Checking for new WIP markers in modified files"

# Get base branch
BASE_BRANCH="${GITHUB_BASE_REF:-master}"

# Get list of modified files
MODIFIED_FILES=$(git diff --name-only "${BASE_BRANCH}...HEAD" || git diff --name-only HEAD~1)

# Extract branch name and file path
BRANCH="${GITHUB_HEAD_REF:-$(git branch --show-current)}"
CLAIMED_FILE=""

if [[ "$BRANCH" =~ wip/.*__L([0-9]+)$ ]]; then
  CLAIMED_FILE=$(echo "$BRANCH" | sed 's/^wip\///' | sed 's/__L[0-9]*$//' | tr '__' '/')
fi

# Check each modified file for new WIP markers
HAS_NEW_WIP=0

while IFS= read -r file; do
  # Skip if file doesn't exist or is not a code file
  [[ ! -f "$file" ]] && continue
  [[ "$file" =~ \.(md|txt|json|toml|yml|yaml)$ ]] && continue

  # Skip the claimed file (it should have WIP changes)
  [[ "$file" == "$CLAIMED_FILE" ]] && continue

  # Get WIP markers in base branch
  BASE_WIP=$(git show "${BASE_BRANCH}:${file}" 2>/dev/null | grep -nE "(WIP:|TODO:|UNIMPL:|unimplemented!|todo!)" || true)

  # Get WIP markers in current branch
  CURRENT_WIP=$(grep -nE "(WIP:|TODO:|UNIMPL:|unimplemented!|todo!)" "$file" || true)

  # Compare counts
  BASE_COUNT=$(echo "$BASE_WIP" | grep -c ^ || echo 0)
  CURRENT_COUNT=$(echo "$CURRENT_WIP" | grep -c ^ || echo 0)

  if [[ "$CURRENT_COUNT" -gt "$BASE_COUNT" ]]; then
    echo "⚠️  Warning: New WIP markers added in $file"
    echo "Base: $BASE_COUNT markers, Current: $CURRENT_COUNT markers"
    echo ""
    echo "New markers:"
    # Show new markers (rough diff)
    diff <(echo "$BASE_WIP") <(echo "$CURRENT_WIP") | grep "^>" || true
    echo ""
    HAS_NEW_WIP=1
  fi
done <<< "$MODIFIED_FILES"

if [[ "$HAS_NEW_WIP" -eq 0 ]]; then
  echo "✅ PASS: No new WIP markers added outside claimed file"
  exit 0
fi

echo ""
echo "⚠️  New WIP markers were added outside the claimed file."
echo "This is allowed but should be intentional."
echo "Consider creating separate tasks for these WIP items."
exit 0  # Warning only, don't fail
