#!/bin/bash
# Release Git State Validation
# FMEA Fix: Prevents release with uncommitted changes (RPN 504)

set -euo pipefail

echo "🔍 Validating git state for release..."

# Check for uncommitted changes
if ! git diff --quiet || ! git diff --cached --quiet; then
    echo "❌ ERROR: Uncommitted changes detected"
    echo ""
    echo "Git status:"
    git status --short
    echo ""
    echo "Please commit or stash all changes before release."
    exit 1
fi

# Check for untracked files (excluding build artifacts)
untracked=$(git ls-files --others --exclude-standard | grep -v "^target/" | grep -v "^node_modules/" | grep -v "^\.git/" || true)
if [ -n "$untracked" ]; then
    echo "⚠️  WARNING: Untracked files detected (excluding build artifacts):"
    echo "$untracked" | head -10
    echo ""
    echo "Please review and commit or add to .gitignore before release."
    exit 1
fi

# Check for incomplete work files
incomplete=$(find . -name "*.new" -o -name "*WIP*" -o -name "*.tmp" 2>/dev/null | grep -v "^\./target/" | grep -v "^\./node_modules/" | grep -v "^\./\.git/" || true)
if [ -n "$incomplete" ]; then
    echo "❌ ERROR: Incomplete work files detected:"
    echo "$incomplete"
    echo ""
    echo "Please remove or complete these files before release."
    exit 1
fi

echo "✅ Git state is clean - ready for release"
exit 0


