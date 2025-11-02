#!/bin/bash
# Auto Add, Commit, Push script for Cursor
# Usage: /acp [commit message]
# If no message provided, generates one based on changes

set -e

# Get the repository root
REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT"

# Stage all changes
echo "📦 Staging all changes..."
git add -A

# Check if there are any changes to commit
if git diff --staged --quiet; then
    echo "ℹ️  No changes to commit."
    exit 0
fi

# Generate commit message
if [ -z "$1" ]; then
    # Auto-generate commit message based on changes
    STAGED_FILES=$(git diff --staged --name-status | head -5 | awk '{print $2}')
    
    # Analyze changes to generate message
    MESSAGE_TYPE="chore"
    MESSAGE_BODY="update"
    
    # Check for common patterns
    if echo "$STAGED_FILES" | grep -q "README\|CHANGELOG\|docs"; then
        MESSAGE_TYPE="docs"
        MESSAGE_BODY="update documentation"
    elif echo "$STAGED_FILES" | grep -q "\.rs$"; then
        MESSAGE_TYPE="refactor"
        MESSAGE_BODY="code changes"
    elif echo "$STAGED_FILES" | grep -q "Cargo\.toml\|Cargo\.lock"; then
        MESSAGE_TYPE="chore"
        MESSAGE_BODY="update dependencies"
    fi
    
    COMMIT_MSG="$MESSAGE_TYPE: $MESSAGE_BODY"
else
    COMMIT_MSG="$1"
fi

# Commit
echo "💾 Committing with message: $COMMIT_MSG"
git commit -m "$COMMIT_MSG"

# Push
echo "🚀 Pushing to remote..."
CURRENT_BRANCH=$(git branch --show-current)
git push origin "$CURRENT_BRANCH"

echo "✅ Done! All changes pushed to $CURRENT_BRANCH"


