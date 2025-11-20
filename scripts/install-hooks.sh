#!/usr/bin/env bash
# Install Git hooks for ggen project

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GIT_DIR="$(git rev-parse --git-dir)"
HOOKS_DIR="$GIT_DIR/hooks"

echo "📦 Installing Git hooks..."

# Create hooks directory if it doesn't exist
mkdir -p "$HOOKS_DIR"

# Install pre-commit hook
echo "  Installing pre-commit hook..."
ln -sf "$SCRIPT_DIR/pre-commit-hook.sh" "$HOOKS_DIR/pre-commit"
chmod +x "$HOOKS_DIR/pre-commit"

echo "✅ Git hooks installed successfully!"
echo ""
echo "Hooks installed:"
echo "  - pre-commit: Runs validation checks before commit"
echo ""
echo "To skip hooks temporarily: git commit --no-verify"
