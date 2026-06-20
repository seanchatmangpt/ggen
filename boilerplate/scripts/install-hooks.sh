#!/usr/bin/env bash
# Install git hooks from .git-hooks/ into the local .git/hooks/ directory.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && git rev-parse --show-toplevel)"
HOOKS_SRC="$REPO_ROOT/boilerplate/.git-hooks"
HOOKS_DEST="$REPO_ROOT/.git/hooks"

for hook in pre-commit pre-push commit-msg; do
    src="$HOOKS_SRC/$hook"
    dest="$HOOKS_DEST/$hook"
    if [ -f "$src" ]; then
        cp "$src" "$dest"
        chmod +x "$dest"
        echo "Installed: $dest"
    fi
done

echo "Git hooks installed successfully."
