#!/usr/bin/env bash
# Install git hooks from .git-hooks/ into the local .git/hooks/ directory.
# Run once after cloning/initialising the project as a standalone git repo.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
HOOKS_SRC="$PROJECT_ROOT/.git-hooks"
HOOKS_DEST="$PROJECT_ROOT/.git/hooks"

if [ ! -d "$HOOKS_DEST" ]; then
    echo "Error: $HOOKS_DEST not found." >&2
    echo "This script must be run from within the project's own git repository." >&2
    echo "If this boilerplate is embedded inside another repo, initialise a" >&2
    echo "standalone checkout first: git clone <url> && cd <dir> && ./scripts/install-hooks.sh" >&2
    exit 1
fi

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
