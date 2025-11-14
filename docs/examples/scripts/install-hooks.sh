#!/usr/bin/env bash
set -euo pipefail

# Install git hooks for automatic code regeneration
# Usage: ./install-hooks.sh

PROJECT_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
HOOKS_SRC="${PROJECT_ROOT}/docs/examples/hooks"
HOOKS_DEST="${PROJECT_ROOT}/.git/hooks"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo -e "${GREEN}Installing git hooks for ontology-driven code generation...${NC}"

# Check if .git directory exists
if [ ! -d "$HOOKS_DEST" ]; then
    echo -e "${YELLOW}Error: .git/hooks directory not found${NC}"
    echo "Are you in a git repository?"
    exit 1
fi

# Install pre-commit hook
if [ -f "$HOOKS_SRC/pre-commit.sh" ]; then
    cp "$HOOKS_SRC/pre-commit.sh" "$HOOKS_DEST/pre-commit"
    chmod +x "$HOOKS_DEST/pre-commit"
    echo -e "${GREEN}✓ Installed pre-commit hook${NC}"
else
    echo -e "${YELLOW}⚠ pre-commit.sh not found in $HOOKS_SRC${NC}"
fi

# Install post-merge hook
if [ -f "$HOOKS_SRC/post-merge.sh" ]; then
    cp "$HOOKS_SRC/post-merge.sh" "$HOOKS_DEST/post-merge"
    chmod +x "$HOOKS_DEST/post-merge"
    echo -e "${GREEN}✓ Installed post-merge hook${NC}"
else
    echo -e "${YELLOW}⚠ post-merge.sh not found in $HOOKS_SRC${NC}"
fi

# Make regeneration script executable
REGEN_SCRIPT="${PROJECT_ROOT}/docs/examples/scripts/regenerate-from-ontology.sh"
if [ -f "$REGEN_SCRIPT" ]; then
    chmod +x "$REGEN_SCRIPT"
    echo -e "${GREEN}✓ Made regeneration script executable${NC}"
fi

echo ""
echo -e "${GREEN}Installation complete!${NC}"
echo ""
echo "Installed hooks:"
ls -lh "$HOOKS_DEST" | grep -E '(pre-commit|post-merge)' || true
echo ""
echo "Test the hooks with:"
echo "  1. Modify an ontology file: vim docs/examples/ontology/task-management.ttl"
echo "  2. Stage and commit: git add . && git commit -m 'test: hook test'"
echo ""
echo "To uninstall, run:"
echo "  rm .git/hooks/pre-commit .git/hooks/post-merge"
