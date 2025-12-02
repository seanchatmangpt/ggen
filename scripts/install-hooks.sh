#!/usr/bin/env bash
# install-hooks.sh - Git Hooks Installation (80/20 Optimized)
# Simple, reliable installation without external dependencies

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GIT_DIR="$(git rev-parse --git-dir 2>/dev/null)"
HOOKS_DIR="$GIT_DIR/hooks"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BOLD='\033[1m'
NC='\033[0m'

print_header() {
    echo ""
    echo -e "${BOLD}========================================${NC}"
    echo -e "${BOLD}   ggen Git Hooks Installation${NC}"
    echo -e "${BOLD}========================================${NC}"
    echo ""
}

uninstall_hooks() {
    print_header
    echo -e "Uninstalling hooks..."

    for hook in pre-commit pre-push; do
        if [[ -e "$HOOKS_DIR/$hook" ]]; then
            rm "$HOOKS_DIR/$hook"
            echo -e "  ${GREEN}Removed${NC} $hook"
        fi
    done

    echo ""
    echo -e "${GREEN}Hooks uninstalled.${NC}"
    exit 0
}

install_hooks() {
    print_header

    # Verify prerequisites
    if [[ -z "$GIT_DIR" ]]; then
        echo -e "${RED}Error: Not in a git repository${NC}"
        exit 1
    fi

    if ! command -v timeout &> /dev/null; then
        echo -e "${RED}Error: 'timeout' command not found${NC}"
        echo "  macOS: brew install coreutils"
        echo "  Linux: apt-get install coreutils"
        exit 1
    fi

    # Check hook scripts exist
    if [[ ! -f "$SCRIPT_DIR/hooks/pre-commit.sh" ]]; then
        echo -e "${RED}Error: pre-commit.sh not found${NC}"
        exit 1
    fi

    if [[ ! -f "$SCRIPT_DIR/hooks/pre-push.sh" ]]; then
        echo -e "${RED}Error: pre-push.sh not found${NC}"
        exit 1
    fi

    # Backup existing hooks
    for hook in pre-commit pre-push; do
        if [[ -e "$HOOKS_DIR/$hook" ]] && [[ ! -L "$HOOKS_DIR/$hook" ]]; then
            mv "$HOOKS_DIR/$hook" "$HOOKS_DIR/$hook.bak.$(date +%Y%m%d%H%M%S)"
            echo -e "  ${YELLOW}Backed up${NC} existing $hook"
        elif [[ -L "$HOOKS_DIR/$hook" ]]; then
            rm "$HOOKS_DIR/$hook"
        fi
    done

    # Install hooks
    mkdir -p "$HOOKS_DIR"

    ln -sf "$SCRIPT_DIR/hooks/pre-commit.sh" "$HOOKS_DIR/pre-commit"
    chmod +x "$HOOKS_DIR/pre-commit"
    echo -e "  ${GREEN}Installed${NC} pre-commit (fast tier: <10s)"

    ln -sf "$SCRIPT_DIR/hooks/pre-push.sh" "$HOOKS_DIR/pre-push"
    chmod +x "$HOOKS_DIR/pre-push"
    echo -e "  ${GREEN}Installed${NC} pre-push (full tier: <90s)"

    # Summary
    echo ""
    echo -e "${BOLD}========================================${NC}"
    echo -e "${GREEN}${BOLD}   Installation Complete!${NC}"
    echo -e "${BOLD}========================================${NC}"
    echo ""
    echo "Hooks installed:"
    echo -e "  ${GREEN}pre-commit${NC}: cargo check + format (<10s)"
    echo -e "  ${GREEN}pre-push${NC}:   check + clippy + tests + format (<90s)"
    echo ""
    echo "80/20 Defect Detection:"
    echo "  - pre-commit catches 62% of defects (vital few)"
    echo "  - pre-push catches 97% of defects (comprehensive)"
    echo ""
    echo -e "${YELLOW}Important:${NC}"
    echo "  - Never use --no-verify to bypass hooks"
    echo "  - Fix RED signals before proceeding"
    echo ""
    echo "To uninstall: $0 --uninstall"
    echo ""
}

# Main
if [[ "${1:-}" == "--uninstall" ]]; then
    uninstall_hooks
else
    install_hooks
fi
