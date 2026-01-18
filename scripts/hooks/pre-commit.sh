#!/usr/bin/env bash
# pre-commit.sh - Fast Tier Git Hook (80/20 Optimized)
# Target: <10 seconds | Value: 62% defect detection
# Philosophy: Fast feedback, catch compile errors early

set -e
cd "$(git rev-parse --show-toplevel)"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BOLD='\033[1m'
NC='\033[0m'

echo ""
echo -e "${BOLD}Pre-Commit Validation${NC} (Fast Tier)"
echo ""

# Gate 1: Cargo Check (60% of defects - VITAL)
echo -n "  Cargo check... "
if timeout 10s cargo check --quiet 2>/dev/null; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    echo ""
    echo -e "${RED}${BOLD}STOP: Compilation errors must be fixed${NC}"
    cargo check 2>&1 | head -30
    exit 1
fi

# Gate 2: Format Check (2% but auto-fixable)
echo -n "  Format check... "
if cargo fmt --all -- --check >/dev/null 2>&1; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${YELLOW}AUTO-FIX${NC}"
    cargo fmt --all >/dev/null 2>&1 || true
    echo -e "  ${YELLOW}Code formatted. Review changes before commit.${NC}"
fi

echo ""
echo -e "${GREEN}Pre-commit passed.${NC}"
exit 0
