#!/usr/bin/env bash
# pre-commit.sh - Fast Tier Git Hook
# Target: <10 seconds | Value: 62% defect detection
# Philosophy: Fast feedback, catch compile errors early

set -e
cd "$(git rev-parse --show-toplevel)"

# Only run validation when on the default branch (main)
CURRENT_BRANCH=$(git symbolic-ref --short HEAD)
if [ "$CURRENT_BRANCH" != "main" ]; then
    exit 0
fi

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
BOLD='\033[1m'
NC='\033[0m'

echo ""
echo -e "${BOLD}Pre-Commit Validation${NC} (Fast Tier)"
echo ""

# Gate 1: Check (60% of defects - VITAL)
echo -n "  Check... "
if just check >/dev/null 2>&1; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    echo ""
    echo -e "${RED}${BOLD}STOP: Compilation errors must be fixed${NC}"
    just check 2>&1 | head -30
    exit 1
fi

# Gate 2: Format check (2% but ensures consistency)
echo -n "  Format check... "
if just fmt-check >/dev/null 2>&1; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    echo ""
    echo -e "${RED}${BOLD}STOP: Code not formatted. Run 'just fmt' before committing.${NC}"
    exit 1
fi

echo ""
echo -e "${GREEN}Pre-commit passed.${NC}"
exit 0
