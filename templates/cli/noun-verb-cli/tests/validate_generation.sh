#!/usr/bin/env bash
#
# Validation script for noun-verb CLI generator
# Ensures generated CLIs compile, test, and run correctly
#

set -euo pipefail

GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m'

TEST_DIR="/tmp/ggen-cli-validation-$$"
FAILURES=0

echo "🧪 Validating noun-verb CLI generator..."
echo ""

# Test 1: Generate simple CLI
echo "Test 1: Generate single-noun CLI..."
./scripts/generate-noun-verb-cli.sh test-cli "resource" "$TEST_DIR/test1" &>/dev/null

if cd "$TEST_DIR/test1" && cargo build --quiet 2>&1 | grep -q "Finished"; then
    echo -e "${GREEN}✅ PASS${NC}: Single-noun CLI builds"
else
    echo -e "${RED}❌ FAIL${NC}: Single-noun CLI failed to build"
    ((FAILURES++))
fi

# Test 2: Generate multi-noun CLI
echo "Test 2: Generate multi-noun CLI..."
./scripts/generate-noun-verb-cli.sh multi-cli "user,project,team" "$TEST_DIR/test2" &>/dev/null

if cd "$TEST_DIR/test2" && cargo test --quiet 2>&1 | grep -q "test result: ok"; then
    echo -e "${GREEN}✅ PASS${NC}: Multi-noun CLI tests pass"
else
    echo -e "${RED}❌ FAIL${NC}: Multi-noun CLI tests failed"
    ((FAILURES++))
fi

# Test 3: Verify command execution
echo "Test 3: Verify command execution..."
cd "$TEST_DIR/test1"
if ./target/debug/test-cli --version | grep -q "0.1.0"; then
    echo -e "${GREEN}✅ PASS${NC}: CLI executes and shows version"
else
    echo -e "${RED}❌ FAIL${NC}: CLI failed to execute"
    ((FAILURES++))
fi

# Cleanup
rm -rf "$TEST_DIR"

echo ""
if [ $FAILURES -eq 0 ]; then
    echo -e "${GREEN}🎉 All validation tests passed!${NC}"
    exit 0
else
    echo -e "${RED}❌ $FAILURES test(s) failed${NC}"
    exit 1
fi
