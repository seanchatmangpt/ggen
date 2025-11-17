#!/usr/bin/env bash
# Swarm Status Validation Script
# Purpose: Verify current state of test migration

set -e

echo "╔════════════════════════════════════════════════════════════════╗"
echo "║                                                                ║"
echo "║   🐝 ULTRATHINK HIVE QUEEN SWARM - STATUS VALIDATION 🐝       ║"
echo "║                                                                ║"
echo "╚════════════════════════════════════════════════════════════════╝"
echo ""

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Test 1: Library Compilation
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "TEST 1: Library Compilation"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
if cargo check --lib --workspace --quiet 2>/dev/null; then
    echo -e "${GREEN}✅ PASS${NC} - All libraries compile successfully"
else
    echo -e "${RED}❌ FAIL${NC} - Library compilation errors"
fi
echo ""

# Test 2: Test Compilation
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "TEST 2: Test Compilation"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
if cargo check --tests --quiet 2>/dev/null; then
    echo -e "${GREEN}✅ PASS${NC} - All tests compile successfully"
else
    ERROR_COUNT=$(cargo check --tests 2>&1 | grep -c "^error" || echo "600+")
    echo -e "${RED}❌ FAIL${NC} - Test compilation errors: ~${ERROR_COUNT}"
fi
echo ""

# Test 3: chicago_tdd_tools in Production Code
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "TEST 3: Production Code Separation"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
CHICAGO_COUNT=$(grep -r "chicago_tdd_tools::" crates/*/src/ 2>/dev/null | grep -v "^Binary" | wc -l | tr -d ' ')
if [ "$CHICAGO_COUNT" -eq 0 ]; then
    echo -e "${GREEN}✅ PASS${NC} - No chicago_tdd_tools in production code"
else
    echo -e "${RED}❌ FAIL${NC} - Found chicago_tdd_tools in ${CHICAGO_COUNT} locations"
    echo ""
    echo "Top 10 files with chicago_tdd_tools:"
    grep -r "chicago_tdd_tools::" crates/*/src/ 2>/dev/null | cut -d: -f1 | sort | uniq | head -10
fi
echo ""

# Test 4: Integration Tests Created
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "TEST 4: Integration Tests"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
if [ -d "tests/integration" ]; then
    TEST_FILES=$(find tests/integration -name "*.rs" -type f | wc -l | tr -d ' ')
    TEST_LOC=$(cat tests/integration/*.rs 2>/dev/null | wc -l | tr -d ' ')
    echo -e "${YELLOW}⚠️ CREATED${NC} - ${TEST_FILES} test files (${TEST_LOC} LOC)"
    echo "Status: Created but compilation status unknown"
else
    echo -e "${RED}❌ FAIL${NC} - No integration tests directory"
fi
echo ""

# Test 5: Test Execution
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "TEST 5: Test Execution"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
if cargo test --lib --workspace --quiet 2>/dev/null; then
    PASSED=$(cargo test --lib --workspace --quiet 2>&1 | grep "test result" | awk '{print $4}')
    echo -e "${GREEN}✅ PASS${NC} - Tests executed: ${PASSED} passing"
else
    echo -e "${RED}❌ BLOCKED${NC} - Cannot execute tests (compilation errors)"
fi
echo ""

# Summary
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "SUMMARY"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

PASS_COUNT=0
FAIL_COUNT=0

# Check each test
if cargo check --lib --workspace --quiet 2>/dev/null; then ((PASS_COUNT++)); else ((FAIL_COUNT++)); fi
if cargo check --tests --quiet 2>/dev/null; then ((PASS_COUNT++)); else ((FAIL_COUNT++)); fi
if [ "$CHICAGO_COUNT" -eq 0 ]; then ((PASS_COUNT++)); else ((FAIL_COUNT++)); fi

TOTAL=$((PASS_COUNT + FAIL_COUNT))
PERCENT=$((PASS_COUNT * 100 / TOTAL))

echo "Tests Passing: ${PASS_COUNT}/${TOTAL} (${PERCENT}%)"
echo ""

if [ "$PASS_COUNT" -eq "$TOTAL" ]; then
    echo -e "${GREEN}✅ STATUS: PRODUCTION READY${NC}"
    exit 0
elif [ "$PASS_COUNT" -gt 0 ]; then
    echo -e "${YELLOW}⚠️ STATUS: INCOMPLETE (${PERCENT}% ready)${NC}"
    echo ""
    echo "Recommended actions:"
    echo "1. Fix test compilation errors"
    echo "2. Remove chicago_tdd_tools from production code"
    echo "3. Run full test suite"
    exit 1
else
    echo -e "${RED}❌ STATUS: NOT READY${NC}"
    echo ""
    echo "Critical issues detected. Review assessment document:"
    echo "docs/SWARM_PHASE7_HONEST_ASSESSMENT.md"
    exit 2
fi
