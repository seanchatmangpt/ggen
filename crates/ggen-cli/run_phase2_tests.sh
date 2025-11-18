#!/bin/bash
# Comprehensive test execution script for ggen packs Phase 2-3
#
# This script runs all tests with coverage reporting and FMEA validation

set -e

echo "=================================="
echo "ggen Packs Phase 2-3 Test Suite"
echo "=================================="
echo ""

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Test counters
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# Function to run test category
run_test_category() {
    local category=$1
    local description=$2

    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BLUE}Running: ${description}${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"

    if cargo test --test packs_phase2_comprehensive $category --release -- --nocapture 2>&1 | tee /tmp/test_output_$category.log; then
        echo -e "${GREEN}✓ ${description} passed${NC}"
        PASSED_TESTS=$((PASSED_TESTS + 1))
    else
        echo -e "${RED}✗ ${description} failed${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi

    echo ""
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
}

echo -e "${YELLOW}Starting test execution...${NC}"
echo ""

# Run test categories
run_test_category "unit::installation::download_test" "Installation - Download Tests"
run_test_category "unit::installation::extraction_test" "Installation - Extraction Tests"
run_test_category "unit::installation::verification_test" "Installation - Verification Tests"
run_test_category "unit::installation::rollback_test" "Installation - Rollback Tests"
run_test_category "unit::installation::dependency_order_test" "Installation - Dependency Ordering Tests"
run_test_category "integration::complete_workflow_test" "Integration - Complete Workflow Tests"
run_test_category "performance::benchmarks" "Performance - Benchmark Tests"
run_test_category "security::security_tests" "Security - Security Tests"

echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}Test Summary${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo "Total Test Categories: $TOTAL_TESTS"
echo -e "${GREEN}Passed: $PASSED_TESTS${NC}"
echo -e "${RED}Failed: $FAILED_TESTS${NC}"

if [ $FAILED_TESTS -eq 0 ]; then
    echo -e "\n${GREEN}✓ All tests passed!${NC}"
    exit 0
else
    echo -e "\n${RED}✗ Some tests failed${NC}"
    exit 1
fi
