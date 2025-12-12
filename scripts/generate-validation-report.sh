#!/bin/bash
# Generate Validation Report
# Part of Andon Signal Validation Framework
#
# Purpose: Generate comprehensive validation report showing all three layers
# When: After validation runs, for reporting and monitoring
# SLO: <5s execution time

set -euo pipefail

# Colors for Andon signals
RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

REPORT_FILE="${1:-validation-report.txt}"
TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

echo "==========================================" > "$REPORT_FILE"
echo "Andon Signal Validation Framework Report" >> "$REPORT_FILE"
echo "Generated: $TIMESTAMP" >> "$REPORT_FILE"
echo "==========================================" >> "$REPORT_FILE"
echo "" >> "$REPORT_FILE"

# Layer 1: Compile-Time Validation
echo "Layer 1: Compile-Time Validation (RED)" >> "$REPORT_FILE"
echo "----------------------------------------" >> "$REPORT_FILE"
if cargo make check > /dev/null 2>&1; then
    echo -e "${GREEN}✅ Compilation: PASSED${NC}"
    echo "✅ Compilation: PASSED" >> "$REPORT_FILE"
else
    echo -e "${RED}❌ Compilation: FAILED${NC}"
    echo "❌ Compilation: FAILED" >> "$REPORT_FILE"
fi

if cargo make lint > /dev/null 2>&1; then
    echo -e "${GREEN}✅ Linting: PASSED${NC}"
    echo "✅ Linting: PASSED" >> "$REPORT_FILE"
else
    echo -e "${RED}❌ Linting: FAILED${NC}"
    echo "❌ Linting: FAILED" >> "$REPORT_FILE"
fi
echo "" >> "$REPORT_FILE"

# Layer 2: Test-Time Validation
echo "Layer 2: Test-Time Validation (YELLOW)" >> "$REPORT_FILE"
echo "----------------------------------------" >> "$REPORT_FILE"
if cargo make test-unit > /dev/null 2>&1; then
    echo -e "${GREEN}✅ Unit Tests: PASSED${NC}"
    echo "✅ Unit Tests: PASSED" >> "$REPORT_FILE"
else
    echo -e "${YELLOW}⚠️  Unit Tests: FAILED${NC}"
    echo "⚠️  Unit Tests: FAILED" >> "$REPORT_FILE"
fi

# Check if clnrm is available
if [ -f "/tmp/clnrm/target/release/clnrm" ]; then
    if cargo make test-clnrm > /dev/null 2>&1; then
        echo -e "${GREEN}✅ Integration Tests (clnrm): PASSED${NC}"
        echo "✅ Integration Tests (clnrm): PASSED" >> "$REPORT_FILE"
    else
        echo -e "${YELLOW}⚠️  Integration Tests (clnrm): FAILED${NC}"
        echo "⚠️  Integration Tests (clnrm): FAILED" >> "$REPORT_FILE"
    fi
else
    echo -e "${YELLOW}⚠️  Integration Tests (clnrm): SKIPPED (clnrm not found)${NC}"
    echo "⚠️  Integration Tests (clnrm): SKIPPED (clnrm not found)" >> "$REPORT_FILE"
fi
echo "" >> "$REPORT_FILE"

# Layer 3: Runtime Validation
echo "Layer 3: Runtime Validation (GREEN)" >> "$REPORT_FILE"
echo "----------------------------------------" >> "$REPORT_FILE"
if cargo make verify-cli > /dev/null 2>&1; then
    echo -e "${GREEN}✅ CLI Verification: PASSED${NC}"
    echo "✅ CLI Verification: PASSED" >> "$REPORT_FILE"
else
    echo -e "${RED}❌ CLI Verification: FAILED${NC}"
    echo "❌ CLI Verification: FAILED" >> "$REPORT_FILE"
fi
echo "" >> "$REPORT_FILE"

# Summary
echo "==========================================" >> "$REPORT_FILE"
echo "Summary" >> "$REPORT_FILE"
echo "==========================================" >> "$REPORT_FILE"
echo "Report saved to: $REPORT_FILE" >> "$REPORT_FILE"
echo "Timestamp: $TIMESTAMP" >> "$REPORT_FILE"
echo "" >> "$REPORT_FILE"

echo ""
echo -e "${BLUE}Validation report generated: $REPORT_FILE${NC}"
cat "$REPORT_FILE"

