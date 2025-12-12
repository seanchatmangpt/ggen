#!/bin/bash
# Validation Monitoring Script
# Part of Andon Signal Validation Framework
#
# Purpose: Monitor validation status and send alerts
# When: Scheduled (cron), manual monitoring, CI/CD integration
# SLO: <5s execution time

set -euo pipefail

# Colors for Andon signals
RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

REPORT_FILE="${1:-validation-report.txt}"
ALERT_THRESHOLD="${2:-1}"  # Number of consecutive failures before alerting

# Check if validation report exists
if [ ! -f "$REPORT_FILE" ]; then
    echo -e "${YELLOW}⚠️  Validation report not found: $REPORT_FILE${NC}"
    echo "Run 'cargo make validation-report' to generate report"
    exit 1
fi

# Parse report and check for failures
LAYER1_FAILED=0
LAYER2_FAILED=0
LAYER3_FAILED=0

if grep -q "❌ Compilation: FAILED" "$REPORT_FILE" || \
   grep -q "❌ Linting: FAILED" "$REPORT_FILE"; then
    LAYER1_FAILED=1
fi

if grep -q "⚠️  Unit Tests: FAILED" "$REPORT_FILE" || \
   grep -q "⚠️  Integration Tests.*FAILED" "$REPORT_FILE"; then
    LAYER2_FAILED=1
fi

if grep -q "❌ CLI Verification: FAILED" "$REPORT_FILE"; then
    LAYER3_FAILED=1
fi

# Determine overall status
if [ $LAYER1_FAILED -eq 1 ]; then
    ANDON_SIGNAL="RED"
    STATUS="CRITICAL"
    MESSAGE="Layer 1 (Compile-Time) validation failed - STOP THE LINE"
elif [ $LAYER3_FAILED -eq 1 ]; then
    ANDON_SIGNAL="RED"
    STATUS="CRITICAL"
    MESSAGE="Layer 3 (Runtime) validation failed - CLI commands not working"
elif [ $LAYER2_FAILED -eq 1 ]; then
    ANDON_SIGNAL="YELLOW"
    STATUS="WARNING"
    MESSAGE="Layer 2 (Test-Time) validation failed - Investigate"
else
    ANDON_SIGNAL="GREEN"
    STATUS="OK"
    MESSAGE="All validation layers passed"
fi

# Output status
echo "=========================================="
echo "Validation Monitoring Report"
echo "=========================================="
echo "Timestamp: $(date -u +"%Y-%m-%dT%H:%M:%SZ")"
echo "Report File: $REPORT_FILE"
echo ""
echo "Layer Status:"
echo "  Layer 1 (Compile-Time): $([ $LAYER1_FAILED -eq 1 ] && echo -e "${RED}FAILED${NC}" || echo -e "${GREEN}PASSED${NC}")"
echo "  Layer 2 (Test-Time):    $([ $LAYER2_FAILED -eq 1 ] && echo -e "${YELLOW}FAILED${NC}" || echo -e "${GREEN}PASSED${NC}")"
echo "  Layer 3 (Runtime):      $([ $LAYER3_FAILED -eq 1 ] && echo -e "${RED}FAILED${NC}" || echo -e "${GREEN}PASSED${NC}")"
echo ""
echo "Overall Status: $STATUS"
echo "Andon Signal: $ANDON_SIGNAL"
echo "Message: $MESSAGE"
echo ""

# Alert if needed
if [ "$ANDON_SIGNAL" != "GREEN" ]; then
    echo -e "${RED}🚨 ALERT: $MESSAGE${NC}"
    echo ""
    echo "Recommended Actions:"
    if [ $LAYER1_FAILED -eq 1 ]; then
        echo "  1. Run 'cargo make check' to see compilation errors"
        echo "  2. Run 'cargo make lint' to see linting errors"
        echo "  3. Fix errors and re-run validation"
    fi
    if [ $LAYER2_FAILED -eq 1 ]; then
        echo "  1. Run 'cargo make test-unit' to see test failures"
        echo "  2. Run 'cargo make test-clnrm' to see integration test failures"
        echo "  3. Fix failing tests and re-run validation"
    fi
    if [ $LAYER3_FAILED -eq 1 ]; then
        echo "  1. Run 'cargo make verify-cli' to see CLI command failures"
        echo "  2. Fix failing CLI commands and re-run validation"
        echo "  3. This may indicate 'fake greens' - tests pass but CLI fails"
    fi
    exit 1
else
    echo -e "${GREEN}✅ All validation layers passing${NC}"
    exit 0
fi

