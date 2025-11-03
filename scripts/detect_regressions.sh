#!/bin/bash
# Performance Regression Detection Script
#
# Analyzes Criterion benchmark output to detect performance regressions.
# Exits with status code 1 if regressions are detected.
#
# Usage:
#   ./scripts/detect_regressions.sh [threshold_percent]
#
# Arguments:
#   threshold_percent: Maximum acceptable regression (default: 10)
#
# Example:
#   ./scripts/detect_regressions.sh 15  # Allow up to 15% regression

set -euo pipefail

# Configuration
THRESHOLD=${1:-10}  # Default 10% regression threshold
RESULTS_FILE="bench_results.txt"

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m' # No Color

# Check if benchmark results exist
if [ ! -f "$RESULTS_FILE" ]; then
    echo -e "${YELLOW}Warning: $RESULTS_FILE not found${NC}"
    echo "Running benchmarks to generate results..."

    # Run benchmarks and save output
    cargo bench --all-features -- --baseline v2.3.0 2>&1 | tee "$RESULTS_FILE"
fi

echo -e "\n${BOLD}${BLUE}========================================${NC}"
echo -e "${BOLD}${BLUE}Performance Regression Analysis${NC}"
echo -e "${BOLD}${BLUE}========================================${NC}\n"

echo -e "Regression Threshold: ${BOLD}${THRESHOLD}%${NC}\n"

# Parse benchmark results
TOTAL_BENCHMARKS=$(grep -c "time:" "$RESULTS_FILE" || echo "0")
REGRESSIONS=$(grep -c "Performance has regressed" "$RESULTS_FILE" || echo "0")
IMPROVEMENTS=$(grep -c "Performance has improved" "$RESULTS_FILE" || echo "0")
NO_CHANGE=$(grep -c "No change in performance detected" "$RESULTS_FILE" || echo "0")

# Extract significant regressions (above threshold)
SIGNIFICANT_REGRESSIONS=0

if [ "$REGRESSIONS" -gt 0 ]; then
    echo -e "${RED}${BOLD}❌ Performance Regressions Detected${NC}\n"

    # Extract each regression with context
    grep -B 3 "Performance has regressed" "$RESULTS_FILE" | while read -r line; do
        if echo "$line" | grep -q "time:"; then
            # Extract benchmark name from previous context
            BENCHMARK_NAME=$(echo "$line" | sed 's/time:.*//')

            # Extract change percentage
            CHANGE_LINE=$(grep -A 1 "$line" "$RESULTS_FILE" | grep "change:" || echo "")

            if [ -n "$CHANGE_LINE" ]; then
                # Parse change percentage (e.g., "+8.2% +10.5% +12.8%")
                CHANGE_PCT=$(echo "$CHANGE_LINE" | sed 's/.*change: \[\([^%]*\)%.*/\1/' | awk '{print $2}' | tr -d '+')

                # Check if regression exceeds threshold
                if awk "BEGIN {exit !($CHANGE_PCT > $THRESHOLD)}"; then
                    SIGNIFICANT_REGRESSIONS=$((SIGNIFICANT_REGRESSIONS + 1))
                    echo -e "${RED}  ⚠️  $BENCHMARK_NAME${NC}"
                    echo -e "     Regression: ${RED}+${CHANGE_PCT}%${NC} (threshold: ${THRESHOLD}%)"
                    echo ""
                fi
            fi
        fi
    done

    # Summary of regressions
    echo -e "\n${BOLD}Regression Summary:${NC}"
    echo -e "  Total Regressions:        ${RED}$REGRESSIONS${NC}"
    echo -e "  Significant (>${THRESHOLD}%): ${RED}$SIGNIFICANT_REGRESSIONS${NC}"
fi

if [ "$IMPROVEMENTS" -gt 0 ]; then
    echo -e "\n${GREEN}${BOLD}✅ Performance Improvements Detected${NC}\n"

    # Extract improvements
    grep -B 3 "Performance has improved" "$RESULTS_FILE" | while read -r line; do
        if echo "$line" | grep -q "time:"; then
            BENCHMARK_NAME=$(echo "$line" | sed 's/time:.*//')

            # Extract change percentage
            CHANGE_LINE=$(grep -A 1 "$line" "$RESULTS_FILE" | grep "change:" || echo "")

            if [ -n "$CHANGE_LINE" ]; then
                CHANGE_PCT=$(echo "$CHANGE_LINE" | sed 's/.*change: \[\([^%]*\)%.*/\1/' | awk '{print $2}' | tr -d '-')
                echo -e "${GREEN}  ✅ $BENCHMARK_NAME${NC}"
                echo -e "     Improvement: ${GREEN}-${CHANGE_PCT}%${NC}"
                echo ""
            fi
        fi
    done

    echo -e "${BOLD}Improvement Summary:${NC}"
    echo -e "  Total Improvements: ${GREEN}$IMPROVEMENTS${NC}"
fi

if [ "$NO_CHANGE" -gt 0 ]; then
    echo -e "\n${BOLD}No Change:${NC} $NO_CHANGE benchmarks"
fi

# Overall summary
echo -e "\n${BOLD}${BLUE}========================================${NC}"
echo -e "${BOLD}${BLUE}Overall Summary${NC}"
echo -e "${BOLD}${BLUE}========================================${NC}\n"

echo -e "Total Benchmarks:          ${BOLD}$TOTAL_BENCHMARKS${NC}"
echo -e "Performance Regressions:   ${RED}$REGRESSIONS${NC}"
echo -e "Significant Regressions:   ${RED}$SIGNIFICANT_REGRESSIONS${NC}"
echo -e "Performance Improvements:  ${GREEN}$IMPROVEMENTS${NC}"
echo -e "No Change:                 $NO_CHANGE"

# Calculate net performance change
NET_CHANGE=$((IMPROVEMENTS - REGRESSIONS))

echo ""
if [ "$NET_CHANGE" -gt 0 ]; then
    echo -e "${GREEN}${BOLD}Net Performance Change: +$NET_CHANGE (improved)${NC}"
elif [ "$NET_CHANGE" -lt 0 ]; then
    echo -e "${RED}${BOLD}Net Performance Change: $NET_CHANGE (regressed)${NC}"
else
    echo -e "${BOLD}Net Performance Change: 0 (neutral)${NC}"
fi

# Exit status determination
echo -e "\n${BOLD}${BLUE}========================================${NC}"

if [ "$SIGNIFICANT_REGRESSIONS" -gt 0 ]; then
    echo -e "${RED}${BOLD}❌ FAILURE: $SIGNIFICANT_REGRESSIONS significant regression(s) detected${NC}"
    echo -e "${RED}   Threshold: ${THRESHOLD}%${NC}"
    echo ""
    echo -e "${YELLOW}Recommendations:${NC}"
    echo -e "  1. Review regression details above"
    echo -e "  2. Profile affected benchmarks: ${BOLD}cargo flamegraph --bench <benchmark>${NC}"
    echo -e "  3. Check recent commits for performance-impacting changes"
    echo -e "  4. Consider reverting or optimizing the change"
    exit 1
elif [ "$REGRESSIONS" -gt 0 ]; then
    echo -e "${YELLOW}${BOLD}⚠️  WARNING: $REGRESSIONS minor regression(s) detected (under ${THRESHOLD}% threshold)${NC}"
    echo -e "${YELLOW}   Monitor these benchmarks in future PRs${NC}"
    exit 0
else
    echo -e "${GREEN}${BOLD}✅ SUCCESS: No performance regressions detected${NC}"

    if [ "$IMPROVEMENTS" -gt 0 ]; then
        echo -e "${GREEN}   Bonus: $IMPROVEMENTS performance improvement(s)!${NC}"
    fi
    exit 0
fi
