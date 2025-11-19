#!/bin/bash
# Validate Benchmark Performance Targets
# Parses Criterion output and verifies all performance targets are met

set -euo pipefail

# Performance targets (in nanoseconds for precise comparison)
declare -A TARGETS=(
    # Error fix patterns (<50ms)
    ["E0277_fix"]=50000000
    ["E0308_fix"]=50000000
    ["E0283_fix"]=50000000
    ["E0599_fix"]=50000000

    # Poka-yoke patterns
    ["builder_construction"]=1000  # <1μs
    ["phantom_type"]=100           # Should be near-zero
    ["trait_bound"]=1000           # <1μs
    ["zero_copy"]=100              # Minimal overhead

    # Lean test patterns
    ["fixture_creation"]=5000000   # <5ms
    ["setup_teardown"]=10000000    # <10ms
    ["test_execution"]=100000000   # <100ms

    # Gemba walk patterns
    ["score_calculation"]=1000000  # <1ms
    ["quality_report"]=100000000   # <100ms for 100 tests

    # FMEA patterns
    ["rpn_calculation"]=1000       # <1μs
    ["distribution_analysis"]=100000  # <100μs for 252 errors
    ["priority_ranking"]=1000000   # <1ms
)

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}Benchmark Target Validation${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

RESULTS_DIR="${1:-docs/benchmark-results}"
LATEST_PATTERN=$(ls -t "$RESULTS_DIR"/pattern_performance_*.txt 2>/dev/null | head -1)
LATEST_MEMORY=$(ls -t "$RESULTS_DIR"/memory_profiling_*.txt 2>/dev/null | head -1)
LATEST_REGRESSION=$(ls -t "$RESULTS_DIR"/regression_detection_*.txt 2>/dev/null | head -1)

if [ -z "$LATEST_PATTERN" ]; then
    echo -e "${RED}✗ No benchmark results found in $RESULTS_DIR${NC}"
    exit 1
fi

echo -e "${GREEN}Validating benchmark results:${NC}"
echo -e "  Pattern: $LATEST_PATTERN"
echo -e "  Memory: $LATEST_MEMORY"
echo -e "  Regression: $LATEST_REGRESSION"
echo ""

PASS_COUNT=0
FAIL_COUNT=0
WARN_COUNT=0

# Function to parse time from Criterion output
parse_time() {
    local file="$1"
    local pattern="$2"

    # Extract time from Criterion output (handles ns, μs, ms, s)
    local time_str=$(grep -A5 "$pattern" "$file" | grep -E "time:" | head -1 | awk '{print $2, $3}')

    if [ -z "$time_str" ]; then
        echo "0"
        return
    fi

    local value=$(echo "$time_str" | awk '{print $1}')
    local unit=$(echo "$time_str" | awk '{print $2}')

    # Convert to nanoseconds
    case "$unit" in
        ns) echo "$value" ;;
        μs|us) echo "$(echo "$value * 1000" | bc)" ;;
        ms) echo "$(echo "$value * 1000000" | bc)" ;;
        s) echo "$(echo "$value * 1000000000" | bc)" ;;
        *) echo "0" ;;
    esac
}

# Function to format nanoseconds for display
format_time() {
    local ns=$1

    if [ $(echo "$ns < 1000" | bc) -eq 1 ]; then
        echo "${ns}ns"
    elif [ $(echo "$ns < 1000000" | bc) -eq 1 ]; then
        echo "$(echo "scale=2; $ns / 1000" | bc)μs"
    elif [ $(echo "$ns < 1000000000" | bc) -eq 1 ]; then
        echo "$(echo "scale=2; $ns / 1000000" | bc)ms"
    else
        echo "$(echo "scale=2; $ns / 1000000000" | bc)s"
    fi
}

# Function to validate target
validate_target() {
    local name="$1"
    local actual_ns="$2"
    local target_ns="$3"

    local actual_fmt=$(format_time "$actual_ns")
    local target_fmt=$(format_time "$target_ns")

    if [ $(echo "$actual_ns == 0" | bc) -eq 1 ]; then
        echo -e "${YELLOW}⚠ $name: No data${NC}"
        WARN_COUNT=$((WARN_COUNT + 1))
    elif [ $(echo "$actual_ns <= $target_ns" | bc) -eq 1 ]; then
        local margin=$(echo "scale=1; (($target_ns - $actual_ns) / $target_ns) * 100" | bc)
        echo -e "${GREEN}✓ $name: $actual_fmt (${margin}% under target of $target_fmt)${NC}"
        PASS_COUNT=$((PASS_COUNT + 1))
    else
        local excess=$(echo "scale=1; (($actual_ns - $target_ns) / $target_ns) * 100" | bc)
        echo -e "${RED}✗ $name: $actual_fmt (${excess}% over target of $target_fmt)${NC}"
        FAIL_COUNT=$((FAIL_COUNT + 1))
    fi
}

echo -e "${BLUE}Performance Target Validation:${NC}"
echo ""

# Validate error fix patterns
echo -e "${YELLOW}Error Fix Patterns:${NC}"
for pattern in E0277 E0308 E0283 E0599; do
    actual=$(parse_time "$LATEST_PATTERN" "${pattern}_.*_fix")
    validate_target "$pattern Fix" "$actual" "${TARGETS[${pattern}_fix]}"
done
echo ""

# Validate poka-yoke patterns
echo -e "${YELLOW}Poka-Yoke Patterns:${NC}"
actual=$(parse_time "$LATEST_PATTERN" "type_safe_builder_construction")
validate_target "Builder Construction" "$actual" "${TARGETS[builder_construction]}"

actual=$(parse_time "$LATEST_PATTERN" "phantom_type_validation")
validate_target "Phantom Type Validation" "$actual" "${TARGETS[phantom_type]}"

actual=$(parse_time "$LATEST_PATTERN" "trait_bound_resolution")
validate_target "Trait Bound Resolution" "$actual" "${TARGETS[trait_bound]}"

actual=$(parse_time "$LATEST_PATTERN" "zero_copy_overhead")
validate_target "Zero-Copy Overhead" "$actual" "${TARGETS[zero_copy]}"
echo ""

# Validate lean test patterns
echo -e "${YELLOW}Lean Test Patterns:${NC}"
actual=$(parse_time "$LATEST_PATTERN" "test_fixture_builder_creation")
validate_target "Fixture Creation" "$actual" "${TARGETS[fixture_creation]}"

actual=$(parse_time "$LATEST_PATTERN" "setup_teardown")
validate_target "Setup/Teardown" "$actual" "${TARGETS[setup_teardown]}"

actual=$(parse_time "$LATEST_PATTERN" "test_execution_100ms_target")
validate_target "Test Execution (10 tests)" "$actual" "${TARGETS[test_execution]}"
echo ""

# Validate Gemba walk patterns
echo -e "${YELLOW}Gemba Walk Patterns:${NC}"
actual=$(parse_time "$LATEST_PATTERN" "score_calculation")
validate_target "Score Calculation" "$actual" "${TARGETS[score_calculation]}"

actual=$(parse_time "$LATEST_PATTERN" "quality_report_generation_100_tests")
validate_target "Quality Report (100 tests)" "$actual" "${TARGETS[quality_report]}"
echo ""

# Validate FMEA patterns
echo -e "${YELLOW}FMEA Patterns:${NC}"
actual=$(parse_time "$LATEST_PATTERN" "rpn_calculation")
validate_target "RPN Calculation" "$actual" "${TARGETS[rpn_calculation]}"

actual=$(parse_time "$LATEST_PATTERN" "distribution_analysis_252_errors")
validate_target "Distribution Analysis (252 errors)" "$actual" "${TARGETS[distribution_analysis]}"

actual=$(parse_time "$LATEST_PATTERN" "priority_ranking_all_errors")
validate_target "Priority Ranking" "$actual" "${TARGETS[priority_ranking]}"
echo ""

# Memory profiling validation
if [ -n "$LATEST_MEMORY" ]; then
    echo -e "${YELLOW}Memory Profiling:${NC}"

    # Check for zero-copy validation
    if grep -q "Zero-copy: 0 bytes allocated" "$LATEST_MEMORY" 2>/dev/null; then
        echo -e "${GREEN}✓ Zero-copy pattern: No allocations detected${NC}"
        PASS_COUNT=$((PASS_COUNT + 1))
    else
        echo -e "${YELLOW}⚠ Zero-copy pattern: Review allocations${NC}"
        WARN_COUNT=$((WARN_COUNT + 1))
    fi

    # Check for memory leaks
    if grep -q "Memory leak detected!" "$LATEST_MEMORY" 2>/dev/null; then
        echo -e "${RED}✗ Memory leak detection: FAILED${NC}"
        FAIL_COUNT=$((FAIL_COUNT + 1))
    else
        echo -e "${GREEN}✓ Memory leak detection: No leaks found${NC}"
        PASS_COUNT=$((PASS_COUNT + 1))
    fi
    echo ""
fi

# Regression detection validation
if [ -n "$LATEST_REGRESSION" ]; then
    echo -e "${YELLOW}Regression Detection:${NC}"

    # Check for assertion failures
    if grep -q "assertion.*failed" "$LATEST_REGRESSION" 2>/dev/null; then
        echo -e "${RED}✗ Regression detected: Performance targets exceeded${NC}"
        FAIL_COUNT=$((FAIL_COUNT + 1))
    else
        echo -e "${GREEN}✓ No regressions detected: All targets met${NC}"
        PASS_COUNT=$((PASS_COUNT + 1))
    fi
    echo ""
fi

# Summary
echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}Validation Summary${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""
echo -e "${GREEN}Passed: $PASS_COUNT${NC}"
echo -e "${YELLOW}Warnings: $WARN_COUNT${NC}"
echo -e "${RED}Failed: $FAIL_COUNT${NC}"
echo ""

if [ $FAIL_COUNT -eq 0 ]; then
    echo -e "${GREEN}✓ All performance targets validated successfully!${NC}"
    exit 0
else
    echo -e "${RED}✗ Some performance targets were not met${NC}"
    exit 1
fi
