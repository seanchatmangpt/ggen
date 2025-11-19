#!/bin/bash
# Andon Monitoring System - Real-time Test Failure Detection
# Implements continuous monitoring with Red/Yellow/Green alerts

set -e

# Colors for output
RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

# Alert thresholds
FAIL_THRESHOLD=5    # Red alert if > 5% tests fail
FLAKY_THRESHOLD=2   # Yellow alert if > 2% tests flaky
TIMEOUT_SECONDS=30  # Red alert if tests take > 30s

# Log file
LOG_FILE="${CARGO_TARGET_DIR:-target}/andon_alerts.log"
mkdir -p "$(dirname "$LOG_FILE")"

# Andon alert functions
andon_red_alert() {
    local test_name="$1"
    local error_msg="$2"
    echo -e "${RED}🚨 RED ALERT: $test_name${NC}" | tee -a "$LOG_FILE"
    echo -e "${RED}ERROR: $error_msg${NC}" | tee -a "$LOG_FILE"
    echo -e "${RED}ACTION: Stop pipeline, fix immediately${NC}" | tee -a "$LOG_FILE"
    echo "---" | tee -a "$LOG_FILE"
    return 1
}

andon_yellow_alert() {
    local test_name="$1"
    local warning_msg="$2"
    echo -e "${YELLOW}⚠️  YELLOW ALERT: $test_name${NC}" | tee -a "$LOG_FILE"
    echo -e "${YELLOW}WARNING: $warning_msg${NC}" | tee -a "$LOG_FILE"
    echo -e "${YELLOW}ACTION: Monitor next run, investigate${NC}" | tee -a "$LOG_FILE"
    echo "---" | tee -a "$LOG_FILE"
}

andon_green_ok() {
    local test_name="$1"
    echo -e "${GREEN}✅ GREEN: $test_name - All tests passing${NC}" | tee -a "$LOG_FILE"
}

# Check 1: Compilation Failure
check_compilation() {
    echo "Checking compilation..."

    if ! cargo test --no-run 2>&1 | tee /tmp/build.log; then
        andon_red_alert "Compilation" "Build failed - check syntax errors"
        return 1
    fi

    if grep -q "^error" /tmp/build.log; then
        andon_red_alert "Compilation" "Compilation errors detected"
        return 1
    fi

    echo "✓ Compilation passed"
    return 0
}

# Check 2: Test Timeout
check_timeout() {
    echo "Checking test timeout..."

    local start_time=$(date +%s)

    if ! timeout ${TIMEOUT_SECONDS} cargo test --quiet 2>&1 | tee /tmp/test.log; then
        local exit_code=$?
        if [ $exit_code -eq 124 ]; then
            andon_red_alert "Test Timeout" "Tests exceeded ${TIMEOUT_SECONDS}s timeout"
            return 1
        fi
    fi

    local end_time=$(date +%s)
    local duration=$((end_time - start_time))

    if [ $duration -gt $TIMEOUT_SECONDS ]; then
        andon_yellow_alert "Test Performance" "Tests took ${duration}s (threshold: ${TIMEOUT_SECONDS}s)"
    else
        echo "✓ Tests completed in ${duration}s"
    fi

    return 0
}

# Check 3: Flaky Tests
check_flaky_tests() {
    echo "Checking for flaky tests..."

    local test_list=$(cargo test --list 2>&1 | grep ": test$" | awk '{print $1}')
    local flaky_count=0

    for test in $test_list; do
        local pass_count=0

        # Run each test 3 times
        for i in {1..3}; do
            if cargo test "$test" --quiet >/dev/null 2>&1; then
                pass_count=$((pass_count + 1))
            fi
        done

        # Flaky if not all passes or all fails
        if [ $pass_count -gt 0 ] && [ $pass_count -lt 3 ]; then
            andon_yellow_alert "Flaky Test: $test" "Passed $pass_count/3 runs"
            flaky_count=$((flaky_count + 1))
        fi
    done

    if [ $flaky_count -eq 0 ]; then
        echo "✓ No flaky tests detected"
    else
        echo "Found $flaky_count flaky tests"
    fi

    return 0
}

# Check 4: Test Failure Rate
check_failure_rate() {
    echo "Checking test failure rate..."

    # Run all tests and capture output
    cargo test 2>&1 | tee /tmp/test_results.log

    # Parse test results
    local total_tests=$(grep -c "^test " /tmp/test_results.log || echo 0)
    local failed_tests=$(grep -c "^test.*FAILED$" /tmp/test_results.log || echo 0)

    if [ "$total_tests" -eq 0 ]; then
        andon_yellow_alert "Test Count" "No tests found"
        return 0
    fi

    local fail_rate=$(awk "BEGIN {printf \"%.1f\", ($failed_tests / $total_tests) * 100}")

    echo "Test Results: $failed_tests failed out of $total_tests (${fail_rate}%)"

    # Check against thresholds
    if (( $(echo "$fail_rate > $FAIL_THRESHOLD" | bc -l) )); then
        andon_red_alert "High Failure Rate" "${fail_rate}% tests failing (threshold: ${FAIL_THRESHOLD}%)"
        return 1
    elif [ "$failed_tests" -gt 0 ]; then
        andon_yellow_alert "Some Failures" "${fail_rate}% tests failing"
    else
        andon_green_ok "Test Suite"
    fi

    return 0
}

# Check 5: Memory Leaks (if valgrind available)
check_memory_leaks() {
    if ! command -v valgrind >/dev/null 2>&1; then
        echo "⊘ Valgrind not available, skipping memory leak check"
        return 0
    fi

    echo "Checking for memory leaks..."

    # Run a subset of tests with valgrind
    if valgrind --leak-check=full --error-exitcode=1 \
        cargo test --release 2>&1 | grep -q "definitely lost"; then
        andon_red_alert "Memory Leak" "Memory leaks detected by valgrind"
        return 1
    fi

    echo "✓ No memory leaks detected"
    return 0
}

# Main monitoring loop
main() {
    echo "═══════════════════════════════════════"
    echo "🚨 ANDON MONITORING SYSTEM"
    echo "═══════════════════════════════════════"
    echo "Started at: $(date)"
    echo "Log file: $LOG_FILE"
    echo ""

    local exit_code=0

    # Run all checks
    check_compilation || exit_code=1
    echo ""

    check_timeout || exit_code=1
    echo ""

    check_failure_rate || exit_code=1
    echo ""

    # Flaky test check is expensive, run optionally
    if [ "${CHECK_FLAKY:-false}" = "true" ]; then
        check_flaky_tests || exit_code=1
        echo ""
    fi

    # Memory check is very expensive, run optionally
    if [ "${CHECK_MEMORY:-false}" = "true" ]; then
        check_memory_leaks || exit_code=1
        echo ""
    fi

    echo "═══════════════════════════════════════"
    echo "Completed at: $(date)"
    echo "═══════════════════════════════════════"

    return $exit_code
}

# Run main function
main "$@"
