#!/bin/bash
# Gemba Walk - On-Floor Test Inspection
# Go see where tests actually run, observe reality

set -e

# Colors
BLUE='\033[0;34m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

# Inspection report file
REPORT_FILE="${CARGO_TARGET_DIR:-target}/gemba_walk_report.txt"
mkdir -p "$(dirname "$REPORT_FILE")"

echo "═══════════════════════════════════════" | tee "$REPORT_FILE"
echo "🚶 GEMBA WALK INSPECTION" | tee -a "$REPORT_FILE"
echo "═══════════════════════════════════════" | tee -a "$REPORT_FILE"
echo "Started at: $(date)" | tee -a "$REPORT_FILE"
echo "" | tee -a "$REPORT_FILE"

# Function to run inspection checks
inspect_test_file() {
    local test_file="$1"

    echo -e "${BLUE}Inspecting: $test_file${NC}" | tee -a "$REPORT_FILE"
    echo "---" | tee -a "$REPORT_FILE"

    # Check 1: Are tests close to actual usage? (not mocked away)
    echo "1. Real Implementation Usage:" | tee -a "$REPORT_FILE"
    local mock_count=$(grep -c "mock\|Mock\|stub" "$test_file" || echo 0)
    if [ "$mock_count" -lt 5 ]; then
        echo -e "   ${GREEN}✅ Minimal mocking ($mock_count occurrences)${NC}" | tee -a "$REPORT_FILE"
    else
        echo -e "   ${YELLOW}⚠️  High mock usage ($mock_count occurrences)${NC}" | tee -a "$REPORT_FILE"
        echo "   Recommendation: Use real implementations where possible" | tee -a "$REPORT_FILE"
    fi

    # Check 2: Can we read failure messages easily?
    echo "2. Assertion Clarity:" | tee -a "$REPORT_FILE"
    local assert_count=$(grep -c "assert!" "$test_file" || echo 0)
    local assert_eq_count=$(grep -c "assert_eq!" "$test_file" || echo 0)
    if [ "$assert_eq_count" -gt "$assert_count" ]; then
        echo -e "   ${GREEN}✅ Good use of descriptive assertions${NC}" | tee -a "$REPORT_FILE"
    else
        echo -e "   ${YELLOW}⚠️  Many bare assert!() calls${NC}" | tee -a "$REPORT_FILE"
        echo "   Recommendation: Use assert_eq!/assert_ne! for clarity" | tee -a "$REPORT_FILE"
    fi

    # Check 3: Do tests reveal actual bugs?
    echo "3. Bug Detection:" | tee -a "$REPORT_FILE"
    if grep -q "edge\|boundary\|error" "$test_file"; then
        echo -e "   ${GREEN}✅ Tests cover edge cases and errors${NC}" | tee -a "$REPORT_FILE"
    else
        echo -e "   ${YELLOW}⚠️  May be too happy-path focused${NC}" | tee -a "$REPORT_FILE"
        echo "   Recommendation: Add edge case and error handling tests" | tee -a "$REPORT_FILE"
    fi

    # Check 4: Is setup/teardown clear?
    echo "4. Setup/Teardown:" | tee -a "$REPORT_FILE"
    if grep -q "fn setup\|before_each\|fn teardown\|after_each" "$test_file"; then
        echo -e "   ${GREEN}✅ Explicit setup/teardown functions${NC}" | tee -a "$REPORT_FILE"
    else
        local line_count=$(wc -l < "$test_file")
        if [ "$line_count" -lt 50 ]; then
            echo -e "   ${GREEN}✅ Simple inline setup is clear${NC}" | tee -a "$REPORT_FILE"
        else
            echo -e "   ${YELLOW}⚠️  Could be more explicit${NC}" | tee -a "$REPORT_FILE"
            echo "   Recommendation: Extract setup/teardown functions" | tee -a "$REPORT_FILE"
        fi
    fi

    # Check 5: Can we debug quickly?
    echo "5. Debug-ability:" | tee -a "$REPORT_FILE"
    if grep -q 'expect(\|, "' "$test_file"; then
        echo -e "   ${GREEN}✅ Custom error messages provided${NC}" | tee -a "$REPORT_FILE"
    else
        echo -e "   ${YELLOW}⚠️  Add custom error messages to assertions${NC}" | tee -a "$REPORT_FILE"
    fi

    # Check 6: Are tests fast enough?
    echo "6. Performance:" | tee -a "$REPORT_FILE"
    echo "   Running tests to measure performance..." | tee -a "$REPORT_FILE"
    local start_time=$(date +%s)
    cargo test --test "$(basename "$test_file" .rs)" --quiet 2>/dev/null || true
    local end_time=$(date +%s)
    local duration=$((end_time - start_time))

    if [ "$duration" -lt 30 ]; then
        echo -e "   ${GREEN}✅ Fast tests (${duration}s < 30s threshold)${NC}" | tee -a "$REPORT_FILE"
    else
        echo -e "   ${YELLOW}⚠️  Slow tests (${duration}s >= 30s)${NC}" | tee -a "$REPORT_FILE"
        echo "   Recommendation: Optimize or parallelize tests" | tee -a "$REPORT_FILE"
    fi

    # Check 7: Do tests run in isolation?
    echo "7. Isolation:" | tee -a "$REPORT_FILE"
    if grep -q "static mut\|lazy_static" "$test_file"; then
        echo -e "   ${YELLOW}⚠️  Shared state detected${NC}" | tee -a "$REPORT_FILE"
        echo "   Recommendation: Consider test isolation" | tee -a "$REPORT_FILE"
    else
        echo -e "   ${GREEN}✅ No obvious shared state${NC}" | tee -a "$REPORT_FILE"
    fi

    # Check 8: Are failures reproducible?
    echo "8. Reproducibility:" | tee -a "$REPORT_FILE"
    echo "   Running tests 3 times to check reproducibility..." | tee -a "$REPORT_FILE"
    local run1_pass=0
    local run2_pass=0
    local run3_pass=0

    cargo test --test "$(basename "$test_file" .rs)" --quiet 2>/dev/null && run1_pass=1 || true
    cargo test --test "$(basename "$test_file" .rs)" --quiet 2>/dev/null && run2_pass=1 || true
    cargo test --test "$(basename "$test_file" .rs)" --quiet 2>/dev/null && run3_pass=1 || true

    local total_passes=$((run1_pass + run2_pass + run3_pass))

    if [ "$total_passes" -eq 3 ] || [ "$total_passes" -eq 0 ]; then
        echo -e "   ${GREEN}✅ Reproducible results (${total_passes}/3 passes)${NC}" | tee -a "$REPORT_FILE"
    else
        echo -e "   ${YELLOW}⚠️  Flaky tests detected (${total_passes}/3 passes)${NC}" | tee -a "$REPORT_FILE"
        echo "   Recommendation: Fix non-deterministic behavior" | tee -a "$REPORT_FILE"
    fi

    echo "" | tee -a "$REPORT_FILE"
}

# Observe actual test execution (Gemba = go see)
observe_actual_run() {
    local test_file="$1"

    echo "═══════════════════════════════════════" | tee -a "$REPORT_FILE"
    echo "👀 OBSERVING ACTUAL TEST RUN" | tee -a "$REPORT_FILE"
    echo "═══════════════════════════════════════" | tee -a "$REPORT_FILE"

    # Run with nocapture to see actual output
    echo "Running: cargo test --test $(basename "$test_file" .rs) -- --nocapture" | tee -a "$REPORT_FILE"
    cargo test --test "$(basename "$test_file" .rs)" -- --nocapture --test-threads=1 2>&1 | tee -a "$REPORT_FILE"

    echo "" | tee -a "$REPORT_FILE"
}

# Interview the code: Why does this test exist?
interview_code() {
    local test_file="$1"

    echo "═══════════════════════════════════════" | tee -a "$REPORT_FILE"
    echo "💬 INTERVIEWING THE CODE" | tee -a "$REPORT_FILE"
    echo "═══════════════════════════════════════" | tee -a "$REPORT_FILE"

    echo "Test function purposes:" | tee -a "$REPORT_FILE"
    grep -B3 "^fn test_\|^    fn test_" "$test_file" | grep "//\|///\|#\[test\]" | tee -a "$REPORT_FILE"

    echo "" | tee -a "$REPORT_FILE"
}

# Main inspection
main() {
    local test_dir="${1:-tests/integration}"

    if [ ! -d "$test_dir" ]; then
        echo "Error: Test directory not found: $test_dir"
        exit 1
    fi

    echo "Inspecting tests in: $test_dir"
    echo ""

    # Find all test files
    for test_file in $(find "$test_dir" -name "*.rs" -type f); do
        inspect_test_file "$test_file"

        # Optional: observe actual run
        if [ "${OBSERVE_RUN:-false}" = "true" ]; then
            observe_actual_run "$test_file"
        fi

        # Optional: interview code
        if [ "${INTERVIEW_CODE:-false}" = "true" ]; then
            interview_code "$test_file"
        fi
    done

    echo "═══════════════════════════════════════" | tee -a "$REPORT_FILE"
    echo "Inspection complete!" | tee -a "$REPORT_FILE"
    echo "Report saved to: $REPORT_FILE" | tee -a "$REPORT_FILE"
    echo "═══════════════════════════════════════" | tee -a "$REPORT_FILE"
}

# Run main
main "$@"
