#!/bin/bash
# Ultra-Deploy Test Runner Script
# Runs comprehensive ultra-deploy workflow tests with detailed reporting

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
VERBOSE=${VERBOSE:-0}
REPORT_DIR="${PROJECT_ROOT}/target/test-reports"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
REPORT_FILE="${REPORT_DIR}/ultra-deploy-test-${TIMESTAMP}.md"

# Test categories
RUN_INTEGRATION=${RUN_INTEGRATION:-1}
RUN_PERFORMANCE=${RUN_PERFORMANCE:-1}
RUN_RELIABILITY=${RUN_RELIABILITY:-1}
RUN_MATRIX=${RUN_MATRIX:-1}

echo_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

echo_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

echo_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

echo_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

# Create report directory
mkdir -p "$REPORT_DIR"

# Initialize report
cat > "$REPORT_FILE" << EOF
# Ultra-Deploy Test Report
Generated: $(date)
Platform: $(uname -s)
Rust Version: $(rustc --version)

## Test Configuration
- Integration Tests: $RUN_INTEGRATION
- Performance Tests: $RUN_PERFORMANCE
- Reliability Tests: $RUN_RELIABILITY
- Matrix Tests: $RUN_MATRIX

---

EOF

# Function to run test and capture results
run_test() {
    local test_name=$1
    local test_pattern=$2

    echo_info "Running: $test_name"

    local start_time=$(date +%s)
    local output_file="${REPORT_DIR}/${test_name}-${TIMESTAMP}.txt"

    if cargo test --test ultra_deploy_test "$test_pattern" -- --nocapture > "$output_file" 2>&1; then
        local end_time=$(date +%s)
        local duration=$((end_time - start_time))
        echo_success "$test_name completed in ${duration}s"

        # Append to report
        cat >> "$REPORT_FILE" << EOF
## $test_name
**Status**: ✓ PASSED
**Duration**: ${duration}s

\`\`\`
$(tail -n 50 "$output_file")
\`\`\`

---

EOF
        return 0
    else
        local end_time=$(date +%s)
        local duration=$((end_time - start_time))
        echo_error "$test_name failed after ${duration}s"

        # Append to report
        cat >> "$REPORT_FILE" << EOF
## $test_name
**Status**: ✗ FAILED
**Duration**: ${duration}s

\`\`\`
$(tail -n 100 "$output_file")
\`\`\`

---

EOF
        return 1
    fi
}

# Preamble
echo_info "Ultra-Deploy Test Runner"
echo_info "========================="
echo_info "Project: $PROJECT_ROOT"
echo_info "Report: $REPORT_FILE"
echo ""

# Build ggen in release mode
echo_info "Building ggen (release mode)..."
if cargo build --release; then
    echo_success "Build completed"
    GGEN_BIN="${PROJECT_ROOT}/target/release/ggen"
    echo_info "Binary: $GGEN_BIN"
else
    echo_error "Build failed"
    exit 1
fi

echo ""

# Test counters
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# Integration Tests
if [ "$RUN_INTEGRATION" -eq 1 ]; then
    echo_info "=== Integration Tests ==="

    if run_test "CLI Workflow" "test_ultra_deploy_cli_under_60s"; then
        PASSED_TESTS=$((PASSED_TESTS + 1))
    else
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi
    TOTAL_TESTS=$((TOTAL_TESTS + 1))

    if run_test "Ggen+Cleanroom Integration" "test_ggen_cleanroom_integration"; then
        PASSED_TESTS=$((PASSED_TESTS + 1))
    else
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi
    TOTAL_TESTS=$((TOTAL_TESTS + 1))

    if run_test "Output Correctness" "test_output_correctness"; then
        PASSED_TESTS=$((PASSED_TESTS + 1))
    else
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi
    TOTAL_TESTS=$((TOTAL_TESTS + 1))

    echo ""
fi

# Performance Tests
if [ "$RUN_PERFORMANCE" -eq 1 ]; then
    echo_info "=== Performance Tests ==="

    if run_test "Performance Benchmark" "test_performance_benchmark"; then
        PASSED_TESTS=$((PASSED_TESTS + 1))
    else
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi
    TOTAL_TESTS=$((TOTAL_TESTS + 1))

    if run_test "Stage Performance Breakdown" "test_stage_performance_breakdown"; then
        PASSED_TESTS=$((PASSED_TESTS + 1))
    else
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi
    TOTAL_TESTS=$((TOTAL_TESTS + 1))

    echo ""
fi

# Reliability Tests
if [ "$RUN_RELIABILITY" -eq 1 ]; then
    echo_info "=== Reliability Tests ==="

    if run_test "Workflow Reliability" "test_workflow_reliability"; then
        PASSED_TESTS=$((PASSED_TESTS + 1))
    else
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi
    TOTAL_TESTS=$((TOTAL_TESTS + 1))

    echo ""
fi

# Matrix Tests
if [ "$RUN_MATRIX" -eq 1 ]; then
    echo_info "=== Matrix Tests ==="

    if run_test "Template Matrix" "test_template_matrix"; then
        PASSED_TESTS=$((PASSED_TESTS + 1))
    else
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi
    TOTAL_TESTS=$((TOTAL_TESTS + 1))

    if run_test "Sequential Workflow" "test_sequential_workflow_performance"; then
        PASSED_TESTS=$((PASSED_TESTS + 1))
    else
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi
    TOTAL_TESTS=$((TOTAL_TESTS + 1))

    echo ""
fi

# Summary
echo_info "=== Test Summary ==="
echo "Total Tests: $TOTAL_TESTS"
echo_success "Passed: $PASSED_TESTS"
if [ "$FAILED_TESTS" -gt 0 ]; then
    echo_error "Failed: $FAILED_TESTS"
else
    echo "Failed: $FAILED_TESTS"
fi
echo ""

# Append summary to report
cat >> "$REPORT_FILE" << EOF

## Summary
- **Total Tests**: $TOTAL_TESTS
- **Passed**: $PASSED_TESTS
- **Failed**: $FAILED_TESTS
- **Success Rate**: $(awk "BEGIN {printf \"%.1f\", ($PASSED_TESTS/$TOTAL_TESTS)*100}")%

## Reports
- Full report: $REPORT_FILE
- Individual test outputs: $REPORT_DIR/*-${TIMESTAMP}.txt

EOF

echo_info "Report saved: $REPORT_FILE"
echo ""

# Exit with appropriate code
if [ "$FAILED_TESTS" -gt 0 ]; then
    echo_error "Some tests failed!"
    exit 1
else
    echo_success "All tests passed!"
    exit 0
fi
