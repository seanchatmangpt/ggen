#!/bin/bash

###############################################################################
# Performance Benchmarking Runner for TAI Erlang Autonomics
#
# Runs comprehensive performance benchmarks and generates a report
#
# Usage:
#   ./run_performance_benchmarks.sh                    # Run all benchmarks
#   ./run_performance_benchmarks.sh http               # Run HTTP benchmarks only
#   ./run_performance_benchmarks.sh --help             # Show help
#
# Environment Variables:
#   BENCHMARK_TIMEOUT - Test timeout in seconds (default: 600)
#   BENCHMARK_VERBOSE - Verbose output (default: false)
#   BENCHMARK_LOG_DIR - Log output directory (default: ./_build/test/logs)
#
###############################################################################

set -euo pipefail

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
BENCHMARK_TIMEOUT=${BENCHMARK_TIMEOUT:-600}
BENCHMARK_VERBOSE=${BENCHMARK_VERBOSE:-false}
LOG_DIR=${BENCHMARK_LOG_DIR:-"$PROJECT_ROOT/_build/test/logs"}
REPORT_DIR="$PROJECT_ROOT/test/perf_benchmarks/reports"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Functions
print_header() {
    echo -e "${BLUE}================================================${NC}"
    echo -e "${BLUE}$1${NC}"
    echo -e "${BLUE}================================================${NC}"
}

print_success() {
    echo -e "${GREEN}✓ $1${NC}"
}

print_error() {
    echo -e "${RED}✗ $1${NC}"
}

print_warning() {
    echo -e "${YELLOW}⚠ $1${NC}"
}

print_info() {
    echo -e "${BLUE}ℹ $1${NC}"
}

show_help() {
    cat << 'EOF'
Performance Benchmarking Runner for TAI Erlang Autonomics

USAGE:
    ./run_performance_benchmarks.sh [SUITE] [OPTIONS]

BENCHMARKS:
    all              Run all performance benchmarks (default)
    http             HTTP endpoint performance tests
    governor         Governor state machine tests
    ledger           Receipt ledger performance tests
    executor         Action executor and poolboy tests
    stress           System-wide stress tests
    quick            Quick smoke test suite

OPTIONS:
    --verbose        Enable verbose output
    --timeout SECS   Set test timeout (default: 600)
    --help           Show this help message
    --clean          Clean build artifacts before testing
    --report         Generate HTML report after tests

EXAMPLES:
    ./run_performance_benchmarks.sh all
    ./run_performance_benchmarks.sh http --verbose
    ./run_performance_benchmarks.sh stress --timeout 900 --report
    ./run_performance_benchmarks.sh quick --clean

ENVIRONMENT VARIABLES:
    BENCHMARK_TIMEOUT - Test timeout in seconds (default: 600)
    BENCHMARK_VERBOSE - Enable verbose output (default: false)
    BENCHMARK_LOG_DIR - Log output directory (default: _build/test/logs)

EOF
}

check_prerequisites() {
    print_header "Checking Prerequisites"

    # Check for rebar3
    if ! command -v rebar3 &> /dev/null; then
        print_error "rebar3 not found. Please install rebar3."
        exit 1
    fi
    print_success "rebar3 found: $(rebar3 version | head -1)"

    # Check for Erlang
    if ! command -v erl &> /dev/null; then
        print_error "Erlang not found. Please install Erlang/OTP 25.0+"
        exit 1
    fi
    local erl_version=$(erl -version 2>&1 | grep -oP '\d+\.\d+')
    print_success "Erlang/OTP found: $erl_version"

    # Check for project root
    if [ ! -f "$PROJECT_ROOT/rebar.config" ]; then
        print_error "rebar.config not found. Please run from project root."
        exit 1
    fi
    print_success "Project root verified: $PROJECT_ROOT"

    echo ""
}

build_project() {
    print_header "Building Project"

    cd "$PROJECT_ROOT"

    # Clean if requested
    if [[ "${CLEAN_BUILD:-false}" == "true" ]]; then
        print_info "Cleaning build artifacts..."
        rebar3 clean
    fi

    # Compile
    print_info "Compiling project..."
    if rebar3 compile &> /dev/null; then
        print_success "Project compiled successfully"
    else
        print_error "Project compilation failed"
        exit 1
    fi

    echo ""
}

run_benchmark_suite() {
    local suite=$1
    local suite_name=""
    local timeout=$BENCHMARK_TIMEOUT

    case $suite in
        http)
            suite_name="HTTP Endpoint Benchmarks"
            suite_path="http_endpoint_bench_SUITE"
            ;;
        governor)
            suite_name="Governor Performance Benchmarks"
            suite_path="governor_perf_bench_SUITE"
            ;;
        ledger)
            suite_name="Receipt Ledger Benchmarks"
            suite_path="receipt_ledger_bench_SUITE"
            ;;
        executor)
            suite_name="Action Executor Benchmarks"
            suite_path="action_executor_bench_SUITE"
            ;;
        stress)
            suite_name="System Stress Benchmarks"
            suite_path="system_stress_bench_SUITE"
            timeout=$((BENCHMARK_TIMEOUT * 2))  # Stress tests need more time
            ;;
        quick)
            suite_name="Quick Smoke Tests"
            # Run only health and governor benchmarks
            suite_path="http_endpoint_bench_SUITE"
            ;;
        *)
            print_error "Unknown benchmark suite: $suite"
            return 1
            ;;
    esac

    print_header "Running $suite_name"
    print_info "Suite: $suite_path"
    print_info "Timeout: ${timeout}s"

    cd "$PROJECT_ROOT"

    # Run the benchmark suite
    local verbose_flag=""
    if [[ "$BENCHMARK_VERBOSE" == "true" ]]; then
        verbose_flag="-v"
    fi

    local start_time=$(date +%s)

    if rebar3 ct --suite=test/perf_benchmarks/$suite_path $verbose_flag --ct_timeout $timeout 2>&1 | tee -a "$LOG_DIR/benchmark_${suite}_${TIMESTAMP}.log"; then
        local end_time=$(date +%s)
        local duration=$((end_time - start_time))
        print_success "$suite_name completed in ${duration}s"
        return 0
    else
        print_error "$suite_name failed"
        return 1
    fi
}

run_all_benchmarks() {
    print_header "Running All Performance Benchmarks"
    print_info "Starting comprehensive performance test suite"
    print_info "Start time: $(date)"
    echo ""

    local start_time=$(date +%s)
    local failed_suites=()
    local passed_suites=()

    # Run each benchmark suite
    for suite in http governor ledger executor stress; do
        if run_benchmark_suite "$suite"; then
            passed_suites+=("$suite")
        else
            failed_suites+=("$suite")
        fi
        echo ""
    done

    local end_time=$(date +%s)
    local total_duration=$((end_time - start_time))

    # Print summary
    print_header "Benchmark Summary"
    print_info "Total duration: ${total_duration}s"
    echo ""

    if [ ${#passed_suites[@]} -gt 0 ]; then
        echo -e "${GREEN}Passed Benchmarks:${NC}"
        for suite in "${passed_suites[@]}"; do
            print_success "$suite"
        done
        echo ""
    fi

    if [ ${#failed_suites[@]} -gt 0 ]; then
        echo -e "${RED}Failed Benchmarks:${NC}"
        for suite in "${failed_suites[@]}"; do
            print_error "$suite"
        done
        echo ""
    fi

    # Summary line
    echo "Result: ${#passed_suites[@]} passed, ${#failed_suites[@]} failed"
    echo ""

    # Return exit code based on failures
    if [ ${#failed_suites[@]} -gt 0 ]; then
        return 1
    else
        return 0
    fi
}

generate_report() {
    print_header "Generating Performance Report"

    mkdir -p "$REPORT_DIR"

    # Copy benchmark report
    if [ -f "$PROJECT_ROOT/test/perf_benchmarks/PERFORMANCE_REPORT.md" ]; then
        cp "$PROJECT_ROOT/test/perf_benchmarks/PERFORMANCE_REPORT.md" \
           "$REPORT_DIR/PERFORMANCE_REPORT_${TIMESTAMP}.md"
        print_success "Performance report generated: $REPORT_DIR/PERFORMANCE_REPORT_${TIMESTAMP}.md"
    fi

    # Create index of reports
    cat > "$REPORT_DIR/INDEX.md" << 'EOFINDEX'
# Performance Report Archive

## Latest Reports

EOFINDEX

    # List recent reports
    ls -1t "$REPORT_DIR"/PERFORMANCE_REPORT_*.md 2>/dev/null | head -5 | while read report; do
        echo "- [$(basename $report)]($(basename $report))" >> "$REPORT_DIR/INDEX.md"
    done

    echo ""
    print_info "Report archive: $REPORT_DIR"
    echo ""
}

analyze_results() {
    print_header "Analyzing Results"

    print_info "Checking log files..."

    if [ -d "$LOG_DIR" ]; then
        # Look for test failures
        local failure_count=$(grep -r "FAILED" "$LOG_DIR" 2>/dev/null | wc -l)
        if [ $failure_count -gt 0 ]; then
            print_warning "Found $failure_count test failures"
            print_info "Review logs: $LOG_DIR"
        else
            print_success "No test failures detected"
        fi

        # Extract key metrics (if available)
        print_info "Key metrics from latest run:"
        grep -r "throughput\|latency\|success rate" "$LOG_DIR" 2>/dev/null | tail -10 || true
    fi

    echo ""
}

main() {
    local benchmark_suite="all"
    local generate_report_flag=false
    local clean_build_flag=false

    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
            all|http|governor|ledger|executor|stress|quick)
                benchmark_suite=$1
                shift
                ;;
            --verbose)
                BENCHMARK_VERBOSE=true
                shift
                ;;
            --timeout)
                BENCHMARK_TIMEOUT=$2
                shift 2
                ;;
            --report)
                generate_report_flag=true
                shift
                ;;
            --clean)
                CLEAN_BUILD=true
                shift
                ;;
            --help|-h)
                show_help
                exit 0
                ;;
            *)
                print_error "Unknown option: $1"
                show_help
                exit 1
                ;;
        esac
    done

    # Create log directory
    mkdir -p "$LOG_DIR"

    # Run benchmarks
    check_prerequisites
    build_project

    if [ "$benchmark_suite" = "all" ]; then
        if run_all_benchmarks; then
            exit_code=0
        else
            exit_code=1
        fi
    else
        if run_benchmark_suite "$benchmark_suite"; then
            exit_code=0
        else
            exit_code=1
        fi
    fi

    # Post-benchmark tasks
    analyze_results

    if [ "$generate_report_flag" = true ]; then
        generate_report
    fi

    # Final summary
    print_header "Benchmark Run Complete"
    if [ $exit_code -eq 0 ]; then
        print_success "All benchmarks completed successfully"
        print_info "Logs: $LOG_DIR"
        print_info "Run with --report to generate HTML report"
    else
        print_error "Some benchmarks failed"
        print_info "Review logs: $LOG_DIR"
    fi
    echo ""

    exit $exit_code
}

# Run main function
main "$@"
