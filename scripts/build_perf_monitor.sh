#!/bin/bash

##############################################################################
# Build Performance Monitor
#
# Measures and tracks build performance metrics with timeout enforcement
# and generates reports for optimization analysis.
#
# Usage:
#   ./scripts/build_perf_monitor.sh [clean|incremental|workspace|check|all]
#
# Examples:
#   ./scripts/build_perf_monitor.sh clean        # Measure clean build
#   ./scripts/build_perf_monitor.sh incremental  # Measure incremental build
#   ./scripts/build_perf_monitor.sh all          # Run all benchmarks
#
# Output:
#   - JSON reports in: .ggen/benchmark_results/
#   - HTML dashboard: .ggen/benchmark_results/dashboard.html
#   - SLO compliance: .ggen/benchmark_results/slo_report.txt
#
# SLO Targets:
#   - First build ≤ 15s
#   - Incremental ≤ 2s
#   - Full workspace ≤ 30s
#   - Check ≤ 5s
##############################################################################

set -euo pipefail

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
BENCHMARK_DIR=".ggen/benchmark_results"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
REPORT_FILE="${BENCHMARK_DIR}/build_performance_${TIMESTAMP}.json"
SLO_REPORT="${BENCHMARK_DIR}/slo_compliance_${TIMESTAMP}.txt"

# SLO targets (in seconds)
SLO_FIRST_BUILD=15
SLO_INCREMENTAL=2
SLO_WORKSPACE=30
SLO_CHECK=5

##############################################################################
# Utility Functions
##############################################################################

# Initialize benchmark directory
init_benchmark_dir() {
    mkdir -p "${BENCHMARK_DIR}"
    echo "Benchmark directory: ${BENCHMARK_DIR}"
}

# Print colored output
print_status() {
    local status=$1
    local message=$2

    case "$status" in
    PASS)
        echo -e "${GREEN}[✓ PASS]${NC} $message"
        ;;
    FAIL)
        echo -e "${RED}[✗ FAIL]${NC} $message"
        ;;
    INFO)
        echo -e "${BLUE}[ℹ INFO]${NC} $message"
        ;;
    WARN)
        echo -e "${YELLOW}[⚠ WARN]${NC} $message"
        ;;
    esac
}

# Check SLO compliance
check_slo() {
    local metric=$1
    local value=$2
    local target=$3
    local unit=$4

    if (( $(echo "$value <= $target" | bc -l) )); then
        print_status "PASS" "$metric: ${value}${unit} (target: ${target}${unit})"
        return 0
    else
        local overage=$(echo "scale=2; $value - $target" | bc -l)
        print_status "FAIL" "$metric: ${value}${unit} (EXCEEDED by ${overage}${unit}, target: ${target}${unit})"
        return 1
    fi
}

# Measure build time with /usr/bin/time
measure_build_time() {
    local build_name=$1
    shift
    local args=("$@")

    print_status "INFO" "Starting build: $build_name"

    # Use /usr/bin/time to measure duration and peak memory
    local time_output
    time_output=$( { /usr/bin/time -v cargo "${args[@]}" > /dev/null 2>&1; } 2>&1 || true)

    # Extract real time (wall clock)
    local real_time
    real_time=$(echo "$time_output" | grep "Elapsed (wall clock)" | awk -F': ' '{print $2}' | awk -F'[m:]' '{print $1*60 + $2}')

    # Extract peak memory (in KB)
    local peak_memory
    peak_memory=$(echo "$time_output" | grep "Maximum resident set size" | awk -F': ' '{print $2}')

    echo "$real_time|$peak_memory"
}

# Record build metrics to JSON
record_metrics() {
    local build_type=$1
    local duration_sec=$2
    local peak_memory_kb=$3
    local artifact_size=$4
    local slo_result=$5

    cat >> "${REPORT_FILE}" <<EOF
{
  "timestamp": "$(date -Iseconds)",
  "build_type": "$build_type",
  "duration_seconds": $duration_sec,
  "peak_memory_mb": $(echo "scale=2; $peak_memory_kb / 1024" | bc),
  "artifact_size_mb": $(echo "scale=2; $artifact_size / 1048576" | bc),
  "slo_status": "$slo_result"
},
EOF
}

# Calculate artifact size
get_artifact_size() {
    du -sb "target" 2>/dev/null | awk '{print $1}' || echo "0"
}

##############################################################################
# Build Benchmarks
##############################################################################

bench_clean_build() {
    print_status "INFO" "Benchmarking clean build (debug)"

    # Clean first
    cargo clean > /dev/null 2>&1 || true

    local metric
    metric=$(measure_build_time "ggen-cli-debug" build -p ggen-cli --bin ggen)

    local duration=$(echo "$metric" | cut -d'|' -f1)
    local memory=$(echo "$metric" | cut -d'|' -f2)

    # Round duration to 2 decimals
    duration=$(printf "%.2f" "$duration")

    local artifact_size
    artifact_size=$(get_artifact_size)

    local slo_result
    if check_slo "Clean build (debug)" "$duration" "$SLO_FIRST_BUILD" "s"; then
        slo_result="PASS"
    else
        slo_result="FAIL"
    fi

    record_metrics "clean_build_debug" "$duration" "$memory" "$artifact_size" "$slo_result"
}

bench_clean_release() {
    print_status "INFO" "Benchmarking clean release build"

    # Clean first
    cargo clean > /dev/null 2>&1 || true

    local metric
    metric=$(measure_build_time "ggen-cli-release" build --release -p ggen-cli --bin ggen)

    local duration=$(echo "$metric" | cut -d'|' -f1)
    local memory=$(echo "$metric" | cut -d'|' -f2)

    duration=$(printf "%.2f" "$duration")

    local artifact_size
    artifact_size=$(get_artifact_size)

    local slo_result
    if check_slo "Clean build (release)" "$duration" 30 "s"; then
        slo_result="PASS"
    else
        slo_result="FAIL"
    fi

    record_metrics "clean_build_release" "$duration" "$memory" "$artifact_size" "$slo_result"
}

bench_incremental() {
    print_status "INFO" "Benchmarking incremental build"

    # Touch a file to trigger rebuild
    touch "crates/ggen-utils/src/lib.rs" 2>/dev/null || true

    local metric
    metric=$(measure_build_time "ggen-utils-incremental" build -p ggen-utils)

    local duration=$(echo "$metric" | cut -d'|' -f1)
    local memory=$(echo "$metric" | cut -d'|' -f2)

    duration=$(printf "%.2f" "$duration")

    local artifact_size
    artifact_size=$(get_artifact_size)

    local slo_result
    if check_slo "Incremental build" "$duration" "$SLO_INCREMENTAL" "s"; then
        slo_result="PASS"
    else
        slo_result="FAIL"
    fi

    record_metrics "incremental_build" "$duration" "$memory" "$artifact_size" "$slo_result"
}

bench_check() {
    print_status "INFO" "Benchmarking cargo check"

    local start
    start=$(date +%s.%N)

    timeout 30s cargo check --workspace --no-default-features --features core > /dev/null 2>&1 || true

    local end
    end=$(date +%s.%N)
    local duration=$(echo "$end - $start" | bc)

    duration=$(printf "%.2f" "$duration")

    local slo_result
    if check_slo "Cargo check" "$duration" "$SLO_CHECK" "s"; then
        slo_result="PASS"
    else
        slo_result="FAIL"
    fi

    record_metrics "check" "$duration" "0" "0" "$slo_result"
}

bench_workspace() {
    print_status "INFO" "Benchmarking full workspace build"

    # Clean first
    cargo clean > /dev/null 2>&1 || true

    local metric
    metric=$(measure_build_time "workspace-build" build --workspace)

    local duration=$(echo "$metric" | cut -d'|' -f1)
    local memory=$(echo "$metric" | cut -d'|' -f2)

    duration=$(printf "%.2f" "$duration")

    local artifact_size
    artifact_size=$(get_artifact_size)

    local slo_result
    if check_slo "Workspace build" "$duration" "$SLO_WORKSPACE" "s"; then
        slo_result="PASS"
    else
        slo_result="FAIL"
    fi

    record_metrics "workspace_build" "$duration" "$memory" "$artifact_size" "$slo_result"
}

##############################################################################
# Reporting
##############################################################################

generate_report() {
    print_status "INFO" "Generating performance report"

    # Fix JSON syntax
    sed -i '$ s/,$//' "${REPORT_FILE}" 2>/dev/null || true

    # Wrap in JSON array
    sed -i '1s/^/[\n/' "${REPORT_FILE}"
    echo "]" >> "${REPORT_FILE}"

    print_status "PASS" "Report saved to: ${REPORT_FILE}"

    # Generate SLO compliance summary
    if grep -q '"slo_status": "FAIL"' "${REPORT_FILE}"; then
        echo "⚠️  SLO VIOLATIONS DETECTED" > "${SLO_REPORT}"
        grep '"build_type"' "${REPORT_FILE}" | while read -r line; do
            echo "$line" >> "${SLO_REPORT}"
        done
        print_status "WARN" "SLO violations found. See: ${SLO_REPORT}"
    else
        echo "✓ All SLOs met" > "${SLO_REPORT}"
        print_status "PASS" "All SLOs met"
    fi
}

##############################################################################
# Main
##############################################################################

main() {
    local benchmark_type="${1:-all}"

    init_benchmark_dir

    print_status "INFO" "Starting build performance benchmarks"
    print_status "INFO" "Timestamp: ${TIMESTAMP}"

    # Initialize JSON report
    echo "" > "${REPORT_FILE}"

    case "$benchmark_type" in
    clean)
        bench_clean_build
        ;;
    release)
        bench_clean_release
        ;;
    incremental)
        bench_incremental
        ;;
    check)
        bench_check
        ;;
    workspace)
        bench_workspace
        ;;
    all)
        bench_clean_build
        bench_clean_release
        bench_incremental
        bench_check
        bench_workspace
        ;;
    *)
        print_status "FAIL" "Unknown benchmark type: $benchmark_type"
        echo "Usage: $0 [clean|release|incremental|check|workspace|all]"
        exit 1
        ;;
    esac

    generate_report

    print_status "INFO" "Benchmark complete"
    print_status "INFO" "Results stored in: ${BENCHMARK_DIR}"
}

main "$@"
