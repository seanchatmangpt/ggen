#!/bin/bash

################################################################################
# Performance Regression Detection System
# Identifies performance degradations against historical baselines
################################################################################

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
METRICS_DIR="${PROJECT_ROOT}/.metrics"
REGRESSION_THRESHOLD=10  # Percent degradation to trigger alert

################################################################################
# Utility Functions
################################################################################

log_info() {
    echo "[INFO] $(date '+%Y-%m-%d %H:%M:%S'): $*"
}

log_warn() {
    echo "[WARN] $(date '+%Y-%m-%d %H:%M:%S'): $*" >&2
}

log_error() {
    echo "[ERROR] $(date '+%Y-%m-%d %H:%M:%S'): $*" >&2
}

log_regression() {
    echo "[REGRESSION] $(date '+%Y-%m-%d %H:%M:%S'): $*" >&2
}

################################################################################
# Baseline Management
################################################################################

create_baseline() {
    local metric_name="$1"
    local value="$2"

    mkdir -p "${METRICS_DIR}/baselines"

    cat > "${METRICS_DIR}/baselines/${metric_name}.baseline" <<EOF
timestamp=$(date +%s)
metric=${metric_name}
value=${value}
created=$(date -u +%Y-%m-%dT%H:%M:%SZ)
EOF

    log_info "Baseline created for $metric_name: $value"
}

get_baseline() {
    local metric_name="$1"
    local baseline_file="${METRICS_DIR}/baselines/${metric_name}.baseline"

    if [ ! -f "$baseline_file" ]; then
        return 1
    fi

    source "$baseline_file"
    echo "$value"
}

################################################################################
# Regression Detection
################################################################################

detect_build_time_regression() {
    log_info "Checking for build time regressions..."

    # Get latest build time from results
    local latest_result=$(ls -t "${METRICS_DIR}/results"/first_build_*.txt 2>/dev/null | head -1)
    if [ -z "$latest_result" ]; then
        log_warn "No build time results found"
        return 0
    fi

    source "$latest_result"
    local current_value=$value

    # Get baseline
    local baseline=$(get_baseline "first_build_time" 2>/dev/null || echo "")
    if [ -z "$baseline" ]; then
        log_info "No baseline for first_build_time, creating one..."
        create_baseline "first_build_time" "$current_value"
        return 0
    fi

    # Calculate regression percentage
    local regression=$(( (current_value - baseline) * 100 / baseline ))

    if [ $regression -gt $REGRESSION_THRESHOLD ]; then
        log_regression "First build time regression detected!"
        log_regression "  Baseline: ${baseline}s"
        log_regression "  Current:  ${current_value}s"
        log_regression "  Change:   +${regression}%"
        return 1
    elif [ $regression -gt 0 ]; then
        log_warn "First build time minor degradation: +${regression}%"
        return 0
    else
        log_info "First build time improved by $((0 - regression))%"
        create_baseline "first_build_time" "$current_value"
        return 0
    fi
}

detect_test_time_regression() {
    log_info "Checking for test execution regressions..."

    local latest_result=$(ls -t "${METRICS_DIR}/results"/test_execution_*.txt 2>/dev/null | head -1)
    if [ -z "$latest_result" ]; then
        log_warn "No test execution results found"
        return 0
    fi

    source "$latest_result"
    local current_value=$value

    # Get baseline
    local baseline=$(get_baseline "test_execution_time" 2>/dev/null || echo "")
    if [ -z "$baseline" ]; then
        log_info "No baseline for test_execution_time, creating one..."
        create_baseline "test_execution_time" "$current_value"
        return 0
    fi

    # Calculate regression percentage
    local regression=$(( (current_value - baseline) * 100 / baseline ))

    if [ $regression -gt $REGRESSION_THRESHOLD ]; then
        log_regression "Test execution time regression detected!"
        log_regression "  Baseline: ${baseline}s"
        log_regression "  Current:  ${current_value}s"
        log_regression "  Change:   +${regression}%"
        return 1
    elif [ $regression -gt 0 ]; then
        log_warn "Test execution time minor degradation: +${regression}%"
        return 0
    else
        log_info "Test execution time improved by $((0 - regression))%"
        create_baseline "test_execution_time" "$current_value"
        return 0
    fi
}

detect_binary_size_regression() {
    log_info "Checking for binary size regressions..."

    local latest_result=$(ls -t "${METRICS_DIR}/results"/binary_size_*.txt 2>/dev/null | head -1)
    if [ -z "$latest_result" ]; then
        log_warn "No binary size results found"
        return 0
    fi

    source "$latest_result"
    local current_value=$value_mb

    # Get baseline
    local baseline=$(get_baseline "binary_size_mb" 2>/dev/null || echo "")
    if [ -z "$baseline" ]; then
        log_info "No baseline for binary_size_mb, creating one..."
        create_baseline "binary_size_mb" "$current_value"
        return 0
    fi

    # Calculate regression percentage
    local regression=$(( (current_value - baseline) * 100 / baseline ))

    if [ $regression -gt $REGRESSION_THRESHOLD ]; then
        log_regression "Binary size regression detected!"
        log_regression "  Baseline: ${baseline}MB"
        log_regression "  Current:  ${current_value}MB"
        log_regression "  Change:   +${regression}%"
        return 1
    elif [ $regression -gt 0 ]; then
        log_warn "Binary size minor increase: +${regression}%"
        return 0
    else
        log_info "Binary size improved by $((0 - regression))%"
        create_baseline "binary_size_mb" "$current_value"
        return 0
    fi
}

################################################################################
# Historical Analysis
################################################################################

analyze_historical_trend() {
    local metric_name="$1"
    local result_pattern="$2"

    log_info "Analyzing historical trend for $metric_name..."

    local results=($(ls -t "${METRICS_DIR}/results"/${result_pattern} 2>/dev/null || echo ""))

    if [ ${#results[@]} -lt 2 ]; then
        log_warn "Insufficient history for trend analysis"
        return 0
    fi

    # Analyze first 5 recent measurements
    local values=()
    for result in "${results[@]:0:5}"; do
        source "$result"
        values+=("$value")
    done

    # Calculate trend
    if [ ${#values[@]} -ge 2 ]; then
        local first=${values[0]}
        local last=${values[-1]}
        local trend=$(( (last - first) * 100 / first ))

        if [ $trend -gt 0 ]; then
            log_warn "$metric_name trend: degrading (+${trend}%)"
        elif [ $trend -lt 0 ]; then
            log_info "$metric_name trend: improving (${trend}%)"
        else
            log_info "$metric_name trend: stable"
        fi
    fi
}

################################################################################
# Report Generation
################################################################################

generate_regression_report() {
    local report_file="${METRICS_DIR}/regression_report_$(date +%Y%m%d_%H%M%S).txt"

    {
        echo "Performance Regression Detection Report"
        echo "========================================"
        echo "Generated: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
        echo ""

        echo "Build Time Metrics"
        echo "------------------"
        if [ -f "${METRICS_DIR}/results"/first_build_*.txt ]; then
            source "$(ls -t "${METRICS_DIR}/results"/first_build_*.txt | head -1)"
            echo "First Build Time: ${value}s (Status: $status)"
        fi

        if [ -f "${METRICS_DIR}/results"/incremental_build_*.txt ]; then
            source "$(ls -t "${METRICS_DIR}/results"/incremental_build_*.txt | head -1)"
            echo "Incremental Build Time: ${value}s (Status: $status)"
        fi

        echo ""
        echo "Test Execution Metrics"
        echo "---------------------"
        if [ -f "${METRICS_DIR}/results"/test_execution_*.txt ]; then
            source "$(ls -t "${METRICS_DIR}/results"/test_execution_*.txt | head -1)"
            echo "Test Execution Time: ${value}s"
        fi

        echo ""
        echo "Binary Metrics"
        echo "--------------"
        if [ -f "${METRICS_DIR}/results"/binary_size_*.txt ]; then
            source "$(ls -t "${METRICS_DIR}/results"/binary_size_*.txt | head -1)"
            echo "Binary Size: ${value_mb}MB (Status: $status)"
        fi

    } | tee "$report_file"

    log_info "Regression report saved to: $report_file"
}

################################################################################
# Main Entry Point
################################################################################

main() {
    log_info "Starting regression detection system..."

    local regressions=0

    # Create metrics directory if needed
    mkdir -p "${METRICS_DIR}/results"
    mkdir -p "${METRICS_DIR}/baselines"

    # Run detections
    detect_build_time_regression || regressions=$((regressions + 1))
    detect_test_time_regression || regressions=$((regressions + 1))
    detect_binary_size_regression || regressions=$((regressions + 1))

    # Analyze trends
    analyze_historical_trend "first_build_time" "first_build_*.txt"
    analyze_historical_trend "test_execution_time" "test_execution_*.txt"
    analyze_historical_trend "binary_size_mb" "binary_size_*.txt"

    # Generate report
    generate_regression_report

    echo ""
    if [ $regressions -eq 0 ]; then
        log_info "✅ No significant regressions detected"
        return 0
    else
        log_error "❌ $regressions regression(s) detected"
        return 1
    fi
}

main "$@"
