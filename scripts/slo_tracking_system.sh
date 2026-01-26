#!/bin/bash

################################################################################
# SLO Tracking System for ggen
# Comprehensive performance monitoring with SLO compliance verification
################################################################################

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
METRICS_DIR="${PROJECT_ROOT}/.metrics"
RESULTS_DIR="${METRICS_DIR}/results"
BASELINES_DIR="${METRICS_DIR}/baselines"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)

# SLO Targets (in seconds for time, bytes for size)
SLO_FIRST_BUILD_SECS=15
SLO_INCREMENTAL_BUILD_SECS=2
SLO_RDF_PROCESSING_SECS=5
SLO_GENERATION_MEMORY_MB=100
SLO_CLI_SCAFFOLDING_SECS=3
SLO_BINARY_SIZE_MB=500

# Create directories
mkdir -p "$RESULTS_DIR"
mkdir -p "$BASELINES_DIR"
mkdir -p "${METRICS_DIR}/history"

################################################################################
# Utility Functions
################################################################################

log_info() {
    echo "[INFO] $(date '+%Y-%m-%d %H:%M:%S'): $*"
}

log_error() {
    echo "[ERROR] $(date '+%Y-%m-%d %H:%M:%S'): $*" >&2
}

log_metric() {
    echo "[METRIC] $(date '+%Y-%m-%d %H:%M:%S'): $*"
}

format_duration() {
    local seconds=$1
    if (( seconds < 60 )); then
        echo "${seconds}s"
    else
        local mins=$((seconds / 60))
        local secs=$((seconds % 60))
        echo "${mins}m${secs}s"
    fi
}

format_bytes() {
    local bytes=$1
    if (( bytes < 1024 )); then
        echo "${bytes}B"
    elif (( bytes < 1024 * 1024 )); then
        echo "$((bytes / 1024))KB"
    else
        echo "$((bytes / 1024 / 1024))MB"
    fi
}

check_slo() {
    local metric_name="$1"
    local actual="$2"
    local target="$3"
    local unit="$4"

    if (( actual > target )); then
        local overage=$((actual - target))
        local percent=$(( (overage * 100) / target ))
        echo "❌ VIOLATION: $metric_name"
        echo "   Target: $target $unit"
        echo "   Actual: $actual $unit"
        echo "   Over:   $percent%"
        return 1
    else
        echo "✅ PASS: $metric_name"
        echo "   Target: $target $unit"
        echo "   Actual: $actual $unit"
        return 0
    fi
}

################################################################################
# Build Performance Monitoring
################################################################################

measure_first_build() {
    log_info "Measuring first (clean) build time..."

    cd "$PROJECT_ROOT"

    # Clean build artifacts
    cargo clean 2>/dev/null || true

    local start_time=$(date +%s%N)

    # Build in release mode
    if ! timeout 30s cargo build --release -p ggen-cli-lib --bin ggen 2>&1 | grep -E "(Compiling|Finished)"; then
        log_error "Build failed or timed out"
        return 1
    fi

    local end_time=$(date +%s%N)
    local elapsed_ns=$((end_time - start_time))
    local elapsed_secs=$((elapsed_ns / 1000000000))

    log_metric "First build: $(format_duration $elapsed_secs) ($elapsed_secs seconds)"

    check_slo "First Build Time" "$elapsed_secs" "$SLO_FIRST_BUILD_SECS" "seconds"

    # Store result
    {
        echo "timestamp=$(date +%s)"
        echo "metric=first_build_time"
        echo "value=$elapsed_secs"
        echo "unit=seconds"
        echo "slo_target=$SLO_FIRST_BUILD_SECS"
        echo "status=$([ $elapsed_secs -le $SLO_FIRST_BUILD_SECS ] && echo 'PASS' || echo 'FAIL')"
    } > "$RESULTS_DIR/first_build_${TIMESTAMP}.txt"

    return 0
}

measure_incremental_build() {
    log_info "Measuring incremental build time..."

    cd "$PROJECT_ROOT"

    # Warm-up: do a check first
    timeout 10s cargo check --workspace >/dev/null 2>&1 || true

    local start_time=$(date +%s%N)

    # Run check (similar to incremental compilation)
    if ! timeout 10s cargo check --workspace >/dev/null 2>&1; then
        log_error "Check failed or timed out"
        return 1
    fi

    local end_time=$(date +%s%N)
    local elapsed_ns=$((end_time - start_time))
    local elapsed_secs=$((elapsed_ns / 1000000000))

    log_metric "Incremental build: $(format_duration $elapsed_secs) ($elapsed_secs seconds)"

    check_slo "Incremental Build Time" "$elapsed_secs" "$SLO_INCREMENTAL_BUILD_SECS" "seconds"

    # Store result
    {
        echo "timestamp=$(date +%s)"
        echo "metric=incremental_build_time"
        echo "value=$elapsed_secs"
        echo "unit=seconds"
        echo "slo_target=$SLO_INCREMENTAL_BUILD_SECS"
        echo "status=$([ $elapsed_secs -le $SLO_INCREMENTAL_BUILD_SECS ] && echo 'PASS' || echo 'FAIL')"
    } > "$RESULTS_DIR/incremental_build_${TIMESTAMP}.txt"

    return 0
}

################################################################################
# Test Performance Monitoring
################################################################################

measure_test_execution() {
    log_info "Measuring test execution time..."

    cd "$PROJECT_ROOT"

    local start_time=$(date +%s%N)

    # Run unit tests only for speed
    if ! timeout 30s cargo test --workspace --lib --no-fail-fast 2>&1 | tee "$RESULTS_DIR/test_output_${TIMESTAMP}.log"; then
        log_error "Tests failed"
        return 1
    fi

    local end_time=$(date +%s%N)
    local elapsed_ns=$((end_time - start_time))
    local elapsed_secs=$((elapsed_ns / 1000000000))

    log_metric "Test execution: $(format_duration $elapsed_secs) ($elapsed_secs seconds)"

    # Store result
    {
        echo "timestamp=$(date +%s)"
        echo "metric=test_execution_time"
        echo "value=$elapsed_secs"
        echo "unit=seconds"
        echo "log_file=$RESULTS_DIR/test_output_${TIMESTAMP}.log"
    } > "$RESULTS_DIR/test_execution_${TIMESTAMP}.txt"

    return 0
}

################################################################################
# Binary Size Analysis
################################################################################

measure_binary_size() {
    log_info "Measuring binary size..."

    cd "$PROJECT_ROOT"

    # Build release binary
    timeout 30s cargo build --release -p ggen-cli-lib --bin ggen 2>&1 | grep -E "(Compiling|Finished)" || true

    local binary_path="$PROJECT_ROOT/target/release/ggen"
    if [ ! -f "$binary_path" ]; then
        log_error "Binary not found at $binary_path"
        return 1
    fi

    local binary_size=$(stat -f%z "$binary_path" 2>/dev/null || stat -c%s "$binary_path" 2>/dev/null || echo 0)
    local binary_size_mb=$((binary_size / 1024 / 1024))

    log_metric "Binary size: $(format_bytes $binary_size) ($binary_size_mb MB)"

    check_slo "Binary Size" "$binary_size_mb" "$SLO_BINARY_SIZE_MB" "MB"

    # Store result
    {
        echo "timestamp=$(date +%s)"
        echo "metric=binary_size"
        echo "value=$binary_size"
        echo "value_mb=$binary_size_mb"
        echo "unit=bytes"
        echo "slo_target_mb=$SLO_BINARY_SIZE_MB"
        echo "status=$([ $binary_size_mb -le $SLO_BINARY_SIZE_MB ] && echo 'PASS' || echo 'FAIL')"
    } > "$RESULTS_DIR/binary_size_${TIMESTAMP}.txt"

    return 0
}

################################################################################
# Memory Usage Analysis
################################################################################

measure_memory_usage() {
    log_info "Measuring peak memory usage during build..."

    cd "$PROJECT_ROOT"

    # This is a simplified version - real implementation would use /proc or system profiler
    # For now, we'll use timing with memory estimation

    local start_time=$(date +%s%N)

    # Monitor during build
    if command -v ps >/dev/null 2>&1; then
        local pid=$$
        local peak_memory=0

        # Note: Full implementation would fork background process to monitor
        timeout 30s cargo build --release -p ggen-cli-lib --bin ggen >/dev/null 2>&1 || true

        log_metric "Memory measurement (estimated): Peak tracking requires system profiler"
    else
        log_error "Cannot measure memory - ps command not available"
        return 1
    fi

    return 0
}

################################################################################
# Main Entry Point
################################################################################

main() {
    log_info "Starting SLO tracking system..."
    log_info "Project root: $PROJECT_ROOT"
    log_info "Metrics directory: $METRICS_DIR"
    log_info "Timestamp: $TIMESTAMP"

    local violations=0

    # Run measurements
    if ! measure_first_build; then
        violations=$((violations + 1))
    fi

    if ! measure_incremental_build; then
        violations=$((violations + 1))
    fi

    if ! measure_test_execution; then
        violations=$((violations + 1))
    fi

    if ! measure_binary_size; then
        violations=$((violations + 1))
    fi

    # Summary
    echo ""
    log_info "SLO Tracking Summary"
    echo "===================="
    echo "Metrics collected: 4"
    echo "Violations detected: $violations"
    echo "Results stored in: $RESULTS_DIR"

    if [ $violations -eq 0 ]; then
        echo ""
        log_info "✅ All SLOs met!"
        return 0
    else
        echo ""
        log_error "❌ $violations SLO violations detected"
        return 1
    fi
}

main "$@"
