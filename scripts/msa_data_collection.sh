#!/bin/bash
# MSA Data Collection Script for Feature 004
# Collects repeatability and reproducibility measurements for test suite
# DfLSS Measure Phase: Measurement Systems Analysis

set -euo pipefail

# Configuration
OUTPUT_DIR="/Users/sac/ggen/specs/004-optimize-test-concurrency/dflss/msa_data"
mkdir -p "$OUTPUT_DIR"

TIMESTAMP=$(date +%Y%m%d_%H%M%S)
DATA_FILE="$OUTPUT_DIR/msa_raw_data_$TIMESTAMP.csv"
LOG_FILE="$OUTPUT_DIR/msa_collection_$TIMESTAMP.log"

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging function
log() {
  echo -e "${BLUE}[$(date +'%Y-%m-%d %H:%M:%S')]${NC} $*" | tee -a "$LOG_FILE"
}

log_success() {
  echo -e "${GREEN}✅ $*${NC}" | tee -a "$LOG_FILE"
}

log_warning() {
  echo -e "${YELLOW}⚠️  $*${NC}" | tee -a "$LOG_FILE"
}

log_error() {
  echo -e "${RED}❌ $*${NC}" | tee -a "$LOG_FILE"
}

# Initialize CSV file
echo "Part,Operator,Trial,Duration_ms,Test_Count,Pass_Count,Fail_Count,Status,Timestamp" > "$DATA_FILE"

# Test suites to measure (representing critical paths)
TESTS=(
  "aci::mod"
  "aci::skill_invocation_tests"
  "aci::timeout_enforcement_tests"
  "aci::tool_selection_tests"
  "unit::version_resolution"
  "unit::registry_client"
  "integration::end_to_end_flow"
  "security::signature_verification"
  "security::dos_resistance"
  "chicago_tdd_smoke_test"
)

# Operators (simulated environmental conditions)
OPERATORS=("Low_Load" "Medium_Load" "High_Load")
TRIALS=3

# Function: Run single test measurement
run_test_measurement() {
  local test_name=$1
  local operator=$2
  local trial=$3

  log "Measuring: ${test_name} | Operator: ${operator} | Trial: ${trial}"

  # Use perl for high-resolution timing (portable across macOS and Linux)
  local start_ms=$(perl -MTime::HiRes=time -e 'printf "%.0f\n", time() * 1000')
  local test_output
  local exit_code

  # Run test and capture output
  # Try to determine package from test name
  local package="ggen-core"  # Default package
  if test_output=$(timeout 300s cargo test --package "$package" --test "${test_name}" --quiet 2>&1); then
    exit_code=0
  else
    exit_code=$?
  fi

  local end_ms=$(perl -MTime::HiRes=time -e 'printf "%.0f\n", time() * 1000')
  local duration_ms=$((end_ms - start_ms))

  # Parse test results
  local test_count=0
  local pass_count=0
  local fail_count=0
  local status="UNKNOWN"

  if echo "$test_output" | grep -q "test result: ok"; then
    status="PASS"
    pass_count=$(echo "$test_output" | grep -oP '\d+(?= passed)' | head -1 || echo "0")
    test_count=$pass_count
  elif echo "$test_output" | grep -q "test result: FAILED"; then
    status="FAIL"
    pass_count=$(echo "$test_output" | grep -oP '\d+(?= passed)' | head -1 || echo "0")
    fail_count=$(echo "$test_output" | grep -oP '\d+(?= failed)' | head -1 || echo "0")
    test_count=$((pass_count + fail_count))
  elif [ $exit_code -eq 124 ]; then
    status="TIMEOUT"
    duration_ms=300000  # 5 minutes
  else
    status="ERROR"
  fi

  # Write to CSV
  echo "${test_name},${operator},${trial},${duration_ms},${test_count},${pass_count},${fail_count},${status},$(date -u +%Y-%m-%dT%H:%M:%SZ)" >> "$DATA_FILE"

  if [ "$status" = "PASS" ]; then
    log_success "${test_name}: ${duration_ms}ms (${test_count} tests)"
  elif [ "$status" = "TIMEOUT" ]; then
    log_error "${test_name}: TIMEOUT (>300s)"
  else
    log_warning "${test_name}: ${status} (${duration_ms}ms)"
  fi
}

# ============================================================================
# REPEATABILITY STUDY (Operator 1: Low Load - Cold Cache)
# ============================================================================

log "=========================================="
log "Starting REPEATABILITY STUDY (Low Load)"
log "Condition: Cold cache, no cargo lock contention"
log "=========================================="

for test in "${TESTS[@]}"; do
  log "Test Suite: ${test}"

  for trial in $(seq 1 $TRIALS); do
    # Clean build artifacts to simulate cold cache
    log "  Trial ${trial}: Cleaning build cache..."
    cargo clean >/dev/null 2>&1 || true

    # Wait for filesystem stabilization
    sleep 2

    # Run measurement
    run_test_measurement "$test" "Low_Load" "$trial"

    # Brief pause between trials
    sleep 1
  done

  log ""
done

# ============================================================================
# REPRODUCIBILITY STUDY (Operator 2: Medium Load - Warm Cache)
# ============================================================================

log "=========================================="
log "Starting REPRODUCIBILITY STUDY (Medium Load)"
log "Condition: Warm cache, incremental compilation"
log "=========================================="

# Pre-warm cache
log "Pre-warming cache with initial build..."
cargo build --workspace --all-targets >/dev/null 2>&1 || true

for test in "${TESTS[@]}"; do
  log "Test Suite: ${test}"

  for trial in $(seq 1 $TRIALS); do
    # No clean - use warm cache for incremental compilation
    log "  Trial ${trial}: Using warm cache (incremental)..."

    # Run measurement
    run_test_measurement "$test" "Medium_Load" "$trial"

    # Brief pause between trials
    sleep 1
  done

  log ""
done

# ============================================================================
# REPRODUCIBILITY STUDY (Operator 3: High Load - System Stress)
# ============================================================================

log "=========================================="
log "Starting REPRODUCIBILITY STUDY (High Load)"
log "Condition: Concurrent cargo processes, system stress"
log "=========================================="

# Check if stress-ng is available
if command -v stress-ng >/dev/null 2>&1; then
  STRESS_AVAILABLE=true
  log "stress-ng available: Will simulate system load"
else
  STRESS_AVAILABLE=false
  log_warning "stress-ng not available: Install with 'brew install stress-ng' for full MSA"
  log_warning "Simulating high load with background cargo build instead"
fi

for test in "${TESTS[@]}"; do
  log "Test Suite: ${test}"

  for trial in $(seq 1 $TRIALS); do
    log "  Trial ${trial}: Starting background load..."

    # Start background load simulation
    if $STRESS_AVAILABLE; then
      stress-ng --cpu 2 --timeout 60s >/dev/null 2>&1 &
      STRESS_PID=$!
      log "    Background stress PID: $STRESS_PID"
    else
      # Alternative: Background cargo build to simulate load
      cargo build --workspace --all-targets >/dev/null 2>&1 &
      STRESS_PID=$!
      log "    Background cargo build PID: $STRESS_PID"
    fi

    # Wait for stress to ramp up
    sleep 2

    # Run measurement under load
    run_test_measurement "$test" "High_Load" "$trial"

    # Kill background stress
    if kill "$STRESS_PID" 2>/dev/null; then
      log "    Killed background load PID $STRESS_PID"
    fi

    # Brief pause between trials
    sleep 2
  done

  log ""
done

# ============================================================================
# DATA COLLECTION SUMMARY
# ============================================================================

log "=========================================="
log "MSA DATA COLLECTION COMPLETE"
log "=========================================="
log_success "Data file: ${DATA_FILE}"
log_success "Log file: ${LOG_FILE}"

# Count measurements
TOTAL_MEASUREMENTS=$(tail -n +2 "$DATA_FILE" | wc -l | tr -d ' ')
PASS_COUNT=$(grep -c ",PASS," "$DATA_FILE" || echo "0")
FAIL_COUNT=$(grep -c ",FAIL," "$DATA_FILE" || echo "0")
TIMEOUT_COUNT=$(grep -c ",TIMEOUT," "$DATA_FILE" || echo "0")

log ""
log "Summary Statistics:"
log "  Total Measurements: ${TOTAL_MEASUREMENTS}"
log "  Passed: ${PASS_COUNT} ($(awk "BEGIN {printf \"%.1f\", (${PASS_COUNT}/${TOTAL_MEASUREMENTS})*100}")%)"
log "  Failed: ${FAIL_COUNT}"
log "  Timeouts: ${TIMEOUT_COUNT}"

log ""
log "Next Steps:"
log "  1. Run MSA analysis: python3 scripts/msa_analysis.py ${DATA_FILE}"
log "  2. Review Gage R&R results (target: <10% acceptable, <30% marginal)"
log "  3. Calculate process capability (Cp/Cpk ≥ 1.33 for 6σ quality)"

log ""
log_success "MSA data collection completed successfully!"
