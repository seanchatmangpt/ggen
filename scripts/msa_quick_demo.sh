#!/bin/bash
# Quick MSA demonstration with single test suite
# Shows measurement variation across 3 runs

set -euo pipefail

OUTPUT_DIR="/Users/sac/ggen/specs/004-optimize-test-concurrency/dflss/msa_data"
mkdir -p "$OUTPUT_DIR"

TIMESTAMP=$(date +%Y%m%d_%H%M%S)
DATA_FILE="$OUTPUT_DIR/msa_demo_$TIMESTAMP.csv"

echo "Part,Operator,Trial,Duration_ms,Test_Count,Pass_Count,Fail_Count,Status,Timestamp" > "$DATA_FILE"

echo "🔍 Running MSA Quick Demo (single test suite, 3 conditions × 3 trials)"
echo ""

# Test to measure
TEST_NAME="chicago_tdd_smoke_test"

# Function to run test and measure
run_test() {
  local operator=$1
  local trial=$2

  echo -n "  ${operator} Trial ${trial}: "

  # Use perl for high-resolution timing (portable across macOS and Linux)
  local start_ms=$(perl -MTime::HiRes=time -e 'printf "%.0f\n", time() * 1000')

  if timeout 60s cargo test --package ggen-core --test "$TEST_NAME" --quiet 2>&1 | grep -q "test result: ok"; then
    local status="PASS"
  else
    local status="FAIL"
  fi

  local end_ms=$(perl -MTime::HiRes=time -e 'printf "%.0f\n", time() * 1000')
  local duration_ms=$((end_ms - start_ms))

  echo "${TEST_NAME},${operator},${trial},${duration_ms},1,1,0,${status},$(date -u +%Y-%m-%dT%H:%M:%SZ)" >> "$DATA_FILE"

  echo "${duration_ms}ms (${status})"
}

# Operator 1: Low Load (cold cache)
echo "Operator 1: Low Load (cold cache)"
for trial in {1..3}; do
  cargo clean >/dev/null 2>&1
  sleep 1
  run_test "Low_Load" "$trial"
done
echo ""

# Operator 2: Medium Load (warm cache)
echo "Operator 2: Medium Load (warm cache)"
cargo build --workspace >/dev/null 2>&1  # Pre-warm
for trial in {1..3}; do
  run_test "Medium_Load" "$trial"
  sleep 0.5
done
echo ""

# Operator 3: High Load (background stress)
echo "Operator 3: High Load (background cargo build)"
for trial in {1..3}; do
  # Background load
  cargo build --workspace >/dev/null 2>&1 &
  BG_PID=$!
  sleep 1

  run_test "High_Load" "$trial"

  kill $BG_PID 2>/dev/null || true
  sleep 1
done
echo ""

echo "✅ Data collected: ${DATA_FILE}"
echo ""
echo "Quick Statistics:"
tail -n +2 "$DATA_FILE" | awk -F',' '{sum+=$4; count++} END {print "  Mean: " sum/count " ms"}'
tail -n +2 "$DATA_FILE" | awk -F',' '{if(min==""){min=max=$4}; if($4>max) {max=$4}; if($4<min) {min=$4}} END {print "  Range: " min " - " max " ms (" max-min " ms variation)"}'

echo ""
echo "Run full analysis:"
echo "  python3 scripts/msa_analysis.py ${DATA_FILE}"
