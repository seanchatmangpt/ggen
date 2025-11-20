#!/usr/bin/env bash
# Metrics collection script for ggen (Week 2 efficiency improvement)
#
# Collects daily build metrics and stores them in .metrics/ directory
# Metrics tracked:
# - Build time (debug and release)
# - Test pass rate
# - Compiler errors/warnings
# - Template accessibility (from build.rs)
# - Code coverage
# - Memory usage

set -euo pipefail

# Configuration
METRICS_DIR=".metrics"
DATE=$(date +%Y-%m-%d)
TIMESTAMP=$(date +%s)
METRICS_FILE="${METRICS_DIR}/daily_${DATE}.json"

# Create metrics directory if it doesn't exist
mkdir -p "${METRICS_DIR}"

echo "📊 Collecting metrics for ${DATE}..."
echo ""

# Initialize metrics object
METRICS="{}"

# Function to add metric
add_metric() {
  local key="$1"
  local value="$2"
  METRICS=$(echo "${METRICS}" | jq --arg key "$key" --argjson value "$value" '. + {($key): $value}')
}

# 1. Measure build time (debug)
echo "1️⃣ Measuring debug build time..."
START=$(date +%s)
if timeout 15s cargo build --quiet 2>/dev/null; then
  END=$(date +%s)
  BUILD_TIME=$((END - START))
  add_metric "build_time_debug_seconds" "$BUILD_TIME"
  echo "   ✅ Debug build: ${BUILD_TIME}s"
else
  add_metric "build_time_debug_seconds" "null"
  echo "   ⚠️  Debug build timed out"
fi
echo ""

# 2. Measure build time (release)
echo "2️⃣ Measuring release build time..."
START=$(date +%s)
if timeout 30s cargo build --release --quiet 2>/dev/null; then
  END=$(date +%s)
  BUILD_TIME=$((END - START))
  add_metric "build_time_release_seconds" "$BUILD_TIME"
  echo "   ✅ Release build: ${BUILD_TIME}s"
else
  add_metric "build_time_release_seconds" "null"
  echo "   ⚠️  Release build timed out"
fi
echo ""

# 3. Check compiler errors/warnings
echo "3️⃣ Checking compiler output..."
if COMPILER_OUTPUT=$(cargo check 2>&1); then
  ERRORS=$(echo "$COMPILER_OUTPUT" | grep -c "error:" || echo "0")
  WARNINGS=$(echo "$COMPILER_OUTPUT" | grep -c "warning:" || echo "0")
  add_metric "compiler_errors" "$ERRORS"
  add_metric "compiler_warnings" "$WARNINGS"
  echo "   ✅ Errors: $ERRORS, Warnings: $WARNINGS"
else
  add_metric "compiler_errors" "null"
  add_metric "compiler_warnings" "null"
  echo "   ⚠️  Compiler check failed"
fi
echo ""

# 4. Run tests and measure pass rate
echo "4️⃣ Running tests..."
if TEST_OUTPUT=$(cargo test --workspace --no-fail-fast -- --nocapture 2>&1); then
  TEST_PASSED=$(echo "$TEST_OUTPUT" | grep "test result:" | grep -oE "[0-9]+ passed" | grep -oE "[0-9]+" || echo "0")
  TEST_FAILED=$(echo "$TEST_OUTPUT" | grep "test result:" | grep -oE "[0-9]+ failed" | grep -oE "[0-9]+" || echo "0")
  TOTAL_TESTS=$((TEST_PASSED + TEST_FAILED))

  if [ "$TOTAL_TESTS" -gt 0 ]; then
    PASS_RATE=$(echo "scale=2; $TEST_PASSED * 100 / $TOTAL_TESTS" | bc)
  else
    PASS_RATE="0"
  fi

  add_metric "test_passed" "$TEST_PASSED"
  add_metric "test_failed" "$TEST_FAILED"
  add_metric "test_pass_rate_percent" "$PASS_RATE"
  echo "   ✅ Tests: ${TEST_PASSED} passed, ${TEST_FAILED} failed (${PASS_RATE}% pass rate)"
else
  add_metric "test_passed" "null"
  add_metric "test_failed" "null"
  add_metric "test_pass_rate_percent" "null"
  echo "   ⚠️  Test execution failed"
fi
echo ""

# 5. Count templates discovered by build.rs
echo "5️⃣ Counting discovered templates..."
if TEMPLATE_COUNT=$(find templates -name "*.tmpl" -type f | wc -l | tr -d ' '); then
  add_metric "templates_discovered" "$TEMPLATE_COUNT"
  echo "   ✅ Templates discovered: $TEMPLATE_COUNT"
else
  add_metric "templates_discovered" "null"
  echo "   ⚠️  Template count failed"
fi
echo ""

# 6. Measure memory usage
echo "6️⃣ Measuring binary size..."
if [ -f "target/release/ggen" ]; then
  BINARY_SIZE=$(stat -f%z "target/release/ggen" 2>/dev/null || stat -c%s "target/release/ggen" 2>/dev/null || echo "0")
  BINARY_SIZE_MB=$(echo "scale=2; $BINARY_SIZE / 1024 / 1024" | bc)
  add_metric "binary_size_mb" "$BINARY_SIZE_MB"
  echo "   ✅ Binary size: ${BINARY_SIZE_MB}MB"
else
  add_metric "binary_size_mb" "null"
  echo "   ⚠️  Release binary not found"
fi
echo ""

# 7. Add metadata
add_metric "date" "\"${DATE}\""
add_metric "timestamp" "$TIMESTAMP"
add_metric "git_commit" "\"$(git rev-parse --short HEAD 2>/dev/null || echo 'unknown')\""

# Write metrics to file
echo "$METRICS" | jq '.' > "${METRICS_FILE}"

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "✅ Metrics collected successfully!"
echo "📁 Saved to: ${METRICS_FILE}"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
echo "📊 Summary:"
cat "${METRICS_FILE}" | jq '.'
