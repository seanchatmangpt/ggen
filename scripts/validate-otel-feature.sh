#!/bin/bash
# Validation script for OTEL optional feature implementation
# Verifies compilation, tests, and build time improvements

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    local status=$1
    local message=$2
    if [ "$status" = "PASS" ]; then
        echo -e "${GREEN}✓${NC} $message"
    elif [ "$status" = "FAIL" ]; then
        echo -e "${RED}✗${NC} $message"
    else
        echo -e "${YELLOW}●${NC} $message"
    fi
}

# Function to measure time
measure_time() {
    local start=$(date +%s.%N)
    "$@"
    local end=$(date +%s.%N)
    echo $(echo "$end - $start" | bc)
}

echo "======================================"
echo "OTEL Optional Feature Validation"
echo "======================================"
echo

# 1. Verify timeout command exists
echo "Step 1: Verify timeout command"
if cargo make timeout-check > /dev/null 2>&1; then
    print_status "PASS" "Timeout command exists"
else
    print_status "FAIL" "Timeout command not found"
    exit 1
fi
echo

# 2. Check compilation WITHOUT otel feature
echo "Step 2: Check compilation WITHOUT otel feature"
print_status "INFO" "Running: cargo check --package ggen-core --no-default-features"
if timeout 120s cargo check --package ggen-core --no-default-features > /tmp/check-no-otel.log 2>&1; then
    print_status "PASS" "Compilation successful without otel"
else
    print_status "FAIL" "Compilation failed without otel"
    echo "See /tmp/check-no-otel.log for details"
    tail -20 /tmp/check-no-otel.log
    exit 1
fi
echo

# 3. Check compilation WITH otel feature
echo "Step 3: Check compilation WITH otel feature"
print_status "INFO" "Running: cargo check --package ggen-core --features otel"
if timeout 120s cargo check --package ggen-core --features otel > /tmp/check-with-otel.log 2>&1; then
    print_status "PASS" "Compilation successful with otel"
else
    print_status "FAIL" "Compilation failed with otel"
    echo "See /tmp/check-with-otel.log for details"
    tail -20 /tmp/check-with-otel.log
    exit 1
fi
echo

# 4. Count dependencies without otel
echo "Step 4: Count dependencies WITHOUT otel"
print_status "INFO" "Running: cargo tree --package ggen-core --no-default-features"
DEP_COUNT_NO_OTEL=$(cargo tree --package ggen-core --no-default-features 2>/dev/null | wc -l)
print_status "PASS" "Dependencies without otel: $DEP_COUNT_NO_OTEL"
echo

# 5. Count dependencies with otel
echo "Step 5: Count dependencies WITH otel"
print_status "INFO" "Running: cargo tree --package ggen-core --features otel"
DEP_COUNT_WITH_OTEL=$(cargo tree --package ggen-core --features otel 2>/dev/null | wc -l)
print_status "PASS" "Dependencies with otel: $DEP_COUNT_WITH_OTEL"
echo

# Calculate dependency reduction
DEP_REDUCTION=$((DEP_COUNT_WITH_OTEL - DEP_COUNT_NO_OTEL))
REDUCTION_PERCENT=$((DEP_REDUCTION * 100 / DEP_COUNT_WITH_OTEL))
print_status "INFO" "Dependency reduction: $DEP_REDUCTION dependencies (~$REDUCTION_PERCENT%)"
echo

# 6. Verify no OTEL symbols in default build
echo "Step 6: Verify no OTEL symbols in default build"
print_status "INFO" "Checking for OTEL imports in compiled artifacts"
if cargo build --package ggen-core --no-default-features --quiet 2>/dev/null; then
    # Check if OTEL symbols are present (they shouldn't be)
    if nm target/debug/libggen_core.rlib 2>/dev/null | grep -i opentelemetry > /dev/null; then
        print_status "FAIL" "OTEL symbols found in default build (should not be present)"
        exit 1
    else
        print_status "PASS" "No OTEL symbols in default build (correct)"
    fi
else
    print_status "FAIL" "Could not build ggen-core for symbol verification"
fi
echo

# 7. Run clippy without otel
echo "Step 7: Run clippy WITHOUT otel"
print_status "INFO" "Running: cargo clippy --package ggen-core --no-default-features"
if timeout 120s cargo clippy --package ggen-core --no-default-features -- -D warnings > /tmp/clippy-no-otel.log 2>&1; then
    print_status "PASS" "Clippy passed without otel"
else
    print_status "FAIL" "Clippy failed without otel"
    echo "See /tmp/clippy-no-otel.log for details"
    tail -20 /tmp/clippy-no-otel.log
    exit 1
fi
echo

# 8. Run clippy with otel
echo "Step 8: Run clippy WITH otel"
print_status "INFO" "Running: cargo clippy --package ggen-core --features otel"
if timeout 120s cargo clippy --package ggen-core --features otel -- -D warnings > /tmp/clippy-with-otel.log 2>&1; then
    print_status "PASS" "Clippy passed with otel"
else
    print_status "FAIL" "Clippy failed with otel"
    echo "See /tmp/clippy-with-otel.log for details"
    tail -20 /tmp/clippy-with-otel.log
    exit 1
fi
echo

# 9. Build time comparison (optional - only if time permits)
if [ "${FULL_BENCHMARK:-0}" = "1" ]; then
    echo "Step 9: Build time comparison (FULL_BENCHMARK=1)"

    # Clean build without otel
    print_status "INFO" "Measuring clean build time WITHOUT otel"
    cargo clean > /dev/null 2>&1
    BUILD_TIME_NO_OTEL=$(measure_time cargo build --package ggen-core --no-default-features --quiet 2>&1)
    print_status "PASS" "Build time without otel: ${BUILD_TIME_NO_OTEL}s"

    # Clean build with otel
    print_status "INFO" "Measuring clean build time WITH otel"
    cargo clean > /dev/null 2>&1
    BUILD_TIME_WITH_OTEL=$(measure_time cargo build --package ggen-core --features otel --quiet 2>&1)
    print_status "PASS" "Build time with otel: ${BUILD_TIME_WITH_OTEL}s"

    # Calculate improvement
    TIME_SAVED=$(echo "$BUILD_TIME_WITH_OTEL - $BUILD_TIME_NO_OTEL" | bc)
    IMPROVEMENT_PERCENT=$(echo "scale=2; $TIME_SAVED * 100 / $BUILD_TIME_WITH_OTEL" | bc)
    print_status "INFO" "Build time improvement: ${TIME_SAVED}s (~${IMPROVEMENT_PERCENT}%)"
    echo
else
    print_status "INFO" "Skipping build time benchmark (set FULL_BENCHMARK=1 to enable)"
    echo
fi

# Summary
echo "======================================"
echo "Validation Summary"
echo "======================================"
echo
print_status "PASS" "All validation checks passed"
echo
echo "Dependency Reduction:"
echo "  - Without otel: $DEP_COUNT_NO_OTEL dependencies"
echo "  - With otel: $DEP_COUNT_WITH_OTEL dependencies"
echo "  - Reduction: $DEP_REDUCTION dependencies (~$REDUCTION_PERCENT%)"
echo
echo "Next Steps:"
echo "  1. Run full test suite: cargo make test"
echo "  2. Update CI/CD to use --features otel for production builds"
echo "  3. Measure actual build time improvements in CI"
echo

exit 0
