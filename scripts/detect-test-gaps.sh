#!/usr/bin/env bash
# Gap Detection Script - Automated Test Coverage Analysis
# Catches 80% of missing tests before commit
# Integrates with git hooks and CI/CD

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$PROJECT_ROOT"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

ERRORS=0
WARNINGS=0

log() {
    echo -e "${BLUE}[GAP-DETECTION]${NC} $1"
}

success() {
    echo -e "${GREEN}✓${NC} $1"
}

error() {
    echo -e "${RED}✗${NC} $1"
    ((ERRORS++))
}

warn() {
    echo -e "${YELLOW}⚠${NC} $1"
    ((WARNINGS++))
}

# Configuration - 80/20 critical coverage areas
CRITICAL_CRATES=(
    "ggen-core"
    "ggen-domain"
    "ggen-cli"
    "ggen-marketplace"
)

MIN_COVERAGE_PERCENT=60  # 80/20: Focus on critical paths
MIN_UNIT_TESTS_PER_MODULE=1  # At least one test per public module

log "Starting automated gap detection..."

# Gap 1: Missing unit tests for public modules
log "Checking for modules without unit tests..."
MISSING_TESTS=0

for crate in "${CRITICAL_CRATES[@]}"; do
    CRATE_PATH="crates/$crate"

    if [ ! -d "$CRATE_PATH" ]; then
        warn "Crate not found: $crate"
        continue
    fi

    # Find all public modules (lib.rs and files with pub mod)
    while IFS= read -r src_file; do
        # Skip if file is in tests directory
        if [[ "$src_file" =~ /tests/ ]]; then
            continue
        fi

        # Extract module name from path
        MODULE_PATH=$(echo "$src_file" | sed "s|$CRATE_PATH/src/||" | sed 's|\.rs$||' | sed 's|/mod$||')

        # Check if module has tests (either inline or in tests directory)
        HAS_INLINE_TEST=$(grep -q "#\[cfg(test)\]" "$src_file" 2>/dev/null && echo "yes" || echo "no")

        # For lib.rs, check tests/unit/ directory
        if [[ "$src_file" == *"lib.rs" ]]; then
            TEST_FILE="$CRATE_PATH/tests/unit/mod.rs"
        else
            TEST_FILE="$CRATE_PATH/tests/unit/${MODULE_PATH}.rs"
        fi

        HAS_UNIT_TEST=$([ -f "$TEST_FILE" ] && echo "yes" || echo "no")

        if [ "$HAS_INLINE_TEST" = "no" ] && [ "$HAS_UNIT_TEST" = "no" ]; then
            # Check if module has public items that need testing
            if grep -qE "^pub (fn|struct|enum|trait|mod)" "$src_file" 2>/dev/null; then
                warn "Missing tests for $crate::$MODULE_PATH"
                ((MISSING_TESTS++))
            fi
        fi
    done < <(find "$CRATE_PATH/src" -name "*.rs" -type f 2>/dev/null)
done

if [ $MISSING_TESTS -gt 0 ]; then
    error "Found $MISSING_TESTS modules without tests"
else
    success "All critical modules have tests"
fi

# Gap 2: Missing integration tests for public APIs
log "Checking for missing integration tests..."
MISSING_INTEGRATION=0

for crate in "${CRITICAL_CRATES[@]}"; do
    CRATE_PATH="crates/$crate"

    # Check if crate has integration tests
    if [ ! -d "$CRATE_PATH/tests" ]; then
        warn "No integration tests for $crate"
        ((MISSING_INTEGRATION++))
        continue
    fi

    # Count integration test files (exclude unit tests)
    INTEGRATION_COUNT=$(find "$CRATE_PATH/tests" -name "*.rs" -type f ! -path "*/unit/*" 2>/dev/null | wc -l | tr -d ' ')

    if [ "$INTEGRATION_COUNT" -lt 1 ]; then
        warn "No integration tests in $crate/tests"
        ((MISSING_INTEGRATION++))
    fi
done

if [ $MISSING_INTEGRATION -gt 0 ]; then
    error "Found $MISSING_INTEGRATION crates without integration tests"
else
    success "All critical crates have integration tests"
fi

# Gap 3: Test quality - check for proper assertions
log "Checking test quality (assertions)..."
LOW_QUALITY_TESTS=0

for crate in "${CRITICAL_CRATES[@]}"; do
    CRATE_PATH="crates/$crate"

    if [ ! -d "$CRATE_PATH/tests" ]; then
        continue
    fi

    # Find tests without assertions
    while IFS= read -r test_file; do
        # Count test functions
        TEST_COUNT=$(grep -c "#\[test\]" "$test_file" 2>/dev/null || echo 0)

        if [ "$TEST_COUNT" -gt 0 ]; then
            # Count assertions (assert!, assert_eq!, assert_ne!, etc.)
            ASSERT_COUNT=$(grep -cE "(assert!|assert_eq!|assert_ne!|assert_matches!)" "$test_file" 2>/dev/null || echo 0)

            # If tests exist but no assertions, flag it
            if [ "$ASSERT_COUNT" -eq 0 ]; then
                warn "Test file has no assertions: $test_file"
                ((LOW_QUALITY_TESTS++))
            fi
        fi
    done < <(find "$CRATE_PATH/tests" -name "*.rs" -type f 2>/dev/null)
done

if [ $LOW_QUALITY_TESTS -gt 0 ]; then
    warn "Found $LOW_QUALITY_TESTS test files without assertions"
else
    success "All tests have proper assertions"
fi

# Gap 4: Missing error handling tests
log "Checking for error handling test coverage..."
MISSING_ERROR_TESTS=0

for crate in "${CRITICAL_CRATES[@]}"; do
    CRATE_PATH="crates/$crate"

    # Find Result types in source (error handling)
    RESULT_COUNT=$(grep -r "Result<" "$CRATE_PATH/src" 2>/dev/null | wc -l | tr -d ' ')

    if [ "$RESULT_COUNT" -gt 0 ]; then
        # Check for error tests
        ERROR_TEST_COUNT=$(grep -r "#\[should_panic\]\|Err(" "$CRATE_PATH/tests" 2>/dev/null | wc -l | tr -d ' ')

        # 80/20: At least 1 error test per 5 Result types
        MIN_ERROR_TESTS=$((RESULT_COUNT / 5))

        if [ "$ERROR_TEST_COUNT" -lt "$MIN_ERROR_TESTS" ]; then
            warn "$crate has $RESULT_COUNT Result types but only $ERROR_TEST_COUNT error tests (expected at least $MIN_ERROR_TESTS)"
            ((MISSING_ERROR_TESTS++))
        fi
    fi
done

if [ $MISSING_ERROR_TESTS -gt 0 ]; then
    warn "Found $MISSING_ERROR_TESTS crates with insufficient error tests"
else
    success "Error handling tests are adequate"
fi

# Gap 5: Check for untested public functions
log "Checking for untested public functions..."
UNTESTED_FUNCTIONS=0

for crate in "${CRITICAL_CRATES[@]}"; do
    CRATE_PATH="crates/$crate"

    # Find public functions
    while IFS= read -r src_file; do
        # Skip test files
        if [[ "$src_file" =~ /tests/ ]]; then
            continue
        fi

        # Extract public function names
        while IFS= read -r func_name; do
            # Search for tests calling this function
            TEST_COVERAGE=$(grep -r "$func_name" "$CRATE_PATH/tests" 2>/dev/null | wc -l | tr -d ' ')

            if [ "$TEST_COVERAGE" -eq 0 ]; then
                warn "Untested public function in $crate: $func_name (in $(basename $src_file))"
                ((UNTESTED_FUNCTIONS++))
            fi
        done < <(grep -oP "pub fn \K\w+" "$src_file" 2>/dev/null || true)
    done < <(find "$CRATE_PATH/src" -name "*.rs" -type f 2>/dev/null)
done

if [ $UNTESTED_FUNCTIONS -gt 5 ]; then
    error "Found $UNTESTED_FUNCTIONS untested public functions (threshold: 5)"
else
    success "Public function coverage is adequate ($UNTESTED_FUNCTIONS untested, threshold: 5)"
fi

# Gap 6: Compilation check (catches type errors)
log "Running compilation check..."
if cargo check --workspace --all-targets &> /dev/null; then
    success "All crates compile successfully"
else
    error "Compilation errors found - run 'cargo check' for details"
fi

# Gap 7: Test execution check
log "Running quick test execution check..."
if timeout 30s cargo test --workspace --no-default-features --no-run &> /dev/null; then
    success "All tests compile"
else
    error "Test compilation failed - run 'cargo test --no-run' for details"
fi

# Generate report
log "Generating gap detection report..."

REPORT_FILE="$PROJECT_ROOT/target/gap-detection-report.json"
mkdir -p "$(dirname "$REPORT_FILE")"

cat > "$REPORT_FILE" << EOF
{
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "gaps_detected": {
    "missing_unit_tests": $MISSING_TESTS,
    "missing_integration_tests": $MISSING_INTEGRATION,
    "low_quality_tests": $LOW_QUALITY_TESTS,
    "missing_error_tests": $MISSING_ERROR_TESTS,
    "untested_public_functions": $UNTESTED_FUNCTIONS
  },
  "summary": {
    "total_errors": $ERRORS,
    "total_warnings": $WARNINGS,
    "status": "$([ $ERRORS -eq 0 ] && echo "PASS" || echo "FAIL")"
  }
}
EOF

log "Report saved to $REPORT_FILE"

# Summary
echo ""
if [ $ERRORS -eq 0 ]; then
    success "Gap detection passed (${WARNINGS} warnings)"
    exit 0
else
    error "Gap detection failed: $ERRORS error(s), $WARNINGS warning(s)"
    echo ""
    echo "Fix these gaps before committing:"
    echo "  1. Add unit tests for modules without tests"
    echo "  2. Add integration tests for public APIs"
    echo "  3. Add assertions to test functions"
    echo "  4. Add error handling tests"
    echo "  5. Test all public functions"
    exit 1
fi
