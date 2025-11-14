#!/bin/bash
# Validate that every module has required tests
#
# Usage: ./scripts/validate-test-coverage.sh <crate>
# Example: ./scripts/validate-test-coverage.sh ggen-core

set -e

CRATE=$1
if [ -z "$CRATE" ]; then
    echo "❌ Missing required argument"
    echo ""
    echo "Usage: $0 <crate>"
    echo ""
    echo "Examples:"
    echo "  $0 ggen-core"
    echo "  $0 ggen-domain"
    echo "  $0 ggen-cli"
    exit 1
fi

# Validate crate exists
if [ ! -d "crates/$CRATE" ]; then
    echo "❌ Crate not found: crates/$CRATE"
    exit 1
fi

echo "🔍 Validating test coverage for $CRATE..."
echo ""

# Find all modules (excluding lib.rs, main.rs, and mod.rs)
MODULES=$(find "crates/$CRATE/src" -name "*.rs" -type f | \
    grep -v "lib.rs" | \
    grep -v "main.rs" | \
    grep -v "mod.rs" | \
    sed 's|.*/||' | \
    sed 's|\.rs||' | \
    sort -u)

TOTAL_MODULES=0
MISSING_UNIT_TESTS=0
MISSING_INTEGRATION_TESTS=0
COVERED_MODULES=0

echo "📊 Module Test Coverage Report"
echo "================================"
echo ""

for MODULE in $MODULES; do
    TOTAL_MODULES=$((TOTAL_MODULES + 1))

    UNIT_TEST="crates/$CRATE/tests/unit/${MODULE}_tests.rs"
    INTEGRATION_TEST="crates/$CRATE/tests/integration/${MODULE}_integration.rs"

    HAS_UNIT_TEST=false
    HAS_INTEGRATION_TEST=false

    if [ -f "$UNIT_TEST" ]; then
        HAS_UNIT_TEST=true
    else
        MISSING_UNIT_TESTS=$((MISSING_UNIT_TESTS + 1))
    fi

    if [ -f "$INTEGRATION_TEST" ]; then
        HAS_INTEGRATION_TEST=true
    fi

    if [ "$HAS_UNIT_TEST" = true ] && [ "$HAS_INTEGRATION_TEST" = true ]; then
        COVERED_MODULES=$((COVERED_MODULES + 1))
        echo "✅ $MODULE"
        echo "   Unit: ✅  Integration: ✅"
    elif [ "$HAS_UNIT_TEST" = true ]; then
        echo "⚠️  $MODULE"
        echo "   Unit: ✅  Integration: ❌"
        MISSING_INTEGRATION_TESTS=$((MISSING_INTEGRATION_TESTS + 1))
    elif [ "$HAS_INTEGRATION_TEST" = true ]; then
        echo "⚠️  $MODULE"
        echo "   Unit: ❌  Integration: ✅"
    else
        echo "❌ $MODULE"
        echo "   Unit: ❌  Integration: ❌"
        MISSING_INTEGRATION_TESTS=$((MISSING_INTEGRATION_TESTS + 1))
    fi

    echo ""
done

echo "================================"
echo "Summary"
echo "================================"
echo ""
echo "Total Modules:             $TOTAL_MODULES"
echo "Fully Covered:             $COVERED_MODULES"
echo "Missing Unit Tests:        $MISSING_UNIT_TESTS"
echo "Missing Integration Tests: $MISSING_INTEGRATION_TESTS"
echo ""

# Calculate coverage percentage
if [ $TOTAL_MODULES -gt 0 ]; then
    COVERAGE_PERCENT=$((COVERED_MODULES * 100 / TOTAL_MODULES))
    echo "Overall Coverage:          $COVERAGE_PERCENT%"
    echo ""
fi

# Determine status
if [ $MISSING_UNIT_TESTS -eq 0 ] && [ $COVERED_MODULES -eq $TOTAL_MODULES ]; then
    echo "✅ All modules have complete test coverage"
    exit 0
elif [ $MISSING_UNIT_TESTS -eq 0 ]; then
    echo "⚠️  All modules have unit tests, but integration tests are incomplete"
    echo ""
    echo "Generate missing integration tests with:"
    echo "  ./scripts/generate-test-skeleton.sh <module> $CRATE"
    exit 0
else
    echo "❌ $MISSING_UNIT_TESTS modules missing unit tests"
    echo ""
    echo "Generate missing tests with:"
    echo "  ./scripts/generate-test-skeleton.sh <module> $CRATE"
    echo ""
    echo "Example:"
    echo "  ./scripts/generate-test-skeleton.sh cache $CRATE"
    exit 1
fi
