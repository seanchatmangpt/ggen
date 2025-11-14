#!/bin/bash
# Validate per-crate coverage thresholds for CI
#
# Usage: ./scripts/validate-crate-coverage.sh
#
# Requires: cargo-tarpaulin
# Install: cargo install cargo-tarpaulin

set -e

echo "🔍 Validating per-crate coverage thresholds..."
echo ""

# Coverage thresholds by crate (following architecture standards)
declare -A THRESHOLDS
THRESHOLDS["ggen-domain"]=95
THRESHOLDS["ggen-core"]=90
THRESHOLDS["ggen-cli"]=85
THRESHOLDS["ggen-marketplace"]=85
THRESHOLDS["ggen-utils"]=80
THRESHOLDS["ggen-ai"]=85
THRESHOLDS["ggen-node"]=75

FAILED_CRATES=()
PASSED_CRATES=()

for CRATE in "${!THRESHOLDS[@]}"; do
    THRESHOLD=${THRESHOLDS[$CRATE]}

    echo "📦 Checking $CRATE (threshold: $THRESHOLD%)..."

    # Run tarpaulin for specific crate
    COVERAGE_OUTPUT=$(cargo tarpaulin \
        --package "$CRATE" \
        --skip-clean \
        --timeout 120 \
        --out Stdout 2>/dev/null || echo "ERROR")

    if [ "$COVERAGE_OUTPUT" = "ERROR" ]; then
        echo "   ⚠️  Failed to run coverage for $CRATE"
        FAILED_CRATES+=("$CRATE (failed to run)")
        continue
    fi

    # Extract coverage percentage
    COVERAGE=$(echo "$COVERAGE_OUTPUT" | \
        grep -oP '\d+\.\d+%' | \
        head -1 | \
        sed 's/%//')

    if [ -z "$COVERAGE" ]; then
        echo "   ⚠️  Failed to parse coverage for $CRATE"
        FAILED_CRATES+=("$CRATE (failed to parse)")
        continue
    fi

    # Compare coverage to threshold
    if (( $(echo "$COVERAGE >= $THRESHOLD" | bc -l) )); then
        echo "   ✅ Coverage: $COVERAGE% (threshold: $THRESHOLD%)"
        PASSED_CRATES+=("$CRATE: $COVERAGE%")
    else
        echo "   ❌ Coverage: $COVERAGE% (threshold: $THRESHOLD%)"
        FAILED_CRATES+=("$CRATE: $COVERAGE% < $THRESHOLD%")
    fi

    echo ""
done

echo "================================"
echo "Coverage Validation Summary"
echo "================================"
echo ""

if [ ${#PASSED_CRATES[@]} -gt 0 ]; then
    echo "✅ Passed Crates:"
    for CRATE in "${PASSED_CRATES[@]}"; do
        echo "   - $CRATE"
    done
    echo ""
fi

if [ ${#FAILED_CRATES[@]} -gt 0 ]; then
    echo "❌ Failed Crates:"
    for CRATE in "${FAILED_CRATES[@]}"; do
        echo "   - $CRATE"
    done
    echo ""
    echo "Action Required:"
    echo "1. Generate missing tests:"
    echo "   ./scripts/generate-test-skeleton.sh <module> <crate>"
    echo ""
    echo "2. Check existing tests:"
    echo "   cargo test --package <crate>"
    echo ""
    echo "3. Validate coverage:"
    echo "   cargo tarpaulin --package <crate> --out Html"
    echo ""
    exit 1
else
    echo "✅ All crates meet coverage thresholds"
    exit 0
fi
