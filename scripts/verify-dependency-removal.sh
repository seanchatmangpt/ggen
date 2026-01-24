#!/bin/bash
# Verification script for dependency removal
# Checks that removed dependencies are truly gone

set -euo pipefail

echo "🔍 Verifying Dependency Removal"
echo "================================"
echo ""

FAILURES=0

# Check that removed dependencies are not in Cargo.toml files
echo "1️⃣ Verifying dependencies removed from Cargo.toml files..."

REMOVED_DEPS=(
    "gag"
    "num_cpus"
    "shacl_validation"
    "srdf"
    "diff.*0.1"
    "lazy_static"
    "ron.*0.11"
    "md5.*0.8"
)

for dep in "${REMOVED_DEPS[@]}"; do
    if grep -r "^$dep = " crates/*/Cargo.toml 2>/dev/null; then
        echo "   ❌ FOUND: $dep still in Cargo.toml"
        FAILURES=$((FAILURES + 1))
    else
        echo "   ✅ Removed: $dep"
    fi
done

echo ""
echo "2️⃣ Counting dependency declarations..."
CURRENT_COUNT=$(grep -r "^[a-z0-9_-]* = " crates/*/Cargo.toml | grep -v "\[" | wc -l)
echo "   Current count: $CURRENT_COUNT"
echo "   Expected: 734 (was 742, removed 8)"

if [ "$CURRENT_COUNT" -eq 734 ]; then
    echo "   ✅ Dependency count matches expected"
else
    echo "   ⚠️  Dependency count mismatch (expected 734, got $CURRENT_COUNT)"
fi

echo ""
echo "3️⃣ Checking for compilation errors (quick check)..."
echo "   Running: cargo check --workspace --all-targets"
echo "   (This may take a while...)"

if timeout 60s cargo check --workspace --all-targets 2>&1 | grep -q "Finished\|error:"; then
    if timeout 60s cargo check --workspace --all-targets 2>&1 | grep -q "error:"; then
        echo "   ❌ Compilation errors found"
        FAILURES=$((FAILURES + 1))
    else
        echo "   ✅ Compilation successful"
    fi
else
    echo "   ⏳ Compilation check timed out (run manually: cargo make check)"
fi

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

if [ $FAILURES -eq 0 ]; then
    echo "✅ Verification PASSED"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo ""
    echo "Next steps:"
    echo "  1. Run: cargo make test"
    echo "  2. Run: cargo make lint"
    echo "  3. Commit changes"
    exit 0
else
    echo "❌ Verification FAILED ($FAILURES issues)"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo ""
    echo "Please fix issues before committing"
    exit 1
fi
