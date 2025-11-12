#!/bin/bash
# Documentation Validation Script
# Prevents documentation failures identified by FMEA

set -e

ERRORS=0
WARNINGS=0

echo "=== Documentation Validation ==="
echo ""

# Check 1: Direct cargo commands (RPN 168)
echo "Checking for direct cargo commands (should use cargo make)..."
if grep -r "cargo test --" docs/ --include="*.md" | grep -v "cargo make" | grep -v "cargo test --test" | head -5; then
    echo "⚠️  WARNING: Found direct cargo test commands (should use cargo make test)"
    WARNINGS=$((WARNINGS + 1))
fi

if grep -r "cargo clippy --" docs/ --include="*.md" | grep -v "cargo make" | head -5; then
    echo "⚠️  WARNING: Found direct cargo clippy commands (should use cargo make lint)"
    WARNINGS=$((WARNINGS + 1))
fi

if grep -r "cargo fmt --" docs/ --include="*.md" | grep -v "cargo make" | head -5; then
    echo "⚠️  WARNING: Found direct cargo fmt commands (should use cargo make fmt)"
    WARNINGS=$((WARNINGS + 1))
fi

# Check 2: Incorrect paths (RPN 120)
echo ""
echo "Checking for incorrect paths..."
if grep -r "cli/src/" docs/ --include="*.md" | grep -v "crates/ggen-cli/src/" | head -5; then
    echo "⚠️  WARNING: Found 'cli/src/' references (should be 'crates/ggen-cli/src/')"
    WARNINGS=$((WARNINGS + 1))
fi

# Check 3: Outdated macro names (RPN 45)
echo ""
echo "Checking for outdated macro names..."
if grep -r "chicago_test!" docs/ --include="*.md" | head -5; then
    echo "⚠️  WARNING: Found 'chicago_test!' (should be 'test!')"
    WARNINGS=$((WARNINGS + 1))
fi

# Check 4: Non-existent file references (RPN 32)
echo ""
echo "Checking for non-existent file references..."
# This would require more complex logic - placeholder for now
echo "✓ File reference check (manual review recommended)"

# Check 5: Wrong import paths (RPN 24)
echo ""
echo "Checking for wrong import paths..."
if grep -r "use chicago_tdd_tools::" docs/ --include="*.md" | grep -v "prelude" | head -5; then
    echo "ℹ️  INFO: Found chicago_tdd_tools imports (verify paths are correct)"
fi

echo ""
echo "=== Validation Summary ==="
echo "Warnings: $WARNINGS"
echo "Errors: $ERRORS"
echo ""

if [ $WARNINGS -gt 0 ] || [ $ERRORS -gt 0 ]; then
    echo "⚠️  Documentation validation found issues"
    exit 1
else
    echo "✅ Documentation validation passed"
    exit 0
fi

