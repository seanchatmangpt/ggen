#!/bin/bash
# Documentation Validation Script
# Prevents documentation failures identified by FMEA
#
# FMEA-based design addresses high-risk failure modes:
# - RPN 168: Direct cargo commands (should use cargo make)
# - RPN 120: Incorrect paths (cli/src/ vs crates/ggen-cli/src/)
# - RPN 45: Outdated macro names (chicago_test! vs test!)

set -e

ERRORS=0
WARNINGS=0

# Track files with issues for summary
FILES_WITH_CARGO_COMMANDS=()
FILES_WITH_INCORRECT_PATHS=()
FILES_WITH_OUTDATED_MACROS=()

echo "=== Documentation Validation ==="
echo ""

# Check 1: Direct cargo commands (RPN 168)
echo "Checking for direct cargo commands (should use cargo make)..."
CARGO_TEST_FILES=$(grep -rl "cargo test --" docs/ --include="*.md" 2>/dev/null | grep -v "cargo make" | head -5 || true)
if [ -n "$CARGO_TEST_FILES" ]; then
    echo "⚠️  WARNING: Found direct cargo test commands (should use cargo make test)"
    WARNINGS=$((WARNINGS + 1))
    while IFS= read -r file; do
        FILES_WITH_CARGO_COMMANDS+=("$file")
    done <<< "$CARGO_TEST_FILES"
fi

CARGO_CLIPPY_FILES=$(grep -rl "cargo clippy --" docs/ --include="*.md" 2>/dev/null | grep -v "cargo make" | head -5 || true)
if [ -n "$CARGO_CLIPPY_FILES" ]; then
    echo "⚠️  WARNING: Found direct cargo clippy commands (should use cargo make lint)"
    WARNINGS=$((WARNINGS + 1))
    while IFS= read -r file; do
        FILES_WITH_CARGO_COMMANDS+=("$file")
    done <<< "$CARGO_CLIPPY_FILES"
fi

CARGO_FMT_FILES=$(grep -rl "cargo fmt --" docs/ --include="*.md" 2>/dev/null | grep -v "cargo make" | head -5 || true)
if [ -n "$CARGO_FMT_FILES" ]; then
    echo "⚠️  WARNING: Found direct cargo fmt commands (should use cargo make fmt)"
    WARNINGS=$((WARNINGS + 1))
    while IFS= read -r file; do
        FILES_WITH_CARGO_COMMANDS+=("$file")
    done <<< "$CARGO_FMT_FILES"
fi

# Check 2: Incorrect paths (RPN 120)
echo ""
echo "Checking for incorrect paths..."
PATH_FILES=$(grep -rl "cli/src/" docs/ --include="*.md" 2>/dev/null | grep -v "crates/ggen-cli/src/" | head -5 || true)
if [ -n "$PATH_FILES" ]; then
    echo "⚠️  WARNING: Found 'cli/src/' references (should be 'crates/ggen-cli/src/')"
    WARNINGS=$((WARNINGS + 1))
    while IFS= read -r file; do
        FILES_WITH_INCORRECT_PATHS+=("$file")
    done <<< "$PATH_FILES"
fi

# Check 3: Outdated macro names (RPN 45)
echo ""
echo "Checking for outdated macro names..."
MACRO_FILES=$(grep -rl "chicago_test!" docs/ --include="*.md" 2>/dev/null | head -5 || true)
if [ -n "$MACRO_FILES" ]; then
    echo "⚠️  WARNING: Found 'chicago_test!' (should be 'test!')"
    WARNINGS=$((WARNINGS + 1))
    while IFS= read -r file; do
        FILES_WITH_OUTDATED_MACROS+=("$file")
    done <<< "$MACRO_FILES"
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

# Kaizen improvement: Add file summary for actionable feedback
if [ $WARNINGS -gt 0 ] || [ $ERRORS -gt 0 ]; then
    echo ""
    echo "=== Files Needing Attention ==="
    
    if [ ${#FILES_WITH_CARGO_COMMANDS[@]} -gt 0 ]; then
        echo ""
        echo "Files with direct cargo commands (use 'cargo make' instead):"
        printf '  - %s\n' "${FILES_WITH_CARGO_COMMANDS[@]}" | sort -u
    fi
    
    if [ ${#FILES_WITH_INCORRECT_PATHS[@]} -gt 0 ]; then
        echo ""
        echo "Files with incorrect paths (use 'crates/ggen-cli/src/' instead of 'cli/src/'):"
        printf '  - %s\n' "${FILES_WITH_INCORRECT_PATHS[@]}" | sort -u
    fi
    
    if [ ${#FILES_WITH_OUTDATED_MACROS[@]} -gt 0 ]; then
        echo ""
        echo "Files with outdated macros (use 'test!' instead of 'chicago_test!'):"
        printf '  - %s\n' "${FILES_WITH_OUTDATED_MACROS[@]}" | sort -u
    fi
    
    echo ""
    echo "⚠️  Documentation validation found issues"
    # Note: Exit code 0 is non-blocking - warnings don't block commits
    # This allows pre-commit to pass while still reporting issues
    exit 0
else
    echo "✅ Documentation validation passed"
    exit 0
fi


