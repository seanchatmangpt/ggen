#!/usr/bin/env bash
# Test Coverage Enforcement - Pre-commit Integration
# Enforces minimum test coverage for changed files

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

log() {
    echo -e "${BLUE}[COVERAGE]${NC} $1"
}

success() {
    echo -e "${GREEN}✓${NC} $1"
}

error() {
    echo -e "${RED}✗${NC} $1"
}

warn() {
    echo -e "${YELLOW}⚠${NC} $1"
}

# Get staged Rust source files (excluding tests)
STAGED_FILES=$(git diff --cached --name-only --diff-filter=d | grep '\.rs$' | grep -v "/tests/" | grep -v "build.rs" || true)

if [ -z "$STAGED_FILES" ]; then
    success "No source files staged, skipping coverage check"
    exit 0
fi

log "Checking test coverage for staged files..."

MISSING_COVERAGE=0

for file in $STAGED_FILES; do
    # Skip if not in a crate
    if [[ ! "$file" =~ ^crates/ ]]; then
        continue
    fi

    # Extract crate and module path
    CRATE=$(echo "$file" | sed 's|crates/||' | cut -d'/' -f1)
    MODULE=$(echo "$file" | sed 's|crates/[^/]*/src/||' | sed 's|\.rs$||' | sed 's|/mod$||')

    # Determine test file location
    if [[ "$file" == *"lib.rs" ]]; then
        TEST_FILE="crates/$CRATE/tests/unit/mod.rs"
    else
        TEST_FILE="crates/$CRATE/tests/unit/${MODULE}.rs"
    fi

    # Check for inline tests
    HAS_INLINE_TESTS=$(grep -q "#\[cfg(test)\]" "$file" 2>/dev/null && echo "yes" || echo "no")

    # Check for unit test file
    HAS_UNIT_TEST=$([ -f "$TEST_FILE" ] && echo "yes" || echo "no")

    # Check if file has public items
    HAS_PUBLIC=$(grep -qE "^pub (fn|struct|enum|trait|mod)" "$file" 2>/dev/null && echo "yes" || echo "no")

    if [ "$HAS_PUBLIC" = "yes" ] && [ "$HAS_INLINE_TESTS" = "no" ] && [ "$HAS_UNIT_TEST" = "no" ]; then
        error "Missing tests for $CRATE::$MODULE"
        echo "     Expected test file: $TEST_FILE"
        echo "     Or inline tests in: $file"
        ((MISSING_COVERAGE++))
    fi
done

if [ $MISSING_COVERAGE -gt 0 ]; then
    echo ""
    error "Test coverage enforcement failed: $MISSING_COVERAGE file(s) without tests"
    echo ""
    echo "Please add tests for changed modules:"
    echo "  Option 1: Add inline tests with #[cfg(test)] mod tests { ... }"
    echo "  Option 2: Add unit test file in crates/*/tests/unit/"
    echo ""
    exit 1
fi

success "All staged files have test coverage"
exit 0
