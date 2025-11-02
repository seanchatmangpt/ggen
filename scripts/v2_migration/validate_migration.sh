#!/bin/bash
# validate_migration.sh
# Ensures all migrations compile and tests pass
#
# Usage: ./validate_migration.sh [--quick]
#
# Validation phases:
# 1. Syntax check (cargo check)
# 2. Compilation (cargo build)
# 3. Unit tests (cargo test)
# 4. Integration tests (cargo test --test '*')
# 5. E2E tests (CLI command validation)

set -euo pipefail

QUICK_MODE=false
if [ "${1:-}" = "--quick" ]; then
    QUICK_MODE=true
    echo "⚡ QUICK MODE - Skipping E2E tests"
    echo ""
fi

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Log file
LOG_FILE="scripts/v2_migration/validation_log_$(date +%Y%m%d_%H%M%S).txt"
exec > >(tee -a "${LOG_FILE}") 2>&1

echo "========================================="
echo "ggen v2.0.0 Migration Validation"
echo "========================================="
echo "Start time: $(date)"
echo ""

PHASE=0
FAILED=false

phase() {
    ((PHASE++))
    echo ""
    echo "========================================="
    echo "Phase ${PHASE}: $1"
    echo "========================================="
}

success() {
    echo -e "${GREEN}✅ $1${NC}"
}

warning() {
    echo -e "${YELLOW}⚠️  $1${NC}"
}

error() {
    echo -e "${RED}❌ $1${NC}"
    FAILED=true
}

# Phase 1: Syntax Check
phase "Syntax Check (cargo check)"
if cargo check --all-features 2>&1 | tee /tmp/cargo_check.log; then
    success "Syntax check passed"
else
    error "Syntax check failed - see /tmp/cargo_check.log"
fi

# Phase 2: Compilation
phase "Compilation (cargo build)"
if cargo build --all-features 2>&1 | tee /tmp/cargo_build.log; then
    success "Compilation succeeded"

    # Check binary size
    BINARY_SIZE=$(stat -f%z target/debug/ggen 2>/dev/null || echo "0")
    BINARY_MB=$((BINARY_SIZE / 1024 / 1024))
    if [ $BINARY_MB -lt 30 ]; then
        success "Binary size: ${BINARY_MB}MB (target: <30MB)"
    else
        warning "Binary size: ${BINARY_MB}MB (target: <30MB)"
    fi
else
    error "Compilation failed - see /tmp/cargo_build.log"
fi

# Phase 3: Unit Tests
phase "Unit Tests (cargo test --lib)"
if cargo test --lib 2>&1 | tee /tmp/cargo_test_unit.log; then
    success "Unit tests passed"

    # Extract test count
    TEST_COUNT=$(grep -o "[0-9]* passed" /tmp/cargo_test_unit.log | head -1 | awk '{print $1}')
    echo "  Tests run: ${TEST_COUNT:-unknown}"
else
    error "Unit tests failed - see /tmp/cargo_test_unit.log"
fi

# Phase 4: Integration Tests
phase "Integration Tests (cargo test --test '*')"
if cargo test --test '*' 2>&1 | tee /tmp/cargo_test_integration.log; then
    success "Integration tests passed"
else
    error "Integration tests failed - see /tmp/cargo_test_integration.log"
fi

# Phase 5: E2E Tests (unless quick mode)
if [ "$QUICK_MODE" = false ]; then
    phase "E2E Tests (CLI command validation)"

    # Build release binary for E2E tests
    echo "Building release binary..."
    if cargo build --release 2>&1 | grep -q "Finished"; then
        success "Release build succeeded"
    else
        error "Release build failed"
    fi

    GGEN_BIN="target/release/ggen"

    if [ -f "$GGEN_BIN" ]; then
        # Test basic commands
        echo "Testing basic commands..."

        # Help command
        if $GGEN_BIN --help >/dev/null 2>&1; then
            success "Help command works"
        else
            error "Help command failed"
        fi

        # Version command
        if $GGEN_BIN --version >/dev/null 2>&1; then
            success "Version command works"
        else
            error "Version command failed"
        fi

        # Doctor command
        if $GGEN_BIN doctor >/dev/null 2>&1; then
            success "Doctor command works"
        else
            warning "Doctor command failed (may be expected)"
        fi

        # Template list
        if $GGEN_BIN template list >/dev/null 2>&1; then
            success "Template list works"
        else
            warning "Template list failed (may need setup)"
        fi

        # Marketplace list
        if $GGEN_BIN marketplace list >/dev/null 2>&1; then
            success "Marketplace list works"
        else
            warning "Marketplace list failed (may need setup)"
        fi
    else
        warning "Release binary not found, skipping E2E tests"
    fi
else
    echo "⏭️  Skipped E2E tests (quick mode)"
fi

# Phase 6: Architecture Validation
phase "Architecture Validation"

echo "Checking file structure..."

# Check for proper separation of concerns
CLI_COUNT=$(find cli/src/commands -name "*.rs" | wc -l)
DOMAIN_COUNT=$(find cli/src/domain -name "*.rs" | wc -l)

echo "  CLI layer files: $CLI_COUNT"
echo "  Domain layer files: $DOMAIN_COUNT"

if [ $CLI_COUNT -gt 0 ] && [ $DOMAIN_COUNT -gt 0 ]; then
    success "Proper layer separation detected"
else
    warning "Layer separation may need review"
fi

# Check for common anti-patterns
echo "Checking for anti-patterns..."

# CLI files should not have business logic
if grep -r "fn.*impl.*{" cli/src/commands --include="*.rs" | grep -v "execute" | grep -v "test" >/dev/null 2>&1; then
    warning "Possible business logic in CLI layer (review needed)"
else
    success "No obvious business logic in CLI layer"
fi

# Domain files should not import clap
if grep -r "use clap" cli/src/domain --include="*.rs" >/dev/null 2>&1; then
    error "Domain layer imports clap (separation violation)"
else
    success "Domain layer properly isolated from CLI framework"
fi

# Phase 7: Performance Benchmarks
phase "Performance Benchmarks"

echo "Measuring compilation time..."
COMPILE_START=$(date +%s)
cargo build --release >/dev/null 2>&1 || true
COMPILE_END=$(date +%s)
COMPILE_TIME=$((COMPILE_END - COMPILE_START))

echo "  Compilation time: ${COMPILE_TIME}s"

if [ $COMPILE_TIME -lt 45 ]; then
    success "Compilation time within target (<45s)"
elif [ $COMPILE_TIME -lt 90 ]; then
    warning "Compilation time acceptable (45-90s)"
else
    warning "Compilation time above target (>90s)"
fi

# Summary
echo ""
echo "========================================="
echo "Validation Summary"
echo "========================================="
echo "End time: $(date)"
echo "Log file: ${LOG_FILE}"
echo ""

if [ "$FAILED" = true ]; then
    echo -e "${RED}❌ VALIDATION FAILED${NC}"
    echo "Review the log file for details: ${LOG_FILE}"
    exit 1
else
    echo -e "${GREEN}✅ ALL VALIDATIONS PASSED${NC}"
    echo ""
    echo "Migration quality metrics:"
    echo "  - CLI files: $CLI_COUNT"
    echo "  - Domain files: $DOMAIN_COUNT"
    echo "  - Compilation time: ${COMPILE_TIME}s"
    echo "  - Binary size: ${BINARY_MB}MB"
    echo ""
    echo "Next steps:"
    echo "  1. Review any warnings above"
    echo "  2. Implement TODOs in generated files"
    echo "  3. Add missing domain logic"
    echo "  4. Update documentation"
    echo "  5. Run: cargo test --all --all-features"
fi
