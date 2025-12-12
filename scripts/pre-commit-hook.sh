#!/usr/bin/env bash
# Pre-Commit Hook for ggen
# Validates all critical checks before allowing commit
# Install: ln -s ../../scripts/pre-commit-hook.sh .git/hooks/pre-commit

set -e  # Exit on first error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "🔍 Running pre-commit validation..."
echo ""

# Function to run check with timeout and status reporting
run_check() {
    local name="$1"
    local cmd="$2"
    local timeout_duration="${3:-30s}"

    echo -n "  ${name}... "

    if timeout "$timeout_duration" bash -c "$cmd" > /tmp/check_output.log 2>&1; then
        echo -e "${GREEN}✓${NC}"
        return 0
    else
        echo -e "${RED}✗${NC}"
        echo ""
        echo -e "${RED}Error output:${NC}"
        cat /tmp/check_output.log
        echo ""
        return 1
    fi
}

# Verify timeout command exists
if ! command -v timeout &> /dev/null; then
    echo -e "${RED}Error: 'timeout' command not found${NC}"
    echo "Install coreutils: brew install coreutils (macOS) or apt-get install coreutils (Linux)"
    exit 1
fi

# Track failures
FAILED=0

# 1. Compilation check (CRITICAL ANDON SIGNAL)
if ! run_check "Compilation check (cargo make check)" "cargo make check" "15s"; then
    FAILED=1
fi

# 2. Formatting check
if ! run_check "Format check (cargo make fmt)" "cargo make fmt -- --check" "10s"; then
    echo -e "${YELLOW}  Run 'cargo make fmt' to fix formatting${NC}"
    FAILED=1
fi

# 3. Linting check (HIGH ANDON SIGNAL)
if ! run_check "Linting (cargo make lint)" "cargo make lint" "15s"; then
    FAILED=1
fi

# 4. Unit tests (CRITICAL ANDON SIGNAL) - 160s timeout for full workspace rebuild
# Note: After cargo clean, full rebuild with oxrocksdb-sys can take ~150s
if ! run_check "Unit tests (cargo make test-unit)" "cargo make test-unit" "160s"; then
    FAILED=1
fi

# 5. CLI Command Verification (Andon Signal Validation Framework - Layer 3: Runtime)
# Verifies all CLI commands work end-to-end (prevents "fake greens")
if ! run_check "CLI verification (cargo make verify-cli)" "cargo make verify-cli" "30s"; then
    echo -e "${YELLOW}  CLI verification failed - this may indicate 'fake greens'${NC}"
    echo -e "${YELLOW}  Run 'cargo make verify-cli' manually to see details${NC}"
    FAILED=1
fi

# 6. Security audit
if ! run_check "Security audit (cargo audit)" "cargo audit" "10s"; then
    echo -e "${YELLOW}  Security vulnerabilities found. Update dependencies.${NC}"
    FAILED=1
fi

# 6. Check for active debug prints (exclude CLI tools, tests, and code generation files)
# Find active debug prints EXCEPT in:
# - CLI binaries (src/bin/)
# - CLI command handlers (crates/ggen-cli/src/cmds/)
# - Example code (*example*.rs, *e2e*.rs, *_example.rs)
# - Test files (*_test.rs, *tests.rs, integration_tests.rs)
# - Code generation/template files (template.rs, *generator*.rs, dx.rs, *_logic.rs)
# - Test code within source (#[cfg(test)] sections)
# - Error formatting utilities
DEBUG_PRINTS=$(grep -rn "^\s*println!\|^\s*dbg!\|^\s*print!" --include="*.rs" crates/*/src/ \
    --exclude-dir=cmds \
    --exclude-dir=bin \
    --exclude="template.rs" \
    --exclude="dx.rs" \
    --exclude="pipeline.rs" \
    | grep -v "example\|e2e_tests\|test_helpers\|enhanced_error\|_test\.rs\|_tests\.rs\|integration_test\|generator\|_logic\.rs" || true)

if [ -n "$DEBUG_PRINTS" ]; then
    echo -e "  ${RED}✗${NC} Active debug prints found in source"
    echo -e "${YELLOW}  Remove println!/dbg!/print! from library logic code${NC}"
    echo "$DEBUG_PRINTS"
    FAILED=1
else
    echo -e "  ${GREEN}✓${NC} No active debug prints in library logic code"
fi

echo ""

# Summary
if [ $FAILED -eq 1 ]; then
    echo -e "${RED}❌ Pre-commit validation FAILED${NC}"
    echo -e "${RED}🛑 ANDON SIGNAL: Stop the line and fix issues before committing${NC}"
    echo ""
    echo "Fix the issues above and try again."
    exit 1
else
    echo -e "${GREEN}✅ All checks passed! Commit can proceed.${NC}"
    exit 0
fi
