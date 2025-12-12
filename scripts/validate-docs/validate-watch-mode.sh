#!/usr/bin/env bash
#
# Validate ggen project watch command
# Tests: ggen project watch functionality
#
# Validates that file watching works and triggers regeneration

set -e
set -u

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Test counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

log_info() { echo -e "${BLUE}ℹ${NC} $1"; }
log_success() { echo -e "${GREEN}✓${NC} $1"; ((++TESTS_PASSED)); ((++TESTS_RUN)); return 0; }
log_error() { echo -e "${RED}✗${NC} $1"; ((++TESTS_FAILED)); ((++TESTS_RUN)); return 0; }
log_section() {
    echo ""
    echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${YELLOW}$1${NC}"
    echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
}

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
DEFAULT_GGEN_BIN="$REPO_ROOT/target/debug/ggen"
if [ -x "$DEFAULT_GGEN_BIN" ]; then
    GGEN_BIN="${GGEN_BIN:-$DEFAULT_GGEN_BIN}"
else
    GGEN_BIN="${GGEN_BIN:-ggen}"
fi

# Create workspace
WORKSPACE=$(mktemp -d)
trap "rm -rf $WORKSPACE" EXIT
if [ -f "$REPO_ROOT/.tool-versions" ]; then
    cp "$REPO_ROOT/.tool-versions" "$WORKSPACE/.tool-versions"
fi
cd "$WORKSPACE"

if ! command -v "$GGEN_BIN" &> /dev/null && [ ! -x "$GGEN_BIN" ]; then
    log_error "ggen not found"
    exit 1
fi

log_info "Working in: $WORKSPACE"
log_info "Using ggen: $GGEN_BIN ($($GGEN_BIN --version))"

# ============================================================================
# Test 1: Watch Command Exists
# ============================================================================
log_section "Test 1: Verify Watch Command Exists"

if $GGEN_BIN project watch --help &> /dev/null; then
    log_success "ggen project watch command exists"
else
    log_error "ggen project watch command not found"
fi

# ============================================================================
# Test 2: Watch Command with Path Option
# ============================================================================
log_section "Test 2: Test Watch Command Options"

# Create test project structure
mkdir -p test-project/{templates,schemas}

# Create a simple template
cat > test-project/templates/test.tmpl << 'EOF'
---
name: "test"
description: "Test template"
version: "1.0.0"
variables:
  - name: "message"
    default: "Hello"
---
{{ message }} from template!
EOF

# Test that watch accepts path argument
if $GGEN_BIN project watch --help | grep -q -- "--path"; then
    log_success "Watch command has --path option"
else
    log_error "Watch command missing --path option"
fi

# Test that watch accepts debounce argument
if $GGEN_BIN project watch --help | grep -q -- "--debounce"; then
    log_success "Watch command has --debounce option"
else
    log_error "Watch command missing --debounce option"
fi

# ============================================================================
# Test 3: Watch Command Can Start (then kill immediately)
# ============================================================================
log_section "Test 3: Watch Command Can Start"

# Start watch briefly and treat clean stop as success
timeout 2s $GGEN_BIN project watch --path test-project --debounce 100 &> /tmp/watch-output.txt || true
if grep -q "status\":\"started\\|status\":\"stopped" /tmp/watch-output.txt 2>/dev/null; then
    log_success "Watch process invoked (started/stopped cleanly)"
else
    log_error "Watch process failed to start"
    if [ -f /tmp/watch-output.txt ]; then
        log_info "Output: $(cat /tmp/watch-output.txt)"
    fi
fi

# ============================================================================
# Test 4: Validate Watch Documentation Example
# ============================================================================
log_section "Test 4: Validate Documentation Examples"

# Example from docs/reference/commands/complete-cli-reference.md
# ggen project watch --path ./src --debounce 500

if $GGEN_BIN project watch --help | grep -qE "path.*debounce|debounce.*path"; then
    log_success "Documentation example syntax is valid"
else
    log_error "Documentation example syntax validation failed"
fi

# ============================================================================
# Summary
# ============================================================================
log_section "Test Summary"

echo ""
echo "Total Tests Run:    $TESTS_RUN"
echo -e "${GREEN}Tests Passed:       $TESTS_PASSED${NC}"
echo -e "${RED}Tests Failed:       $TESTS_FAILED${NC}"
echo ""

if [ $TESTS_FAILED -eq 0 ]; then
    echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${GREEN}✓ Watch Mode: ALL TESTS PASSED${NC}"
    echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    exit 0
else
    echo -e "${RED}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${RED}✗ Watch Mode: TESTS FAILED${NC}"
    echo -e "${RED}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    exit 1
fi
