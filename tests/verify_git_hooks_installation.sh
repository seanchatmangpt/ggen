#!/usr/bin/env bash
# Verification Test: Git Hooks Auto-Installation
# Tests that ggen init correctly installs git hooks across platforms
#
# Test Objectives:
# 1. Verify pre-commit hook is installed by ggen init
# 2. Verify pre-push hook is installed
# 3. Test hooks execute correctly
# 4. Verify --skip-hooks flag works
# 5. Test cross-platform compatibility (Unix paths)

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Test counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# Helper functions
log_test() {
    echo -e "${BLUE}[TEST]${NC} $1"
    TESTS_RUN=$((TESTS_RUN + 1))
}

log_pass() {
    echo -e "${GREEN}[PASS]${NC} $1"
    TESTS_PASSED=$((TESTS_PASSED + 1))
}

log_fail() {
    echo -e "${RED}[FAIL]${NC} $1"
    TESTS_FAILED=$((TESTS_FAILED + 1))
}

log_info() {
    echo -e "${YELLOW}[INFO]${NC} $1"
}

# Cleanup function
cleanup() {
    if [[ -n "${TEST_DIR}" && -d "${TEST_DIR}" ]]; then
        rm -rf "${TEST_DIR}"
    fi
}

trap cleanup EXIT

echo "======================================================================"
echo "Git Hooks Auto-Installation Verification Test"
echo "======================================================================"
echo ""

# Build the project first
log_info "Building ggen CLI..."
cd "$PROJECT_ROOT"
cargo build --release --bin ggen 2>&1 | grep -E "(Compiling|Finished)" || true
GGEN_BIN="$PROJECT_ROOT/target/release/ggen"

if [[ ! -f "$GGEN_BIN" ]]; then
    echo -e "${RED}ERROR: ggen binary not found at $GGEN_BIN${NC}"
    exit 1
fi

log_info "Using ggen binary: $GGEN_BIN"
echo ""

# ============================================================================
# TEST 1: Fresh git repo - hooks should be installed
# ============================================================================
log_test "Test 1: Fresh git repo - hooks should be installed"

TEST_DIR=$(mktemp -d)
cd "$TEST_DIR"
git init --quiet
log_info "Created fresh git repo at: $TEST_DIR"

# Run ggen init
"$GGEN_BIN" init --skip_hooks false 2>&1 > /dev/null || true

# Verify hooks exist
if [[ -f ".git/hooks/pre-commit" && -f ".git/hooks/pre-push" ]]; then
    log_pass "Both hooks installed in fresh git repo"
else
    log_fail "Hooks not installed in fresh git repo"
    ls -la .git/hooks/ || true
fi

# Verify hooks are executable (Unix only)
if [[ "$(uname)" != "MINGW"* && "$(uname)" != "MSYS"* ]]; then
    if [[ -x ".git/hooks/pre-commit" ]]; then
        log_pass "pre-commit hook is executable"
    else
        log_fail "pre-commit hook is not executable"
    fi

    if [[ -x ".git/hooks/pre-push" ]]; then
        log_pass "pre-push hook is executable"
    else
        log_fail "pre-push hook is not executable"
    fi
fi

# Verify hook content
if grep -q "cargo check" .git/hooks/pre-commit; then
    log_pass "pre-commit hook contains cargo check"
else
    log_fail "pre-commit hook missing cargo check"
fi

if grep -q "cargo make pre-commit" .git/hooks/pre-push; then
    log_pass "pre-push hook contains cargo make pre-commit"
else
    log_fail "pre-push hook missing cargo make pre-commit"
fi

cleanup
echo ""

# ============================================================================
# TEST 2: --skip-hooks flag - hooks should NOT be installed
# ============================================================================
log_test "Test 2: --skip-hooks flag prevents installation"

TEST_DIR=$(mktemp -d)
cd "$TEST_DIR"
git init --quiet
log_info "Created fresh git repo at: $TEST_DIR"

# Run ggen init with --skip_hooks
"$GGEN_BIN" init --skip_hooks true 2>&1 > /dev/null || true

# Verify hooks do NOT exist
if [[ ! -f ".git/hooks/pre-commit" && ! -f ".git/hooks/pre-push" ]]; then
    log_pass "Hooks correctly skipped with --skip-hooks flag"
else
    log_fail "Hooks were installed despite --skip-hooks flag"
    ls -la .git/hooks/ || true
fi

cleanup
echo ""

# ============================================================================
# TEST 3: Non-git directory - hooks should be skipped gracefully
# ============================================================================
log_test "Test 3: Non-git directory - graceful handling"

TEST_DIR=$(mktemp -d)
cd "$TEST_DIR"
# Do NOT initialize git

# Run ggen init
OUTPUT=$("$GGEN_BIN" init 2>&1 || true)

# Should succeed even without git
if echo "$OUTPUT" | grep -q "success"; then
    log_pass "Init succeeds in non-git directory"
else
    log_fail "Init failed in non-git directory"
    echo "$OUTPUT"
fi

# Hooks directory should not exist or be empty
if [[ ! -d ".git/hooks" ]]; then
    log_pass "No .git/hooks directory created in non-git repo"
else
    log_fail "Unexpected .git/hooks directory in non-git repo"
fi

cleanup
echo ""

# ============================================================================
# TEST 4: Existing hooks - should NOT be overwritten
# ============================================================================
log_test "Test 4: Existing hooks are preserved"

TEST_DIR=$(mktemp -d)
cd "$TEST_DIR"
git init --quiet
mkdir -p .git/hooks

# Create custom hook
echo "#!/bin/bash" > .git/hooks/pre-commit
echo "# CUSTOM HOOK - DO NOT OVERWRITE" >> .git/hooks/pre-commit
echo "echo 'Custom pre-commit'" >> .git/hooks/pre-commit
chmod +x .git/hooks/pre-commit

ORIGINAL_CONTENT=$(cat .git/hooks/pre-commit)

# Run ggen init
"$GGEN_BIN" init 2>&1 > /dev/null || true

# Verify hook was NOT overwritten
CURRENT_CONTENT=$(cat .git/hooks/pre-commit)
if [[ "$ORIGINAL_CONTENT" == "$CURRENT_CONTENT" ]]; then
    log_pass "Existing pre-commit hook preserved"
else
    log_fail "Existing pre-commit hook was overwritten"
    echo "Expected: $ORIGINAL_CONTENT"
    echo "Got: $CURRENT_CONTENT"
fi

cleanup
echo ""

# ============================================================================
# TEST 5: Hook execution - verify hooks can run
# ============================================================================
log_test "Test 5: Hooks can execute without errors"

TEST_DIR=$(mktemp -d)
cd "$TEST_DIR"
git init --quiet
git config user.email "test@example.com"
git config user.name "Test User"

# Run ggen init
"$GGEN_BIN" init 2>&1 > /dev/null || true

# Create a simple Rust project to test hooks
cat > Cargo.toml <<'EOF'
[package]
name = "test-project"
version = "0.1.0"
edition = "2021"
EOF

mkdir -p src
cat > src/main.rs <<'EOF'
fn main() {
    println!("Hello, world!");
}
EOF

# Stage files for commit
git add .

# Test pre-commit hook execution
# Note: This might fail on cargo check, but we're testing that the hook runs
log_info "Testing pre-commit hook execution..."
if bash .git/hooks/pre-commit 2>&1 | grep -q "Pre-commit"; then
    log_pass "pre-commit hook executed"
else
    log_fail "pre-commit hook did not execute"
fi

cleanup
echo ""

# ============================================================================
# TEST 6: Cross-platform path handling
# ============================================================================
log_test "Test 6: Cross-platform path handling"

TEST_DIR=$(mktemp -d)
cd "$TEST_DIR"
git init --quiet

# Run ggen init
"$GGEN_BIN" init 2>&1 > /dev/null || true

# Verify hooks use portable shebangs
if head -1 .git/hooks/pre-commit | grep -q "#!/usr/bin/env bash"; then
    log_pass "pre-commit uses portable shebang"
else
    log_fail "pre-commit has non-portable shebang"
fi

if head -1 .git/hooks/pre-push | grep -q "#!/usr/bin/env bash"; then
    log_pass "pre-push uses portable shebang"
else
    log_fail "pre-push has non-portable shebang"
fi

cleanup
echo ""

# ============================================================================
# TEST 7: Init in subdirectory with --path
# ============================================================================
log_test "Test 7: Init with --path creates hooks in target directory"

TEST_DIR=$(mktemp -d)
SUBDIR="$TEST_DIR/my-project"

cd "$TEST_DIR"
git init --quiet "$SUBDIR"

# Run ggen init with --path
"$GGEN_BIN" init --path "$SUBDIR" 2>&1 > /dev/null || true

# Verify hooks exist in subdirectory
if [[ -f "$SUBDIR/.git/hooks/pre-commit" && -f "$SUBDIR/.git/hooks/pre-push" ]]; then
    log_pass "Hooks installed in --path target directory"
else
    log_fail "Hooks not installed in --path target directory"
fi

cleanup
echo ""

# ============================================================================
# TEST 8: Reinit with --force preserves hooks
# ============================================================================
log_test "Test 8: Reinit with --force preserves existing hooks"

TEST_DIR=$(mktemp -d)
cd "$TEST_DIR"
git init --quiet

# First init
"$GGEN_BIN" init 2>&1 > /dev/null || true

# Verify hooks installed
HOOK_COUNT=$(ls -1 .git/hooks/ | grep -E "^(pre-commit|pre-push)$" | wc -l)
if [[ "$HOOK_COUNT" -eq 2 ]]; then
    log_pass "First init installed hooks"
else
    log_fail "First init did not install hooks"
fi

# Modify ggen.toml
echo "# Modified" >> ggen.toml

# Reinit with --force
"$GGEN_BIN" init --force true 2>&1 > /dev/null || true

# Hooks should still exist
HOOK_COUNT_AFTER=$(ls -1 .git/hooks/ | grep -E "^(pre-commit|pre-push)$" | wc -l)
if [[ "$HOOK_COUNT_AFTER" -eq 2 ]]; then
    log_pass "Hooks preserved after --force reinit"
else
    log_fail "Hooks lost after --force reinit"
fi

cleanup
echo ""

# ============================================================================
# Summary
# ============================================================================
echo "======================================================================"
echo "Test Summary"
echo "======================================================================"
echo "Tests run:    $TESTS_RUN"
echo -e "Tests passed: ${GREEN}$TESTS_PASSED${NC}"
echo -e "Tests failed: ${RED}$TESTS_FAILED${NC}"
echo ""

if [[ $TESTS_FAILED -eq 0 ]]; then
    echo -e "${GREEN}✓ All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}✗ Some tests failed${NC}"
    exit 1
fi
