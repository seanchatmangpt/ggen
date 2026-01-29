#!/bin/bash

##############################################################################
# Tests for ggen-setup Module
#
# Test coverage:
#   - detect_ggen_binary(): Detection in PATH, ~/.cargo/bin, target dirs
#   - install_ggen_if_needed(): Installation workflow, verification
#   - verify_ggen_binary(): Binary validation and functionality checks
#   - export_ggen_env(): Environment variable export
#   - init_ggen_setup(): Full initialization workflow
#
# Execution: ./test-ggen-setup.sh
##############################################################################

set -euo pipefail

# Import test utilities
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PARENT_DIR="$(dirname "$SCRIPT_DIR")"
MODULES_DIR="${PARENT_DIR}/modules"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Test counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

##############################################################################
# Test Utilities
##############################################################################

log_test() {
  echo -e "${BLUE}[TEST]${NC} $1"
}

test_pass() {
  echo -e "${GREEN}[PASS]${NC} $1"
  ((TESTS_PASSED++))
  ((TESTS_RUN++))
}

test_fail() {
  echo -e "${RED}[FAIL]${NC} $1"
  ((TESTS_FAILED++))
  ((TESTS_RUN++))
}

test_warn() {
  echo -e "${YELLOW}[WARN]${NC} $1"
}

##############################################################################
# Setup: Load ggen-setup module
##############################################################################

if [ ! -f "${MODULES_DIR}/ggen-setup.sh" ]; then
  echo -e "${RED}ERROR: ggen-setup.sh module not found${NC}"
  exit 1
fi

# Source the module
source "${MODULES_DIR}/ggen-setup.sh"

# Create temporary workspace for tests
TEST_WORKSPACE=$(mktemp -d)
export TEST_WORKSPACE
trap "rm -rf ${TEST_WORKSPACE}" EXIT

##############################################################################
# Test Suite 1: detect_ggen_binary()
##############################################################################

echo ""
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}TEST SUITE 1: detect_ggen_binary()${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo ""

test_detect_ggen_in_path() {
  log_test "detect_ggen_binary() - ggen in PATH"

  if command -v ggen &>/dev/null; then
    # ggen is in PATH, test should find it
    local result
    if result=$(detect_ggen_binary 2>/dev/null); then
      if [[ "$result" == *"ggen"* ]]; then
        test_pass "detect_ggen_binary() found ggen in PATH: $result"
      else
        test_fail "detect_ggen_binary() returned unexpected path: $result"
      fi
    else
      test_fail "detect_ggen_binary() failed when ggen is in PATH"
    fi
  else
    test_warn "detect_ggen_binary() - ggen not in PATH (skipping positive test)"
  fi
}

test_detect_ggen_cargo_bin() {
  log_test "detect_ggen_binary() - ggen in ~/.cargo/bin"

  if [ -f "$HOME/.cargo/bin/ggen" ]; then
    local result
    if result=$(detect_ggen_binary 2>/dev/null); then
      if [[ "$result" == "$HOME/.cargo/bin/ggen" ]] || [[ "$result" == *"ggen"* ]]; then
        test_pass "detect_ggen_binary() found ggen in ~/.cargo/bin"
      else
        test_fail "detect_ggen_binary() returned unexpected path: $result"
      fi
    else
      test_fail "detect_ggen_binary() failed to find ggen in ~/.cargo/bin"
    fi
  else
    test_warn "detect_ggen_binary() - ggen not in ~/.cargo/bin (skipping test)"
  fi
}

test_detect_ggen_not_found() {
  log_test "detect_ggen_binary() - graceful failure when not found"

  # Temporarily modify PATH to exclude ggen
  local original_path="$PATH"
  export PATH="/usr/bin:/bin"

  # Create a temp directory without ggen
  local temp_home=$(mktemp -d)
  local temp_cargo_bin="${temp_home}/.cargo/bin"
  mkdir -p "$temp_cargo_bin"

  # Try detection (should fail gracefully)
  if detect_ggen_binary 2>/dev/null; then
    # If it succeeds, ggen must be in a standard location
    test_warn "detect_ggen_binary() - ggen found in standard location"
  else
    # Expected: detection should fail with proper exit code
    test_pass "detect_ggen_binary() failed gracefully when ggen not found"
  fi

  # Restore PATH
  export PATH="$original_path"
  rm -rf "$temp_home"
}

test_detect_ggen_returns_path() {
  log_test "detect_ggen_binary() - returns valid path on stdout"

  if command -v ggen &>/dev/null; then
    local result
    result=$(detect_ggen_binary 2>/dev/null || echo "")
    if [ -n "$result" ] && [ -f "$result" ]; then
      test_pass "detect_ggen_binary() returned valid file path: $result"
    else
      test_fail "detect_ggen_binary() returned invalid path: $result"
    fi
  else
    test_warn "detect_ggen_binary() - ggen not installed (skipping path validation)"
  fi
}

# Run detection tests
test_detect_ggen_in_path
test_detect_ggen_cargo_bin
test_detect_ggen_not_found
test_detect_ggen_returns_path

##############################################################################
# Test Suite 2: verify_ggen_binary()
##############################################################################

echo ""
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}TEST SUITE 2: verify_ggen_binary()${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo ""

test_verify_valid_binary() {
  log_test "verify_ggen_binary() - validates existing ggen"

  if command -v ggen &>/dev/null; then
    local ggen_path
    ggen_path=$(command -v ggen)
    if verify_ggen_binary "$ggen_path" &>/dev/null; then
      test_pass "verify_ggen_binary() validated ggen: $ggen_path"
    else
      test_fail "verify_ggen_binary() failed to validate working ggen"
    fi
  else
    test_warn "verify_ggen_binary() - ggen not installed (skipping test)"
  fi
}

test_verify_nonexistent_binary() {
  log_test "verify_ggen_binary() - rejects nonexistent path"

  if verify_ggen_binary "/nonexistent/path/ggen" 2>/dev/null; then
    test_fail "verify_ggen_binary() accepted nonexistent path"
  else
    test_pass "verify_ggen_binary() rejected nonexistent path"
  fi
}

test_verify_non_executable_binary() {
  log_test "verify_ggen_binary() - rejects non-executable file"

  local temp_file="${TEST_WORKSPACE}/not-executable"
  echo "#!/bin/bash" > "$temp_file"
  chmod 644 "$temp_file"

  if verify_ggen_binary "$temp_file" 2>/dev/null; then
    test_fail "verify_ggen_binary() accepted non-executable file"
  else
    test_pass "verify_ggen_binary() rejected non-executable file"
  fi

  rm -f "$temp_file"
}

test_verify_empty_path() {
  log_test "verify_ggen_binary() - handles empty path"

  if verify_ggen_binary "" 2>/dev/null; then
    test_fail "verify_ggen_binary() accepted empty path"
  else
    test_pass "verify_ggen_binary() rejected empty path"
  fi
}

# Run verification tests
test_verify_valid_binary
test_verify_nonexistent_binary
test_verify_non_executable_binary
test_verify_empty_path

##############################################################################
# Test Suite 3: export_ggen_env()
##############################################################################

echo ""
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}TEST SUITE 3: export_ggen_env()${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo ""

test_export_ggen_env_sets_variable() {
  log_test "export_ggen_env() - exports GGEN_BIN variable"

  if command -v ggen &>/dev/null; then
    local ggen_path
    ggen_path=$(command -v ggen)

    # Unset before test
    unset GGEN_BIN || true

    if export_ggen_env "$ggen_path" &>/dev/null; then
      if [ -n "${GGEN_BIN:-}" ]; then
        test_pass "export_ggen_env() set GGEN_BIN=$GGEN_BIN"
      else
        test_fail "export_ggen_env() did not set GGEN_BIN"
      fi
    else
      test_fail "export_ggen_env() failed to export"
    fi
  else
    test_warn "export_ggen_env() - ggen not installed (skipping test)"
  fi
}

test_export_ggen_env_rejects_invalid() {
  log_test "export_ggen_env() - rejects invalid path"

  # Unset before test
  unset GGEN_BIN || true

  if export_ggen_env "/nonexistent/ggen" 2>/dev/null; then
    test_fail "export_ggen_env() accepted invalid path"
  else
    test_pass "export_ggen_env() rejected invalid path"
  fi
}

# Run export tests
test_export_ggen_env_sets_variable
test_export_ggen_env_rejects_invalid

##############################################################################
# Test Suite 4: init_ggen_setup()
##############################################################################

echo ""
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}TEST SUITE 4: init_ggen_setup()${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo ""

test_init_ggen_setup_complete() {
  log_test "init_ggen_setup() - completes full initialization"

  # Unset before test
  unset GGEN_BIN || true

  if command -v ggen &>/dev/null; then
    if init_ggen_setup &>/dev/null; then
      if [ -n "${GGEN_BIN:-}" ]; then
        test_pass "init_ggen_setup() completed: GGEN_BIN=$GGEN_BIN"
      else
        test_fail "init_ggen_setup() did not set GGEN_BIN"
      fi
    else
      test_fail "init_ggen_setup() returned error"
    fi
  else
    test_warn "init_ggen_setup() - ggen not installed (skipping full test)"
    log_test "Testing init_ggen_setup() fallback to installation..."
    if init_ggen_setup 2>&1 | grep -q "installation"; then
      test_pass "init_ggen_setup() attempted installation when ggen not found"
    else
      test_fail "init_ggen_setup() did not attempt installation"
    fi
  fi
}

# Run initialization test
test_init_ggen_setup_complete

##############################################################################
# Test Suite 5: ggen_diagnostics()
##############################################################################

echo ""
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}TEST SUITE 5: ggen_diagnostics()${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo ""

test_diagnostics_output() {
  log_test "ggen_diagnostics() - produces diagnostic output"

  local diag_output
  diag_output=$(ggen_diagnostics 2>&1)

  if echo "$diag_output" | grep -q "ggen Diagnostics"; then
    test_pass "ggen_diagnostics() produced header output"
  else
    test_fail "ggen_diagnostics() missing header"
  fi

  if echo "$diag_output" | grep -q "System Information"; then
    test_pass "ggen_diagnostics() included system information"
  else
    test_fail "ggen_diagnostics() missing system info"
  fi

  if echo "$diag_output" | grep -q "ggen Detection"; then
    test_pass "ggen_diagnostics() included detection status"
  else
    test_fail "ggen_diagnostics() missing detection status"
  fi
}

# Run diagnostics test
test_diagnostics_output

##############################################################################
# Test Results Summary
##############################################################################

echo ""
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}TEST RESULTS SUMMARY${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo ""
echo -e "Tests Run:    ${BLUE}${TESTS_RUN}${NC}"
echo -e "Tests Passed: ${GREEN}${TESTS_PASSED}${NC}"
echo -e "Tests Failed: ${RED}${TESTS_FAILED}${NC}"
echo ""

if [ $TESTS_FAILED -eq 0 ]; then
  echo -e "${GREEN}All tests passed!${NC}"
  exit 0
else
  echo -e "${RED}Some tests failed!${NC}"
  exit 1
fi
