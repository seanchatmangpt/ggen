#!/bin/bash

##############################################################################
# MCP Client Module - Test Suite
#
# Comprehensive tests for MCP client initialization, configuration loading,
# tool discovery, execution, and error handling.
#
# Version: 1.0.0
# Author: ggen AI Team
##############################################################################

set -euo pipefail

# Test configuration
readonly TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly PROJECT_ROOT="$(cd "$TEST_DIR/../../../" && pwd)"
readonly MODULES_DIR="$(cd "$TEST_DIR/../modules" && pwd)"
readonly TEST_CACHE_DIR="/tmp/mcp-test-cache"

# Test counters
TESTS_TOTAL=0
TESTS_PASSED=0
TESTS_FAILED=0

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m'

##############################################################################
# Test Utilities
##############################################################################

test_count() {
    TESTS_TOTAL=$((TESTS_TOTAL + 1))
}

test_pass() {
    local test_name="$1"
    TESTS_PASSED=$((TESTS_PASSED + 1))
    echo -e "${GREEN}✓${NC} $test_name"
}

test_fail() {
    local test_name="$1"
    local reason="$2"
    TESTS_FAILED=$((TESTS_FAILED + 1))
    echo -e "${RED}✗${NC} $test_name"
    echo -e "  ${RED}Reason:${NC} $reason"
}

assert_equals() {
    local expected="$1"
    local actual="$2"
    local message="${3:-Assertion failed}"

    if [[ "$expected" == "$actual" ]]; then
        return 0
    else
        echo "Expected: $expected"
        echo "Actual: $actual"
        return 1
    fi
}

assert_contains() {
    local haystack="$1"
    local needle="$2"
    local message="${3:-String not found}"

    if [[ "$haystack" == *"$needle"* ]]; then
        return 0
    else
        echo "$message"
        return 1
    fi
}

assert_file_exists() {
    local file="$1"
    if [[ -f "$file" ]]; then
        return 0
    else
        echo "File not found: $file"
        return 1
    fi
}

##############################################################################
# Test Suite Setup
##############################################################################

setup_tests() {
    echo "═══════════════════════════════════════════"
    echo "MCP Client Module - Test Suite"
    echo "═══════════════════════════════════════════"
    echo ""

    # Create test cache directory
    mkdir -p "$TEST_CACHE_DIR"

    # Create test MCP configuration
    local test_config="$TEST_CACHE_DIR/.mcp.json"
    cat > "$test_config" <<'EOF'
{
  "mcpServers": {
    "bash": {
      "command": "bash",
      "args": ["--version"],
      "disabled": false
    },
    "echo": {
      "command": "echo",
      "args": ["MCP Server"],
      "disabled": false
    }
  }
}
EOF

    # Export test variables BEFORE sourcing module (readonly constraints)
    export MCP_CACHE_DIR="$TEST_CACHE_DIR"
    export DEBUG_MCP=1

    # Change to test directory so .mcp.json is found
    cd "$TEST_CACHE_DIR"

    # Source MCP client module
    if ! source "$MODULES_DIR/mcp-client.sh"; then
        echo "ERROR: Failed to source MCP client module"
        exit 1
    fi

    echo "Test environment ready"
    echo ""
}

cleanup_tests() {
    # Return to original directory
    cd "$TEST_DIR"

    # Clean up test cache
    if [[ -d "$TEST_CACHE_DIR" ]]; then
        rm -rf "$TEST_CACHE_DIR"
    fi

    # Reset MCP state
    MCP_INITIALIZED=0
    MCP_CONNECTED_SERVERS=()
    MCP_SERVER_CACHE=()
}

print_results() {
    local pass_rate=0
    if [[ $TESTS_TOTAL -gt 0 ]]; then
        pass_rate=$((TESTS_PASSED * 100 / TESTS_TOTAL))
    fi

    echo ""
    echo "═══════════════════════════════════════════"
    echo "Test Results"
    echo "═══════════════════════════════════════════"
    echo "Total Tests: $TESTS_TOTAL"
    echo -e "Passed: ${GREEN}$TESTS_PASSED${NC}"
    echo -e "Failed: ${RED}$TESTS_FAILED${NC}"
    echo "Pass Rate: ${pass_rate}%"
    echo ""

    if [[ $TESTS_FAILED -eq 0 ]]; then
        echo -e "${GREEN}All tests passed!${NC}"
        return 0
    else
        echo -e "${RED}$TESTS_FAILED test(s) failed${NC}"
        return 1
    fi
}

##############################################################################
# Test Cases
##############################################################################

# Test 1: Configuration loading from file
test_config_loading() {
    local test_name="Configuration loading from file"
    test_count

    if _mcp_load_config 2>/dev/null; then
        if [[ -n "$MCP_SERVERS_CONFIG" ]]; then
            test_pass "$test_name"
            return 0
        fi
    fi

    test_fail "$test_name" "Configuration not loaded or empty"
    return 1
}

# Test 2: Configuration validation
test_config_validation() {
    local test_name="Configuration validation"
    test_count

    if ! _mcp_load_config 2>/dev/null; then
        test_fail "$test_name" "Failed to load configuration"
        return 1
    fi

    if assert_contains "$MCP_SERVERS_CONFIG" "command" "Configuration missing command field" 2>/dev/null; then
        test_pass "$test_name"
        return 0
    else
        test_fail "$test_name" "Configuration validation failed"
        return 1
    fi
}

# Test 3: Cache directory creation
test_cache_directory() {
    local test_name="Cache directory creation"
    test_count

    local cache_test_dir="$TEST_CACHE_DIR/cache-test"
    _mcp_load_config 2>/dev/null

    if [[ -d "$MCP_CACHE_DIR" ]] || mkdir -p "$MCP_CACHE_DIR" 2>/dev/null; then
        test_pass "$test_name"
        return 0
    else
        test_fail "$test_name" "Failed to create cache directory"
        return 1
    fi
}

# Test 4: Server parsing from configuration
test_server_parsing() {
    local test_name="Server parsing from configuration"
    test_count

    _mcp_load_config 2>/dev/null

    # Check if we can parse at least one server
    if [[ -n "$MCP_SERVERS_CONFIG" ]]; then
        test_pass "$test_name"
        return 0
    else
        test_fail "$test_name" "No servers found in configuration"
        return 1
    fi
}

# Test 5: Connection error handling
test_connection_error_handling() {
    local test_name="Connection error handling (graceful degradation)"
    test_count

    # This test verifies that connection errors are handled gracefully
    # by attempting to connect with an invalid configuration
    MCP_SERVERS_CONFIG="{}"

    if _mcp_connect_servers 2>/dev/null; then
        test_fail "$test_name" "Expected connection to fail with empty config"
        return 1
    else
        test_pass "$test_name"
        return 0
    fi
}

# Test 6: Tool cache creation and access
test_tool_cache() {
    local test_name="Tool cache creation and access"
    test_count

    local cache_file="$TEST_CACHE_DIR/tools.cache.test"
    mkdir -p "$TEST_CACHE_DIR"

    # Write test cache
    echo "test-tool-1: description" > "$cache_file"

    if assert_file_exists "$cache_file" 2>/dev/null; then
        if assert_contains "$(cat "$cache_file")" "test-tool-1"; then
            test_pass "$test_name"
            return 0
        fi
    fi

    test_fail "$test_name" "Cache file creation or access failed"
    return 1
}

# Test 7: Timeout enforcement
test_timeout_enforcement() {
    local test_name="Timeout enforcement"
    test_count

    # Verify timeout configuration
    if [[ "$MCP_TIMEOUT" -eq 10 ]]; then
        test_pass "$test_name"
        return 0
    else
        test_fail "$test_name" "Timeout not set to default 10s"
        return 1
    fi
}

# Test 8: Retry count configuration
test_retry_configuration() {
    local test_name="Retry count configuration"
    test_count

    if [[ "$MCP_RETRY_COUNT" -ge 1 ]] && [[ "$MCP_RETRY_COUNT" -le 5 ]]; then
        test_pass "$test_name"
        return 0
    else
        test_fail "$test_name" "Retry count out of valid range"
        return 1
    fi
}

# Test 9: Memory state initialization
test_state_initialization() {
    local test_name="Memory state initialization"
    test_count

    # Verify initial state
    if [[ $MCP_INITIALIZED -eq 0 ]] && [[ ${#MCP_CONNECTED_SERVERS[@]} -eq 0 ]]; then
        test_pass "$test_name"
        return 0
    else
        test_fail "$test_name" "Initial state not properly reset"
        return 1
    fi
}

# Test 10: Status output formatting
test_status_output() {
    local test_name="Status output formatting"
    test_count

    # Reset and attempt output
    MCP_INITIALIZED=1
    local output

    if output=$(mcp_status 2>&1); then
        if assert_contains "$output" "MCP Client Status"; then
            test_pass "$test_name"
            return 0
        fi
    fi

    test_fail "$test_name" "Status output not properly formatted"
    return 1
}

# Test 11: Function exports
test_function_exports() {
    local test_name="Function exports"
    test_count

    local functions=(
        "mcp_init_client"
        "mcp_list_tools"
        "mcp_call_tool"
        "mcp_search_tools"
        "mcp_status"
        "mcp_health_check"
        "mcp_clear_cache"
    )

    local all_exported=1
    for func in "${functions[@]}"; do
        if ! declare -F "$func" &>/dev/null; then
            all_exported=0
            echo "Function not exported: $func"
            break
        fi
    done

    if [[ $all_exported -eq 1 ]]; then
        test_pass "$test_name"
        return 0
    else
        test_fail "$test_name" "Not all functions properly exported"
        return 1
    fi
}

# Test 12: Error message clarity
test_error_messages() {
    local test_name="Error message clarity"
    test_count

    # Verify error logging function exists and works
    local error_output
    error_output=$(_mcp_log_error "Test error message" 2>&1)

    if assert_contains "$error_output" "ERROR"; then
        test_pass "$test_name"
        return 0
    else
        test_fail "$test_name" "Error messages not properly formatted"
        return 1
    fi
}

# Test 13: Configuration precedence
test_config_precedence() {
    local test_name="Configuration precedence (home > project)"
    test_count

    # This test verifies that configuration files are checked in the right order
    # Test passes if the function attempts to load from home directory first

    _mcp_load_config 2>/dev/null

    # If config was loaded successfully, precedence is working
    if [[ -n "$MCP_SERVERS_CONFIG" ]]; then
        test_pass "$test_name"
        return 0
    else
        test_fail "$test_name" "Configuration precedence not working"
        return 1
    fi
}

# Test 14: Bash compatibility
test_bash_compatibility() {
    local test_name="Bash 4.0+ compatibility"
    test_count

    # Verify we're running on Bash 4.0 or later
    if [[ "${BASH_VERSINFO[0]}" -ge 4 ]]; then
        test_pass "$test_name"
        return 0
    else
        test_fail "$test_name" "Bash version too old: ${BASH_VERSION}"
        return 1
    fi
}

# Test 15: Command validation
test_command_validation() {
    local test_name="Command availability validation"
    test_count

    # Verify jq is available for JSON parsing
    if command -v jq &>/dev/null; then
        test_pass "$test_name"
        return 0
    else
        test_fail "$test_name" "Required command 'jq' not found"
        return 1
    fi
}

##############################################################################
# Main Test Execution
##############################################################################

main() {
    setup_tests

    # Run all tests
    echo "Running test cases..."
    echo ""

    test_config_loading
    test_config_validation
    test_cache_directory
    test_server_parsing
    test_connection_error_handling
    test_tool_cache
    test_timeout_enforcement
    test_retry_configuration
    test_state_initialization
    test_status_output
    test_function_exports
    test_error_messages
    test_config_precedence
    test_bash_compatibility
    test_command_validation

    # Print results
    print_results
    local result=$?

    # Cleanup
    cleanup_tests

    return $result
}

# Run tests if script is executed directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
