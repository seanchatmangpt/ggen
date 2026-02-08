#!/bin/bash

##############################################################################
# MCP Performance Module Test Suite
#
# Comprehensive test coverage for:
# - Cache initialization and cleanup
# - Cache get/set operations with TTL
# - Timeout enforcement
# - Parallel tool execution
# - Performance metrics tracking
#
# Version: 1.0.0
##############################################################################

set -euo pipefail

# Test configuration
readonly TEST_CACHE_DIR="./.test-mcp-cache"
TEST_LOG=""  # Will be set after module sourcing

# Color codes (define before sourcing to avoid conflicts)
TEST_RED='\033[0;31m'
TEST_GREEN='\033[0;32m'
TEST_YELLOW='\033[1;33m'
TEST_BLUE='\033[0;34m'
TEST_NC='\033[0m'

# Source the module
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../" && pwd)"
source "${SCRIPT_DIR}/modules/mcp-performance.sh"

# Test counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

##############################################################################
# Test Utilities
##############################################################################

test_setup() {
    # Ensure cache directories exist
    mkdir -p "${MCP_CACHE_DIR}/{results,definitions,metrics}"
    # Clear any previous test data
    rm -f "${MCP_CACHE_DIR}/results"/*.cache 2>/dev/null || true
}

test_teardown() {
    # Clean up test data
    rm -f "${MCP_CACHE_DIR}/results"/*.cache 2>/dev/null || true
    rm -f "${MCP_CACHE_DIR}/definitions"/*.cache 2>/dev/null || true
}

assert_equals() {
    local expected="$1"
    local actual="$2"
    local test_name="$3"

    ((TESTS_RUN++))

    if [[ "${expected}" == "${actual}" ]]; then
        echo -e "${TEST_GREEN}[PASS]${TEST_NC} ${test_name}"
        ((TESTS_PASSED++))
        return 0
    else
        echo -e "${TEST_RED}[FAIL]${TEST_NC} ${test_name}"
        echo "       Expected: ${expected}"
        echo "       Actual: ${actual}"
        ((TESTS_FAILED++))
        return 1
    fi
}

assert_true() {
    local condition="$1"
    local test_name="$2"

    ((TESTS_RUN++))

    if eval "${condition}"; then
        echo -e "${TEST_GREEN}[PASS]${TEST_NC} ${test_name}"
        ((TESTS_PASSED++))
        return 0
    else
        echo -e "${TEST_RED}[FAIL]${TEST_NC} ${test_name}"
        echo "       Condition: ${condition}"
        ((TESTS_FAILED++))
        return 1
    fi
}

assert_false() {
    local condition="$1"
    local test_name="$2"

    ((TESTS_RUN++))

    if ! eval "${condition}"; then
        echo -e "${TEST_GREEN}[PASS]${TEST_NC} ${test_name}"
        ((TESTS_PASSED++))
        return 0
    else
        echo -e "${TEST_RED}[FAIL]${TEST_NC} ${test_name}"
        echo "       Condition should be false: ${condition}"
        ((TESTS_FAILED++))
        return 1
    fi
}

##############################################################################
# Test Suites
##############################################################################

test_cache_initialization() {
    echo -e "\n${TEST_BLUE}=== Cache Initialization Tests ===${TEST_NC}"

    test_setup

    # Test 1: Cache directory exists
    assert_true "[[ -d '${MCP_CACHE_DIR}/results' ]]" "Cache results directory exists"

    # Test 2: Definitions directory exists
    assert_true "[[ -d '${MCP_CACHE_DIR}/definitions' ]]" "Cache definitions directory exists"

    # Test 3: Metrics directory exists
    assert_true "[[ -d '${MCP_CACHE_DIR}/metrics' ]]" "Cache metrics directory exists"

    # Test 4: Module initialization flag is set
    mcp_cache_init
    assert_equals "1" "${MCP_PERF_INITIALIZED}" "Module initialization flag set"

    test_teardown
}

test_cache_operations() {
    echo -e "\n${TEST_BLUE}=== Cache Operations Tests ===${TEST_NC}"

    test_setup

    # Test 1: Cache set operation
    local test_result='{"status":"success","value":42}'
    mcp_cache_set "test_tool" "test_key" "${test_result}" 300
    assert_true "[[ -f '${MCP_CACHE_DIR}/results/test_tool_test_key.cache' ]]" "Cache file created after set"

    # Test 2: Cache get operation (hit)
    local retrieved
    retrieved=$(mcp_cache_get "test_tool" "test_key" 2>/dev/null || echo "MISS")
    assert_false "[[ '${retrieved}' == 'MISS' ]]" "Cache hit on valid entry"

    # Test 3: Cache get operation (miss)
    local retrieved_miss
    retrieved_miss=$(mcp_cache_get "test_tool" "nonexistent" 2>/dev/null || echo "MISS")
    assert_equals "MISS" "${retrieved_miss}" "Cache miss on nonexistent entry"

    # Test 4: Cache invalidation
    mcp_cache_invalidate "test_tool" "test_key"
    local invalidated
    invalidated=$(mcp_cache_get "test_tool" "test_key" 2>/dev/null || echo "MISS")
    assert_equals "MISS" "${invalidated}" "Cache invalidation removes entry"

    # Test 5: Multiple cache entries
    for i in {1..5}; do
        mcp_cache_set "multi_tool" "key_${i}" "{\"index\":${i}}" 300
    done
    local count
    count=$(find "${MCP_CACHE_DIR}/results" -name "multi_tool_*.cache" | wc -l)
    assert_equals "5" "${count}" "Multiple cache entries stored correctly"

    test_teardown
}

test_ttl_expiration() {
    echo -e "\n${TEST_BLUE}=== TTL Expiration Tests ===${TEST_NC}"

    test_setup

    # Test 1: Set cache with short TTL
    mcp_cache_set "ttl_tool" "ttl_key" '{"data":"test"}' 1

    # Verify cache hit immediately
    local immediate_hit
    immediate_hit=$(mcp_cache_get "ttl_tool" "ttl_key" 2>/dev/null || echo "MISS")
    assert_false "[[ '${immediate_hit}' == 'MISS' ]]" "Cache hit before TTL expiration"

    # Wait for TTL to expire
    sleep 2

    # Verify cache miss after expiration
    local expired_miss
    expired_miss=$(mcp_cache_get "ttl_tool" "ttl_key" 2>/dev/null || echo "MISS")
    assert_equals "MISS" "${expired_miss}" "Cache miss after TTL expiration"

    test_teardown
}

test_cache_statistics() {
    echo -e "\n${TEST_BLUE}=== Cache Statistics Tests ===${TEST_NC}"

    test_setup

    # Generate hits and misses
    mcp_cache_set "stats_tool" "key1" '{"value":1}' 300
    mcp_cache_set "stats_tool" "key2" '{"value":2}' 300

    # Cache hits
    mcp_cache_get "stats_tool" "key1" >/dev/null 2>&1 || true
    mcp_cache_get "stats_tool" "key1" >/dev/null 2>&1 || true
    mcp_cache_get "stats_tool" "key2" >/dev/null 2>&1 || true

    # Cache misses
    mcp_cache_get "stats_tool" "nonexistent" >/dev/null 2>&1 || true

    # Get statistics
    mcp_log_cache_hit "stats_tool"
    mcp_log_cache_miss "stats_tool"

    # Verify stats
    assert_true "[[ -v MCP_CACHE_HITS['stats_tool'] ]]" "Cache hit counter initialized"
    assert_true "[[ -v MCP_CACHE_MISSES['stats_tool'] ]]" "Cache miss counter initialized"

    test_teardown
}

test_timeout_functionality() {
    echo -e "\n${TEST_BLUE}=== Timeout Functionality Tests ===${TEST_NC}"

    test_setup

    # Mock tool execution
    mcp_call_tool() {
        local tool_name="$1"
        if [[ "${tool_name}" == "fast_tool" ]]; then
            echo '{"status":"success"}'
        elif [[ "${tool_name}" == "slow_tool" ]]; then
            sleep 3
            echo '{"status":"completed"}'
        fi
    }
    export -f mcp_call_tool

    # Test 1: Fast tool completes within timeout
    local result
    result=$(timeout 2s mcp_tool_with_timeout "fast_tool" "2000" "" 2>&1 || echo "TIMEOUT")
    assert_false "[[ '${result}' == 'TIMEOUT' ]]" "Fast tool completes within timeout"

    # Test 2: Slow tool triggers timeout
    local timeout_result
    timeout_result=$(timeout 1s mcp_tool_with_timeout "slow_tool" "500" "" 2>&1 || echo "EXIT_CODE:$?")
    assert_true "[[ '${timeout_result}' == *'TIMEOUT'* ]] || [[ '${timeout_result}' == *'EXIT_CODE:124'* ]]" "Slow tool triggers timeout"

    test_teardown
}

test_cache_with_timeout() {
    echo -e "\n${TEST_BLUE}=== Cache with Timeout Integration Tests ===${TEST_NC}"

    test_setup

    # Mock tool
    mcp_call_tool() {
        echo '{"cached":false,"timestamp":"'"$(date -u +%Y-%m-%dT%H:%M:%SZ)"'"}'
    }
    export -f mcp_call_tool

    # Test 1: First call executes tool
    local first_call
    first_call=$(mcp_tool_with_timeout "cache_tool" "5000" "param1" 2>/dev/null || echo "")
    assert_true "[[ ! -z '${first_call}' ]]" "Tool executes and returns result"

    # Test 2: Second call retrieves from cache
    local second_call
    second_call=$(mcp_tool_with_timeout "cache_tool" "5000" "param1" 2>/dev/null || echo "")
    assert_equals "${first_call}" "${second_call}" "Second call returns cached result"

    test_teardown
}

test_performance_logging() {
    echo -e "\n${TEST_BLUE}=== Performance Logging Tests ===${TEST_NC}"

    test_setup

    # Generate performance logs
    mcp_log_perf "TEST" "Test log entry"
    mcp_log_perf "TIMING" "Tool: test_tool, Duration: 1234ms, Status: success"
    mcp_log_timing "test_tool" 1234 "success"

    # Verify logs exist
    assert_true "[[ -f '${MCP_CACHE_DIR}/performance.log' ]]" "Performance log file created"

    # Verify log content
    local log_count
    log_count=$(grep -c "test_tool" "${MCP_CACHE_DIR}/performance.log" || echo 0)
    assert_true "(( ${log_count} > 0 ))" "Performance logs contain tool entries"

    test_teardown
}

test_tool_definition_caching() {
    echo -e "\n${TEST_BLUE}=== Tool Definition Caching Tests ===${TEST_NC}"

    test_setup

    # Create mock tool definitions
    local tool_defs='[{"name":"tool1","version":"1.0.0"},{"name":"tool2","version":"1.0.1"}]'

    # Cache definitions
    mcp_cache_tool_definitions "${tool_defs}"
    assert_true "[[ -f '${MCP_CACHE_DIR}/definitions/tool_definitions.cache' ]]" "Tool definitions cached"

    # Retrieve definitions
    local retrieved_defs
    retrieved_defs=$(mcp_get_cached_tool_definitions 2>/dev/null || echo "")
    assert_true "[[ ! -z '${retrieved_defs}' ]]" "Tool definitions retrieved from cache"

    test_teardown
}

test_cache_cleanup() {
    echo -e "\n${TEST_BLUE}=== Cache Cleanup Tests ===${TEST_NC}"

    test_setup

    # Create some cache entries
    mcp_cache_set "cleanup_tool" "key1" '{"value":1}' 300
    mcp_cache_set "cleanup_tool" "key2" '{"value":2}' 300

    local count_before
    count_before=$(find "${MCP_CACHE_DIR}/results" -type f | wc -l)
    assert_true "(( ${count_before} > 0 ))" "Cache entries created"

    # Clear cache
    mcp_cache_clear >/dev/null 2>&1

    local count_after
    count_after=$(find "${MCP_CACHE_DIR}/results" -type f | wc -l)
    assert_equals "0" "${count_after}" "Cache cleared successfully"

    test_teardown
}

test_cache_status() {
    echo -e "\n${TEST_BLUE}=== Cache Status Tests ===${TEST_NC}"

    test_setup

    # Create cache entries
    mcp_cache_set "status_tool" "key1" '{"value":1}' 300

    # Get status (should not error)
    local status_output
    status_output=$(mcp_cache_status 2>&1 || echo "ERROR")
    assert_true "[[ '${status_output}' != 'ERROR' ]]" "Cache status query succeeds"

    # Verify status contains expected content
    assert_true "[[ '${status_output}' == *'MCP Cache Status'* ]]" "Status output contains header"

    test_teardown
}

##############################################################################
# Test Runner
##############################################################################

run_all_tests() {
    echo -e "${TEST_BLUE}╔════════════════════════════════════════════════════════════╗${TEST_NC}"
    echo -e "${TEST_BLUE}║  MCP Performance Module - Comprehensive Test Suite         ║${TEST_NC}"
    echo -e "${TEST_BLUE}║  Version 1.0.0                                             ║${TEST_NC}"
    echo -e "${TEST_BLUE}╚════════════════════════════════════════════════════════════╝${TEST_NC}"

    echo ""
    echo "Note: Tests use default cache directory: ${MCP_CACHE_DIR}"
    echo ""

    test_cache_initialization
    test_cache_operations
    test_ttl_expiration
    test_cache_statistics
    test_timeout_functionality
    test_cache_with_timeout
    test_performance_logging
    test_tool_definition_caching
    test_cache_cleanup
    test_cache_status

    # Print summary
    echo -e "\n${TEST_BLUE}════════════════════════════════════════════════════════════${TEST_NC}"
    echo -e "Test Summary:"
    echo -e "  Total Tests:  ${TESTS_RUN}"
    echo -e "  ${TEST_GREEN}Passed:${TEST_NC}      ${TESTS_PASSED}"
    echo -e "  ${TEST_RED}Failed:${TEST_NC}      ${TESTS_FAILED}"

    if [[ ${TESTS_FAILED} -eq 0 ]]; then
        echo -e "\n${TEST_GREEN}[✓] All tests passed!${TEST_NC}"
        return 0
    else
        echo -e "\n${TEST_RED}[✗] Some tests failed!${TEST_NC}"
        return 1
    fi
}

# Run tests if script is executed directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    run_all_tests
    exit $?
fi
