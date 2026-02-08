#!/bin/bash

##############################################################################
# Tests for SQLite Persistence Layer
#
# Comprehensive test suite for database operations with:
# - Receipt persistence and retrieval
# - Agent memory management
# - Audit trail logging
# - Data export and analytics
# - Error handling and recovery
#
# Version: 1.0.0
##############################################################################

set -euo pipefail

# Source the persistence module
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PARENT_DIR="$(dirname "${SCRIPT_DIR}")"
source "${SCRIPT_DIR}/persistence.sh"

# Test configuration
readonly TEST_DB_PATH="${SCRIPT_DIR}/test.db"
readonly TEST_DIR="${SCRIPT_DIR}/test-workspace"
readonly TESTS_PASSED=0
readonly TESTS_FAILED=0

# Color codes
readonly GREEN='\033[0;32m'
readonly RED='\033[0;31m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m'

##############################################################################
# Test Helper Functions
##############################################################################

setup_test_env() {
    # Create test workspace
    mkdir -p "${TEST_DIR}"/.ggen

    # Override DB path for tests
    export DB_DIR="${TEST_DIR}/.ggen"
    export DB_PATH="${TEST_DB_PATH}"

    echo -e "${BLUE}[TEST SETUP]${NC} Environment initialized"
}

teardown_test_env() {
    # Clean up test database
    if [[ -f "${TEST_DB_PATH}" ]]; then
        rm -f "${TEST_DB_PATH}"
    fi

    # Clean up test workspace
    if [[ -d "${TEST_DIR}" ]]; then
        rm -rf "${TEST_DIR}"
    fi

    echo -e "${BLUE}[TEST TEARDOWN]${NC} Environment cleaned"
}

assert_success() {
    local test_name="$1"
    if [[ $? -eq 0 ]]; then
        echo -e "${GREEN}✓${NC} ${test_name}"
        ((TESTS_PASSED++))
        return 0
    else
        echo -e "${RED}✗${NC} ${test_name}"
        ((TESTS_FAILED++))
        return 1
    fi
}

assert_equals() {
    local test_name="$1"
    local expected="$2"
    local actual="$3"

    if [[ "${expected}" == "${actual}" ]]; then
        echo -e "${GREEN}✓${NC} ${test_name}"
        ((TESTS_PASSED++))
        return 0
    else
        echo -e "${RED}✗${NC} ${test_name} (expected: ${expected}, got: ${actual})"
        ((TESTS_FAILED++))
        return 1
    fi
}

assert_contains() {
    local test_name="$1"
    local haystack="$2"
    local needle="$3"

    if [[ "${haystack}" == *"${needle}"* ]]; then
        echo -e "${GREEN}✓${NC} ${test_name}"
        ((TESTS_PASSED++))
        return 0
    else
        echo -e "${RED}✗${NC} ${test_name} (expected to contain: ${needle})"
        ((TESTS_FAILED++))
        return 1
    fi
}

##############################################################################
# Initialization Tests
##############################################################################

test_db_init() {
    echo ""
    echo -e "${BLUE}=== Database Initialization Tests ===${NC}"

    setup_test_env

    # Test 1: Database initialization
    db_init
    assert_success "Database initialization"

    # Test 2: Verify database file created
    [[ -f "${TEST_DB_PATH}" ]]
    assert_success "Database file created"

    # Test 3: Verify schema tables exist
    local table_count
    table_count=$(sqlite3 "${TEST_DB_PATH}" \
        "SELECT COUNT(*) FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%'")
    assert_equals "Schema tables created" "4" "${table_count}"

    teardown_test_env
}

##############################################################################
# Receipt Tests
##############################################################################

test_receipt_persistence() {
    echo ""
    echo -e "${BLUE}=== Receipt Persistence Tests ===${NC}"

    setup_test_env
    db_init

    # Test 1: Save receipt successfully
    local test_receipt='{"status":"success","files":5,"duration_ms":1234}'
    db_save_receipt "exec-001" "validator-1" "validation" "${test_receipt}" "success" 1234
    assert_success "Save receipt"

    # Test 2: Query receipts by agent_id
    local receipts
    receipts=$(db_query_receipts "validator-1" "json")
    assert_contains "Query receipts" "${receipts}" "exec-001"

    # Test 3: Get specific receipt
    local receipt_list
    receipt_list=$(db_query_receipts "validator-1" "json")
    local receipt_id
    receipt_id=$(echo "${receipt_list}" | jq -r '.[0].receipt_id' 2>/dev/null || echo "")

    if [[ -n "${receipt_id}" ]]; then
        local receipt_data
        receipt_data=$(db_get_receipt "${receipt_id}")
        assert_contains "Get receipt details" "${receipt_data}" "status"
    fi

    # Test 4: Multiple receipts from same agent
    db_save_receipt "exec-002" "validator-1" "validation" "${test_receipt}" "success" 1500
    db_save_receipt "exec-003" "validator-1" "validation" "${test_receipt}" "success" 1000

    local count
    count=$(db_query_receipts "validator-1" "json" | jq 'length' 2>/dev/null || echo "0")
    assert_equals "Multiple receipts stored" "3" "${count}"

    # Test 5: Receipt status tracking
    db_save_receipt "exec-004" "validator-1" "validation" "${test_receipt}" "error" 2000
    local error_receipts
    error_receipts=$(sqlite3 "${TEST_DB_PATH}" \
        "SELECT COUNT(*) FROM receipts WHERE status = 'error'")
    assert_equals "Error status tracked" "1" "${error_receipts}"

    teardown_test_env
}

test_receipt_error_handling() {
    echo ""
    echo -e "${BLUE}=== Receipt Error Handling Tests ===${NC}"

    setup_test_env
    db_init

    # Test 1: Invalid JSON handling
    if ! db_save_receipt "exec-001" "validator-1" "validation" "invalid json" "success" 100 2>/dev/null; then
        echo -e "${GREEN}✓${NC} Invalid JSON rejected"
        ((TESTS_PASSED++))
    else
        echo -e "${RED}✗${NC} Invalid JSON should be rejected"
        ((TESTS_FAILED++))
    fi

    # Test 2: Missing execution_id
    if ! db_save_receipt "" "validator-1" "validation" '{}' "success" 100 2>/dev/null; then
        echo -e "${GREEN}✓${NC} Missing execution_id rejected"
        ((TESTS_PASSED++))
    else
        echo -e "${RED}✗${NC} Missing execution_id should be rejected"
        ((TESTS_FAILED++))
    fi

    # Test 3: Missing agent_id
    if ! db_save_receipt "exec-001" "" "validation" '{}' "success" 100 2>/dev/null; then
        echo -e "${GREEN}✓${NC} Missing agent_id rejected"
        ((TESTS_PASSED++))
    else
        echo -e "${RED}✗${NC} Missing agent_id should be rejected"
        ((TESTS_FAILED++))
    fi

    teardown_test_env
}

##############################################################################
# Agent Memory Tests
##############################################################################

test_agent_memory_persistence() {
    echo ""
    echo -e "${BLUE}=== Agent Memory Persistence Tests ===${NC}"

    setup_test_env
    db_init

    # Test 1: Save agent memory
    local memory='{"state":"active","iterations":5,"results":[]}'
    db_save_memory "agent-validator" "validation" "${memory}"
    assert_success "Save agent memory"

    # Test 2: Retrieve agent memory
    local retrieved
    retrieved=$(db_get_memory "agent-validator")
    assert_contains "Retrieve agent memory" "${retrieved}" "active"

    # Test 3: Update agent memory
    local updated_memory='{"state":"paused","iterations":10,"results":["result1","result2"]}'
    db_save_memory "agent-validator" "validation" "${updated_memory}"

    local updated
    updated=$(db_get_memory "agent-validator")
    assert_contains "Update agent memory" "${updated}" "paused"

    # Test 4: Multiple agent memory
    db_save_memory "agent-coder" "code_generation" '{"state":"ready"}'
    db_save_memory "agent-tester" "testing" '{"state":"running"}'

    local memory_count
    memory_count=$(db_query_memory "%" "json" | jq 'length' 2>/dev/null || echo "0")
    assert_equals "Multiple agent memory stored" "3" "${memory_count}"

    # Test 5: Memory version tracking
    local version_before
    version_before=$(sqlite3 "${TEST_DB_PATH}" \
        "SELECT version FROM agent_memory WHERE agent_id = 'agent-validator'")

    db_save_memory "agent-validator" "validation" '{"state":"active","iterations":15}'

    local version_after
    version_after=$(sqlite3 "${TEST_DB_PATH}" \
        "SELECT version FROM agent_memory WHERE agent_id = 'agent-validator'")

    if [[ ${version_after} -gt ${version_before} ]]; then
        echo -e "${GREEN}✓${NC} Memory version incremented"
        ((TESTS_PASSED++))
    else
        echo -e "${RED}✗${NC} Memory version should increment"
        ((TESTS_FAILED++))
    fi

    teardown_test_env
}

##############################################################################
# Audit Logging Tests
##############################################################################

test_audit_logging() {
    echo ""
    echo -e "${BLUE}=== Audit Logging Tests ===${NC}"

    setup_test_env
    db_init

    # Test 1: Log basic audit entry
    db_log_audit "agent-validator" "validate_schema" "completed" "validation" 1200 "Schema validation passed" ""
    assert_success "Log audit entry"

    # Test 2: Query audit trail
    local audit_entries
    audit_entries=$(db_query_audit_trail "agent-%" "7" "100" "json")
    assert_contains "Query audit trail" "${audit_entries}" "validate_schema"

    # Test 3: Multiple audit entries
    db_log_audit "agent-validator" "extract_data" "completed" "extraction" 800 "" ""
    db_log_audit "agent-validator" "render_template" "completed" "rendering" 500 "" ""
    db_log_audit "agent-validator" "format_output" "failed" "formatting" 0 "" "Formatting error"

    local total_entries
    total_entries=$(sqlite3 "${TEST_DB_PATH}" \
        "SELECT COUNT(*) FROM audit_log WHERE agent_id = 'agent-validator'")
    assert_equals "Multiple audit entries logged" "4" "${total_entries}"

    # Test 4: Audit entry with error message
    local error_entry
    error_entry=$(db_query_audit_trail "agent-validator" "7" "1" "json")
    assert_contains "Error message recorded" "${error_entry}" "Formatting error"

    # Test 5: Status filtering
    local failed_count
    failed_count=$(sqlite3 "${TEST_DB_PATH}" \
        "SELECT COUNT(*) FROM audit_log WHERE status = 'failed'")
    assert_equals "Failed operations tracked" "1" "${failed_count}"

    teardown_test_env
}

##############################################################################
# Data Export Tests
##############################################################################

test_data_export() {
    echo ""
    echo -e "${BLUE}=== Data Export Tests ===${NC}"

    setup_test_env
    db_init

    # Setup test data
    db_save_receipt "exec-001" "agent-1" "validation" '{"status":"success"}' "success" 1000
    db_save_memory "agent-1" "validation" '{"state":"active"}'
    db_log_audit "agent-1" "test_op" "completed" "test" 500 "" ""

    # Test 1: Export audit trail to JSON
    local export_file="${TEST_DIR}/audit-export.json"
    db_export_audit_trail "${export_file}" "json"
    assert_success "Export audit trail to JSON"

    # Test 2: Verify exported file content
    if [[ -f "${export_file}" ]] && jq empty "${export_file}" 2>/dev/null; then
        echo -e "${GREEN}✓${NC} Exported JSON is valid"
        ((TESTS_PASSED++))
    else
        echo -e "${RED}✗${NC} Exported JSON should be valid"
        ((TESTS_FAILED++))
    fi

    # Test 3: Export full backup
    local backup_dir="${TEST_DIR}/backup"
    db_export_full_backup "${backup_dir}"
    assert_success "Export full backup"

    # Test 4: Verify backup contents
    if [[ -f "${backup_dir}/receipts.json" ]] && [[ -f "${backup_dir}/audit_log.json" ]] && [[ -f "${backup_dir}/agent_memory.json" ]]; then
        echo -e "${GREEN}✓${NC} Backup files created"
        ((TESTS_PASSED++))
    else
        echo -e "${RED}✗${NC} All backup files should be created"
        ((TESTS_FAILED++))
    fi

    teardown_test_env
}

##############################################################################
# Analytics Tests
##############################################################################

test_analytics() {
    echo ""
    echo -e "${BLUE}=== Analytics Tests ===${NC}"

    setup_test_env
    db_init

    # Setup test data
    for i in {1..5}; do
        db_save_receipt "exec-00${i}" "agent-validator" "validation" '{"status":"success"}' "success" $((i * 200))
        db_log_audit "agent-validator" "operation_${i}" "completed" "test" $((i * 100)) "" ""
    done

    # Test 1: Get analytics
    local analytics
    analytics=$(db_analytics "agent-validator" "last-7-days")
    assert_contains "Generate analytics" "${analytics}" "agent_metrics"

    # Test 2: Verify analytics structure
    if echo "${analytics}" | jq '.agent_metrics' > /dev/null 2>&1; then
        echo -e "${GREEN}✓${NC} Analytics structure valid"
        ((TESTS_PASSED++))
    else
        echo -e "${RED}✗${NC} Analytics should have valid structure"
        ((TESTS_FAILED++))
    fi

    # Test 3: Get database statistics
    local stats
    stats=$(db_stats)
    assert_contains "Get database stats" "${stats}" "receipts_total"

    # Test 4: Verify stats content
    if echo "${stats}" | jq '.receipts_total' > /dev/null 2>&1; then
        echo -e "${GREEN}✓${NC} Database stats valid"
        ((TESTS_PASSED++))
    else
        echo -e "${RED}✗${NC} Stats should be valid JSON"
        ((TESTS_FAILED++))
    fi

    teardown_test_env
}

##############################################################################
# Maintenance Tests
##############################################################################

test_database_maintenance() {
    echo ""
    echo -e "${BLUE}=== Database Maintenance Tests ===${NC}"

    setup_test_env
    db_init

    # Setup test data
    db_save_receipt "exec-001" "agent-1" "validation" '{}' "success" 100
    db_log_audit "agent-1" "operation" "completed" "test" 50 "" ""

    # Test 1: Cleanup (should not delete recent data)
    local initial_receipt_count
    initial_receipt_count=$(sqlite3 "${TEST_DB_PATH}" "SELECT COUNT(*) FROM receipts")

    db_cleanup 90

    local after_cleanup_count
    after_cleanup_count=$(sqlite3 "${TEST_DB_PATH}" "SELECT COUNT(*) FROM receipts")

    assert_equals "Cleanup preserves recent data" "${initial_receipt_count}" "${after_cleanup_count}"

    # Test 2: Database reset
    db_reset
    assert_success "Database reset"

    # Test 3: Verify database is reinitialized
    local tables_after_reset
    tables_after_reset=$(sqlite3 "${TEST_DB_PATH}" \
        "SELECT COUNT(*) FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%'")
    assert_equals "Database reinitialized after reset" "4" "${tables_after_reset}"

    teardown_test_env
}

##############################################################################
# Concurrent Access Tests
##############################################################################

test_concurrent_access() {
    echo ""
    echo -e "${BLUE}=== Concurrent Access Tests ===${NC}"

    setup_test_env
    db_init

    # Test 1: Parallel writes (simulated)
    for i in {1..10}; do
        db_save_receipt "exec-00${i}" "agent-${i}" "validation" '{}' "success" 100 &
    done
    wait

    local final_count
    final_count=$(sqlite3 "${TEST_DB_PATH}" "SELECT COUNT(*) FROM receipts")
    assert_equals "Parallel writes completed" "10" "${final_count}"

    teardown_test_env
}

##############################################################################
# Test Execution
##############################################################################

run_all_tests() {
    echo ""
    echo -e "${BLUE}╔════════════════════════════════════════════════════════╗${NC}"
    echo -e "${BLUE}║  SQLite Persistence Layer - Comprehensive Test Suite   ║${NC}"
    echo -e "${BLUE}╚════════════════════════════════════════════════════════╝${NC}"

    # Run test suites
    test_db_init
    test_receipt_persistence
    test_receipt_error_handling
    test_agent_memory_persistence
    test_audit_logging
    test_data_export
    test_analytics
    test_database_maintenance
    test_concurrent_access

    # Summary
    echo ""
    echo -e "${BLUE}════════════════════════════════════════════════════════${NC}"
    echo -e "Tests Passed: ${GREEN}${TESTS_PASSED}${NC}"
    echo -e "Tests Failed: ${RED}${TESTS_FAILED}${NC}"
    echo -e "${BLUE}════════════════════════════════════════════════════════${NC}"

    if [[ ${TESTS_FAILED} -eq 0 ]]; then
        echo -e "${GREEN}All tests passed!${NC}"
        return 0
    else
        echo -e "${RED}Some tests failed!${NC}"
        return 1
    fi
}

# Main
run_all_tests
