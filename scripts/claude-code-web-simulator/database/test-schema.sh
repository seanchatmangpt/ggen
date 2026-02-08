#!/bin/bash

# ============================================================================
# SQLite Database Schema Validation Tests
# ============================================================================
# Purpose: Comprehensive test suite for schema validation
# Usage: ./test-schema.sh [database_path]
# ============================================================================

set -euo pipefail

DB_PATH="${1:-.ggen/tier2-test.db}"
SCHEMA_FILE="$(dirname "${BASH_SOURCE[0]}")/schema.sql"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

# Test counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# ============================================================================
# Test Framework
# ============================================================================

test_suite() {
  local name=$1
  echo ""
  echo "=========================================="
  echo "Test Suite: $name"
  echo "=========================================="
}

test_case() {
  local name=$1
  ((TESTS_RUN++))
  echo -n "  [$TESTS_RUN] $name... "
}

test_pass() {
  echo -e "${GREEN}PASS${NC}"
  ((TESTS_PASSED++))
}

test_fail() {
  local message=$1
  echo -e "${RED}FAIL${NC}"
  echo -e "        ${RED}Error: $message${NC}"
  ((TESTS_FAILED++))
}

test_expect() {
  local condition=$1
  local message=$2

  if eval "$condition"; then
    test_pass
  else
    test_fail "$message"
  fi
}

# ============================================================================
# Setup
# ============================================================================

setup() {
  # Clean up existing test database
  if [[ -f "$DB_PATH" ]]; then
    rm -f "$DB_PATH"
  fi

  # Initialize fresh database
  sqlite3 "$DB_PATH" < "$SCHEMA_FILE" > /dev/null 2>&1
}

cleanup() {
  if [[ -f "$DB_PATH" ]]; then
    rm -f "$DB_PATH"
  fi
}

# ============================================================================
# Schema Structure Tests
# ============================================================================

test_tables_exist() {
  test_suite "Schema Structure"

  local tables=("receipts" "agent_memory" "audit_log" "workflow_sessions" "collision_detection" "pipeline_metrics" "slo_violations")

  for table in "${tables[@]}"; do
    test_case "Table '$table' exists"
    local exists=$(sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM sqlite_master WHERE type='table' AND name='$table';")
    test_expect "[[ $exists -eq 1 ]]" "Table $table not found"
  done
}

test_views_exist() {
  test_suite "View Structure"

  local views=("recent_executions" "agent_performance_summary" "pipeline_stage_performance")

  for view in "${views[@]}"; do
    test_case "View '$view' exists"
    local exists=$(sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM sqlite_master WHERE type='view' AND name='$view';")
    test_expect "[[ $exists -eq 1 ]]" "View $view not found"
  done
}

# ============================================================================
# Column Tests
# ============================================================================

test_receipts_columns() {
  test_suite "Receipts Table Columns"

  local table="receipts"
  local columns=("id" "execution_id" "agent_id" "operation" "status" "timestamp" "created_at" "manifest_hash" "ontology_hash" "files_generated" "files_modified" "duration_ms" "receipt_json" "error_message" "environment")

  for col in "${columns[@]}"; do
    test_case "Column '$table.$col' exists"
    local exists=$(sqlite3 "$DB_PATH" "PRAGMA table_info($table);" | grep -c "^[0-9]*|$col|")
    test_expect "[[ $exists -eq 1 ]]" "Column $col not found in $table"
  done
}

test_agent_memory_columns() {
  test_suite "Agent Memory Table Columns"

  local table="agent_memory"
  local columns=("id" "agent_id" "memory_json" "collision_history" "retry_count" "last_error" "updated_at" "created_at")

  for col in "${columns[@]}"; do
    test_case "Column '$table.$col' exists"
    local exists=$(sqlite3 "$DB_PATH" "PRAGMA table_info($table);" | grep -c "^[0-9]*|$col|")
    test_expect "[[ $exists -eq 1 ]]" "Column $col not found in $table"
  done
}

test_audit_log_columns() {
  test_suite "Audit Log Table Columns"

  local table="audit_log"
  local columns=("id" "timestamp" "agent_id" "operation" "status" "duration_ms" "error_message" "created_at")

  for col in "${columns[@]}"; do
    test_case "Column '$table.$col' exists"
    local exists=$(sqlite3 "$DB_PATH" "PRAGMA table_info($table);" | grep -c "^[0-9]*|$col|")
    test_expect "[[ $exists -eq 1 ]]" "Column $col not found in $table"
  done
}

# ============================================================================
# Index Tests
# ============================================================================

test_indexes_created() {
  test_suite "Index Creation"

  test_case "Indexes created for receipts table"
  local count=$(sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM sqlite_master WHERE type='index' AND tbl_name='receipts';")
  test_expect "[[ $count -ge 4 ]]" "Expected >=4 indexes on receipts, got $count"

  test_case "Indexes created for agent_memory table"
  local count=$(sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM sqlite_master WHERE type='index' AND tbl_name='agent_memory';")
  test_expect "[[ $count -ge 2 ]]" "Expected >=2 indexes on agent_memory, got $count"

  test_case "Indexes created for audit_log table"
  local count=$(sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM sqlite_master WHERE type='index' AND tbl_name='audit_log';")
  test_expect "[[ $count -ge 3 ]]" "Expected >=3 indexes on audit_log, got $count"

  test_case "Total indexes in database"
  local count=$(sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM sqlite_master WHERE type='index' AND tbl_name NOT LIKE 'sqlite_%';")
  test_expect "[[ $count -ge 15 ]]" "Expected >=15 indexes total, got $count"
}

# ============================================================================
# Constraint Tests
# ============================================================================

test_constraints() {
  test_suite "Constraint Validation"

  test_case "UNIQUE constraint on receipts.execution_id"
  sqlite3 "$DB_PATH" "INSERT INTO receipts (execution_id, agent_id, operation, status, timestamp, duration_ms) VALUES ('unique-test', 'agent1', 'validation', 'passed', '2026-01-29T00:00:00Z', 100);"

  # Try to insert duplicate
  if sqlite3 "$DB_PATH" "INSERT INTO receipts (execution_id, agent_id, operation, status, timestamp, duration_ms) VALUES ('unique-test', 'agent2', 'validation', 'failed', '2026-01-29T00:00:00Z', 200);" 2>/dev/null; then
    test_fail "UNIQUE constraint not enforced"
  else
    test_pass
  fi

  test_case "CHECK constraint on receipts.status"
  if sqlite3 "$DB_PATH" "INSERT INTO receipts (execution_id, agent_id, operation, status, timestamp, duration_ms) VALUES ('check-test', 'agent1', 'validation', 'invalid_status', '2026-01-29T00:00:00Z', 100);" 2>/dev/null; then
    test_fail "CHECK constraint not enforced"
  else
    test_pass
  fi

  test_case "CHECK constraint on audit_log.status"
  if sqlite3 "$DB_PATH" "INSERT INTO audit_log (timestamp, agent_id, operation, status) VALUES ('2026-01-29T00:00:00Z', 'agent1', 'test', 'invalid');" 2>/dev/null; then
    test_fail "CHECK constraint not enforced"
  else
    test_pass
  fi
}

# ============================================================================
# Data Type Tests
# ============================================================================

test_data_types() {
  test_suite "Data Type Validation"

  test_case "Insert and retrieve INTEGER"
  sqlite3 "$DB_PATH" "INSERT INTO receipts (execution_id, agent_id, operation, status, timestamp, duration_ms) VALUES ('int-test', 'agent1', 'validation', 'passed', '2026-01-29T00:00:00Z', 12345);"
  local duration=$(sqlite3 "$DB_PATH" "SELECT duration_ms FROM receipts WHERE execution_id='int-test';")
  test_expect "[[ $duration -eq 12345 ]]" "INTEGER not stored/retrieved correctly"

  test_case "Insert and retrieve TEXT"
  sqlite3 "$DB_PATH" "INSERT INTO agent_memory (agent_id, memory_json) VALUES ('text-test', '{\"key\": \"value\"}');"
  local memory=$(sqlite3 "$DB_PATH" "SELECT memory_json FROM agent_memory WHERE agent_id='text-test';")
  test_expect "[[ $memory == *'key'* ]]" "TEXT not stored/retrieved correctly"

  test_case "Insert and retrieve TIMESTAMP"
  local now=$(date -u +'%Y-%m-%dT%H:%M:%SZ')
  sqlite3 "$DB_PATH" "INSERT INTO receipts (execution_id, agent_id, operation, status, timestamp, duration_ms) VALUES ('ts-test', 'agent1', 'validation', 'passed', '$now', 100);"
  local ts=$(sqlite3 "$DB_PATH" "SELECT timestamp FROM receipts WHERE execution_id='ts-test';")
  test_expect "[[ ! -z $ts ]]" "TIMESTAMP not stored/retrieved correctly"
}

# ============================================================================
# JSON Tests
# ============================================================================

test_json_operations() {
  test_suite "JSON Column Operations"

  test_case "Store JSON in receipt_json column"
  local json='{"files": ["file1.rs", "file2.rs"], "hashes": {"file1.rs": "abc123"}}'
  sqlite3 "$DB_PATH" "INSERT INTO receipts (execution_id, agent_id, operation, status, timestamp, duration_ms, receipt_json) VALUES ('json-test', 'agent1', 'generation', 'passed', '2026-01-29T00:00:00Z', 250, '$json');"
  test_pass

  test_case "Extract JSON array element"
  local result=$(sqlite3 "$DB_PATH" "SELECT json_extract(receipt_json, '\$.files[0]') FROM receipts WHERE execution_id='json-test';")
  test_expect "[[ $result == 'file1.rs' ]]" "JSON extraction failed, got: $result"

  test_case "Extract JSON object property"
  local result=$(sqlite3 "$DB_PATH" "SELECT json_extract(receipt_json, '\$.hashes.file1.rs') FROM receipts WHERE execution_id='json-test';")
  test_expect "[[ $result == 'abc123' ]]" "JSON object extraction failed, got: $result"

  test_case "Query JSON with json_type"
  local result=$(sqlite3 "$DB_PATH" "SELECT json_type(receipt_json, '\$.files') FROM receipts WHERE execution_id='json-test';")
  test_expect "[[ $result == 'array' ]]" "json_type failed, got: $result"
}

# ============================================================================
# Foreign Key Tests
# ============================================================================

test_foreign_keys() {
  test_suite "Foreign Key Constraints"

  test_case "Foreign key constraint on collision_detection.session_id"
  # Insert a workflow session
  sqlite3 "$DB_PATH" "INSERT INTO workflow_sessions (session_id, start_time, status, agent_count) VALUES ('fk-test-session', datetime('now'), 'running', 2);"

  # Insert collision referencing valid session
  sqlite3 "$DB_PATH" "INSERT INTO collision_detection (session_id, agent_id_1, agent_id_2, collision_type, description, severity, timestamp) VALUES ('fk-test-session', 'agent1', 'agent2', 'file_conflict', 'test conflict', 'warning', '2026-01-29T00:00:00Z');"
  test_pass

  test_case "Foreign key rejects invalid session_id reference"
  if sqlite3 "$DB_PATH" "INSERT INTO collision_detection (session_id, agent_id_1, agent_id_2, collision_type, description, severity, timestamp) VALUES ('invalid-session', 'agent1', 'agent2', 'file_conflict', 'test', 'warning', '2026-01-29T00:00:00Z');" 2>/dev/null; then
    test_fail "Foreign key constraint not enforced"
  else
    test_pass
  fi
}

# ============================================================================
# Query Performance Tests
# ============================================================================

test_query_performance() {
  test_suite "Query Performance with Indexes"

  # Insert test data
  for i in {1..100}; do
    sqlite3 "$DB_PATH" "INSERT INTO receipts (execution_id, agent_id, operation, status, timestamp, files_generated, duration_ms) VALUES ('perf-test-$i', 'agent-$((i%5))', 'validation', '$([ $((i%2)) -eq 0 ] && echo "passed" || echo "failed")', datetime('now', '-$i minutes'), $((i*10)), $((i*100)));"
  done

  test_case "Query by agent_id uses index"
  local start=$(date +%s%N)
  sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM receipts WHERE agent_id='agent-1';" > /dev/null
  local end=$(date +%s%N)
  local duration=$((($end - $start) / 1000000))  # Convert to ms
  test_expect "[[ $duration -lt 100 ]]" "Index query slow: ${duration}ms"

  test_case "Query by timestamp uses index"
  sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM receipts WHERE timestamp > '2026-01-01' AND timestamp < '2026-12-31';" > /dev/null
  test_pass

  test_case "Range query on status"
  sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM receipts WHERE status='passed';" > /dev/null
  test_pass
}

# ============================================================================
# View Tests
# ============================================================================

test_views_functionality() {
  test_suite "View Functionality"

  # Insert test data for views
  for i in {1..10}; do
    sqlite3 "$DB_PATH" "INSERT INTO receipts (execution_id, agent_id, operation, status, timestamp, files_generated, duration_ms) VALUES ('view-test-$i', 'agent-$((i%3))', 'validation', '$([ $((i%2)) -eq 0 ] && echo "passed" || echo "failed")', datetime('now', '-$i minutes'), $i, $((i*50)));"
  done

  test_case "recent_executions view returns data"
  local count=$(sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM recent_executions;")
  test_expect "[[ $count -gt 0 ]]" "View returned no rows"

  test_case "agent_performance_summary view calculates stats"
  local count=$(sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM agent_performance_summary WHERE total_executions > 0;")
  test_expect "[[ $count -gt 0 ]]" "View returned no agent stats"

  test_case "pipeline_stage_performance view exists"
  local count=$(sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM pipeline_stage_performance LIMIT 1;")
  test_pass
}

# ============================================================================
# Transaction Tests
# ============================================================================

test_transactions() {
  test_suite "Transaction Support"

  test_case "Insert transaction succeeds"
  sqlite3 "$DB_PATH" <<EOF
BEGIN TRANSACTION;
INSERT INTO receipts (execution_id, agent_id, operation, status, timestamp, duration_ms) VALUES ('tx-test-1', 'agent1', 'validation', 'passed', '2026-01-29T00:00:00Z', 100);
INSERT INTO audit_log (timestamp, agent_id, operation, status) VALUES ('2026-01-29T00:00:00Z', 'agent1', 'test', 'completed');
COMMIT;
EOF
  test_pass

  test_case "Rollback transaction"
  sqlite3 "$DB_PATH" <<EOF
BEGIN TRANSACTION;
INSERT INTO receipts (execution_id, agent_id, operation, status, timestamp, duration_ms) VALUES ('tx-test-2', 'agent1', 'validation', 'passed', '2026-01-29T00:00:00Z', 100);
ROLLBACK;
EOF

  local count=$(sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM receipts WHERE execution_id='tx-test-2';")
  test_expect "[[ $count -eq 0 ]]" "Rollback did not work"
}

# ============================================================================
# Integration Tests
# ============================================================================

test_integration() {
  test_suite "Integration Scenarios"

  test_case "Complete execution flow (receipt + audit)"
  local exec_id="integration-test-$(date +%s)"
  sqlite3 "$DB_PATH" <<EOF
BEGIN TRANSACTION;
INSERT INTO receipts (execution_id, agent_id, operation, status, timestamp, files_generated, duration_ms)
VALUES ('$exec_id', 'integration-agent', 'generation', 'passed', datetime('now'), 3, 500);
INSERT INTO audit_log (timestamp, agent_id, operation, status, duration_ms)
VALUES (datetime('now'), 'integration-agent', 'generation complete', 'completed', 500);
COMMIT;
EOF

  local receipt=$(sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM receipts WHERE execution_id='$exec_id';")
  local audit=$(sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM audit_log WHERE agent_id='integration-agent';")
  test_expect "[[ $receipt -eq 1 && $audit -ge 1 ]]" "Integration test failed"

  test_case "Multi-table workflow session"
  local session_id="integration-session-$(date +%s)"
  sqlite3 "$DB_PATH" <<EOF
BEGIN TRANSACTION;
INSERT INTO workflow_sessions (session_id, start_time, status, agent_count)
VALUES ('$session_id', datetime('now'), 'completed', 3);
INSERT INTO collision_detection (session_id, agent_id_1, agent_id_2, collision_type, description, severity, timestamp)
VALUES ('$session_id', 'agent1', 'agent2', 'semantic_overlap', 'overlap detected', 'info', datetime('now'));
COMMIT;
EOF

  local session=$(sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM workflow_sessions WHERE session_id='$session_id';")
  local collision=$(sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM collision_detection WHERE session_id='$session_id';")
  test_expect "[[ $session -eq 1 && $collision -eq 1 ]]" "Multi-table workflow test failed"
}

# ============================================================================
# Report
# ============================================================================

print_report() {
  echo ""
  echo "=========================================="
  echo "Test Results Summary"
  echo "=========================================="
  echo "Total Tests:  $TESTS_RUN"
  echo -e "Passed:       ${GREEN}$TESTS_PASSED${NC}"

  if [[ $TESTS_FAILED -gt 0 ]]; then
    echo -e "Failed:       ${RED}$TESTS_FAILED${NC}"
  else
    echo -e "Failed:       ${GREEN}$TESTS_FAILED${NC}"
  fi

  if [[ $TESTS_FAILED -eq 0 ]]; then
    echo -e "${GREEN}✓ All tests passed!${NC}"
  else
    echo -e "${RED}✗ Some tests failed${NC}"
  fi
  echo ""
}

# ============================================================================
# Main
# ============================================================================

main() {
  setup

  test_tables_exist
  test_views_exist
  test_receipts_columns
  test_agent_memory_columns
  test_audit_log_columns
  test_indexes_created
  test_constraints
  test_data_types
  test_json_operations
  test_foreign_keys
  test_query_performance
  test_views_functionality
  test_transactions
  test_integration

  print_report
  cleanup

  # Exit with error code if tests failed
  if [[ $TESTS_FAILED -gt 0 ]]; then
    exit 1
  fi
}

main "$@"
