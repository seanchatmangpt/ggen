#!/bin/bash

# ============================================================================
# SQLite Database Initialization Script
# ============================================================================
# Purpose: Initialize Tier 2 persistence database with schema
# Usage: ./init-db.sh [database_path]
# ============================================================================

set -euo pipefail

# Configuration
DB_PATH="${1:-.ggen/tier2.db}"
SCHEMA_FILE="$(dirname "${BASH_SOURCE[0]}")/schema.sql"
LOG_FILE="${DB_PATH%.db}.log"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# ============================================================================
# Functions
# ============================================================================

log() {
  local level=$1
  shift
  local message="$@"
  local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
  echo "[${timestamp}] [${level}] ${message}" | tee -a "${LOG_FILE}"
}

error() {
  echo -e "${RED}ERROR: $@${NC}" >&2
  log "ERROR" "$@"
  exit 1
}

success() {
  echo -e "${GREEN}✓ $@${NC}"
  log "INFO" "✓ $@"
}

info() {
  echo -e "${BLUE}ℹ $@${NC}"
  log "INFO" "ℹ $@"
}

warning() {
  echo -e "${YELLOW}⚠ $@${NC}"
  log "WARN" "⚠ $@"
}

# ============================================================================
# Verification Functions
# ============================================================================

verify_sqlite() {
  if ! command -v sqlite3 &> /dev/null; then
    error "sqlite3 not found. Please install sqlite3."
  fi
  success "sqlite3 command available"
}

verify_schema_file() {
  if [[ ! -f "$SCHEMA_FILE" ]]; then
    error "Schema file not found: $SCHEMA_FILE"
  fi
  success "Schema file found: $SCHEMA_FILE"
}

verify_directories() {
  local db_dir=$(dirname "$DB_PATH")
  if [[ ! -d "$db_dir" ]]; then
    info "Creating database directory: $db_dir"
    mkdir -p "$db_dir"
  fi
  success "Database directory ready: $db_dir"
}

# ============================================================================
# Database Initialization
# ============================================================================

initialize_database() {
  info "Initializing database: $DB_PATH"

  # Remove existing database if it exists (fresh start)
  if [[ -f "$DB_PATH" ]]; then
    warning "Database already exists: $DB_PATH (will be replaced)"
    rm -f "$DB_PATH"
  fi

  # Load schema
  info "Loading schema from $SCHEMA_FILE"
  sqlite3 "$DB_PATH" < "$SCHEMA_FILE" 2>&1 | tee -a "${LOG_FILE}"

  success "Database initialized: $DB_PATH"
}

# ============================================================================
# Validation Functions
# ============================================================================

validate_tables() {
  info "Validating tables..."

  local tables=(
    "receipts"
    "agent_memory"
    "audit_log"
    "workflow_sessions"
    "collision_detection"
    "pipeline_metrics"
    "slo_violations"
  )

  for table in "${tables[@]}"; do
    local count=$(sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM sqlite_master WHERE type='table' AND name='$table';")
    if [[ $count -eq 1 ]]; then
      success "Table exists: $table"
    else
      error "Table missing: $table"
    fi
  done
}

validate_indexes() {
  info "Validating indexes..."

  local count=$(sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM sqlite_master WHERE type='index' AND tbl_name NOT LIKE 'sqlite_%';")
  if [[ $count -ge 15 ]]; then
    success "Indexes created: $count"
  else
    warning "Expected 15+ indexes, found: $count"
  fi
}

validate_views() {
  info "Validating views..."

  local views=(
    "recent_executions"
    "agent_performance_summary"
    "pipeline_stage_performance"
  )

  for view in "${views[@]}"; do
    local count=$(sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM sqlite_master WHERE type='view' AND name='$view';")
    if [[ $count -eq 1 ]]; then
      success "View exists: $view"
    else
      error "View missing: $view"
    fi
  done
}

validate_foreign_keys() {
  info "Validating foreign key constraints..."

  # Check if foreign keys are enabled
  local fk_enabled=$(sqlite3 "$DB_PATH" "PRAGMA foreign_keys;")
  if [[ "$fk_enabled" -eq 1 ]]; then
    success "Foreign keys enabled"
  else
    warning "Foreign keys not enabled (will be enabled on connection)"
  fi
}

# ============================================================================
# Test Operations
# ============================================================================

test_insert_receipt() {
  info "Testing receipt insertion..."

  local execution_id="test-$(date +%s)"
  local timestamp=$(date -u +'%Y-%m-%dT%H:%M:%SZ')

  sqlite3 "$DB_PATH" <<EOF
INSERT INTO receipts (
  execution_id, agent_id, operation, status, timestamp,
  files_generated, duration_ms
) VALUES (
  '$execution_id',
  'test-agent',
  'validation',
  'passed',
  '$timestamp',
  0,
  100
);
EOF

  success "Receipt insertion test passed: $execution_id"
}

test_insert_audit_log() {
  info "Testing audit log insertion..."

  local timestamp=$(date -u +'%Y-%m-%dT%H:%M:%SZ')

  sqlite3 "$DB_PATH" <<EOF
INSERT INTO audit_log (
  timestamp, agent_id, operation, status, duration_ms
) VALUES (
  '$timestamp',
  'test-agent',
  'test operation',
  'completed',
  50
);
EOF

  success "Audit log insertion test passed"
}

test_insert_agent_memory() {
  info "Testing agent memory insertion..."

  sqlite3 "$DB_PATH" <<EOF
INSERT INTO agent_memory (
  agent_id, memory_json
) VALUES (
  'test-agent',
  '{"state": "initialized", "version": 1}'
);
EOF

  success "Agent memory insertion test passed"
}

test_insert_workflow_session() {
  info "Testing workflow session insertion..."

  local session_id="session-$(date +%s)"

  sqlite3 "$DB_PATH" <<EOF
INSERT INTO workflow_sessions (
  session_id, start_time, status, agent_count
) VALUES (
  '$session_id',
  datetime('now'),
  'running',
  2
);
EOF

  success "Workflow session insertion test passed: $session_id"
}

test_json_queries() {
  info "Testing JSON column queries..."

  # Insert a receipt with JSON data
  local execution_id="json-test-$(date +%s)"
  local timestamp=$(date -u +'%Y-%m-%dT%H:%M:%SZ')

  sqlite3 "$DB_PATH" <<EOF
INSERT INTO receipts (
  execution_id, agent_id, operation, status, timestamp,
  files_generated, duration_ms, receipt_json
) VALUES (
  '$execution_id',
  'test-agent-json',
  'generation',
  'passed',
  '$timestamp',
  5,
  250,
  '{"files": ["file1.rs", "file2.rs"], "hashes": {"file1.rs": "abc123"}}'
);
EOF

  # Query JSON data
  local result=$(sqlite3 "$DB_PATH" "SELECT json_extract(receipt_json, '$.files[0]') FROM receipts WHERE execution_id='$execution_id';")
  if [[ "$result" == "file1.rs" ]]; then
    success "JSON query test passed"
  else
    error "JSON query test failed: got '$result'"
  fi
}

# ============================================================================
# Report Functions
# ============================================================================

print_schema_info() {
  echo ""
  info "Database Schema Information"
  echo ""

  echo "Tables:"
  sqlite3 "$DB_PATH" "SELECT name FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%' ORDER BY name;" | sed 's/^/  ✓ /'

  echo ""
  echo "Indexes:"
  sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM sqlite_master WHERE type='index' AND tbl_name NOT LIKE 'sqlite_%';" | sed 's/^/  Total: /'

  echo ""
  echo "Views:"
  sqlite3 "$DB_PATH" "SELECT name FROM sqlite_master WHERE type='view' ORDER BY name;" | sed 's/^/  ✓ /'

  echo ""
}

print_statistics() {
  echo ""
  info "Database Statistics"
  echo ""

  echo "Receipts:"
  sqlite3 "$DB_PATH" "SELECT COUNT(*) as total FROM receipts;" | sed 's/^/  Total: /'

  echo ""
  echo "Audit Logs:"
  sqlite3 "$DB_PATH" "SELECT COUNT(*) as total FROM audit_log;" | sed 's/^/  Total: /'

  echo ""
}

# ============================================================================
# Main Execution
# ============================================================================

main() {
  echo ""
  echo "=========================================="
  echo "SQLite Database Initialization"
  echo "=========================================="
  echo ""

  # Pre-flight checks
  verify_sqlite
  verify_schema_file
  verify_directories

  # Initialize database
  initialize_database

  # Validate schema
  validate_tables
  validate_indexes
  validate_views
  validate_foreign_keys

  # Run tests
  test_insert_receipt
  test_insert_audit_log
  test_insert_agent_memory
  test_insert_workflow_session
  test_json_queries

  # Print information
  print_schema_info
  print_statistics

  echo ""
  success "Database initialization completed successfully!"
  echo ""
  echo "Database path: $DB_PATH"
  echo "Log file: $LOG_FILE"
  echo ""
}

# Run main function
main "$@"
