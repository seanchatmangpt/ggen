#!/bin/bash

##############################################################################
# SQLite Persistence Layer for ggen Receipts and Agent Memory
#
# Provides production-ready database operations for:
# - Receipt storage and retrieval
# - Agent memory persistence
# - Audit trail logging
# - Data export and analytics
#
# Version: 1.0.0
# Production-ready with error handling, retry logic, and schema migration
##############################################################################

set -euo pipefail

# Database configuration
readonly DB_DIR="${SCRIPT_DIR:-.}/workspace/.ggen"
readonly DB_PATH="${DB_DIR}/ggen.db"
readonly MAX_RETRIES=5
readonly RETRY_DELAY=100  # milliseconds
readonly DB_TIMEOUT=30000  # milliseconds

# Color codes
readonly GREEN='\033[0;32m'
readonly RED='\033[0;31m'
readonly YELLOW='\033[1;33m'
readonly NC='\033[0m'

##############################################################################
# Logging Functions
##############################################################################

db_log_info() {
    echo -e "${GREEN}[DB]${NC} $(date '+%Y-%m-%d %H:%M:%S') $1"
}

db_log_error() {
    echo -e "${RED}[DB ERROR]${NC} $(date '+%Y-%m-%d %H:%M:%S') $1" >&2
}

db_log_warn() {
    echo -e "${YELLOW}[DB WARN]${NC} $(date '+%Y-%m-%d %H:%M:%S') $1"
}

##############################################################################
# Schema Definition Functions
##############################################################################

# Define the complete schema for all persistence tables
define_schema() {
    cat <<'EOF'
-- Receipts table: stores execution receipts with metadata
CREATE TABLE IF NOT EXISTS receipts (
    receipt_id TEXT PRIMARY KEY,
    execution_id TEXT NOT NULL UNIQUE,
    agent_id TEXT NOT NULL,
    agent_type TEXT NOT NULL,
    receipt_json TEXT NOT NULL,
    status TEXT NOT NULL CHECK(status IN ('success', 'error', 'partial')),
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    duration_ms INTEGER,
    INDEX idx_agent_id (agent_id),
    INDEX idx_execution_id (execution_id),
    INDEX idx_status (status),
    INDEX idx_created_at (created_at)
);

-- Agent memory table: stores persistent agent state and context
CREATE TABLE IF NOT EXISTS agent_memory (
    memory_id TEXT PRIMARY KEY,
    agent_id TEXT NOT NULL UNIQUE,
    agent_type TEXT NOT NULL,
    memory_json TEXT NOT NULL,
    last_updated DATETIME DEFAULT CURRENT_TIMESTAMP,
    version INTEGER DEFAULT 1,
    INDEX idx_agent_id (agent_id),
    INDEX idx_agent_type (agent_type),
    INDEX idx_last_updated (last_updated)
);

-- Audit log table: detailed operation tracking for compliance
CREATE TABLE IF NOT EXISTS audit_log (
    audit_id TEXT PRIMARY KEY,
    timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
    agent_id TEXT NOT NULL,
    operation TEXT NOT NULL,
    operation_type TEXT,
    status TEXT NOT NULL CHECK(status IN ('started', 'completed', 'failed', 'retry')),
    duration_ms INTEGER,
    details TEXT,
    error_message TEXT,
    INDEX idx_agent_id (agent_id),
    INDEX idx_timestamp (timestamp),
    INDEX idx_operation (operation),
    INDEX idx_status (status)
);

-- Metadata table: schema version and migration tracking
CREATE TABLE IF NOT EXISTS _schema_metadata (
    key TEXT PRIMARY KEY,
    value TEXT NOT NULL,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

-- Insert initial schema version
INSERT OR REPLACE INTO _schema_metadata (key, value) VALUES ('schema_version', '1.0.0');
INSERT OR REPLACE INTO _schema_metadata (key, value) VALUES ('last_migration', CURRENT_TIMESTAMP);
EOF
}

##############################################################################
# Database Initialization and Migration
##############################################################################

# Initialize database with schema if needed
db_init() {
    local retry_count=0

    db_log_info "Initializing database at ${DB_PATH}"

    # Create directory structure
    mkdir -p "${DB_DIR}"

    # Check if database exists
    if [[ -f "${DB_PATH}" ]]; then
        db_log_info "Database exists, verifying schema..."
        db_verify_schema
        return 0
    fi

    # Create new database with schema
    while [[ ${retry_count} -lt ${MAX_RETRIES} ]]; do
        if sqlite3 "${DB_PATH}" "$(define_schema)" 2>/dev/null; then
            db_log_info "Database initialized successfully"
            chmod 0600 "${DB_PATH}"
            return 0
        else
            retry_count=$((retry_count + 1))
            if [[ ${retry_count} -lt ${MAX_RETRIES} ]]; then
                db_log_warn "Database initialization attempt ${retry_count}/${MAX_RETRIES} failed, retrying..."
                sleep 0.$(printf "%03d" ${RETRY_DELAY})
            fi
        fi
    done

    db_log_error "Failed to initialize database after ${MAX_RETRIES} attempts"
    return 1
}

# Verify schema is correct
db_verify_schema() {
    local table_count
    table_count=$(sqlite3 "${DB_PATH}" \
        "SELECT COUNT(*) FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%';" 2>/dev/null || echo 0)

    if [[ ${table_count} -eq 0 ]]; then
        db_log_warn "No tables found, reinitializing schema..."
        sqlite3 "${DB_PATH}" "$(define_schema)" 2>/dev/null || return 1
    fi

    return 0
}

##############################################################################
# Receipt Persistence Functions
##############################################################################

# Save receipt to database
db_save_receipt() {
    local execution_id="$1"
    local agent_id="$2"
    local agent_type="$3"
    local receipt_json="$4"
    local status="${5:-success}"
    local duration_ms="${6:-0}"
    local retry_count=0

    if [[ -z "${execution_id}" ]] || [[ -z "${agent_id}" ]]; then
        db_log_error "Missing required parameters for save_receipt"
        return 1
    fi

    db_init || return 1

    local receipt_id
    receipt_id=$(printf '%s-%s-%d' "${agent_id}" "${execution_id}" "$(date +%s%N)" | sha256sum | cut -c1-16)

    # Validate JSON
    if ! echo "${receipt_json}" | jq empty 2>/dev/null; then
        db_log_error "Invalid JSON in receipt"
        return 1
    fi

    local sql
    sql="INSERT INTO receipts (receipt_id, execution_id, agent_id, agent_type, receipt_json, status, duration_ms)
         VALUES ('${receipt_id}', '${execution_id}', '${agent_id}', '${agent_type}', json('${receipt_json//\'/\'\'}'), '${status}', ${duration_ms})"

    while [[ ${retry_count} -lt ${MAX_RETRIES} ]]; do
        if sqlite3 "${DB_PATH}" "${sql}" 2>/dev/null; then
            db_log_info "Receipt saved: ${receipt_id} (${execution_id})"
            return 0
        else
            retry_count=$((retry_count + 1))
            if [[ ${retry_count} -lt ${MAX_RETRIES} ]]; then
                db_log_warn "Save receipt attempt ${retry_count}/${MAX_RETRIES} failed, retrying..."
                sleep 0.$(printf "%03d" ${RETRY_DELAY})
            fi
        fi
    done

    db_log_error "Failed to save receipt after ${MAX_RETRIES} attempts"
    return 1
}

# Query receipts by agent_id
db_query_receipts() {
    local agent_id="$1"
    local format="${2:-json}"
    local limit="${3:-100}"

    db_init || return 1

    local sql
    sql="SELECT receipt_id, execution_id, agent_id, agent_type, status, created_at, duration_ms
         FROM receipts WHERE agent_id LIKE '${agent_id}' ORDER BY created_at DESC LIMIT ${limit}"

    case "${format}" in
        json)
            sqlite3 -json "${DB_PATH}" "${sql}" 2>/dev/null || return 1
            ;;
        csv)
            sqlite3 -csv "${DB_PATH}" "${sql}" 2>/dev/null || return 1
            ;;
        *)
            db_log_error "Unknown format: ${format}"
            return 1
            ;;
    esac
}

# Get full receipt details by receipt_id
db_get_receipt() {
    local receipt_id="$1"

    db_init || return 1

    local sql
    sql="SELECT receipt_json FROM receipts WHERE receipt_id = '${receipt_id}'"

    sqlite3 "${DB_PATH}" "${sql}" 2>/dev/null || return 1
}

##############################################################################
# Agent Memory Persistence Functions
##############################################################################

# Save or update agent memory
db_save_memory() {
    local agent_id="$1"
    local agent_type="$2"
    local memory_json="$3"
    local retry_count=0

    if [[ -z "${agent_id}" ]] || [[ -z "${memory_json}" ]]; then
        db_log_error "Missing required parameters for save_memory"
        return 1
    fi

    db_init || return 1

    # Validate JSON
    if ! echo "${memory_json}" | jq empty 2>/dev/null; then
        db_log_error "Invalid JSON in memory"
        return 1
    fi

    local memory_id
    memory_id=$(printf '%s-%d' "${agent_id}" "$(date +%s%N)" | sha256sum | cut -c1-16)

    # Check if memory exists for agent
    local existing_id
    existing_id=$(sqlite3 "${DB_PATH}" \
        "SELECT memory_id FROM agent_memory WHERE agent_id = '${agent_id}'" 2>/dev/null || echo "")

    local sql
    if [[ -z "${existing_id}" ]]; then
        # INSERT new memory
        sql="INSERT INTO agent_memory (memory_id, agent_id, agent_type, memory_json)
             VALUES ('${memory_id}', '${agent_id}', '${agent_type}', json('${memory_json//\'/\'\'}'))"
    else
        # UPDATE existing memory
        sql="UPDATE agent_memory SET memory_json = json('${memory_json//\'/\'\'}'),
             last_updated = CURRENT_TIMESTAMP, version = version + 1
             WHERE agent_id = '${agent_id}'"
    fi

    while [[ ${retry_count} -lt ${MAX_RETRIES} ]]; do
        if sqlite3 "${DB_PATH}" "${sql}" 2>/dev/null; then
            db_log_info "Agent memory saved: ${agent_id}"
            return 0
        else
            retry_count=$((retry_count + 1))
            if [[ ${retry_count} -lt ${MAX_RETRIES} ]]; then
                db_log_warn "Save memory attempt ${retry_count}/${MAX_RETRIES} failed, retrying..."
                sleep 0.$(printf "%03d" ${RETRY_DELAY})
            fi
        fi
    done

    db_log_error "Failed to save memory after ${MAX_RETRIES} attempts"
    return 1
}

# Retrieve agent memory
db_get_memory() {
    local agent_id="$1"

    db_init || return 1

    local sql
    sql="SELECT memory_json FROM agent_memory WHERE agent_id = '${agent_id}'"

    sqlite3 "${DB_PATH}" "${sql}" 2>/dev/null || return 1
}

# Query all agent memory with optional filtering
db_query_memory() {
    local agent_pattern="${1:-%}"
    local format="${2:-json}"

    db_init || return 1

    local sql
    sql="SELECT agent_id, agent_type, version, last_updated
         FROM agent_memory WHERE agent_id LIKE '${agent_pattern}' ORDER BY last_updated DESC"

    case "${format}" in
        json)
            sqlite3 -json "${DB_PATH}" "${sql}" 2>/dev/null || return 1
            ;;
        csv)
            sqlite3 -csv "${DB_PATH}" "${sql}" 2>/dev/null || return 1
            ;;
        *)
            db_log_error "Unknown format: ${format}"
            return 1
            ;;
    esac
}

##############################################################################
# Audit Log Functions
##############################################################################

# Log audit event
db_log_audit() {
    local agent_id="$1"
    local operation="$2"
    local status="$3"
    local operation_type="${4:-other}"
    local duration_ms="${5:-0}"
    local details="${6:-}"
    local error_message="${7:-}"
    local retry_count=0

    if [[ -z "${agent_id}" ]] || [[ -z "${operation}" ]]; then
        db_log_error "Missing required parameters for log_audit"
        return 1
    fi

    db_init || return 1

    local audit_id
    audit_id=$(printf '%s-%s-%d' "${agent_id}" "${operation}" "$(date +%s%N)" | sha256sum | cut -c1-16)

    local sql
    sql="INSERT INTO audit_log (audit_id, agent_id, operation, operation_type, status, duration_ms, details, error_message)
         VALUES ('${audit_id}', '${agent_id}', '${operation}', '${operation_type}', '${status}', ${duration_ms},
         '${details//\'/\'\'}', '${error_message//\'/\'\'}')"

    while [[ ${retry_count} -lt ${MAX_RETRIES} ]]; do
        if sqlite3 "${DB_PATH}" "${sql}" 2>/dev/null; then
            return 0
        else
            retry_count=$((retry_count + 1))
            if [[ ${retry_count} -lt ${MAX_RETRIES} ]]; then
                sleep 0.$(printf "%03d" ${RETRY_DELAY})
            fi
        fi
    done

    db_log_error "Failed to log audit after ${MAX_RETRIES} attempts"
    return 1
}

# Query audit trail
db_query_audit_trail() {
    local agent_pattern="${1:-%}"
    local days_back="${2:-7}"
    local limit="${3:-1000}"
    local format="${4:-json}"

    db_init || return 1

    local sql
    sql="SELECT audit_id, timestamp, agent_id, operation, operation_type, status, duration_ms
         FROM audit_log
         WHERE agent_id LIKE '${agent_pattern}'
         AND timestamp > datetime('now', '-${days_back} days')
         ORDER BY timestamp DESC
         LIMIT ${limit}"

    case "${format}" in
        json)
            sqlite3 -json "${DB_PATH}" "${sql}" 2>/dev/null || return 1
            ;;
        csv)
            sqlite3 -csv "${DB_PATH}" "${sql}" 2>/dev/null || return 1
            ;;
        *)
            db_log_error "Unknown format: ${format}"
            return 1
            ;;
    esac
}

##############################################################################
# Data Export Functions
##############################################################################

# Export audit trail to file
db_export_audit_trail() {
    local output_file="${1:-./audit-export-$(date +%Y%m%d-%H%M%S).json}"
    local format="${2:-json}"

    db_init || return 1

    local sql
    sql="SELECT * FROM audit_log ORDER BY timestamp DESC"

    case "${format}" in
        json)
            if sqlite3 -json "${DB_PATH}" "${sql}" > "${output_file}" 2>/dev/null; then
                db_log_info "Audit trail exported to ${output_file}"
                return 0
            fi
            ;;
        csv)
            if sqlite3 -csv "${DB_PATH}" "${sql}" > "${output_file}" 2>/dev/null; then
                db_log_info "Audit trail exported to ${output_file}"
                return 0
            fi
            ;;
        *)
            db_log_error "Unknown format: ${format}"
            return 1
            ;;
    esac

    db_log_error "Failed to export audit trail"
    return 1
}

# Export all data for backup
db_export_full_backup() {
    local output_dir="${1:-./.ggen-backup}"
    mkdir -p "${output_dir}"

    db_init || return 1

    db_log_info "Exporting full database backup to ${output_dir}"

    # Export receipts
    sqlite3 -json "${DB_PATH}" "SELECT * FROM receipts" > "${output_dir}/receipts.json" 2>/dev/null || return 1

    # Export agent memory
    sqlite3 -json "${DB_PATH}" "SELECT * FROM agent_memory" > "${output_dir}/agent_memory.json" 2>/dev/null || return 1

    # Export audit log
    sqlite3 -json "${DB_PATH}" "SELECT * FROM audit_log" > "${output_dir}/audit_log.json" 2>/dev/null || return 1

    db_log_info "Backup completed: ${output_dir}/"
    return 0
}

##############################################################################
# Analytics Functions
##############################################################################

# Get analytics for agents
db_analytics() {
    local agent_pattern="${1:-%}"
    local time_period="${2:-last-7-days}"

    db_init || return 1

    # Parse time period
    local days_back=7
    case "${time_period}" in
        last-24-hours) days_back=1 ;;
        last-7-days) days_back=7 ;;
        last-30-days) days_back=30 ;;
        last-90-days) days_back=90 ;;
        *) days_back=7 ;;
    esac

    local json_output
    json_output=$(cat <<EOF
{
  "period": "${time_period}",
  "generated_at": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "agent_metrics": $(sqlite3 -json "${DB_PATH}" "
    SELECT
      agent_id,
      COUNT(*) as total_operations,
      SUM(CASE WHEN status = 'completed' THEN 1 ELSE 0 END) as successful_operations,
      SUM(CASE WHEN status = 'failed' THEN 1 ELSE 0 END) as failed_operations,
      ROUND(AVG(duration_ms), 2) as avg_duration_ms,
      MIN(duration_ms) as min_duration_ms,
      MAX(duration_ms) as max_duration_ms
    FROM audit_log
    WHERE agent_id LIKE '${agent_pattern}' AND timestamp > datetime('now', '-${days_back} days')
    GROUP BY agent_id
    ORDER BY total_operations DESC
  "),
  "receipt_statistics": $(sqlite3 -json "${DB_PATH}" "
    SELECT
      status,
      COUNT(*) as count,
      ROUND(AVG(duration_ms), 2) as avg_duration_ms
    FROM receipts
    WHERE agent_id LIKE '${agent_pattern}' AND created_at > datetime('now', '-${days_back} days')
    GROUP BY status
  ")
}
EOF
    )

    echo "${json_output}"
}

# Get summary statistics
db_stats() {
    db_init || return 1

    local stats
    stats=$(cat <<EOF
{
  "receipts_total": $(sqlite3 "${DB_PATH}" "SELECT COUNT(*) FROM receipts"),
  "receipts_success": $(sqlite3 "${DB_PATH}" "SELECT COUNT(*) FROM receipts WHERE status = 'success'"),
  "receipts_error": $(sqlite3 "${DB_PATH}" "SELECT COUNT(*) FROM receipts WHERE status = 'error'"),
  "agents_tracked": $(sqlite3 "${DB_PATH}" "SELECT COUNT(DISTINCT agent_id) FROM agent_memory"),
  "audit_entries": $(sqlite3 "${DB_PATH}" "SELECT COUNT(*) FROM audit_log"),
  "database_size_kb": $(du -k "${DB_PATH}" | cut -f1),
  "last_receipt": "$(sqlite3 "${DB_PATH}" "SELECT created_at FROM receipts ORDER BY created_at DESC LIMIT 1")"
}
EOF
    )

    echo "${stats}"
}

##############################################################################
# Cleanup and Maintenance
##############################################################################

# Cleanup old data
db_cleanup() {
    local days_to_keep="${1:-90}"

    db_init || return 1

    db_log_info "Cleaning up data older than ${days_to_keep} days..."

    # Archive old receipts
    local archived_receipts
    archived_receipts=$(sqlite3 "${DB_PATH}" \
        "SELECT COUNT(*) FROM receipts WHERE created_at < datetime('now', '-${days_to_keep} days')")

    if [[ ${archived_receipts} -gt 0 ]]; then
        sqlite3 "${DB_PATH}" \
            "DELETE FROM receipts WHERE created_at < datetime('now', '-${days_to_keep} days')" || return 1
        db_log_info "Deleted ${archived_receipts} old receipts"
    fi

    # Archive old audit logs
    local archived_audit
    archived_audit=$(sqlite3 "${DB_PATH}" \
        "SELECT COUNT(*) FROM audit_log WHERE timestamp < datetime('now', '-${days_to_keep} days')")

    if [[ ${archived_audit} -gt 0 ]]; then
        sqlite3 "${DB_PATH}" \
            "DELETE FROM audit_log WHERE timestamp < datetime('now', '-${days_to_keep} days')" || return 1
        db_log_info "Deleted ${archived_audit} old audit entries"
    fi

    # Optimize database
    sqlite3 "${DB_PATH}" "VACUUM" || return 1
    db_log_info "Database optimized"

    return 0
}

# Reset database completely
db_reset() {
    if [[ -f "${DB_PATH}" ]]; then
        rm -f "${DB_PATH}"
        db_log_info "Database file deleted"
    fi

    db_init || return 1
    return 0
}

##############################################################################
# Export functions for external use
##############################################################################

export -f db_init
export -f db_save_receipt
export -f db_query_receipts
export -f db_get_receipt
export -f db_save_memory
export -f db_get_memory
export -f db_query_memory
export -f db_log_audit
export -f db_query_audit_trail
export -f db_export_audit_trail
export -f db_export_full_backup
export -f db_analytics
export -f db_stats
export -f db_cleanup
export -f db_reset
