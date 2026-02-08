#!/bin/bash

##############################################################################
# Database CLI Extension for ggen Web Simulator
#
# Provides command-line interface for database operations:
# - db-query-receipts: Query receipt data by agent
# - db-query-memory: Query agent memory state
# - db-export-audit-trail: Export audit logs
# - db-analytics: Generate analytics reports
# - db-stats: Display database statistics
#
# Version: 1.0.0
##############################################################################

set -euo pipefail

# Source the persistence module
PERSISTENCE_MODULE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/persistence.sh"
if [[ ! -f "${PERSISTENCE_MODULE}" ]]; then
    echo "Error: persistence.sh not found at ${PERSISTENCE_MODULE}"
    exit 1
fi

source "${PERSISTENCE_MODULE}"

# Color codes (use existing if already defined)
if [[ -z "${GREEN:-}" ]]; then
    readonly GREEN='\033[0;32m'
    readonly BLUE='\033[0;34m'
    readonly YELLOW='\033[1;33m'
    readonly NC='\033[0m'
fi

##############################################################################
# Database Query Commands
##############################################################################

# Query receipts by agent pattern
cmd_db_query_receipts() {
    local agent_pattern="${1:-%}"
    local format="${2:-json}"
    local limit="${3:-100}"

    echo -e "${BLUE}[DB QUERY]${NC} Receipts for agent pattern: ${agent_pattern}"

    db_init || return 1

    if ! db_query_receipts "${agent_pattern}" "${format}" "${limit}"; then
        echo -e "${YELLOW}[WARN]${NC} No receipts found for pattern: ${agent_pattern}"
        return 1
    fi

    return 0
}

# Query agent memory by agent pattern
cmd_db_query_memory() {
    local agent_pattern="${1:-%}"
    local format="${2:-json}"

    echo -e "${BLUE}[DB QUERY]${NC} Agent memory for pattern: ${agent_pattern}"

    db_init || return 1

    if ! db_query_memory "${agent_pattern}" "${format}"; then
        echo -e "${YELLOW}[WARN]${NC} No memory found for pattern: ${agent_pattern}"
        return 1
    fi

    return 0
}

# Get detailed receipt by ID
cmd_db_get_receipt() {
    local receipt_id="$1"

    if [[ -z "${receipt_id}" ]]; then
        echo "Error: receipt_id required"
        return 1
    fi

    echo -e "${BLUE}[DB GET]${NC} Receipt: ${receipt_id}"

    db_init || return 1

    db_get_receipt "${receipt_id}" || return 1

    return 0
}

# Get agent memory by agent_id
cmd_db_get_memory() {
    local agent_id="$1"

    if [[ -z "${agent_id}" ]]; then
        echo "Error: agent_id required"
        return 1
    fi

    echo -e "${BLUE}[DB GET]${NC} Memory for agent: ${agent_id}"

    db_init || return 1

    db_get_memory "${agent_id}" || return 1

    return 0
}

##############################################################################
# Audit Trail Commands
##############################################################################

# Export audit trail to file
cmd_db_export_audit_trail() {
    local format="${1:-json}"
    local output_file="${2:-}"

    echo -e "${BLUE}[DB EXPORT]${NC} Exporting audit trail (format: ${format})"

    db_init || return 1

    if [[ -z "${output_file}" ]]; then
        output_file="./audit-export-$(date +%Y%m%d-%H%M%S).${format}"
    fi

    db_export_audit_trail "${output_file}" "${format}" || return 1

    echo -e "${GREEN}[SUCCESS]${NC} Exported to: ${output_file}"
    return 0
}

# Query audit trail with filters
cmd_db_query_audit_trail() {
    local agent_pattern="${1:-%}"
    local days_back="${2:-7}"
    local format="${3:-json}"

    echo -e "${BLUE}[DB QUERY]${NC} Audit trail for agent pattern: ${agent_pattern} (last ${days_back} days)"

    db_init || return 1

    db_query_audit_trail "${agent_pattern}" "${days_back}" "1000" "${format}" || return 1

    return 0
}

##############################################################################
# Backup and Export Commands
##############################################################################

# Full backup of database
cmd_db_backup() {
    local backup_dir="${1:-./.ggen-backup-$(date +%Y%m%d-%H%M%S)}"

    echo -e "${BLUE}[DB BACKUP]${NC} Creating full database backup"

    db_init || return 1

    db_export_full_backup "${backup_dir}" || return 1

    echo -e "${GREEN}[SUCCESS]${NC} Backup location: ${backup_dir}"
    return 0
}

##############################################################################
# Analytics Commands
##############################################################################

# Generate analytics report
cmd_db_analytics() {
    local agent_pattern="${1:-%}"
    local time_period="${2:-last-7-days}"

    echo -e "${BLUE}[ANALYTICS]${NC} Generating report for agents: ${agent_pattern} (${time_period})"

    db_init || return 1

    db_analytics "${agent_pattern}" "${time_period}" || return 1

    return 0
}

# Display database statistics
cmd_db_stats() {
    echo -e "${BLUE}[DB STATS]${NC} Database statistics"

    db_init || return 1

    db_stats || return 1

    return 0
}

##############################################################################
# Maintenance Commands
##############################################################################

# Cleanup old data
cmd_db_cleanup() {
    local days_to_keep="${1:-90}"

    echo -e "${BLUE}[DB CLEANUP]${NC} Cleaning data older than ${days_to_keep} days"

    db_init || return 1

    db_cleanup "${days_to_keep}" || return 1

    echo -e "${GREEN}[SUCCESS]${NC} Cleanup completed"
    return 0
}

# Reset database
cmd_db_reset() {
    local confirm="${1:-}"

    if [[ "${confirm}" != "--force" ]]; then
        echo -e "${YELLOW}[WARN]${NC} This will delete all database data!"
        echo "Run with --force to confirm: ./main.sh db-reset --force"
        return 1
    fi

    echo -e "${BLUE}[DB RESET]${NC} Resetting database..."

    db_reset || return 1

    echo -e "${GREEN}[SUCCESS]${NC} Database reset completed"
    return 0
}

##############################################################################
# Testing Commands
##############################################################################

# Run persistence layer tests
cmd_db_test() {
    local test_file="${PERSISTENCE_MODULE%/*.sh}/persistence.test.sh"

    if [[ ! -f "${test_file}" ]]; then
        echo "Error: Test file not found at ${test_file}"
        return 1
    fi

    echo -e "${BLUE}[DB TEST]${NC} Running persistence layer tests"

    bash "${test_file}" || return 1

    return 0
}

##############################################################################
# Help
##############################################################################

print_db_help() {
    cat <<'EOF'
DATABASE COMMANDS:

Query Operations:
  db-query-receipts [AGENT_PATTERN] [FORMAT] [LIMIT]
    Query receipts by agent pattern
    Example: ./main.sh db-query-receipts "validator-*" json 50

  db-query-memory [AGENT_PATTERN] [FORMAT]
    Query agent memory state
    Example: ./main.sh db-query-memory "agent-*" json

  db-query-audit-trail [AGENT_PATTERN] [DAYS] [FORMAT]
    Query audit log entries
    Example: ./main.sh db-query-audit-trail "agent-*" 7 json

  db-get-receipt RECEIPT_ID
    Get detailed receipt information
    Example: ./main.sh db-get-receipt abc123def456

  db-get-memory AGENT_ID
    Get agent memory details
    Example: ./main.sh db-get-memory agent-validator

Export & Backup:
  db-export-audit-trail [FORMAT] [OUTPUT_FILE]
    Export audit trail to file (json, csv)
    Example: ./main.sh db-export-audit-trail json ./audit.json

  db-backup [BACKUP_DIR]
    Full database backup
    Example: ./main.sh db-backup ./backups/2026-01-29

Analytics:
  db-analytics [AGENT_PATTERN] [TIME_PERIOD]
    Generate analytics report
    Time periods: last-24-hours, last-7-days, last-30-days, last-90-days
    Example: ./main.sh db-analytics "agent-*" last-7-days

  db-stats
    Display database statistics
    Example: ./main.sh db-stats

Maintenance:
  db-cleanup [DAYS_TO_KEEP]
    Delete data older than specified days (default: 90)
    Example: ./main.sh db-cleanup 90

  db-reset --force
    Reset entire database (destructive)
    Example: ./main.sh db-reset --force

Testing:
  db-test
    Run persistence layer test suite
    Example: ./main.sh db-test

EOF
}

##############################################################################
# Command Router
##############################################################################

route_db_command() {
    local subcommand="${1:-}"
    shift || true

    case "${subcommand}" in
        query-receipts)
            cmd_db_query_receipts "$@"
            ;;
        query-memory)
            cmd_db_query_memory "$@"
            ;;
        query-audit-trail)
            cmd_db_query_audit_trail "$@"
            ;;
        get-receipt)
            cmd_db_get_receipt "$@"
            ;;
        get-memory)
            cmd_db_get_memory "$@"
            ;;
        export-audit-trail)
            cmd_db_export_audit_trail "$@"
            ;;
        backup)
            cmd_db_backup "$@"
            ;;
        analytics)
            cmd_db_analytics "$@"
            ;;
        stats)
            cmd_db_stats "$@"
            ;;
        cleanup)
            cmd_db_cleanup "$@"
            ;;
        reset)
            cmd_db_reset "$@"
            ;;
        test)
            cmd_db_test "$@"
            ;;
        help|--help|-h)
            print_db_help
            ;;
        *)
            echo "Unknown database command: ${subcommand}"
            echo "Run './main.sh db help' for available commands"
            return 1
            ;;
    esac
}

##############################################################################
# Export functions for external use
##############################################################################

export -f cmd_db_query_receipts
export -f cmd_db_query_memory
export -f cmd_db_query_audit_trail
export -f cmd_db_get_receipt
export -f cmd_db_get_memory
export -f cmd_db_export_audit_trail
export -f cmd_db_backup
export -f cmd_db_analytics
export -f cmd_db_stats
export -f cmd_db_cleanup
export -f cmd_db_reset
export -f cmd_db_test
export -f print_db_help
export -f route_db_command
