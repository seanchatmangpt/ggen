# SQLite Persistence Layer for ggen Web Simulator

Production-ready database abstraction for storing receipts, agent memory, and audit trails in the Claude Code Web simulation environment.

**Version**: 1.0.0
**Status**: Production-Ready
**Location**: `scripts/claude-code-web-simulator/database/`

---

## Overview

The persistence layer provides comprehensive SQLite database operations for:

- **Receipt Storage**: Save and query execution receipts with metadata
- **Agent Memory**: Persistent agent state and context management
- **Audit Logging**: Detailed compliance and operational tracking
- **Data Export**: Backup and export capabilities for analytics
- **Analytics**: Performance metrics and statistics generation

### Key Features

- **Production-Ready**: Error handling, retry logic, transaction safety
- **Schema Versioning**: Automatic migration and validation
- **Query Interface**: JSON and CSV export formats
- **Concurrent Access**: Safe multi-process database operations
- **Deterministic**: Content hashing for reproducibility verification

---

## Architecture

### Database Schema

The persistence layer maintains four core tables:

#### `receipts` Table
Stores execution receipts with metadata and deterministic hashing.

```sql
CREATE TABLE receipts (
    receipt_id TEXT PRIMARY KEY,
    execution_id TEXT NOT NULL UNIQUE,
    agent_id TEXT NOT NULL,
    agent_type TEXT NOT NULL,
    receipt_json TEXT NOT NULL,
    status TEXT NOT NULL,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    duration_ms INTEGER
);
```

**Fields**:
- `receipt_id`: Unique receipt identifier (SHA-256 hash)
- `execution_id`: Execution trace ID from ggen sync
- `agent_id`: Agent identifier (e.g., "validator-1")
- `agent_type`: Agent type (e.g., "validation", "generation")
- `receipt_json`: Full receipt data as JSON
- `status`: Execution status ("success", "error", "partial")
- `duration_ms`: Execution duration in milliseconds

#### `agent_memory` Table
Persistent storage for agent state and context.

```sql
CREATE TABLE agent_memory (
    memory_id TEXT PRIMARY KEY,
    agent_id TEXT NOT NULL UNIQUE,
    agent_type TEXT NOT NULL,
    memory_json TEXT NOT NULL,
    last_updated DATETIME DEFAULT CURRENT_TIMESTAMP,
    version INTEGER DEFAULT 1
);
```

**Fields**:
- `memory_id`: Unique memory record identifier
- `agent_id`: Agent identifier (unique constraint)
- `agent_type`: Agent classification
- `memory_json`: Serialized agent state
- `last_updated`: Last modification timestamp
- `version`: Incremental version number (for update tracking)

#### `audit_log` Table
Compliance and operational event tracking.

```sql
CREATE TABLE audit_log (
    audit_id TEXT PRIMARY KEY,
    timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
    agent_id TEXT NOT NULL,
    operation TEXT NOT NULL,
    operation_type TEXT,
    status TEXT NOT NULL,
    duration_ms INTEGER,
    details TEXT,
    error_message TEXT
);
```

**Fields**:
- `audit_id`: Unique audit event identifier
- `timestamp`: Event timestamp (ISO 8601)
- `agent_id`: Agent identifier
- `operation`: Operation name (e.g., "validate_schema")
- `operation_type`: Classification (validation, extraction, rendering, etc.)
- `status`: Result status (started, completed, failed, retry)
- `duration_ms`: Operation duration
- `details`: Additional context
- `error_message`: Error details if applicable

#### `_schema_metadata` Table
Version and migration tracking.

```sql
CREATE TABLE _schema_metadata (
    key TEXT PRIMARY KEY,
    value TEXT NOT NULL,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
);
```

---

## API Reference

### Database Initialization

#### `db_init()`
Initialize database with schema if needed.

```bash
db_init
# Returns: 0 on success, 1 on failure
```

**Behavior**:
- Creates database directory if needed
- Initializes schema if database doesn't exist
- Verifies schema on existing databases
- Retries on lock conflicts (up to 5 attempts)

**Example**:
```bash
if db_init; then
    echo "Database ready"
else
    echo "Database initialization failed"
    exit 1
fi
```

---

### Receipt Management

#### `db_save_receipt()`
Save execution receipt to database.

**Signature**:
```bash
db_save_receipt execution_id agent_id agent_type receipt_json [status] [duration_ms]
```

**Parameters**:
- `execution_id`: Unique execution identifier (required)
- `agent_id`: Agent identifier (required)
- `agent_type`: Agent type/classification (required)
- `receipt_json`: JSON receipt data (required, must be valid JSON)
- `status`: Execution status - "success", "error", "partial" (default: "success")
- `duration_ms`: Execution duration (default: 0)

**Returns**: 0 on success, 1 on failure

**Example**:
```bash
receipt_data='{
    "status": "success",
    "files_generated": 12,
    "rules_executed": 5,
    "inference_rules": 3,
    "generation_rules": 2,
    "determinism_hash": "sha256:abc123..."
}'

db_save_receipt "exec-001" "validator-1" "validation" "$receipt_data" "success" 1234
```

#### `db_query_receipts()`
Query receipts by agent pattern.

**Signature**:
```bash
db_query_receipts [agent_pattern] [format] [limit]
```

**Parameters**:
- `agent_pattern`: SQL LIKE pattern for agent_id (default: "%")
- `format`: Output format - "json" or "csv" (default: "json")
- `limit`: Maximum results (default: 100)

**Returns**: Query results in specified format

**Example**:
```bash
# Get all receipts for validator agents
db_query_receipts "validator-*" json 100 | jq '.[] | {execution_id, status}'

# Export as CSV
db_query_receipts "agent-%"  csv 1000 > receipts.csv
```

#### `db_get_receipt()`
Retrieve full receipt by ID.

**Signature**:
```bash
db_get_receipt receipt_id
```

**Parameters**:
- `receipt_id`: Receipt identifier

**Returns**: Full receipt JSON

**Example**:
```bash
receipt_json=$(db_get_receipt "abc123def456")
echo "$receipt_json" | jq '.determinism_hash'
```

---

### Agent Memory Management

#### `db_save_memory()`
Save or update agent memory.

**Signature**:
```bash
db_save_memory agent_id agent_type memory_json
```

**Parameters**:
- `agent_id`: Agent identifier (required, unique)
- `agent_type`: Agent type/classification (required)
- `memory_json`: Serialized agent state (required, valid JSON)

**Returns**: 0 on success, 1 on failure

**Behavior**:
- Creates new memory record on first call
- Updates existing record on subsequent calls
- Increments version number on update

**Example**:
```bash
agent_state='{
    "active": true,
    "iterations": 5,
    "results": [
        {"rule": "validate_schema", "status": "passed"},
        {"rule": "extract_data", "status": "passed"}
    ],
    "context": {
        "ontology_loaded": true,
        "templates_count": 12
    }
}'

db_save_memory "agent-validator" "validation" "$agent_state"
```

#### `db_get_memory()`
Retrieve agent memory.

**Signature**:
```bash
db_get_memory agent_id
```

**Parameters**:
- `agent_id`: Agent identifier

**Returns**: Agent memory JSON or error

**Example**:
```bash
memory=$(db_get_memory "agent-validator")
echo "$memory" | jq '.active'
```

#### `db_query_memory()`
Query all agent memory with optional filtering.

**Signature**:
```bash
db_query_memory [agent_pattern] [format]
```

**Parameters**:
- `agent_pattern`: SQL LIKE pattern (default: "%")
- `format`: "json" or "csv" (default: "json")

**Returns**: Query results

**Example**:
```bash
# List all agents
db_query_memory "%" json | jq '.[] | {agent_id, agent_type, version}'

# Export memory state
db_query_memory "agent-*" csv > agent_memory.csv
```

---

### Audit Logging

#### `db_log_audit()`
Log audit event.

**Signature**:
```bash
db_log_audit agent_id operation status [operation_type] [duration_ms] [details] [error_message]
```

**Parameters**:
- `agent_id`: Agent identifier (required)
- `operation`: Operation name (required)
- `status`: Status - "started", "completed", "failed", "retry" (required)
- `operation_type`: Operation classification (optional)
- `duration_ms`: Duration in milliseconds (optional, default: 0)
- `details`: Additional context (optional)
- `error_message`: Error details (optional)

**Returns**: 0 on success, 1 on failure

**Example**:
```bash
# Log successful operation
db_log_audit "agent-1" "validate_schema" "completed" "validation" 1200 "Schema validated" ""

# Log failed operation
db_log_audit "agent-1" "render_template" "failed" "rendering" 0 "" "Template not found: layout.tera"
```

#### `db_query_audit_trail()`
Query audit trail.

**Signature**:
```bash
db_query_audit_trail [agent_pattern] [days_back] [limit] [format]
```

**Parameters**:
- `agent_pattern`: SQL LIKE pattern (default: "%")
- `days_back`: History in days (default: 7)
- `limit`: Maximum results (default: 1000)
- `format`: "json" or "csv" (default: "json")

**Returns**: Query results

**Example**:
```bash
# Get failed operations from last 7 days
db_query_audit_trail "agent-%" 7 1000 json | jq '.[] | select(.status == "failed")'

# Export audit trail for compliance
db_query_audit_trail "agent-*" 30 10000 csv > audit_trail.csv
```

---

### Data Export

#### `db_export_audit_trail()`
Export audit trail to file.

**Signature**:
```bash
db_export_audit_trail [output_file] [format]
```

**Parameters**:
- `output_file`: Output file path (default: auto-generated)
- `format`: "json" or "csv" (default: "json")

**Returns**: 0 on success, 1 on failure

**Example**:
```bash
# Auto-generated filename
db_export_audit_trail

# Specific file with format
db_export_audit_trail "./audit-2026-01.json" json
db_export_audit_trail "./audit.csv" csv
```

#### `db_export_full_backup()`
Full database backup.

**Signature**:
```bash
db_export_full_backup [backup_dir]
```

**Parameters**:
- `backup_dir`: Backup directory (default: "./.ggen-backup")

**Returns**: 0 on success, 1 on failure

**Output Files**:
- `receipts.json`: All receipts
- `agent_memory.json`: All agent memory
- `audit_log.json`: All audit events

**Example**:
```bash
db_export_full_backup "./backups/2026-01-29"
# Creates:
# ./backups/2026-01-29/receipts.json
# ./backups/2026-01-29/agent_memory.json
# ./backups/2026-01-29/audit_log.json
```

---

### Analytics

#### `db_analytics()`
Generate analytics report.

**Signature**:
```bash
db_analytics [agent_pattern] [time_period]
```

**Parameters**:
- `agent_pattern`: SQL LIKE pattern (default: "%")
- `time_period`: "last-24-hours", "last-7-days", "last-30-days", "last-90-days" (default: "last-7-days")

**Returns**: JSON analytics report

**Report Structure**:
```json
{
  "period": "last-7-days",
  "generated_at": "2026-01-29T18:00:00Z",
  "agent_metrics": [
    {
      "agent_id": "agent-validator",
      "total_operations": 150,
      "successful_operations": 148,
      "failed_operations": 2,
      "avg_duration_ms": 845.3,
      "min_duration_ms": 123,
      "max_duration_ms": 2341
    }
  ],
  "receipt_statistics": [
    {
      "status": "success",
      "count": 95,
      "avg_duration_ms": 1023.4
    },
    {
      "status": "error",
      "count": 5,
      "avg_duration_ms": 567.2
    }
  ]
}
```

**Example**:
```bash
# Get analytics for all agents
db_analytics "%" "last-7-days" | jq '.agent_metrics[] | {agent_id, total_operations}'

# Get analytics for specific agent type
db_analytics "validator-*" "last-30-days" | jq '.receipt_statistics'
```

#### `db_stats()`
Display database statistics.

**Signature**:
```bash
db_stats
```

**Returns**: JSON statistics object

**Example**:
```bash
db_stats | jq '.'
# Output:
# {
#   "receipts_total": 1247,
#   "receipts_success": 1235,
#   "receipts_error": 12,
#   "agents_tracked": 8,
#   "audit_entries": 5421,
#   "database_size_kb": 2341,
#   "last_receipt": "2026-01-29 18:05:30"
# }
```

---

### Maintenance

#### `db_cleanup()`
Delete old data from database.

**Signature**:
```bash
db_cleanup [days_to_keep]
```

**Parameters**:
- `days_to_keep`: Data retention period (default: 90)

**Returns**: 0 on success, 1 on failure

**Behavior**:
- Deletes receipts older than specified days
- Deletes audit entries older than specified days
- Optimizes database (VACUUM)

**Example**:
```bash
# Keep last 90 days
db_cleanup 90

# Aggressive cleanup - keep 30 days
db_cleanup 30
```

#### `db_reset()`
Reset entire database.

**Signature**:
```bash
db_reset
```

**Returns**: 0 on success, 1 on failure

**Behavior**:
- Deletes database file
- Reinitializes schema

**Warning**: This is destructive and cannot be undone. Always backup first.

**Example**:
```bash
# Export backup first
db_export_full_backup "./backup-before-reset"

# Then reset
db_reset
```

---

## CLI Integration

### Command Format

```bash
./main.sh db [subcommand] [arguments]
```

### Available Subcommands

```bash
# Query operations
./main.sh db query-receipts [AGENT_PATTERN] [FORMAT] [LIMIT]
./main.sh db query-memory [AGENT_PATTERN] [FORMAT]
./main.sh db query-audit-trail [AGENT_PATTERN] [DAYS] [FORMAT]
./main.sh db get-receipt RECEIPT_ID
./main.sh db get-memory AGENT_ID

# Export operations
./main.sh db export-audit-trail [FORMAT] [OUTPUT_FILE]
./main.sh db backup [BACKUP_DIR]

# Analytics
./main.sh db analytics [AGENT_PATTERN] [TIME_PERIOD]
./main.sh db stats

# Maintenance
./main.sh db cleanup [DAYS_TO_KEEP]
./main.sh db reset --force

# Testing
./main.sh db test

# Help
./main.sh db help
```

### Usage Examples

```bash
# Initialize and test
cd scripts/claude-code-web-simulator
./main.sh start
./main.sh db stats

# Query receipts for validation agents
./main.sh db query-receipts "validator-*" json | jq '.[] | {execution_id, status}'

# Export audit trail for compliance
./main.sh db export-audit-trail json ./audit-export.json

# Generate analytics report
./main.sh db analytics "agent-*" "last-7-days" | jq '.agent_metrics'

# Run tests
./main.sh db test

# Backup before maintenance
./main.sh db backup ./backups/2026-01-29
```

---

## Error Handling

### Retry Logic

The persistence layer implements automatic retry on database locks:

- **Max Retries**: 5 attempts
- **Retry Delay**: 100ms between attempts
- **Timeout**: 30 seconds total

### Common Errors

| Error | Cause | Solution |
|-------|-------|----------|
| `Database initialization failed` | File permission or disk space | Check file permissions, disk space |
| `Invalid JSON in receipt` | Malformed JSON | Validate JSON before saving |
| `Missing required parameters` | Empty parameter | Provide all required parameters |
| `Database locked` | Concurrent access conflict | Retry or wait for other process |

---

## Testing

### Run Test Suite

```bash
# Via CLI
./main.sh db test

# Direct execution
bash database/persistence.test.sh
```

### Test Coverage

The test suite includes:

- ✓ Database initialization
- ✓ Receipt persistence and retrieval
- ✓ Receipt error handling
- ✓ Agent memory persistence
- ✓ Agent memory updates
- ✓ Audit logging
- ✓ Data export (JSON, CSV)
- ✓ Full backup operations
- ✓ Analytics generation
- ✓ Database statistics
- ✓ Data cleanup
- ✓ Database reset
- ✓ Concurrent access safety

---

## Performance

### SLO Targets

- **DB Initialization**: <100ms
- **Receipt Save**: <50ms (with retries)
- **Memory Save**: <50ms (with retries)
- **Audit Log**: <30ms (with retries)
- **Query (100 records)**: <100ms
- **Full Backup**: <500ms
- **Analytics**: <200ms

### Indexes

Optimized query performance with indexes on:
- `receipts(agent_id, created_at, status)`
- `agent_memory(agent_id, agent_type, last_updated)`
- `audit_log(agent_id, timestamp, status, operation)`

---

## Security

### Data Protection

- **File Permissions**: Database file restricted to owner (0600)
- **Input Validation**: JSON parsing with error handling
- **SQL Injection Prevention**: Parameterized queries (via SQLite)
- **Concurrent Access**: Database-level locking

### Audit Trail

All operations logged with:
- Timestamp (ISO 8601)
- Agent identifier
- Operation type
- Status (success/failure)
- Duration metrics
- Error messages

---

## Configuration

### Environment Variables

```bash
# Override default database path (optional)
export DB_DIR="/custom/path/.ggen"
export DB_PATH="/custom/path/.ggen/ggen.db"

# These are set automatically by the simulator
export SCRIPT_DIR="$(pwd)"
export WORKSPACE_DIR="$(pwd)/workspace"
```

### Database Location

Default: `scripts/claude-code-web-simulator/workspace/.ggen/ggen.db`

---

## Troubleshooting

### Database File Not Found

```bash
# Initialize database
./main.sh db stats  # Auto-initializes if needed

# Or manually initialize
bash database/persistence.sh
db_init
```

### Permission Denied

```bash
# Fix file permissions
chmod 0600 workspace/.ggen/ggen.db
```

### SQLite Command Not Found

```bash
# Install SQLite
# Ubuntu/Debian
sudo apt-get install sqlite3

# macOS
brew install sqlite3

# Or use container
docker run -v $(pwd):/workspace ubuntu:latest bash -c "apt-get update && apt-get install -y sqlite3"
```

### Test Failures

```bash
# Run tests with verbose output
bash database/persistence.test.sh

# Check database integrity
sqlite3 workspace/.ggen/ggen.db "PRAGMA integrity_check;"
```

---

## Integration Examples

### With Main Simulator

```bash
cd scripts/claude-code-web-simulator

# Start simulator
./main.sh start

# Run agent and save receipt
./main.sh run-agent validation --spec test.ttl

# Query receipts
./main.sh db query-receipts "%" json

# Export for analysis
./main.sh db export-audit-trail json
```

### Programmatic Usage

```bash
#!/bin/bash
source database/persistence.sh

# Initialize
db_init || exit 1

# Save receipt
receipt='{"status":"success","files":5}'
db_save_receipt "exec-1" "agent-1" "validation" "$receipt"

# Query
db_query_receipts "agent-%"

# Analytics
db_analytics "agent-%" "last-7-days"
```

---

## Future Enhancements

- [ ] Database compression for large deployments
- [ ] Replication support for distributed setups
- [ ] GraphQL query interface
- [ ] Real-time event streaming
- [ ] Integration with observability platforms (Datadog, New Relic)
- [ ] Encryption at rest (SQLCipher)
- [ ] Time-series analytics optimization

---

## Support & Contribution

For issues, questions, or contributions:

1. Check this documentation
2. Review test suite in `persistence.test.sh`
3. Check main simulator logs in `workspace/audit-logs/`
4. Open issue on ggen repository

---

**Version**: 1.0.0
**Last Updated**: 2026-01-29
**Status**: Production-Ready
