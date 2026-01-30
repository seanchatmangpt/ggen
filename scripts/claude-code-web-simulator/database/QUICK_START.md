# SQLite Persistence Layer - Quick Start Guide

## Installation

The persistence layer is automatically included with the ggen Web Simulator.

```bash
cd scripts/claude-code-web-simulator
./main.sh start
```

## Database Location

- **Path**: `workspace/.ggen/ggen.db`
- **Auto-created**: Yes, on first use
- **Size**: Typically <5MB for normal usage

## Quick Examples

### 1. Check Database Status

```bash
./main.sh db stats
```

**Output**:
```json
{
  "receipts_total": 1247,
  "receipts_success": 1235,
  "receipts_error": 12,
  "agents_tracked": 8,
  "audit_entries": 5421,
  "database_size_kb": 2341,
  "last_receipt": "2026-01-29 18:05:30"
}
```

### 2. Query Receipts

```bash
# All receipts
./main.sh db query-receipts "%" json

# Specific agent
./main.sh db query-receipts "validator-1" json

# Export to CSV
./main.sh db query-receipts "agent-*" csv 100 > receipts.csv
```

### 3. Query Agent Memory

```bash
# All agents
./main.sh db query-memory "%" json

# Specific pattern
./main.sh db query-memory "agent-*" json | jq '.[] | {agent_id, version}'
```

### 4. View Audit Trail

```bash
# Last 7 days
./main.sh db query-audit-trail "agent-*" 7 json

# Last 30 days
./main.sh db query-audit-trail "agent-%" 30 json | jq '.[] | {timestamp, operation, status}'

# Export to CSV
./main.sh db query-audit-trail "%" 7 csv > audit.csv
```

### 5. Generate Analytics

```bash
# Last 7 days
./main.sh db analytics "%" "last-7-days"

# Last 30 days
./main.sh db analytics "agent-*" "last-30-days" | jq '.agent_metrics'
```

### 6. Backup Database

```bash
# Auto-generated directory
./main.sh db backup

# Specific location
./main.sh db backup ./backups/2026-01-29
```

### 7. Export Audit Trail

```bash
# Auto-generated filename
./main.sh db export-audit-trail

# Specific file
./main.sh db export-audit-trail json ./audit.json
./main.sh db export-audit-trail csv ./audit.csv
```

### 8. Clean Old Data

```bash
# Keep last 90 days
./main.sh db cleanup 90

# Keep last 30 days (aggressive)
./main.sh db cleanup 30
```

### 9. Reset Database

```bash
# WARNING: Destructive!
# Backup first
./main.sh db backup

# Then reset
./main.sh db reset --force
```

### 10. Run Tests

```bash
./main.sh db test
```

## Common Queries

### Get Failed Operations

```bash
./main.sh db query-audit-trail "agent-%" 7 json | \
  jq '.[] | select(.status == "failed")'
```

### List All Agents

```bash
./main.sh db query-memory "%" json | \
  jq '.[] | {agent_id, agent_type, version}'
```

### Find Slow Operations

```bash
./main.sh db query-audit-trail "%" 7 json | \
  jq '.[] | select(.duration_ms > 1000) | {operation, duration_ms}' | \
  sort_by(.duration_ms) | reverse | head -20
```

### Export Success Rate

```bash
./main.sh db analytics "agent-*" "last-7-days" | \
  jq '.receipt_statistics[] | {status, count}'
```

### Get Specific Receipt

```bash
# First get receipt ID
receipt_id=$(./main.sh db query-receipts "agent-1" json | jq -r '.[0].receipt_id')

# Get full details
./main.sh db get-receipt "$receipt_id"
```

### Get Agent Memory

```bash
./main.sh db get-memory "agent-validator" | jq '.'
```

## Database Schema at a Glance

### receipts
- `execution_id`, `agent_id`, `agent_type`
- `receipt_json`, `status`
- `created_at`, `duration_ms`

### agent_memory
- `agent_id` (unique), `agent_type`
- `memory_json`, `version`
- `last_updated`

### audit_log
- `timestamp`, `agent_id`
- `operation`, `operation_type`, `status`
- `duration_ms`, `details`, `error_message`

## Command Syntax

```bash
# Query
./main.sh db query-receipts [PATTERN] [FORMAT] [LIMIT]
./main.sh db query-memory [PATTERN] [FORMAT]
./main.sh db query-audit-trail [PATTERN] [DAYS] [FORMAT]

# Get specific records
./main.sh db get-receipt RECEIPT_ID
./main.sh db get-memory AGENT_ID

# Export
./main.sh db export-audit-trail [FORMAT] [FILE]
./main.sh db backup [DIR]

# Analytics
./main.sh db analytics [PATTERN] [PERIOD]
./main.sh db stats

# Maintenance
./main.sh db cleanup [DAYS]
./main.sh db reset --force

# Test
./main.sh db test
./main.sh db help
```

## Parameters

### Format
- `json` (default)
- `csv`

### Pattern
- `%` - All records
- `agent-*` - Agents matching pattern
- `validator-%` - Validator agents
- `agent-1` - Specific agent

### Time Period
- `last-24-hours`
- `last-7-days` (default)
- `last-30-days`
- `last-90-days`

### Status
- `success`
- `error`
- `partial`

## Programmatic Usage

```bash
#!/bin/bash
source scripts/claude-code-web-simulator/database/persistence.sh

# Initialize
db_init

# Save receipt
db_save_receipt "exec-001" "agent-1" "validation" '{"status":"success"}'

# Query
db_query_receipts "agent-%" json

# Log audit
db_log_audit "agent-1" "test_op" "completed" "test" 1000

# Export
db_export_audit_trail "/tmp/audit.json" json
```

## Troubleshooting

### Database Not Found

```bash
./main.sh db stats  # Auto-initializes
```

### Permission Error

```bash
chmod 0600 workspace/.ggen/ggen.db
```

### No Results

Check pattern:
```bash
./main.sh db query-receipts "agent-*" json | jq 'length'
```

### Test Failures

```bash
./main.sh db test
# Review test output for specific failures
```

## Performance Tips

1. **Use patterns**: Query specific agents rather than all
2. **Limit results**: Specify reasonable limits
3. **Archive old data**: Run `db cleanup` periodically
4. **Export regularly**: Backup with `db backup`
5. **Monitor size**: Check with `du -h workspace/.ggen/ggen.db`

## Integration with Workflows

```bash
# Capture agent execution with database logging
run_agent_with_logging() {
    local agent_id="$1"
    local start_time=$(date +%s%N)

    # Run agent
    ./main.sh run-agent validation --spec test.ttl

    local end_time=$(date +%s%N)
    local duration_ms=$(( (end_time - start_time) / 1000000 ))

    # Log to database
    ./main.sh db log-audit "$agent_id" "agent_execution" "completed" \
        "agent_run" "$duration_ms"
}
```

## Best Practices

1. **Backup before destructive operations**
   ```bash
   ./main.sh db backup && ./main.sh db reset --force
   ```

2. **Regular cleanup to maintain performance**
   ```bash
   # Monthly cleanup
   ./main.sh db cleanup 90
   ```

3. **Monitor database size**
   ```bash
   du -h workspace/.ggen/ggen.db
   ```

4. **Export audit trails for compliance**
   ```bash
   ./main.sh db export-audit-trail json ./audit-$(date +%Y%m%d).json
   ```

5. **Verify data with analytics**
   ```bash
   ./main.sh db analytics "%" "last-7-days"
   ```

## More Information

- Full documentation: `PERSISTENCE.md`
- Test suite: `persistence.test.sh`
- API Reference: `PERSISTENCE.md` (API Reference section)
- Main simulator: `../main.sh`

---

**Quick Reference v1.0.0** | Last Updated: 2026-01-29
