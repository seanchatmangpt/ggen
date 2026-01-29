# Tier 2 Persistence: SQLite Database Schema

## Overview

This directory contains the production-ready SQLite database schema and tools for **Tier 2 Persistence** in the ggen Tier 2 MVP. The schema stores critical data for multi-agent orchestration, execution receipts, audit trails, and performance metrics.

**Status**: ✅ Production-Ready (Validated with 36 comprehensive tests)

---

## Files

### Schema & Initialization

- **`schema.sql`** (15 KB) - Complete SQLite schema with 7 tables, 20+ indexes, 3 views
- **`init-db.sh`** (8.7 KB) - Bash script for database initialization with pre-flight validation
- **`test-schema.py`** (13 KB) - Comprehensive Python test suite (36 tests, 100% pass rate)

---

## Database Structure

### Tables (7 Total)

#### 1. **receipts** - Execution Receipts
Stores deterministic execution outcomes from the five-stage ggen pipeline (μ₁-μ₅).

**Columns**:
- `id` (INTEGER PRIMARY KEY) - Auto-increment primary key
- `execution_id` (TEXT UNIQUE NOT NULL) - Unique identifier per ggen sync
- `agent_id` (TEXT NOT NULL) - Agent that executed the operation
- `operation` (TEXT NOT NULL) - Type: validation, generation, normalization, extraction, emission, canonicalization, receipt, dry_run, watch
- `status` (TEXT NOT NULL) - Result: passed, failed, partial, cancelled
- `timestamp` (TEXT NOT NULL) - ISO8601 timestamp of execution
- `created_at` (TIMESTAMP) - Record creation time
- `manifest_hash` (TEXT) - SHA-256 hash of manifest
- `ontology_hash` (TEXT) - SHA-256 hash of RDF ontology
- `files_generated` (INTEGER) - Number of files created
- `files_modified` (INTEGER) - Number of files modified
- `duration_ms` (INTEGER NOT NULL) - Execution time in milliseconds
- `receipt_json` (TEXT) - Full structured receipt data (JSON)
- `error_message` (TEXT) - Error details if status = 'failed'
- `environment` (TEXT) - dev/staging/production

**Indexes** (4):
- `idx_receipts_agent_timestamp` - (agent_id, timestamp DESC)
- `idx_receipts_execution_id` - (execution_id)
- `idx_receipts_status` - (status, created_at DESC)
- `idx_receipts_created_at` - (created_at DESC)

#### 2. **agent_memory** - Agent State & Memory
Per-agent memory for state persistence and collaboration history.

**Columns**:
- `id` (INTEGER PRIMARY KEY)
- `agent_id` (TEXT UNIQUE NOT NULL)
- `memory_json` (TEXT NOT NULL) - Agent state (JSON)
- `collision_history` (TEXT) - Conflicts detected (JSON)
- `retry_count` (INTEGER) - Retry attempts
- `last_error` (TEXT) - Last error encountered
- `updated_at` (TIMESTAMP) - Last update time
- `created_at` (TIMESTAMP) - Creation time

**Indexes** (2):
- `idx_agent_memory_agent_id` - (agent_id)
- `idx_agent_memory_updated_at` - (updated_at DESC)

#### 3. **audit_log** - Compliance Audit Trail
Comprehensive audit trail for traceability and forensics.

**Columns**:
- `id` (INTEGER PRIMARY KEY)
- `timestamp` (TEXT NOT NULL) - ISO8601 timestamp
- `agent_id` (TEXT NOT NULL) - Agent performing operation
- `operation` (TEXT NOT NULL) - Operation description
- `status` (TEXT NOT NULL) - started, completed, failed, skipped
- `duration_ms` (INTEGER) - Execution duration
- `error_message` (TEXT) - Error info if failed
- `context_json` (TEXT) - Additional context (JSON)
- `created_at` (TIMESTAMP) - Record creation time

**Indexes** (3):
- `idx_audit_log_timestamp` - (timestamp DESC)
- `idx_audit_log_agent_id` - (agent_id, timestamp DESC)
- `idx_audit_log_status` - (status, created_at DESC)

#### 4. **workflow_sessions** - Multi-Agent Session Tracking
Aggregates metrics across parallel agent execution.

**Columns**:
- `id` (INTEGER PRIMARY KEY)
- `session_id` (TEXT UNIQUE NOT NULL) - Unique per workflow run
- `start_time` (TIMESTAMP NOT NULL) - Session start
- `end_time` (TIMESTAMP) - Session end
- `status` (TEXT NOT NULL) - running, completed, failed, cancelled
- `agent_count` (INTEGER NOT NULL) - Number of agents
- `total_duration_ms` (INTEGER) - Total execution time
- `successful_operations` (INTEGER) - Success count
- `failed_operations` (INTEGER) - Failure count
- `context_json` (TEXT) - Session configuration (JSON)
- `created_at` (TIMESTAMP) - Record creation time

**Indexes** (3):
- `idx_workflow_sessions_session_id` - (session_id)
- `idx_workflow_sessions_status` - (status, created_at DESC)
- `idx_workflow_sessions_created_at` - (created_at DESC)

#### 5. **collision_detection** - Agent Collision Tracking
Conflict tracking for root cause analysis in parallel execution.

**Columns**:
- `id` (INTEGER PRIMARY KEY)
- `session_id` (TEXT NOT NULL) - FK: workflow_sessions
- `agent_id_1` (TEXT NOT NULL) - First agent involved
- `agent_id_2` (TEXT NOT NULL) - Second agent involved
- `collision_type` (TEXT NOT NULL) - file_conflict, memory_conflict, semantic_overlap, structural_overlap
- `description` (TEXT NOT NULL) - Collision description
- `resolution` (TEXT) - Resolution applied
- `severity` (TEXT NOT NULL) - info, warning, critical
- `timestamp` (TEXT NOT NULL) - ISO8601 timestamp
- `created_at` (TIMESTAMP) - Record creation time

**Indexes** (3):
- `idx_collision_detection_session_id` - (session_id, timestamp DESC)
- `idx_collision_detection_agents` - (agent_id_1, agent_id_2)
- `idx_collision_detection_severity` - (severity, created_at DESC)

#### 6. **pipeline_metrics** - Five-Stage Pipeline Performance
Performance tracking for μ₁-μ₅ pipeline stages.

**Columns**:
- `id` (INTEGER PRIMARY KEY)
- `execution_id` (TEXT NOT NULL) - FK: receipts
- `stage` (TEXT NOT NULL) - μ₁_normalize, μ₂_extract, μ₃_emit, μ₄_canonicalize, μ₅_receipt
- `duration_ms` (INTEGER NOT NULL) - Stage duration
- `input_size_bytes` (INTEGER) - Input data size
- `output_size_bytes` (INTEGER) - Output data size
- `status` (TEXT NOT NULL) - passed, failed
- `error_message` (TEXT) - Error if failed
- `timestamp` (TEXT NOT NULL) - ISO8601 timestamp
- `created_at` (TIMESTAMP) - Record creation time

**Indexes** (3):
- `idx_pipeline_metrics_execution_id` - (execution_id)
- `idx_pipeline_metrics_stage` - (stage, duration_ms DESC)
- `idx_pipeline_metrics_timestamp` - (timestamp DESC)

#### 7. **slo_violations** - SLO Tracking
Records when performance targets are not met.

**Columns**:
- `id` (INTEGER PRIMARY KEY)
- `execution_id` (TEXT NOT NULL) - FK: receipts
- `metric` (TEXT NOT NULL) - Metric name
- `target_value` (REAL NOT NULL) - Target SLO value
- `actual_value` (REAL NOT NULL) - Actual measured value
- `severity` (TEXT NOT NULL) - warning, critical
- `description` (TEXT) - Violation description
- `timestamp` (TEXT NOT NULL) - ISO8601 timestamp
- `created_at` (TIMESTAMP) - Record creation time

**Indexes** (3):
- `idx_slo_violations_execution_id` - (execution_id)
- `idx_slo_violations_metric` - (metric, created_at DESC)
- `idx_slo_violations_severity` - (severity, created_at DESC)

---

### Views (3 Total)

#### 1. **recent_executions**
Quick view of recent execution results with summary statistics.

**Query**: Joins receipts with audit_log for last 100 executions, includes audit entry count and SLO violations.

#### 2. **agent_performance_summary**
Aggregate performance statistics per agent.

**Metrics**:
- Total executions
- Successful vs. failed executions
- Average/min/max duration
- Total files generated
- Last execution time

#### 3. **pipeline_stage_performance**
Performance analysis per pipeline stage.

**Metrics**:
- Execution count per stage
- Average/min/max duration
- Pass/fail counts

---

## Usage

### Initialization

#### Bash (with validation)
```bash
./init-db.sh [database_path]
```

Example:
```bash
./init-db.sh .ggen/tier2.db
```

This script:
- Verifies sqlite3 installation
- Creates database directories
- Loads schema from schema.sql
- Validates tables, indexes, views
- Tests JSON operations
- Prints schema information

#### Python (direct)
```python
import sqlite3

conn = sqlite3.connect('.ggen/tier2.db')
conn.execute("PRAGMA foreign_keys = ON")

with open('schema.sql', 'r') as f:
    conn.executescript(f.read())

conn.commit()
```

### Testing

Run comprehensive test suite:
```bash
python3 test-schema.py [database_path]
```

Example:
```bash
python3 test-schema.py .ggen/tier2-test.db
```

**Test Coverage** (36 tests):
- Schema structure (7 tests)
- Table columns (15 tests)
- Index creation (2 tests)
- Constraint validation (2 tests)
- Data types (2 tests)
- JSON operations (2 tests)
- Foreign keys (2 tests)
- View functionality (3 tests)
- Integration scenarios (1 test)

**Result**: ✅ 36/36 passed (100% success rate)

---

## Data Import Examples

### Insert Receipt

```sql
INSERT INTO receipts (
  execution_id, agent_id, operation, status, timestamp,
  files_generated, duration_ms, receipt_json
) VALUES (
  'exec-001',
  'code-generator',
  'generation',
  'passed',
  '2026-01-29T12:34:56Z',
  5,
  2500,
  '{"files": ["src/main.rs", "src/lib.rs"], "hashes": {"src/main.rs": "abc123"}}'
);
```

### Insert Audit Log Entry

```sql
INSERT INTO audit_log (
  timestamp, agent_id, operation, status, duration_ms, context_json
) VALUES (
  '2026-01-29T12:34:56Z',
  'code-generator',
  'RDF normalization',
  'completed',
  450,
  '{"triples_processed": 1240, "rules_applied": 15}'
);
```

### Insert Agent Memory

```sql
INSERT INTO agent_memory (
  agent_id, memory_json, collision_history
) VALUES (
  'code-generator',
  '{"state": "initialized", "capacity": 100, "decisions": ["use_const_generics"]}',
  '[]'
);
```

### Create Workflow Session

```sql
INSERT INTO workflow_sessions (
  session_id, start_time, status, agent_count, context_json
) VALUES (
  'session-001',
  datetime('now'),
  'running',
  3,
  '{"topology": "hierarchical", "collision_detection": true}'
);
```

---

## Query Examples

### Find Recent Failures

```sql
SELECT * FROM recent_executions
WHERE status = 'failed'
ORDER BY timestamp DESC
LIMIT 10;
```

### Agent Performance Statistics

```sql
SELECT * FROM agent_performance_summary
ORDER BY successful_executions DESC;
```

### Pipeline Stage Bottlenecks

```sql
SELECT * FROM pipeline_stage_performance
ORDER BY avg_duration_ms DESC;
```

### SLO Violations by Metric

```sql
SELECT metric, COUNT(*) as violation_count, AVG(actual_value) as avg_value
FROM slo_violations
WHERE created_at > datetime('now', '-7 days')
GROUP BY metric
ORDER BY violation_count DESC;
```

### Collision Detection Summary

```sql
SELECT collision_type, severity, COUNT(*) as count
FROM collision_detection
WHERE timestamp > datetime('now', '-24 hours')
GROUP BY collision_type, severity;
```

---

## Performance Characteristics

### Indexes
- **Primary key indexes**: O(1) lookup by execution_id or session_id
- **Composite indexes**: O(log N) range queries on (agent_id, timestamp)
- **Status indexes**: Fast filtering by operation status
- **Time-based indexes**: Optimized for recent data queries

### Expected Performance
- Single receipt insertion: <1ms
- Batch insert (100 receipts): <50ms
- Range query (last 24 hours): <10ms
- Aggregation query (agent stats): <25ms
- Full table scan (all receipts): <100ms for 10,000 records

### Recommendations

1. **Regular Maintenance**:
   ```sql
   VACUUM;              -- Reclaim space
   ANALYZE;             -- Update statistics
   PRAGMA integrity_check;  -- Verify consistency
   ```

2. **Archival Strategy**:
   ```sql
   -- Archive old records (monthly)
   INSERT INTO receipts_archive SELECT * FROM receipts
   WHERE created_at < datetime('now', '-90 days');

   DELETE FROM receipts
   WHERE created_at < datetime('now', '-90 days');
   ```

3. **Backup**:
   ```bash
   sqlite3 .ggen/tier2.db ".backup '.ggen/tier2-backup.db'"
   ```

---

## Foreign Key Relationships

```
workflow_sessions (1)
  ├── collision_detection (N)
  │   └── References: session_id

receipts (1)
  ├── pipeline_metrics (N)
  │   └── References: execution_id
  └── slo_violations (N)
      └── References: execution_id

agent_memory (1:1)
  └── Independent table
      └── One row per agent
```

---

## Constraints & Validation

### Primary Keys
- All tables have INTEGER PRIMARY KEY with AUTOINCREMENT

### Unique Constraints
- `receipts.execution_id` - Prevents duplicate executions
- `agent_memory.agent_id` - One row per agent
- `workflow_sessions.session_id` - Unique sessions

### Check Constraints
- `receipts.operation` - Enum validation (9 allowed values)
- `receipts.status` - Enum validation (4 allowed values)
- `audit_log.status` - Enum validation (4 allowed values)
- `workflow_sessions.status` - Enum validation (4 allowed values)
- `collision_detection.collision_type` - Enum validation (4 types)
- `collision_detection.severity` - Enum validation (3 levels)
- `pipeline_metrics.stage` - Enum validation (5 stages)
- `slo_violations.severity` - Enum validation (2 levels)

### Foreign Keys
- `collision_detection.session_id` → `workflow_sessions.session_id` (RESTRICT)
- `pipeline_metrics.execution_id` → `receipts.execution_id` (RESTRICT)
- `slo_violations.execution_id` → `receipts.execution_id` (RESTRICT)

---

## Integration with ggen

### Initialization Flow

1. **Agent Coordinator** calls `init-db.sh` during startup
2. **Database** initializes with empty schema
3. **Agents** begin execution and insert receipts
4. **Audit logger** records all operations
5. **Collision detector** tracks parallel conflicts
6. **Performance monitor** records SLO metrics

### Data Flow

```
ggen sync command
    ↓
[μ₁ Normalize] → Record metrics (μ₁_normalize)
    ↓
[μ₂ Extract] → Record metrics (μ₂_extract)
    ↓
[μ₃ Emit] → Record metrics (μ₃_emit)
    ↓
[μ₄ Canonicalize] → Record metrics (μ₄_canonicalize)
    ↓
[μ₅ Receipt] → Record metrics (μ₅_receipt)
    ↓
Insert receipt → Create deterministic proof
    ↓
Insert audit entries → Record all operations
    ↓
Check SLOs → Track violations
```

---

## SQLite Configuration

### Required Pragmas

```sql
-- Enable foreign key constraints (CRITICAL)
PRAGMA foreign_keys = ON;

-- Use write-ahead logging for concurrency
PRAGMA journal_mode = WAL;

-- Optimize for typical query patterns
PRAGMA optimize;

-- Set reasonable cache size (10MB)
PRAGMA cache_size = -10000;
```

### Optional Optimizations

```sql
-- Synchronous writes (balance safety/performance)
PRAGMA synchronous = NORMAL;

-- Increase temp_store for large sorts
PRAGMA temp_store = MEMORY;

-- Enable query optimizer
PRAGMA optimize;
```

---

## Size Estimates

| Metric | Estimate |
|--------|----------|
| Empty database | ~32 KB |
| Per receipt | ~500 bytes |
| Per audit entry | ~300 bytes |
| Per agent memory | ~2 KB |
| 10,000 receipts | ~5 MB |
| 100,000 receipts | ~50 MB |
| 1M receipts | ~500 MB |

---

## Troubleshooting

### Issue: Foreign Key Constraint Violation

**Cause**: Inserting collision_detection without valid session_id

**Solution**:
```sql
-- Verify session exists
SELECT COUNT(*) FROM workflow_sessions WHERE session_id = 'your-id';

-- Create session if missing
INSERT INTO workflow_sessions (session_id, start_time, status, agent_count)
VALUES ('your-id', datetime('now'), 'running', 2);
```

### Issue: Unique Constraint Violation

**Cause**: Duplicate execution_id

**Solution**:
```sql
-- Check existing execution
SELECT * FROM receipts WHERE execution_id = 'duplicate-id';

-- Generate new unique ID or use REPLACE
REPLACE INTO receipts (execution_id, agent_id, operation, status, timestamp, duration_ms)
VALUES (...);
```

### Issue: Database Locked

**Cause**: Concurrent writes without WAL mode

**Solution**:
```sql
-- Enable WAL mode
PRAGMA journal_mode = WAL;

-- Retry connection
sqlite3 .ggen/tier2.db
```

---

## Related Documentation

- **Schema Design**: See inline SQL comments in `schema.sql`
- **Tier 2 MVP**: See `/docs/releases/v0.2.0/`
- **ggen Pipeline**: See CLAUDE.md - Five-stage μ pipeline
- **Performance SLOs**: See SLO targets in CLAUDE.md

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2026-01-29 | Initial production-ready schema (7 tables, 20+ indexes, 3 views) |

---

**Last Updated**: 2026-01-29
**Status**: ✅ Production-Ready
**Test Coverage**: 36/36 passing (100%)
