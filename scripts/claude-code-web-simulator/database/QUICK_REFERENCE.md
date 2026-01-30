# Tier 2 Persistence - Quick Reference Guide

## Quick Start (30 seconds)

```bash
# Initialize database
./init-db.sh .ggen/tier2.db

# Run tests (validate schema)
python3 test-schema.py .ggen/tier2-test.db

# Connect to database
sqlite3 .ggen/tier2.db

# Run query
SELECT * FROM recent_executions LIMIT 5;
```

---

## Tables at a Glance

| Table | Purpose | Key Column | Rows |
|-------|---------|-----------|------|
| **receipts** | Execution results | execution_id | 10k-100k |
| **agent_memory** | Agent state | agent_id | 5-20 |
| **audit_log** | Compliance trail | timestamp | 100k+ |
| **workflow_sessions** | Multi-agent runs | session_id | 100-1k |
| **collision_detection** | Conflict tracking | session_id | 10-100 |
| **pipeline_metrics** | Stage performance | execution_id | 50k-500k |
| **slo_violations** | SLO breaches | metric | 100-1k |

---

## Common Operations

### Insert Receipt
```sql
INSERT INTO receipts (execution_id, agent_id, operation, status, timestamp, duration_ms, files_generated)
VALUES ('exec-001', 'coder', 'generation', 'passed', datetime('now'), 2500, 5);
```

### Insert Audit Entry
```sql
INSERT INTO audit_log (timestamp, agent_id, operation, status, duration_ms)
VALUES (datetime('now'), 'coder', 'code generation', 'completed', 2500);
```

### Query Recent Executions
```sql
SELECT execution_id, agent_id, status, duration_ms
FROM recent_executions
ORDER BY timestamp DESC
LIMIT 10;
```

### Get Agent Stats
```sql
SELECT * FROM agent_performance_summary
ORDER BY successful_executions DESC;
```

### Find SLO Violations
```sql
SELECT metric, count(*) as violations
FROM slo_violations
WHERE created_at > datetime('now', '-24 hours')
GROUP BY metric;
```

### Track Collisions
```sql
SELECT * FROM collision_detection
WHERE severity = 'critical'
ORDER BY timestamp DESC;
```

---

## Enums & Constants

### Operation Types
```
'validation'         -- Schema validation
'generation'         -- Code generation
'normalization'      -- RDF normalization (μ₁)
'extraction'         -- SPARQL extraction (μ₂)
'emission'          -- Template rendering (μ₃)
'canonicalization'  -- Code formatting (μ₄)
'receipt'           -- Proof generation (μ₅)
'dry_run'           -- Preview mode
'watch'             -- Continuous mode
```

### Status Values
```
'passed'            -- Operation succeeded
'failed'            -- Operation failed
'partial'           -- Partial success
'cancelled'         -- Operation cancelled
```

### Collision Types
```
'file_conflict'         -- Same file modified
'memory_conflict'       -- Memory state conflict
'semantic_overlap'      -- Semantic duplication
'structural_overlap'    -- Structural duplication
```

### Severity Levels
```
'info'              -- Informational
'warning'           -- Warning level
'critical'          -- Critical issue
```

### Pipeline Stages
```
'μ₁_normalize'      -- Normalization stage
'μ₂_extract'        -- Extraction stage
'μ₃_emit'          -- Emission stage
'μ₄_canonicalize'  -- Canonicalization stage
'μ₅_receipt'       -- Receipt generation stage
```

---

## Performance Tips

### ✅ DO
- Use indexed columns in WHERE clauses
- Use LIMIT for large result sets
- Run VACUUM periodically
- Use batch inserts in transactions
- Query views for aggregations

### ❌ DON'T
- Use functions on indexed columns in WHERE
- Insert large text without JSON extraction
- Leave transactions open
- Query full tables without LIMIT
- Ignore foreign key violations

---

## Connection String

**Python**:
```python
import sqlite3
conn = sqlite3.connect('.ggen/tier2.db')
conn.execute("PRAGMA foreign_keys = ON")
```

**Bash**:
```bash
sqlite3 .ggen/tier2.db
```

**Node.js**:
```javascript
const Database = require('better-sqlite3');
const db = new Database('.ggen/tier2.db');
db.pragma('foreign_keys = ON');
```

---

## View the Data

### Recent Executions (Last 10)
```sql
SELECT * FROM recent_executions LIMIT 10;
```

### Agent Performance
```sql
SELECT agent_id, total_executions, successful_executions, avg_duration_ms
FROM agent_performance_summary;
```

### Pipeline Bottlenecks
```sql
SELECT stage, avg_duration_ms, executions
FROM pipeline_stage_performance
ORDER BY avg_duration_ms DESC;
```

### Active Sessions
```sql
SELECT * FROM workflow_sessions
WHERE status = 'running'
ORDER BY start_time DESC;
```

---

## JSON Queries

### Extract from receipt_json
```sql
-- Get first file generated
SELECT json_extract(receipt_json, '$.files[0]')
FROM receipts WHERE execution_id = 'exec-001';

-- Get manifest hash
SELECT json_extract(receipt_json, '$.manifest_hash')
FROM receipts WHERE execution_id = 'exec-001';

-- Query with condition
SELECT * FROM receipts
WHERE json_extract(receipt_json, '$.files_count') > 5;
```

### Query Memory JSON
```sql
-- Get agent state
SELECT json_extract(memory_json, '$.state')
FROM agent_memory WHERE agent_id = 'coder';

-- Get agent capacity
SELECT json_extract(memory_json, '$.capacity')
FROM agent_memory WHERE agent_id = 'coder';
```

---

## Useful Queries

### Duration Statistics
```sql
SELECT
  agent_id,
  ROUND(AVG(duration_ms), 2) as avg_ms,
  MIN(duration_ms) as min_ms,
  MAX(duration_ms) as max_ms
FROM receipts
GROUP BY agent_id;
```

### Success Rate
```sql
SELECT
  agent_id,
  ROUND(100.0 * SUM(CASE WHEN status='passed' THEN 1 ELSE 0 END) / COUNT(*), 2) as success_rate
FROM receipts
GROUP BY agent_id;
```

### Files Generated
```sql
SELECT
  agent_id,
  SUM(files_generated) as total_files,
  AVG(files_generated) as avg_per_execution
FROM receipts
WHERE status = 'passed'
GROUP BY agent_id;
```

### Last 24 Hours Activity
```sql
SELECT
  DATE(created_at) as date,
  COUNT(*) as executions,
  SUM(CASE WHEN status='passed' THEN 1 ELSE 0 END) as passed,
  SUM(CASE WHEN status='failed' THEN 1 ELSE 0 END) as failed
FROM receipts
WHERE created_at > datetime('now', '-24 hours')
GROUP BY DATE(created_at);
```

---

## Database Maintenance

### Check Integrity
```bash
sqlite3 .ggen/tier2.db "PRAGMA integrity_check;"
```

### Optimize
```bash
sqlite3 .ggen/tier2.db "PRAGMA optimize;"
```

### Vacuum & Analyze
```bash
sqlite3 .ggen/tier2.db "VACUUM; ANALYZE;"
```

### Backup
```bash
sqlite3 .ggen/tier2.db ".backup '.ggen/tier2-backup.db'"
```

### Export to CSV
```bash
sqlite3 .ggen/tier2.db ".mode csv" ".output receipts.csv" "SELECT * FROM receipts;" ".output stdout"
```

---

## Test Coverage

| Category | Tests | Status |
|----------|-------|--------|
| Schema Structure | 7 | ✅ Pass |
| Table Columns | 15 | ✅ Pass |
| Indexes | 2 | ✅ Pass |
| Constraints | 2 | ✅ Pass |
| Data Types | 2 | ✅ Pass |
| JSON Operations | 2 | ✅ Pass |
| Foreign Keys | 2 | ✅ Pass |
| Views | 3 | ✅ Pass |
| Integration | 1 | ✅ Pass |
| **TOTAL** | **36** | **✅ Pass** |

---

## File Sizes

```
schema.sql          15 KB    - Complete schema definition
init-db.sh          8.7 KB   - Initialization script
test-schema.py      13 KB    - Validation test suite
README.md           25 KB    - Full documentation
QUICK_REFERENCE.md  ~8 KB    - This file

Empty database      32 KB    - Created by schema.sql
With 1k receipts    1 MB     - Typical development
With 100k receipts  50 MB    - Production scale
```

---

## Troubleshooting

### Q: Database locked?
**A**: Enable WAL mode: `PRAGMA journal_mode = WAL;`

### Q: Foreign key error?
**A**: Check: `SELECT COUNT(*) FROM workflow_sessions WHERE session_id = 'your-id';`

### Q: Duplicate execution_id?
**A**: Use INSERT OR REPLACE instead of INSERT

### Q: Slow queries?
**A**: Run: `PRAGMA optimize;` then check index usage

### Q: Need backup?
**A**: Run: `sqlite3 db.sqlite ".backup 'db-backup.sqlite'"`

---

## Next Steps

1. **Initialize**: Run `./init-db.sh`
2. **Validate**: Run `python3 test-schema.py`
3. **Insert Data**: Add receipts and audit logs
4. **Query**: Use views for analytics
5. **Monitor**: Track SLO violations and collisions
6. **Maintain**: Regular VACUUM and ANALYZE

---

**Production-Ready**: ✅ All 36 tests passing
**Last Updated**: 2026-01-29
**Version**: 1.0.0
