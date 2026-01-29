-- ============================================================================
-- ggen Tier 2 Persistence: SQLite Database Schema
-- ============================================================================
-- Purpose: Production-ready schema for receipt, memory, and audit storage
-- Version: 1.0.0
-- Created: 2026-01-29
-- ============================================================================

-- Enable foreign keys (required for SQLite)
PRAGMA foreign_keys = ON;

-- ============================================================================
-- TABLE: receipts
-- ============================================================================
-- Purpose: Store execution receipts from the five-stage ggen pipeline (μ₁-μ₅)
-- Description: Records deterministic execution outcomes with cryptographic hashes
--
CREATE TABLE IF NOT EXISTS receipts (
  -- Primary key
  id INTEGER PRIMARY KEY AUTOINCREMENT,

  -- Execution identification (unique per ggen sync)
  execution_id TEXT NOT NULL UNIQUE,

  -- Agent identification (from swarm orchestration)
  agent_id TEXT NOT NULL,

  -- Operation type performed
  operation TEXT NOT NULL CHECK(
    operation IN ('validation', 'generation', 'normalization', 'extraction',
                  'emission', 'canonicalization', 'receipt', 'dry_run', 'watch')
  ),

  -- Status of execution
  status TEXT NOT NULL CHECK(status IN ('passed', 'failed', 'partial', 'cancelled')),

  -- Timestamps (ISO8601 format)
  timestamp TEXT NOT NULL,  -- When the operation executed
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,

  -- Cryptographic hashes (SHA-256)
  manifest_hash TEXT,       -- Manifest fingerprint (if applicable)
  ontology_hash TEXT,       -- RDF ontology fingerprint (if applicable)

  -- Execution metrics
  files_generated INTEGER DEFAULT 0,
  files_modified INTEGER DEFAULT 0,
  duration_ms INTEGER NOT NULL,  -- Time in milliseconds

  -- Full receipt JSON (structured data for later analysis)
  receipt_json TEXT,        -- JSON blob with complete execution details

  -- Error tracking (populated if status = 'failed')
  error_message TEXT,

  -- Additional metadata
  environment TEXT          -- dev/staging/production
);

-- Index: Fast lookup by agent and time (primary query pattern)
CREATE INDEX IF NOT EXISTS idx_receipts_agent_timestamp
  ON receipts(agent_id, timestamp DESC);

-- Index: Fast lookup by execution_id
CREATE INDEX IF NOT EXISTS idx_receipts_execution_id
  ON receipts(execution_id);

-- Index: Status queries for monitoring dashboards
CREATE INDEX IF NOT EXISTS idx_receipts_status
  ON receipts(status, created_at DESC);

-- Index: Time-based queries for audit trails
CREATE INDEX IF NOT EXISTS idx_receipts_created_at
  ON receipts(created_at DESC);

-- ============================================================================
-- TABLE: agent_memory
-- ============================================================================
-- Purpose: Store agent state, decisions, and collaboration history
-- Description: Per-agent memory for state persistence across executions
--
CREATE TABLE IF NOT EXISTS agent_memory (
  -- Primary key
  id INTEGER PRIMARY KEY AUTOINCREMENT,

  -- Agent identification (one row per agent)
  agent_id TEXT NOT NULL UNIQUE,

  -- Agent state (JSON: context, decisions, learned patterns)
  memory_json TEXT NOT NULL,

  -- Collision history (JSON: conflicts detected in parallel execution)
  collision_history TEXT,

  -- Retry tracking
  retry_count INTEGER DEFAULT 0,

  -- Last error encountered
  last_error TEXT,

  -- Timestamps
  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- Index: Agent lookups by ID
CREATE INDEX IF NOT EXISTS idx_agent_memory_agent_id
  ON agent_memory(agent_id);

-- Index: Time-based queries for memory management
CREATE INDEX IF NOT EXISTS idx_agent_memory_updated_at
  ON agent_memory(updated_at DESC);

-- ============================================================================
-- TABLE: audit_log
-- ============================================================================
-- Purpose: Comprehensive audit trail for compliance and debugging
-- Description: Detailed log of all operations for traceability and forensics
--
CREATE TABLE IF NOT EXISTS audit_log (
  -- Primary key
  id INTEGER PRIMARY KEY AUTOINCREMENT,

  -- Timestamp (ISO8601)
  timestamp TEXT NOT NULL,

  -- Agent identification
  agent_id TEXT NOT NULL,

  -- Operation description (detailed)
  operation TEXT NOT NULL,

  -- Operation status
  status TEXT NOT NULL CHECK(status IN ('started', 'completed', 'failed', 'skipped')),

  -- Execution duration
  duration_ms INTEGER,

  -- Error information (if status = 'failed')
  error_message TEXT,

  -- Additional context (JSON for flexible data)
  context_json TEXT,

  -- Record creation time
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- Index: Time-based queries (most common pattern for audit trails)
CREATE INDEX IF NOT EXISTS idx_audit_log_timestamp
  ON audit_log(timestamp DESC);

-- Index: Agent-based queries for per-agent audit trails
CREATE INDEX IF NOT EXISTS idx_audit_log_agent_id
  ON audit_log(agent_id, timestamp DESC);

-- Index: Status queries for monitoring
CREATE INDEX IF NOT EXISTS idx_audit_log_status
  ON audit_log(status, created_at DESC);

-- ============================================================================
-- TABLE: workflow_sessions
-- ============================================================================
-- Purpose: Track multi-agent workflow orchestration sessions
-- Description: Aggregates metrics across parallel agent execution
--
CREATE TABLE IF NOT EXISTS workflow_sessions (
  -- Primary key
  id INTEGER PRIMARY KEY AUTOINCREMENT,

  -- Session identification (unique per workflow run)
  session_id TEXT NOT NULL UNIQUE,

  -- Session lifecycle
  start_time TIMESTAMP NOT NULL,
  end_time TIMESTAMP,

  -- Status tracking
  status TEXT NOT NULL CHECK(status IN ('running', 'completed', 'failed', 'cancelled')),

  -- Metrics
  agent_count INTEGER NOT NULL,           -- Number of agents in session
  total_duration_ms INTEGER,              -- Total execution time
  successful_operations INTEGER DEFAULT 0,
  failed_operations INTEGER DEFAULT 0,

  -- Session context (JSON: orchestration config, parameters)
  context_json TEXT,

  -- Record creation time
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- Index: Session lookup
CREATE INDEX IF NOT EXISTS idx_workflow_sessions_session_id
  ON workflow_sessions(session_id);

-- Index: Status and time queries
CREATE INDEX IF NOT EXISTS idx_workflow_sessions_status
  ON workflow_sessions(status, created_at DESC);

-- Index: Time-based queries
CREATE INDEX IF NOT EXISTS idx_workflow_sessions_created_at
  ON workflow_sessions(created_at DESC);

-- ============================================================================
-- TABLE: collision_detection
-- ============================================================================
-- Purpose: Track conflicts detected during parallel agent execution
-- Description: Stores collision metadata for root cause analysis
--
CREATE TABLE IF NOT EXISTS collision_detection (
  -- Primary key
  id INTEGER PRIMARY KEY AUTOINCREMENT,

  -- Session that detected collision
  session_id TEXT NOT NULL,

  -- Agents involved in collision
  agent_id_1 TEXT NOT NULL,
  agent_id_2 TEXT NOT NULL,

  -- Collision details
  collision_type TEXT NOT NULL CHECK(
    collision_type IN ('file_conflict', 'memory_conflict', 'semantic_overlap', 'structural_overlap')
  ),

  -- Description of collision
  description TEXT NOT NULL,

  -- Resolution applied (if any)
  resolution TEXT,

  -- Severity level
  severity TEXT NOT NULL CHECK(severity IN ('info', 'warning', 'critical')),

  -- Collision detection timestamp
  timestamp TEXT NOT NULL,
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,

  -- Foreign key reference
  FOREIGN KEY(session_id) REFERENCES workflow_sessions(session_id)
);

-- Index: Session-based collision queries
CREATE INDEX IF NOT EXISTS idx_collision_detection_session_id
  ON collision_detection(session_id, timestamp DESC);

-- Index: Agent-based collision queries
CREATE INDEX IF NOT EXISTS idx_collision_detection_agents
  ON collision_detection(agent_id_1, agent_id_2);

-- Index: Severity queries for alerting
CREATE INDEX IF NOT EXISTS idx_collision_detection_severity
  ON collision_detection(severity, created_at DESC);

-- ============================================================================
-- TABLE: pipeline_metrics
-- ============================================================================
-- Purpose: Track performance metrics for the five-stage ggen pipeline
-- Description: Stores μ₁-μ₅ stage timings for performance analysis
--
CREATE TABLE IF NOT EXISTS pipeline_metrics (
  -- Primary key
  id INTEGER PRIMARY KEY AUTOINCREMENT,

  -- Execution identification (links to receipts)
  execution_id TEXT NOT NULL,

  -- Pipeline stage
  stage TEXT NOT NULL CHECK(stage IN ('μ₁_normalize', 'μ₂_extract', 'μ₃_emit',
                                       'μ₄_canonicalize', 'μ₅_receipt')),

  -- Timing (milliseconds)
  duration_ms INTEGER NOT NULL,

  -- Input/output metrics
  input_size_bytes INTEGER,
  output_size_bytes INTEGER,

  -- Status
  status TEXT NOT NULL CHECK(status IN ('passed', 'failed')),

  -- Error information (if failed)
  error_message TEXT,

  -- Timestamp
  timestamp TEXT NOT NULL,
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,

  -- Foreign key reference
  FOREIGN KEY(execution_id) REFERENCES receipts(execution_id)
);

-- Index: Execution-based metrics queries
CREATE INDEX IF NOT EXISTS idx_pipeline_metrics_execution_id
  ON pipeline_metrics(execution_id);

-- Index: Stage performance analysis
CREATE INDEX IF NOT EXISTS idx_pipeline_metrics_stage
  ON pipeline_metrics(stage, duration_ms DESC);

-- Index: Time-based queries
CREATE INDEX IF NOT EXISTS idx_pipeline_metrics_timestamp
  ON pipeline_metrics(timestamp DESC);

-- ============================================================================
-- TABLE: slo_violations
-- ============================================================================
-- Purpose: Track SLO (Service Level Objective) violations
-- Description: Records when performance targets are not met
--
CREATE TABLE IF NOT EXISTS slo_violations (
  -- Primary key
  id INTEGER PRIMARY KEY AUTOINCREMENT,

  -- Execution that violated SLO
  execution_id TEXT NOT NULL,

  -- SLO metric name
  metric TEXT NOT NULL,

  -- Target and actual values
  target_value REAL NOT NULL,
  actual_value REAL NOT NULL,

  -- Severity
  severity TEXT NOT NULL CHECK(severity IN ('warning', 'critical')),

  -- Description
  description TEXT,

  -- Timestamp
  timestamp TEXT NOT NULL,
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,

  -- Foreign key reference
  FOREIGN KEY(execution_id) REFERENCES receipts(execution_id)
);

-- Index: Execution-based SLO queries
CREATE INDEX IF NOT EXISTS idx_slo_violations_execution_id
  ON slo_violations(execution_id);

-- Index: Metric-based aggregation
CREATE INDEX IF NOT EXISTS idx_slo_violations_metric
  ON slo_violations(metric, created_at DESC);

-- Index: Severity alerting
CREATE INDEX IF NOT EXISTS idx_slo_violations_severity
  ON slo_violations(severity, created_at DESC);

-- ============================================================================
-- View: recent_executions
-- ============================================================================
-- Purpose: Quick view of recent execution results with summary stats
--
CREATE VIEW IF NOT EXISTS recent_executions AS
SELECT
  r.execution_id,
  r.agent_id,
  r.operation,
  r.status,
  r.timestamp,
  r.files_generated,
  r.files_modified,
  r.duration_ms,
  COUNT(DISTINCT al.id) as audit_entries,
  (SELECT COUNT(*) FROM slo_violations WHERE execution_id = r.execution_id) as slo_violations
FROM receipts r
LEFT JOIN audit_log al ON al.timestamp >= r.timestamp AND al.timestamp <= datetime(r.timestamp, '+' || r.duration_ms || ' milliseconds')
GROUP BY r.execution_id
ORDER BY r.created_at DESC
LIMIT 100;

-- ============================================================================
-- View: agent_performance_summary
-- ============================================================================
-- Purpose: Aggregate performance statistics per agent
--
CREATE VIEW IF NOT EXISTS agent_performance_summary AS
SELECT
  agent_id,
  COUNT(*) as total_executions,
  SUM(CASE WHEN status = 'passed' THEN 1 ELSE 0 END) as successful_executions,
  SUM(CASE WHEN status = 'failed' THEN 1 ELSE 0 END) as failed_executions,
  ROUND(AVG(duration_ms), 2) as avg_duration_ms,
  MIN(duration_ms) as min_duration_ms,
  MAX(duration_ms) as max_duration_ms,
  SUM(files_generated) as total_files_generated,
  MAX(timestamp) as last_execution
FROM receipts
GROUP BY agent_id
ORDER BY total_executions DESC;

-- ============================================================================
-- View: pipeline_stage_performance
-- ============================================================================
-- Purpose: Analyze performance per pipeline stage
--
CREATE VIEW IF NOT EXISTS pipeline_stage_performance AS
SELECT
  stage,
  COUNT(*) as executions,
  ROUND(AVG(duration_ms), 2) as avg_duration_ms,
  MIN(duration_ms) as min_duration_ms,
  MAX(duration_ms) as max_duration_ms,
  SUM(CASE WHEN status = 'passed' THEN 1 ELSE 0 END) as passed,
  SUM(CASE WHEN status = 'failed' THEN 1 ELSE 0 END) as failed
FROM pipeline_metrics
GROUP BY stage
ORDER BY avg_duration_ms DESC;

-- ============================================================================
-- Initialization: Default Data
-- ============================================================================

-- Initialize default values (optional - uncomment if needed)
-- INSERT OR IGNORE INTO receipts (execution_id, agent_id, operation, status, timestamp, duration_ms)
-- VALUES ('init-001', 'system', 'initialization', 'passed', datetime('now'), 0);

-- ============================================================================
-- Schema Information
-- ============================================================================
-- Tables: 7 (receipts, agent_memory, audit_log, workflow_sessions,
--           collision_detection, pipeline_metrics, slo_violations)
-- Views: 3 (recent_executions, agent_performance_summary, pipeline_stage_performance)
-- Indexes: 20+ for optimal query performance
-- Foreign Keys: 2 (collision_detection → workflow_sessions, pipeline_metrics → receipts, slo_violations → receipts)
-- ============================================================================
