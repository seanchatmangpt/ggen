# KNHK v0.4.0 Implementation Plan

**Status**: Planning  
**Target**: Production Integration & Testing  
**Focus**: 80/20 Critical Path Completion

## Overview

v0.4.0 focuses on **integration, testing, and production readiness** by completing the CLI tool, implementing end-to-end integration tests, adding real network integrations, and ensuring all components work together seamlessly.

## Goals

1. **Complete CLI Tool** - Production-ready command-line interface
2. **End-to-End Integration** - Full pipeline from connector to lockchain
3. **Real Network Integrations** - HTTP/gRPC for emit stage, OTEL exporters
4. **Production Configuration** - Configuration management and deployment features
5. **Comprehensive Testing** - Integration tests, E2E tests, performance validation

## Phase 1: CLI Tool Completion ✅ (HIGH PRIORITY)

### 1.1 Hook Commands
- **File**: `rust/knhk-cli/src/main.rs`
- **Tasks**:
  - `hook list` - List all registered hooks using `knhk-hot` FFI
  - `hook create` - Create new hook with validation
  - `hook eval` - Evaluate hook and display receipt
  - `hook show` - Display hook details (IR, metadata, receipts)
- **Dependencies**: `knhk-hot`, `knhk-etl` (for hook registration)
- **Testing**: Unit tests for each command, integration tests with real hooks

### 1.2 Connector Commands
- **File**: `rust/knhk-cli/src/main.rs`
- **Tasks**:
  - `connector list` - List all registered connectors with status
  - `connector create` - Create connector with schema validation
  - `connector fetch` - Fetch delta from connector (with circuit breaker)
  - `connector status` - Show connector health, metrics, circuit breaker state
- **Dependencies**: `knhk-connectors`
- **Testing**: Tests with Kafka/Salesforce connectors

### 1.3 Receipt Commands
- **File**: `rust/knhk-cli/src/main.rs`
- **Tasks**:
  - `receipt list` - List receipts from lockchain (with pagination)
  - `receipt show` - Display receipt details (ticks, span_id, a_hash)
  - `receipt verify` - Verify receipt integrity (Merkle tree verification)
  - `receipt merge` - Merge multiple receipts (⊕ operation)
- **Dependencies**: `knhk-lockchain`, `knhk-etl`
- **Testing**: Tests with real receipts, Merkle verification

### 1.4 Pipeline Commands
- **File**: `rust/knhk-cli/src/main.rs`
- **Tasks**:
  - `pipeline run` - Execute full ETL pipeline (Ingest → Emit)
  - `pipeline status` - Show pipeline execution status and metrics
- **Dependencies**: `knhk-etl`, `knhk-connectors`, `knhk-lockchain`
- **Testing**: E2E tests with real connectors

### 1.5 Epoch Commands
- **File**: `rust/knhk-cli/src/main.rs`
- **Tasks**:
  - `epoch create` - Create new epoch with Λ ordering
  - `epoch run` - Execute epoch (all hooks in deterministic order)
  - `epoch list` - List epochs with execution status
- **Dependencies**: Erlang `knhk_epoch` module (via FFI or IPC)
- **Testing**: Integration tests with Erlang layer

### 1.6 Context Commands
- **File**: `rust/knhk-cli/src/main.rs`
- **Tasks**:
  - `context list` - List all contexts
  - `context current` - Show current context
  - `context create` - Create new context with schema
  - `context use` - Switch to different context
- **Dependencies**: Context management (local file-based or Erlang)
- **Testing**: Tests for context switching and isolation

### 1.7 CLI Features
- **Error Handling**: Proper error messages, exit codes
- **Output Formatting**: JSON, YAML, table formats
- **Configuration**: `~/.knhk/config.toml` for default settings
- **Logging**: Structured logging with OTEL integration
- **Progress Indicators**: For long-running operations
- **Color Support**: Terminal colors for better UX (optional)

## Phase 2: End-to-End Integration ✅ (HIGH PRIORITY)

### 2.1 Integrated Pipeline Test
- **File**: `tests/chicago_integration_e2e.c` (new)
- **Scenario**: Full pipeline from Kafka → Transform → Load → Reflex → Emit
- **Steps**:
  1. Register Kafka connector
  2. Fetch delta from Kafka
  3. Transform triples (IRI hashing, schema validation)
  4. Load into SoA arrays
  5. Execute hooks (hot path)
  6. Generate receipts
  7. Write to lockchain
  8. Verify receipts in lockchain
- **Assertions**: Receipt integrity, tick budget compliance, provenance hash

### 2.2 Connector → ETL Integration
- **File**: `rust/knhk-etl/src/integration.rs` (enhance existing)
- **Tasks**:
  - Wire connectors to ingest stage
  - Handle connector errors gracefully
  - Circuit breaker integration
  - Metrics collection throughout pipeline
- **Testing**: Integration tests with mock connectors

### 2.3 Erlang ↔ Rust Integration
- **Tasks**:
  - FFI bindings for Erlang → Rust calls
  - Or: IPC/NIF for Erlang ↔ Rust communication
  - Schema registry integration (`knhk_sigma`)
  - Invariant registry integration (`knhk_q`)
  - Lockchain integration (`knhk_lockchain`)
- **Testing**: Tests for Erlang supervision tree ↔ Rust components

### 2.4 Receipt → Lockchain Integration
- **Tasks**:
  - Verify receipt writing to lockchain
  - Merkle tree construction
  - Git commit integration (if git2 available)
  - Receipt querying from lockchain
- **Testing**: Tests with real receipts and lockchain

## Phase 3: Real Network Integrations ✅ (MEDIUM PRIORITY)

### 3.1 HTTP Client for Emit Stage
- **File**: `rust/knhk-etl/src/lib.rs` (EmitStage)
- **Tasks**:
  - Implement `send_action_to_endpoint()` with reqwest
  - Retry logic with exponential backoff
  - Timeout handling
  - Authentication support (Bearer token, API key)
  - Error handling and logging
- **Testing**: Tests with mock HTTP server

### 3.2 gRPC Client for Emit Stage
- **File**: `rust/knhk-etl/src/lib.rs` (EmitStage)
- **Tasks**:
  - Implement gRPC client using `tonic`
  - Action serialization (protobuf)
  - Connection pooling
  - Error handling
- **Testing**: Tests with mock gRPC server

### 3.3 Kafka Producer for Emit Stage
- **File**: `rust/knhk-etl/src/lib.rs` (EmitStage)
- **Tasks**:
  - Implement Kafka producer using rdkafka
  - Action serialization (JSON-LD or protobuf)
  - Topic configuration
  - Error handling and retries
- **Dependencies**: `rdkafka` (producer support)
- **Testing**: Tests with embedded Kafka or test container

### 3.4 OTEL Exporter Integration
- **File**: `rust/knhk-etl/src/lib.rs` (ReflexStage)
- **Tasks**:
  - Export spans to OTEL collector (OTLP/gRPC or HTTP)
  - Export metrics to OTEL collector
  - Batch export for performance
  - Error handling and retries
- **Dependencies**: `opentelemetry` SDK, `opentelemetry-otlp`
- **Testing**: Tests with mock OTEL collector

## Phase 4: Production Configuration ✅ (MEDIUM PRIORITY)

### 4.1 Configuration Management
- **File**: `rust/knhk-config/src/lib.rs` (new crate)
- **Tasks**:
  - Configuration file parsing (TOML, YAML, JSON)
  - Environment variable support
  - Default configuration
  - Configuration validation
  - Schema for configuration (connectors, epochs, hooks, etc.)
- **Structure**:
  ```toml
  [connectors.kafka]
  bootstrap_servers = ["localhost:9092"]
  topic = "triples"
  schema = "http://example.org/schema"
  
  [epochs.default]
  max_ticks = 8
  ordering = "deterministic"
  
  [hooks]
  max_count = 100
  ```

### 4.2 Logging Infrastructure
- **File**: `rust/knhk-logging/src/lib.rs` (new crate)
- **Tasks**:
  - Structured logging (JSON format)
  - Log levels (trace, debug, info, warn, error)
  - OTEL log correlation
  - File and console output
  - Log rotation
- **Dependencies**: `tracing`, `tracing-opentelemetry`

### 4.3 Metrics Collection
- **File**: `rust/knhk-metrics/src/lib.rs` (new crate)
- **Tasks**:
  - Counter metrics (hook executions, connector fetches)
  - Histogram metrics (latency, tick distribution)
  - Gauge metrics (circuit breaker state, queue depth)
  - OTEL metric export
- **Dependencies**: `opentelemetry`, `opentelemetry-metrics`

### 4.4 Health Checks
- **File**: `rust/knhk-health/src/lib.rs` (new crate)
- **Tasks**:
  - Health check endpoint (HTTP)
  - Component health checks (connectors, lockchain, etc.)
  - Liveness and readiness probes
  - Health check aggregation
- **Testing**: Tests for health check endpoints

## Phase 5: Enhanced RDF Parsing ✅ (LOW PRIORITY)

### 5.1 Complete RDF/Turtle Parser
- **File**: `rust/knhk-etl/src/lib.rs` (IngestStage)
- **Tasks**:
  - Full Turtle syntax support (not just basic)
  - Prefix resolution
  - Blank node handling
  - Base URI resolution
  - Error reporting with line numbers
- **Options**: Use existing crate (`rio_turtle`) or improve current parser
- **Testing**: Tests with complex Turtle files

### 5.2 Complete JSON-LD Parser
- **File**: `rust/knhk-etl/src/lib.rs` (IngestStage)
- **Tasks**:
  - Full JSON-LD expansion
  - Context resolution
  - Frame support
  - Error handling
- **Dependencies**: `json-ld` crate or similar
- **Testing**: Tests with various JSON-LD documents

### 5.3 RDF Format Detection
- **File**: `rust/knhk-etl/src/lib.rs` (IngestStage)
- **Tasks**:
  - Auto-detect format (Turtle, JSON-LD, N-Triples)
  - Content-Type header parsing
  - File extension detection
  - Magic number detection
- **Testing**: Tests with various formats

## Phase 6: Enhanced Testing ✅ (HIGH PRIORITY)

### 6.1 Integration Test Suite
- **File**: `tests/chicago_integration_suite.c`
- **Scenarios**:
  - Connector → ETL → Lockchain flow
  - Hook execution with real receipts
  - Receipt merging and verification
  - Circuit breaker behavior
  - Error handling and recovery
- **Framework**: Chicago TDD pattern (existing test structure)

### 6.2 Performance Validation Tests
- **File**: `tests/chicago_performance_validation.c`
- **Tasks**:
  - Verify ≤8 ticks for all hot path operations
  - Measure p50, p95, p99 latencies
  - Validate Chatman Constant compliance
  - Test with various data sizes
  - Cache warming verification
- **Assertions**: All operations ≤8 ticks p95

### 6.3 E2E Test Suite
- **File**: `tests/e2e/` (new directory)
- **Scenarios**:
  - Full pipeline with Kafka connector
  - Full pipeline with Salesforce connector
  - Multi-connector pipeline
  - Error recovery scenarios
  - Lockchain verification
- **Framework**: Test containers or embedded services

### 6.4 Property-Based Tests
- **File**: `rust/knhk-etl/tests/property.rs` (new)
- **Tasks**:
  - Receipt merging properties (associative, commutative)
  - IRI hashing properties (deterministic, collision-resistant)
  - SoA alignment properties (64-byte alignment)
  - Guard constraint properties (max_run_len ≤ 8)
- **Framework**: `proptest` or `quickcheck`

## Phase 7: Documentation & Examples ✅ (MEDIUM PRIORITY)

### 7.1 CLI Documentation
- **File**: `docs/cli.md`
- **Content**:
  - Command reference
  - Examples for each command
  - Configuration guide
  - Troubleshooting guide

### 7.2 Integration Guide
- **File**: `docs/integration.md`
- **Content**:
  - End-to-end integration examples
  - Connector development guide
  - Hook development guide
  - ETL pipeline configuration

### 7.3 Deployment Guide
- **File**: `docs/deployment.md`
- **Content**:
  - Docker deployment
  - Kubernetes deployment
  - Configuration management
  - Monitoring and observability setup
  - Health check configuration

### 7.4 Examples Directory
- **Directory**: `examples/`
- **Examples**:
  - Basic hook execution
  - Kafka connector setup
  - ETL pipeline execution
  - Receipt verification
  - CLI usage examples

## 80/20 Priority Summary

### Critical Path (80% Value):
1. ✅ **CLI Tool Completion** - Essential for usability
2. ✅ **End-to-End Integration** - Validates full system
3. ✅ **Enhanced Testing** - Ensures reliability
4. ✅ **Real Network Integrations** - Production readiness

### Lower Priority (20% Value):
5. ⚠️ **Enhanced RDF Parsing** - Can use existing libraries
6. ⚠️ **Production Configuration** - Basic config sufficient
7. ⚠️ **Documentation** - Incremental improvements

## Success Criteria

v0.4.0 is complete when:
- ✅ All CLI commands implemented and tested
- ✅ End-to-end integration tests passing
- ✅ Real network integrations working (HTTP, gRPC, Kafka, OTEL)
- ✅ Performance validation confirms ≤8 ticks compliance
- ✅ Configuration management in place
- ✅ Documentation updated with examples

## Estimated Effort

- **Phase 1 (CLI)**: 2-3 weeks
- **Phase 2 (E2E Integration)**: 1-2 weeks
- **Phase 3 (Network Integrations)**: 1-2 weeks
- **Phase 4 (Configuration)**: 1 week
- **Phase 5 (RDF Parsing)**: 1 week (optional)
- **Phase 6 (Testing)**: 1-2 weeks
- **Phase 7 (Documentation)**: 1 week

**Total**: 8-12 weeks for full implementation

## Dependencies

### New Crates Needed:
- `knhk-config` - Configuration management
- `knhk-logging` - Structured logging
- `knhk-metrics` - Metrics collection
- `knhk-health` - Health checks

### External Dependencies:
- `reqwest` - HTTP client (already added)
- `tonic` - gRPC client
- `opentelemetry-otlp` - OTEL exporter
- `tracing` - Structured logging
- `toml` or `serde_yaml` - Configuration parsing
- `proptest` or `quickcheck` - Property-based testing

## Risks & Mitigations

### Risk 1: Erlang ↔ Rust Integration Complexity
- **Mitigation**: Start with simple IPC (HTTP or Unix sockets), add FFI later

### Risk 2: Network Integration Latency
- **Mitigation**: Use async/await, connection pooling, batch operations

### Risk 3: Configuration Complexity
- **Mitigation**: Start with simple TOML config, add advanced features incrementally

### Risk 4: Testing Coverage
- **Mitigation**: Focus on critical paths first, add comprehensive tests incrementally

## Next Steps After v0.4.0

- v0.5.0: Advanced features (multi-shard, replication, distributed lockchain)
- v1.0.0: Production release with full feature set

