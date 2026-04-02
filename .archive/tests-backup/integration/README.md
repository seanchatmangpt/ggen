# P2P Marketplace Integration Tests

## Quick Start

### 1. OTEL Validation (CRITICAL - Run First!)

```bash
# Automated validation script
bash scripts/validate-otel.sh

# Manual validation
docker-compose -f tests/integration/docker-compose.otel-test.yml up -d
cargo test --test otel_validation_tests --features otel -- --ignored --nocapture
```

**Why this matters:** Per project requirements, *"running a CLI help command is a false positive. ONLY TRUST OTEL SPAN/TRACES"*

This validates that traces are **actually emitted** to the OTEL collector, not just that commands run.

### 2. Unit Tests

```bash
cargo test --lib --features p2p,otel
```

### 3. Integration Tests

```bash
cargo test --test p2p_cli_tests --features p2p,otel
cargo test --test p2p_e2e_tests --features p2p,otel
```

## OTEL Test Coverage

**13 comprehensive validation tests:**

1. ✅ Collector infrastructure health
2. ✅ Actual span emission to collector
3. ✅ Parent-child span relationships
4. ✅ Trace context propagation
5. ✅ Service name validation
6. ✅ Span attributes verification
7. ✅ Performance impact measurement
8. ✅ Burst load handling
9. ✅ Jaeger integration
10. ✅ Prometheus metrics
11. ✅ Multi-operation tracing
12. ✅ Error case tracing
13. ✅ End-to-end trace flow

## Viewing Results

### Jaeger UI (Traces)
```bash
open http://localhost:16686
```

Search for service: `ggen-marketplace-p2p`

### Prometheus (Metrics)
```bash
open http://localhost:9090
```

Query: `otelcol_receiver_accepted_spans`

### Collector Metrics
```bash
curl http://localhost:8888/metrics | grep otelcol_receiver_accepted_spans
```

### Validation Report
```bash
cat tests/integration/otel_validation_report.txt
```

## Cleanup

```bash
docker-compose -f tests/integration/docker-compose.otel-test.yml down -v
```

## Troubleshooting

**No spans received:**
1. Verify collector health: `curl http://localhost:13133`
2. Check logs: `docker-compose -f tests/integration/docker-compose.otel-test.yml logs otel-collector`
3. Ensure features enabled: `--features otel`

**Tests hang:**
1. Reduce concurrency: `-- --test-threads=1`
2. Check Docker stats: `docker stats`
3. Increase timeouts in test code

**Docker issues:**
1. Restart Docker daemon
2. Check port conflicts: `lsof -i :4318`
3. Verify compose file: `docker-compose -f tests/integration/docker-compose.otel-test.yml config`

## Architecture

```
┌─────────────────────────────────────────────┐
│  ggen CLI (marketplace search)              │
│  + OTEL instrumentation                     │
└────────────┬────────────────────────────────┘
             │ OTLP HTTP (port 4318)
             ▼
┌─────────────────────────────────────────────┐
│  OTEL Collector                             │
│  - Receives spans via OTLP                  │
│  - Batches and processes                    │
│  - Exports to Jaeger + Prometheus           │
└────────┬───────────────────┬────────────────┘
         │                   │
         ▼                   ▼
┌─────────────────┐   ┌─────────────────┐
│  Jaeger         │   │  Prometheus     │
│  (Trace UI)     │   │  (Metrics)      │
│  :16686         │   │  :9090          │
└─────────────────┘   └─────────────────┘
```

## Test Files

- `otel_validation_tests.rs` - 13 OTEL validation tests (623 LOC)
- `docker-compose.otel-test.yml` - Infrastructure setup
- `otel-collector-config.yaml` - Collector configuration
- `prometheus.yml` - Prometheus scrape config

## Related Documentation

- [Test Execution Plan](../../docs/validation/TEST_EXECUTION_PLAN.md)
- [OTEL Validation Guide](../../scripts/validate-otel.sh)
- [clnrm OTEL Reference](../../../clnrm/.cursor/commands-archive/validate-otel-integration.md)

---

# ggen Integration Tests (Chicago TDD Tools v1.4.0)

Comprehensive integration test suite for testing critical workflows in ggen using chicago-tdd-tools.

## Test Organization

```
tests/
├── integration/
│   ├── lifecycle_tests.rs          # Lifecycle phase execution
│   ├── code_generation_tests.rs    # Template generation workflow
│   ├── cache_tests.rs              # Cache validation
│   ├── package_scoring_tests.rs    # Marketplace scoring
│   └── cross_crate_tests.rs        # Multi-crate coordination
└── common/
    ├── mod.rs                      # Test configuration
    ├── fixtures.rs                 # Test data and fixtures
    └── helpers.rs                  # Helper functions
```

## Running Tests

```bash
# Run all integration tests
cargo test --test lifecycle_tests
cargo test --test code_generation_tests
cargo test --test cache_tests
cargo test --test package_scoring_tests
cargo test --test cross_crate_tests

# Run specific test
cargo test --test lifecycle_tests test_single_phase_execution
```

## Test Coverage (~100 tests, ~2,500 LOC)

### Lifecycle Tests (~25 tests)
- Phase execution, state management, hooks, dependencies
- Performance: < 5s for simple pipeline

### Code Generation Tests (~20 tests)
- Template rendering, file tree generation, RDF integration
- Performance: < 100ms rendering, < 2s file tree

### Cache Tests (~20 tests)
- Pack caching, validation, cleanup, statistics
- Performance: < 500ms write 1MB, < 100ms read

### Package Scoring Tests (~20 tests)
- Marketplace scoring, maturity assessment, filtering
- Performance: < 50ms single, < 1s batch 100

### Cross-Crate Tests (~15 tests)
- ggen-core + ggen-marketplace + ggen-ai integration
- Performance: < 200ms end-to-end workflow

## Why Tests are in `tests/` Directory

Tests are in the `tests/` directory to avoid Result type conflicts with ggen's custom Result type.

**Problem (inline tests):**
```rust
// ❌ Conflict between ggen::Result and std::result::Result
test!(test_example, {
    let result = function()?;
    Ok(())
});
```

**Solution (integration tests):**
```rust
// ✅ Uses standard Rust error handling
use chicago_tdd_tools::prelude::*;
test!(test_example, {
    let result = function()?;
    Ok(())
});
```
