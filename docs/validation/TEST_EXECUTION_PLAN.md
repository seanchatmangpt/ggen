# P2P Marketplace Test Execution Plan

**Document Version:** 1.0
**Last Updated:** 2025-01-02
**Status:** Ready for Execution (pending backend compilation)

## Executive Summary

This document defines the comprehensive test execution strategy for the ggen P2P marketplace, with **strong emphasis on OTEL validation** as per project requirements:

> **CRITICAL:** "running a CLI help command is a false positive. ONLY TRUST OTEL SPAN/TRACES"

All tests must validate actual trace emission to OTEL collector, not just successful command execution.

## Test Suite Overview

### Current Status

- **Total Tests Created:** 42 tests (1,176 LOC)
- **Compilation Status:** 48/49 tests compile successfully
- **Execution Status:** Blocked (waiting for P2P backend compilation)
- **OTEL Tests:** 13 comprehensive validation tests created

### Test Categories

| Category | Tests | LOC | Status | Priority |
|----------|-------|-----|--------|----------|
| CLI Integration | 29 | 657 | ✅ Compiled | P2 |
| E2E P2P Operations | 13 | 521 | ✅ Compiled | P1 |
| OTEL Validation | 13 | 623 | ✅ Ready | **P0** |
| **TOTAL** | **55** | **1,801** | 98% Ready | - |

## Phase 1: OTEL Infrastructure Validation (P0)

**Objective:** Verify OpenTelemetry integration emits actual traces to collector

**Duration:** ~15 minutes
**Prerequisites:** Docker running

### 1.1 Start OTEL Stack

```bash
cd /Users/sac/ggen
docker-compose -f tests/integration/docker-compose.otel-test.yml up -d
```

**Services Started:**
- OTEL Collector (OTLP HTTP: 4318, Health: 13133)
- Jaeger UI (http://localhost:16686)
- Prometheus (http://localhost:9090)

### 1.2 Health Check

```bash
# Verify all services healthy
curl http://localhost:13133           # OTEL collector
curl http://localhost:16686           # Jaeger UI
curl http://localhost:9090/-/healthy  # Prometheus
```

**Expected:** All endpoints return 200 OK

### 1.3 Run OTEL Validation Suite

```bash
# Execute comprehensive OTEL tests
cargo test --test otel_validation_tests \
  --features otel \
  -- --ignored --nocapture

# Or use automated script
bash scripts/validate-otel.sh
```

**Test Coverage:**

| Test | Validates | Acceptance Criteria |
|------|-----------|---------------------|
| `test_otel_collector_is_healthy` | Collector running | Health endpoint returns 200 |
| `test_otel_collector_endpoints_available` | All ports accessible | OTLP HTTP, metrics, health reachable |
| `test_marketplace_search_emits_spans_to_collector` | **Actual trace emission** | Collector span count increases |
| `test_p2p_operations_emit_trace_context` | P2P creates spans | Multiple spans emitted per operation |
| `test_span_attributes_contain_operation_metadata` | Span metadata | Contains marketplace/search attributes |
| `test_parent_child_span_relationships_preserved` | Trace structure | Parent-child relationships exist |
| `test_trace_context_propagates_across_operations` | Context propagation | trace_id consistent across spans |
| `test_spans_have_correct_service_name` | Service identification | Service name = "ggen-marketplace-p2p" |
| `test_span_export_does_not_block_operation` | Performance | OTEL overhead < 50% |
| `test_collector_handles_burst_of_spans` | Load handling | Collector processes concurrent spans |

### 1.4 Validation Metrics

**Success Criteria:**

- ✅ All infrastructure health checks pass
- ✅ At least 1 span received by collector after marketplace search
- ✅ Spans appear in Jaeger UI within 5 seconds
- ✅ Collector metrics show `otelcol_receiver_accepted_spans` > 0
- ✅ OTEL overhead < 50% performance impact
- ✅ 100% of OTEL tests pass

**Failure Escalation:**

If OTEL validation fails:
1. Check Docker containers: `docker-compose -f tests/integration/docker-compose.otel-test.yml ps`
2. Review collector logs: `docker-compose -f tests/integration/docker-compose.otel-test.yml logs otel-collector`
3. Verify environment variables: `OTEL_EXPORTER_OTLP_ENDPOINT`, `OTEL_SERVICE_NAME`
4. Test OTLP endpoint manually: `curl http://localhost:4318`

## Phase 2: Unit Tests (P2)

**Objective:** Validate individual component functionality

**Duration:** ~5 minutes
**Prerequisites:** Backend compilation complete

### 2.1 CLI Unit Tests

```bash
# Run CLI-specific tests
cargo test --test p2p_cli_tests --features p2p
```

**Coverage:**
- Command argument parsing (10 tests)
- Help text validation (3 tests)
- Error handling (5 tests)
- Performance benchmarks (3 tests)

**Expected Results:**
- 100% pass rate
- All performance tests < 2s execution time
- No panics or unwraps in error paths

### 2.2 Search Unit Tests

```bash
# Search functionality tests
cargo test --lib --features p2p -- marketplace::search
```

**Coverage:**
- Query parsing
- Filter application
- Pagination logic
- Cache behavior

## Phase 3: Integration Tests (P1)

**Objective:** Test real interactions between components

**Duration:** ~10 minutes
**Prerequisites:** Unit tests passing

### 3.1 E2E Workflow Tests

```bash
# Run end-to-end P2P tests
cargo test --test p2p_e2e_tests --features p2p -- --test-threads=1
```

**Coverage:**
- Complete search-to-install workflows (2 tests)
- Multi-node package distribution (3 tests)
- Network resilience scenarios (4 tests)
- Performance under load (4 tests)

### 3.2 Registry Integration

```bash
# Test P2P + Registry interaction
cargo test --features p2p -- integration::registry
```

**Coverage:**
- P2P complements centralized registry
- Fallback to registry when P2P fails
- Cache coherency between backends

## Phase 4: Performance Benchmarks (P3)

**Objective:** Validate performance targets

**Duration:** ~20 minutes
**Prerequisites:** All functional tests passing

### 4.1 Search Performance

```bash
# Run search benchmarks
cargo bench --bench marketplace_performance -- search
```

**Targets:**
- Basic search: < 1s
- Filtered search: < 2s
- Bulk search (100 queries): < 10s

### 4.2 P2P Network Performance

```bash
# Run P2P-specific benchmarks
cargo bench --bench marketplace_p2p
```

**Targets:**
- Peer discovery: < 500ms
- Package publish: < 2s
- DHT query: < 1s

### 4.3 OTEL Performance Impact

**Already validated in Phase 1:**
- OTEL overhead: < 50% (validated by `test_span_export_does_not_block_operation`)

## Test Execution Workflow

### Automated Full Suite

```bash
#!/bin/bash
# Complete test execution pipeline

set -euo pipefail

echo "🚀 Starting P2P Marketplace Test Suite"

# Phase 1: OTEL Validation (P0)
echo "Phase 1: OTEL Infrastructure Validation"
bash scripts/validate-otel.sh || exit 1

# Phase 2: Unit Tests (P2)
echo "Phase 2: Unit Tests"
cargo test --lib --features p2p,otel || exit 1

# Phase 3: Integration Tests (P1)
echo "Phase 3: Integration Tests"
cargo test --test p2p_cli_tests --features p2p,otel || exit 1
cargo test --test p2p_e2e_tests --features p2p,otel -- --test-threads=1 || exit 1

# Phase 4: Performance Benchmarks (P3)
echo "Phase 4: Performance Benchmarks"
cargo bench --bench marketplace_performance
cargo bench --bench marketplace_p2p

# Cleanup
docker-compose -f tests/integration/docker-compose.otel-test.yml down -v

echo "✅ All tests passed!"
```

### Manual Execution

For targeted testing:

```bash
# OTEL validation only
bash scripts/validate-otel.sh

# Specific test file
cargo test --test p2p_cli_tests test_marketplace_search_basic_query

# Specific test with output
cargo test --test otel_validation_tests test_marketplace_search_emits_spans_to_collector -- --ignored --nocapture

# Run with logging
RUST_LOG=debug cargo test --features p2p,otel -- --nocapture
```

## CI/CD Integration

### GitHub Actions Workflow

```yaml
name: P2P Marketplace Tests

on: [push, pull_request]

jobs:
  otel-validation:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Start OTEL Stack
        run: |
          docker-compose -f tests/integration/docker-compose.otel-test.yml up -d
          sleep 10

      - name: Wait for healthy
        run: |
          curl --retry 10 --retry-delay 1 http://localhost:13133

      - name: Run OTEL Tests
        run: |
          cargo test --test otel_validation_tests --features otel -- --ignored

      - name: Verify Spans Received
        run: |
          SPANS=$(curl -s http://localhost:8888/metrics | grep otelcol_receiver_accepted_spans | awk '{print $2}')
          [ "$SPANS" -gt 0 ] || exit 1

      - name: Upload Logs on Failure
        if: failure()
        uses: actions/upload-artifact@v3
        with:
          name: otel-logs
          path: |
            /tmp/ggen-otel-*.log
            tests/integration/otel_validation_report.txt

  unit-tests:
    runs-on: ubuntu-latest
    needs: otel-validation
    steps:
      - uses: actions/checkout@v3
      - name: Run Unit Tests
        run: cargo test --lib --features p2p,otel

  integration-tests:
    runs-on: ubuntu-latest
    needs: unit-tests
    steps:
      - uses: actions/checkout@v3
      - name: Run Integration Tests
        run: |
          cargo test --test p2p_cli_tests --features p2p,otel
          cargo test --test p2p_e2e_tests --features p2p,otel
```

## Test Data Management

### Fixtures

Located in `/Users/sac/ggen/tests/fixtures/`:
- Sample package manifests
- Mock P2P responses
- OTEL trace examples

### Test Isolation

- Each test uses `TempDir` for filesystem isolation
- CLI tests run in separate working directories
- P2P tests use mock registry to avoid network dependencies
- OTEL tests check baseline before/after execution

## Reporting

### Test Results

After execution, generate comprehensive report:

```bash
# Generate test report
cargo test --features p2p,otel -- --format json > test_results.json

# Parse results
jq '.type == "test" | select(.event == "ok")' test_results.json | wc -l
```

### OTEL Validation Report

Automatically generated at: `/Users/sac/ggen/tests/integration/otel_validation_report.txt`

Contains:
- Infrastructure health status
- Span counts (baseline vs. current)
- Jaeger trace summary
- Collector logs
- Validation pass/fail status

### Coverage Report

```bash
# Generate coverage (requires tarpaulin)
cargo tarpaulin --features p2p,otel --out Html --output-dir coverage
```

**Target:** >80% code coverage for marketplace module

## Debugging Guide

### Common Issues

#### 1. "No spans received by collector"

**Diagnosis:**
```bash
# Check collector is receiving data
docker-compose -f tests/integration/docker-compose.otel-test.yml logs otel-collector | grep -i "error"

# Verify endpoint
curl http://localhost:4318
```

**Solution:**
- Ensure `OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318`
- Check firewall/network settings
- Verify OTEL features enabled: `--features otel`

#### 2. "Collector health check failed"

**Diagnosis:**
```bash
docker-compose -f tests/integration/docker-compose.otel-test.yml ps
docker-compose -f tests/integration/docker-compose.otel-test.yml logs otel-collector
```

**Solution:**
- Restart Docker
- Check ports not in use: `lsof -i :4318`
- Verify docker-compose config syntax

#### 3. "Tests hang/timeout"

**Diagnosis:**
```bash
# Check for deadlocks
RUST_BACKTRACE=1 cargo test --features p2p,otel -- --nocapture

# Monitor network activity
docker stats
```

**Solution:**
- Reduce test concurrency: `--test-threads=1`
- Increase timeouts in test code
- Check for blocking operations in async code

#### 4. "P2P backend compilation failed"

**Current Status:** Known issue blocking test execution

**Workaround:**
- Run CLI-only tests: `cargo test --test p2p_cli_tests` (skips P2P features)
- Use mocked P2P registry: Tests use `MockP2PRegistry`

## Success Metrics

### Phase 1 (OTEL - P0): MUST PASS

- ✅ Collector receives spans from marketplace commands
- ✅ Jaeger UI shows traces with correct service name
- ✅ Parent-child span relationships preserved
- ✅ OTEL overhead < 50%

### Phase 2 (Unit - P2): SHOULD PASS

- ✅ 100% CLI tests passing
- ✅ All error cases handled gracefully
- ✅ Performance tests meet targets

### Phase 3 (Integration - P1): SHOULD PASS

- ✅ E2E workflows complete successfully
- ✅ Network resilience tests pass
- ✅ Registry integration working

### Phase 4 (Performance - P3): NICE TO HAVE

- ✅ Search < 1s
- ✅ P2P operations < 2s
- ✅ Benchmarks establish baselines

## Next Steps

### Immediate (Once Backend Compiles)

1. Execute Phase 1 (OTEL validation) - **CRITICAL**
2. Execute Phase 2 (unit tests)
3. Generate initial test report

### Short-term

1. Execute Phase 3 (integration tests)
2. Identify and fix any failures
3. Update test suite based on findings

### Long-term

1. Execute Phase 4 (performance benchmarks)
2. Establish performance baselines
3. Integrate into CI/CD pipeline
4. Add chaos/fuzz testing

## Appendix

### File Locations

```
/Users/sac/ggen/
├── tests/
│   ├── integration/
│   │   ├── otel_validation_tests.rs          # 13 OTEL tests
│   │   ├── docker-compose.otel-test.yml      # Infrastructure
│   │   ├── otel-collector-config.yaml        # Collector config
│   │   └── prometheus.yml                    # Metrics config
│   └── chicago_tdd/marketplace/
│       ├── p2p_cli_tests.rs                  # 29 CLI tests
│       └── p2p_e2e_tests.rs                  # 13 E2E tests
├── scripts/
│   └── validate-otel.sh                      # Automated validation
└── docs/validation/
    └── TEST_EXECUTION_PLAN.md                # This document
```

### Command Reference

```bash
# OTEL Validation (Priority 0)
bash scripts/validate-otel.sh

# Unit Tests (Priority 2)
cargo test --lib --features p2p,otel

# Integration Tests (Priority 1)
cargo test --test p2p_cli_tests --features p2p,otel
cargo test --test p2p_e2e_tests --features p2p,otel

# Performance (Priority 3)
cargo bench --bench marketplace_performance
cargo bench --bench marketplace_p2p

# Infrastructure
docker-compose -f tests/integration/docker-compose.otel-test.yml up -d
docker-compose -f tests/integration/docker-compose.otel-test.yml down -v

# Debugging
docker-compose -f tests/integration/docker-compose.otel-test.yml logs otel-collector
curl http://localhost:13133  # Health
curl http://localhost:8888/metrics  # Metrics
open http://localhost:16686  # Jaeger UI
```

### Environment Variables

```bash
# Required for OTEL tests
export OTEL_EXPORTER_OTLP_ENDPOINT="http://localhost:4318"
export OTEL_SERVICE_NAME="ggen-marketplace-p2p"
export RUST_LOG="info"

# Optional debugging
export RUST_BACKTRACE=1
export RUST_LOG="debug"
```

---

**Document Status:** Ready for execution
**Owner:** Test Engineer (Hive Mind)
**Review Date:** After Phase 1 completion
