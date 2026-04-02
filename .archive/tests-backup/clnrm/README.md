# CLNRM Test Suite

This directory contains `.clnrm.toml` test configurations that validate ggen functionality using hermetic, deterministic execution with comprehensive OpenTelemetry tracing.

## Overview

CLNRM (Cleanroom) provides 7-layer validation to prevent false positives ("fake greens"):

1. **Lifecycle Events Detection** - Validates that required events occur
2. **Span Graph Validation** - Ensures proper parent-child relationships
3. **Span Count Matching** - Verifies expected number of spans
4. **Temporal Ordering** - Confirms chronological execution
5. **Window Containment** - Validates time boundaries
6. **Status Validation** - Checks span success/failure states
7. **Hermeticity Enforcement** - Ensures isolated execution

## Directory Structure

```
tests/clnrm/
├── README.md                      # This file
├── marketplace/                   # Marketplace functionality tests
│   ├── search.clnrm.toml         # Search validation
│   └── install.clnrm.toml        # Package installation
├── lifecycle/                     # Project lifecycle tests
│   ├── init.clnrm.toml           # Project initialization
│   └── deploy.clnrm.toml         # Deployment workflow
└── run-all-tests.sh              # Test execution script
```

## Prerequisites

### 1. Build CLNRM CLI

```bash
cd ~/dev/clnrm
cargo build --release --bin cleanroom
```

### 2. Start OTEL Collector

```bash
# Start OpenTelemetry collector for trace collection
docker run -d \
  --name otel-collector \
  -p 4318:4318 \
  -p 4317:4317 \
  otel/opentelemetry-collector:latest
```

### 3. Build ggen

```bash
cd /Users/sac/ggen
cargo build --release
```

## Running Tests

### Validate All Tests

```bash
# Validate TOML syntax and configuration
~/dev/clnrm/target/release/cleanroom validate tests/clnrm/marketplace/
~/dev/clnrm/target/release/cleanroom validate tests/clnrm/lifecycle/
```

### Run Individual Test

```bash
# Run marketplace search test
~/dev/clnrm/target/release/cleanroom run \
  tests/clnrm/marketplace/search.clnrm.toml \
  --otel-endpoint http://localhost:4318

# Run lifecycle init test
~/dev/clnrm/target/release/cleanroom run \
  tests/clnrm/lifecycle/init.clnrm.toml \
  --otel-endpoint http://localhost:4318
```

### Run All Tests

```bash
# Execute all CLNRM tests
./tests/clnrm/run-all-tests.sh
```

## Test Reports

### JSON Report with Digest

```bash
~/dev/clnrm/target/release/cleanroom run \
  tests/clnrm/marketplace/search.clnrm.toml \
  --report-json /tmp/report.json \
  --digest /tmp/trace.sha256
```

### JUnit XML for CI/CD

```bash
~/dev/clnrm/target/release/cleanroom run \
  tests/clnrm/marketplace/search.clnrm.toml \
  --report-junit /tmp/junit.xml
```

## 7-Layer Validation

Each test includes comprehensive validation:

### 1. Lifecycle Events Detection

```toml
[validation.lifecycle_events]
required_events = [
    "marketplace.search.start",
    "marketplace.search.complete"
]
```

### 2. Span Graph Validation

```toml
[validation.span_graph]
expected_spans = [
    { name = "marketplace_search", parent = "root" },
    { name = "query_execution", parent = "marketplace_search" }
]
```

### 3. Span Count Matching

```toml
[validation.span_counts]
min_spans = 3
max_spans = 10
expected_root_spans = 1
```

### 4. Temporal Ordering

```toml
[validation.temporal_ordering]
enforce_chronological = true
max_span_overlap_ms = 100
```

### 5. Window Containment

```toml
[validation.window_containment]
enforce_containment = true
tolerance_ms = 10
```

### 6. Status Validation

```toml
[validation.status_validation]
allowed_statuses = ["ok", "error"]
require_final_status = "ok"
```

### 7. Hermeticity Enforcement

```toml
[validation.hermeticity]
forbidden_network_hosts = ["github.com", "crates.io"]
allowed_network_hosts = ["localhost"]
```

## Analyzing Traces

### View Trace JSON

```bash
# Export traces to JSON
~/dev/clnrm/target/release/cleanroom run \
  tests/clnrm/marketplace/search.clnrm.toml \
  --trace-output /tmp/traces.json

# Pretty print traces
jq . /tmp/traces.json
```

### Analyze for Fake Greens

```bash
# Analyze test execution for false positives
~/dev/clnrm/target/release/cleanroom analyze \
  tests/clnrm/marketplace/search.clnrm.toml \
  /tmp/traces.json
```

## Comparison with Rust Tests

CLNRM tests provide several advantages over traditional Rust tests:

| Feature | Rust Tests | CLNRM Tests |
|---------|-----------|-------------|
| **Hermetic Execution** | ❌ Shared state | ✅ Isolated containers |
| **Deterministic** | ⚠️ Flaky timing | ✅ Fixed seeds & timestamps |
| **Trace Validation** | ❌ No observability | ✅ 7-layer validation |
| **Fake-Green Detection** | ❌ Manual inspection | ✅ Automated analysis |
| **CI/CD Integration** | ⚠️ XML reports only | ✅ JSON + JUnit + SHA-256 |
| **Performance Metrics** | ⚠️ Limited | ✅ Comprehensive |

## Test Categories

### Marketplace Tests

- **search.clnrm.toml** - Validates marketplace search functionality
- **install.clnrm.toml** - Tests package installation and verification

### Lifecycle Tests

- **init.clnrm.toml** - Project initialization and scaffolding
- **deploy.clnrm.toml** - Deployment workflow validation

## Troubleshooting

### OTEL Collector Not Responding

```bash
# Check collector logs
docker logs otel-collector

# Restart collector
docker restart otel-collector
```

### Test Timeout

```bash
# Increase timeout in .clnrm.toml
[test]
timeout_seconds = 120
```

### Trace Analysis Fails

```bash
# Verify trace file exists and is valid JSON
jq empty /tmp/traces.json

# Check OTEL endpoint is accessible
curl -v http://localhost:4318/v1/traces
```

## CI/CD Integration

### GitHub Actions Example

```yaml
name: CLNRM Tests

on: [push, pull_request]

jobs:
  clnrm-tests:
    runs-on: ubuntu-latest

    services:
      otel-collector:
        image: otel/opentelemetry-collector:latest
        ports:
          - 4318:4318
          - 4317:4317

    steps:
      - uses: actions/checkout@v3

      - name: Build CLNRM
        run: |
          cd ~/dev/clnrm
          cargo build --release --bin cleanroom

      - name: Run CLNRM Tests
        run: ./tests/clnrm/run-all-tests.sh

      - name: Upload Test Reports
        uses: actions/upload-artifact@v3
        with:
          name: clnrm-reports
          path: target/clnrm-reports/
```

## Performance Benchmarks

Expected performance for each test:

| Test | Expected Duration | Max Duration |
|------|-------------------|--------------|
| marketplace/search.clnrm.toml | <5s | 30s |
| marketplace/install.clnrm.toml | <15s | 60s |
| lifecycle/init.clnrm.toml | <10s | 45s |
| lifecycle/deploy.clnrm.toml | <30s | 120s |

## Documentation

- **[CLNRM Migration Results](../../docs/testing/CLNRM_MIGRATION_RESULTS.md)** - Complete migration report
- **[Fake-Green Detection](../../docs/testing/FAKE_GREEN_DETECTION.md)** - False positive analysis
- **[Trace Analysis Guide](../../docs/testing/TRACE_ANALYSIS.md)** - OpenTelemetry trace analysis

## Support

- **CLNRM Issues**: https://github.com/sac/clnrm/issues
- **Ggen Issues**: https://github.com/seanchatmangpt/ggen/issues

---

**Note**: CLNRM tests provide proof of execution through comprehensive trace validation, eliminating false positives that plague traditional test suites.
