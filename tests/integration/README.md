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
