# P2P Marketplace OTEL Validation Architecture

**Status:** Design Document
**Created:** 2025-11-02
**Author:** System Architect Agent (Hive Mind)
**Reference:** clnrm OTEL validation patterns

---

## Executive Summary

This document defines the OpenTelemetry validation architecture for the ggen P2P marketplace, following proven patterns from clnrm. The architecture validates that P2P operations **actually emit traces** to a real OTEL collector, not just that code compiles or runs.

### Key Principle

> "Running a CLI help command is a false positive. ONLY TRUST OTEL SPAN/TRACES"

We validate actual trace emission, span relationships, and collector metrics - not just successful command execution.

---

## 1. Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                    P2P OTEL Validation Stack                 │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌──────────────┐  OTLP    ┌──────────────┐   Jaeger  ┌────┤
│  │   P2P CLI    │ ───────> │     OTEL     │ ────────> │    │
│  │  (ggen p2p)  │  HTTP    │   Collector  │  Export   │ UI │
│  └──────────────┘  :4318   └──────────────┘           └────┤
│         │                          │                         │
│         │ Instrumented             │ Metrics                 │
│         │ Operations               │ :8888                   │
│         │                          │                         │
│  ┌──────▼──────┐            ┌──────▼──────┐                │
│  │  P2P Swarm  │            │ Prometheus  │                │
│  │  (libp2p)   │            │  (optional) │                │
│  └─────────────┘            └─────────────┘                │
│                                                               │
└─────────────────────────────────────────────────────────────┘
```

### Components

1. **P2P CLI with OTEL**: ggen marketplace commands with tracing instrumentation
2. **OTEL Collector**: Receives, processes, and exports traces/metrics
3. **Jaeger**: Trace visualization and query interface
4. **Prometheus**: (Optional) Metrics collection and alerting
5. **Test Suite**: Validates trace emission and span correctness

---

## 2. Infrastructure Setup

### 2.1 Docker Compose Configuration

**File:** `/Users/sac/ggen/tests/integration/docker-compose.p2p-otel.yml`

```yaml
version: '3.8'

services:
  # OTEL Collector - receives traces from ggen
  otel-collector:
    image: otel/opentelemetry-collector-contrib:latest
    container_name: ggen-p2p-otel-collector
    command: ["--config=/etc/otel-collector-config.yaml"]
    volumes:
      - ./otel-collector-p2p-config.yaml:/etc/otel-collector-config.yaml
      - /tmp/ggen-otel:/tmp/otel-output
    ports:
      - "4317:4317"   # OTLP gRPC
      - "4318:4318"   # OTLP HTTP
      - "13133:13133" # Health check
      - "8888:8888"   # Prometheus metrics
      - "55679:55679" # zpages (debug)
    healthcheck:
      test: ["CMD", "wget", "--spider", "-q", "http://localhost:13133/"]
      interval: 5s
      timeout: 3s
      retries: 5
    networks:
      - ggen-p2p-test

  # Jaeger - trace visualization
  jaeger:
    image: jaegertracing/all-in-one:latest
    container_name: ggen-p2p-jaeger
    ports:
      - "16686:16686" # Jaeger UI
      - "14250:14250" # Jaeger gRPC
    environment:
      - COLLECTOR_OTLP_ENABLED=true
    networks:
      - ggen-p2p-test

  # Prometheus (optional) - metrics collection
  prometheus:
    image: prom/prometheus:latest
    container_name: ggen-p2p-prometheus
    command:
      - '--config.file=/etc/prometheus/prometheus.yml'
      - '--storage.tsdb.path=/prometheus'
    volumes:
      - ./prometheus-p2p-config.yml:/etc/prometheus/prometheus.yml
    ports:
      - "9090:9090"
    networks:
      - ggen-p2p-test

networks:
  ggen-p2p-test:
    driver: bridge
```

### 2.2 OTEL Collector Configuration

**File:** `/Users/sac/ggen/tests/integration/otel-collector-p2p-config.yaml`

```yaml
receivers:
  otlp:
    protocols:
      http:
        endpoint: 0.0.0.0:4318
        cors:
          allowed_origins:
            - "*"
      grpc:
        endpoint: 0.0.0.0:4317

processors:
  batch:
    timeout: 1s
    send_batch_size: 1024
    send_batch_max_size: 2048

  # Add resource attributes
  resource:
    attributes:
      - key: service.name
        value: ggen-p2p-marketplace
        action: upsert

  # Filter out health check spans (if needed)
  filter:
    traces:
      span:
        - 'attributes["http.target"] == "/health"'

exporters:
  # Console logging for debugging
  logging:
    loglevel: info
    sampling_initial: 5
    sampling_thereafter: 200

  # File export for test validation
  file:
    path: /tmp/otel-output/traces.json
    format: json

  # Jaeger export for visualization
  jaeger:
    endpoint: jaeger:14250
    tls:
      insecure: true

  # Prometheus metrics
  prometheus:
    endpoint: 0.0.0.0:8888
    namespace: ggen_p2p

service:
  extensions: [health_check, zpages]

  pipelines:
    traces:
      receivers: [otlp]
      processors: [resource, batch, filter]
      exporters: [logging, file, jaeger]

    metrics:
      receivers: [otlp]
      processors: [resource, batch]
      exporters: [logging, prometheus]

    logs:
      receivers: [otlp]
      processors: [batch]
      exporters: [logging, file]

extensions:
  health_check:
    endpoint: 0.0.0.0:13133

  zpages:
    endpoint: 0.0.0.0:55679
```

### 2.3 Prometheus Configuration (Optional)

**File:** `/Users/sac/ggen/tests/integration/prometheus-p2p-config.yml`

```yaml
global:
  scrape_interval: 15s
  evaluation_interval: 15s

scrape_configs:
  - job_name: 'otel-collector'
    static_configs:
      - targets: ['otel-collector:8888']

  - job_name: 'ggen-p2p'
    static_configs:
      - targets: ['host.docker.internal:8889'] # If ggen exposes metrics
```

---

## 3. P2P Operations to Validate

### 3.1 Critical P2P Operations

Each operation must emit verifiable spans:

| Operation | Trace Name | Expected Spans | Validation Criteria |
|-----------|-----------|----------------|---------------------|
| **P2P Init** | `p2p.swarm.init` | `swarm_create`, `kad_bootstrap`, `gossipsub_subscribe` | Swarm created, DHT bootstrapped, topic subscribed |
| **Package Publish** | `p2p.package.publish` | `dht_put`, `gossipsub_publish`, `content_routing` | Package stored in DHT, announced via gossipsub |
| **Package Search** | `p2p.package.search` | `dht_get_providers`, `peer_query`, `content_fetch` | Providers found, peers queried, content retrieved |
| **Peer Discovery** | `p2p.peer.discovery` | `kad_query`, `identify_exchange`, `reputation_update` | Peers discovered, identified, reputation tracked |
| **Content Distribution** | `p2p.content.distribute` | `chunked_transfer`, `peer_selection`, `replication` | Content chunked, peers selected, replicated |
| **Swarm Gossip** | `p2p.gossip.propagate` | `message_validation`, `peer_broadcast`, `mesh_maintenance` | Messages validated, broadcast, mesh maintained |

### 3.2 Span Hierarchy Example

```
p2p.package.publish (root span)
├── p2p.package.validate (child)
│   ├── signature.verify
│   └── content.hash
├── dht.put (child)
│   ├── kad.query_closest_peers
│   ├── kad.store_value
│   └── kad.confirm_storage
└── gossipsub.publish (child)
    ├── message.encode
    ├── peers.select
    └── network.broadcast
```

---

## 4. Test Suite Design

### 4.1 Test Structure

```
tests/
├── integration/
│   ├── p2p_otel_validation.rs         # Main test suite
│   ├── docker-compose.p2p-otel.yml    # Infrastructure
│   ├── otel-collector-p2p-config.yaml # Collector config
│   └── helpers/
│       ├── collector_client.rs         # Query collector metrics
│       ├── jaeger_client.rs            # Query Jaeger traces
│       └── span_validator.rs           # Validate span structure
```

### 4.2 Core Test Cases

#### Test 1: P2P Swarm Initialization Emits Traces

```rust
#[tokio::test]
#[ignore] // Run with --ignored in CI
async fn test_p2p_swarm_init_emits_traces() -> Result<()> {
    // 1. Start OTEL infrastructure
    let infra = start_otel_infrastructure().await?;

    // 2. Initialize P2P swarm with OTEL enabled
    std::env::set_var("OTEL_EXPORTER_OTLP_ENDPOINT", "http://localhost:4318");
    std::env::set_var("RUST_LOG", "ggen_marketplace=trace");

    let output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--", "p2p", "init"])
        .output()
        .await?;

    assert!(output.status.success(), "P2P init failed");

    // 3. Wait for span export (max 5s)
    tokio::time::sleep(Duration::from_secs(2)).await;

    // 4. Verify collector received traces
    let collector_metrics = query_collector_metrics(&infra).await?;
    assert!(
        collector_metrics.spans_received > 0,
        "No spans received by collector"
    );

    // 5. Verify specific spans exist in Jaeger
    let traces = query_jaeger_traces(&infra, "ggen-p2p-marketplace").await?;
    let swarm_init_trace = traces.iter()
        .find(|t| t.operation_name == "p2p.swarm.init")
        .expect("p2p.swarm.init trace not found");

    // 6. Validate span hierarchy
    assert_child_span_exists(&swarm_init_trace, "swarm_create")?;
    assert_child_span_exists(&swarm_init_trace, "kad_bootstrap")?;
    assert_child_span_exists(&swarm_init_trace, "gossipsub_subscribe")?;

    // 7. Verify span attributes
    assert_span_attribute(&swarm_init_trace, "service.name", "ggen-p2p-marketplace")?;
    assert_span_attribute(&swarm_init_trace, "p2p.peer_id", is_peer_id)?;

    infra.shutdown().await?;
    Ok(())
}
```

#### Test 2: Package Publish Creates Complete Trace

```rust
#[tokio::test]
#[ignore]
async fn test_package_publish_creates_complete_trace() -> Result<()> {
    let infra = start_otel_infrastructure().await?;
    std::env::set_var("OTEL_EXPORTER_OTLP_ENDPOINT", "http://localhost:4318");

    // Publish a test package
    let output = Command::new("cargo")
        .args(&[
            "run", "--bin", "ggen", "--",
            "p2p", "publish", "--package", "test-pkg-v1.0.0"
        ])
        .output()
        .await?;

    assert!(output.status.success());
    tokio::time::sleep(Duration::from_secs(2)).await;

    // Query traces
    let traces = query_jaeger_traces(&infra, "ggen-p2p-marketplace").await?;
    let publish_trace = traces.iter()
        .find(|t| t.operation_name == "p2p.package.publish")
        .expect("p2p.package.publish trace not found");

    // Validate trace structure
    validate_trace_structure(&publish_trace, &[
        "p2p.package.validate",
        "dht.put",
        "gossipsub.publish"
    ])?;

    // Verify DHT operations
    let dht_span = find_child_span(&publish_trace, "dht.put")?;
    assert_span_attribute(&dht_span, "kad.key", "test-pkg-v1.0.0")?;
    assert_span_status(&dht_span, SpanStatus::Ok)?;

    // Verify gossipsub propagation
    let gossip_span = find_child_span(&publish_trace, "gossipsub.publish")?;
    assert_span_attribute(&gossip_span, "topic", "/ggen/packages/v1")?;

    infra.shutdown().await?;
    Ok(())
}
```

#### Test 3: P2P Search Validates Peer Discovery

```rust
#[tokio::test]
#[ignore]
async fn test_p2p_search_validates_peer_discovery() -> Result<()> {
    let infra = start_otel_infrastructure().await?;
    std::env::set_var("OTEL_EXPORTER_OTLP_ENDPOINT", "http://localhost:4318");

    // Search for a package
    let output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--", "p2p", "search", "test-pkg"])
        .output()
        .await?;

    assert!(output.status.success());
    tokio::time::sleep(Duration::from_secs(2)).await;

    // Verify trace structure
    let traces = query_jaeger_traces(&infra, "ggen-p2p-marketplace").await?;
    let search_trace = traces.iter()
        .find(|t| t.operation_name == "p2p.package.search")
        .expect("p2p.package.search trace not found");

    // Validate peer discovery spans
    assert_child_span_exists(&search_trace, "dht_get_providers")?;
    assert_child_span_exists(&search_trace, "peer_query")?;

    // Verify peers were contacted
    let peer_query_span = find_child_span(&search_trace, "peer_query")?;
    let peers_contacted = get_span_attribute::<usize>(&peer_query_span, "peers.contacted")?;
    assert!(peers_contacted > 0, "No peers were contacted");

    infra.shutdown().await?;
    Ok(())
}
```

#### Test 4: Collector Metrics Validation

```rust
#[tokio::test]
#[ignore]
async fn test_collector_metrics_show_span_flow() -> Result<()> {
    let infra = start_otel_infrastructure().await?;

    // Run multiple P2P operations
    for i in 0..5 {
        Command::new("cargo")
            .args(&["run", "--bin", "ggen", "--", "p2p", "status"])
            .output()
            .await?;
    }

    tokio::time::sleep(Duration::from_secs(3)).await;

    // Query collector Prometheus metrics
    let metrics = query_collector_metrics(&infra).await?;

    // Verify span reception
    assert!(
        metrics.otelcol_receiver_accepted_spans >= 5,
        "Expected at least 5 spans, got {}",
        metrics.otelcol_receiver_accepted_spans
    );

    // Verify span export to Jaeger
    assert!(
        metrics.otelcol_exporter_sent_spans >= 5,
        "Spans not exported to Jaeger"
    );

    // Verify no dropped spans
    assert_eq!(
        metrics.otelcol_processor_dropped_spans, 0,
        "Spans were dropped by processor"
    );

    infra.shutdown().await?;
    Ok(())
}
```

#### Test 5: Span Parent-Child Relationships

```rust
#[tokio::test]
#[ignore]
async fn test_span_parent_child_relationships() -> Result<()> {
    let infra = start_otel_infrastructure().await?;
    std::env::set_var("OTEL_EXPORTER_OTLP_ENDPOINT", "http://localhost:4318");

    // Execute complex P2P operation
    Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--", "p2p", "publish", "--package", "test"])
        .output()
        .await?;

    tokio::time::sleep(Duration::from_secs(2)).await;

    // Get trace from Jaeger
    let traces = query_jaeger_traces(&infra, "ggen-p2p-marketplace").await?;
    let publish_trace = traces.first().expect("No traces found");

    // Validate trace hierarchy
    let validator = SpanHierarchyValidator::new();
    validator.validate_hierarchy(&publish_trace)?;

    // Specific validations
    let root_span = &publish_trace.spans[0];
    assert_eq!(root_span.operation_name, "p2p.package.publish");

    for child_span in &publish_trace.spans[1..] {
        // Every child must reference the parent
        assert!(
            child_span.references.iter().any(|r| {
                r.ref_type == "CHILD_OF" && r.span_id == root_span.span_id
            }),
            "Span {} missing parent reference",
            child_span.operation_name
        );
    }

    infra.shutdown().await?;
    Ok(())
}
```

### 4.3 Helper Functions

```rust
/// Start OTEL infrastructure (collector, Jaeger, Prometheus)
async fn start_otel_infrastructure() -> Result<OtelInfrastructure> {
    let compose_file = "tests/integration/docker-compose.p2p-otel.yml";

    // Start services
    Command::new("docker-compose")
        .args(&["-f", compose_file, "up", "-d"])
        .output()
        .await?;

    // Wait for health checks
    wait_for_health("http://localhost:13133", Duration::from_secs(30)).await?;
    wait_for_health("http://localhost:16686", Duration::from_secs(30)).await?;

    Ok(OtelInfrastructure {
        compose_file: compose_file.to_string(),
        collector_endpoint: "http://localhost:4318".to_string(),
        jaeger_ui: "http://localhost:16686".to_string(),
        prometheus_endpoint: "http://localhost:9090".to_string(),
    })
}

/// Query OTEL collector Prometheus metrics
async fn query_collector_metrics(infra: &OtelInfrastructure) -> Result<CollectorMetrics> {
    let response = reqwest::get("http://localhost:8888/metrics").await?;
    let body = response.text().await?;

    // Parse Prometheus metrics
    let mut metrics = CollectorMetrics::default();
    for line in body.lines() {
        if line.starts_with("otelcol_receiver_accepted_spans") {
            metrics.otelcol_receiver_accepted_spans = parse_metric_value(line)?;
        } else if line.starts_with("otelcol_exporter_sent_spans") {
            metrics.otelcol_exporter_sent_spans = parse_metric_value(line)?;
        } else if line.starts_with("otelcol_processor_dropped_spans") {
            metrics.otelcol_processor_dropped_spans = parse_metric_value(line)?;
        }
    }

    Ok(metrics)
}

/// Query Jaeger for traces
async fn query_jaeger_traces(
    infra: &OtelInfrastructure,
    service_name: &str,
) -> Result<Vec<JaegerTrace>> {
    let url = format!(
        "{}/api/traces?service={}&limit=100",
        infra.jaeger_ui, service_name
    );

    let response = reqwest::get(&url).await?;
    let jaeger_response: JaegerTracesResponse = response.json().await?;

    Ok(jaeger_response.data)
}

/// Validate span attribute exists and matches predicate
fn assert_span_attribute<T, F>(
    span: &JaegerSpan,
    key: &str,
    predicate: F,
) -> Result<()>
where
    F: Fn(&T) -> bool,
    T: serde::de::DeserializeOwned,
{
    let tag = span.tags.iter()
        .find(|t| t.key == key)
        .ok_or_else(|| anyhow!("Missing span attribute: {}", key))?;

    let value: T = serde_json::from_value(tag.value.clone())?;

    if !predicate(&value) {
        bail!("Span attribute {} failed validation", key);
    }

    Ok(())
}
```

---

## 5. Validation Criteria

### 5.1 Mandatory Validations

Every P2P operation test MUST validate:

1. **Span Emission**
   - ✅ Spans are exported to collector (verify via `/metrics`)
   - ✅ Collector received spans (check `otelcol_receiver_accepted_spans`)
   - ✅ Spans exported to Jaeger (check `otelcol_exporter_sent_spans`)

2. **Span Structure**
   - ✅ Root span exists with correct operation name
   - ✅ Child spans reference parent (via `CHILD_OF` relationship)
   - ✅ Trace ID is consistent across all spans
   - ✅ Span timestamps are monotonic

3. **Span Attributes**
   - ✅ `service.name` = `ggen-p2p-marketplace`
   - ✅ `service.version` = current version
   - ✅ `p2p.peer_id` = valid libp2p PeerId
   - ✅ Operation-specific attributes (e.g., `kad.key`, `topic`)

4. **Span Status**
   - ✅ Successful operations have `status.code = 0` (OK)
   - ✅ Errors have `status.code = 2` (ERROR)
   - ✅ Error spans include `error.message` and `error.type`

5. **Collector Health**
   - ✅ No dropped spans (`otelcol_processor_dropped_spans = 0`)
   - ✅ Export success rate > 99%
   - ✅ Batch processor functioning (`otelcol_processor_batch_size > 0`)

### 5.2 Performance Validations

P2P operations must meet SLOs:

| Metric | SLO | Validation |
|--------|-----|------------|
| Span export latency | < 500ms | Check span timestamp vs collector receipt |
| Trace completeness | 100% | All expected child spans present |
| Collector CPU | < 50% | Query Prometheus `process_cpu_seconds_total` |
| Collector memory | < 512MB | Query Prometheus `process_resident_memory_bytes` |

---

## 6. CI Integration

### 6.1 GitHub Actions Workflow

**File:** `.github/workflows/p2p-otel-validation.yml`

```yaml
name: P2P OTEL Validation

on:
  push:
    branches: [master]
    paths:
      - 'ggen-marketplace/src/backend/p2p.rs'
      - 'ggen-core/src/telemetry.rs'
      - 'tests/integration/p2p_otel_validation.rs'
  pull_request:
    branches: [master]

jobs:
  p2p-otel-validation:
    runs-on: ubuntu-latest
    timeout-minutes: 15

    steps:
      - uses: actions/checkout@v3

      - name: Install Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
          override: true

      - name: Start OTEL Infrastructure
        run: |
          docker-compose -f tests/integration/docker-compose.p2p-otel.yml up -d

      - name: Wait for OTEL Collector Health
        run: |
          timeout 60 bash -c 'until curl -f http://localhost:13133/health; do sleep 2; done'

      - name: Wait for Jaeger Health
        run: |
          timeout 60 bash -c 'until curl -f http://localhost:16686; do sleep 2; done'

      - name: Run P2P OTEL Validation Tests
        env:
          OTEL_EXPORTER_OTLP_ENDPOINT: http://localhost:4318
          RUST_LOG: ggen_marketplace=trace,ggen_core=trace
        run: |
          cargo test --test p2p_otel_validation --features otel -- --ignored --nocapture

      - name: Verify Collector Received Traces
        run: |
          SPANS=$(curl -s http://localhost:8888/metrics | grep 'otelcol_receiver_accepted_spans' | awk '{print $2}')
          if [ "$SPANS" -lt 1 ]; then
            echo "ERROR: No spans received by collector"
            exit 1
          fi
          echo "✅ Collector received $SPANS spans"

      - name: Verify No Dropped Spans
        run: |
          DROPPED=$(curl -s http://localhost:8888/metrics | grep 'otelcol_processor_dropped_spans' | awk '{print $2}' || echo 0)
          if [ "$DROPPED" -gt 0 ]; then
            echo "ERROR: $DROPPED spans were dropped"
            exit 1
          fi
          echo "✅ No spans dropped"

      - name: Export Jaeger Traces (on failure)
        if: failure()
        run: |
          curl -o jaeger-traces.json 'http://localhost:16686/api/traces?service=ggen-p2p-marketplace&limit=100'

      - name: Upload Collector Logs (on failure)
        if: failure()
        uses: actions/upload-artifact@v3
        with:
          name: otel-collector-logs
          path: |
            /tmp/ggen-otel/traces.json
            jaeger-traces.json

      - name: Cleanup
        if: always()
        run: |
          docker-compose -f tests/integration/docker-compose.p2p-otel.yml down -v
```

### 6.2 Validation Script

**File:** `scripts/validate-p2p-otel.sh`

```bash
#!/bin/bash
set -euo pipefail

echo "🚀 Starting P2P OTEL Validation"
echo "================================"

# 1. Start infrastructure
echo "📦 Starting OTEL infrastructure..."
docker-compose -f tests/integration/docker-compose.p2p-otel.yml up -d

# 2. Wait for health
echo "⏳ Waiting for collector health..."
timeout 60 bash -c 'until curl -sf http://localhost:13133/health > /dev/null; do sleep 2; done'
echo "✅ Collector healthy"

echo "⏳ Waiting for Jaeger health..."
timeout 60 bash -c 'until curl -sf http://localhost:16686 > /dev/null; do sleep 2; done'
echo "✅ Jaeger healthy"

# 3. Run validation tests
echo "🧪 Running P2P OTEL validation tests..."
export OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318
export RUST_LOG=ggen_marketplace=trace,ggen_core=trace

cargo test --test p2p_otel_validation --features otel -- --ignored --nocapture

# 4. Verify collector metrics
echo "📊 Verifying collector metrics..."
SPANS=$(curl -s http://localhost:8888/metrics | grep 'otelcol_receiver_accepted_spans{' | head -n1 | awk '{print $2}')
EXPORTED=$(curl -s http://localhost:8888/metrics | grep 'otelcol_exporter_sent_spans{' | head -n1 | awk '{print $2}')
DROPPED=$(curl -s http://localhost:8888/metrics | grep 'otelcol_processor_dropped_spans{' | head -n1 | awk '{print $2}' || echo 0)

echo "  Spans received: $SPANS"
echo "  Spans exported: $EXPORTED"
echo "  Spans dropped: $DROPPED"

if [ "$SPANS" -lt 1 ]; then
  echo "❌ ERROR: No spans received"
  exit 1
fi

if [ "$DROPPED" -gt 0 ]; then
  echo "❌ ERROR: Spans were dropped"
  exit 1
fi

# 5. Generate validation report
echo "📝 Generating validation report..."
cat > /tmp/p2p-otel-validation-report.txt <<EOF
P2P OTEL Validation Report
==========================
Date: $(date)

Collector Metrics:
  - Spans Received: $SPANS
  - Spans Exported: $EXPORTED
  - Spans Dropped: $DROPPED
  - Export Success Rate: $(echo "scale=2; ($EXPORTED/$SPANS)*100" | bc)%

Jaeger UI: http://localhost:16686
Collector zpages: http://localhost:55679

Status: ✅ PASS
EOF

cat /tmp/p2p-otel-validation-report.txt

# 6. Cleanup (optional)
# docker-compose -f tests/integration/docker-compose.p2p-otel.yml down -v

echo ""
echo "✅ P2P OTEL Validation Complete!"
echo "🔍 View traces at http://localhost:16686"
```

---

## 7. Debugging & Troubleshooting

### 7.1 Common Issues

#### Issue 1: No Spans Received

**Symptoms:**
- `otelcol_receiver_accepted_spans = 0`
- Jaeger shows no traces

**Debugging:**
```bash
# 1. Check collector logs
docker-compose -f tests/integration/docker-compose.p2p-otel.yml logs otel-collector

# 2. Verify OTEL endpoint is set
echo $OTEL_EXPORTER_OTLP_ENDPOINT

# 3. Test direct span export
curl -X POST http://localhost:4318/v1/traces \
  -H "Content-Type: application/json" \
  -d '{"resourceSpans":[{"resource":{"attributes":[{"key":"service.name","value":{"stringValue":"test"}}]},"scopeSpans":[{"spans":[{"traceId":"00000000000000000000000000000001","spanId":"0000000000000001","name":"test","kind":1,"startTimeUnixNano":"1609459200000000000","endTimeUnixNano":"1609459201000000000"}]}]}]}'

# 4. Check zpages for active traces
curl http://localhost:55679/debug/tracez
```

#### Issue 2: Incomplete Traces

**Symptoms:**
- Missing child spans
- Broken parent-child relationships

**Debugging:**
```bash
# Query Jaeger for specific trace
curl 'http://localhost:16686/api/traces?service=ggen-p2p-marketplace&operation=p2p.package.publish' | jq .

# Check trace structure
TRACE_ID="..." # from Jaeger
curl "http://localhost:16686/api/traces/$TRACE_ID" | jq '.data[0].spans[] | {operation: .operationName, spanID, references}'
```

#### Issue 3: Spans Dropped

**Symptoms:**
- `otelcol_processor_dropped_spans > 0`

**Debugging:**
```bash
# Check collector metrics
curl http://localhost:8888/metrics | grep dropped

# Increase batch size in collector config
# processors.batch.send_batch_max_size = 4096

# Check collector resource limits
docker stats ggen-p2p-otel-collector
```

### 7.2 Debugging Endpoints

| Endpoint | Purpose |
|----------|---------|
| `http://localhost:13133/health` | Collector health check |
| `http://localhost:8888/metrics` | Collector Prometheus metrics |
| `http://localhost:55679/debug/tracez` | Active traces (zpages) |
| `http://localhost:55679/debug/spanprocessorz` | Span processor status |
| `http://localhost:16686` | Jaeger UI |
| `http://localhost:16686/api/traces?service=ggen-p2p-marketplace` | Jaeger API |

---

## 8. Success Criteria

### 8.1 Definition of Done

P2P OTEL validation is complete when:

- ✅ All 5+ test cases pass in CI
- ✅ Collector receives spans (`otelcol_receiver_accepted_spans > 0`)
- ✅ No spans dropped (`otelcol_processor_dropped_spans = 0`)
- ✅ Jaeger shows complete traces for all P2P operations
- ✅ Span parent-child relationships are correct
- ✅ All mandatory span attributes are present
- ✅ Validation script (`scripts/validate-p2p-otel.sh`) passes
- ✅ CI workflow (`p2p-otel-validation.yml`) is green

### 8.2 Acceptance Tests

**Manual Validation:**
```bash
# 1. Start infrastructure
bash scripts/validate-p2p-otel.sh

# 2. Run a P2P operation
cargo run --bin ggen -- p2p publish --package test-v1.0.0

# 3. Verify in Jaeger UI
open http://localhost:16686
# Search for service: ggen-p2p-marketplace
# Verify traces exist with correct spans

# 4. Check collector metrics
curl http://localhost:8888/metrics | grep otelcol_receiver_accepted_spans

# 5. Verify no errors in logs
docker-compose -f tests/integration/docker-compose.p2p-otel.yml logs --tail=50
```

---

## 9. Next Steps

### 9.1 Implementation Tasks

1. **Create Docker Compose setup**
   - Write `docker-compose.p2p-otel.yml`
   - Write `otel-collector-p2p-config.yaml`
   - Write `prometheus-p2p-config.yml`

2. **Instrument P2P operations**
   - Add tracing to `ggen-marketplace/src/backend/p2p.rs`
   - Add spans to DHT operations
   - Add spans to Gossipsub operations
   - Add spans to peer discovery

3. **Create test suite**
   - Write `tests/integration/p2p_otel_validation.rs`
   - Implement helper functions (collector_client, jaeger_client)
   - Write validation functions

4. **Add CI integration**
   - Create `.github/workflows/p2p-otel-validation.yml`
   - Add validation script `scripts/validate-p2p-otel.sh`

5. **Documentation**
   - Add OTEL validation section to README
   - Create troubleshooting guide
   - Document debugging workflows

### 9.2 Dependencies

- **Runtime**: Docker, docker-compose
- **Rust crates**: `opentelemetry`, `opentelemetry-otlp`, `tracing-opentelemetry`
- **Infrastructure**: OTEL Collector, Jaeger, Prometheus (optional)
- **Test dependencies**: `reqwest`, `tokio`, `serde_json`

---

## 10. References

- **clnrm OTEL config**: `/Users/sac/clnrm/.clnrm/otel-collector-config.yaml`
- **clnrm validation docs**: `/Users/sac/clnrm/.cursor/commands-archive/validate-otel-integration.md`
- **ggen telemetry**: `/Users/sac/ggen/ggen-core/src/telemetry.rs`
- **P2P backend**: `/Users/sac/ggen/ggen-marketplace/src/backend/p2p.rs`
- **OpenTelemetry Docs**: https://opentelemetry.io/docs/
- **Jaeger Docs**: https://www.jaegertracing.io/docs/
- **OTLP Spec**: https://github.com/open-telemetry/opentelemetry-specification/blob/main/specification/protocol/otlp.md

---

## Appendix A: Data Structures

### CollectorMetrics

```rust
#[derive(Debug, Default)]
pub struct CollectorMetrics {
    pub otelcol_receiver_accepted_spans: u64,
    pub otelcol_exporter_sent_spans: u64,
    pub otelcol_processor_dropped_spans: u64,
    pub otelcol_processor_batch_size: u64,
}
```

### JaegerTrace

```rust
#[derive(Debug, Deserialize)]
pub struct JaegerTrace {
    pub trace_id: String,
    pub spans: Vec<JaegerSpan>,
    pub processes: HashMap<String, JaegerProcess>,
}

#[derive(Debug, Deserialize)]
pub struct JaegerSpan {
    pub trace_id: String,
    pub span_id: String,
    pub operation_name: String,
    pub references: Vec<JaegerReference>,
    pub start_time: u64,
    pub duration: u64,
    pub tags: Vec<JaegerTag>,
    pub logs: Vec<JaegerLog>,
}

#[derive(Debug, Deserialize)]
pub struct JaegerReference {
    pub ref_type: String, // "CHILD_OF" or "FOLLOWS_FROM"
    pub trace_id: String,
    pub span_id: String,
}
```

---

**Document Version:** 1.0
**Last Updated:** 2025-11-02
**Status:** Ready for Implementation
