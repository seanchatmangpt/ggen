# OpenTelemetry Integration for FactoryPaaS Attribution Context

## Overview

This document describes the comprehensive OpenTelemetry (OTEL) instrumentation for the FactoryPaaS attribution tracking system. The integration provides **distributed tracing**, **custom metrics**, and **structured logging** with automatic correlation.

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    FactoryPaaS Kernel                           │
│  ┌───────────────┐  ┌────────────────┐  ┌──────────────────┐  │
│  │ OTEL Tracing  │  │ OTEL Metrics   │  │ OTEL Logging     │  │
│  │ (spans)       │  │ (counters/hist)│  │ (structured)     │  │
│  └───────┬───────┘  └────────┬───────┘  └────────┬─────────┘  │
│          │                   │                    │             │
│          └───────────────────┴────────────────────┘             │
│                              │                                  │
└──────────────────────────────┼──────────────────────────────────┘
                               │
                      ┌────────▼─────────┐
                      │  OTLP Exporter   │
                      │  (gRPC/HTTP)     │
                      └────────┬─────────┘
                               │
        ┌──────────────────────┼──────────────────────┐
        │                      │                      │
┌───────▼────────┐   ┌─────────▼────────┐   ┌────────▼────────┐
│ GCP Cloud      │   │ GCP Cloud        │   │ Local OTLP      │
│ Trace          │   │ Monitoring       │   │ Collector       │
│ (production)   │   │ (production)     │   │ (development)   │
└────────────────┘   └──────────────────┘   └─────────────────┘
```

## Components

### 1. Distributed Tracing (`otel_tracing.rs`)

**Purpose**: Track request flow across service boundaries with W3C Trace Context propagation.

**Key Features**:
- W3C Trace Context format (traceparent header)
- Span creation for commands, events, HTTP requests, DB operations
- Parent-child span relationships
- GCP Cloud Trace integration

**Example Usage**:
```rust
use crate::otel_tracing::{SpanContext, create_command_span};

// Create root span from HTTP request
let span_ctx = SpanContext::from_headers(&request_headers)
    .unwrap_or_else(|| SpanContext::new_root());

// Create instrumented command span
let span = create_command_span("CreateClick", &command_id, &aggregate_id);
let _guard = span.enter();

// All operations within this scope are traced
handle_create_click(command, &span_ctx).await?;
```

**W3C Trace Context Format**:
```
traceparent: 00-{trace-id}-{span-id}-01
             ^^  ^^^^^^^^   ^^^^^^^^  ^^
             │   │          │         └─ flags (01 = sampled)
             │   │          └─────────── span ID (16 hex chars)
             │   └────────────────────── trace ID (32 hex chars)
             └────────────────────────── version (00)
```

### 2. Custom Metrics (`otel_metrics.rs`)

**Purpose**: Track business metrics for affiliate attribution, revenue, and system performance.

**Metrics Exposed**:

| Metric Name                        | Type      | Description                          |
|------------------------------------|-----------|--------------------------------------|
| `affiliate.clicks.total`           | Counter   | Total clicks tracked                 |
| `affiliate.clicks.by_id`           | Counter   | Clicks grouped by affiliate ID       |
| `affiliate.conversions.total`      | Counter   | Total conversions                    |
| `affiliate.revenue.cents`          | Counter   | Revenue attributed in cents          |
| `affiliate.conversion.latency`     | Histogram | Click-to-conversion time (seconds)   |
| `affiliate.windows.active`         | Gauge     | Active attribution windows           |
| `commands.total`                   | Counter   | Commands processed                   |
| `commands.latency`                 | Histogram | Command execution time (seconds)     |
| `commands.errors`                  | Counter   | Command failures                     |
| `receipts.total`                   | Counter   | Receipts generated                   |
| `receipts.verifications.success`   | Counter   | Successful verifications             |
| `receipts.verifications.failed`    | Counter   | Failed verifications                 |

**Example Usage**:
```rust
use crate::otel_metrics::MetricsRegistry;

// Record affiliate click
metrics_registry.affiliate.record_click(&affiliate_id, &click_id);

// Record conversion with revenue
metrics_registry.affiliate.record_conversion(
    &affiliate_id,
    &click_id,
    9999, // revenue in cents ($99.99)
    120.5 // latency in seconds
);

// Update active windows count
metrics_registry.affiliate.set_active_windows(42).await;
```

### 3. Structured Logging (`otel_logs.rs`)

**Purpose**: Emit JSON-formatted logs with automatic trace ID correlation.

**Log Format**:
```json
{
  "level": "info",
  "timestamp": "2026-01-24T12:00:00.000Z",
  "trace_id": "550e8400-e29b-41d4-a716-446655440000",
  "span_id": "6ba7b810-9dad-11d1-80b4-00c04fd430c8",
  "message": "Affiliate click tracked",
  "fields": {
    "affiliate.id": "...",
    "click.id": "...",
    "http.client.ip": "192.168.1.1",
    "http.user_agent": "Mozilla/5.0"
  }
}
```

**Example Usage**:
```rust
use crate::otel_logs::{log_click_tracked, log_conversion_tracked};

// Log click with trace context
log_click_tracked(
    &affiliate_id,
    &click_id,
    &ip_address,
    &user_agent,
    span_ctx.trace_id,
    span_ctx.span_id,
);

// Log conversion
log_conversion_tracked(
    &affiliate_id,
    &click_id,
    &order_id,
    revenue_cents,
    latency_secs,
    span_ctx.trace_id,
    span_ctx.span_id,
);
```

## Receipt Integration

Every receipt now includes OpenTelemetry trace context for end-to-end traceability:

```rust
pub struct Receipt<T> {
    pub receipt_id: Uuid,
    pub timestamp: DateTime<Utc>,
    pub sequence: u64,
    pub correlation_id: Uuid,
    pub causation_id: Uuid,
    pub aggregate_id: Uuid,
    pub aggregate_type: String,
    pub payload: T,
    pub payload_hash: String,
    pub signature: Option<String>,
    // 🆕 OpenTelemetry trace context
    pub trace_id: Uuid,
    pub span_id: Uuid,
}
```

This enables:
- **Full traceability**: From HTTP request → command → event → receipt
- **Distributed debugging**: Follow trace ID across all services
- **Audit compliance**: Every receipt has immutable trace context

## Configuration

### Environment Variables

| Variable                          | Description                          | Default                   |
|-----------------------------------|--------------------------------------|---------------------------|
| `GCP_PROJECT_ID`                  | GCP project for Cloud Trace/Monitoring | (local OTLP if unset)   |
| `OTEL_EXPORTER_OTLP_ENDPOINT`     | OTLP endpoint (gRPC)                 | `http://localhost:4317`   |
| `OTEL_SAMPLING_RATIO`             | Trace sampling ratio (0.0-1.0)       | `1.0` (100%)              |
| `ENVIRONMENT`                     | Environment (dev/staging/prod)       | `dev`                     |

### Local Development

**Run local OTLP collector**:
```bash
docker run -d --name otel-collector \
  -p 4317:4317 \
  -p 4318:4318 \
  otel/opentelemetry-collector:latest
```

**Start kernel**:
```bash
cd kernel
OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4317 cargo run
```

**View traces/metrics**:
- Jaeger UI: http://localhost:16686 (if using Jaeger backend)
- Prometheus: http://localhost:9090 (if using Prometheus backend)

### GCP Production

**Set GCP project**:
```bash
export GCP_PROJECT_ID="your-project-id"
```

**Authenticate**:
```bash
gcloud auth application-default login
```

**Deploy**:
```bash
cd kernel
cargo build --release
./target/release/attribution-kernel
```

**View in GCP Console**:
- Cloud Trace: https://console.cloud.google.com/traces
- Cloud Monitoring: https://console.cloud.google.com/monitoring

## Testing

### Chicago TDD Tests

All OpenTelemetry modules include Chicago TDD tests:
- **State-based**: Verify observable outputs (span context, metrics, logs)
- **Real collaborators**: Use actual OTEL SDK, not mocks
- **Behavior verification**: Test what code *does*, not implementation details

**Run tests**:
```bash
cd world  # After generation
cargo test --test otel_integration_tests
```

**Example test**:
```rust
#[test]
fn test_span_context_creates_unique_ids() {
    // Arrange: Create two root span contexts
    let ctx1 = SpanContext::new_root();
    let ctx2 = SpanContext::new_root();

    // Act: Compare IDs
    let trace_ids_different = ctx1.trace_id != ctx2.trace_id;

    // Assert: IDs are unique (observable behavior)
    assert!(trace_ids_different, "Trace IDs should be unique");
}
```

## Performance

### SLOs

- **Span creation overhead**: < 1μs
- **Metric recording overhead**: < 100ns
- **Log emission overhead**: < 10μs (buffered)
- **OTLP export latency**: < 100ms (batch export every 60s)

### Sampling

For production with high traffic:
```bash
# Sample 10% of traces
export OTEL_SAMPLING_RATIO=0.1
```

**Head-based sampling**: Decision made at root span creation (consistent for entire trace).

## Troubleshooting

### Traces not appearing in GCP Cloud Trace

**Check authentication**:
```bash
gcloud auth application-default login
```

**Verify GCP_PROJECT_ID is set**:
```bash
echo $GCP_PROJECT_ID
```

**Check OTLP endpoint**:
```bash
# Should be https://cloudtrace.googleapis.com:443 for GCP
echo $OTEL_EXPORTER_OTLP_ENDPOINT
```

### Metrics not exporting

**Check export interval**:
- Default: 60 seconds
- Metrics are batched and exported periodically

**Verify OTLP endpoint is reachable**:
```bash
curl -v https://monitoring.googleapis.com
```

### Local OTLP collector not receiving data

**Check port binding**:
```bash
docker ps | grep otel-collector
netstat -an | grep 4317
```

**Check logs**:
```bash
docker logs otel-collector
```

## References

- [OpenTelemetry Specification](https://opentelemetry.io/docs/specs/otel/)
- [W3C Trace Context](https://www.w3.org/TR/trace-context/)
- [GCP Cloud Trace](https://cloud.google.com/trace/docs)
- [GCP Cloud Monitoring](https://cloud.google.com/monitoring/docs)
- [OTLP Specification](https://opentelemetry.io/docs/specs/otlp/)

## Next Steps

1. **Generate world modules**: Run `ggen sync` to generate instrumented code
2. **Start kernel**: Boot kernel with OTEL configuration
3. **Send test traffic**: Trigger affiliate clicks and conversions
4. **View traces**: Open GCP Cloud Trace console
5. **Query metrics**: Use Cloud Monitoring to build dashboards
6. **Analyze logs**: Use Cloud Logging with trace correlation

---

**Last Updated**: 2026-01-24
**Version**: 1.0.0 (FactoryPaaS v6.0.0)
