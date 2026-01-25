# ggen-tps-andon: Toyota Production System Andon

Production-grade Andon system implementing Toyota Production System principles for software.

## What is Andon?

In Toyota manufacturing, **Andon** (安灯, "a light") is a visual control system that signals problems:

- A worker sees an issue and pulls a cord
- A light illuminates above the station (RED = critical, YELLOW = warning, GREEN = normal)
- Team members respond immediately
- If not fixed quickly, the entire production line stops (jidoka - autonomic response)

In software, **Andon means**:
- Every failure is **visible** (structured logs, metrics, traces)
- Problems are **detected automatically** (thresholds trigger alerts)
- Critical issues **stop processing** (fail fast, don't hide errors)
- **No silent failures** - observability is mandatory, not optional

## Core Components

### 1. AndonLogger - Structured Logging
- JSON structured logs (machine-readable)
- Log levels: DEBUG, INFO, WARNING (Andon signal), CRITICAL (stop-the-line)
- Multiple sinks: file, syslog, cloud logging (Stackdriver)
- Sampling for high-volume logs (don't overwhelm the system)
- Handler-based architecture (similar to Erlang Lager)

```rust
let logger = AndonLogger::new(config)?;
logger.warn("Queue approaching threshold").await?;
logger.critical("Queue overflow - STOP").await?;
```

### 2. AndonMetrics - Prometheus Metrics
- Counters: signal count, failure count, alert count
- Gauges: queue depth, pool utilization, memory usage
- Histograms: request latency, processing time
- Threshold monitoring: when metric crosses threshold, trigger alert
- Prometheus scrape endpoint: `/metrics`

```rust
let metrics = AndonMetrics::new(config)?;
metrics.record_signal(&signal).await?;
metrics.update_queue_depth("default", 150)?;
metrics.record_request_latency("/api/test", duration)?;
```

### 3. AndonTracer - Distributed Tracing
- OpenTelemetry compatible
- Request ID (trace ID) tracked through entire system
- Span context with parent/child relationships
- W3C Trace Context headers for interop with other systems
- Integration with Jaeger for visualization
- Baggage for carrying metadata through trace

```rust
let tracer = AndonTracer::new(config)?;
let span_id = tracer.start_span("request-processing")?;
tracer.add_span_attribute(&span_id, "user_id", json!("123"))?;
tracer.end_span(&span_id, SpanStatus::Ok)?;
```

### 4. AndonObserver - Runtime Diagnostics
- System health checks: memory, CPU, processes
- Network metrics
- Disk usage
- Similar to Erlang Recon (runtime diagnostics)
- Scheduled health checks every 60 seconds
- Alerts when thresholds crossed

```rust
let observer = AndonObserver::new(config)?;
let metrics = observer.run_diagnostics().await?;
println!("Memory: {}%", metrics.memory_percent);
println!("Health: {:?}", metrics.status);
```

### 5. AlertManager - Threshold-Based Alerting
- Alert rules based on metrics and events
- Multiple channels: PagerDuty, Slack, email, file
- Deduplication: don't spam same alert
- Escalation: if not acknowledged in N minutes, escalate
- When critical, collect diagnostics snapshot

```rust
let mut alerts = AlertManager::new(config, metrics)?;
alerts.add_rule(AlertRule {
    name: "high-queue".to_string(),
    condition: AlertCondition::QueueDepth { max_depth: 100 },
    severity: AlertSeverity::Critical,
    channels: vec![AlertChannel::Slack { ... }],
    ..Default::default()
})?;

alerts.check_and_fire_alerts().await?;
```

## Complete Example

```rust
use ggen_tps_andon::{AndonConfig, AndonSystem, AndonSignal};

#[tokio::main]
async fn main() -> Result<()> {
    // Configure all components
    let config = AndonConfig::default();

    // Create unified Andon system
    let system = AndonSystem::new(config).await?;

    // Signal a problem (pull the Andon cord)
    let signal = AndonSignal::red("Queue overflow - stop processing")
        .with_component("queue-handler")
        .with_trace_id("trace-123");

    system.signal_problem(signal).await?;

    // System automatically:
    // 1. Logs at CRITICAL level
    // 2. Records metric
    // 3. Creates trace span
    // 4. Fires alerts
    // 5. Collects diagnostics

    system.shutdown().await?;
    Ok(())
}
```

## Configuration

### Logger Config
```toml
[logger]
level = "INFO"           # DEBUG, INFO, WARNING, CRITICAL
json_format = true       # JSON structured logs
sampling_enabled = true  # Sample high-volume logs
sample_ratio = 10        # Log 1 in every 10 messages
sinks = ["Stdout"]       # File, Syslog, CloudLogging, Stdout, Stderr
```

### Metrics Config
```toml
[metrics]
enabled = true
port = 9090
scrape_path = "/metrics"
buckets = [0.001, 0.005, 0.01, ..., 10.0]  # Histogram buckets

[metrics.thresholds]
queue_depth = 100
memory_usage_mb = 512
cpu_usage_percent = 90
```

### Tracer Config
```toml
[tracer]
service_name = "ggen-andon"
enabled = true
sampling_ratio = 0.01    # 1% in production, 1.0 in dev
otlp_endpoint = "http://localhost:4317"
```

### Observer Config
```toml
[observer]
enabled = true
check_interval_secs = 60
memory_threshold_percent = 80
cpu_threshold_percent = 90
include_system_metrics = true
include_network_metrics = true
include_disk_metrics = true
```

## Integration with Other Systems

### With ggen-domain (MAPE-K Loop)
- Monitor (M): AndonObserver collects metrics
- Analyze (A): Metrics compared to thresholds
- Plan (P): AlertManager creates action plan
- Execute (E): Alerts trigger corrective actions
- Knowledge (K): Traces stored for postmortem analysis

### With ggen-core (Code Generation)
- Every generated component emits Andon signals
- Failed generations create RED signals
- Slow generations create YELLOW signals
- Successfully generated code creates GREEN signals

### With External Systems
- **Prometheus**: Scrape `/metrics` endpoint
- **Jaeger**: Export spans via OTLP
- **PagerDuty**: Alert escalation
- **Slack**: Team notifications
- **Cloud Logging**: Centralized log aggregation

## Andon in Production

### Best Practices
1. **Problem Visibility**: Make failures visible immediately (structured logs + metrics + traces)
2. **Automatic Detection**: Don't wait for humans to notice - thresholds trigger alerts
3. **Stop-the-Line**: Critical issues should halt processing (fail fast, fail loud)
4. **Root Cause Analysis**: Traces show exact request path for debugging
5. **Prevent Recurrence**: Use historical data to identify patterns and prevent future failures

### SLOs for Andon System
- Log message latency: <100ms
- Metric update latency: <50ms
- Alert response time: <5s
- Trace export latency: <500ms
- Observer diagnostics: <2s (every 60s)

## Testing

```bash
# Run all tests
cargo test --all

# Run with logging
RUST_LOG=debug cargo test -- --nocapture

# Run example
cargo run --example andon_demo
```

## Production Readiness

- [x] Type-safe APIs (Result<T,E> for all fallible ops)
- [x] No unwrap/expect in production code
- [x] Error context mapping (thiserror)
- [x] Chicago TDD pattern (AAA with real objects)
- [x] Comprehensive documentation
- [x] Performance-aware design (sampling, batching)
- [x] Fail-safe semantics (alerts don't crash system)

## References

- **Toyota Production System**: Shigeo Shingo, "Zero Quality Control"
- **OpenTelemetry**: https://opentelemetry.io
- **Prometheus**: https://prometheus.io
- **Erlang/OTP Lager**: https://github.com/basho/lager
- **Recon**: https://ferd.github.io/recon/

## License

MIT

## Author

ggen contributors <sean@chatmangpt.com>
