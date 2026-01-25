# Toyota Production System: Andon (安灯)

**Andon** (安灯, "safe light") is a visual control system in Toyota manufacturing for signaling and responding to problems.

## Core Philosophy

### Manufacturing Context

In Toyota factory floors:

1. **Problem Visibility**: When a worker spots a quality issue, they pull an Andon cord
2. **Visual Signal**: A light illuminates above the station:
   - **RED** = Critical problem (e.g., safety issue, major defect)
   - **YELLOW** = Warning (e.g., quality concern, deviation)
   - **GREEN** = Normal operation
3. **Immediate Response**: Team leads respond immediately to investigate
4. **Stop-the-Line**: If not resolved quickly, the entire production line stops (autonomic control)
5. **Root Cause Analysis**: Why did this happen? Implement countermeasures to prevent recurrence

### Software System Context

In software systems, **Andon means**:

- **Problems are visible**: Every failure is logged, measured, and traced
- **Automatic detection**: Don't wait for humans - thresholds trigger alerts
- **Fail fast**: Critical issues should stop processing immediately
- **No silent failures**: Observability is mandatory, not optional
- **Traceable**: Root cause is discoverable from logs and traces

## Implementation in ggen-tps-andon

The `ggen-tps-andon` crate implements Andon using five components:

### 1. Structured Logging (AndonLogger)

**Purpose**: Make problems visible through searchable logs

**Design**:
```
Log Entry → JSON Formatter → Sinks (file, syslog, cloud)
                           ↓
                    Handler Pipeline
                           ↓
                    Sampling (high-volume)
```

**Log Levels**:
- `DEBUG`: Detailed diagnostic information (for developers)
- `INFO`: General information about system operation
- `WARNING`: Andon signal - investigate before release (YELLOW cord)
- `CRITICAL`: Andon cord pulled - stop-the-line (RED cord)

**Features**:
- JSON structured logs (machine-readable for parsing)
- Multiple sinks (file rotation, syslog, cloud logging)
- Sampling for high-volume logs (don't overwhelm storage)
- Handler-based architecture (similar to Erlang Lager)

**Example**:
```rust
logger.critical("Queue overflow - STOP PROCESSING").await?;
// Output: {"timestamp":"2025-01-25T...", "level":"CRITICAL",
//          "message":"Queue overflow - STOP PROCESSING", ...}
```

### 2. Metrics (AndonMetrics)

**Purpose**: Quantify system state for threshold-based alerting

**Metric Types**:

| Type | Use Case | Example |
|------|----------|---------|
| **Counter** | Total occurrences | Failure count, signal count |
| **Gauge** | Current state | Queue depth, memory usage |
| **Histogram** | Distribution | Latency percentiles (p50, p99) |

**Key Metrics**:
- `andon_signals_total` - Total signals by color (RED/YELLOW/GREEN)
- `andon_failures_total` - Total failures by component
- `andon_queue_depth` - Current queue depth (gauge)
- `andon_memory_usage_mb` - Memory usage (gauge)
- `andon_request_latency_seconds` - Request latency (histogram)

**Threshold Monitoring**:
When metric crosses threshold → automatically fire alert

```rust
if queue_depth > 100 {
    // Alert: "Queue overflow - capacity exceeded"
}
```

**Prometheus Integration**:
- Metrics exposed at `/metrics` endpoint
- Prometheus scrapes metrics every 15 seconds
- Grafana visualizes metrics in real-time dashboards

### 3. Distributed Tracing (AndonTracer)

**Purpose**: Track requests through system for debugging

**Design**:
```
Request arrives
    ↓
Trace ID created (unique per request)
    ↓
Span created for each operation (with parent/child relationships)
    ↓
Trace exported to Jaeger (for visualization)
```

**Trace Context Propagation**:
- W3C `traceparent` header: `00-{trace-id}-{span-id}-{flags}`
- Baggage: metadata carried through entire trace
- Includes: user_id, request_path, session_id, etc.

**Example Trace**:
```
incoming-request (root span, 100ms)
  ├─ queue-processing (child span, 45ms)
  │   └─ event: processing_started
  ├─ failure-handling (child span, 40ms)
  │   └─ event: error_detected
  └─ response-serialization (child span, 15ms)
```

**Benefits**:
- See exact request path through system
- Identify bottlenecks (which spans took longest?)
- Understand failure cascades (which operation failed first?)
- Debug distributed systems (request touches multiple services)

### 4. Runtime Diagnostics (AndonObserver)

**Purpose**: Monitor system health and detect degradation

**Metrics Monitored**:
- Memory: total, used, available, percent
- CPU: overall usage percentage
- Processes: count of running processes
- Network: bytes in/out, connections
- Disk: space per mount point

**Health Status**:
- `HEALTHY`: All metrics normal
- `DEGRADED`: Some thresholds exceeded (e.g., memory >80%)
- `CRITICAL`: Multiple thresholds exceeded (e.g., memory >95%, CPU >95%)

**Scheduled Checks**:
- Every 60 seconds: run full diagnostics
- If degraded/critical: log warnings, trigger alerts
- History tracking: last 1000 diagnostics retained

**Equivalent to Erlang Recon**:
In Erlang/OTP systems, `recon` is used for:
```erlang
recon:info(memory).        % Memory usage
recon:proc_count(memory).  % Top memory-using processes
recon:tcp([]).             % Open TCP connections
```

In ggen-tps-andon, `AndonObserver` provides equivalent functionality:
```rust
observer.run_diagnostics().await?
observer.memory_usage()
observer.is_healthy()
```

### 5. Alert Management (AlertManager)

**Purpose**: Detect problems and notify teams

**Alert Lifecycle**:

```
Rule defined (thresholds, channels)
    ↓
Metric/log triggers condition
    ↓
Alert fired → send to channels (Slack, PagerDuty, email)
    ↓
Deduplication check (don't spam same alert)
    ↓
If not acknowledged → escalate (5 min)
    ↓
Team acknowledges → alert status: ACKNOWLEDGED
    ↓
Problem resolved → alert status: RESOLVED
```

**Alert Channels**:
- **Slack**: Team notifications in real-time
- **PagerDuty**: On-call escalation and incident management
- **Email**: Formal notifications and records
- **File**: Alert audit log
- **Stdout**: Development/testing

**Alert Deduplication**:
```
Alert: "Queue overflow"
  First fire: 10:00:00 → notify
  10:02:00 (within 5min window) → deduplicated, don't notify
  10:06:00 (after 5min window) → notify again
```

**Escalation**:
```
Alert fired at 10:00:00
  ↓ (not acknowledged)
Escalate at 10:05:00 → notify escalation_team
  ↓ (still not acknowledged)
Escalate at 10:10:00 → notify exec_team
  ↓ (still not acknowledged)
Auto-escalate at 10:15:00 → page on-call engineer
```

## Integration with MAPE-K Loop

In **ggen-domain**, the autonomic system uses MAPE-K (Monitor, Analyze, Plan, Execute, Knowledge):

```
┌─────────────────────────────────────────────┐
│          MAPE-K Autonomic Loop              │
├─────────────────────────────────────────────┤
│                                             │
│  Monitor (M) ← AndonObserver runs every 60s │
│    ↓                                        │
│  Analyze (A) ← Metrics vs Thresholds        │
│    ↓                                        │
│  Plan (P) ← AlertManager creates action     │
│    ↓                                        │
│  Execute (E) ← Alert fires, team responds   │
│    ↓                                        │
│  Knowledge (K) ← Trace stored for analysis  │
│    ↓                                        │
│    └─→ Loop back to Monitor                 │
│                                             │
└─────────────────────────────────────────────┘
```

## Dashboard & Visualization

### Prometheus + Grafana

**Metrics exposed**: `/metrics` endpoint (Prometheus format)

**Example Grafana dashboard**:

```
┌────────────────────────────────────────────────┐
│ Andon System Status                            │
├────────────────────────────────────────────────┤
│                                                │
│  Queue Depth          Memory Usage             │
│  ╔══════╗             ╔═════╗                  │
│  ║ 75   ║             ║ 82% ║                  │
│  ╚══════╝             ╚═════╝                  │
│  (normal)             (degraded)               │
│                                                │
│  CPU Usage            Active Alerts            │
│  ╔══════╗             ╔════╗                   │
│  ║ 45%  ║             ║ 3  ║                   │
│  ╚══════╝             ╚════╝                   │
│                                                │
│  Signal Distribution (24h)                     │
│  ┌─────────────────────────────────────────┐  │
│  │ 🟢 Green:   847  (95%)                  │  │
│  │ 🟡 Yellow:  40   (4%)                   │  │
│  │ 🔴 Red:     5    (1%)                   │  │
│  └─────────────────────────────────────────┘  │
│                                                │
└────────────────────────────────────────────────┘
```

### Jaeger Trace Visualization

Distributed traces visualized in Jaeger UI:

```
Service: api-server
Trace: trace-abc123
Duration: 250ms

Timeline:
|-------- incoming-request (250ms) --------|
  |--- queue-handler (120ms) ---|
    |-- process (80ms) --|
    |-- store (40ms) -|
  |--- response (50ms) --|
```

## Best Practices

### 1. Structured Logging
✅ DO:
```rust
logger.warn("Queue approaching threshold").await?;
logger.critical("Queue overflow detected").await?;
```

❌ DON'T:
```rust
eprintln!("queue overflow"); // not structured, not searchable
```

### 2. Metric Naming
✅ DO:
```rust
// Descriptive, hierarchical names
andon_queue_depth_items
andon_memory_usage_megabytes
andon_request_latency_seconds
```

❌ DON'T:
```rust
// Vague names
queue_size      // size in what units?
memory          // total or available?
latency         // latency of what operation?
```

### 3. Alert Threshold Tuning
✅ DO:
```rust
// Based on actual system characteristics
memory_threshold_percent: 80,  // leaf room for spikes
cpu_threshold_percent: 90,     // before performance degrades
```

❌ DON'T:
```rust
// Arbitrary thresholds
memory_threshold_percent: 50,  // too aggressive, too many false alarms
cpu_threshold_percent: 99,     // too lenient, problems not detected
```

### 4. Trace Context Propagation
✅ DO:
```rust
// Pass trace context to downstream services
let headers = tracer.w3c_headers();
let response = http_client.post(url)
    .headers(headers)  // Propagate trace
    .send()
    .await?;
```

❌ DON'T:
```rust
// Lost trace context
let response = http_client.post(url)
    .send()
    .await?;  // Trace broken, debugging harder
```

### 5. Alert Response
✅ DO:
```rust
// Acknowledge alerts to prevent escalation
alert_manager.acknowledge_alert(&alert_id).await?;
// Root cause analysis
investigate_trace(&trace_id).await?;
// Fix and resolve
alert_manager.resolve_alert(&alert_id).await?;
```

❌ DON'T:
```rust
// Ignore alerts
// (They will escalate and wake the on-call engineer)
```

## Production Deployment

### Environment Variables
```bash
# Logging
ANDON_LOG_LEVEL=INFO
ANDON_JSON_FORMAT=true
ANDON_SAMPLING_ENABLED=true
ANDON_SAMPLE_RATIO=10

# Metrics
ANDON_METRICS_ENABLED=true
ANDON_METRICS_PORT=9090

# Tracing
ANDON_TRACER_ENABLED=true
ANDON_SAMPLING_RATIO=0.01    # 1% in production
OTEL_EXPORTER_OTLP_ENDPOINT=http://jaeger:4317

# Observer
ANDON_OBSERVER_ENABLED=true
ANDON_MEMORY_THRESHOLD=80    # Alert if >80%
ANDON_CPU_THRESHOLD=90       # Alert if >90%

# Alerting
ANDON_ALERTS_ENABLED=true
SLACK_WEBHOOK_URL=https://hooks.slack.com/services/...
PAGERDUTY_API_KEY=...
```

### Docker Compose Example
```yaml
version: '3.8'
services:
  app:
    image: ggen-app:latest
    environment:
      ANDON_LOG_LEVEL: INFO
      OTEL_EXPORTER_OTLP_ENDPOINT: http://jaeger:4317
    ports:
      - "9090:9090"  # Prometheus /metrics endpoint

  prometheus:
    image: prom/prometheus:latest
    volumes:
      - ./prometheus.yml:/etc/prometheus/prometheus.yml
    ports:
      - "9091:9090"

  grafana:
    image: grafana/grafana:latest
    ports:
      - "3000:3000"
    environment:
      - GF_SECURITY_ADMIN_PASSWORD=admin

  jaeger:
    image: jaegertracing/all-in-one:latest
    ports:
      - "4317:4317"  # OTLP receiver
      - "16686:16686"  # UI
```

## Troubleshooting

### Problem: No logs appearing
**Debug**:
```bash
RUST_LOG=debug cargo run
# Check: Is log level set correctly?
# Check: Are sinks configured?
# Check: Is sampling too aggressive?
```

**Solution**:
```rust
let config = LogConfig {
    level: LogLevel::Debug,  // Lower threshold
    sampling_enabled: false, // Disable sampling temporarily
    ..Default::default()
};
```

### Problem: Metrics not updating
**Debug**:
```bash
curl http://localhost:9090/metrics
# Check: Is metrics collection enabled?
# Check: Are you calling metrics.update_*() ?
```

**Solution**:
- Verify `metrics.enabled = true` in config
- Call metrics update functions explicitly
- Check Prometheus scrape interval (default 15s)

### Problem: Traces not appearing in Jaeger
**Debug**:
```bash
# Check Jaeger is running
curl http://localhost:16686
# Check OTLP endpoint is configured
echo $OTEL_EXPORTER_OTLP_ENDPOINT
```

**Solution**:
```rust
let config = TracerConfig {
    enabled: true,
    otlp_endpoint: "http://jaeger:4317".to_string(),
    sampling_ratio: 1.0,  // Trace all (for testing)
    ..Default::default()
};
```

### Problem: Too many alerts (alert fatigue)
**Solution**:
- Increase dedup window: `dedup_window_minutes = 15`
- Tune thresholds higher: `memory_threshold_percent = 85`
- Use smarter conditions (e.g., "sustained high" not "spike")

## References

- **Toyota Production System (TPS)**: Shigeo Shingo, *Zero Quality Control*
- **Lean Manufacturing**: James Womack, *The Machine That Changed the World*
- **OpenTelemetry**: https://opentelemetry.io
- **Prometheus**: https://prometheus.io
- **Jaeger**: https://www.jaegertracing.io/
- **Erlang Lager**: https://github.com/basho/lager
- **Erlang Recon**: https://ferd.github.io/recon/

## Summary

Andon in software embodies TPS principles:

| Principle | Implementation | Component |
|-----------|----------------|-----------|
| **Visibility** | Structured JSON logs | AndonLogger |
| **Measurement** | Quantified metrics | AndonMetrics |
| **Traceability** | Distributed traces | AndonTracer |
| **Health** | Runtime diagnostics | AndonObserver |
| **Response** | Automatic alerting | AlertManager |

When a problem occurs:
1. **Visibility**: AndonLogger outputs WARNING/CRITICAL
2. **Measurement**: AndonMetrics crosses threshold
3. **Detection**: AlertManager fires alert
4. **Traceability**: AndonTracer provides full request path
5. **Diagnosis**: AndonObserver provides system state
6. **Response**: Team responds immediately (or system auto-escalates)
7. **Prevention**: Root cause analyzed from logs/traces, countermeasures implemented

**Result**: Problems are visible, detected, reported, debugged, and prevented from recurring.
