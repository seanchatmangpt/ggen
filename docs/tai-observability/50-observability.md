# TAI Observability: Production-Grade Observability for Rust Applications

**Version**: 0.1.0 | **Last Updated**: January 2026

This document describes the complete observability system for Rust applications with GCP Cloud integration, covering metrics, traces, profiles, and logs.

## Table of Contents

1. [Observability Philosophy](#observability-philosophy)
2. [Architecture Overview](#architecture-overview)
3. [Cloud Profiler Integration](#cloud-profiler-integration)
4. [Cloud Trace Distributed Tracing](#cloud-trace-distributed-tracing)
5. [Cloud Monitoring Metrics](#cloud-monitoring-metrics)
6. [Continuous Profiling Service](#continuous-profiling-service)
7. [Regression Detection](#regression-detection)
8. [Dashboard Design](#dashboard-design)
9. [Cost Optimization](#cost-optimization)
10. [Best Practices](#best-practices)

## Observability Philosophy

Observability is the ability to measure the internal state of a system by examining its outputs. In distributed systems, observability is built on **three pillars**:

### The Three Pillars of Observability

1. **Metrics**: Aggregated numeric data about system behavior
   - Gauges: Point-in-time values (CPU%, memory, connections)
   - Counters: Monotonically increasing (requests, errors, bytes)
   - Histograms: Distribution of values (latency, request size)
   - Example: `request_latency_ms = 145.2` at timestamp `2026-01-25T10:30:00Z`

2. **Traces**: End-to-end request flow through distributed system
   - Root span: Entry point (HTTP request)
   - Child spans: Sub-operations (database query, cache lookup, external API)
   - Attributes: Context metadata (user_id, resource_id, status_code)
   - Example: HTTP request → database query → cache lookup → response

3. **Logs**: Structured text records of discrete events
   - Structured: JSON with key=value pairs
   - Contextual: Include trace_id, span_id, request_id
   - Level-based: DEBUG, INFO, WARN, ERROR, FATAL
   - Example: `{"trace_id":"abc123", "level":"ERROR", "message":"Connection timeout"}`

**Why Three Pillars?**
- **Metrics**: Efficient storage, shows what happened at scale (millions of data points)
- **Traces**: Shows why it happened in one request (request flow, timing, errors)
- **Logs**: Shows how it happened (detailed events, state changes)

Together: Metrics guide investigation → traces find the problem → logs explain the details.

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                    Application (Rust Service)                   │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │ Request Handler                                          │   │
│  │  - Span creation (root span per request)                │   │
│  │  - Metric collection (latency, throughput)             │   │
│  │  - Log correlation (trace_id injection)                │   │
│  └──────────────────────────────────────────────────────────┘   │
│                           │                                       │
│        ┌──────────────────┼──────────────────┐                   │
│        ↓                  ↓                  ↓                   │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐            │
│  │   Metrics    │  │    Traces    │  │  Profiler    │            │
│  │ (Gauge,      │  │  (Spans,     │  │  (CPU, heap, │            │
│  │  Counter,    │  │   attrs,     │  │   thread)    │            │
│  │  Histogram)  │  │   events)    │  │              │            │
│  └──────────────┘  └──────────────┘  └──────────────┘            │
│        │                  │                  │                    │
│        └──────────────────┼──────────────────┘                   │
│                           ↓                                       │
│                  ┌──────────────────┐                             │
│                  │ Observability    │                             │
│                  │ Client (Batch    │                             │
│                  │ Export Layer)    │                             │
│                  └────────┬─────────┘                             │
└─────────────────────────┼──────────────────────────────────────┘
                          │ HTTP/gRPC (batched)
                          ↓
        ┌─────────────────────────────────────┐
        │     Google Cloud Observability      │
        │  ┌─────────────┐  ┌──────────────┐  │
        │  │   Cloud     │  │    Cloud     │  │
        │  │   Trace     │  │  Monitoring  │  │
        │  └─────────────┘  └──────────────┘  │
        │  ┌─────────────┐  ┌──────────────┐  │
        │  │   Cloud     │  │   Cloud      │  │
        │  │  Profiler   │  │   Logging    │  │
        │  └─────────────┘  └──────────────┘  │
        └─────────────────────────────────────┘
                          │
        ┌─────────────────┼─────────────────┐
        ↓                 ↓                 ↓
    ┌────────┐      ┌────────┐      ┌────────┐
    │Dashboards     │Alerts  │    │Reports │
    │(Visualization)│(Anom.)│      │(SLO)   │
    └────────┘      └────────┘      └────────┘
```

## Cloud Profiler Integration

### What is Profiling?

Profiling captures runtime behavior:
- **CPU Profiling**: Which functions consume CPU time?
- **Heap Profiling**: Where are allocations happening?
- **Thread Profiling**: How many threads? What are they doing?

### CPU Profiling (100 samples/second)

```rust
use tai_observability::cloud_profiler::CloudProfiler;
use std::sync::Arc;

#[tokio::main]
async fn main() {
    let profiler = Arc::new(
        CloudProfiler::new("my-project".to_string(), 100) // 100 samples/sec
    );

    // Start CPU profile
    profiler.start_cpu_profile("profile-1".to_string()).await?;

    // Do work... (profiler collects samples)

    // Finish profile
    let profile = profiler.finish_cpu_profile().await?;

    // Analyze hot paths
    let hot = profile.hot_paths(10); // Top 10 functions by CPU time
    for (func, cpu_micros) in hot {
        println!("{}: {} us", func, cpu_micros);
    }
}
```

### Profile Comparison (Regression Detection)

```rust
// Set baseline (from previous deployment)
let baseline = profiler.get_completed_profile("baseline-v1").await?;
profiler.set_baseline_profile(baseline).await;

// Collect new profile
profiler.start_cpu_profile("current".to_string()).await?;
// ... work ...
profiler.finish_cpu_profile().await?;

// Compare: Are there regressions?
let comparison = profiler.compare_with_baseline().await?;
for regression in comparison.regressions {
    // Regression: function_name increased by 25%
    eprintln!(
        "REGRESSION: {} increased by {:.1}%",
        regression.function, regression.increase_percent
    );
}
```

### Flame Graph Generation

Flame graphs visualize CPU usage as stacked rectangles:

```
main ────────────────────────────────────────────────────
  process_request ──────────────────────────────
    parse_body ──────────┐
    validate ────┐       │
    database_query ─────────────────────
      prepare_statement ───────┐
      execute ────────────────────────
    serialize ─────┐
```

Width = CPU time, Height = call depth

```rust
// Generate flame graph data (stackcollapse format)
let graph_data = profiler.generate_flame_graph_data().await?;
// Output format: "main;process;database 50000" (50ms of CPU time)

// Use with tools:
// - flamegraph crate: generates SVG visualization
// - Google Cloud Profiler: automatic flame graph UI
```

### Production-Safe Profiling (<5% CPU Overhead)

```
Overhead calculation:
- Sample rate: 100 samples/second
- Per sample: ~100-200 nanoseconds (stack capture + storage)
- Total: 100 * 150ns = 15μs per second
- As % of 1 second: 15μs / 1s = 0.0015% overhead ✓

Real overhead factors:
- L1 cache misses from sampling code: +2-3%
- Memory writes for sample storage: +1-2%
- Total: ~3-5% CPU overhead (acceptable)
```

Verification:

```rust
// Check if overhead is within limits
let within_limits = profiler.check_overhead().await?;
assert!(within_limits, "Profiler overhead exceeds 5%");
```

## Cloud Trace Distributed Tracing

### Root Span Per Request

Every incoming request gets a root span with a unique trace_id:

```rust
let tracer = CloudTrace::new("my-project".to_string(), 0.01); // 1% sampling

// Handle HTTP request
let trace_id = tracer.start_trace("http-request".to_string()).await?;
// trace_id = "abc123def456..." (UUID)

// Set attributes
tracer.set_span_attribute(
    &trace_id,
    &root_span_id,
    "http.method".to_string(),
    "POST".to_string(),
).await?;

tracer.set_span_attribute(
    &trace_id,
    &root_span_id,
    "http.status_code".to_string(),
    "200".to_string(),
).await?;

// Finish trace
tracer.finish_trace(&trace_id).await?;
```

### Span Hierarchy (Parent/Child Relationships)

```
Root Span: HTTP POST /users
├── Child: Database Query
│   ├── Grandchild: Prepare Statement (2ms)
│   └── Grandchild: Execute (15ms)
├── Child: Cache Lookup (1ms)
└── Child: Serialize Response (3ms)

Total latency: 21ms
```

Implementation:

```rust
// Create root span
let root_span = tracer.start_trace("request".to_string()).await?;

// Create child spans
let db_span = tracer.create_span(
    &root_span,
    "database".to_string(),
    None // No parent yet
).await?;

let query_span = tracer.create_span(
    &root_span,
    "query".to_string(),
    Some(db_span.clone()) // Parent = db_span
).await?;

// Child spans inherit trace_id, but have their own span_id
// Parent-child relationship tracked for visualization
```

### Span Timing (Automatic Latency Measurement)

Each span automatically captures duration:

```rust
let span = TraceSpan::new("my-operation".to_string());
// ... do work ...
span.finish(SpanStatus::Ok);

// Duration calculated automatically
let duration = span.get_duration();
// Returns: Some(Duration { secs: 0, nanos: 45000000 }) = 45ms
```

### Sampling (100% Dev, 1% Prod)

Sampling reduces cost and storage but maintains statistical validity:

```rust
// Dev environment: 100% sampling
let tracer_dev = CloudTrace::new("project".to_string(), 1.0);

// Prod environment: 1% sampling
let tracer_prod = CloudTrace::new("project".to_string(), 0.01);

// Each request:
// - 1% goes to Cloud Trace (1 in 100 requests)
// - 99% stay in-memory or discarded
// - Still captures 99th percentile latency (1% sampling is statistically valid)
```

### Batch Export

Traces are batched to reduce API calls:

```rust
// Export every 100 traces or every 30 seconds
let (count, traces) = tracer.batch_export().await?;
tracer.upload_traces(&traces).await?;

// Network efficiency:
// - 100 traces per batch
// - ~5KB per trace = 500KB per batch
// - 1 API call per batch (not per trace)
// - 1000x reduction in API calls vs per-trace export
```

## Cloud Monitoring Metrics

### Custom Metrics

Define application-specific metrics:

```rust
use tai_observability::gcp_monitoring::{
    CloudMonitoring, MetricDescriptor, MetricType, MetricUnit
};

let monitoring = CloudMonitoring::new("my-project".to_string());

// Define custom metric
let descriptor = MetricDescriptor {
    metric_type: "custom.googleapis.com/myapp/request_latency".to_string(),
    display_name: "Request Latency".to_string(),
    value_type: MetricType::Distribution,
    unit: MetricUnit::Milliseconds,
    description: "HTTP request latency in milliseconds".to_string(),
    labels: vec![
        // Dimensions for segmentation
        LabelDescriptor {
            key: "method".to_string(),
            description: "HTTP method (GET, POST, etc.)".to_string(),
            value_type: "string".to_string(),
        },
        LabelDescriptor {
            key: "endpoint".to_string(),
            description: "API endpoint path".to_string(),
            value_type: "string".to_string(),
        },
    ],
};

monitoring.create_metric_descriptor(descriptor).await?;
```

### Metric Types

**Gauge** - Instantaneous value (can go up or down)
```rust
// CPU usage at a point in time
monitoring.write_metric_point(
    "custom.googleapis.com/app/cpu_usage",
    MetricValue::Double(42.5),  // 42.5% CPU
    labels,
).await?;
```

**Counter** - Monotonically increasing (only goes up)
```rust
// Total requests processed
monitoring.write_metric_point(
    "custom.googleapis.com/app/total_requests",
    MetricValue::Int64(100_000),
    labels,
).await?;
```

**Histogram/Distribution** - Value distribution with percentiles
```rust
// Request latency distribution
let dist = DistributionValue {
    count: 1000,
    mean: 150.0,  // 150ms average
    sum_of_squared_deviation: 50000.0,
    percentiles: vec![
        ("p50".to_string(), 120.0),   // 50th percentile
        ("p95".to_string(), 300.0),   // 95th percentile
        ("p99".to_string(), 500.0),   // 99th percentile
    ].into_iter().collect(),
};

monitoring.write_metric_point(
    "custom.googleapis.com/app/latency_distribution",
    MetricValue::Distribution(dist),
    labels,
).await?;
```

### Metric Querying

```rust
// Get latest value
let value = monitoring.get_latest_metric_value(
    "custom.googleapis.com/app/cpu_usage"
).await?;

// Query time series (last 1000 data points)
let series = monitoring.query_time_series(
    "custom.googleapis.com/app/cpu_usage",
    1000
).await?;

// Get statistics
let stats = monitoring.get_metric_statistics(
    "custom.googleapis.com/app/latency"
).await?;
println!("Mean: {}ms, P99: {}ms", stats.mean, stats.percentiles.get("p99"));
```

### Alerting Rules

```rust
use tai_observability::gcp_monitoring::{
    AlertingRule, ComparisonOperator
};

let rule = AlertingRule {
    id: "high-error-rate".to_string(),
    display_name: "Error Rate > 5%".to_string(),
    metric_type: "custom.googleapis.com/app/error_rate".to_string(),
    threshold: 5.0,
    comparison_operator: ComparisonOperator::GreaterThan,
    evaluation_window_secs: 300,  // 5 minutes
    trigger_count: 2,  // Alert if threshold exceeded 2+ windows
    notification_channels: vec![
        "projects/my-project/notificationChannels/123".to_string()
    ],
};

monitoring.create_alerting_rule(rule).await?;

// Check if threshold is exceeded
let exceeds = monitoring.check_threshold(
    "custom.googleapis.com/app/error_rate",
    5.0,
    ComparisonOperator::GreaterThan,
).await?;

if exceeds {
    eprintln!("ALERT: Error rate exceeded threshold!");
}
```

## Continuous Profiling Service

### Automatic Production Profiling

The continuous profiling service runs in the background, always profiling:

```rust
use tai_observability::continuous_profiling::ContinuousProfilingService;
use std::sync::Arc;

// Create service
let profiler = Arc::new(
    CloudProfiler::new("my-project".to_string(), 100)
);
let service = ContinuousProfilingService::new(
    profiler.clone(),
    60,    // Profile collection interval: 60 seconds
    300,   // Upload interval: 5 minutes
    10.0   // Regression threshold: 10% increase
);

// Start service (runs in background)
service.start().await?;

// Service automatically:
// 1. Collects CPU profile every 60 seconds
// 2. Uploads profiles to Cloud Profiler every 5 minutes
// 3. Detects regressions (>10% CPU increase)
// 4. Triggers alerts for critical regressions

// Get events/statistics
let stats = service.get_statistics().await?;
println!("Profiles collected: {}", stats.profiles_finished);
println!("Regressions detected: {}", stats.total_alerts);

// Stop service gracefully
service.stop().await?;
```

### Event Logging

The service logs all profiling events:

```
ProfileStarted: profile-1234 started
ProfileFinished: profile-1234 finished (1000 samples, 342ms CPU time)
RegressionDetected: function_a increased by 15% (threshold: 10%)
AlertTriggered: MEDIUM severity alert (multiple regressions)
ProfileUploaded: profile-1234 uploaded to Cloud Profiler
```

### Regression Detection

```rust
let comparison = profiler.compare_with_baseline().await?;

for regression in comparison.regressions {
    eprintln!(
        "REGRESSION: {} increased from {}us to {}us ({:.1}%)",
        regression.function,
        regression.baseline_micros,
        regression.current_micros,
        regression.increase_percent
    );
}

for improvement in comparison.improvements {
    println!(
        "IMPROVEMENT: {} decreased from {}us to {}us ({:.1}%)",
        improvement.function,
        improvement.baseline_micros,
        improvement.current_micros,
        improvement.reduction_percent
    );
}
```

## Regression Detection

### Automatic Comparison

When a new profile is collected, it's compared to baseline:

```
Baseline Profile        Current Profile        Status
─────────────────       ───────────────        ──────
func_a: 100ms    vs    func_a: 115ms          +15% ⚠️ REGRESSION
func_b: 50ms     vs    func_b: 48ms           -4% ✓ Improvement
func_c: 30ms     vs    func_c: 31ms           +3% (below threshold)
func_d: 25ms     vs    (not called)           -100% ✓ Removed
```

### Threshold Configuration

```rust
let service = ContinuousProfilingService::new(
    profiler.clone(),
    60,    // Interval: 60s
    300,   // Upload: 300s
    10.0   // Threshold: 10% - only alert if increase > 10%
);

// Regressions < 10% are ignored
// Regressions >= 10% trigger alerts with severity based on percentage
```

### Alert Severity Levels

```
5-10% regression   → LOW severity
10-25% regression  → MEDIUM severity
25-50% regression  → HIGH severity
>50% regression    → CRITICAL severity (requires immediate attention)
```

### Integration with Continuous Profiling

```rust
// Service automatically:
1. Collect profile every 60 seconds
2. Compare with baseline
3. Detect regressions > 10%
4. Create alert with severity level
5. Trigger notification (Slack, PagerDuty, email)
6. Store alert in database for alerting dashboard

// Example output:
RegressionAlert {
    timestamp: 2026-01-25T10:30:00Z,
    alert_id: "alert-abc123",
    regressions: vec![
        Regression {
            function: "database_query",
            baseline_micros: 50_000,
            current_micros: 75_000,
            increase_percent: 50.0,  // CRITICAL threshold exceeded
        }
    ],
    threshold_percent: 10.0,
    severity: AlertSeverity::Critical,
}
```

## Dashboard Design

### Three-Tier Dashboard Hierarchy

**Tier 1: Executive Dashboard** (SLO status at a glance)
```
┌─────────────────────────────────────────────┐
│ Service: Payment Processing                 │
│ ┌──────────┐ ┌──────────┐ ┌──────────┐    │
│ │ Uptime   │ │ Latency  │ │ Errors   │    │
│ │ 99.95%   │ │ 145ms    │ │ 0.02%    │    │
│ │ ✓ PASS   │ │ ✓ PASS   │ │ ✓ PASS   │    │
│ └──────────┘ └──────────┘ └──────────┘    │
│                                             │
│ Last 24 hours - All metrics green          │
└─────────────────────────────────────────────┘
```

**Tier 2: Operations Dashboard** (SLI trends & incidents)
```
┌─────────────────────────────────────────────┐
│ Request Latency (Last 24h)                  │
│ 500ms │     ╱╲        ╱╲╱╲                 │
│       │    ╱  ╲      ╱      ╲               │
│ 250ms │   ╱    ╲    ╱        ╲  ╱           │
│       │  ╱      ╲──╱          ╲╱────        │
│   0ms │                                     │
│       └─────────────────────────────────    │
│       Spike at 14:30 (DB migration)        │
│       P50: 145ms | P95: 250ms | P99: 450ms│
└─────────────────────────────────────────────┘

Regressions:
├── database_query: +25% (MEDIUM) at 14:30
├── cache_lookup: +5% (OK, <10% threshold)
└── serialize: -3% (IMPROVEMENT)
```

**Tier 3: Engineering Dashboard** (Detailed investigation)
```
Trace Analysis (Sample trace from spike)
ROOT: HTTP POST /payments
  Duration: 450ms
  ├── validate_request (10ms)  ✓
  ├── acquire_lock (50ms)      ✓
  ├── database_write (350ms)   ⚠️ SLOW
  │   ├── prepare_statement (50ms)
  │   ├── execute (280ms)      ← Problem here!
  │   │   └── Slow query log shows: Missing index
  │   └── commit (20ms)
  ├── log_transaction (15ms)   ✓
  └── serialize_response (25ms) ✓

CPU Profile Flame Graph:
  database_query: 78% of CPU time
    └── execute: 75% of CPU time
        └── index_scan: 65% (should be index seek!)

Action: Add missing database index
```

### Key Metrics to Monitor

**Golden Four** (most important):
1. **Latency** (response time)
   - Metric: `request_latency_ms` (p50, p95, p99)
   - SLO: `p99 < 500ms`

2. **Traffic** (request volume)
   - Metric: `requests_per_second`
   - SLO: `handle 10k req/s`

3. **Errors** (failure rate)
   - Metric: `error_rate_percent = errors / total_requests`
   - SLO: `error_rate < 0.1%`

4. **Saturation** (resource usage)
   - Metrics: `cpu_percent`, `memory_percent`, `connection_pool_usage`
   - SLO: `cpu < 70%`, `memory < 80%`

### Custom Dashboard Configuration

```json
{
  "title": "Payment Service",
  "refresh_interval": "30s",
  "rows": [
    {
      "title": "SLO Status",
      "panels": [
        {
          "title": "Latency (p99)",
          "metric": "custom.googleapis.com/app/latency",
          "query": "percentile(request_latency_ms, 99)",
          "threshold": 500,
          "alert_if_exceeds": true
        },
        {
          "title": "Error Rate",
          "metric": "custom.googleapis.com/app/error_rate",
          "query": "errors / total_requests * 100",
          "threshold": 0.1,
          "alert_if_exceeds": true
        }
      ]
    }
  ]
}
```

## Cost Optimization

### Sampling Strategy

**Development**: 100% sampling
```rust
let tracer = CloudTrace::new("project".to_string(), 1.0);
// Every request traced
// Cost: ~$0.01 per 1000 traces
// For 100 req/sec = ~$86k/month (acceptable in dev)
```

**Staging**: 10% sampling
```rust
let tracer = CloudTrace::new("project".to_string(), 0.1);
// 1 in 10 requests traced
// Still captures 99th percentile latency (valid statistically)
// Cost: ~$8.6k/month (more reasonable)
```

**Production**: 1% sampling
```rust
let tracer = CloudTrace::new("project".to_string(), 0.01);
// 1 in 100 requests traced
// For high-volume services: statistically valid with 1% sampling
// Cost: ~$860/month (acceptable at scale)

// Exception: Trace errors at 100%
let trace_id = if error {
    tracer.start_trace("error-trace".to_string()).await?
} else {
    tracer.start_trace("request".to_string()).await? // May not be sampled
};
```

### Profile Batching

**Without Batching** (inefficient):
```
Profile 1 → upload
Profile 2 → upload
Profile 3 → upload
...
= 1000s of API calls = expensive
```

**With Batching** (efficient):
```
[Profile 1, 2, 3, ..., 100] → single API call
[Profile 101, 102, ..., 200] → single API call
...
= 10s of API calls = 100x reduction
```

Configuration:
```rust
service = ContinuousProfilingService::new(
    profiler,
    60,    // Collect every 60s
    300,   // Upload every 300s (batch ~5 profiles)
    10.0
);
```

### Retention Policies

Keep recent data (hot tier), archive old data (cold tier):

```
1 day:  Real-time monitoring (high detail)
 └─> 100% of data retained

1 week: Hourly aggregation (medium detail)
 └─> Aggregated to p50, p95, p99

1 month: Daily aggregation (low detail)
 └─> Only SLO violations retained

> 1 month: Archive to Cloud Storage (cheap)
 └─> For compliance, post-mortems, audits
```

**Cost Impact**:
- High-resolution (1-day retention): Expensive
- Aggregated (7-day retention): Moderate
- Archive (1-month+ retention): Cheap (~$0.01 per GB/month)

## Best Practices

### 1. Instrument at Request Boundaries

```rust
// Middleware for web framework
#[axum::middleware]
async fn observability_middleware(
    request: Request,
    next: Next,
) -> Response {
    let trace_id = uuid::Uuid::new_v4().to_string();
    let span_id = tracer.create_span(...).await?;

    // Inject trace context into request
    let response = next.run(request).await;

    // Add span attributes
    tracer.set_span_attribute(
        &trace_id,
        &span_id,
        "http.status_code".to_string(),
        response.status().to_string(),
    ).await?;

    tracer.finish_span(&trace_id, &span_id, SpanStatus::Ok).await?;

    response
}
```

### 2. Tag Traces with Business Context

```rust
// Include meaningful identifiers
tracer.set_span_attribute(
    &trace_id,
    &span_id,
    "user_id".to_string(),
    user.id.to_string(),
).await?;

tracer.set_span_attribute(
    &trace_id,
    &span_id,
    "order_id".to_string(),
    order.id.to_string(),
).await?;

// Enables queries like:
// "Show me all traces for user 123"
// "Show me all slow traces (>500ms) for this order"
```

### 3. Use Structured Logging with Trace Context

```rust
// Log with trace correlation
tracing::info!(
    trace_id = %trace_id,
    span_id = %span_id,
    user_id = %user.id,
    "Processing payment request"
);

// Logs are automatically correlated with traces
// Query in Cloud Logging:
// resource.type="k8s_container" AND "trace_id"="abc123"
// Returns all logs from that request
```

### 4. Profile Regularly, Not Just on Errors

```rust
// Common mistake: Only profile on errors
// Better: Profile continuously (even successful requests)

let service = ContinuousProfilingService::new(
    profiler.clone(),
    60,  // Profile every 60s
    300, // Upload every 5 minutes
    10.0 // 10% threshold
);
service.start().await?;

// Catches performance regressions that don't cause errors:
// - Slow queries due to missing indices
// - Unnecessary memory allocations
// - Lock contention under load
```

### 5. Set SLOs Before Alerting

Don't alert on everything; focus on user-impacting metrics:

```rust
// Good SLO (user-visible)
SLO {
    metric: "request_latency_p99",
    threshold: 500ms,
    window: "30-day rolling",
    error_budget: 99.9%, // Can fail 0.1% of requests
}

// Bad SLO (implementation detail)
SLO {
    metric: "database_connection_pool_size",
    threshold: 10, // Who cares if it's 9 or 11?
}
```

### 6. Use Percentiles, Not Averages

```rust
// Bad: Average latency
avg_latency = (100 + 100 + 100 + 100 + 5000) / 5 = 1060ms
// 80% of users saw fast response, 20% saw slow response

// Good: Percentiles
p50: 100ms (50% of requests)
p95: 100ms (95% of requests)
p99: 5000ms (99% of requests, slowest 1%)
// Immediately visible: 1% of users have poor experience
```

### 7. Correlate Metrics, Traces, Logs

When investigating an incident:

```
Metric Alert: "Error rate exceeded 5%"
     ↓
Query Traces: "Show traces where status=ERROR"
     ↓
Identify Pattern: "All errors in database_write span"
     ↓
Query Logs: "Show logs for trace_id=xyz with 'database'"
     ↓
Root Cause: "Connection pool exhausted - timeout waiting for connection"
     ↓
Fix: "Increase pool size" or "Optimize query to release connections faster"
```

## Conclusion

Observability is not a feature—it's a practice. By implementing the three pillars (metrics, traces, logs) with proper sampling, batching, and alerting, you gain visibility into production systems and can respond quickly to incidents.

**Remember**: You can't optimize what you don't measure. Instrument first, then measure, then optimize.

---

**Related Resources**:
- [Google Cloud Profiler Documentation](https://cloud.google.com/profiler/docs)
- [Cloud Trace Concepts](https://cloud.google.com/trace/docs/concepts)
- [Cloud Monitoring Best Practices](https://cloud.google.com/monitoring/kubernetes-engine/best-practices)
- [Observability Engineering (Book)](https://www.oreilly.com/library/view/observability-engineering/9781492076438/) by Mateo & Beyer
