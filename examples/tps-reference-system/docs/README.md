# TPS Reference System - Visual Management with Grafana + Prometheus

A production-ready visual management system implementing all 6 Toyota Production System (TPS) principles using Grafana dashboards and Prometheus metrics.

## Overview

This reference system demonstrates how to visualize TPS principles in a software system:

- **Jidoka**: Autonomation and failure detection (circuit breakers)
- **Kanban**: Pull-based work distribution (queue management)
- **Andon**: Problem visibility (immediate alerts)
- **Kaizen**: Continuous improvement (SLO tracking)
- **Heijunka**: Load leveling (resource distribution)
- **Tracing**: Value stream visualization (component latency)

## Quick Start

### Prerequisites

- Docker & Docker Compose
- Prometheus 2.0+
- Grafana 8.0+
- Application emitting TPS metrics

### 1. Start Prometheus and Grafana

```bash
cd examples/tps-reference-system

# Copy configurations
cp prometheus/prometheus.yml /path/to/prometheus/config/
cp prometheus/alert-rules.yml /path/to/prometheus/config/
cp grafana/provisioning/dashboards.yaml /path/to/grafana/provisioning/dashboards/
cp grafana/dashboards/*.json /path/to/grafana/provisioning/dashboards/

# Or use Docker Compose
docker-compose up -d prometheus grafana
```

### 2. Configure Your Application

Emit metrics from your application:

```rust
// Example: Emit queue depth metric
metrics.queue_depth.set(work_queue.len() as f64);

// Example: Emit latency metric
let start = Instant::now();
do_work();
metrics.latency.observe(start.elapsed().as_millis() as f64);
```

See [Implementation Guide](70-visual-management.md#implementation-guide) for details.

### 3. View Dashboards

Open Grafana: http://localhost:3000

**Default login:** admin / admin

**Available dashboards:**
- TPS System Overview (all 6 principles)
- Jidoka (failure detection)
- Kanban (queue management)
- Andon (problem visibility)
- Kaizen (SLO tracking)
- Heijunka (load leveling)
- Tracing (component latency)

## Dashboard Descriptions

### TPS Overview Dashboard
Shows all 6 TPS principles in one view with color-coded status.
- **Jidoka**: % circuit open
- **Kanban**: Queue depth
- **Andon**: Error rate %
- **Kaizen**: SLO attainment %
- **Heijunka**: Avg pool utilization
- **Tracing**: P99 latency
- System health trends over 24 hours

### Jidoka Deep-Dive (Failure Detection)
Tracks circuit breaker behavior and recovery:
- Circuit breaker state (open/closed/half-open)
- Failure rate by component
- Failure type breakdown
- Recovery time analysis
- Request rejection rate
- Half-open success rate

### Kanban Deep-Dive (Queue Management)
Tracks work queue and throughput:
- Queue depth by queue
- Throughput by queue
- Latency P50/P99 distribution
- Dead-letter queue depth
- Worker availability
- Queue utilization ratio

### Andon Deep-Dive (Problem Visibility)
Tracks error detection and response:
- Error rate by service
- Alert frequency by type
- Mean Time To Detect (MTTD)
- Top error types
- Error vs throughput correlation

### Kaizen Deep-Dive (Continuous Improvement)
Tracks SLO and performance trends:
- SLO attainment % (target >95%)
- Weekly trend analysis
- Latency outlier detection
- Improvement recommendations
- Week-over-week latency comparison

### Heijunka Deep-Dive (Load Leveling)
Tracks resource distribution:
- Pool utilization by pool
- Load balance coefficient
- Scaling events and cost
- Burst detection
- Cost analysis

### Tracing Deep-Dive (Value Stream)
Tracks component-level performance:
- Latency by component
- Latency percentiles (P50/P95/P99)
- Request volume by endpoint
- Error traces by component
- Top 5 slowest components

## File Structure

```
tps-reference-system/
├── grafana/
│   ├── dashboards/                 # Grafana dashboard JSON files
│   │   ├── tps-overview.json      # Overview dashboard (all 6 principles)
│   │   ├── jidoka.json            # Jidoka deep-dive
│   │   ├── kanban.json            # Kanban deep-dive
│   │   ├── andon.json             # Andon deep-dive
│   │   ├── kaizen.json            # Kaizen deep-dive
│   │   ├── heijunka.json          # Heijunka deep-dive
│   │   └── tracing.json           # Tracing deep-dive
│   └── provisioning/
│       └── dashboards.yaml        # Auto-provisioning config
├── prometheus/
│   ├── prometheus.yml             # Prometheus scrape config
│   └── alert-rules.yml            # Alert rules (25+ alerts)
├── docs/
│   ├── 70-visual-management.md    # Complete implementation guide
│   └── README.md                  # This file
└── [other files]
```

## Key Metrics

### Color Coding Standards

| Status | Color | Meaning | Action |
|--------|-------|---------|--------|
| Healthy | Green | ✓ All good | Monitor |
| Warning | Yellow | ⚠ Investigate | Check trend |
| Critical | Red | ✗ Action needed | Page on-call |

### TPS Principle Thresholds

| Principle | Metric | Green | Yellow | Red |
|-----------|--------|-------|--------|-----|
| Jidoka | Circuit Open % | 0% | 10-30% | >30% |
| Kanban | Queue Depth | <500 | 500-1000 | >1000 |
| Andon | Error Rate % | <0.5% | 0.5-1% | >1% |
| Kaizen | SLO Attainment | >95% | 90-95% | <90% |
| Heijunka | Avg Pool Util % | <70% | 70-85% | >85% |
| Tracing | P99 Latency | <3s | 3-5s | >5s |

## Alert Rules

### Critical Alerts (Page On-Call)
- Jidoka: Circuit open for >5 minutes
- Kanban: No available workers
- Andon: Error rate >1%
- Kaizen: SLO attainment <90%
- Tracing: P99 latency >5 seconds

### Warning Alerts (Investigate)
- Jidoka: Failure rate >1/sec or recovery >60s
- Kanban: Queue depth >1000
- Andon: Alert spam >10/min
- Kaizen: SLO attainment <95%
- Heijunka: Pool utilization >85%

## Integration Guide

### Step 1: Implement Metrics Library

Add prometheus metrics to your application:
```rust
use prometheus::{Counter, Gauge, Histogram, Registry};

pub struct TpsMetrics {
    pub queue_depth: Gauge,
    pub throughput: Counter,
    pub latency: Histogram,
    pub errors: Counter,
    // ... more metrics
}
```

### Step 2: Emit Metrics

Track metrics as operations occur:
```rust
// Track queue depth
metrics.queue_depth.set(queue.len() as f64);

// Track latency
let start = Instant::now();
let result = process();
metrics.latency.observe(start.elapsed().as_millis() as f64);

// Track errors
if let Err(e) = result {
    metrics.errors.inc();
}
```

### Step 3: Export Metrics Endpoint

Expose `/metrics` endpoint for Prometheus to scrape:
```rust
app.get("/metrics", |encoder: web::Data<TextEncoder>| {
    let metric_families = prometheus::gather();
    let mut buffer = vec![];
    encoder.encode(&metric_families, &mut buffer).unwrap();
    HttpResponse::Ok()
        .content_type("text/plain; charset=utf-8")
        .body(buffer)
})
```

### Step 4: Configure Prometheus

Configure Prometheus to scrape your application:
```yaml
scrape_configs:
  - job_name: 'tps-system'
    static_configs:
      - targets: ['localhost:9090']
    metrics_path: '/metrics'
    scrape_interval: 15s
```

### Step 5: Verify in Grafana

1. Open http://localhost:3000
2. Go to Dashboards → Manage
3. Verify TPS dashboards are loaded
4. Click "TPS System Overview"
5. Confirm metrics are populated

## Troubleshooting

### Metrics Not Appearing

**Check Prometheus target:**
- Navigate to http://localhost:9090/targets
- Should show your service as "UP"

**Check metrics endpoint:**
```bash
curl http://localhost:9090/metrics | grep tps_
```

**Check Grafana datasource:**
- Configuration → Data Sources
- Click Prometheus
- Click "Save & Test"

### Alerts Not Firing

**Verify alert rules are loaded:**
- Prometheus → Alerts
- Should show all alert rules

**Test alert threshold:**
- Prometheus → Graph
- Try: `tps_kanban_queue_depth > 1000`
- Should show metric value

**Alerts require duration:**
- Alerts have `for: 5m` - must be true for 5 minutes
- Wait 5 minutes before alert fires

### Dashboard Shows No Data

**Check time range:**
- Top-right corner: Select "24h"
- Time range should include "now"

**Check query:**
- Click metric in Prometheus UI first
- Should show data
- Copy query to Grafana

**Reload dashboard:**
- Press Ctrl+Shift+R in browser
- Reload page

## Documentation

For complete implementation guide, see:
- [Visual Management Implementation Guide](70-visual-management.md)
- [Metrics Reference](70-visual-management.md#metrics-reference)
- [Customization Guide](70-visual-management.md#customization)
- [Troubleshooting](70-visual-management.md#troubleshooting)

## Best Practices

1. **One-glance status**: All 6 principles visible without scrolling
2. **Color-coded decisions**: Green/Yellow/Red = action level
3. **Drill-down capability**: Click to see details
4. **Historical trends**: Show week-over-week improvements
5. **Actionable alerts**: Each alert includes what to do

## References

- [Toyota Production System Overview](../../docs/tps-reference/)
- [Prometheus Documentation](https://prometheus.io/docs/)
- [Grafana Documentation](https://grafana.com/docs/)
- [Visual Management Principles](70-visual-management.md)
