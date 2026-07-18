# Visual Management in Toyota Production System (TPS)

## Table of Contents
1. [Introduction](#introduction)
2. [Core Concepts](#core-concepts)
3. [Visual Management in Manufacturing](#visual-management-in-manufacturing)
4. [Visual Management in Software](#visual-management-in-software)
5. [Dashboard Architecture](#dashboard-architecture)
6. [Implementation Guide](#implementation-guide)
7. [Metrics Reference](#metrics-reference)
8. [Customization](#customization)
9. [Alert Configuration](#alert-configuration)
10. [Troubleshooting](#troubleshooting)

---

## Introduction

**Visual Management** is the practice of making system status visible at a glance. In TPS, problems must be immediately obvious so they can be addressed before they impact customers.

### The Problem It Solves

Without visual management:
- Problems are hidden in logs
- Issues are discovered hours or days later
- Engineers react to customer complaints
- Root causes are forgotten by the time investigation happens

With visual management:
- Problems are **immediately visible**
- Thresholds trigger **before** customers are impacted
- Teams can see **status at a glance**
- Continuous improvement data is always available

### Mental Model

Think of visual management like a dashboard on a car:

| Car Dashboard | TPS System Dashboard |
|---|---|
| Oil pressure gauge | Component health |
| Fuel level | Queue depth |
| Engine temperature | Error rate |
| RPM gauge | Throughput |
| Warning lights (red) | Andon alerts (critical) |
| Yellow lights | Andon alerts (warning) |

Just as you don't open your engine compartment to check oil, you shouldn't need to grep logs to check system health.

---

## Core Concepts

### 1. One-Glance Status

The dashboard must show system health in **one glance** without scrolling.

**What this means:**
- Top-level overview shows all 6 TPS principles
- Green = healthy, Yellow = investigate, Red = critical
- No more than 6-8 key metrics visible at once
- Color coding enables instant understanding

**Why it matters:**
- Decision-makers can see health without opening laptop
- Problems are caught immediately
- Operators know when to escalate
- Prevents information overload

### 2. Anomaly Highlighting

Changes must stand out visually.

**Implementation:**
- Thresholds are color-coded
- Red zones trigger alerts
- Yellow zones need investigation
- Green zones are normal

**Example:**
```
SLO Attainment: 94.2% (Yellow - warning)
  Target: >95%
  This week: 94.2%
  Last week: 96.1%
  Trend: Degrading
```

### 3. Drill-Down Paths

One-glance status links to deep dives.

**Path structure:**
```
Level 1: System Overview (all 6 principles)
    ↓
Level 2: Principle Deep-Dive (Kanban example)
    ↓
Level 3: Component Analysis (which queue is slow?)
    ↓
Level 4: Historical Trends (is this getting better or worse?)
```

### 4. Actionable Metrics

Every metric must answer a question.

**Good metrics:**
- Queue depth: "Are customers waiting?" → YES/NO
- Error rate: "Are there problems?" → YES/NO
- Latency P99: "Is service fast?" → YES/NO
- SLO attainment: "Are we meeting customer promise?" → YES/NO

**Bad metrics:**
- Average CPU usage (what does this tell us?)
- Total requests (are they fast or slow?)
- Memory utilization (is this bad?)

---

## Visual Management in Manufacturing

To understand visual management in software, let's start with how Toyota uses it on the factory floor.

### Physical Visual Management

**The Goal:** "Stop the Line" problems are visible instantly.

**Example: Toyota Assembly Plant**

Without visual management:
1. Worker 5 doesn't notice Worker 4's station has a problem
2. Worker 3 keeps sending work to Worker 4
3. Inventory piles up
4. 4 hours later, supervisor discovers the bottleneck
5. Rework is needed

With visual management:
1. Worker 4 pulls an **Andon cord**
2. **Red light** above the station turns on
3. **Alarm sounds**
4. Supervisor arrives in <30 seconds
5. Problem is fixed within 5 minutes
6. Production resumes

**Key principle:** Make problems **visible instantly**, not discoverable hours later.

### Visual Management Tools in Manufacturing

| Tool | Purpose | Software Equivalent |
|---|---|---|
| Andon lights | Problem visibility | Alert/notification system |
| Work-in-progress limits | Queue management | Max queue depth metric |
| Color-coded inventory | Status indication | Health gauge (green/yellow/red) |
| Kanban cards | Pull system | Queue depth + throughput |
| Standard work charts | Process clarity | Runbooks + documentation |
| Kaizen boards | Continuous improvement | SLO dashboard + trends |

---

## Visual Management in Software

### Translation to Software Systems

The same principles apply to software, using dashboards instead of factory floors.

### TPS Principles → Visual Indicators

#### 1. **Jidoka** (Autonomation)
- **What we see:** Circuit breaker state, failure rate, recovery time
- **Visual:** Red when open, yellow when half-open, green when closed
- **Action trigger:** Red = immediate investigation

#### 2. **Kanban** (Pull System)
- **What we see:** Queue depth, latency (P50/P99), throughput
- **Visual:** Green queue (processing), yellow (backing up), red (full)
- **Action trigger:** Yellow queue = add workers, red queue = page on-call

#### 3. **Andon** (Problem Visibility)
- **What we see:** Error rate, alert frequency, MTTD (mean time to detect)
- **Visual:** Spike in errors = red, escalate immediately
- **Action trigger:** Error rate >1% = critical incident

#### 4. **Kaizen** (Continuous Improvement)
- **What we see:** SLO attainment, trends, outliers
- **Visual:** Green >95%, yellow 90-95%, red <90%
- **Action trigger:** Yellow = investigate before it becomes critical

#### 5. **Heijunka** (Load Leveling)
- **What we see:** Pool utilization, balance coefficient, scaling costs
- **Visual:** Green <70%, yellow 70-85%, red >85%
- **Action trigger:** Red = add workers to reduce bottleneck

#### 6. **Tracing** (Value Stream)
- **What we see:** Latency by component, slowest paths, error locations
- **Visual:** Shows which component is bottleneck
- **Action trigger:** Slowest component >3s = optimize or scale

### Decision Flow

Visual management enables rapid decisions:

```
Dashboard shows issue (RED)
       ↓
Is it Jidoka (failure)?
  YES → Fix component → Resume operations
  NO  → Check Kanban (queue backed up)?
        YES → Add workers
        NO  → Check Andon (errors high)?
              YES → Fix error cause
              NO  → Check Kaizen (SLO miss)?
                    YES → Optimize bottleneck
                    NO  → Check Heijunka (load imbalanced)?
                          YES → Rebalance pools
                          NO  → Check Tracing (slow component)?
                                YES → Profile and optimize
```

---

## Dashboard Architecture

### Overview Dashboard (Level 1)

**Purpose:** See all 6 TPS principles in one view

**Layout:**
```
┌─────────────────────────────────────────────────┐
│ TPS System Health Overview                      │
├─────────────────────────────────────────────────┤
│                                                 │
│ Jidoka: [●] 0% open    Kanban: [●] 450 items  │
│ Andon:  [●] 0.3% err   Kaizen: [●] 96.2% SLO │
│ Heijunka: [●] 62% util Tracing: [●] 234ms P99│
│                                                 │
├─────────────────────────────────────────────────┤
│ System Trends (24 hours)                        │
│ [Line chart showing all 6 principles]          │
│                                                 │
│ [Link] Deep-Dive: Jidoka  Kanban  Andon       │
│ [Link] Deep-Dive: Kaizen  Heijunka Tracing   │
└─────────────────────────────────────────────────┘
```

**Panels:**
1. **System Health Grid** (6 gauges)
   - Jidoka: % circuit open (0% = green, >30% = red)
   - Kanban: Queue depth (0-500 = green, >1000 = red)
   - Andon: Error rate % (<0.5% = green, >1% = red)
   - Kaizen: SLO attainment % (>95% = green, <90% = red)
   - Heijunka: Avg pool utilization (<70% = green, >85% = red)
   - Tracing: P99 latency (<3s = green, >5s = red)

2. **System Trends Chart**
   - Line chart showing 24-hour trend
   - Each line is one principle
   - Shows if metrics are improving or degrading

3. **Navigation Links**
   - Click to deep-dive into individual principle
   - Links preserve current time range
   - Easy navigation back to overview

### Principle Deep-Dive Dashboards (Level 2)

#### Jidoka Dashboard
Shows failure detection and circuit breaker behavior.

**Key panels:**
- Circuit breaker state (open/closed/half-open)
- Failure rate by component
- Failure types (breakdown of error categories)
- Recovery time (how fast does system recover?)
- Rejected requests (impact on users)
- Half-open success rate (is recovery working?)

**Decision:** When to page on-call
- Circuit open >5 min → PAGE
- Failure rate >1/sec → INVESTIGATE
- Slow recovery >60s → FIX
- Rejection rate >50/sec → CRITICAL

#### Kanban Dashboard
Shows queue depth, throughput, and worker availability.

**Key panels:**
- Queue depth by queue (is work backing up?)
- Throughput by queue (how fast is work moving?)
- Latency P50/P99 (how long do items wait?)
- Dead-letter queue (failed items)
- Worker availability (are workers ready to pull?)
- Queue utilization ratio (how full is queue?)

**Decision:** When to scale workers
- Queue depth >1000 → ADD WORKERS
- Workers available <1 → CRITICAL
- P99 latency >5s → INVESTIGATE SLOWNESS
- DLQ growing → INVESTIGATE FAILURES

#### Andon Dashboard
Shows error detection and escalation.

**Key panels:**
- Error rate by service (where are problems?)
- Alert frequency (how often do we alert?)
- MTTD - Mean Time To Detect (how fast do we see problems?)
- Top error types (what's breaking?)
- Error vs Throughput correlation (do errors slow us down?)

**Decision:** When to escalate
- Error rate >1% → CRITICAL
- Alert spam >10/min → INVESTIGATE ALERTS
- MTTD >60s → IMPROVE MONITORING
- New error type → INVESTIGATE ROOT CAUSE

#### Kaizen Dashboard
Shows SLO tracking and continuous improvement.

**Key panels:**
- SLO attainment % (are we meeting promise?)
- Weekly trend (is it improving?)
- Latency outliers (where are slow requests?)
- Recommended improvements (what should we optimize?)
- Latency trend vs last week (regression detection?)

**Decision:** When to optimize
- SLO <95% → OPTIMIZE
- SLO degrading → INVESTIGATE REGRESSION
- High outlier rate → PROFILE BOTTLENECK
- Recommendations →PRIORITIZE IMPROVEMENTS

#### Heijunka Dashboard
Shows load distribution and scaling.

**Key panels:**
- Pool utilization by pool (which pools are busy?)
- Load balance coefficient (is load fair?)
- Scaling events (when did we scale?)
- Burst detection (are there spikes?)
- Cost analysis (how much did scaling cost?)

**Decision:** When to rebalance load
- Any pool >85% → ADD WORKERS
- Balance coefficient <0.7 → REBALANCE
- Frequent bursts → IMPLEMENT RATE LIMITING
- High scaling cost → USE RESERVED CAPACITY

#### Tracing Dashboard
Shows value stream performance and bottlenecks.

**Key panels:**
- Latency by component (which is slowest?)
- Latency distribution P50/P95/P99 (what's tail latency?)
- Request volume by endpoint (how much load?)
- Error traces (where do failures occur?)
- Top 5 slowest components (bottleneck analysis)

**Decision:** When to optimize
- P99 >5s → OPTIMIZE OR SCALE
- Latency regression → INVESTIGATE PERF
- High error rate in component → FIX BUGS
- Bottleneck component >3s → PROFILE OR CACHE

---

## Implementation Guide

### Step 1: Set Up Prometheus

#### 1.1 Install Prometheus
```bash
# Docker
docker run -d \
  --name prometheus \
  -p 9090:9090 \
  -v $(pwd)/prometheus.yml:/etc/prometheus/prometheus.yml \
  -v $(pwd)/alert-rules.yml:/etc/prometheus/alert-rules.yml \
  prom/prometheus

# Or using Docker Compose
docker-compose up -d prometheus
```

#### 1.2 Configure Prometheus
Place `prometheus.yml` in the Prometheus config directory:
```yaml
global:
  scrape_interval: 15s
  evaluation_interval: 15s

scrape_configs:
  - job_name: 'tps-system'
    static_configs:
      - targets: ['localhost:9090']
```

#### 1.3 Configure Alert Rules
Place `alert-rules.yml` in the Prometheus config directory:
```bash
# Reference in prometheus.yml
rule_files:
  - 'alert-rules.yml'
```

### Step 2: Set Up Grafana

#### 2.1 Install Grafana
```bash
# Docker
docker run -d \
  --name grafana \
  -p 3000:3000 \
  grafana/grafana

# Or using Docker Compose
docker-compose up -d grafana
```

#### 2.2 Configure Prometheus as Datasource
1. Open Grafana: http://localhost:3000
2. Login (default: admin/admin)
3. Go to Configuration → Data Sources
4. Add Prometheus: http://prometheus:9090
5. Save & Test

#### 2.3 Auto-Load Dashboards
Create `provisioning/dashboards.yaml`:
```yaml
apiVersion: 1
providers:
  - name: 'TPS Dashboards'
    orgId: 1
    type: file
    options:
      path: /etc/grafana/provisioning/dashboards
```

Mount dashboards in Docker:
```bash
docker run -d \
  -v $(pwd)/provisioning:/etc/grafana/provisioning \
  -v $(pwd)/dashboards:/etc/grafana/provisioning/dashboards \
  grafana/grafana
```

### Step 3: Instrument Your Application

Your application must emit metrics for TPS monitoring.

#### 3.1 Create Metrics Library

Example in Rust:
```rust
use prometheus::{register_gauge, register_counter_vec, Gauge, Counter, Registry};

pub struct TpsMetrics {
    // Jidoka
    pub circuit_open: Gauge,
    pub failures: Counter,

    // Kanban
    pub queue_depth: Gauge,
    pub throughput: Counter,

    // Andon
    pub errors: Counter,
    pub alerts: Counter,

    // Kaizen
    pub slo_attainment: Gauge,

    // Heijunka
    pub pool_utilization: Gauge,
    pub scaling_events: Counter,

    // Tracing
    pub latency: Histogram,
}

impl TpsMetrics {
    pub fn new(registry: &Registry) -> Result<Self, Box<dyn std::error::Error>> {
        Ok(TpsMetrics {
            circuit_open: register_gauge!("tps_jidoka_circuit_state", registry)?,
            failures: register_counter_vec!("tps_jidoka_failures_total", &[], registry)?,
            queue_depth: register_gauge!("tps_kanban_queue_depth", registry)?,
            throughput: register_counter_vec!("tps_kanban_throughput_total", &[], registry)?,
            errors: register_counter_vec!("tps_andon_errors_total", &[], registry)?,
            alerts: register_counter_vec!("tps_andon_alerts_total", &[], registry)?,
            slo_attainment: register_gauge!("tps_kaizen_slo_attainment", registry)?,
            pool_utilization: register_gauge!("tps_heijunka_pool_utilization", registry)?,
            scaling_events: register_counter_vec!("tps_heijunka_scaling_events_total", &[], registry)?,
            latency: register_histogram!("tps_tracing_duration_ms", registry)?,
        })
    }
}
```

#### 3.2 Update Metrics in Your Code

Track metrics as you execute:
```rust
// Jidoka - track failures
match process_request() {
    Ok(result) => {
        metrics.throughput.inc();
        Ok(result)
    }
    Err(e) => {
        metrics.failures.inc();
        metrics.circuit_open.set(1.0);  // Open circuit
        Err(e)
    }
}

// Kanban - track queue
let queue_depth = work_queue.len();
metrics.queue_depth.set(queue_depth as f64);

// Andon - track errors
match call_service() {
    Ok(resp) => Ok(resp),
    Err(e) => {
        metrics.errors.inc();
        metrics.alerts.inc();
        Err(e)
    }
}

// Tracing - track latency
let start = Instant::now();
let result = process();
let latency_ms = start.elapsed().as_millis() as f64;
metrics.latency.observe(latency_ms);
```

#### 3.3 Export Metrics

Expose `/metrics` endpoint:
```rust
// Using prometheus crate
use prometheus::TextEncoder;

app.get("/metrics", |encoder: web::Data<TextEncoder>| {
    let metric_families = prometheus::gather();
    let mut buffer = vec![];
    encoder.encode(&metric_families, &mut buffer).unwrap();
    web::HttpResponse::Ok()
        .content_type("text/plain; charset=utf-8")
        .body(buffer)
})
```

### Step 4: Test the Dashboard

1. Navigate to Grafana: http://localhost:3000
2. Click "Dashboards" → "Manage"
3. Verify all 7 dashboards are loaded:
   - TPS System Overview
   - Jidoka (Autonomation)
   - Kanban (Pull System)
   - Andon (Problem Visibility)
   - Kaizen (Continuous Improvement)
   - Heijunka (Load Leveling)
   - Tracing (Value Stream)

4. Click on "TPS System Overview"
5. Verify time range is set to "24h"
6. Check that metrics are populated

### Step 5: Verify Alert Rules

1. Go to Prometheus: http://localhost:9090
2. Click "Alerts"
3. Verify all alert rules are loaded
4. Verify alert status (green = no alerts, red = alerts firing)

---

## Metrics Reference

### Jidoka Metrics

| Metric | Type | Description | Threshold |
|---|---|---|---|
| `tps_jidoka_circuit_state` | Gauge | 0=closed, 1=open, 2=half-open | Alert if > 0 for 5 min |
| `tps_jidoka_failures_total` | Counter | Total number of failures | Alert if rate > 1/sec |
| `tps_jidoka_failure_types_total` | Counter | Count by failure type | For categorization |
| `tps_jidoka_recovery_time_seconds` | Gauge | Time to recover from failure | Warn if > 60s |
| `tps_jidoka_requests_rejected_total` | Counter | Requests rejected (circuit open) | Alert if rate > 50/sec |
| `tps_jidoka_halfopen_success_rate` | Gauge | Success rate during recovery | Must be >90% |

### Kanban Metrics

| Metric | Type | Description | Threshold |
|---|---|---|---|
| `tps_kanban_queue_depth` | Gauge | Number of items waiting | Alert if > 1000 |
| `tps_kanban_latency_ms_bucket` | Histogram | Item wait time distribution | Alert if P99 > 5000ms |
| `tps_kanban_throughput_total` | Counter | Items processed per minute | Track for trends |
| `tps_kanban_dlq_depth` | Gauge | Failed items in dead-letter queue | Alert if > 50 |
| `tps_kanban_workers_available` | Gauge | Ready workers available | Alert if < 1 |
| `tps_kanban_queue_utilization` | Gauge | Queue full ratio (0-1) | Alert if > 0.9 |

### Andon Metrics

| Metric | Type | Description | Threshold |
|---|---|---|---|
| `tps_andon_errors_total` | Counter | Total errors by type | Alert if rate > 1% |
| `tps_andon_alerts_total` | Counter | Alert count by type | Warn if rate > 10/min |
| `tps_andon_mttd_seconds` | Gauge | Mean time to detect problems | Warn if > 60s |
| `tps_andon_error_rate` | Gauge | Calculated error % | Alert if > 1% |

### Kaizen Metrics

| Metric | Type | Description | Threshold |
|---|---|---|---|
| `tps_kaizen_slo_attainment` | Gauge | Fraction meeting SLO (0-1) | Alert if < 0.95 |
| `tps_kaizen_slo_attainment_weekly` | Gauge | Weekly SLO % by week | Track trends |
| `tps_kaizen_outliers_detected_total` | Counter | Latency outliers detected | Warn if rate > 10/5min |
| `tps_kaizen_recommendations_total` | Counter | Recommendations by type | For improvement planning |

### Heijunka Metrics

| Metric | Type | Description | Threshold |
|---|---|---|---|
| `tps_heijunka_pool_utilization` | Gauge | Pool capacity used (0-1) | Warn if > 0.7, alert > 0.85 |
| `tps_heijunka_balance_coefficient` | Gauge | Load fairness (0-1) | Warn if < 0.7 |
| `tps_heijunka_scaling_events_total` | Counter | Scaling up/down events | Track trends |
| `tps_heijunka_burst_detected_total` | Counter | Load spike events | Warn if > 5/5min |
| `tps_heijunka_scaling_cost_total` | Counter | Cost of scaling operations | Budget tracking |

### Tracing Metrics

| Metric | Type | Description | Threshold |
|---|---|---|---|
| `tps_tracing_duration_ms_bucket` | Histogram | Request latency distribution | Alert if P99 > 5000ms |
| `tps_tracing_requests_total` | Counter | Request count by endpoint | Volume tracking |
| `tps_tracing_component_latency_ms` | Gauge | Avg latency by component | Alert if > 3000ms |
| `tps_tracing_error_traces_total` | Counter | Errors by component | Identify failure points |

---

## Customization

### Adding Custom Metrics

1. Add metric definition to your metrics library
2. Emit the metric in your code
3. Add panel to dashboard to visualize

Example:
```rust
// Add metric
pub custom_metric: Gauge = register_gauge!("my_custom_metric", registry)?;

// Emit in code
metrics.custom_metric.set(value);

// Add Grafana panel
{
  "targets": [
    {
      "expr": "my_custom_metric",
      "legendFormat": "Custom Metric"
    }
  ],
  "title": "My Custom Metric"
}
```

### Adjusting Alert Thresholds

Edit `alert-rules.yml` and change thresholds:

```yaml
# Example: Change queue alert threshold
- alert: KanbanQueueDepthCritical
  expr: 'tps_kanban_queue_depth > 2000'  # Changed from 1000
  for: 5m
```

Reload Prometheus to apply changes:
```bash
# HTTP API
curl -X POST http://prometheus:9090/-/reload

# Or restart Prometheus
docker restart prometheus
```

### Creating Custom Alerts

Add new alert rules:
```yaml
- alert: MyCustomAlert
  expr: 'my_metric > threshold'
  for: 5m
  labels:
    severity: critical
  annotations:
    summary: "Alert summary"
    description: "Alert description"
    action: "What to do"
```

### Changing Time Range

Edit dashboard JSON:
```json
"time": {
  "from": "now-7d",    // Changed from "now-24h"
  "to": "now"
}
```

Or use Grafana UI to change time range (top-right corner).

---

## Alert Configuration

### Setting Up Alertmanager

Alertmanager routes alerts to notification channels (email, Slack, PagerDuty).

#### 1. Install Alertmanager
```bash
docker run -d \
  --name alertmanager \
  -p 9093:9093 \
  -v $(pwd)/alertmanager.yml:/etc/alertmanager/alertmanager.yml \
  prom/alertmanager
```

#### 2. Configure Routes
Create `alertmanager.yml`:
```yaml
global:
  resolve_timeout: 5m

route:
  receiver: 'default'
  group_by: ['alertname', 'cluster']
  group_wait: 10s
  group_interval: 10s
  repeat_interval: 12h

  # Route critical TPS alerts to PagerDuty
  routes:
    - match:
        severity: critical
      receiver: 'pagerduty'
      continue: true

    - match:
        severity: warning
      receiver: 'slack'

receivers:
  - name: 'default'

  - name: 'pagerduty'
    pagerduty_configs:
      - service_key: 'YOUR_PAGERDUTY_KEY'

  - name: 'slack'
    slack_configs:
      - api_url: 'YOUR_SLACK_WEBHOOK'
        channel: '#alerts'
```

#### 3. Configure Prometheus to Use Alertmanager
```yaml
# In prometheus.yml
alerting:
  alertmanagers:
    - static_configs:
        - targets: ['alertmanager:9093']
```

#### 4. Verify Alerts Are Firing
- Prometheus: http://localhost:9090/alerts
- Alertmanager: http://localhost:9093

---

## Troubleshooting

### Problem: Metrics are not showing in Grafana

**Check:**
1. Is Prometheus scraping the metrics endpoint?
   - Prometheus UI → Status → Targets
   - Should show "UP" for your service

2. Are the metrics being emitted?
   ```bash
   curl http://localhost:9090/metrics | grep tps_
   ```

3. Is the Grafana query correct?
   - Test in Prometheus UI first
   - Then use exact query in Grafana

**Fix:**
```bash
# Verify metrics endpoint is accessible
curl http://localhost:9090/metrics

# Restart Prometheus
docker restart prometheus

# Reload Grafana
docker restart grafana
```

### Problem: Alerts not firing

**Check:**
1. Are metrics available?
   ```bash
   # In Prometheus UI
   # Test alert condition: tps_kanban_queue_depth > 1000
   ```

2. Is alert rule syntax correct?
   - Prometheus UI → Alerts
   - Look for `INACTIVE` status (no matches) vs `PENDING/FIRING`

3. Has condition been true for duration?
   - Alert requires condition to be true for `for: 5m`
   - Wait 5 minutes before alert fires

**Fix:**
```bash
# Manually test alert condition in Prometheus UI
# Then reload alert rules
curl -X POST http://prometheus:9090/-/reload
```

### Problem: High cardinality metrics (too many time series)

**Symptom:** Prometheus is slow, high memory usage

**Cause:** Labels have unbounded values (e.g., user_id, request_id)

**Fix:**
```yaml
# In prometheus.yml, drop high-cardinality labels
relabel_configs:
  - source_labels: [request_id]
    action: drop
```

Or use bounded labels:
```rust
// Good: component has few values
metrics.latency.with_label_values(&["component"]).observe(value);

// Bad: user_id has millions of values
metrics.latency.with_label_values(&["user_id"]).observe(value);
```

### Problem: Dashboard not updating

**Check:**
1. Is refresh interval set?
   - Top-right of dashboard: "30s" should show
   - Click to change refresh rate

2. Are queries stale?
   - Click refresh button (top-right)
   - Or press Ctrl+Shift+R

3. Is time range correct?
   - Time range selector (top-right)
   - Should include "now"

**Fix:**
```bash
# Reload dashboard
# Press Ctrl+Shift+R in browser

# Check Grafana logs
docker logs grafana
```

### Problem: Out of memory (OOM)

**Cause:**
1. Prometheus has too many metrics
2. Grafana dashboard has too many queries

**Fix:**
```bash
# Increase memory limits in Docker
docker run -d \
  -m 2g \  # Set to 2GB
  prometheus/prometheus

# Or in docker-compose.yml
services:
  prometheus:
    mem_limit: 2g
```

Or reduce metrics:
```yaml
# In prometheus.yml, drop unwanted metrics
metric_relabeling:
  - source_labels: [__name__]
    regex: 'unwanted_metric'
    action: drop
```

---

## Best Practices

### 1. One Dashboard for All Decision-Makers

**Principle:** Everyone from CEO to engineer understands status in 5 seconds.

**Implementation:**
- Color-coded health (green/yellow/red)
- No scrolling needed for overview
- Deep-dives available for details

### 2. Verify Metrics Before Adding Alerts

**Anti-pattern:** Create alert rule without testing metric

**Process:**
1. Emit metric in code
2. Verify metric appears in Prometheus
3. Test threshold in Prometheus UI
4. Create alert rule
5. Test alert in Prometheus
6. Deploy

### 3. Set Realistic Thresholds

**Anti-pattern:** Set thresholds too low → alert spam

**Process:**
1. Observe baseline for 1 week
2. Calculate 95th percentile
3. Set threshold 10-20% above baseline
4. Adjust after 1 week of data

### 4. Include Context in Alert Messages

**Bad alert:**
```
KanbanQueueDepth alert fired
```

**Good alert:**
```
Kanban queue depth >1000
Current: 1,250 items
Queue: api-requests
Action: Check worker availability, may need to add workers
```

### 5. Use Time Series to Spot Trends

**Anti-pattern:** Look at single data point

**Pattern:**
1. Look at current value (red/yellow/green)
2. Look at 1-hour trend (increasing/decreasing)
3. Look at 24-hour trend (pattern)
4. Make decision based on trend + threshold

---

## Conclusion

Visual management transforms operations from reactive (fixing problems when discovered) to proactive (addressing problems as they emerge).

By following this implementation guide, you've created:
1. **One-glance status** - All 6 TPS principles visible in <5 seconds
2. **Automated alerting** - Problems trigger alerts before customers are impacted
3. **Deep-dive capability** - Navigate from overview to root cause
4. **Continuous improvement** - Historical data shows what's improving

**Result:** Your team becomes a responsive, data-driven organization that continuously improves instead of constantly firefighting.
