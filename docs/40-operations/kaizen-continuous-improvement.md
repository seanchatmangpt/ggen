<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Kaizen - Continuous Improvement Framework & Metrics](#kaizen---continuous-improvement-framework--metrics)
  - [Table of Contents](#table-of-contents)
  - [Kaizen Philosophy](#kaizen-philosophy)
    - [Historical Context (Manufacturing)](#historical-context-manufacturing)
    - [Kaizen in Software](#kaizen-in-software)
  - [Core Principles](#core-principles)
    - [1. **Measure Everything - Data First**](#1-measure-everything---data-first)
    - [2. **Know What "Good" Means - SLOs are Explicit**](#2-know-what-good-means---slos-are-explicit)
    - [3. **Trends Matter More Than Absolutes**](#3-trends-matter-more-than-absolutes)
    - [4. **Root Cause Analysis - Don't Fix Symptoms**](#4-root-cause-analysis---dont-fix-symptoms)
    - [5. **Data Retention - History Enables Insights**](#5-data-retention---history-enables-insights)
  - [Metrics Architecture](#metrics-architecture)
    - [Four Pillars of TPS Metrics](#four-pillars-of-tps-metrics)
    - [Detailed Metric Definitions](#detailed-metric-definitions)
      - [**Jidoka Metrics** (Autonomic Quality)](#jidoka-metrics-autonomic-quality)
      - [**Kanban Metrics** (Flow Control)](#kanban-metrics-flow-control)
      - [**Andon Metrics** (Visibility)](#andon-metrics-visibility)
      - [**Heijunka Metrics** (Level Scheduling)](#heijunka-metrics-level-scheduling)
    - [Overall System Health Score](#overall-system-health-score)
  - [Implementation Guide](#implementation-guide)
    - [1. Initialization](#1-initialization)
    - [2. Register SLOs](#2-register-slos)
    - [3. Record Events](#3-record-events)
    - [4. Analysis Loop](#4-analysis-loop)
  - [Improvement Workflows](#improvement-workflows)
    - [Workflow 1: Weekly Kaizen Review](#workflow-1-weekly-kaizen-review)
    - [Workflow 2: When Metric Degrades](#workflow-2-when-metric-degrades)
    - [Workflow 3: Kaizen Experimentation](#workflow-3-kaizen-experimentation)
  - [Dashboard Setup](#dashboard-setup)
    - [Grafana Dashboard Import](#grafana-dashboard-import)
    - [Dashboard Panels Explained](#dashboard-panels-explained)
    - [Creating Custom Dashboards](#creating-custom-dashboards)
  - [Root Cause Analysis](#root-cause-analysis)
    - [The 5 Whys Technique](#the-5-whys-technique)
    - [SPARQL Evidence Queries](#sparql-evidence-queries)
  - [Examples](#examples)
    - [Example 1: Monitoring Payment Service](#example-1-monitoring-payment-service)
    - [Example 2: Weekly Review Report](#example-2-weekly-review-report)
  - [Best Practices](#best-practices)
    - [1. **Metrics Hygiene**](#1-metrics-hygiene)
    - [2. **Alert Tuning**](#2-alert-tuning)
    - [3. **Continuous Improvement**](#3-continuous-improvement)
    - [4. **Documentation**](#4-documentation)
  - [Troubleshooting](#troubleshooting)
    - [Problem: Alert Fatigue](#problem-alert-fatigue)
    - [Problem: Missing Data Points](#problem-missing-data-points)
    - [Problem: Metric Spikes / Outliers](#problem-metric-spikes--outliers)
    - [Problem: SLO Frequently Missed](#problem-slo-frequently-missed)
  - [Appendix: Complete Metric Reference](#appendix-complete-metric-reference)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Kaizen - Continuous Improvement Framework & Metrics

**Kaizen** (改善) = "Change for better" or "Good change, continuous improvement"

This document provides the comprehensive Kaizen implementation guide for the ggen project, covering metrics collection, analysis, dashboarding, and continuous improvement workflows.

## Table of Contents

1. [Kaizen Philosophy](#kaizen-philosophy)
2. [Core Principles](#core-principles)
3. [Metrics Architecture](#metrics-architecture)
4. [Implementation Guide](#implementation-guide)
5. [Improvement Workflows](#improvement-workflows)
6. [Dashboard Setup](#dashboard-setup)
7. [Root Cause Analysis](#root-cause-analysis)
8. [Examples](#examples)
9. [Best Practices](#best-practices)
10. [Troubleshooting](#troubleshooting)

---

## Kaizen Philosophy

### Historical Context (Manufacturing)

In Toyota's production system, Kaizen is foundational:

> "Continuous improvement means that every employee has the responsibility and authority to find
> better ways to do their job, and report them to their supervisors." - Toyota Production System

**Example**: A line worker notices that moving Component A closer to Station 3 saves 3 seconds per cycle.
Over one year, this saves: 3 sec × 250 shifts × 8 hours = 20,000 minutes (333 hours!) of labor.

Small improvements compound:
- **Week 1**: 5 improvements (1% each) = 5% total gain
- **Month 1**: 20 improvements (1% each) = 22% total gain
- **Year 1**: 250+ improvements (1% each) = 1,200%+ gain (theoretical exponential)

### Kaizen in Software

In code, the same principle applies:

> "Every developer should measure their system, identify one slow path,
> and optimize it this week." - Kaizen applied to software

**Example**: Latency optimization journey:
- Week 1: Reduce API response time from 250ms → 230ms (8% improvement)
- Week 2: Batch database queries: 230ms → 180ms (22% improvement)
- Week 3: Add caching layer: 180ms → 95ms (47% improvement)
- Week 4: Optimize hot path with SIMD: 95ms → 42ms (56% improvement)
- **Total after 1 month: 83% latency reduction** (250ms → 42ms)

---

## Core Principles

### 1. **Measure Everything - Data First**

> "In God we trust; all others must bring data." - W. Edwards Deming

**Principle**: Metrics are FACTS, not opinions.

**Implementation**:
- Record EVERY event that matters (Jidoka failures, Kanban capacity changes, Andon alerts)
- Use Prometheus for time-series storage (industry standard)
- Persist data long-term (1+ year) to detect seasonal patterns
- Enable root cause analysis: "What else changed when this metric degraded?"

**Anti-patterns**:
- ❌ "I think the system is slow" (without metrics)
- ❌ "It worked yesterday" (without baseline)
- ❌ "We should optimize X" (without measuring X)

**Best practice**:
- ✅ "Latency P99 degraded 15% in the last 6 hours. Queue depth also increased 40%. Likely cause: worker pool saturation."

### 2. **Know What "Good" Means - SLOs are Explicit**

> "If you're not measuring it, you can't manage it."

**Principle**: Define success explicitly with SLOs (Service Level Objectives).

**Example SLOs**:
```
Service: checkout-service
├── Latency P99: 200ms (max, 5-minute window)
├── Success rate: 99.95% (min)
├── Availability: 99.99% (uptime)
└── Queue depth: ≤50 tasks (peak)
```

**Implementation**:
- Register SLOs with metric system
- Track attainment continuously (100% = met, <99% = degraded)
- Tie SLOs to business outcomes (customer satisfaction, revenue impact)
- Review weekly in operations meeting

**What happens without SLOs**:
- ❌ "Is latency good or bad?" (No baseline to compare)
- ❌ "Did we improve?" (Improvement relative to what?)
- ❌ "Which metric is most critical?" (Everything feels important)

**With SLOs**:
- ✅ "P99 latency is at 185ms, target is 200ms" (10% headroom remaining)
- ✅ "Success rate dropped 0.3%, that's 3 nines of failure budget left"
- ✅ "Priority order: latency SLO (critical) > queue depth (warning) > alert count (info)"

### 3. **Trends Matter More Than Absolutes**

> "Direction matters more than position." - Kaizen thinking

**Principle**: Is the system improving or degrading over time?

**Example**:
```
Scenario A: Latency = 150ms
- Without trend context: "Is this good?"
- With trend data:
  - 30 days ago: 150ms
  - 7 days ago: 145ms
  - Yesterday: 142ms
  - Trend: -0.27% per day (IMPROVING ✅)

Scenario B: Latency = 150ms
- With trend data:
  - 30 days ago: 80ms
  - 7 days ago: 110ms
  - Yesterday: 145ms
  - Trend: +2.3% per day (DEGRADING ❌)
```

**Implementation**:
- Analyze trend over 1h, 24h, 7d, 30d windows
- Calculate slope: (current - mean) / mean
- Alert on trend reversal (getting worse after improving)
- Celebrate trend improvements (getting better!)

**Trend analysis output**:
```
Metric: kanban_latency_p99_ms
├── Current: 185ms
├── 1h mean: 180ms (99% health)
├── 24h mean: 170ms (trend improving)
├── 7d mean: 150ms (strong improvement)
├── 30d mean: 120ms (massive improvement)
└── Slope: -2.2ms per day (EXCELLENT TREND)
```

### 4. **Root Cause Analysis - Don't Fix Symptoms**

> "Fix the problem, not the alarm." - TPS principle

**Principle**: When metrics degrade, correlate with events to find ROOT CAUSE.

**Anti-pattern (fixing symptoms)**:
```
Metric: Latency increasing
Action: Add more workers → helps temporarily
Result: Next week, latency increases again
Root cause: NEVER FOUND → problem compounds
```

**Kaizen pattern (finding root cause)**:
```
Observation: Latency increased 40% in last 6h

Correlations:
  - Queue depth increased? YES (+60%)
  - Worker count changed? NO
  - Database response time increased? YES (+80%) ← SMOKING GUN
  - External API latency? YES (+120%) ← ROOT CAUSE

Root cause: Upstream service (payment gateway) is slow
Options:
  1. Wait for payment gateway to recover (short-term)
  2. Implement fallback caching (medium-term)
  3. Move to faster payment provider (long-term)

Action taken: Enable fallback cache (1-hour TTL)
Result: Latency returns to normal within 30 minutes
Permanent fix: Migrate to FastPay API next quarter
```

**Implementation**:
- SPARQL queries over evidence receipts (RDF)
- Correlate events with metric changes
- Create runbooks for known patterns
- Post-mortems: "What else changed when this happened?"

### 5. **Data Retention - History Enables Insights**

> "Yesterday's data is tomorrow's wisdom."

**Principle**: Keep detailed metrics for 1+ year to detect patterns.

**Benefits**:
- Seasonal patterns (e.g., higher traffic on Mondays)
- Long-term trends (is system getting faster or slower?)
- Regression detection (metrics similar to June after recent deployment)
- Capacity planning (growth rate extrapolation)

**Example**:
```
Metric: daily_active_users
├── 1 year ago: 10k
├── 6 months ago: 15k
├── 3 months ago: 22k
├── Current: 35k
└── Growth rate: +15% per month
└── Projection (1 year): ~355k users
└── Action: Plan infrastructure for 500k capacity

Without 1-year history: "We have 35k users now"
(Can't see growth trajectory, can't plan capacity)
```

**Implementation**:
- 1-minute granularity for hot metrics (latency, throughput)
- 1-hour aggregation for historical storage (1+ year retention)
- Use Prometheus (built-in retention) or InfluxDB/CloudSQL
- Archive to cold storage (S3, GCS) for long-term trend analysis

---

## Metrics Architecture

### Four Pillars of TPS Metrics

```
┌─────────────────────────────────────────────────────────┐
│         KAIZEN METRICS ARCHITECTURE (Four Pillars)      │
├─────────────────────────────────────────────────────────┤
│                                                           │
│  JIDOKA               KANBAN              ANDON           │
│  (Autonomic)         (Flow)              (Visibility)     │
│  ├─ Circuit open %    ├─ Queue depth      ├─ Alert freq  │
│  ├─ Failure rate      ├─ Latency P99      ├─ MTTD        │
│  ├─ Recovery time     ├─ Throughput       ├─ Log volume  │
│  └─ Total opens       ├─ Wait time        └─ Crit alerts │
│                       └─ Saturation %                     │
│                                                           │
│                    ↓ (All feed into) ↓                   │
│                                                           │
│              HEIJUNKA (Level Scheduling)                 │
│              ├─ Load balance (0-1.0)                     │
│              ├─ Worker utilization                       │
│              ├─ Queue variance                           │
│              ├─ Workers active                           │
│              └─ Workers idle                             │
│                                                           │
└─────────────────────────────────────────────────────────┘
```

### Detailed Metric Definitions

#### **Jidoka Metrics** (Autonomic Quality)

| Metric | Definition | Formula | Target | Alert >|
|--------|-----------|---------|--------|--------|
| `jidoka_circuit_open_percent` | % of circuits in open state | (open_count / total_circuits) × 100 | <5% | 10% |
| `jidoka_failure_rate` | Failures detected per minute | failures / 60 sec | <1/min | 5/min |
| `jidoka_recovery_time_secs` | Mean time circuit auto-recovers | (sum recovery times) / open_count | <60s | 180s |
| `jidoka_circuit_opens_total` | Cumulative circuit open events | counter (never reset) | N/A | (trend) |
| `jidoka_failures_total` | Cumulative failure count | counter (never reset) | N/A | (trend) |

**Jidoka Health Score**:
```
jidoka_health = 100 - (circuit_open_percent × 2 + failure_rate × 0.5)
  = 100 - (5% × 2 + 2/min × 0.5)
  = 100 - 10 - 1 = 89% (healthy)
```

#### **Kanban Metrics** (Flow Control)

| Metric | Definition | Formula | Target | Alert >|
|--------|-----------|---------|--------|--------|
| `kanban_queue_depth` | Tasks currently waiting | queued_task_count | <50 | 100 |
| `kanban_latency_p99_ms` | 99th percentile latency | sorted_latencies[0.99] | <100ms | 500ms |
| `kanban_throughput_per_min` | Tasks completed per minute | completed_tasks / 60 sec | >10/min | (trend) |
| `kanban_wait_time_secs` | Mean task wait time | (sum wait times) / task_count | <5s | 30s |
| `kanban_queue_saturation` | Queue utilization | (queue_depth / max_queue) × 100 | <80% | 95% |
| `kanban_latency_histogram` | Full latency distribution | histogram buckets | P50<50ms, P95<100ms, P99<200ms | N/A |

**Kanban Health Score**:
```
kanban_health = (100 - latency_p99/5) × (throughput/target_throughput)
```

#### **Andon Metrics** (Visibility)

| Metric | Definition | Formula | Target | Alert >|
|--------|-----------|---------|--------|--------|
| `andon_log_volume_total` | Total log entries recorded | counter (never reset) | N/A | (trend) |
| `andon_alerts_per_min` | Alert frequency | alerts / 60 sec | <2/min | 5/min |
| `andon_mttd_secs` | Mean Time To Detection | (detection_time - problem_start) | <60s | 300s |
| `andon_alert_types_total` | Unique alert types triggered | distinct alert types | N/A | >20 types |
| `andon_critical_alerts` | Currently active critical alerts | count (real-time) | 0 | 1+ |

**Andon Health Score**:
```
andon_health = 100 - (alert_frequency × 10 + mttd / 30)
  = 100 - (1.5 × 10 + 45 / 30)
  = 100 - 15 - 1.5 = 83.5% (good)
```

#### **Heijunka Metrics** (Level Scheduling)

| Metric | Definition | Formula | Target | Alert <|
|--------|-----------|---------|--------|--------|
| `heijunka_load_balance_coeff` | Load balance quality | 1.0 - (stddev(worker_loads) / mean(worker_loads)) | >0.85 | 0.7 |
| `heijunka_worker_utilization` | % workers busy | (active_workers / total_workers) × 100 | 70-85% | 50% |
| `heijunka_queue_variance` | Queue depth variance | stddev(queue_depth_samples) | <20 | 50 |
| `heijunka_workers_active` | Currently active workers | count (real-time) | varies | (trend) |
| `heijunka_workers_idle` | Currently idle workers | count (real-time) | varies | (trend) |

**Heijunka Health Score**:
```
heijunka_health = load_balance_coeff × 100 × worker_utilization / 80
```

### Overall System Health Score

```
health_score = (jidoka_health + kanban_health + andon_health + heijunka_health) / 4

Health ranges:
├── 80-100%: GREEN (System operating excellently)
├── 60-80%:  YELLOW (Degraded, investigate)
├── 40-60%:  ORANGE (Multiple issues, take action)
└── <40%:    RED (Critical, escalate immediately)
```

---

## Implementation Guide

### 1. Initialization

```rust
use tps_kaizen::{KaizenMetrics, MetricRecorder, MetricAnalyzer, Dashboard};
use std::sync::Arc;

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize metrics
    let metrics = Arc::new(KaizenMetrics::new()?);

    // Create recorder
    let recorder = MetricRecorder::new(metrics.clone());

    // Create analyzer
    let analyzer = MetricAnalyzer::new(metrics.clone());

    // Create dashboard
    let dashboard = Dashboard::kaizen_default();

    Ok(())
}
```

### 2. Register SLOs

```rust
// Define SLOs
let slos = vec![
    tps_kaizen::Slo {
        name: "checkout_latency_p99".to_string(),
        metric: "kanban_latency_p99_ms".to_string(),
        target: 200.0,
        window_secs: 300,
        is_maximum: false, // lower is better
    },
    tps_kaizen::Slo {
        name: "checkout_success_rate".to_string(),
        metric: "jidoka_success_rate".to_string(),
        target: 99.9,
        window_secs: 3600,
        is_maximum: true, // higher is better
    },
];

// Register
for slo in slos {
    metrics.register_slo(slo);
}
```

### 3. Record Events

```rust
// Record Jidoka events
recorder.record_circuit_open("payment-service").await?;
recorder.record_failure("checkout", "Database timeout").await?;

// Record Kanban events
recorder.record_queue_depth("checkout", 42).await?;
recorder.record_task_completed("checkout", 185.5).await?; // 185.5ms latency

// Record Andon events
recorder.record_alert_fired("HighLatency", "critical").await?;
recorder.record_alert_cleared("HighLatency").await?;

// Record Heijunka events
recorder.record_worker_activated("worker-001").await?;
recorder.record_worker_deactivated("worker-002").await?;
```

### 4. Analysis Loop

```rust
#[tokio::main]
async fn main() -> Result<()> {
    let metrics = Arc::new(KaizenMetrics::new()?);
    let analyzer = MetricAnalyzer::new(metrics);

    loop {
        // Every 30 seconds
        tokio::time::sleep(tokio::time::Duration::from_secs(30)).await;

        // Check health
        let health = analyzer.health_score().await?;
        println!("System health: {:.1}%", health);

        // Get recommendations
        let recommendations = analyzer.analyze().await?;
        for rec in recommendations {
            println!("[P{}] {} - {}", rec.priority, rec.title, rec.action);
        }

        // Check SLO attainment
        for slo in metrics.all_slos() {
            let (met, attainment) = analyzer.check_slo_attainment(&slo).await?;
            let status = if met { "✅" } else { "⚠️" };
            println!("{} {}: {:.1}%", status, slo.name, attainment);
        }
    }
}
```

---

## Improvement Workflows

### Workflow 1: Weekly Kaizen Review

**Purpose**: Identify and prioritize improvements for the week

**Steps**:
1. **Gather metrics** (Monday morning)
   - Pull last 7 days of data
   - Compare to previous week
   - Identify trends (improving/degrading)

2. **Analyze root causes** (Monday 2h)
   - Use SPARQL to correlate events with metrics
   - Review incident logs
   - Identify 3-5 patterns

3. **Prioritize improvements** (Monday 3h)
   - Estimate effort: small (1-2h), medium (half-day), large (1+ day)
   - Estimate impact: measured in % improvement
   - Create tickets: Effort × Impact = Priority

4. **Assign and track** (Monday EOD)
   - Each engineer owns 1-2 improvements
   - Update metrics tracking dashboard
   - Daily standup: progress on improvements

5. **Verify impact** (Friday EOD)
   - Run improvement analysis
   - Measure % improvement vs. baseline
   - Document in Kaizen log

### Workflow 2: When Metric Degrades

**Trigger**: Metric X crosses alert threshold

**Steps**:
1. **Immediate (0-5 min)**
   - Alert fires (ops team notified)
   - Dashboard shows red/orange
   - Page on-call engineer

2. **Triage (5-30 min)**
   - On-call checks: What changed in last hour?
   - Deploy rollback if recent change
   - Collect evidence: logs, traces, metrics

3. **Root Cause (30-120 min)**
   - Run SPARQL queries for correlated events
   - Interview recent deployments
   - Check external dependency status

4. **Mitigation (immediate)**
   - Scale service / add workers / clear cache
   - Monitor metric recovery
   - Document action taken

5. **Follow-up (within 24h)**
   - Root cause analysis meeting
   - Permanent fix (code change, config, alert tuning)
   - Update runbooks for this pattern

### Workflow 3: Kaizen Experimentation

**Purpose**: Test hypothesis that change X improves metric Y

**Steps**:
1. **Hypothesis**
   ```
   If we [action], then [metric] will improve by [%]
   because [reason]

   Example: "If we add caching to payment API, then
   latency P99 will improve by 30% because fewer DB queries"
   ```

2. **Baseline**
   - Measure current metric for 1 hour
   - Record: mean, p95, p99, throughput

3. **Experiment (canary)**
   - Deploy change to 10% of traffic
   - Monitor for 1-2 hours
   - Compare metrics to 90% control group

4. **Analyze**
   - Calculate actual improvement
   - Was it 30%? More? Less? Negative?
   - Was improvement statistically significant?

5. **Decide**
   - If successful: rollout 100%
   - If marginal: continue canary or revert
   - If negative: rollback immediately, investigate why

6. **Document**
   - Add to Kaizen log
   - Include before/after metrics
   - Explain surprising results

---

## Dashboard Setup

### Grafana Dashboard Import

```bash
# Export dashboard JSON
curl http://localhost:3000/api/datasources/proxy/1/metrics \
  -H "Authorization: Bearer <token>" \
  > kaizen-dashboard.json

# Or generate from code
cargo run --example export_dashboard > kaizen-dashboard.json

# Import to Grafana
1. Open Grafana: http://localhost:3000
2. Dashboards → Import
3. Upload kaizen-dashboard.json
4. Configure Prometheus datasource
5. Click Import
```

### Dashboard Panels Explained

**Jidoka Panel** (Top-left, Gauge):
- Shows circuit open %
- Red zone: >10%
- Yellow zone: 5-10%
- Green zone: <5%

**Kanban Panel** (Center, Time-series):
- Queue depth (blue line)
- Latency P99 (orange line)
- Throughput (green area)
- Useful for: detecting queue backlog, latency spikes

**Andon Panel** (Top-right, Gauge):
- Critical alerts count
- Red if >0
- Yellow if 1-2
- Green if 0

**Heijunka Panel** (Bottom, Area):
- Worker utilization (stacked bars)
- Load balance coefficient (line)
- Queue variance (shaded area)

### Creating Custom Dashboards

```rust
use tps_kaizen::{Dashboard, DashboardPanel};

let custom_dashboard = Dashboard {
    title: "Payment Service SLO Dashboard".to_string(),
    description: "Real-time SLO tracking".to_string(),
    time_range: "24h".to_string(),
    panels: vec![
        DashboardPanel {
            title: "Latency P99 vs SLO".to_string(),
            query: "kanban_latency_p99_ms".to_string(),
            panel_type: "graph".to_string(),
            ylabel: Some("ms".to_string()),
            min: Some(0.0),
            max: Some(200.0), // SLO target
        },
        // ... more panels
    ],
};

let json = custom_dashboard.to_grafana_json()?;
```

---

## Root Cause Analysis

### The 5 Whys Technique

When a metric degrades, ask "Why?" 5 times:

```
Problem: Latency increased 40%

Why 1: Queue depth increased 60%
Why 2: Workers became unavailable
Why 3: Out of memory (OOM) crash
Why 4: Memory leak in payment cache
Why 5: Bug in cache eviction logic (LINE 342 of cache.rs)

ROOT CAUSE: Cache eviction bug in payment service
FIX: Implement LRU eviction instead of FIFO
PREVENTION: Add heap memory monitoring alert
```

### SPARQL Evidence Queries

```sparql
# Query 1: What events happened around latency spike?
PREFIX tps: <http://example.com/tps/>

SELECT ?event ?timestamp ?value
WHERE {
    ?event a tps:Event ;
        tps:timestamp ?timestamp ;
        tps:metricValue ?value .

    # Within 5 minutes of spike
    FILTER (?timestamp > "2024-01-25T10:00:00Z"^^xsd:dateTime &&
            ?timestamp < "2024-01-25T10:05:00Z"^^xsd:dateTime)
}
ORDER BY ?timestamp

# Query 2: Which components failed before latency spike?
SELECT ?component ?failureTime
WHERE {
    ?failure a tps:JidokaFailure ;
        tps:component ?component ;
        tps:timestamp ?failureTime .

    # 1 hour before spike
    FILTER (?failureTime > "2024-01-25T09:00:00Z"^^xsd:dateTime &&
            ?failureTime < "2024-01-25T10:00:00Z"^^xsd:dateTime)
}

# Query 3: Correlation analysis
SELECT ?metric (COUNT(?event) as ?event_count) ?correlation
WHERE {
    ?event a tps:Event ;
        tps:correlatedMetric ?metric .
}
GROUP BY ?metric
ORDER BY DESC(?event_count)
```

---

## Examples

### Example 1: Monitoring Payment Service

```rust
use tps_kaizen::*;
use std::sync::Arc;

#[tokio::main]
async fn main() -> Result<()> {
    // Setup
    let metrics = Arc::new(KaizenMetrics::new()?);
    let recorder = MetricRecorder::new(metrics.clone());
    let analyzer = MetricAnalyzer::new(metrics.clone());

    // Define SLOs
    metrics.register_slo(Slo {
        name: "checkout_latency".to_string(),
        metric: "kanban_latency_p99_ms".to_string(),
        target: 200.0,
        window_secs: 300,
        is_maximum: false,
    });

    // Simulation: process 100 requests
    for i in 0..100 {
        // Simulate varying latencies
        let latency = if i % 10 == 0 {
            500.0 // occasional slow request
        } else {
            100.0 + (i as f64) % 50.0 // mostly fast
        };

        recorder.record_task_completed("payment-checkout", latency).await?;

        // Simulate occasional failures
        if i % 25 == 0 {
            recorder
                .record_failure("payment-service", "Timeout")
                .await?;
            recorder.record_circuit_open("payment-service").await?;
        }

        tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
    }

    // Analyze
    let health = analyzer.health_score().await?;
    println!("Health score: {:.1}%", health);

    let recommendations = analyzer.analyze().await?;
    println!("\nRecommendations:");
    for rec in recommendations {
        println!("  [P{}] {}: {}", rec.priority, rec.title, rec.action);
    }

    Ok(())
}
```

### Example 2: Weekly Review Report

```rust
#[tokio::main]
async fn main() -> Result<()> {
    let metrics = Arc::new(KaizenMetrics::new()?);
    let analyzer = MetricAnalyzer::new(metrics.clone());

    // Get metrics from last 7 days
    let health_now = analyzer.health_score().await?;
    let trend_latency = analyzer.analyze_trend("kanban_latency_p99_ms", 604800).await?;
    let trend_queue = analyzer.analyze_trend("kanban_queue_depth", 604800).await?;

    println!("╔════════════════════════════════════════════════╗");
    println!("║       KAIZEN WEEKLY REVIEW - Week of Jan 22   ║");
    println!("╚════════════════════════════════════════════════╝\n");

    println!("📊 SYSTEM HEALTH");
    println!("  Current score: {:.1}% {} health status", health_now,
        if health_now > 80.0 { "🟢" } else if health_now > 60.0 { "🟡" } else { "🔴" });

    println!("\n📈 TRENDS");
    println!("  Latency P99:  {:.3} ({} trend)", trend_latency.trend,
        if trend_latency.trend < 0.0 { "IMPROVING ✅" } else { "DEGRADING ⚠️" });
    println!("  Queue depth:  {:.3} ({} trend)", trend_queue.trend,
        if trend_queue.trend < 0.0 { "IMPROVING ✅" } else { "DEGRADING ⚠️" });

    println!("\n🎯 SLO ATTAINMENT");
    for slo in metrics.all_slos() {
        let (met, att) = analyzer.check_slo_attainment(&slo).await?;
        let status = if met { "✅ MET" } else { "⚠️ MISS" };
        println!("  {}: {:.1}% (target: {:.0})", slo.name, att, slo.target);
    }

    Ok(())
}
```

---

## Best Practices

### 1. **Metrics Hygiene**

✅ **DO**:
- Record metrics with descriptive names
- Use consistent labels (service, region, tier)
- Aggregate periodically to prevent data explosion
- Set reasonable retention (1-2 years)

❌ **DON'T**:
- Record same metric multiple times
- Create metrics without SLO context
- Log PII or credentials in metric labels
- Never reset counters (always cumulative)

### 2. **Alert Tuning**

✅ **DO**:
- Alert on trends, not absolute values
- Set thresholds based on SLOs
- Use time-series smoothing (avoid noise)
- Test alert firing regularly

❌ **DON'T**:
- Set alert threshold = SLO target (too tight)
- Alert on every metric individually (alert fatigue)
- Ignore alerts for >30 minutes (escalate sooner)
- Tune alerts by disabling them

### 3. **Continuous Improvement**

✅ **DO**:
- Pick ONE improvement per week per engineer
- Measure impact (before/after metrics)
- Document improvements in Kaizen log
- Celebrate wins (1% improvements compound)

❌ **DON'T**:
- "Optimize everything" (pick prioritized improvements)
- Guess improvements work (measure always)
- Revert improvements due to marginal gains (1% → 300% annually)
- Forget why you made each change

### 4. **Documentation**

✅ **DO**:
- Explain metric definitions
- Document SLO rationale
- Keep runbooks updated
- Link metrics to business outcomes

❌ **DON'T**:
- Leave metrics undocumented
- Change SLO without analysis
- Keep outdated runbooks
- Assume "everyone knows what this means"

---

## Troubleshooting

### Problem: Alert Fatigue

**Symptoms**:
- 50+ alerts firing per day
- Team ignores alerts
- Can't identify real issues

**Root causes**:
- Thresholds set too low (below normal operation)
- Multiple alerts for same issue
- Noisy metrics (high variance)

**Solution**:
```rust
// 1. Review alert thresholds
let alerts = metrics.andon.alert_frequency_per_min.get();
if alerts > 10.0 {
    println!("⚠️ Alert fatigue detected: {:.1} alerts/min", alerts);

    // 2. Adjust thresholds up
    // Before: alert if queue_depth > 50
    // After: alert if queue_depth > 100 AND latency > 500ms (AND condition)

    // 3. Use time-series smoothing
    // Before: alert on any latency spike
    // After: alert if (p99_latency > 200ms for 5+ minutes)
}
```

### Problem: Missing Data Points

**Symptoms**:
- Gaps in metric graph
- Can't analyze trends
- SLO calculation incomplete

**Root causes**:
- Service crashed (not recording)
- Metrics scrape failing
- Data retention too short

**Solution**:
```rust
// 1. Monitor scrape health
let last_scrape = metrics.get_history("jidoka_failures_total").last();
if last_scrape.is_none() || (Utc::now() - last_scrape.timestamp) > Duration::minutes(5) {
    println!("🔴 Metrics scrape stalled!");
    // Escalate: Prometheus down? Network issue? Service dead?
}

// 2. Increase retention
// From: 15 days (default)
// To: 1 year for historical trends
```

### Problem: Metric Spikes / Outliers

**Symptoms**:
- Random latency P99 spikes
- Can't identify real degradation
- SLO threshold calculations off

**Root causes**:
- Garbage collection pauses
- Disk I/O hiccups
- External service slowness
- Measurement outliers (measurement error)

**Solution**:
```rust
// 1. Use percentiles, not max
// Before: alert on max latency (prone to outliers)
// After: alert on p99 latency (robust to outliers)

// 2. Remove obvious outliers
for value in &metric_values {
    if value > mean + (3 * stddev) {
        println!("Outlier detected: {}, removing from analysis", value);
        // Don't delete from raw data, just exclude from SLO calc
    }
}

// 3. Smooth with moving average
let window_secs = 60;
let smoothed = recorder.aggregate_window("metric_name", window_secs).await?;
```

### Problem: SLO Frequently Missed

**Symptoms**:
- SLO attainment: 95% (target 99.9%)
- Frequent alerts
- Customer complaints

**Root causes**:
- SLO too aggressive (unrealistic target)
- System under capacity
- Process issues (deployments, scaling delays)

**Solution**:
```rust
// 1. Analyze why SLO missed
let (met, attainment) = analyzer.check_slo_attainment(&slo).await?;
if attainment < 99.0 {
    // Is it all the time? Or periodic?
    let trend = analyzer.analyze_trend(&slo.metric, 86400).await?;
    println!("Trend: {:.3}, degrading at {:.2}% per hour", trend.trend);
}

// 2. Either:
//    a) Improve system (scale, optimize)
//    b) Revise SLO (be realistic)
//    c) Increase failure budget (99.9% → 99.5%)

// 3. Phase approach
// Week 1: 95% attainment target (catch up)
// Week 2: 97% attainment target
// Week 3: 99% attainment target
// Week 4: 99.9% attainment target (sustainable)
```

---

## Appendix: Complete Metric Reference

See `crates/tps-kaizen/README.md` for API documentation and examples.

---

**Remember**: Kaizen is not a one-time activity. It's a mindset of continuous improvement,
where every engineer owns their piece of the system and makes it incrementally better each week.

> "The idea is not to increase production, but to decrease defects through continuous improvement."
> - Kaizen principle
