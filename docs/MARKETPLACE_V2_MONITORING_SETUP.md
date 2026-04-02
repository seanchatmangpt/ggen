<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Marketplace V2 Monitoring & Observability Setup](#marketplace-v2-monitoring--observability-setup)
  - [Table of Contents](#table-of-contents)
  - [OpenTelemetry Configuration](#opentelemetry-configuration)
    - [Rust Application Setup](#rust-application-setup)
    - [Instrumentation in Code](#instrumentation-in-code)
  - [Prometheus Metrics](#prometheus-metrics)
    - [Metrics Definitions](#metrics-definitions)
    - [Metric Collection](#metric-collection)
  - [Grafana Dashboards](#grafana-dashboards)
    - [Main SLO Dashboard](#main-slo-dashboard)
    - [Performance Comparison Dashboard (V1 vs V2)](#performance-comparison-dashboard-v1-vs-v2)
  - [Alert Rules](#alert-rules)
    - [Prometheus Alert Rules](#prometheus-alert-rules)
    - [Alert Routing (AlertManager)](#alert-routing-alertmanager)
  - [SLO Definitions](#slo-definitions)
    - [Service Level Objectives](#service-level-objectives)
    - [SLO Tracking](#slo-tracking)
  - [Logging Setup](#logging-setup)
    - [Structured Logging with Tracing](#structured-logging-with-tracing)
    - [Log Aggregation (Loki)](#log-aggregation-loki)
    - [Log Queries (Loki)](#log-queries-loki)
  - [Deployment Checklist](#deployment-checklist)
    - [Before Deployment](#before-deployment)
    - [After Deployment](#after-deployment)
  - [Maintenance](#maintenance)
    - [Weekly Tasks](#weekly-tasks)
    - [Monthly Tasks](#monthly-tasks)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Marketplace V2 Monitoring & Observability Setup

**Version:** 1.0.0
**Date:** 2025-11-18
**Purpose:** Production monitoring configuration for marketplace v2 migration

---

## Table of Contents

1. [OpenTelemetry Configuration](#opentelemetry-configuration)
2. [Prometheus Metrics](#prometheus-metrics)
3. [Grafana Dashboards](#grafana-dashboards)
4. [Alert Rules](#alert-rules)
5. [SLO Definitions](#slo-definitions)
6. [Logging Setup](#logging-setup)

---

## OpenTelemetry Configuration

### Rust Application Setup

**Add to `Cargo.toml`:**
```toml
[dependencies]
opentelemetry = { workspace = true }
opentelemetry-otlp = { workspace = true }
opentelemetry_sdk = { workspace = true, features = ["rt-tokio"] }
tracing = { workspace = true }
tracing-opentelemetry = { workspace = true }
tracing-subscriber = { workspace = true }
```

**Initialize in `main.rs`:**
```rust
use opentelemetry::global;
use opentelemetry_otlp::WithExportConfig;
use opentelemetry_sdk::runtime;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

fn init_telemetry() -> anyhow::Result<()> {
    // OTLP exporter configuration
    let otlp_exporter = opentelemetry_otlp::new_exporter()
        .tonic()
        .with_endpoint("http://localhost:4317");

    // Tracer provider
    let tracer = opentelemetry_otlp::new_pipeline()
        .tracing()
        .with_exporter(otlp_exporter)
        .with_trace_config(
            opentelemetry_sdk::trace::config()
                .with_resource(opentelemetry_sdk::Resource::new(vec![
                    opentelemetry::KeyValue::new("service.name", "ggen-marketplace-v2"),
                    opentelemetry::KeyValue::new("service.version", env!("CARGO_PKG_VERSION")),
                ]))
        )
        .install_batch(runtime::Tokio)?;

    // Metrics provider
    let meter = opentelemetry_otlp::new_pipeline()
        .metrics(runtime::Tokio)
        .with_exporter(otlp_exporter)
        .build()?;

    // Set up tracing subscriber
    tracing_subscriber::registry()
        .with(tracing_opentelemetry::layer().with_tracer(tracer))
        .with(tracing_subscriber::fmt::layer())
        .with(tracing_subscriber::EnvFilter::from_default_env())
        .init();

    Ok(())
}
```

### Instrumentation in Code

**Search operations:**
```rust
use tracing::{instrument, info, warn};

#[instrument(skip(self), fields(query = %query, limit = limit))]
pub async fn search(&self, query: &str, limit: usize) -> Result<Vec<Package>> {
    let start = std::time::Instant::now();

    let results = self.execute_search(query, limit).await?;

    let duration_ms = start.elapsed().as_millis() as u64;
    info!(
        duration_ms = duration_ms,
        result_count = results.len(),
        "Search completed"
    );

    // Record metric
    self.metrics.record_search_latency(duration_ms);
    self.metrics.record_search_results(results.len());

    Ok(results)
}
```

**Cache operations:**
```rust
#[instrument(skip(self), fields(key = %key))]
pub async fn get_cached<T>(&self, key: &str) -> Option<Arc<T>> {
    let result = self.cache.get(key).await;

    let hit = result.is_some();
    self.metrics.record_cache_access(hit);

    if hit {
        info!("Cache HIT for key: {}", key);
    } else {
        warn!("Cache MISS for key: {}", key);
    }

    result
}
```

---

## Prometheus Metrics

### Metrics Definitions

**Create `src/metrics.rs`:**
```rust
use opentelemetry::{
    metrics::{Counter, Histogram, Meter, ObservableGauge},
    KeyValue,
};
use std::sync::Arc;

pub struct MarketplaceMetrics {
    // Counters
    search_total: Counter<u64>,
    search_errors: Counter<u64>,
    cache_hits: Counter<u64>,
    cache_misses: Counter<u64>,

    // Histograms
    search_latency: Histogram<u64>,
    install_latency: Histogram<u64>,
    rdf_query_latency: Histogram<u64>,

    // Gauges
    cache_size: ObservableGauge<u64>,
    active_searches: ObservableGauge<i64>,
}

impl MarketplaceMetrics {
    pub fn new(meter: &Meter) -> Self {
        Self {
            search_total: meter
                .u64_counter("marketplace.search.total")
                .with_description("Total number of search operations")
                .init(),

            search_errors: meter
                .u64_counter("marketplace.search.errors")
                .with_description("Total number of search errors")
                .init(),

            cache_hits: meter
                .u64_counter("marketplace.cache.hits")
                .with_description("Cache hit count")
                .init(),

            cache_misses: meter
                .u64_counter("marketplace.cache.misses")
                .with_description("Cache miss count")
                .init(),

            search_latency: meter
                .u64_histogram("marketplace.search.latency")
                .with_description("Search operation latency in milliseconds")
                .with_unit("ms")
                .init(),

            install_latency: meter
                .u64_histogram("marketplace.install.latency")
                .with_description("Package installation latency")
                .with_unit("ms")
                .init(),

            rdf_query_latency: meter
                .u64_histogram("marketplace.rdf.query.latency")
                .with_description("RDF query latency")
                .with_unit("ms")
                .init(),

            cache_size: meter
                .u64_observable_gauge("marketplace.cache.size")
                .with_description("Current cache size")
                .init(),

            active_searches: meter
                .i64_observable_gauge("marketplace.searches.active")
                .with_description("Number of active search operations")
                .init(),
        }
    }

    pub fn record_search_latency(&self, duration_ms: u64) {
        self.search_latency.record(duration_ms, &[]);
        self.search_total.add(1, &[]);
    }

    pub fn record_search_error(&self, error_type: &str) {
        self.search_errors.add(1, &[KeyValue::new("error_type", error_type.to_string())]);
    }

    pub fn record_cache_access(&self, hit: bool) {
        if hit {
            self.cache_hits.add(1, &[]);
        } else {
            self.cache_misses.add(1, &[]);
        }
    }
}
```

### Metric Collection

**In application code:**
```rust
// Initialize metrics
let meter = global::meter("ggen-marketplace-v2");
let metrics = Arc::new(MarketplaceMetrics::new(&meter));

// Record search operation
let start = Instant::now();
let results = search_engine.search(query).await?;
let duration_ms = start.elapsed().as_millis() as u64;
metrics.record_search_latency(duration_ms);

// Record cache access
let cached = cache.get(&key).await;
metrics.record_cache_access(cached.is_some());

// Record errors
if let Err(e) = operation().await {
    metrics.record_search_error(&e.to_string());
}
```

---

## Grafana Dashboards

### Main SLO Dashboard

**Dashboard JSON:**
```json
{
  "dashboard": {
    "title": "Marketplace V2 - Production SLOs",
    "tags": ["marketplace", "v2", "slo"],
    "timezone": "browser",
    "panels": [
      {
        "id": 1,
        "title": "Search Latency (p95)",
        "type": "graph",
        "targets": [
          {
            "expr": "histogram_quantile(0.95, sum(rate(marketplace_search_latency_bucket[5m])) by (le))",
            "legendFormat": "p95 Latency",
            "refId": "A"
          }
        ],
        "yaxes": [
          {
            "label": "milliseconds",
            "format": "ms"
          }
        ],
        "thresholds": [
          {
            "value": 200,
            "colorMode": "critical",
            "fill": true,
            "line": true
          }
        ],
        "gridPos": {
          "h": 8,
          "w": 12,
          "x": 0,
          "y": 0
        }
      },
      {
        "id": 2,
        "title": "Cache Hit Rate",
        "type": "stat",
        "targets": [
          {
            "expr": "sum(rate(marketplace_cache_hits[5m])) / (sum(rate(marketplace_cache_hits[5m])) + sum(rate(marketplace_cache_misses[5m])))",
            "refId": "A"
          }
        ],
        "fieldConfig": {
          "defaults": {
            "unit": "percentunit",
            "thresholds": {
              "steps": [
                { "value": 0, "color": "red" },
                { "value": 0.8, "color": "yellow" },
                { "value": 0.9, "color": "green" }
              ]
            }
          }
        },
        "gridPos": {
          "h": 8,
          "w": 6,
          "x": 12,
          "y": 0
        }
      },
      {
        "id": 3,
        "title": "Error Rate",
        "type": "graph",
        "targets": [
          {
            "expr": "sum(rate(marketplace_search_errors[5m])) / sum(rate(marketplace_search_total[5m]))",
            "legendFormat": "Error Rate",
            "refId": "A"
          }
        ],
        "yaxes": [
          {
            "label": "percentage",
            "format": "percentunit"
          }
        ],
        "thresholds": [
          {
            "value": 0.01,
            "colorMode": "critical"
          }
        ],
        "gridPos": {
          "h": 8,
          "w": 6,
          "x": 18,
          "y": 0
        }
      },
      {
        "id": 4,
        "title": "Request Volume",
        "type": "graph",
        "targets": [
          {
            "expr": "sum(rate(marketplace_search_total[5m]))",
            "legendFormat": "Searches/sec",
            "refId": "A"
          }
        ],
        "gridPos": {
          "h": 8,
          "w": 12,
          "x": 0,
          "y": 8
        }
      },
      {
        "id": 5,
        "title": "RDF Query Performance",
        "type": "graph",
        "targets": [
          {
            "expr": "histogram_quantile(0.95, sum(rate(marketplace_rdf_query_latency_bucket[5m])) by (le))",
            "legendFormat": "p95 RDF Latency",
            "refId": "A"
          }
        ],
        "gridPos": {
          "h": 8,
          "w": 12,
          "x": 12,
          "y": 8
        }
      }
    ],
    "refresh": "30s",
    "time": {
      "from": "now-1h",
      "to": "now"
    }
  }
}
```

### Performance Comparison Dashboard (V1 vs V2)

```json
{
  "dashboard": {
    "title": "Marketplace V1 vs V2 Comparison",
    "panels": [
      {
        "title": "Latency Comparison",
        "targets": [
          {
            "expr": "histogram_quantile(0.95, sum(rate(marketplace_search_latency_bucket{version=\"v1\"}[5m])) by (le))",
            "legendFormat": "V1 p95"
          },
          {
            "expr": "histogram_quantile(0.95, sum(rate(marketplace_search_latency_bucket{version=\"v2\"}[5m])) by (le))",
            "legendFormat": "V2 p95"
          }
        ]
      },
      {
        "title": "Error Rate Comparison",
        "targets": [
          {
            "expr": "sum(rate(marketplace_errors{version=\"v1\"}[5m])) / sum(rate(marketplace_requests{version=\"v1\"}[5m]))",
            "legendFormat": "V1 Errors"
          },
          {
            "expr": "sum(rate(marketplace_errors{version=\"v2\"}[5m])) / sum(rate(marketplace_requests{version=\"v2\"}[5m]))",
            "legendFormat": "V2 Errors"
          }
        ]
      }
    ]
  }
}
```

---

## Alert Rules

### Prometheus Alert Rules

**Create `alerts/marketplace_v2.yaml`:**
```yaml
groups:
  - name: marketplace_v2_slos
    interval: 30s
    rules:
      # Latency SLO
      - alert: MarketplaceHighSearchLatency
        expr: |
          histogram_quantile(0.95,
            sum(rate(marketplace_search_latency_bucket[5m])) by (le)
          ) > 200
        for: 5m
        labels:
          severity: warning
          component: marketplace
          version: v2
        annotations:
          summary: "Search latency above SLO ({{ $value }}ms)"
          description: "95th percentile search latency is {{ $value }}ms, exceeding 200ms SLO"
          runbook_url: "https://docs.internal/runbooks/marketplace-latency"

      - alert: MarketplaceCriticalLatency
        expr: |
          histogram_quantile(0.95,
            sum(rate(marketplace_search_latency_bucket[5m])) by (le)
          ) > 500
        for: 2m
        labels:
          severity: critical
          component: marketplace
        annotations:
          summary: "CRITICAL: Search latency {{ $value }}ms"
          description: "Immediate action required - search latency severely degraded"

      # Cache SLO
      - alert: MarketplaceLowCacheHitRate
        expr: |
          sum(rate(marketplace_cache_hits[5m]))
          /
          (sum(rate(marketplace_cache_hits[5m])) + sum(rate(marketplace_cache_misses[5m])))
          < 0.80
        for: 10m
        labels:
          severity: info
          component: marketplace
        annotations:
          summary: "Cache hit rate below target ({{ $value | humanizePercentage }})"
          description: "Cache hit rate is {{ $value | humanizePercentage }}, below 80% target"

      # Error Rate SLO
      - alert: MarketplaceHighErrorRate
        expr: |
          sum(rate(marketplace_search_errors[5m]))
          /
          sum(rate(marketplace_search_total[5m]))
          > 0.01
        for: 2m
        labels:
          severity: critical
          component: marketplace
        annotations:
          summary: "Error rate above 1% ({{ $value | humanizePercentage }})"
          description: "Marketplace error rate is {{ $value | humanizePercentage }}"
          runbook_url: "https://docs.internal/runbooks/marketplace-errors"

      # Availability SLO
      - alert: MarketplaceDown
        expr: up{job="marketplace"} == 0
        for: 1m
        labels:
          severity: critical
          component: marketplace
        annotations:
          summary: "Marketplace service is down"
          description: "No successful health checks in last minute"

      # RDF Performance
      - alert: MarketplaceSlowRDFQueries
        expr: |
          histogram_quantile(0.95,
            sum(rate(marketplace_rdf_query_latency_bucket[5m])) by (le)
          ) > 100
        for: 5m
        labels:
          severity: warning
          component: marketplace
        annotations:
          summary: "RDF queries slow ({{ $value }}ms)"
          description: "RDF query latency degraded, may impact search performance"

      # Memory Usage
      - alert: MarketplaceHighMemory
        expr: process_resident_memory_bytes{job="marketplace"} > 1e9
        for: 5m
        labels:
          severity: warning
          component: marketplace
        annotations:
          summary: "High memory usage ({{ $value | humanize1024 }}B)"
          description: "Marketplace using over 1GB memory"
```

### Alert Routing (AlertManager)

**Configure `alertmanager.yaml`:**
```yaml
route:
  receiver: 'default'
  group_by: ['alertname', 'component']
  group_wait: 30s
  group_interval: 5m
  repeat_interval: 4h

  routes:
    - match:
        severity: critical
        component: marketplace
      receiver: 'marketplace-oncall'
      continue: true

    - match:
        severity: warning
        component: marketplace
      receiver: 'marketplace-team'

receivers:
  - name: 'default'
    slack_configs:
      - channel: '#alerts'
        send_resolved: true

  - name: 'marketplace-oncall'
    pagerduty_configs:
      - service_key: '<marketplace-oncall-key>'
        severity: '{{ .CommonLabels.severity }}'
    slack_configs:
      - channel: '#marketplace-incidents'
        send_resolved: true
        title: 'CRITICAL: {{ .CommonAnnotations.summary }}'

  - name: 'marketplace-team'
    slack_configs:
      - channel: '#marketplace-alerts'
        send_resolved: true
```

---

## SLO Definitions

### Service Level Objectives

| SLO | Target | Measurement Window | Error Budget |
|-----|--------|-------------------|--------------|
| **Search Latency (p95)** | <200ms | 30 days | 5% of requests >200ms |
| **Lookup Latency (p95)** | <100ms | 30 days | 5% of requests >100ms |
| **Cache Hit Rate** | >80% | 7 days | 20% miss rate allowed |
| **Error Rate** | <1% | 30 days | 1% of requests can fail |
| **Availability** | >99.9% | 30 days | 43.2 min downtime/month |

### SLO Tracking

**Prometheus Recording Rules:**
```yaml
groups:
  - name: marketplace_slo_recording
    interval: 1m
    rules:
      # Search latency p95 (good vs bad)
      - record: marketplace:search_latency:p95
        expr: |
          histogram_quantile(0.95,
            sum(rate(marketplace_search_latency_bucket[5m])) by (le)
          )

      - record: marketplace:search_requests:good
        expr: |
          sum(rate(marketplace_search_latency_bucket{le="200"}[5m]))

      - record: marketplace:search_requests:total
        expr: |
          sum(rate(marketplace_search_total[5m]))

      # SLO compliance (search latency)
      - record: marketplace:slo:search_latency:compliance
        expr: |
          marketplace:search_requests:good
          /
          marketplace:search_requests:total

      # Error budget remaining
      - record: marketplace:slo:error_budget:remaining
        expr: |
          1 - (
            (1 - marketplace:slo:search_latency:compliance)
            /
            0.05  # 5% error budget
          )

      # Cache hit rate
      - record: marketplace:cache:hit_rate
        expr: |
          sum(rate(marketplace_cache_hits[5m]))
          /
          (sum(rate(marketplace_cache_hits[5m])) + sum(rate(marketplace_cache_misses[5m])))

      # Error rate
      - record: marketplace:error_rate
        expr: |
          sum(rate(marketplace_search_errors[5m]))
          /
          sum(rate(marketplace_search_total[5m]))
```

---

## Logging Setup

### Structured Logging with Tracing

**Configure log levels:**
```bash
# Environment variables
export RUST_LOG=ggen_marketplace_v2=info,ggen_marketplace_v2::search=debug
export RUST_LOG_FORMAT=json  # JSON for machine parsing
```

**Log structure:**
```rust
use tracing::{info, warn, error, debug};

// Search operation logging
#[instrument(skip(self))]
pub async fn search(&self, query: &str) -> Result<Vec<Package>> {
    info!(
        query = %query,
        "Starting search operation"
    );

    let start = Instant::now();
    let results = self.execute_search(query).await;
    let duration_ms = start.elapsed().as_millis();

    match results {
        Ok(ref packages) => {
            info!(
                query = %query,
                result_count = packages.len(),
                duration_ms = duration_ms,
                cache_hit = self.was_cache_hit(),
                "Search completed successfully"
            );
        }
        Err(ref e) => {
            error!(
                query = %query,
                error = %e,
                duration_ms = duration_ms,
                "Search failed"
            );
        }
    }

    results
}
```

### Log Aggregation (Loki)

**Promtail configuration:**
```yaml
server:
  http_listen_port: 9080
  grpc_listen_port: 0

positions:
  filename: /tmp/positions.yaml

clients:
  - url: http://loki:3100/loki/api/v1/push

scrape_configs:
  - job_name: marketplace-v2
    static_configs:
      - targets:
          - localhost
        labels:
          job: marketplace-v2
          __path__: /var/log/ggen/marketplace-v2.log
    pipeline_stages:
      - json:
          expressions:
            level: level
            timestamp: timestamp
            message: message
            query: fields.query
      - labels:
          level:
          query:
```

### Log Queries (Loki)

```logql
# All errors in last hour
{job="marketplace-v2"} |= "level=ERROR" | json

# Slow searches (>200ms)
{job="marketplace-v2"} | json | duration_ms > 200

# Cache misses
{job="marketplace-v2"} | json | cache_hit = "false"

# Search query patterns
{job="marketplace-v2"} | json | query =~ ".*test.*"
```

---

## Deployment Checklist

### Before Deployment

- [ ] **Configure OTLP Exporter**
  - Set endpoint: `http://localhost:4317`
  - Verify connection

- [ ] **Set Up Prometheus**
  - Import recording rules
  - Import alert rules
  - Verify scraping

- [ ] **Create Grafana Dashboards**
  - Import SLO dashboard
  - Import comparison dashboard
  - Set up permissions

- [ ] **Configure AlertManager**
  - Set up Slack integration
  - Set up PagerDuty
  - Test alert routing

- [ ] **Enable Logging**
  - Configure Promtail
  - Set up Loki
  - Verify log ingestion

### After Deployment

- [ ] **Validate Metrics**
  - Check Prometheus targets
  - Verify metrics appearing
  - Test alerts (manual trigger)

- [ ] **Check Dashboards**
  - Verify data flowing to Grafana
  - Test refresh rates
  - Validate thresholds

- [ ] **Test Alerts**
  - Trigger test alert
  - Verify Slack notification
  - Verify PagerDuty incident

- [ ] **Monitor for 24h**
  - Watch error rates
  - Check SLO compliance
  - Review logs for anomalies

---

## Maintenance

### Weekly Tasks

- [ ] Review SLO compliance
- [ ] Check error budget consumption
- [ ] Analyze slow query patterns
- [ ] Review cache hit rates

### Monthly Tasks

- [ ] Update alert thresholds based on actual performance
- [ ] Review and archive old metrics
- [ ] Optimize dashboard layout
- [ ] Update runbooks based on incidents

---

**Document Version:** 1.0.0
**Last Updated:** 2025-11-18
**Owner:** DevOps Team
**Review Date:** 2025-12-18
