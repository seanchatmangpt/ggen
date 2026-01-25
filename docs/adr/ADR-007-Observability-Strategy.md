<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ADR-007: Observability Strategy (CloudTrace, CloudProfiler, OpenTelemetry)](#adr-007-observability-strategy-cloudtrace-cloudprofiler-opentelemetry)
  - [Problem Statement](#problem-statement)
  - [Decision](#decision)
  - [Rationale](#rationale)
    - [Prometheus Metrics](#prometheus-metrics)
    - [Jaeger Distributed Tracing](#jaeger-distributed-tracing)
    - [Cloud Profiler](#cloud-profiler)
  - [Implementation](#implementation)
    - [OpenTelemetry Setup (Rust)](#opentelemetry-setup-rust)
    - [Prometheus Metrics](#prometheus-metrics-1)
    - [Kubernetes Prometheus Configuration](#kubernetes-prometheus-configuration)
    - [Jaeger Deployment](#jaeger-deployment)
    - [Cloud Profiler Integration](#cloud-profiler-integration)
    - [Observability Instrumentation in Services](#observability-instrumentation-in-services)
  - [Monitoring Dashboards](#monitoring-dashboards)
    - [Prometheus + Grafana](#prometheus--grafana)
    - [Jaeger UI](#jaeger-ui)
    - [Cloud Profiler](#cloud-profiler-1)
  - [Log Aggregation](#log-aggregation)
  - [Alerts Configuration](#alerts-configuration)
  - [Consequences](#consequences)
    - [Positive](#positive)
    - [Negative](#negative)
  - [Performance Impact](#performance-impact)
  - [SLOs for Observability](#slos-for-observability)
  - [Cost Optimization](#cost-optimization)
  - [References](#references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ADR-007: Observability Strategy (CloudTrace, CloudProfiler, OpenTelemetry)

**Status:** Accepted
**Date:** 2026-01-25
**Context:** Implementing monitoring, tracing, and profiling for TAI microservices
**Deciders:** Observability Engineering Team

## Problem Statement

TAI system requires visibility into:
- Request paths through services (distributed tracing)
- Performance bottlenecks (profiling)
- System health and metrics
- Error rates and root causes
- Resource utilization and costs

Need: Centralized observability, minimal application changes, automatic instrumentation.

## Decision

**Three-pillar observability stack:**
1. **Metrics:** Prometheus (scrape-based) + CloudMonitoring
2. **Traces:** Jaeger (via OpenTelemetry) + Cloud Trace
3. **Profiles:** pprof (in-process) + Cloud Profiler

## Rationale

### Prometheus Metrics
- Industry standard for Kubernetes
- Works with Istio automatically
- Scrape-based (services don't push)
- AlertManager integration
- Long-term storage in CloudMonitoring

### Jaeger Distributed Tracing
- Open-source, mature
- Integrated with Istio (automatic sidecar tracing)
- OpenTelemetry SDK for application instrumentation
- Visualization and root cause analysis
- Cloud Trace as managed backend

### Cloud Profiler
- CPU and memory profiling in production
- Low overhead (<5% CPU)
- Continuous profiling (not sampling)
- Comparison view for performance regression detection

## Implementation

### OpenTelemetry Setup (Rust)

```rust
use opentelemetry::global;
use opentelemetry_jaeger::new_jaeger_pipeline;
use tracing_opentelemetry::OpenTelemetryLayer;
use tracing_subscriber::layer::SubscriberExt;

pub fn init_tracing() -> Result<()> {
    // Configure Jaeger exporter
    let jaeger_pipeline = new_jaeger_pipeline()
        .install_simple()?;

    let tracer = global::tracer("tai-governor");

    // Layer tracing with OpenTelemetry
    let otel_layer = OpenTelemetryLayer::new(tracer);

    // Combine with fmt layer
    let subscriber = tracing_subscriber::registry()
        .with(otel_layer)
        .with(tracing_subscriber::fmt::layer());

    tracing::subscriber::set_default(subscriber);

    Ok(())
}

// Automatic span creation for gRPC calls
#[tonic::async_trait]
impl Governor for GovernorService {
    #[tracing::instrument(skip(self, request))]
    async fn propose_policy(
        &self,
        request: Request<Policy>,
    ) -> Result<Response<Receipt>, Status> {
        let span = tracing::info_span!(
            "propose_policy",
            policy.id = %request.get_ref().id,
            policy.version = request.get_ref().version
        );

        async {
            // Span automatically created with timing/outcome
            self.process_policy(request.into_inner()).await
        }
        .instrument(span)
        .await
    }
}

// Manual span for business logic
pub async fn validate_policy(policy: &Policy) -> Result<()> {
    let span = tracing::info_span!("validate_policy", policy_type = %policy.policy_type);

    async {
        // Validation logic
        Ok(())
    }
    .instrument(span)
    .await
}
```

### Prometheus Metrics

```rust
use prometheus::{Counter, Histogram, Registry};

lazy_static::lazy_static! {
    pub static ref REGISTRY: Registry = Registry::new();

    pub static ref REQUEST_COUNT: Counter = Counter::new(
        "tai_requests_total",
        "Total requests"
    ).expect("metric creation failed");

    pub static ref REQUEST_DURATION_SECONDS: Histogram = Histogram::new(
        "tai_request_duration_seconds",
        "Request duration in seconds"
    ).expect("metric creation failed");

    pub static ref SIGNAL_PROCESSING_DURATION: Histogram = Histogram::new(
        "tai_signal_processing_duration_seconds",
        "Signal processing time"
    ).expect("metric creation failed");

    pub static ref POLICY_ENFORCEMENT_ERRORS: Counter = Counter::new(
        "tai_policy_enforcement_errors_total",
        "Policy enforcement errors"
    ).expect("metric creation failed");
}

// Use metrics in handlers
pub async fn process_signal(signal: &Signal) -> Result<()> {
    let timer = REQUEST_DURATION_SECONDS.start_timer();

    // Processing logic
    match validate_and_process(signal).await {
        Ok(_) => {
            REQUEST_COUNT.inc();
            timer.observe_duration();
            Ok(())
        }
        Err(e) => {
            POLICY_ENFORCEMENT_ERRORS.inc();
            timer.stop_and_discard();
            Err(e)
        }
    }
}
```

### Kubernetes Prometheus Configuration

```yaml
# ServiceMonitor for Prometheus scraping
apiVersion: monitoring.coreos.com/v1
kind: ServiceMonitor
metadata:
  name: tai-services
  namespace: tai-system
spec:
  selector:
    matchLabels:
      app: governor
  endpoints:
  - port: metrics
    interval: 30s
    path: /metrics

---
# PrometheusRule for alerts
apiVersion: monitoring.coreos.com/v1
kind: PrometheusRule
metadata:
  name: tai-alerts
  namespace: tai-system
spec:
  groups:
  - name: tai.rules
    interval: 30s
    rules:
    - alert: HighErrorRate
      expr: |
        (
          sum(rate(tai_requests_total{status=~"5.."}[5m]))
          /
          sum(rate(tai_requests_total[5m]))
        ) > 0.05
      for: 5m
      annotations:
        summary: "High error rate detected"

    - alert: SlowRequests
      expr: |
        histogram_quantile(
          0.99,
          rate(tai_request_duration_seconds_bucket[5m])
        ) > 1.0
      for: 5m
      annotations:
        summary: "99th percentile latency > 1 second"
```

### Jaeger Deployment

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: jaeger
  namespace: monitoring
spec:
  replicas: 1
  template:
    spec:
      containers:
      - name: jaeger
        image: jaegertracing/all-in-one:latest
        ports:
        - name: jaeger-agent-zipkin-thrift
          containerPort: 6831
          protocol: UDP
        - name: jaeger-ui
          containerPort: 16686
        env:
        - name: COLLECTOR_ZIPKIN_HOST_PORT
          value: ":9411"
        - name: STORAGE
          value: "badger"
        volumeMounts:
        - name: badger-data
          mountPath: /badger/data
      volumes:
      - name: badger-data
        emptyDir: {}
---
apiVersion: v1
kind: Service
metadata:
  name: jaeger
spec:
  ports:
  - name: jaeger-agent-zipkin-thrift
    port: 6831
    protocol: UDP
  - name: jaeger-ui
    port: 16686
  selector:
    app: jaeger
```

### Cloud Profiler Integration

```rust
// Cloud Profiler for continuous profiling
use google_cloudprofiler::{Profile, ProfilerClient};

pub async fn start_cloud_profiler() -> Result<()> {
    let mut client = ProfilerClient::new().await?;

    // Create profile
    let profile = Profile {
        profile_type: "CPU".to_string(),
        duration: Duration::from_secs(60),
    };

    // Upload to Cloud Profiler
    client.create_profile("tai-governor", &profile).await?;

    Ok(())
}
```

### Observability Instrumentation in Services

```rust
// Governor service with full instrumentation
#[tonic::async_trait]
impl Governor for GovernorService {
    #[tracing::instrument(skip(self, request))]
    async fn propose_policy(
        &self,
        request: Request<Policy>,
    ) -> Result<Response<Receipt>, Status> {
        let policy_id = request.get_ref().id.clone();

        // OpenTelemetry span automatically created
        // Prometheus metrics automatically collected
        // Jaeger trace automatically sent

        // Custom metrics
        REQUEST_COUNT.inc();
        let _timer = REQUEST_DURATION_SECONDS.start_timer();

        // Process policy
        let receipt = self.store_policy(request.into_inner()).await
            .map_err(|e| {
                POLICY_ENFORCEMENT_ERRORS.inc();
                Status::internal(e.to_string())
            })?;

        Ok(Response::new(receipt))
    }
}
```

## Monitoring Dashboards

### Prometheus + Grafana

Create dashboards for:
1. **Service Health:** Request rate, error rate, latency (p50, p95, p99)
2. **Resource Usage:** CPU, memory, network per pod
3. **Business Metrics:** Policies enforced, signals processed, actions completed
4. **Istio Mesh:** Service-to-service latency, drop rate
5. **Application:** Custom spans, custom metrics

### Jaeger UI

- Service list and dependencies
- Trace search by service/operation/duration
- Trace waterfall view
- Span comparison for performance regression

### Cloud Profiler

- CPU flame graphs
- Memory allocation timeline
- Comparison between versions
- Anomaly detection

## Log Aggregation

```yaml
# Fluent Bit configuration for log shipping
apiVersion: v1
kind: ConfigMap
metadata:
  name: fluent-bit-config
data:
  fluent-bit.conf: |
    [SERVICE]
      Flush 5
      Daemon Off
      Log_Level info
      Parsers_File parsers.conf

    [INPUT]
      Name tail
      Path /var/log/containers/*.log
      Parser docker
      Tag kube.*

    [FILTER]
      Name kubernetes
      Match kube.*
      Kube_Tag_Prefix kube.

    [OUTPUT]
      Name stackdriver
      Match *
      resource k8s_container
      resource_labels k8s_cluster_name=tai-cluster
```

## Alerts Configuration

```yaml
# Critical alerts
- name: ServiceDown
  condition: up{job="tai-governor"} == 0
  duration: 1m
  severity: critical

- name: HighErrorRate
  condition: rate(errors_total[5m]) / rate(requests_total[5m]) > 0.1
  duration: 5m
  severity: warning

- name: SlowRequests
  condition: histogram_quantile(0.99, latency_bucket) > 500ms
  duration: 10m
  severity: warning

- name: HighMemoryUsage
  condition: container_memory_usage_bytes / container_spec_memory_limit_bytes > 0.9
  duration: 5m
  severity: warning
```

## Consequences

### Positive
- Full visibility into system behavior
- Automatic instrumentation (minimal code)
- Centralized logs, traces, metrics
- Historical data for trend analysis
- Automated alerting on anomalies

### Negative
- Observability infrastructure overhead (~30 pods)
- Network traffic for metrics/traces (~10% overhead)
- Cost of Cloud Profiler and Cloud Trace
- Learning curve for operators
- Data retention costs

## Performance Impact

- OpenTelemetry instrumentation: <5% CPU overhead
- Prometheus scraping: <1% CPU
- Log shipping: <2% CPU
- Total: <8% resource overhead (acceptable)

## SLOs for Observability

- Trace delivery latency: <5 seconds
- Metrics scrape latency: <10 seconds
- Log delivery latency: <30 seconds
- Jaeger query response: <500ms
- Alert notification: <1 minute

## Cost Optimization

1. Sample traces in production (10% sample rate)
2. Reduce metric cardinality (drop high-cardinality labels)
3. Compress logs before shipping
4. Set retention policies (30 days for Jaeger)
5. Use Reserved capacity for Cloud Monitoring

## References
- [OpenTelemetry Rust SDK](https://docs.rs/opentelemetry/)
- [Jaeger Documentation](https://www.jaegertracing.io/docs/)
- [Cloud Trace](https://cloud.google.com/trace)
- [Cloud Profiler](https://cloud.google.com/profiler)
- [Prometheus Documentation](https://prometheus.io/docs/)
