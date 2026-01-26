# Monitoring and Alerting

Complete guide to monitoring, metrics, logging, and alerting for TAI Erlang Autonomics.

---

## Overview

Comprehensive observability stack with Prometheus metrics, structured logging, OpenTelemetry tracing, and GCP Cloud Monitoring integration.

---

## Prometheus Metrics

### Metric Categories

#### HTTP Metrics

```
# Request counts by endpoint and status
tai_http_requests_total{endpoint, method, status}

# Request duration distribution
tai_http_request_duration_seconds{endpoint, quantile=[0.5, 0.95, 0.99]}

# Request/response sizes
tai_http_request_size_bytes{endpoint}
tai_http_response_size_bytes{endpoint}

# Active connections
tai_http_active_connections{endpoint}
```

#### Governor Metrics

```
# State transitions
tai_governor_transitions_total{governor, from_state, to_state}

# Current state distribution
tai_governor_state{governor, state}

# State hold time (how long in current state)
tai_governor_state_duration_seconds{governor, state}
```

#### Receipt Metrics

```
# Receipt emission rate
tai_receipts_emitted_total{type} # type: transition, refusal, action_attempt, action_result

# Receipt emission latency
tai_receipt_emission_duration_ms{quantile=[0.5, 0.95, 0.99]}

# Hash chain verification rate
tai_receipt_verification_total{result} # result: success, failed

# Storage write latency (by backend)
tai_receipt_storage_duration_ms{backend, operation}
```

#### Action Executor Metrics

```
# Action execution count
tai_action_executions_total{action_type, result} # result: success, timeout, error

# Action execution duration
tai_action_execution_duration_ms{action_type, quantile=[0.5, 0.95, 0.99]}

# Action queue depth
tai_action_queue_depth{pool_id}

# Worker pool utilization
tai_worker_pool_utilization{pool_id}
```

#### System Metrics

```
# Erlang memory usage
erlang_memory_bytes{type} # type: total, processes, atom, binary, ets

# Erlang process count
erlang_process_count

# Erlang port count
erlang_port_count

# Garbage collection
erlang_gc_total{type} # type: collections, words_reclaimed
```

### Querying Metrics

**Prometheus endpoint**:
```bash
curl http://localhost:8080/metrics
```

**Example queries**:

```promql
# Request rate (requests/sec)
rate(tai_http_requests_total[5m])

# P95 latency by endpoint
histogram_quantile(0.95, rate(tai_http_request_duration_seconds_bucket[5m]))

# Error rate percentage
(sum(rate(tai_http_requests_total{status=~"5.."}[5m])) /
 sum(rate(tai_http_requests_total[5m]))) * 100

# State transition rate
rate(tai_governor_transitions_total[5m])

# Receipt emission distribution
sum(rate(tai_receipts_emitted_total[5m])) by (type)
```

---

## Structured Logging

### Log Format

All logs are emitted as structured JSON:

```json
{
  "timestamp": "2024-01-01T12:00:00.000Z",
  "level": "info",
  "logger": "tai_autonomics",
  "message": "Request received",
  "context": {
    "request_id": "req-123",
    "tenant_id": "tenant-123",
    "endpoint": "/pubsub",
    "method": "POST",
    "status": 200,
    "duration_ms": 45,
    "receipt_id": "receipt-001"
  },
  "metadata": {}
}
```

### Log Levels

| Level | When to Use | Examples |
|-------|------------|----------|
| `DEBUG` | Development and troubleshooting | Governor state transitions, detailed request processing |
| `INFO` | Normal operations | Request received, receipt emitted, service started |
| `WARN` | Potential issues | Slow requests (>500ms), retries, deprecated usage |
| `ERROR` | Error conditions | Failed requests, service degradation, validation errors |

### Log Configuration

**Environment Variable**:
```bash
export LOG_LEVEL=info
export JSON_LOGS=true
```

**sys.config**:
```erlang
{kernel, [
  {logger_level, info},
  {logger, [
    {handler, default, logger_std_h, #{
      config => #{type => {file, "/var/log/tai-autonomics.log"}},
      formatter => {logger_formatter, #{
        template => [time, " ", level, " ", msg],
        time_designator => "T"
      }}
    }}
  ]}
]}
```

### Querying Logs (GCP Cloud Logging)

```bash
# All logs
gcloud logging read "resource.type=cloud_run_revision" --limit=50

# Error logs
gcloud logging read "resource.type=cloud_run_revision AND severity=ERROR" --limit=50

# By tenant
gcloud logging read "resource.type=cloud_run_revision AND jsonPayload.tenant_id=tenant-123" --limit=50

# Slow requests (>1s)
gcloud logging read "resource.type=cloud_run_revision AND jsonPayload.duration_ms>1000" --limit=50

# Failed transitions
gcloud logging read "resource.type=cloud_run_revision AND jsonPayload.type=refusal" --limit=50
```

---

## OpenTelemetry Tracing

### Trace Setup

**Environment Variables**:
```bash
export TRACING_ENABLED=true
export TRACING_SAMPLE_RATE=0.1  # Sample 10% of traces
```

**Configuration**:
```erlang
{tai_autonomics, [
  {tracing, #{
    enabled => true,
    sample_rate => 0.1,
    exporter => otel_exporter_trace_otlp,
    endpoint => "http://localhost:4317",  # OTLP endpoint
    resource_attributes => #{
      <<"service.name">> => <<"tai-autonomics">>,
      <<"service.version">> => <<"1.0.0">>,
      <<"environment">> => <<"production">>
    }
  }}
]}
```

### Spans Emitted

```
tai-autonomics.request
├── http.method: POST
├── http.target: /pubsub
├── http.status_code: 200
├── request_id: req-123
└── duration: 45ms
    ├── pubsub.parse
    ├── governor.transition
    │   ├── governor.validate_state
    │   └── governor.emit_receipt
    ├── receipt.store
    └── http.response
```

### Querying Traces (GCP Cloud Trace)

```bash
# View recent traces
gcloud trace list

# Get trace details
gcloud trace describe TRACE_ID

# Export traces (JSON)
gcloud trace export --limit=10 --format=json

# Search by tenant
gcloud trace list --filter="tenant_id=tenant-123"
```

---

## Dashboards

### Dashboard 1: Service Health

**Key Panels**:
- Health check status (red/yellow/green)
- Request rate (requests/sec)
- Error rate (%)
- Average latency (ms)
- P99 latency (ms)

**Query Examples**:

```promql
# Health status
up{job="tai-autonomics"}

# Request rate
rate(tai_http_requests_total[5m])

# Error rate
(sum(rate(tai_http_requests_total{status=~"5.."}[5m])) /
 sum(rate(tai_http_requests_total[5m]))) * 100

# Latency percentiles
histogram_quantile(0.99, rate(tai_http_request_duration_seconds_bucket[5m]))
```

### Dashboard 2: Receipt Processing

**Key Panels**:
- Receipt emission rate (by type)
- Receipt storage latency
- Hash verification success rate
- Firestore write latency
- Refusal reasons (pie chart)

**Query Examples**:

```promql
# Emission rate by type
sum(rate(tai_receipts_emitted_total[5m])) by (type)

# Storage latency (p95)
histogram_quantile(0.95, rate(tai_receipt_storage_duration_ms_bucket[5m]))

# Verification success rate
(sum(rate(tai_receipt_verification_total{result="success"}[5m])) /
 sum(rate(tai_receipt_verification_total[5m]))) * 100
```

### Dashboard 3: Governor State

**Key Panels**:
- State distribution (current)
- Transition rate (by governor)
- Time in current state
- Action execution rate
- Action failure rate

**Query Examples**:

```promql
# Current state distribution
tai_governor_state

# Transition rate
rate(tai_governor_transitions_total[5m])

# Action success rate
(sum(rate(tai_action_executions_total{result="success"}[5m])) /
 sum(rate(tai_action_executions_total[5m]))) * 100
```

### Dashboard 4: Resource Utilization

**Key Panels**:
- Memory usage (MB)
- CPU usage (%)
- Active connections
- Action queue depth
- Worker pool utilization

**Query Examples**:

```promql
# Memory in MB
erlang_memory_bytes / 1024 / 1024

# Process count
erlang_process_count

# Action queue depth
tai_action_queue_depth

# Worker utilization
(tai_worker_pool_active / tai_worker_pool_total) * 100
```

---

## Alerting

### Alert Rules

**File**: `monitoring/alerts.yml`

```yaml
groups:
  - name: tai_autonomics
    interval: 30s
    rules:
      # Service health
      - alert: HealthCheckFailing
        expr: up{job="tai-autonomics"} == 0
        for: 2m
        annotations:
          summary: "TAI Autonomics health check failing"
          runbook: "RUNBOOK.md#health-check-fails"

      # High error rate
      - alert: HighErrorRate
        expr: |
          (sum(rate(tai_http_requests_total{status=~"5.."}[5m])) /
           sum(rate(tai_http_requests_total[5m]))) * 100 > 1
        for: 5m
        annotations:
          summary: "Error rate > 1% for 5 minutes"
          runbook: "RUNBOOK.md"

      # High latency
      - alert: HighLatency
        expr: |
          histogram_quantile(0.99, rate(tai_http_request_duration_seconds_bucket[5m])) > 1
        for: 5m
        annotations:
          summary: "P99 latency > 1s for 5 minutes"
          runbook: "RUNBOOK.md#high-latency"

      # Memory pressure
      - alert: MemoryUsageHigh
        expr: |
          (erlang_memory_bytes / 1024 / 1024 / 2048) * 100 > 80
        for: 5m
        annotations:
          summary: "Memory usage > 80% for 5 minutes"
          runbook: "RUNBOOK.md#out-of-memory"

      # Receipt processing delays
      - alert: ReceiptProcessingDelay
        expr: |
          histogram_quantile(0.95, rate(tai_receipt_storage_duration_ms_bucket[5m])) > 1000
        for: 5m
        annotations:
          summary: "Receipt storage latency (p95) > 1s"
          runbook: "RUNBOOK.md#receipts-not-being-written"
```

### Alert Notification Channels

**GCP Cloud Monitoring**:

```bash
# Create notification channel
gcloud alpha monitoring channels create \
  --display-name="TAI Autonomics Email" \
  --type=email \
  --channel-labels=email_address=team@example.com

# Create alert policy
gcloud alpha monitoring policies create \
  --notification-channels=CHANNEL_ID \
  --display-name="TAI Autonomics High Error Rate" \
  --condition-display-name="High error rate" \
  --condition-threshold-value=0.01 \
  --condition-threshold-filter='metric.type="custom.googleapis.com/tai_http_requests_total"'
```

**Slack Integration**:

```bash
# Create Slack webhook
# 1. Go to https://api.slack.com/apps
# 2. Create new app > From scratch
# 3. Enable Incoming Webhooks
# 4. Create new webhook

# Add notification channel
gcloud alpha monitoring channels create \
  --display-name="TAI Autonomics Slack" \
  --type=slack \
  --channel-labels=channel_name=#alerts \
  --channel-labels=url=https://hooks.slack.com/services/YOUR/WEBHOOK/URL
```

---

## Health Checks

### Liveness Probe

Checks if the service is still running:

```bash
curl -f http://localhost:8080/health || exit 1
```

**Configuration** (Cloud Run):
```yaml
livenessProbe:
  httpGet:
    path: /health
    port: 8080
  initialDelaySeconds: 30
  periodSeconds: 10
  timeoutSeconds: 5
  failureThreshold: 3
```

### Readiness Probe

Checks if the service can accept requests:

```bash
curl -f http://localhost:8080/health || exit 1
```

**Configuration** (Cloud Run):
```yaml
readinessProbe:
  httpGet:
    path: /health
    port: 8080
  initialDelaySeconds: 10
  periodSeconds: 5
  timeoutSeconds: 3
  failureThreshold: 3
```

### Startup Probe

Checks if the service finished startup:

```bash
curl -f http://localhost:8080/health || exit 1
```

**Configuration** (Cloud Run):
```yaml
startupProbe:
  httpGet:
    path: /health
    port: 8080
  initialDelaySeconds: 0
  periodSeconds: 10
  timeoutSeconds: 5
  failureThreshold: 30  # 30 * 10 = 300 seconds max startup
```

---

## SLOs and Objectives

### Service Level Objectives

| Objective | Target | Measurement |
|-----------|--------|-------------|
| Availability | 99.99% | Uptime / total time |
| Error Rate | < 0.1% | Failed requests / total requests |
| Latency P99 | < 500ms | 99th percentile request time |
| Latency P95 | < 100ms | 95th percentile request time |
| Receipt Processing | < 500ms | Receipt storage latency (p99) |

### Calculating SLO Compliance

```promql
# Monthly availability
(1 - (sum(increase(tai_http_requests_total{status=~"5.."}[30d])) /
       sum(increase(tai_http_requests_total[30d])))) * 100

# P99 latency
histogram_quantile(0.99, rate(tai_http_request_duration_seconds_bucket[30d]))

# Error budget (how much error is allowed)
error_budget = (1 - target) * total_requests
allowed_errors = 0.001 * total_requests
```

---

## Incident Response

### Alert Runbooks

**Alert**: `HealthCheckFailing`
- **Severity**: Critical
- **Runbook**: See RUNBOOK.md#health-check-fails
- **Mitigation**: Restart service, check logs
- **Escalation**: 5 min

**Alert**: `HighErrorRate`
- **Severity**: High
- **Runbook**: See RUNBOOK.md
- **Mitigation**: Check error logs, rollback if needed
- **Escalation**: 15 min

**Alert**: `HighLatency`
- **Severity**: Medium
- **Runbook**: See RUNBOOK.md#high-latency
- **Mitigation**: Increase concurrency, scale horizontally
- **Escalation**: 30 min

### Incident Severity Levels

| Severity | Impact | Response Time | Escalation |
|----------|--------|---------------|------------|
| Critical | Service down, > 50% error rate | 5 minutes | Immediate |
| High | Degraded performance, 1-50% error | 15 minutes | 30 min |
| Medium | Slow performance, < 1% error | 30 minutes | 1 hour |
| Low | Minor issues, informational | 1 hour | 24 hours |

---

## Tools and Integrations

### Local Development

**Prometheus**:
```bash
# Pull metrics
curl http://localhost:8080/metrics

# Run local Prometheus
docker run -d \
  -p 9090:9090 \
  -v prometheus.yml:/etc/prometheus/prometheus.yml \
  prom/prometheus
```

**Grafana**:
```bash
# Run local Grafana
docker run -d \
  -p 3000:3000 \
  grafana/grafana

# Access at http://localhost:3000
# Add Prometheus as data source: http://host.docker.internal:9090
```

### GCP Cloud Monitoring

```bash
# List metrics
gcloud monitoring metrics-descriptors list

# Create custom dashboard
gcloud monitoring dashboards create --config-from-file=dashboard.json

# View dashboard
gcloud monitoring dashboards list
```

### DataDog Integration

```erlang
{tai_autonomics, [
  {metrics, #{
    enabled => true,
    exporter => otel_exporter_metrics_datadog,
    endpoint => "https://api.datadoghq.com",
    api_key => os:getenv("DATADOG_API_KEY", ""),
    hostname => "tai-autonomics-prod"
  }}
]}
```

---

## Troubleshooting Observability

### Missing Metrics

**Problem**: Metrics endpoint returns 404

**Solution**:
```bash
# Verify metrics are enabled
export METRICS_ENABLED=true
_build/default/rel/tai_autonomics/bin/tai_autonomics restart

# Check metrics endpoint
curl http://localhost:8080/metrics
```

### No Logs Appearing

**Problem**: Logs not visible in Cloud Logging

**Solution**:
```bash
# Verify JSON logging is enabled
export JSON_LOGS=true

# Check log level
export LOG_LEVEL=info

# Verify GCP credentials
gcloud auth application-default login

# Check logs manually
tail -f _build/default/rel/tai_autonomics/log/erlang.log.1
```

### Traces Not Sampled

**Problem**: Distributed traces not appearing

**Solution**:
```bash
# Increase sample rate for testing
export TRACING_SAMPLE_RATE=1.0

# Verify OTLP endpoint is reachable
curl http://localhost:4317

# Check trace configuration
application:get_env(tai_autonomics, tracing).
```

---

## References

- Prometheus Documentation: https://prometheus.io/docs
- OpenTelemetry Documentation: https://opentelemetry.io/docs
- GCP Cloud Monitoring: https://cloud.google.com/monitoring/docs
- Grafana Documentation: https://grafana.com/docs
- Alert Rules: monitoring/alerts.yml
