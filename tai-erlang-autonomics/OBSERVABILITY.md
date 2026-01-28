# Observability & Monitoring Guide - TAI Erlang Autonomics

## Overview

TAI Erlang Autonomics provides comprehensive observability through Google Cloud Monitoring, Cloud Logging, Cloud Trace, and Prometheus metrics export.

The system emits:
- **Structured JSON logs** to Cloud Logging (BigQuery)
- **Custom metrics** to Cloud Monitoring
- **Distributed traces** to Cloud Trace (via OpenTelemetry)
- **Prometheus metrics** for Kubernetes monitoring

---

## 1. Cloud Monitoring Dashboards

### Main Operations Dashboard
The primary dashboard shows key service health metrics in real-time.

**URL**: [Cloud Monitoring Dashboard](https://console.cloud.google.com/monitoring/dashboards)

**Key Metrics**:
- **Error Rate (%)**: Current error percentage. Alert triggers at > 1%
- **P99 Latency (ms)**: 99th percentile request latency. Alert triggers at > 500ms
- **Request Rate (req/s)**: Throughput in requests per second
- **Memory Usage (GB)**: Heap and process memory consumption
- **Governor State Transitions**: Real-time state changes per governor
- **Receipts Generated**: Acceptance and refusal counts

### Creating Custom Dashboards

```bash
# Via gcloud CLI
gcloud monitoring dashboards create --config-from-file=dashboard.json

# Via Terraform
terraform apply -target=google_monitoring_dashboard.tai_autonomics
```

---

## 2. Cloud Logging

All logs are sent to Google Cloud Logging as structured JSON for machine parsing.

### Viewing Logs

**Console**: [Cloud Logging](https://console.cloud.google.com/logs)

### Basic Queries

#### View all errors
```sql
resource.type="cloud_run_revision"
resource.labels.service_name="tai-autonomics"
severity="ERROR"
```

#### View all warnings and errors
```sql
resource.type="cloud_run_revision"
resource.labels.service_name="tai-autonomics"
(severity="ERROR" OR severity="WARNING")
```

#### View governance events
```sql
resource.type="cloud_run_revision"
resource.labels.service_name="tai-autonomics"
jsonPayload.event_type="governor_transition"
```

#### View refusals
```sql
resource.type="cloud_run_revision"
resource.labels.service_name="tai-autonomics"
jsonPayload.event_type="refusal"
```

### Structured Log Format

All logs contain:
```json
{
  "timestamp": "2026-01-26T15:30:45.123456Z",
  "severity": "INFO|WARNING|ERROR|CRITICAL",
  "message": "Human-readable message",
  "jsonPayload": {
    "event_type": "receipt_generated",
    "receipt_type": "acceptance",
    "reason": "policy_check_passed",
    "details": {}
  },
  "labels": {
    "audit": "true",
    "event_type": "receipt"
  },
  "resource": {
    "type": "cloud_run_revision",
    "labels": {
      "project_id": "PROJECT_ID",
      "service_name": "tai-autonomics",
      "revision_name": "REVISION",
      "region": "us-central1"
    }
  },
  "traceId": "TRACE_ID",
  "spanId": "SPAN_ID"
}
```

### BigQuery Log Analysis

Logs are exported to BigQuery for long-term analysis and reporting.

**Dataset**: `tai_autonomics_logs`
**Table**: `application_logs`

#### Queries

**Error rate over time**:
```sql
SELECT
  TIMESTAMP_TRUNC(timestamp, HOUR) as hour,
  COUNT(*) as total_logs,
  COUNTIF(severity="ERROR") as error_count,
  ROUND(100.0 * COUNTIF(severity="ERROR") / COUNT(*), 2) as error_rate_percent
FROM `PROJECT_ID.tai_autonomics_logs.application_logs`
WHERE severity IN ("ERROR", "INFO")
GROUP BY hour
ORDER BY hour DESC
LIMIT 24
```

**Governor state transitions by hour**:
```sql
SELECT
  TIMESTAMP_TRUNC(timestamp, HOUR) as hour,
  json_extract(jsonPayload, '$.governor') as governor,
  json_extract(jsonPayload, '$.to_state') as to_state,
  COUNT(*) as transition_count
FROM `PROJECT_ID.tai_autonomics_logs.application_logs`
WHERE json_extract(jsonPayload, '$.event_type') = 'governor_transition'
GROUP BY hour, governor, to_state
ORDER BY hour DESC, transition_count DESC
```

**Refusal analysis**:
```sql
SELECT
  json_extract(jsonPayload, '$.reason') as refusal_reason,
  COUNT(*) as count,
  ROUND(100.0 * COUNT(*) / SUM(COUNT(*)) OVER(), 2) as percent
FROM `PROJECT_ID.tai_autonomics_logs.application_logs`
WHERE json_extract(jsonPayload, '$.event_type') = 'refusal'
GROUP BY refusal_reason
ORDER BY count DESC
```

---

## 3. Cloud Trace (Distributed Tracing)

OpenTelemetry integration provides distributed tracing for request flows.

### Viewing Traces

**Console**: [Cloud Trace](https://console.cloud.google.com/traces)

### Trace Structure

Each request generates spans for:
1. **HTTP Request** - Entry point
2. **Governor Processing** - Policy evaluation
3. **State Transitions** - Governor state changes
4. **Receipt Generation** - Acceptance/refusal decision

### Sample Trace Analysis

**Query**: View slow requests
```
latency:>500ms
status:error
```

**Query**: View all requests to /health endpoint
```
span.http.url:/health
```

### Trace Sampling

By default, 10% of requests are traced (configurable).

**Enable all traces** (dev only):
```bash
export TRACE_SAMPLING_RATE=1.0
```

**Disable tracing**:
```bash
export TRACE_SAMPLING_RATE=0.0
```

### BigQuery Trace Export

Spans are exported to BigQuery for analysis.

**Dataset**: `tai_autonomics_traces`
**Table**: `spans`

**Query**: Find slowest operations
```sql
SELECT
  operation_name,
  COUNT(*) as count,
  ROUND(AVG(duration_ms), 2) as avg_duration_ms,
  MAX(duration_ms) as max_duration_ms
FROM `PROJECT_ID.tai_autonomics_traces.spans`
WHERE start_time > TIMESTAMP_SUB(CURRENT_TIMESTAMP(), INTERVAL 24 HOUR)
GROUP BY operation_name
ORDER BY avg_duration_ms DESC
```

---

## 4. Custom Metrics

### Available Custom Metrics

All metrics are available in Cloud Monitoring under `custom.googleapis.com/tai_autonomics/`

| Metric | Type | Labels | Description |
|--------|------|--------|-------------|
| `request_rate_per_second` | GAUGE | endpoint, method, status_code | HTTP requests/sec |
| `error_rate_percent` | GAUGE | error_type, governor | Errors as percentage |
| `latency_ms_p99` | DISTRIBUTION | endpoint, operation | 99th percentile latency |
| `memory_bytes` | GAUGE | node, type | Memory usage |
| `governor_state_transitions` | CUMULATIVE | governor, from_state, to_state | State change count |
| `receipts_total` | CUMULATIVE | receipt_type, reason | Total receipts issued |

### Prometheus Scraping

Metrics are exported in Prometheus format on port 8888.

**Scrape endpoint**: `http://localhost:8888/metrics`

**Configuration**:
```yaml
scrape_configs:
  - job_name: 'tai-autonomics'
    static_configs:
      - targets: ['localhost:8888']
    scrape_interval: 10s
```

---

## 5. Alert Policies

### Error Rate Alert
- **Condition**: Error rate > 1%
- **Duration**: 5 minutes
- **Action**: Notify on-call engineer

### Latency Alert
- **Condition**: P99 latency > 500ms
- **Duration**: 5 minutes (sustained)
- **Action**: Notify for investigation

### CPU Alert
- **Condition**: CPU utilization > 70%
- **Duration**: 5 minutes
- **Action**: Consider scaling

### Memory Alert
- **Condition**: Memory utilization > 80%
- **Duration**: 5 minutes
- **Action**: Check for leaks

### Service Unavailable Alert
- **Condition**: 5xx errors > 5/min
- **Duration**: 1 minute
- **Action**: Page on-call

### No Traffic Alert
- **Condition**: Zero requests for 5 minutes
- **Duration**: 5 minutes
- **Action**: Check connectivity

### Error Budget Alerts
- **50% Consumed**: Burn rate alert
- **100% Consumed**: SLO breach - immediate escalation to PagerDuty

---

## 6. SLO Definition

### Uptime SLO: 99.5%
- **Calculation**: Successful responses / total responses
- **Threshold**: 99.5% availability
- **Monthly budget**: 3.6 hours downtime
- **Alert**: Error budget 100% consumed

### Success Rate SLO: 99%
- **Calculation**: 2xx responses / total responses
- **Threshold**: 99% success
- **Alert**: Success rate falls below 99%

### Latency SLO: P99 < 500ms
- **Calculation**: Requests with latency < 500ms
- **Threshold**: 95% of requests
- **Alert**: P99 latency exceeds 500ms

### Monitoring SLOs

**Console**: [SLO Details](https://console.cloud.google.com/monitoring/slos)

**Query**: SLO compliance
```bash
gcloud monitoring slos list --filter="displayName:'TAI Autonomics'"
```

---

## 7. Performance Baselines

### Expected Metrics

| Metric | Baseline | Target | Alert Threshold |
|--------|----------|--------|-----------------|
| Error Rate | < 0.1% | < 0.5% | > 1% |
| P50 Latency | 50ms | < 100ms | - |
| P99 Latency | 250ms | < 500ms | > 500ms |
| Request Rate | 100 req/s | 1000 req/s | - |
| Memory/Instance | 512MB | 1GB | > 1.6GB (80%) |
| CPU/Instance | 10% | 30% | > 70% |
| Refusal Rate | < 1% | < 2% | > 5% |

### Performance Testing

```bash
# Load test with Apache Bench
ab -n 10000 -c 100 https://tai-autonomics.run.app/health

# Monitor live metrics
watch -n 1 'gcloud monitoring time-series list --filter="metric.type:custom.googleapis.com/tai_autonomics/*"'
```

---

## 8. Operational Runbooks

### Alert Response

#### High Error Rate
1. Check Cloud Logs for error patterns
2. Review recent code deployments
3. Check downstream dependencies (Firestore, Pub/Sub)
4. If critical: trigger rollback

#### High Latency
1. Check CPU and memory utilization
2. Review database query performance
3. Check Cloud Trace for slow operations
4. Scale up instances if needed

#### Out of Memory
1. Check memory graphs in dashboard
2. Look for memory leak patterns
3. Review process memory in logs
4. Restart service and apply fix

#### Service Unavailable
1. Check Cloud Run revision health
2. Review startup logs
3. Check resource quotas
4. Verify IAM permissions

---

## 9. Logging Best Practices

### Structured Logging

Use the `structured_logger` module for all logging:

```erlang
% Info log
structured_logger:info(<<"request_processed">>, #{
    endpoint => <<"/api/v1/action">>,
    status => 200,
    duration_ms => 145
}),

% Error with trace context
structured_logger:error(
    <<"operation_failed">>,
    #{reason => invalid_state, attempts => 3},
    #{trace_id => TraceId, span_id => SpanId}
),

% Audit log
structured_logger:audit(entitlement_change, <<"policy_updated">>, #{
    customer_id => CustomerId,
    old_policy => OldPolicy,
    new_policy => NewPolicy
}),

% Refusal
structured_logger:emit_refusal(
    rate_limit_exceeded,
    <<"Too many requests from IP">>,
    #{ip => <<"192.168.1.1">>, limit => 100}
)
```

### Metric Recording

```erlang
% Record receipt
structured_logger:emit_receipt(acceptance, #{
    receipt_id => ReceiptId,
    policy => PolicyName,
    decision_time_ms => DecisionTime
}),

% Via OpenTelemetry
otel_instrumentation:record_governor_transition(
    billing_governor,
    accepting,
    throttling,
    <<"quota_exceeded">>
),

otel_instrumentation:record_http_request(
    <<"POST">>,
    <<"/api/v1/check">>,
    200,
    145
)
```

---

## 10. Integration with External Systems

### PagerDuty

Alerts automatically create incidents:
```hcl
resource "google_monitoring_notification_channel" "pagerduty" {
  type   = "pagerduty"
  labels = { service_key = var.pagerduty_service_key }
}
```

### Slack

Real-time notifications in #tai-autonomics-alerts:
```hcl
resource "google_monitoring_notification_channel" "slack" {
  type   = "slack"
  labels = { channel_name = "#tai-autonomics-alerts" }
}
```

### BigQuery

Daily exports for analysis:
```bash
# Query daily error trends
bq query --use_legacy_sql=false '
SELECT
  DATE(timestamp) as date,
  COUNT(*) as total,
  COUNTIF(severity="ERROR") as errors
FROM `PROJECT.tai_autonomics_logs.application_logs`
GROUP BY date
ORDER BY date DESC
'
```

---

## 11. Troubleshooting

### Logs Not Appearing

1. Check Cloud Run service logs
2. Verify service account has logging permissions
3. Check `GOOGLE_CLOUD_PROJECT` environment variable
4. Review structured_logger error handling

### Metrics Not Showing

1. Verify metrics_collector is running
2. Check custom metric registration
3. Confirm service account has monitoring.metricWriter role
4. Review Prometheus endpoint on port 8888

### Traces Not Visible

1. Check trace sampling rate (default 10%)
2. Verify OpenTelemetry SDK initialization
3. Confirm OTEL_EXPORTER_OTLP_ENDPOINT is set
4. Review trace processor batch settings

### Dashboard Empty

1. Check dashboard time range
2. Verify filters are correct
3. Confirm metrics exist in Cloud Monitoring
4. Check resource type and labels

---

## 12. Documentation

- **Terraform Configs**: `/tai-erlang-autonomics/terraform/monitoring.tf`, `alerting.tf`
- **OTEL Config**: `/tai-erlang-autonomics/tools/otel-config.yaml`
- **Logger Module**: `/apps/tai_autonomics/src/structured_logger.erl`
- **Metrics Module**: `/apps/tai_autonomics/src/metrics_collector.erl`
- **Tracing Module**: `/apps/tai_autonomics/src/otel_instrumentation.erl`

---

## References

- [Google Cloud Monitoring](https://cloud.google.com/monitoring/docs)
- [Google Cloud Logging](https://cloud.google.com/logging/docs)
- [Google Cloud Trace](https://cloud.google.com/trace/docs)
- [OpenTelemetry Erlang](https://opentelemetry.io/docs/instrumentation/erlang/)
- [Prometheus Best Practices](https://prometheus.io/docs/practices/instrumentation/)
