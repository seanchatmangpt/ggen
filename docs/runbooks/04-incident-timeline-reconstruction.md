# Incident Timeline Reconstruction

**Version**: 2.0
**Last Updated**: 2026-01-25
**Status**: Production Ready

> **Core Principle**: Reconstruct the complete incident timeline from multiple data sources (logs, traces, metrics). Understand the sequence of events to identify root cause.

---

## Timeline Reconstruction Process

### Data Sources (Priority Order)

1. **Cloud Logging** (Structured logs with timestamps)
2. **Cloud Trace** (Distributed tracing, latency breakdown)
3. **Cloud Monitoring** (Metrics time series)
4. **Incident Ticket** (Comments and updates)
5. **Slack #incidents** (Manual updates from on-call)
6. **Kubernetes Events** (Pod/deployment changes)
7. **Git Commits** (Recent deployments)

---

## Step 1: Extract Logs

### Query Cloud Logging

```bash
# Export logs from incident window
gcloud logging read \
  --filter='timestamp >= "2026-01-25T14:15:00Z" AND timestamp <= "2026-01-25T14:45:00Z"' \
  --format=json \
  --limit=10000 \
  > incident-logs.json

# Parse logs into timeline
cat incident-logs.json | jq -r '.[] | "\(.timestamp) | \(.severity) | \(.labels.service) | \(.jsonPayload.message)"' \
  | sort > timeline-logs.txt

# Filter by service of interest
cat incident-logs.json | jq -r '.[] | select(.labels.service == "governor") | "\(.timestamp) | \(.jsonPayload.message)"' \
  | sort > timeline-governor.txt
```

### Key Log Events to Look For

**Alert Triggered**:
```json
{
  "timestamp": "2026-01-25T14:15:00Z",
  "severity": "ERROR",
  "service": "monitoring",
  "message": "Alert triggered: queue_depth > 1000",
  "value": 1245,
  "threshold": 1000
}
```

**Action Initiated**:
```json
{
  "timestamp": "2026-01-25T14:16:15Z",
  "severity": "INFO",
  "service": "governor",
  "message": "Scaling concurrency",
  "from": 20,
  "to": 50
}
```

**Error Occurred**:
```json
{
  "timestamp": "2026-01-25T14:18:32Z",
  "severity": "ERROR",
  "service": "action-handler",
  "message": "Timeout: downstream API unresponsive",
  "duration_ms": 500,
  "url": "https://api.upstream.com/v1/process"
}
```

---

## Step 2: Extract Traces

### Query Cloud Trace

```bash
# Export traces from incident window
gcloud trace list \
  --filter='timestamp >= "2026-01-25T14:15:00Z" AND timestamp <= "2026-01-25T14:45:00Z"' \
  --limit=1000 \
  > incident-traces.json

# Extract latency breakdown per request
cat incident-traces.json | jq -r '.[] | "\(.timestamp) | \(.spans[0].name) | \(.spans[0].duration_ms)ms"' \
  | sort > timeline-traces.txt

# Find slowest requests
cat incident-traces.json | jq -r '.[] | "\(.spans[0].duration_ms) | \(.spans[0].name)"' \
  | sort -rn \
  | head -20 > slowest-requests.txt
```

### Trace Event Filtering

**Find Requests with Latency Spike**:
```bash
cat incident-traces.json | jq -r '.[] | select(.spans[0].duration_ms > 1000) | .spans[0].name' | sort | uniq -c
```

**Output**:
```
123 /api/submit-action    (123 requests > 1s latency)
45  /api/check-status     (45 requests > 1s latency)
12  /api/get-config       (12 requests > 1s latency)
```

---

## Step 3: Extract Metrics

### Query Cloud Monitoring

```bash
# Export metrics timeseries from incident window
gcloud monitoring timeseries list \
  --filter='metric.type="custom.governor/queue_depth" AND resource.type="global"' \
  --interval-start-time=2026-01-25T14:15:00Z \
  --interval-end-time=2026-01-25T14:45:00Z \
  > incident-metrics.json

# Convert to CSV for analysis
cat incident-metrics.json | jq -r '.timeSeries[] | .points[] | "\(.interval.end_time), \(.value.double_value)"' \
  | sort > queue-depth-timeseries.csv
```

### Key Metrics to Extract

**Queue Depth**:
- 14:15:00 - 400 (baseline)
- 14:16:00 - 850 (trending up)
- 14:17:00 - 1,245 (exceeds threshold)
- 14:18:00 - 1,890 (continuing up)
- 14:19:00 - 2,150 (peak approaching)
- 14:28:00 - 3,847 (peak)
- 14:35:00 - 500 (back to baseline)

**Error Rate**:
- 14:15:00 - 0.3% (baseline)
- 14:17:00 - 1.2% (slight increase)
- 14:19:00 - 8.5% (spike)
- 14:23:00 - 15.2% (peak)
- 14:32:00 - 2.1% (recovering)
- 14:35:00 - 0.5% (back to normal)

---

## Step 4: Construct Timeline

### Unified Timeline Example

```
14:15:00 UTC - BASELINE STATE
  Queue Depth: 400 msgs
  Error Rate: 0.3%
  Latency P99: 120ms
  Action: All normal

14:15:45 UTC - FIRST SPIKE NOTICED (Log analysis)
  Log: "Pub/Sub lag detected: 2.3 seconds"
  Queue Depth: 600 msgs (trending up)
  On-call: Not yet alerted (threshold not met)

14:16:30 UTC - ALERT THRESHOLD APPROACHED
  Queue Depth: 900 msgs
  Error Rate: 0.8%
  Latency P99: 250ms
  Action: Still within threshold

14:17:15 UTC - ALERT TRIGGERED ⚠️
  Queue Depth: 1,245 msgs (> 1000 threshold)
  Event: "Alert fired: queue_depth > 1000"
  Monitoring: Alert sent to PagerDuty
  On-call: Paged (will acknowledge in ~30 sec)

14:17:20 UTC - AUTO-MITIGATION INITIATED
  Governor: Scaling concurrency 20 → 50
  Action Handler: Accepting more parallel requests
  Database: Connection pool 5 → 10
  Log: "Scaling factor: 2.5x"

14:18:00 UTC - ON-CALL ACKNOWLEDGES ALERT
  On-call: Woke up, read PagerDuty alert
  On-call: Viewing monitoring dashboard
  On-call: Identifies queue backlog scenario
  Action: Watching for auto-recovery

14:18:45 UTC - UNEXPECTED: SCALING NOT HELPING
  Queue Depth: 2,100 msgs (still growing despite 2.5x scaling)
  Error Rate: 5.2% (increasing errors)
  Latency P99: 1,200ms (significant slowdown)
  Log: "Action handler: Timeout connecting to upstream API"
  Trace: Requests timing out after 500ms

14:19:30 UTC - ROOT CAUSE IDENTIFIED
  Log: "Upstream API returns 503: Service Unavailable"
  Circuit Breaker: Opens (prevents cascading failures)
  On-call: Sees circuit opened, calls Tech Lead
  Action: Investigate upstream service health

14:20:15 UTC - TECH LEAD INVESTIGATING
  Tech Lead: SSHes to upstream service pod
  Tech Lead: Discovers pod memory pressure (OOM risk)
  Tech Lead: Pod is in CrashLoopBackOff state
  Action: Restart pod manually

14:20:45 UTC - UPSTREAM SERVICE RESTARTED
  Pod: Restarts successfully (fresh memory)
  Service: Health checks passing
  Circuit Breaker: Transitions to HALF_OPEN (test probes)
  Action: Monitor for recovery

14:21:30 UTC - HALF-OPEN PROBES SUCCEEDING
  Circuit Breaker: Receives 10 successful probes
  Circuit Breaker: State → CLOSED (service healthy)
  Governor: Accepts requests to upstream again
  Queue: Starting to drain (processing > ingestion)

14:25:00 UTC - QUEUE PEAK REACHED
  Queue Depth: 3,847 msgs (peak, now starting decline)
  Error Rate: 8.2% (peaked earlier, now declining)
  Processing Rate: 400 msgs/min (> 350 msgs/min ingestion rate)
  Action: Continue monitoring

14:32:00 UTC - SIGNIFICANT RECOVERY
  Queue Depth: 500 msgs (back to baseline)
  Error Rate: 0.6% (near baseline)
  Latency P99: 140ms (normal)
  Circuit Breaker: CLOSED and stable
  Upstream Service: Health checks passing

14:35:00 UTC - ALL CLEAR
  Queue Depth: 380 msgs (stable)
  Error Rate: 0.2% (baseline)
  Latency P99: 115ms (baseline)
  All systems: Green on dashboard
  On-call: Resolves incident

14:40:00 UTC - INCIDENT CLOSED
  Total Duration: 25 minutes
  MTTR (Time to resolution): 25 minutes
  Customer Impact: 2,847 delayed messages (all eventually processed)
  Root Cause: Upstream service OOM, needed restart
```

---

## Step 5: Visualization

### Timeline Diagram (ASCII)

```
14:15 ─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─ 14:40
      │         │         │         │         │         │         │         │         │
      Baseline  Spike   Alert!    Scaling   Service   Draining  Recovery   All-Clear Closed
      State     Noticed  Fired     Applied   Restarted  Begins    Underway
      │         │         │         │         │         │         │         │         │
      ├─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┤
      │                        INCIDENT WINDOW: 25 MINUTES                            │
      │                                                                               │
      │ Queue  │        ┌─────────────────────────────────┐                         │
      │ Depth  │        │                      Peak 3,847 │                         │
      │        │    ┌───┘                                 └──────────────┐           │
      │        │    │                                                    └──────┐    │
      │ Baseline 400│ 1,245 (alert)                                            400  │
      │        └────┴────────────────────────────────────────────────────────────────┘
      │
      │ Error  │                      ┌────────────────────┐                        │
      │ Rate   │                  ┌───┘ Peak 15%          └─────────────┐         │
      │        │  0.3%  ┌────────┘                                      └────┐   │
      │        │        │                                                    │ 0.2%
      │        └────────┴────────────────────────────────────────────────────┴───┘
```

---

## Step 6: Data Validation

### Sanity Checks

**Does the timeline make sense?**
- [ ] Events are in chronological order (no time reversals)
- [ ] Causality is clear (A happens, then B results, not reverse)
- [ ] Metrics align with events (alert threshold crossed matches log)
- [ ] Durations are reasonable (recovery shouldn't be faster than logs suggest)

**Are gaps filled in?**
- [ ] Alert time matches log + metric spike time?
- [ ] Scaling time matches Governor log entry?
- [ ] Recovery time matches circuit breaker state change?
- [ ] Total duration matches on-call ticket?

**Did we miss anything?**
- [ ] Check for errors in logs we haven't explained
- [ ] Check for unexplained metric spikes
- [ ] Check for deployment changes during incident
- [ ] Check for manual interventions we haven't documented

---

## Step 7: Root Cause Mapping

### Connect Events to Root Cause

```
Events                          Root Cause Chain
────────────────────────────────────────────────────────────

Queue Depth Spikes        ←──  Governor processing slower

Governor Processing Slower ←── Upstream API timing out

Upstream API Timeout      ←──  Action Handler service crashed

Action Handler Crashed    ←──  Memory pressure (OOM)

Memory Pressure           ←──  Memory leak in Action Handler code
                          OR
                          ←──  More concurrent requests than capacity
```

---

## Tools & Commands Reference

### Cloud Logging Commands

```bash
# Export logs with context
gcloud logging read --format=json --limit=10000 \
  --filter='timestamp>="2026-01-25T14:15:00Z" AND timestamp<="2026-01-25T14:45:00Z"' \
  > logs.json

# Search for specific event
gcloud logging read --format="table(timestamp,jsonPayload.service,jsonPayload.message)" \
  --filter='jsonPayload.message=~".*timeout.*"' \
  --limit=100

# Export to BigQuery for analysis
gcloud logging sinks create pir-export \
  bigquery.googleapis.com/projects/myproject/datasets/incident_logs \
  --log-filter='timestamp>="2026-01-25T14:15:00Z"'
```

### Prometheus Query

```
# Queue depth over time
rate(queue_depth[5m])

# Error rate per endpoint
rate(http_requests_total{status=~"5.."}[5m])

# Latency percentiles
histogram_quantile(0.99, http_request_duration_seconds_bucket)
```

### Datadog Query

```python
from datadog import initialize, api

# Query metrics
query = "avg:custom.queue_depth{*}"
results = api.Metric.query(
  start="2026-01-25T14:15:00Z",
  end="2026-01-25T14:45:00Z",
  query=query
)
```

---

## Reconstruction Output Checklist

**Before PIR, ensure you have**:
- [ ] Logs exported and parsed (start/end times)
- [ ] Traces extracted (request latencies, failures)
- [ ] Metrics timeseries extracted (queue depth, error rate, latency)
- [ ] Timeline constructed (minute-by-minute events)
- [ ] Timeline validated (causality, durations make sense)
- [ ] Root cause chain mapped (events to root cause)
- [ ] Visualization created (ASCII or diagram)
- [ ] Data gaps identified (missing information to collect)

**Typical Time to Reconstruct**: 30-45 minutes (for average incident)

---

## Related Documentation

- [Incident Timeline Reconstruction](#) (this document)
- [PIR Template](./03-pir-template.md)
- [Incident Runbooks](./02-incident-runbooks/)

---

**Status**: READY FOR PRODUCTION
**Last Updated**: 2026-01-25
**Owner**: Agent 10 (Incident Playbooks & Operational Runbooks)
