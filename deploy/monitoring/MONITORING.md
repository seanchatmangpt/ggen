# ggen Production Monitoring Stack

**Version:** 1.0.0 | **Last Updated:** 2026-03-24 | **Owner:** Platform Team

---

## Overview

Comprehensive production monitoring for ggen with four critical alert scenarios, real-time dashboards, and on-call runbooks. The stack tracks supervisor health, circuit breaker states, consensus quorum, and event store performance.

### Stack Components

| Component | Purpose | Port | Interval |
|-----------|---------|------|----------|
| **Prometheus** | Metrics aggregation & query engine | 9090 | 10s scrape |
| **Alertmanager** | Alert routing & escalation | 9093 | Event-driven |
| **Grafana** | Visualization & dashboards | 3000 | Real-time |
| **Health Dashboard** | Application metrics endpoint | 8080 | 10s push |
| **Supervisor Metrics** | Process supervisor monitoring | 9001 | 10s push |
| **Circuit Breaker** | Resilience layer metrics | 9002 | 10s push |
| **Consensus Metrics** | Distributed coordination health | 9003 | 10s push |
| **Event Store** | Persistence layer performance | 9004 | 10s push |

---

## Metrics Explained

### 1. Supervisor Restart Rate
**Metric:** `rate(supervisor_restart_count[1m])`
**Unit:** restarts per minute
**Threshold:** 5/min (alert if exceeded for 1 minute)

#### What It Measures
- Frequency of supervisor process restarts
- Indicates application stability and crash patterns
- High rate = systemic instability

#### Interpretation

| Rate | Status | Action |
|------|--------|--------|
| < 0.1/min | Excellent | Healthy - continue monitoring |
| 0.1 - 0.5/min | Good | Normal variance, no action needed |
| 0.5 - 2/min | Warning | Degraded, investigate logs |
| 2 - 5/min | Critical | Immediate action required |
| > 5/min | CRITICAL | **PAGE ON-CALL - Stop the line** |

#### Alert: `SupervisorHighRestartRate`
```
Condition: rate(supervisor_restart_count[1m]) > 5 for 1 minute
Severity: CRITICAL
Action: Immediately page on-call team
```

**Troubleshooting Steps:**
1. Check supervisor logs for crash stack traces:
   ```bash
   journalctl -u ggen-supervisor -n 100 --no-pager
   tail -f /var/log/ggen/supervisor.log
   ```
2. Check system resources:
   ```bash
   free -h          # Memory
   df -h            # Disk
   top -b -n 1      # CPU
   ```
3. Review recent deployments for regressions
4. Check for dependency availability (databases, external services)
5. If recurring pattern: implement rate limiting or exponential backoff

**Dashboard:** [Supervisor Health](https://grafana.internal/d/supervisor-health)
**Runbook:** [Supervisor Restart Spike](https://ggen-docs.internal/runbooks/supervisor-restart-spike)

---

### 2. Circuit Breaker State
**Metric:** `circuit_breaker_state`
**States:** 0=CLOSED, 1=OPEN, 2=HALF_OPEN, 3=SLOW_START
**Threshold:** Should not be OPEN for > 30 seconds

#### What It Measures
- Current state of circuit breaker for a protected service
- Indicates downstream service health
- Tracks failure-recovery cycle

#### State Machine

```
CLOSED (healthy)
    ↓ [failure threshold exceeded]
OPEN (service unavailable)
    ↓ [timeout_duration elapsed]
HALF_OPEN (testing recovery)
    ↓ [success_threshold met]
CLOSED or [failures continue]
    ↓
SLOW_START (gradual traffic increase)
    ↓ [ramp-up complete]
CLOSED
```

#### Interpretation

| State | Meaning | Status | Action |
|-------|---------|--------|--------|
| **CLOSED (0)** | Service healthy, requests pass | ✅ Normal | Monitor |
| **OPEN (1)** | Service unhealthy, circuit open | 🔴 Critical | Investigate downstream |
| **HALF_OPEN (2)** | Testing recovery, limited traffic | 🟡 Recovering | Wait/assist recovery |
| **SLOW_START (3)** | Gradual ramp-up after recovery | 🟠 Recovering | Monitor success rate |

#### Alert: `CircuitBreakerOpen`
```
Condition: circuit_breaker_state == 1 for 30 seconds
Severity: WARNING
Action: Warn platform team
```

**Troubleshooting Steps:**
1. Identify the protected service:
   ```promql
   circuit_breaker_state{service="X"}
   ```
2. Check downstream service health:
   ```bash
   curl -i http://<downstream-service>:8080/health
   ```
3. Review circuit breaker configuration:
   ```bash
   cat /etc/ggen/circuit-breaker-config.yml | grep -A 10 "service: X"
   ```
4. If false alarm, consider adjusting thresholds:
   - Increase `failure_threshold` if transient issues
   - Increase `request_timeout` if slow but working
5. If persistent, implement downstream scaling/fixes

**Dashboard:** [Resilience Layer](https://grafana.internal/d/resilience-layer)
**Runbook:** [Circuit Breaker Open](https://ggen-docs.internal/runbooks/circuit-breaker-open)

---

### 3. Consensus Quorum Health
**Metric:** `(count healthy nodes) / (total nodes)`
**Threshold:** Must remain ≥ 2/3 (66.7%)
**Alert Threshold:** < 66.7% for 2 minutes

#### What It Measures
- Ratio of healthy consensus nodes to total nodes
- Indicates ability to make distributed decisions
- Critical for data consistency and availability

#### Interpretation

| Ratio | Status | Impact | Action |
|-------|--------|--------|--------|
| **100%** | Optimal | Full availability | ✅ Healthy - continue |
| **80-99%** | Good | 1 node degraded | Monitor |
| **66.7-79%** | Acceptable | 2/3 quorum just met | 🟡 Watch closely |
| **50-66.6%** | Lost | Cannot make decisions | 🔴 Page on-call |
| **< 50%** | Failed | Consensus impossible | 🚨 Declare incident |

#### Alert: `ConsensusQuorumLost`
```
Condition: (healthy_nodes / total_nodes) < 0.6666 for 2 minutes
Severity: CRITICAL
Action: Declare incident, page entire on-call team
```

**Why This Matters:**
- Consensus algorithm requires majority to make decisions
- Without quorum: cannot assign new work, no progress
- System grinds to halt → **direct customer impact**
- Estimated cost: $X per minute of downtime

**Troubleshooting Steps:**
1. List all consensus nodes and their status:
   ```bash
   ggen consensus status --all
   # or
   curl http://localhost:9003/consensus/nodes | jq
   ```
2. For each unhealthy node, check:
   ```bash
   # Network connectivity
   ping <node-ip>

   # Service running
   systemctl status ggen-consensus@<node-id>

   # Logs
   journalctl -u ggen-consensus@<node-id> -n 50
   ```
3. For network partition:
   ```bash
   # Check network interfaces and routes
   ip link show
   ip route show

   # Check firewall rules for consensus port (default 50051)
   ufw status | grep 50051
   ```
4. Recovery procedure:
   - If single node down: restart it
   - If multiple nodes down: follow disaster recovery runbook
   - If network partition: contact infrastructure team

**Dashboard:** [Consensus Health](https://grafana.internal/d/consensus-health)
**Runbook:** [Consensus Quorum Loss](https://ggen-docs.internal/runbooks/consensus-quorum-loss)
**Disaster Recovery:** [Consensus Recovery](https://ggen-docs.internal/runbooks/consensus-recovery)

---

### 4. Event Store Latency
**Metric:** `histogram_quantile(0.99, rate(event_store_operation_duration_seconds_bucket[5m]))`
**Unit:** seconds
**Threshold:** p99 < 1.0 second
**Alert Threshold:** > 1.0s for 5 minutes

#### What It Measures
- 99th percentile latency of event store operations
- Indicates persistence layer performance
- Reflects database query speed and disk I/O

#### Interpretation

| p99 Latency | p95 Latency | p50 Latency | Status | Action |
|-------------|-------------|-------------|--------|--------|
| < 100ms | < 50ms | < 20ms | ✅ Excellent | Monitor |
| 100-250ms | 50-150ms | 20-50ms | ✅ Good | Monitor |
| 250-500ms | 150-300ms | 50-100ms | 🟡 Degraded | Investigate |
| 500ms-1s | 300-600ms | 100-200ms | 🟠 Warning | Take action |
| **> 1s** | > 600ms | > 200ms | 🔴 Critical | **Alert** |

#### Alert: `EventStoreHighLatency`
```
Condition: p99 latency > 1.0s for 5 minutes
Severity: WARNING
Action: Warn platform team
```

**Why This Matters:**
- Slow event store = slow event sourcing pipeline
- May cascade to user-facing latency
- Indicates resource saturation or query inefficiency

**Common Causes & Solutions:**

| Cause | Symptom | Solution |
|-------|---------|----------|
| High query load | p99 >> p95 | Reduce queries, add caching |
| Long transactions | All percentiles high | Review transaction scope |
| Slow disk I/O | Consistent high latency | Check iostat, upgrade storage |
| Connection pool exhaustion | Intermittent spikes | Increase pool size |
| Missing indexes | Specific query types slow | Add database indexes |
| Table locks | p99 spikes periodically | Review schema changes |

**Troubleshooting Steps:**
1. Check database connection pool:
   ```bash
   mysql -u ggen -p -e "SHOW PROCESSLIST;" | wc -l
   ```
2. Review slow query log:
   ```bash
   tail -f /var/log/mysql/slow.log
   ```
3. Check disk I/O:
   ```bash
   iostat -x 1 5
   # Look for: %util, await
   ```
4. Analyze problematic queries:
   ```bash
   EXPLAIN SELECT ... FROM event_store WHERE ...;
   ```
5. If misconfigurations persist:
   - Horizontal scaling: add read replicas
   - Vertical scaling: more CPU/RAM for database
   - Query optimization: batching, indexing
   - Event batching: reduce write frequency

**Dashboard:** [Event Store Performance](https://grafana.internal/d/event-store-performance)
**Runbook:** [Event Store Latency](https://ggen-docs.internal/runbooks/event-store-latency)

---

## Dashboard Guide

### Dashboard Panels

**Row 1: Status Indicators (Real-Time Gauges)**
1. **Consensus Quorum Health** - Single number, green/yellow/red
2. **Circuit Breaker State** - State text with color mapping
3. **Supervisor Restarts/min** - Current 1-minute rate
4. **Event Store p99 Latency** - Current p99 in milliseconds

**Row 2: Time Series Graphs**
1. **Supervisor Restart Rate** - 1-hour history with max/mean/last stats
2. **Circuit Breaker State Timeline** - State changes over time
3. **Consensus Quorum Ratio** - Quorum trend with 2/3 threshold line
4. **Event Store Latency Percentiles** - p50/p95/p99 stacked

**Row 3: Alerts Table**
- Active alerts with severity colors
- Instance and alert details
- Sortable by time, severity

### Using the Dashboard

**Quick Health Check (2 minutes):**
1. Open dashboard: https://grafana.internal/d/ggen-prod-monitoring
2. Scan top row gauges for red/yellow indicators
3. If any red: check Active Alerts table
4. Click alert to open runbook link

**Detailed Investigation (5-15 minutes):**
1. Click on metric panel title to drill down
2. Adjust time range: top-right dropdown (now-1h to now-7d)
3. Hover over graphs for exact values
4. Click legend items to toggle series on/off
5. Use `Compare` button to compare time periods

**Setting Up Persistent Access:**
```bash
# Add dashboard to favorites
# Share as JSON via "Share" button
# Create custom alerts with "Grafana Alerts"

# Export to file for version control
curl -H "Authorization: Bearer $GRAFANA_API_TOKEN" \
  http://grafana.internal/api/dashboards/uid/ggen-prod-monitoring \
  > grafana-dashboard.json
```

---

## Alert Rules Explained

### Alert Severities

| Severity | Meaning | Action | SLA |
|----------|---------|--------|-----|
| **critical** | System failure, immediate action | Page on-call + declare incident | 5 min response |
| **warning** | Degradation, investigation needed | Alert platform team | 15 min response |
| **info** | Informational, trending data | Log + monitor | No action required |

### Alert Lifecycle

```
1. Condition triggered
     ↓
2. Threshold duration met (e.g., 1m for SupervisorHighRestartRate)
     ↓
3. Alert fires → Alertmanager receives
     ↓
4. Routing rules applied (to Slack, PagerDuty, email)
     ↓
5. On-call notified
     ↓
6. Runbook consulted, investigation begins
     ↓
7. Resolution → Condition clears
     ↓
8. Alert resolves (fires 'alert name' = 0)
     ↓
9. Notifications sent for resolution
```

### Custom Alert Rules

To add new alerts, edit `alerts.yml`:

```yaml
- alert: YourAlertName
  expr: your_metric_expr
  for: 1m                  # Duration before alerting
  labels:
    severity: warning
    team: platform
  annotations:
    summary: "Short description"
    description: |
      Long form explanation.
      - Action 1
      - Action 2
```

Reload without restart:
```bash
curl -X POST http://prometheus:9090/-/reload
```

---

## Silencing Alerts (Testing Mode)

### During Deployment/Testing

**Method 1: Alertmanager UI (Quick)**
1. Open https://alertmanager.internal
2. Click "Silence" button on alert row
3. Set duration (e.g., 30m, 2h)
4. Add comment: "Deployment testing - feature X"
5. Confirm

**Method 2: API (Programmatic)**
```bash
curl -X POST http://alertmanager:9093/api/v1/silences \
  -H "Content-Type: application/json" \
  -d '{
    "matchers": [
      {"name": "alertname", "value": "SupervisorHighRestartRate", "isRegex": false}
    ],
    "startsAt": "'$(date -u +'%Y-%m-%dT%H:%M:%S.000Z')'",
    "endsAt": "'$(date -u -d '+2 hours' +'%Y-%m-%dT%H:%M:%S.000Z')'",
    "createdBy": "deployment-bot",
    "comment": "Supervisor upgrades - expected restarts"
  }'
```

**Method 3: Prometheus Config (Permanent)**
```yaml
# prometheus.yml - Disable specific rules
rule_files:
  - '/etc/prometheus/alerts.yml'

# Or, modify alerts.yml to disable:
- alert: SupervisorHighRestartRate
  if: false  # Disabled
```

**Best Practices:**
- Always set explicit end time (not infinite)
- Document reason in comment
- Remove after testing is complete
- Monitor dashboards even while silenced

---

## On-Call Runbook Reference

### Quick Runbook Links

1. **Supervisor Restart Spike**
   - Symptom: `SupervisorHighRestartRate` fired
   - Link: https://ggen-docs.internal/runbooks/supervisor-restart-spike
   - Time to resolve: 5-30 minutes

2. **Circuit Breaker Open**
   - Symptom: `CircuitBreakerOpen` fired
   - Link: https://ggen-docs.internal/runbooks/circuit-breaker-open
   - Time to resolve: 10-60 minutes

3. **Consensus Quorum Loss**
   - Symptom: `ConsensusQuorumLost` fired
   - Link: https://ggen-docs.internal/runbooks/consensus-quorum-loss
   - Time to resolve: 15-120 minutes (critical incident)

4. **Event Store Latency**
   - Symptom: `EventStoreHighLatency` fired
   - Link: https://ggen-docs.internal/runbooks/event-store-latency
   - Time to resolve: 10-45 minutes

### Emergency Contacts

| Role | Contact | Backup | Escalation |
|------|---------|--------|-----------|
| On-Call Engineer | PagerDuty | Slack #ggen-incidents | Engineering Lead |
| Database Admin | ops-db-team | | CTO |
| Infrastructure | ops-infra | | VP Engineering |

---

## SLO Validation

### Service Level Objectives

| Metric | Target | Alert | Validation |
|--------|--------|-------|-----------|
| Supervisor stability | < 0.1 restarts/min | > 0.1 for 5m | Monthly report |
| Consensus availability | > 99.9% | < 99% for 2m | Automated check |
| Event store p99 latency | < 1.0s | > 1.0s for 5m | SLI tracking |
| Circuit breaker recovery | < 2 minutes | > 2m open | Per-incident review |

**Check SLO status:**
```bash
# All SLOs
curl http://prometheus:9090/api/v1/query?query=slo_status

# Specific SLO
curl http://prometheus:9090/api/v1/query?query='slo_status{slo="event_store_latency"}'
```

---

## Troubleshooting the Monitoring Stack

### Issue: Metrics Not Appearing

**Symptom:** Grafana shows "No data"

**Troubleshooting:**
```bash
# 1. Verify Prometheus is scraping targets
curl http://localhost:9090/api/v1/targets

# 2. Check scrape job status
curl http://localhost:9090/api/v1/targets | jq '.data.activeTargets[] | select(.job == "health-dashboard")'

# 3. Query prometheus directly
curl 'http://localhost:9090/api/v1/query?query=up'

# 4. Check application metrics endpoint
curl http://localhost:8080/api/v1/metrics | head -20

# 5. Verify network connectivity
ping <target-host>
telnet <target-host> 8080
```

### Issue: Alerts Not Firing

**Symptom:** Alert condition met but no notification

**Troubleshooting:**
```bash
# 1. Check alert rule evaluation
curl 'http://localhost:9090/api/v1/query?query=ALERTS'

# 2. Verify Alertmanager is running
curl http://localhost:9093/api/v1/alerts

# 3. Check Alertmanager configuration
curl http://localhost:9093/api/v1/status

# 4. Test notification channel
curl -X POST http://alertmanager:9093/api/v1/alerts \
  -H "Content-Type: application/json" \
  -d '[{"labels": {"alertname": "TestAlert"}}]'

# 5. Review notification logs
journalctl -u prometheus-alertmanager -n 50
```

### Issue: High Prometheus Memory Usage

**Troubleshooting:**
```bash
# 1. Check number of time series
curl 'http://localhost:9090/api/v1/query?query=prometheus_tsdb_symbol_table_size_bytes'

# 2. Reduce scrape frequency
# In prometheus.yml: change scrape_interval from 10s to 30s

# 3. Remove low-value targets
# In prometheus.yml: comment out unused job_names

# 4. Increase retention period cleanup
# Add to prometheus.yml:
# storage:
#   retention:
#     size: "2GB"
```

---

## Maintenance

### Daily
- Check dashboard for any yellow/red indicators
- Review alert patterns in Slack #ggen-alerts
- Spot check top metrics

### Weekly
- Review alert firing frequency
- Identify recurring false positives
- Adjust thresholds if needed
- Check Prometheus storage usage

### Monthly
- Review SLO compliance report
- Update runbooks based on incidents
- Capacity planning for metrics storage
- Rotation of log retention policies

### Quarterly
- Full alerting rule audit
- Dashboard usability review
- On-call team training refresher
- Cost optimization review

---

## Configuration Files

### File Locations
```
/etc/prometheus/
  ├── prometheus.yml        (this repo: deploy/monitoring/prometheus.yml)
  └── alerts.yml            (this repo: deploy/monitoring/alerts.yml)

/etc/grafana/
  └── provisioning/
      └── dashboards/
          └── ggen-dashboard.json  (this repo: deploy/monitoring/grafana-dashboard.json)

/var/log/prometheus/
  └── prometheus.log

/var/log/alertmanager/
  └── alertmanager.log
```

### Deployment

**Docker Compose:**
```yaml
version: '3.8'
services:
  prometheus:
    image: prom/prometheus:latest
    volumes:
      - ./deploy/monitoring/prometheus.yml:/etc/prometheus/prometheus.yml
      - ./deploy/monitoring/alerts.yml:/etc/prometheus/alerts.yml
    ports:
      - "9090:9090"

  alertmanager:
    image: prom/alertmanager:latest
    ports:
      - "9093:9093"

  grafana:
    image: grafana/grafana:latest
    ports:
      - "3000:3000"
    environment:
      - GF_SECURITY_ADMIN_PASSWORD=changeme
```

---

## Further Reading

- [Prometheus Alerting Guide](https://prometheus.io/docs/alerting/latest/overview/)
- [SLO Best Practices](https://sre.google/sre-book/service-level-objectives/)
- [ggen Architecture Docs](https://ggen-docs.internal/architecture)
- [On-Call Handbook](https://ggen-docs.internal/on-call)
