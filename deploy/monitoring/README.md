# ggen Production Monitoring Stack

Production-grade monitoring for ggen with real-time alerts, dashboards, and on-call runbooks.

## Files in This Directory

### 1. **prometheus.yml** (111 lines)
Prometheus scrape configuration with 10-second intervals for:
- Health Dashboard on :8080 (main application metrics)
- Supervisor Metrics on :9001 (process supervisor)
- Circuit Breaker Metrics on :9002 (resilience layer)
- Consensus Metrics on :9003 (distributed coordination)
- Event Store Metrics on :9004 (persistence layer)

**Key Features:**
- Multi-component scraping with distinct job names
- Per-service labels for dashboard filtering
- Relabeling rules for instance tracking
- Ready for remote storage configuration

### 2. **alerts.yml** (202 lines)
Prometheus alert rules with four critical scenarios:

#### Alert Rules
1. **SupervisorHighRestartRate** (CRITICAL)
   - Fires when: restart_count > 5/min for 1 minute
   - Action: Page on-call immediately
   - SLO: Recovery within 5 min

2. **CircuitBreakerOpen** (WARNING)
   - Fires when: circuit breaker state = Open for 30s
   - Action: Warn platform team
   - Indicates: Downstream service unavailability

3. **ConsensusQuorumLost** (CRITICAL)
   - Fires when: Healthy nodes < 66.7% for 2 minutes
   - Action: Declare incident + page all on-call
   - Impact: System cannot make distributed decisions

4. **EventStoreHighLatency** (WARNING)
   - Fires when: p99 latency > 1.0s for 5 minutes
   - Action: Warn platform team
   - SLO: p99 < 1.0s at all times

#### Recording Rules
- 1-minute restart rate
- Circuit breaker state snapshots
- Consensus quorum ratio
- Event store latency percentiles (p50/p95/p99)

#### SLO Validation Rules
- Auto-generated SLO alerts for compliance tracking

### 3. **grafana-dashboard.json** (925 lines)
Complete Grafana dashboard with 10 panels:

#### Row 1: Status Indicators (Gauges)
- Consensus Quorum Health (0-100%, red/yellow/green)
- Circuit Breaker State (CLOSED/OPEN/HALF-OPEN/SLOW-START)
- Supervisor Restarts/min (0-10, threshold at 5)
- Event Store p99 Latency (0-2s, threshold at 1s)

#### Row 2: Time Series Graphs
- Supervisor Restart Rate (1-hour rolling with stats)
- Circuit Breaker State Timeline (state changes)
- Consensus Quorum Ratio (with 2/3 threshold line)
- Event Store Latency Percentiles (p50/p95/p99 stacked)

#### Row 3: Active Alerts Table
- Real-time alert firing status
- Sortable by time, severity, instance
- Direct links to runbooks

**Features:**
- Auto-refresh every 10 seconds
- 1-hour default time range
- Color-coded thresholds
- Share-ready JSON (no credentials embedded)

### 4. **MONITORING.md** (646 lines)
Comprehensive operational guide covering:

#### Documentation Sections
1. **Overview** - Stack components and ports
2. **Metrics Explained** - Deep dive into each metric:
   - Supervisor Restart Rate (thresholds, troubleshooting)
   - Circuit Breaker State (state machine, interpretation)
   - Consensus Quorum Health (why it matters, recovery)
   - Event Store Latency (causes, solutions)
3. **Dashboard Guide** - How to use Grafana dashboard
4. **Alert Rules Explained** - Severity levels, lifecycle
5. **Silencing Alerts** - Testing mode procedures
6. **On-Call Runbook Reference** - Quick links
7. **SLO Validation** - Service level objectives
8. **Troubleshooting** - Common issues and fixes
9. **Maintenance** - Daily/weekly/monthly tasks
10. **Configuration Files** - Deployment instructions

#### Key Features
- Color-coded status indicators (red/yellow/green)
- Specific troubleshooting commands (curl, bash)
- Example queries and procedures
- SLA response times per severity
- Emergency contact procedures

---

## Quick Start

### 1. Deploy Prometheus
```bash
docker run -d \
  --name prometheus \
  -p 9090:9090 \
  -v $(pwd)/prometheus.yml:/etc/prometheus/prometheus.yml \
  -v $(pwd)/alerts.yml:/etc/prometheus/alerts.yml \
  prom/prometheus
```

### 2. Deploy Alertmanager
```bash
docker run -d \
  --name alertmanager \
  -p 9093:9093 \
  prom/alertmanager
```

### 3. Deploy Grafana
```bash
docker run -d \
  --name grafana \
  -p 3000:3000 \
  -e GF_SECURITY_ADMIN_PASSWORD=changeme \
  grafana/grafana
```

### 4. Load Dashboard
1. Open Grafana: http://localhost:3000
2. Create Prometheus data source (http://prometheus:9090)
3. Import dashboard: upload `grafana-dashboard.json`
4. Verify: Should show 4 gauges + time series graphs

### 5. Verify Metrics Flow
```bash
# Check Prometheus targets
curl http://localhost:9090/api/v1/targets

# Query a metric
curl 'http://localhost:9090/api/v1/query?query=up'

# List active alerts
curl http://localhost:9093/api/v1/alerts
```

---

## Architecture Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                    ggen Production Services                     │
├──────────────┬────────────────────┬───────────────┬──────────────┤
│ Health Dash  │   Supervisor       │ Circuit Break │ Consensus    │
│ :8080/metrics│   :9001/metrics    │ :9002/metrics │ :9003/metrics│
│              │                    │               │              │
│ - Throughput │ - Restarts/min     │ - State       │ - Quorum     │
│ - Errors     │ - Uptime           │ - Failures    │ - Nodes      │
│ - Latency    │ - Memory           │ - Recovery    │ - Consensus  │
└──────────────┴────────────────────┴───────────────┴──────────────┘
       ↓                ↓                  ↓                 ↓
┌─────────────────────────────────────────────────────────────────┐
│              Prometheus (9090) - Metric Aggregator             │
│  • Scrapes all endpoints every 10 seconds                      │
│  • Evaluates alert rules every 10 seconds                      │
│  • Stores 15 days of metrics (default retention)               │
├─────────────────────────────────────────────────────────────────┤
│ ┌─────────────────┐    ┌──────────────────┐                     │
│ │  Alert Rules    │    │ Recording Rules  │                     │
│ │ - SupervisorHR  │    │ - Restart Rate   │                     │
│ │ - CBOpen        │    │ - CB State       │                     │
│ │ - QuorumLost    │    │ - Quorum Ratio   │                     │
│ │ - StoreLatency  │    │ - P99 Latency    │                     │
│ └─────────────────┘    └──────────────────┘                     │
└──────────┬──────────────────────────────────┬────────────────────┘
           ↓                                   ↓
    ┌────────────────┐              ┌──────────────────┐
    │  Alertmanager  │              │     Grafana      │
    │    (9093)      │              │     (3000)       │
    ├────────────────┤              ├──────────────────┤
    │ Routes alerts: │              │ - Status gauges  │
    │ • Slack        │              │ - Time series    │
    │ • PagerDuty    │              │ - Alert table    │
    │ • Email        │              │ - Drill-down     │
    └────────────────┘              └──────────────────┘
           ↓                                   ↑
    ┌────────────────┐              Queries back to
    │ On-Call Team   │              Prometheus
    └────────────────┘
```

---

## Integration Checklist

- [ ] Deploy Prometheus from `prometheus.yml`
- [ ] Deploy Alertmanager with notification config
- [ ] Deploy Grafana from `grafana-dashboard.json`
- [ ] Verify all 5 target endpoints are scraping (9090/targets)
- [ ] Test alert firing (adjust metric threshold temporarily)
- [ ] Configure Slack/PagerDuty integration
- [ ] Set up on-call rotation
- [ ] Schedule team training on dashboard usage
- [ ] Document runbook URLs
- [ ] Test alert silencing procedure

---

## Support & Escalation

| Issue | Contact | Response Time |
|-------|---------|----------------|
| Dashboard not loading | Platform Team | 15 min |
| Alert not firing | SRE On-Call | 5 min |
| High latency alert | Database Team | 15 min |
| Quorum loss alert | Incident Commander | Immediate |
| Metrics missing | Prometheus Admin | 30 min |

---

**Version:** 1.0.0
**Created:** 2026-03-24
**Last Updated:** 2026-03-24
**Owner:** Platform Team
**Status:** Production Ready
