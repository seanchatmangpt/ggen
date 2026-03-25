# Production Deployment Guide

Quick reference for deploying ggen monitoring stack to production.

---

## Prerequisites

- Docker & Docker Compose installed
- 4GB RAM minimum for Prometheus + Alertmanager + Grafana
- Persistent volumes configured (for metrics retention)
- Network access to all target endpoints (:8080, :9001-9004)
- Slack/PagerDuty integration configured (optional but recommended)

---

## Docker Compose Setup

### 1. Create docker-compose.yml

```yaml
version: '3.8'

services:
  prometheus:
    image: prom/prometheus:v2.45.0
    container_name: ggen-prometheus
    ports:
      - "9090:9090"
    volumes:
      - ./prometheus.yml:/etc/prometheus/prometheus.yml:ro
      - ./alerts.yml:/etc/prometheus/alerts.yml:ro
      - prometheus_data:/prometheus
    command:
      - '--config.file=/etc/prometheus/prometheus.yml'
      - '--storage.tsdb.path=/prometheus'
      - '--storage.tsdb.retention.time=15d'
      - '--web.console.libraries=/etc/prometheus/console_libraries'
      - '--web.console.templates=/etc/prometheus/consoles'
      - '--web.enable-lifecycle'
    restart: unless-stopped
    networks:
      - ggen-monitoring

  alertmanager:
    image: prom/alertmanager:v0.26.0
    container_name: ggen-alertmanager
    ports:
      - "9093:9093"
    volumes:
      - ./alertmanager.yml:/etc/alertmanager/alertmanager.yml:ro
      - alertmanager_data:/alertmanager
    command:
      - '--config.file=/etc/alertmanager/alertmanager.yml'
      - '--storage.path=/alertmanager'
    restart: unless-stopped
    networks:
      - ggen-monitoring

  grafana:
    image: grafana/grafana:10.0.0
    container_name: ggen-grafana
    ports:
      - "3000:3000"
    environment:
      - GF_SECURITY_ADMIN_USER=admin
      - GF_SECURITY_ADMIN_PASSWORD=${GRAFANA_PASSWORD:-changeme}
      - GF_SERVER_ROOT_URL=https://grafana.example.com
      - GF_INSTALL_PLUGINS=grafana-piechart-panel
      - GF_ANALYTICS_CHECK_FOR_UPDATES=false
    volumes:
      - grafana_data:/var/lib/grafana
      - ./provisioning:/etc/grafana/provisioning:ro
    depends_on:
      - prometheus
    restart: unless-stopped
    networks:
      - ggen-monitoring

volumes:
  prometheus_data:
    driver: local
  alertmanager_data:
    driver: local
  grafana_data:
    driver: local

networks:
  ggen-monitoring:
    driver: bridge
```

### 2. Create alertmanager.yml

```yaml
global:
  resolve_timeout: 5m
  slack_api_url: '${SLACK_WEBHOOK_URL}'

route:
  receiver: 'default'
  group_by: ['alertname', 'cluster', 'service']
  group_wait: 10s
  group_interval: 10s
  repeat_interval: 12h

  routes:
    # Critical alerts - page immediately
    - match:
        severity: critical
      receiver: 'critical-team'
      group_wait: 0s
      repeat_interval: 5m

    # Warning alerts - daily digest
    - match:
        severity: warning
      receiver: 'warning-team'
      group_wait: 30s
      repeat_interval: 4h

receivers:
  - name: 'default'
    slack_configs:
      - channel: '#ggen-alerts'
        title: 'Alert: {{ .GroupLabels.alertname }}'
        text: '{{ range .Alerts }}{{ .Annotations.description }}{{ end }}'
        send_resolved: true

  - name: 'critical-team'
    slack_configs:
      - channel: '#ggen-incidents'
        title: 'CRITICAL: {{ .GroupLabels.alertname }}'
        text: '@here {{ range .Alerts }}{{ .Annotations.description }}{{ end }}'
        send_resolved: true
    pagerduty_configs:
      - service_key: '${PAGERDUTY_SERVICE_KEY}'
        description: 'ggen: {{ .GroupLabels.alertname }}'

  - name: 'warning-team'
    slack_configs:
      - channel: '#ggen-alerts'
        title: 'Warning: {{ .GroupLabels.alertname }}'
        text: '{{ range .Alerts }}{{ .Annotations.description }}{{ end }}'
        send_resolved: true

inhibit_rules:
  # Don't alert on circuit breaker if quorum is lost (parent issue)
  - source_match:
      severity: 'critical'
      alertname: 'ConsensusQuorumLost'
    target_match:
      alertname: 'CircuitBreakerOpen'
    equal: ['cluster']
```

### 3. Setup Grafana Provisioning

Create `provisioning/datasources/prometheus.yml`:
```yaml
apiVersion: 1

datasources:
  - name: Prometheus
    type: prometheus
    access: proxy
    url: http://prometheus:9090
    isDefault: true
    editable: true
```

Create `provisioning/dashboards/provider.yml`:
```yaml
apiVersion: 1

providers:
  - name: 'ggen dashboards'
    orgId: 1
    folder: 'ggen'
    type: file
    disableDeletion: false
    updateIntervalSeconds: 10
    allowUiUpdates: true
    options:
      path: /etc/grafana/provisioning/dashboards
```

Then place `grafana-dashboard.json` in `provisioning/dashboards/`.

---

## Deployment Steps

### Step 1: Prepare Environment

```bash
# Create monitoring directory
mkdir -p /srv/ggen-monitoring/{provisioning/{datasources,dashboards},data}
cd /srv/ggen-monitoring

# Copy configuration files
cp /path/to/prometheus.yml .
cp /path/to/alerts.yml .
cp /path/to/alertmanager.yml .
cp /path/to/grafana-dashboard.json provisioning/dashboards/

# Create provisioning files
mkdir -p provisioning/datasources provisioning/dashboards
# ... create files from section above ...

# Set permissions
chmod 755 .
chmod 644 *.yml
chmod -R 755 provisioning
```

### Step 2: Configure Environment Variables

Create `.env` file:
```bash
GRAFANA_PASSWORD=<secure-password>
SLACK_WEBHOOK_URL=https://hooks.slack.com/services/...
PAGERDUTY_SERVICE_KEY=<pagerduty-key>
```

### Step 3: Deploy Stack

```bash
# Start all services
docker-compose up -d

# Verify services are running
docker-compose ps

# Check logs
docker-compose logs -f prometheus
docker-compose logs -f alertmanager
docker-compose logs -f grafana
```

### Step 4: Verify Deployment

```bash
# Check Prometheus
curl http://localhost:9090/api/v1/status/config
curl http://localhost:9090/api/v1/targets

# Check Alertmanager
curl http://localhost:9093/api/v1/status
curl http://localhost:9093/api/v1/alerts

# Check Grafana
curl http://localhost:3000/api/health

# Test metrics scraping
curl http://localhost:9090/api/v1/query?query=up
```

### Step 5: Load Dashboard

1. Open Grafana: http://localhost:3000
2. Login: admin / (password from .env)
3. Navigate to Dashboards → Import
4. Upload `grafana-dashboard.json`
5. Select Prometheus data source
6. Confirm and save

---

## Health Checks

### Pre-Production Checklist

- [ ] Prometheus scraping all 5 targets (9090/targets)
- [ ] All metrics visible in Prometheus (query `up`)
- [ ] Alert rules loaded (`/api/v1/rules`)
- [ ] Alertmanager receiving alerts (`/api/v1/alerts`)
- [ ] Grafana dashboard displays all panels
- [ ] Test alert firing (reduce threshold temporarily)
- [ ] Slack notifications working
- [ ] PagerDuty integration tested
- [ ] DNS resolves all hostnames
- [ ] Network policies allow traffic on required ports

### Manual Testing

```bash
# 1. Test metric scraping
for target in 8080 9001 9002 9003 9004; do
  echo "Testing :$target"
  curl -s http://localhost:$target/metrics 2>/dev/null | head -5 || echo "Failed"
done

# 2. Test Prometheus scraping
curl -s http://localhost:9090/api/v1/targets | jq '.data.activeTargets[].labels.instance'

# 3. Test metric queries
for metric in supervisor_restart_count circuit_breaker_state consensus_node_healthy event_store_operation_duration_seconds_count; do
  echo "=== $metric ==="
  curl -s "http://localhost:9090/api/v1/query?query=$metric" | jq '.data.result[0]' || echo "Not found"
done

# 4. Fire test alert
curl -X POST http://localhost:9090/-/reload

# Temporarily lower threshold in alerts.yml, reload, verify alert fires
```

---

## Scaling & Performance

### Prometheus Tuning

For > 1 million time series:
```yaml
# prometheus.yml
tsdb:
  out-of-order-time-window: 24h

command:
  - '--storage.tsdb.max-block-duration=2h'
  - '--storage.tsdb.min-block-duration=2h'
  - '--query.max-concurrency=10'
```

### Alertmanager Tuning

For > 1000 alerts/minute:
```yaml
# alertmanager.yml
global:
  resolve_timeout: 30m
  group_interval: 5m
  group_wait: 5s
```

### Grafana Tuning

For > 50 concurrent users:
- Use read-only replica database
- Enable caching: `database.cache_mode = true`
- Configure reverse proxy with caching

---

## Backup & Recovery

### Prometheus Data

```bash
# Backup prometheus data
docker exec ggen-prometheus tar czf - /prometheus > prometheus-backup.tar.gz

# Restore
docker cp prometheus-backup.tar.gz ggen-prometheus:/tmp/
docker exec ggen-prometheus tar xzf /tmp/prometheus-backup.tar.gz -C /

# Verify
docker exec ggen-prometheus du -sh /prometheus
```

### Grafana Configuration

```bash
# Backup Grafana data
docker exec ggen-grafana grafana-cli admin export-dashboard

# Backup Grafana database
docker exec ggen-grafana tar czf - /var/lib/grafana > grafana-backup.tar.gz

# Restore
docker cp grafana-backup.tar.gz ggen-grafana:/tmp/
docker exec ggen-grafana tar xzf /tmp/grafana-backup.tar.gz -C /
```

### Disaster Recovery

```bash
# Full recovery procedure
docker-compose down --volumes

# Copy backups
cp prometheus-backup.tar.gz .
cp grafana-backup.tar.gz .

docker-compose up -d

docker cp prometheus-backup.tar.gz ggen-prometheus:/tmp/
docker exec ggen-prometheus tar xzf /tmp/prometheus-backup.tar.gz -C /

docker cp grafana-backup.tar.gz ggen-grafana:/tmp/
docker exec ggen-grafana tar xzf /tmp/grafana-backup.tar.gz -C /

docker-compose restart

# Verify
curl http://localhost:9090/api/v1/status
curl http://localhost:3000/api/health
```

---

## Troubleshooting

### Service Won't Start

```bash
# Check logs
docker-compose logs prometheus

# Common issues:
# 1. Port already in use: change ports in docker-compose.yml
# 2. Volume permission denied: sudo chown -R 65534:65534 prometheus_data
# 3. Invalid config: docker run --rm -v $(pwd)/prometheus.yml:/etc/prometheus/prometheus.yml prom/prometheus --config.file=/etc/prometheus/prometheus.yml --dry-run
```

### Metrics Not Appearing

```bash
# 1. Verify targets are reachable
curl http://<target-host>:8080/metrics

# 2. Check Prometheus logs for scrape errors
docker-compose logs prometheus | grep "error"

# 3. Manually query Prometheus
curl 'http://localhost:9090/api/v1/query?query=up'

# 4. Check alert rule evaluation
curl http://localhost:9090/api/v1/rules
```

### High Memory Usage

```bash
# Check memory consumption
docker stats ggen-prometheus

# Reduce retention
# In docker-compose.yml, prometheus command:
# - '--storage.tsdb.retention.time=7d'

# Or remove old data
docker exec ggen-prometheus prometheus \
  --storage.tsdb.path=/prometheus \
  --storage.tsdb.max-block-duration=1h \
  --storage.tsdb.min-block-duration=1h
```

---

## Maintenance Schedule

| Task | Frequency | Time |
|------|-----------|------|
| Check storage usage | Daily | 5 min |
| Review alert patterns | Daily | 10 min |
| Verify backups | Weekly | 15 min |
| Update Docker images | Monthly | 30 min |
| Full system test | Monthly | 1 hour |
| Capacity planning review | Quarterly | 1 hour |

---

## Support

- **Documentation:** See `MONITORING.md`
- **Metrics Schema:** See `METRICS-SCHEMA.md`
- **Runbooks:** https://ggen-docs.internal/runbooks
- **On-Call:** See escalation contacts in `MONITORING.md`
