# Monitoring Infrastructure Deployment Guide

Complete guide to deploying the expanded monitoring and alerting infrastructure.

## Overview

This deployment adds:
- 10 new Grafana dashboards (API Gateway, Load Balancer, gRPC, Multi-Region, Cost, Capacity, SLO, Tracing, Profiling, Correlation)
- Comprehensive Prometheus alert rules (SLO, latency, errors, resources, infrastructure)
- Alertmanager configuration with smart routing and deduplication
- Alert suppression rules for maintenance windows
- Runbooks and metric correlation engine documentation

## Prerequisites

```
- Prometheus 2.30+
- Alertmanager 0.21+
- Grafana 8.0+
- Go 1.16+ (for Alertmanager)
- curl, jq (for verification)
```

## Deployment Steps

### 1. Backup Existing Configuration

```bash
# Backup current Prometheus rules
cp prometheus/alert-rules.yml prometheus/alert-rules.yml.backup

# Backup current Alertmanager config
cp prometheus/alertmanager.yml prometheus/alertmanager.yml.backup

# Backup current Grafana dashboards
mkdir -p grafana/dashboards/backup
cp grafana/dashboards/*.json grafana/dashboards/backup/
```

### 2. Deploy Grafana Dashboards

```bash
# Copy new dashboards to Grafana
cp grafana/dashboards/{api-gateway,load-balancer,grpc-metrics,multi-region,cost-tracking,capacity-planning,slo-error-budget,trace-analysis,profiling,metric-correlation}.json /var/lib/grafana/dashboards/

# Restart Grafana to load new dashboards
sudo systemctl restart grafana-server

# Verify dashboards loaded
curl -s http://localhost:3000/api/search | jq '.[] | .title' | grep -E "API Gateway|Load Balancer|gRPC|Multi-Region|Cost|Capacity|SLO|Trace|Profiling|Correlation"
```

### 3. Deploy Prometheus Alert Rules

```bash
# Expand alert-rules.yml with new alert groups
# File already contains both old and new alert groups

# Validate Prometheus configuration
promtool check config prometheus/prometheus.yml

# Validate alert rules
promtool check rules prometheus/alert-rules.yml

# Reload Prometheus configuration
curl -X POST http://localhost:9090/-/reload

# Verify rules loaded
curl -s http://localhost:9090/api/v1/rules | jq '.data.groups | length'
```

### 4. Deploy Alertmanager Configuration

```bash
# Set environment variables for notifications
export SLACK_WEBHOOK_URL="https://hooks.slack.com/services/YOUR/WEBHOOK/URL"
export PAGERDUTY_SERVICE_KEY="your-service-key-here"
export PAGERDUTY_SLO_SERVICE_KEY="your-slo-service-key-here"
export SMTP_HOST="smtp.company.com:587"
export SMTP_USERNAME="alertmanager@company.com"
export SMTP_PASSWORD="your-password-here"
export SMTP_FROM="alerting@company.com"
export INFRASTRUCTURE_TEAM_EMAIL="ops@company.com"

# Replace placeholders in alertmanager.yml
envsubst < prometheus/alertmanager.yml > prometheus/alertmanager-rendered.yml

# Validate Alertmanager configuration
amtool check-config prometheus/alertmanager-rendered.yml

# Deploy (choose one based on deployment method)

# Option A: Kubernetes
kubectl create configmap alertmanager-config --from-file=prometheus/alertmanager-rendered.yml --dry-run=client -o yaml | kubectl apply -f -
kubectl rollout restart deployment/alertmanager

# Option B: Docker Compose
docker-compose down alertmanager
docker-compose up -d alertmanager

# Option C: Systemd
sudo cp prometheus/alertmanager-rendered.yml /etc/alertmanager/alertmanager.yml
sudo systemctl restart alertmanager

# Verify Alertmanager is running
curl -s http://localhost:9093/api/v1/alerts | jq '.data | length'
```

### 5. Deploy Alert Suppression Rules

```bash
# Add suppression rules to Prometheus
cat prometheus/alert-suppression-rules.yml >> prometheus/prometheus.yml

# Reload Prometheus
curl -X POST http://localhost:9090/-/reload

# Verify suppression rules loaded
curl -s http://localhost:9090/api/v1/rules | jq '.data.groups[] | select(.name=="suppression_rules") | .rules | length'
```

### 6. Configure Slack Integration

```bash
# 1. Create Slack incoming webhook
# Go to https://api.slack.com/apps → Create App → Incoming Webhooks
# Create webhooks for each channel:
#   - #incidents (critical alerts)
#   - #alerts (warnings)
#   - #slo-violations (SLO alerts)
#   - #performance (performance alerts)
#   - #infrastructure (infrastructure alerts)

# 2. Test Slack integration
curl -X POST $SLACK_WEBHOOK_URL \
  -H 'Content-Type: application/json' \
  -d '{
    "text": "✅ Monitoring infrastructure deployed successfully!",
    "color": "good"
  }'
```

### 7. Configure PagerDuty Integration

```bash
# 1. Create PagerDuty service for critical alerts
# Go to PagerDuty → Services → Create new service
# Select "Event Intelligence" integration type
# Copy service key to PAGERDUTY_SERVICE_KEY

# 2. Create separate service for SLO violations
# Repeat step 1 for SLO-specific alerts

# 3. Test PagerDuty integration
# (Will be tested with first actual alert)
```

### 8. Configure Email Integration

```bash
# 1. Set SMTP credentials (already done above)

# 2. Test email sending
echo "Test email" | mail -s "Monitoring Test" infrastructure-team@company.com

# 3. Verify SMTP settings in alertmanager.yml
grep -A 5 "smtp_" prometheus/alertmanager.yml
```

### 9. Verify Monitoring Stack

```bash
# Check all components running
echo "=== Prometheus ==="
curl -s http://localhost:9090/api/v1/query?query=up | jq '.data.result | length'

echo "=== Alertmanager ==="
curl -s http://localhost:9093/api/v1/alerts | jq '.data | length'

echo "=== Grafana ==="
curl -s http://localhost:3000/api/datasources | jq '.[] | select(.name=="Prometheus") | .name'

# Check alert rules count
echo "=== Alert Rules ==="
curl -s http://localhost:9090/api/v1/rules | jq '.data.groups | map(.rules | length) | add'

# Test Alertmanager reload
echo "=== Testing Alertmanager Reload ==="
amtool check-config prometheus/alertmanager.yml && echo "✅ Config valid"
```

## Validation Checklist

- [ ] All 10 dashboards visible in Grafana
- [ ] All alert rule groups loaded (7 total: tps_jidoka, tps_kanban, tps_andon, tps_kaizen, tps_heijunka, tps_tracing, slo_compliance, latency_performance, error_reliability, resource_utilization, infrastructure)
- [ ] Alertmanager configuration valid and loaded
- [ ] Slack integration working (test message received)
- [ ] PagerDuty integration configured
- [ ] Email alerts configured
- [ ] Alert suppression rules loaded
- [ ] Runbooks accessible and up-to-date
- [ ] Metric Correlation dashboard showing metrics
- [ ] No Prometheus errors in logs
- [ ] No Alertmanager errors in logs

## Testing

### Test Alert Rules

```bash
# Test if an alert would fire (use promtool)
promtool query instant 'sum(rate(http_requests_total[5m]))'

# Manually trigger test alert
# Open Prometheus and create temporary rule:
# - alert: TestAlert
#   expr: vector(1)
#   for: 1m

# Check if alert fires in Alertmanager:
curl -s http://localhost:9093/api/v1/alerts | jq '.data[] | select(.labels.alertname=="TestAlert")'
```

### Test Notification Channels

```bash
# Test Slack
echo 'Test message to Slack' | curl -X POST -d @- \
  -H 'Content-Type: application/x-www-form-urlencoded' \
  $SLACK_WEBHOOK_URL

# Test email
mail -s "Test from Alertmanager" infrastructure-team@company.com < /dev/null

# Test PagerDuty (requires proper alert context)
# Will be tested with first critical alert
```

## Rollback Plan

If something goes wrong:

```bash
# 1. Restore Prometheus rules
cp prometheus/alert-rules.yml.backup prometheus/alert-rules.yml
curl -X POST http://localhost:9090/-/reload

# 2. Restore Alertmanager config
cp prometheus/alertmanager.yml.backup prometheus/alertmanager.yml
sudo systemctl restart alertmanager

# 3. Remove new Grafana dashboards
rm grafana/dashboards/{api-gateway,load-balancer,grpc-metrics,multi-region,cost-tracking,capacity-planning,slo-error-budget,trace-analysis,profiling,metric-correlation}.json
sudo systemctl restart grafana-server

# 4. Verify rollback
curl -s http://localhost:9090/api/v1/rules | jq '.data.groups | length'
```

## Post-Deployment Tasks

### 1. Update Runbooks

```bash
# Ensure all team members have access to runbooks
# Deploy runbooks to internal wiki/documentation site

# Create symlink to accessible location
ln -s $(pwd)/docs/RUNBOOKS.md /var/www/html/runbooks/main.md
ln -s $(pwd)/docs/METRIC_CORRELATION_ENGINE.md /var/www/html/runbooks/correlation-engine.md
```

### 2. Train Team

- [ ] Conduct monitoring training for on-call engineers
- [ ] Walk through each dashboard and alert
- [ ] Demonstrate runbook usage
- [ ] Practice incident response scenarios

### 3. Configure Alert Routing

- [ ] Verify Slack channels created and members added
- [ ] Verify PagerDuty escalation policies configured
- [ ] Verify email recipients correct
- [ ] Test alert routing with test alert

### 4. Set Up Maintenance Windows

```bash
# Add recurring maintenance windows in Alertmanager
# Example: Monday 2am UTC
# Automatically suppress non-critical alerts during window

# Set in prometheus alerting configuration
```

### 5. Tune Alert Thresholds

Over first 2 weeks:

- [ ] Monitor alert volume (too many = false positives)
- [ ] Check MTTR (Mean Time To Resolution) improvement
- [ ] Adjust thresholds if needed
- [ ] Document why changes were made

## Monitoring the Monitoring

After deployment, monitor these metrics:

```bash
# Prometheus health
curl -s http://localhost:9090/metrics | grep prometheus_tsdb

# Alertmanager metrics
curl -s http://localhost:9093/metrics | grep alertmanager

# Alert generation rate
curl -s http://localhost:9090/metrics | grep prometheus_rule_evaluation_duration_seconds
```

## Performance Expectations

After deployment:

- **Prometheus scrape time:** < 5 seconds
- **Alert evaluation time:** < 2 seconds
- **Alertmanager processing:** < 100ms
- **Notification delivery:** < 5 seconds to Slack/email
- **Grafana dashboard load:** < 3 seconds

## Troubleshooting

### Alerts Not Firing

```bash
# Check alert rules
curl -s http://localhost:9090/api/v1/rules | jq '.data.groups[]'

# Check alert state
curl -s http://localhost:9090/api/v1/alerts | jq '.data[]'

# Check Prometheus logs
journalctl -u prometheus -f
```

### Notifications Not Arriving

```bash
# Check Alertmanager configuration
amtool check-config /etc/alertmanager/alertmanager.yml

# Check notification logs
journalctl -u alertmanager -f

# Test webhook manually
curl -X POST $SLACK_WEBHOOK_URL -d '{"text":"test"}'
```

### Dashboards Not Loading

```bash
# Check Grafana logs
docker logs grafana-container
# or
journalctl -u grafana-server -f

# Check dashboard JSON syntax
jq . grafana/dashboards/api-gateway.json > /dev/null
```

## Support

For issues:

1. Check [RUNBOOKS.md](./RUNBOOKS.md) for troubleshooting steps
2. Review [METRIC_CORRELATION_ENGINE.md](./METRIC_CORRELATION_ENGINE.md) for root cause analysis
3. Contact #sre-incidents Slack channel
4. Contact DevOps team if infrastructure issues

## Version History

- **v1.0.0** (2026-01-25) - Initial release with 10 dashboards, comprehensive alert rules, smart alerting, and runbooks
