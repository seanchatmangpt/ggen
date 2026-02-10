<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [How to Monitor Agent Health and Metrics](#how-to-monitor-agent-health-and-metrics)
  - [Monitoring Overview](#monitoring-overview)
  - [Health Checks](#health-checks)
  - [Real-time Metrics](#real-time-metrics)
  - [Performance Monitoring](#performance-monitoring)
    - [Request Latency](#request-latency)
    - [Throughput](#throughput)
    - [Error Tracking](#error-tracking)
  - [Resource Monitoring](#resource-monitoring)
    - [CPU Usage](#cpu-usage)
    - [Memory Usage](#memory-usage)
    - [Network I/O](#network-io)
  - [Log Monitoring](#log-monitoring)
    - [Viewing Logs](#viewing-logs)
    - [Filtering Logs](#filtering-logs)
    - [Log Analytics](#log-analytics)
  - [Alerting](#alerting)
    - [Setting Up Alerts](#setting-up-alerts)
    - [Alert Channels](#alert-channels)
    - [Alert Rules](#alert-rules)
  - [Dashboards](#dashboards)
    - [Built-in Dashboard](#built-in-dashboard)
    - [Metrics Export](#metrics-export)
    - [Integrations](#integrations)
  - [Troubleshooting](#troubleshooting)
    - [Agent Not Reporting](#agent-not-reporting)
    - [Incorrect Metrics](#incorrect-metrics)
    - [Alerts Not Firing](#alerts-not-firing)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# How to Monitor Agent Health and Metrics

Guide to monitoring A2A agent health, performance, and resource usage.

## Monitoring Overview

ggen provides three levels of monitoring:

| Level | Description | Tools |
|-------|-------------|-------|
| **Health** | Agent availability and status | `ggen agent health` |
| **Metrics** | Performance and usage data | `ggen agent metrics` |
| **Logs** | Detailed activity records | `ggen agent logs` |

## Health Checks

**Problem:** You need to verify if agents are healthy.

**Solution:** Use health check commands.

```bash
# Check all agents
ggen agent health

# Check specific agent
ggen agent health "text-generator"

# Continuous health monitoring
ggen agent health --watch

# Health with details
ggen agent health --verbose
```

**Expected Output:**
```bash
$ ggen agent health

Agent Health Status
===================

text-generator: Healthy
  Status: ready
  Uptime: 2h 34m
  Last check: just now

code-analyzer: Healthy
  Status: ready
  Uptime: 1h 15m
  Last check: 5s ago

workflow-agent: Degraded
  Status: busy
  Uptime: 45m
  Warning: High memory usage (85%)

Overall: 2 healthy, 1 degraded, 0 down
```

**Health check endpoints:**

```bash
# HTTP health endpoint
curl http://localhost:8080/health

# Detailed health
curl http://localhost:8080/health?detailed

# Agent-specific health
curl http://localhost:8080/agents/{agent-id}/health
```

## Real-time Metrics

**Problem:** You need to see current agent metrics.

**Solution:** Use the metrics command.

```bash
# Show all agent metrics
ggen agent metrics

# Show specific agent
ggen agent metrics "text-generator"

# Show specific metric
ggen agent metrics "text-generator" --metric request-rate

# Continuous metrics
ggen agent metrics --watch
```

**Expected Output:**
```bash
$ ggen agent metrics "text-generator"

Agent Metrics: text-generator
==============================

Performance:
  Request rate: 45.2 req/min
  Average latency: 52ms
  P95 latency: 120ms
  P99 latency: 250ms
  Error rate: 0.1%

Throughput:
  Messages sent: 1,234
  Messages received: 1,234
  Bytes sent: 2.3MB
  Bytes received: 1.8MB

Queue:
  Pending: 0
  Processing: 1
  Completed: 1,233
  Failed: 2

Time since last reset: 2h 34m
```

## Performance Monitoring

### Request Latency

```bash
# Show latency metrics
ggen agent metrics "agent" --latency

# Latency histogram
ggen agent metrics "agent" --latency --histogram

# Latency by percentile
ggen agent metrics "agent" --latency --percentiles 50,95,99
```

### Throughput

```bash
# Show throughput
ggen agent metrics "agent" --throughput

# Requests per second
ggen agent metrics "agent" --rps

# Messages per minute
ggen agent metrics "agent" --mpm
```

### Error Tracking

```bash
# Show error rate
ggen agent metrics "agent" --errors

# Recent errors
ggen agent errors "agent" --recent 10

# Error breakdown
ggen agent errors "agent" --by-type
```

## Resource Monitoring

### CPU Usage

```bash
# Show CPU usage
ggen agent resources "agent" --cpu

# CPU breakdown
ggen agent resources "agent" --cpu --detailed

# CPU history
ggen agent resources "agent" --cpu --history 60
```

**Expected Output:**
```bash
$ ggen agent resources "text-generator" --cpu

CPU Usage: text-generator
=========================

Current: 15% (0.3 / 2 cores)
Average (5m): 12%
Average (15m): 14%
Average (1h): 13%

Peak: 45% at 10:15:23

CPU by thread:
  Main: 8%
  Request handler: 5%
  Background: 2%
```

### Memory Usage

```bash
# Show memory usage
ggen agent resources "agent" --memory

# Memory breakdown
ggen agent resources "agent" --memory --detailed

# Memory history
ggen agent resources "agent" --memory --history 60
```

**Expected Output:**
```bash
$ ggen agent resources "text-generator" --memory

Memory Usage: text-generator
============================

Current: 1.2GB / 4GB (30%)
Average (5m): 1.1GB
Peak: 1.8GB at 10:10:15

Memory breakdown:
  Heap: 950MB
  Stack: 120MB
  Code: 50MB
  Other: 80MB

GC stats:
  Collections: 23
  Total time: 450ms
  Avg pause: 19.5ms
```

### Network I/O

```bash
# Show network usage
ggen agent resources "agent" --network

# Connection stats
ggen agent resources "agent" --connections
```

## Log Monitoring

### Viewing Logs

```bash
# Show recent logs
ggen agent logs "text-generator"

# Tail logs
ggen agent logs "text-generator" --tail

# Follow logs
ggen agent logs "text-generator" --follow

# Logs with timestamp
ggen agent logs "text-generator" --timestamps
```

### Filtering Logs

```bash
# Filter by level
ggen agent logs "agent" --level error

# Filter by pattern
ggen agent logs "agent" --grep "timeout"

# Filter by time range
ggen agent logs "agent" --since "1h ago"

# Filter multiple
ggen agent logs "agent" \
  --level warn \
  --grep "error" \
  --since "30m ago"
```

### Log Analytics

```bash
# Count by level
ggen agent logs "agent" --count-by-level

# Top errors
ggen agent logs "agent" --top-errors

# Log summary
ggen agent logs "agent" --summary
```

## Alerting

### Setting Up Alerts

**Problem:** You need to be notified of issues.

**Solution:** Configure alerts.

```bash
# Create alert
ggen alerts create \
  --name "high-error-rate" \
  --condition "error-rate > 5%" \
  --action "notify:email:admin@example.com"

# Create resource alert
ggen alerts create \
  --name "high-memory" \
  --condition "memory > 80%" \
  --action "notify:slack:#alerts"

# List alerts
ggen alerts list

# Test alert
ggen alerts test "high-error-rate"
```

### Alert Channels

```bash
# Configure email channel
ggen alerts channel create email \
  --to "admin@example.com" \
  --smtp "smtp.example.com"

# Configure Slack channel
ggen alerts channel create slack \
  --webhook "https://hooks.slack.com/..." \
  --channel "#alerts"

# Configure webhook channel
ggen alerts channel create webhook \
  --url "https://example.com/webhook" \
  --headers "Authorization=Bearer xxx"
```

### Alert Rules

```yaml
# alerts.yaml
alerts:
  - name: high-error-rate
    condition:
      metric: error-rate
      operator: ">"
      threshold: 5
      duration: 5m
    actions:
      - type: notify
        channel: email
        template: error-alert

  - name: high-latency
    condition:
      metric: p95-latency
      operator: ">"
      threshold: 500ms
      duration: 10m
    actions:
      - type: notify
        channel: slack

  - name: agent-down
    condition:
      metric: health
      operator: "=="
      threshold: "down"
      duration: 1m
    actions:
      - type: restart
        agent: "{{agent_name}}"
      - type: notify
        channel: pagerduty
```

```bash
# Load alert rules
ggen alerts load alerts.yaml
```

## Dashboards

### Built-in Dashboard

```bash
# Start dashboard server
ggen dashboard start --port 8081

# Open in browser
open http://localhost:8081
```

### Metrics Export

```bash
# Export to Prometheus format
ggen agent metrics --format prometheus

# Export to JSON
ggen agent metrics --format json --output metrics.json

# Export to CSV
ggen agent metrics --format csv --output metrics.csv
```

### Integrations

```bash
# Enable Prometheus endpoint
ggen config set monitoring.prometheus.enabled=true
ggen config set monitoring.prometheus.port=9090

# Enable OpenTelemetry
ggen config set monitoring.otel.enabled=true
ggen config set monitoring.otel.endpoint="http://otel-collector:4317"

# Enable StatsD
ggen config set monitoring.statsd.enabled=true
ggen config set monitoring.statsd.host="localhost"
ggen config set monitoring.statsd.port=8125
```

## Troubleshooting

### Agent Not Reporting

**Problem:** Agent health status shows as "unknown".

**Solutions:**
```bash
# Check agent is running
ggen agent status "agent-name"

# Check monitoring configuration
ggen config show | grep -A 10 monitoring

# Verify health endpoint
curl http://localhost:8080/agents/{agent-id}/health

# Check network connectivity
ping agent-host

# Review agent logs
ggen agent logs "agent-name" --grep "health"
```

### Incorrect Metrics

**Problem:** Reported metrics don't match actual values.

**Solutions:**
```bash
# Reset metrics
ggen agent metrics "agent" --reset

# Recalibrate metrics
ggen agent metrics "agent" --recalibrate

# Verify metric collection
ggen agent metrics "agent" --debug

# Compare with system metrics
top -pid $(pgrep -f "agent-name")
```

### Alerts Not Firing

**Problem:** Alerts configured but not triggering.

**Solutions:**
```bash
# Verify alert conditions
ggen alerts test "alert-name"

# Check alert status
ggen alerts status "alert-name"

# Verify alert configuration
ggen alerts show "alert-name"

# Check notification channels
ggen alerts channel test email

# Test with manual trigger
ggen alerts trigger "alert-name"
```

## Next Steps

- **Start agents:** [How to Start A2A Agents](start-agent.md)
- **Send messages:** [How to Send Messages Between Agents](send-messages.md)
- **Configure transport:** [How to Configure Transport Protocols](configure-transport.md)
- **Performance guide:** [Performance Optimization Guide](../performance.md)
- **Integration guide:** [MCP A2A Integration Guide](../MCP_A2A_INTEGRATION.md)
