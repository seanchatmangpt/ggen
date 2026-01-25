# TPS Reference Implementation - Complete Guide

**Version**: 1.0.0
**Last Updated**: January 2026
**Status**: Production-Ready

## Table of Contents

1. [Overview](#overview)
2. [Architecture](#architecture)
3. [Quick Start](#quick-start)
4. [Prerequisites](#prerequisites)
5. [Installation & Setup](#installation--setup)
6. [Configuration](#configuration)
7. [Running the System](#running-the-system)
8. [API Reference](#api-reference)
9. [Load Testing](#load-testing)
10. [Observation & Monitoring](#observation--monitoring)
11. [Troubleshooting](#troubleshooting)
12. [Customization](#customization)
13. [Advanced Topics](#advanced-topics)
14. [References](#references)

---

## Overview

The TPS Reference Implementation is a production-ready system that demonstrates all **6 core Toyota Production System (TPS) principles** working together in an integrated, observable environment:

### The 6 TPS Principles

1. **Jidoka** (Autonomation with Human Touch)
   - Automatic circuit breaker for fault isolation
   - Prevents cascade failures and system overload
   - Recovers gracefully when conditions improve

2. **Kanban** (Pull-Based Work Distribution)
   - Queue-based signal processing
   - Work pulled only when capacity available
   - No pushing work; prevents queue buildup

3. **Andon** (Visual Signal System)
   - Real-time status indicators (RED/YELLOW/GREEN)
   - Alert system for problems
   - Historical signal tracking

4. **Kaizen** (Continuous Improvement)
   - Comprehensive metrics collection
   - Success/error tracking
   - Performance percentiles (P95, P99)

5. **Heijunka** (Level Loading)
   - Workload distributed evenly across workers
   - Automatic load balancing
   - Prevents worker overload

6. **Tracing** (End-to-End Observability)
   - Request correlation via trace IDs
   - Full request lifecycle visibility
   - Integration with Jaeger for distributed tracing

### What You'll Learn

This implementation teaches:
- How TPS principles prevent defects and waste
- Integration of multiple quality systems
- Production-ready error handling
- Observability at scale
- Graceful degradation under load

---

## Architecture

### System Components

```
┌─────────────────────────────────────────────────────────┐
│              TPS Reference System (HTTP)                 │
│  Port 8080: /signal, /health, /metrics, /status         │
└────────────────┬────────────────────────────────────────┘
                 │
    ┌────────────┼────────────┬──────────┬─────────┐
    │            │            │          │         │
    ▼            ▼            ▼          ▼         ▼
┌────────┐  ┌────────┐  ┌────────┐ ┌────────┐ ┌────────┐
│Jidoka  │  │Kanban  │  │Andon   │ │Kaizen  │ │Heijunka│
│Circuit │  │Queue   │  │Signals │ │Metrics │ │Loading │
│Breaker │  │(NATS)  │  │(Logs)  │ │(Prom)  │ │(Pool)  │
└───┬────┘  └───┬────┘  └───┬────┘ └───┬────┘ └───┬────┘
    │           │           │          │          │
    └───────────┼───────────┼──────────┼──────────┘
                │           │          │
    ┌───────────┴─┐     ┌───┴──────┐ ┌┴──────────┐
    │   Worker    │     │Observ.   │ │Messaging  │
    │   Pool (4)  │     │Stack     │ │Backup     │
    └─────────────┘     └───┬──────┘ └──────┬────┘
                        ┌───┴─────────────┐ │
                        │                 │ │
                    ┌───▼────┐ ┌──────┐ ┌─▼────┐
                    │ Jaeger  │ │Loki  │ │RabbitMQ
                    │ Traces  │ │Logs  │ │Queue
                    └────┬────┘ └──┬───┘ └──────┘
                         │         │
                    ┌────▼────┬────▼──┐
                    │ Grafana  │ Prometheus
                    │ Display  │ Storage
                    └──────────┴───────┘
```

### Data Flow

**Signal Processing Pipeline**:
```
Input Signal
    ↓
[Jidoka] Circuit Breaker Check
    ↓
[Kanban] Enqueue Signal
    ↓
[Worker] Process Signal
    ↓
[Andon] Record Status (RED/YELLOW/GREEN)
    ↓
[Kaizen] Record Metrics
    ↓
[Heijunka] Update Load Distribution
    ↓
[Tracing] Complete Trace
    ↓
Output Result
```

### Supervision Tree

Every TPS component is supervised:
- **Jidoka**: Monitors for failures, opens circuit when threshold exceeded
- **Kanban**: Manages queue depth, applies backpressure
- **Andon**: Records signal history, tracks state changes
- **Kaizen**: Aggregates metrics, computes percentiles
- **Heijunka**: Monitors worker distribution, rebalances load
- **Tracing**: Correlates requests across service boundaries

---

## Quick Start

### 60-Second Startup

```bash
# 1. Clone and navigate to example
cd examples/tps-reference-system

# 2. Start the complete stack
make up

# 3. Check health (in another terminal)
make health

# 4. Send a signal
curl -X POST http://localhost:8080/signal \
  -H "Content-Type: application/json" \
  -d '{"signal_type":"execute","payload":{"task":"demo"}}'

# 5. View metrics
curl http://localhost:8080/metrics | jq .

# 6. Open dashboards
make dashboard      # Grafana (localhost:3000)
make traces         # Jaeger (localhost:16686)
```

### Expected Output

```json
{
  "success": true,
  "signal_id": "a1b2c3d4-e5f6-7890-abcd-ef1234567890",
  "message": "Execution successful",
  "trace_id": "x1y2z3-abc-def-123",
  "duration_ms": 50
}
```

---

## Prerequisites

### Required Software

- **Docker** 20.10+ and **Docker Compose** 1.29+
- **Rust** 1.70+ (for local development)
- **Cargo** (included with Rust)
- **curl** (for API testing)

### System Requirements

- **CPU**: 4+ cores (recommended)
- **RAM**: 8GB+ (Docker stack uses ~4GB)
- **Disk**: 10GB+ free space (for volumes)
- **Network**: Localhost port access (8080, 9090, 3000, 16686)

### Port Requirements

| Service | Port | Protocol | Purpose |
|---------|------|----------|---------|
| TPS System | 8080 | HTTP | API and documentation |
| Metrics | 9090 | HTTP | Prometheus metrics |
| Prometheus | 9091 | HTTP | Metrics database |
| Grafana | 3000 | HTTP | Dashboards |
| Jaeger UI | 16686 | HTTP | Tracing visualizer |
| Jaeger Collector | 14268 | HTTP | Trace collection |
| NATS | 4222 | TCP | Message queue |
| NATS Monitor | 8222 | HTTP | NATS status |
| RabbitMQ | 5672 | AMQP | Message queue backup |
| RabbitMQ UI | 15672 | HTTP | Management |
| Loki | 3100 | HTTP | Log storage |

---

## Installation & Setup

### 1. Clone Repository

```bash
git clone https://github.com/seanchatmangpt/ggen.git
cd ggen/examples/tps-reference-system
```

### 2. Verify Docker Installation

```bash
docker --version
docker-compose --version

# Should output version 20.10+ and 1.29+
```

### 3. Build Docker Image

```bash
make build

# Or manually:
docker-compose build
```

### 4. Verify Port Availability

```bash
# Check if ports are available
lsof -i :8080   # TPS System
lsof -i :9090   # Metrics
lsof -i :3000   # Grafana
lsof -i :16686  # Jaeger
```

If ports are in use, either:
- Stop conflicting services
- Modify `docker-compose.yml` to use different ports
- Run services on a different host

### 5. Start Stack

```bash
make up

# Verify all services are up
make health

# Should show:
# TPS System: ✓ UP
# Prometheus: ✓ UP
# Grafana: ✓ UP
# Jaeger: ✓ UP
# NATS: ✓ UP
# RabbitMQ: ✓ UP
```

---

## Configuration

### Environment Variables

Create `.env` file in project root:

```bash
# TPS Configuration
TPS_NUM_WORKERS=4
TPS_CB_THRESHOLD=5                    # Circuit breaker failure threshold
TPS_CB_TIMEOUT=30                     # Circuit breaker reset timeout (seconds)
TPS_KANBAN_BUFFER=1000                # Queue buffer size
TPS_ANDON_HISTORY=100                 # Signal history records
TPS_HEIJUNKA_POOL=4                   # Worker pool size

# Service Ports
HTTP_PORT=8080
METRICS_PORT=9090

# Message Queues
NATS_URL=nats://nats:4222
RABBITMQ_URL=amqp://guest:guest@rabbitmq:5672/

# Observability
JAEGER_ENDPOINT=http://jaeger:14268/api/traces
RUST_LOG=debug

# Application
APP_NAME=tps-reference-system
APP_ENVIRONMENT=production
```

### Configuration Profiles

#### Development Profile

```bash
# .env.dev
TPS_NUM_WORKERS=2
TPS_CB_THRESHOLD=3
TPS_KANBAN_BUFFER=100
RUST_LOG=debug
```

Load with:
```bash
export $(cat .env.dev | xargs)
make up
```

#### Staging Profile

```bash
# .env.staging
TPS_NUM_WORKERS=4
TPS_CB_THRESHOLD=5
TPS_KANBAN_BUFFER=500
TPS_ANDON_HISTORY=200
RUST_LOG=info
```

#### Production Profile

```bash
# .env.prod
TPS_NUM_WORKERS=8
TPS_CB_THRESHOLD=10
TPS_KANBAN_BUFFER=2000
TPS_ANDON_HISTORY=500
RUST_LOG=warn
```

### Tuning Parameters

**For High Throughput**:
```bash
TPS_NUM_WORKERS=16
TPS_KANBAN_BUFFER=5000
TPS_HEIJUNKA_POOL=16
```

**For Low Latency**:
```bash
TPS_NUM_WORKERS=2
TPS_KANBAN_BUFFER=100
TPS_CB_THRESHOLD=2
TPS_CB_TIMEOUT=10
```

**For Fault Tolerance**:
```bash
TPS_CB_THRESHOLD=20           # Tolerate more failures
TPS_CB_TIMEOUT=60             # Longer reset timeout
TPS_KANBAN_BUFFER=10000       # Large buffer
```

---

## Running the System

### Starting Services

```bash
# Start entire stack (all services)
make up

# Start specific service
docker-compose up -d tps-system

# Start without rebuild
docker-compose up -d --no-recreate
```

### Checking Status

```bash
# Quick health check
make health

# Detailed status
make status

# System information
make info

# Watch service status
docker-compose ps
```

### Stopping Services

```bash
# Stop all services (data persists in volumes)
make down

# Stop and remove all data
make clean

# Stop specific service
docker-compose stop tps-system

# Restart specific service
docker-compose restart tps-system
```

### Viewing Logs

```bash
# All service logs
make logs

# Tail specific service
make logs-system      # TPS system
make logs-nats        # NATS queue
make logs-rabbitmq    # RabbitMQ

# Filter by level
docker-compose logs --tail=100 tps-system | grep ERROR
```

---

## API Reference

### Endpoints

#### POST /signal

Process a work signal through the TPS pipeline.

**Request**:
```bash
curl -X POST http://localhost:8080/signal \
  -H "Content-Type: application/json" \
  -d '{
    "signal_type": "validate",
    "priority": 2,
    "payload": {"data": [1, 2, 3]},
    "timeout_ms": 30000
  }'
```

**Response**:
```json
{
  "success": true,
  "signal_id": "550e8400-e29b-41d4-a716-446655440000",
  "message": "Validation successful",
  "trace_id": "550e8400-e29b-41d4-a716-446655440001",
  "duration_ms": 12
}
```

**Signal Types**:
- `validate` - Validate input data
- `execute` - Execute work
- `report` - Generate report

**Priority Levels**:
- `1` - Critical (immediate)
- `2` - High (soon)
- `3` - Normal (standard queue)
- `4` - Low (background)

**Status Codes**:
- `202` - Accepted (processing)
- `400` - Bad request (invalid signal)
- `503` - Service unavailable (circuit open)

#### GET /health

Check system health status.

**Request**:
```bash
curl http://localhost:8080/health
```

**Response**:
```json
{
  "healthy": true,
  "circuit_state": "Closed",
  "queue_depth": 5,
  "active_workers": 4,
  "error_rate": 0.5,
  "timestamp": "2026-01-25T12:00:00Z"
}
```

**Interpretation**:
- `healthy: true` - System is operational
- `circuit_state: Closed` - Normal operation
- `circuit_state: Open` - Overloaded (pause requests)
- `error_rate > 50` - Many failures (investigate)

#### GET /metrics

Get system metrics and statistics.

**Request**:
```bash
curl http://localhost:8080/metrics
```

**Response**:
```json
{
  "total_processed": 1000,
  "success_count": 987,
  "error_count": 13,
  "error_rate": 1.3,
  "avg_duration_ms": 42.5,
  "p95_duration_ms": 78.2,
  "p99_duration_ms": 105.6
}
```

**Key Metrics**:
- `error_rate` - Percentage of failed signals
- `avg_duration_ms` - Average processing time
- `p95_duration_ms` - 95th percentile (slowest 5%)
- `p99_duration_ms` - 99th percentile (slowest 1%)

#### GET /info

Get system information and configuration.

**Request**:
```bash
curl http://localhost:8080/info
```

**Response**:
```json
{
  "name": "TPS Reference System",
  "version": "1.0.0",
  "uptime_secs": 3600,
  "config": {
    "num_workers": 4,
    "http_port": 8080,
    "metrics_port": 9090
  }
}
```

#### GET /status

Get comprehensive system status.

**Request**:
```bash
curl http://localhost:8080/status
```

**Response**:
```json
{
  "status": "running",
  "health": {
    "healthy": true,
    "circuit_state": "Closed",
    "queue_depth": 0,
    "active_workers": 4,
    "error_rate": 0.0,
    "timestamp": "2026-01-25T12:00:00Z"
  },
  "metrics": {
    "total_processed": 1000,
    "success_count": 1000,
    "error_count": 0,
    "error_rate": 0.0,
    "avg_duration_ms": 42.5,
    "p95_duration_ms": 78.2,
    "p99_duration_ms": 105.6
  }
}
```

#### GET /

Get HTML documentation page.

**Request**:
```bash
curl http://localhost:8080/
```

**Response**: Interactive HTML page with API documentation.

---

## Load Testing

### Load Test Scenarios

#### 1. Normal Load (100 req/s)

```bash
# Run for 60 seconds at 100 req/s
make load

# Or manually:
for i in {1..60}; do
  for j in {1..100}; do
    curl -s -X POST http://localhost:8080/signal \
      -H "Content-Type: application/json" \
      -d '{"signal_type":"validate","payload":{"index":'$j'}}' &
  done
  wait
  sleep 1
done
```

**Expected Results**:
- All requests succeed (error_rate ~0%)
- avg_duration_ms: 40-60ms
- p99_duration_ms: 100-150ms
- Circuit breaker: Closed

#### 2. Traffic Spike (1000 req/s)

```bash
# Run for 10 seconds at 1000 req/s
make spike

# Observe queue buildup
while true; do
  curl -s http://localhost:8080/status | jq '.health.queue_depth'
  sleep 1
done
```

**Expected Results**:
- Queue depth increases rapidly
- Some requests queued (acceptable backpressure)
- Circuit breaker holds (or opens briefly)
- Error rate < 5%

#### 3. Sustained Stress (500 req/s)

```bash
# Run for 30 seconds at 500 req/s
make stress

# Monitor performance degradation
watch -n 1 'curl -s http://localhost:8080/metrics | jq .'
```

**Expected Results**:
- Sustained throughput increases
- Latency increases gradually (linear)
- Error rate < 1%
- System recovers when load decreases

#### 4. Chaos Testing

```bash
# Kill random components and verify recovery
make chaos

# Monitor recovery
while true; do
  echo "Health: $(curl -s http://localhost:8080/health | jq '.healthy')"
  sleep 5
done
```

**Expected Results**:
- Services restart automatically
- Queue drains on recovery
- Metrics resume collection
- No data loss

### Load Test Interpretation

**Green Indicators**:
- Error rate < 1%
- p99_duration_ms < 200ms
- Circuit breaker rarely opens
- Queue drains within 10s

**Yellow Indicators**:
- Error rate 1-5%
- p99_duration_ms 200-500ms
- Circuit breaker opens occasionally
- Queue takes 30-60s to drain

**Red Indicators**:
- Error rate > 5%
- p99_duration_ms > 500ms
- Circuit breaker open continuously
- Queue never drains

---

## Observation & Monitoring

### Health Checks

```bash
# Quick health
make health

# Detailed health
curl http://localhost:8080/health | jq .
curl http://localhost:8080/status | jq .
curl http://localhost:8080/metrics | jq .
```

### Metrics Collection

#### Prometheus

```bash
# Access at http://localhost:9091

# Query signals processed
curl 'http://localhost:9091/api/v1/query?query=tps_signals_processed'

# Query error rate
curl 'http://localhost:9091/api/v1/query?query=tps_error_rate'

# Get metrics history
curl 'http://localhost:9091/api/v1/query_range?query=tps_signals_processed&start=2026-01-25T00:00:00Z&end=2026-01-25T23:59:59Z&step=300'
```

#### Grafana Dashboards

```bash
# Open dashboard
make dashboard

# Default credentials: admin/admin

# Available dashboards:
# - TPS Overview (system health, queue depth, worker distribution)
# - Performance Metrics (latency percentiles, throughput)
# - Circuit Breaker Status (state changes, recovery)
# - Error Analysis (error rates, types)
```

### Distributed Tracing

```bash
# Open Jaeger
make traces

# Query traces
# 1. Select "tps-system" service
# 2. Set operation to "process_signal"
# 3. Click "Find Traces"
# 4. Click trace to view full span tree

# Command line query
curl 'http://localhost:16686/api/traces?service=tps-system&operation=process_signal&limit=10'
```

### Log Aggregation

```bash
# Loki logs
curl 'http://localhost:3100/api/prom/query?query={job="tps-system"}'

# View logs in Grafana
# 1. Open http://localhost:3000
# 2. Select Loki data source
# 3. Query: {job="tps-system"}
```

### Real-Time Monitoring

```bash
# Watch queue depth
watch -n 1 'curl -s http://localhost:8080/status | jq .health.queue_depth'

# Watch error rate
watch -n 1 'curl -s http://localhost:8080/metrics | jq .error_rate'

# Watch circuit breaker
watch -n 1 'curl -s http://localhost:8080/health | jq .circuit_state'

# Watch CPU/memory
make profile
```

---

## Troubleshooting

### Common Issues & Solutions

#### Issue: "Connection refused" on port 8080

**Cause**: Service not running or not started yet

**Solution**:
```bash
# Check if service is running
docker-compose ps

# If not running, start it
docker-compose up -d tps-system

# Wait for startup (20 seconds)
sleep 20
curl http://localhost:8080/health
```

#### Issue: Circuit breaker stuck in "Open" state

**Cause**: Too many failures or low reset timeout

**Solution**:
```bash
# Option 1: Wait for timeout (default 30 seconds)
sleep 35
curl http://localhost:8080/health

# Option 2: Increase timeout
export TPS_CB_TIMEOUT=60
docker-compose restart tps-system

# Option 3: Restart to reset
docker-compose restart tps-system
```

#### Issue: Queue growing unbounded

**Cause**: Worker pool too small, processing too slow

**Solution**:
```bash
# Increase worker count
export TPS_NUM_WORKERS=8
docker-compose restart tps-system

# Or reduce input load
# Check queue depth
curl http://localhost:8080/status | jq '.health.queue_depth'

# Check processing time
curl http://localhost:8080/metrics | jq '.avg_duration_ms'
```

#### Issue: High error rate (> 5%)

**Cause**: Processing errors, timeouts, or bugs

**Solution**:
```bash
# Check logs for error messages
make logs-system | grep ERROR

# Check circuit breaker status
curl http://localhost:8080/health | jq '.circuit_state'

# Reduce load to isolate issue
# Send single signal
curl -X POST http://localhost:8080/signal \
  -d '{"signal_type":"validate","payload":{}}'

# Check metrics
curl http://localhost:8080/metrics | jq '.error_rate'
```

#### Issue: "Port already in use"

**Cause**: Another service using the port

**Solution**:
```bash
# Find what's using port 8080
lsof -i :8080

# Stop the conflicting service
kill -9 <PID>

# Or use different port
export HTTP_PORT=8081
docker-compose up -d
```

#### Issue: Docker out of disk space

**Cause**: Volumes consuming too much space

**Solution**:
```bash
# Clean up unused volumes
docker volume prune

# Or remove specific volumes
docker-compose down -v

# Check space usage
docker system df
```

#### Issue: Services won't start (Docker error)

**Cause**: Resource constraints or image issues

**Solution**:
```bash
# Rebuild images
docker-compose build --no-cache

# Remove containers and restart
docker-compose down
docker-compose up -d

# Check Docker daemon status
docker info

# Increase Docker resources
# (System preferences → Resources → increase CPUs/RAM)
```

### Performance Troubleshooting

#### Latency increasing over time

**Cause**: Memory leak or queue accumulation

**Solution**:
```bash
# Monitor memory usage
docker stats tps-system

# Check queue depth
curl http://localhost:8080/status | jq '.health.queue_depth'

# Restart service to clear
docker-compose restart tps-system
```

#### CPU usage very high

**Cause**: Too much load or inefficient processing

**Solution**:
```bash
# Reduce load
make spike  # Stop this, if running

# Increase worker threads
export TPS_NUM_WORKERS=16
docker-compose restart tps-system

# Profile with CPU metrics
docker stats --no-stream tps-system
```

#### Metrics not appearing in Prometheus

**Cause**: Metrics endpoint not working or scrape config wrong

**Solution**:
```bash
# Test metrics endpoint
curl http://localhost:8080/metrics

# Check Prometheus scrape config
cat prometheus.yml | grep tps-system

# Trigger Prometheus scrape
curl 'http://localhost:9091/api/v1/admin/tsdb/snapshot'
```

### Database & Log Issues

#### Prometheus data missing

**Solution**:
```bash
# Check if volume mounted
docker inspect prometheus | grep Mounts

# Increase retention
# Edit prometheus.yml: --storage.tsdb.retention.time=30d

# Rebuild volume
docker-compose down -v
docker-compose up -d prometheus
```

#### Loki not receiving logs

**Solution**:
```bash
# Check Promtail scraping
docker-compose logs promtail | grep scrape

# Verify log source
docker inspect promtail | grep Mounts

# Reload config
docker-compose restart promtail
```

---

## Customization

### Adapting to Your Use Case

#### Custom Signal Types

Edit `crates/tps-reference/src/lib.rs`:

```rust
// In execute_signal method
match signal.signal_type.as_str() {
    "validate" => self.handle_validate(signal).await,
    "execute" => self.handle_execute(signal).await,
    "report" => self.handle_report(signal).await,
    "my_custom_type" => self.handle_custom(signal).await,  // Add this
    _ => Err(anyhow!("Unknown signal type: {}", signal.signal_type)),
}

// Add handler
async fn handle_custom(&self, signal: &WorkSignal) -> Result<ProcessingResult> {
    // Your custom logic here
    tokio::time::sleep(Duration::from_millis(100)).await;
    Ok(ProcessingResult {
        signal_id: signal.id.clone(),
        success: true,
        message: "Custom processing completed".to_string(),
        duration_ms: 100,
        worker_id: None,
        trace_id: signal.trace_id.clone(),
    })
}
```

#### Custom Metrics

Edit `examples/tps-reference-system/src/main.rs`:

```rust
// Add custom metrics in handlers
use metrics::{counter, gauge, histogram};

pub async fn post_signal(
    State(state): State<AppState>,
    Json(req): Json<SignalRequest>,
) -> impl IntoResponse {
    // Your custom metric
    counter!("custom.signals.received", "type" => req.signal_type.clone()).increment(1);

    // ... rest of handler
}
```

#### Custom Alerting

Create alert rules in Prometheus:

```yaml
# prometheus.yml
global:
  evaluation_interval: 15s

rule_files:
  - "custom-rules.yml"

alerting:
  alertmanagers:
    - static_configs:
        - targets:
            - localhost:9093  # Alertmanager
```

File `custom-rules.yml`:

```yaml
groups:
  - name: tps_alerts
    rules:
      - alert: HighErrorRate
        expr: tps_error_rate > 5
        for: 5m
        annotations:
          summary: "High error rate detected"

      - alert: CircuitBreakerOpen
        expr: tps_circuit_state == 1
        annotations:
          summary: "Circuit breaker is open"

      - alert: QueueBacklog
        expr: tps_queue_depth > 1000
        annotations:
          summary: "Queue backlog exceeding threshold"
```

#### Custom Dashboard

Create Grafana dashboard in `grafana/dashboards/custom.json`:

```json
{
  "dashboard": {
    "title": "Custom TPS Dashboard",
    "panels": [
      {
        "title": "Signal Processing Rate",
        "targets": [
          {
            "expr": "rate(tps_signals_processed[5m])"
          }
        ]
      }
    ]
  }
}
```

### Integration with External Systems

#### Send signals from external app

```bash
# Via HTTP
curl -X POST http://tps-system:8080/signal \
  -H "Content-Type: application/json" \
  -d '{"signal_type":"execute","payload":{"external_id":"123"}}'
```

#### Connect to external queue

```bash
# Edit Kanban to subscribe to RabbitMQ
# In crates/tps-reference/src/kanban.rs

async fn subscribe_rabbitmq(&self) -> Result<()> {
    let conn = lapin::Connection::connect(&self.rabbitmq_url).await?;
    let channel = conn.create_channel().await?;
    // ... subscribe logic
    Ok(())
}
```

#### Export metrics to external system

```bash
# Prometheus remote write
# Edit prometheus.yml:
remote_write:
  - url: "https://external-metrics.example.com/api/v1/write"
    bearer_token: "YOUR_TOKEN"
```

---

## Advanced Topics

### High Availability Setup

**Cluster Configuration**:
```yaml
# docker-compose-ha.yml
services:
  tps-system-1:
    # ... configuration
    environment:
      INSTANCE_ID: "1"

  tps-system-2:
    # ... configuration
    environment:
      INSTANCE_ID: "2"

  tps-system-3:
    # ... configuration
    environment:
      INSTANCE_ID: "3"

  load-balancer:
    image: nginx:alpine
    ports:
      - "8080:8080"
    volumes:
      - ./nginx.conf:/etc/nginx/nginx.conf
```

### Multi-Region Deployment

**Distributed trace correlation**:
```bash
# All regions send traces to central Jaeger
JAEGER_ENDPOINT=http://central-jaeger:14268/api/traces

# Tag traces by region
JAEGER_TAGS="region=us-east-1,env=prod"
```

### Security Hardening

```bash
# Enable TLS
SSL_ENABLED=true
SSL_CERT_PATH=/path/to/cert.pem
SSL_KEY_PATH=/path/to/key.pem

# API authentication
AUTH_ENABLED=true
AUTH_TOKEN=your-secure-token

# Rate limiting
RATE_LIMIT=1000  # requests per minute
```

### Performance Optimization

```bash
# Profile with perf
docker run --privileged -it \
  -v /sys/kernel/debug:/sys/kernel/debug \
  -v /home/user/ggen:/app \
  rust:latest \
  sh -c "cd /app && cargo install flamegraph && cargo flamegraph"

# View flame graph
open flamegraph.svg
```

### Cost Optimization

```bash
# Reduce resource usage
TPS_NUM_WORKERS=2        # Fewer workers
KANBAN_BUFFER=100        # Smaller buffer
ANDON_HISTORY=50         # Less history
METRICS_PORT=disabled    # Skip metrics if not needed

# Or use spot instances in cloud
# Edit docker-compose for cloud-native options
```

---

## References

### Official Documentation

- [Toyota Production System](https://en.wikipedia.org/wiki/Toyota_production_system)
- [Lean Manufacturing](https://en.wikipedia.org/wiki/Lean_manufacturing)
- [Kanban](https://en.wikipedia.org/wiki/Kanban_(development))
- [Circuit Breaker Pattern](https://martinfowler.com/bliki/CircuitBreaker.html)

### Technologies Used

- [Rust Language](https://www.rust-lang.org/)
- [Tokio Async Runtime](https://tokio.rs/)
- [Axum Web Framework](https://github.com/tokio-rs/axum)
- [NATS Messaging](https://nats.io/)
- [RabbitMQ](https://www.rabbitmq.com/)
- [Prometheus](https://prometheus.io/)
- [Grafana](https://grafana.com/)
- [Jaeger Tracing](https://www.jaegertracing.io/)
- [Loki Logs](https://grafana.com/loki/)

### Related Projects

- [ggen Repository](https://github.com/seanchatmangpt/ggen)
- [Cloud-Native Patterns](https://www.patterns.dev/posts/cloud-native/)
- [SRE Book](https://sre.google/sre-book/)

### Learning Resources

- TPS Case Studies: Toyota's success stories
- Lean Six Sigma: Combining lean and six sigma principles
- DevOps Culture: Continuous delivery and deployment
- Observability: Three pillars (metrics, logs, traces)

---

## Support & Contributing

### Getting Help

1. Check troubleshooting section above
2. Review logs: `make logs-system`
3. Check health status: `make health`
4. Open GitHub issue with:
   - Error message
   - Steps to reproduce
   - System info (OS, Docker version, hardware)
   - Relevant logs

### Contributing

1. Fork the repository
2. Create feature branch: `git checkout -b feature/my-improvement`
3. Add tests for new functionality
4. Ensure all tests pass: `cargo test`
5. Submit pull request with clear description

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | Jan 2026 | Initial release with all 6 TPS principles |

---

**Last Updated**: January 25, 2026
**Maintainer**: TPS Reference Team
**License**: MIT

