# TPS Reference System: Quick Start Guide

**Version**: 1.0
**Last Updated**: January 2026
**Purpose**: Hands-on learning system for TPS principles
**Estimated Setup Time**: 10 minutes
**Estimated Learning Time**: 2-4 hours

---

## Overview

The TPS Reference System is a Docker-based sample application that demonstrates all Toyota Production System principles:

- **Jidoka**: Circuit breaker prevents cascading failures
- **Kanban**: Queue-based worker pool with pull-based processing
- **Andon**: Real-time dashboards and distributed tracing
- **Kaizen**: SLO monitoring and continuous improvement
- **Heijunka**: Separate worker pools for fast/slow tasks
- **Value Stream**: Distributed tracing shows request flow

**What You'll Learn**:
1. How TPS systems work in practice
2. How to read dashboards and metrics
3. How to troubleshoot failures
4. How systems respond to load spikes
5. How circuit breakers protect systems

---

## Prerequisites

**Required**:
- Docker (version 20.10+)
- Docker Compose (version 2.0+)
- Make (or bash for running commands)
- 4GB free memory
- 10GB free disk space

**Optional**:
- curl (for testing)
- jq (for parsing JSON)

**System Check**:
```bash
# Verify Docker
docker --version  # Should be 20.10+

# Verify Docker Compose
docker-compose --version  # Should be 2.0+

# Verify memory
free -h  # Should show 4GB+ available

# Verify disk space
df -h /  # Should show 10GB+ available
```

---

## Installation

### Step 1: Clone or Copy Files

```bash
# If cloning from repository
git clone https://github.com/yourorg/tps-reference-system.git
cd tps-reference-system

# Or if you have a zip file
unzip tps-reference-system.zip
cd tps-reference-system

# Check if files are there
ls -la
# Should show: docker-compose.yml, Makefile, dashboards/, etc.
```

### Step 2: Start Services

```bash
# Start all services (will take 1-2 minutes)
docker-compose up -d

# Watch the logs to see services starting
docker-compose logs -f

# Should see messages like:
# api-1 | Started server on :8080
# worker-1 | Worker ready, processing tasks
# prometheus-1 | Server started
# grafana-1 | HTTP Server listening

# Wait until you see "All services ready" message (or stop with Ctrl+C)
```

### Step 3: Verify Services Are Running

```bash
# Check if all containers are running
docker ps

# Should show:
# api, worker-fast, worker-slow, nats-queue, prometheus, grafana, jaeger, etc.

# Verify each service
curl http://localhost:8080/health
# Should return: {"status":"healthy"}

# Check Grafana is ready
curl http://localhost:3000/api/health
# Should return: {"database":"ok","version":"X.X.X"}

# All good? You're ready to explore!
```

---

## Accessing Dashboards

### Grafana Dashboard (Real-Time Metrics)

**URL**: http://localhost:3000

**Default Login**:
- Username: `admin`
- Password: `admin`

**Dashboards to Explore**:
1. **TPS System Health**: Overview of entire system
   - Circuit breaker status
   - Queue depth
   - Worker utilization
   - Error rate
   - Latency distribution

2. **Worker Pools**: Detailed view of each worker type
   - Fast worker pool utilization
   - Slow worker pool utilization
   - Processing times by worker type

3. **Bottleneck Analysis**: Find what's slow
   - Latency breakdown by service
   - Database query performance
   - Network latency

4. **Capacity Planning**: Trend analysis
   - Load over time (last 24 hours)
   - Resource usage trends
   - Scaling events

**Dashboard Shortcuts**:
- Refresh: 5-second auto-refresh to see live updates
- Time range: Set to "Last 1 hour" or "Last 6 hours"
- Zoom: Click and drag on chart to zoom in

---

### Jaeger Traces (Distributed Tracing)

**URL**: http://localhost:16686

**How to Use**:
1. Select service: "api" or "worker"
2. Select operation: "ProcessRequest" or "ProcessItem"
3. Click "Find Traces"
4. Click a trace to see detailed breakdown

**What You'll See**:
- Request flow: api → database → worker → response
- Timing: How long each step took
- Errors: Which step failed (if any)

**Use Cases**:
- Latency spike? Find slow span in trace
- Error occurred? See exact point of failure
- Understanding flow: See request path

---

## First Load Test: Generate Traffic

### Simple Load Test (1 minute)

```bash
# Generate load for 1 minute at 100 req/sec
make load DURATION=60 RATE=100

# This will:
# - Send 100 requests per second
# - For 60 seconds (total 6000 requests)
# - Output progress every 10 seconds

# While this is running:
# 1. Open Grafana dashboard (http://localhost:3000)
# 2. Watch queue depth increase (should stay < 80%)
# 3. Watch worker utilization increase (should stay < 100%)
# 4. Watch throughput match incoming load
```

**What to Expect**:
- Queue depth: Increases to ~50% of max
- Workers: Scale up slightly (not much for 100 req/sec)
- Latency: Should stay stable (< 50ms p99)
- Error rate: Should stay low (< 0.1%)

---

### Burst Load Test (sudden spike)

```bash
# Generate burst: 500 req/sec for 10 seconds
make load DURATION=10 RATE=500

# Watch dashboard:
# 1. Queue depth jumps (should approach max)
# 2. Workers scale up rapidly
# 3. Latency might spike initially (recovers as workers scale)
# 4. Circuit breaker stays closed (system handling load)
```

**What You're Observing**:
- **Heijunka (Load Leveling)**: Auto-scaling spreads the load
- **Kanban (Pull-Based)**: Workers pull from queue at their own pace
- **Jidoka (Autonomation)**: System detects overload, scales up

---

### Stress Test (until breaking point)

```bash
# Stress test: Gradually increase load until system struggles
make stress-test

# This will:
# - Start at 100 req/sec
# - Increase by 50 req/sec every 30 seconds
# - Until system starts rejecting requests
# - Show at what point system breaks

# Watch:
# 1. Queue depth: Grows over time
# 2. Error rate: When does it spike?
# 3. Latency p99: When does it blow up?
# 4. Worker count: Does it scale fast enough?
# 5. Circuit breaker: Does it stay closed?
```

**What You'll Learn**:
- System capacity (max throughput before breaking)
- Scaling speed (how fast workers are added)
- Breaking behavior (graceful vs hard failure)

---

## Generate Failures: Chaos Testing

### Scenario 1: Downstream Service Fails

```bash
# Make downstream service (like database) slow/fail
make chaos-downstream LATENCY=5000

# This makes downstream service take 5 seconds per request
# (instead of normal 50ms)

# Watch dashboard:
# 1. Latency spikes (requests now take much longer)
# 2. Circuit breaker: Does it open?
# 3. Error rate: Do requests start failing?
# 4. Queue: Does it fill up?

# Questions to answer:
# - How long before circuit opens?
# - Does circuit protect the system?
# - What's the user experience?
```

**Stop chaos**:
```bash
make chaos-stop
# System should recover in 30-60 seconds
```

---

### Scenario 2: Worker Crashes

```bash
# Kill a worker (simulates worker crash)
make chaos-crash-worker

# This kills one worker container
# (Kubernetes would automatically restart it)

# Watch dashboard:
# 1. Worker count decreases
# 2. Queue for that worker drains
# 3. Requests go to remaining workers
# 4. Load rebalances

# Questions:
# - How quickly is queue drained?
# - Does system notice? (alerts?)
# - Can system recover?
```

---

### Scenario 3: Queue Backend Fails

```bash
# Stop NATS (queue system)
make chaos-queue-down

# This simulates queue backend being unavailable

# Watch dashboard:
# 1. All workers lose connection
# 2. Error rate jumps to 100%
# 3. Requests being rejected
# 4. Circuit breaker opens

# Questions:
# - How long before system recovers?
# - Can requests be retried?
# - Data loss?

# Restart queue
make chaos-queue-recover
```

---

### Scenario 4: Cascading Failure

```bash
# Simulate cascading failure:
# - Slow one service
# - See if it affects others

make chaos-cascade

# This simulates:
# 1. Service A becomes slow
# 2. Service B calls A, queue builds up
# 3. Service B becomes slow
# 4. Service C calls B, queue builds up
# 5. Entire system slow (cascade)

# Watch cascade propagate:
# 1. Service A latency spikes
# 2. Service B latency increases
# 3. Service C latency increases
# 4. User experience degrades

# See how circuit breakers help:
# - Circuit opens at appropriate point
# - Prevents full cascade
# - System gracefully degrades
```

---

## Exploring Dashboards

### Dashboard 1: System Health Overview

**Key Metrics**:
- **Circuit Breaker Status**: Green (closed) = healthy, Red (open) = failing
- **Queue Depth**: Shows work backlog
- **Worker Utilization**: Shows if system is busy or idle
- **Error Rate**: Shows percentage of failed requests
- **Latency**: Shows request processing time

**Reading Tips**:
1. All metrics green? System is healthy
2. Queue filling up? Workers can't keep up, scale them
3. Error rate spiking? Something broke, investigate logs
4. Latency spiking? Find bottleneck in traces

---

### Dashboard 2: Worker Pool Status

**What It Shows**:
- Fast worker pool: How many busy, how many idle
- Slow worker pool: How many busy, how many idle
- Queue for each pool: How many items waiting

**Interpretation**:
- If one pool idle while other is 100% busy: Imbalanced load
- If both pools high: System capacity reached
- If both pools low: System over-provisioned

---

### Dashboard 3: Latency Analysis

**Percentiles**:
- p50: 50% of requests faster than this
- p90: 90% of requests faster than this
- p99: 99% of requests faster than this (worst users)

**Interpretation**:
- p50 stable, p99 spiky: Some slow outliers (need to investigate)
- All percentiles increasing: System degrading
- p99 within 2-3x of p50: Good distribution
- p99 > 10x of p50: Very uneven (something is very slow sometimes)

---

## Exploring Traces

### Finding a Slow Trace

1. Open Jaeger (http://localhost:16686)
2. Select service: "api"
3. Select operation: "ProcessRequest"
4. Under "Tags": Set "span.kind=server"
5. In Latency range, set min = 100ms (find slow traces)
6. Click "Find Traces"
7. Click first trace (slowest)

**What You'll See**:
- Timeline shows each span
- Duration of each span
- Which span is slowest? That's your bottleneck

**Next Steps**:
- If database span is slow: Database is bottleneck
- If network span is slow: Network is bottleneck
- If compute span is slow: Algorithm/code is bottleneck

---

### Finding an Error Trace

1. Open Jaeger
2. Select service: "worker"
3. Filter by "error=true" (show only errors)
4. Click a trace
5. Look for red spans (indicate errors)
6. Check logs for error message

---

## Hands-On Exercises

### Exercise 1: Scale the System

**Goal**: Learn how auto-scaling works

**Steps**:
1. Start load test: `make load RATE=100`
2. Watch worker count in Grafana
3. Increase load: `make load RATE=200` (in different terminal)
4. Watch worker count increase
5. Reduce load: `make load RATE=50`
6. Watch worker count decrease

**Questions**:
- How long before workers scale?
- Do they scale up or down faster?
- What's the max worker count?

---

### Exercise 2: Find the Bottleneck

**Goal**: Use traces to find performance bottleneck

**Steps**:
1. Run normal load: `make load RATE=200`
2. Open Jaeger traces
3. Find slowest trace (longest duration)
4. Click on it, examine each span
5. Identify which span is slowest

**What You'll Find**:
- Database query might be 80% of latency
- Network call might be 10% of latency
- Processing might be 10% of latency

**Optimization opportunity**: Optimize the 80% (database)

---

### Exercise 3: Trigger Circuit Breaker

**Goal**: Understand how circuit breaker prevents cascading failure

**Steps**:
1. Check circuit status in Grafana (should be green)
2. Run chaos: `make chaos-downstream LATENCY=10000`
3. Watch circuit breaker in Grafana
4. When does it open? (should be within 30 seconds)
5. Check error rate: Does it spike before or after circuit opens?

**Observation**:
- Before circuit opens: Requests timing out (hanging)
- After circuit opens: Requests fail fast (rejected immediately)
- After circuit recovers: Requests succeed again

---

### Exercise 4: Diagnose Queue Issues

**Goal**: Understand queue behavior under stress

**Steps**:
1. Run stress test: `make stress-test`
2. Watch queue depth in Grafana
3. When does queue hit 100%?
4. What's the system response? (rejection rate, latency)
5. What load level is "good"? (queue < 80%)

**Capacity Planning**:
- At what load does queue hit 80%? (that's your practical limit)
- At what load does queue hit 100%? (that's your absolute limit)
- How many workers needed to handle peak load at 70% utilization?

---

### Exercise 5: Recovery Time

**Goal**: Measure how long system takes to recover from failure

**Steps**:
1. Baseline: Run load test, note normal latency
2. Trigger failure: `make chaos-downstream LATENCY=5000`
3. Time when: Circuit opens, error rate spikes, latency changes
4. Stop chaos: `make chaos-stop`
5. Time recovery: When does latency return to normal?
6. Record: Recovery time (RTO)

**Expected**:
- Circuit opens: ~30 seconds after failures start
- Recovery to normal: ~60 seconds after chaos stops

---

## Customization

### Changing Load Test Parameters

```bash
# Syntax
make load DURATION=<seconds> RATE=<req/sec>

# Examples
make load DURATION=300 RATE=100    # 5 minutes at 100 req/sec
make load DURATION=60 RATE=500     # 1 minute at 500 req/sec
make load DURATION=120 RATE=1000   # 2 minutes at 1000 req/sec
```

---

### Changing System Configuration

Edit `docker-compose.yml`:

```yaml
# Number of fast workers
services:
  worker-fast:
    environment:
      WORKER_COUNT: 20  # Was 10, now 20

# Circuit breaker threshold
  api:
    environment:
      CIRCUIT_BREAKER_THRESHOLD: 5  # Was 10, now 5 (more sensitive)

# Queue max size
  nats:
    environment:
      MAX_QUEUE_DEPTH: 2000  # Was 1000, now 2000 (more buffering)
```

After changes:
```bash
docker-compose down
docker-compose up -d
```

---

### Adding Custom Dashboards

1. Open Grafana (http://localhost:3000)
2. Click "Create" → "Dashboard"
3. Add panels for metrics you care about
4. Save dashboard
5. Export JSON (gear icon → "Share" → "Export")
6. Save to `dashboards/` folder

---

## Next Steps

### Learn More

**Recommended Reading**:
1. `/docs/tps-reference/handbook-tps-operations.md` - Complete operations guide
2. `/docs/tps-reference/tps-glossary.md` - Terminology
3. `/docs/tps-reference/tps-faq.md` - Common questions

### Get Certified

**TPS Operator Certification**:
- Level 1 (Awareness): Read handbook + take quiz
- Level 2 (Basic Operations): Hands-on with this system
- Level 3 (Advanced Operations): Complex scenarios + tuning

See `/docs/tps-reference/certification-tps-operator.md`

---

## Troubleshooting

### Services Won't Start

```bash
# Check Docker is running
docker --version

# Check logs
docker-compose logs

# Common issues:
# - Port 3000, 8080, 9090 in use (another service running)
# - Not enough memory (need 4GB)
# - Docker daemon not running

# Solution: Stop other services, restart Docker
docker system prune
docker-compose up -d
```

### Can't Access Grafana

```bash
# Wait 1-2 minutes for Grafana to start
docker logs grafana

# If still not working:
docker restart grafana

# Then try: http://localhost:3000
```

### No Metrics in Dashboard

```bash
# Prometheus might not have scraped yet (wait 30 seconds)
sleep 30

# Check Prometheus: http://localhost:9090
# Look for: api_requests_total, api_latency_seconds, etc.

# If still no metrics, restart Prometheus:
docker restart prometheus
```

### Load Test Not Working

```bash
# Check if API is running
curl http://localhost:8080/health

# Check API logs
docker logs api

# If API down, restart it:
docker restart api
```

---

## Cleanup

```bash
# Stop all services (but keep data)
docker-compose down

# Stop and delete everything
docker-compose down -v

# Remove images (free disk space)
docker rmi <image-name>
```

---

## Support

**Questions or Issues?**
- Check FAQ: `/docs/tps-reference/tps-faq.md`
- Check Handbook: `/docs/tps-reference/handbook-tps-operations.md`
- Create GitHub issue: https://github.com/yourorg/tps-reference-system/issues

---

**End of TPS Reference System README**

Happy learning!
