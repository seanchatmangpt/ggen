<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [TPS Hands-On Lab Exercises](#tps-hands-on-lab-exercises)
  - [Lab Prerequisites](#lab-prerequisites)
  - [Lab 1: Jidoka (Autonomation with Human Touch)](#lab-1-jidoka-autonomation-with-human-touch)
    - [1.1: Baseline - Healthy System (5 minutes)](#11-baseline---healthy-system-5-minutes)
    - [1.2: Inject Failure - Downstream Broken (5 minutes)](#12-inject-failure---downstream-broken-5-minutes)
    - [1.3: Understand the Metrics (10 minutes)](#13-understand-the-metrics-10-minutes)
    - [1.4: Recovery - Restore Downstream (5 minutes)](#14-recovery---restore-downstream-5-minutes)
    - [Lab 1 Summary](#lab-1-summary)
  - [Lab 2: Kanban (Pull-Based Work)](#lab-2-kanban-pull-based-work)
    - [2.1: Baseline - Normal Queue (5 minutes)](#21-baseline---normal-queue-5-minutes)
    - [2.2: Load Test - Fill Queue (10 minutes)](#22-load-test---fill-queue-10-minutes)
    - [2.3: Queue Limit Test - Exceed Capacity (5 minutes)](#23-queue-limit-test---exceed-capacity-5-minutes)
    - [2.4: Understand Queue Dynamics (5 minutes)](#24-understand-queue-dynamics-5-minutes)
    - [Lab 2 Summary](#lab-2-summary)
  - [Lab 3: Andon (Problem Visibility)](#lab-3-andon-problem-visibility)
    - [3.1: Baseline - All Metrics Green (5 minutes)](#31-baseline---all-metrics-green-5-minutes)
    - [3.2: Introduce Problem - Slow Downstream (5 minutes)](#32-introduce-problem---slow-downstream-5-minutes)
    - [3.3: Alert Setup - Configure Threshold (5 minutes)](#33-alert-setup---configure-threshold-5-minutes)
    - [3.4: Trace Analysis - Find Root Cause (10 minutes)](#34-trace-analysis---find-root-cause-10-minutes)
    - [3.5: Restore and Verify (5 minutes)](#35-restore-and-verify-5-minutes)
    - [Lab 3 Summary](#lab-3-summary)
  - [Lab 4: Kaizen (Continuous Improvement)](#lab-4-kaizen-continuous-improvement)
    - [4.1: Baseline Metrics Collection (5 minutes)](#41-baseline-metrics-collection-5-minutes)
    - [4.2: Identify Bottleneck (5 minutes)](#42-identify-bottleneck-5-minutes)
    - [4.3: Test Hypothesis - Add More Workers (10 minutes)](#43-test-hypothesis---add-more-workers-10-minutes)
    - [4.4: Calculate Improvement (5 minutes)](#44-calculate-improvement-5-minutes)
    - [4.5: Revert and Reflect (5 minutes)](#45-revert-and-reflect-5-minutes)
    - [Lab 4 Summary](#lab-4-summary)
  - [Lab 5: Heijunka (Level Loading / Leveled Pull)](#lab-5-heijunka-level-loading--leveled-pull)
    - [5.1: Baseline - Uneven Load Distribution (5 minutes)](#51-baseline---uneven-load-distribution-5-minutes)
    - [5.2: Observe Heijunka Rebalancing (10 minutes)](#52-observe-heijunka-rebalancing-10-minutes)
    - [5.3: Understand Rebalancing Algorithm (10 minutes)](#53-understand-rebalancing-algorithm-10-minutes)
    - [5.4: Simulate Uneven Work - Long vs Short Tasks (5 minutes)](#54-simulate-uneven-work---long-vs-short-tasks-5-minutes)
    - [Lab 5 Summary](#lab-5-summary)
  - [Lab 6: Tracing (Observability through Distributed Tracing)](#lab-6-tracing-observability-through-distributed-tracing)
    - [6.1: Send Signal and Examine Trace (5 minutes)](#61-send-signal-and-examine-trace-5-minutes)
    - [6.2: Analyze Trace Structure (10 minutes)](#62-analyze-trace-structure-10-minutes)
    - [6.3: Find Bottleneck in Trace (10 minutes)](#63-find-bottleneck-in-trace-10-minutes)
    - [6.4: Trace Correlation - Follow Signal Through System (5 minutes)](#64-trace-correlation---follow-signal-through-system-5-minutes)
    - [6.5: Use Traces for Debugging (5 minutes)](#65-use-traces-for-debugging-5-minutes)
    - [Lab 6 Summary](#lab-6-summary)
  - [All Labs Complete!](#all-labs-complete)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# TPS Hands-On Lab Exercises

**Version**: 1.0
**Duration**: 30 minutes per lab (180 minutes total for all 6)
**Audience**: SRE/DevOps engineers (TPS system running)
**Goal**: Learn each TPS principle through practical experimentation

---

## Lab Prerequisites

Before starting ANY lab:

```bash
# Verify system is running
kubectl get pods -n tps-system | grep Running
# Should show at least 10 pods running

# Port-forward API and Grafana
kubectl port-forward -n tps-system svc/tps-api 8080:8080 &
kubectl port-forward -n tps-system svc/grafana 3000:3000 &
kubectl port-forward -n tps-system svc/prometheus 9090:9090 &

# Test API is responsive
curl -s http://localhost:8080/health | jq .status
# Should show: "healthy"
```

---

## Lab 1: Jidoka (Autonomation with Human Touch)

**Duration**: 30 minutes
**Principle**: System detects problems automatically and stops gracefully
**What You'll Learn**: Circuit breaker behavior, fault isolation, fail-fast

### 1.1: Baseline - Healthy System (5 minutes)

```bash
# Check circuit breaker status
curl -s http://localhost:8080/status | jq .jidoka
# Output should show:
# {
#   "circuit_breaker": "Closed",
#   "health_checks": {...},
#   "failures": 0
# }

# Send 5 healthy signals
for i in {1..5}; do
  curl -s -X POST http://localhost:8080/signal \
    -H "Content-Type: application/json" \
    -d '{"signal_type":"execute","payload":{"task":"healthy-'$i'"}}' | jq .signal_id
done

# In Grafana, watch TPS Overview dashboard
# Metrics should show:
# - Circuit breaker state: 0 (Closed)
# - Processing latency p99: < 50ms
# - Success rate: 100%
```

**Observation**: All signals process successfully. Circuit is Closed.

---

### 1.2: Inject Failure - Downstream Broken (5 minutes)

```bash
# Simulate downstream service failure
kubectl patch configmap tps-config -n tps-system --type merge -p \
  '{"data":{"downstream_enabled":"false"}}'

# Pod will restart with new config (wait ~30 seconds)
kubectl rollout status deployment/tps-system -n tps-system --timeout=2m

# Try to send signals now
for i in {1..5}; do
  curl -s -X POST http://localhost:8080/signal \
    -H "Content-Type: application/json" \
    -d '{"signal_type":"execute","payload":{"task":"will-fail-'$i'"}}' | jq '.status'
done

# Check circuit breaker status
curl -s http://localhost:8080/status | jq .jidoka.circuit_breaker
# Should show: "Open"
```

**Observation**: Circuit breaker opened automatically. New signals rejected (fail-fast).

---

### 1.3: Understand the Metrics (10 minutes)

Go to Grafana (http://localhost:3000 > TPS Overview dashboard) and observe:

**Metric 1: Circuit Breaker State**
- 0 = Closed (accepting work)
- 1 = Open (rejecting work)
- Your graph should jump from 0 to 1 when you enabled downstream_enabled=false

**Metric 2: Jidoka Rejections**
- Count of signals rejected by circuit breaker
- Should increase as you send signals while circuit is Open

**Metric 3: Dead Letter Queue (DLQ)**
- Count of signals that couldn't be processed
- DLQ acts as "hold area" for broken signals

**Question 1**: Why does system reject immediately instead of queueing?
> Answer: If downstream is broken, queueing work just accumulates. Better to reject immediately (fail-fast) so caller can retry elsewhere or alert user.

**Question 2**: How long does circuit take to check downstream?
> Answer: ~100ms (single health check). Check latency in traces.

**Question 3**: Why not retry automatically?
> Answer: That's for caller to decide. System's job is to detect failure and signal it clearly.

---

### 1.4: Recovery - Restore Downstream (5 minutes)

```bash
# Restore downstream service
kubectl patch configmap tps-config -n tps-system --type merge -p \
  '{"data":{"downstream_enabled":"true"}}'

# Pod will restart (wait ~30 seconds)
kubectl rollout status deployment/tps-system -n tps-system --timeout=2m

# Try signals again
for i in {1..5}; do
  curl -s -X POST http://localhost:8080/signal \
    -H "Content-Type: application/json" \
    -d '{"signal_type":"execute","payload":{"task":"recovery-'$i'"}}' | jq '.status'
done

# Check circuit breaker
curl -s http://localhost:8080/status | jq .jidoka.circuit_breaker
# Should show: "Closed"
```

**Observation**: Circuit automatically closes when downstream recovers. No manual intervention needed.

---

### Lab 1 Summary

| Scenario | Circuit State | Signals Accepted | Signals Rejected |
|----------|---------------|------------------|------------------|
| Healthy downstream | Closed | 5/5 | 0 |
| Broken downstream | Open | 0/5 | 5 |
| Recovery | Closed | 5/5 | 0 |

**Key Learning**: Jidoka stops the line automatically. No manual circuit breaker resets. System self-heals.

---

## Lab 2: Kanban (Pull-Based Work)

**Duration**: 30 minutes
**Principle**: Workers pull work based on capacity; queue has fixed limit
**What You'll Learn**: Queue depth, backpressure, worker pool sizing

### 2.1: Baseline - Normal Queue (5 minutes)

```bash
# Check current queue configuration
curl -s http://localhost:8080/status | jq .kanban
# Output shows:
# {
#   "queue_max_size": 100,
#   "queue_current_depth": 0,
#   "workers": 2,
#   "processing_rate": 5.2  (signals/second)
# }

# Measure processing rate (baseline)
TIME_START=$(date +%s%3N)
for i in {1..10}; do
  curl -s -X POST http://localhost:8080/signal \
    -H "Content-Type: application/json" \
    -d '{"signal_type":"execute","payload":{"task":"baseline-'$i'"}}' > /dev/null
done
TIME_END=$(date +%s%3N)
ELAPSED=$((TIME_END - TIME_START))
echo "Processed 10 signals in $ELAPSED ms"
# Should be ~200-400ms (20-50 signals/second)
```

**Observation**: Queue is small, processing is fast. Workers can keep up.

---

### 2.2: Load Test - Fill Queue (10 minutes)

```bash
# Send 80 signals rapidly (faster than workers can process)
for i in {1..80}; do
  curl -s -X POST http://localhost:8080/signal \
    -H "Content-Type: application/json" \
    -d '{"signal_type":"execute","payload":{"task":"load-test-'$i'"}}' &
done
wait  # Wait for all to finish

# Watch queue depth fill up
for i in {1..20}; do
  DEPTH=$(curl -s http://localhost:8080/status | jq .kanban.queue_current_depth)
  echo "Queue depth: $DEPTH"
  sleep 1
done

# In Grafana, observe:
# - Queue depth graph (should ramp up, then settle at ~60% full)
# - Processing latency p99 (should increase as queue grows)
# - Worker utilization (should spike to 100%)
```

**Observation**: Queue fills as signals arrive faster than workers can process.

---

### 2.3: Queue Limit Test - Exceed Capacity (5 minutes)

```bash
# Send 200 signals to exceed max queue size (100)
failed=0
succeeded=0
for i in {1..200}; do
  RESPONSE=$(curl -s -X POST http://localhost:8080/signal \
    -H "Content-Type: application/json" \
    -d '{"signal_type":"execute","payload":{"task":"overflow-'$i'"}}')

  STATUS=$(echo "$RESPONSE" | jq -r '.status')
  if [ "$STATUS" = "accepted" ]; then
    ((succeeded++))
  elif [ "$STATUS" = "rejected" ]; then
    ((failed++))
  fi
done

echo "Signals accepted: $succeeded"
echo "Signals rejected: $failed"
# Expected: ~100 accepted (filled queue), ~100 rejected (queue full)
```

**Observation**: Once queue hits limit (100), system rejects new work (backpressure).

---

### 2.4: Understand Queue Dynamics (5 minutes)

Go to Grafana (http://localhost:3000 > Kanban dashboard) and observe:

**Metric 1: Queue Depth Over Time**
- Your graph should show ramp-up, plateau near 100, then gradual drain
- Plateau occurs when incoming rate = processing rate

**Metric 2: Processing Rate (signals/sec)**
- Should stay constant (~5-10 signals/sec regardless of queue size)
- This is processing_rate = workers × work_per_second_per_worker

**Metric 3: Rejection Rate**
- Should jump when queue hits max
- Rejection rate = incoming_rate - processing_rate (when queue is full)

**Question 1**: Why reject instead of queue more?
> Answer: If you queue more, latency goes up (poor user experience). Better to reject quickly and let caller retry.

**Question 2**: What's the "sweet spot" queue depth?
> Answer: 40-60% full. Empty = idle workers (waste). Full = long latency (poor experience).

**Question 3**: How do you control queue depth?
> Answer: Adjust workers (more workers = faster processing = smaller queue needed).

---

### Lab 2 Summary

| Phase | Queue Depth | Processing Rate | Result |
|-------|-------------|-----------------|--------|
| Baseline | 0-5 | 20 sig/sec | All accepted |
| Load test | 40-60 | 20 sig/sec | Backpressure starts |
| Overflow | 100 | 20 sig/sec | Rejections occur |

**Key Learning**: Queue is the release valve. When it fills, system rejects work gracefully (backpressure), not overloads.

---

## Lab 3: Andon (Problem Visibility)

**Duration**: 30 minutes
**Principle**: Make problems visible immediately
**What You'll Learn**: Metrics interpretation, alert setup, trace analysis

### 3.1: Baseline - All Metrics Green (5 minutes)

```bash
# Check current state
curl -s http://localhost:8080/metrics | grep -E "tps_" | head -20
# Should show:
# - tps_signal_latency_p99 < 100ms
# - tps_signal_latency_p50 < 50ms
# - tps_error_rate 0 (0%)
# - tps_circuit_breaker_state 0 (Closed)

# In Prometheus (http://localhost:9090), run:
# Query: tps_signal_success_rate
# Result should be: ~1.0 (100%)

# In Grafana Andon dashboard, observe:
# - All lights are Green (everything healthy)
# - Error rate card shows 0%
# - Circuit breaker shows Closed
```

**Observation**: All signals green, no problems visible.

---

### 3.2: Introduce Problem - Slow Downstream (5 minutes)

```bash
# Simulate slow downstream (add 500ms latency)
kubectl patch configmap tps-config -n tps-system --type merge -p \
  '{"data":{"downstream_latency_ms":"500"}}'

# Restart pods
kubectl rollout restart deployment/tps-system -n tps-system
kubectl rollout status deployment/tps-system -n tps-system --timeout=2m

# Send signals
for i in {1..10}; do
  curl -s -X POST http://localhost:8080/signal \
    -H "Content-Type: application/json" \
    -d '{"signal_type":"execute","payload":{"task":"slow-'$i'"}}' > /dev/null
done

# Observe metrics change
curl -s http://localhost:8080/metrics | grep tps_signal_latency_p99
# Should show > 500ms now

# In Grafana Andon dashboard:
# - Red light appears (problem detected)
# - Latency card shows > 500ms
# - P99 latency bar is now red
```

**Observation**: Problem automatically detected and signaled in dashboard.

---

### 3.3: Alert Setup - Configure Threshold (5 minutes)

```bash
# Edit Prometheus alert rules
kubectl edit configmap prometheus-rules -n tps-system

# Add/verify this alert rule exists:
# - alert: HighLatency
#   expr: tps_signal_latency_p99 > 200  (milliseconds)
#   for: 1m  (must be high for 1 minute)
#   annotations:
#     summary: "P99 latency exceeds threshold"

# Verify alert is firing
curl -s http://localhost:9090/api/v1/alerts | jq '.data.alerts[] | select(.labels.alertname=="HighLatency")'
# Should show an alert firing

# In Grafana, go to Alerting > Alert Rules
# Click "HighLatency" rule
# Status should show "Firing"
```

**Observation**: Alert automatically fires when metric exceeds threshold.

---

### 3.4: Trace Analysis - Find Root Cause (10 minutes)

```bash
# Open Jaeger (http://localhost:16686)
# Find a trace from the last minute
# In "Service" dropdown, select "tps-system"
# In "Operations", select "ProcessSignal"
# Click a trace

# Trace should show:
# 1. API.ReceiveSignal (1-5ms)
# 2. Kanban.Enqueue (2-10ms)
# 3. Worker.ProcessSignal (510-520ms) ← This is where delay is!
# 4. Jidoka.Check (1-5ms)
# 5. Downstream.Call (500-505ms) ← HERE is the root cause!
# 6. Worker.ProcessSignal returns
# 7. Kaizen.RecordMetric (1-2ms)
# 8. Heijunka.UpdateLoad (1-2ms)

# Key insight: Span for "Downstream.Call" is 500ms
# That's the slow downstream you configured
```

**Observation**: Traces show exact location of problem (downstream latency).

---

### 3.5: Restore and Verify (5 minutes)

```bash
# Remove the latency injection
kubectl patch configmap tps-config -n tps-system --type merge -p \
  '{"data":{"downstream_latency_ms":"0"}}'

# Restart pods
kubectl rollout restart deployment/tps-system -n tps-system
kubectl rollout status deployment/tps-system -n tps-system --timeout=2m

# Send signals
for i in {1..10}; do
  curl -s -X POST http://localhost:8080/signal \
    -H "Content-Type: application/json" \
    -d '{"signal_type":"execute","payload":{"task":"recovered-'$i'"}}' > /dev/null
done

# Verify metrics back to normal
curl -s http://localhost:8080/metrics | grep tps_signal_latency_p99
# Should be < 100ms again

# In Grafana Andon dashboard:
# - Red light turns Green
# - Alert status changes to "Not Firing"
```

**Observation**: Problem automatically cleared when root cause fixed.

---

### Lab 3 Summary

**Three Layers of Visibility**:
1. **Metrics**: Red light when P99 latency > 200ms
2. **Alerts**: Notification when problem persists > 1 min
3. **Traces**: Detailed view showing exact bottleneck location

**Key Learning**: Don't just have metrics. Have alerts. Don't just have alerts. Have traces. Traces show WHERE (not just THAT) problem exists.

---

## Lab 4: Kaizen (Continuous Improvement)

**Duration**: 30 minutes
**Principle**: Collect data, analyze patterns, improve systematically
**What You'll Learn**: Metrics collection, percentile analysis, bottleneck identification

### 4.1: Baseline Metrics Collection (5 minutes)

```bash
# Kaizen collects metrics automatically. Let's examine them.

# Raw percentile data
curl -s http://localhost:8080/status | jq .kaizen
# Output shows:
# {
#   "percentile_p50": 25.3,
#   "percentile_p90": 47.2,
#   "percentile_p99": 89.4,
#   "avg": 35.6,
#   "max": 125.4,
#   "samples": 247
# }

# Generate normal load (baseline)
timeout 60 bash -c 'while true; do curl -s -X POST http://localhost:8080/signal -H "Content-Type: application/json" -d "{\"signal_type\":\"execute\",\"payload\":{\"task\":\"baseline\"}}" > /dev/null; done &'

# Wait 60 seconds for data collection
sleep 60

# Capture baseline metrics
BASELINE=$(curl -s http://localhost:8080/status | jq .kaizen)
echo "Baseline metrics: $BASELINE"
```

**Observation**: System captures latency distribution (p50, p90, p99).

---

### 4.2: Identify Bottleneck (5 minutes)

```bash
# Where is time being spent?
curl -s http://localhost:8080/status | jq .kaizen.component_breakdown
# Output shows breakdown like:
# {
#   "api_receive": 1.2,         # API receives signal
#   "enqueue": 2.5,              # Queue enqueue
#   "dequeue_wait": 12.3,        # Wait for worker (queue time!)
#   "process_work": 15.4,        # Actual work
#   "jidoka_check": 2.1,         # Fault check
#   "metrics_record": 1.2        # Metric recording
# }
# Total: 1.2 + 2.5 + 12.3 + 15.4 + 2.1 + 1.2 = 34.7ms

# Analysis:
# - dequeue_wait (12.3ms) + process_work (15.4ms) = 27.7ms of 34.7ms (80%)
# - Could optimize both worker pool (reduce queue wait) or process logic (reduce work time)
```

**Observation**: Component breakdown shows where time is spent.

---

### 4.3: Test Hypothesis - Add More Workers (10 minutes)

**Hypothesis**: If we add workers, queue wait time will decrease.

```bash
# Current setup: 2 workers
# Check current configuration
kubectl get deployment tps-system -n tps-system -o jsonpath='{.spec.replicas}'
# Output: 2

# Scale to 4 workers
kubectl scale deployment tps-system -n tps-system --replicas=4
kubectl rollout status deployment/tps-system -n tps-system --timeout=2m

# Generate load for 60 seconds
timeout 60 bash -c 'while true; do curl -s -X POST http://localhost:8080/signal -H "Content-Type: application/json" -d "{\"signal_type\":\"execute\",\"payload\":{\"task\":\"with-4-workers\"}}" > /dev/null; done &'

sleep 60

# Capture new metrics
NEW_METRICS=$(curl -s http://localhost:8080/status | jq .kaizen)
echo "With 4 workers: $NEW_METRICS"

# Compare component breakdown
# dequeue_wait should be LOWER (less queue wait with more workers)
# process_work should be similar (same amount of work)
```

**Observation**: With 4 workers, queue wait decreased but total work time is same.

---

### 4.4: Calculate Improvement (5 minutes)

```bash
# Extract latency percentiles before/after

# Before (2 workers): p99 = 89.4ms
# After (4 workers):  p99 = ? (should be ~60-70ms)

# Improvement = (89.4 - 65) / 89.4 = 27% faster

# Check in Prometheus (http://localhost:9090)
# Run query:
# rate(tps_signal_latency_sum[1m]) / rate(tps_signal_latency_count[1m])
# Graph should show latency decrease over time

# In Grafana, the Kaizen dashboard should show:
# - P50, P90, P99 all decreased
# - Worker utilization increased (from ~50% to ~75%)
# - Queue depth decreased (faster processing)
```

**Observation**: More workers → less queue wait → faster latency.

---

### 4.5: Revert and Reflect (5 minutes)

```bash
# Scale back to 2 workers
kubectl scale deployment tps-system -n tps-system --replicas=2
kubectl rollout status deployment/tps-system -n tps-system --timeout=2m

# Question: Should we keep 4 workers permanently?
# Answer: Consider:
# 1. Cost: 2x more pod resources
# 2. Improvement: 27% latency reduction
# 3. Baseline: Were users complaining about 89ms latency?
# 4. Trade-off: Better latency vs higher cost

# This is kaizen: measure, improve, decide, repeat
```

**Observation**: Kaizen isn't about max performance. It's about smart tradeoffs.

---

### Lab 4 Summary

**Kaizen Workflow**:
1. **Collect**: Measure p50, p90, p99, component breakdown
2. **Analyze**: Identify where time is spent
3. **Hypothesis**: "Adding workers will reduce latency"
4. **Test**: Scale to 4 workers, measure again
5. **Decide**: Is improvement worth the cost?
6. **Document**: Store decision (why we run 2 workers, not 4)

**Key Learning**: Kaizen is data-driven improvement, not random tuning.

---

## Lab 5: Heijunka (Level Loading / Leveled Pull)

**Duration**: 30 minutes
**Principle**: Distribute work evenly across workers to prevent peaks and valleys
**What You'll Learn**: Worker pool balancing, load distribution, SLA stability

### 5.1: Baseline - Uneven Load Distribution (5 minutes)

```bash
# Send 20 signals as fast as possible
for i in {1..20}; do
  curl -s -X POST http://localhost:8080/signal \
    -H "Content-Type: application/json" \
    -d '{"signal_type":"execute","payload":{"task":"burst-'$i'"}}' &
done
wait

# Check heijunka status
curl -s http://localhost:8080/status | jq .heijunka
# Output shows:
# {
#   "worker_loads": [45, 52],  # Uneven: one worker has 45, other has 52
#   "imbalance_factor": 0.12,   # 12% imbalance
#   "avg_per_worker": 48.5,
#   "max_per_worker": 52,
#   "min_per_worker": 45,
#   "rebalance_attempts": 0
# }

# In Grafana Heijunka dashboard:
# - Bar chart shows uneven heights (some workers busy, others less so)
# - "Imbalance Factor" shows 12%
```

**Observation**: Signals are distributed unevenly across workers.

---

### 5.2: Observe Heijunka Rebalancing (10 minutes)

```bash
# Heijunka automatically rebalances work every 30 seconds
# Let's watch it happen

# Send another burst
for i in {1..30}; do
  curl -s -X POST http://localhost:8080/signal \
    -H "Content-Type: application/json" \
    -d '{"signal_type":"execute","payload":{"task":"rebalance-test-'$i'"}}' &
done
wait

# Watch worker loads change over 90 seconds
for i in {1..9}; do
  LOADS=$(curl -s http://localhost:8080/status | jq .heijunka.worker_loads)
  IMBALANCE=$(curl -s http://localhost:8080/status | jq .heijunka.imbalance_factor)
  echo "Time $((i*10))s - Loads: $LOADS, Imbalance: $IMBALANCE"
  sleep 10
done

# Graph in Grafana should show:
# - Imbalance spike when you send burst
# - Gradual smoothing as heijunka rebalances
# - Worker loads converge toward equal distribution
```

**Observation**: Heijunka automatically moves work to less-busy workers.

---

### 5.3: Understand Rebalancing Algorithm (10 minutes)

```bash
# Heijunka uses a simple algorithm:
# 1. Every 30 seconds, measure worker load
# 2. Calculate: imbalance = (max - min) / avg
# 3. If imbalance > threshold (5%), rebalance
# 4. Move work from busier workers to less-busy ones
# 5. Goal: imbalance < 5%

# View detailed logs
kubectl logs -l app=tps-system -n tps-system --tail=50 | grep -i heijunka
# Should show lines like:
# [heijunka] Rebalance triggered: imbalance=0.12 > threshold=0.05
# [heijunka] Moving 3 items from worker-0 to worker-1
# [heijunka] New imbalance: 0.03

# Key insight: Heijunka doesn't wait for human intervention
# It continuously self-heals worker distribution
```

**Observation**: Heijunka is automatic and continuous.

---

### 5.4: Simulate Uneven Work - Long vs Short Tasks (5 minutes)

```bash
# Send mix of fast and slow tasks
# Fast tasks: execute immediately
# Slow tasks: take 100ms

for i in {1..20}; do
  if [ $((i % 3)) -eq 0 ]; then
    # Slow task
    LATENCY=100
  else
    # Fast task
    LATENCY=5
  fi

  curl -s -X POST http://localhost:8080/signal \
    -H "Content-Type: application/json" \
    -d "{\"signal_type\":\"execute\",\"payload\":{\"task\":\"mixed-'$i'\",\"latency_ms\":$LATENCY}}" &
done
wait

# Check worker loads
curl -s http://localhost:8080/status | jq .heijunka
# Even though some tasks are slower, heijunka balances by COUNT
# Worker with more slow tasks might show higher cumulative latency
# But by item count, they should be balanced

# Question: How does heijunka balance work by count if tasks have different durations?
# Answer: It doesn't perfectly. For perfect balance, would need to weight by duration.
# Current implementation balances by item count (simpler, good enough).
```

**Observation**: Heijunka balances by work count, not by time.

---

### Lab 5 Summary

| Scenario | Imbalance Factor | Result |
|----------|------------------|--------|
| Baseline | 12% | Uneven load |
| After 1 rebalance cycle | 8% | Better |
| After 2 rebalance cycles | 3% | Leveled |

**Key Learning**: Heijunka prevents worker starvation. If one worker is overloaded, work flows to less-busy workers automatically.

---

## Lab 6: Tracing (Observability through Distributed Tracing)

**Duration**: 30 minutes
**Principle**: Every signal creates a complete trace showing its journey
**What You'll Learn**: Trace structure, span relationships, latency attribution

### 6.1: Send Signal and Examine Trace (5 minutes)

```bash
# Send a test signal and capture its ID
SIGNAL_RESPONSE=$(curl -s -X POST http://localhost:8080/signal \
  -H "Content-Type: application/json" \
  -d '{"signal_type":"execute","payload":{"task":"trace-test-1"}}')

SIGNAL_ID=$(echo "$SIGNAL_RESPONSE" | jq -r '.signal_id')
TRACE_ID=$(echo "$SIGNAL_RESPONSE" | jq -r '.trace_id')

echo "Signal ID: $SIGNAL_ID"
echo "Trace ID: $TRACE_ID"

# Open Jaeger: http://localhost:16686
# In Service dropdown: select "tps-system"
# In "Trace ID" field: paste $TRACE_ID
# Click search
```

**Observation**: Trace shows complete journey of one signal.

---

### 6.2: Analyze Trace Structure (10 minutes)

A complete trace should have this structure:

```
ProcessSignal (total: ~50ms) [RootSpan]
├── ReceiveSignal (1-5ms)
│   └── ParseRequest (0.5-1ms)
├── Kanban.Enqueue (2-10ms)
│   └── CheckQueueCapacity (0.5-1ms)
├── Worker.Dequeue (1-5ms)
│   └── WaitForWork (0-20ms, varies)
├── ProcessWork (10-30ms)
│   ├── ValidateSignal (1-2ms)
│   ├── ExecuteTask (5-25ms) [where actual work happens]
│   └── RecordResult (1-2ms)
├── Jidoka.Check (1-5ms)
│   └── CallDownstream (1-5ms)
├── Kaizen.RecordMetric (1-2ms)
├── Heijunka.UpdateLoad (1-2ms)
└── ReturnResponse (1-2ms)
```

**In Jaeger**, click on the root span "ProcessSignal" and observe:

```
Span name               | Duration  | Service
-----------------------|-----------|--------
ProcessSignal          | 50ms      | tps-system
├─ ReceiveSignal       | 3ms       | tps-api
├─ Kanban.Enqueue      | 5ms       | tps-queue
├─ Worker.Dequeue      | 15ms      | tps-worker
├─ ProcessWork         | 20ms      | tps-worker
├─ Jidoka.Check        | 3ms       | tps-jidoka
├─ Kaizen.RecordMetric | 2ms       | tps-metrics
├─ Heijunka.UpdateLoad | 1ms       | tps-heijunka
└─ ReturnResponse      | 1ms       | tps-api
```

Each span has:
- **Name**: What operation
- **Duration**: How long
- **Service**: Which component
- **Tags**: Key metadata (user_id, task_type, etc.)
- **Logs**: Events during execution (circuit_opened, retry_attempt, etc.)

---

### 6.3: Find Bottleneck in Trace (10 minutes)

```bash
# Send signals with different types to compare traces
for task_type in quick slow; do
  SIGNAL=$(curl -s -X POST http://localhost:8080/signal \
    -H "Content-Type: application/json" \
    -d "{\"signal_type\":\"execute\",\"payload\":{\"task\":\"$task_type\",\"type\":\"$task_type\"}}")

  TRACE_ID=$(echo "$SIGNAL" | jq -r '.trace_id')
  echo "$task_type task: $TRACE_ID"
done
```

**In Jaeger**:
1. Open first trace (quick task)
   - Look at "ProcessWork" span duration (should be ~10ms)
2. Open second trace (slow task)
   - Look at "ProcessWork" span duration (should be ~100ms)
3. Compare the two

**Question**: Which component takes longest?
- Use "Service" column to filter:
- For quick task: probably "ReceiveSignal" (relative)
- For slow task: definitely "ProcessWork" (absolute)

**Lesson**: Traces show WHERE time is spent. Metrics show THAT it's slow. Traces show WHY.

---

### 6.4: Trace Correlation - Follow Signal Through System (5 minutes)

```bash
# Key insight: Single trace_id travels with signal through entire system
# All components add spans to same trace

# Send signal
SIGNAL=$(curl -s -X POST http://localhost:8080/signal \
  -H "Content-Type: application/json" \
  -d '{"signal_type":"execute","payload":{"task":"correlation-test"}}')

TRACE_ID=$(echo "$SIGNAL" | jq -r '.trace_id')

# Check logs for same trace_id
kubectl logs -l app=tps-system -n tps-system --tail=100 | grep "$TRACE_ID"
# Should see same trace_id in multiple components:
# [api] trace_id=xyz123 Processing signal
# [queue] trace_id=xyz123 Enqueued
# [worker] trace_id=xyz123 Dequeued
# [jidoka] trace_id=xyz123 Circuit check passed
# [metrics] trace_id=xyz123 Recorded

# In Jaeger, click on a span and look at "Tags":
# You should see:
# - trace_id: xyz123
# - span_id: abc456
# - parent_span_id: parent789
# - service.name: tps-system
# - http.method: POST (if API call)
# - user_id: test-user
```

**Observation**: Single trace_id threads through all components, making it easy to follow signal.

---

### 6.5: Use Traces for Debugging (5 minutes)

```bash
# Scenario: User reports "my signal took 2 seconds!"
# You need to find where the delay occurred

# Simulate a slow scenario
# Scale down workers to 1 to create queue wait
kubectl scale deployment tps-system -n tps-system --replicas=1
kubectl rollout status deployment/tps-system -n tps-system --timeout=2m

# Send some signals
for i in {1..5}; do
  curl -s -X POST http://localhost:8080/signal \
    -H "Content-Type: application/json" \
    -d '{"signal_type":"execute","payload":{"task":"debug-test-'$i'"}}' > /dev/null &
done
wait

# In Jaeger:
# - Find slowest trace (sort by duration, descending)
# - Expand "Worker.Dequeue" span
# - See "WaitForWork" took 1500ms (90% of total time!)
# - Root cause: Only 1 worker, so signals queue up

# Scale back to 2
kubectl scale deployment tps-system -n tps-system --replicas=2
kubectl rollout status deployment/tps-system -n tps-system --timeout=2m

# New signals should have < 100ms "WaitForWork"
```

**Observation**: Traces pinpoint exact location of delay, enabling fast debugging.

---

### Lab 6 Summary

**Trace Anatomy**:
- Root span: Total signal processing (all operations)
- Child spans: Each component (API, Queue, Worker, etc.)
- Span tags: Metadata (trace_id, user_id, circuit_breaker_state)
- Span logs: Events (circuit_opened, retry, etc.)

**Key Learning**: Traces are your detective tool. When something is slow, traces show exactly WHERE and WHY.

---

## All Labs Complete!

You've now learned all 6 TPS principles hands-on:

| Lab | Principle | Key Learning |
|-----|-----------|--------------|
| 1 | Jidoka | Circuit breaks automatically; no manual resets needed |
| 2 | Kanban | Queue limit creates natural backpressure |
| 3 | Andon | Metrics + Alerts + Traces = complete visibility |
| 4 | Kaizen | Data-driven improvement, not random tuning |
| 5 | Heijunka | Work auto-balances across workers |
| 6 | Tracing | Traces show WHERE (not just THAT) problems exist |

---

## Next Steps

1. **Run production load test**: `kubectl exec deployment/tps-system -n tps-system -- /opt/load-test spike`
2. **Chaos test**: Deliberately break components and observe recovery
3. **Configuration tuning**: Read [07-configuration-tuning-guide.md](07-configuration-tuning-guide.md)
4. **Incident response**: Study [runbook-incident-response.md](runbook-incident-response.md)

---

**Status**: All 6 labs complete
**Time Invested**: ~3 hours
**Skills Gained**: Deep understanding of TPS principles in production system

**Next Lab Date**: ___________ (schedule your chaos testing)

---

**Version**: 1.0
**Last Updated**: January 2026
**Tested On**: GKE 1.25+ with n1-standard-4 nodes
