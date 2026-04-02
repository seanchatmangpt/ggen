<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [TPS Troubleshooting Decision Trees](#tps-troubleshooting-decision-trees)
  - [Decision Tree 1: Queue is Growing Indefinitely](#decision-tree-1-queue-is-growing-indefinitely)
    - [Step 2: Check Processing Rate](#step-2-check-processing-rate)
    - [Step 3: Compare Incoming vs Processing Rate](#step-3-compare-incoming-vs-processing-rate)
    - [Step 4: Determine Root Cause](#step-4-determine-root-cause)
    - [Step 5A: Check Downstream Service](#step-5a-check-downstream-service)
    - [Step 5B: Check Processing Task Time](#step-5b-check-processing-task-time)
    - [Step 5C: Check Queue Enqueue Time](#step-5c-check-queue-enqueue-time)
  - [Decision Tree 2: Circuit Breaker is Open](#decision-tree-2-circuit-breaker-is-open)
    - [Step 2: Understand Why Circuit Opened](#step-2-understand-why-circuit-opened)
    - [Remediation: Downstream Service Down](#remediation-downstream-service-down)
    - [Remediation: Downstream Service Slow](#remediation-downstream-service-slow)
    - [Remediation: Downstream Service Broken](#remediation-downstream-service-broken)
  - [Decision Tree 3: Error Rate is High](#decision-tree-3-error-rate-is-high)
    - [Step 2: Categorize Error Types](#step-2-categorize-error-types)
  - [Decision Tree 4: No Processing is Happening](#decision-tree-4-no-processing-is-happening)
    - [Step 2: Check Worker Pods](#step-2-check-worker-pods)
    - [Step 3: Check Worker Health](#step-3-check-worker-health)
    - [Step 4: Check Message Queue (Kanban)](#step-4-check-message-queue-kanban)
    - [Step 5: Force Recovery](#step-5-force-recovery)
  - [Decision Tree 5: Latency Spike](#decision-tree-5-latency-spike)
    - [Step 2: Identify Source of Latency](#step-2-identify-source-of-latency)
    - [If API is Slow](#if-api-is-slow)
    - [If Queue is Slow](#if-queue-is-slow)
    - [If Worker Dequeue Wait is High](#if-worker-dequeue-wait-is-high)
    - [If ProcessWork is Slow](#if-processwork-is-slow)
    - [If Jidoka.Check is Slow](#if-jidokacheck-is-slow)
  - [Decision Tree 6: Scaling Issues](#decision-tree-6-scaling-issues)
    - [Step 2: Identify Remaining Bottleneck](#step-2-identify-remaining-bottleneck)
    - [If Broker is Bottleneck](#if-broker-is-bottleneck)
    - [If Downstream is Bottleneck](#if-downstream-is-bottleneck)
    - [If Database is Bottleneck](#if-database-is-bottleneck)
  - [Emergency Procedures](#emergency-procedures)
    - [Complete System Failure (All Pods Down)](#complete-system-failure-all-pods-down)
    - [Data Loss (Queue Lost)](#data-loss-queue-lost)
    - [Performance Degradation (Can't Root Cause)](#performance-degradation-cant-root-cause)
  - [Summary Table: Quick Lookup](#summary-table-quick-lookup)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# TPS Troubleshooting Decision Trees

**Version**: 1.0
**Purpose**: Rapid diagnosis of common failures
**Audience**: On-call operators, SREs
**Format**: Decision trees for 5 common scenarios

---

## Decision Tree 1: Queue is Growing Indefinitely

**Symptom**: `queue_current_depth` continuously increasing, never decreases

```
┌─ Queue Growing? ─────────────────────────────────┐
│                                                   │
│ 1. Check queue depth trend (last 5 minutes)     │
│    kubectl exec svc/tps-api -n tps-system --   │
│      -- curl -s http://localhost:8080/status |  │
│      jq .kanban.queue_current_depth             │
│                                                   │
├─ Decision Point 1: Is queue depth increasing?   │
│                                                   │
├─ YES ──→ Go to Step 2                           │
│                                                   │
└─ NO ──→ False alarm. Queue is stable.           │
          Check alerts for misconfig.             │
```

### Step 2: Check Processing Rate

```
┌─ Processing Rate Check ───────────────────────────────────────┐
│                                                                 │
│ 2. Measure how many signals are being processed/second:       │
│    In Grafana > TPS Overview > "Processing Rate" gauge        │
│    Expected: 5-20 signals/second (depends on your load)       │
│                                                                 │
├─ Decision Point 2: Is processing rate > 0?                    │
│                                                                 │
├─ YES (processing is happening) ──→ Go to Step 3               │
│                                                                 │
└─ NO (processing rate = 0) ──→ CRITICAL                        │
     Processing has stopped!                                     │
     Go to "Decision Tree 4: No Processing Happening"           │
```

### Step 3: Compare Incoming vs Processing Rate

```
┌─ Rate Analysis ───────────────────────────────┐
│                                                 │
│ 3. Check both rates:                          │
│    Incoming rate: # signals/sec arriving       │
│    Processing rate: # signals/sec being done   │
│                                                 │
│    In Prometheus:                             │
│    - incoming: rate(signal_total[5m])         │
│    - processing: rate(signal_completed[5m])   │
│                                                 │
├─ Decision Point 3:                           │
│  Incoming rate > Processing rate?             │
│                                                 │
├─ YES ──→ Go to Step 4                         │
│          (Legitimate backpressure)             │
│                                                 │
└─ NO ──→ ERROR - Metrics inconsistent!         │
     Check Prometheus scrape config.             │
```

### Step 4: Determine Root Cause

```
┌─ Root Cause Analysis ────────────────────────────────────┐
│                                                            │
│ 4. WHY is processing slower than incoming?              │
│    Check worker metrics:                                 │
│    kubectl exec svc/tps-api -n tps-system -- \         │
│      curl -s http://localhost:8080/status | \          │
│      jq .kanban.workers                                 │
│                                                            │
├─ Decision Point 4A: How many workers?                   │
│                                                            │
├─ Only 1 worker ──→ Scale up!                            │
│  kubectl scale deployment tps-system \                   │
│    -n tps-system --replicas=4                            │
│  Go to "Remediation: Increased Workers"                 │
│                                                            │
├─ 2-4 workers but high utilization (>80%) ──→           │
│  Bottleneck is either:                                   │
│  (a) Downstream service slow → Go to Step 5A             │
│  (b) Processing task slow → Go to Step 5B                │
│  (c) Queue enqueue slow → Go to Step 5C                  │
│                                                            │
└─ Workers OK but queue still grows ──→                    │
  Go to Decision Tree 5: Latency Spike                     │
```

### Step 5A: Check Downstream Service

```bash
# Check if downstream is responsive
curl -s http://downstream-service:8080/health | jq .

# Check latency to downstream
kubectl logs -l app=tps-system -n tps-system --tail=50 | \
  grep "downstream_latency_ms" | tail -5
# Should show: downstream_latency_ms=0-5 (normal)
# If > 50ms, downstream is slow

# Check if downstream is returning errors
kubectl logs -l app=tps-system -n tps-system --tail=50 | \
  grep "downstream_error"
# If errors, downstream is broken
```

**If downstream is slow**:
- Scale downstream service
- Contact downstream team for investigation

**If downstream is broken**:
- Circuit breaker should have opened
- Check "Decision Tree 2: Circuit Breaker Open"

### Step 5B: Check Processing Task Time

```bash
# Examine task execution time in traces
kubectl port-forward -n tps-system svc/jaeger 16686:16686 &
# Open http://localhost:16686
# Find slowest trace
# Expand "ProcessWork" span
# Look for sub-spans taking time

# Measure task time directly
curl -s -X POST http://localhost:8080/signal \
  -H "Content-Type: application/json" \
  -d '{"signal_type":"execute","payload":{"task":"test"}}' | jq .timing

# If process_work > 100ms, task is slow
```

**If task is slow**:
- Optimize algorithm (O(n²) → O(n log n))
- Offload to background worker
- Cache results

### Step 5C: Check Queue Enqueue Time

```bash
# In Jaeger traces, look at "Kanban.Enqueue" span
# Should be < 10ms
# If > 50ms, queue system is struggling

# Check NATS/RabbitMQ broker
kubectl get svc -n tps-system | grep nats
kubectl get svc -n tps-system | grep rabbitmq

# Check broker CPU/memory
kubectl top pod -n tps-system | grep nats
kubectl top pod -n tps-system | grep rabbitmq
# If > 80% usage, scale broker
```

---

## Decision Tree 2: Circuit Breaker is Open

**Symptom**: `circuit_breaker_state = 1` (Open), signals being rejected

```
┌─ Circuit Breaker Open? ──────────────────────┐
│                                               │
│ 1. Verify circuit is actually open:         │
│    curl -s http://localhost:8080/status | jq │
│      .jidoka.circuit_breaker               │
│    Should be: "Closed" (0) or "Open" (1)   │
│                                               │
├─ Decision Point 1: Is circuit open?         │
│                                               │
├─ YES ──→ Go to Step 2                       │
│                                               │
└─ NO ──→ False alarm. Circuit is Closed.     │
          Problem elsewhere.                   │
```

### Step 2: Understand Why Circuit Opened

```
┌─ Circuit Open Reason ──────────────────────────────────┐
│                                                         │
│ 2. Why did circuit open?                              │
│    Options:                                            │
│    (a) Downstream service is unreachable             │
│    (b) Downstream is returning errors                 │
│    (c) Downstream is timing out                       │
│    (d) Health check is failing                        │
│                                                         │
│    Check Jidoka logs:                                 │
│    kubectl logs -l app=jidoka -n tps-system \        │
│      --tail=30 | grep "circuit\|reason"              │
│                                                         │
├─ Decision Point 2A: Which error?                      │
│                                                         │
├─ "Connection refused" ──→ Downstream is DOWN         │
│  Go to "Remediation: Downstream Service Down"        │
│                                                         │
├─ "Timeout" ──→ Downstream is SLOW                    │
│  Go to "Remediation: Downstream Service Slow"        │
│                                                         │
├─ "500 errors" ──→ Downstream is BROKEN              │
│  Go to "Remediation: Downstream Service Broken"      │
│                                                         │
└─ "Health check failed" ──→ Liveness probe issue      │
  Go to "Remediation: Health Check Failing"            │
```

### Remediation: Downstream Service Down

```bash
# Step 1: Verify it's down
curl http://downstream-service:8080/health
# Should return error or timeout

# Step 2: Check logs
kubectl logs -l app=downstream-service -n production --tail=50

# Step 3: Check pod status
kubectl get pods -l app=downstream-service -n production
# If 0 running, restart:
kubectl rollout restart deployment/downstream-service -n production

# Step 4: Wait for recovery
kubectl rollout status deployment/downstream-service -n production --timeout=5m

# Step 5: Verify circuit closes
# Wait 30 seconds (circuit recovery timeout)
sleep 30
curl -s http://localhost:8080/status | jq .jidoka.circuit_breaker
# Should now show: "Closed"

# Step 6: Resume normal operations
curl -s -X POST http://localhost:8080/signal \
  -H "Content-Type: application/json" \
  -d '{"signal_type":"execute","payload":{"task":"test"}}' | jq .status
# Should show: "accepted"
```

### Remediation: Downstream Service Slow

```bash
# Step 1: Measure downstream latency
time curl http://downstream-service:8080/health

# Step 2: Check downstream performance
kubectl top pod -l app=downstream-service -n production

# Step 3: Scale downstream service
kubectl scale deployment/downstream-service -n production --replicas=4
kubectl rollout status deployment/downstream-service -n production --timeout=5m

# Step 4: Re-test downstream latency
time curl http://downstream-service:8080/health

# Step 5: Verify circuit closes
sleep 30
curl -s http://localhost:8080/status | jq .jidoka.circuit_breaker
# Should be: "Closed"

# Step 6: Monitor circuit state
kubectl logs -l app=jidoka -n tps-system --tail=20
# Should show: "circuit_breaker_state=0 (Closed)"
```

### Remediation: Downstream Service Broken

```bash
# Step 1: Check downstream logs
kubectl logs -l app=downstream-service -n production --tail=100 | tail -30
# Look for exception stack traces

# Step 2: Identify the error
# Common patterns:
# - "OOM" (out of memory): increase pod memory
# - "Database connection failed": check database
# - "Panic": check for code regression
# - "Segfault": restart pod

# Step 3: Take action based on error type
# If memory: kubectl set resources deployment/downstream-service \
#   -n production --limits memory=2Gi

# If database: kubectl get svc -n production | grep database
#   Check if database pod is running

# If code regression: kubectl rollout undo deployment/downstream-service \
#   -n production (revert to previous version)

# Step 4: Wait for recovery
kubectl rollout status deployment/downstream-service -n production --timeout=5m

# Step 5: Verify circuit closes
sleep 30
curl -s http://localhost:8080/status | jq .jidoka.circuit_breaker
```

---

## Decision Tree 3: Error Rate is High

**Symptom**: Error rate > 5%, alerts firing

```
┌─ High Error Rate? ────────────────────────────┐
│                                                 │
│ 1. Measure error rate:                        │
│    In Prometheus:                              │
│    100 * rate(signal_errors[5m]) /            │
│      rate(signal_total[5m])                    │
│                                                 │
│    In Grafana > TPS Overview:                 │
│    Look at "Error Rate" gauge                 │
│                                                 │
├─ Decision Point 1: What's the error rate?    │
│                                                 │
├─ 0-1% ──→ Normal (some failures expected)    │
│           Check if alert threshold is wrong    │
│                                                 │
├─ 1-5% ──→ Warning (investigate)              │
│           Go to Step 2                         │
│                                                 │
└─ > 5% ──→ CRITICAL (system degraded)         │
     Go to Step 2 immediately                    │
```

### Step 2: Categorize Error Types

```bash
# What errors are occurring?
kubectl logs -l app=tps-system -n tps-system --tail=200 | \
  grep "error\|Error\|ERROR" | \
  cut -d':' -f3 | \
  sort | uniq -c | \
  sort -rn | head -10

# Categories:
# - Validation errors (bad input): user's problem
# - Timeout errors: downstream slow
# - Circuit open errors: downstream down
# - Serialization errors: data format issue
# - Internal errors: bug in system
```

**If validation errors (400s)**: Users sending bad data. Document valid format.

**If timeout errors (504)**: Downstream is slow. Scale downstream.

**If circuit open (503)**: Already covered in "Decision Tree 2".

**If serialization errors (500)**: Data format mismatch. Check if API schema changed.

**If internal errors (500)**: Bug in system. Check logs for panic. Roll back deployment.

---

## Decision Tree 4: No Processing is Happening

**Symptom**: Processing rate = 0, signals queued but not processed

```
┌─ No Processing? ───────────────────────────────┐
│                                                  │
│ 1. Verify no processing:                       │
│    In Prometheus: rate(signal_completed[5m])   │
│    Should be > 0                               │
│    If = 0, no signals are completing           │
│                                                  │
├─ Decision Point 1: Is processing actually 0?   │
│                                                  │
├─ YES ──→ Go to Step 2                          │
│                                                  │
└─ NO ──→ False alarm. Processing is happening.  │
```

### Step 2: Check Worker Pods

```bash
# Are worker pods running?
kubectl get pods -l app=tps-system -n tps-system
# STATUS should be "Running" for all workers

# If any are NOT Running, check reason:
kubectl describe pod <pod-name> -n tps-system | tail -30
# Look at "Events" section for error messages

# Common issues:
# - "CrashLoopBackOff": Code crash. Check logs:
#   kubectl logs <pod-name> -n tps-system | tail -50

# - "Pending": Resource unavailable. Check:
#   kubectl describe nodes
#   Look for "MemoryPressure" or "DiskPressure"

# - "Unknown": Node disappeared. Check:
#   kubectl get nodes
#   Look for node "NotReady" or "SchedulingDisabled"
```

### Step 3: Check Worker Health

```bash
# Confirm workers are actually processing
kubectl exec <worker-pod> -n tps-system -- \
  ps aux | grep "worker\|process" | head -5

# Check worker thread pool
kubectl exec <worker-pod> -n tps-system -- \
  curl -s http://localhost:8081/debug/threads | jq '.active_threads'
# Should be > 0

# Check worker queue
kubectl exec <worker-pod> -n tps-system -- \
  curl -s http://localhost:8081/debug/queue | jq '.queue_depth'
# Should show items in queue waiting to process
```

### Step 4: Check Message Queue (Kanban)

```bash
# NATS or RabbitMQ is the "source of work"
# If it's broken, workers can't get signals

kubectl get svc -n tps-system | grep -E "nats|rabbitmq"
# Should see a broker service

kubectl logs -l app=nats -n tps-system --tail=30 2>/dev/null || \
kubectl logs -l app=rabbitmq -n tps-system --tail=30 2>/dev/null
# Look for errors about connections, memory, disk

# If broker is stuck:
kubectl rollout restart statefulset/nats -n tps-system
# or
kubectl rollout restart statefulset/rabbitmq -n tps-system
```

### Step 5: Force Recovery

```bash
# If everything seems OK but processing is still stuck:

# 1. Restart all workers
kubectl rollout restart deployment/tps-system -n tps-system

# 2. Wait for rollout
kubectl rollout status deployment/tps-system -n tps-system --timeout=5m

# 3. Verify processing resumed
sleep 10
curl -s http://localhost:8080/metrics | grep "signal_completed"
# Should show increasing numbers

# 4. If still stuck, check for deadlock
kubectl logs -l app=tps-system -n tps-system | \
  grep -i "lock\|deadlock\|mutex" | tail -10
```

---

## Decision Tree 5: Latency Spike

**Symptom**: P99 latency jumped from 50ms to 500ms

```
┌─ Latency Spike? ──────────────────────────────┐
│                                                 │
│ 1. Measure current latency:                   │
│    In Prometheus: histogram_quantile(0.99,    │
│      tps_signal_latency_bucket)               │
│                                                 │
│    In Grafana > TPS Overview > "P99 Latency"  │
│                                                 │
├─ Decision Point 1: Latency spike confirmed?   │
│                                                 │
├─ YES ──→ Go to Step 2                         │
│                                                 │
└─ NO ──→ Spike is over. No action needed.      │
          Monitor for recurrence.                 │
```

### Step 2: Identify Source of Latency

```bash
# Sample recent traces to find slow ones
# In Jaeger (http://localhost:16686):
# 1. Service: tps-system
# 2. Operation: ProcessSignal
# 3. Sort by: Duration (descending)
# 4. Click slowest trace

# Look at span breakdown:
# Which span is taking longest?
# - ReceiveSignal (1-5ms): API is slow
# - Kanban.Enqueue (2-10ms): Queue is slow
# - Worker.Dequeue + wait (varies): Queue depth high
# - ProcessWork (varies): Task is slow
# - Jidoka.Check (1-5ms): Downstream is slow
```

### If API is Slow

```bash
# API pod CPU/memory spiked?
kubectl top pod -l app=tps-api -n tps-system

# If > 80% CPU, scale API
kubectl scale deployment/tps-api -n tps-system --replicas=4

# Or reduce load (ingress might be rate-limited incorrectly)
```

### If Queue is Slow

```bash
# NATS/RabbitMQ broker saturated?
kubectl top pod -l app=nats -n tps-system

# If > 80%, broker is bottleneck
# Option 1: Add broker replicas (if clustered)
# Option 2: Scale down clients temporarily
# Option 3: Check if queue is full (Jidoka rejecting)
```

### If Worker Dequeue Wait is High

```bash
# Queue depth is high (workers waiting for work)
# Not actually "system slow", but "backpressure"

# Options:
# 1. Add workers: kubectl scale deployment/tps-system \
#      -n tps-system --replicas=8

# 2. Reduce incoming load: Contact load generator team

# 3. Optimize task processing: profile traces, optimize code
```

### If ProcessWork is Slow

```bash
# The actual task is taking too long

# Check what task is slow:
kubectl logs -l app=tps-system -n tps-system | \
  grep "execution_time" | \
  awk '{print $NF}' | sort -rn | head -10
# Look for outliers (e.g., one task taking 2 seconds)

# If data-dependent:
# - Certain inputs cause slowdown
# - Check input size in slow traces
# - Optimize algorithm for large inputs

# If cache-miss:
# - Database query taking 300ms
# - Add caching layer
# - Pre-load frequently accessed data

# If subprocess slow:
# - External tool is slow
# - Upgrade tool or offload to background
```

### If Jidoka.Check is Slow

```bash
# Calling downstream is slow
# Same as "Decision Tree 2: Downstream Service Slow"

curl -s http://downstream-service:8080/health
# Measure latency

# If > 100ms, scale downstream
```

---

## Decision Tree 6: Scaling Issues

**Symptom**: After scaling, performance doesn't improve, or new bottleneck appears

```
┌─ Scaling Didn't Help? ────────────────────────┐
│                                                 │
│ 1. Confirm you actually scaled:               │
│    kubectl get deployment/tps-system -n tps-system │
│    Check DESIRED vs CURRENT replicas          │
│    Should be equal if rollout complete        │
│                                                 │
├─ Decision Point 1: Did scaling complete?      │
│                                                 │
├─ NO (CURRENT < DESIRED) ──→                   │
│  kubectl rollout status \                     │
│    deployment/tps-system -n tps-system        │
│  Wait for rollout to finish                   │
│  Check pod events for why stuck               │
│                                                 │
└─ YES ──→ Go to Step 2 (scaling done but not helping)
```

### Step 2: Identify Remaining Bottleneck

```bash
# With more workers, what's now the bottleneck?
# Options (by frequency):
# 1. Queue broker (NATS/RabbitMQ saturated)
# 2. Downstream service (can't handle more requests)
# 3. Database (too many queries)
# 4. Cache (too many cache misses)

# Measure each:
kubectl top pod | grep -E "nats|rabbitmq"  # Broker CPU
kubectl top pod | grep downstream          # Downstream CPU
kubectl get --raw /metrics | grep pg_       # Database metrics

# Whichever is > 80% is the new bottleneck
```

### If Broker is Bottleneck

```bash
# NATS can handle 50k+ msg/sec, so rarely the bottleneck
# But if it is:

# Option 1: Add more NATS replicas
kubectl scale statefulset/nats -n tps-system --replicas=3

# Option 2: Tune NATS performance
# Edit NATS config and increase:
# - max_connections
# - max_subscriptions
# Restart NATS after config change

# Verify:
kubectl top pod -l app=nats -n tps-system
# Should be < 50% CPU now
```

### If Downstream is Bottleneck

```bash
# Downstream service can't handle increased load

# Option 1: Scale downstream
kubectl scale deployment/downstream-service -n production --replicas=8

# Option 2: Add caching to reduce downstream calls
# Implement HTTP caching headers or client-side cache

# Option 3: Batch requests to downstream
# Instead of 1 request per signal, batch 10 signals per request

# Verify:
curl http://downstream-service:8080/metrics | grep "rps"
# Should show stable RPS even with more tps-system load
```

### If Database is Bottleneck

```bash
# Database can't handle query volume

# Measure query latency
kubectl exec -n production <db-pod> -- \
  psql -c "SELECT query, mean_time FROM pg_stat_statements \
           ORDER BY mean_time DESC LIMIT 10;"

# Top query taking too long?
# Option 1: Add index
# Option 2: Optimize query (rewrite/simplify)
# Option 3: Add read replicas
# Option 4: Implement caching

# Verify:
kubectl logs -l app=database -n production | grep "slow query" | wc -l
# Should decrease after optimization
```

---

## Emergency Procedures

### Complete System Failure (All Pods Down)

```bash
# Step 1: Check cluster health
kubectl get nodes
# If nodes are NotReady, check node logs

# Step 2: Restart entire system
helm uninstall tps-release -n tps-system
helm install tps-release tps/tps-reference \
  -n tps-system \
  --wait --timeout=10m

# Step 3: Verify recovery
kubectl wait --for=condition=Ready \
  pod -l app=tps-system \
  -n tps-system --timeout=5m

# Step 4: Test endpoint
curl -s http://localhost:8080/health | jq .status
```

### Data Loss (Queue Lost)

```bash
# Signals in flight are lost. Options:

# Option 1: Replay from source (if you have request logs)
# Option 2: Manual retry from monitoring/alerting system
# Option 3: Accept loss (for non-critical signals)

# Prevent future:
# - Enable RabbitMQ persistence (durable queues)
# - Enable NATS persistence (JetStream)
# - Back up queue to database
```

### Performance Degradation (Can't Root Cause)

```bash
# When all else fails:

# Step 1: Gather diagnostics
kubectl logs -l app=tps-system -n tps-system > /tmp/tps.logs
kubectl describe pod -l app=tps-system -n tps-system > /tmp/pods.txt
kubectl top pods -n tps-system > /tmp/metrics.txt
kubectl get pvc -n tps-system > /tmp/storage.txt

# Step 2: Full restart
kubectl rollout restart deployment/tps-system -n tps-system
kubectl wait --for=condition=Ready \
  pod -l app=tps-system \
  -n tps-system --timeout=5m

# Step 3: Re-test
curl -s -X POST http://localhost:8080/signal \
  -H "Content-Type: application/json" \
  -d '{"signal_type":"execute","payload":{"task":"test"}}'

# Step 4: File support ticket with diagnostic bundle
tar czf tps-diagnostics.tar.gz /tmp/tps.logs /tmp/pods.txt /tmp/metrics.txt
```

---

## Summary Table: Quick Lookup

| Symptom | Root Cause | Fix |
|---------|-----------|-----|
| Queue growing | Incoming > processing | Scale workers |
| Processing = 0 | Worker pods dead | kubectl rollout restart |
| Circuit open | Downstream down | Restart downstream |
| Error rate high | Bad inputs/downstream errors | Fix input validation or scale downstream |
| Latency spike | Queue full or task slow | Scale workers or optimize task |
| After scaling, still slow | New bottleneck | Identify and scale bottleneck |

---

**Version**: 1.0
**Last Updated**: January 2026
**Tested On**: GKE 1.25+ with n1-standard-4 nodes
