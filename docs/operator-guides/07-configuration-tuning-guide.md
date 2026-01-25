# TPS Configuration Tuning Guide

**Version**: 1.0
**Purpose**: Guidance on adjusting parameters for your specific workload
**Audience**: Operations engineers, system architects
**Goal**: Optimize TPS system for your performance and cost requirements

---

## Configuration Philosophy

### Core Principle: Start Conservative, Tune Empirically

```
Default Config (conservative)
    ↓
Run load test
    ↓
Identify bottleneck (metrics/traces)
    ↓
Adjust ONE parameter
    ↓
Re-test
    ↓
Measure improvement
    ↓
Decide: Keep or revert?
    ↓
Repeat until optimal
```

**Never**: Guess at values or use "Internet defaults"
**Always**: Measure before and after every change

---

## 1. Worker Pool Sizing

**Parameter**: `replicas` (number of pod replicas)
**Default**: 2
**Range**: 1-32 (1 = minimal, 32 = maximum burst handling)

### When to Increase Workers

**Symptom 1: Queue Growing, Processing Rate Constant**
```
Metric: Queue depth trending up
        Processing rate NOT increasing

Diagnosis: Queue is backing up faster than workers can drain it
Action: Increase workers

Formula: target_workers = current_workers * (incoming_rate / current_processing_rate)

Example:
  Incoming: 20 sig/sec
  Processing: 5 sig/sec (with 2 workers)
  Target: 2 * (20 / 5) = 8 workers
```

**Symptom 2: P99 Latency High, Workers Busy**
```
Metric: P99 > 200ms AND Worker utilization > 80%

Diagnosis: Workers are overloaded, causing queue wait
Action: Add more workers

Quick fix: Scale to 2x current
  kubectl scale deployment/tps-system -n tps-system --replicas=8
  (Double from 4 to 8, for example)

Then measure: P99 latency should drop as queue wait decreases
```

**Symptom 3: Error Rate High, Circuit Breaker Closed**
```
Metric: Error rate > 5% AND Circuit = Closed

Diagnosis: Timeouts due to slow task processing
Action: Add workers to reduce queue wait

Then measure: Error rate should drop as latency improves
```

### When to Decrease Workers (Cost Optimization)

**Symptom: Queue Consistently Empty, Workers Idle**
```
Metric: Queue depth < 10% AND Worker utilization < 20%

Diagnosis: More workers than needed
Action: Scale down

Safe reduction: Reduce by 25% at a time
  kubectl scale deployment/tps-system -n tps-system --replicas=3
  (Reduce from 4 to 3)

Monitor for 30 minutes:
  ☐ Queue depth still healthy (40-60%)?
  ☐ P99 latency still acceptable?
  ☐ No errors?

If yes to all: Reduction was safe
If no: Scale back up
```

### Worker Sizing: Quick Decision Tree

```
             Is P99 latency high (> 200ms)?
                        ↙              ↘
                    YES                NO
                     ↓                  ↓
           Is queue depth high      Is queue consistently
           (> 70%)?                 empty (< 20%)?
               ↙    ↘                    ↙
            YES      NO              YES
             ↓        ↓               ↓
          ADD      INVESTIGATE    REDUCE
        WORKERS    TASK SLOW      WORKERS
```

### Implementation: Scale Workers

```bash
# View current replicas
kubectl get deployment/tps-system -n tps-system -o jsonpath='{.spec.replicas}'

# Scale to new count
kubectl scale deployment/tps-system -n tps-system --replicas=8

# Watch rollout
kubectl rollout status deployment/tps-system -n tps-system --timeout=5m

# Verify new workers started
kubectl get pods -l app=tps-system -n tps-system | tail -5

# Test performance
curl -s http://localhost:8080/metrics | grep "queue_depth\|processing_rate"
```

---

## 2. Queue Configuration

**Parameters**: `queue_max_size`, `queue_timeout`
**Defaults**: `queue_max_size=100`, `queue_timeout=30s`

### Queue Max Size

**Definition**: Maximum number of items that can be queued
**Default**: 100

**When to Increase**
```
Scenario: Bursty traffic (20 items arrive, then 5 minutes of quiet)
Current: queue_max_size=100
Problem: Queue fills (100% utilized) during burst, then slowly drains

Solution: Increase to 200
  kubectl set env deployment/tps-system -n tps-system \
    QUEUE_MAX_SIZE=200

Rationale: Larger queue can absorb bursts without rejecting signals
Cost: More memory usage (~10KB per item)
```

**When to Decrease**
```
Scenario: Strict low-latency requirement
Current: queue_max_size=100
Problem: P99 latency is 150ms (too much queue wait)

Solution: Decrease to 50
  kubectl set env deployment/tps-system -n tps-system \
    QUEUE_MAX_SIZE=50

Rationale: Smaller queue forces earlier rejection (fail-fast)
          Users get fast failure instead of slow success
Cost: More rejections during load spikes
Trade-off: Lower latency p-tail vs higher error rate during spikes
```

**Formula: Calculate Right Size**

```
Max queue size should allow:
  Duration = Queue_max_size / Processing_rate

Example:
  Processing rate: 10 sig/sec
  Target queue drain time: 30 seconds
  Queue_max_size = 10 * 30 = 300

  But also consider:
  Memory per item: ~10KB
  Memory budget: 1GB
  Max items: 1GB / 10KB = ~100,000 (not limiting factor)

  Final: queue_max_size = 300 (balanced for 30-sec drain)
```

### Queue Timeout

**Definition**: How long to wait before timing out in queue
**Default**: 30 seconds

**When to Decrease**
```
Scenario: Long queue wait is worse than rejection
Current: queue_timeout=30s
Problem: User sends signal, waits 25 seconds, then times out

Solution: Decrease to 10 seconds
  kubectl set env deployment/tps-system -n tps-system \
    QUEUE_TIMEOUT=10s

Rationale: User finds out earlier that system is overloaded
         Allows them to retry elsewhere or use cache
Trade-off: More timeouts, but faster feedback
```

**When to Increase**
```
Scenario: Legitimate long-duration tasks
Current: queue_timeout=30s
Problem: Task takes 40 seconds, times out before completion

Solution: Increase to 60 seconds
  kubectl set env deployment/tps-system -n tps-system \
    QUEUE_TIMEOUT=60s

Rationale: Allow longer-running tasks to complete
Trade-off: Higher perceived latency for failed requests
Risk: User might not get response and retry, causing double-processing
```

### Configuration: Queue Settings

```yaml
# In ConfigMap or deployment env:
apiVersion: v1
kind: ConfigMap
metadata:
  name: tps-config
  namespace: tps-system
data:
  QUEUE_MAX_SIZE: "100"      # Items
  QUEUE_TIMEOUT_SECONDS: "30" # Seconds
  QUEUE_DRAIN_RATE: "20"     # Items/sec (processing rate target)
```

---

## 3. Circuit Breaker Configuration

**Parameters**: `circuit_failure_threshold`, `circuit_recovery_timeout`, `circuit_half_open_requests`
**Defaults**: `threshold=5`, `timeout=30s`, `half_open=3`

### Failure Threshold

**Definition**: How many failures before opening circuit
**Default**: 5 failures

**When to Lower (Aggressive)**
```
Scenario: Want fast failure detection
Current: threshold=5 (opens after 5 failures)
Problem: Takes 5 failures to detect issue, might be 2+ seconds of latency

Solution: Decrease to 2
  kubectl set env deployment/tps-system -n tps-system \
    CIRCUIT_FAILURE_THRESHOLD=2

Rationale: Detect failures faster
Trade-off: Higher false positive risk (transient failures open circuit)
```

**When to Raise (Conservative)**
```
Scenario: Downstream has occasional transient failures
Current: threshold=5
Problem: Circuit opens too easily for transient blips

Solution: Increase to 10
  kubectl set env deployment/tps-system -n tps-system \
    CIRCUIT_FAILURE_THRESHOLD=10

Rationale: Tolerate transient failures without opening
Trade-off: Might send requests to failing downstream, causing latency
```

### Recovery Timeout

**Definition**: How long before circuit tries to recover
**Default**: 30 seconds

**When to Decrease**
```
Scenario: Downstream recovers quickly
Current: timeout=30s
Problem: Waiting 30 seconds is too long for fast-recovery outages

Solution: Decrease to 10 seconds
  kubectl set env deployment/tps-system -n tps-system \
    CIRCUIT_RECOVERY_TIMEOUT=10s

Rationale: Recover faster from quick-fix outages
Trade-off: More frequent recovery attempts (more load on downstream)
```

**When to Increase**
```
Scenario: Downstream takes long time to recover
Current: timeout=30s
Problem: Circuit recovers at 30s, but downstream isn't ready yet
         Recovery fails, circuit opens again

Solution: Increase to 60 seconds
  kubectl set env deployment/tps-system -n tps-system \
    CIRCUIT_RECOVERY_TIMEOUT=60s

Rationale: Give downstream adequate recovery time
Trade-off: Longer outage before system returns to operation
```

### Configuration: Circuit Breaker

```yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: tps-jidoka-config
  namespace: tps-system
data:
  CIRCUIT_FAILURE_THRESHOLD: "5"        # Failures before opening
  CIRCUIT_RECOVERY_TIMEOUT_SECONDS: "30" # Time between recovery attempts
  CIRCUIT_HALF_OPEN_REQUESTS: "3"       # Requests to test recovery
  CIRCUIT_SUCCESS_THRESHOLD: "2"        # Successes to close (out of half_open)
```

---

## 4. Timeouts Configuration

**Parameters**: `request_timeout`, `downstream_timeout`, `health_check_timeout`
**Defaults**: `request=30s`, `downstream=5s`, `health=2s`

### Request Timeout

**Definition**: How long to wait for a signal to complete
**Default**: 30 seconds

**When to Decrease**
```
Scenario: Users expect fast response
Current: request_timeout=30s
Problem: Slow task takes 20 seconds, user perceives as failure

Solution: Decrease to 10 seconds
  kubectl set env deployment/tps-system -n tps-system \
    REQUEST_TIMEOUT=10s

Effect:
  - Signals taking > 10s get cancelled
  - User gets quick failure (can retry)
  - System stops wasting resources on doomed signals

Risk: Legitimate 15-second tasks will timeout
      Document new SLA: maximum 10-second tasks
```

**When to Increase**
```
Scenario: Legitimate long-running tasks (data processing, etc.)
Current: request_timeout=30s
Problem: Task takes 45 seconds, gets cancelled prematurely

Solution: Increase to 60 seconds
  kubectl set env deployment/tps-system -n tps-system \
    REQUEST_TIMEOUT=60s

Cost: Users wait longer for failure/success
Risk: Bad user experience if task is actually broken
```

### Downstream Timeout

**Definition**: How long to wait for downstream service response
**Default**: 5 seconds

**When to Decrease**
```
Scenario: Downstream is normally very fast
Current: downstream_timeout=5s
Problem: Downstream occasionally is slow (5s+), causing cascading delay

Solution: Decrease to 2 seconds
  kubectl set env deployment/tps-system -n tps-system \
    DOWNSTREAM_TIMEOUT=2s

Effect:
  - Circuit opens faster (after 2s instead of 5s)
  - Less wasted time waiting for broken downstream
  - Users get failure faster (can retry)

Risk: Normal 3-second responses will timeout
      Only use if downstream is normally < 2s
```

**When to Increase**
```
Scenario: Downstream is legitimately slow
Current: downstream_timeout=5s
Problem: Downstream sometimes takes 6-7 seconds, causing circuit opens

Solution: Increase to 10 seconds
  kubectl set env deployment/tps-system -n tps-system \
    DOWNSTREAM_TIMEOUT=10s

Cost: More wasted time waiting for slow responses
Trade-off: Fewer false-positive circuit opens
```

---

## 5. Rate Limiting Configuration

**Parameters**: `rate_limit_per_second`, `rate_limit_burst`
**Defaults**: `per_second=1000`, `burst=100`

**Purpose**: Prevent a single user from monopolizing system

### Per-Second Limit

**Definition**: Maximum signals per second per user
**Default**: 1000/sec

**When to Decrease**
```
Scenario: Noisy neighbor problem (one user sending huge volume)
Current: rate_limit=1000
Problem: User A sends 500 sig/sec, User B starves with 2 sig/sec

Solution: Decrease to 100/sec per user
  kubectl set env deployment/tps-system -n tps-system \
    RATE_LIMIT_PER_SECOND=100

Effect:
  - User A can send max 100/sec (rest rejected)
  - User B guaranteed min rate (queue is fairly distributed)
  - Fair resource sharing

Risk: High-volume legitimate users get throttled
      May need per-tier rate limits (premium=500, standard=100)
```

### Burst Limit

**Definition**: Temporary spikes above per-second rate
**Default**: 100 items

**When to Increase**
```
Scenario: Users legitimately need to send bursts
Current: burst=100
Problem: User sends burst of 150 items, gets throttled

Solution: Increase to 200
  kubectl set env deployment/tps-system -n tps-system \
    RATE_LIMIT_BURST=200

Effect:
  - Spikes up to 200 items allowed
  - Over time, rate still limited to per_second

Trade-off: Potentially unfair to other users during burst
```

---

## 6. Metric Collection Configuration

**Parameters**: `metrics_sample_rate`, `metrics_retention`
**Defaults**: `sample_rate=1.0` (100%), `retention=30d`

### Sample Rate

**Definition**: What percentage of signals to collect metrics for
**Default**: 1.0 (100% - collect all)

**When to Decrease (High Volume)**
```
Scenario: Very high throughput (50k sig/sec)
Current: sample_rate=1.0 (100%)
Problem: Prometheus can't keep up with metric cardinality

Solution: Decrease to 0.1 (10%)
  kubectl set env deployment/tps-system -n tps-system \
    METRICS_SAMPLE_RATE=0.1

Effect:
  - Collect metrics for 1 in 10 signals
  - Metrics become statistical (not exact)
  - Less Prometheus load

Trade-off: Metrics are estimates, not exact
         Still accurate for percentiles and trends
```

**Formula: Right Sample Rate**

```
Targets:
  - Prometheus to handle metric cardinality
  - Grafana to display without lag

If experiencing:
  - Prometheus scrape latency > 10s: Decrease sample rate
  - Grafana query latency > 5s: Decrease sample rate
  - Metrics gaps: Increase retention period instead

Conservative: Start at 1.0, decrease only if needed
```

### Retention Period

**Definition**: How long to keep metrics
**Default**: 30 days

**When to Increase**
```
Scenario: Want longer trend history
Current: retention=30d
Problem: Can only see 1 month back

Solution: Increase to 90d
  kubectl patch statefulset/prometheus -n tps-system \
    --type json -p '[{
      "op":"replace",
      "path":"/spec/template/spec/containers/0/args",
      "value":["--storage.tsdb.retention.time=90d"]
    }]'

Cost: More storage (Prometheus uses ~1GB per 1M time series per week)
      Estimate: 30d = 30GB, 90d = 90GB
```

---

## 7. Load Balancing Configuration (Heijunka)

**Parameters**: `rebalance_interval`, `imbalance_threshold`
**Defaults**: `interval=30s`, `threshold=0.05` (5%)

### Rebalance Interval

**Definition**: How often to redistribute work across workers
**Default**: 30 seconds

**When to Decrease**
```
Scenario: Need faster load rebalancing
Current: interval=30s
Problem: After burst, wait 30 seconds for load to rebalance

Solution: Decrease to 10 seconds
  kubectl set env deployment/tps-system -n tps-system \
    REBALANCE_INTERVAL=10s

Effect:
  - Rebalance every 10 seconds instead of 30
  - Faster response to load imbalance

Cost: More rebalancing operations = slightly higher CPU
      But negligible unless rebalancing is expensive
```

**When to Increase**
```
Scenario: Constant jitter from rebalancing
Current: interval=10s
Problem: Work keeps moving between workers (thrashing)

Solution: Increase to 60 seconds
  kubectl set env deployment/tps-system -n tps-system \
    REBALANCE_INTERVAL=60s

Effect:
  - Rebalance less frequently
  - Less jitter/movement

Trade-off: Takes longer to recover from imbalanced state
```

### Imbalance Threshold

**Definition**: Percentage deviation from mean before rebalancing
**Default**: 0.05 (5%)

**When to Lower (Strict Balance)**
```
Scenario: Need perfect load distribution
Current: threshold=0.05 (5%)
Problem: Workers can be up to 5% imbalanced

Solution: Lower to 0.02 (2%)
  kubectl set env deployment/tps-system -n tps-system \
    IMBALANCE_THRESHOLD=0.02

Effect:
  - Rebalance more aggressively (when > 2% deviation)
  - More even load distribution

Cost: More frequent rebalancing
```

---

## 8. Memory and Resource Configuration

**Parameters**: Memory limits, CPU requests, JVM heap (if Java backend)
**Defaults**: `memory=512Mi`, `cpu=250m`

### Memory Limits

**Definition**: Maximum memory per pod
**Default**: 512Mi

**When to Increase**
```
Scenario: Pod getting OOMKilled (out of memory)
Current: memory=512Mi
Symptom: kubectl get events | grep OOMKilled

Solution: Increase to 1Gi
  kubectl set resources deployment/tps-system -n tps-system \
    --limits memory=1Gi

Effect:
  - Pod can use up to 1GB before being killed
  - More room for in-flight requests and caches

Cost: GKE charges for requested memory
      512Mi → 1Gi = 2x memory cost per pod
```

**When to Decrease (Cost Optimization)**
```
Scenario: Pods not using allocated memory
Current: memory=1Gi, actual use ~200Mi
Cost: Paying for 1GB but using 200MB

Solution: Decrease to 512Mi
  kubectl set resources deployment/tps-system -n tps-system \
    --limits memory=512Mi

Effect:
  - Still have 200MB free headroom
  - Pay less for unused capacity

Risk: If memory usage spikes above 512Mi, pod gets OOMKilled
      Monitor with: kubectl top pod -l app=tps-system
```

### CPU Requests

**Definition**: CPU guarantee per pod
**Default**: 250m (0.25 CPU cores)

**When to Increase**
```
Scenario: Pods throttled (consistently hitting CPU limit)
Symptom: kubectl top pod shows > 250m
Current: request=250m

Solution: Increase to 500m
  kubectl set requests deployment/tps-system -n tps-system \
    cpu=500m

Effect:
  - Kubernetes guarantees 500m CPU per pod
  - Less CPU throttling

Cost: More CPU capacity reserved
      Limit concurrent pods per node
```

---

## Quick Tuning Checklist

### Symptom: Queue Growing

```
☐ Check processing rate: Is it > 0?
  If 0: Restart workers
  If > 0: Go to next

☐ Compare incoming vs processing rate
  If incoming > processing: ADD WORKERS
  kubectl scale deployment/tps-system -n tps-system --replicas=<new>

☐ Re-test in 5 minutes
  Queue stabilizing? Done
  Queue still growing? Check task latency (Jaeger)
```

### Symptom: High Latency

```
☐ Check queue depth
  If queue > 60%: Add workers
  If queue < 60%: Task is slow

☐ If task is slow:
  Open Jaeger, find slowest spans
  If ProcessWork > 100ms: Optimize task or add resources
  If Downstream > 100ms: Scale downstream
```

### Symptom: High Error Rate

```
☐ Check circuit breaker status
  If Open: Downstream is broken, restart it
  If Closed: Check error type in logs

☐ If error type is "timeout": Increase timeout
  kubectl set env deployment/tps-system -n tps-system \
    REQUEST_TIMEOUT=60s
```

### Symptom: High Cost

```
☐ Check worker utilization
  If < 30%: Scale down workers
  kubectl scale deployment/tps-system -n tps-system --replicas=<less>

☐ Check memory usage
  If < 300Mi: Decrease memory limit
  kubectl set resources deployment/tps-system -n tps-system \
    --limits memory=256Mi

☐ Review metric retention
  If 90d retention but only need 30d: Decrease
  kubectl patch statefulset/prometheus -n tps-system \
    --type json -p '[{"op":"replace","path":"/spec/template/spec/containers/0/args","value":["--storage.tsdb.retention.time=30d"]}]'
```

---

## Before & After Tuning Template

```
Configuration Change:  _____________________________________________

Date: __________________  Operator: _________________

BEFORE Metrics (measure for 5 minutes):
  ☐ P99 Latency: _________ ms
  ☐ P50 Latency: _________ ms
  ☐ Error Rate: _________%
  ☐ Processing Rate: _________ sig/sec
  ☐ Queue Depth: _________%
  ☐ Worker Utilization: _________%
  ☐ Memory per pod: _________ Mi
  ☐ Cost/hour: $ _________

CHANGE MADE:
  Parameter: ______________________
  Old value: ______________________
  New value: ______________________
  Reasoning: _____________________________________________________

AFTER Metrics (measure for 5 minutes):
  ☐ P99 Latency: _________ ms (Δ _________ %)
  ☐ P50 Latency: _________ ms (Δ _________ %)
  ☐ Error Rate: _________% (Δ _________ %)
  ☐ Processing Rate: _________ sig/sec (Δ _________ %)
  ☐ Queue Depth: _________% (Δ _________ %)
  ☐ Worker Utilization: _________% (Δ _________ %)
  ☐ Memory per pod: _________ Mi (Δ _________ %)
  ☐ Cost/hour: $ _________ (Δ _________ %)

ASSESSMENT:
  ☐ Improvement achieved (goal was ________________)
  ☐ Trade-off acceptable (cost: ________________)
  ☐ Change is permanent (revert if issues)

NEXT STEPS:
  ☐ Document in runbooks
  ☐ Commit change to git
  ☐ Update SLA if needed
  ☐ Schedule next tuning session
```

---

## Performance Tuning Case Studies

### Case Study 1: From 50 to 500 sig/sec

**Starting Point**:
- 2 workers, P99=200ms, Error rate=2%, Cost=$100/month

**Problem**:
- Rapid growth, queue hitting limits during peaks

**Tuning Steps**:
1. Scale workers: 2 → 8 (P99 improved 200→120ms)
2. Increase queue size: 100 → 300 (bursts handled)
3. Adjust downstream timeout: 5s → 10s (fewer circuit opens)
4. Add task caching (ProcessWork latency 80ms → 20ms)

**Final State**:
- 8 workers, P99=45ms, Error rate=0.3%, Cost=$400/month
- Achieved 10x throughput improvement, acceptable cost increase

### Case Study 2: Cost Optimization

**Starting Point**:
- 8 workers, 2GB memory, $500/month, but P99=30ms (SLA only requires 100ms)

**Opportunity**:
- Workers are over-provisioned for the SLA

**Tuning Steps**:
1. Reduce workers: 8 → 4 (still meet 100ms SLA)
2. Reduce memory: 2GB → 512MB per pod
3. Increase rate limiting: Allow bursts instead of scaling
4. Increase queue size: To absorb bursts without rejecting

**Final State**:
- 4 workers, 512MB memory, $250/month, P99=75ms (still within 100ms SLA)
- 50% cost reduction while maintaining SLA

---

## Golden Rules of Tuning

1. **Measure before and after** - Always
2. **Change one parameter at a time** - Never multiple changes simultaneously
3. **Wait for steady state** - 5 minutes minimum, 30 minutes ideal
4. **Document decisions** - Write down why you changed each value
5. **Test on dev first** - Never tune production on first attempt
6. **Monitor for regressions** - Check the next day, next week
7. **Use decision trees** - Don't guess, follow the data

---

**Version**: 1.0
**Last Updated**: January 2026
**Examples Tested**: GKE with 2-32 worker nodes
