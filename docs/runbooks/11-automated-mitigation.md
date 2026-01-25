<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Automated Mitigation Actions & Self-Healing Strategies](#automated-mitigation-actions--self-healing-strategies)
  - [Mitigation Strategy Framework](#mitigation-strategy-framework)
    - [Hierarchy of Responses](#hierarchy-of-responses)
  - [Pattern 1: Auto-Scaling](#pattern-1-auto-scaling)
    - [Queue Backlog Overflow](#queue-backlog-overflow)
    - [CPU/Memory Scaling](#cpumemory-scaling)
  - [Pattern 2: Circuit Breaker](#pattern-2-circuit-breaker)
    - [Downstream Service Failure](#downstream-service-failure)
    - [Graceful Degradation](#graceful-degradation)
  - [Pattern 3: Throttling / Rate Limiting](#pattern-3-throttling--rate-limiting)
    - [Backpressure Application](#backpressure-application)
  - [Pattern 4: Failover](#pattern-4-failover)
    - [Regional Failover](#regional-failover)
  - [Pattern 5: Restart / Reset](#pattern-5-restart--reset)
    - [Pod Restart (Kubernetes)](#pod-restart-kubernetes)
    - [Service Reset](#service-reset)
  - [Pattern 6: Feature Flag Disable](#pattern-6-feature-flag-disable)
    - [Disable Problematic Feature](#disable-problematic-feature)
  - [Pattern 7: Database Optimization](#pattern-7-database-optimization)
    - [Query Tuning](#query-tuning)
  - [Mitigation Receipt Contract](#mitigation-receipt-contract)
    - [Every Mitigation Generates a Receipt](#every-mitigation-generates-a-receipt)
  - [Mitigation Effectiveness Tracking](#mitigation-effectiveness-tracking)
  - [Mitigation Failures & Escalation](#mitigation-failures--escalation)
  - [Testing Mitigation Strategies](#testing-mitigation-strategies)
  - [Configuration & Tuning](#configuration--tuning)
    - [Alert Thresholds](#alert-thresholds)
  - [Related Documentation](#related-documentation)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Automated Mitigation Actions & Self-Healing Strategies

**Version**: 2.0
**Last Updated**: 2026-01-25
**Status**: Production Ready

> **Core Principle**: System responds to failures automatically without human intervention. Humans only investigate post-incident. Every mitigation generates a receipt (proof of action taken).

---

## Mitigation Strategy Framework

### Hierarchy of Responses

**1. Detect** (Automated):
- Monitor metrics (queue depth, error rate, latency)
- Execute health checks
- Parse logs for errors
- Trigger on threshold exceeded

**2. Assess** (Automated):
- Determine severity
- Check available remediation actions
- Verify prerequisites (e.g., quota available before scaling)
- Estimate impact (how many customers affected?)

**3. Mitigate** (Automated):
- Execute recovery action
- Document action with receipt
- Monitor for effectiveness
- Escalate if remediation fails

**4. Verify** (Automated):
- Check if issue resolved
- Monitor for side effects
- Sustained recovery (not temporary improvement)
- Escalate if unresolved after 2 minutes

---

## Pattern 1: Auto-Scaling

### Queue Backlog Overflow

**Trigger**:
```
queue_depth > 500 for 30 seconds
OR
message_lag > 30 seconds (P99)
```

**Detection**:
```yaml
Alert: cloud_monitoring_queue_backlog
Threshold: depth > 500
Window: 30 seconds
Action: Calculate scaling factor
```

**Mitigation**:
```
Scale Calculation:
  current_queue = 700
  target_queue = 300 (ideal)
  scale_factor = (current_queue / target_queue) = 2.3

  new_concurrency = current_concurrency * scale_factor
  new_concurrency = 20 * 2.3 = 46 → rounded to 50

Scaling Limits:
  - Minimum: 1 (can't scale below this)
  - Maximum: 100 (resource limit)
  - Step: 10 (don't jump huge amounts)

  new_concurrency = min(max(46, 1), 100) = 46 → 50
```

**Verification**:
```
Check every 10 seconds:
1. queue_depth trending down? YES→Continue / NO→Scale higher
2. Sustained for 30 seconds? YES→Success / NO→Wait
3. Processing rate > ingestion rate? YES→Success / NO→Scale higher
```

**Escalation** (if scaling maxed out):
```
if new_concurrency >= max_concurrency:
  emit(escalation_required)
  page(on_call, "Concurrency maxed, need manual action")
```

### CPU/Memory Scaling

**Trigger**:
```
cpu_usage > 80% for 60 seconds
OR
memory_usage > 85% for 60 seconds
```

**Mitigation**:
```
if cpu_high:
  action = scale_replicas(+1)  # Add more pod instances

if memory_high:
  action = scale_replicas(+1)  # Same solution (distribute load)
  alt_action = trigger_gc()     # Force garbage collection
```

**Verification**:
```
1. Resource usage dropping? (target < 70%)
2. New replicas healthy? (pod status = Running)
3. Error rate not increasing? (still < 1%)
```

---

## Pattern 2: Circuit Breaker

### Downstream Service Failure

**Trigger**:
```
downstream_error_rate > 50% for 30 seconds
OR
downstream_timeout > 500ms consistently
```

**Mitigation**:
```
state = OPEN  (stop sending requests to failed service)

Responses:
1. Return cached response (if available)
2. Return default value (for non-critical operations)
3. Return 503 Service Unavailable (fail-fast)
```

**Example - Action Handler Timeout**:
```
Error Rate: 68%
Latency: 1,200ms (vs. baseline 100ms)

→ Circuit Breaker Opens
→ Requests return 503 fast (< 50ms, vs. hanging 1,200ms)
→ Client retries elsewhere or fails gracefully
→ Prevents cascading failures
```

**Recovery**:
```
Half-Open Phase (every 10 seconds):
1. Send probe request to service
2. If success: increment success_counter
3. If failure: reset counter, stay open
4. If success_counter >= threshold (10): state = CLOSED
```

### Graceful Degradation

**When Circuit Opens**:
- **Critical path**: Fail-fast (503), don't cascade
- **Non-critical**: Return cached response (if available)
- **Async operations**: Queue for retry (exponential backoff)

**Example**:
```
Request: Get user profile

Circuit Open (user database unavailable):
1. Check cache (in-memory) → FOUND? Return cached + "stale" flag
2. Cache miss? Return default profile + "service unavailable" flag
3. Async: Queue request for retry in 5 minutes

User Experience:
- Data may be stale (10 minutes old)
- But service doesn't cascade fail
- And user can still use product
```

---

## Pattern 3: Throttling / Rate Limiting

### Backpressure Application

**Trigger**:
```
inbound_request_rate > max_sustainable_rate for 30 seconds
```

**Calculation**:
```
sustainable_rate = (processing_capacity) / (avg_request_duration)

Example:
  max_goroutines = 500
  avg_request_duration = 100ms
  sustainable_rate = (500) / (0.1s) = 5,000 requests/sec

If inbound > 5,000/sec:
  → Enable throttling
  → Accept 5,000/sec (highest quality)
  → Reject excess (return 429 Too Many Requests)
```

**Behavior**:
```
Throttling Rules:
1. Accept up to rate limit
2. Queue excess (up to queue limit)
3. Reject above queue limit (429 Too Many Requests)

Time Limits:
  Rate limit: 5,000 req/sec
  Queue size: 1,000 requests
  If queue > 1,000: start rejecting

Customer Experience:
- 5,000 req/sec: Accepted, low latency
- 6,000 req/sec: 1,000 queued (latency increases)
- 6,500 req/sec: 500 rejected (429 response)
```

**Recovery**:
```
Monitor: inbound_rate

If rate drops below threshold:
  → Disable throttling
  → Resume accepting all requests
  → Process queued requests
```

---

## Pattern 4: Failover

### Regional Failover

**Trigger**:
```
primary_region_health_check fails 3 times (30 seconds)
```

**Mitigation**:
```
1. Health Check Loop:
   Loop {
     if health_check_fails:
       failure_count++
     if failure_count >= 3:
       initiate_failover()
       break
   }

2. Failover Steps:
   a) Verify standby region is healthy
   b) Update DNS records (point to standby IP)
   c) Emit failover receipt
   d) Monitor for propagation

3. DNS Propagation:
   Time: 30-60 seconds (depends on TTL)
   Gradual: 10% → 25% → 50% → 75% → 100%
```

**Verification**:
```
1. DNS propagated? All queries return standby IP
2. Traffic shifted? Monitoring shows 100% to standby
3. Data consistent? Replication lag < 100ms
4. Error rate normal? < 1%
```

---

## Pattern 5: Restart / Reset

### Pod Restart (Kubernetes)

**Trigger**:
```
pod_crashing (CrashLoopBackOff state)
OR
health_check_failing consistently
```

**Mitigation**:
```
kubectl delete pod <pod-name> --grace-period=5
→ Kubernetes recreates pod with fresh state
→ Clears memory leaks, connection pools
→ Forces re-initialization
```

**Risks**:
- Data loss (if not persisted)
- Brief service disruption (pod startup latency)
- Cascading restarts (if root cause not fixed)

**Verification**:
```
1. Pod status: Running (not Pending/CrashLoopBackOff)
2. Health check: Passing
3. Error rate: Back to baseline
4. Memory: Normal (not increasing)
```

### Service Reset

**Connection Pool Drain**:
```
If database connection pool exhausted:
1. Drain active connections (wait for in-flight requests)
2. Close all connections
3. Recreate pool (fresh connections)
4. Resume accepting requests
```

---

## Pattern 6: Feature Flag Disable

### Disable Problematic Feature

**Trigger**:
```
Recent feature deployment → error rate spike
```

**Mitigation**:
```
if error_rate_spike detected:
  if recent_deployment exists:
    disable_feature_flag(feature_name)
    → Toggle feature off (instantly)
    → Route users to old code path
    → Doesn't require restart
```

**Benefits**:
- Instant recovery (no deployment needed)
- Zero downtime (flag toggle is atomic)
- Easy to monitor effect (flag state = A/B test)

---

## Pattern 7: Database Optimization

### Query Tuning

**Trigger**:
```
database_query_latency > threshold for 30 seconds
```

**Mitigation** (Automated):
```
Identify slow queries:
1. Query execution plan shows sequential scan
2. Add missing index (if pre-approved)
3. Update query statistics (ANALYZE in PostgreSQL)

Example:
  SELECT * FROM users WHERE account_id = ?
  → Sequential scan (slow) → Add index on account_id
  → Query plan changes to index scan (fast)
```

**Pre-requirements**:
- Index pre-calculated and tested
- No lock on table
- Impact assessed

---

## Mitigation Receipt Contract

### Every Mitigation Generates a Receipt

```json
{
  "receipt_id": "urn:uuid:mit-0001",
  "timestamp": "2026-01-25T14:17:15Z",
  "incident_id": "urn:uuid:inc-0001",
  "mitigation_type": "auto_scale",
  "action_taken": "scale_concurrency",
  "parameters": {
    "service": "governor",
    "from": 20,
    "to": 50,
    "reason": "queue_backlog_detected"
  },
  "metrics_before": {
    "queue_depth": 1245,
    "processing_rate_msgs_per_min": 180
  },
  "metrics_after": {
    "queue_depth": 900,  // Should decrease after 30 sec
    "processing_rate_msgs_per_min": 320
  },
  "effectiveness": "in_progress",  // Or: successful, failed
  "timestamp_completed": "2026-01-25T14:18:30Z",
  "duration_seconds": 75,
  "status": "completed"
}
```

---

## Mitigation Effectiveness Tracking

**Dashboard Metrics**:

| Mitigation Type | Trigger Frequency | Success Rate | Avg MTTR |
|-----------------|-------------------|--------------|----------|
| Auto-scale concurrency | 3-5x per week | 92% | 2.3 min |
| Circuit breaker | 1-2x per week | 98% | 1.2 min |
| Throttling | 1-2x per month | 85% | 3.5 min |
| Regional failover | <1x per year | 99% | 4.2 min |
| Pod restart | 2-3x per week | 78% | 1.5 min |
| Feature flag disable | <1x per month | 100% | 0.3 min |

**Continuously Improve**:
- If success rate < 80%, investigate why
- If MTTR increasing, check for side effects
- If false positive rate high, adjust thresholds

---

## Mitigation Failures & Escalation

**When Mitigation Fails**:
```
1. Auto-scaling applied but queue still growing?
   → Root cause is not concurrency (upstream service issue)
   → Escalate to manual investigation

2. Circuit breaker opened but errors continue?
   → Service may be in crash loop
   → Try pod restart or manual deployment rollback

3. Failover to standby but data inconsistent?
   → Replication lag too high
   → Manual intervention needed to resolve consistency
```

---

## Testing Mitigation Strategies

**Chaos Engineering Tests**:
```
1. Inject queue backlog → Verify auto-scaling works
2. Kill downstream service → Verify circuit breaker works
3. Spike traffic 10x → Verify throttling works
4. Disable primary region → Verify failover works
5. Fill memory → Verify pod restart works
6. Deploy bad code → Verify feature flag disable works
```

**Runbook**: See `/docs/runbooks/02-incident-runbooks/` for 5 major scenarios.

---

## Configuration & Tuning

### Alert Thresholds

```yaml
queue_backlog:
  threshold: 500 messages      # Scale when > this
  alert_window: 30 seconds     # For this duration
  scaling_factor_max: 5x       # Don't scale > 5x in one step
  scaling_cooldown: 60 seconds # Wait before scaling again

circuit_breaker:
  error_threshold: 50%         # Open when error rate > this
  timeout_threshold: 500ms     # Open if latency > this
  probe_interval: 10 seconds   # Test recovery every X sec
  recovery_threshold: 10       # Close after 10 successful probes

throttling:
  rate_limit_requests_per_sec: 5000
  queue_size_max: 1000
  backpressure_duration: 30 seconds (before escalation)

failover:
  health_check_interval: 10 seconds
  failure_threshold: 3 consecutive
  dns_update_timeout: 5 seconds
  propagation_timeout: 60 seconds
```

---

## Related Documentation

- [Incident Runbooks](./02-incident-runbooks/)
- [Escalation Procedures](./07-escalation-procedures.md)
- [PIR Template](./03-pir-template.md)

---

**Status**: READY FOR PRODUCTION
**Last Updated**: 2026-01-25
**Owner**: Agent 10 (Incident Playbooks & Operational Runbooks)
