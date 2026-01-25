# Failure Modes & Recovery Procedures
## GCP Erlang Autonomics System

**Document Version**: 1.0
**Last Updated**: January 25, 2026
**Audience**: DevOps, SRE, Platform Engineers
**Severity Levels**: CRITICAL, HIGH, MEDIUM, LOW

---

## Table of Contents

1. [Quick Reference](#quick-reference)
2. [Governor Service Failures](#1-governor-service-failures)
3. [Decision Logic Failures](#2-decision-logic-failures)
4. [Action Execution Failures](#3-action-execution-failures)
5. [Cascading Failures](#4-cascading-failures)
6. [Compliance Failures](#5-compliance-failures)
7. [Performance Failures](#6-performance-failures)
8. [Security Failures](#7-security-failures)
9. [Customer Communication](#8-customer-communication)
10. [Recovery Runbooks](#9-recovery-runbooks)
11. [Chaos Engineering Tests](#10-chaos-engineering-tests)

---

## Quick Reference

### Severity Matrix

| Severity | Definition | Response Time | Max Duration |
|----------|-----------|----------------|--------------|
| **CRITICAL** | Service unavailable, data loss risk, compliance violation | 5 minutes | 30 minutes max |
| **HIGH** | Degraded service, potential customer impact | 15 minutes | 2 hours max |
| **MEDIUM** | Minor impact, workaround available | 1 hour | 8 hours max |
| **LOW** | Logging/monitoring issues, no customer impact | 8 hours | 24 hours max |

### Common Failure Scenarios Quick Links

| Scenario | Severity | Detection | Fix Time |
|----------|----------|-----------|----------|
| Governor crash | CRITICAL | Heartbeat missing >30s | <2 min (auto) |
| Database unavailable | HIGH | Query timeout >30s | <5 min (auto) |
| Decision timeout | CRITICAL | Decision >10s | <2 min (manual) |
| False positive throttle | HIGH | Customer report | <30 sec (revert) |
| Rollback failure | CRITICAL | 5 min no completion | <5 min (escalate) |
| Network partition | HIGH | Pub/Sub timeout | <2 min (buffer) |
| Compliance violation | CRITICAL | PII in logs | <5 min (redact) |

---

## 1. Governor Service Failures

### 1.1 Governor Cloud Run Crashes

**Scenario**: Governor service crashes due to OOM, segfault, or timeout on Cloud Run

#### Root Causes
- Memory leak in signal processing loop
- Unbounded state machine growth
- Large receipt batch causes OOM
- Timeout during SPARQL query execution
- Uncaught panic in FSM state transition

#### Detection
```yaml
Metric: Cloud Run instance crashes
Alert Condition: |
  - Governor revision shows 0/N healthy instances (p99 >5s response)
  - Cloud Logging shows panic trace
  - Pub/Sub subscription lag grows (messages not being read)
Trigger Threshold: Heartbeat missing >30s
Alert Latency: <5s
```

**Monitoring Setup**:
```yaml
# Cloud Monitoring query
resource.type="cloud_run_revision"
resource.labels.service_name="gcp-erlang-autonomics-governor"
metric.type="run.googleapis.com/request_latencies"
metric.value > 30000  # milliseconds
```

#### Action (Safe Fail-Closed)
1. **Immediate**: Cloud Run supervisor automatically restarts the revision
   - OTP-like behavior: dead_letter restart strategy
   - Max 3 restart attempts within 60s window
   - If 3 restarts fail → Manual escalation

2. **During crash**: New signals queue in Pub/Sub (buffered indefinitely)
   - No signals are processed, but none are lost
   - Governor state reverts to last saved checkpoint
   - Pending actions remain pending (safe)

3. **Recovery sequence**:
   ```bash
   # Automatic (OTP supervisor pattern)
   1. Detect crash (Cloud Run health check fails)
   2. Spawn new container (500ms)
   3. Load latest state from Firestore (1-2s)
   4. Resume signal processing (100ms)
   5. Emit recovery receipt to BigQuery
   ```

#### Customer Impact
- **Visibility**: Zero customer impact
- **Duration**: <2 minutes (usually <30s with auto-restart)
- **Data Loss**: None (signals buffered in Pub/Sub)
- **Action Loss**: None (pending actions wait for governor recovery)

#### Recovery Verification
```bash
# SRE: Verify Governor healthy
gcloud run services describe gcp-erlang-autonomics-governor --region us-central1

# Expected output:
# - All replicas healthy ✓
# - Latest deployment active ✓
# - Cloud Logging shows no errors from last 5 minutes ✓

# Verify no signal backlog
gcloud pubsub subscriptions describe governor-signal-sub \
  | grep -A5 "backlogBytes"
# Expected: Near 0 (all messages processed)

# Check BigQuery for recovery receipt
bq query --use_legacy_sql=false '
SELECT execution_id, timestamp, status FROM `project.dataset.receipts`
WHERE action = "RECOVERY"
ORDER BY timestamp DESC LIMIT 1'
```

---

### 1.2 Governor Database Unavailable

**Scenarios**:
- BigQuery down for maintenance
- Firestore exceeds quota
- Cloud SQL connection pool exhausted
- Network connectivity issue to databases

#### Detection

**BigQuery Unavailable**:
```yaml
Signal: Query timeout >30s
Detection Method: |
  - Governor attempts SPARQL query
  - Response takes >30s
  - Query status = "BACKENDDATA_UNAVAILABLE"
Alert Trigger: 3 consecutive failed queries
```

**Firestore Unavailable**:
```yaml
Signal: State save fails
Detection Method: |
  - Governor writes state checkpoint to Firestore
  - Write fails with DEADLINE_EXCEEDED
  - Retry budget exhausted
Alert Trigger: 2 consecutive failed writes
```

#### Action (Graceful Degradation)

**Mode: DEGRADED** - Governor enters safe operational mode
```
Timeline:
1. T+0s:   Database query fails
2. T+1s:   Retry with exponential backoff (1s, 2s, 4s)
3. T+7s:   Give up on database, enter DEGRADED mode
4. T+8s:   Governor emits DEGRADED receipt
5. T+9s:   Fallback to in-memory state machine
6. T+10s:  Continue processing signals using cached state
```

**DEGRADED Mode Limitations**:
- No new SPARQL enrichment (use signal cache)
- No receipt emission (cache in memory, replay on recovery)
- No entitlement checks (assume current tier active)
- State machine operates from last known snapshot

**DEGRADED Mode Safety**:
```yaml
In-Memory Cache:
  - Latest 1000 signals (10MB max)
  - State snapshots (100KB max)
  - Pending receipts (500 receipts max)

Risk Mitigation:
  - No new actions taken (FSM frozen)
  - Existing throttles/pauses remain active
  - Manual override available (1-click escalate)
```

#### Customer Impact
- **Duration**: While DB unavailable (typically <5 min, max 30 min)
- **Visibility**:
  - No BigQuery receipts generated (queued in memory)
  - Customers cannot view audit trail (temporary)
  - Governors continue to operate (safe)
- **No service disruption to workloads** (targets unaffected)

#### Recovery Process
```bash
# Automatic on database recovery:
1. Governor detects database responding (5 test queries)
2. Exit DEGRADED mode
3. Replay queued receipts to BigQuery (sorted by timestamp)
4. Resume SPARQL enrichment
5. Emit RECOVERY receipt

# Time estimate:
  - Database recovery: <5 min (GCP SLA)
  - Governor switchback: <30s
  - Receipt replay: <5s (batched)
  - Total: <5:35 minutes
```

#### Recovery Verification
```bash
# Check Governor status
curl https://governor-health-check.example.com/status
# Expected response: { "mode": "NORMAL", "db_connected": true }

# Verify receipt replay
bq query --use_legacy_sql=false '
SELECT COUNT(*) as receipt_count, MAX(timestamp) as latest
FROM `project.dataset.receipts`
WHERE timestamp > TIMESTAMP_SUB(NOW(), INTERVAL 5 MINUTE)'

# Verify no data loss
bq query --use_legacy_sql=false '
SELECT DISTINCT action FROM `project.dataset.receipts`
WHERE timestamp > TIMESTAMP_SUB(NOW(), INTERVAL 10 MINUTE)
ORDER BY action'
```

---

### 1.3 Governor Network Partition

**Scenario**: Governor loses connectivity to Pub/Sub (network partition, firewall rule, quota exceeded)

#### Detection
```yaml
Signal: Pub/Sub subscription read timeout
Condition: |
  - Governor subscription.pull() timeout >30s
  - Multiple consecutive read failures
Trigger: 5 consecutive read failures within 60s
```

#### Action (Local Buffering)
```
Timeline:
1. T+0s:   First Pub/Sub read timeout
2. T+5s:   Retry with backoff
3. T+30s:  Give up on Pub/Sub read, enter BUFFERING mode
4. T+35s:  Governor emits BUFFERING receipt
5. T+40s:  Continue FSM processing from in-memory queue
```

**BUFFERING Mode Operation**:
- Signal inputs: Buffered locally (bounded queue 10MB)
- Processing: FSM continues with queued signals
- Outputs: Pending receipts cached (up to 500)
- Duration: Until Pub/Sub reconnects

**Memory Bounds** (Prevent OOM):
```yaml
Buffer Limits:
  - Signal queue: Max 10,000 signals (10MB)
  - Receipt queue: Max 500 receipts (500KB)
  - State cache: Max 1MB

Overflow Behavior:
  - When buffer full: Drop oldest signals (FIFO)
  - Log dropped signal count
  - Emit BUFFER_OVERFLOW receipt
  - Customer can request replay (backfill from source)
```

#### Customer Impact
- **Duration**: Network partition recovery (<2 min typical GCP)
- **Signals**: May be delayed 1-2 minutes
- **Actions**: Queued locally, executed on reconnect
- **No data loss**: If buffer doesn't overflow

#### Recovery Process
```bash
# Automatic on network recovery:
1. Pub/Sub read succeeds
2. Exit BUFFERING mode
3. Process buffered signals (in order)
4. Emit queued receipts to BigQuery
5. Emit RECOVERY receipt

# Time estimate:
  - Network recovery: <2 min (GCP SLA)
  - Signal processing: 10,000 signals * 10ms = ~100s
  - Receipt replay: <5s
  - Total: <2:10 minutes
```

#### Recovery Verification
```bash
# Check Pub/Sub connection
gcloud pubsub subscriptions pull governor-signal-sub --limit=1

# Verify buffer state
curl https://governor-debug.example.com/buffers
# Expected: { "buffered_signals": 0, "mode": "NORMAL" }

# Check signal lag
gcloud pubsub subscriptions describe governor-signal-sub \
  | grep -A2 "newestRetainedAckTime"
```

---

## 2. Decision Logic Failures

### 2.1 False Positive: Throttles During Legitimate Traffic Spike

**Scenario**: Governor sees legitimate traffic spike and incorrectly throttles service

#### Example
```
Timeline:
T+0:00   - Traffic increases 100% over 2 minutes (legitimate product launch)
T+1:00   - Governor sees 80% traffic increase, enters Warn state
T+2:00   - Traffic continues increasing, Governor enters Intervene
T+2:15   - Governor throttles requests to 50%
T+3:00   - Customer reports: "Service is throttled but metrics show it's healthy"
T+3:30   - SRE realizes false positive
```

#### Prevention Mechanisms

**1. Sustained Signal Window** (5-minute rolling window, not single sample)
```yaml
Signal Validation:
  - Single spike: Check if sustained for 5+ minutes
  - Transient blips: Ignored
  - Legitimate growth: Detected by trend analysis

Thresholds:
  - Warn: 75-80% metric increase, sustained for 3+ minutes
  - Intervene: >90% increase, sustained for 5+ minutes
  - Critical: >99% increase with service timeouts

Trend Analysis:
  - Is metric increasing linearly or exponentially?
  - Is latency proportional to load increase?
  - Do multiple metrics show consistent pattern?
```

**2. Multi-Signal Correlation**
```yaml
Must See Multiple Signals Before Action:
  - High CPU + High Memory + High Latency
  - High Error Rate (>1%) + High Latency
  - Quota exceeded + Sustained spike
  - Never act on single metric alone
```

**3. Dry-Run Decision Mode**
```yaml
Governor Decision Log:
  - WHAT action would be taken
  - WHY (which signals triggered it)
  - CONFIDENCE level (0-100%)

  Only execute if CONFIDENCE > 85%
```

#### Detection of False Positive
```yaml
Detection Triggers:
  - Customer report: Service throttled but health metrics normal
  - Metrics analysis: CPU 40%, Memory 35%, Latency <100ms
  - Business impact: No actual service degradation
  - Signal analysis: Single spike, not sustained
```

#### Immediate Recovery (<30 seconds)
```bash
# 1. SRE reviews Governor decision logs
curl https://governor-debug.example.com/decisions/latest
# Shows: "THROTTLE (confidence: 72%) - high CPU spike"

# 2. SRE clicks "Manual Override" in cloud console
#    OR: Call API endpoint
gcloud run services update governor \
  --set-env-vars="OVERRIDE_MODE=ENABLED,OVERRIDE_ACTION=DISABLE"

# 3. Governor receives override within next 5 seconds
#    (Firestore-based configuration)

# 4. Governor reverts throttle immediately
#    - Remove throttle policy from Load Balancer
#    - Emit OVERRIDE receipt with reason

# 5. Verify revert
gcloud compute health-checks describe governor-loadbalancer-health \
  | grep -A5 "throttle"
# Expected: No throttle policy applied
```

#### Automatic Revert (Safety Measure)
```yaml
Governor Revert Behavior:
  - If false positive detected by Governor: Auto-revert within 30s
  - If customer manually overrides: Immediate revert
  - If metrics normalize: Auto-revert within 3 minutes

Revert Process:
  1. Governor detects metrics normalized (5 min window)
  2. Signal analysis shows no sustained threshold breach
  3. Governor transitions: Intervene → Warn → Stable
  4. Actions unwound: Throttle → Pause → Resume (takes 2-3 min)
  5. Emit REVERT receipt with original action ID
```

#### Root Cause Analysis
```bash
# Post-incident analysis
bq query --use_legacy_sql=false '
WITH decision_log AS (
  SELECT timestamp, action, confidence, signal_vector
  FROM `project.dataset.governor_decisions`
  WHERE timestamp > TIMESTAMP_SUB(NOW(), INTERVAL 1 HOUR)
),
metrics AS (
  SELECT timestamp, cpu_percent, memory_percent, latency_ms, error_rate
  FROM `project.dataset.metrics`
  WHERE timestamp > TIMESTAMP_SUB(NOW(), INTERVAL 1 HOUR)
)
SELECT
  d.timestamp, d.action, d.confidence,
  m.cpu_percent, m.memory_percent, m.latency_ms, m.error_rate
FROM decision_log d
LEFT JOIN metrics m ON TIMESTAMP_DIFF(d.timestamp, m.timestamp, SECOND) < 5
WHERE d.action = "THROTTLE"
ORDER BY d.timestamp DESC'

# Expected: Shows single metric spike without corresponding degradation
```

#### Learning & Prevention
```yaml
Post-Incident Actions:
1. Increase threshold for Intervene: 90% → 95%
2. Increase sustained window: 5 min → 10 min
3. Add confidence check: >85% → >90%
4. Add anti-pattern: "Ignore pure traffic spikes without latency impact"
5. Update documentation with this false positive pattern
```

---

### 2.2 False Negative: Missed Threshold (No Action When Should Act)

**Scenario**: Governor fails to act when service actually degrading

#### Example
```
Timeline:
T+0:00   - Database connection pool leaking connections
T+2:00   - Available connections drop to 10%
T+3:00   - Service starts rejecting requests (5% error rate)
T+4:00   - Governor should intervene but doesn't
T+5:00   - Service becomes completely unavailable
T+6:00   - SRE manually escalates
```

#### Prevention

**1. Comprehensive Signal Coverage**
```yaml
Monitored Metrics:
  ✓ CPU utilization (per container)
  ✓ Memory usage (per container)
  ✓ Request latency (p50, p95, p99)
  ✓ Error rate (HTTP 5xx percentage)
  ✓ Database connections (pool exhaustion)
  ✓ Disk I/O (if applicable)
  ✓ Network throughput
  ✓ Custom SLO metrics

Missing Signals = False Negatives
```

**2. Threshold Testing (Unit Tests)**
```rust
#[test]
fn test_threshold_breach_triggers_intervention() {
    let signals = vec![
        NormalizedSignal {
            metric: "error_rate",
            value: 0.05,  // 5% errors
            threshold: 0.01,  // 1% threshold
        }
    ];

    let governor = Governor::new();
    let action = governor.analyze(&signals);

    assert_eq!(action, Some(Action::Throttle(50)));
}

#[test]
fn test_sustained_threshold_breach() {
    // Test 10+ signals in sustained violation
    let signals = vec![/* 12 signals with error_rate > threshold */];
    let governor = Governor::new();
    let action = governor.analyze(&signals);

    assert_eq!(action, Some(Action::Shed(25)));
}
```

**3. Automated Threshold Validation**
```bash
# Monthly: Verify thresholds against production data
cargo test --test threshold_validation -- --nocapture

# Expected output:
# ✓ Error rate threshold: <1% false negatives
# ✓ Latency threshold: <2% false negatives
# ✓ Memory threshold: <1% false negatives
```

#### Detection After Failure

```yaml
Detection Signals:
  - SRE Notice: Error rate >5% but no Governor action taken
  - Metrics: Action log shows no entry for the time period
  - Signals: Error rate signals received but ignored
  - Result: Service degrades for 2+ minutes without action
```

#### Recovery

**Immediate** (Customer communication):
```bash
# 1. SRE escalates manually
gcloud compute backend-services update target-service \
  --health-checks=governor-override \
  --enable-iap

# 2. Enable manual intervention mode
gcloud run services update governor \
  --set-env-vars="MANUAL_MODE=ENABLED"

# 3. Customer notified: "Incident detected, manual intervention engaged"

# 4. Automatic rollback to previous revision
gcloud run services update target-service \
  --rollback
```

**Root Cause Analysis** (Post-incident):
```bash
# 1. Check signal ingestion
bq query --use_legacy_sql=false '
SELECT metric, COUNT(*) as signal_count
FROM `project.dataset.signals`
WHERE timestamp > TIMESTAMP_SUB(NOW(), INTERVAL 24 HOUR)
GROUP BY metric
ORDER BY signal_count DESC'

# 2. Check if error_rate signal was missing
bq query --use_legacy_sql=false '
SELECT COUNT(*) as error_signals
FROM `project.dataset.signals`
WHERE metric = "error_rate"
AND timestamp > TIMESTAMP_SUB(NOW(), INTERVAL 24 HOUR)'

# 3. If low count: Signal ingestion pipeline failure
# 4. If high count: Governor analysis logic failure
```

#### Compensation
```yaml
Customer Compensation (Tier 3 Enterprise):
  - Root cause: Governor false negative
  - SLA miss: 2 minutes of unavailability
  - Compensation: 1 month free service credit
  - Prevention: Reviewed threshold logic, added signal validation
  - Communication: Root cause analysis sent within 24 hours
```

---

### 2.3 Wrong Action Taken: Rollback Bug

**Scenario**: Governor rolls back to wrong revision (e.g., rolls back current good version instead of previous bad version)

#### Example
```
Timeline:
T+0:00   - Revision v1.2.3 deployed (good)
T+1:00   - Metrics degrade (actually due to data pipeline issue, not code)
T+2:00   - Governor sees degradation
T+2:30   - Governor rolls back to "previous revision"
T+2:45   - Governor rolls back to v1.2.1 (good)
         - But due to bug, selected v1.2.0 (contains old bug)
T+3:00   - New set of errors appear (old bug reactivated)
T+3:30   - SRE realizes wrong revision was selected
```

#### Prevention

**1. Revision Selection Logic Tests**
```rust
#[test]
fn test_rollback_selects_previous_healthy_revision() {
    let revisions = vec![
        Revision { id: "v1.2.3", health: true, deployed: T+1000 },
        Revision { id: "v1.2.2", health: true, deployed: T+800 },
        Revision { id: "v1.2.1", health: false, deployed: T+500 },
        Revision { id: "v1.2.0", health: true, deployed: T+200 },
    ];

    let target = select_rollback_target(&revisions);
    // Must select v1.2.2 (last healthy before current)
    assert_eq!(target.id, "v1.2.2");
    assert_ne!(target.id, "v1.2.1");  // Skip unhealthy
    assert_ne!(target.id, "v1.2.0");  // Never go further back
}

#[test]
fn test_no_rollback_if_no_healthy_previous() {
    let revisions = vec![
        Revision { id: "v1.2.3", health: true, deployed: T+1000 },
        Revision { id: "v1.2.2", health: false, deployed: T+800 },
        Revision { id: "v1.2.1", health: false, deployed: T+500 },
    ];

    let target = select_rollback_target(&revisions);
    // Return None - don't rollback without healthy target
    assert_eq!(target, None);
}
```

**2. Canary Testing on Real Workloads**
```yaml
Before Production Rollback:
  - Governor tests rollback on 10% of traffic
  - Monitors metrics for 1 minute
  - If metrics improve: Execute full rollback
  - If metrics degrade: Abort rollback, try different action

Canary Logic:
  1. Select target revision for rollback
  2. Create canary deployment with 10% traffic
  3. Monitor for 60 seconds
  4. Metrics improved? → Full rollback
  5. Metrics worsened? → Abort, emit ROLLBACK_ABORT receipt
```

**3. Rollback Verification Checklist**
```yaml
Pre-Rollback Verification:
  ✓ Current revision: v1.2.3 (verified in system)
  ✓ Previous revision: v1.2.2 (verified healthy in history)
  ✓ Skip unhealthy: v1.2.1 not selected (verified)
  ✓ Canary metrics: Improvement detected (verified)
  ✓ Traffic: All traffic canary-ed before full rollback (verified)

If ANY check fails: Abort rollback, escalate to manual
```

#### Detection of Wrong Rollback
```yaml
Detection Signals:
  - Metrics don't improve after rollback (expected improvement missing)
  - New error patterns appear (different from original)
  - Governor receipt shows wrong revision ID
  - SRE observes unfamiliar error messages
```

#### Immediate Recovery (1-Click)
```bash
# 1. SRE detects wrong revision was deployed
# 2. SRE clicks "Reverse Rollback" in cloud console
gcloud run services update target-service \
  --revision=v1.2.3 \
  --traffic=100

# 3. Governor detects revision change (5s detection latency)
# 4. Emit ROLLBACK_REVERSED receipt
# 5. Service restored to original version

# Time to full recovery: ~30 seconds
```

#### Root Cause Analysis
```bash
# Review rollback decision logic
bq query --use_legacy_sql=false '
SELECT
  timestamp,
  action,
  selected_revision,
  previous_healthy_revision,
  canary_metrics_before,
  canary_metrics_after,
  decision_confidence
FROM `project.dataset.governor_decisions`
WHERE action = "ROLLBACK"
AND timestamp > TIMESTAMP_SUB(NOW(), INTERVAL 1 HOUR)'

# Expected: selected_revision matches previous_healthy_revision
# If mismatch: Bug in selection logic, fix and redeploy
```

#### Learning & Accountability
```yaml
Post-Incident Actions:
1. Code Review: Examine rollback selection logic
   - Find root cause (off-by-one error? sorting bug?)
   - Write new unit test for this scenario
   - Fix logic, submit PR for review

2. Testing: Add 10+ edge cases for revision selection
   - Multiple unhealthy revisions in history
   - No healthy previous revision
   - Newly deployed revision
   - Revisions with same health status

3. Blame Assignment (Blameless):
   - Root cause: Test coverage gap
   - Owner: Assign to team for remediation
   - Prevention: Add E2E tests for rollback scenarios

4. Customer Communication:
   - Incident: Wrong revision selected by Governor (our fault)
   - Duration: 2 minutes extra unavailability
   - Compensation: 2 months free service
   - Prevention: "We added 15 new tests for rollback scenarios"
```

---

## 3. Action Execution Failures

### 3.1 Rollback Action Hangs

**Scenario**: Governor decides to rollback, sends command to Cloud Run, but rollback hangs and doesn't complete

#### Detection
```yaml
Signal: Rollback command doesn't complete
Condition: |
  - Governor sends rollback command
  - Expected completion time: 2-5 minutes
  - Actual: No completion after 5 minutes
Trigger: Command status still "IN_PROGRESS" after 5 minutes
Alert Latency: <10 seconds
```

#### Action Timeline
```
T+0:00   - Governor sends: POST /rollback?revision=v1.2.2
T+0:30   - Cloud Run API responds: 202 Accepted (request queued)
T+2:30   - Rollback should complete (expected)
T+3:00   - Governor checks status: Still "IN_PROGRESS"
T+3:30   - Monitors warning: Rollback taking longer than expected
T+4:00   - Governor checks status: Still "IN_PROGRESS"
T+4:30   - Governor timeout threshold reached (5 min)
T+5:00   - Governor cancels rollback, escalates to manual intervention
```

#### Causes
- Cloud Run quota exceeded (max concurrent deployments)
- Network connectivity issue between Governor and Cloud Run
- Deadlock in Cloud Run deployment scheduler
- Corrupted revision (malformed deployment manifest)
- Cloud Run API rate limit hit

#### Safe Fail Action (Don't Make Things Worse)
```yaml
On Timeout:
  1. Governor stops waiting (prevents indefinite hang)
  2. Governor does NOT attempt alternative actions
  3. Governor emits ROLLBACK_TIMEOUT receipt
  4. Governor escalates to on-call SRE
  5. Service stays in current state (limbo, but safe)

Why Don't Escalate Automatically:
  - Current revision is questionable (that's why rollback was attempted)
  - Previous revision might be equally bad
  - Cascading actions could make it worse
  - Require human judgment for next step
```

#### Customer Impact
- **Duration**: 5 minutes before escalation
- **Service Status**: Degraded (reason for rollback still present)
- **Actions**: Stuck in "Rollback In Progress" limbo
- **No Further Damage**: Governor doesn't attempt additional actions

#### Manual Recovery Procedure

**Step 1: Assess Current State**
```bash
# Check Cloud Run revision status
gcloud run services describe target-service --region us-central1
# Expected: Traffic split shows which revision is current

# Check last Governor decision
curl https://governor-debug.example.com/last-decision
# Expected: { "action": "ROLLBACK", "status": "TIMEOUT" }

# Check Governor health
curl https://governor-health-check.example.com/status
# Expected: { "health": "DEGRADED", "last_decision": "TIMEOUT" }
```

**Step 2: Investigate Rollback Hang**
```bash
# Check Cloud Run deployment logs
gcloud run operations describe deployment-operation-id --region us-central1

# Check for quota exhaustion
gcloud compute resource-quotas describe

# Check for network connectivity
gcloud beta compute networks connectivity-tests create test-governor-to-run \
  --source=governor-instance \
  --destination=cloud-run-endpoint

# Check API rate limits
gcloud compute rate-limits list
```

**Step 3: Complete Rollback Manually (If Rollback Still Desired)**
```bash
# Option A: Force complete the rollback via gcloud
gcloud run services update target-service \
  --revision=v1.2.2 \
  --traffic=100 \
  --region=us-central1

# Option B: Or deploy directly (if rollback API unavailable)
gcloud run deploy target-service \
  --image=gcr.io/project/service:v1.2.2 \
  --region=us-central1

# Option C: Or if rollback was wrong decision, revert to current
gcloud run services update target-service \
  --revision=v1.2.3 \
  --traffic=100 \
  --region=us-central1
  --update-env-vars=GOVERNOR_OVERRIDE=MANUAL

# Verify completion
for i in {1..10}; do
  status=$(gcloud run services describe target-service --region us-central1 | grep -A2 "Traffic")
  if [[ $status == *"100%"* ]]; then
    echo "Deployment complete"
    break
  fi
  sleep 3
done
```

**Step 4: Notify Governor of Manual Action**
```bash
# Prevent Governor from retrying same rollback
gcloud run services update governor \
  --set-env-vars="LAST_ACTION_OVERRIDE=COMPLETED"

# Or: Disable Governor temporarily
gcloud run services update governor \
  --set-env-vars="GOVERNOR_ENABLED=false"

# Add manual override to Firestore
curl -X POST https://firestore.googleapis.com/v1/projects/PROJECT_ID/databases/default/documents/overrides \
  -d '{
    "fields": {
      "action": { "stringValue": "ROLLBACK_COMPLETED" },
      "timestamp": { "timestampValue": "'$(date -u +"%Y-%m-%dT%H:%M:%SZ")'" },
      "reason": { "stringValue": "Manual recovery from rollback hang" }
    }
  }'
```

#### Prevention

**1. Rollback Pre-Flight Tests**
```bash
# Before production rollback, test on staging
cargo test test_rollback_e2e --features staging

# Simulated rollback on staging every hour
cargo test test_rollback_canary --features staging -- --ignored

# In test: Deploy v1, deploy v2, rollback to v1, verify metrics
```

**2. Timeout Enforcement**
```rust
pub const ROLLBACK_TIMEOUT_SECS: u64 = 300;  // 5 minutes

async fn execute_rollback(revision: &str) -> Result<(), ActuatorError> {
    let timeout = Duration::from_secs(ROLLBACK_TIMEOUT_SECS);

    match tokio::time::timeout(timeout, self.rollback_impl(revision)).await {
        Ok(Ok(())) => Ok(()),
        Ok(Err(e)) => Err(ActuatorError::RollbackFailed { reason: e.to_string() }),
        Err(_) => {
            self.emit_receipt(Receipt::RollbackTimeout { revision }).await;
            Err(ActuatorError::ExecutionTimeout { duration_secs: ROLLBACK_TIMEOUT_SECS })
        }
    }
}
```

**3. Status Polling During Rollback**
```rust
async fn monitor_rollback_status(&self, operation_id: &str) -> Result<(), Error> {
    let start = Instant::now();
    let timeout = Duration::from_secs(300);

    loop {
        if start.elapsed() > timeout {
            return Err(Error::Timeout);
        }

        let status = self.check_operation_status(operation_id).await?;

        match status {
            OperationStatus::Complete => return Ok(()),
            OperationStatus::InProgress => {
                // Still working, continue
                tokio::time::sleep(Duration::from_secs(10)).await;
            }
            OperationStatus::Failed => {
                return Err(Error::OperationFailed(status.error_message));
            }
        }
    }
}
```

---

### 3.2 Rollback Succeeds But Service Doesn't Recover

**Scenario**: Rollback completes, service deployed successfully, but still showing errors

#### Example
```
Timeline:
T+0:00   - Error rate spike detected (5% errors)
T+1:00   - Governor decides to rollback
T+2:00   - Rollback completes successfully
T+2:30   - Service is running v1.2.2 (previous revision)
T+3:00   - But error rate STILL 5% (unexpected!)
T+4:00   - SRE realizes: Issue wasn't the code, it was the database
```

#### Root Causes
- Error in data layer, not application code
- Database schema mismatch after rollback
- Configuration mismatch (new service version needs new config)
- Cached data in upstream systems (CDN, memcache)
- DNS propagation delay (still pointing to old revision)

#### Prevention: Pre-Flight Rollback Tests

**Test on Staging**:
```bash
# Before allowing production rollback, validate on staging
# 1. Deploy current version with known-bad config
# 2. Trigger rollback on staging
# 3. Verify metrics improve within 1 minute
# 4. Check data consistency (no schema mismatches)
# 5. Verify no new errors appear

# Run monthly
cargo test test_rollback_data_consistency --features staging
```

**Test in Production (Canary)**:
```yaml
Rollback Canary Process:
  1. Deploy current version to 100% of traffic
  2. Simulate error condition (or wait for real error)
  3. Select target revision (should be previous)
  4. Route 10% traffic to target revision
  5. Monitor target revision metrics for 60 seconds
  6. If metrics improve: Scale to 100%, complete rollback
  7. If metrics unchanged: Abort rollback, try different action
```

#### Detection: Metrics Not Improving
```bash
# Check if rollback was effective
bq query --use_legacy_sql=false '
WITH rollback_event AS (
  SELECT timestamp, action, target_revision
  FROM `project.dataset.receipts`
  WHERE action = "ROLLBACK"
  ORDER BY timestamp DESC LIMIT 1
),
before_metrics AS (
  SELECT AVG(error_rate) as avg_error_before
  FROM `project.dataset.metrics`
  WHERE timestamp BETWEEN
    TIMESTAMP_SUB((SELECT timestamp FROM rollback_event), INTERVAL 1 MINUTE)
    AND (SELECT timestamp FROM rollback_event)
),
after_metrics AS (
  SELECT AVG(error_rate) as avg_error_after
  FROM `project.dataset.metrics`
  WHERE timestamp BETWEEN
    (SELECT timestamp FROM rollback_event)
    AND TIMESTAMP_ADD((SELECT timestamp FROM rollback_event), INTERVAL 1 MINUTE)
)
SELECT
  b.avg_error_before,
  a.avg_error_after,
  CASE
    WHEN a.avg_error_after < b.avg_error_before * 0.5 THEN "EFFECTIVE"
    ELSE "INEFFECTIVE"
  END as rollback_effectiveness
FROM before_metrics b, after_metrics a'

# Expected: avg_error_after < avg_error_before * 0.5
# If not: Rollback was ineffective, different action needed
```

#### Recovery: Data Consistency Check

**Run Provided Script**:
```bash
# 1. Customer provided with data consistency check script
gcloud sql instances describe target-database --format=json > db-state.json

# 2. Check schema version matches revision
curl https://service-health.example.com/config | jq '.schema_version'

# 3. Compare expected vs actual
# Expected schema for v1.2.2: 42
# Actual schema: 45 (mismatch!)

# 4. Requires manual schema downgrade
gcloud sql instances patch target-database \
  --database-flags=schema_version=42
```

**Manual Remediation**:
```bash
# If database schema is incompatible:

# Option A: Upgrade code back to current version
gcloud run services update target-service \
  --revision=v1.2.3  # Current, broken code but schema-compatible
  --traffic=100

# Option B: Downgrade database schema
gcloud sql instances patch target-database \
  --database-flags=schema_version=42

# Option C: Migrate data to compatible schema
gcloud sql instances clone target-database \
  --clone-name=target-database-v1.2.2-compat \
  --database-flags=schema_version=42

# Then redirect service to new database
```

---

## 4. Cascading Failures

### 4.1 Multiple Governors Interfering

**Scenario**: Cost Circuit Breaker (CCB) throttles traffic → Backlog Valve scales up services → Deploy Guard detects lag → Rollback is triggered

#### Potential Cascade
```
Timeline:
T+0:00   - CPU spike in Cloud Run services
T+0:30   - Cost Circuit Breaker detects high spend, throttles requests 50%
T+1:00   - Request queue builds up (throttle = less throughput)
T+2:00   - Backlog Valve detects queue depth >1000, scales up 2x
T+3:00   - Deploy Guard detects deployment in progress (new instances)
T+3:30   - Deploy Guard sees error rate spike (old+new mixing)
T+4:00   - Deploy Guard decides to rollback
T+4:30   - Rollback reverts CPU-optimized version
T+5:00   - CPU usage goes back up
T+5:30   - Cost Circuit Breaker throttles again
         - CYCLE REPEATS (cascading failure loop!)
```

#### Prevention: Governor Communication Protocol

**1. Shared Signal Bus**
```yaml
Governors Share Status:
  - Cost Circuit Breaker: Publishes "THROTTLE_APPLIED (reason: cost)"
  - Backlog Valve: Publishes "SCALE_UP_IN_PROGRESS"
  - Deploy Guard: Consumes both, understands context
  - Rollback Guard: Knows CCB throttle in effect, doesn't rollback

Signal Flow:
  CCB → Pub/Sub Topic "governor-actions" → All governors consume
```

**2. Action Coordination**
```rust
pub struct GovernorContext {
    my_action: Action,
    other_actions: Vec<(GovernorName, Action)>,
    conflict_detected: bool,
}

async fn should_take_action(&self) -> bool {
    // Check for conflicting actions
    if let Some(conflict) = self.detect_conflict() {
        match conflict {
            // Cost circuit breaker throttling, don't also rollback
            Conflict::OtherThrottling => false,
            // Backlog valve scaling, don't deploy
            Conflict::OtherScaling => false,
            // No conflict, safe to act
            Conflict::None => true,
        }
    } else {
        true
    }
}

fn detect_conflict(&self) -> Option<Conflict> {
    for (governor, action) in &self.other_actions {
        match (&self.my_action, action) {
            // Don't rollback if CCB is already throttling
            (Action::Rollback(_), Action::Throttle(_)) if governor == "ccb" => {
                return Some(Conflict::OtherThrottling);
            }
            // Don't scale if deploy guard is rolling back
            (Action::Scale(_), Action::Rollback(_)) if governor == "deploy_guard" => {
                return Some(Conflict::OtherRollingBack);
            }
            _ => {}
        }
    }
    None
}
```

**3. Don't Re-Act on Cascading Actions**
```rust
async fn analyze_signals(&self, signals: &[NormalizedSignal]) -> Option<Action> {
    // Filter out signals caused by other governors' actions
    let filtered_signals: Vec<_> = signals
        .iter()
        .filter(|s| !self.is_caused_by_other_action(s))
        .collect();

    // Only analyze signals caused by real issues
    if filtered_signals.is_empty() {
        return None;  // No action needed
    }

    // Normal analysis
    self.compute_action(&filtered_signals)
}

fn is_caused_by_other_action(&self, signal: &NormalizedSignal) -> bool {
    // CPU spike caused by recent scale-up? Ignore it
    if signal.metric == "cpu" {
        if let Some(scale_event) = self.recent_scale_event() {
            if signal.timestamp - scale_event.timestamp < Duration::from_secs(60) {
                return true;  // Ignore, caused by scale-up
            }
        }
    }

    // Queue spike caused by throttle? Ignore it
    if signal.metric == "queue_depth" {
        if let Some(throttle_event) = self.recent_throttle_event() {
            if signal.timestamp - throttle_event.timestamp < Duration::from_secs(60) {
                return true;  // Ignore, caused by throttle
            }
        }
    }

    false
}
```

#### Detection: Loop Detection Algorithm

**Detect Cascades in Real-Time**:
```rust
pub struct CascadeDetector {
    action_history: VecDeque<(Timestamp, Action)>,
    max_recent_actions: usize,
}

impl CascadeDetector {
    pub fn is_cascading(&self) -> bool {
        // Pattern: same action within 3 minutes, 3+ times
        // = likely cascade loop

        let recent = self.action_history
            .iter()
            .filter(|(t, _)| t.elapsed() < Duration::from_secs(180))
            .collect::<Vec<_>>();

        if recent.len() < 3 {
            return false;
        }

        // Check if all recent actions are the same
        let first_action = &recent[0].1;
        let all_same = recent.iter().all(|(_, a)| a == first_action);

        all_same  // If same action 3+ times in 3 min = cascade
    }
}

// Usage in Governor
async fn analyze_signals(&self, signals: &[NormalizedSignal]) -> Option<Action> {
    let action = self.compute_action(signals)?;

    // Check for cascade
    if self.cascade_detector.is_cascading() {
        warn!("Cascade detected! Proposed: {:?}", action);
        self.emit_receipt(Receipt::CascadeDetected {
            action: action.clone(),
        }).await;

        // Don't take action, escalate to manual
        return None;
    }

    Some(action)
}
```

#### Recovery: Break the Loop

**Automatic Loop Breaking**:
```yaml
When Cascade Detected:
  1. Governor emits CASCADING_FAILURE receipt
  2. Governor STOPS taking actions (freeze FSM)
  3. Governor queues receipt to Firestore
  4. On-call SRE receives alert: "Cascading failure detected"
  5. SRE must manually break loop:
     - Disable 1-2 governors
     - Or change thresholds
     - Or fix root cause

Why Manual:
  - Automatic solutions might make it worse
  - Human judgment required to break loop safely
  - Can't blindly disable governors (removes protections)
```

**Manual Recovery Steps**:
```bash
# 1. Identify which governors are in the loop
bq query --use_legacy_sql=false '
SELECT action, COUNT(*) as count, MIN(timestamp) as first, MAX(timestamp) as latest
FROM `project.dataset.receipts`
WHERE timestamp > TIMESTAMP_SUB(NOW(), INTERVAL 5 MINUTE)
GROUP BY action
ORDER BY count DESC'
# Expected: See pattern like "THROTTLE, SCALE, ROLLBACK, THROTTLE, SCALE, ..."

# 2. Disable the governor in the middle
gcloud run services update cost-circuit-breaker \
  --set-env-vars="ENABLED=false"

# 3. Wait for cascade to stop
# Other governors will execute their queued actions once, then stop
# (now that the loop is broken)

# 4. Check if issue resolved
for i in {1..10}; do
  status=$(bq query --use_legacy_sql=false '
    SELECT COUNT(*) as recent_actions
    FROM `project.dataset.receipts`
    WHERE timestamp > TIMESTAMP_SUB(NOW(), INTERVAL 30 SECOND)'
    --format=csv | tail -1)

  if [[ $status -lt 2 ]]; then
    echo "Cascade stopped"
    break
  fi
  sleep 5
done

# 5. Re-enable governor with adjusted thresholds
gcloud run services update cost-circuit-breaker \
  --set-env-vars="ENABLED=true,THROTTLE_THRESHOLD=0.85"
```

---

## 5. Compliance Failures

### 5.1 Governor Logs PII to Cloud Logging

**Scenario**: Governor logs customer PII (credit card number, email, SSN) to Cloud Logging

#### Detection
```yaml
Signal: Security scanner finds PII in logs
Trigger: |
  - Automated scan of Cloud Logging
  - Regex pattern matches: Credit card (4532...), SSN (123-45-6789)
  - Manual report from customer: "My PII was in logs"
Alert Latency: <5 minutes (automated scan) or immediate (manual)
```

#### Prevention

**1. Redaction Before Logging**
```rust
pub fn redact_signal(signal: &NormalizedSignal) -> String {
    let signal_str = format!("{:?}", signal);

    // Redact credit cards
    let no_cc = CC_REGEX.replace_all(&signal_str, "****-****-****-****");

    // Redact SSN
    let no_ssn = SSN_REGEX.replace_all(&no_cc, "***-**-****");

    // Redact emails
    let no_email = EMAIL_REGEX.replace_all(&no_ssn, "***@***.com");

    no_email.to_string()
}

// Usage in Governor
async fn process_signal(&self, signal: &NormalizedSignal) -> Result<()> {
    info!("Processing signal: {}", redact_signal(signal));
    // ... rest of processing
}
```

**2. Logging Configuration Validation**
```yaml
# Unit tests for redaction
#[test]
fn test_credit_card_redacted() {
    let signal = NormalizedSignal {
        data: "4532123456789012".to_string(),  // Valid CC number
    };

    let redacted = redact_signal(&signal);
    assert!(!redacted.contains("4532"));
    assert!(redacted.contains("****"));
}

#[test]
fn test_ssn_redacted() {
    let signal = NormalizedSignal {
        data: "123-45-6789".to_string(),  // Valid SSN
    };

    let redacted = redact_signal(&signal);
    assert!(!redacted.contains("123-45"));
    assert!(redacted.contains("***-**"));
}

#[test]
fn test_email_redacted() {
    let signal = NormalizedSignal {
        data: "customer@example.com".to_string(),
    };

    let redacted = redact_signal(&signal);
    assert!(!redacted.contains("customer"));
    assert!(redacted.contains("***@"));
}
```

**3. Data Classification**
```yaml
Signal Types:
  - Classified as PII: customer_id, email, phone, credit_card, ssn
  - Classified as Internal: cpu_percent, memory_usage, latency_ms

Logging Rules:
  - PII signals: Log only action taken, not signal data
  - Internal signals: Log full data

Example:
  ❌ WRONG: info!("Processing signal: {:?}", signal);  // Logs CC number
  ✓ CORRECT: info!("Processing signal type: {}", signal.signal_type);  // Logs type only
```

#### Immediate Recovery (<5 minutes)

**1. Auto-Redaction Filter** (Applied immediately):
```bash
# Deploy redaction filter to Cloud Logging
gcloud logging sinks update _Default \
  --add-filter='resource.type="cloud_run_revision" AND
    (jsonPayload.message=~"4[0-9]{15}" OR
     jsonPayload.message=~"[0-9]{3}-[0-9]{2}-[0-9]{4}" OR
     jsonPayload.message=~"[a-z]+@[a-z]+\.com")'
```

**2. Purge Logs** (Delete PII from Cloud Logging):
```bash
# For HIPAA/CCPA: Must delete within 24 hours
gcloud logging delete-log /cloud-run-revision.googleapis.com/default_json \
  --severity=ERROR \
  --before-delete-confirmation=true
```

**3. Customer Notification** (Immediate):
```yaml
Email to Customer (within 5 minutes):
  Subject: Security Incident Notification: PII in Logs

  Body:
    We detected that your personal information was inadvertently logged.

    Affected Data: Customer email addresses (not credit cards)
    Impact: Read-only access (no external exposure)
    Duration: ~2 hours (Jan 25, 01:00 PM - 03:00 PM UTC)

    Actions Taken:
    ✓ Logs purged from Cloud Logging
    ✓ Auto-redaction filter deployed
    ✓ Code fix submitted for review

    Next Steps:
    - Code fix released in v0.2.1 (Jan 26)
    - Audit logs attached (see below)
    - Monitoring enhanced to prevent recurrence

    Questions? Contact security@company.com
```

#### Root Cause Analysis

```bash
# 1. Find all logs containing PII
gcloud logging read \
  'jsonPayload.message=~"4[0-9]{15}"' \
  --limit 100 \
  --format json > exposed_logs.json

# 2. Analyze which signals contained PII
jq '.[] | {timestamp: .timestamp, message: .jsonPayload.message}' exposed_logs.json

# 3. Find the code that logged PII
git log --all -S 'info!("Processing signal:' \
  --oneline
# Output: abc1234 Governor: Log processed signals

# 4. Review the diff
git show abc1234
# Find: info!("Processing signal: {:?}", signal);

# 5. Create fix
# Replace with: info!("Processing signal type: {}", signal.signal_type);

# 6. Add test to prevent regression
# test_no_pii_in_logs() { ... }
```

#### Compensation & Accountability

```yaml
Customer Compensation:
  - Affected: 1 customer (small exposure)
  - Duration: 2 hours
  - Compensation: 1 month free service

  Affected: 100+ customers (large exposure)
  - Compensation: 3 months free service + 1 credit card monitoring

Internal Accountability:
  - Root cause: Code review didn't catch debug logging
  - Owner: Assign to team
  - Prevention:
    1. Add automated PII scanning to CI
    2. Block debugging logs in production code
    3. Add secrets linting to pre-commit hook
    4. Train team on data classification

Scope:
  - Timeline: Review all logs from last 30 days
  - SRE: Run PII scanner on all logs
  - Legal: File incident report (if HIPAA/GDPR covered)
```

---

### 5.2 GDPR: EU Customer Data in US Region

**Scenario**: EU customer data is being processed in us-central1 instead of eu-west1 (required by GDPR)

#### Detection
```yaml
Signal: Data residency validation failure
Trigger: |
  - Customer from EU deploys Governor
  - Configuration says: region="us-central1" (default)
  - Compliance check: FAIL (GDPR violation!)
Alert Latency: <1 second (pre-deployment check)
```

#### Prevention: Geo-Constraint at Install Time

**1. Mandatory Configuration Validation**
```yaml
# ggen.toml validation
[project]
name = "gcp-erlang-autonomics"
region = "us-central1"

# Validation rule:
# If customer_location == "EU" AND region NOT IN ["eu-west1", "eu-north1"]
#   → FAIL deployment with error

# Example error message:
Error: GDPR Violation - EU customer with non-EU data processing
  Customer Location: EU (from signup)
  Processing Region: us-central1
  Required Region: eu-west1

  Fix: Update ggen.toml:
    region = "eu-west1"
```

**2. Compliance Check at Deployment**
```bash
# Pre-deployment check
cargo make compliance-check

# Verifies:
✓ Customer location matches deployment region
✓ All services in same region
✓ Database in same region
✓ Logging in same region
✓ No cross-region data transfer
```

#### Immediate Recovery (Data Re-localization)

**If Violation Detected**:
```bash
# 1. Stop all data processing (graceful shutdown)
gcloud run services update governor \
  --no-traffic

# 2. Export data from us-central1
gcloud sql export sql target-db-us \
  gs://bucket-us-central1/export-$(date +%s).sql \
  --sql-dialect=postgres

# 3. Create new database in eu-west1
gcloud sql instances create target-db-eu \
  --region=eu-west1 \
  --database-version=POSTGRES_15 \
  --tier=db-custom-4-16384

# 4. Import data to eu-west1
gcloud sql import sql target-db-eu \
  gs://bucket-us-central1/export-*.sql

# 5. Update service configuration
gcloud run services update governor \
  --set-env-vars=DATABASE_REGION=eu-west1,DATABASE_HOST=target-db-eu

# 6. Redirect traffic to eu-west1 service
gcloud run services update governor \
  --region=eu-west1 \
  --traffic=100

# 7. Delete us-central1 data (immutable)
gcloud sql backups delete backup-id --instance=target-db-us
gcloud sql instances delete target-db-us
```

**Time Estimate**: 30-45 minutes for complete re-localization

#### Customer Notification

```yaml
Email (Immediate):
  Subject: URGENT: GDPR Compliance Issue - Action Required

  We detected your data is being processed in the wrong region.

  Issue: EU customer data in US region (GDPR violation)
  Detected: 2026-01-25 10:30 UTC
  Service: Paused (to prevent further violation)

  Actions:
  1. Legal: Notifying DPA within 1 hour
  2. Technical: Relocating data to eu-west1 now
  3. Timeline: Should complete in 30-45 minutes

  Your Data Coordinator: [Contact info]
  Data Protection Officer: [Contact info]

Email (Post-Recovery):
  Subject: RESOLVED: GDPR Compliance Issue

  We have successfully relocated your data to eu-west1.

  Service Status: ✓ Resumed (fully operational)
  Data: ✓ Fully relocated to eu-west1
  Compliance: ✓ GDPR compliant

  Evidence:
  - Export timestamp: 2026-01-25 10:35 UTC
  - Import timestamp: 2026-01-25 11:10 UTC
  - Zero data loss verification: ✓
  - Compliance validation: ✓

  Compensation:
  - 1 month free service (for violation period)
  - Legal review completed (see attached report)
```

---

## 6. Performance Failures

### 6.1 Governor Decision Latency >10s (SLO Miss)

**Scenario**: Governor decision logic takes >10s (SLO: <5s p99)

#### Detection
```yaml
Alert: Decision latency SLO miss
Condition: Governor decision >10s (p99 <5s)
Trigger: 5+ decisions exceed 5s within 1-minute window
Alert Latency: <30 seconds
```

#### Root Cause Analysis

**Potential Causes**:
1. SPARQL query slow (complex graph traversal)
2. Governor state machine blocked (mutex contention)
3. Signal enrichment process slow
4. Database query slow (Firestore, BigQuery)
5. Pub/Sub message large (slow deserialization)

#### Quick Diagnosis
```bash
# 1. Check Governor logs for timing info
gcloud logging read \
  'resource.type="cloud_run_revision" AND
   resource.labels.service_name="governor" AND
   jsonPayload.decision_latency_ms > 5000' \
  --limit 50 \
  --format='table(timestamp, jsonPayload.decision_latency_ms, jsonPayload.slow_operation)'

# Expected output:
# 2026-01-25 10:05:30  10500  "sparql_query_slow"
# 2026-01-25 10:06:45  12300  "firestore_read_slow"

# 2. Identify which operation is slow
bq query --use_legacy_sql=false '
SELECT
  operation,
  AVG(duration_ms) as avg_duration,
  MAX(duration_ms) as max_duration,
  COUNT(*) as count
FROM `project.dataset.governor_timings`
WHERE timestamp > TIMESTAMP_SUB(NOW(), INTERVAL 5 MINUTE)
GROUP BY operation
ORDER BY max_duration DESC'

# 3. Check if correlation with other systems
# Is slow operation correlated with high CPU, memory, or network?

# 4. Check cloud resource metrics
gcloud compute instances describe governor-instance \
  --format='value[](status, cpuUtilization, memoryUtilization)'
```

#### Recovery: Auto-Scale Governor

```bash
# 1. Increase Governor Cloud Run replicas
gcloud run services update governor \
  --min-instances=3 \
  --max-instances=10

# 2. Wait for new replicas to warm up (30-60s)
# 3. Monitor decision latency
for i in {1..20}; do
  latency_p99=$(gcloud monitoring time-series list \
    --filter='metric.type="custom.googleapis.com/governor/decision_latency"' \
    --format='value(points[0].value.double_value)' | sort | tail -1)

  if (( $(echo "$latency_p99 < 5000" | bc -l) )); then
    echo "✓ Decision latency recovered: ${latency_p99}ms"
    break
  fi

  echo "Decision latency: ${latency_p99}ms (waiting...)"
  sleep 3
done

# 4. Verify SLO met
gcloud monitoring dashboard update governor-dashboard \
  --config='config.yaml'
```

#### Prevention: Continuous Benchmarking

```bash
# Weekly: Run decision latency benchmark
cargo bench --bench governor_decision_bench

# Expected output:
# decision_latency/default    time:   [450 ms 465 ms 485 ms]
# ✓ PASS: <500ms (well below 5s SLO)

# If benchmark shows regression:
# 1. Investigate which operation regressed
# 2. Profile with flamegraph
# 3. Optimize or revert change
```

---

## 7. Security Failures

### 7.1 Governor Compromised (Arbitrary Code Execution)

**Scenario**: Attacker gains code execution on Governor (via vulnerability, supply chain attack, etc.)

#### Detection
```yaml
Signal: Suspicious Governor process behavior
Indicators: |
  - Unusual system calls (execve, open with suspicious paths)
  - Network connections to unknown IPs
  - Large unexpected file reads/writes
  - Process privilege elevation attempts
  - Modified Governor binary (checksum mismatch)
Alert: Automatic kill-switch activation
```

#### Detection Mechanisms

**1. Process Monitoring**
```rust
// Governor starts with monitoring enabled
#[tokio::main]
async fn main() {
    // Start security monitor in background
    tokio::spawn(monitor_security_signals());

    // Main Governor loop
    run_governor().await;
}

async fn monitor_security_signals() {
    loop {
        // Monitor for suspicious system calls
        if detect_suspicious_syscalls() {
            emit_security_alert("SUSPICIOUS_SYSCALLS");
            trigger_kill_switch().await;
            return;
        }

        // Monitor for unexpected file access
        if detect_suspicious_file_access() {
            emit_security_alert("SUSPICIOUS_FILE_ACCESS");
            trigger_kill_switch().await;
            return;
        }

        // Monitor for privilege escalation
        if detect_privilege_escalation() {
            emit_security_alert("PRIVILEGE_ESCALATION");
            trigger_kill_switch().await;
            return;
        }

        tokio::time::sleep(Duration::from_millis(100)).await;
    }
}
```

**2. Kill-Switch Logic**
```rust
async fn trigger_kill_switch() {
    // Immediate actions on security detection

    // 1. Stop all processing
    GOVERNOR_ENABLED.store(false, Ordering::SeqCst);

    // 2. Cancel all pending actions
    cancel_all_pending_actions().await;

    // 3. Emit security receipt
    emit_receipt(Receipt::SecurityAlert {
        level: "CRITICAL",
        message: "Compromised - kill-switch activated",
        timestamp: Utc::now(),
    }).await;

    // 4. Shutdown gracefully
    std::process::exit(1);  // Cloud Run will restart us
}
```

#### Immediate Action: Shutdown

```bash
# Cloud Run health check will detect exit(1)
# Within 10-30 seconds:
# 1. Cloud Run marks instance as unhealthy
# 2. Cloud Run terminates instance
# 3. New instance created (clean slate)
# 4. New instance starts with latest image

# Manual verification
gcloud run revisions list --service=governor \
  --sort-by=CREATED --limit=5
# Should show: Latest revision deployed within last 2 minutes
```

#### Investigation & Forensics

```bash
# 1. Collect forensic data before instance termination
#    (Cloud Run keeps logs for 30 days)
gcloud logging read \
  'resource.type="cloud_run_revision" AND
   resource.labels.service_name="governor"' \
  --limit 1000 \
  --format json > governor-logs-forensics.json

# 2. Analyze for intrusion patterns
grep -i "unauthorized\|exec\|shell\|suspicious" governor-logs-forensics.json

# 3. Check for modified files (if detected before crash)
gcloud logging read \
  'jsonPayload.message=~"SUSPICIOUS_FILE_ACCESS"' \
  --format json > file-access-logs.json

# 4. Timeline analysis
jq -r '.[] | "\(.timestamp): \(.jsonPayload.message)"' \
  governor-logs-forensics.json | sort
```

#### Recovery Steps

```bash
# 1. Disable Governor temporarily (manual mode only)
gcloud run services update governor \
  --set-env-vars="ENABLED=false"

# 2. Analyze root cause
# a) Was it a code vulnerability?
#    → Review recent commits, dependencies, CVEs
# b) Was it a supply chain attack?
#    → Check container image signature, build logs
# c) Was it a credential compromise?
#    → Rotate all service account keys

# 3. Patch and rebuild
# a) Fix the vulnerability
git checkout -b fix/security-issue
# ... apply fix ...
git commit -m "Security: Fix arbitrary code execution vulnerability"

# b) Rebuild container with fix
gcloud builds submit . \
  --config=cloudbuild.yaml \
  --substitutions=_IMAGE_TAG=v0.2.1-patched

# c) Re-deploy
gcloud run deploy governor \
  --image=gcr.io/project/governor:v0.2.1-patched \
  --region=us-central1

# 4. Verify security
# a) Run security scanner
trivy image gcr.io/project/governor:v0.2.1-patched

# b) Run integration tests
cargo test --test security_integration_tests

# c) Run fuzzing (if applicable)
cargo fuzz run fuzz_signal_parsing

# 5. Re-enable Governor
gcloud run services update governor \
  --set-env-vars="ENABLED=true"
```

#### Prevention

```yaml
Security Best Practices:

1. Code Review:
   - All changes reviewed by 2+ people
   - Security review required for auth/crypto code
   - Automated scanning (snyk, trivy) in CI

2. Dependencies:
   - Pin to specific versions (no wildcards)
   - Weekly: cargo audit (catch CVEs)
   - Monthly: Review new versions

3. Container Security:
   - Non-root user (no privilege escalation)
   - Read-only filesystem where possible
   - No secrets in image
   - Sign container images

4. Supply Chain:
   - Source code: GitHub signed commits required
   - Build artifacts: Signed by CI/CD system
   - Container registry: Enforce signature verification
   - Dependencies: SBOM (Software Bill of Materials)

5. Monitoring:
   - Process monitoring (syscalls)
   - File access monitoring
   - Network monitoring
   - Continuous security scanning
```

---

### 7.2 Audit Trail Tampered (Logs Deleted)

**Scenario**: Attacker deletes Governor audit logs to cover tracks

#### Prevention: Immutable Logs

**1. Hash Chain Integrity**
```rust
// Each receipt includes hash of previous receipt
pub struct Receipt {
    id: String,
    action: String,
    previous_hash: String,  // SHA256 of previous receipt
    content_hash: String,   // SHA256 of this receipt
    timestamp: DateTime<Utc>,
}

// If someone deletes receipt N, the chain breaks:
// Receipt N-1: previous_hash = SHA256(Receipt N-2)
// Receipt N: [DELETED]
// Receipt N+1: previous_hash = SHA256(Receipt N)  ← MISMATCH!

// Verification
fn verify_chain_integrity(receipts: &[Receipt]) -> bool {
    for window in receipts.windows(2) {
        let [prev, current] = window;
        let prev_hash = SHA256(prev);

        if current.previous_hash != prev_hash {
            return false;  // Chain broken!
        }
    }
    true
}
```

**2. Immutable Backup Logs** (Separate Region)
```bash
# Logs are replicated to separate region
# Immutable storage (Firestore backup, Cloud SQL backup)
# Cannot be deleted by attacker

# Daily backup to gs://immutable-backups/
gsutil -m cp gs://logs/*.json \
  gs://immutable-backups/daily-$(date +%Y%m%d).json

# With object lock enabled:
gsutil object-hold set gs://immutable-backups/daily-*.json
# Now: Even bucket owner can't delete for 30 days
```

#### Detection: Integrity Check Failure

```yaml
Alert: Audit trail integrity failure
Trigger: |
  - Hash chain verification fails
  - Receipts missing from sequence
  - Timestamp gaps detected
  - Receipt count drops unexpectedly
```

#### Recovery: Restore from Backup

```bash
# 1. Detect tampering
bq query --use_legacy_sql=false '
SELECT COUNT(*) as receipt_count FROM `project.dataset.receipts`
WHERE timestamp > TIMESTAMP_SUB(NOW(), INTERVAL 1 DAY)'
# Result: 150 receipts

# Previous day: 250 receipts
# Difference: 100 receipts missing (tampering detected!)

# 2. Verify from backup
gsutil cp gs://immutable-backups/daily-$(date -d yesterday +%Y%m%d).json - | \
  grep -c '"id"'
# Result: 250 receipts in backup

# 3. Restore missing receipts
bq load --source_format=NEWLINE_DELIMITED_JSON \
  --allow_quoted_newlines \
  project:dataset.receipts \
  gs://immutable-backups/daily-$(date -d yesterday +%Y%m%d).json

# 4. Verify restoration
bq query --use_legacy_sql=false '
SELECT COUNT(*) as receipt_count FROM `project.dataset.receipts`'
# Result: 250 receipts restored
```

#### Post-Incident: Forensic Analysis

```bash
# 1. Who deleted the receipts?
gcloud logging read \
  'protoPayload.methodName="cloudsql.instances.delete"' \
  --format='table(timestamp, protoPayload.authenticationInfo.principalEmail)'

# 2. When was it deleted?
gcloud logging read \
  'protoPayload.methodName="cloudsql.instances.delete"' \
  --limit=1 \
  --format='value(timestamp)'

# 3. What IP did it come from?
gcloud logging read \
  'protoPayload.methodName="cloudsql.instances.delete"' \
  --limit=1 \
  --format='value(protoPayload.requestMetadata.callerIp)'

# 4. What credentials were used?
gcloud logging read \
  'protoPayload.authenticationInfo.principalEmail="attacker@example.com"' \
  --format='table(timestamp, protoPayload.methodName, resource.labels.service_account)'
```

---

## 8. Customer Communication

### 8.1 Incident Communication Playbook

**5 Minute Response: Automated Alert Email**
```yaml
To: customers@example.com
Subject: ALERT: Potential service degradation
From: incident-alerts@company.com

Dear Customer,

We detected a potential issue with your service.

Incident Details:
  Time: 2026-01-25 10:15 UTC
  What: Error rate spike (5% errors detected)
  Impact: Request throughput may be reduced
  Duration: Ongoing (started 5 minutes ago)

Current Actions:
  ✓ Alert received by on-call team
  ✓ Investigation in progress
  ✓ Estimated resolution: <15 minutes

What You'll See:
  - Some requests may fail (retry recommended)
  - Latency may increase (still operational)
  - Services not unavailable (operating in degraded mode)

Next Update: 15 minutes from now
Status Page: https://status.company.com

Questions? Contact support@company.com
```

**15 Minute Response: Status Page Update**
```yaml
Status: INVESTIGATING
Component: Governor Service
Impact: Medium - Some customers affected
Updated: 2026-01-25 10:20 UTC

What Happened:
  Autonomous Governor detected error rate spike and is currently
  analyzing the cause. Services are operating normally.

What We're Doing:
  - Collecting diagnostic data
  - Analyzing root cause (database? code?)
  - Testing mitigation strategies

Estimated Impact Resolution: <15 minutes
Next Update: 5 minutes

Follow: https://status.company.com for updates
```

**30 Minute Response: Phone Call (Enterprise)**
```yaml
For Tier 3 Enterprise customers:
  - Direct phone call from on-call SRE
  - 5-minute briefing on situation
  - Estimated recovery time
  - Direct contact for urgent issues

Call Script:
  "Hi [Customer Name],

  This is [SRE Name] from [Company].

  We have an ongoing incident affecting your service.

  Situation:
    - Started: 10 minutes ago
    - Impact: Database latency spike
    - Current status: Investigation phase

  What we're doing:
    - Scaling up database replicas
    - Rerouting traffic to less-loaded instances
    - Estimated 10 minutes to resolution

  What you'll see:
    - Latency may be higher than normal
    - Some requests may timeout
    - Services should recover within 10 minutes

  Questions? I'm here to answer them."
```

**Post-Incident: Root Cause Analysis Email** (Within 24 hours)
```yaml
Subject: Post-Incident Report: Service Degradation Jan 25

Dear Customer,

We completed our analysis of yesterday's incident.

Timeline:
  01:15 PM - Error rate spike detected (5% errors)
  01:17 PM - Governor analyzed root cause (database latency)
  01:20 PM - Scaling actions initiated
  01:30 PM - Service fully recovered
  Total Duration: 15 minutes

Root Cause:
  A failed database connection pool upgrade caused:
    1. Connection leaks (connections not returned to pool)
    2. Pool exhaustion (no connections available for new requests)
    3. Request timeouts and error rate spike
    4. Governor triggered scaling action

Impact:
  - Duration: 15 minutes
  - Affected: ~2% of your requests
  - Data Loss: None
  - SLA: Missed (recovery time exceeded 10 min SLO)

Prevention:
  We've implemented:
    1. Connection pool health monitoring
    2. Automatic pool reset on leak detection
    3. Enhanced pre-flight tests for connection upgrades
    4. Faster alert threshold (3 min → 1 min)

Compensation:
  - 2 hours of service credit applied to your account
  - No action needed on your part

Questions? Reply to this email or contact support@company.com

Sincerely,
[Company] Reliability Team
```

---

## 9. Recovery Runbooks

### 9.1 Governor Reboot (Manual Recovery)

**Use When**: Auto-restart failed, Governor is stuck, or manual recovery needed

**Prerequisites**:
```bash
# Verify you have:
- gcloud CLI configured
- Access to Cloud Run service account
- Firestore read/write access
```

**Procedure**:
```bash
#!/bin/bash
set -euo pipefail

# 1. Check current Governor health
echo "Step 1: Checking current Governor health..."
gcloud run services describe governor --region us-central1 --format=json \
  | jq '.status | {ready_replicas, traffic_targets}'

# 2. Force restart by updating with no-traffic
echo "Step 2: Stopping Governor (removing traffic)..."
gcloud run services update governor \
  --region us-central1 \
  --no-traffic

# 3. Wait for clean shutdown
echo "Step 3: Waiting 30 seconds for graceful shutdown..."
sleep 30

# 4. Restore traffic (triggers new instance)
echo "Step 4: Restarting Governor (restoring traffic)..."
gcloud run services update governor \
  --region us-central1 \
  --traffic=100

# 5. Wait for new instance to start
echo "Step 5: Waiting 60 seconds for new instance to start..."
sleep 60

# 6. Verify health
echo "Step 6: Verifying new Governor is healthy..."
for i in {1..10}; do
  health=$(curl -s https://governor-health.example.com/status || echo '{"healthy":false}')
  healthy=$(echo "$health" | jq '.healthy')

  if [[ "$healthy" == "true" ]]; then
    echo "✓ Governor is healthy!"

    # Get instance info
    echo "New Governor Instance:"
    gcloud run services describe governor --region us-central1 \
      --format='value(status.latestCreatedRevisionName)'
    exit 0
  fi

  echo "Attempt $i/10: Governor not yet healthy (waiting...)"
  sleep 5
done

echo "✗ Governor failed to become healthy after restart"
exit 1
```

---

### 9.2 Database Failover

**Use When**: Primary database down, need to switch to replica

**Procedure**:
```bash
#!/bin/bash
set -euo pipefail

# Determine which database is primary
echo "Checking database status..."
for instance in target-db-primary target-db-replica; do
  status=$(gcloud sql instances describe $instance --format='value(state)')
  echo "  $instance: $status"
done

# Failover to replica
echo "Initiating failover to replica..."
gcloud sql instances failover target-db-primary

# Wait for failover to complete
echo "Waiting for failover to complete (5-10 minutes)..."
for i in {1..60}; do
  status=$(gcloud sql instances describe target-db-primary --format='value(state)')

  if [[ "$status" == "RUNNABLE" ]]; then
    echo "✓ Failover complete!"
    exit 0
  fi

  echo "Failover in progress... ($i/60)"
  sleep 10
done

echo "✗ Failover timed out"
exit 1
```

---

### 9.3 Signal Replay from Audit Log

**Use When**: Signals were lost/dropped during network partition, need to re-process

**Procedure**:
```bash
#!/bin/bash
set -euo pipefail

# Find which signals are missing
echo "Comparing signal sources and receipts..."
signal_count=$(bq query --use_legacy_sql=false '
  SELECT COUNT(*) as count FROM `project.dataset.signals`
  WHERE timestamp > TIMESTAMP_SUB(NOW(), INTERVAL 1 HOUR)'
  --format=csv | tail -1)

receipt_count=$(bq query --use_legacy_sql=false '
  SELECT COUNT(*) as count FROM `project.dataset.receipts`
  WHERE timestamp > TIMESTAMP_SUB(NOW(), INTERVAL 1 HOUR)'
  --format=csv | tail -1)

echo "Signals received: $signal_count"
echo "Receipts generated: $receipt_count"

if (( signal_count > receipt_count * 2 )); then
  echo "⚠️  Many unprocessed signals detected"

  # Get unprocessed signals
  echo "Extracting unprocessed signals..."
  bq query --use_legacy_sql=false '
    SELECT signal_id, timestamp, metric, value
    FROM `project.dataset.signals`
    WHERE signal_id NOT IN (
      SELECT signal_id FROM `project.dataset.receipts`
    )
    AND timestamp > TIMESTAMP_SUB(NOW(), INTERVAL 1 HOUR)
    ORDER BY timestamp
    LIMIT 1000' \
    --format=newline_delimited_json \
    > unprocessed_signals.json

  echo "Replaying $(wc -l < unprocessed_signals.json) signals..."

  # Replay signals to Governor
  for i in $(seq 1 $(wc -l < unprocessed_signals.json)); do
    signal=$(sed -n "${i}p" unprocessed_signals.json)

    curl -X POST https://governor-api.example.com/signals \
      -H "Content-Type: application/json" \
      -d "$signal"

    echo "Replayed signal $i"
    sleep 0.1  # Rate limit to prevent overwhelming
  done

  echo "✓ Signal replay complete"
else
  echo "✓ No signal replay needed (all signals processed)"
fi
```

---

## 10. Chaos Engineering Tests

### 10.1 Weekly: Kill Random Governor Process

**Frequency**: Weekly (Monday 2 AM UTC)
**Duration**: 5 minutes
**Blast Radius**: Single Governor instance (auto-restart tested)

```bash
#!/bin/bash

# Weekly Chaos Test: Kill Governor Process
echo "Starting weekly chaos test: Kill Governor process"

# 1. Get current Governor instance
instance=$(gcloud run services describe governor \
  --region us-central1 \
  --format='value(status.latestCreatedRevisionName)')

echo "Target: $instance"

# 2. Record metrics before
latency_before=$(curl -s https://monitoring.googleapis.com/v3/projects/PROJECT/timeSeries \
  --filter 'metric.type="custom.googleapis.com/governor/decision_latency"' \
  --format=json | jq '.[0].points[0].value.double_value')

echo "Latency before: ${latency_before}ms"

# 3. Kill the instance (remove traffic)
gcloud run services update governor --no-traffic

# 4. Monitor recovery
recovery_time=0
for i in {1..30}; do
  health=$(curl -s https://governor-health.example.com/status || echo '{"healthy":false}')
  healthy=$(echo "$health" | jq '.healthy')

  if [[ "$healthy" == "true" ]]; then
    recovery_time=$((i * 10))
    break
  fi

  sleep 10
done

# 5. Restore traffic
gcloud run services update governor --traffic=100

# 6. Record metrics after
sleep 10
latency_after=$(curl -s https://monitoring.googleapis.com/v3/projects/PROJECT/timeSeries \
  --filter 'metric.type="custom.googleapis.com/governor/decision_latency"' \
  --format=json | jq '.[0].points[0].value.double_value')

echo "Recovery time: ${recovery_time}s"
echo "Latency after: ${latency_after}ms"

# 7. Verify SLOs
if (( recovery_time < 180 )); then
  echo "✓ PASS: Recovery <3 minutes SLO"
else
  echo "✗ FAIL: Recovery >3 minutes"
  exit 1
fi

if (( $(echo "$latency_after < 5000" | bc -l) )); then
  echo "✓ PASS: Latency SLO met"
else
  echo "✗ FAIL: Latency SLO missed"
  exit 1
fi

echo "✓ Weekly chaos test PASSED"
```

### 10.2 Monthly: Inject Network Latency

**Frequency**: Monthly (first Saturday)
**Duration**: 10 minutes
**Impact**: Pub/Sub subscription latency +500ms

```bash
#!/bin/bash

# Monthly Chaos Test: Network latency to Pub/Sub
echo "Starting monthly chaos test: Pub/Sub latency injection"

# 1. Create network policy with latency
cat > network-chaos.yaml <<EOF
apiVersion: networking.gke.io/v1
kind: ServicePolicy
metadata:
  name: inject-pubsub-latency
spec:
  targetRef:
    kind: Service
    name: governor
  rules:
  - from:
    - podSelector:
        matchLabels:
          app: governor
    to:
    - podSelector:
        matchLabels:
          app: pubsub
    tcp:
      port: 443
      latency: "500ms"  # Add 500ms latency
      loss: "1%"         # Add 1% packet loss
EOF

kubectl apply -f network-chaos.yaml

# 2. Monitor metrics during chaos
echo "Monitoring for 10 minutes during latency injection..."
for i in {1..60}; do
  metrics=$(curl -s https://monitoring.googleapis.com/v3/projects/PROJECT/timeSeries \
    --filter 'metric.type="custom.googleapis.com/governor/pubsub_latency"' \
    --format=json)

  latency=$(echo "$metrics" | jq '.[0].points[0].value.double_value')
  echo "PubSub latency: ${latency}ms"

  # Alert if decision latency SLO missed
  decision_latency=$(curl -s https://monitoring.googleapis.com/v3/projects/PROJECT/timeSeries \
    --filter 'metric.type="custom.googleapis.com/governor/decision_latency"' \
    --format=json | jq '.[0].points[0].value.double_value')

  if (( $(echo "$decision_latency > 5000" | bc -l) )); then
    echo "⚠️  Decision latency SLO miss: ${decision_latency}ms"
  fi

  sleep 10
done

# 3. Remove latency injection
kubectl delete -f network-chaos.yaml

# 4. Verify recovery
echo "Verifying recovery..."
sleep 30

final_latency=$(curl -s https://monitoring.googleapis.com/v3/projects/PROJECT/timeSeries \
  --filter 'metric.type="custom.googleapis.com/governor/pubsub_latency"' \
  --format=json | jq '.[0].points[0].value.double_value')

if (( $(echo "$final_latency < 100" | bc -l) )); then
  echo "✓ PASS: Latency recovered"
else
  echo "✗ FAIL: Latency didn't recover: ${final_latency}ms"
  exit 1
fi

echo "✓ Monthly chaos test PASSED"
```

### 10.3 Quarterly: Simulate Region Failure

**Frequency**: Quarterly (first day of quarter)
**Duration**: 15 minutes
**Impact**: Simulated us-central1 failure (Tier 3 only, opt-in)

**Prerequisites**: Must have eu-west1 replica configured

```bash
#!/bin/bash

# Quarterly Chaos Test: Region failure simulation
echo "Starting quarterly chaos test: Region failure"

# Only run if Tier 3 customer and EU replica exists
tier=$(gcloud sql instances describe target-db \
  --format='value(databaseVersion)' | head -c 2)

if [[ "$tier" != "3" ]]; then
  echo "Skipping: Not Tier 3 customer (opt-in test)"
  exit 0
fi

# 1. Verify EU replica exists
eu_status=$(gcloud sql instances describe target-db-eu \
  --region eu-west1 \
  --format='value(state)' || echo "DOES_NOT_EXIST")

if [[ "$eu_status" != "RUNNABLE" ]]; then
  echo "Skipping: EU replica not available"
  exit 0
fi

echo "Target: us-central1 → eu-west1 failover"

# 2. Prevent traffic to us-central1
echo "Blocking us-central1 traffic..."
gcloud compute firewall-rules create chaos-block-us-central1 \
  --direction=INGRESS \
  --priority=1000 \
  --source-ranges="0.0.0.0/0" \
  --deny=all \
  --target-tags="governor-us-central1"

# 3. Monitor failover
echo "Monitoring failover (15 minutes)..."
for i in {1..90}; do
  # Check which region is serving traffic
  current_region=$(curl -s https://governor-health.example.com/region || echo "unknown")
  echo "Current region: $current_region"

  if [[ "$current_region" == "eu-west1" ]]; then
    echo "✓ Successfully failed over to eu-west1"
    failover_time=$((i * 10))
    break
  fi

  sleep 10
done

# 4. Remove the firewall rule (restore us-central1)
echo "Restoring us-central1 traffic..."
gcloud compute firewall-rules delete chaos-block-us-central1 --quiet

# 5. Monitor failback
echo "Monitoring failback (10 minutes)..."
sleep 30
for i in {1..60}; do
  current_region=$(curl -s https://governor-health.example.com/region || echo "unknown")

  if [[ "$current_region" == "us-central1" ]]; then
    echo "✓ Successfully failed back to us-central1"
    break
  fi

  sleep 10
done

# 6. Verify both regions healthy
echo "Verifying both regions..."
for region in us-central1 eu-west1; do
  health=$(curl -s https://governor-$region.example.com/status | jq '.healthy')
  if [[ "$health" == "true" ]]; then
    echo "✓ $region: Healthy"
  else
    echo "✗ $region: Unhealthy"
  fi
done

echo "✓ Quarterly chaos test PASSED"
```

---

## Appendix: Alert Configuration

### Cloud Monitoring Alerts Setup

```yaml
# Deploy these alerts to Cloud Monitoring

alerts:
  - name: governor-heartbeat-missing
    condition: |
      metric.type="custom.googleapis.com/governor/heartbeat"
      AND metric.value == 0
      FOR 30s
    notification_channel: slack-sre
    severity: CRITICAL

  - name: decision-latency-slo
    condition: |
      metric.type="custom.googleapis.com/governor/decision_latency"
      AND metric.percentile_99 > 5000
      FOR 60s
    notification_channel: slack-sre
    severity: HIGH

  - name: database-query-slow
    condition: |
      metric.type="custom.googleapis.com/governor/query_latency"
      AND metric.value > 30000
      FOR 30s
    notification_channel: slack-sre
    severity: HIGH

  - name: mutex-poisoning
    condition: |
      metric.type="custom.googleapis.com/governor/mutex_poisoned"
      AND metric.value > 0
    notification_channel: pagerduty
    severity: CRITICAL

  - name: cascade-detected
    condition: |
      metric.type="custom.googleapis.com/governor/cascading_failure"
      AND metric.value == 1
    notification_channel: pagerduty
    severity: CRITICAL

  - name: compliance-violation
    condition: |
      metric.type="custom.googleapis.com/governor/compliance_check"
      AND metric.value == 0
    notification_channel: legal-team
    severity: CRITICAL
```

---

## Summary

This document covers failure modes and recovery procedures for the GCP Erlang Autonomics Governor system. Key takeaways:

1. **Safe Fail-Closed**: Governor fails safely (no action) rather than taking wrong action
2. **Automatic Recovery**: Most failures auto-recover (<2-5 minutes)
3. **Manual Escalation**: Complex failures escalate to on-call SRE
4. **Communication**: Customers notified at 5 min, 15 min, and 30 min marks
5. **Prevention**: Chaos engineering tests validate recovery procedures weekly
6. **Learning**: Post-incident analysis drives prevention improvements

For questions or updates, contact the SRE team.
