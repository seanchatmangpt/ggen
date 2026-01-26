# Incident Response Playbook

On-call procedures and incident response playbooks for TAI Erlang Autonomics.

---

## Quick Start (First 5 Minutes)

When an incident is declared:

1. **Acknowledge Alert** (30 seconds)
   ```bash
   # Acknowledge in PagerDuty
   pagerduty acknowledge INCIDENT_ID

   # Join incident war room
   # Slack: #tai-autonomics-incidents
   ```

2. **Initial Assessment** (2 minutes)
   ```bash
   # Check service status
   curl https://SERVICE_URL/health

   # View latest errors
   gcloud logging read "severity=ERROR" --limit=20

   # Get metrics snapshot
   gcloud monitoring timeseries list --limit=10
   ```

3. **Declare Severity** (1 minute)
   - **Critical**: Service down or error rate > 50%
   - **High**: Degradation or error rate > 1%
   - **Medium**: Slow performance or < 1% errors
   - **Low**: Informational issues

4. **Notify Stakeholders** (1 minute)
   ```bash
   # Post to Slack
   # :alert: INCIDENT: [Severity] - [Description]
   # https://SERVICE_URL/health
   # Errors: $(gcloud logging read "severity=ERROR" --limit=1)
   ```

---

## Severity Levels

### Critical (SEV-1)

**Criteria**:
- Service completely down (health check failing)
- Error rate > 50%
- Data loss occurring
- Complete feature unavailability

**Response Time**: Immediate (< 5 min)

**Escalation**: Auto-page all on-call engineers

**Initial Actions**:
```bash
# 1. Declare SEV-1 incident
# 2. Page incident commander
# 3. Open war room
# 4. Attempt quick rollback
gcloud run services update-traffic tai-autonomics \
  --to-revisions PREV_REVISION=100
sleep 60
curl https://SERVICE_URL/health
```

### High (SEV-2)

**Criteria**:
- Error rate 1-50%
- Latency P99 > 5s
- Partial service degradation
- Some features working, some failing

**Response Time**: 15 minutes

**Escalation**: Page SRE team if not resolved in 15 min

**Initial Actions**:
```bash
# 1. Investigate error logs
gcloud logging read "severity=ERROR" --limit=50

# 2. Check governor states
# 3. Verify Firestore connectivity
# 4. Monitor metrics
```

### Medium (SEV-3)

**Criteria**:
- Error rate < 1%
- Latency P99 > 1s but < 5s
- Intermittent issues
- Workaround available

**Response Time**: 1 hour

**Escalation**: Create ticket for next sprint

**Initial Actions**:
```bash
# 1. Document issue
# 2. Monitor trends
# 3. Plan fix
```

### Low (SEV-4)

**Criteria**:
- Minor issues
- No customer impact
- Informational
- Can be resolved in planning

**Response Time**: Next business day

**Actions**: Document and assign to backlog

---

## Incident Command System

### Incident Commander (IC)

**Responsibilities**:
- Declares incident start/end
- Manages communications
- Coordinates all teams
- Makes escalation decisions
- Documents timeline

**Initial Actions**:
```bash
# 1. Open war room (Slack thread)
# 2. Assign roles
# 3. Set status update cadence (every 15 min)
# 4. Take notes
# 5. Create incident ticket
```

### Technical Lead (TL)

**Responsibilities**:
- Leads technical investigation
- Directs troubleshooting efforts
- Tests fixes
- Recommends rollback
- Documents root cause

### Communications Lead

**Responsibilities**:
- Updates stakeholders
- Manages status page
- Communicates with customers
- Provides regular updates

### Scribe

**Responsibilities**:
- Documents timeline
- Records decisions
- Takes notes
- Creates post-mortem

---

## Troubleshooting Decision Tree

```
START
│
├─ Health check failing? (503)
│  ├─ YES → Supervisor issue
│  │        1. Check logs: grep "ERROR" logs/*
│  │        2. Restart supervisors
│  │        3. If persist: ROLLBACK
│  │
│  └─ NO → Continue
│
├─ Error rate high? (> 1%)
│  ├─ YES → Check error types
│  │        1. gcloud logging read "severity=ERROR" --limit=50
│  │        2. Identify pattern
│  │        3. If code issue: ROLLBACK
│  │        4. If config: FIX or ROLLBACK
│  │
│  └─ NO → Continue
│
├─ Latency high? (P99 > 1s)
│  ├─ YES → Resource or dependency
│  │        1. Check memory: erlang:memory()
│  │        2. Check Firestore: gcloud firestore databases describe
│  │        3. Scale up or FIX
│  │
│  └─ NO → Continue
│
├─ Receipts missing?
│  ├─ YES → Firestore issue
│  │        1. Check connectivity
│  │        2. Verify IAM permissions
│  │        3. Check quotas
│  │        4. Restart if needed
│  │
│  └─ NO → No issue detected
│
└─ END
```

---

## Incident Response Playbooks

### Playbook 1: Health Check Failing (503)

**Symptoms**:
```
curl https://SERVICE_URL/health
{"status": "unavailable", "reason": "supervisor governance_sup not running"}
```

**Investigation** (5 min):

```bash
# 1. Check recent errors
gcloud logging read "resource.type=cloud_run_revision AND severity=ERROR" --limit=10

# 2. Check supervisor status
# Connect to Erlang console and check
erlang:whereis(governance_sup)

# 3. View startup logs
gcloud logging read --limit=50 --filter="severity>=ERROR"
```

**Root Causes**:
- Configuration error
- Dependency initialization failure
- OOM or resource exhaustion
- Network connectivity

**Resolution** (Choose one):

**Option 1: Configuration Fix**
```bash
# 1. Identify config issue in logs
# 2. Fix config/sys.config
# 3. Rebuild and redeploy
make build
make docker-build
make docker-push
make terraform-apply
```

**Option 2: Restart Service**
```bash
# 1. Cold restart
gcloud run deploy tai-autonomics --image IMAGE_URI

# 2. Monitor health
watch -n 5 'curl -s https://SERVICE_URL/health'
```

**Option 3: Rollback**
```bash
# 1. Get previous revision
PREV=$(gcloud run revisions list --service=tai-autonomics --format='value(name)' --limit=2 | tail -1)

# 2. Rollback traffic
gcloud run services update-traffic tai-autonomics --to-revisions $PREV=100

# 3. Verify
curl https://SERVICE_URL/health
```

**Post-Incident**:
- [ ] Document root cause
- [ ] Update monitoring/alerts
- [ ] Create prevention plan
- [ ] Update runbook

---

### Playbook 2: High Error Rate (> 1%)

**Symptoms**:
```
Error rate spike in Prometheus
tai_http_requests_total{status=~"5.."} increasing
Multiple 500 errors in logs
```

**Investigation** (5 min):

```bash
# 1. Get error rate
gcloud monitoring timeseries list \
  --filter='metric.type="custom.googleapis.com/tai_http_requests_total"'

# 2. Get error details
gcloud logging read "severity=ERROR" --limit=50

# 3. Categorize errors
gcloud logging read "severity=ERROR" \
  --format='table(jsonPayload.error_code, textPayload)' \
  --limit=100
```

**Common Error Categories**:

**Category 1: Validation Errors (400s)**
```
Error: invalid_message_format, missing_field, invalid_tenant_id
Action: Check client requests, monitor if systematic

# Get samples
gcloud logging read 'jsonPayload.reason=~"invalid.*"' --limit=10
```

**Category 2: Business Logic Errors (422s)**
```
Error: entitlement_already_active, state_error, duplicate_message
Action: Check application logic, verify state machines

# Get samples
gcloud logging read 'jsonPayload.type="refusal"' --limit=10
```

**Category 3: Service Errors (503s)**
```
Error: service_unavailable, firestore_error, governor_not_ready
Action: Check service health, restart if needed

# Get samples
gcloud logging read 'jsonPayload.reason="service_unavailable"' --limit=10
```

**Category 4: Timeout Errors (504s)**
```
Error: timeout, processing_delay, slow_response
Action: Scale up or investigate bottleneck

# Get samples
gcloud logging read 'httpRequest.status=504' --limit=10
```

**Resolution by Category**:

**For Validation Errors**:
```bash
# 1. Verify API contract
# 2. Check client configuration
# 3. Monitor for trends
# 4. If spike: contact clients
# 5. Update API documentation
```

**For Business Logic Errors**:
```bash
# 1. Review state machine logic
# 2. Check governor implementation
# 3. Verify test coverage
# 4. If code bug: ROLLBACK and FIX
# 5. Deploy fix
```

**For Service Errors**:
```bash
# 1. Check Firestore health
gcloud firestore databases describe

# 2. Verify network connectivity
# 3. Check IAM permissions
gcloud projects get-iam-policy PROJECT_ID

# 4. Monitor resource usage
erlang:memory()

# 5. If persist: RESTART or ROLLBACK
```

**For Timeout Errors**:
```bash
# 1. Check latency metrics
# 2. Increase max instances
gcloud run deploy tai-autonomics --max-instances=100

# 3. Increase concurrency
export ACTION_POOL_SIZE=50
# Redeploy

# 4. Monitor latency improvements
# 5. If persist: INVESTIGATE BOTTLENECK
```

---

### Playbook 3: High Latency (P99 > 1s)

**Symptoms**:
```
Requests taking > 1 second
P99 latency spike
Customers reporting slow responses
```

**Investigation** (5 min):

```bash
# 1. Get latency metrics
gcloud monitoring timeseries list \
  --filter='metric.type="custom.googleapis.com/tai_http_request_duration_seconds"'

# 2. Identify slow endpoints
gcloud logging read 'duration_ms > 1000' --limit=20

# 3. Check resource usage
# CPU, memory, connections

# 4. Check dependencies
# Firestore latency, Pub/Sub latency
```

**Root Causes**:
- Resource exhaustion (CPU, memory)
- Firestore write latency
- Action executor queue overflow
- Network congestion

**Resolution**:

**If Memory High**:
```bash
# 1. Check memory metrics
erlang:memory()

# 2. Reduce buffer sizes
export RECEIPT_BUFFER_SIZE=1000

# 3. Increase flush frequency
export RECEIPT_FLUSH_INTERVAL_MS=100

# 4. If persist: scale or rollback
gcloud run deploy tai-autonomics --memory=4Gi
```

**If Firestore Slow**:
```bash
# 1. Check Firestore latency
gcloud monitoring timeseries list \
  --filter='metric.type="firestore.googleapis.com/operation_latency"'

# 2. Check for hot spots
# Too many writes to same document

# 3. Check quotas
gcloud firestore admin indexes list

# 4. Scale Firestore capacity
# Or reduce write frequency
```

**If Action Queue Overflow**:
```bash
# 1. Check queue depth
# From logs: action_queue_depth = N

# 2. Increase pool size
export ACTION_POOL_SIZE=50

# 3. Scale horizontally
gcloud run deploy tai-autonomics --max-instances=100

# 4. Re-test latency
```

**If Network Congestion**:
```bash
# 1. Check network metrics
# Depends on monitoring setup

# 2. Scale instances
gcloud run deploy tai-autonomics --max-instances=50

# 3. Use regional endpoints
# Firestore and Pub/Sub closer to service

# 4. Monitor bandwidth usage
```

---

### Playbook 4: Memory Leak / OOM

**Symptoms**:
```
Memory usage growing over time
Process killed with exit code 137
Cloud Run revisions crashing
```

**Investigation** (10 min):

```bash
# 1. Check memory trend
gcloud monitoring timeseries list \
  --filter='metric.type="run.googleapis.com/memory_utilization"'

# 2. Check for OOM kills
gcloud logging read "exit_code=137" --limit=10

# 3. Identify growth pattern
# Gradual: likely memory leak
# Sudden: likely spike in usage

# 4. Connect to Erlang shell and check
erlang:memory()
erlang:statistics(memory)
```

**Diagnosis**:

```erlang
% Check for large processes
lists:sort(
  fun({_,A},{_,B}) -> A > B end,
  [(erlang:process_info(Pid, memory), Pid) || Pid <- erlang:processes()]
).

% Check ETS tables
erlang:system_info(ets_count).
ets:info(receipt_ledger).

% Check binaries
erlang:memory(binary).
```

**Root Causes**:
- Receipt buffer not flushing
- ETS table growing without limit
- Process accumulating messages
- Binary garbage not collected

**Resolution**:

**For Buffer Issues**:
```bash
# 1. Reduce buffer size
export RECEIPT_BUFFER_SIZE=1000

# 2. Increase flush frequency
export RECEIPT_FLUSH_INTERVAL_MS=100

# 3. Verify Firestore write success
# Check for write failures

# 4. Deploy fix and monitor
make docker-build
make docker-push
make terraform-apply
```

**For ETS Issues**:
```bash
# 1. Check ETS table size
ets:info(receipt_ledger, memory).

# 2. Verify retention policy
% Should auto-clean old receipts

# 3. Manually cleanup if needed
ets:delete(receipt_ledger, OldKey).

# 4. Deploy fix
```

**For Process Accumulation**:
```erlang
% 1. Check message queue depths
lists:sort(
  fun({_,A},{_,B}) -> A > B end,
  [{Pid, erlang:process_info(Pid, message_queue_len)} || Pid <- erlang:processes()]
).

% 2. Identify process with large queue
% Check process state

% 3. Restart process or service
supervisor:terminate_child(tai_autonomics_sup, governor_sup).
supervisor:restart_child(tai_autonomics_sup, governor_sup).
```

---

## Communication Templates

### Initial Alert (First Notification)

```
INCIDENT DECLARED: [SEVERITY] - [DESCRIPTION]

Service: TAI Autonomics
Status: Investigating
URL: https://SERVICE_URL/health
Started: [TIMESTAMP]

Updates in: #tai-autonomics-incidents
```

### Status Update (Every 15 min)

```
[15:30] Update: [What we've done]
- Checked [system]
- Found [root cause or continue investigating]
- Next: [action]

Status: Still investigating / Implementing fix / Testing
ETA to resolution: [time]
```

### All Clear (Resolution)

```
INCIDENT RESOLVED

Root Cause: [What happened]
Impact: [What was affected]
Duration: [Start time] - [End time]
Affected Users: [%]

Action Taken:
- [Action 1]
- [Action 2]

Next Steps:
- Post-mortem scheduled
- Preventive measures planned
```

### Post-Mortem (24 Hours Later)

```
POST-MORTEM: [INCIDENT TITLE]

Timeline:
[timestamp] - Incident started
[timestamp] - Detected
[timestamp] - Resolved
[timestamp] - Root cause found

Root Cause:
[Detailed explanation]

Contributing Factors:
[Factors that made incident worse]

Action Items:
- [ ] [Prevention action]
- [ ] [Detection improvement]
- [ ] [Response improvement]
```

---

## Escalation Procedures

### Escalation Level 1 (Tier 1 On-Call)

**Trigger**: Alert received

**Action**:
1. Acknowledge alert (30 sec)
2. Initial investigation (5 min)
3. Determine severity (1 min)
4. If resolvable: start troubleshooting
5. If critical: escalate immediately

**Contact**:
```bash
# Page Tier 1 on-call
pagerduty trigger "Incident" --urgency=high
```

### Escalation Level 2 (Incident Commander)

**Trigger**: Severity 1-2 after 15 minutes OR multiple systems down

**Action**:
1. Page incident commander immediately
2. Brief on investigation status
3. Discuss options (continue fix, rollback, scale)
4. IC makes decision
5. Execute decision

**Contact**:
```bash
# Page IC
pagerduty trigger "Incident" --urgency=high --escalation=ic
```

### Escalation Level 3 (VP Engineering)

**Trigger**: Severity 1 after 30 minutes OR customer-visible impact > 1 hour

**Action**:
1. Brief VP on situation
2. Get executive support if needed
3. Activate war room
4. Get additional resources
5. Focus on fastest resolution

**Contact**:
```bash
# Call VP
# Emergency contact list attached
```

---

## Recovery Procedures

### Quick Recovery (< 5 min)

**For transient issues**:

```bash
# 1. Restart service
gcloud run deploy tai-autonomics \
  --image IMAGE_URI \
  --no-traffic

# 2. Route traffic to new revision
gcloud run services update-traffic tai-autonomics --to-revisions NEW=100

# 3. Verify health
curl https://SERVICE_URL/health

# 4. Monitor metrics (5 min)
watch -n 5 'gcloud run metrics'
```

### Full Rollback (< 10 min)

**For deployment-related issues**:

```bash
# 1. Get previous working revision
PREV=$(gcloud run revisions list \
  --service=tai-autonomics \
  --format='value(name)' \
  --limit=5 | grep -v $(gcloud run services describe tai-autonomics --format='value(status.latestReadyRevision)'))

# 2. Rollback traffic
gcloud run services update-traffic tai-autonomics --to-revisions $PREV=100

# 3. Verify
curl https://SERVICE_URL/health

# 4. Monitor (10 min)
```

### Database Recovery (< 30 min)

**For data-related issues**:

```bash
# 1. List backups
gcloud firestore backups list

# 2. Restore from last known good
gcloud firestore restore BACKUP_ID --database=default

# 3. Verify data
gcloud firestore documents list --collection-id=receipts --limit=5

# 4. Re-test service
curl https://SERVICE_URL/health
```

---

## Incident Checklist

### Before Incident Happens

- [ ] On-call schedule up to date
- [ ] PagerDuty configured
- [ ] Runbooks accessible
- [ ] Alert rules in place
- [ ] Communication channels ready

### During Incident

- [ ] [ ] Incident declared and severity set
- [ ] [ ] IC assigned and war room opened
- [ ] [ ] Investigation started
- [ ] [ ] Status updates every 15 min
- [ ] [ ] Resolution attempted
- [ ] [ ] Rollback ready if needed
- [ ] [ ] All clear message sent

### After Incident

- [ ] [ ] Incident documented
- [ ] [ ] Post-mortem scheduled (24 hours)
- [ ] [ ] Root cause analysis done
- [ ] [ ] Action items created
- [ ] [ ] Prevention measures planned
- [ ] [ ] Team debriefed
- [ ] [ ] Lessons learned captured

---

## Quick Reference

### Emergency Numbers

- On-Call: [PagerDuty]
- Incident Commander: [Number]
- VP Engineering: [Number]
- GCP Support: +1-844-4-GOOGLE

### Critical Dashboards

- Service Health: [Dashboard URL]
- Error Dashboard: [Dashboard URL]
- Performance: [Dashboard URL]

### Critical Runbooks

- Health Check Failing: RUNBOOK.md#health-check-fails
- High Error Rate: RUNBOOK.md#troubleshooting
- Memory Issues: RUNBOOK.md#out-of-memory

---

## References

- Runbook: RUNBOOK.md
- Configuration: CONFIG.md
- Monitoring: MONITORING.md
- Deployment: DEPLOYMENT_CHECKLIST.md
