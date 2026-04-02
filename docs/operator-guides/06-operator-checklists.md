<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [TPS Operator Checklists](#tps-operator-checklists)
  - [Daily Operations Checklist (Every Morning)](#daily-operations-checklist-every-morning)
    - [System Health (2 minutes)](#system-health-2-minutes)
    - [Metrics Verification (2 minutes)](#metrics-verification-2-minutes)
    - [Dashboard Review (3 minutes)](#dashboard-review-3-minutes)
    - [Alerts Review (2 minutes)](#alerts-review-2-minutes)
    - [Log Review (1 minute)](#log-review-1-minute)
    - [Daily Sign-Off](#daily-sign-off)
  - [Weekly Operations Checklist (Every Monday 10:00 AM)](#weekly-operations-checklist-every-monday-1000-am)
    - [Performance Trend Analysis (10 minutes)](#performance-trend-analysis-10-minutes)
    - [Capacity Planning (10 minutes)](#capacity-planning-10-minutes)
    - [Security & Compliance (5 minutes)](#security--compliance-5-minutes)
    - [Capacity & Cost Review (5 minutes)](#capacity--cost-review-5-minutes)
    - [Weekly Sign-Off](#weekly-sign-off)
  - [Monthly Operations Checklist (First Day of Month)](#monthly-operations-checklist-first-day-of-month)
    - [Comprehensive System Audit (30 minutes)](#comprehensive-system-audit-30-minutes)
    - [Performance Audit (20 minutes)](#performance-audit-20-minutes)
    - [Dependency Updates (15 minutes)](#dependency-updates-15-minutes)
    - [Documentation Review (10 minutes)](#documentation-review-10-minutes)
    - [Cost & Capacity Planning (10 minutes)](#cost--capacity-planning-10-minutes)
    - [Disaster Recovery Audit (10 minutes)](#disaster-recovery-audit-10-minutes)
    - [Monthly Sign-Off](#monthly-sign-off)
  - [Ad-Hoc Checklists](#ad-hoc-checklists)
    - [Incident Response Checklist](#incident-response-checklist)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# TPS Operator Checklists

**Version**: 1.0
**Purpose**: Structured checklists for daily/weekly/monthly operations
**Format**: Printable checklists with yes/no validation

---

## Daily Operations Checklist (Every Morning)

**Duration**: 5-10 minutes
**Time**: 9:00 AM every weekday
**Owner**: On-call operator

### System Health (2 minutes)

**Environment**:
```bash
Date: ____________  Operator: ____________  Cluster: _______________
```

**Check 1: Cluster Status**
```
☐ GKE cluster is healthy
  Command: kubectl cluster-info
  Expected: "Kubernetes master is running at..."

  If FAILED:
    ☐ Check GKE console for cluster issues
    ☐ Verify network connectivity
    ☐ Escalate to platform team if cluster is down
```

**Check 2: All Pods Running**
```
☐ All TPS system pods are Running
  Command: kubectl get pods -n tps-system | grep -v Running
  Expected: No output (all Running)

  If FAILED (pods showing Pending, CrashLoopBackOff, etc.):
    ☐ Check pod status: kubectl describe pod <name> -n tps-system
    ☐ Check logs: kubectl logs <name> -n tps-system
    ☐ If OOM: increase memory: kubectl set resources pod <name> -n tps-system --limits memory=1Gi
    ☐ If CrashLoopBackOff: restart pod: kubectl rollout restart deployment/tps-system -n tps-system
```

**Check 3: API Health**
```
☐ API is responding to health check
  Command: curl -s http://localhost:8080/health | jq .status
  Expected: "healthy"

  If FAILED:
    ☐ Check pod logs: kubectl logs -l app=tps-api -n tps-system | tail -30
    ☐ Check if API pod is running: kubectl get pods -l app=tps-api -n tps-system
    ☐ If API pod crashed, check for errors
    ☐ Escalate if unable to restart
```

### Metrics Verification (2 minutes)

**Check 4: Prometheus Collecting Metrics**
```
☐ Prometheus has recent data points
  Access: http://localhost:9090/graph
  Query: up{job="prometheus"}
  Expected: Value = 1 (healthy)

  If FAILED:
    ☐ Check Prometheus pod status: kubectl get pods -l app=prometheus -n tps-system
    ☐ Check Prometheus logs: kubectl logs -l app=prometheus -n tps-system | tail -50
    ☐ Restart Prometheus: kubectl rollout restart statefulset/prometheus -n tps-system
```

**Check 5: Grafana Accessible**
```
☐ Grafana dashboard loads without errors
  Access: http://localhost:3000
  Login: admin / admin (or your password)
  Expected: TPS Overview dashboard displays

  If FAILED:
    ☐ Check Grafana pod: kubectl get pods -l app=grafana -n tps-system
    ☐ Restart Grafana: kubectl rollout restart deployment/grafana -n tps-system
    ☐ Clear browser cache and refresh
```

### Dashboard Review (3 minutes)

**Check 6: Review TPS Overview Dashboard**
```
☐ System Status = GREEN (healthy)
☐ Circuit Breaker = CLOSED (0)
☐ Queue Depth = 40-60% (healthy range)
☐ P99 Latency < 200ms (SLA met)
☐ Error Rate < 1% (normal)
☐ Processing Rate > 5 signals/sec (non-zero)
☐ No Active Alerts (red alerts)

If ANY check fails:
  ☐ Go to corresponding dashboard (Jidoka, Kanban, etc.)
  ☐ Investigate root cause
  ☐ Take corrective action or escalate
```

### Alerts Review (2 minutes)

**Check 7: Check Alert Status**
```
☐ No CRITICAL alerts firing
  Access: http://localhost:3000 > Alerting > Alert Rules

  If alerts are firing:
    ☐ Note which alerts: _________________________
    ☐ Check dashboard drill-down for root cause
    ☐ Follow appropriate incident response runbook
    ☐ Document: When found, what was root cause, how fixed
```

### Log Review (1 minute)

**Check 8: Quick Log Scan**
```
☐ No ERROR or PANIC lines in recent logs
  Command: kubectl logs -l app=tps-system -n tps-system --tail=50 | grep -E "ERROR|PANIC"
  Expected: No output

  If found:
    ☐ Review error context: kubectl logs -l app=tps-system -n tps-system --tail=100
    ☐ Identify component: which service logged the error?
    ☐ Check if error is transient or persistent
    ☐ Take action: restart, scale, or investigate
```

### Daily Sign-Off

```
All checks passed: ☐ YES  ☐ NO

If NO:
  Issues found: _________________________________________________
  Actions taken: _________________________________________________
  Escalated to: _________________________________________________
  Time to resolve: _________________________________________________

Operator signature: __________________  Time: __________________
```

---

## Weekly Operations Checklist (Every Monday 10:00 AM)

**Duration**: 30 minutes
**Frequency**: Once per week
**Owner**: SRE team lead

### Performance Trend Analysis (10 minutes)

**Check 1: Review Last 7 Days of Metrics**
```
☐ P99 Latency trend (7-day view)
  Expected: Stable or improving
  Action if degrading: Investigate what changed

  Baseline P99: _________ ms
  Current P99: _________ ms
  Trend: ☐ Stable  ☐ Improving  ☐ Degrading

  If degrading:
    ☐ Check deployment history: kubectl rollout history deployment/tps-system -n tps-system
    ☐ Check for recent config changes
    ☐ File incident ticket
```

**Check 2: Review Error Rate Trend**
```
☐ Error rate trend (7-day view)
  Expected: < 1% average

  Average error rate: _________%
  Peak error rate: _________%

  If > 1% average:
    ☐ Identify when errors occurred
    ☐ Check logs for error patterns
    ☐ Fix root cause or document as known issue
```

**Check 3: Review Throughput Trend**
```
☐ Throughput trend (7-day view)
  Expected: Stable or growing slightly

  Average throughput: _________ sig/sec
  Peak throughput: _________ sig/sec

  If stable: Good (system is performing consistently)
  If declining: Investigate possible issues
  If spiking: Check if load increased intentionally
```

### Capacity Planning (10 minutes)

**Check 4: Analyze Resource Utilization**
```
☐ Worker CPU utilization (average)
  Command: kubectl top pod -l app=tps-system -n tps-system
  Expected: 30-70% (not too idle, not overloaded)

  Current: _________% CPU, _________% Memory

  If > 80% CPU:
    ☐ Scale workers: kubectl scale deployment/tps-system -n tps-system --replicas=<new-count>
    ☐ Note new replica count: _________

  If < 20% CPU:
    ☐ Could reduce workers to save cost (document decision)
    ☐ Current replicas: _________ (consider reducing)
```

**Check 5: Analyze Queue Behavior**
```
☐ Queue depth patterns
  Peak queue depth (this week): _________%
  Average queue depth: _________%

  If peak > 80% frequently:
    ☐ System is approaching capacity
    ☐ Plan to scale workers next week
    ☐ Notify team of capacity concerns

  If peak consistently 40-60%:
    ☐ Current worker count is appropriate
    ☐ No scaling needed
```

**Check 6: Downstream Service Health**
```
☐ Downstream service performance
  Avg latency: _________ ms
  Error rate: _________%

  If latency > 100ms:
    ☐ Contact downstream team
    ☐ Check if they've made changes
    ☐ Document SLA expectations

  If error rate > 2%:
    ☐ Escalate to downstream team
    ☐ Document issue ticket
```

### Security & Compliance (5 minutes)

**Check 7: Review Access Logs**
```
☐ Check for suspicious access patterns
  Command: kubectl logs -l app=tps-api -n tps-system | grep "401\|403\|suspicious"

  If found suspicious activity:
    ☐ Document: _________________________________________________
    ☐ Escalate to security team
```

**Check 8: Verify Secret Rotation**
```
☐ API keys / secrets haven't been rotated recently
  Check: When was each secret last rotated?
  Expected: Within 90 days

  Action if > 90 days old:
    ☐ Rotate secret: kubectl create secret generic <name> --from-literal=key=value -n tps-system
    ☐ Restart affected pods
    ☐ Document rotation date
```

### Capacity & Cost Review (5 minutes)

**Check 9: Resource Costs**
```
☐ Current cluster costs (from GKE console)
  Compute cost: $ _________ / month
  Storage cost: $ _________ / month
  Total: $ _________ / month

  Compared to last month: ☐ Higher  ☐ Same  ☐ Lower

  If higher:
    ☐ Explain why: _________________________________________________
    ☐ Is cost justified? ☐ YES  ☐ NO (optimize)
```

**Check 10: Upcoming Capacity Needs**
```
☐ Forecast next week's load
  Expected peak throughput: _________ sig/sec
  Current capacity: _________ sig/sec

  Headroom available: _________% (should be > 20%)

  If headroom < 20%:
    ☐ Plan to scale workers before peak
    ☐ Schedule scaling time: _________________
```

### Weekly Sign-Off

```
✓ All checks completed: ☐ YES

Issues identified this week: _________________________________________________________

Actions taken: ______________________________________________________________________

Recommended for next week: ___________________________________________________________

SRE Lead signature: __________________  Date: __________________
```

---

## Monthly Operations Checklist (First Day of Month)

**Duration**: 1-2 hours
**Frequency**: Once per month
**Owner**: Operations Manager

### Comprehensive System Audit (30 minutes)

**Check 1: Full Cluster Health Assessment**
```
☐ All nodes are healthy
  Command: kubectl get nodes
  Expected: All STATUS = Ready

  Unhealthy nodes: _____________________

  If unhealthy:
    ☐ Check node status: kubectl describe node <name>
    ☐ Cordon and drain: kubectl drain <name> --ignore-daemonsets
    ☐ Replace node (GKE will auto-provision)
```

**Check 2: PVC/Storage Health**
```
☐ All persistent volumes are Bound
  Command: kubectl get pvc -n tps-system
  Expected: All STATUS = Bound

  Unbound PVCs: _____________________

  If any unbound:
    ☐ Investigate why PVC couldn't be provisioned
    ☐ Check storage quota
    ☐ Check region availability
```

**Check 3: Image Registry Access**
```
☐ All images can be pulled
  Action: kubectl rollout restart statefulset/prometheus -n tps-system
  (This forces image pull)
  Expected: All pods restart successfully

  If pull fails:
    ☐ Check registry credentials
    ☐ Check image path
    ☐ Check registry availability
```

### Performance Audit (20 minutes)

**Check 4: 30-Day Performance Summary**
```
☐ Generate performance report
  Date range: _____________ to _____________

  Metrics to capture:
    - Max P99 latency: _________ ms (when?)
    - Min P99 latency: _________ ms (when?)
    - Avg P99 latency: _________ ms
    - Max error rate: _________%
    - Avg error rate: _________%
    - Max throughput: _________ sig/sec
    - Avg throughput: _________ sig/sec
    - Circuit breaker incidents: _________ (how many?)
    - Queue full events: _________ (how many?)

  Analysis:
    ☐ Performance improved: ☐ YES  ☐ NO
    ☐ Major incidents occurred: ☐ YES  ☐ NO
    ☐ SLA breaches: ☐ YES  ☐ NO

  If SLA breaches:
    ☐ Document each breach
    ☐ Root cause analysis
    ☐ Preventive action for next month
```

**Check 5: Load Testing Results**
```
☐ Run load test simulation
  Test: Normal load (100 req/sec for 5 min)
  Expected: P99 < 200ms, Error rate < 1%

  Results:
    - P99 latency: _________ ms
    - Error rate: _________%
    - Processing rate: _________ sig/sec

  ☐ Test: Spike load (1000 req/sec for 30 sec)
  Expected: Circuit handles gracefully, queue doesn't exceed 90%

  Results:
    - Max queue depth: _________%
    - Rejections: _________ (should be > 0)
    - Recovery time: _________ sec
```

### Dependency Updates (15 minutes)

**Check 6: Security Patch Review**
```
☐ Check for available Kubernetes patches
  Command: gke-cluster-updates --describe

  Available patches: _____________________

  If patches available:
    ☐ Schedule maintenance window
    ☐ Plan cluster upgrade date: _________________
    ☐ Test patches on dev cluster first
    ☐ Document change plan
```

**Check 7: Application Dependencies**
```
☐ Check for outdated container images
  Command: kubectl get pods -n tps-system -o jsonpath='{.items[*].spec.containers[*].image}' | tr ' ' '\n' | sort | uniq

  Image versions:
    tps-system: _________
    prometheus: _________
    grafana: _________
    jaeger: _________
    loki: _________

  Updates available: ☐ YES  ☐ NO

  If updates available:
    ☐ Review release notes
    ☐ Plan upgrade timing
    ☐ Test on dev cluster
    ☐ Schedule production upgrade
```

### Documentation Review (10 minutes)

**Check 8: Update Documentation**
```
☐ Review and update runbooks if needed
  ☐ Verify procedures are still accurate
  ☐ Add new incident types discovered this month
  ☐ Update any outdated commands or procedures

☐ Review and update dashboards if needed
  ☐ Verify all panels are displaying correctly
  ☐ Add new metrics if needed
  ☐ Remove deprecated panels

☐ Update operational wiki
  ☐ Document any new failure modes discovered
  ☐ Document workarounds for known issues
  ☐ Update capacity planning forecasts
```

**Check 9: Team Training & Knowledge Sharing**
```
☐ Conduct incident review meeting
  ☐ Review incidents from past month
  ☐ Discuss root causes
  ☐ Share lessons learned

  Incidents this month: _________
  Major incidents: _________

  Training topics for team:
    ☐ ________________________
    ☐ ________________________

  Next month training scheduled: _______________
```

### Cost & Capacity Planning (10 minutes)

**Check 10: Monthly Cost Analysis**
```
☐ Review previous month costs
  Compute: $ _________
  Storage: $ _________
  Networking: $ _________
  Total: $ _________

  Month-over-month change: _________%
  Year-to-date total: $ _________

  Cost optimization opportunities:
    ☐ Reduce worker count during off-peak
    ☐ Use preemptible instances for non-critical tasks
    ☐ Optimize storage (archive old logs)
    ☐ Reserve instances for baseline capacity
```

**Check 11: Capacity Forecast**
```
☐ Forecast next 3 months

  Current capacity: _________ sig/sec (at 80% utilization)

  Q1 forecast: _________ sig/sec (growth: ________%)
  Q2 forecast: _________ sig/sec (growth: ________%)

  Scaling plan:
    ☐ Month 1: Scale to _________ sig/sec (date: ___)
    ☐ Month 2: Scale to _________ sig/sec (date: ___)
    ☐ Month 3: Scale to _________ sig/sec (date: ___)

  Budget impact: $ _________
```

### Disaster Recovery Audit (10 minutes)

**Check 12: Backup Verification**
```
☐ Verify database backups exist
  Command: kubectl get pvc -n tps-system | grep backup
  Last backup: _________

  ☐ Test restore procedure (on dev cluster)
  ☐ Document recovery time objective (RTO): _________ min
  ☐ Document recovery point objective (RPO): _________ min
```

**Check 13: Incident Response Plan Review**
```
☐ Runbooks are current and accurate
  ☐ All contact information is up to date
  ☐ Escalation paths are clear
  ☐ All team members have copy of runbooks

☐ Team is trained on incident response
  ☐ Last incident response drill: _________________
  ☐ All on-call engineers trained: ☐ YES  ☐ NO

  Recommended drill date this month: _______________
```

### Monthly Sign-Off

```
MONTHLY AUDIT COMPLETED: ☐ YES

Executive Summary (3-4 sentences):
________________________________________________________________________
________________________________________________________________________
________________________________________________________________________

Key Metrics This Month:
  - Uptime: _________%
  - Avg P99 Latency: _________ ms
  - Avg Error Rate: _________%
  - Major Incidents: _________

Priorities for Next Month:
  1. _________________________________________________________________________
  2. _________________________________________________________________________
  3. _________________________________________________________________________

Operations Manager signature: __________________  Date: __________________
```

---

## Ad-Hoc Checklists

### Incident Response Checklist

Use when investigating any production incident:

```
Incident ID: _______________  Time Detected: _______________
Operator: _______________  Duration: ___ minutes

1. Initial Assessment (< 2 min)
   ☐ Confirm incident is real (not false alarm)
   ☐ Determine severity (P1/P2/P3)
   ☐ Notify team members
   ☐ Open incident channel (#incident-[ID])

2. Diagnosis (< 10 min)
   ☐ Check TPS Overview dashboard
   ☐ Identify affected component (queue, circuit, latency, etc.)
   ☐ Check relevant dashboard (Jidoka, Kanban, Kaizen, etc.)
   ☐ Open Jaeger for detailed traces
   ☐ Hypothesis: Root cause is _______________________________

3. Remediation (varies)
   ☐ Take action per decision tree
   ☐ Document steps taken
   ☐ Verify fix worked (metrics improving)

4. Resolution (< 5 min)
   ☐ Confirm system is back to normal
   ☐ All alerts cleared
   ☐ Post incident message to team

5. Post-Mortem (schedule within 24 hours)
   ☐ Schedule meeting time: _________________
   ☐ Document root cause
   ☐ Identify action items to prevent recurrence
```

---

**Version**: 1.0
**Last Updated**: January 2026
**Print & Post**: Yes (laminate for durability)
