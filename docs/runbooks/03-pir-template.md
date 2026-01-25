# Post-Incident Review (PIR) Template

**Version**: 2.0
**Last Updated**: 2026-01-25
**Status**: Production Ready

> **Core Principle**: Every incident (Sev1/2/3) requires a PIR within 24 hours. PIRs drive continuous improvement through systematic root cause analysis and preventive action items.

---

## PIR Scheduling

**PIR Timing by Severity**:
- **Sev1** (Full Outage): PIR within 4 hours (urgent)
- **Sev2** (Partial Outage): PIR within 24 hours
- **Sev3** (Degradation): PIR within 48 hours
- **Sev4** (Minor): Optional (if pattern emerges)

**PIR Attendees**:
- Incident Commander (led)
- On-Call Engineer(s)
- Service Owner(s)
- Tech Lead (SRE perspective)
- Product Manager (business impact)
- Optional: Customer Success (if customer escalation)

---

## PIR Template

### 1. Incident Summary

**Incident ID**: `[UUID]`
**Severity**: `[Sev1/2/3/4]`
**Duration**: `[Start Time] - [End Time]` (XX minutes)
**Estimated Customer Impact**: `[# accounts affected] / [% of customer base]`

**Executive Summary** (2-3 sentences):
```
Briefly describe what happened from customer perspective:
- What service was affected?
- How long was the impact?
- What could customers not do?

Example: "Pub/Sub message delivery was delayed by up to 2 minutes for 15%
of customers for 12 minutes on Jan 25 at 2:15 PM. Messages eventually
delivered, but processing SLA missed."
```

---

### 2. Timeline of Events

**Format**: Hourly breakdown with key events

```
14:15 UTC  Alert triggered: Queue depth > 1000
14:16 UTC  Auto-scaling initiated: Concurrency 20→50
14:18 UTC  On-call engineer acknowledges alert
14:20 UTC  Queue depth still growing, escalated to Tech Lead
14:22 UTC  Identified root cause: downstream service timeout
14:23 UTC  Circuit breaker opened on downstream service
14:25 UTC  Queue depth peaked at 3,847
14:28 UTC  Recovery initiated: Scale replicas 2→4
14:32 UTC  Queue draining successfully
14:35 UTC  Queue depth normalized < 500
14:40 UTC  All-clear: Error rate < 1%, latency normal
14:45 UTC  Post-mortem initiated
```

**Data Sources**:
- [ ] Check Cloud Logging timestamps
- [ ] Check Cloud Trace for end-to-end latency
- [ ] Check metrics dashboard export (CSV)
- [ ] Check incident ticket comments
- [ ] Check Slack #incidents channel

---

### 3. Root Cause Analysis (5 Whys)

**Follow the chain of causation, don't stop at the first answer**:

#### Root Cause #1
```
Q1: Why did queue depth spike?
A1: Pub/Sub was receiving more messages than Governor could process.

Q2: Why couldn't Governor process faster?
A2: Governor concurrency limit was set too low (20 concurrent actions).

Q3: Why was concurrency limit low?
A3: It was set conservatively based on old capacity estimates.

Q4: Why not updated?
A4: Capacity planning was last done 6 months ago, no recent review.

Q5: Why no regular review?
A5: No automated capacity planning process, relies on manual monitoring.

ROOT CAUSE: Lack of automated capacity planning. Manual process insufficient.
```

#### Root Cause #2 (Deeper)
```
Q1: Why didn't auto-scaling trigger earlier?
A1: Alert threshold was set to queue depth > 1000.

Q2: Why threshold so high?
A2: Threshold tuned to reduce false positives.

Q3: Why reduce false positives?
A3: Previous thresholds generated too many alerts (alert fatigue).

Q4: Why so many alerts?
A4: Baseline metrics not well understood (high variance).

Q5: Why high variance?
A5: Metrics collected at 1-minute interval (too coarse).

ROOT CAUSE: Coarse-grained metrics (1-min) obscure trends, forcing high thresholds.
```

#### Primary Root Cause (Contributing Factors)
- **Factor 1**: Capacity planning outdated (6 months old)
- **Factor 2**: Auto-scaling triggered too late (threshold too high)
- **Factor 3**: Monitoring resolution too coarse (1-minute intervals)

**Single Root Cause Statement**:
"System failed to auto-scale proactively because capacity planning was outdated,
alerts were tuned to minimize false positives (high thresholds), and monitoring
resolution was insufficient to detect early load trends."

---

### 4. What Went Well

**Identify the positive aspects** (prevents only focusing on failures):

- [ ] Auto-mitigation worked (concurrency scaled, queue eventually drained)
- [ ] Alert was triggered automatically (no manual detection needed)
- [ ] On-call engineer responded quickly (acknowledged within 2 minutes)
- [ ] Communication was clear (updated customers every 5 minutes)
- [ ] Circuit breaker pattern prevented cascading failures
- [ ] Rollback capability available (could revert if scaling made worse)
- [ ] Data remained consistent (no data loss during incident)
- [ ] Service did not crash (graceful degradation, not catastrophic failure)

**Reinforce these positive behaviors**: Continue these practices.

---

### 5. What Went Wrong

**Identify the failure modes** (be honest, no blame):

- [ ] Auto-scaling triggered too late (queue depth already at 3,800)
- [ ] Alert threshold too high (missed early warning signs)
- [ ] Capacity planning outdated (assumptions no longer valid)
- [ ] Monitoring metrics too coarse (1-minute intervals insufficient)
- [ ] No predictive scaling (reactive instead of proactive)
- [ ] No circuit breaker on action handler until after timeout occurred
- [ ] Insufficient documentation (engineer had to remember architecture)

**Do not blame individuals**: Focus on system and process failures.

---

### 6. Action Items (Preventive)

**Categorize by type and priority**:

#### High Priority (Implement Within 1 Week)

1. **Lower Auto-Scaling Threshold**
   - Current: Queue depth > 1000
   - New: Queue depth > 500
   - Owner: SRE Team
   - Effort: 30 minutes (configuration change)
   - Expected Impact: Reduce detection time from 30 sec to 10 sec
   - Status: `[ ] Not Started [ ] In Progress [ ] Complete`

2. **Increase Monitoring Resolution**
   - Current: Metrics every 60 seconds
   - New: Metrics every 10 seconds (for queue depth)
   - Owner: Monitoring Team
   - Effort: 2 hours (code change + testing)
   - Expected Impact: Earlier detection of trends
   - Status: `[ ] Not Started [ ] In Progress [ ] Complete`

#### Medium Priority (Implement Within 1 Month)

3. **Implement Predictive Auto-Scaling**
   - Current: Reactive (scale when threshold exceeded)
   - New: Predictive (scale based on 5-minute forecast)
   - Owner: ML/SRE Team
   - Effort: 40 hours (algorithm design + implementation + testing)
   - Expected Impact: Prevent queue depth spikes entirely
   - Status: `[ ] Not Started [ ] In Progress [ ] Complete`

4. **Update Capacity Planning**
   - Current: 6 months old, based on old traffic patterns
   - New: Real-time capacity model updated monthly
   - Owner: Capacity Planning Team
   - Effort: 20 hours (modeling + tooling)
   - Expected Impact: Accurate resource allocation forecasts
   - Status: `[ ] Not Started [ ] In Progress [ ] Complete`

#### Low Priority (Implement Within 3 Months)

5. **Add Circuit Breaker on Action Handler**
   - Current: No circuit breaker (cascading failures if handler slow)
   - New: Circuit breaker opens if error rate > 50%
   - Owner: Backend Team
   - Effort: 20 hours (code + testing)
   - Expected Impact: Prevent cascading failures
   - Status: `[ ] Not Started [ ] In Progress [ ] Complete`

6. **Improve Documentation**
   - Current: Architecture not well documented
   - New: Create runbook for queue scaling scenarios
   - Owner: Documentation Team
   - Effort: 4 hours (write + review)
   - Expected Impact: Faster on-call response times
   - Status: `[ ] Not Started [ ] In Progress [ ] Complete`

---

### 7. Metrics & Impact

**Quantify the incident**:

| Metric | Value |
|--------|-------|
| **Duration** | 25 minutes |
| **Accounts Affected** | 487 / 5,234 (9.3%) |
| **Failed Requests** | 2,847 messages delayed |
| **Revenue Impact** | Estimated $2,300 (SLA credits) |
| **MTTR** | 25 minutes (target < 5) |
| **Customer Complaints** | 12 support tickets filed |
| **Data Loss** | 0 messages lost (eventually delivered) |

**Comparison to Baseline**:
- Previous similar incident (queue spike): 42 minutes MTTR
- Improvement: 67% reduction vs. previous

---

### 8. Follow-Up Items

**Tracking and ownership**:

| Item | Owner | Due Date | Status |
|------|-------|----------|--------|
| Lower queue threshold | SRE | 2026-01-27 | In Progress |
| Increase metrics resolution | Monitoring | 2026-02-01 | Not Started |
| Implement predictive scaling | ML/SRE | 2026-02-25 | Planned |
| Update capacity model | Capacity | 2026-02-15 | Not Started |
| Add circuit breaker | Backend | 2026-04-25 | Backlogged |
| Create runbook | Documentation | 2026-01-29 | Not Started |

**Verification**: Items marked "Complete" require:
- [ ] Code reviewed (if applicable)
- [ ] Tests passing (if applicable)
- [ ] Deployed to production (if applicable)
- [ ] Monitored for effectiveness (if applicable)
- [ ] Documented (if applicable)

---

### 9. Lessons Learned

**Key insights** (candid reflection):

1. **What we learned**:
   - Auto-scaling threshold was too conservative (missed early warning)
   - Capacity planning should be continuous, not annual
   - Monitoring resolution should match SLO targets (1-min too coarse for 5-min SLO)
   - Circuit breaker pattern helps but comes too late (predictive scaling better)

2. **What surprised us**:
   - Queue depth grew from 500 to 3,800 in just 5 minutes (very fast)
   - Customers didn't complain about slow responses, but delayed delivery
   - Auto-recovery worked well once scaling triggered (scaling is effective)

3. **What we'll do differently next time**:
   - Use lower thresholds (accept some false positives vs. missing real incidents)
   - Monitor at higher frequency (10-sec intervals for critical metrics)
   - Build predictive models for high-impact metrics
   - Document architecture decisions (why was threshold set to 1000?)

---

### 10. Sign-Off

**Incident Review Completed By**:

- **Incident Commander**: `[Name]` on `[Date]`
- **Tech Lead Review**: `[Name]` on `[Date]`
- **Manager Approval**: `[Name]` on `[Date]`

**Document Status**: `[DRAFT / FINAL / APPROVED]`

**Circulation**:
- [ ] Engineering Team (all SREs)
- [ ] Product Team (understand impact)
- [ ] Customer Success (context for support)
- [ ] Leadership (monthly review)

---

## PIR Review Cadence

**Weekly**: Review all open action items
- Are we on track?
- Do we need to re-prioritize?
- Any blockers?

**Monthly**: Aggregate insights from 4-5 incidents
- Are we seeing patterns?
- Do thresholds need adjustment?
- Should we change runbooks?

**Quarterly**: Comprehensive incident retrospective
- What categories of incidents increased/decreased?
- Are prevention strategies working?
- Need to update training?

---

## PIR Anti-Patterns (Avoid)

**❌ Don't do this**:
- Blame individuals ("John didn't see the alert")
- Stop at first answer ("The alert didn't trigger" - why not?)
- Only discuss what went wrong (reinforce positives too)
- Create vague action items ("Improve monitoring" - too broad)
- Forget to follow up (action items should have owners + due dates)
- Write PIR without data (speculation vs. facts from logs)

**✅ Do this**:
- Focus on system failures (alert thresholds, process gaps)
- Ask "why" 5+ times to find root cause
- Acknowledge what went well
- Create specific, measurable, actionable items
- Assign owners and dates
- Use data from logs, metrics, traces

---

## PIR Effectiveness Metrics

**Track your PIRs**:

| Metric | Target | Current |
|--------|--------|---------|
| **PIRs completed within SLA** | 100% | 94% |
| **Action items completed on time** | 85% | 72% |
| **Repeat incident rate** | < 10% | 12% |
| **MTTR trend** | Decreasing | -8% last quarter |
| **Customer satisfaction (post-incident)** | > 90% | 87% |

**Continuous Improvement**: Each PIR should result in measurable SLO/MTTR improvement.

---

## Related Documentation

- [Severity Classification](./01-severity-classification.md)
- [Incident Runbooks](./02-incident-runbooks/)
- [Knowledge Base](./05-knowledge-base/)
- [Prevention Strategies](./06-prevention-strategies.md)

---

**Status**: READY FOR PRODUCTION
**Last Updated**: 2026-01-25
**Owner**: Agent 10 (Incident Playbooks & Operational Runbooks)
