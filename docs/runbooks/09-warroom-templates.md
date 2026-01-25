# Warroom Coordination & Communication Protocols

**Version**: 2.0
**Last Updated**: 2026-01-25
**Status**: Production Ready

> **Core Principle**: Clear communication prevents cascading failures. One leader (Incident Commander), clear roles, structured updates.

---

## Warroom Setup

### Creation

**Automated**:
- PagerDuty alert includes Zoom link
- If link missing, Incident Commander creates room immediately
- Zoom URL: `https://ggen.zoom.us/my/ggen-incidents` (standing room)

**Manual** (if automated fails):
```bash
ggen incident warroom create --incident-id <incident-id>
```

### Communication Channels (By Severity)

**Sev1** (Critical):
- Zoom warroom (real-time discussion)
- Slack #incidents (public status updates)
- Email/SMS (customer notifications)
- Pagerduty (alerts and escalations)

**Sev2** (High):
- Slack #incidents (primary)
- Zoom optional (if multiple experts needed)
- Ticket system (detailed status)

**Sev3** (Medium):
- Slack #operations (internal only)
- Ticket system (details)

---

## Warroom Roles

### Incident Commander (Lead)

**Responsibilities**:
1. **Control**: Manage who speaks, avoid chaos
2. **Direct**: Assign tasks ("Bob, check database")
3. **Decide**: Make escalation/rollback decisions
4. **Document**: Ensure scribe captures everything
5. **Communicate**: Update status externally

**Commands**:
- "Let's focus on the database issue, network problem secondary for now"
- "Sarah, can you check if load is distributed?"
- "We're going to roll back the deployment. Bob, is that safe?"
- "We need more information before escalating. Give us 2 minutes"

**Never**:
- Let everyone talk over each other
- Make decisions without context
- Skip escalation when appropriate
- Go silent (always communicate status)

### Technical Lead / Subject Matter Expert (SME)

**Responsibilities**:
1. Diagnose root cause
2. Recommend mitigation actions
3. Validate effectiveness of changes
4. Flag risks ("This will cause 2-minute outage")

**Role in Warroom**:
- Answer technical questions
- Suggest next diagnostic steps
- Approve manual interventions
- Provide historical context ("We had similar issue 3 months ago")

### On-Call Engineer (Primary Responder)

**Responsibilities**:
1. Execute actions (SSH, restart, deploy)
2. Monitor metrics in real-time
3. Report findings to IC
4. Document commands executed

**Role in Warroom**:
- Execute IC's directions
- Report status updates every 2-3 minutes
- Flag uncertainties ("I think this will help but unsure")
- Request help from SMEs

### Scribe (Documentation)

**Responsibilities**:
1. Capture all actions taken
2. Record timestamps
3. Document decisions + rationale
4. Note who said what

**Warroom Scribe Template**:
```
Incident ID: urn:uuid:xyz
Severity: Sev2
Start Time: 2026-01-25 14:15:00 UTC
Incident Commander: Alice
Scribe: Bob

TIMELINE:
14:15 - Alert triggered: Queue depth > 1000
  Action: Alice called emergency warroom
  SME: Joined within 2 min
  On-call: Acknowledged alert

14:16 - Diagnostics:
  Sarah: "Queue depth is 1,200, growing"
  Bob: "Downstream service responding slow (500ms)"
  Recommendation: Investigate downstream

14:17 - Decision:
  Alice: "Open circuit breaker on downstream, Bob execute"
  Bob: "Circuit breaker opening..."
  Result: Success, requests now failing fast
```

### Observers (Optional)

**Allowed**:
- Product manager (business context)
- Customer success (customer communication)
- Manager (visibility)

**Not Allowed in Warroom**:
- Never more than 10 people (too many cooks)
- Stakeholders can read Slack #incidents instead

---

## Warroom Protocol

### Opening (First 5 Minutes)

**IC Opens**:
```
"Starting incident warroom for [incident type].
Severity: [Sev1/2/3]
Start time: [UTC]

Current status: [brief description]

Let me go around: Sarah - database status?
```

**Each Person Reports** (30 seconds each):
- **On-call**: "I see queue depth is 1,200, received page 2 min ago"
- **SME**: "I checked logs, action handler timing out to upstream API"
- **Scribe**: "Recording everything, will send transcript in 1 hour"

### Mid-Incident (Updates Every 5 Minutes)

**IC Asks**:
- "What's our status on that database query?"
- "Has the concurrency scaling helped?"
- "Do we need to escalate?"

**Response Format**:
- "Latency decreasing, down to 600ms (was 1,200ms)"
- "Still working on it, need another 5 minutes"
- "No progress, need to try different approach"

**Decision Point** (at 10+ minutes):
```
"We've been working on this for 10 minutes.
Options:
1. Continue current approach (2 more min to see results)
2. Rollback deployment (5 min, brief outage)
3. Failover to standby (10 min, known to work)

I'm going to try option 1 for another 2 minutes.
If no improvement, we escalate to VP for rollback decision."
```

### Closing (When Resolved)

**IC Announces**:
```
"All clear! Metrics normal, error rate < 1%.
Status:
- Resolution time: 25 minutes
- Root cause: Upstream service OOM
- Temporary fix: Restarted service
- Action items: Create runbook for faster recovery

Let's do PIR tomorrow at 2 PM.
Scribe, send transcript to engineering team.
"
```

---

## Communication Templates

### External Status Update (Post to #incidents)

**Format** (Every 5-15 minutes during Sev1):

```
🚨 Incident Update - Queue Backlog Overflow
================================================
Status: INVESTIGATING
Severity: Sev2 (Partial Outage)
Duration: 14 minutes
Affected: 487 accounts

Current situation:
- Queue depth: 2,100 messages (threshold: 1,000)
- Error rate: 5.2% (target: <1%)
- Auto-scaling: Applied (20→50 concurrency)
- Investigation: Downstream API timeout detected

Timeline:
14:15 - Alert triggered
14:16 - Auto-scaling initiated
14:18 - Downstream service identified as bottleneck
14:29 - (NOW) Working on solution

Next update: In 5 minutes
```

### Escalation Notification

```
🔴 ESCALATION ALERT
===================
Incident: Queue backlog overflow
Duration: 15 minutes MTTR exceeded (target: 5)
Status: Auto-recovery insufficient

Escalation:
- Tech Lead: [Name] PAGED
- VP Engineering: [Name] NOTIFIED
- Decision needed: Manual intervention or rollback

Current actions:
- Warroom active with 4 people
- Investigating upstream service
- Considering rollback of recent deployment

Next communication: Status update in 2 minutes
```

### All-Clear Announcement

```
✅ Incident RESOLVED
====================
Incident ID: urn:uuid:xyz
Severity: Sev2
Duration: 25 minutes
Root Cause: Upstream service OOM, manual restart resolved

Resolution:
- Upstream service restarted (14:35 UTC)
- Queue drained successfully (14:40 UTC)
- Error rate normalized (14:42 UTC)
- All metrics green

Impact:
- Customer impact: 2,847 delayed messages (all delivered)
- SLA breach: 25-min MTTR vs 5-min target
- Revenue impact: ~$2,300 (SLA credit given)

Next Steps:
- Post-incident review: Tomorrow 2 PM UTC
- Temporary fix: None needed (service recovered naturally)
- Permanent fix: Add circuit breaker to prevent cascade
- Runbook: Document for faster recovery next time

Thank you all for quick response!
```

---

## Decision-Making in Warroom

### Rollback Decision Matrix

**When to rollback** (recent deployment):

| Signal | Assessment |
|--------|-----------|
| Error rate spike within 5 min of deployment | Strong candidate for rollback |
| Latency spike within 5 min of deployment | Likely rollback candidate |
| Error rate spike 1+ hour after deployment | Probably not deployment issue |
| Errors specific to new feature | Likely rollback candidate |

**How to decide**:
1. IC: "Was there a recent deployment?"
2. On-call: "Yes, 3 minutes ago. Queue backlog started 2 min later"
3. IC: "We're going to rollback. Bob, verify it's safe and execute"
4. On-call: "Verified - no critical operations in flight, safe to rollback"
5. IC: "Rolling back in 30 seconds. Everyone watch metrics"

### Escalation Decision Matrix

**When to escalate** (call Tech Lead / VP):

| Time | No Escalation | Escalate to Tech | Escalate to VP |
|------|---------------|------------------|-----------------|
| 0-5 min | Working well | Working but slow | Insufficient |
| 5-10 min | Problem solving | Reached plateau | Decision needed |
| 10-15 min | No progress | Stuck | Decision required |
| 15+ min | Still trying | ESCALATE NOW | Immediate action |

**IC Thinking**:
- "We've been at this 12 minutes. Auto-recovery isn't working."
- "Tech Lead says there's nothing else to try without risking cascade."
- "Time to make a business decision: rollback or accept partial outage?"
- "VP input needed: Is short-term outage (rollback) better than ongoing degradation?"

---

## Communication Anti-Patterns (Avoid)

**❌ Don't**:
- Have multiple people talking simultaneously (chaos)
- Make decisions without consultation (miss context)
- Blame individuals ("Bob didn't see the alert")
- Go silent (30+ seconds without update)
- Assume everything is fine (verify before closing)
- Escalate too early (waste VP time for minor issues)
- Escalate too late (miss decision window)

**✅ Do**:
- IC controls who speaks (organized)
- Ask for input before decisions
- Focus on systems, not people
- Update status every 5 minutes
- Verify metrics before declaring resolved
- Escalate when stuck
- Escalate early if Sev1

---

## Warroom Closure Checklist

**Before closing warroom, verify**:
- [ ] Root cause identified (or "unknown" documented)
- [ ] Issue resolved (metrics green, error rate normal)
- [ ] Temporary fix applied (if needed until next business day)
- [ ] Escalation items documented (action items with owners)
- [ ] Customer communication sent
- [ ] PIR scheduled
- [ ] Scribe will send transcript

---

## Related Documentation

- [Escalation Procedures](./07-escalation-procedures.md)
- [PIR Template](./03-pir-template.md)
- [Status Page Management](./10-status-page-management.md)

---

**Status**: READY FOR PRODUCTION
**Last Updated**: 2026-01-25
**Owner**: Agent 10 (Incident Playbooks & Operational Runbooks)
