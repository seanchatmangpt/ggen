# Status Page Management & Customer Communication

**Version**: 2.0
**Last Updated**: 2026-01-25
**Status**: Production Ready

> **Core Principle**: Transparent communication prevents panic. Update status page every 5 minutes during Sev1, every 15 minutes during Sev2. Never leave customers wondering.

---

## Status Page Architecture

### Pages

**External** (for customers):
- URL: `https://status.ggen.io`
- Access: Public (no auth required)
- Audience: Customers, prospects, press
- Updates: Sev1/2 incidents only

**Internal** (for team):
- URL: `https://status-internal.ggen.io`
- Access: Employees only (SAML auth)
- Audience: All staff
- Updates: All incidents (Sev1/2/3/4)

### Components

**Customer-Facing Services**:
- API Endpoints (submit action, check status)
- Pub/Sub Message Delivery
- Dashboard / UI
- Mobile Apps

**Internal-Only Services**:
- Database
- Cache
- Analytics pipeline
- Monitoring system

---

## Status Updates by Severity

### Sev1: Critical (Full Outage)

**Update Frequency**: Every 5 minutes

**First Update** (Within 5 minutes of detection):
```
🔴 MAJOR OUTAGE - API Service
==================================
STATUS: INVESTIGATING

We are experiencing issues with our API service.
Customers are unable to submit actions.
We are actively investigating and will provide
updates every 5 minutes.

Started: 2026-01-25 14:15 UTC
Duration: <5 minutes
Affected: API, Pub/Sub Delivery

We sincerely apologize for the disruption.
```

**Investigating Update** (Every 5 minutes):
```
🔴 MAJOR OUTAGE - API Service (Update #2)
==================================
STATUS: INVESTIGATING

We have identified the root cause:
Pub/Sub message queue backlog due to downstream service slowness.

Actions taken:
- Scaled up consumer concurrency (initiated at 14:17 UTC)
- Opened circuit breaker on slow service (14:18 UTC)
- Still investigating downstream service issues

Current metrics:
- Queue depth: 2,100 messages (target: <500)
- Error rate: 5.2% (target: <1%)
- Latency: Elevated

Next update: 2026-01-25 14:24 UTC
```

**Recovery Update**:
```
🟡 DEGRADED - API Service (Update #4)
==================================
STATUS: RECOVERING

Root cause identified: Upstream service memory exhaustion.
Service restarted and recovering.

Actions completed:
✅ Opened circuit breaker (prevented cascade)
✅ Scaled concurrency (handled backlog)
✅ Identified and restarted upstream service
✅ Queue depth now decreasing

Current metrics:
- Queue depth: 800 messages (was 3,800, improving)
- Error rate: 2.1% (improving from 15%)
- Latency: Normalizing

Expected resolution: <5 minutes
Next update: 2026-01-25 14:29 UTC
```

**Resolved Update**:
```
✅ RESOLVED - API Service
==================================
STATUS: OPERATIONAL

The incident is now RESOLVED.
All services returned to normal operation at 2026-01-25 14:40 UTC.

Root cause: Upstream service memory exhaustion
  → Service auto-recovered after restart
  → Queue drained successfully
  → All requests now processing normally

Timeline:
- Started: 14:15 UTC
- Root cause identified: 14:18 UTC
- Service recovered: 14:35 UTC
- Resolved: 14:40 UTC
- Total duration: 25 minutes

Impact:
- Error rate peak: 15% (2 minutes)
- Affected accounts: 487 / 5,234 (9.3%)
- Messages delayed: 2,847 (all eventually delivered)
- SLA credits: Being issued to affected customers

We apologize for the disruption and appreciate your patience.
```

### Sev2: High (Partial Outage)

**Update Frequency**: Every 15 minutes (or less if escalating)

**Initial Status**:
```
🟡 DEGRADED - Dashboard Performance
==================================
STATUS: INVESTIGATING

Some customers may experience slow load times
or occasional timeouts on the Dashboard.
The API is operating normally.
We are investigating the root cause.

Affected: Dashboard UI only
Not affected: API, Pub/Sub, Mobile apps

Next update: 2026-01-25 15:45 UTC
```

**Resolution Path**:
- Investigating (15 min)
- Implementing fix (15 min)
- Verifying (5 min)
- Resolved (post update)

### Sev3: Medium (Degradation)

**Update Frequency**: Once at start + once at resolution

**Minimal Status**:
```
ℹ️ DEGRADED - Analytics Pipeline
==================================
STATUS: INVESTIGATING

Our analytics pipeline is experiencing delays.
Reports may take up to 1 hour to process
instead of the usual 5-10 minutes.

This does not impact real-time operations or data integrity.
Historical reports will be available.

Estimated resolution: 2 hours
```

### Sev4: Minor

**No Status Page Update Required**

---

## Communication Templates

### Status Page Message Guidelines

**Good**:
- "Queue backlog detected at 14:17 UTC. Scaling concurrency in progress."
- "Root cause: Upstream service returned 503 Service Unavailable"
- "ETA for resolution: 10 minutes. Next update in 5 minutes."

**Bad**:
- "System broken lol" (too informal)
- "Database crashed and we don't know why" (too technical, alarming)
- "Will update you sometime" (no ETA)
- "It's not our fault, it's upstream" (not customer's problem)

### Tone Guidelines

**Professional but honest**:
- Acknowledge the issue (don't minimize)
- Provide timeline (don't speculate)
- Explain what we're doing (transparency builds trust)
- Apologize for impact (show empathy)

### Technical Details

**Include** (for Sev1):
- What service is affected
- What customers can't do
- Root cause (once identified)
- What we're doing to fix it
- ETA for resolution

**Don't Include**:
- Internal tool names (Kubernetes, Firestore)
- Blame individuals ("Bob misconfigured the database")
- Speculation ("Probably a network issue")
- Technical jargon (stick to customer language)

---

## Incident Page Setup

### Before Incident

```
1. Have template ready (pre-filled with basic details)
2. Test that status page can be updated quickly
3. Ensure on-call knows how to update it
4. Have backup person if on-call can't update
```

### During Incident

```
1. Create incident on status page (set time to ASAP)
2. Set severity (🔴 Critical / 🟡 Degraded / ℹ️ Minor)
3. Post initial message (within 2 minutes)
4. Set update frequency (every 5/15 min)
5. Monitor that updates are posted (IC/scribe responsible)
```

### After Incident

```
1. Mark incident as RESOLVED
2. Include root cause (brief)
3. Include timeline (total duration)
4. Include impact (# customers, # messages affected)
5. Include link to status page post-mortem (if applicable)
```

---

## Customer Notification Channels

### Sev1: All Channels

**Email**: Sent immediately to all affected customers
```
Subject: Service Outage - ggen API Unavailable

We are currently experiencing service outage affecting API availability.
Started: 2026-01-25 14:15 UTC
Status: INVESTIGATING (updates every 5 minutes)
Status page: https://status.ggen.io

We will update this email thread as we make progress.
```

**Slack**: Post to shared customer Slack channel (if exists)
**SMS**: SMS alert to customers with premium support (optional)
**Twitter**: Public announcement on @ggen_incidents account

### Sev2: Limited Channels

**Email**: Sent to affected customers only
**Slack**: Post to customer channel (if exists)
**Status Page**: Primary channel

### Sev3/4: Status Page Only

No email/SMS/Slack needed (minimal impact).

---

## Post-Incident Actions

### Incident Page Closure

```
✅ RESOLVED
Status: All systems operational

Resolution completed: 2026-01-25 14:40 UTC
Total duration: 25 minutes
Root cause: Upstream service memory exhaustion

Detailed post-incident review coming in 24 hours.
```

### Follow-Up Email

Sent within 24 hours after resolution:

```
Subject: Post-Incident Summary - Queue Backlog Incident on Jan 25

Dear Valued Customer,

We have completed our investigation into the incident that
occurred on 2026-01-25 from 14:15-14:40 UTC.

WHAT HAPPENED:
Pub/Sub message queue backlog caused delayed message delivery.
Some customers experienced up to 60-second delays.

ROOT CAUSE:
Upstream service (Action Handler) experienced memory exhaustion
and became unresponsive, causing queue to grow faster than
we could process messages.

WHAT WE DID:
1. Detected slowdown and triggered auto-scaling (14:16)
2. Opened circuit breaker to prevent cascade failure (14:18)
3. Identified and restarted upstream service (14:28)
4. Queue drained and service recovered (14:40)

IMPACT:
- 487 affected customers / 5,234 total
- 2,847 delayed messages (all eventually delivered)
- 25-minute total duration
- 0 messages lost

ACTIONS WE'RE TAKING:
1. Add circuit breaker to prevent similar cascade failures
2. Increase memory limits for upstream service
3. Implement predictive scaling to catch issues earlier
4. Improve monitoring for early detection

SLA CREDIT:
If you have premium support, we are issuing $X credit
to your account for this incident.

We sincerely apologize for the disruption and appreciate
your patience while we investigated and resolved the issue.

Questions? Contact our support team.
```

---

## SLA & Uptime Reporting

### Monthly Report

Sent to all customers (public on status page):

```
UPTIME REPORT - January 2026
============================

Overall Uptime: 99.92%
Downtime: 11.5 minutes total
Incidents: 2

INCIDENTS:
1. Queue backlog (Jan 25, 14:15-14:40): 25 min
   - Severity: Sev2
   - Impact: Message delivery delays
   - Root cause: Upstream service memory exhaustion

2. Dashboard latency spike (Jan 12, 09:30-09:45): 15 min
   - Severity: Sev3
   - Impact: Slow page loads
   - Root cause: Database lock contention

IMPROVEMENTS IMPLEMENTED:
- Circuit breaker on upstream service
- Increased memory limits
- Enhanced monitoring and alerting
- Predictive auto-scaling

LOOKING AHEAD:
We're committed to reaching 99.99% uptime in Q1 2026.
```

---

## Status Page Checklist

**Before Going Live**:
- [ ] All components listed (API, Dashboard, Pub/Sub, etc.)
- [ ] Update frequency set (every 5 min for Sev1)
- [ ] On-call trained on status page interface
- [ ] Templates pre-created and ready
- [ ] Backup person identified (if on-call unavailable)

**During Incident**:
- [ ] Status page updated within 2 minutes of detection
- [ ] Updates posted every 5 minutes (Sev1)
- [ ] No >10 minute gaps without communication
- [ ] Tone is professional and honest
- [ ] ETA provided (and updated as needed)

**After Incident**:
- [ ] Root cause included
- [ ] Impact metrics included
- [ ] Timeline included
- [ ] Resolution marked as complete
- [ ] Follow-up email sent within 24 hours
- [ ] Monthly uptime report sent

---

## Related Documentation

- [Warroom Coordination](./09-warroom-templates.md)
- [Escalation Procedures](./07-escalation-procedures.md)
- [PIR Template](./03-pir-template.md)

---

**Status**: READY FOR PRODUCTION
**Last Updated**: 2026-01-25
**Owner**: Agent 10 (Incident Playbooks & Operational Runbooks)
