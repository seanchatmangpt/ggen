<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [On-Call Schedule Management & Rotation](#on-call-schedule-management--rotation)
  - [On-Call Rotation](#on-call-rotation)
    - [Schedule Structure](#schedule-structure)
  - [Pre-On-Call Preparation](#pre-on-call-preparation)
    - [Before Your Week Starts](#before-your-week-starts)
    - [On-Call Preparation Checklist](#on-call-preparation-checklist)
  - [During On-Call Week](#during-on-call-week)
    - [Daily Routine](#daily-routine)
    - [Incident Response](#incident-response)
    - [Handoff to Secondary](#handoff-to-secondary)
  - [Post-On-Call Handoff](#post-on-call-handoff)
    - [Handoff Call (Mandatory)](#handoff-call-mandatory)
  - [On-Call SLA & Expectations](#on-call-sla--expectations)
    - [Response SLA](#response-sla)
    - [Work Restrictions During On-Call](#work-restrictions-during-on-call)
    - [Rest Days After On-Call](#rest-days-after-on-call)
  - [On-Call Burnout Prevention](#on-call-burnout-prevention)
    - [Fair Rotation](#fair-rotation)
    - [Incident Load Balancing](#incident-load-balancing)
    - [Compensation](#compensation)
  - [On-Call Training](#on-call-training)
    - [Mandatory Training (Before First On-Call)](#mandatory-training-before-first-on-call)
    - [Annual Refresher](#annual-refresher)
  - [On-Call Rotations & Fairness](#on-call-rotations--fairness)
    - [Rotation Schedule (Example)](#rotation-schedule-example)
    - [Holiday & Time-Off Handling](#holiday--time-off-handling)
  - [On-Call Performance Metrics](#on-call-performance-metrics)
  - [Related Documentation](#related-documentation)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# On-Call Schedule Management & Rotation

**Version**: 2.0
**Last Updated**: 2026-01-25
**Status**: Production Ready

> **Core Principle**: Clear on-call rotation ensures 24/7 coverage. Fair distribution reduces burnout. Continuous preparation maintains skills.

---

## On-Call Rotation

### Schedule Structure

**Coverage Model**: 24/7 with overlapping shifts

```
Time (UTC)      Primary         Secondary
─────────────────────────────────────────────
00:00 - 08:00   Alice          Bob
08:00 - 16:00   Charlie        Diana
16:00 - 00:00   Eve            Frank

Weekly rotation (every Monday):
  Week 1: Alice/Bob, Charlie/Diana, Eve/Frank
  Week 2: Charlie/Diana, Eve/Frank, Alice/Bob
  Week 3: Eve/Frank, Alice/Bob, Charlie/Diana
  ... repeat
```

**On-Call Zones**:
- **US/Americas**: 00:00-08:00 UTC (evening NA time)
- **EU/EMEA**: 08:00-16:00 UTC (day EU time)
- **APAC**: 16:00-00:00 UTC (evening APAC time)

**Duration**: 1 week per rotation cycle (7-day stint)

---

## Pre-On-Call Preparation

### Before Your Week Starts

**48 Hours Before** (Send reminder):
```bash
# Email on-call team
"Your on-call week starts Monday Jan 30 at 00:00 UTC.
Please verify:
- [ ] PagerDuty app installed on phone
- [ ] PagerDuty notifications enabled
- [ ] Phone number updated
- [ ] Can SSH to production
- [ ] VPN credentials working
- [ ] Slack notifications on
- [ ] You've read the runbooks (15 min)"
```

**24 Hours Before**:
```
- [ ] Check PagerDuty schedule (are you listed?)
- [ ] Test receiving alert (request test page)
- [ ] Verify contact information in PagerDuty
- [ ] Read this week's runbooks
- [ ] Know who your secondary is
```

**1 Hour Before**:
```
- [ ] Handoff call with previous on-call (15 min)
  - Any ongoing issues?
  - Any edge cases to watch?
  - Any preventive maintenance planned?
- [ ] Launch monitoring dashboard (bookmark it)
- [ ] Have laptop nearby (don't leave for > 30 min without hotspot)
```

### On-Call Preparation Checklist

**Skills & Knowledge**:
- [ ] Can navigate monitoring dashboard
- [ ] Know how to SSH to all prod servers
- [ ] Understand 5 major runbooks
- [ ] Know escalation chain
- [ ] Can execute basic recovery actions (restart, scale, rollback)

**Tools & Access**:
- [ ] PagerDuty account active + app on phone
- [ ] Slack notifications enabled
- [ ] SSH keys working
- [ ] VPN credentials current
- [ ] Database access verified
- [ ] Cloud CLI (gcloud) authenticated

**Contact Information**:
- [ ] Have Tech Lead's phone number
- [ ] Have VP's phone number
- [ ] Have backup on-call person's number
- [ ] Know how to reach each other if Slack/email down

---

## During On-Call Week

### Daily Routine

**Morning** (start of day in your timezone):
- [ ] Check PagerDuty for overnight incidents
- [ ] Review status page for any open incidents
- [ ] Check #incidents Slack channel for context
- [ ] Verify monitoring dashboard loads

**Evening** (before sleep):
- [ ] Keep phone nearby + volume up
- [ ] Ensure VPN connected or laptop accessible
- [ ] Let secondary know you're going to sleep (optional)
- [ ] Have water/coffee nearby (late-night incident often requires focus)

### Incident Response

**Upon Page**:
1. Acknowledge immediately (< 2 min)
2. Read alert details
3. Join warroom if exists, or create one
4. Follow relevant runbook
5. Post status update to #incidents every 5 min (Sev1)

**Escalation Threshold**:
- If stuck > 5 minutes, page Tech Lead
- If stuck > 15 minutes (Sev1), notify VP

### Handoff to Secondary

**If Going Offline** (urgent personal matter):
```
1. Immediately message secondary on Slack
2. Conference call (30 sec) to brief them
3. They acknowledge and take over
4. Keep phone on (in case they need context)
```

**If Secondary Needs to Take Over** (illness, emergency):
```
1. Page secondary immediately
2. If secondary doesn't respond (5 min), page Tech Lead
3. Tech Lead will cover or find backup
```

---

## Post-On-Call Handoff

### Handoff Call (Mandatory)

**Duration**: 15 minutes
**When**: Exactly at shift boundary (e.g., 16:00 UTC)
**Who**: Outgoing on-call + incoming on-call (Tech Lead optional)

**Agenda**:

```
1. Any Ongoing Incidents? (5 min)
   - Status of any active incidents
   - Runbooks currently in use
   - Next troubleshooting steps if unresolved

2. Recent Changes (3 min)
   - Deployments in last 24 hours
   - Configuration changes
   - Known issues or edge cases

3. Action Items (3 min)
   - Anything incoming should watch for
   - Monitoring thresholds (any recent adjustments?)
   - Maintenance windows scheduled

4. Questions? (4 min)
   - Anything incoming is unsure about
   - Permission to wake Tech Lead with non-urgent questions?
   - Emergency hotline number if needed
```

**Handoff Documentation**:
```
Outgoing: [Alice]
Incoming: [Charlie]
Shift: 2026-01-30 16:00-2026-01-31 00:00 UTC

ONGOING INCIDENTS:
- None (all clear)

RECENT CHANGES:
- Deployment: Governor v2.3.1 (Jan 29 14:00 UTC)
  Status: Stable, no incidents
- Config change: Increased queue threshold 500→700
  Reason: Reduce false positives, monitor for effectiveness

WATCH FOR:
- Higher queue depth (new threshold 700)
- Any errors from Governor post-deployment
- Database lock contention (observed yesterday, may recur)

QUESTIONS?
- Should I wake Tech Lead for non-critical questions? A: No, use Slack, page for emergencies only
```

---

## On-Call SLA & Expectations

### Response SLA

| Severity | Response Time | Action |
|----------|---------------|--------|
| **Sev1** | < 5 min | ACK and join warroom |
| **Sev2** | < 15 min | ACK and investigate |
| **Sev3** | < 30 min | ACK and review ticket |
| **Sev4** | < 24 hours | No hard SLA |

**If you can't respond**: Contact secondary on-call immediately (by phone, not Slack)

### Work Restrictions During On-Call

**During on-call, you**:
- Should not be driving/unavailable for >1 hour
- Should respond to alerts within 5 minutes
- Should keep laptop + phone powered and nearby

**You are allowed to**:
- Take breaks (notify secondary if >30 min away)
- Sleep (keep phone on, notify if unavailable)
- Work on non-blocking tasks (reviews, documentation)
- Ask secondary to cover if you have appointment

### Rest Days After On-Call

**After 7-day on-call stint**:
- You get **2 consecutive days off** (no on-call duties)
- During those days, don't check Slack #incidents
- Tech Lead covers any emergencies

**Rationale**: Prevent burnout, allow recovery

---

## On-Call Burnout Prevention

### Fair Rotation

**Frequency**: Each engineer on-call 1 week every 3 weeks (1/3 of time)

**Calculation**:
- If 6 engineers: Each rotates 1-2 weeks per cycle (every 6 weeks)
- If 3 engineers: Each rotates 1 week (every 3 weeks)
- If 9 engineers: Each rotates 1 week (every 9 weeks) - ideal

**Monitor**: Track on-call frequency by person (ensure fairness)

### Incident Load Balancing

**If incidents clustered** (same person gets 3+ Sev1 in one week):
- Rotate secondary with primary for next week
- Previous secondary becomes primary
- Gives breather to heavily loaded on-call

**If severe incident occurs** (Sev1 lasted > 4 hours):
- Offer on-call person to take next day off
- Bring in senior engineer as backup

### Compensation

**Considerations**:
- Sev1 incident during sleep: Additional pay or comp time
- On-call week with 5+ pages: Consider extra comp
- Deployment during on-call causing incident: Review process

**Current Policy**: 1.5x pay for night shift on-call (optional / company policy dependent)

---

## On-Call Training

### Mandatory Training (Before First On-Call)

**Duration**: 4 hours (spread over 2 days)

**Topics**:
1. **PagerDuty & Alert System** (30 min)
   - How to receive alerts
   - How to acknowledge
   - Escalation policies

2. **Incident Response** (60 min)
   - Read 5 major runbooks
   - Understand escalation chain
   - Practice mock incident

3. **Tools & Access** (30 min)
   - SSH to production
   - Navigate monitoring dashboard
   - Execute recovery actions

4. **Communication** (30 min)
   - Status page updates
   - Warroom protocol
   - Customer communication

5. **Common Mistakes & Recovery** (30 min)
   - What not to do
   - How to recover from mistakes
   - When to escalate

### Annual Refresher

**Mandatory Yearly**: Review all runbooks + practice mock incident

---

## On-Call Rotations & Fairness

### Rotation Schedule (Example)

```
ROTATION CYCLE: 3 weeks (9 on-call weeks per 9-engineer team)

Week Starting  Zone 1     Zone 2     Zone 3
─────────────────────────────────────────────────
Jan 1          Alice→Bob  Charlie→D  Eve→Frank
Jan 8          Charlie→D  Eve→Frank  Alice→Bob
Jan 15         Eve→Frank  Alice→Bob  Charlie→D
Jan 22         Alice→Bob  Charlie→D  Eve→Frank
... repeat
```

### Holiday & Time-Off Handling

**1 Month Before Time-Off**:
- Notify on-call coordinator
- List dates when unavailable
- Coordinator swaps with available team member
- Swaps logged in schedule

**Emergency Time-Off**:
- Notify coordinator ASAP
- Secondary takes over
- Coordinator finds replacement for future dates

**No Penalties**: Taking time off is okay, just notify in advance

---

## On-Call Performance Metrics

**Track per on-call person**:

| Metric | Target | How to Improve |
|--------|--------|-----------------|
| Response time | < 5 min (Sev1) | Keep phone closer, set louder alert |
| Escalation rate | < 20% incidents | Study runbooks more, practice |
| MTTR | Meets target | Document lessons learned |
| Page frequency | Fair (1/3 of incidents) | Report if unfair |
| Job satisfaction | > 7/10 (survey) | Reduce frequency, improve tooling |

**Monthly Review**:
- How many incidents did you handle?
- Any patterns? (e.g., always late at night?)
- Anything we can improve?

---

## Related Documentation

- [Escalation Procedures](./07-escalation-procedures.md)
- [Warroom Coordination](./09-warroom-templates.md)
- [Incident Runbooks](./02-incident-runbooks/)

---

**Status**: READY FOR PRODUCTION
**Last Updated**: 2026-01-25
**Owner**: Agent 10 (Incident Playbooks & Operational Runbooks)

**PagerDuty Integration**: Sync schedule weekly
**Calendar Integration**: Google Calendar / Outlook calendar link: [Link to shared calendar]
