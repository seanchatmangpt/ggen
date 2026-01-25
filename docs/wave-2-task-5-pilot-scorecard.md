<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Wave 2, Task 5: Pilot Scorecard (Week 11)](#wave-2-task-5-pilot-scorecard-week-11)
  - [Quick Reference: Pass/Fail Criteria](#quick-reference-passfail-criteria)
  - [Metric 1: Exception Handling Rate](#metric-1-exception-handling-rate)
  - [Metric 2: Pilot Satisfaction](#metric-2-pilot-satisfaction)
  - [Metric 3: Escalation Quality (Zero Rework)](#metric-3-escalation-quality-zero-rework)
  - [Metric 4: Incident Response SLO (Maintained)](#metric-4-incident-response-slo-maintained)
  - [Metric 5: Retention (Zero Attrition)](#metric-5-retention-zero-attrition)
  - [Decision Matrix: Go/No-Go for Week 12 Switchover](#decision-matrix-gono-go-for-week-12-switchover)
  - [Scorecard Summary Template (Friday Week 11)](#scorecard-summary-template-friday-week-11)
  - [Pilot Scorecard Dashboard (Real-Time Week 11)](#pilot-scorecard-dashboard-real-time-week-11)
  - [Contingency: If Pilot Fails](#contingency-if-pilot-fails)
  - [Success Looks Like (Friday End of Week 11)](#success-looks-like-friday-end-of-week-11)
  - [Escalation Contacts](#escalation-contacts)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Wave 2, Task 5: Pilot Scorecard (Week 11)

**Pilot Program**: 3 volunteer ops engineers, 1-week parallel run (Week 11)
**Evaluation Date**: Friday Week 11
**Go/No-Go Decision**: All metrics pass = scale to 20. Metrics fail = investigate before scaling.

---

## Quick Reference: Pass/Fail Criteria

| Metric | Target | Status | Pass/Fail |
|--------|--------|--------|-----------|
| **Exception Handling Rate** | >70% handled solo | [Dashboard] | ☐ PASS / ☐ FAIL |
| **Pilot Satisfaction** | >70% (avg >3.5/5) | [Survey] | ☐ PASS / ☐ FAIL |
| **Escalation Quality** | 0 reworks | [Audit] | ☐ PASS / ☐ FAIL |
| **Incident Response SLO** | ≤5% slower | [Metrics] | ☐ PASS / ☐ FAIL |
| **Retention** | 0 attrition | [HR Check] | ☐ PASS / ☐ FAIL |

**Overall Status**: ☐ GO (all pass) / ☐ NO-GO (any fail) / ☐ CONDITIONAL (3/5 pass, 2/5 close)

---

## Metric 1: Exception Handling Rate

**What It Measures**: Self-sufficiency. Can pilots handle exceptions without escalation?

**Target**: >70%

**Formula**:
```
Exception Handling Rate = (Exceptions handled solo) / (Total exceptions assigned) × 100
```

**How It Works**:
- Pilot gets assigned 100 exceptions during week 11
- Pilot reviews each exception in Slack workflow
- Pilot decides: approve, deny, or escalate
- Solo = approve/deny. Escalation = flagged to mentor/lead
- Rate = % of exceptions handled without escalation

**Example**:
```
Pilot A: 100 exceptions assigned
         75 handled solo (approve/deny)
         25 escalated
Rate = 75/100 = 75% ✓ PASS (>70%)

Pilot B: 100 exceptions assigned
         60 handled solo
         40 escalated
Rate = 60/100 = 60% ✗ FAIL (≤70%)
```

**Tracking**: Dashboard (daily update)
- Filter by pilot name
- Show: solo decisions, escalations, rate trend

**If FAIL**:
- Reason: exception complexity, training gap, or missing domain knowledge?
- Action: Mentor deep-dive on that category + extra practice
- Timeline: Extra training before full rollout (week 12)

**If CONDITIONAL (60-70%)**:
- Decision: Acceptable if trajectory is improving (day 1 = 50%, day 5 = 70%)
- Action: Short mentorship sprint week 12, gradual task escalation

---

## Metric 2: Pilot Satisfaction

**What It Measures**: Do pilots feel the new role is achievable? Do they believe in the model?

**Target**: >70% (survey average >3.5/5)

**Survey Questions** (Likert 1-5 scale, Friday end-of-day):

1. **"This role feels achievable for me (not overwhelming)"**
   - 1 = Completely overwhelmed, should not scale
   - 5 = Very confident, ready to teach others
   - Target: ≥3.5 avg

2. **"I'm confident in my exception decisions"**
   - 1 = Guessing, not confident
   - 5 = Very confident, would defend my decisions
   - Target: ≥3.5 avg

3. **"I see myself doing this role long-term"**
   - 1 = No, want to go back to old role
   - 5 = Yes, excited about new direction
   - Target: ≥3.5 avg

4. **"My job is not being replaced (I'm valued)"**
   - 1 = Automation replacing me, job insecure
   - 5 = Clear expansion role, excited
   - Target: ≥3.5 avg

5. **"Training prepared me well"**
   - 1 = Not prepared at all
   - 5 = Excellent preparation
   - Target: ≥3.5 avg

**Calculation**:
```
Satisfaction = (Q1 + Q2 + Q3 + Q4 + Q5) / 5

Pilot A: (4 + 4 + 5 + 4 + 4) / 5 = 4.2 ✓ PASS
Pilot B: (3 + 3 + 2 + 2 + 3) / 5 = 2.6 ✗ FAIL
Pilot C: (4 + 4 + 3 + 4 + 4) / 5 = 3.8 ✓ PASS

Program Average = (4.2 + 2.6 + 3.8) / 3 = 3.5 PASS (borderline)
```

**Open-Ended Questions** (qualitative):

6. **"What's one thing that exceeded your expectations?"**
   - Looking for: positive surprise (e.g., "exception handling was clearer than I thought")

7. **"What's one thing that worried you?"**
   - Looking for: real concerns (e.g., "I'm still not sure about policy edge cases")

8. **"What would help you feel more confident?"**
   - Looking for: actionable feedback (e.g., "more examples of similar exceptions")

**Tracking**:
- Survey link sent Friday 3pm
- Responses due Friday 5pm (before weekly wrap-up)
- Results reviewed with program PM + pilot leads

**If FAIL** (<3.5 avg):
- Which questions failed? (Q1=overwhelming, Q4=job insecurity, Q2=confidence?)
- Open-ended responses: What's the actual concern?
- Action: Emergency sessions with failing pilots
  - If Q4 fails: career conversation + assurance messaging
  - If Q1 fails: re-training on specific category + simpler tasks week 12
  - If Q2 fails: mentorship on decision framework + practice

**If CONDITIONAL** (3.2-3.5 avg):
- Trajectory: Are pilots trending up or down?
- Action: Extra mentorship week 12 + monitor closely

---

## Metric 3: Escalation Quality (Zero Rework)

**What It Measures**: Sound judgment. When pilots escalate, do managers agree with escalation?

**Target**: 0 reworks (no pilot decisions overturned)

**What "Rework" Means**:
- Pilot escalates exception to mentor/lead
- Mentor/lead reviews escalation
- Manager says: "Actually, you should have handled that solo" or "You made wrong call"
- That's a rework = pilot's judgment wasn't sound

**Example**:
```
Week 11: Pilot handles 100 exceptions

Pilot solo decisions (75):
- All approved by post-hoc audit ✓

Pilot escalations (25):
- 24 escalations reviewed, manager agrees: "Right call" ✓
- 1 escalation reviewed, manager says: "You should have approved this, not escalated"
  ✗ REWORK

Rework Rate = 1/100 = 1%
FAIL (target = 0%)
```

**Tracking**:
- Audit trail system captures pilot decisions
- Post-week-11, mentors review 100% of escalations
- Flag any "actually, you should have..." comments

**If REWORK Found**:
- Which exception category? (policy violations, customer exemptions, etc.)
- What was the decision rule the pilot missed?
- Action: Mentor review of that category + re-training before full rollout

**If Zero Rework**:
- Confirms: pilots understood training + are making sound decisions ✓

---

## Metric 4: Incident Response SLO (Maintained)

**What It Measures**: Does new role slow down incident response?

**Target**: ≤5% slower than historical baseline (new role doesn't break ops)

**SLO Definition**:
- P1 incident (all systems down): Detection → Resolution ≤30 minutes
- P2 incident (partial outage): Detection → Resolution ≤4 hours

**Tracking**:
- Baseline: Historical average for P1 and P2 response times (last 4 weeks before week 11)
- Week 11: Measure actual response times with pilots in new role (50% exceptions, 50% incident handling)

**Example**:
```
Baseline (historical):
- P1 avg: 15 minutes
- P2 avg: 90 minutes

Week 11 with pilots (50% exceptions, 50% incidents):
- P1 avg: 16 minutes (6.7% slower) ✗ FAIL
- P2 avg: 85 minutes (5.6% faster) ✓

Overall: One metric failed. Flag as concern.
```

**Calculation**:
```
Variance = ((Week 11 Time - Baseline Time) / Baseline Time) × 100

If variance ≤5% (slower OR faster) = PASS
If variance >5% slower = FAIL
```

**Context**:
- Week 11 is a learning week (pilots slower at exception handling)
- Expected: some slowdown OK
- Target: slowdown <5% (pilots handling exceptions doesn't break incident response)

**If FAIL** (>5% slower):
- Root cause: Are pilots taking too long on exceptions? Too many escalations during incidents?
- Action: Reduce exception volume for pilots during incident times
- Or: More mentorship on quick exception decisions

**If PASS**:
- Confirms: new role doesn't compromise incident response ✓

---

## Metric 5: Retention (Zero Attrition)

**What It Measures**: No one quits or transfers due to role change.

**Target**: 0 attrition

**Tracking**:
- Check with HR end of week 11
- Have any pilots resigned, requested transfer, or indicated intent to leave?

**Example**:
```
Pilot A: Still employed, no indication of leaving ✓
Pilot B: Submitted resignation Friday (reason: "Role too complex, prefer old model") ✗
Pilot C: Still employed, no indication of leaving ✓

Attrition = 1/3 = 33% ✗ FAIL (>0%)
```

**If Attrition Occurs**:
- Get resignation reason (is it training, job security, or external offer?)
- Learning: What would have prevented this?
- Action: Before scaling to 20, address that gap
- Communication: Exit interview feedback → improve program for others

**If Zero Attrition**:
- Confirms: pilots feel role is sustainable ✓

---

## Decision Matrix: Go/No-Go for Week 12 Switchover

**Friday Week 11, 5pm Decision Call**

| Scenario | Metrics Status | Decision | Next Steps |
|----------|---|----------|-----------|
| **Scenario A: ALL PASS** | All 5 metrics ≥target | **GO** (scale to 20) | Proceed to week 12 full switchover as planned |
| **Scenario B: 4/5 PASS** | 1 metric slightly miss (3-5% below target) | **GO with conditions** | Scale to 20, but add extra support for that domain (e.g., policy training if policy violation rate high) |
| **Scenario C: 3/5 PASS** | 2 metrics miss, but trends improving | **CONDITIONAL** | Do NOT scale yet. Run 2-3 day intensive retraining (Mon-Wed week 12), then retry metrics. Go/no-go decision Wednesday. |
| **Scenario D: <3/5 PASS** | 3+ metrics significantly miss | **NO-GO** | PAUSE. Investigate root cause (training gap? Process unclear? Role design wrong?). Redesign + retry week 12. Full team waits. |

---

## Scorecard Summary Template (Friday Week 11)

```
PILOT SCORECARD - WEEK 11 RESULTS
==================================
Date: Friday Week 11
Evaluation: [Program PM Name]

1. EXCEPTION HANDLING RATE
   Pilot A: 75/100 = 75%  ✓ PASS
   Pilot B: 60/100 = 60%  ✗ FAIL
   Pilot C: 78/100 = 78%  ✓ PASS
   Status: 2/3 PASS

2. PILOT SATISFACTION
   Pilot A: 4.2/5  ✓ PASS
   Pilot B: 2.6/5  ✗ FAIL
   Pilot C: 3.8/5  ✓ PASS
   Status: 2/3 PASS

3. ESCALATION QUALITY
   Pilot A: 0 reworks  ✓ PASS
   Pilot B: 2 reworks  ✗ FAIL
   Pilot C: 0 reworks  ✓ PASS
   Status: 2/3 PASS

4. INCIDENT RESPONSE SLO
   P1 variance: +2.1%  ✓ PASS
   P2 variance: -1.5%  ✓ PASS
   Status: 1/1 PASS

5. RETENTION
   Pilots employed: 3/3  ✓ PASS
   Status: 1/1 PASS

OVERALL: 3/5 PASS, 2/3 pilots struggling
DECISION: CONDITIONAL - Pilot B needs intervention

NEXT STEPS:
- Monday week 12: Emergency training for Pilot B (exception categories + decision framework)
- Wednesday week 12: Re-assess Pilot B metrics
- If improved: Scale to 20 (week 12 onwards)
- If still failing: Redesign program, investigate training gaps

LESSONS LEARNED:
- Pilot A & C ready immediately
- Pilot B (Concern 1): struggled with policy violation category (training gap)
- Pilot B (Concern 2): worried about job security (needs career conversation)
- Action: Improve Module 4 (policy interpretation) before scaling to 20
```

---

## Pilot Scorecard Dashboard (Real-Time Week 11)

**Suggested dashboard for daily tracking**:

```
PILOT PROGRAM WEEK 11 - REAL-TIME DASHBOARD
=============================================

Exception Handling Rate (Daily Trending):
Pilot A: Day 1=45% → Day 2=55% → Day 3=70% → Day 4=72% → Day 5=75%
Pilot B: Day 1=40% → Day 2=50% → Day 3=58% → Day 4=60% → Day 5=60% [FLAT]
Pilot C: Day 1=60% → Day 2=65% → Day 3=72% → Day 4=76% → Day 5=78%

Escalations (By Category):
Policy Violations: 40 (Pilot B struggling here - 15 of 40)
Config Conflicts: 35
Customer Exemptions: 15
Rollback Scenarios: 8
Other: 2

Satisfaction Pulse (Anonymous daily check-in, 1-5):
Pilot A: Day 1=3 → Day 2=3 → Day 3=4 → Day 4=4 → Day 5=4
Pilot B: Day 1=3 → Day 2=3 → Day 3=2 → Day 4=2 → Day 5=3 [RED]
Pilot C: Day 1=4 → Day 2=4 → Day 3=4 → Day 4=4 → Day 5=4

SLO Incidents:
P1 incidents: 2 (both handled <30min) ✓
P2 incidents: 4 (avg 87min, vs baseline 90min) ✓

Attendance:
Pilot A: 5/5 days ✓
Pilot B: 4/5 days (absent Wednesday) ✗
Pilot C: 5/5 days ✓
```

---

## Contingency: If Pilot Fails

**If Pilot B significantly underperforms**:

1. **Immediate (Friday afternoon week 11)**:
   - 1-on-1 with Pilot B + mentor
   - Diagnosis: What's the blocker? (complexity? confidence? job security fear?)
   - Decision: Can it be fixed? (Training gap) Or structural? (Role design issue)

2. **Weekend (if GO decision needed Monday)**:
   - Create targeted re-training plan (if training gap)
   - Create career assurance messaging (if job security fear)

3. **Monday week 12**:
   - Execute re-training or career conversation
   - Clear metric: "If you hit X, we proceed. If not, we pause and investigate."

4. **Wednesday week 12**:
   - Re-assess Pilot B
   - If improved: Scale to 20 (week 12 onwards)
   - If still failing: PAUSE full rollout (investigate why program design isn't working)

---

## Success Looks Like (Friday End of Week 11)

**Green Light (All Metrics Pass)**:
- "All 3 pilots completed 1-week parallel run successfully"
- "Pilots handled 70%+ exceptions independently"
- "Pilot satisfaction 3.5+/5 (role feels achievable)"
- "Zero rework on escalations (sound judgment)"
- "SLO maintained (incidents handled normally)"
- "0 attrition (pilots staying)"
- **Decision**: FULL ROLLOUT TO 20 WEEK 12 ✓

**Yellow Light (Conditional Pass)**:
- "2/3 pilots doing well, 1 pilot struggling"
- "Specific training gaps identified (policy category)"
- "Fix-able with 2-3 day intensive training week 12"
- **Decision**: Delay switchover by 3 days + emergency training, then GO

**Red Light (Metrics Fail)**:
- "Pilots too overwhelmed or uncertain"
- "Training insufficient for role complexity"
- "Role design needs rethinking"
- **Decision**: PAUSE. Investigate + redesign before scaling to 20

---

## Escalation Contacts

**If metrics fail**: Contact Program PM immediately
**If pilot resignation**: Contact ops director + HR
**If SLO breach during week 11**: Contact incident commander (keep new role functional)

---

**Scorecard Prepared By**: [Program PM]
**Date**: Week 8 (pre-launch)
**Status**: Ready for week 11 evaluation

**Scoring Begins**: Monday Week 11, 9am
**Final Results**: Friday Week 11, 5pm
**Decision Call**: Friday Week 11, 5:30pm
