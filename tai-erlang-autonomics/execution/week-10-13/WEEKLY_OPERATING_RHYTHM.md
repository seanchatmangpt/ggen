# Weekly Operating Rhythm: Monday Planning & Friday Reviews

**Status:** Ready for Implementation
**Purpose:** Establish repeatable weekly cadence for scaling team
**Timeline:** Starting Week 10, maintain through Week 13 and beyond
**Success Metric:** 100% team attendance, decisions documented, follow-up actions tracked

---

## Overview: The Weekly Rhythm

**Two anchor meetings per week:**

| Meeting | Day/Time | Duration | Attendees | Purpose |
|---------|----------|----------|-----------|---------|
| **Monday Planning** | Mon 9:00am | 90 min | CEO, CTO, VP Sales, CSM | Set weekly priorities, align on goals |
| **Daily Standup** | Daily 3:00pm | 10 min | CEO, CTO, VP Sales, CSM | Quick status, escalate blockers |
| **Friday Review** | Fri 4:00pm | 30 min | CEO, CTO, VP Sales, CSM | Review metrics, celebrate wins, plan next week |

---

## Part 1: Monday Planning Meeting (90 minutes)

**Purpose:** Align team on weekly priorities, identify risks, assign owners
**Owner:** CEO (runs meeting)
**Attendees:** CEO, CTO, VP Sales, CSM
**Format:** Agenda-driven, decisions documented

### Pre-Meeting Preparation (30 min, Sunday evening)

**CEO prepares:**
- [ ] Agenda (5 items max)
- [ ] Last week's status updates (what we said we'd do)
- [ ] Customer health summary (any red flags?)
- [ ] Sales pipeline summary (new leads, win rate)
- [ ] Upcoming deadlines (contract signatures, implementations)
- [ ] Blockers from last week (unresolved issues)

**CTO prepares:**
- [ ] Infrastructure status (uptime, incidents, deployment schedule)
- [ ] Engineering progress (on track? any delays?)
- [ ] Technical blockers (need resources? decisions?)

**VP Sales prepares:**
- [ ] Pipeline summary (total $, stage breakdown, conversion rate)
- [ ] Closed deals (customer #2 or #3 signature target this week?)
- [ ] Upcoming customer calls (scheduled deals)
- [ ] Sales blockers (proposal delay? legal? pricing?)

**CSM prepares:**
- [ ] Customer implementations (% complete, on track?)
- [ ] Customer health (NPS, any red flags?)
- [ ] Upcoming milestones (pilots, go-lives)
- [ ] Support blockers (technical issues? training needed?)

### Monday Meeting Agenda (90 minutes)

**1. Opening & Context (5 min)**
```
CEO: "Here's the week we're planning for. Our north star: Customer #2
contract by Friday + Customer #3 onboarding started."

Key themes for this week:
  • Scaling: 3 customers = more coordination
  • Quality: Zero incidents while implementing
  • Communication: Daily syncs are critical
```

**2. Last Week Review (10 min)**
```
Quick scorecard on what we committed to:
  ✓ Customer #1 confidence high (NPS 9)
  ✓ Customer #2 onboarding started (Day 1 kickoff)
  ✗ Customer #3 deal delayed (still in pricing)
  ✓ Infrastructure scaling 50% done (Cloud Run updated)

What went well?
  • Customer #2 onboarding smooth
  • Zero incidents all week
  • Team morale high

What could be better?
  • Customer #3 pricing negotiation slow
  • Sales coordinator still not hired
  • Need more marketing assets (case study)
```

**3. Customer Health & Success (15 min)**

CSM leads:
```
Customer #1:
  • Status: 95% accuracy (on track for 97% by Week 3)
  • NPS: 9 (very satisfied)
  • Next: Week 1 of 30-60-90 complete, Week 2 starts Monday
  • Risk: None identified
  • Action: Schedule quarterly business review for Week 4

Customer #2:
  • Status: Day 1 kickoff completed
  • Next: Week 1 - admin training and pilot environment setup
  • Risk: Customer admin on vacation Wed-Fri (need to adjust training)
  • Action: Reschedule training to Tuesday morning

Customer #3:
  • Status: Contract not yet signed (still in negotiations)
  • Next: Close contract by Friday, kickoff Week 11 Monday
  • Risk: Pricing negotiation stalled
  • Action: CEO + VP Sales have call with customer Wednesday (price finalization)
```

**Decision:** CSM owns customer timeline, CEO/VP Sales handle blockers

**4. Sales Pipeline & Revenue (15 min)**

VP Sales leads:
```
Pipeline Summary:
  Total: $625K (target was $600K) ✓

Stage breakdown:
  Discovery: $250K (5 customers)
  Proposal: $200K (3 customers - one closes this week)
  Closing: $175K (Customer #3 - targeting signature Friday)

New pipeline added this week: $75K (3 new leads)
Conversion rate: 30% (3 closed of 10 in pipeline)

This week's focus:
  • Close Customer #3: $35K target (CEO + VP Sales lead)
  • Send 3 proposals: $150K total value
  • 6 discovery calls: Generate $100K+ in new pipeline

Risk: Sales coordinator still not filled (admin load on VP Sales)
Action: Accelerate hiring (post role today, hire by end of Week 11)
```

**Decision:** VP Sales owns revenue, hiring critical by Week 11

**5. Engineering & Infrastructure (15 min)**

CTO leads:
```
Current state:
  • Cloud Run scaling: Updated to 50 max instances ✓
  • Firestore indexes: 20 new indexes created ✓
  • Performance: P95 latency 120ms (target: <500ms) ✓
  • Backups: Automated daily, tested ✓

This week's work:
  • Customer #2 integration: Architecture design complete, coding starts Tue
  • Firestore optimization: Slow query analysis
  • Monitoring: Add 2 new dashboards (customer health, financial)

Blockers:
  • Backend contractors not yet hired (needed for Customer #3)
  • DevOps documentation incomplete (need runbooks)

Risks:
  • Integration complexity for Customer #2 higher than estimated (2x scope)
  • May need to extend timeline or add resources

Actions:
  • CTO to call Customer #2 Tuesday (scope confirmation)
  • Post contractor role Wednesday (target hire end of week)
  • Document integration runbook by Friday
```

**Decision:** CTO owns engineering timeline, contractors critical for week 11-12

**6. Weekly Priorities (20 min)**

CEO synthesizes:
```
Top 3 priorities this week (in order):

1. CRITICAL: Close Customer #3 contract (due Friday)
   Owner: CEO + VP Sales
   Success: Signed contract + $10K deposit
   Impact: Unlocks Week 11 kickoff, achieves 3-customer goal

2. CRITICAL: Customer #2 implementation on track (50% by Friday)
   Owner: CTO + CSM
   Success: Integration architecture done, admin training complete
   Impact: Validates scaling playbook for Customer #3

3. MEDIUM: Hire sales coordinator + post contractors (by Friday)
   Owner: CEO + CTO
   Success: Job posted, 10+ candidates in pipeline
   Impact: Reduces admin burden, unblocks engineering scaling

Secondary priorities:
  • Document case study (Customer #1) for marketing
  • Prepare Series A narrative draft
  • Team celebration (3 wins this week!)

Stretch goal (if time):
  • Redis caching implementation (performance optimization)
  • SOC 2 compliance checklist (future customers)
```

**7. Blockers & Escalations (10 min)**

CEO asks each person:
```
"What's blocking you? What do you need from me this week?"

CTO: "Need decision on contractor vs FTE for engineering. I recommend
contractors for Weeks 10-13 (fast ramp), then convert to FTE in Month 5."
→ CEO decision: Approved, post contractors today

VP Sales: "Need CEO support for Customer #3 pricing call Wednesday.
They're pushing for $30K (below our $35K target). Need executive buy-in."
→ CEO decision: Will join call, have pricing authority ($28K-38K band approved)

CSM: "Need CTO to meet with Customer #2 Wednesday to review integration scope.
Customer asking for 3 features we didn't quote. Need to clarify scope."
→ CEO decision: CTO+CSM call with Customer #2 Wed 2pm, documented scope req'd
```

**8. Wrap-Up & Commitments (5 min)**

CEO summarizes:
```
This week's commitments:

✓ CLOSE CUSTOMER #3 DEAL (CEO + VP Sales)
✓ CUSTOMER #2 IMPLEMENTATION 50% (CTO + CSM)
✓ POST HIRING (CEO sends job descriptions)
✓ SCOPE CONFIRMATION WITH CUSTOMER #2 (CTO + CSM)

Team, we're crushing it. Three customers by Week 13 is real now. Let's
celebrate Friday if we hit our targets.

Daily standup at 3pm - please be on time. Any urgent blockers, Slack me.

Any questions on priorities?
```

---

## Part 2: Daily Standup (10 minutes)

**Purpose:** Quick status sync, identify blockers, maintain momentum
**Owner:** Anyone can run (usually CEO)
**Format:** Tight, focused, 2 min per person

### Standup Template (10 min)

**Time:** 3:00pm sharp (not 3:01, discipline matters)
**Location:** Slack channel #standup or Zoom (if remote)
**Format:** Each person goes 2 minutes, 1 minute questions

**Agenda:**
```
1. What I SHIPPED yesterday (shipped = done, deployed, or measurable progress)
   Example: "Shipped Customer #2 integration design doc, got CTO approval"

2. What I'm WORKING ON today (current task, ETA)
   Example: "Starting Customer #2 integration coding, target: auth module by EOD"

3. What's BLOCKING me (if anything)
   Example: "Waiting on Customer #2 IT to approve firewall access, should be today"

4. SHOUTOUT (optional - recognize someone's contribution)
   Example: "Shout out to CSM for handling Customer #2 scope negotiation"
```

### Standup Examples (Week 10)

**Monday, 3pm Standup**
```
CEO: "Shipped: Customer #2 onboarding kickoff, team alignment meeting.
Today: Final pricing call with Customer #3 (3pm). Blocker: Need VP Sales
on pricing call. Shout out to CSM for excellent kickoff execution."

VP Sales: "Shipped: 3 discovery calls (new leads $100K pipeline). Today:
Customer #3 pricing negotiation (3pm with CEO). Blocker: None. Shout out
to CEO for executive presence with Customer #2."

CTO: "Shipped: Cloud Run scaling config (50 max instances), Firestore
indexes (20 new). Today: Start Customer #2 integration coding. Blocker:
Need Customer #2 IT specs (firewall, API access). Expect by EOD."

CSM: "Shipped: Customer #2 Day 1 kickoff, admin training scheduled.
Today: Pilot environment setup, sample data load. Blocker: Customer admin
on vacation Wed-Fri (rescheduled training). Shout out to team for smooth kickoff."

[60 seconds of questions/quick clarifications]

CEO: "Great day everyone. Let's bring the energy to these customer calls
this afternoon. We close #3 deal by Friday. Dismissed!"
```

**Friday, 3pm Standup**
```
CEO: "Shipped: Customer #3 contract signed! 🎉 Sent to legal for countersign.
Also: Hired sales coordinator (offer accepted, starts Week 11). Today:
Customer celebration call, series A investor intro. Blocker: None."

VP Sales: "Shipped: Customer #3 contract closed, $40K ACV! 3 new proposals
sent ($150K). 4 discovery calls (new $100K in leads). Today: Follow-up on
proposals. Blocker: Sales coordinator starts Monday."

CTO: "Shipped: Customer #2 integration 50% done (auth module working).
16 new Firestore indexes. Today: Integration testing. Blocker: None.
On track for Customer #2 go-live Week 12."

CSM: "Shipped: Customer #2 pilot setup complete, training done. NPS from
Customer #1: 9/10! 😊 Today: Prep Customer #3 kickoff plan. Blocker: None."

CEO: "Let's celebrate this weekend. Customer #3 closed, infrastructure
ready, team strong. Friday review at 4pm - bring your energy!"
```

### Standup Discipline

**What makes standups work:**
- [ ] **Start on time** (3:00pm sharp, not 3:02)
- [ ] **Stick to 2 min per person** (enforce with timer if needed)
- [ ] **Only 1 minute for questions** (save deeper convos for after)
- [ ] **Focus on shipped/blocker** (not task details)
- [ ] **Celebrate wins** (shoutouts matter for morale)
- [ ] **Flag blockers early** (don't hide problems)
- [ ] **No status reports** (we're moving fast, not reporting to stakeholders)

---

## Part 3: Friday Review Meeting (30 minutes)

**Purpose:** Celebrate wins, review metrics, plan next week
**Owner:** CEO (runs meeting)
**Attendees:** CEO, CTO, VP Sales, CSM
**Format:** Structured review, decisions documented

### Pre-Meeting Preparation (30 min, Thursday evening)

**CEO compiles:**
- [ ] Weekly KPI dashboard (MRR, pipeline, implementation %, team count)
- [ ] Wins list (what we accomplished)
- [ ] Challenges list (what didn't go as planned)
- [ ] Blockers (anything that needs resolution?)
- [ ] Next week priorities (3 items)

### Friday Review Agenda (30 min)

**1. Wins Celebration (5 min)**

```
What did we ship this week? (celebrate wins, big and small)

Week 10 example:
  ✓ Customer #2 onboarding started (30-60-90 plan engaged)
  ✓ Cloud Run infrastructure scaled (50 instances ready)
  ✓ Sales coordinator hired (starts Monday, admin load off VP Sales)
  ✓ 5 new leads added to pipeline ($250K+)
  ✓ Zero incidents (infrastructure stable)

Let's acknowledge: CTO's scaling work was flawless. VP Sales closed
Customer #3 at target price. CSM executed perfect kickoff. Team nailing it.
```

**2. Metrics Review (10 min)**

CEO shows dashboard:

```
REVENUE METRICS:
  Week 10: MRR $10,000 (Customer #2 just started, prorated)
  Cumulative MRR: $10,000
  Year-to-date pipeline: $625K (+$75K this week)
  Conversion rate: 30% (3 closed / 10 discussed)
  Target: $10,417/month MRR by Week 13 ✓ on track

CUSTOMER SUCCESS:
  Customer #1: 95% accuracy, NPS 9 ✓ Healthy
  Customer #2: 30% implementation, on track ✓ On track
  Customer #3: Contract signed, kickoff next week ✓ Signed
  Expansion identified: 2 upsell opportunities ✓ Identified

ENGINEERING:
  Uptime: 99.7% ✓ Excellent
  API latency p95: 120ms ✓ Excellent
  Error rate: 0.1% ✓ Excellent
  Incidents: 0 ✓ Perfect

TEAM:
  FTE: 5.5 → 6 (sales coordinator hired)
  Contractors: 0 → 2 (backend engineers to be hired)
  Hiring pipeline: CTO role has 6 finalists
  Morale: 9/10 (team executing well)
```

**3. Blockers & Challenges (5 min)**

```
What could be better? (be honest, focus on solutions)

Week 10 example:
  • Customer #3 pricing negotiation took longer than expected
    → Action: Pre-approve pricing bands earlier (CEO decision)

  • Backend contractor hiring is slow (need by Week 12)
    → Action: Extend offer range, reach out to references (CTO decision)

  • Sales coordinator hiring: 4 weeks to hire is too slow
    → Action: Use referral bonus, accelerate offer (CEO decision)

None of these are failures - they're learning opportunities. How do we
solve for next hire?
```

**4. Next Week Priorities (8 min)**

```
Monday, we'll plan in detail. For now, our 3 big rocks:

1. CUSTOMER #2: Implementation 50% → 75% (CTO + CSM own)
   Success: Integration code 75% done, pilot warehouse data loaded
   Blocker: If architecture scope creep → escalate to CEO

2. CUSTOMER #3: Kickoff week (CSM owns)
   Success: Kickoff meeting, technical spec agreed, training scheduled
   Blocker: If IT delays firewall access → CEO to escalate

3. HIRING: Backend contractors + CTO candidates (CEO owns)
   Success: 2 contractor offers out, CTO interviews ongoing
   Blocker: If can't close contractors → consider FTE earlier

Stretch goals (if ahead of schedule):
  • Redis caching implementation (engineering optimization)
  • Customer #1 case study published (marketing)
  • SOC 2 compliance checklist (future-proofing)
```

**5. Wrap-Up & Celebration (2 min)**

```
CEO: "Team, this was an incredible week. Customer #3 signed, infrastructure
scaled, hiring progressing. We're 50% of the way through our 13-week sprint
to prove this business works.

Next week, we hit Customer #2 implementation hard and bring #3 online.
This is the moment - three customers in production is the thesis.

Team celebration: If we hit next week's targets, we're doing team lunch
Friday. Let's keep this energy.

Have a great weekend. See you Monday 9am for planning. Dismissed!"
```

---

## Part 4: Weekly KPI Dashboard

**Update:** Every Friday, shared in team Slack

```
WEEK 10 OPERATIONS SUMMARY
═══════════════════════════════════════════════════════════

REVENUE & SALES
├─ MRR (Monthly Recurring Revenue): $10,000
├─ ARR (Annualized): $120,000 (on track for $125K by Week 13)
├─ Sales Pipeline: $625,000 (+$75K this week)
├─ Deals Closed This Week: 1 (Customer #3, $40K ACV)
├─ Pipeline Conversion Rate: 30%
├─ Expected Close Rate: 50% ($312K from $625K by Week 13)
└─ Status: 🟢 ON TRACK

CUSTOMER SUCCESS
├─ Customers Live: 2 (Customer #1 + #2 kickoff)
├─ Customers Implementing: 1 (Customer #2, 30% complete)
├─ Average NPS: 9.0 (target: 8.5+)
├─ Implementation % Complete: 30% (3 customers)
│   ├─ Customer #1: Live (go-live achieved)
│   ├─ Customer #2: 30% (pilot setup in progress)
│   └─ Customer #3: 0% (kickoff Week 11)
├─ Churn Rate: 0% (no customers lost)
├─ Expansion Revenue: 0% (too early, identifying opportunities)
└─ Status: 🟢 ON TRACK

ENGINEERING & INFRASTRUCTURE
├─ Uptime: 99.7%
├─ API Latency (p95): 120ms (target: <500ms)
├─ Error Rate: 0.1% (target: <0.5%)
├─ Incidents This Week: 0 (target: 0)
├─ Cloud Run Instances (avg): 2 (scaling: 1-8)
├─ Cloud Run Instances (max): 50 (ready for 3 customers)
├─ Firestore Indexes: 32 (was 12)
├─ Backup Status: Daily, tested ✓
└─ Status: 🟢 ON TRACK

TEAM & HIRING
├─ Current FTE: 5.5
├─ New Hires This Week: 1 (sales coordinator offer accepted)
├─ Open Roles: 3 (CTO, CSM, Backend contractors)
├─ Active Candidates: 15+ (across 3 roles)
├─ Offers Outstanding: 1 (sales coordinator)
├─ Expected Hires by Week 13: 3-4
├─ Team Morale: 9/10
└─ Status: 🟡 ON TRACK (hiring slower than ideal, accelerating)

FINANCIAL
├─ Cash Balance: $[X] (confidential)
├─ Monthly Burn Rate: $[Y]
├─ Runway: [N] months
├─ Infrastructure Cost: $430/month (3 customers)
├─ Gross Margin: 96% (revenue - infrastructure cost)
└─ Status: 🟢 HEALTHY

OVERALL STATUS: 🟢 ON TRACK
All three customers delivered on time, team scaling smoothly,
no major blockers. Series A positioning strong.
```

---

## Part 5: Decision-Making Framework

**How we make decisions in the operating rhythm:**

### Decision Authorities

| Decision | Owner | Timeline | Approval |
|----------|-------|----------|----------|
| Customer pricing (within band) | VP Sales | 24 hours | CEO |
| Engineering timeline (customer commitment) | CTO | 24 hours | CEO |
| Hiring (contractors/FTE) | CEO | 48 hours | Advisory board (if $50K+) |
| New feature requests (customer) | CSM | 48 hours | CTO (feasibility) |
| Series A narrative | CEO | 1 week | Board |
| Emergency decision (incident) | CTO | Real-time | CEO (informed after) |

### How Decisions Get Made in Monday/Friday Meetings

```
1. Problem stated (e.g., "Customer #3 wants feature X, not in scope")

2. Context provided (e.g., "Will delay go-live 2 weeks, cost $10K engineering")

3. Owner proposes solution (e.g., "CSM: I recommend deferring to Phase 2")

4. Discussion (e.g., "CTO: That's feasible" / "CEO: What's customer impact?")

5. Decision made (e.g., "CEO: Defer to Phase 2, CSM to communicate to customer")

6. Action assigned (e.g., "CSM: Inform customer by EOD, document in contract")

7. Follow-up set (e.g., "Friday review: Report customer reaction")

Total time: 5-10 minutes per decision
```

### Escalation Path

**Low-stakes decision (≤2 hours impact):**
→ Owner decides, inform CEO async

**Medium-stakes decision (≤1 day impact):**
→ Owner proposes, CEO approves via Slack (same day)

**High-stakes decision (customer commitment, $10K+ commitment, >1 day impact):**
→ Wait for Monday/Friday meeting OR call emergency huddle (30 min)

**Crisis (customer outage, data loss, security breach):**
→ CTO decides immediately, CEO informed, full team convenes

---

## Part 6: Meeting Templates (Copy & Paste)

### Monday Planning Meeting Agenda Template

```
MONDAY PLANNING - [DATE]
9:00am PT | 90 minutes | Zoom [link]

Attendees: CEO, CTO, VP Sales, CSM

AGENDA:
1. Opening & Context (5 min) - CEO
   → What week are we in?
   → North star for this week
   → Key themes/priorities

2. Last Week Review (10 min) - CEO
   → What we said we'd do vs. what we did
   → Wins
   → Gaps

3. Customer Health & Success (15 min) - CSM
   → Customer #1 status
   → Customer #2 status
   → Customer #3 status
   → Risks/blockers

4. Sales Pipeline & Revenue (15 min) - VP Sales
   → Pipeline summary
   → Stage breakdown
   → This week's deals
   → New pipeline generated

5. Engineering & Infrastructure (15 min) - CTO
   → Current state
   → This week's work
   → Blockers/risks

6. Weekly Priorities (20 min) - CEO
   → Top 3 priorities (in order of importance)
   → Secondary priorities
   → Stretch goals

7. Blockers & Escalations (10 min) - All
   → CEO: "What's blocking you?"
   → Resolve blockers or assign owner

8. Wrap-Up & Commitments (5 min) - CEO
   → Summarize commitments
   → Confirm priorities
   → Dismiss

DECISIONS TO MAKE:
[ ] Priority #1: _________
[ ] Priority #2: _________
[ ] Priority #3: _________
[ ] Hiring: _________
[ ] Blocker: _________

FOLLOW-UP:
[ ] Action items documented in Asana
[ ] Team notified of priorities (Slack)
[ ] External stakeholders informed (customers, board)
```

### Friday Review Meeting Agenda Template

```
FRIDAY REVIEW - [DATE]
4:00pm PT | 30 minutes | Zoom [link]

Attendees: CEO, CTO, VP Sales, CSM

AGENDA:
1. Wins Celebration (5 min) - CEO
   → What did we ship this week?
   → Celebrate wins (big and small)
   → Acknowledge great work

2. Metrics Review (10 min) - CEO
   → MRR/ARR
   → Pipeline
   → Customer NPS
   → Engineering metrics
   → Team metrics

3. Blockers & Challenges (5 min) - All
   → What could be better?
   → Root cause?
   → Action for next time?

4. Next Week Priorities (8 min) - CEO
   → 3 big rocks for next week
   → Owner for each
   → Success criteria
   → Potential blockers

5. Wrap-Up & Celebration (2 min) - CEO
   → Celebrate week
   → Motivate for next week
   → Team activities (if applicable)
   → Dismiss

METRICS TO REVIEW:
[ ] MRR: $________
[ ] Pipeline: $________
[ ] Customers: ________
[ ] Implementation: ________%
[ ] Team: ________
[ ] Infrastructure: Uptime ________%, Latency ________ms

FOLLOW-UP:
[ ] KPI dashboard updated
[ ] Wins shared in company Slack
[ ] Monday planning agenda drafted
[ ] Action items assigned
```

---

## Part 7: Operating Rhythm in Action (Week 10-13 Timeline)

### Week 10 Rhythm

**Monday 9am:** Planning meeting
- Context: "Customer #3 deal is hot. Need to close this week."
- Priorities: Close #3, implement #2 (50%), hire sales coordinator
- Blockers: Customer #3 still negotiating price

**Daily 3pm:** Standup (4 people, 10 min)
- Updates on #3 negotiation, #2 implementation progress, hiring

**Friday 4pm:** Review meeting
- Wins: #3 deal signed! #2 kickoff successful! Sales coordinator hired!
- Metrics: MRR $10K, pipeline $625K, team morale 9/10
- Next week: #2 implementation 75%, #3 kickoff, CTO onboarding starts

### Week 11 Rhythm

**Monday 9am:** Planning meeting
- Context: "Three customers now active. This is the moment - scaling playbook."
- Priorities: #2 implementation 75%, #3 kickoff, CTO starts
- Blockers: Backend contractors not yet hired

**Daily 3pm:** Standup
- Daily coordination on 3 customer implementations
- CTO onboarding updates
- Backend contractor hiring progress

**Friday 4pm:** Review meeting
- Wins: #3 kickoff successful! #2 at 75%! CTO hired (starts Monday Week 12)!
- Metrics: MRR $13K, pipeline $700K, 3 customers implementing
- Next week: #2 go-live, #3 implementation 40%, contractor onboarding

### Week 12 Rhythm

**Monday 9am:** Planning meeting
- Context: "Two customers going live this week. Engineering crunch time."
- Priorities: #2 go-live, #3 implementation, contractor ramp
- Blockers: Infrastructure scaling confirmed

**Daily 3pm:** Standup
- Cutover countdown
- Go-live support coordination
- CSM check-ins

**Friday 4pm:** Review meeting
- Wins: #2 LIVE! #3 at 75%! Contractors productive! Zero incidents!
- Metrics: MRR $13.3K, 2 customers live, infrastructure flawless
- Next week: #3 go-live, all customers live by Week 13 end

### Week 13 Rhythm

**Monday 9am:** Planning meeting
- Context: "Final week - get all 3 customers live, finish Series A narrative."
- Priorities: #3 go-live, SLO validation, Series A deck ready
- Blockers: None identified

**Daily 3pm:** Standup
- #3 cutover support
- All-hands focus on customer success
- Series A narrative reviews

**Friday 4pm:** Review meeting - CELEBRATION
- Wins: ALL 3 CUSTOMERS LIVE! 🎉 $125K ARR! Zero incidents! Team expanded!
- Metrics: MRR $10,417, ARR $125K, 3 customers 100% live, team 8 FTE
- Next: Series A fundraising begins Monday

---

## Success Criteria (Operating Rhythm)

```
EXECUTION CADENCE:
  ✓ 100% attendance: All Monday/Friday meetings
  ✓ Agenda shared: By Sunday evening (for Monday)
  ✓ Decisions documented: In Asana within 2 hours
  ✓ Action items: Owner assigned, deadline set

ALIGNMENT:
  ✓ Team knows priorities: Before Monday meeting ends
  ✓ Team knows blockers: Escalated same day
  ✓ Team knows next week plan: By Friday 4:30pm
  ✓ Leadership aligned: No surprises

METRICS:
  ✓ KPI dashboard: Updated weekly (Friday)
  ✓ Wins celebrated: Every Friday review
  ✓ Blockers addressed: Within 24 hours
  ✓ Decisions made: 80%+ by consensus (20% by CEO authority)

MORALE:
  ✓ Team feels heard: Can speak in meetings without fear
  ✓ Team celebrates wins: Shoutouts every standup
  ✓ Team knows impact: Understands how work connects to customers
  ✓ Team energy: Improves week-over-week
```

---

**Document Status:** Ready for Implementation
**Last Updated:** January 26, 2026
**Owner:** CEO
**Review Cycle:** Weekly (adjust after first month if needed)
**Start Date:** Monday, January 27, 2026 (Week 10 Day 1)
