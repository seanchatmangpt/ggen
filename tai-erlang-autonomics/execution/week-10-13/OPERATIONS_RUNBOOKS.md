# Operations Runbooks: Week 10-13 Executable Processes

**Status:** Ready for Implementation
**Purpose:** Repeatable, executable checklists for customer success, sales, and engineering
**Format:** Markdown checklists with time estimates, owners, and decision trees
**Review:** Update after each use (continuous improvement)

---

## Runbook 1: Customer Onboarding (30-60-90 Day Program)

### Week 1: Kickoff & Discovery

**Duration:** 5-7 business days
**Owner:** CSM (with CTO support)
**Success Criteria:** Customer signed, team aligned, baseline metrics captured

**Pre-Kickoff (Day -2)**
- [ ] Contract signed and executed
- [ ] Admin user created in Auth0
- [ ] Customer Firestore namespace created
- [ ] API keys generated (secure handoff planned)
- [ ] Training materials prepared (PDF + video)
- [ ] CSM scheduled 2-hour kickoff call
- [ ] CTO scheduled 1-hour technical architecture call
- **Time estimate:** 4 hours (CSM + CTO)

**Day 1: Kickoff Meeting (2 hours, 10am-12pm)**
- [ ] Attendees: Customer sponsor, inventory manager, IT lead; TAI: CEO, CSM, CTO
- [ ] Agenda:
  1. Welcome + relationship building (10 min)
  2. Success plan overview (15 min): 30/60/90 timeline, milestones, go-live date
  3. Current state assessment (30 min): Warehouse locations, inventory volume, pain points
  4. Success metrics (15 min): Baseline accuracy, cycle count time, sample data
  5. Technical architecture (15 min, CTO): API, data sync, dashboard access
  6. Next steps (10 min): Pilot warehouse selection, training dates
  7. Q&A (5 min)
- [ ] Document: Meeting notes, action items, key contacts
- [ ] Action items captured in Asana/Trello (shared with customer)
- **Time estimate:** 2 hours

**Day 2-3: Admin Setup & Training**
- [ ] Customer admin training (1 hour, video call)
  - [ ] Create inventory records (batch upload)
  - [ ] Run sample transaction
  - [ ] Access dashboard, reports
  - [ ] Review API documentation
- [ ] IT/security requirements (checklist):
  - [ ] Firewall rules (if needed)
  - [ ] VPN/network access
  - [ ] Data classification (PII handling)
  - [ ] Audit logging requirements
- [ ] Pilot warehouse selected
- [ ] Sample data exported from customer's system
- **Time estimate:** 3 hours (CSM + CTO)

**Day 4-5: Environment Setup**
- [ ] Sample data loaded to pilot environment
- [ ] Dashboard configured (custom fields, filters)
- [ ] User accounts created (inventory manager + operator)
- [ ] Training session #1: "Welcome to TAI" (recorded, 1 hour)
  - [ ] Dashboard overview
  - [ ] Transaction types (receive, move, count, transfer)
  - [ ] Report generation
  - [ ] Mobile app walkthrough
- [ ] Customer records all questions in shared doc
- **Time estimate:** 4 hours (CSM + CTO)

**Week 1 Completion Check**
- [ ] Customer team trained and confident
- [ ] Pilot environment ready for data
- [ ] No blockers for Week 2 (escalate if found)
- [ ] CSM 1:1 with customer sponsor (5-min feedback)
- [ ] Internal: CSM debriefs CEO on customer sentiment (10 min)

---

### Week 2: Pilot Execution

**Duration:** 5-7 business days
**Owner:** CSM + Inventory Manager (customer)
**Success Criteria:** Real data loaded, baseline accuracy measured, customer confident

**Day 6: Data Loading & Parallel Running**

Objective: Load real inventory data without disrupting current operations

- [ ] Export full inventory from customer system (CSV format)
- [ ] Data validation: Count records, check for duplicates, verify required fields
- [ ] Load to TAI pilot environment:
  ```
  Location 1: [X items] warehouse in [city]
  Location 2: [Y items] warehouse in [city]
  Total items loaded: [X+Y]
  ```
- [ ] Customer runs parallel cycle count:
  - [ ] Manual count (current system)
  - [ ] TAI count (new system) simultaneously
  - [ ] Document variance (expected: <5% variance acceptable)
- [ ] CSM available via Slack for real-time support (8am-6pm)
- **Time estimate:** 6 hours (CSM monitoring, customer executing)

**Day 7-8: Baseline Metrics**

- [ ] Measure current state:
  - [ ] Cycle count accuracy: % variance from physical inventory
  - [ ] Cycle count time: hours per warehouse per quarter
  - [ ] Current errors: Top 3-5 error types (lost items, location errors, data entry)
  - [ ] User satisfaction: How do current users rate the process?
  - [ ] ROI baseline: Cost of errors, time spent, labor inefficiency
- [ ] Customer workshop (1 hour, on-site if possible):
  - [ ] Show data quality issues found
  - [ ] Explain TAI's solution (real-time sync prevents errors)
  - [ ] Preview expected improvements
- [ ] Document findings in shared success plan spreadsheet
- **Time estimate:** 4 hours (CSM analysis + customer review)

**Day 9-10: Refinement & Go/No-Go**

- [ ] Review Week 2 metrics with customer:
  - [ ] Accuracy measurement: Baseline established ✓
  - [ ] System stability: Any data issues? ✓
  - [ ] Training effectiveness: Users comfortable? ✓
  - [ ] Timeline: Still on track for go-live? ✓
- [ ] Address any issues found:
  - [ ] Data cleanup (if needed)
  - [ ] Additional training (if needed)
  - [ ] Technical adjustments (if needed)
- [ ] Customer sign-off: "Ready for production?"
  - [ ] YES → Proceed to Week 3 (production cutover)
  - [ ] NO → Diagnose issue, schedule fix, extend pilot 1-2 weeks
- [ ] Internal: Celebrate pilot success (small win)
- **Time estimate:** 3 hours (CSM + CTO if needed)

**Week 2 Completion Check**
- [ ] Baseline accuracy documented
- [ ] Customer team confident with system
- [ ] No critical issues found
- [ ] Go/No-Go decision made and documented

---

### Week 3: Production Launch

**Duration:** 5-7 business days (critical week)
**Owner:** CSM + CTO (high support)
**Success Criteria:** Zero data loss, smooth transition, customer operating independently

**Day 11: Pre-Cutover Preparation**

Objective: Get ready for go-live without rushing

- [ ] Final data validation:
  - [ ] Verify all customer data in staging environment
  - [ ] Test critical API endpoints (no errors)
  - [ ] Verify backup (database snapshot taken)
- [ ] Cutover plan review (1 hour call):
  - [ ] Cutover window: Date/time (recommend: Friday 2pm, minimize customer impact)
  - [ ] Who's involved: CSM, CTO, Customer team
  - [ ] Rollback plan: If issues, how do we revert?
  - [ ] Communication: How do we stay in touch during cutover? (Slack + phone)
- [ ] Monitoring setup:
  - [ ] CSM monitors customer environment (dashboard, alerts)
  - [ ] CTO monitors system health (logs, error rates)
  - [ ] Customer monitors their operations (spot-checks)
- [ ] Communication to warehouse team:
  - [ ] Announcement: "New system going live Friday 2pm"
  - [ ] What's new: Dashboard, mobile app, real-time sync
  - [ ] What stays same: Warehouse processes, cycle counts
  - [ ] How to get help: "Slack message to CSM or @[customer-admin]"
- **Time estimate:** 3 hours (CSM + CTO)

**Day 12: Cutover Day (Production Go-Live)**

Timeline: Friday 2pm-5pm (3 hours)

- **2:00 PM: Customer communications stop, sync begins**
  - [ ] CSM sends final message to customer team: "Cutover starting now. Stand by."
  - [ ] CTO stops data sync (read-only mode for old system)
  - [ ] Final data migration runs: Old system → TAI production
  - [ ] Verification: All items present, no missing records ✓
  - **Time:** 30 min

- **2:30 PM: System goes live**
  - [ ] Dashboard goes live for customer
  - [ ] Customer team can log in and see data ✓
  - [ ] Mobile app enabled for warehouse staff
  - [ ] CSM on Slack for immediate support
  - [ ] CTO monitoring: Error logs, API latency, database health
  - **Time:** Ongoing

- **2:45 PM: Smoke test by customer**
  - [ ] Inventory manager logs in, verifies data looks correct
  - [ ] Warehouse operator completes 1 sample transaction (receive)
  - [ ] Report generated (proves system working)
  - [ ] Customer confirms: "We're good to go" via Slack
  - **Time:** 15 min

- **3:00 PM: Production declared live**
  - [ ] CSM announces: "You're live! All systems normal."
  - [ ] Customer closes old system (irreversible point)
  - [ ] Monitoring increases: Hourly check-ins first 24 hours
  - **Time:** Immediate

- **3:00 PM - 5:00 PM: Hyper-responsive support**
  - [ ] CSM available on Slack (respond <5 min)
  - [ ] CTO on standby (respond <5 min if called)
  - [ ] Any issues: Communicate, fix, verify
  - [ ] Weekly accuracy check: Still 97%+ accurate? ✓
  - **Time:** 2 hours monitoring

- **5:00 PM: End of day handoff**
  - [ ] Wrap-up call (10 min): How did it go? Any issues?
  - [ ] Document: Cutover success, any issues found, resolutions
  - [ ] Plan: Check-in call Monday 10am
  - **Time:** 10 min

**Time estimate:** 3 hours (CSM) + 3 hours (CTO) on cutover day

**Post-Cutover (Days 13-15): Stabilization**

- [ ] Daily check-ins (10 min each):
  - [ ] "How's the system performing?"
  - [ ] "Any issues with inventory accuracy?"
  - [ ] "Any user confusion or training needs?"
- [ ] Monitoring:
  - [ ] System health: No errors, latency normal, data integrity OK ✓
  - [ ] Data accuracy: Spot-check 50+ items (random sample)
  - [ ] User adoption: Are warehouse operators comfortable? ✓
- [ ] Training as needed:
  - [ ] If users struggling, schedule 30-min training call
  - [ ] Record session for future reference
  - [ ] Create FAQ doc from common questions
- [ ] Issues resolution:
  - [ ] If data discrepancy found: Investigate, fix, document
  - [ ] If performance issue: Optimize, test, deploy
  - [ ] If user confusion: Re-train, update docs

**Week 3 Completion Check**
- [ ] Customer live in production ✓
- [ ] Data integrity verified ✓
- [ ] Users confident and trained ✓
- [ ] No critical issues ✓
- [ ] Stabilization period: No major issues during first week ✓

---

### Week 4: Success Metrics & Expansion

**Duration:** 5-7 business days
**Owner:** CSM
**Success Criteria:** Baseline achieved, expansion identified, customer success confirmed

**Day 16-20: Measurement & Planning**

- [ ] Measure new state (compare to baseline from Week 2):
  - [ ] Accuracy improvement: Baseline 80% → Current ___% (expect 97%+)
  - [ ] Cycle count time: Baseline 40 hrs/quarter → Current ___ hrs
  - [ ] Error reduction: Top error types eliminated?
  - [ ] User satisfaction: Net Promoter Score (NPS) survey
  - [ ] ROI: "How much is this worth to you?"
- [ ] Document results in case study format
- [ ] Celebrate wins with customer (email + call):
  - [ ] "You're up 17 percentage points in accuracy!"
  - [ ] "You're saving 12 hours per cycle count!"
  - [ ] "Your inventory is now real-time synchronized!"
- [ ] Identify expansion opportunities (ask):
  - [ ] "How many other warehouses would benefit from TAI?"
  - [ ] "What other pain points are still manual?"
  - [ ] "How could we help with [related process]?"
- [ ] Quarterly Business Review planning:
  - [ ] Schedule for Week 8 (4-week check-in)
  - [ ] Agenda: Performance metrics, expansion opportunities, roadmap
- **Time estimate:** 4 hours (CSM)

**Success Criteria for 30-Day Program**
```
✓ Customer signed and trained
✓ Data accurately migrated
✓ System performing reliably
✓ Users confident and productive
✓ Baseline metrics established
✓ Expansion opportunities identified
✓ Customer satisfied (NPS 8+)
✓ CSM confident in success
```

---

## Runbook 2: Sales Qualification (BANT Framework)

**Purpose:** Quick 20-minute qualification to determine if prospect is worth pursuing
**Owner:** VP Sales
**Success Criteria:** Clear GO/NO-GO decision, time not wasted on bad fits

### Sales Qualification Checklist (BANT)

**B - Budget (Does customer have money?)**
- [ ] Budget allocated for this year: YES / NO / UNSURE
- [ ] Budget range: $[10K-50K] ACV / [$50K-100K] / [$100K+]
- [ ] Decision maker confirmed: Who approves spending?
- [ ] Procurement process: How long? (expect 4-8 weeks)
- **Red flags:** "No budget" OR "Budget approved next year" OR "Don't know"

**A - Authority (Can this person decide?)**
- [ ] Buyer's title: [e.g., VP Supply Chain, Operations Director]
- [ ] Do they control the budget? YES / NO
- [ ] Do they control the implementation? YES / NO
- [ ] Who else must sign off? [e.g., CEO, CFO, IT]
- [ ] Is this person empowered to say YES? YES / NO
- **Red flags:** "I'll need approval from [15 people]" OR Repeated "I'll get back to you"

**N - Need (Do they have the problem?)**
- [ ] Current system: [Legacy system? Spreadsheets? Manual process?]
- [ ] Current pain (quantify):
  - [ ] Inventory accuracy: ___% (target: 97%+)
  - [ ] Cycle count time: ___ hours (target: reduce 30%+)
  - [ ] Errors per month: ___
  - [ ] Cost of errors: $___
- [ ] Urgency: When do they need to solve this? (Q1? Q2? Q4?)
- **Red flags:** "We're happy with current system" OR "No timeline" OR Problem too small

**T - Timeline (When do they want to buy?)**
- [ ] Decision timeline: 2-4 weeks / 1-2 months / 3+ months / TBD
- [ ] Implementation start: [Ideally Week 10-11]
- [ ] Go-live target: [Ideally Week 11-12]
- [ ] Budget approval complete: YES / NO / In process
- **Red flags:** "Sometime next year" OR "Just exploring options" OR "No timeline"

### Qualification Decision Tree

```
Budget?
  → NO → PASS (tell them: "Let's reconnect when budget is allocated")
  → UNSURE → ASK (confirm in follow-up)
  → YES ↓

Authority?
  → NO → PASS (ask them to refer to decision maker)
  → UNSURE → INVESTIGATE (request intro to actual buyer)
  → YES ↓

Need?
  → NO → PASS (they don't feel the pain)
  → UNSURE → DISCOVER (deeper demo call scheduled)
  → YES ↓

Timeline?
  → NO TIMELINE → PASS (too early stage)
  → >6 MONTHS → WAIT (long cycle, circle back Q3)
  → 2-4 WEEKS → GO (hot lead, prioritize)
  → WARM → GO (likely to close)

Decision:
  → 4 YES = PURSUE (move to proposal stage)
  → 2-3 YES = WARM (schedule deeper discovery)
  → <2 YES = PASS (move on, save time)
```

### Sales Qualification Call Script (20 minutes)

**Opening (2 min)**
- "Thanks for taking the time. My goal is just to see if TAI Autonomics is a fit for [Company]."
- "I'm going to ask some quick questions—if at any point you feel like we're not aligned, just tell me and we'll wrap up."
- "Sound good?"

**Need (5 min)**
- "Walk me through how you currently manage inventory. What does a typical cycle count look like?"
- [LISTEN for pain: accuracy issues, time, manual effort]
- "On a scale of 1-10, how painful is [issue] for your team?"
- "What's the impact? [Probe for ROI]"

**Authority & Budget (5 min)**
- "If this solution could solve [pain], what would that be worth to you?"
- "Who else in your organization would need to approve a decision like this?"
- "Has your team budgeted anything for this type of solution this year?"

**Timeline & Next Steps (5 min)**
- "When are you hoping to have this solved by?"
- "What does the next step look like for you?"
- "If I sent over some information, when could we talk again?"

**Wrap-up (3 min)**
- If QUALIFIED: "Great! I'd love to schedule a deeper conversation next week. Let me send over some info on how we've helped [similar company]."
- If NOT QUALIFIED: "Thanks for the conversation. Based on what I'm hearing, it doesn't sound like the timing is right right now. Let me send you our website—feel free to reach out in [Q3/when budget is allocated]."

**Post-Call Actions**
- [ ] Logged in CRM with BANT score
- [ ] If qualified: Schedule next meeting (demo / deeper discovery)
- [ ] If not qualified: Add to future pipeline (6-12 months)
- [ ] Send follow-up email (20 min after call)

---

## Runbook 3: Proposal Generation (24-Hour SLO)

**Purpose:** Generate customer proposal from template within 24 hours
**Owner:** Sales Coordinator (with VP Sales approval)
**Success Criteria:** Professional proposal sent within 1 business day

### Proposal Template & Customization Checklist

**Before Proposal Generation**
- [ ] Customer name, company, industry confirmed
- [ ] Problem statement documented (from discovery call)
- [ ] Budget range known (e.g., $40K-50K)
- [ ] Timeline confirmed (e.g., 8-week implementation)
- [ ] Implementation scope defined (e.g., 1-2 warehouses)
- [ ] Key success metrics identified (e.g., 97%+ accuracy)
- [ ] Sales Coordinator has template (use standard Google Doc)

### Proposal Structure (2-3 pages)

**Page 1: Executive Summary**
```
[CUSTOMER NAME] | Inventory Intelligence Implementation
Partner: TAI Autonomics

SITUATION:
[Customer name] manages X warehouses with Y inventory items.
Currently using [current system]. Pain points include:
  • Inventory accuracy: 80% (need 97%+)
  • Cycle count time: 40 hrs/quarter (need 30 hrs/quarter)
  • Error resolution time: 48 hours average

SOLUTION:
TAI Autonomics provides real-time inventory synchronization
via cloud-based API integration. Benefits:
  • Automated cycle counts (real-time, always accurate)
  • 95%+ accuracy achieved by [similar customer] in 60 days
  • Estimated $[X] annual savings from error reduction

INVESTMENT:
Professional Services: $[X] (60-day implementation)
Annual License: $[Y] (includes support, updates)
Total Year 1: $[X+Y]

Expected ROI: [Payback in 6-9 months, $[Z] annual value]

NEXT STEPS:
1. Kick-off meeting: [Date]
2. Pilot warehouse: [Location]
3. Go-live: [Date]
```

**Page 2: Detailed Solution**
```
IMPLEMENTATION TIMELINE (60 days)

Week 1: Discovery & Setup
  • Technical architecture review
  • Admin setup and training
  • Pilot warehouse selection

Week 2: Pilot Execution
  • Load sample data
  • Baseline accuracy measurement
  • Parallel running (validate accuracy)

Week 3: Production Launch
  • Cutover to TAI system
  • Real-time monitoring
  • Stabilization

Week 4: Optimization & Success
  • Performance metrics review
  • Expansion planning
  • Quarterly business review

DELIVERABLES:
  ✓ System implementation and configuration
  ✓ Staff training (3 sessions)
  ✓ Data migration and validation
  ✓ Custom dashboard and reporting
  ✓ 90-day support included

INVESTMENT DETAILS:
Professional Services: $[X]
  • Implementation: [X hours] @ $[rate]
  • Training: [X hours] @ $[rate]
  • Support: [X hours] @ $[rate]

Annual License: $[Y]
  • Monthly cloud infrastructure
  • API calls (included with tier)
  • Support and updates
```

**Page 3: Terms & Next Steps**
```
TERMS:
• Price: $[X] professional services + $[Y] annual license
• Payment: $[Deposit] due upon signature, balance upon go-live
• Start date: [Date]
• Implementation duration: 60 days
• Support: Included for first 90 days
• SLA: 99.5% uptime, <500ms API latency

APPENDIX:
[Include: Case study from similar customer, pricing breakdown, team bios]

PROPOSAL VALID: 30 days from this date

NEXT STEPS:
1. Review proposal (your end)
2. Questions call (schedule for [Date] if needed)
3. Sign agreement (we'll send legal docs)
4. Payment processing
5. Kick-off meeting and implementation

Contact:
[VP Sales Name]
[Email]
[Phone]
[calendly link]
```

### Proposal Generation Steps

**Step 1: Gather Information (30 min)**
- [ ] Pull customer details from CRM (discovery notes)
- [ ] Review discovery call transcript (pain points, budget, timeline)
- [ ] Confirm implementation scope with CTO (2-warehouse or 1?)
- [ ] Get pricing approval from CEO (budget range known?)

**Step 2: Create Customized Proposal (45 min)**
- [ ] Copy proposal template from shared drive
- [ ] Update customer name (all 3 pages)
- [ ] Update SITUATION section (specific pain points)
- [ ] Update INVESTMENT section (customer's specific budget)
- [ ] Update TIMELINE (their specific implementation window)
- [ ] Include relevant case study (similar customer, similar problem)
- [ ] Add team bios (short 2-3 sentence bios: VP Sales, CTO, CSM)

**Step 3: Internal Review & Approval (30 min)**
- [ ] VP Sales reviews (1:1 check-in, 10 min)
  - [ ] "Does this match our conversation?"
  - [ ] "Pricing appropriate for this customer?"
  - [ ] "Timeline realistic?"
- [ ] VP Sales approves or requests changes
- [ ] If changes: Update proposal (15 min), re-review (5 min)

**Step 4: Send to Customer (15 min)**
- [ ] Send via email with personalized message
  ```
  Subject: Inventory Intelligence Implementation Proposal - [Customer Name]

  Hi [Contact Name],

  Thanks again for the great conversation on [date]. Based on what I learned
  about [customer name]'s challenges, I've put together a proposal for how TAI
  can help you achieve real-time inventory visibility.

  Key highlights:
  • 97% inventory accuracy (from current 80%)
  • 60-day implementation, go-live by [date]
  • $[X] investment, ROI in 6-9 months

  I'm happy to answer any questions. How does [date] look for a brief
  questions call? I'll also send over a case study from [similar customer]
  in a separate email.

  Looking forward to working together!

  [VP Sales Name]
  ```

- [ ] Log in CRM: "Proposal sent [date]"
- [ ] Schedule follow-up: "Check in on Friday if no response"

**Performance Target: 24-hour SLO**
- Proposal requested: Monday 10am
- Proposal sent: Tuesday 10am (or same day if urgent)

---

## Runbook 4: Incident Response (Production Issue)

**Purpose:** Quick escalation and resolution process for customer-impacting issues
**Owner:** CTO (escalates to VP Sales/CEO if needed)
**Success Criteria:** Issue diagnosed within 15 min, resolution path clear within 30 min

### Incident Severity Levels

**CRITICAL (Page CTO immediately)**
- Customer unable to access system (downtime)
- Data corruption or loss
- Security breach (unauthorized access)
- API errors for all customers

**HIGH (Email CTO, resolve <1 hour)**
- API latency > 2 seconds
- Error rate > 5%
- Single customer experiencing data issues
- Performance degradation for majority of users

**MEDIUM (Email CTO, resolve <4 hours)**
- Slow dashboard (> 1 sec load time)
- Minor API errors (< 1% rate)
- Single feature not working
- Cosmetic UI issues

**LOW (Log ticket, resolve next business day)**
- Documentation errors
- Enhancement requests
- Minor UI improvements
- Training requests

### Incident Response Playbook (CRITICAL)

**Minute 0-5: Alert & Acknowledge**
- [ ] Alert received (PagerDuty, customer email, error log)
- [ ] CTO gets paged immediately
- [ ] CTO acknowledges: "Investigating" (via Slack to customer within 5 min)
- [ ] Launch incident bridge (Slack channel: #incident-[timestamp])

**Minute 5-15: Diagnosis**
- [ ] CTO investigates:
  - [ ] Check Cloud Run logs (errors? latency?)
  - [ ] Check Firestore (quota exceeded? slow queries?)
  - [ ] Check Load Balancer (healthy backends?)
  - [ ] Ping API endpoints (responding?)
  - [ ] Check monitoring dashboard (any anomalies?)
- [ ] Parallel: CSM contacts customer
  - [ ] "We're aware of the issue. Our team is investigating."
  - [ ] "What exactly are you seeing?"
  - [ ] "Expected resolution time: [30 min / 1 hour / TBD]"
- [ ] Diagnosis documented in incident log

**Minute 15-30: Resolution Path**
- [ ] Root cause identified (likely: database quota, slow query, deployment issue, etc.)
- [ ] Resolution plan clear:
  - [ ] If quota: Scale up database, retry requests
  - [ ] If slow query: Optimize query, add index, fallback to simpler query
  - [ ] If deployment: Rollback to previous version
  - [ ] If infrastructure: Scale Cloud Run, restart service
- [ ] If rollback needed: Execute immediately (automated or manual)
- [ ] If fix needed: Implement and test in staging (5-10 min)

**Minute 30-60: Resolution & Verification**
- [ ] Fix deployed to production
- [ ] Verification steps:
  - [ ] API endpoints responding (latency < 500ms)
  - [ ] Customer can access system
  - [ ] Data integrity verified (spot-check)
  - [ ] No new errors appearing (error rate < 0.5%)
- [ ] CSM confirms with customer: "System is back to normal. We'll send a summary email in 30 min."

**Post-Incident (Next business day)**
- [ ] Incident summary email to customer:
  ```
  INCIDENT SUMMARY: [Issue description]
  Duration: [Time down]
  Root cause: [Technical explanation in plain English]
  Resolution: [What we did to fix]
  Prevention: [What we'll do to prevent recurrence]
  Timeline: [When we'll implement prevention]
  ```
- [ ] Internal post-mortem (15 min meeting):
  - [ ] What happened?
  - [ ] Why did it happen?
  - [ ] What's our action plan to prevent?
  - [ ] Timeline for implementation
- [ ] Action items tracked in Asana
- [ ] Customer success impact: Any follow-up needed?

---

## Runbook 5: Weekly Operations Review (Friday 4pm)

**Purpose:** Review week's performance, identify issues, plan next week
**Owner:** CEO (with CSM, VP Sales, CTO)
**Duration:** 30 minutes
**Success Criteria:** Team aligned, blockers identified, plan for next week clear

### Weekly Review Agenda (30 minutes)

**Preparation (Monday-Thursday)**
- [ ] CSM: Compile customer metrics
  - [ ] Implementation progress (% complete per customer)
  - [ ] NPS feedback (any red flags?)
  - [ ] Any customer concerns or blockers?
- [ ] VP Sales: Update pipeline
  - [ ] New leads added to pipeline (count + total $)
  - [ ] Proposals sent this week (count + total $)
  - [ ] Deals closed (count + total $)
  - [ ] Current pipeline ($)
- [ ] CTO: Infrastructure status
  - [ ] Any incidents? (brief summary)
  - [ ] Infrastructure scaling on track?
  - [ ] Any concerns for next week?
- [ ] CFO: Financial snapshot (if exists)
  - [ ] Cash balance
  - [ ] Invoices sent
  - [ ] Burn rate projection

**Meeting Flow (Friday 4pm, 30 min)**

1. **CEO Opening (2 min)**
   - "How's the team feeling?"
   - "Any major wins?"
   - "Any major concerns?"

2. **Customer Success Review (8 min)**
   - CSM: "Customer #1 is at 95% accuracy, happy with system. Customer #2 onboarding started Monday, on track for Week 2 completion. Customer #3 still negotiating timeline."
   - Metrics dashboard (1-2 min review):
     ```
     Customer #1: 95% accuracy ✓ | NPS 9 ✓
     Customer #2: 50% implementation | on-track
     Customer #3: contract signed, kickoff next week
     ```
   - Blockers? → Escalate if any

3. **Sales Pipeline Review (8 min)**
   - VP Sales: "Pipeline $[X], added $[Y] this week. 3 proposals outstanding. Expecting $[Z] in new leads next week."
   - Pipeline dashboard:
     ```
     Discovery phase: $[X] (N customers)
     Proposal phase: $[Y] (N customers)
     Closing phase: $[Z] (N customers)
     Total pipeline: $[Total]
     ```
   - Blockers? → Escalate if any

4. **Engineering Status (5 min)**
   - CTO: "Customer #2 integrations in progress. No incidents. Database scaling on track. No blockers for Customer #3 deployment next week."
   - Infrastructure status:
     ```
     Uptime this week: 99.7% ✓
     API latency p95: 380ms ✓
     No critical issues ✓
     ```
   - Blockers? → Escalate if any

5. **Financial & Cash (3 min)**
   - Cash balance
   - Burn rate trajectory
   - Fundraising impact
   - Any concerns?

6. **Wins Celebration (2 min)**
   - Highlight: "Customer #2 onboarding successful"
   - Highlight: "New pipeline hit $500K"
   - Highlight: "Zero incidents, infrastructure stable"

7. **Next Week Priorities (2 min)**
   - Top 3 priorities for team:
     1. Customer #2 implementation 75% complete
     2. Customer #3 deployment planning
     3. Sales: Schedule 10+ discovery calls
   - Assign owners
   - Identify dependencies

**Post-Meeting**
- [ ] Update QUICK_REFERENCE.md KPI dashboard (15 min)
- [ ] Document action items in Asana
- [ ] Team message: "Here's what we accomplished this week and what's coming next"

---

## Runbook 6: Monthly Financial Close (Last Friday of Month)

**Purpose:** Close monthly financials, prepare for investor/board review
**Owner:** CFO or Finance contractor
**Duration:** 4-8 hours
**Success Criteria:** All invoices sent, revenue recognized, cash reconciled

### Month-End Closing Checklist

**First Week of Month (Closes Previous Month)**

- [ ] Invoicing
  - [ ] Generate invoices for all services delivered
  - [ ] Send via QuickBooks/Stripe
  - [ ] Follow up on past-due accounts (>30 days)
  - [ ] Record payment when received

- [ ] Revenue Recognition
  - [ ] Monthly recurring revenue (MRR) calculated
  - [ ] Professional services revenue (if not milestone-based)
  - [ ] Deferred revenue (prepayments) adjusted
  - [ ] Update financial model (MRR, ARR)

- [ ] Expense Tracking
  - [ ] Categorize all credit card expenses
  - [ ] Match invoices to bank statements
  - [ ] Record any out-of-pocket expenses (reimburse if > $100)
  - [ ] Review expense trends (any unusual items?)

- [ ] Cash Reconciliation
  - [ ] Bank balance matches accounting records
  - [ ] Credit card balances recorded
  - [ ] Outstanding checks tracked
  - [ ] PayPal/Stripe balances included

- [ ] Financial Summary
  - [ ] MRR: $[X] (target: $10,417 by Week 13)
  - [ ] Monthly expenses: $[Y]
  - [ ] Monthly burn rate: $[Z] (if not yet profitable)
  - [ ] Cash runway: [N] months
  - [ ] Cash impact: [+/- $K this month]

**Reporting & Sharing**
- [ ] Investor email (if exists):
  ```
  Monthly Financial Summary - [Month]

  Revenue:
    Customer #1: $[4,167]/month
    Customer #2: $[3,333]/month (partial, started mid-month)
    Total MRR: $[X]
    Total ARR: $[X × 12]

  Expenses:
    Payroll: $[Y]
    Cloud infrastructure: $[Z]
    Other: $[A]
    Total: $[B]

  Cash position:
    Starting balance: $[X]
    Cash in: $[Y]
    Cash out: $[Z]
    Ending balance: $[Ending]
    Runway: [N] months

  Key metrics:
    - CAC: $[X]
    - LTV: $[X]
    - Payback: [N] months
  ```

- [ ] Board/advisor update (if monthly reviews exist)
- [ ] Archive: Save monthly financial report for future reference

---

## Summary: Runbook Index

| Runbook | Duration | Owner | When Used |
|---------|----------|-------|-----------|
| Customer Onboarding (30-60-90) | 28 days | CSM | Per customer, every time |
| Sales Qualification (BANT) | 20 min | VP Sales | Every discovery call |
| Proposal Generation | 2 hours | Sales Coord | After qualification → before close |
| Incident Response | 30-60 min | CTO | When production issue occurs |
| Weekly Operations Review | 30 min | CEO | Every Friday 4pm |
| Monthly Financial Close | 4-8 hours | CFO | Last Friday of month |

---

**Document Status:** Ready for Execution
**Last Updated:** January 26, 2026
**Owner:** CEO
**Review Cycle:** After each use (continuous improvement)
**Next Review:** February 2, 2026 (after first uses)
