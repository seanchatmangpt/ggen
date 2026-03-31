# PRODUCT OPERATIONS GUIDE: Week 1-13 Reference

**Purpose:** Quick guide to running the product during 13-week execution
**Audience:** CEO, VP Product, VP Engineering, CSM, VP Sales
**Last Updated:** January 26, 2026

---

## WHAT TO DO THIS WEEK (Week 1: Jan 27 - Feb 2)

### Your Primary Document
📄 **PRODUCT_EXECUTION_LOG.md** - See "Section 13: Week 1 Snapshot"

### This Week's Focus
**Legal Setup Phase** - No product work yet

| Role | This Week's Task | Owner | Deadline |
|------|------------------|-------|----------|
| **CEO** | Delaware incorporation filed + EIN applied | You | Jan 28 |
| **CTO** | Finalize tech stack with advisors | CTO | Jan 31 |
| **VP Sales** | Confirm 50 warm intro targets | VP Sales | Jan 31 |
| **CSM** | Draft customer success SLA | CSM | Feb 2 |

### Key Metrics This Week
- [ ] Incorporation status (file number received?)
- [ ] EIN confirmation (received via email/phone?)
- [ ] Bank account opened (can we accept customer payments?)
- [ ] Team alignment (everyone understands 90-day plan?)

---

## WEEK 2-3: PRODUCT ARCHITECTURE & DEMO PREP

### Primary Documents
📄 **PRODUCT_EXECUTION_LOG.md** - Sections 6, 7 (Performance, Testing)
📄 **V1.1_ROADMAP.md** - Optional preview of anticipated features

### Key Activities
1. **CTO:** Architecture review with external advisor
2. **Engineering:** Begin MVP development
3. **VP Sales:** Finalize demo deck + sales playbook
4. **Product:** Define 6 core MVP features

### Quality Gates
Before Week 4 starts:
- [ ] Architecture documented + approved
- [ ] Tech stack locked (no changes)
- [ ] MVP scope locked (6 features max)
- [ ] Demo script tested (10 min, 7 slides)

---

## WEEK 4-5: MVP LAUNCH & SALES PREP

### Primary Documents
📄 **PRODUCT_EXECUTION_LOG.md** - Sections 6, 8 (Performance, Deployment)
📄 **week-3-4/WEEK_3_4_SALES_PIPELINE.md** - Sales targeting

### Key Activities
1. **Engineering:** MVP shipped + performance baseline tested
2. **VP Sales:** Cold outreach campaign (50+ prospects)
3. **Product:** Collect early feedback from advisors
4. **CSM:** Prepare customer onboarding runbook

### Quality Gates
Before Week 6 starts:
- [ ] MVP shipped (all 6 core features)
- [ ] Performance baseline: <200ms p95 response time
- [ ] Test coverage: >80%
- [ ] 10+ POC proposals sent
- [ ] 99%+ uptime in staging

---

## WEEK 6-7: FIRST CUSTOMER POC & DEAL

### Primary Documents
📄 **PRODUCT_EXECUTION_LOG.md** - Sections 2, 10, 11 (Customer #1, Incidents, Health)
📄 **week-5-6/WEEK_5_6_CUSTOMER_SUCCESS.md** - Implementation playbook

### Key Activities
1. **CSM:** Customer #1 onboarding + weekly check-ins
2. **Engineering:** Fix any POC-blocking issues (P0 bugs)
3. **VP Sales:** Drive customer #1 deal closure
4. **Product:** Collect detailed feedback on feature usage

### Quality Gates
Before Week 8 starts:
- [ ] Customer #1 signed contract
- [ ] First revenue booked ($50K ACV)
- [ ] Customer NPS >7 (baseline)
- [ ] Zero P0 critical bugs
- [ ] Implementation roadmap locked

### Weekly Product Review (Friday 4pm)
```
Agenda (30 min):
1. Customer feedback: What did C1 ask for? (5 min)
2. Feature velocity: What shipped? (3 min)
3. Bug status: Any P0/P1 issues? (3 min)
4. Performance: Any slowness complaints? (2 min)
5. Next week: Major milestones? (2 min)
6. Blockers: What needs help? (10 min)
7. Celebrate: What went well? (5 min)
```

---

## WEEK 8-9: SCALE TO 3 CUSTOMERS

### Primary Documents
📄 **PRODUCT_EXECUTION_LOG.md** - Sections 3, 4 (Customers #2, #3)
📄 **week-7-9/WEEK_7_9_CUSTOMER_IMPLEMENTATION.md** - Implementation at scale

### Key Activities
1. **CSM:** Manage Customer #1 early live, onboard C2 & C3
2. **Engineering:** Implement top 2-3 feature requests from Customer #1
3. **VP Sales:** Close deals with Customers #2 & #3
4. **Product:** Analyze usage data from Customer #1 (first 2 months)

### Quality Gates
Before Week 10 starts:
- [ ] Customers #2 & #3 signed
- [ ] $125K ARR on books
- [ ] All 3 customers in implementation
- [ ] Customer #1 feature requests addressed
- [ ] 0% churn rate
- [ ] Team expanded to 5.5 FTE

### Key Metrics Tracking
```
Update PRODUCT_EXECUTION_LOG weekly:
├─ Customer #1: Usage data, NPS, support tickets, churn risk
├─ Customer #2: POC progress, deal stage, expected close date
├─ Customer #3: POC progress, deal stage, expected close date
├─ Bug log: Any P0/P1 issues emerging?
├─ Performance: Still <200ms p95?
└─ Revenue: On track for $125K target?
```

---

## WEEK 10-13: PRODUCTION HARDENING & SERIES A PREP

### Primary Documents
📄 **PRODUCT_EXECUTION_LOG.md** - Sections 5, 6, 8 (Bugs, Performance, Deployment)
📄 **V1.1_ROADMAP.md** - Plan next phase based on customer feedback
📄 **week-10-13/WEEK_10_13_OPERATIONS_SCALING.md** - Infrastructure scaling

### Key Activities
1. **VP Product:** Finalize v1.1 roadmap based on customer feedback (Week 11)
2. **Engineering:** Fix technical debt + prepare for 10+ customer scale
3. **CSM:** Get case studies from all 3 customers
4. **CEO:** Prepare Series A pitch including roadmap

### Critical Decision Gates (See V1.1_ROADMAP.md)
- [ ] **Gate 1 (End of Week 11):** Customer feedback collected + feature list prioritized
- [ ] **Gate 2 (End of Week 11):** Roadmap approved by board
- [ ] **Gate 3 (End of Week 13):** Month 4 sprint plan finalized

### Roadmap Planning Process
```
Week 10 (Mar 31 - Apr 6): Data Collection
├─ VP Product: Analyze 3 months of Customer #1 data
├─ CSM: Interview all 3 customers (30 min each)
├─ Sales: Summarize top 5 prospect objections
└─ Engineering: Estimate effort on common requests

Week 11 (Apr 7 - Apr 13): Planning & Approval
├─ Product: Prioritize feature requests (vote on top 5)
├─ Engineering: Estimate effort (in story points)
├─ Board: Review + approve roadmap
└─ Communicate to customers: "v1.1 coming in Month 4"

Week 12 (Apr 14 - Apr 20): Preparation
├─ Engineering: Create detailed user stories
├─ Product: Define success metrics
├─ Sales: Brief on roadmap messaging
└─ Team: Prepare for Month 4 execution

Week 13 (Apr 21 - Apr 27): Execution Kickoff
├─ Publish roadmap to customers
├─ Sprint planning for Month 4
├─ Begin Epic 1 & Epic 2 work
└─ Celebrate Day 90 milestone!
```

### Success Criteria for Week 10-13
- [ ] All 3 customers live in production
- [ ] 99.5%+ uptime achieved
- [ ] $125K ARR baseline confirmed
- [ ] $600K+ pipeline for Q2+
- [ ] 0% churn
- [ ] NPS average >8 across 3 customers
- [ ] v1.1 roadmap approved + communicated
- [ ] Series A materials complete

---

## CRITICAL DOCUMENTS BY SCENARIO

### "A customer found a bug"
1. Check: **PRODUCT_EXECUTION_LOG.md** - Section 5 (Bug Log)
2. If critical (blocks customer): **PRODUCT_EXECUTION_LOG.md** - Section 10 (Incident Response)
3. Follow: **week-1-2/INCIDENT_RESPONSE_RUNBOOK.md** (Week 1-2) or **week-10-13/OPERATIONS_RUNBOOKS.md** (Week 10-13)

### "A customer wants a feature"
1. Check: **PRODUCT_EXECUTION_LOG.md** - Section 14 (Customer Feedback Template)
2. Record: Add to appropriate customer section (Sections 2-4)
3. Prioritize: Use **V1.1_ROADMAP.md** - Section 1 (Decision Criteria)
4. Communicate: Reply with timeline + add to roadmap if priority

### "Sales is complaining about missing features"
1. Check: **PRODUCT_EXECUTION_LOG.md** - Section 12 (Velocity)
2. Escalate: Weekly standup - discuss with VP Product + VP Sales
3. Plan: Either accelerate feature or brief sales on timeline
4. Document: Add to **V1.1_ROADMAP.md** - Section 2 (Feature Requests)

### "Performance is slow"
1. Check: **PRODUCT_EXECUTION_LOG.md** - Section 6 (Performance Metrics)
2. Investigate: Run load test with customer data
3. If critical: Escalate to daily standup
4. Fix: Add to **PRODUCT_EXECUTION_LOG.md** - Section 9 (Technical Debt)

### "Time to plan v1.1 roadmap"
1. Read: **V1.1_ROADMAP.md** - Sections 1-3 (Framework + anticipated requests)
2. Execute: Follow **V1.1_ROADMAP.md** - Section 5 (Roadmap Timeline)
3. Decide: Use decision gates in **V1.1_ROADMAP.md** - Section 8

### "Need to update KPIs"
1. Check: **execution/KPI_DASHBOARD.md** - Revenue tracking
2. Update: Friday 5pm after weekly review
3. Sync: Make sure **PRODUCT_EXECUTION_LOG.md** - Section 13 (Weekly Snapshot) is consistent

---

## WEEKLY STANDUP TALKING POINTS

### For VP Product
```
"This week I'm tracking:
1. Customer feedback patterns (3 customers, top 2-3 themes)
2. Feature velocity (shipped this week? on track?)
3. Bug trends (P0/P1 count trend up or down?)
4. v1.1 planning (if Week 10+: roadmap progress)

Priority today: [One thing that must happen]
Blocker: [What needs help from team?]
```
```

### For VP Engineering
```
"This week I'm tracking:
1. Test coverage (moving toward 80%+?)
2. Performance (p95 response time trend)
3. Incident response (any P0 critical issues?)
4. Technical debt (is it growing or shrinking?)

Priority today: [One bug fix or feature delivery]
Blocker: [Infrastructure, hiring, or architecture blocking us?]
```

### For CSM/VP Sales
```
"This week I'm tracking:
1. Customer health (NPS, support tickets, usage)
2. Customer requests (what are they asking for?)
3. Sales pipeline (new POCs? objections?)
4. Feature requests (which 3 are most common?)

Priority today: [One customer issue or feature priority]
Blocker: [What's slowing customer onboarding or closing?]
```

---

## DOCUMENT MAP: WHICH FILE TO OPEN WHEN

```
WEEKLY OPERATIONS:
├─ Standing daily standup? → PRODUCT_EXECUTION_LOG.md
├─ Weekly KPI review (Friday)? → KPI_DASHBOARD.md + PRODUCT_EXECUTION_LOG.md
├─ Monthly board update? → KPI_DASHBOARD.md + DELIVERY_SUMMARY.txt
└─ Customer issue? → PRODUCT_EXECUTION_LOG.md Sections 2-4

WEEK-SPECIFIC PLANNING:
├─ Week 1-2? → week-1-2/WEEK_1_2_IMPLEMENTATION_GUIDE.md + INCORPORATION checklist
├─ Week 3-4? → week-3-4/WEEK_3_4_DEMO_AND_POC.md
├─ Week 5-6? → week-5-6/WEEK_5_6_CUSTOMER_SUCCESS.md
├─ Week 7-9? → week-7-9/WEEK_7_9_CUSTOMER_IMPLEMENTATION.md
└─ Week 10-13? → week-10-13/WEEK_10_13_OPERATIONS_SCALING.md + V1.1_ROADMAP.md

ROADMAP & FEATURE PLANNING:
├─ Prioritizing features? → V1.1_ROADMAP.md Sections 1-3
├─ Planning Month 4? → V1.1_ROADMAP.md Sections 4-5
├─ Evaluating risk? → V1.1_ROADMAP.md Section 10
└─ Communicating to investors? → V1.1_ROADMAP.md Section 11

INCIDENTS & SUPPORT:
├─ Critical bug or incident? → PRODUCT_EXECUTION_LOG.md Section 10
├─ Customer support escalation? → week-X-Y/INCIDENT_RESPONSE_RUNBOOK.md
└─ Performance issue? → PRODUCT_EXECUTION_LOG.md Section 6

SERIES A PREP (Week 13+):
├─ Roadmap narrative? → V1.1_ROADMAP.md Section 11
├─ Scaling plan? → week-10-13/WEEK_10_13_OPERATIONS_SCALING.md
├─ Customer references? → week-10-13/WEEK_10_13_SCALE_AND_CASE_STUDIES.md
└─ Financial model? → week-10-13/FINANCIAL_MODEL.md
```

---

## INTEGRATION WITH DAILY CADENCE

### Daily (3pm EST)
- **Owner:** CEO
- **Participants:** CEO, CTO, VP Sales, CSM
- **Document:** Reference **PRODUCT_EXECUTION_LOG.md** Section 13 (this week's snapshot)
- **Time:** 15 min (hard stop)
- **Format:** Each person: shipped yesterday → working on today → blockers → win

### Friday Review (4pm EST)
- **Owner:** CEO + VP Product
- **Participants:** Full team
- **Documents:** Update **PRODUCT_EXECUTION_LOG.md** + **KPI_DASHBOARD.md**
- **Time:** 30 min (hard stop)
- **Format:** KPI review → wins → blockers → next week preview

### Monthly Board (1st Monday, 60 min)
- **Owner:** CEO
- **Participants:** Board + CEO + CTO + VP Sales + advisors
- **Documents:** **KPI_DASHBOARD.md** + **DELIVERY_SUMMARY.txt** from week-X-Y folder
- **Format:** Financial review → customers → product → team → risks

---

## CHECKLISTS FOR COMMON TASKS

### Weekly Product Review Checklist (Friday 4pm)

**Before standup (30 min prep):**
- [ ] Read any new support tickets from the week
- [ ] Check customer usage data (if available)
- [ ] Tally bugs: any new P0/P1?
- [ ] Check performance metrics: response time trends
- [ ] Review features shipped this week
- [ ] Prepare 3-slide summary for team

**During standup (25 min):**
- [ ] Customer feedback themes (5 min)
- [ ] Feature velocity summary (3 min)
- [ ] Bug status + trend (3 min)
- [ ] Performance status (2 min)
- [ ] Next week priorities (2 min)
- [ ] Blockers needing escalation (10 min)

**After standup (30 min):**
- [ ] Update PRODUCT_EXECUTION_LOG.md Section 13
- [ ] Update KPI_DASHBOARD.md
- [ ] Send summary email to team + advisors
- [ ] Schedule any escalation meetings if needed

---

### Month-End Retrospective (Last Friday of Month)

- [ ] Gather all customer feedback from the month
- [ ] Compile most-requested features (vote tally)
- [ ] Analyze performance trends (is system getting faster/slower?)
- [ ] Count bugs by severity (P0, P1, P2, P3)
- [ ] Measure test coverage trend
- [ ] Review technical debt (is it growing?)
- [ ] Celebrate wins as a team
- [ ] Document in week-X-Y/EXECUTION_SUMMARY.txt

---

## GLOSSARY & ABBREVIATIONS

| Term | Definition |
|------|-----------|
| **P0** | Critical bug - blocks entire system |
| **P1** | High priority - blocks customer use |
| **P2** | Medium - workaround available |
| **P3** | Low - nice-to-have fix |
| **MTTR** | Mean Time To Recovery (time from incident to fix) |
| **NPS** | Net Promoter Score (customer satisfaction, 0-10) |
| **ACV** | Annual Contract Value (customer contract amount) |
| **ARR** | Annual Recurring Revenue (all customers combined) |
| **POC** | Proof of Concept (customer trial period) |
| **SLA** | Service Level Agreement (uptime commitment) |
| **p95** | 95th percentile response time (performance metric) |
| **DoD** | Definition of Done (when feature is complete) |
| **MVP** | Minimum Viable Product (6 core features) |
| **RCA** | Root Cause Analysis (incident postmortem) |

---

## COMMUNICATION TEMPLATES

### Customer Feature Request Response

```
Hi [Customer Name],

Thanks for requesting [Feature Name]. We hear this from [# customers] of our users.

Timeline:
├─ Month [M]: Investigating feasibility + estimating effort
├─ Month [M+1]: Adding to roadmap (final confirmation in [date])
└─ Target delivery: [Month M+2-3]

In the meantime:
├─ Here's a workaround: [Workaround if exists]
└─ Talk to your CSM about: [Alternative approach]

We'll update you monthly on progress.

Best,
[VP Product]
```

### Bug Status Update

```
Hi [Customer Name],

We found and fixed the issue you reported: "[Bug Description]"

Details:
├─ Root cause: [Technical explanation]
├─ Impact: [How many customers affected]
├─ Fix deployed: [Date/time]
└─ Verification: [We tested with your data and confirmed fixed]

No action needed on your end. It's live now.

Questions? Reach out to [CSM Name].

Best,
[VP Engineering]
```

### Roadmap Preview for Customer

```
Hi [Customer Name],

Great feedback from your Month [M] review. We're planning v1.1 to address the top requests from all our customers.

Top features coming in [Month]:
├─ Feature A (you requested this) - [brief description]
├─ Feature B (2 other customers also asked) - [brief description]
└─ Feature C (high-impact for your use case) - [brief description]

Expected timeline:
├─ Planning: [Week date]
├─ Launch: [Month target]
└─ Detailed: [Point to V1.1_ROADMAP.md public link]

Your feedback helped shape this. Thank you!

Best,
[VP Product]
```

---

## NEXT IMMEDIATE ACTIONS (This Week)

**TODAY (Monday, Jan 27):**
- [ ] Read this guide (15 min)
- [ ] Read PRODUCT_EXECUTION_LOG.md (20 min)
- [ ] Schedule daily standups (3pm EST) for all of Week 1

**THIS WEEK:**
- [ ] Complete legal setup (incorporation, EIN, bank account)
- [ ] Confirm all team members received their copy of 90DAY_EXECUTION_PLAN.md
- [ ] First all-hands meeting (explain 90-day mission)
- [ ] Create PRODUCT_EXECUTION_LOG.md tracking spreadsheet (optional: Google Sheets version)

**BEFORE WEEK 2 STARTS:**
- [ ] Confirm tech stack with advisors
- [ ] Brief CTO on MVP scope (6 features max)
- [ ] Confirm sales team has 50 warm intro targets
- [ ] Setup basic monitoring for performance tracking (Week 5+)

---

**Document Status:** ✓ READY TO USE
**Date Created:** January 26, 2026
**Last Updated:** January 26, 2026
**Questions?** Reach out to CEO

---
