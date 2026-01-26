# TAI Erlang Autonomics: Sales Playbook Implementation Guide

**Purpose**: Step-by-step instructions to launch the sales playbook THIS WEEK

**Timeline**: 5 days to launch, 5 months to 3 customers

---

## Day 1 (Monday): Sales Kickoff

**Morning (2 hours)**: Executive alignment
```
Attendees: CEO/Founder, Sales Lead, Product Manager, Customer Success Manager
Agenda:
  1. Playbook overview (15 min)
  2. Target market + ICP review (20 min)
  3. Success metrics + incentives (15 min)
  4. Questions + concerns (10 min)
Outcome: Agreement on 3-customer target, timeline, team assignments
```

**Afternoon (2 hours)**: Detailed planning
```
Sales Lead, AE (if hired), Product Manager
Agenda:
  1. First 50 target companies (identify by hand on LinkedIn)
  2. 3 specific personas per target
  3. Warm intro sources (investors, advisors, network)
  4. Collateral priorities (what to build first)
Outcome: List of 50 targets with contacts
```

---

## Day 2 (Tuesday): Collateral Creation Sprint

**Deliverable 1: Elevator Pitch** (30 min)
```
Create 3 versions (print/reference):
  30-second: "We automate entitlement governance at <50ms with
             cryptographic audit trails, saving 40+ hours/month
             per team and enabling faster product launches."

  60-second: [Above + 1 customer ROI example + your credibility]

  5-minute: [60s + feature demo explanation + next steps]

File: sales/ELEVATOR_PITCH.txt
Owner: Sales Lead
Review: Product Manager (ensure accuracy)
```

**Deliverable 2: One-Pager Template** (1 hour)
```
Create PDF/web one-pager (1 page, front & back):

  Front:
    - TAI logo + headline
    - 3 pain points (pulled from IDEAL_CUSTOMER_PROFILE)
    - 3 solution bullets
    - 1 proof point (customer case study or benchmark)

  Back:
    - "How it works" diagram (3 steps)
    - "By the numbers" ROI example
    - CTA: "Schedule 30-min demo"
    - Contact + QR code to demo booking

Variants: 1 per segment (SaaS / Infrastructure / Fintech)

File: sales/ONEPAGER_[Segment].pdf
Owner: Marketing or designer
Timeline: 4 hours (or use template in SALES_PLAYBOOK.md section 6)
```

**Deliverable 3: ROI Calculator** (30 min)
```
Simple Excel spreadsheet:

  Inputs (customer enters):
    - FTE managing entitlements
    - Annual salary per FTE
    - Manual hours/month
    - Number of SKU rules
    - Compliance audit frequency
    - Violations experienced ($)

  Outputs (auto-calculated):
    - Annual manual cost
    - TAI cost (per tier)
    - Year 1 net savings
    - Payback period (months)
    - 3-year TCO
    - ROI percentage

File: sales/ROI_CALCULATOR.xlsx
Owner: Finance or sales ops
Timeline: 2 hours
```

**Deliverable 4: LinkedIn Outreach Template** (30 min)
```
Create 20 customizable LinkedIn messages:

  Template:
    Subject: Quick question about your entitlement management at [Company]

    Hi [Name],

    I came across [Company] and noticed [specific recent news/scaling signal].
    We built an autonomic entitlement engine for [similar company], and it
    typically saves 40+ hours/month on manual SKU management.

    No pitch—just curious: How are you managing entitlements across your
    [N] customer tiers today? Any pain points?

    [Your name]

File: sales/LINKEDIN_TEMPLATES.txt (or Google Doc)
Owner: Sales Lead
Timeline: 1 hour
```

---

## Day 3 (Wednesday): Sales Infrastructure

**Deliverable 1: Salesforce Setup** (2 hours)
```
Create CRM pipeline:

  Lead stages:
    1. Prospect (target company identified)
    2. Contacted (outreach sent, waiting response)
    3. Qualified (discovery call scheduled)
    4. POC Candidate (ICP Score 75+)
    5. POC Active (30-day engagement)
    6. Negotiation (contract discussion)
    7. Closed Won (signature + go-live)
    8. Closed Lost (reason documented)

  Fields to track:
    - Company name, size, funding stage
    - Contact name, title, email
    - ICP Score (1-10)
    - Est. ACV (tier estimation)
    - Current annual spend on alternative
    - Decision timeline
    - Warm intro source (if applicable)
    - Next action + due date

File: Salesforce instance (sales.tai-autonomics.io)
Owner: Sales Ops (or Sales Lead if no ops hire)
Timeline: 2 hours
```

**Deliverable 2: Demo Environment** (1 hour)
```
Verify staging TAI environment is accessible:

  URL: demo.tai-autonomics.io
  Login: demo@tai-autonomics.io / [password]

  Pre-load environment with:
    - 100+ SKU rules (sample data)
    - 1M+ events (mock transaction history)
    - Live governance dashboard
    - Audit trail query interface
    - Latency benchmarks showing <50ms
    - 3 sample entitlement scenarios

  Document: Demo script (see SALES_PLAYBOOK.md section 5)
  Owner: Engineering or DevOps
  Timeline: 1 hour
```

**Deliverable 3: POC Charter Template** (30 min)
```
Create standardized POC document:

  Sections:
    1. Objective (30-day validation of TAI fit)
    2. Scope (2-3 customer SKU rules, 100+ req/sec test)
    3. Timeline (4 weeks)
    4. Deliverables (Report + ROI analysis + compliance assessment)
    5. Success criteria (All must pass: latency, audit trail, efficiency)
    6. Customer responsibilities (20 hours team time)
    7. TAI responsibilities (40-60 hours)
    8. Next steps if successful (contract + implementation)

File: sales/POC_CHARTER_TEMPLATE.md
Owner: Product Manager
Timeline: 1 hour
```

**Deliverable 4: Messaging Review** (30 min)
```
Review all outreach messages with Product Manager:
  - Elevator pitch accuracy
  - One-pager claims (all backed by data)
  - LinkedIn templates (tone + personalization)
  - ROI calculator assumptions

Approval: Product Manager sign-off on all messaging
Timeline: 30 min call
```

---

## Day 4 (Thursday): First Outreach

**Morning (3 hours)**: LinkedIn outreach launch
```
Sales Lead + AE (if hired):

  1. Connect on LinkedIn with 50 target companies
     - Search criteria: "VP Product", "VP Engineering", "Director of Ops"
     - Company size: 50-200 (Tier 1 SaaS), $100M+ (Infrastructure), Fintech
     - Personalize 1-2 sentences per message

  2. Send initial LinkedIn message to 20 contacts
     - Use LinkedIn_TEMPLATES.txt
     - Personalize based on recent news/scaling signals
     - Set "follow-up in 3 days" reminder

  3. Add all 50 to CRM (Salesforce)
     - Stage: "Contacted" (if LinkedIn sent)
     - Source: "LinkedIn cold"
     - Next action: "Wait for response" (3-5 days)

File: Salesforce
Owner: Sales Lead or AE
Timeline: 3 hours
Outcome: 20 LinkedIn messages sent, 50 targets in CRM
```

**Afternoon (2 hours)**: Email follow-up prep
```
Sales Lead:

  1. Identify warm intro sources for remaining 30 contacts
     - Investors, advisors, mutual connections
     - Reach out to warm intro intermediaries
     - Request 2-3 intros per week

  2. Prepare email follow-up sequence
     - Day 4: Initial cold email (if no LinkedIn response)
     - Day 9: Follow-up email
     - Day 14: Final attempt before moving on

File: sales/EMAIL_SEQUENCES.txt
Owner: Sales Lead
Timeline: 2 hours
Outcome: Warm intro requests sent, email sequences ready
```

---

## Day 5 (Friday): Metrics & Launch Readiness

**Morning (2 hours)**: Metrics dashboard setup
```
Sales Lead + CFO/Finance:

  Create weekly tracking dashboard:
    - Pipeline by stage ($)
    - Qualified leads (count)
    - Discovery calls scheduled (count)
    - POC starts (count)
    - Closed won (count)
    - Sales velocity (avg days from first call to close)
    - Win rate (% of POCs that close)
    - CAC ($ spent / # of customers)

  Cadence: Review every Friday
  Escalation: If pipeline <$1M, alert exec team

File: Google Sheet or Tableau dashboard
Owner: Sales Ops (or CFO if no ops hire)
Timeline: 2 hours
```

**Afternoon (1 hour)**: Launch readiness checklist
```
Run through launch readiness:

  Collateral:
    ✓ Elevator pitch (3 versions)
    ✓ One-pager (PDF)
    ✓ ROI calculator (Excel)
    ✓ LinkedIn templates (20 messages)
    ✓ Demo script (printed)

  Sales Infrastructure:
    ✓ Salesforce live (50 targets loaded)
    ✓ Demo environment accessible
    ✓ POC charter finalized
    ✓ Messaging approved by product

  Outreach:
    ✓ 20 LinkedIn messages sent
    ✓ 50 targets in CRM
    ✓ Warm intro requests sent
    ✓ Email sequences ready

  Team:
    ✓ Sales lead assigned
    ✓ Product manager on-call for demos
    ✓ Engineering ready for POC (Week 2+)
    ✓ Customer success manager assigned

If all ✓: LAUNCH APPROVED
If any ✗: Address this week
```

**Late Friday**: Sales team celebration
```
- Team meeting: "We launched! Here's the playbook."
- Share first 20 LinkedIn messages sent
- Set expectations: "First responses come Monday/Tuesday"
- Celebration: Team dinner/drinks (morale + team building)
```

---

## Week 2-8: Discovery Call Sprint (20 calls, 3-5 POC candidates)

### Weekly Rhythm

**Monday**:
- Consolidate weekend LinkedIn responses
- Schedule discovery calls (target: 5 calls by Friday)
- Send calendar invites with discovery guide

**Tuesday-Friday**:
- Run 1-2 discovery calls per day (30-45 min each)
- Use DISCOVERY_CHECKLIST.md (20 questions)
- Record ICP Score (1-10) in Salesforce
- Assign next action (POC proposal, nurture, or pass)

**Friday**:
- Weekly team sync (15 min)
- Review: Calls completed, ICP scores, POC candidates
- Plan next week outreach

### Targets

```
Week 2-3: 8 calls (target: 2-3 POC-ready leads)
Week 4-5: 8 calls (target: 3-5 POC candidates)
Week 6-8: 4 calls (follow-ups on warm leads)
──────────────────────────────────────────────
Total:    20 calls (target: 3-5 POC candidates by end of Week 8)
```

---

## Week 9-12: POC Launch (3 POCs, 30-day engagements)

### POC #1 Launch (Week 9)

**Pre-POC (Day 1-2)**:
- Kickoff call: Customer + TAI engineering team
- Understand customer's SKU rules + data model
- Setup GCP staging environment
- Assign TAI implementation lead

**POC Execution (Week 1-3)**:
- Day 1-7: Migrate customer rules to TAI format
- Day 8-21: Integration testing + compliance audit trail
- Day 22-28: Production validation + performance benchmarks
- Day 29: Final validation + customer sign-off

**POC Completion (Day 30)**:
- Deliver: POC report (efficiency gains, audit trail proof, ROI analysis)
- Customer confirms: 50%+ efficiency gain ✓
- Customer confirms: Audit trail complete ✓
- Next step: Contract negotiation starts

### POC #2 & #3 Launches (Weeks 10-11)

```
Stagger launches:
  Week 9: POC #1 starts
  Week 10: POC #2 starts (POC #1 at 50% complete)
  Week 11: POC #3 starts (POC #1 finishing, #2 at 50%)

This keeps engineering team at 1.5-2 FTE utilization
while allowing parallel customer progress.
```

---

## Week 13-16: Negotiations & Signatures (3 contracts)

### Contract Process (Per Customer)

**Week 1**: Pricing negotiation
```
- Customer seen POC results: 50%+ efficiency, audit trail ✓
- TAI proposes: Growth tier $100K (with 20% discount = $80K)
- Customer counter: "$60K is our budget"
- TAI response: "Standard pricing is $100K. For Tier 1 customers
               willing to be references, I can offer $80K Year 1,
               $110K Years 2-3."
- Expected: Deal settled in 3-5 days
```

**Week 2**: Contract review & signature
```
- Customer's legal/finance review (3-7 days)
- TAI legal approval (1 day)
- Execute contract
- Setup invoice + billing automation
```

**Expected Timeline**: Negotiations start Week 13, signatures by end Week 16

---

## Week 17-20: Implementation Start (3 × 30-day rollouts)

### Implementation Process (Per Customer)

**Week 1: Planning**
- Customer identifies technical lead
- TAI assigns implementation lead + success manager
- Create implementation Gantt chart
- Define success metrics

**Week 2-3: Configuration & testing**
- Migrate all SKU rules
- Setup tenant isolation
- Test quota enforcement
- Validate receipt ledger

**Week 4: Production cutover**
- Deploy to production (blue-green)
- Monitor 24/7 for issues
- Customer signs off
- Handoff to ops team

---

## Success Metrics (Month 5 = All 3 Go-Live)

```
✅ 3 customers signed contracts
✅ 3 customers implemented (live in production)
✅ 3 reference relationships + testimonials collected
✅ 3 case studies written
✅ $470K ARR committed (Tier 1 with 20% discount)
✅ <45 day average sales cycle
✅ 25-30% win rate on POCs (3 closed / 10 POC candidates)
✅ $30K CAC (3 × $90K sales effort / 3 customers)
```

---

## Month 6-11: Growth Phase (Customers 4-10)

### Tier 2 Launch (7 customers)

**New outreach sources**:
- Warm leads from 3 Tier 1 references (3-5 per customer = 9-15 leads)
- Case studies circulated (5-10 inbound)
- Industry conference presence (AWS Reinvent Q4 = 5-10 leads)

**Accelerated sales cycle**:
- Shorter discovery (case studies as proof)
- Shorter POC (engineering confident)
- Faster close (reference calls)

**Expected timeline**:
```
Month 6: Tier 2 customers 4-5 close
Month 7-8: Tier 2 customers 6-8 close
Month 9-11: Tier 2 customers 9-10 close + early Tier 3
────────────────────────────────────────────────
Month 11: 10 customers live, $1.07M ARR
```

---

## Ongoing: Expansion & Retention

### Expansion Conversations (Start Month 4, execute Month 6+)

**Trigger**: After 3 months on platform, customer has stabilized on core use case

**Approach**:
1. CSM: "We're seeing you've deployed TAI for SKU quotas. We also serve [X]."
2. Demo: 15-min walkthrough of adjacent use case
3. Proposal: "Would add $X/month to your contract"
4. Decision: Usually quick if CSM has done good job

**Target**: 20% ACV expansion per customer in Year 2

---

## FAQ: Playbook Implementation

**Q: What if we don't hit 20 discovery calls by Week 8?**
A: Extend outreach. Add more LinkedIn, warm intro requests, or conference outreach. Reset target to Week 10.

**Q: What if no POC candidates qualify (ICP Score <75)?**
A: Either expand ICP criteria OR change messaging to better attract. Review DISCOVERY_CHECKLIST.md results with product.

**Q: What if POC shows <50% efficiency gain?**
A: Document findings. Either pivot to different use case or pass on customer. This is healthy (avoids bad fits).

**Q: How do we get 3 warm intros to first 50 targets?**
A: Investors + advisors in your cap table. Reach out to each: "I have 30 companies I'd love intros to. Can you help with 2-3?" Most say yes.

**Q: Should we hire a sales AE now or wait?**
A: Founder should lead Tier 1 (build relationships, test messaging). After 3 customers + case studies, hire sales AE for Tier 2.

**Q: What if customer wants to delay after POC success?**
A: Common. Respond: "Budget closed? Let's target Q2 planning. I'll check in October." No pressure. But 90% convert to customer eventually.

---

## Document Reference Guide

**Use these documents in order**:

1. **IDEAL_CUSTOMER_PROFILE.md**
   - Reference: Who to target (specific companies + personas)
   - When: Before outreach
   - Action: Identify first 50 targets

2. **DISCOVERY_CHECKLIST.md**
   - Reference: 20 discovery questions
   - When: During discovery calls
   - Action: Score customer fit (1-10), decide next step

3. **SALES_PLAYBOOK.md**
   - Reference: Complete sales system
   - When: As you execute (demo, pricing, negotiation, etc.)
   - Action: Follow section for each stage

4. **README.md**
   - Reference: Quick start guide
   - When: Onboard new sales team member
   - Action: Shows 3-month timeline + messaging templates

5. **EXECUTIVE_SUMMARY.md**
   - Reference: High-level overview for exec team
   - When: Weekly/monthly reporting
   - Action: Metrics + forecast vs. plan

---

## Files Created (5 total)

```
/Users/sac/ggen/tai-erlang-autonomics/sales/
├── README.md (this file)
├── IDEAL_CUSTOMER_PROFILE.md (who to target)
├── DISCOVERY_CHECKLIST.md (20 questions)
├── SALES_PLAYBOOK.md (complete system)
├── EXECUTIVE_SUMMARY.md (exec overview)
└── IMPLEMENTATION_GUIDE.md (this file - step-by-step)
```

**Total Size**: 84 KB (all documents fit on USB drive, easy to share)

---

## Next Step: Schedule Sales Kickoff

**This Week**: Run Day 1 kickoff meeting
```
Attendees: CEO, Sales Lead, Product Manager, Customer Success Manager
Duration: 2 hours
Goal: Align on playbook, assign owners, confirm 3-customer target

Questions to answer:
  1. Who leads sales (Founder? Hired AE? Both)?
  2. Launch date (Jan 27? Feb 3?)?
  3. First 5 targets (which companies/personas)?
  4. Budget for collateral ($5-10K)?
  5. Success manager assigned?
```

---

**Document Version**: 1.0
**Date**: 2026-01-25
**Owner**: Sales & Product Strategy
**Status**: Ready to execute

**LAUNCH THIS WEEK!**
