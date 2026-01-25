<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Modification (MOD) Handling Policy](#modification-mod-handling-policy)
  - [Table of Contents](#table-of-contents)
  - [Executive Summary](#executive-summary)
  - [MOD Definition & Scope](#mod-definition--scope)
    - [What is a MOD?](#what-is-a-mod)
    - [MOD vs Change Request](#mod-vs-change-request)
  - [MOD Types](#mod-types)
    - [Scope MODs](#scope-mods)
    - [Schedule MODs](#schedule-mods)
    - [Budget MODs](#budget-mods)
    - [Compliance MODs](#compliance-mods)
    - [Technical MODs](#technical-mods)
  - [MOD Process Workflow](#mod-process-workflow)
    - [Step 1: MOD Received](#step-1-mod-received)
    - [Step 2: Impact Analysis](#step-2-impact-analysis)
    - [Step 3: MOD Proposal](#step-3-mod-proposal)
    - [Step 4: Approval & Negotiation](#step-4-approval--negotiation)
    - [Step 5: Implementation & Testing](#step-5-implementation--testing)
    - [Step 6: MOD Closure](#step-6-mod-closure)
  - [Impact Analysis](#impact-analysis)
    - [Technical Impact](#technical-impact)
    - [Schedule Impact](#schedule-impact)
    - [Compliance Impact](#compliance-impact)
    - [Cost Impact](#cost-impact)
  - [MOD Proposal](#mod-proposal)
    - [Proposal Structure](#proposal-structure)
    - [Proposal Submission](#proposal-submission)
  - [Approval & Negotiation](#approval--negotiation)
    - [Negotiation Scenarios](#negotiation-scenarios)
  - [Implementation & Testing](#implementation--testing)
    - [Development Best Practices](#development-best-practices)
    - [Testing Requirements for MODs](#testing-requirements-for-mods)
  - [MOD Closure](#mod-closure)
    - [Closure Verification Checklist](#closure-verification-checklist)
  - [Backlog Management](#backlog-management)
    - [MOD Backlog Structure](#mod-backlog-structure)
    - [Priority Ordering](#priority-ordering)
    - [Resource Constraints](#resource-constraints)
  - [Compliance MOD Process](#compliance-mod-process)
    - [Fast-Track for Government Mandates](#fast-track-for-government-mandates)
  - [Communication Protocol](#communication-protocol)
    - [Stakeholder Notifications](#stakeholder-notifications)
    - [Communication Templates](#communication-templates)
  - [MOD Request Template](#mod-request-template)
  - [Closure Verification Checklist](#closure-verification-checklist-1)
  - [Definition of Done](#definition-of-done)
  - [Receipt Contract](#receipt-contract)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Modification (MOD) Handling Policy

**Classification**: Internal | **Version**: 1.0.0 | **For**: Government Contract Management (TAI 2030)

**Authority**: Director of Contracts & Compliance | **Last Updated**: January 2026 | **Next Review**: April 2026

---

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [MOD Definition & Scope](#mod-definition--scope)
3. [MOD Types](#mod-types)
4. [MOD Process Workflow](#mod-process-workflow)
5. [Impact Analysis](#impact-analysis)
6. [MOD Proposal](#mod-proposal)
7. [Approval & Negotiation](#approval--negotiation)
8. [Implementation & Testing](#implementation--testing)
9. [MOD Closure](#mod-closure)
10. [Backlog Management](#backlog-management)
11. [Compliance MOD Process](#compliance-mod-process)
12. [Communication Protocol](#communication-protocol)
13. [MOD Request Template](#mod-request-template)
14. [Closure Verification Checklist](#closure-verification-checklist)
15. [Definition of Done](#definition-of-done)

---

## Executive Summary

**Modification (MOD)** is a formal change request from a government customer or agency that alters contract scope, schedule, budget, or compliance requirements. This policy defines how ggen receives, analyzes, proposes, implements, and closes MODs while maintaining government contract compliance and transparency.

**Core Principles**:
- Every MOD follows a formal process (no ad-hoc changes)
- MOD priority is customer-driven (government decides what's urgent)
- Impact analysis precedes proposal (cost, schedule, compliance implications determined early)
- MOD proposals are formal documents (submitted to customer for review)
- Compliance MODs are expedited (government-mandated changes prioritized)
- Every MOD is closed with evidence (test results, delivery records, customer sign-off)

**Contract Context**:
- **Customer**: Government agency (GSA, DoD, FedRAMP authority, etc.)
- **Contract**: Fixed-price or time-and-materials, with change order (CO) mechanism
- **MOD Authority**: Customer can request MOD; company proposes cost/schedule impact

---

## MOD Definition & Scope

### What is a MOD?

A **Modification** is a formal, documented change request that affects one or more of:

| Impact Area | Examples | MOD? |
|---|---|---|
| **Scope** | Add new feature, expand marketplace, change SKU catalog | ✅ YES |
| **Schedule** | Extend delivery date, accelerate timeline, change release schedule | ✅ YES |
| **Budget** | Change pricing model, add resources, increase contract value | ✅ YES |
| **Compliance** | Add audit logging, change encryption, new government policy | ✅ YES |
| **Technical** | Change architecture, swap technology, new API design | ✅ YES |
| **People** | Change team lead, add contractor, shift staffing | ✅ YES |
| Internal bug fix (no external impact) | Fix internal race condition | ❌ NO |
| Internal refactoring (no API change) | Restructure code organization | ❌ NO |
| Performance optimization (SLO met) | Improve latency within SLO | ❌ NO |

### MOD vs Change Request

| Aspect | Change Request | MOD |
|---|---|---|
| **Initiated By** | Internal (development team) | External (customer/government) |
| **Process** | Change Control (see change-control.md) | This policy (mod-handling.md) |
| **Authority** | CTO, Engineering Lead | Customer, Government Agency |
| **Approval** | Internal review | Customer approval + contract amendment |
| **Cost Impact** | Usually internal trade-offs | Change order, contract amendment |
| **Timeline** | ~1-2 weeks | ~4-12 weeks (negotiation + implementation) |

---

## MOD Types

### Scope MODs

**Definition**: Customer requests addition, removal, or change to deliverable features.

**Examples**:
- Add support for AWS marketplace (currently GCP-only)
- Expand SKU catalog from 100 to 500 items
- Add multi-tenancy capability
- Change reporting frequency (daily → weekly)

**Impact Determination**:
1. Assess effort (story points, person-weeks)
2. Identify dependencies (what else needs to change?)
3. Model schedule impact (can it fit in current timeline?)
4. Calculate cost (labor + infrastructure + testing)

**Typical Timeline**:
- Request received → 1 week analysis → Proposal submitted → 2-4 weeks negotiation → Implementation (varies)

### Schedule MODs

**Definition**: Customer requests change to delivery dates, milestones, or release cycles.

**Examples**:
- Accelerate delivery by 3 months
- Extend delivery by 6 months
- Move release from quarterly to monthly
- Change UAT window

**Impact Determination**:
1. Assess current schedule criticality (can we accelerate?)
2. Identify resource constraints (do we need to hire?)
3. Calculate cost (overtime, contractor ramp, SLA penalties?)
4. Model risk (what quality aspects suffer if accelerated?)

**Typical Timeline**:
- Request → 3-5 days analysis → Proposal → 1-2 weeks negotiation → Re-baseline schedule

### Budget MODs

**Definition**: Customer requests change to contract value, labor rates, or resource allocation.

**Examples**:
- Reduce budget by 10% (scope-reducing)
- Increase budget to hire additional engineers
- Change labor rates (contract renewal)
- Shift budget from labor to contractor services

**Impact Determination**:
1. Assess resource requirements (current team + proposed)
2. Identify quality/schedule trade-offs (if budget decreases)
3. Model ROI (if budget increases, what's new capability?)

**Typical Timeline**:
- Request → 5 days analysis → Proposal → 1 week negotiation → Update budget baseline

### Compliance MODs

**Definition**: Government-mandated changes to meet regulatory requirements or new policies.

**Examples**:
- Implement new NIST 800-53 control (security baseline change)
- Add FedRAMP requirement (documentation, assessment)
- Enforce new data retention policy
- Update incident response procedures

**Impact Determination**:
1. Assess compliance requirement (is it mandatory?)
2. Identify implementation effort (controls, testing, documentation)
3. Model schedule impact (how does this fit current timeline?)
4. Calculate cost (new controls, testing, training)

**Typical Timeline**:
- Government mandate → Immediate analysis (same day) → Expedited proposal (1-2 days) → Emergency implementation (varies)
- **Compliance MODs bypass standard negotiation timelines** (must implement or lose contract)

### Technical MODs

**Definition**: Customer requests change to architecture, design, or implementation approach.

**Examples**:
- Switch from GCP to Azure
- Change database from Firestore to PostgreSQL
- Implement different authentication mechanism
- Change API design (REST → GraphQL)

**Impact Determination**:
1. Assess technical feasibility (is it possible with current architecture?)
2. Identify rework effort (what code must be rewritten?)
3. Model timeline impact (how long to redesign + reimplement?)
4. Calculate cost (engineering effort + testing)

**Typical Timeline**:
- Request → 1-2 weeks technical analysis → Proposal → 2-4 weeks negotiation → Implementation (major rework, often 3-6 months)

---

## MOD Process Workflow

### Step 1: MOD Received

**Trigger**: Customer submits MOD via:
- Formal written request (email, letter, form)
- Contract amendment notice
- Government mandate / regulation

**Activities**:
1. Log MOD in MOD Register (`/ggen/receipts/mods/MOD-REGISTER.json`)
2. Assign MOD ID: `MOD-YYYYMMDD-###`
3. Assign MOD Shepherd (primary contact for this MOD)
4. Schedule kickoff meeting with customer
5. Create MOD record file: `/ggen/receipts/mods/MOD-YYYYMMDD-###.json`

**Outputs**:
- MOD Record created (JSON)
- MOD Shepherd assigned + notified
- Customer kickoff scheduled (within 2 business days for ROUTINE, same day for EMERGENCY)

**Example MOD Record** (`/ggen/receipts/mods/MOD-20260125-001.json`):
```json
{
  "modId": "MOD-20260125-001",
  "type": "Scope MOD",
  "title": "Add AWS Marketplace Support",
  "customer": "GSA",
  "contactPerson": "John Smith <john@gsa.gov>",
  "receivedDate": "2026-01-20",
  "urgency": "PRIORITY",
  "description": "Customer requests ggen to support AWS Marketplace in addition to current GCP support. This enables GSA to offer multi-cloud deployments.",
  "customerObjective": "Reduce vendor lock-in, offer customers choice of cloud provider",
  "contractReference": "TAI-2030-VA-2026-001 Section 5.2.1",
  "state": "RECEIVED",
  "shepherd": "alice@example.com",
  "createdAt": "2026-01-20T10:15:00Z"
}
```

---

### Step 2: Impact Analysis

**Trigger**: MOD Shepherd initiates analysis (within 2 days of receipt).

**Team Involvement**:
- **MOD Shepherd** (coordinator, owns MOD end-to-end)
- **Engineering Lead** (technical feasibility, effort estimate)
- **Compliance Officer** (compliance impact assessment)
- **Finance** (cost estimate)
- **Project Manager** (schedule impact)
- **Product Manager** (scope clarification)

**Analysis Activities**:

**2.1 Scope Clarification**
- [ ] What exactly is the customer asking for?
- [ ] What are out-of-scope boundaries?
- [ ] Are there dependencies on other work?
- [ ] Document clarifications in MOD record

**2.2 Technical Feasibility**
- [ ] Is the change technically feasible given current architecture?
- [ ] What code needs to be written/modified?
- [ ] What components are affected?
- [ ] Are there design alternatives? (document pros/cons)
- [ ] Document technical analysis in MOD record

**2.3 Effort Estimation**
- [ ] Break down work into tasks (per component)
- [ ] Estimate effort per task (in story points or person-weeks)
- [ ] Identify parallel vs sequential work
- [ ] Apply risk buffer (20% for unknowns)
- [ ] Document effort estimate in MOD record

**Example Effort Breakdown** (AWS Marketplace MOD):
```
AWS IAM Integration:           8 story points
AWS S3 Catalog Sync:           13 story points
AWS CloudFormation Templates:  5 story points
Testing (unit + integration):  8 story points
Documentation:                 3 story points
Deployment + UAT:              5 story points
Risk Buffer (20%):             8.4 story points
─────────────────────────────────────────────
Total:                         50.4 story points ≈ 12-14 person-weeks
```

**2.4 Schedule Impact**
- [ ] Current project timeline: [baseline schedule]
- [ ] MOD implementation duration: [estimated weeks]
- [ ] Can MOD fit in current timeline? (yes/no, explain)
- [ ] If yes: critical path impact? (delayed milestones?)
- [ ] If no: what would need to change? (add resources? extend deadline? reduce scope?)
- [ ] Document schedule analysis in MOD record

**2.5 Compliance Impact**
- [ ] Does MOD affect government contract requirements?
- [ ] Does MOD require additional compliance controls?
- [ ] Does MOD trigger FedRAMP re-assessment?
- [ ] Does MOD require government notification/approval?
- [ ] Document compliance analysis in MOD record

**2.6 Cost Estimation**
- [ ] Engineering cost (effort × loaded hourly rate)
- [ ] Infrastructure cost (if new capabilities = new cloud resources)
- [ ] Contractor costs (if skills not available internally)
- [ ] Testing/QA cost
- [ ] Documentation cost
- [ ] Travel/meetings cost
- [ ] Apply overhead (facilities, benefits, management)
- [ ] Apply profit margin (per contract terms)
- [ ] Document cost estimate in MOD record

**Outputs**:
- Impact Analysis Report (detailed, 5-20 pages)
- Effort estimate (story points + person-weeks)
- Schedule impact (timeline, critical path)
- Cost estimate (fully-loaded, includes overhead + profit)
- Risk assessment (technical, compliance, schedule risks)
- MOD record updated with analysis findings

**Analysis SLO**:
- ROUTINE: 5 business days
- PRIORITY: 3 business days
- EXPEDITED: 1 business day
- EMERGENCY (Compliance MOD): 4-8 hours

---

### Step 3: MOD Proposal

**Trigger**: Impact analysis complete; MOD Shepherd prepares proposal for customer review.

**Proposal Contents**:

1. **Executive Summary** (1 page)
   - What customer asked for
   - What company proposes to deliver
   - Cost + schedule impact
   - Key assumptions

2. **Scope Statement** (1-2 pages)
   - Detailed description of deliverables
   - What IS included
   - What IS NOT included (scope boundaries)
   - Success criteria

3. **Technical Approach** (2-5 pages)
   - Architecture / design changes required
   - Implementation plan (stages)
   - Design alternatives considered + rationale
   - Technology choices justified

4. **Effort & Resource Plan** (1-2 pages)
   - Work breakdown structure (WBS)
   - Effort estimate per task (with risk buffer)
   - Resources required (team composition, skills)
   - Staffing timeline (ramp-up schedule)

5. **Schedule** (1 page + Gantt chart)
   - MOD implementation duration (weeks)
   - Key milestones + delivery dates
   - Critical path items
   - Integration with existing project timeline
   - Dependencies on other work

6. **Cost Estimate** (1 page)
   - Direct costs (engineering, infrastructure, testing)
   - Indirect costs (overhead, management, G&A)
   - Profit margin
   - Total contract value change
   - Payment schedule (if phased)

7. **Risk Assessment** (1 page)
   - Technical risks (feasibility unknowns, tech obsolescence)
   - Schedule risks (dependencies, resource availability)
   - Compliance risks (new government requirements)
   - Mitigation strategies for each risk
   - Contingency buffer applied

8. **Assumptions & Constraints** (0.5 page)
   - Customer will provide X, Y, Z
   - Existing infrastructure available
   - No changes to other contract terms
   - Government approval timeline

9. **Compliance Statement** (0.5-1 page)
   - FedRAMP impact (if any)
   - Security control changes (if any)
   - Audit trail impact (if any)
   - Government notification required? (yes/no)

10. **Customer Approval Section** (signature block)
    - Customer acknowledges proposal terms
    - Customer authorizes implementation
    - Customer signature + date

**Example Proposal Excerpt** (AWS Marketplace MOD):
```
MOD PROPOSAL: AWS Marketplace Support

Executive Summary
─────────────────
GSA requests ggen to support AWS Marketplace deployment in addition to current GCP
support. This MOD adds AWS CloudFormation templates, AWS IAM integration, and AWS S3
catalog sync.

Proposal:
- Add 50 story points of AWS-specific development
- Estimated cost: $185,000 (labor + infrastructure)
- Schedule impact: +12 weeks (can run parallel to other work, no critical path delay)
- Compliance: No new FedRAMP requirements (AWS is pre-authorized for GSA)

Deliverables:
✓ AWS CloudFormation templates (IaC for ggen deployment)
✓ AWS IAM OIDC integration (federated identity)
✓ AWS S3 catalog sync (marketplace data source)
✓ Testing: unit + integration + staging deployment
✓ Documentation: AWS deployment guide, troubleshooting
✓ UAT support: Customer testing environment + engineering support

Timeline:
- Week 1-2: Design + requirements review with customer
- Week 3-8: Implementation (AWS integration + testing)
- Week 9-10: UAT staging environment
- Week 11-12: Production deployment + cutover
- Delivery date: [Date], 12 weeks from MOD approval

Cost:
- Engineering (12 person-weeks @ $8,500/week): $102,000
- Infrastructure (AWS test/staging): $15,000
- Testing/QA: $20,000
- Documentation: $12,000
- Overhead (40%): $49,600
- Profit margin (15% on labor): $24,000
───────────────────────────────────────────────────
Total MOD Value: $185,600
```

**Outputs**:
- MOD Proposal document (PDF)
- Proposal submitted to customer
- MOD record updated with proposal metadata
- Customer review SLO: 10 business days for decision

---

### Step 4: Approval & Negotiation

**Trigger**: Proposal submitted to customer; customer reviews and responds.

**Possible Outcomes**:

**4A. Customer Approves as-is** ✅
- Customer signs proposal approval section
- MOD moves to APPROVED state
- Proceed to Implementation (Step 5)

**4B. Customer Requests Changes** 🔄
- Customer provides feedback/revision requests
- Company assesses impact (scope/cost/schedule)
- Company submits revised proposal
- Negotiation loop continues until agreement

**4C. Customer Rejects** ❌
- Customer declines MOD
- MOD closed without implementation
- Reasons documented in MOD record

**Negotiation Rules**:
1. No MOD changes proceed to implementation without formal customer approval
2. Customer approval must be documented (signed proposal or formal letter)
3. Approval must include acknowledgment of cost/schedule/scope terms
4. Approval triggers contract amendment (if cost/schedule affected)

**Typical Negotiation Timeline**:
- Proposal submitted → 10-day customer review → Feedback received → 3-5 day revision → Resubmitted → Approved

**Total Timeline** (ROUTINE MOD): ~4 weeks from receipt to approval

**MOD Record Update** (upon approval):
```json
{
  "modId": "MOD-20260125-001",
  "state": "APPROVED",
  "approval": {
    "approvedBy": "John Smith <john@gsa.gov>",
    "approvalDate": "2026-02-14",
    "proposalVersion": "Rev B",
    "contractAmendmentRequired": true,
    "contractAmendmentId": "CA-2026-001",
    "costApproved": 185600,
    "scheduleApproved": "2026-03-21 (12 weeks)"
  }
}
```

---

### Step 5: Implementation & Testing

**Trigger**: MOD approved; company begins development (after contract amendment executed, if required).

**Implementation Activities**:

**5.1 Detailed Design**
- [ ] Finalize technical design (architecture, APIs, data model)
- [ ] Design review with engineering team
- [ ] Customer input/approval on design (if specified in MOD)

**5.2 Development**
- [ ] Implement per work breakdown structure
- [ ] Regular code review
- [ ] Unit tests + integration tests (Chicago TDD pattern)
- [ ] Continuous integration pipeline validates

**5.3 Testing**
- [ ] Unit test coverage ≥ 80% for new code
- [ ] Integration tests for AWS-specific flows
- [ ] Staging environment deployment (mirrors production)
- [ ] Customer UAT (if specified in MOD proposal)
- [ ] Security review (if compliance implications)
- [ ] Performance testing (if critical path affected)

**5.4 Documentation**
- [ ] Design documentation (architecture, APIs, design decisions)
- [ ] User documentation (AWS deployment guide, troubleshooting)
- [ ] Operations documentation (monitoring, alerting, runbooks)
- [ ] Knowledge transfer (if handoff to operations)

**5.5 Deployment**
- [ ] Production deployment (canary: 5% → 25% → 50% → 100%)
- [ ] SLI monitoring + post-deployment checks
- [ ] Customer notification of production go-live

**MOD Implementation Record** (updated continuously):
```json
{
  "modId": "MOD-20260125-001",
  "state": "IN_PROGRESS",
  "implementationTeam": [
    {"name": "Alice Engineer", "role": "Tech Lead", "allocation": "100%"},
    {"name": "Bob Developer", "role": "AWS Developer", "allocation": "80%"},
    {"name": "Carol QA", "role": "Test Engineer", "allocation": "60%"}
  ],
  "milestones": [
    {
      "name": "Design Review",
      "plannedDate": "2026-02-21",
      "actualDate": "2026-02-21",
      "status": "COMPLETE"
    },
    {
      "name": "Development Complete",
      "plannedDate": "2026-03-07",
      "actualDate": null,
      "status": "IN_PROGRESS"
    },
    {
      "name": "Testing Complete",
      "plannedDate": "2026-03-14",
      "actualDate": null,
      "status": "PENDING"
    },
    {
      "name": "Production Deployment",
      "plannedDate": "2026-03-21",
      "actualDate": null,
      "status": "PENDING"
    }
  ]
}
```

---

### Step 6: MOD Closure

**Trigger**: Implementation complete; deliverables verified; customer accepts.

**Closure Activities**:

**6.1 Verification of Deliverables**
- [ ] All scope items delivered (per MOD proposal)
- [ ] All tests passed (unit, integration, customer UAT)
- [ ] All documentation provided
- [ ] Production deployment confirmed
- [ ] Customer confirms receipt of deliverables

**6.2 Customer Sign-Off**
- [ ] Customer acknowledges deliverables are complete
- [ ] Customer signs MOD Closure Form
- [ ] Customer provides formal acceptance (email or written)
- [ ] Date recorded in MOD record

**6.3 Financial Closure**
- [ ] All invoices issued (per payment schedule in MOD proposal)
- [ ] All payments received (or on AR aging report)
- [ ] Contract value updated (if cost change)
- [ ] Billings recorded in accounting system

**6.4 Archive**
- [ ] MOD record marked CLOSED
- [ ] All MOD artifacts collected + archived (/ggen/receipts/mods/MOD-*/documents/)
- [ ] Lessons learned documented (what went well, what to improve)
- [ ] MOD removed from active backlog (if applicable)

**MOD Closure Record**:
```json
{
  "modId": "MOD-20260125-001",
  "state": "CLOSED",
  "closure": {
    "closedDate": "2026-03-21",
    "closedBy": "MOD Shepherd <alice@example.com>",
    "customerAcceptance": {
      "accepted": true,
      "acceptedBy": "John Smith <john@gsa.gov>",
      "acceptanceDate": "2026-03-21"
    },
    "deliverables": [
      {
        "name": "AWS CloudFormation Templates",
        "status": "DELIVERED",
        "deliveryDate": "2026-03-21"
      },
      {
        "name": "AWS IAM Integration",
        "status": "DELIVERED",
        "deliveryDate": "2026-03-21"
      },
      {
        "name": "Testing Evidence",
        "status": "DELIVERED",
        "testCoveragePercent": 87,
        "testPassRate": 100
      },
      {
        "name": "Documentation",
        "status": "DELIVERED",
        "documentationPages": 42
      }
    ],
    "financialClosure": {
      "totalCostActual": 181500,
      "budgetedCost": 185600,
      "variance": -4100,
      "allInvoicesPaid": true
    },
    "lessonsLearned": {
      "whatWentWell": ["AWS IAM design was straightforward", "Customer collaboration was excellent", "Testing caught 3 edge cases early"],
      "whatToImprove": ["Underestimated CloudFormation template complexity", "Should have allocated more QA time for staging env"],
      "suggestionsForFuture": ["Create AWS module template library for future MODs", "Pre-stage AWS IAM design decisions"]
    }
  }
}
```

---

## Impact Analysis

### Technical Impact

| Factor | Assessment | Example |
|---|---|---|
| **Code Complexity** | How much code to write? | AWS MOD: 50 story points → ~2,000 lines |
| **Dependencies** | What else needs to change? | Add AWS IAM → Update authentication layer |
| **Risk** | Unknown complexity, tech risk? | AWS CloudFormation syntax learning curve |
| **Rework** | What existing code must change? | Marketplace adapter interface |
| **Testing** | New test scenarios? | AWS credential rotation, multi-cloud failover |

### Schedule Impact

| Factor | Assessment | Example |
|---|---|---|
| **Duration** | How long to implement? | AWS MOD: 12 weeks |
| **Critical Path** | Does it delay other milestones? | No (can run parallel) |
| **Dependencies** | What must happen first? | Design review, AWS sandbox access |
| **Skill Gaps** | Do we need to train/hire? | AWS expertise (can use existing) |
| **Parallelization** | What can run in parallel? | AWS IAM + CloudFormation can overlap |

### Compliance Impact

| Factor | Assessment | Example |
|---|---|---|
| **Government Requirements** | New NIST controls? | AWS is already FedRAMP authorized |
| **Audit Trail** | Does it affect audit logging? | No (AWS accounts isolated) |
| **Data Security** | Data classification change? | No (same classification) |
| **Notification** | Does gov need to know? | No (AWS already approved) |
| **Re-Assessment** | FedRAMP re-assessment needed? | No (within scope of existing ATO) |

### Cost Impact

| Category | Estimate | Example |
|---|---|---|
| **Engineering Labor** | Person-weeks × rate | 12 weeks × $8,500 = $102,000 |
| **Infrastructure** | Test/staging cloud costs | AWS sandbox + staging: $15,000 |
| **Contractor** | Skills not available internally | AWS specialist contractor: $0 (using internal talent) |
| **Testing/QA** | Additional QA effort | Complex integration testing: $20,000 |
| **Documentation** | Documentation effort | AWS deployment guides: $12,000 |
| **Overhead** | Facilities, management, benefits | 40% overhead = $49,600 |
| **Profit** | Company margin | 15% = $24,000 |

---

## MOD Proposal

### Proposal Structure

MOD proposals follow a standard template to enable consistent customer review and approval.

**See MOD Request Template section below for detailed template.**

### Proposal Submission

Proposals are submitted via:
1. **Formal letter** (signed by Director of Contracts or designated authority)
2. **Email with proposal document** (PDF, tracked via MOD record)
3. **In-person meeting** (for high-value MODs, with walkthrough)

**Proposal Approval SLO**:
- Customer has 10 business days to review and respond
- Company follows up if no response by day 8
- Negotiation back-and-forth continues until agreement

---

## Approval & Negotiation

### Negotiation Scenarios

**Scenario 1: Customer Requests Cost Reduction**
- Customer: "Can you reduce cost to $150,000?"
- Company assessment:
  - Original proposal: 12 person-weeks @ $185,600
  - 20% cost reduction: Remove non-critical features (AWS CloudWatch integration)
  - New estimate: 10 person-weeks @ $155,000 ✅ possible
- Company response: Revised proposal with reduced scope
- Customer approval: MOD proceeds with revised scope

**Scenario 2: Customer Requests Faster Delivery**
- Customer: "We need this in 6 weeks, not 12"
- Company assessment:
  - 2x acceleration requires 2x resources (hire contractor or shift staff)
  - Additional cost: ~$50,000 for accelerated hiring + overtime
  - New estimate: 10 weeks (parallel work) + $50k cost increase = $235,600
- Company response: Proposal with accelerated schedule + cost increase
- Customer decision: Approve acceleration OR accept standard 12-week timeline

**Scenario 3: Customer Requests More Scope**
- Customer: "Can you also add Azure support?"
- Company assessment:
  - Original AWS MOD: 12 weeks, $185,600
  - Azure equivalent effort: Additional 10 weeks, $160,000
  - Combined AWS+Azure: 18 weeks (parallel possible: 15 weeks), $340,000
- Company response: Separate MOD for Azure, or revised MOD with combined scope
- Customer decision: Proceed with AWS only, defer Azure, or approve combined effort

**Negotiation Best Practices**:
1. Always provide detailed rationale for estimates
2. Offer alternatives (speed vs cost vs scope trade-offs)
3. Document assumptions (customer to provide X, company to provide Y)
4. Never commit to proposals without analysis
5. Escalate to management if customer requests seem unreasonable (e.g., 50% cost reduction + 2x acceleration)

---

## Implementation & Testing

### Development Best Practices

- **Chicago TDD**: Write tests first, then implementation
- **Continuous Integration**: Every commit tested, fails fast
- **Code Review**: All MOD code reviewed before merge (peer + architect)
- **Architecture Review**: Design review with engineering leads before coding

### Testing Requirements for MODs

| Test Type | Requirement | Example |
|---|---|---|
| **Unit Tests** | >80% code coverage | AWS IAM auth: 12 test cases |
| **Integration Tests** | End-to-end workflows | Deploy ggen on AWS, verify marketplace sync |
| **Staging UAT** | Customer testing environment | Customer tests AWS deployment, validates SKU sync |
| **Production Smoke Test** | Basic sanity check on production | Deploy 5% canary, verify SLIs healthy |
| **Customer UAT** | Customer acceptance testing (if specified) | Customer tests 10 use cases, signs off |

---

## MOD Closure

### Closure Verification Checklist

Before marking MOD as CLOSED, verify:

- [ ] **All Deliverables Delivered**
  - [ ] Code merged to production
  - [ ] Documentation provided (AWS deployment guide, API docs, runbooks)
  - [ ] Tests completed + passing (unit, integration, customer UAT)
  - [ ] Training provided (if specified in MOD)

- [ ] **Customer Acceptance**
  - [ ] Customer has tested deliverables
  - [ ] Customer accepts deliverables (written sign-off)
  - [ ] No open defects or issues
  - [ ] Customer satisfied with quality

- [ ] **Financial Closure**
  - [ ] All invoices issued per payment schedule
  - [ ] All payments received or in AR aging
  - [ ] Budget variance documented (actual vs proposed cost)
  - [ ] Any unused budget returned to customer (if applicable)

- [ ] **Compliance Closure**
  - [ ] All security reviews completed
  - [ ] Audit trail updated (if applicable)
  - [ ] Government notification sent (if required)
  - [ ] New compliance controls implemented (if required by MOD)

- [ ] **Archive**
  - [ ] MOD record marked CLOSED
  - [ ] All artifacts collected + stored (/ggen/receipts/mods/)
  - [ ] Lessons learned documented
  - [ ] Team debriefing completed
  - [ ] MOD removed from active backlog

---

## Backlog Management

### MOD Backlog Structure

```
Active MODs (in progress):
  ├── MOD-20260125-001 [AWS Marketplace] - IN_PROGRESS (Design phase)
  ├── MOD-20260110-002 [Compliance Audit] - APPROVED (Awaiting implementation)
  └── MOD-20260115-003 [Performance SLA] - PROPOSED (Awaiting review)

Approved MODs (ready to implement):
  ├── MOD-20250830-005 [Multi-tenancy] - APPROVED (Start date: Feb 2026)
  └── MOD-20250820-004 [Azure Support] - APPROVED (Start date: Mar 2026)

Proposed MODs (awaiting customer decision):
  ├── MOD-20260105-006 [Mobile App] - PROPOSED (Customer review ongoing)
  └── MOD-20260112-007 [API v2] - PROPOSED (Awaiting customer feedback)

Closed MODs (completed):
  ├── MOD-20240615-001 [GCP Marketplace] - CLOSED
  ├── MOD-20240901-002 [Firestore Migration] - CLOSED
  └── MOD-20250611-003 [FedRAMP Controls] - CLOSED
```

### Priority Ordering

MODs are prioritized by:

1. **Compliance MODs** (EMERGENCY/EXPEDITED)
   - Government-mandated changes
   - Cannot refuse; must implement or lose contract
   - Highest priority, interrupt other work if necessary

2. **Critical MODs** (PRIORITY)
   - Affects contract performance, customer satisfaction, revenue
   - Typically approved scope changes from customer

3. **Routine MODs** (ROUTINE)
   - Normal feature requests, enhancements
   - Planned into standard sprint planning

### Resource Constraints

MOD execution is constrained by:

- **Engineering capacity**: Team has X story points/sprint available
- **Other commitments**: Ongoing operational support, maintenance, planned releases
- **Skill availability**: Some MODs require specialized skills (AWS, compliance, etc.)

**Resource Planning Example**:
```
Team capacity: 100 story points/2-week sprint

Current commitments:
- Operational support:        20 story points
- Planned features (Q1):       40 story points
- Technical debt:              15 story points
─────────────────────────────────────────
Allocated:                      75 story points

Available for MODs:             25 story points/sprint

Active MODs:
- AWS Marketplace (MOD-001):    10 sp/sprint (weeks 1-12)
- Compliance Audit (MOD-002):    5 sp/sprint (weeks 1-2)
- Performance SLA (MOD-003):    10 sp/sprint (weeks 3-8)

Status: OVER CAPACITY by 0 sp (AWS MOD can run parallel to ops; compressed sprint Q1)
```

---

## Compliance MOD Process

### Fast-Track for Government Mandates

Compliance MODs (government-mandated changes) follow expedited process:

**Timeline**:
- Government mandate received → Immediate analysis (same-day, 4 hours)
- Expedited proposal (next business day)
- Emergency implementation approval (skip normal negotiation if mandatory)
- Compressed implementation (prioritized, resource surge)
- Fast delivery (weeks, not months)

**Process**:
```
Government Mandate Received
  ↓ (Compliance Officer notified)
Immediate Impact Assessment (4 hours)
  ├─ Is it mandatory? (YES → proceed; NO → treat as normal MOD)
  ├─ What's the deadline? (e.g., 30 days, 90 days, 6 months)
  └─ What's the implementation effort?
  ↓
Expedited Proposal (24 hours)
  ├─ Minimal negotiation (mandatory, no trade-offs)
  ├─ Cost estimate provided
  └─ Implementation plan provided
  ↓
Government Approval (assumed, or formal approval if cost-impacting)
  ↓
Compressed Implementation
  ├─ Resource surge (reassign staff, hire contractors if necessary)
  ├─ Accelerated schedule (timeline-driven, not quality-driven)
  ├─ Fast testing (focus on compliance control verification)
  └─ Go-live (often on fixed government deadline)
  ↓
Compliance Closure
  ├─ Verify control implemented + tested
  ├─ Provide evidence for government assessment
  └─ Update contract compliance artifacts
```

**Examples of Compliance MODs**:
1. **New NIST Control** (e.g., "SC-7: Boundary Protection")
   - Government: "Implement DMZ with IDS/IPS"
   - Mandate deadline: 60 days
   - Company: 30-day implementation + 30-day testing

2. **Data Residency Requirement**
   - Government: "All PII must reside in [specific region]"
   - Mandate deadline: 90 days
   - Company: Database migration + replication

3. **Incident Response Update**
   - Government: "New incident reporting frequency (24 hours, not 72 hours)"
   - Mandate deadline: 30 days
   - Company: Procedure update + staff training

---

## Communication Protocol

### Stakeholder Notifications

| Event | Channels | Recipients | Template |
|---|---|---|---|
| **MOD Received** | Email + Project Dashboard | Leadership, Finance, Engineering | "New MOD Received" |
| **Impact Analysis Started** | Email | MOD Shepherd + Analysis Team | "MOD Impact Analysis Underway" |
| **Proposal Ready** | Email + Meeting | Customer, Leadership, Finance | "MOD Proposal Ready for Review" |
| **Proposal Submitted** | Email + Formal Letter | Customer (Government) | "MOD Proposal Submission" |
| **Customer Feedback** | Email + Meeting | MOD Shepherd, Negotiations Team | "Customer Feedback on Proposal" |
| **MOD Approved** | Email + Dashboard Update | All Stakeholders | "MOD Approved - Implementation Starting" |
| **Implementation Status** | Weekly email (Monday) | Leadership, Customer (if requested) | "MOD Status Update" |
| **Milestone Completed** | Email | MOD Shepherd, Customer | "MOD Milestone Complete" |
| **Testing Underway** | Email (weekly) | QA, Customer | "MOD Testing Update" |
| **Production Ready** | Email + Meeting | All Stakeholders, Customer | "MOD Ready for Production Deployment" |
| **Deployment Complete** | Email | All Stakeholders, Customer | "MOD Deployed to Production" |
| **Customer UAT Complete** | Email | All Stakeholders, Customer | "MOD UAT Complete - Ready for Closure" |
| **MOD Closure** | Email + Meeting | All Stakeholders, Finance | "MOD Officially Closed" |

### Communication Templates

**"New MOD Received"**:
```
Subject: New MOD Received - [Title]
To: leadership@example.com, finance@example.com

MOD ID: MOD-20260125-001
Title: [MOD Title]
Customer: GSA
Received: 2026-01-20

Description: [Customer's MOD request, one paragraph]

Initial Assessment:
- Type: Scope MOD
- Urgency: PRIORITY
- Preliminary Impact: [High-level estimate, e.g., "10-15 weeks, $150-200k"]

Next Steps:
- MOD Shepherd assigned: [Name]
- Impact analysis scheduled: [Date]
- Customer meeting: [Date/Time]

Questions? Contact [MOD Shepherd].
```

**"MOD Proposal Submitted"**:
```
Subject: MOD Proposal Submitted - [Title]
To: customer.contact@gsa.gov
CC: leadership@example.com

MOD ID: MOD-20260125-001
Title: AWS Marketplace Support

Dear [Customer Contact],

We are pleased to submit our proposal for the AWS Marketplace Support MOD.

Proposal Summary:
- Deliverables: [AWS CloudFormation, AWS IAM integration, AWS S3 sync]
- Schedule: 12 weeks from MOD approval
- Cost: $185,600
- Key Assumptions: [AWS sandbox access provided, no additional security review needed]

Proposal document is attached.

We request your approval or feedback by [Date - 10 business days].

Please contact [MOD Shepherd] with any questions.

Best regards,
[Company Name]
```

**"MOD Implementation Status"** (weekly update):
```
Subject: MOD-20260125-001 Status Update - Week 4
To: customer@gsa.gov, leadership@example.com

MOD: AWS Marketplace Support
Status: IN_PROGRESS (Week 4 of 12)

Milestones:
✅ Design Review (completed 2/21)
✅ Development Started (2/24)
🔄 Development Ongoing (target 3/7)
⏳ Testing (target 3/14)
⏳ Production Deployment (target 3/21)

Key Metrics:
- Effort to date: 8 person-weeks (on track)
- Code commits: 34
- Test coverage: 82%
- Issues: 0 blockers, 2 minor (tracking to resolve)

Risks:
- None currently

Next Week:
- Complete AWS CloudFormation templates (critical path)
- Begin integration testing

Questions? Contact [MOD Shepherd].
```

---

## MOD Request Template

```markdown
# MOD REQUEST FORM

## Metadata
- **MOD ID**: [Auto-generated]
- **Submitted By**: [Customer Name + Organization]
- **Submission Date**: [Date]
- **MOD Type**: [Scope / Schedule / Budget / Compliance / Technical]
- **Urgency**: [ROUTINE / PRIORITY / EXPEDITED / EMERGENCY]

## MOD Description

### What is the customer requesting?
[Detailed description of the change, requirement, or mandate]

### Why does the customer need this?
[Business justification, regulatory requirement, competitive pressure, etc.]

### When does the customer need this?
[Target delivery date, deadline constraints]

### Success Criteria
[How will customer know MOD is successful? What's the acceptance criteria?]

## Scope & Boundaries

### What IS included in this MOD?
[Specific deliverables, features, requirements]

### What IS NOT included?
[Clarify out-of-scope boundaries]

## Current Contract Reference

### Existing Contract Terms
- **Contract ID**: [e.g., TAI-2030-VA-2026-001]
- **Current Scope**: [Brief summary of current contract]
- **Affected Sections**: [Contract sections impacted by MOD]

## Company Response (filled by MOD Shepherd)

### Initial Assessment
- [ ] Scope clearly defined
- [ ] Customer objective understood
- [ ] Feasibility assessed (possible / complex / not feasible)
- [ ] Preliminary impact: [Effort estimate, cost estimate, schedule impact]

### Impact Analysis Complete
- [ ] Technical feasibility: [Assessment + design approach]
- [ ] Effort estimate: [Person-weeks + story points]
- [ ] Schedule impact: [Can fit in timeline? Duration?]
- [ ] Cost estimate: [Fully loaded cost]
- [ ] Compliance impact: [New controls? Government notification?]
- [ ] Risk assessment: [Technical, schedule, compliance risks]

### Proposal Prepared
- [ ] Executive summary written
- [ ] Scope statement finalized
- [ ] Technical approach documented
- [ ] Effort & resource plan detailed
- [ ] Schedule & Gantt chart prepared
- [ ] Cost estimate provided
- [ ] Risk mitigation strategies included
- [ ] Assumptions & constraints listed
- [ ] Compliance statement completed

### Proposal Submitted
- [ ] Date submitted: [Date]
- [ ] Submitted to: [Customer contact]
- [ ] Document: [Link to proposal PDF]
- [ ] Approval SLO: [Target approval date]

### MOD Approved / Rejected
- [ ] Decision: [APPROVED / REJECTED]
- [ ] Approved by: [Customer name + signature]
- [ ] Approval date: [Date]
- [ ] Contract amendment: [Executed / Pending]

### Implementation Status
- [ ] Design review completed: [Date]
- [ ] Development in progress: [% complete]
- [ ] Testing in progress: [% complete]
- [ ] Customer UAT scheduled: [Date]
- [ ] Production deployment scheduled: [Date]

### MOD Closed
- [ ] Closure date: [Date]
- [ ] Customer acceptance: [Yes / No]
- [ ] All deliverables delivered: [Yes / No]
- [ ] All invoices paid: [Yes / No]
- [ ] Lessons learned documented: [Yes / No]

## Approvals

### Customer Approval
- **Approver**: [Name + Organization]
- **Title**: [Role]
- **Date**: [Date]
- **Signature**: [Digital or handwritten]

### Company Approval
- **Approved by**: [Director of Contracts, or CTO]
- **Date**: [Date]
- **Signature**: [Digital or handwritten]
```

---

## Closure Verification Checklist

**MOD Closure Verification** - Ensure MOD is truly complete:

**Deliverables**:
- [ ] All features delivered (per MOD proposal scope)
- [ ] All code merged to production
- [ ] All documentation delivered (deployment guide, API docs, runbooks)
- [ ] All tests completed + passing (unit ≥80%, integration, customer UAT)
- [ ] All training completed (if specified in MOD)

**Quality**:
- [ ] No critical/blocking defects outstanding
- [ ] All security reviews completed (if applicable)
- [ ] All compliance reviews completed (if applicable)
- [ ] Production deployment successful (canary healthy, SLIs met)

**Customer Acceptance**:
- [ ] Customer has tested deliverables in staging
- [ ] Customer has tested deliverables in production
- [ ] Customer formally accepts deliverables (written sign-off)
- [ ] Customer satisfied with quality + schedule
- [ ] No outstanding customer issues/complaints

**Financial**:
- [ ] All invoices issued per payment schedule
- [ ] All payments received (or AR aging documented)
- [ ] Actual cost vs budgeted cost reconciled
- [ ] Any budget variance explained + documented

**Compliance**:
- [ ] All government notifications sent (if required)
- [ ] All compliance controls implemented + verified
- [ ] Audit trail updated (if applicable)
- [ ] Government acceptance documented (if required)

**Administration**:
- [ ] MOD record marked CLOSED in database
- [ ] All artifacts collected + archived (/ggen/receipts/mods/MOD-*)
- [ ] Lessons learned documented in MOD record
- [ ] Team debriefing held (retrospective)
- [ ] MOD removed from active backlog
- [ ] Finance system updated (budget closed)

---

## Definition of Done

MOD is complete when:

- ✅ MOD Request Form received + logged (MOD ID assigned)
- ✅ Impact Analysis conducted (scope, cost, schedule, compliance assessed)
- ✅ MOD Proposal prepared (detailed, with all cost/schedule/scope terms)
- ✅ MOD Proposal submitted to customer + reviewed
- ✅ MOD Approved by customer (signed proposal or formal approval letter)
- ✅ Contract amendment executed (if cost/schedule changed)
- ✅ Implementation completed (all features coded + tested)
- ✅ Testing completed (unit ≥80%, integration, customer UAT passing)
- ✅ Production deployment completed (canary all stages passed, SLIs healthy)
- ✅ Customer UAT completed (customer tested, formally accepted)
- ✅ All documentation delivered (deployment guide, API docs, runbooks)
- ✅ Customer sign-off received (formal written acceptance)
- ✅ All invoices paid (financial closed)
- ✅ MOD record archived (/ggen/receipts/mods/)
- ✅ Lessons learned documented
- ✅ MOD marked CLOSED

---

## Receipt Contract

**This document is a binding policy specification. Every MOD must produce:**

1. **MOD Request Receipt**: JSON record of customer request
2. **Impact Analysis Receipt**: JSON record of analysis findings + estimates
3. **Proposal Receipt**: PDF + JSON record of proposal submitted
4. **Approval Receipt**: JSON record of customer approval (with signature)
5. **Implementation Receipt**: JSON record of development progress
6. **Testing Receipt**: JSON record of test results + coverage metrics
7. **Deployment Receipt**: JSON record of production deployment + SLI metrics
8. **Closure Receipt**: JSON record of final state (CLOSED) + customer sign-off

**All receipts are stored in `/ggen/receipts/mods/MOD-*/` and immutable (GCS WORM retention).**

**Verification**: Government can audit any MOD by requesting `/ggen/receipts/mods/MOD-*.json`

---

**Last Updated**: January 2026 | **Next Review**: April 2026
