# TAI Erlang Autonomics: Discovery Checklist & Sales Questions

**Purpose**: 20 discovery questions to validate customer fit and uncover TAI value before proposing a POC.

**Duration**: 30-45 minutes (2-3 discovery calls)

**Success Criteria**:
- ICP Score 75+ (see IDEAL_CUSTOMER_PROFILE.md)
- Customer confirms 3+ pain points that TAI solves
- Budget availability identified
- Decision timeline clear

---

## SECTION 1: Current State Assessment (10 min)

### Operational Baseline
**Q1: How many people on your team spend time managing SKU/entitlements/quotas each week?**
- Target answer: 1-5+ FTE/month = high pain, good fit
- Sub-questions:
  - What % of their time is manual (vs. automated)?
  - What tasks take the most time?
  - Have you tried to automate it? What happened?

**Q2: What does your current entitlement/quota system look like?**
- Listen for:
  - Manual processes (spreadsheets, ad-hoc scripts)
  - Database triggers or application logic
  - Custom-built systems with limited audit trails
  - Scaling challenges (N tenants = N² configuration complexity)

**Q3: How do you currently track changes to SKU rules, pricing tiers, or quota limits?**
- Target answer: "Manually in docs" or "Git commits" = no audit trail
- Red flag: No tracking mechanism (compliance risk)
- Green flag: Audit requirements mentioned = TAI's core value

**Q4: Have you experienced any incidents where entitlements were incorrect (over-provisioned, under-provisioned, or misconfigured)?**
- Listen for:
  - Revenue leakage (customer got service without paying)
  - Customer complaints (quota enforcement broke UX)
  - Compliance issues (wrong tier, access violations)
  - Cost: How much did it impact revenue/reputation?

### Systems Landscape
**Q5: What's your current technology stack? (Focus on: Database, messaging, cloud platform)**
- Ideal: GCP + Pub/Sub + Firestore (native fit for TAI)
- Good: GCP but using Postgres (TAI still solves governance)
- Possible: Multi-cloud (TAI focused on GCP)
- Challenge: On-prem only (consider later)

**Q6: Do you use Google Cloud Platform (GCP) today?**
- Yes = immediate fit, faster implementation
- No = need migration discussion
- Hybrid = possible phased approach

---

## SECTION 2: Pain Point Validation (10 min)

### Compliance & Audit
**Q7: Do you have compliance requirements (SOC2, HIPAA, GDPR, PCI-DSS, or financial audits)?**
- Yes + "entitlements are part of audit" = strong TAI fit
- Yes + "no audit trail for entitlements" = compliance risk + TAI solves
- No = less urgent, but TAI still valuable for scaling

**Q8: When auditors ask about how entitlements are enforced and changed, what's your current answer?**
- "We have detailed logs" = might not be cryptographically verified
- "It's in our code" = no audit trail, compliance risk
- "We don't have one" = critical gap, TAI's breakthrough value
- Cost of audit: How long does it take? What's the cost?

### Scaling & Performance
**Q9: How many concurrent entitlement checks do you need to support? (e.g., "100/sec", "5,000/sec")**
- <1,000/sec = moderate scale, TAI still adds audit value
- 1,000-10,000/sec = TAI's sweet spot (handles with <50ms latency)
- >10,000/sec = potential fit if latency is bottleneck

**Q10: Have you hit performance issues with entitlement checks? (Latency, timeout, database locks)**
- Yes = TAI's <50ms latency is differentiator
- No = still valuable for audit trail, but less urgent
- Frequency: How often does this happen? Impact on customers?

### Business Agility
**Q11: How long does it take to launch a new pricing tier, change a quota, or modify an entitlement rule?**
- Days/weeks = TAI's agility advantage (1-2 hours)
- Minutes (already fast) = still valuable for audit + scale
- "I don't know" = ask engineering team

**Q12: Have you had to delay product launches because entitlement logic was complex?**
- Yes = TAI reduces time-to-market
- No = ask why (might reveal they don't scale frequently, lower TAI fit)

---

## SECTION 3: Business Impact & ROI (10 min)

### Quantifying Current Cost
**Q13: How much time (in hours/month) do you spend on SKU/entitlement management tasks?**
- Manually creating/modifying rules
- Troubleshooting entitlement bugs
- Responding to customer quota questions
- Running audits or compliance checks

**Calculation**: Hours × Fully-Loaded Cost ($100-150/hour) = Annual Cost
- Example: 40 hours/month × 12 × $125/hour = $60K/year wasted effort

**Q14: Roughly how much revenue is at risk if entitlements fail?**
- Revenue leakage (customer gets service without paying)
- Customer churn (quota bugs hurt UX)
- Compliance fines (non-negotiable for fintech/healthcare)
- Target: Identify $500K+ at-risk revenue = strong ROI case for TAI

### Decision Authority & Budget
**Q15: Who controls the budget for tools like this? (Finance, Product, Engineering, CTO)**
- Single decision-maker = faster close
- Multiple stakeholders = longer sales cycle
- CFO approval required = needs ROI justification
- No budget allocated = likely not a fit for Q1/Q2

**Q16: What's your typical procurement timeline for a tool like this? (Budget approval → contract → go-live)**
- Weeks = good fit
- Months = manageable
- Months + RFP process = longer sales cycle
- Follow-up: Is there a 2026 budget already approved for this category?

---

## SECTION 4: Organizational Readiness (8 min)

### Technical Integration
**Q17: Do you have an engineering team that can integrate with our API, or will you need professional services?**
- In-house = faster implementation (30 days)
- Need help = requires services engagement (60 days)
- Answer reveals implementation scope + revenue opportunity (services revenue)

**Q18: Have you evaluated other autonomic governance solutions? If so, why are you open to a conversation with us?**
- Reveals competitive landscape + objections to prep for
- If no others evaluated = good buying signal (early-stage problem awareness)
- If evaluated others = understand objections, differentiation needed

### Organization & Sponsorship
**Q19: Who's your biggest champion for solving this problem? (Title, department)**
- Strong sponsor (VP level) = likely to win
- Weak sponsor (IC engineer) = will need to convince upward
- Multiple sponsors (Eng + Product + Finance) = strong deal signal

**Q20: If this solution reduced manual effort by 50% and gave you a complete audit trail, what would you do with the freed-up team time?**
- Listen for: Feature development, scaling, compliance work, team expansion plans
- Reveals true value (not just cost savings, but growth enablement)
- Helps build ROI narrative for approval

---

## Scoring: Is This a Fit?

After 20 questions, score customer on these dimensions:

| Dimension | Low (0) | Medium (5) | High (10) | Score |
|---|---|---|---|---|
| **Current manual effort** | <10 hrs/mo | 10-30 hrs/mo | 30+ hrs/mo | ___ |
| **Compliance requirements** | None | Nice-to-have | Mandatory/audited | ___ |
| **Performance/scale pain** | No issues | Some latency | Major bottleneck | ___ |
| **Budget availability** | No budget | Request possible | Pre-approved | ___ |
| **Decision timeline** | 3+ months | 4-8 weeks | <4 weeks | ___ |
| **Technical fit (GCP)** | On-prem only | Multi-cloud | GCP-native | ___ |
| **Business impact (ROI)** | <$100K savings | $100-500K savings | $500K+ at-risk | ___ |
| **Executive sponsorship** | None/weak | Medium (manager) | Strong (VP+) | ___ |

**Total Score**: Sum all 8 dimensions ÷ 8 = Average (out of 10)

**ICP Fit Threshold**:
- 8-10 = Ideal fit, recommend POC
- 6-8 = Good fit, qualify further
- 4-6 = Medium fit, revisit in 6 months
- <4 = Low fit, pass or redesign approach

---

## Follow-Up Actions by Score

### Score 8-10 (Ideal Fit)
- **Immediate Next Step**: Schedule technical discovery with their engineering team
- **Sales Objective**: Get commitment for 30-day POC by end of week
- **Proposal Template**: POC Charter (see SALES_PLAYBOOK.md > Demo/POC section)
- **Timeline**: POC in 2-4 weeks, contract signatures by end of month

### Score 6-8 (Good Fit)
- **Immediate Next Step**: Send use case study + ROI calculator
- **Sales Objective**: Setup follow-up call with CFO/budget owner
- **Proposal Template**: Business case + ROI analysis
- **Timeline**: Decision likely in 6-8 weeks

### Score 4-6 (Medium Fit)
- **Immediate Next Step**: Add to nurture sequence
- **Sales Objective**: Send quarterly updates as product evolves
- **Proposal Template**: Educational content (blog, webinar)
- **Timeline**: Revisit in 6 months when pain likely increases

### Score <4 (Low Fit)
- **Immediate Next Step**: Thank them, stay in touch
- **Sales Objective**: Position for future market segment shift
- **Proposal Template**: Annual check-in
- **Timeline**: 12-month follow-up if circumstances change

---

## Sales Call Prep (Before Discovery Call)

**Research Phase (15 min)**:
1. Find company: Employee count, funding stage, revenue (Crunchbase, LinkedIn)
2. Find contact: LinkedIn search, mutual connections, company website
3. Analyze: Current tech stack (G2, StackShare, job postings), recent product launches
4. Prepare: 2-3 specific pain point hypotheses based on their business

**Opening (2 min)**:
> "Thanks for taking time. We work with [similar company type] solving autonomous SKU management. I know you're busy, so I'll ask some specific questions and listen. Fair?"

**Close (2 min)**:
> "Based on this conversation, it sounds like [summarize pain points]. We've seen similar challenges at [peer company]. Would it make sense to spend 30 minutes next week with our engineering team to see if we could help? No pressure—I'll send a summary regardless."

---

## Red Flags (Don't Pursue Further)

- "We've never had issues with entitlements" (no problem awareness)
- "Our system is perfect, no changes needed" (mature/satisfied, low urgency)
- "We need 6+ months for any evaluation" (too slow for startup velocity)
- "Our on-prem system is locked in, can't move to GCP" (wrong segment)
- "Budget is gone for 2026" (wait until 2027 planning)
- "We've rejected similar solutions before" (understand why before pursuing)

---

## Green Flags (Fast-Track to POC)

- "We've been struggling with this for 2+ years"
- "Auditors have flagged our entitlement audit trail as a gap"
- "We're losing deals because of scaling limitations"
- "We have budget approved and timeline for implementation"
- "Our VP of Product is already aware of autonomic systems"
- "We're planning 3-5 new pricing tiers next quarter"

---

## Discovery Call Recording & Notes Template

**Meeting Date**: _______________
**Company**: _______________
**Contact Name/Title**: _______________
**Duration**: _______________

**Key Learnings** (captured real-time):
1. Current manual effort: _______________ hours/month
2. Main pain point: _______________
3. Compliance requirements: Yes / No / Partial
4. Budget & timeline: _______________
5. Key stakeholders: _______________

**ICP Score**: ___ / 10

**Next Steps**:
- [ ] Send POC charter
- [ ] Schedule engineering call
- [ ] Send ROI calculator
- [ ] Add to nurture sequence
- [ ] Pass to competitor evaluation

**Objections & Responses**:
1. Objection: _______________
   Response planned: _______________

---

## Document Version

- **Version**: 1.0
- **Date**: 2026-01-25
- **Owner**: Sales & Product Strategy
- **Next Review**: 2026-03-25 (after first 3 discovery calls)
