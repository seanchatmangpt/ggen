# TAI Autonomics - Organizational Structure & Headcount Plan

**Version**: 1.0.0
**Date**: January 25, 2026
**Classification**: Internal - Leadership
**Planning Period**: 24 months (2026-2027)

---

## Executive Summary

TAI Autonomics will scale from 1 founder to 30 people over 24 months, organized in 6 functional teams reporting to CEO. Structure is designed for:

- **Capital efficiency**: Lean teams with external contractors for specialized work
- **Rapid market entry**: Sales + engineering velocity before Series A
- **Product excellence**: Redundancy in critical paths (engineering, compliance)
- **Cultural cohesion**: Max team size 10 people before adding management layer

---

## Founding Team (Months 1-3)

### Core 3-Person Founding Team

**1. Sean Chatman - Founder & CEO (Month 0)**
- **Background**: 5+ years identity infrastructure (Auth0 equivalent experience)
- **Education**: CS degree, cloud architecture certifications
- **Role**: Business strategy, investor relations, customer relationships
- **Equity**: 48% (common stock, vesting 4 years with 1-year cliff)
- **Salary**: $100K (below market, founder-stage sacrifice)
- **Key contributions**: Architecture decisions, compliance expertise, GTM

**2. CTO - Sr Backend Engineer (Hire Month 2)**
- **Background**: 10+ years Erlang/OTP systems (telecom or financial trading)
- **Education**: CS degree, systems architecture expert
- **Role**: Technical leadership, platform architecture, infrastructure
- **Equity**: 16% (common stock, vesting 4 years with 1-year cliff)
- **Salary**: $180K (market rate for expert Erlang)
- **Key contributions**: Production-grade system design, team scaling

**3. VP Product/Strategy (Hire Month 2)**
- **Background**: 7+ years identity products (Okta, Auth0, or equivalent)
- **Education**: MBA or equivalent startup experience
- **Role**: Product roadmap, customer discovery, market strategy
- **Equity**: 12% (common stock, vesting 4 years with 1-year cliff)
- **Salary**: $150K
- **Key contributions**: Product-market fit validation, pricing strategy

### Supporting 4-5 Person Extended Founding Team

**4. Head of Sales (Hire Month 3)**
- **Background**: 8+ years enterprise SaaS sales ($50M+ deal experience)
- **Role**: Enterprise pipeline, sales strategy, first customer acquisition
- **Equity**: 8% (common stock, vesting 4 years with 1-year cliff)
- **Salary**: $120K + $30K commission (target $150-180K total)
- **Key contributions**: First 5 enterprise pilots, GTM playbook

**5. Sr Backend Engineer #2 (Hire Month 3)**
- **Background**: 8+ years distributed systems (Rust or Go preferred)
- **Role**: Platform scaling, DevOps, release management
- **Equity**: 6% (common stock, vesting 4 years with 1-year cliff)
- **Salary**: $170K
- **Key contributions**: Second engineer for redundancy, CI/CD setup

**6. Head of Compliance & Security (Hire Month 6)**
- **Background**: 10+ years compliance (SOC 2, HIPAA, GDPR expert)
- **Role**: Security architecture, regulatory alignment, audit readiness
- **Equity**: 4% (common stock, vesting 4 years with 1-year cliff)
- **Salary**: $140K
- **Key contributions**: Security foundation, customer compliance validation

**7. Head of Operations (Hire Month 6)**
- **Background**: 5+ years operations at 10-50 person startups
- **Role**: Finance, HR, legal, office management
- **Equity**: 2% (common stock, vesting 4 years with 1-year cliff)
- **Salary**: $100K
- **Key contributions**: Financial controls, payroll, vendor management

**8. Customer Success Lead (Hire Month 6)**
- **Background**: 5+ years SaaS customer success
- **Role**: Customer onboarding, support, retention
- **Equity**: 2% (common stock, vesting 4 years with 1-year cliff)
- **Salary**: $90K
- **Key contributions**: First customer references, NPS management

---

## Equity Allocation - Founding Team

### Cap Table (Pre-Seed Round)

| Role | Shares | % | Salary | Notes |
|------|--------|---|--------|-------|
| **CEO/Founder** | 4,800,000 | 48% | $100K | Controls major decisions (board seat) |
| **CTO** | 1,600,000 | 16% | $180K | Technical decisions (board seat) |
| **VP Product** | 1,200,000 | 12% | $150K | Product decisions |
| **Head of Sales** | 800,000 | 8% | $120K + $30K | Sales performance (VP-track) |
| **Sr Backend #2** | 600,000 | 6% | $170K | Platform decisions |
| **Head of Compliance** | 400,000 | 4% | $140K | Security sign-off |
| **Head of Ops** | 200,000 | 2% | $100K | Finance oversight |
| **Customer Success** | 200,000 | 2% | $90K | Revenue operations |
| **Option Pool** | 200,000 | 2% | - | Early hires (Months 9-12) |
| **TOTAL** | 10,000,000 | 100% | | |

**Notes on Equity**:
- All equity is common stock (no preferred yet until Series A)
- All vests 4 years / 1 year cliff (industry standard)
- Acceleration: Single-trigger acceleration only if acquired (no double-trigger in seed)
- Re-vesting: Roles hired after Month 6 get 3-year vesting instead of 4-year
- Annual true-ups: Equitable adjustments if major roles unfilled longer than expected

### Pre-Seed Financing (Month 3-4)

**Round**: $500K pre-seed

| Investor | Amount | % Equity | Notes |
|----------|--------|----------|-------|
| **Lead Investor(s)** | $250K | 4.2% | Likely warm intro VC or angel |
| **Secondary** | $150K | 2.5% | 1-2 additional angels |
| **SAFE Notes / Friends & Family** | $100K | 1.7% | Conservative friends, advisors |
| **Total** | $500K | 8.4% | Pre-money $5.95M → Post-money $6.45M |

**Pro-rata Rights**: All pre-seed investors get pro-rata rights in Series A (typical VC clause)

---

## Organizational Structure

### Reporting Structure (Day 1 - Month 12)

```
Board of Directors (CEO + 1 Lead VC + 1 Independent)
    |
    └─── CEO (Sean)
         ├─ CTO (Chief Technology Officer)
         │  ├─ Sr Backend Engineer #2
         │  └─ DevOps/Infrastructure (Contractor, Months 4-12)
         │
         ├─ VP Product & Strategy
         │  ├─ Product Manager (Hire M9)
         │  └─ Customer Insights/Analytics (Contractor, M6+)
         │
         ├─ Head of Sales
         │  └─ Sales Development Rep (Hire M9)
         │
         ├─ Head of Compliance & Security
         │  └─ Security Engineer/Auditor (Contractor, M3-6)
         │
         ├─ Head of Operations
         │  └─ Finance/Admin (Part-time contractor M1-12)
         │
         └─ Head of Customer Success
            └─ Support Engineer (Hire M9)
```

### Lines of Authority

**Decision Rights by Role**:

| Decision Type | Owner | Veto | Frequency |
|---------------|-------|------|-----------|
| **Customer commitments** (>$50K) | CEO | CTO (technical feasibility) | Weekly |
| **Engineering architecture** | CTO | CEO (budget) | Bi-weekly |
| **Product roadmap** | VP Product | CTO + CEO | Monthly |
| **Hiring decisions** | CEO | Finance (budget) | As needed |
| **Spending >$10K** | Operations | CEO (policy) | Monthly |
| **Security policies** | Compliance | CEO (operational impact) | Quarterly |
| **Pricing/packaging** | VP Product + Sales | CEO | Quarterly |

**Board Governance**:
- Board meetings: Monthly (first Monday)
- Board composition: CEO + Lead Investor + 1 Independent Director
- Independent director: Hired by Month 6 (brings compliance expertise)
- CEO reports on: MRR, customer count, burn rate, key risks

---

## Year 1 Headcount Plan (Months 1-12)

### Timeline by Month

| Month | Hire | Total | Team Comp | Notes |
|-------|------|-------|-----------|-------|
| **M1** | Sean (CEO) | 1 | 100% leadership | Founder-stage |
| **M2** | CTO + VP Product | 3 | 67% eng + ops, 33% biz | Founding team core |
| **M3** | Head Sales + Sr Backend | 5 | 60% eng, 20% sales, 20% ops | First sales motion |
| **M4** | - | 5 | - | Hiring freeze (cash flow) |
| **M5** | - | 5 | - | - |
| **M6** | Compliance + Ops + CS Lead | 8 | 50% eng, 25% ops, 25% sales/cs | Operational foundation |
| **M7** | PM + Sales Dev | 10 | 50% eng, 20% ops, 30% sales/cs | Sales motion scaling |
| **M8** | Support Engineer | 11 | 45% eng, 20% ops, 35% sales/cs | CS foundation |
| **M9** | - | 11 | - | - |
| **M10** | Contractor: DevOps | 11.5 | - | Infrastructure scaling |
| **M11** | - | 11.5 | - | - |
| **M12** | - | 11.5 | - | Preparation for Series A |
| **Total Y1** | - | **11.5 FTE** | - | **Target: 15 customers, $180K ARR** |

### Team Composition by Function (Year 1)

```
Engineering (5 FTE):
  - CTO (lead)
  - Sr Backend Engineer #2
  - PM (hybrid product/eng)
  - DevOps Contractor (0.5 FTE)

Sales & Customer Success (3 FTE):
  - Head of Sales
  - Sales Dev Rep (hired M7)
  - Customer Success Lead

Operations & Compliance (2.5 FTE):
  - Head of Operations
  - Head of Compliance & Security (contractor support M3-6)
  - Support Engineer (M8)
```

---

## Year 2 Headcount Plan (Months 13-24)

### Target: Scale to 30 people

### Timeline by Month

| Month | Hire | Total | Notes |
|-------|------|-------|-------|
| **M13** | Sr Backend Eng #3 + Frontend | 13 | Series A prep |
| **M14** | Marketing Manager | 14 | Brand/content |
| **M15** | Enterprise Sales Rep #1 | 15 | Scale enterprise |
| **M16** | Customer Success Manager #2 | 16 | Multiple deployments |
| **M17** | - | 16 | Stabilize |
| **M18** | Series A Close → Hiring spree | 18 | 2 more engineers |
| **M19** | Sales Rep #2 (SaaS) + Product Manager #2 | 20 | Multi-segment |
| **M20** | QA Engineer + Support #2 | 22 | Product quality |
| **M21** | Business Analyst + Analytics | 24 | Revenue operations |
| **M22** | Head of Marketing (backfill) | 25 | Brand building |
| **M23** | Sales Manager (manage reps) | 26 | Sales leadership |
| **M24** | Final hires (Finance, HR) | 30 | Org maturity |

### Series A Funding (Month 18-20)

**Target raise**: $10-15M at $50M+ post-money valuation

**Use of capital**:
- Sales team: $3-4M (3-4 new sales reps + manager)
- Engineering: $3-4M (3-4 new engineers + product leadership)
- Marketing & brand: $1.5-2M
- Infrastructure & compliance: $1-1.5M
- Operations & working capital: $1.5-2M

---

## Year 2 Team Structure (Post Series A)

### Reporting Structure (Month 20+)

```
Board of Directors (CEO + 2 VCs + 1 Independent)
    |
    └─── CEO
         ├─ VP Engineering (New hire, M18)
         │  ├─ Sr Backend Engineer #1, #2, #3
         │  ├─ Frontend Engineer
         │  ├─ QA Engineer
         │  └─ DevOps/Infrastructure
         │
         ├─ VP Sales (Hired externally or promoted Head of Sales)
         │  ├─ Enterprise Account Exec #1, #2
         │  ├─ SaaS Sales Rep
         │  ├─ Sales Development Rep
         │  └─ Sales Manager
         │
         ├─ VP Product & Strategy
         │  ├─ Product Manager #1 (Enterprise)
         │  ├─ Product Manager #2 (SaaS)
         │  └─ Analytics/Insights
         │
         ├─ Head of Customer Success
         │  ├─ CSM #1 (Enterprise)
         │  ├─ CSM #2 (SaaS)
         │  ├─ Support Engineer #1, #2
         │  └─ Onboarding Specialist
         │
         ├─ Chief Compliance Officer (promoted from Head)
         │  ├─ Security Engineer
         │  └─ Compliance Auditor (part-time)
         │
         ├─ CFO/Head of Finance
         │  ├─ Controller
         │  └─ Finance Analyst
         │
         ├─ VP Marketing (New hire, M22)
         │  ├─ Content Marketer
         │  └─ Marketing Operations
         │
         └─ Head of People (New hire, M24)
            └─ Recruiting Coordinator
```

### Functions by Year 2 End

| Function | Count | Reports To | Notes |
|----------|-------|-----------|-------|
| **Engineering** | 8 | VP Eng | 3 backend, 1 frontend, 1 QA, 1 DevOps, 1-2 contractors |
| **Sales** | 6 | VP Sales | 2 enterprise, 1 SaaS, 1 SDR, 1 sales mgr, 1 sales ops |
| **Product & Strategy** | 3 | VP Product | 2 PMs (enterprise + SaaS), 1 analyst |
| **Customer Success** | 5 | Head CS | 2 CSMs, 2 support, 1 onboarding |
| **Compliance & Security** | 3 | Chief Compliance Officer | 1 security eng, 1 auditor |
| **Finance & Ops** | 3 | CFO | 1 controller, 1 analyst |
| **Marketing** | 3 | VP Marketing | 1 content, 1 marketing ops |
| **People & Culture** | 1 | CEO | Recruiting + culture |
| **Total** | **30** | | ~$4.3M annual payroll |

---

## Advisory Board (Months 1-24)

### Advisors (3-5 Domain Experts)

**1. Identity & Access Expert - Advisory Board Chair**
- **Profile**: Former VP/Director at Okta, Auth0, or Ping Identity
- **Expertise**: IAM architecture, enterprise GTM, compliance
- **Commitment**: 4 hours/month, quarterly in-person (if possible)
- **Compensation**: 0.25% equity (vesting over 4 years), no salary
- **Specific role**: Technical advisor on product strategy, enterprise customer introductions

**2. Compliance & Regulatory Expert**
- **Profile**: Former CISO or Chief Compliance Officer at financial/healthcare firm
- **Expertise**: GDPR, HIPAA, SOX, audit readiness
- **Commitment**: 2 hours/month, especially during customer audits
- **Compensation**: 0.15% equity, no salary
- **Specific role**: Regulatory strategy, compliance playbooks, customer confidence

**3. Sales & Go-to-Market Advisor**
- **Profile**: Former VP Sales at $10M+ ARR SaaS (esp. B2B infrastructure)
- **Expertise**: Enterprise sales playbook, channel strategy, pricing
- **Commitment**: 3 hours/month, weekly check-ins during sales ramp
- **Compensation**: 0.1% equity, no salary
- **Specific role**: Sales playbook design, hiring first VP Sales, enterprise introductions

**4. Infrastructure & Reliability Advisor** (Optional, Month 6+)
- **Profile**: Former VP Infrastructure at telecom or financial trading
- **Expertise**: Erlang/OTP, high-reliability systems, multi-region deployment
- **Commitment**: 2 hours/month (intensive during scaling)
- **Compensation**: 0.1% equity, no salary
- **Specific role**: Platform scaling strategy, infrastructure partnerships

**5. VC/Investor Advisor** (Board Seat, Month 4+)
- **Profile**: Lead pre-seed investor or partner at VC with infrastructure focus
- **Expertise**: Startup scaling, Series A fundraising, investor relations
- **Commitment**: Board meetings monthly + ad-hoc
- **Compensation**: Equity stake (from investment round) + potential board fee
- **Specific role**: Investor relations, Series A strategy, executive recruitment

### Advisory Board Schedule

| Frequency | Activity | Attendees |
|-----------|----------|-----------|
| **Monthly** | Advisory board email (highlights + asks) | All advisors |
| **Quarterly** | Virtual sync (1 hour, 60 min max) | All available |
| **Semi-annual** | In-person if possible (Bay Area) | Ideally all |
| **As needed** | Customer intro calls, technical deep-dives | Specific advisors |

---

## Communication & Decision-Making

### Weekly Cadence

**Monday 9am**: Leadership standup (30 min)
- CEO, CTO, VP Product, Head of Sales, Head of Operations
- Format: Key metrics, blockers, decisions needed
- Output: Weekly priorities for all teams

**Wednesday 2pm**: All-hands (60 min, Months 8+)
- Entire company (when >5 people)
- Format: CEO update, wins, roadmap, culture
- Output: Alignment across teams

**Friday 4pm**: Executive retro (30 min, leadership only)
- What worked, what didn't, improvements for next week
- Format: Informal, blameless
- Output: Process improvements

### Monthly Rhythm

| Week | Activity | Attendees |
|------|----------|-----------|
| **W1** | Board meeting (M1-M12, M13+) | CEO + Lead investor + Independent director |
| **W2** | Revenue review | Sales + Finance + CEO |
| **W3** | Engineering sprint planning | CTO + Engineers + PM |
| **W4** | Investor update (after M4) | CEO + Finance (2-hour prep) |

### Decision-Making Authority

**Decisions CEO can make alone** (report to board):
- Customer commitments <$50K
- Hiring decisions (within budget)
- Day-to-day operational decisions
- Marketing & partnership strategy

**Decisions requiring CTO approval**:
- Customer commitments requiring technical changes
- Infrastructure spending >$5K
- Architectural decisions

**Decisions requiring board vote** (2/3 majority):
- Fundraising strategy
- M&A or strategic partnerships >$100K
- Major product pivots
- Equity grants >0.5%
- Compensation changes >20%

---

## Scaling Principles (Months 12-24 Transition)

### When to Add Management Layers

**Add VP Engineering** (Month 18-20, during Series A):
- When engineering team grows from 3 to 5+ people
- Need dedicated architecture/hiring leadership
- CTO should focus on technical strategy, not day-to-day management

**Add VP Sales** (Month 18-20, during Series A):
- When sales team grows from 1 to 3+ people
- Head of Sales promoted or external hire
- CEO needs to focus on investor relations + strategy

**Add VP Product** (Month 20-24):
- When product strategy becomes multi-segment (Enterprise + SaaS + Embedded)
- Hire external VP or promote from PM
- Coordinate across 2-3 product managers

**Add CFO/Finance** (Month 18-20):
- Prepare for Series A financial diligence
- Build financial controls + GAAP accounting
- Support investor reporting

### Culture & Scaling Risks

**Risk 1: Decision velocity decreases**
- **Mitigation**: Clear decision rights documented (see above)
- **Target**: <24hr approval time for non-board decisions

**Risk 2: Team culture dilutes with new hires**
- **Mitigation**: Hiring for culture fit in first 15 people
- **Target**: Strong team values defined by Month 3
- **Practice**: Founder involvement in all first 20 hires

**Risk 3: Technical debt accumulation**
- **Mitigation**: 20% engineering time on tech debt each sprint
- **Target**: Refactoring sprint every quarter
- **Practice**: Architecture reviews monthly

**Risk 4: Sales commissions create perverse incentives**
- **Mitigation**: Clear customer success metrics tied to commission
- **Target**: Minimum 90% gross retention rate
- **Practice**: Clawback commission if customer churns in first year

---

## Success Metrics & Accountability

### Team Health Indicators (Monthly)

| Metric | Target | Owner | Frequency |
|--------|--------|-------|-----------|
| **Hiring plan on track** | 80%+ of planned hires | CEO | Weekly |
| **Employee retention** | >95% | Operations | Monthly |
| **Offer acceptance rate** | >70% | Recruiting | Monthly |
| **Onboarding satisfaction** | NPS >8 | Operations | Monthly |
| **Decision velocity** | <24hr for non-board | CEO | Monthly |
| **Team engagement** | 1x/quarter pulse survey | CEO | Quarterly |

### Team Composition Audit (Quarterly)

By Month 12, evaluate:
- Is 50/50 engineering/sales-ops split optimal?
- Do we have enough product expertise?
- Should any roles be promoted to manager level?
- Are we attracting talent to Erlang/OTP stack?

---

## Appendix: Salary Benchmarking

### Market Rates (San Francisco, 2026)

| Role | Min | Typical | Max | Notes |
|------|-----|---------|-----|-------|
| **Founder/CEO** | $50K | $100K | $150K | Typically underpaid at startup stage |
| **CTO (Erlang expert)** | $160K | $190K | $230K | Erlang expertise premium |
| **Sr Backend Engineer** | $150K | $180K | $220K | 8+ years, systems experience |
| **Frontend Engineer** | $140K | $170K | $210K | 5+ years React/TypeScript |
| **VP Product** | $140K | $170K | $210K | Experience + MBA typical |
| **Head of Sales** | $100K | $140K | $200K | Base only, commission on top |
| **Head of Compliance** | $130K | $160K | $200K | Regulatory expertise premium |
| **Head of Operations** | $80K | $110K | $150K | Varies by scope |
| **Customer Success Lead** | $70K | $95K | $130K | Lower early, scales with team |

### Benefits Package (Fixed, All Roles)

- **Health Insurance**: Company pays 80% premium (medical/dental/vision)
- **401(k)**: Company matches 3-5% (deferred to Series A fundraising)
- **Equity**: Vesting schedule 4 years / 1 year cliff (roles post-M6: 3 years)
- **PTO**: 20 days + 5 sick days (unlimited after M12)
- **Remote stipend**: $500/month (laptop, internet, ergonomics)
- **Professional development**: $2K/person/year (books, courses, conferences)
- **Life insurance**: 1x salary (company-paid)

### Bonus Structure (Post-Series A)

- **Engineering**: 15-20% of salary (based on delivery vs roadmap)
- **Sales**: 20-30% of salary (based on quota attainment)
- **Operations**: 10-15% of salary (based on budget adherence)
- **Product/Compliance**: 15% of salary (based on OKRs)

---

## References & Related Documents

- **HIRING_PLAN.xlsx**: Month-by-month hiring + cash flow projections
- **CULTURE_DOCUMENT.md**: Team values, interview process, onboarding
- **EQUITY_MODEL.md**: Cap table, dilution scenarios, option pool management
- **FINANCIAL_MODEL.md**: Salary + benefits built into 24-month burn projections
- **BOARD_GOVERNANCE.md**: Board composition, meeting cadence, decision rights (separate doc)

---

**Document**: TAI Autonomics Organization Chart
**Version**: 1.0.0
**Last Updated**: January 25, 2026
**Classification**: Confidential - Internal

---

*Next review: April 2026 (post-Series A funding)*
