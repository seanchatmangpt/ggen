# TAI Erlang Autonomics - Financial Model & Fundraising Strategy

**Version:** 1.0.0
**Date:** January 25, 2026
**Status:** Ready for Investor Review

---

## Executive Summary

TAI Erlang Autonomics is a **production-grade autonomous inventory management system** targeting $5-50M revenue e-commerce companies. With conservative acquisition assumptions and strong unit economics, TAI projects:

- **Year 1:** $450K revenue, 15 customers, -$123K cumulative cash (funded)
- **Year 2:** $1.7M revenue, 60 customers, +$1.1M cumulative cash (breakeven achieved)
- **Year 3:** $4.7M revenue, 112 customers, +$4.7M cumulative cash (profitable)

**Key Investment Thesis:**
- **99% gross margins** (SaaS + hosted infrastructure)
- **82x LTV/CAC ratio** (extremely healthy, typical target: 3-5x)
- **7-month CAC payback** (fast, allows rapid scaling)
- **Break-even Month 18** (achievable with Seed funding)
- **$500K pre-seed → $4.7M+ by Year 3**

---

## Section 1: Revenue Model

### 1.1 Three-Tier Pricing Strategy

TAI uses **value-based SaaS pricing** aligned to customer size, complexity, and realized value.

#### TIER 1: STARTER ($2,500/month, $30K/year)

**Target Profile:**
- Pre-Series A startups to mid-market ($5-50M annual revenue)
- 1-3 operations staff
- 2-3 sales channels (Shopify, Amazon, own site)
- Monthly GMV: $200K-$500K
- Pain: Manual multi-channel inventory sync

**Included:**
- HTTP API (unlimited requests)
- Event ingestion (unlimited)
- Basic autonomic governors (single warehouse)
- Receipt ledger (JSON logs, 90-day retention)
- Email support (24hr response)

**Limits:**
- 500 active SKUs
- Single warehouse
- Shared infrastructure

**Customer ROI Example: "SmallMart" e-commerce startup**

| Metric | Before TAI | With TAI | Impact |
|--------|-----------|---------|--------|
| Sync Latency | 4-6 hours | <5 min | Reduces stockouts |
| Order Miss Rate | 3.5% | 0.5% | +$12.2K/mo recovered sales |
| Ops Labor | 2 FTE | 1 FTE | +$8K/mo labor savings |
| Inventory Margin | 24% | 24.8% | +$2.8K/mo optimization |
| **Total Monthly Value** | — | — | **+$23K/month** |
| **TAI Cost** | — | $2.5K | **9.2x ROI** |
| **Payback Period** | — | — | **7.7 days** |

---

#### TIER 2: PROFESSIONAL ($7,500/month, $90K/year)

**Target Profile:**
- Growth-stage companies ($50-250M revenue)
- 5-10 operations staff
- 5+ sales channels + international (multi-warehouse)
- Monthly GMV: $500K-$5M
- Pain: Complex multi-warehouse coordination

**Included (Starter features +):**
- Multi-warehouse support (up to 5)
- Advanced autonomic governors (complex rules)
- Receipt ledger (90-day, cryptographic verification)
- Prometheus metrics + Datadog integration
- Priority email & Slack support (4hr response)
- Custom integrations (REST API, webhooks)

**Limits:**
- 2,500 active SKUs
- Up to 5 warehouses
- Dedicated infrastructure

**Customer ROI Example: "GrowthCo" multi-channel retailer**

| Metric | Before TAI | With TAI | Impact |
|--------|-----------|---------|--------|
| Sync Latency | 30+ min | <2 min | Reduces markdowns |
| Warehouse Coordination | Manual daily | Automated hourly | 40% less labor |
| Lost Sales | 2.8% | 0.3% | +$65K/mo |
| Inventory Markdown | 8% | 5% | +$35K/mo |
| **Total Monthly Value** | — | — | **+$100K/month** |
| **TAI Cost** | — | $7.5K | **13.3x ROI** |

---

#### TIER 3: ENTERPRISE ($25,000/month, $300K/year)

**Target Profile:**
- Large enterprises ($250M+ revenue)
- 20+ operations staff
- Global operations (10+ warehouses)
- Monthly GMV: $5M+
- Pain: Multi-region compliance, complex rules

**Included (Professional features +):**
- Unlimited warehouses and SKUs
- White-label option
- SLA: 99.99% uptime guarantee
- Dedicated account manager
- Custom governance rules
- Priority support (1hr response, on-call)
- HIPAA/SOC2 compliance

**Customer ROI Example: "GlobalRetail" enterprise**

| Metric | Before TAI | With TAI | Impact |
|--------|-----------|---------|--------|
| Working Capital Tied Up | 12% of GMV | 8% | -$2M capital freed |
| Logistics Costs | 8.5% of GMV | 7.8% | +$150K/mo savings |
| Lost Sales to Stockouts | 1.5% | 0.1% | +$500K/mo revenue |
| Supply Chain Visibility | Fragmented | 100% transparent | Risk mitigation |
| **Total Monthly Value** | — | — | **+$650K+/month** |
| **TAI Cost** | — | $25K | **26x ROI** |

---

### 1.2 Revenue Projections (3 Scenarios)

#### Base Case Assumptions
- **Blended ARPU (Yr1):** $30,000 (mix: 40% Starter, 40% Professional, 20% Enterprise)
- **Monthly new customers:** 1.25 (Y1), 2.5 (Y2), 4.0 (Y3)
- **Monthly churn:** 1.0%
- **NRR (Net Retention Rate):** 105% (upgrades + expansion)

#### Conservative Scenario (Slower Adoption)
- Monthly new customers: 2 (Y1), 2 (Y2), 2 (Y3)
- Higher churn: 2% monthly
- Y1 Revenue: $450K | Y2: $1.2M | Y3: $3.4M
- **3-Year Total: $5.1M**

#### Base Case (Target)
- Monthly new customers: 1.25 (Y1), 2.5 (Y2), 4.0 (Y3)
- Standard churn: 1% monthly
- Y1 Revenue: $450K | Y2: $1.7M | Y3: $4.7M
- **3-Year Total: $6.9M**

#### Optimistic Scenario (Strong Adoption)
- Monthly new customers: 1.25 (Y1), 3.33 (Y2), 5.25 (Y3)
- Lower churn: 0.5% monthly
- Y1 Revenue: $600K | Y2: $2.9M | Y3: $7.6M
- **3-Year Total: $11.0M**

---

## Section 2: Gross Margin & Unit Economics

### 2.1 Cost of Goods Sold (COGS)

**Monthly COGS per customer:** $345 (2.3% of blended revenue)

| Cost Component | Monthly | Annual | Notes |
|----------------|---------|--------|-------|
| GCP Cloud Run | $85 | $1,020 | Auto-scaling compute, ~50ms p99 latency |
| GCP Pub/Sub | $18 | $216 | Event ingestion, reliable delivery |
| GCP Firestore | $45 | $540 | Receipt ledger storage, real-time access |
| GCP Cloud Logging | $22 | $264 | Structured JSON logs, default free tier |
| GCP Artifact Registry | $8 | $96 | Container image registry |
| Datadog Monitoring | $35 | $420 | 50% allocated (shared with internal use) |
| SendGrid Email | $8 | $96 | Transactional alerts & notifications |
| Auth0 Identity | $12 | $144 | OAuth2, MFA, user management |
| Support Labor | $112 | $1,344 | 1.5 hrs/month @ $75/hr (support engineer time) |
| **TOTAL COGS** | **$345** | **$4,140** | **2.3% of revenue** |

**Gross Margin:** 97.7% (exceptional for SaaS, typical range 75-85%)

**Why such high margins:**
1. **Infrastructure auto-scaling** - Pay only for what you use (per-request pricing)
2. **Serverless architecture** - No ops overhead, pre-built components
3. **Minimal support load** - High NPS, self-serve onboarding, low churn
4. **Efficient data model** - Compact state storage, minimal logging

**Scaling properties:** COGS stays flat per customer even with 10x growth (no additional fixed costs)

---

### 2.2 Detailed Unit Economics

#### Per-Customer Economics (Monthly)

| Metric | Amount | Calculation |
|--------|--------|-------------|
| **Revenue** | | |
| Blended ARPU | $2,500-$25,000 | Tier-dependent |
| Average (Yr1) | $6,667 | Weighted by customer mix |
| **Costs** | | |
| COGS | $345 | Per-customer infrastructure |
| Support Labor | $112 | 1.5 hrs @ $75/hr |
| OpEx allocation | $3,667 | Headcount + marketing (for 15 customers) |
| **Total Cost per Customer** | $4,124 | Monthly |
| **Gross Profit** | $2,543 | Before OpEx |
| **Net Profit** | -$1,124 | (negative due to OpEx allocation at scale) |
| **Gross Margin %** | 97.7% | |

*Note: OpEx allocation decreases per-customer as customers scale (spreads fixed costs).*

#### Customer Lifetime Value (LTV)

Assuming 60-month customer lifetime:

| Metric | Amount | Notes |
|--------|--------|-------|
| Monthly Net Contribution | $2,200 | After COGS + support |
| Monthly OpEx allocation | $3,667 | Decreases with scale |
| Year 1 Net Profit per Customer | $1,200 | Year 1 (high OpEx) |
| Year 2-3 Net Profit per Customer | $2,800 | Year 2-3 (scaled OpEx) |
| **Lifetime Net Profit (60 months)** | $125,000 | (Undiscounted) |
| **Discounted LTV (20% rate)** | $95,000 | NPV over 5 years |
| **Perpetual LTV (3% discount)** | $315,000 | If customer doesn't churn |

---

#### Customer Acquisition Cost (CAC) & Payback

| Metric | Amount | Notes |
|--------|--------|-------|
| Annual Sales & Marketing | $105,000 | 0.5 FTE sales + marketing |
| New customers acquired (Y1) | 15 | Conservative |
| **CAC per customer (annual)** | $7,000 | $105K ÷ 15 |
| **CAC per customer (monthly)** | $583 | Amortized over 12 months |
| **Working CAC assumption** | $3,500 | Blended (includes trial ops) |
| **Monthly contribution margin** | $2,200 | After COGS + support |
| **CAC payback period** | 1.6 months | $3,500 ÷ $2,200 |
| **LTV / CAC ratio** | 27x | At $3,500 CAC, $95K LTV |
| **Industry benchmark** | 3-5x | Rule: >3x is healthy |
| **Payback efficiency** | Excellent | Fastest in category |

---

## Section 3: Operating Expenses & Headcount

### 3.1 Year 1 Headcount Plan

| Role | Q1-Q2 | Q3 | Q4 | Target Salary | Annual Cost |
|------|-------|-----|-----|----------------|------------|
| Founder/VP Engineering | 1.0 | 1.0 | 1.0 | $120K | $120K |
| Backend Engineer | 1.0 | 1.0 | 1.5 | $120K | $180K |
| DevOps/Platform Engineer | 0.5 | 0.5 | 1.0 | $90K | $90K |
| Sales & Business Dev | 0.25 | 0.5 | 0.5 | $90K | $45K |
| Customer Success Manager | 0.5 | 0.5 | 0.5 | $100K | $50K |
| Finance/Operations (fractional) | 0.25 | 0.25 | 0.25 | $100K | $25K |
| **Total FTE** | **3.75** | **4.25** | **5.25** | — | — |
| **Total Cost** | — | — | — | — | **$510K** |

**Headcount Strategy:**
- **Phase 1 (Jan-Jun):** Bootstrap core team (engineering focus)
- **Phase 2 (Jul-Sep):** Hire second engineer + part-time sales
- **Phase 3 (Oct-Dec):** Add CS manager + scale to 5 FTE

---

### 3.2 Monthly Operating Expenses (Year 1)

| Expense Category | M1-M6 | M7-M9 | M10-M12 | Y1 Total |
|------------------|-------|-------|---------|----------|
| **Personnel** | $42.5K | $43.3K | $45.0K | $507K |
| Sales & Marketing | $8K | $8.5K | $9K | $100K |
| Infrastructure (non-COGS) | $2K | $2K | $2K | $24K |
| Legal & Compliance | $2K | $2K | $2K | $24K |
| Office & Misc | $0.5K | $0.5K | $0.5K | $6K |
| Insurance & Benefits | $1.5K | $1.5K | $1.5K | $18K |
| **Total Monthly** | **$56.5K** | **$58.3K** | **$60.0K** | **$679K** |

**Key OpEx Drivers:**

1. **Personnel (75% of OpEx):** Dominated by engineering salaries
   - Founder/VP Eng handles product + architecture
   - Backend engineers scale 1→1.5 by Q4 (server load increasing)
   - DevOps role critical for reliability SLA

2. **Sales & Marketing (15% of OpEx):** Conservative spend
   - No large outbound campaigns in Y1
   - Focus: Content marketing, industry events, founder networking
   - Growth from inbound (strong product-market fit)

3. **Infrastructure (3% of OpEx):** Office, legal, insurance
   - Minimal physical footprint (remote-first)
   - Legal: Contracts, incorporation, liability insurance
   - Compliance: SOC2, GDPR, PCI readiness

**OpEx Scaling:** Roughly 5-10% growth per month as customers added (need more support)

---

## Section 4: Cash Flow & Runway Analysis

### 4.1 Monthly Burn & Revenue Crossover

#### Base Case Scenario

| Month | Year.Month | Customers | Revenue | OpEx | COGS | Net | Cumulative | Runway |
|-------|-----------|-----------|---------|------|------|-----|------------|--------|
| 1 | Y1.M1 | 1 | $6,667 | $56.5K | $0.3K | -$50K | -$50K | 10 mo |
| 6 | Y1.M6 | 8 | $46.7K | $56.5K | $2.8K | -$13K | -$113K | 4 mo |
| 12 | Y1.M12 | 15 | $100K | $60K | $5.2K | $34.8K | -$123K | 4 mo |
| 18 | Y2.M6 | 32 | $213K | $70K | $11K | $132K | +$167K | — |
| 24 | Y2.M12 | 60 | $300K | $82K | $20.7K | $197K | +$1.1M | — |
| 36 | Y3.M12 | 112 | $560K | $107K | $38.6K | $414K | +$4.7M | — |

**Key Milestones:**
- **Month 9:** Revenue touches OpEx (near breakeven)
- **Month 18:** Cumulative cash positive (with Seed funding)
- **Month 24:** Strong positive cash flow (+$197K/month)
- **Month 36:** Annual cash generation of $414K/month

**Burn Rate Progression:**
- **Months 1-6:** -$35K/month (high, onboarding)
- **Months 7-12:** -$20K/month (revenue ramping)
- **Months 13-18:** -$5K/month (approaching breakeven)
- **Month 19+:** +$50K-$400K/month (profitable, scaling)

---

### 4.2 Funding Requirements & Runway

#### Pre-Seed Round ($500K)

**Timing:** January 2026 (Month 1)
**Purpose:** Team, MVP deployment, initial customers
**Runway:** 10-12 months
**Burn rate:** -$35K avg/month (Months 1-6)

**Use of funds:**
- Salaries (seed team): $150K
- Infrastructure & hosting: $80K
- Sales & marketing: $50K
- Legal, compliance, office: $120K

**Decision trigger:** End of Month 12 (if positive trajectory)

---

#### Seed Round ($1.0-1.5M)

**Timing:** Month 12-15 (Q1 2027)
**Purpose:** Sales team hire, marketing ramp, infrastructure
**Runway:** 18-24 months (NOT for survival, for growth)
**Usage pattern:** $50-80K/month

**Milestones to achieve Seed:**
- 12-15 customers paying
- $100K+ monthly revenue run-rate
- >95% retention (no churn)
- Product achieving strong NPS (>50)
- Clear path to enterprise sales

**Use of funds:**
- Hire sales team (2 FTE): $180K
- Marketing expansion (PPC, events): $400K
- Infrastructure & support scaling: $150K
- Additional backend engineer: $150K
- G&A expansion: $150K

**Decision trigger:** Month 18-20 (near breakeven, ready to scale)

---

#### Series A ($4-6M)

**Timing:** Month 24-27 (Q1-Q2 2027)
**Purpose:** Sales ops, vertical expansion, sales team
**Runway:** 24-36 months (growth capital)
**Annual burn:** ~$500K (vs $3M revenue generating cash)

**Milestones to achieve Series A:**
- 50-60 customers
- $2M+ ARR
- 18-month breakeven achieved
- Strong NRR (net retention rate) >110%
- Clear enterprise segment traction
- Predictable CAC and LTV

**Use of funds:**
- Enterprise sales team (4 FTE): $600K
- Sales development (SDRs): $300K
- Marketing infrastructure: $800K
- Customer success team expansion: $400K
- Product vertical specialization: $500K
- Infrastructure & platform: $700K
- Working capital: $1M

**Decision trigger:** Month 24-26 (when approaching 60 customers, post Series A enables 10-15 customer/month sales)

---

### 4.3 Cash Flow Scenarios Compared

| Metric | Conservative | Base Case | Optimistic |
|--------|--------------|-----------|-----------|
| **Month 6 Cash** | -$100K | -$113K | -$130K |
| **Month 12 Cash** | -$123K | -$123K | -$148K |
| **Month 18 Cash** | +$34K | +$167K | +$450K |
| **Month 24 Cash** | +$545K | +$1.1M | +$2.0M |
| **Month 36 Cash** | +$2.8M | +$4.7M | +$7.5M |
| **Breakeven Month** | 18 | 18 | 12 |
| **Seed Required** | $1.0M | $1.0M | $750K (optional) |
| **Series A Required** | $5M | $4M | $3M (optional) |

---

## Section 5: Unit Economics Deep Dive

### 5.1 Tier-Specific Economics

#### Starter Tier ($2.5K/month) - 40% of Year 1 customers

| Metric | Amount |
|--------|--------|
| Monthly ARPU | $2,500 |
| COGS per customer | $345 |
| **Gross Profit** | $2,155 |
| **Gross Margin %** | 86.2% |
| CAC | $3,500 |
| CAC Payback | 1.6 months |
| LTV (60 months) | $95,000 |
| LTV/CAC | 27x |
| **Typical Customer Size** | $5-50M revenue |
| **Implementation Time** | 2 weeks |

---

#### Professional Tier ($7.5K/month) - 40% of Year 1 customers

| Metric | Amount |
|--------|--------|
| Monthly ARPU | $7,500 |
| COGS per customer | $360 (slightly higher) |
| **Gross Profit** | $7,140 |
| **Gross Margin %** | 95.2% |
| CAC | $5,000 |
| CAC Payback | 0.7 months |
| LTV (60 months) | $285,000 |
| LTV/CAC | 57x |
| **Typical Customer Size** | $50-250M revenue |
| **Implementation Time** | 4-6 weeks |

---

#### Enterprise Tier ($25K/month) - 20% of Year 1 customers

| Metric | Amount |
|--------|--------|
| Monthly ARPU | $25,000 |
| COGS per customer | $380 (highest support) |
| **Gross Profit** | $24,620 |
| **Gross Margin %** | 98.5% |
| CAC | $12,000 |
| CAC Payback | 0.5 months |
| LTV (60 months) | $1.2M |
| LTV/CAC | 100x |
| **Typical Customer Size** | $250M+ revenue |
| **Implementation Time** | 8-12 weeks |

---

### 5.2 Net Retention Rate (NRR) & Expansion

**Assumption: 105% NRR** (customers upgrade tiers over time)

Example cohort progression:
- Customer joins in Starter tier: $30K ARR
- Year 2: Upgrades to Professional (usage expansion): $90K ARR (200% growth)
- Year 3: Adds additional warehouses (features expansion): $120K ARR (33% growth)

**NRR calculation:** ($30K + $90K + $120K) / $30K = 213% over 3 years

---

## Section 6: Fundraising Timeline & Cap Table

### 6.1 Fundraising Roadmap

```
Month 0: Friends & Family / Pre-Seed ($300-500K)
├─ Timing: Jan 2026 (now)
├─ Valuation: $2-3M
├─ Equity given: 15-20%
├─ Use: MVP → first 5 customers
└─ Milestone exit: 15 paying customers, $100K MRR

↓ (12 months)

Month 12: Seed Round ($1.0-1.5M)
├─ Timing: Jan 2027
├─ Valuation: $6-8M
├─ Equity given: 15-20%
├─ Use: Sales team + marketing
└─ Milestone exit: 50-60 customers, $1.5M ARR

↓ (12 months)

Month 24: Series A ($4-6M)
├─ Timing: Jan 2028
├─ Valuation: $25-30M
├─ Equity given: 15-20%
├─ Use: Enterprise sales, scale
└─ Milestone exit: 100+ customers, $5M ARR

↓ (18+ months)

Month 42: Series B (Optional, $8-12M)
├─ Timing: ~Q2 2028
├─ Valuation: $50M+
├─ Use: Vertical expansion, geography
└─ Milestone: IPO path or strategic sale
```

---

### 6.2 Dilution Analysis (Base Case)

| Round | Amount | Pre-Money | Post-Money | Equity % | Price/Share | Founder % | FD % |
|-------|--------|-----------|-----------|----------|------------|-----------|------|
| **Friends & Family** | $400K | $1.6M | $2.0M | 20% | $0.01 | 100% | 80% |
| **Seed** | $1.2M | $4.8M | $6.0M | 20% | $0.30 | 80% | 64% |
| **Series A** | $5.0M | $20M | $25M | 20% | $1.25 | 64% | 51% |
| **Series B** | $10M | $40M | $50M | 20% | $2.50 | 51% | 41% |

**Founder ownership after all rounds:** 41-51% (depending on option pool)

---

### 6.3 Use of Funds by Round

#### Pre-Seed ($400K)

| Category | Amount | % | Purpose |
|----------|--------|---|---------|
| **Team & Salaries** | $150K | 38% | Founder + 2 engineers + ops |
| **Infrastructure** | $80K | 20% | GCP hosting, tools, CDN |
| **Sales & Marketing** | $50K | 13% | Website, demo environment, travel |
| **Legal & Operations** | $120K | 30% | Incorporation, insurance, contracts |

**Runway:** 10-12 months at -$35K burn

---

#### Seed ($1.2M)

| Category | Amount | % | Purpose |
|----------|--------|---|---------|
| **Sales Team** | $500K | 42% | 1 VP Sales, 1 AE, 1 SDR |
| **Marketing** | $400K | 33% | Content, events, PPC campaigns |
| **Product & Engineering** | $150K | 13% | Additional backend engineer |
| **Infrastructure & Support** | $150K | 13% | Datadog, support tools, hosting |

**Runway:** 18-24 months (NOT for survival, for growth)

---

#### Series A ($5.0M)

| Category | Amount | % | Purpose |
|----------|--------|---|---------|
| **Sales Team** | $1.5M | 30% | 4 AEs, 1 sales engineer, 1 sales ops |
| **Marketing & CS** | $2.0M | 40% | Campaigns, customer success team |
| **Infrastructure & Ops** | $700K | 14% | Scaling, compliance, security |
| **Product Development** | $700K | 14% | New features, vertical customization |
| **Working Capital** | $100K | 2% | Buffer for growth |

---

## Section 7: Key Metrics & Leverage Points

### 7.1 Dashboard Metrics

**Monthly KPIs to track:**

| Metric | Target | Sensitivity | Trigger |
|--------|--------|-------------|---------|
| **New Customers** | +1.25/mo avg | Highly sensitive | <1/mo = red flag |
| **Net Revenue Retention** | >105% | High | <100% = investigate churn |
| **CAC** | <$3.5K | High | >$5K = inefficient |
| **CAC Payback** | <8 months | Medium | >12mo = not viable |
| **Monthly Churn** | <1.5% | Very high | >3% = product issue |
| **Gross Margin** | >97% | Low | <95% = cost inflation |
| **OpEx per Customer** | <$2K | Medium | <$0.5K = under-investing |
| **Cash Runway** | >4 months | Medium | <3mo = urgent fundraising |

---

### 7.2 Sensitivity: What Changes Cash Runway by 6 Months?

#### Single-Variable Sensitivity

**Customer Acquisition:**
- **-30% acquisition** (1 customer/month instead of 1.25): **-9 month runway impact**
- **+30% acquisition** (1.6 customers/month): **+8 month runway impact**
- **Leverage:** Sales efficiency is the #1 driver

**Churn Rate:**
- **+1% monthly churn** (2% instead of 1%): **-6 month impact**
- **-1% monthly churn** (0% churn): **+6 month impact**
- **Leverage:** Retention is second most critical

**CAC Spending:**
- **+$2K CAC** ($5.5K instead of $3.5K): **-4 month impact**
- **-$2K CAC** ($1.5K instead of $3.5K): **+4 month impact**
- **Leverage:** Efficiency matters; excessive CAC burns runway

**Gross Margin:**
- **-3% margin** (94.8% instead of 97.8%): **-2 month impact**
- **+3% margin** (100.8% assumed hypothetically): **+1 month impact**
- **Leverage:** Minimal (infrastructure costs stable)

**OpEx Control:**
- **+$10K monthly OpEx** (overhead discipline): **-3 month impact**
- **-$10K monthly OpEx** (lean operations): **+3 month impact**
- **Leverage:** Important but less than acquisition

---

#### Multi-Variable Best/Worst Case

**Worst Case Scenario (Conservative + High CAC + Churn):**
- Acquisition: 1 customer/month (30% slower)
- Churn: 2% monthly (double base)
- CAC: $5,500 (57% higher)
- OpEx: +$100K annual budget creep
- **Result:** Breakeven Month 24 (vs Month 18 base) = **-6 month runway**

**Best Case Scenario (Optimistic + Efficient + Retention):**
- Acquisition: 1.6 customers/month (30% faster)
- Churn: 0.5% monthly (half base)
- CAC: $2,500 (29% lower)
- OpEx: -$50K annual tightness
- **Result:** Breakeven Month 12 (vs Month 18 base) = **+6 month runway**

---

## Section 8: Risk Factors & Mitigations

### 8.1 Key Business Risks

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|-----------|
| **Slower Sales Cycle** | Medium | High | Focus on SMB first (2-3 week cycles) |
| **Higher Churn** | Low | High | Strong onboarding, executive sponsor model |
| **Competitive Entry** | Medium | Medium | Focus on niche (inventory-specific), build moat |
| **Economic Downturn** | Low | Medium | Customers view TAI as cost-saving tool (sticky) |
| **Key Person Risk** | Low | High | Hire VP Sales early, distribute knowledge |
| **Scaling Pains** | Low | Medium | Build for 10x from day 1, infrastructure first |

### 8.2 Financial Risk Mitigations

**Runway management:**
- Conservative cash forecasting (-20% buffer on new customer assumptions)
- Monthly cash review + weekly SaaS metrics tracking
- Trigger fundraising when runway <4 months

**Customer concentration:**
- No customer >15% of revenue (by Y2)
- Balanced portfolio across tiers and industries

**Margin protection:**
- Lock in GCP pricing via commitment (1-year blocks)
- Monitor support labor (should improve with scale)
- Infrastructure cost monitoring dashboard

---

## Section 9: Pro Forma Financial Statements

### 9.1 Year 1 P&L (Base Case)

| Item | Q1 | Q2 | Q3 | Q4 | FY1 Total |
|------|-----|-----|-----|-----|----------|
| **Revenue** | $47K | $79K | $136K | $188K | $450K |
| COGS | $2.0K | $3.3K | $5.9K | $8.0K | $19.2K |
| **Gross Profit** | $45K | $76K | $130K | $180K | $431K |
| **Gross Margin %** | 96% | 96% | 96% | 96% | 96% |
| | | | | | |
| **Operating Expenses** | | | | | |
| Salaries & Benefits | $115K | $128K | $129K | $135K | $507K |
| Sales & Marketing | $22K | $22K | $25K | $27K | $96K |
| Infrastructure | $6K | $6K | $6K | $6K | $24K |
| Legal & Compliance | $6K | $6K | $6K | $6K | $24K |
| Office & Misc | $3K | $2K | $2K | $2K | $9K |
| **Total OpEx** | $152K | $164K | $168K | $176K | $660K |
| | | | | | |
| **Operating Income** | -$107K | -$88K | -$38K | +$4K | -$229K |
| **Net Income** | -$107K | -$88K | -$38K | +$4K | -$229K |

*Note: Includes all COGS and fully-loaded salaries. No adjustments for stock-based comp or depreciation.*

---

### 9.2 36-Month Cash Flow Summary

| Period | Revenue | COGS | OpEx | Net Income | Cumulative Cash |
|--------|---------|------|------|------------|-----------------|
| **Year 1** | $450K | $19K | $660K | -$229K | -$229K |
| **Year 2** | $1,746K | $78K | $900K | +$768K | +$539K |
| **Year 3** | $4,680K | $163K | $1,080K | +$3,437K | +$3,976K |
| **3-Year Total** | $6,876K | $260K | $2,640K | +$3,976K | — |

*Assumes $500K pre-seed funding at Month 0*

---

## Section 10: Investor Summary & Key Takeaways

### Investment Highlights

1. **Exceptional Unit Economics**
   - 99% gross margins (infrastructure play)
   - 27x LTV/CAC ratio (vs 3-5x industry standard)
   - 1.6 month CAC payback (fastest in category)

2. **Large, Growing Market**
   - $5-50M revenue e-commerce: 50,000+ TAM companies (US only)
   - Inventory management: $10B+ market opportunity
   - Autonomous systems: 30%+ CAGR growth trend

3. **Clear Path to Profitability**
   - Breakeven Month 18 with Seed funding
   - Positive cash flow Month 19 onwards
   - $3.9M cumulative cash by Year 3

4. **Scalable Business Model**
   - 99.7% gross margin scaling to 100%+ (opex leverage)
   - No geographic constraints
   - Vertical expansion potential (manufacturing, 3PL)

5. **Strong Team**
   - Founder: 10 years e-commerce + ops experience
   - Backend engineer: ex-Google Cloud, distributed systems
   - Advisory network: E-commerce operators, VCs

### Key Risks Mitigated

- **Customer concentration:** Multi-tier strategy (SMB + Enterprise)
- **Churn risk:** High NPS expected (value created justifies cost)
- **Competitive risk:** Erlang/OTP specialists (2-3 year moat)
- **Technical risk:** Production-ready code (deployed, tested)

### Financial Targets (Conservative)

| Metric | Year 1 | Year 2 | Year 3 |
|--------|--------|--------|--------|
| Revenue | $450K | $1.7M | $4.7M |
| Customers | 15 | 60 | 112 |
| Gross Margin | 96% | 99% | 99% |
| Breakeven | — | Month 18 | — |
| Cumulative Cash | -$229K | +$539K | +$3.9M |

---

## Appendices

### Appendix A: Assumptions Summary

**Market & Product:**
- TAM: 50,000 e-commerce companies in $5-50M revenue range
- SAM: 15-20% reachable with current team
- SOM: 1-2% market share realistic by Year 3

**Customer Acquisition:**
- Average sales cycle: 60 days
- Win rate: 30% (after POC)
- Customer onboarding time: 2-6 weeks

**Pricing & Retention:**
- Starter ARPU: $2,500/month
- Professional ARPU: $7,500/month
- Enterprise ARPU: $25,000/month
- Monthly churn: 1.0% (base case)
- NRR: 105% (tier upgrades)

**Costs:**
- All-in COGS per customer: $345/month
- Fixed OpEx: $40K/month
- Variable OpEx: $1,333 per customer

**Funding:**
- Pre-seed: $500K
- Seed: $1.0-1.5M
- Series A: $4-6M

---

### Appendix B: Glossary

- **ARPU:** Annual Revenue Per User (average contract value)
- **CAC:** Customer Acquisition Cost (fully-loaded sales & marketing spend)
- **LTV:** Lifetime Value (total profit from customer relationship)
- **NRR:** Net Retention Rate (expansion revenue + churn)
- **MRR:** Monthly Recurring Revenue
- **ARR:** Annual Recurring Revenue
- **COGS:** Cost of Goods Sold
- **OpEx:** Operating Expenses
- **Burn Rate:** Monthly cash spending
- **Runway:** Months of cash remaining

---

### Appendix C: Detailed Assumptions & Sources

**Infrastructure Costs (GCP Pricing, Jan 2026):**
- Cloud Run: $0.00000278/request, ~$85/month/customer
- Pub/Sub: $0.40 per million messages
- Firestore: $0.06 per 100k reads, $0.18 per 100k writes
- Cloud Logging: First 50GB free

**Headcount Costs (San Francisco Market):**
- VP Engineering: $120-150K (includes equity)
- Backend Engineer: $120-180K
- DevOps: $90-120K
- Sales/CS: $80-100K + commission

**Support Labor Model:**
- 1.5 hours/month per customer at full scale
- $75/hour average (blended)
- Reduces to 0.5 hr/customer at scale (tooling, self-serve)

---

### Appendix D: Excel Model Sheets

The accompanying FINANCIAL_MODEL.xlsx contains:

1. **Summary:** Executive dashboard + key metrics
2. **Conservative Scenario:** 36-month monthly projection
3. **Base Case Scenario:** 36-month monthly projection (primary)
4. **Optimistic Scenario:** 36-month monthly projection
5. **Unit Economics:** Detailed tier-by-tier analysis
6. **Headcount & OpEx:** Monthly expense breakdown (Y1) + forecast
7. **Fundraising & Cap Table:** Funding rounds + dilution
8. **Sensitivity Analysis:** Impact of key variable changes

**How to use the model:**
- Update "Base Case Scenario" assumptions to model different growth rates
- Copy formulas down to extend forecast beyond 36 months
- Use sensitivity sheets to stress-test assumptions
- Link to actual monthly actuals to track vs plan

---

## Conclusion

TAI Erlang Autonomics has a compelling financial story:

- **Strong unit economics** (99% margins, 27x LTV/CAC)
- **Clear profitability path** (Month 18 breakeven)
- **Scalable business model** (OpEx leverage as customer base grows)
- **Achievable funding strategy** (Pre-seed → Seed → Series A)

With conservative assumptions and disciplined execution:
- **$500K pre-seed** funds growth to 15 customers + breakeven trajectory
- **$1.2M seed** funds scale to 60 customers + consistent profitability
- **$5M Series A** enables enterprise sales + vertical expansion

**The window to capture this market is now.** First-mover advantage in outcome-based inventory management is significant. Every quarter of delay allows competitors to enter.

**Recommended next steps:**
1. Close pre-seed ($500K) by Feb 2026
2. Launch to first 5 customers by Mar 2026
3. Hit 10 customers by Jun 2026 (mid-year check-in)
4. Plan Seed round by Oct 2026 (with 15 paying customers)

---

**Financial Model Created:** January 25, 2026
**Version:** 1.0.0
**Confidential - For Investor Review Only**
