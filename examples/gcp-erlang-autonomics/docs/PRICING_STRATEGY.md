# GCP Erlang Autonomics - Pricing Strategy

**Version**: 1.0.0
**Document Date**: January 2026
**Target Market**: GCP customers (startups to enterprises)
**Strategic Goal**: Become the default autonomic governance layer for GCP, with 1,000+ paying customers by Year 3.

---

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Pricing Model Rationale](#pricing-model-rationale)
3. [Tier Structure](#tier-structure)
4. [Usage Metrics & Per-Action Pricing](#usage-metrics--per-action-pricing)
5. [Economics Model](#economics-model)
6. [Enterprise Pricing](#enterprise-pricing)
7. [GTM Economics](#gtm-economics)
8. [Price Sensitivity Analysis](#price-sensitivity-analysis)
9. [Forecasting Model](#forecasting-model)
10. [Competitive Positioning](#competitive-positioning)
11. [Pricing Psychology](#pricing-psychology)

---

## Executive Summary

**GCP Erlang Autonomics** pricing model combines **subscription-based reliability** (predictable monthly costs) with **usage-based fairness** (pay-for-value via per-action charges). This hybrid approach aligns with customer expectations in the SaaS observability/governance market while capturing value proportional to customer success.

**Key Pricing Decisions**:
- **Hybrid Model**: Subscription base + per-action consumption charges
- **Value-Based**: Tiers defined by governor count, daily signal throughput, and monthly action budget
- **Transparency**: Per-action costs clearly published (builds trust, reduces "surprise bills")
- **Scalability**: Enterprise contracts with annual prepay for predictability

**Expected Year 1 Revenue**: $297,000 MRR → ~$3.6M annual
**Expected Year 3 Revenue**: $1.2M MRR → ~$14.4M annual
**Path to Profitability**: Month 18 (gross margin 70% by Year 2)

---

## Pricing Model Rationale

### Why Hybrid (Subscription + Per-Action)?

**The Core Problem**: Pure SaaS subscriptions misalign incentives. A customer who uses 10 actions/month pays the same as one using 1,000 actions/month. Pure usage-based pricing creates unpredictability and "bill shock" fears.

**The Solution**: Hybrid model balances both concerns:

| Model | Pros | Cons | Alignment |
|-------|------|------|-----------|
| **Pure Subscription** | Predictable revenue, simple billing | Customers worry about "overage surprises" | ❌ Misaligned: we want heavy usage |
| **Pure Usage-Based** | Perfect value alignment, transparent | Unpredictable revenue, high churn risk | ⚠️ Partial: customer billing anxiety |
| **Hybrid (Ours)** | Predictable base + fair overage, customer success driven | Slight billing complexity (easily mitigated) | ✅ **Aligned: subscription foundation + usage rewards** |

**Our hybrid approach**:
- **Subscription Base**: Monthly tier fee ($99–$999) covers baseline signal processing and action budget
- **Overage**: Per-action charges only if customers exceed tier limits (transparent, published pricing)
- **Customer Incentive**: Scale confidently knowing there's a cap; pay only for excess

**Why This Wins**:
1. **Predictability**: Enterprise buyers get predictable base costs; startups get $99 entry
2. **Alignment**: We profit when customers take more actions (success metric)
3. **Reduced Churn**: No surprise bills; overage charges are incremental and clearly communicated
4. **Product Feedback**: Usage patterns guide roadmap (which actions do customers need most?)

### Alternative Models Considered

#### 1. **Pure SaaS Subscription** ($99/mo – $5,000/mo)
```
Pros:
- Simplicity in billing
- Predictable MRR
- Easier to explain to enterprises

Cons:
- Doesn't capture customer success (heavy users pay same as light users)
- Creates resentment if customer hits limits
- Revenue opportunity lost on power users
- Disconnects us from actual value delivered

Decision: ❌ Rejected. Leaves $X revenue on the table and creates misalignment.
```

#### 2. **Per-Incident Pricing** ($50 per incident detected/resolved)
```
Pros:
- Perfect alignment: more problems = more revenue
- No "overage shock" (pay for actual resolution)
- Works for reactive use cases

Cons:
- Perverse incentive: we profit from customer problems (not helping prevent them)
- Customers fear billing based on incidents (hard to forecast)
- Doesn't reward prevention (our core value prop)
- Competitors won't adopt (bad optics)

Decision: ❌ Rejected. Bad optics and misaligned incentives.
```

#### 3. **Per-Prevented-Dollar Pricing** ($X% of cost savings, e.g., 10%)
```
Pros:
- Perfect ROI alignment: we capture % of savings we generate
- Highest possible enterprise deal sizes
- Customers only pay if they see value

Cons:
- Requires measuring customer savings (hard, customer-controlled)
- Long sales cycle (need ROI models)
- Unpredictable revenue (customers control savings narrative)
- No baseline revenue guarantee
- Difficult to implement in contracts

Decision: ⚠️ Considered for enterprise tier only (future expansion).
```

#### 4. **Freemium + Premium** (Free tier capped at 1 governor)
```
Pros:
- Massive adoption (free tier pulls in startups)
- Land-and-expand (free → paid upgrade)
- Network effects (more users → more use cases)

Cons:
- Support burden (free users need help too)
- Churn rate from free → paid (often 2–5%)
- Cannibalization (some would pay, won't with free tier)
- Our market (autonomic governance) isn't viral (B2B, not consumer)

Decision: ⚠️ Not now. Revisit at 5,000+ user acquisition.
```

### Selected Model: Hybrid (Subscription + Per-Action)

**Final Choice**: Subscription base + transparent per-action overage charges.

**Why it wins**:
- ✅ Predictable MRR (subscription base)
- ✅ Aligns with customer success (usage rewards)
- ✅ Transparent pricing (builds trust)
- ✅ Enterprise-friendly (annual contracts with prepay discounts)
- ✅ Scalable (supports freemium expansion later)

---

## Tier Structure

### Tier 1: Startup ($99/month)

**Target Customer**: Early-stage startups, single GCP project, learning/evaluation phase

**Included**:
- **Governors**: 1 active governor (covers 1 GCP project)
- **Signal Processing**: Up to 100 signals/day (≈3K signals/month)
  - Includes CPU, Memory, Latency, Error Rate, Traffic signals
- **Action Budget**: 10 actions/month included
  - Example: 10 scale-up events, or 5 throttles + 5 restarts
- **Support**: Email support (24–48 hour response)
- **Integrations**: Cloud Monitoring, Cloud Logging, Pub/Sub (native GCP)
- **Audit Trail**: Receipt ledger in BigQuery (read-only, 90-day retention)
- **SLA**: 99.5% uptime (best-effort, no SLA credit)

**Overage Pricing**:
- Additional signals beyond 100/day: $0.001/signal (billed monthly)
  - Example: 150 signals/day = 1,500 excess signals = $45/month overage
- Actions beyond 10/month: Priced per action (see Usage Metrics section)

**Ideal Use Case**:
- Single microservice (3–5 instances)
- Learning how autonomous governance works
- Non-production or staging environment
- Startup getting product-market fit

**Expected Customers**: 70% of Year 1 cohort (price-sensitive, early stage)

---

### Tier 2: Growth ($499/month)

**Target Customer**: Series A companies, multi-service architecture, moving to production

**Included**:
- **Governors**: 3 active governors (covers 3 GCP projects or regions)
  - Example: separate governors for API, web, backend services
- **Signal Processing**: Up to 10,000 signals/day (≈300K signals/month)
  - Higher throughput for complex multi-service monitoring
- **Action Budget**: 1,000 actions/month included
  - Covers typical operational cadence (scale-ups, rollbacks, throttles)
- **Support**: Slack integration + priority support (4-hour response SLA)
- **Integrations**: Cloud Monitoring, Cloud Logging, Pub/Sub, Cloud Trace
- **Advanced Features**:
  - Custom policy packs (write SPARQL rules for domain-specific logic)
  - Receipt ledger: 1-year retention with query analytics
  - Audit export (monthly)
- **SLA**: 99.9% uptime with SLA credits (1% month credit if SLA missed)

**Overage Pricing**:
- Additional signals beyond 10K/day: $0.0005/signal (50% discount vs Startup tier)
  - Example: 15K signals/day = 5K excess signals = $75/month overage
- Actions beyond 1K/month: Priced per action (see Usage Metrics section)
- Additional governor beyond 3: $199/governor/month (bulk purchase)

**Ideal Use Case**:
- Series A company (10–50 engineers)
- Multi-service production system (5–20 services)
- Operating at scale (100K–1M GCP API calls/day)
- Integrating with team workflows (Slack alerts)

**Expected Customers**: 25% of Year 1 cohort (fastest growth segment, efficient operations)

---

### Tier 3: Enterprise (Custom Pricing)

**Target Customer**: Post-Series B+ companies, multi-region, compliance-sensitive, high-touch needs

**Included**:
- **Governors**: Unlimited (typically 5–50 depending on deployment)
  - Global multi-region deployments
  - Separate governors per environment (dev, staging, prod)
- **Signal Processing**: Unlimited signals
- **Action Budget**: Unlimited actions
- **Support**: Dedicated support engineer + 24/7 on-call SLA
- **Advanced Features**:
  - Multi-tenant isolation (separate audit trails per customer)
  - Custom retention policy (audit trail: 3+ years)
  - SPARQL query optimization (for large deployments)
  - Custom integrations (Datadog, Splunk, Prometheus exporters)
  - Role-based access control (RBAC) with SSO
- **SLA Guarantees**: 99.99% uptime with financial credits
  - 0.1% refund per 0.01% downtime below SLA
  - 1-minute incident response (paging on-call)

**Pricing Models** (Choose One):
1. **Annual Prepay**: $X,000/year (20% discount vs monthly equiv.)
   - Typical: $50K–$500K/year depending on deployment scale
   - Payment: Annual upfront or quarterly invoicing
2. **Usage-Based (Volume Discount)**:
   - Overage actions: $0.005 per action (vs $0.01–0.10 in starter tiers)
   - 50% discount at 10K actions/month, 60% at 50K actions/month
3. **Blended**: $X base + overage actions at discounted rate
   - Example: $30K base + actions at $0.003/action above 5K/mo

**Custom Negotiations** (Enterprise Sales):
- Volume discounts: 10–50% based on projected annual spend
- Multi-year contracts: 15–25% discount for 2–3 year commit
- Performance pricing: 10% reduction if SLA uptime > 99.99%
- Bundling: Add-ons (e.g., dedicated account manager: +$5K/mo)

**Ideal Use Case**:
- Post-Series B+ (50–1,000+ engineers)
- Multi-region GCP deployments (10+ active governors)
- High compliance requirements (HIPAA, FedRAMP, SOC 2)
- Requires enterprise integrations and dedicated support

**Expected Customers**: 5% of Year 1, 15% of Year 3 (highest revenue contribution)

---

## Usage Metrics & Per-Action Pricing

This section defines the cost of actions taken by the Governor, enabling transparent overage pricing.

### Action Categories & Pricing

#### **Category A: Low-Impact Operational Actions** ($0.01–$0.02 per action)

These are lightweight, non-destructive operations that improve efficiency without risk.

| Action | Price | Rationale | Example |
|--------|-------|-----------|---------|
| **Cost Circuit Breaker: Throttle** | $0.01 | Minimal impact (request limiting) | Rate-limit to 50 req/s to control costs |
| **Backlog Pressure Valve: Queue Shed** | $0.01 | Drops non-critical messages (recoverable) | Drop 10% of telemetry to reduce latency spikes |
| **Cost Circuit Breaker: Load Shed** | $0.05 | Reduces workload (temporary, predictable) | Shed 20% traffic to background queue |
| **Autoscaling: Scale Up** | $0.02 | Increases capacity (preventive, standard ops) | Add 2 instances to handle traffic spike |

**Rationale**: These actions are **preventive** (stopping problems before they happen) and **reversible** (can be undone quickly). We charge low prices because:
- Customers benefit immediately (prevented incident > cost of action)
- High volume of these actions is good (shows active governance)
- Low infrastructure cost to execute (lightweight API calls)

---

#### **Category B: Medium-Impact Corrective Actions** ($0.10–$0.25 per action)

These actions fix active problems but may have temporary customer impact.

| Action | Price | Rationale | Example |
|--------|-------|-----------|---------|
| **Autoscaling: Scale Down** | $0.02 | Reduces costs (standard ops, automatic) | Remove 2 instances when traffic drops |
| **Container: Restart Service** | $0.10 | Recovers from transient failures | Restart misbehaving API service |
| **Container: Blue-Green Deploy** | $0.15 | Safe deployment (zero-downtime, standard) | Shift traffic to new version, roll forward |
| **Cost Circuit Breaker: DB Connection Limit** | $0.08 | Prevents resource exhaustion | Limit DB connections to 100 to prevent pool exhaustion |

**Rationale**: These actions **correct** active problems and may cause brief interruption. We charge medium prices because:
- Customers explicitly benefit (problem fixed)
- Minor user impact (temporary degradation)
- Prevents larger incidents (prevents cascading failures)
- Infrastructure cost is moderate (API calls + state changes)

---

#### **Category C: High-Impact Recovery Actions** ($0.50–$1.00 per action)

These are serious interventions that typically happen during incidents. High cost reflects the impact and should be rare in healthy systems.

| Action | Price | Rationale | Example |
|--------|-------|-----------|---------|
| **Failover: Regional Failover** | $0.50 | Critical recovery (worst-case scenario) | Failover from us-central1 to us-east1 |
| **Deploy: Automated Rollback** | $0.50 | Reverts bad deployment (incident response) | Rollback to previous stable version |
| **Circuit: Emergency Isolation** | $0.75 | Quarantines bad service (nuclear option) | Isolate compromised service to prevent spread |
| **Infrastructure: Force Restart Cluster** | $1.00 | Severe recovery (cluster-wide intervention) | Force restart GKE cluster (rare, high impact) |

**Rationale**: These actions **recover** from major incidents and have significant customer impact. We charge high prices because:
- Should be **rare** in well-designed systems (high cost incentivizes prevention)
- Customer impact is significant (downtime, data loss risk)
- Requires manual verification post-incident
- Prevents business-critical outages (ROI is enormous)

---

### Signal Processing Pricing (Included in Tier, Overages Defined)

Signals are monitored metrics that trigger governance decisions. Pricing is transparent:

**Tier 1 (Startup)**: 100 signals/day included
- Excess: $0.001/signal/month (or $0.00003 per signal for granular billing)
- Example: 150 signals/day × 30 days = 4,500 signals/month overage = $4.50/month

**Tier 2 (Growth)**: 10,000 signals/day included
- Excess: $0.0005/signal/month (50% discount vs Startup)
- Example: 15,000 signals/day × 30 days = 150K signals/month, 5K excess = $2.50/month

**Tier 3 (Enterprise)**: Unlimited signals

**Signal Types** (all counted equally):
- CPU % / Memory % / Disk I/O
- Latency (p50, p95, p99)
- Error rate / Request rate
- Custom SPARQL-evaluated signals (calculated from Cloud Monitoring)

---

## Economics Model

This section models the business economics to ensure profitability and scalability.

### Cost Structure

#### **COGS (Cost of Goods Sold) per Tier**

| Cost Category | Startup | Growth | Enterprise | Notes |
|---------------|---------|--------|------------|-------|
| **GCP Infrastructure** | $15/mo | $45/mo | $200+/mo | Firestore, BigQuery, Pub/Sub, Cloud Run |
| **Support** | $5/mo | $25/mo | $100+/mo | Engineering time (email vs Slack vs dedicated) |
| **Payment Processing** | $9.90 | $49.90 | $0 (invoiced) | Stripe fees (10% of subscription, 0 for enterprise) |
| **Third-Party Services** | $2/mo | $3/mo | $5/mo | Monitoring, observability, external APIs |
| **Allocated Overhead** | $10/mo | $25/mo | $75/mo | R&D, product, legal, HR (scaled per customer) |
| **Total COGS** | **$41.90** | **$147.90** | **$380+** | |
| **Gross Margin %** | 58% | 70% | 85%+ | Margins improve at scale |

**COGS Assumptions**:
- **GCP Infrastructure**: Cloud Run ($0.15/hour), Firestore (read/write ops), BigQuery (storage + queries)
- **Support**: Email (1 hour/customer/month) costs ~$5; Slack + priority (8 hours) costs ~$25; Dedicated (40+ hours) costs ~$100+
- **Payment Processing**: Stripe charges 2.2% + $0.30 per transaction
- **Allocated Overhead**: $500K/year operational costs ÷ 1,000 customers = $500/customer/year = $41.67/customer/month

**Why Gross Margin Scales**:
1. **Infrastructure**: Nearly fixed cost (Firestore/BigQuery don't scale linearly with customer count)
2. **Support**: Automation (self-serve docs, knowledge base) reduces per-customer support load
3. **Payment Processing**: No charge for enterprise (invoiced directly)
4. **Overhead**: Spreads across more customers

---

### Customer Acquisition Economics

#### **CAC (Customer Acquisition Cost)**

**Assumption Model**:
- **Sales & Marketing Budget Year 1**: $400K
  - Digital marketing (GCP partners, blogs, events): $150K
  - Sales engineer (1 FTE, $150K all-in): $150K
  - Content/community: $100K
- **Expected Year 1 Customers**: 150 total
  - CAC = $400K ÷ 150 = **$2,667 per customer**

**CAC Payback Period**:
- **Startup Tier ($99/mo, 58% margin)**: $99 × 0.58 = $57.42/mo contribution
  - Payback = $2,667 ÷ $57.42 = **46 months** (unsustainable)
  - ⚠️ Can't acquire Startup-only customers at this CAC
  - Solution: Aim for 20% Startup, 60% Growth, 20% Enterprise mix

- **Growth Tier ($499/mo, 70% margin)**: $499 × 0.70 = $349.30/mo contribution
  - Payback = $2,667 ÷ $349.30 = **7.6 months** ✅ (acceptable)

- **Enterprise Tier ($150K/year, 85% margin)**: $150K × 0.85 = $127.5K/year = $10.6K/month contribution
  - Payback = $2,667 ÷ $10.6K = **3 weeks** ✅ (exceptional)

**CAC Payback Strategy**:
- Focus enterprise sales (3-week payback)
- Drive Growth tier adoption (7.6-month payback acceptable if LTV is high)
- Use Startup tier as learning/evaluation (accept poor payback as investment in future expansion)

---

#### **LTV (Lifetime Value)**

**Assumptions**:
- **Average Customer Lifespan**: 36 months (3 years) before churn or upgrade
- **Annual Churn Rate**:
  - Startup: 40% (experimental, learn-and-leave)
  - Growth: 15% (expansion becomes priority)
  - Enterprise: 5% (sticky, integrations)
- **Expansion Revenue**:
  - Startup → Growth upgrade: 30% of Startups, +$400/mo increase
  - Growth → Enterprise upgrade: 40% of Growth, +$8K/mo increase
  - Within-tier expansion: +5% per year (more governors, higher throughput)

**LTV Calculation** (Growth Tier Example):

```
Base LTV (no churn, no expansion):
  $499/mo × 36 months = $17,964
  Less COGS: $17,964 - (36 × $147.90) = $12,280

Adjusted for 15% churn and expansion:
  Year 1: 12 months × $499 = $5,988
  Year 2: 10.2 months × $499 × 1.05 (expansion) = $5,340
  Year 3: 8.7 months × $499 × 1.10 (expansion) = $4,764
  Total Revenue: $16,092

  Less COGS: $16,092 - (30.9 months × $147.90) = $11,462

  LTV/CAC Ratio = $11,462 ÷ $2,667 = 4.3x ✅ (healthy; target is 3x+)
```

**LTV Insights**:
- Startup tier: 2.1x LTV/CAC (marginal, focus on upgrade path)
- Growth tier: 4.3x LTV/CAC (excellent, target tier)
- Enterprise tier: 15x+ LTV/CAC (exceptional, pursue aggressively)
- **Overall target**: Maintain 3.5x+ LTV/CAC ratio

---

### Unit Economics Summary

| Metric | Startup | Growth | Enterprise | Target |
|--------|---------|--------|------------|--------|
| Monthly Price | $99 | $499 | $12.5K | |
| COGS | $41.90 | $147.90 | $380 | <50% of revenue |
| Gross Margin % | 58% | 70% | 85% | >70% |
| CAC | $2,667 | $2,667 | $2,667 | Shared across tiers |
| CAC Payback | 46 mo | 7.6 mo | 3 weeks | <12 months |
| LTV/CAC | 2.1x | 4.3x | 15x | >3x |

**Implication**: Profitability depends on **tier mix**. Target: 20% Startup, 60% Growth, 20% Enterprise.

---

## Enterprise Pricing

Enterprise customers require customized contracts, SLAs, and integrations. This section covers negotiation strategies and pricing models.

### Typical Enterprise Profile

| Attribute | Value |
|-----------|-------|
| **Annual GCP Spend** | $500K–$10M+ |
| **Number of Governors Needed** | 5–50 |
| **Monthly Actions** | 5K–100K+ |
| **Support Level** | Dedicated account manager + on-call SLA |
| **Integration Needs** | Datadog, Splunk, Prometheus, custom SPARQL |
| **Compliance** | SOC 2, HIPAA, FedRAMP |
| **Contract Length** | 2–3 years |

### Enterprise Pricing Models

#### **Model 1: Annual Prepay (Recommended for most deals)**

**Structure**:
```
Base Fee (annual prepay): $X
Included: Unlimited governors, signals, actions, 24/7 support

Example Deal (Mid-Market):
- 10 governors × $5K/governor/year = $50K
- Unlimited signals/actions
- Dedicated support engineer
- Annual prepay discount: 20% = -$10K
- **Total Annual Contract Value (ACV): $40K**
```

**Pricing Guide by Scale**:

| Deployment Size | Governors | Estimated ACV | Monthly Equivalent |
|-----------------|-----------|----------------|-------------------|
| **Regional** | 3–5 | $30K–$50K | $2.5K–$4.2K |
| **Multi-Regional** | 5–15 | $50K–$150K | $4.2K–$12.5K |
| **Global** | 15–30 | $150K–$300K | $12.5K–$25K |
| **Mega-Enterprise** | 30+ | $300K–$1M+ | $25K–$83K+ |

**Negotiation Strategy**:
- Start: $X base price (anchored on governor count)
- Customer asks: "Can you give us a discount?" (always)
- Response: "Sure. If you commit to 2 years, we can offer 20% off. If 3 years, 25% off."
- Result: Lock in long-term revenue + improve LTV

---

#### **Model 2: Usage-Based with Volume Discounts**

**Structure**:
```
Base Fee: $10K–$50K/month (covers support + admin)
Per-Action Charge: Negotiated discount based on volume

Example Deal (Heavy User):
- Base: $30K/month
- Monthly Actions: 50K (exceeds 5K included)
- Overage: 45K actions × $0.003/action (60% discount vs Growth tier $0.01) = $135
- **Total: $30,135/month**
```

**Volume Discount Tiers**:

| Monthly Actions | Per-Action Price | Annual Spend at 100K actions/month |
|-----------------|------------------|-----------------------------------|
| <5K | $0.01 | $1.2M (list price) |
| 5K–10K | $0.008 | $960K (20% discount) |
| 10K–50K | $0.006 | $720K (40% discount) |
| 50K+ | $0.003 | $360K (70% discount) |

**When to Use**: For customers with highly variable usage patterns (e.g., batch processing jobs spike in evening).

---

#### **Model 3: Blended Pricing (Hybrid)**

**Structure**:
```
Base Fee: Covers X governors + Y actions/month
Overage: Per-action charge at negotiated rate

Example Deal:
- Base: $50K/year (includes 5 governors, 2K actions/month)
- Typical usage: 3K actions/month
- Overage: 1K actions × $0.005 = $5/month (rare)
- **Total: $50K/year + overages**
```

**Advantages**:
- Predictable base revenue
- Flexibility for customer variable costs
- Easy to forecast (base is known; overage is minor)

---

### Enterprise Add-Ons

Enterprises often request additional features beyond base service. Price these separately:

| Add-On | Annual Price | Notes |
|--------|--------------|-------|
| **Dedicated Account Manager** | +$5K–$10K/year | 1:1 support, quarterly business reviews |
| **Custom Integrations** | +$10K–$50K | Datadog, Splunk, Prometheus, Elasticsearch export |
| **Extended Audit Trail** | +$5K–$15K/year | 3–7 year retention instead of 1 year |
| **Multi-Tenant Isolation** | +$10K/year | Separate audit trail per customer (for resellers) |
| **SPARQL Query Optimization** | +$5K–$25K | Custom index design, query tuning for large deployments |
| **SLA Credits** | Included | 99.99% SLA with 1% monthly credit if breached |
| **Single Sign-On (SSO)** | +$2K/year | SAML 2.0, Okta, Azure AD integration |

---

### Enterprise Negotiation Playbook

**Scenario 1: "Your competitor costs $5K/month"**
- Response: "Which competitor? [Listen]. Our pricing reflects our value:
  - Autonomous governance (vs manual alerts)
  - Cryptographic audit trail (vs logs)
  - Deterministic receipt verification (vs guess-and-check)
  - If you want to test-drive, we offer a $X pilot tier."

**Scenario 2: "Can you do volume discounts?"**
- Response: "Absolutely. For 2-year annual prepay, we offer 20% discount. For 3 years, 25%. What works for your budget cycle?"

**Scenario 3: "We need custom SPARQL rules"**
- Response: "That's our strength. We can do a Statement of Work (SOW) to build custom governance policies. Typical cost: $5K–$25K depending on complexity. Want to schedule a technical discovery?"

**Scenario 4: "What about per-incident pricing instead?"**
- Response: "We deliberately don't do that because we want to align on *preventing* incidents, not profiting from them. Our model: you pay a base fee + low per-action costs. That incentivizes us to make sure every action is high-value."

---

## GTM Economics

### Sales & Marketing Budget Allocation

**Year 1 Budget: $400K**

| Channel | Allocation | Expected Leads | Expected Customers | CAC |
|---------|------------|-----------------|-------------------|-----|
| **Content Marketing** | $100K | 200 | 20 | $5K |
| *Blogs, SEO, case studies, webinars, podcasts* | | | | |
| **Paid Digital (Google, LinkedIn)** | $80K | 150 | 25 | $3.2K |
| *Google Ads on "GCP governance", "autonomic systems"* | | | | |
| **Partner Channel (GCP)** | $60K | 100 | 15 | $4K |
| *GCP Partner referral, marketplace placement* | | | | |
| **Sales Engineer (1 FTE)** | $150K | 80 | 60 | $2.5K |
| *Direct outreach to Series A/B companies on GCP* | | | | |
| **Events & Community** | $10K | 30 | 5 | $2K |
| *GCP conferences, Rust community, open source sponsorships* | | | | |
| **Ops/Tooling** | $0 | — | — | — |
| | **$400K** | **~560 leads** | **~125 customers** | **$3.2K avg** |

**Conversion Funnel**:
- Leads to SQL (Sales Qualified Lead): 30% = 168 SQLs
- SQL to Customer: 75% = 126 customers
- Expected mix: 25 Startup, 75 Growth, 26 Enterprise (WIP)

---

### Sales Commission Structure

**For Sales Engineers / Account Executives**:

| Customer Tier | Commission | Example ACV | Commission Amount |
|---------------|------------|-------------|-------------------|
| **Startup** | 15% | $1,188/year | $178 |
| **Growth** | 20% | $5,988/year | $1,198 |
| **Enterprise** | 10% | $40,000–$100,000/year | $4,000–$10,000 |

**Rationale**:
- **Startup**: Low commission (hard to close, low value) but still incentivized
- **Growth**: Highest commission % (sweet spot: good value, closeable)
- **Enterprise**: Lower % (large deals are strategic; motivation is career growth, not commission)

**Annual Rep Pay**:
- Base salary: $120K
- On-target earnings (OTE): 120K + (26 Growth deals × $1,198) + (5 Enterprise deals × $6K) = $162K

---

### Marketplace Revenue Share

**GCP Marketplace Listing** (Recommended):

- **Commission to GCP**: 30% of ACV (GCP Marketplace standard)
- **Our Take**: 70% of list price

**Example**:
```
List Price: $499/month = $5,988/year
GCP Commission (30%): -$1,796
Our Take: $4,192/year (70%)
```

**Why Use Marketplace**:
- ✅ Discoverability (customers searching "governance" in GCP Console)
- ✅ Easy procurement (attaches to GCP billing, no separate vendor setup)
- ✅ Trust (GCP Partner badge)
- ❌ 30% revenue loss (should be offset by higher volume)

**Marketplace Strategy**:
- List Startup ($99) + Growth ($499) tiers
- Enterprise goes through direct sales (bypass marketplace, avoid 30% cut)
- Expected split: 70% marketplace, 30% direct

---

### Customer Acquisition Cost (CAC) Payback

**Summary**:

| Tier | ACV | COGS/year | Gross Profit/year | CAC | Payback Period |
|------|-----|----------|------------------|-----|-----------------|
| **Startup** | $1,188 | $503 | $685 | $2,667 | 47 months ❌ |
| **Growth** | $5,988 | $1,775 | $4,213 | $2,667 | 7.6 months ✅ |
| **Enterprise** | $50,000 | $5,000 | $45,000 | $2,667 | 0.7 months ✅ |

**Implication**:
- Can't acquire Startup-only customers profitably
- Focus sales on Growth + Enterprise
- Use Startup as **free trial equivalent** (acquisition cost is relationship investment for future expansion)

---

### Expansion Revenue

Once acquired, customers expand naturally. Capture this:

| Expansion Type | Frequency | Estimated Lift | Example |
|----------------|-----------|-----------------|---------|
| **Governor Upgrade** | 40% of customers | +$199–$500/mo | Growth customer adds 2 governors (ops in 2nd region) |
| **Tier Migration** | 30% of customers | +$300–$8K/mo | Startup → Growth as company scales |
| **Add-Ons** | 50% of customers | +$100–$500/mo | Custom SPARQL rules, SSO, extended retention |
| **Annual Usage Growth** | 100% of customers | +5–10%/year | More signals, more actions as system grows |

**Expansion Revenue Impact**:
- Year 1: MRR = acquisition only
- Year 2: MRR = Year 1 cohort (expanded) + Year 2 acquisition
- Year 3: MRR = Years 1–2 cohorts (expanded) + Year 3 acquisition

**Example** (Growth Tier Customer):
```
Year 1: $499/month
Year 2: $499 × 1.15 (expansion) = $574/month (add 1 governor, +10% usage)
Year 3: $574 × 1.10 (expansion) = $632/month (add SSO, custom rules)
3-year revenue: $499 + $574 + $632 = $1,705/month customer (71% lift)
```

---

## Price Sensitivity Analysis

This section explores "what-if" scenarios to understand pricing elasticity and revenue impact.

### Scenario 1: Lower Startup Tier to $49/month

**Current**: $99/month
**Proposed**: $49/month (50% reduction)

**Expected Impact**:

| Metric | Current | Proposed | Change |
|--------|---------|----------|--------|
| **Acquisition Volume** | 20 Startup customers | 40 Startup customers | +100% |
| **Gross Margin %** | 58% | 25% | -33 pts |
| **Monthly Revenue (Year 1)** | 20 × $99 = $1,980 | 40 × $49 = $1,960 | -1% |
| **Unit Margin (Year 1)** | $57/month | $12/month | -79% |
| **LTV/CAC** | 2.1x | 0.4x | ❌ Broken |

**Recommendation**: ❌ **Don't lower to $49**. Math doesn't work:
- Acquisition volume doubles (good)
- But margin per customer drops 79% (bad)
- Net revenue is flat
- LTV/CAC collapses to 0.4x (unsustainable)

**Alternative**: Keep $99, add $49 "Micro" tier with heavy limitations (1 governor, 10 signals/day, 1 action/month). Attracts price-sensitive users without cannibalizing Growth tier.

---

### Scenario 2: Per-Signal Pricing vs Per-Action

**Current Model**: Subscription base + per-action overages
**Alternative**: Subscription base + per-signal overages (no per-action charge)

**Comparison**:

| Customer Usage | Current Model | Per-Signal Model | Winner |
|----------------|---------------|-----------------|--------|
| Light (100 signals/day, 5 actions/mo) | $99 base | $99 base | Tie |
| Moderate (1K signals/day, 100 actions/mo) | $499 base + $4/mo (overage) | $499 base + $1.50/mo (signal overage) | **Per-Signal** (lower customer cost) |
| Heavy (10K signals/day, 1K actions/mo) | $999 base + $100/mo (overage) | $999 base + $15/mo (signal overage) | **Per-Signal** (much lower) |

**Analysis**:

**Pros of Per-Signal Model**:
- ✅ Cheaper for customers (lower perceived overage)
- ✅ Transparent (signals are easier to understand than abstract "actions")
- ✅ Better alignment (more signals = more money for us)

**Cons of Per-Signal Model**:
- ❌ Misaligned incentives (we profit from noisy signals, not intelligent decisions)
- ❌ Encourages customer behavior (more signals = higher bill, might reduce signal volume artificially)
- ❌ Harder to forecast revenue (signal volume is more variable than action count)
- ❌ Doesn't capture "aha" value (taking an action is where customer sees value, not receiving a signal)

**Recommendation**: ✅ **Stay with per-action model**. It better aligns our incentives:
- We profit when customers **take actions** (governance is working)
- Encourages us to reduce false positives (fewer unnecessary actions)
- Customers trust the model (paying for outcomes, not inputs)

---

### Scenario 3: Bundle Deal Pricing

**Current Model**: Tier-based pricing (Startup, Growth, Enterprise)
**Proposal**: "Governance Suite" bundle

```
Bundle Option: All 3 Governors + Unlimited Signals + 500 Actions/month
Price: $799/month (vs $99 × 3 = $297 if buying tiers separately)

Bundle Discount: 0% (actually a 169% premium!)
```

**Analysis**:

| Customer | Saves If Bought Separately | Pays Bundle Price | Bundle Margin |
|----------|--------------------------|------------------|----------------|
| Power user with 3 governors | $297/mo | $799/mo | +169% premium |
| Multiple teams sharing 1 governor | $99/mo | $799/mo | +708% premium |

**Cannibalization Risk**: HIGH ❌
- Why pay $99 for Growth when bundle is "only" $799?
- Customers might bundle when they don't need it
- Reduces our per-governor revenue

**Recommendation**: ❌ **Don't bundle**. Instead:
- Keep à la carte tiering (per-governor charges for multi-governor deployments)
- Build in "volume discounts" for multiple governors:
  - 1–3 governors: list price
  - 4–10 governors: 10% discount per governor
  - 10+ governors: 20% discount per governor
- This incentivizes scale without bundling

---

### Scenario 4: Annual vs Monthly Pricing

**Current**: Monthly billing ($99/mo, can cancel anytime)
**Proposal**: Annual prepay ($1,100/year = 8% discount, locks customer in)

**Economics**:

| Metric | Monthly | Annual Prepay | Impact |
|--------|---------|---------------|--------|
| **Upfront Revenue** | $99 | $1,100 | +$1,001 one-time |
| **Churn Rate** | 5%/month | 2%/year | ✅ Reduces churn 60% |
| **SLO Reserves** | $0 | $100 (refund risk if downtime) | +$100 cost |
| **Customer Stickiness** | Low | High | ✅ Better LTV |

**Recommendation**: ✅ **Offer both**:
- **Monthly**: $99 (default for risk-averse customers, easier sales)
- **Annual Prepay**: $1,100 ($91.67/mo equivalent, 8% discount)
- **2-Year Prepay**: $2,000 ($83.33/mo equivalent, 16% discount) — Enterprise only
- **3-Year Prepay**: $2,750 ($76.39/mo equivalent, 23% discount) — Enterprise only

This balances:
- ✅ Simplicity (monthly) for unsure buyers
- ✅ Revenue certainty (annual) for confident buyers
- ✅ Long-term locks (3-year) for strategic accounts

---

## Forecasting Model

This section projects revenue, customer count, and profitability for Years 1–3.

### Year 1 (Months 1–12)

**Acquisition Strategy**:
- Heavy focus on Growth tier (best CAC payback)
- Small Startup tier for volume
- Early Enterprise pilots for case studies

**Month-by-Month Projection**:

| Month | Growth MRR | Growth Count | Startup MRR | Startup Count | Enterprise MRR | Enterprise Count | **Total MRR** |
|-------|-----------|------------|------------|---------------|----------------|-----------------|-------------|
| 1 | $0 | 0 | $0 | 0 | $0 | 0 | $0 |
| 2 | $499 | 1 | $99 | 1 | $0 | 0 | $598 |
| 3 | $1,497 | 3 | $198 | 2 | $0 | 0 | $1,695 |
| 4 | $2,995 | 6 | $396 | 4 | $0 | 0 | $3,391 |
| 5 | $4,990 | 10 | $495 | 5 | $2,500 | 1 | $7,985 |
| 6 | $6,985 | 14 | $693 | 7 | $5,000 | 2 | $12,678 |
| 7 | $9,975 | 20 | $891 | 9 | $10,000 | 3 | $20,866 |
| 8 | $12,470 | 25 | $1,089 | 11 | $17,500 | 5 | $31,059 |
| 9 | $14,965 | 30 | $1,287 | 13 | $25,000 | 7 | $41,252 |
| 10 | $17,460 | 35 | $1,485 | 15 | $32,500 | 9 | $51,445 |
| 11 | $19,955 | 40 | $1,683 | 17 | $42,500 | 12 | $64,138 |
| 12 | $22,450 | 45 | $1,881 | 19 | $52,500 | 15 | $76,831 |

**Year 1 Summary**:
- **Total Customers Acquired**: 79 (45 Growth, 19 Startup, 15 Enterprise)
- **Ending MRR**: $76,831
- **Annual Revenue**: $76,831 × 12 = **$921,972**
- **YoY Growth Rate**: N/A (first year)

---

### Year 2 (Months 13–24)

**Acquisition Strategy**:
- Growth tier accelerates (word-of-mouth from Year 1)
- Startup tier grows but deprioritized (low ROI)
- Enterprise becomes primary focus (highest LTV)

**Churn Assumptions**:
- Startup: 40% annual (learn-and-leave)
- Growth: 15% annual (expansion becomes priority)
- Enterprise: 5% annual (sticky)

**Month 13 (Start of Year 2)**:
- Cohort Retention: 45 × 60% = 27 Growth; 19 × 60% = 11 Startup; 15 × 95% = 14 Enterprise
- Base MRR (retained): (27 × $499) + (11 × $99) + (14 × $12,500) = $188,588

| Month | Growth MRR | Growth Count | Startup MRR | Startup Count | Enterprise MRR | Enterprise Count | **Total MRR** |
|-------|-----------|------------|------------|---------------|----------------|-----------------|-------------|
| 13 | $13,473 | 27 | $1,089 | 11 | $175,000 | 14 | $189,562 |
| 14–18 | +2K/mo | +4/mo | +100/mo | +1/mo | +$15K/mo | +1–2/mo | *Linear growth* |
| 19 | $31,937 | 64 | $1,881 | 19 | $365,000 | 29 | $398,818 |
| 20–24 | +2K/mo | +4/mo | +100/mo | +1/mo | +$15K/mo | +1–2/mo | *Linear growth* |
| 24 | $48,905 | 98 | $3,069 | 31 | $575,000 | 46 | $626,974 |

**Year 2 Summary**:
- **Total Customers Acquired**: ~140 new (goal: 80 Growth, 30 Startup, 30 Enterprise)
- **Ending Customer Count**: ~220 total (125 Growth, 50 Startup, 46 Enterprise)
- **Ending MRR**: $626,974
- **Annual Revenue**: ~$4.9M
- **YoY Growth**: 5.3x ✅

---

### Year 3 (Months 25–36)

**Acquisition Strategy**:
- Enterprise focus (highest revenue, stickiest customers)
- Growth tier self-sustaining (product-market fit achieved)
- Startup tier as funnel for future expansion

**Churn & Expansion**:
- Same churn rates
- Expansion revenue: 10–15% annual increase within tiers

| Month | Growth MRR | Growth Count | Startup MRR | Startup Count | Enterprise MRR | Enterprise Count | **Total MRR** |
|-------|-----------|------------|------------|---------------|----------------|-----------------|-------------|
| 25 | $52,412 | 105 | $3,267 | 33 | $630,000 | 50 | $685,679 |
| 26–30 | +2.5K/mo | +5/mo | +150/mo | +2/mo | +$25K/mo | +2/mo | *Accelerating* |
| 31 | $72,085 | 145 | $4,851 | 49 | $875,000 | 70 | $951,936 |
| 31–36 | +3K/mo | +6/mo | +200/mo | +2/mo | +$30K/mo | +2.5/mo | *Accelerating* |
| 36 | $99,470 | 200 | $7,425 | 75 | $1,200,000 | 100 | $1,306,895 |

**Year 3 Summary**:
- **Total Customers Acquired**: ~160 new (goal: 75 Growth, 25 Startup, 60 Enterprise)
- **Ending Customer Count**: ~380 total (200 Growth, 75 Startup, 100 Enterprise)
- **Ending MRR**: $1,306,895
- **Annual Revenue**: ~$11.6M
- **YoY Growth**: 2.4x ✅ (still strong, maturing)

---

### 3-Year Summary

| Metric | Year 1 | Year 2 | Year 3 | Notes |
|--------|--------|--------|--------|-------|
| **Customers Acquired** | 79 | 140 | 160 | Accelerating, then plateauing |
| **Ending Customer Count** | 79 | 220 | 380 | |
| **Ending MRR** | $76,831 | $626,974 | $1,306,895 | 17x growth in 3 years |
| **Annual Revenue** | $0.92M | $4.9M | $11.6M | |
| **YoY Growth Rate** | N/A | 5.3x | 2.4x | Growth trajectory |
| **Gross Margin** | 65% | 70% | 72% | Improves with scale |
| **Operating Margin** | -45% | -5% | +15% | Breakeven at Month 18 |

**Profitability Timeline**:
- **Months 1–12**: Heavy investment (negative margin, expected)
- **Months 13–17**: Approaching breakeven (investments paying off)
- **Month 18**: Breakeven (cumulative revenue = cumulative costs)
- **Month 24+**: Profitable (operating leverage kicks in)

---

## Competitive Positioning

This section compares GCP Erlang Autonomics to competing governance solutions.

### Competitive Landscape

#### **1. DataDog Cost Governance**

| Feature | DataDog | Erlang Autonomics | Winner |
|---------|---------|-------------------|--------|
| **Pricing** | $300+/month base (enterprise negotiation) | $99–$499/month tiers | ✅ **Erlang** (10x cheaper) |
| **Autonomic Actions** | Manual recommendations only | Automated + cryptographically verified | ✅ **Erlang** |
| **Audit Trail** | Logs (queryable, not immutable) | Receipt ledger (tamper-proof, BigQuery) | ✅ **Erlang** |
| **SPARQL Integration** | No (proprietary QL) | Full SPARQL support (standard RDF) | ✅ **Erlang** |
| **GCP-Native** | Partial (multi-cloud focus) | 100% GCP optimized | ✅ **Erlang** |
| **Ease of Setup** | Complex (agent installation) | Native Cloud Run/GKE integration | ✅ **Erlang** |

**Our Positioning**: "DataDog for alerts; Erlang for autonomous action. Same governance intelligence, 1/10th the price."

---

#### **2. AWS Trusted Advisor Premium**

| Feature | AWS Trusted Advisor | Erlang Autonomics | Winner |
|---------|-------------------|-------------------|--------|
| **Pricing** | $100/month | $99–$499/month | **Tie** (similar price) |
| **Autonomic Actions** | No (recommendations only) | Yes (auto-scaling, rollback, failover) | ✅ **Erlang** |
| **GCP Support** | No (AWS-only) | 100% GCP | ✅ **Erlang** |
| **Real-Time Signals** | Weekly scans | Real-time (sub-second) | ✅ **Erlang** |
| **Customization** | Pre-built checks only | Custom SPARQL rules | ✅ **Erlang** |
| **Multi-Cloud** | No | Expandable (future: Azure, on-prem) | ✅ **Erlang** |

**Our Positioning**: "Trusted Advisor meets autonomic systems. We recommend AND execute."

---

#### **3. Azure Advisor (Free)**

| Feature | Azure Advisor | Erlang Autonomics | Winner |
|---------|---------------|-------------------|--------|
| **Pricing** | Free | $99+/month | **Tie** (Azure wins on price) |
| **Autonomic Actions** | No | Yes | ✅ **Erlang** |
| **GCP Support** | No (Azure-only) | 100% GCP | ✅ **Erlang** |
| **Enterprise SLA** | No | Yes (99.99% uptime) | ✅ **Erlang** |
| **Audit Trail** | Limited | Cryptographic receipts | ✅ **Erlang** |

**Our Positioning**: "Azure Advisor + execution layer + cryptographic proof. Governance you can trust."

---

#### **4. Custom In-House Solutions**

| Feature | In-House | Erlang Autonomics | Winner |
|---------|----------|-------------------|--------|
| **Time to Build** | 6–12 months | Day 1 (deploy YAML) | ✅ **Erlang** |
| **Maintenance Burden** | 2–3 FTE/year | Managed by us | ✅ **Erlang** |
| **Cost** | $500K–$2M total | $99–$500/month | ✅ **Erlang** (100x cheaper) |
| **Security/Compliance** | Custom (audit burden) | Built-in (SOC 2, SHACL validation) | ✅ **Erlang** |
| **RDF Standardization** | Unlikely | Full W3C SPARQL compliance | ✅ **Erlang** |

**Our Positioning**: "Build vs. Buy decision is clear. We're 100x cheaper and 6 months faster."

---

### Competitive Strategy

**Target Segments**:
1. **Growth Tier**: Series A startups (evaluating governance, budget-conscious)
   - Competitive threat: Custom in-house solutions, Azure Advisor (free)
   - Win message: "Governance without hiring. Production-ready in 1 day."

2. **Enterprise Tier**: Series B+ companies (need reliable, auditable governance)
   - Competitive threat: DataDog (integrated platform), custom solutions
   - Win message: "Cryptographic autonomy. Every action is verified and immutable."

**Win-Back Strategy** (if lost to DataDog):
- "DataDog excels at observability. We specialize in autonomic action."
- Offer complementary positioning: "Run Erlang + DataDog together. We execute; they monitor."
- Price incentive: "Save $X/month vs DataDog governance module by using Erlang separately."

---

## Pricing Psychology

This section explains pricing strategy from a behavioral economics perspective.

### The $99 Anchor (Startup Tier)

**Why $99 (not $100 or $95)?**

**Psychological Anchoring**:
- **$99** feels like a "real deal" (vs $100, which sounds "round and expensive")
- **Charm Pricing**: Leverages the psychological illusion that $99 is significantly cheaper than $100
  - Studies show: $99 → perceived as ~$50 discount vs $100 (20% mental reduction)
  - Conversion lift: 8–15% vs round prices
- **Precise Number Signal**: $99 signals "we calculated this, not arbitrary"
  - $100 = random round number
  - $99 = "we tested pricing to find sweet spot"

**What We're Signaling**:
- "We're lean startup-like (price matters to us)"
- "We've done pricing science (not a guess)"
- "You get expert-designed pricing, not round-number sales tactics"

**Why Not Lower?**
- $49: Signals "cheap" (quality concern, lowers perceived value)
- $59: Unusual number (confuses customers)
- $89: Feels arbitrary (why not $99?)
- $99: **Sweet spot** (feels fair, signals intelligence)

---

### The $499 Step (Growth Tier)

**Why 5x jump (not 3x or 7x)?**

**Tier Spacing Logic**:
```
Startup: $99   (1 governor, 100 signals/day)
Growth:  $499  (3 governors, 10K signals/day)
         ↑
         5x price jump
         ↓
Feature increase: 3x governors, 100x signals

Ratio: 100x feature increase ÷ 5x price = 20x value ratio
→ Customers feel they're getting 20x value from Growth tier
```

**Psychological Effect**:
- The 5x jump signals: "Growth tier is genuinely different (not incremental)"
- Customers self-select:
  - Cheap? → Buy Startup
  - Need scale? → Growth is "worth the investment"
  - Big player? → Enterprise (custom negotiation)

**Anchoring Impact**:
- If we price Growth at $299: Customers think "premium, must be worth it" (willing to pay)
- If we price Growth at $699: Customers think "too expensive, I'll stick with Startup" (churn risk)
- **$499 is sweet spot**: Feels 5x jump (justified by 100x feature increase)

---

### Annual Prepay Discount Psychology

**Current**: $99/month or $1,100/year (8% discount)

**Why 8% (not 20%)?**

| Discount | Customer Perception | Company Benefit | Recommendation |
|----------|-------------------|------------------|-----------------|
| **2%** | "Why bother?" (not worth committing) | High margin retention | ❌ Too low |
| **8%** | "Okay incentive, saves ~$10/mo" (noticed, not excessive) | Good margin retention + revenue certainty | ✅ **Optimal** |
| **20%** | "Wow, real discount!" (strong incentive) | Revenue certainty but margin loss | ⚠️ Too high (erodes margins) |
| **30%** | "This is the real price" (anchors monthly as inflated) | Maximum revenue certainty but risks monthly pricing power | ❌ Too high |

**Psychology of Annual Prepay**:
- Customers who prepay are **committed** (lower churn, higher LTV)
- 8% discount is enough to feel "special" without eroding margins
- The "savings" (8–10% = ~$12/month) feels meaningful psychologically even though it's small

**Our Discount Tiers**:
- Monthly: $99 (baseline)
- Annual: $1,100 = $91.67/mo (8% discount) ✅
- 2-Year: $2,000 = $83.33/mo (16% discount) ✅ Enterprise discount
- 3-Year: $2,750 = $76.39/mo (23% discount) ✅ Maximum commitment discount

---

### Transparency as Trust Signal

**Why We Publish Per-Action Pricing**:

Most SaaS tools hide usage-based pricing ("Contact sales for overage rates"). We do the opposite:

**Published Pricing**:
```
Cost Circuit Breaker Throttle: $0.01/action
Autoscaling Scale-Up: $0.02/action
Failover Regional Failover: $0.50/action
```

**Why This Builds Trust**:
1. **No Hidden Fees**: Customers see exact overage cost upfront
2. **Signals Confidence**: Only transparent companies hide their pricing
3. **Builds Pricing Authority**: We're willing to stand behind our numbers
4. **Reduces Sales Anxiety**: Customers don't fear surprise invoices

**Psychological Impact**:
- Hidden pricing → Customer paranoia ("How much will this really cost?")
- Published pricing → Customer confidence ("I know exactly what I'm paying")
- Studies show: 23% higher conversion with transparent usage pricing

---

### Freemium Expansion (Future)

**When to Launch Freemium Tier** (once we hit 5,000 users):

**Proposed Free Tier**:
- 1 governor
- 10 signals/day
- 1 action/month
- Email support (best-effort)
- 7-day retention (not 90-day)

**Pricing Psychology**:
- Free tier gets you in the door ("No risk to try")
- Limitations force upgrade decision when they hit limits ("Need scale? Upgrade to Growth")
- Paid tier feels "worth it" (you already experienced free limitations)

**Conversion Rate Target**: 5–10% of free users upgrade to Startup/Growth within 3 months

**Launch Criteria**:
- ✅ 1,000+ Growth/Enterprise customers (proof of product-market fit)
- ✅ Product quality stable (no major bugs, refund risk managed)
- ✅ Support team can handle free-to-paid conversion funnel
- ✅ Marketing team ready for 10–50x acquisition spike

---

### Messaging Framework

**For Each Tier**:

**Startup ($99/month)**
- Headline: "Learn autonomous governance without the enterprise price tag"
- Subheading: "Perfect for evaluating if Erlang is right for your team"
- CTA: "Start Free Trial"
- Psychology: "Low risk. Try it. Upgrade later if you need scale."

**Growth ($499/month)**
- Headline: "Production-grade autonomic governance for Series A companies"
- Subheading: "Scale confidently with automated incident prevention"
- CTA: "Start 14-Day Trial"
- Psychology: "You've product-market fit. Now scale operations."

**Enterprise (Custom)**
- Headline: "Governance with cryptographic proof. For companies that can't afford downtime."
- Subheading: "Dedicated support, unlimited scale, 99.99% SLA"
- CTA: "Request Demo"
- Psychology: "You're big enough to matter. We'll treat you as a partner."

---

## Appendix: Implementation Checklist

### Phase 1: Launch (Month 1)
- [ ] Set up Stripe/billing integration
- [ ] Publish pricing page (website + GCP Marketplace)
- [ ] Create sales collateral (1-pager per tier)
- [ ] Train sales team on messaging + negotiation playbook
- [ ] Monitor conversion funnel metrics

### Phase 2: Optimize (Months 2–6)
- [ ] Analyze CAC by channel (content, paid, partnerships, sales)
- [ ] A/B test messaging (annual vs monthly emphasis)
- [ ] Track churn by cohort (identify at-risk segments)
- [ ] Refine per-action pricing based on customer behavior

### Phase 3: Scale (Months 7–12)
- [ ] Launch GCP Marketplace listing
- [ ] Expand enterprise sales team
- [ ] Develop customer success program (reduce churn)
- [ ] Plan freemium tier launch (Year 2)

### Key Metrics to Monitor
- **CAC by Tier**: Are we acquiring Growth customers efficiently?
- **LTV/CAC Ratio**: Maintain 3x+ (break-even is 1x)
- **Churn Rate**: Target <3%/month (Growth), <1%/month (Enterprise)
- **Expansion Revenue**: Target 5–10% annual uplift per customer
- **Gross Margin**: Target 70%+ (scale operations to achieve)
- **MRR Growth Rate**: Target 15%+ month-over-month (Year 1), 5%+ (Year 2)

---

## Conclusion

**GCP Erlang Autonomics** pricing strategy balances:
- ✅ **Predictability** (subscription base)
- ✅ **Fairness** (transparent per-action pricing)
- ✅ **Scalability** (enterprise customization)
- ✅ **Psychological anchoring** (pricing that builds trust)

**Expected outcomes**:
- Year 1: $0.92M revenue, 79 customers, -45% operating margin (investment phase)
- Year 2: $4.9M revenue, 220 customers, -5% operating margin (approaching breakeven)
- Year 3: $11.6M revenue, 380 customers, +15% operating margin (profitable)
- **LTV/CAC Ratio**: 4.3x (healthy, sustainable growth)

**Success metrics**:
1. Growth tier adoption (target: 60% of revenue)
2. Expansion revenue from existing customers (target: 10%/year)
3. Enterprise contract size (target: $50K–$200K ACV by Year 2)
4. Customer satisfaction (target: NPS 50+, churn <3%/month)

---

**Document Version**: 1.0.0
**Last Updated**: January 25, 2026
**Next Review**: April 2026 (after Q1 customer feedback)
