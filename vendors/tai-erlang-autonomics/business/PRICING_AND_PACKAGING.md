# TAI Erlang Autonomics - Pricing & Packaging Strategy

**Version:** 1.0.0
**Date:** January 25, 2026

---

## Overview

TAI uses **value-based, tiered SaaS pricing** with an optional **outcome-based participation model** for customers who want risk-sharing.

**Pricing Philosophy:**
- Charge for value captured (margin improvement, operational savings)
- Make pricing transparent and outcome-focused
- Allow customers to scale smoothly (Starter → Professional → Enterprise)
- Offer participation option for risk-averse customers

---

## Pricing Tiers

### TIER 1: STARTER ($2,500/month)

**Target Customer Profile:**
```
Company Size: Pre-Series A startups to mid-market
Annual Revenue: $5-50M
Monthly GMV: $200K - $500K
Team Size: 1-3 operations staff
Pain Point: Manual inventory sync across 2-3 channels
```

**What's Included:**

**Core Features:**
- HTTP API (unlimited requests)
- Event ingestion (unlimited events)
- Autonomic governors (basic: single warehouse, simple rules)
- Receipt ledger (JSON logs, 90-day retention)

**Observability:**
- Prometheus metrics export
- Structured JSON logging (stdout)
- Health check endpoint
- No external monitoring integration

**Support:**
- Email support (24-hour response time)
- Community Slack channel (shared with all tiers)
- Knowledge base access
- Basic troubleshooting

**Limits:**
- 500 active SKUs
- Single warehouse
- 1 integration (HTTP API only)
- 1 API key / credentials
- Shared infrastructure (noisy neighbors possible)

**SLA:**
- 99.5% uptime (29 minutes down/month)
- No guaranteed response time

---

#### Starter Tier ROI Example

**Customer: "SmallMart" E-commerce Startup**

**Current State:**
```
Channels: Shopify, Amazon, own website
Warehouses: 1
SKUs: 200
Monthly Volume: $350K GMV
Team: 2 ops staff (managing inventory manually)

Problems:
- 4-6 hour sync latency between channels
- 3.5% order miss rate (lost sales)
- Manual spreadsheet management (error-prone)
- $15K/month in ops labor

Current Margin: 24%
```

**With TAI Starter:**
```
Setup: 2 weeks (API integration)
Sync Latency: <5 minutes (automated)
Order Miss Rate: 0.5% (-86%)
Labor: -1 ops staff (FTE redeployed)

Financial Impact:
- Recovered missed sales: +$12.2K/month (3% of 350K × 4% recovery rate)
- Labor saved: +$8K/month (1 FTE @ $8K/month)
- Inventory optimization: +0.8% margin = +$2.8K/month
- Total value: +$23K/month

TAI Cost: $2.5K/month
Net Benefit: +$20.5K/month ($246K/year)
ROI: 820%
Payback: 1.2 weeks
```

**Why Upgrade to Professional in Year 2?**
- Add 2nd warehouse → need multi-location governors
- Expand to 1,000 SKUs → need advanced pricing
- Launch new channel → need Professional-tier integrations

---

### TIER 2: PROFESSIONAL ($15,000/month)

**Target Customer Profile:**
```
Company Size: Growth-stage (Series A-C)
Annual Revenue: $50-500M
Monthly GMV: $2M - $5M
Team Size: 5-20 operations staff
Pain Point: Multi-warehouse coordination, margin erosion, complex rules
```

**What's Included:**

**Core Features:**
- Everything in Starter +
- Advanced autonomic governors (multi-warehouse, complex rules)
- Multi-tenant isolation (per-tenant data segregation)
- Pub/Sub real-time event streaming
- Firestore-backed receipt ledger (enterprise audit trail)
- Advanced value indexing (dynamic pricing algorithms)

**Integrations:**
- Shopify GraphQL API (real-time sync)
- WooCommerce REST API
- Custom webhook support
- FTP/SFTP for bulk uploads
- Quickbooks accounting integration (preview)

**Observability:**
- OpenTelemetry distributed tracing
- Custom Datadog dashboards (if customer has Datadog)
- Structured logging with JSON fields
- Alert configuration API
- SLA breach notifications

**Support:**
- Priority support (4-hour response time)
- Dedicated Slack channel
- Monthly check-in calls
- Custom integration support
- Escalation to engineering team

**Analytics & BI:**
- BI dashboard (basic Looker/Tableau ready)
- Custom metric reports
- Performance trend analysis
- ROI calculator (automated)

**Limits:**
- 5,000 active SKUs
- Up to 10 warehouses/locations
- Up to 5 integrations
- 1 admin + 3 operational users
- No custom SLA (standard 99.9%)

**SLA:**
- 99.9% uptime (4 minutes 20 seconds down/month)
- 4-hour support response time

---

#### Professional Tier ROI Example

**Customer: "MultiMart" Marketplace Aggregator**

**Current State:**
```
Business Model: Marketplace of 50+ merchant partners
Warehouses: 3 fulfillment centers (TX, CA, IL)
SKUs: 3,000+
Monthly Volume: $3.5M GMV
Team: 12 ops staff (coordinating inventory, pricing)

Problems:
- Inventory misalignment across DCs (stockouts in 1 DC, overstock in another)
- 2-4% margin leakage (pricing varies by channel, missed discounts)
- Manual replenishment (4-6 hour lag, lost opportunities)
- Shrinkage: 2.1% (outdated inventory counts)
- $120K/month in ops coordination cost

Current Margins: 26%
```

**With TAI Professional:**
```
Setup: 4 weeks (integrations, data import, training)
Inventory Accuracy: 99%+ (real-time sync across all 3 DCs)
Pricing Alignment: <30 second (automated across channels)
Replenishment Latency: <15 minutes (vs 4-6 hours)
Shrinkage: 0.9% (70% reduction, automated counting)

Financial Impact:
- Recovered missed replenishment opportunities: +$84K/month (2.4% of $3.5M)
- Margin recovery (pricing alignment): +$52.5K/month (1.5% of $3.5M)
- Shrinkage reduction: +$42K/month (1.2% of $3.5M)
- Ops labor efficiency: -$85K/month (71% staff reduction to 3.5 FTE)
- Total value: +$263.5K/month

TAI Cost: $15K/month
Net Benefit: +$248.5K/month ($2.98M/year)
ROI: 1,656%
Payback: 3.6 days
```

**Upsell Opportunities in Year 2:**
- Predictive Analytics module: +$10K/month (demand forecasting)
- Value-Indexed Pricing: +$5K/month (advanced margin optimization)
- Participation revenue: +$20-50K/month (2% of margin gains)

---

### TIER 3: ENTERPRISE ($75,000+/month, Custom)

**Target Customer Profile:**
```
Company Size: Enterprise (Fortune 500, major retailers)
Annual Revenue: $5B+
Monthly GMV: $20M - $500M+
Team Size: 50+ operations and analytics staff
Pain Point: Complex supply chains, regulatory compliance, margin at scale
```

**What's Included:**

**Core Features:**
- Everything in Professional +
- Unlimited SKU capacity
- Unlimited warehouse/location capacity
- Private Cloud Run instance (dedicated deployment)
- Custom value indexing models (per business logic)
- Advanced ML/AI capabilities:
  - Predictive overstock detection
  - Demand forecasting (ARIMA, prophet integration)
  - Seasonal trend analysis
  - Anomaly detection

**Integrations:**
- Everything in Professional +
- SAP / Oracle ERP integration
- Salesforce integration
- Custom API development support
- Legacy system bridge (as needed)

**Observability & Analytics:**
- Advanced analytics dashboard (Looker, Tableau, Power BI)
- Predictive analytics
- Margin scenario modeling ("what-if" analysis)
- Custom KPI definitions
- Real-time alerts with ML-based anomaly detection

**Compliance & Security:**
- SOC 2 Type II attestation
- HIPAA compliance (if healthcare)
- Custom data residency (if required)
- Encryption at rest (CMEK)
- VPC Service Controls integration
- Advanced audit logging

**Support:**
- Dedicated account team:
  - Account Manager (primary contact)
  - Technical Architect (design partner)
  - Customer Success Manager (quarterly business reviews)
  - Engineering escalation (direct access)
- 15-minute incident response (on-call)
- Dedicated Slack channel with guaranteed response times
- Quarterly executive business reviews
- Annual strategy summit

**SLA & Guarantees:**
- 99.95% uptime SLA (5 minutes down/month max)
- 15-minute incident response
- 1-hour resolution target
- Financial penalties for SLA breaches

**Professional Services:**
- Custom implementation (as needed)
- Training for customer's team
- Data migration from legacy systems
- Custom report development

---

#### Enterprise Tier ROI Example

**Customer: "MegaRetail" National Omnichannel Retailer**

**Current State:**
```
Business Model: 500+ physical stores + 10 distribution centers + online
SKUs: 50,000+
Monthly Volume: $80M GMV
Team: 120+ ops + analytics staff
Current Supply Chain Platform: SAP

Problems:
- SAP integration: 24-hour batch (live data every night at 2 AM)
- Inventory misalignment: 0.8% across all channels (costs $640K/month)
- Pricing misalignment: 2.1% margin leakage = $1.68M/month
- Supply chain complexity: 90-day lead times (excessive inventory)
- Analytics team: $2M/year in salaries (can't keep up with demand)
- Current Margin: 28%
```

**With TAI Enterprise:**
```
Setup: 12 weeks (SAP integration, data migration, custom model training)
Real-Time Sync: Replaced 24-hour batch with <60 second real-time
Inventory Accuracy: 99.9%+
Pricing Accuracy: <99.5%
Supply Chain Lead Time: 30-40% reduction (freeing $3-5M from buffer stock)

Financial Impact:
- Recovered margin (pricing optimization): +$1.2M/month (15% recovery)
- Inventory optimization (freed cash): +$4M one-time (cash redeployment)
- Supply chain efficiency: +$840K/month (12-day reduction in lead time)
- Analytics team efficiency: -$1.2M/year ($100K/month recurring)
- Shrinkage reduction: +$320K/month (0.4% improvement)
- Total Annual Value: $26.88M

TAI Cost: $75K/month = $900K/year
Net Benefit: $25.98M/year
ROI: 2,886%
Payback: 13 days

Participation Model (Optional):
- 2% of margin gains = 2% × $1.2M = +$20K/month
- Additional TAI revenue: +$240K/year
- Total TAI revenue: $1.14M/year
- Enterprise customer LTV: $6.84M (6-year horizon)
```

**Why Choose TAI Enterprise?**
1. **Retail transformation:** Move from legacy SAP batch to real-time AI
2. **Regulatory:** SOC 2 + custom compliance (vs unvalidated point solutions)
3. **Efficiency:** Reduce analytics team by 40%, redeploy to strategy
4. **Financial impact:** $26M+ annual value vs $900K cost

---

## Optional Add-On Modules

### Module 1: Predictive Analytics ($10,000/month)

**Includes:**
- Demand forecasting (ARIMA, Prophet, ML models)
- Overstock prediction (early warning, 2-week lead time)
- Seasonal trend analysis
- Anomaly detection in demand patterns
- Forecast accuracy tracking (RMSE, MAPE)

**ROI Example:**
- Reduce overstock by 15% = $300K+ annual savings
- Payback: <2 weeks
- Prerequisite: Tier 2+ subscription

**Target Customers:**
- Retailers with fast-moving inventory (fashion, food, e-commerce)
- Marketplace sellers (high inventory turnover)

---

### Module 2: Value-Indexed Pricing Engine ($5,000/month)

**Includes:**
- Dynamic price optimization (margin vs velocity trade-off)
- A/B testing framework (price sensitivity testing)
- Competitor price integration (if available)
- Margin protection guarantees (floor price rules)
- Pricing recommendations by SKU/channel

**ROI Example:**
- Increase realized margin by 1-2% = $20-40K/month for typical Professional customer
- Payback: <1 week
- Prerequisite: Tier 2+ subscription

**Target Customers:**
- E-commerce retailers (direct control over pricing)
- Marketplace aggregators (price coordination)
- CPG manufacturers (wholesale pricing optimization)

---

### Module 3: Dedicated Success Manager ($8,000/month)

**Includes:**
- Quarterly business reviews (executive attendees)
- Expansion planning (new channels, geographies)
- Custom training (customer's team)
- ROI tracking and reporting
- 24/7 escalation access
- Strategic recommendations

**When to Recommend:**
- Enterprise customers (Tier 3 standard)
- Professional customers with >$1M/year engagement
- Customers at risk of churn

**Target Customers:**
- Enterprise retail chains
- Healthcare networks
- Marketplace platforms

---

## Pricing Comparison Table

| Feature | Starter | Professional | Enterprise |
|---------|---------|--------------|-----------|
| **Price** | $2.5K/mo | $15K/mo | $75K+/mo |
| **Setup Cost** | $0 | $0 | $50-100K |
| **SKU Limit** | 500 | 5,000 | Unlimited |
| **Warehouses** | 1 | 10 | Unlimited |
| **Integrations** | 1 (API) | 5 | Custom |
| **Support Response** | 24h | 4h | 15min |
| **SLA Uptime** | 99.5% | 99.9% | 99.95% |
| **Dedicated Support** | No | No | Yes |
| **Advanced Analytics** | No | Basic | Advanced |
| **Custom Development** | No | Limited | Yes |
| **Participation Model** | No | Optional | Yes |
| **Typical GMV Range** | $200K-500K | $2M-5M | $20M+ |
| **Typical ROI Payback** | 7-14 days | 2-7 days | 10-30 days |

---

## Pricing Strategy Details

### Volume Discounts (Multi-Year Commitment)

**Annual Prepay:** 15% discount
- Starter: $2,500 × 12 × 0.85 = $25,500/year ($2,125/month effective)
- Professional: $15,000 × 12 × 0.85 = $153,000/year ($12,750/month effective)

**Multi-Year Commitment (3 years):** 25% discount on Year 1, progressive improvements in Years 2-3
- Professional Year 1: $15K/month
- Professional Year 2: $14K/month (if GMV stable)
- Professional Year 3: $13K/month (if GMV stable)
- Example 3-year value: 10% cumulative savings + predictable costs

---

### Outcome-Based Participation Model (Optional)

**Available for:** Professional tier and above

**How It Works:**
1. Establish baseline: 90-day average of customer's prior performance (before TAI)
2. Measure: Track actual performance with TAI
3. Calculate gains: Compare to baseline
4. Share: TAI receives 2% of documented gains

**Example Calculation:**

```
Baseline (90-day pre-TAI average):
- Margin Realization: 26%
- Monthly GMV: $3M
- Baseline Margin $: $780K/month

With TAI (after 90 days):
- Margin Realization: 27.5%
- Monthly GMV: $3M
- Actual Margin $: $825K/month

Gain: $825K - $780K = $45K/month
Participation (2%): $900/month

Total TAI Invoice:
- Base subscription: $15,000
- Participation: $900
- Total: $15,900/month
```

**Why Participation Model?**
- De-risks customer (lower risk with upside sharing)
- Aligns TAI incentives with customer success
- Reduces churn (skin in the game)
- Attracts deal-averse customers

**Typical Participation Results:**
- Starter customers: No participation
- Professional customers: 10-15% opt-in (add +$2-3K/month average)
- Enterprise customers: 30-40% opt-in (add +$15-30K/month average)

---

## Competitive Pricing Analysis

### vs Competitors

| Competitor | Model | Typical Cost | Differentiation |
|------------|-------|--------------|-----------------|
| **Shopify Inventory** | Fixed licensing | $300-500/month | Limited governance, not autonomic |
| **Blue Yonder** | Enterprise license | $200K-500K/year | Full WMS, not plug-and-play |
| **Kinaxis** | Demand planning | $100K-300K/year | Forecasting only, no execution |
| **Custom Build** | Services + labor | $500K+ upfront | Bespoke, high maintenance cost |
| **TAI Starter** | SaaS, usage-based | $2,500/month | Quick deployment, autonomous |
| **TAI Professional** | SaaS, value-based | $15K/month | End-to-end, managed service |

**TAI Competitive Advantages:**
1. **Speed to value:** 30 days vs 6-12 months
2. **Cost:** 90% cheaper than enterprise solutions
3. **Autonomy:** Self-healing governors vs manual rules
4. **Compliance:** Cryptographic audit trail vs logs
5. **Modern:** Cloud-native vs legacy on-premise

---

## Pricing Decision Tree

**Use this to recommend tier to prospects:**

```
START: "What's your monthly GMV?"

├─ < $500K
│  └─ → RECOMMEND: Starter ($2.5K/month)
│       Benefits: Quick start, clear ROI
│       Suggest upgrade: "When you expand to 2nd warehouse"
│
├─ $500K - $5M
│  └─ → RECOMMEND: Professional ($15K/month)
│       Benefits: Multi-warehouse, integrations, priority support
│       Suggest add-on: "Predictive Analytics if inventory-heavy"
│
└─ $5M+
   └─ → RECOMMEND: Enterprise (Custom, $75K+/month)
        Benefits: Private instance, dedicated team, custom development
        Suggest participation: "If open to outcome-sharing"
```

---

## Sales Collateral

### Pricing Page (Recommended CTA)

```
TAI Erlang Autonomics - Simple Pricing

STARTER
$2,500/month
For growing e-commerce
- Up to 500 SKUs
- Single warehouse
- HTTP API
- Email support
→ Start Free Trial (14 days)

PROFESSIONAL
$15,000/month
For mid-market + 3PLs
- Up to 5,000 SKUs
- 10 warehouses
- Integrations (Shopify, WooCommerce)
- Priority support (4h)
→ Schedule Demo

ENTERPRISE
Custom pricing
For enterprise retailers
- Unlimited scale
- Private deployment
- Dedicated team
- Custom development
→ Request Proposal

---

ADD-ONS
Predictive Analytics: +$10K/month
Value-Indexed Pricing: +$5K/month
Dedicated Success Manager: +$8K/month
Participation Revenue Share: 2% of gains (optional)
```

---

## Price Increase Strategy

**Year 1 (Validation Phase):** No increases
- Focus on customer success and retention
- Gather data on true customer value

**Year 2 (Growth Phase):** 10-15% increase for new customers
- Demonstrate ROI with case studies
- Grandfathered pricing for existing customers
- Transition new Tier 2 customers to Professional+ tier

**Year 3+ (Scale Phase):** 5-10% annual increases
- Tie to CPI + feature additions
- Offer 15-25% discounts for 3-year prepay
- Implement annual adjustment clauses

**Mechanics:**
- Notify existing customers 90 days in advance
- Offer 15% discount for annual prepay (locks in price)
- Grandfather current customers for first year
- Implement on next renewal date

---

## Financial Impact of Pricing Model

### Year 1 Projected Revenue (Base Case: 15 customers)

| Tier | Customers | Monthly Revenue | Annual Revenue | Blended ACV |
|------|-----------|-----------------|-----------------|------------|
| Starter | 6 | $15,000 | $180,000 | $30,000 |
| Professional | 7.5 | $112,500 | $1,350,000 | $180,000 |
| Enterprise | 1.5 | $112,500 | $1,350,000 | $900,000 |
| **Total** | **15** | **$240,000** | **$2,880,000** | **$192,000** |

*Note: Blended ACV includes growth through the year*

### Participation Revenue Impact (Optional)

**If 15% of customers opt-in to participation model:**

| Tier | Customers Opted-In | Avg Monthly Participation | Annual Revenue |
|------|-------------------|-------------------------|-----------------|
| Starter | 0 | $0 | $0 |
| Professional | 1.1 | $3,000 | $39,600 |
| Enterprise | 0.45 | $15,000 | $81,000 |
| **Total Participation** | **1.55** | **$18,000** | **$120,600** |

**Total Year 1 Revenue:** $2,880,000 + $120,600 = **$3,000,600**

**Impact on Unit Economics:**
- Gross margin: 98.8% (unchanged, participation revenue is pass-through)
- CAC: Slightly lower due to lower upfront cost for participation customers
- LTV: +15% due to additional revenue streams

---

## Implementation Timeline

### Month 1-2: Launch with Starter & Professional
- Beta pricing (private beta customers get 20% discount)
- Refine messaging based on feedback
- Create pricing page + sales collateral

### Month 3-6: Validate with First 10 Customers
- Tier mix should be 40% Starter, 60% Professional
- Gather feedback on pricing and packaging
- Adjust if needed

### Month 6-9: Add Enterprise Tier
- First Enterprise customer closes (custom pricing)
- Use as template for future Enterprise deals
- Create Enterprise case study

### Month 9-12: Scale & Optimize
- Standardize Enterprise pricing around $75K base
- Launch participation model (opt-in)
- Introduce add-on modules

---

## Key Pricing Metrics to Track

### Monthly KPIs

| Metric | Target | Alert Threshold |
|--------|--------|-----------------|
| **Average Revenue per User (ARPU)** | $30K | <$25K |
| **Tier Mix (% Professional)** | 45% | <40% |
| **Participation Adoption Rate** | 15% | <10% |
| **Price Realization (vs list)** | 95% | <90% |
| **Churn Rate** | <2% monthly | >2% |
| **Gross Margin** | 98%+ | <95% |

### Quarterly Business Review Metrics

| Metric | Target | Status |
|--------|--------|--------|
| **Blended ACV Growth** | +5% QoQ | — |
| **Mix Shift to Professional/Enterprise** | +3% QoQ | — |
| **Participation Revenue** | +2% of base revenue | — |
| **Price Increase Impact** | Maintained retention | — |

---

## Conclusion

TAI's pricing model balances:
1. **Customer value capture** (price for outcomes)
2. **Market competitiveness** (90% cheaper than enterprise solutions)
3. **Scalability** (enables rapid expansion)
4. **Alignment** (participation model shares upside)

**Key Success Factor:** Communicate pricing as ROI payback, not cost.
- Starter: 7-14 day payback
- Professional: 2-7 day payback
- Enterprise: 10-30 day payback

---

**Document Version:** 1.0.0
**Classification:** Business - Confidential
**Last Updated:** January 25, 2026
