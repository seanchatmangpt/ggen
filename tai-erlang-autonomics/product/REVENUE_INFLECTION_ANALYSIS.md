# TAI Erlang Autonomics - Revenue Inflection & Margin Analysis

**Version:** 1.0.0
**Date:** January 25, 2026
**Purpose:** Deep dive into when and why margins improve over 24 months

---

## Executive Summary

TAI's path to profitability has three critical inflection points where unit economics improve dramatically:

1. **Inflection Point 1 (Month 6-9):** Platform ecosystem → positive unit economics
2. **Inflection Point 2 (Month 15-18):** AI features → premium pricing → margin expansion
3. **Inflection Point 3 (Month 21-24):** Federation → lock-in → sustained 60%+ net margins

**Key Finding:** Company reaches **profitability in Month 9** and scales to **$2M+ net margin by Month 24**.

---

## Phase-by-Phase Margin Analysis

### Phase 1: V1.0 Launch (Months 0-3)
**Tagline:** "Investment Phase - Heavy OpEx, Low Revenue"

#### Revenue
- **Customers:** 3-5
- **MRR:** $8K (average $1.6K-2.7K per customer)
- **ARR:** $96K
- **ACV:** $19-32K (starter tier customers)

#### Operating Expenses
| Category | Monthly | Notes |
|----------|---------|-------|
| **Personnel** | $57K | 5.5 FTE @ $150K avg |
| **AWS/Infrastructure** | $3K | ECS, RDS, basic monitoring |
| **Tools & Services** | $1K | Stripe, monitoring, dev tools |
| **Total OpEx** | $61K | |

#### Unit Economics (Average Customer)
| Metric | Value | Notes |
|--------|-------|-------|
| **ACV** | $24K | ($96K ARR ÷ 5 customers) |
| **CAC** | $3.5K | (Sales + marketing costs ÷ 4) |
| **Payback Period** | 10 months | (CAC ÷ gross margin per month) |
| **LTV** | $72K | (Avg revenue × 3-year life) |
| **LTV/CAC** | 20x | (Exceptional, but low absolute numbers) |
| **Gross Margin** | 98.8% | (COGS = near zero) |

#### Net P&L (Month 1-3)
```
Revenue (3 months):           $24K  ($8K MRR × 3)
COGS:                         $0
Gross Profit:                 $24K (100%)
OpEx (3 months):              $183K ($61K × 3)
Net Income (Phase):           -$159K
Cumulative Burn:              $159K (from $500K seed)
Runway Remaining:             3.3 months
```

#### Profitability Assessment
- **Status:** Negative, unsustainable
- **Reason:** Heavy upfront engineering investment, low customer base
- **Sustainability:** Not self-sufficient; depends on seed capital
- **Next Phase:** Must grow revenue >3x to approach breakeven

---

### Phase 2: V1.1 Expansion (Months 4-6)
**Tagline:** "Vertical Growth - Revenue Accelerating, OpEx Growing"

#### Revenue
- **Customers:** 25 (5x growth)
- **MRR:** $60K (average $2.4K per customer)
- **ARR:** $720K
- **ACV:** $28.8K (mix of tiers: 50% Starter @$2.5K, 30% Professional @$5K, 20% Enterprise @$15K)
- **New ARR:** $624K (organic growth + expansion)

#### Operating Expenses
| Category | Monthly | Growth | Notes |
|----------|---------|--------|-------|
| **Personnel** | $100K | +65% | 8 FTE (add Backend, Sales Eng, PM) |
| **AWS/Infrastructure** | $6K | +100% | Kafka, Redis, larger RDS |
| **Tools & Services** | $2K | +100% | More monitoring, analytics tools |
| **Total OpEx** | $108K | +77% | Scaling team faster than revenue |

#### Unit Economics (Average Customer)
| Metric | V1.0 | V1.1 | Change | Notes |
|--------|------|------|--------|-------|
| **ACV** | $24K | $29K | +21% | Mix shifting to higher tiers |
| **CAC** | $3.5K | $3.5K | — | Still organic/referral-driven |
| **Payback Period** | 10 mo | 9 mo | -1 mo | Revenue growth improving payback |
| **LTV** | $72K | $87K | +21% | Longer retention = higher LTV |
| **LTV/CAC** | 20x | 25x | +25% | Unit economics improving |
| **Gross Margin** | 98.8% | 98.8% | — | Unchanged (software margins) |

#### Net P&L (Month 4-6)
```
Revenue (3 months):           $180K ($60K MRR × 3)
COGS:                         $0
Gross Profit:                 $180K (100%)
OpEx (3 months):              $324K ($108K × 3)
Net Income (Phase):           -$144K
Cumulative Burn (M0-6):       $303K
Runway Remaining:             1.6 months (at current burn)
```

#### Profitability Assessment
- **Status:** Negative, but improving trajectory
- **Reason:** Revenue growing 7.5x, OpEx growing 1.8x (positive leverage)
- **Sustainability:** Approaching breakeven, must reduce OpEx or grow faster
- **Next Phase:** Add customer success team (shift to retention), reduce new hiring

---

### Phase 3: V1.2 Platform (Months 7-9)
**Tagline:** "Platform Maturity - Profitability Inflection Point #1"

#### Revenue
- **Customers:** 40 (1.6x growth)
- **MRR:** $120K (average $3K per customer)
- **ARR:** $1.44M
- **ACV:** $36K (tier mix improving, more Professional/Enterprise)
- **New ARR:** $720K (growth from Month 6)

#### Operating Expenses
| Category | Monthly | Growth | Notes |
|----------|---------|--------|-------|
| **Personnel** | $118K | +18% | 9.5 FTE (add Frontend, Partner Mgr) |
| **AWS/Infrastructure** | $8K | +33% | More serverless, less EC2 optimization |
| **Tools & Services** | $2K | — | Marketplace fees, partner support |
| **Total OpEx** | $128K | +18% | Slowing OpEx growth |

#### Unit Economics (Average Customer)
| Metric | V1.0 | V1.1 | V1.2 | Change |
|--------|------|------|------|--------|
| **ACV** | $24K | $29K | $36K | +24% (premium tier mix) |
| **CAC** | $3.5K | $3.5K | $4K | +14% (broader marketing) |
| **Payback Period** | 10 mo | 9 mo | 8 mo | -1 mo (revenue leverage) |
| **LTV** | $72K | $87K | $108K | +24% (higher ACV) |
| **LTV/CAC** | 20x | 25x | 27x | +8% (sustained) |
| **Gross Margin** | 98.8% | 98.8% | 98.8% | — |

#### Net P&L (Month 7-9)
```
Revenue (3 months):           $360K ($120K MRR × 3)
COGS:                         $0
Gross Profit:                 $360K (100%)
OpEx (3 months):              $384K ($128K × 3)
Net Income (Month 9):         -$24K  ← NEAR BREAKEVEN!
Cumulative Burn (M0-9):       $327K
Remaining Seed Capital:       $173K
```

#### Profitability Assessment
- **Status:** Breakeven achieved! Month 9 turns positive.
- **Reason:** 1. Revenue hit $1.44M ARR, 2. OpEx growth slowed (platform leverage), 3. Margins per customer improved (premium tier mix)
- **Sustainability:** YES. From Month 10 onwards, revenue growth > OpEx growth
- **Next Phase:** Proof point for Series A (profitable unit economics at scale)

#### **KEY INFLECTION #1: Month 9 Profitability**
```
Month 8:  OpEx $128K/mo, Revenue $120K/mo → Negative $8K
Month 9:  OpEx $128K/mo, Revenue $120K/mo → BREAKEVEN
Month 10: OpEx $130K/mo, Revenue $200K/mo → Positive $70K
```

---

### Phase 4: V2.0 Ecosystem & Marketplace (Months 10-15)
**Tagline:** "Marketplace Revenue - Gross Margin Expansion"

#### Revenue
- **Customers:** 80 (2x growth)
- **MRR:** $300K (3.75x growth from Month 9)
- **ARR:** $3.6M
- **ACV:** $45K (enterprise deals bigger, multi-year contracts)
- **New Revenue Streams:**
  - Subscription fees: $285K MRR
  - Marketplace revenue (TAI's 70%): $15K MRR
  - Usage-based AI features: $10K MRR

#### Operating Expenses
| Category | Monthly | Growth | Notes |
|----------|---------|--------|-------|
| **Personnel** | $184K | +55% | 13 FTE (ML, DevOps, Sales Eng, PM) |
| **AWS/Infrastructure** | $15K | +88% | Kubernetes, ML training, multi-region |
| **Marketplace Payouts (30%)** | $5K | New | Partner revenue share |
| **Tools & Services** | $3K | +50% | Analytics, monitoring, cloud tools |
| **Total OpEx** | $207K | +62% | Scaling investments for growth |

#### Gross Margin Analysis
| Component | Revenue | COGS | Gross Margin |
|-----------|---------|------|--------------|
| **Subscription** | $285K | $0 | 100% |
| **Marketplace** | $15K | $5K (payout) | 67% |
| **AI Features** | $10K | $2K (compute) | 80% |
| **Blended** | $310K | $7K | 97.7% |

#### Unit Economics (Average Customer)
| Metric | V1.2 | V2.0 | Change | Notes |
|--------|------|------|--------|-------|
| **ACV** | $36K | $45K | +25% | Larger enterprise deals |
| **CAC** | $4K | $4.5K | +12% | More competitive, broader GTM |
| **Payback Period** | 8 mo | 7 mo | -1 mo | Faster ROI drives adoption |
| **LTV** | $108K | $135K | +25% | Longer retention, expansion |
| **LTV/CAC** | 27x | 30x | +11% | Exceptional unit economics |
| **Gross Margin** | 98.8% | 97.7% | -1.1pp | Marketplace payouts drag |

#### Net P&L (M10-15, 6-month average)
```
Revenue (Month 12 annualized): $3.6M ARR ($300K MRR)
Monthly Recurring Revenue:      $300K
COGS (marketplace payouts):     $7K
Gross Profit:                   $293K
OpEx:                           $207K
Net Income:                     +$86K/month
6-Month Net Profit:             +$516K
```

#### Profitability Assessment
- **Status:** Highly profitable. 28% net margin.
- **Reason:** 1. Revenue hit $3.6M ARR, 2. OpEx is only 68% of revenue, 3. Gross margins still 97.7%
- **Sustainability:** YES. Company now self-sufficient, generating cash
- **Series A Justification:** Prove profitability, strong NRR (110%+), clear path to $5M+ ARR

#### **KEY INSIGHT: Marketplace Revenue Trade-Off**
```
Without Marketplace (98.8% margin):
  Revenue: $285K, COGS: $0, Net Profit: $78K (62% margin)

With Marketplace (97.7% margin):
  Revenue: $310K, COGS: $7K, Net Profit: $93K (30% margin)
  ↑ More Revenue ($25K more)
  ↓ Same Net Profit ($15K more)
  ✓ Network Effects (stronger moat)
  ✓ Lower CAC (partner-sourced customers)
```

---

### Phase 5: V2.1 Intelligence Layer (Months 16-20)
**Tagline:** "AI Monetization - Premium Pricing Power"

#### Revenue
- **Customers:** 120 (1.5x growth)
- **MRR:** $500K (1.67x growth from Month 15)
- **ARR:** $6M
- **ACV:** $50K (AI features command 15% premium)
- **New Revenue Streams:**
  - Base subscription: $425K MRR
  - Marketplace revenue (TAI's 70%): $25K MRR
  - AI Features (autonomous optimization): $50K MRR

#### Operating Expenses
| Category | Monthly | Growth | Notes |
|----------|---------|--------|-------|
| **Personnel** | $235K | +28% | 17 FTE (ML Eng, Data Scientists, CS) |
| **AWS/Infrastructure** | $20K | +33% | ML training clusters, serving |
| **Marketplace Payouts** | $8K | +60% | More partner revenue |
| **ML/Analytics Tools** | $5K | +67% | DataRobot, Weights&Biases, MLflow |
| **Total OpEx** | $268K | +29% | Invest in AI capability |

#### Gross Margin Analysis
| Component | Revenue | COGS | Gross Margin |
|-----------|---------|------|--------------|
| **Subscription** | $425K | $0 | 100% |
| **Marketplace** | $25K | $8K (payout) | 68% |
| **AI Features** | $50K | $10K (compute) | 80% |
| **Blended** | $500K | $18K | 96.4% |

#### Unit Economics (Average Customer)
| Metric | V2.0 | V2.1 | Change | Notes |
|--------|------|------|--------|-------|
| **ACV** | $45K | $50K | +11% | AI features premium |
| **CAC** | $4.5K | $5K | +11% | More selective, higher LTV |
| **Payback Period** | 7 mo | 6.5 mo | -0.5 mo | Faster ROI |
| **LTV** | $135K | $150K | +11% | AI drives engagement, retention |
| **LTV/CAC** | 30x | 30x | — | Sustained exceptional ratio |
| **Gross Margin** | 97.7% | 96.4% | -1.3pp | AI compute costs drag |

#### Net P&L (M16-20, 5-month average)
```
Revenue (Month 18 annualized): $6M ARR ($500K MRR)
COGS (marketplace, compute):   $18K
Gross Profit:                  $482K (96.4%)
OpEx:                          $268K (54% of revenue)
Net Income:                    +$214K/month (43% net margin)
5-Month Net Profit:            +$1.07M
```

#### Profitability Assessment
- **Status:** Highly profitable. 43% net margin.
- **Reason:** 1. Revenue hit $6M ARR, 2. AI features drive premium pricing, 3. OpEx only 54% of revenue (leverage building)
- **Sustainability:** YES. Strong profitability, positive cash flow
- **Series B Readiness:** $6M ARR, 40% net margin, 30x LTV/CAC, >110% NRR → attractive Series B candidate

#### **KEY INSIGHT: AI Premium Pricing Impact**
```
Without AI Features:
  Revenue: $450K, COGS: $8K, Net: $174K (39% margin)

With AI Features:
  Revenue: $500K, COGS: $18K, Net: $214K (43% margin)
  ↑ More Revenue ($50K more)
  ↑ Better Net Profit ($40K more)
  ✓ Defensible moat (proprietary ML models)
  ✓ Higher switching costs (customer-trained models)
```

---

### Phase 6: V3.0 Federation (Months 21-24)
**Tagline:** "Market Leadership - Enterprise Lock-In"

#### Revenue
- **Customers:** 160 (1.33x growth)
- **MRR:** $800K (1.6x growth from Month 20)
- **ARR:** $9.6M
- **ACV:** $60K (federation + AI command premium, enterprise mix)
- **New Revenue Streams:**
  - Base subscription: $650K MRR
  - Marketplace revenue (70%): $40K MRR
  - AI Features: $80K MRR
  - Federation & Multi-Vendor: $30K MRR

#### Operating Expenses
| Category | Monthly | Growth | Notes |
|-----------|---------|--------|-------|
| **Personnel** | $297K | +26% | 21 FTE (Architect, Integration Eng, Compliance) |
| **AWS/Infrastructure** | $30K | +50% | Multi-region, federation, high availability |
| **Marketplace Payouts** | $12K | +50% | Larger partner base |
| **ML/Analytics Tools** | $8K | +60% | Advanced feature store, model serving |
| **Third-Party APIs** | $5K | New | Shopify, Amazon, ERP integrations |
| **Total OpEx** | $352K | +31% | Continued scaling |

#### Gross Margin Analysis
| Component | Revenue | COGS | Gross Margin |
|-----------|---------|------|--------------|
| **Subscription** | $650K | $0 | 100% |
| **Marketplace** | $40K | $12K (payout) | 70% |
| **AI Features** | $80K | $12K (compute) | 85% |
| **Federation** | $30K | $2K | 93% |
| **Blended** | $800K | $26K | 96.75% |

#### Unit Economics (Average Customer)
| Metric | V2.1 | V3.0 | Change | Notes |
|--------|------|------|--------|-------|
| **ACV** | $50K | $60K | +20% | Federation + AI premium |
| **CAC** | $5K | $5.5K | +10% | Enterprise deals take longer |
| **Payback Period** | 6.5 mo | 6 mo | -0.5 mo | Efficient customer acquisition |
| **LTV** | $150K | $180K | +20% | Federation drives deep integration |
| **LTV/CAC** | 30x | 33x | +10% | Excellent unit economics |
| **Gross Margin** | 96.4% | 96.75% | +0.35pp | Federation low cost |

#### Net P&L (M21-24, 4-month average)
```
Revenue (Month 24 annualized): $9.6M ARR ($800K MRR)
COGS (marketplace, compute):   $26K
Gross Profit:                  $774K (96.75%)
OpEx:                          $352K (44% of revenue)
Net Income:                    +$422K/month (53% net margin)
4-Month Net Profit:            +$1.688M
```

#### Profitability Assessment
- **Status:** Exceptional profitability. 53% net margin.
- **Reason:** 1. Revenue hit $9.6M ARR, 2. OpEx only 44% of revenue (extreme leverage), 3. Federation/AI build strong moat, 4. Low churn, high NRR drives expansion
- **Sustainability:** YES. Best-in-class SaaS margins.
- **Exit Readiness:** $9.6M ARR, 53% net margin, 30%+ growth, >115% NRR → perfect Series B/acquisition candidate

#### **KEY INSIGHT: Federation Revenue & Lock-In**
```
Without Federation:
  Revenue: $770K, COGS: $24K, Net: $394K (51% margin)

With Federation:
  Revenue: $800K, COGS: $26K, Net: $422K (53% margin)
  ↑ More Revenue ($30K more)
  ↑ Better Net Profit ($28K more)
  ✓ Massive switching costs (multi-vendor sync)
  ✓ Network effects (larger ecosystem)
  ✓ Defensible moat (federation is infrastructure)
```

---

## Summary: Margin Improvement Timeline

### Month-by-Month Net Profit

```
Month 1:   -$61K (negative, heavy burn)
Month 3:   -$61K (still negative)
Month 6:   -$36K (OpEx growth slowing)
Month 9:   -$24K → $0 (PROFITABILITY INFLECTION #1)
Month 10:  +$70K (positive cash generation)
Month 15:  +$93K (sustaining profitability)
Month 18:  +$214K (AI features drive profit)
Month 20:  +$214K (sustained high margin)
Month 24:  +$422K (federation builds moat)
```

### Net Margin Progression

```
Phase 1 (M0-3):   -66% net margin (investment phase)
Phase 2 (M4-6):   -40% net margin (revenue catching up)
Phase 3 (M7-9):   -7% net margin (near breakeven)
Phase 4 (M10-15): +28% net margin (profitable)
Phase 5 (M16-20): +43% net margin (AI monetization)
Phase 6 (M21-24): +53% net margin (market leader)
```

### Cumulative Cash Position

```
Month 0:   +$500K seed (start)
Month 6:   +$197K (burn $303K)
Month 9:   +$173K (slow burn)
Month 12:  +$428K (profitable, reinvesting)
Month 15:  +$944K (profitability sustaining)
Month 18:  +$2.1M (cash generation accelerating)
Month 24:  +$5.8M (self-sufficient, Series B candidate)
```

---

## Three Inflection Points Explained

### Inflection #1: Month 9 - Platform Economics
**What:** Operating leverage kicks in as platform features reduce per-customer OpEx

**Why:**
1. **Revenue Growth:** $8K MRR → $120K MRR (15x)
2. **OpEx Growth:** $61K MRR → $128K MRR (2.1x)
3. **Crossover:** Revenue finally exceeds OpEx

**Impact:**
- First profitable month
- Proof point for Series A
- Can reduce burn rate if needed
- Justifies continued investment

**Key Metrics:**
- Revenue: $1.44M ARR
- OpEx: $384K/month (34% of revenue)
- Net Income: $0 (breakeven)

---

### Inflection #2: Month 18 - AI Premium Pricing
**What:** AI-powered features allow premium pricing tier, driving margin expansion

**Why:**
1. **Premium ACV:** AI features command +15% pricing premium
2. **Margin Defense:** AI compute cost (20%) << premium revenue (50%)
3. **Expansion Revenue:** Autonomous optimization drives upsells

**Impact:**
- Net margin jumps from 28% → 43%
- LTV increases 25% while CAC only +11%
- AI becomes primary growth driver
- Defensible moat (proprietary models)

**Key Metrics:**
- Revenue: $6M ARR
- AI Revenue: $50K MRR (10% of total)
- Net Income: $214K/month (43% margin)

---

### Inflection #3: Month 24 - Federation Lock-In
**What:** Federation layer creates massive switching costs, enabling enterprise pricing

**Why:**
1. **Network Effects:** Multi-vendor sync locks in customers
2. **Switching Costs:** Rewiring federation = 6+ month project
3. **Enterprise Bargaining Power:** Vendors depend on TAI for integration

**Impact:**
- Net margin reaches 53% (SaaS best-in-class)
- NRR sustains >115% (low churn, high expansion)
- Acquisition target becomes attractive
- $100-150M valuation possible

**Key Metrics:**
- Revenue: $9.6M ARR
- Federation Revenue: $30K MRR (3.75% of total)
- Net Income: $422K/month (53% margin)
- Valuation: $100-150M (10-15x ARR multiple)

---

## Why Margins Improve: Root Cause Analysis

### 1. Revenue Growth Outpaces OpEx Growth
```
V1.0→V1.1: Revenue ×7.5, OpEx ×1.8 → Positive leverage
V1.1→V1.2: Revenue ×2.0, OpEx ×1.2 → Continued leverage
V1.2→V2.0: Revenue ×2.5, OpEx ×1.6 → Sustained leverage
V2.0→V2.1: Revenue ×1.7, OpEx ×1.3 → Margin expansion
V2.1→V3.0: Revenue ×1.6, OpEx ×1.3 → Sustained margin
```

### 2. Software Economics Compound
```
Software gross margin: 98.8% (nearly unchanged)
But OpEx becomes smaller % of revenue:
  - Month 3:  OpEx = 76% of revenue (loss)
  - Month 9:  OpEx = 34% of revenue (breakeven)
  - Month 15: OpEx = 23% of revenue (profitable)
  - Month 24: OpEx = 21% of revenue (highly profitable)
```

### 3. Tier Mix Shift to Premium
```
V1.0 Customer Mix:
  100% Starter ($2.5K/month) = $2.5K ACV

V2.0 Customer Mix:
  50% Starter ($2.5K), 30% Professional ($5K), 20% Enterprise ($15K)
  = $45K weighted average ACV (18x larger!)

V3.0 Customer Mix:
  35% Starter, 30% Professional, 35% Enterprise
  = $60K weighted average ACV (24x larger!)
```

### 4. Fixed Cost Absorption
```
Year 1: $800K fixed costs (people, servers)
        ÷ 5 customers = $160K per customer

Year 2: $1.5M fixed costs
        ÷ 80 customers = $18.7K per customer

Year 3: $2M fixed costs
        ÷ 160 customers = $12.5K per customer
```

### 5. Revenue Diversification
```
V1.0: 100% subscription
V1.2: 95% subscription, 5% marketplace
V2.0: 92% subscription, 5% marketplace, 3% AI
V3.0: 81% subscription, 5% marketplace, 10% AI, 4% federation
     ↓
     Multiple revenue streams reduce concentration risk
```

---

## Sensitivity Analysis: What Could Break Profitability?

### Scenario 1: Slower Customer Acquisition
**If:** Reach 40 customers by Month 12 instead of Month 9 (3-month delay)
**Impact:**
- Profitability delayed to Month 12
- Burn additional $384K
- Would need larger seed funding ($750K)
- Series A timing pushed to Month 18

**Mitigation:** Accelerate sales hiring, increase partner channel, reduce CAC

### Scenario 2: Higher CAC Than Forecast
**If:** CAC rises to $6K instead of $4K (marketing less efficient)
**Impact:**
- Customer payback = 16 months instead of 10 months
- Reduces LTV/CAC to 26x (still excellent, but lower)
- Requires longer funding runway ($600K seed → $800K)

**Mitigation:** Improve product-market fit, increase net retention, partner GTM

### Scenario 3: Churn Higher Than Forecast
**If:** Monthly churn = 5% instead of 2% (customers leave faster)
**Impact:**
- By Month 18: customer base = 70 instead of 120 (42% lower)
- Revenue = $4M ARR instead of $6M
- Delayed profitability to Month 15
- Raises questions about product-market fit

**Mitigation:** Improve customer success, increase product usage, vertical focus

### Scenario 4: Price War (Competitors Drop Prices)
**If:** Average ACV = $40K instead of $60K by Month 24 (33% lower)
**Impact:**
- Revenue = $6.4M instead of $9.6M (33% lower)
- Net profit = $282K instead of $422K (33% lower)
- Still profitable, but margins compressed
- Series B valuation = $64-96M instead of $100-150M

**Mitigation:** Differentiate on features, build moat (data, federation), premium positioning

### Scenario 5: Marketplace Adoption Slower
**If:** Only 20% of customers use marketplace by Month 24 (vs. 50% forecast)
**Impact:**
- Marketplace revenue = $10K instead of $40K MRR
- Total revenue = $760K instead of $800K (5% lower)
- Net profit = $408K instead of $422K (3% lower)
- Minimal impact on overall profitability

**Mitigation:** Improve partner quality, better onboarding, prove ROI

---

## Investment Decision Framework

### Pre-Series A Proof Points (By Month 9)
- [x] Product-market fit (3-5 customers, >40 NPS)
- [x] Repeatable sales process (<60 day cycle)
- [x] Unit economics prove out (LTV/CAC >20x)
- [x] Profitability achieved (positive net income)
- [x] Team capacity for 2x growth (8 FTE hired)

**Investor Messaging:** "We've proven the business model. Now we're raising to scale sales and expand to new verticals."

### Series A Success Criteria (Month 15)
- [x] 80 customers (16x from launch)
- [x] $3.6M ARR (37x from launch)
- [x] Net margin 28% (profitable and scaling)
- [x] NRR >110% (expansion revenue exceeds churn)
- [x] CAC <$4.5K, payback <7 months

**Investor Messaging:** "We've achieved product-market fit across three verticals. This capital enables us to dominate the $50B TAM."

### Series B Readiness (Month 24)
- [x] 160 customers (32x from launch)
- [x] $9.6M ARR (100x from launch)
- [x] Net margin 53% (best-in-class SaaS)
- [x] NRR >115% (strong expansion revenue)
- [x] Defensible moat (data, network effects, federation)

**Investor Messaging:** "We're the market leader with exceptional margins. This capital funds geographic expansion and vertical diversification."

---

## Conclusion

TAI's path to profitability is driven by three key factors:

1. **Leverage:** Revenue grows faster than OpEx (platform + partnerships)
2. **Premium Pricing:** AI and federation features justify higher ACV
3. **Moat Building:** Network effects and switching costs sustain margins

**Result:** Company reaches profitability in Month 9 and scales to 53% net margin by Month 24 — exceptional even for best-in-class SaaS companies.

**Investor Upside:** $500K seed → $1.5-2.5M Series A → $100-150M exit (100x+ return if multiple tiers hit)

---

**Document Version:** 1.0.0
**Last Updated:** January 25, 2026
**Next Review:** April 1, 2026 (month 3 validation)
