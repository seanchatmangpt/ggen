# ROI Calculator: Prospect-Facing Tool

**Status:** Ready for Sales Use
**Purpose:** Personalized ROI calculation for each customer
**Format:** Can be Excel, Google Sheet, or web form

---

## Healthcare ROI Calculator

### Input Fields (Fill in with customer numbers)

**Hospital Profile:**
```
[ ] Hospital name: ________________________
[ ] Total beds: __________
[ ] Annual revenue: $ __________
[ ] Current bed utilization: _________%
```

**Current Pain Points (estimate from conversation):**
```
[ ] Cancelled surgeries per month: __________
[ ] Average cancelled surgery revenue: $ __________
[ ] Discharge processing time (avg minutes): __________
[ ] Annual nursing staff time on bed scheduling: __________
    (estimated hours: __________ @ $80/hour)
```

**System Assumptions (defaults, override if needed):**
```
[ ] Bed availability accuracy improvement: Default 95% (was 65%)
[ ] Discharge time reduction: Default 80% (was 45 min → 8 min)
[ ] Cancelled surgery prevention: Default 90% reduction
[ ] Staff time savings: Default 15 hours/week
```

### Output: ROI Summary

**Annual Benefit Calculation:**

```
BENEFIT #1: Cancelled Surgery Prevention
├── Current: X cancelled surgeries/month
├── With TAI: X × 10% (assume 90% prevention)
├── Prevented surgeries: _________
├── Revenue per surgery: $ __________
├── Annual benefit: $ __________
└── Example: 3/month × 12 × $15K = $540K/year

BENEFIT #2: Discharge Time Reduction
├── Current average: 45 minutes
├── With TAI average: 8 minutes
├── Time saved per discharge: 37 minutes
├── Discharges per month: __________
├── Annual discharges: __________
├── Nursing time saved: __________ hours
├── @ $80/hour: $ __________/year
└── Example: 150 × 37 min = 5,550 hours = $444K/year

BENEFIT #3: Bed Utilization Improvement
├── Current utilization: 62%
├── With TAI utilization: 89%
├── Improvement: 27 percentage points
├── Revenue per bed per year: $ __________
├── Additional beds "unlocked": __________
├── Annual benefit: $ __________
└── Example: 27% × 300 beds × $150K = $1.22M/year

TOTAL ANNUAL BENEFIT: $ __________
Example: $540K + $444K + $1.22M = $2.204M/year
```

**Cost Calculation:**

```
COST: TAI License + Implementation
├── TAI License (Year 1): $24,000
├── Implementation services: $0-20,000 (included in POC proposal)
├── Training: Included
└── TOTAL YEAR 1 COST: $ __________
```

**ROI Calculation:**

```
ROI = (Annual Benefit - Annual Cost) / Annual Cost × 100%

Example:
($2,204,000 - $24,000) / $24,000 = 90.7x (or 9,070% return)
```

**Sensitivity Analysis (What if we're wrong?):**

```
Downside Case (50% of projected benefit):
├── Annual benefit: $1,102,000 (half of estimate)
├── Cost: $24,000
├── ROI: 45x
└── Still excellent

Upside Case (150% of projected benefit):
├── Annual benefit: $3,306,000
├── Cost: $24,000
├── ROI: 136x
└── Very strong
```

---

## Finance ROI Calculator

### Input Fields

**Fund Profile:**
```
[ ] Fund name: ________________________
[ ] AUM: $ __________
[ ] Strategy: _______________ (long/short, hedge, commodity, etc.)
[ ] Number of positions: __________
[ ] Annual management fee: $ __________
```

**Current Risk Management Process:**
```
[ ] Time to run daily risk calculations: __________ hours
[ ] Cost of risk infrastructure: $ __________/year
[ ] Risk calculation method: __________ (VaR, historical, Monte Carlo)
[ ] Auditor compliance time: __________ hours/quarter
```

**Performance Impact (estimate from CRO):**
```
[ ] Hedges missed in past year: __________
[ ] Average cost per missed hedge: $ __________
[ ] Sharpe ratio vs peers: __________
[ ] Target Sharpe ratio: __________
[ ] Gap: __________
```

**System Assumptions:**
```
[ ] Time to risk calculation: Default 8 hours → 5 minutes
[ ] Hedging recommendation implementation rate: Default 60-80%
[ ] Risk calculation accuracy: Default 95% match to current system
[ ] Compliance audit efficiency: Default 90% time savings
```

### Output: ROI Summary

**Annual Benefit Calculation:**

```
BENEFIT #1: Missed Hedging Opportunity Prevention
├── Missed hedges in past year: X
├── Average cost per miss: $Y
├── With TAI (real-time recommendations): Reduce misses by 80%
├── Hedges recovered: X × 80% = __________
├── Annual benefit: $ __________
└── Example: 8 missed hedges × $2.1M = $16.8M saved by preventing misses

BENEFIT #2: Improved Decision Quality (Sharpe Ratio)
├── Current Sharpe ratio: 1.2
├── Peer average: 1.5
├── Gap: 0.3 (TAI addresses ~50% of this)
├── Improved Sharpe: 1.2 + 0.15 = 1.35
├── AUM: $ __________
├── Volatility: __________% (from current data)
├── Annual performance improvement: $ __________
└── Example: $500M × 0.5% = $2.5M annual performance gain

BENEFIT #3: Staff Time Savings
├── Risk manager time saved: 7.5 hours/day × 250 trading days
├── = 1,875 hours/year
├── @ $150/hour loaded cost: $ __________/year
└── Example: 1,875 × $150 = $281.25K/year

BENEFIT #4: Reduced Compliance Burden
├── Current audit prep time: 60 hours/quarter
├── With TAI automated logging: 5 hours/quarter
├── Time saved: 55 hours/quarter × 4 = 220 hours/year
├── @ $150/hour: $ __________/year
└── Example: 220 × $150 = $33K/year

TOTAL ANNUAL BENEFIT: $ __________
Example: $16.8M + $2.5M + $281K + $33K = $19.6M/year
```

**Cost Calculation:**

```
COST: TAI License + Implementation
├── TAI License (Year 1): $50,000
├── Implementation services: $10-30K (Bloomberg integration)
├── Training: Included
└── TOTAL YEAR 1 COST: $ __________
```

**ROI Calculation:**

```
ROI = (Annual Benefit - Annual Cost) / Annual Cost × 100%

Example:
($19,614,000 - $50,000) / $50,000 = 391.7x (or 39,170% return)
```

**Sensitivity Analysis:**

```
Downside Case (25% of projected benefit):
├── Annual benefit: $4,903,500
├── Cost: $50,000
├── ROI: 97x
└── Exceptional

Upside Case (200% of projected benefit):
├── Annual benefit: $39,228,000
├── Cost: $50,000
├── ROI: 783x
└── Extraordinary
```

---

## E-Commerce ROI Calculator

### Input Fields

**Store Profile:**
```
[ ] Store name: ________________________
[ ] Annual GMV: $ __________
[ ] Average order value: $ __________
[ ] Gross margin: ___________%
[ ] Sales channels: [ ] Shopify [ ] Amazon [ ] Retail [ ] Other: _______
[ ] Total SKUs: __________
[ ] Warehouse space: __________ sqft
```

**Current Inventory Challenges (from operations):**
```
[ ] Stock-out events per week: __________
[ ] Average lost revenue per stock-out: $ __________
[ ] Overstock percentage: __________% (inventory that never sells)
[ ] Overstock annual waste: $ __________
[ ] Manual allocation time: __________ hours/week
[ ] Carrying cost (% of inventory value): ___________%
```

**System Assumptions:**
```
[ ] Stock-out reduction: Default 90% (47 → 2 per week)
[ ] Overstock improvement: Default 75% (8% → 2%)
[ ] Inventory turns improvement: Default +20% (better allocation)
[ ] Inventory sync accuracy: Default 98% (all channels)
```

### Output: ROI Summary

**Annual Benefit Calculation:**

```
BENEFIT #1: Stock-Out Prevention
├── Current stock-out events/week: X
├── With TAI: X × 10% = ___________ events/week
├── Prevented stock-outs/week: ___________
├── Average revenue per prevented stock-out: $ __________
├── Weeks per year: 52
├── Annual benefit: $ __________
└── Example: 45 events × $8K × 52 = $18.72M/year

BENEFIT #2: Overstock Waste Reduction
├── Current overstock: 8% of $ __________ inventory
├── = $ __________ in unsold inventory
├── With TAI: Reduce to 2% (better allocation, fewer discounts)
├── Waste eliminated: 6% of inventory value
├── Carrying cost: __________% per year
├── Annual benefit: $ __________
└── Example: 6% × $2.25M × 50% of cost = $67.5K/year

BENEFIT #3: Working Capital Freed
├── Inventory value: $ __________
├── Overstock reduction (from 8% to 2%): 6% improvement
├── = $ __________ of working capital freed
├── Cost of capital (carrying cost): __________% per year
├── Annual benefit: $ __________
└── Example: $135K freed × 30% carrying cost = $40.5K/year

BENEFIT #4: Operational Efficiency
├── Manual allocation time: __________ hours/week
├── Annual hours: __________ × 52
├── Hourly cost: $ __________
├── Annual benefit: $ __________
└── Example: 10 hours × 52 × $50 = $26K/year

BENEFIT #5: Revenue per Square Foot Improvement
├── Current revenue per sqft: __________
├── With better inventory turns: +20% efficiency
├── Additional revenue: $ __________
├── Gross margin: ___________%
├── Annual benefit: $ __________
└── Example: 5K sqft × $450 × 20% = $450K/year

TOTAL ANNUAL BENEFIT: $ __________
Example: $18.72M + $67.5K + $40.5K + $26K + $450K = $19.3M/year
```

**Cost Calculation:**

```
COST: TAI License + Implementation
├── TAI License (Year 1): $40,000
├── Implementation (Shopify/Amazon API): $5-15K
├── Training: Included
└── TOTAL YEAR 1 COST: $ __________
```

**ROI Calculation:**

```
ROI = (Annual Benefit - Annual Cost) / Annual Cost × 100%

Example:
($19,304,000 - $40,000) / $40,000 = 481.6x (or 48,160% return)
```

**Sensitivity Analysis:**

```
Downside Case (20% of projected benefit):
├── Annual benefit: $3,860,800
├── Cost: $40,000
├── ROI: 96x
└── Still exceptional

Upside Case (150% of projected benefit):
├── Annual benefit: $28,956,000
├── Cost: $40,000
├── ROI: 723x
└── Extraordinary
```

---

## How to Use This Calculator

### In a Demo (Real-Time)

1. **Before demo:** Get customer to share key numbers (bed count, GMV, stock-outs, etc.)
2. **During demo:** Ask clarifying questions ("Is that 45 minutes discharged to clean, or discharged to new patient in bed?")
3. **After demo:** Fill in calculator while customer watches
4. **Share result:** "Based on your numbers, TAI delivers $X annual value vs $Y cost"

### In Follow-Up Email

1. **Pre-fill with baseline:** Use numbers from conversation
2. **Allow override:** "Please update if these numbers don't match your actual data"
3. **Emphasize sensitivity:** "Even at 50% of projected benefit, ROI is exceptional"
4. **Send as:** Excel sheet (editable) or Google Sheet (collaborative)

### For Finance Review

1. **Highlight assumptions:** "We're conservative—we only count stock-out prevention, not overstock waste reduction"
2. **Show sensitivity:** Three scenarios (downside, base case, upside)
3. **Benchmark:** "Typical customer in your segment sees X% of these benefits"
4. **Conservative estimate:** "We hit 70% of projected benefit in Year 1, full benefit in Year 2"

---

## Tips for Credibility

### Do This:
- ✅ Use customer's actual numbers (don't assume)
- ✅ Show the math (transparent assumptions)
- ✅ Provide sensitivity analysis (what if we're wrong?)
- ✅ Compare to benchmarks ("Typical customer saves $X")
- ✅ Update after POC ("Here's your actual Month 1 ROI")

### Don't Do This:
- ❌ Overestimate (sets up for disappointment)
- ❌ Hide assumptions (looks like sales BS)
- ❌ Use vague numbers ("Saves significant time")
- ❌ Ignore downside ("Even in worst case, you win")
- ❌ Make promises you can't keep ("Guaranteed 100x ROI")

---

## After POC: Actual ROI Report

**During POC, you'll measure actual numbers. After 30 days, create a report:**

```
ACTUAL ROI - [Customer Name] - Month 1 Results
═════════════════════════════════════════════

PROJECTED vs ACTUAL:

Benefit #1: Stock-Out Prevention
  Projected: 45 events/week prevented
  Actual: 42 events/week prevented
  Achievement: 93% of projection ✓

Benefit #2: Overstock Reduction
  Projected: From 8% → 2%
  Actual: From 8% → 3%
  Achievement: 75% of projection (slower than expected)

TOTAL PROJECTED ANNUAL BENEFIT: $19.3M
ACTUAL MONTH 1 EXTRAPOLATED: $18.2M (94% of projection)

YEAR 1 OUTLOOK:
- Month 1: 94% of projection
- Expected Year 1: 85-90% of projection (conservative estimate)
- Year 2: 100%+ (as you optimize further)

CUSTOMER SATISFACTION:
- NPS: 8.5/10 ✓
- "Already saved us $500K this month"
- "Can't imagine going back to manual allocation"

═════════════════════════════════════════════

OUTCOME: Ready to scale. Recommend expanding to additional channels/departments.
```

---

**Last Updated:** 2026-01-26
**Usage:** Copy to Google Sheets, share with prospect during follow-up email
**Template:** Available for all three verticals (customize healthcare/finance/ecommerce versions)
