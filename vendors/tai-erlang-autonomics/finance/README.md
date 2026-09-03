# TAI Erlang Autonomics - Financial Model Suite

Complete financial projections for TAI Erlang Autonomics, ready for investor review.

## Files Included

### 1. FINANCIAL_MODEL.xlsx (24 KB)
**Comprehensive Excel workbook with 8 sheets:**

- **Summary:** Executive dashboard with 3-year overview + key milestones
- **Conservative Scenario:** 36-month month-by-month projection (slower adoption)
- **Base Case Scenario:** 36-month month-by-month projection (target)
- **Optimistic Scenario:** 36-month month-by-month projection (strong adoption)
- **Unit Economics:** Detailed tier-by-tier analysis + LTV/CAC calculations
- **Headcount & OpEx:** Monthly expense breakdown for Year 1 + forecast
- **Fundraising & Cap Table:** Funding rounds, valuation, and equity dilution
- **Sensitivity Analysis:** Impact analysis on key variables

**Key metrics in workbook:**
- Monthly projections: customers, revenue, COGS, OpEx, net income, cumulative cash
- Unit economics: ARPU, CAC, LTV, payback period by tier
- Breakeven analysis and cash runway calculations
- Sensitivity to customer acquisition, churn, CAC, and margin changes

### 2. FINANCIAL_SUMMARY.md (29 KB)
**Detailed 10-section narrative analysis:**

1. Revenue Model (3-tier pricing with ROI examples)
2. Gross Margin & Unit Economics (99% margins, cost breakdown)
3. Operating Expenses & Headcount (hiring plan + monthly OpEx)
4. Cash Flow & Runway (burn rate + funding timeline)
5. Unit Economics Deep Dive (LTV, CAC, payback by tier)
6. Fundraising Timeline & Cap Table (pre-seed → series A)
7. Key Metrics & Leverage Points (sensitivity analysis)
8. Risk Factors & Mitigations (competitive, customer, financial)
9. Pro Forma Financial Statements (P&L + cash flow)
10. Appendices (assumptions, glossary, model guide)

**Use this document for:**
- Investor discussions
- Board presentations
- Due diligence responses
- Financial planning

### 3. EXECUTIVE_BRIEF.md (5 KB)
**One-page summary for quick reference:**

- Revenue model overview
- 3-year financial highlights
- Unit economics comparison
- Funding timeline
- Risk mitigation
- Key decision points

**Use this document for:**
- Elevator pitch
- Quick stakeholder updates
- Email attachments
- First impression before detailed review

### 4. financial_model_builder.py (30 KB)
**Python script to regenerate the Excel model:**

Parameterized builder that creates all sheets with proper formatting. Modify parameters to:
- Change customer acquisition rates
- Adjust pricing tiers
- Update headcount assumptions
- Regenerate sensitivities

**Usage:**
```bash
python3 financial_model_builder.py
```

---

## Key Financial Metrics (Base Case)

### 3-Year Summary
| Period | Revenue | Customers | OpEx | Profit | Cash |
|--------|---------|-----------|------|--------|------|
| Year 1 | $450K | 15 | $660K | -$229K | -$229K |
| Year 2 | $1.7M | 60 | $900K | +$768K | +$539K |
| Year 3 | $4.7M | 112 | $1.1M | +$3.4M | +$3.9M |

### Unit Economics
- **Gross Margin:** 99% (infrastructure play)
- **COGS per customer:** $345/month
- **CAC:** $3,500
- **CAC Payback:** 1.6 months
- **LTV/CAC:** 27x (exceptional)
- **Churn:** 1% monthly (strong)

### Funding Path
- **Pre-seed:** $500K → 10-12 month runway
- **Seed:** $1.2M → 18-24 months (growth capital)
- **Series A:** $5M → 24-36+ months (scale)

### Breakeven
- **Month:** 18 (1.5 years)
- **With Seed funding:** Yes, achievable
- **Customer count:** 30-35 customers

---

## How to Use This Model

### For Investor Pitches
1. Start with EXECUTIVE_BRIEF.md (1-2 min read)
2. Deep dive with FINANCIAL_SUMMARY.md (key sections)
3. Reference FINANCIAL_MODEL.xlsx for specific questions

### For Financial Planning
1. Update assumptions in FINANCIAL_MODEL.xlsx "Base Case" sheet
2. Regenerate scenarios using financial_model_builder.py
3. Track actuals vs projections monthly

### For Hiring Decisions
1. Check "Headcount & OpEx" sheet for planned headcount
2. Compare vs cash runway in "Base Case Scenario"
3. Adjust hiring plans if acquisition slows

### For Fundraising
1. Review "Fundraising & Cap Table" sheet for equity implications
2. Check cash runway in base case
3. Identify trigger milestones for next round

---

## Scenario Comparison

### Conservative (Slower Adoption)
- Monthly new customers: 2 (vs 1.25 base)
- Year 3 revenue: $3.4M
- Breakeven: Month 18
- Cash at Y3: $2.8M
- **Use case:** Realistic downside scenario

### Base Case (Target)
- Monthly new customers: 1.25 (Y1), 2.5 (Y2), 4 (Y3)
- Year 3 revenue: $4.7M
- Breakeven: Month 18
- Cash at Y3: $4.7M
- **Use case:** Primary financial plan

### Optimistic (Strong Adoption)
- Monthly new customers: 1.25 (Y1), 3.33 (Y2), 5.25 (Y3)
- Year 3 revenue: $7.6M
- Breakeven: Month 12
- Cash at Y3: $7.5M
- **Use case:** Upside scenario, Series A accelerator

---

## Key Assumptions

### Customer Economics
- Blended ARPU Year 1: $30K ($2.5K-$25K tiered)
- Average sales cycle: 60 days
- Monthly churn: 1% (12% annual)
- Net retention rate: 105% (expansion revenue)

### Costs
- COGS per customer: $345/month
- Support labor: 1.5 hr/month @ $75/hr
- Fixed OpEx: $40K/month (personnel base)
- Variable OpEx: $1,333/customer (allocation)

### Headcount
- Year 1: 3.75 → 5.25 FTE
- Year 2: 8-10 FTE
- Year 3: 12-15 FTE

### Funding
- Pre-seed burn rate: -$35K/month
- Seed enables 1.5 years growth
- Series A enables enterprise expansion

---

## Sensitivity Analysis Highlights

**What changes cash runway by 6 months?**

| Variable | Change | Impact |
|----------|--------|--------|
| Customer acquisition | ±30% | ±9 months |
| Monthly churn | ±1% | ±6 months |
| CAC | ±$2K | ±4 months |
| OpEx | ±$10K/mo | ±3 months |
| Gross margin | ±3% | ±2 months |

**Strategic focus:** Acquisition efficiency + retention = biggest levers

---

## Monthly Metrics to Track

**Acquisition:**
- New customers/month (target 1.25+)
- Sales cycle length (target <60 days)
- Win rate (target >25%)

**Retention:**
- Monthly churn % (target <1.5%)
- Net retention rate (target >105%)
- NPS score (target >50)

**Efficiency:**
- CAC (target <$3.5K)
- Payback period (target <8 months)
- LTV/CAC ratio (target >20x)

**Cash:**
- Monthly burn rate
- Cash runway (alert if <4 months)
- Revenue growth rate (month-over-month)

---

## Questions & Support

**About the model:**
See FINANCIAL_SUMMARY.md Appendix A (Assumptions) and Appendix B (Glossary)

**Modifying assumptions:**
Edit FINANCIAL_MODEL.xlsx base scenario, or regenerate using financial_model_builder.py

**Extending forecast:**
Copy formulas down in monthly sheets to extend beyond 36 months

**Different scenarios:**
Use Conservative/Base/Optimistic sheets as templates, modify parameters

---

## Document Versions

- **v1.0.0** - Initial model (Jan 25, 2026)
- Covers 3-year base case + conservative/optimistic scenarios
- Includes detailed unit economics + cap table
- Ready for investor review

---

**Created:** January 25, 2026
**Status:** Ready for investor review
**Classification:** Confidential - Internal Use Only
