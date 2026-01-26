# POC Success Criteria & Measurement Framework

**Status:** Ready for Execution
**Purpose:** Define measurable outcomes that trigger contract signature
**Version:** 1.0

---

## Overview

POC must have **crystal-clear success criteria** that are:
1. **Measurable** (quantifiable, not subjective)
2. **Achievable** (realistic given customer environment)
3. **Time-bound** (measured over 30-day period)
4. **Decision-triggering** (outcome is either "sign contract" or "exit professionally")

---

## Healthcare POC: Hospital Operations

### Success Criteria Framework

**3 Essential Criteria (ALL must pass):**
1. Real-time bed availability accuracy ≥95%
2. Discharge processing time reduction ≥40%
3. Customer satisfaction (NPS) ≥8/10

**If all 3 essential criteria pass: SIGN CONTRACT**

---

### Essential Criterion #1: Bed Availability Accuracy ≥95%

**Definition:** TAI's view of available beds matches actual hospital bed status

**Baseline Measurement (Week 1):**
- Select 5 departments (ICU, orthopedics, general medicine, cardiac, emergency observation)
- Manually verify bed status at 3 time points per day (8am, 12pm, 4pm) for 5 days
- Record: Available beds, occupied beds, being cleaned, under maintenance
- Calculate accuracy: (Beds where TAI status = Actual status) / Total beds checked

**Expected baseline accuracy:** 65-75% (current manual system has gaps)

**TAI System Measurement (Week 2-4):**
- TAI syncs with EHR + nursing call system
- Compare TAI bed status to manual verification at same 3 time points
- Calculate accuracy daily
- Target: ≥95% accuracy

**Success Definition:**
```
95% accuracy means: Out of 100 bed status checks, TAI is correct 95 times
This is "clinically reliable"
(5 errors is within acceptable margin for operations decisions)
```

**How Measured:**
- [ ] Daily accuracy spreadsheet (3 time points × 30 days = 90 data points)
- [ ] Running average tracked (target: 95% by Day 15, maintain through Day 30)
- [ ] Root cause analysis for errors (if < 95%, investigate why)

**Outcome if PASS:** ✅ Criterion #1 achieved
**Outcome if FAIL:** ⚠️ Extend POC 2 weeks (investigate data quality issues)

---

### Essential Criterion #2: Discharge Processing Time ≥40% Reduction

**Definition:** Time from "discharge approved" to "bed clean & ready for next patient"

**Baseline Measurement (Week 1):**
- Hospital IT exports discharge records for past 30 days
- For each discharge: Calculate time from approval to housekeeping completion
- Include:
  - Time for doctor to approve (1-5 min)
  - Time for notification to housekeeping (5-15 min, email lag)
  - Time for cleaning (30-45 min)
  - Time to enter in system (5-10 min)
- Calculate average (typical: 45-50 minutes)

**TAI System Measurement (Week 2-4):**
- TAI automates notification + tracking
- Housekeeping gets instant alert (no email lag)
- TAI tracks cleaning completion (integrates with nurse call system)
- Calculate discharge-to-ready time daily

**Success Definition:**
```
40% reduction = 50 min baseline × 0.60 = 30 minutes target
Or 45 min baseline × 0.60 = 27 minutes target

Typical TAI result: 8-12 minutes (automation removes email/manual delays)
```

**How Measured:**
- [ ] Daily discharge processing time spreadsheet (target: 50+ discharges/day)
- [ ] Running average tracked (target: 30 min by Day 15, maintain through Day 30)
- [ ] Distribution chart (to see if improvement is consistent or just a few lucky days)

**Outcome if PASS:** ✅ Criterion #2 achieved
**Outcome if FAIL:** ⚠️ Investigate integration issues (housekeeping not getting alerts?)

---

### Essential Criterion #3: Customer Satisfaction (NPS) ≥8/10

**Definition:** Operations team's likelihood to recommend TAI (0-10 scale)

**Measurement (Week 4, Friday afternoon):**
- Survey 5+ users from operations team
- Question: "On a scale of 0-10, how likely are you to recommend this system to other hospitals?"
- Also ask: "What's one thing that would make this perfect?"
- Calculate average NPS

**Success Definition:**
```
NPS ≥8 = Strong positive feedback
NPS 6-7 = Cautious, needs improvement
NPS <6 = Negative feedback, investigate issues
```

**How Measured:**
- [ ] Google Form or Typeform survey (anonymous)
- [ ] Follow-up interviews with any <8 responses (understand concerns)

**Outcome if PASS:** ✅ Criterion #3 achieved
**Outcome if FAIL:** ⚠️ Extend POC 2 weeks to address concerns

---

### Contract Signature Trigger

**If all 3 criteria achieved by Day 28:**
1. Calculate total ROI (using their actual metrics)
2. Prepare contract for signature
3. Schedule 30-minute signature meeting
4. Deliver Year 1 implementation timeline

**If any criterion fails by Day 28:**
1. Option A: Extend POC 2 weeks (give more time to fix issues)
2. Option B: Exit professionally ("Timing not right, we'll reconnect Q2")
3. Option C: Downsize scope (pilot with 1 department instead of 5)

---

## Finance POC: Portfolio Risk Management

### Success Criteria Framework

**3 Essential Criteria (ALL must pass):**
1. VaR calculation accuracy ≥95% match to current system
2. Hedging recommendation track record ≥60% CRO implementation rate
3. Compliance audit trail acceptance from Big 4 auditor

**If all 3 essential criteria pass: SIGN CONTRACT**

---

### Essential Criterion #1: VaR Accuracy ≥95% Match

**Definition:** TAI's 1-day VaR calculations match current system within acceptable tolerance

**Baseline Setup (Week 1):**
- Export current portfolio (50+ positions)
- Export past 30 days of market data (prices, volatility, correlations)
- Current VaR calculation process: Document exactly what you do (model, assumptions, parameters)

**TAI System Measurement (Week 2-4):**
- Run TAI on same data, same methodology (variance-covariance, historical, etc.)
- Compare VaR outputs daily
- Calculate % difference: |(TAI VaR - Current VaR) / Current VaR| × 100

**Success Definition:**
```
95% match = Difference within ±0.5% on average
Example:
  Current system VaR: $4.2M
  TAI VaR: $4.21M (±0.24% difference) ✓ PASS

This is "clinically reliable" for risk management
(0.5% difference is noise, not model failure)
```

**How Measured:**
- [ ] Daily VaR comparison spreadsheet (20+ trading days of data)
- [ ] Running average difference tracked
- [ ] Distribution of errors (to see if bias or random noise)
- [ ] Backtesting: Do both models predict losses correctly? (both should have 5% exceedance rate)

**Outcome if PASS:** ✅ Criterion #1 achieved
**Outcome if FAIL:** ⚠️ Investigate model differences (are assumptions different?)

---

### Essential Criterion #2: Hedging Recommendation Implementation Rate ≥60%

**Definition:** % of TAI hedging recommendations that CRO actually implements

**Measurement (Week 2-4):**
- TAI generates hedging recommendations (e.g., "Buy SPY puts", "Rebalance tech exposure")
- Track each recommendation with:
  - What TAI recommended (position, size, strike, expiry)
  - What CRO decided (did they implement it? modify it? reject it?)
  - What was the outcome (if CRO implemented, did it work?)

**Success Definition:**
```
60% implementation = Out of 10 recommendations, CRO acts on 6+
This is "high confidence"

Why 60%? Because:
- Some recommendations will conflict with CRO's conviction (that's OK)
- Some will be too expensive (that's OK)
- But 60% should be "obviously good ideas"
```

**Scoring Details:**
```
Score = 1.0 if: CRO implements TAI recommendation
Score = 0.5 if: CRO implements modified version (we were close)
Score = 0.0 if: CRO rejects recommendation

Success = (Sum of scores / Total recommendations) ≥ 0.60

Example (10 recommendations):
  6 implemented as-is = 6.0 points
  2 modified + implemented = 1.0 points
  2 rejected = 0.0 points
  Total = 7.0 / 10 = 70% ✓ PASS
```

**Also Measure: Post-Hoc Validation**
- For recommendations CRO implemented: Did they work?
- Example: "Buy SPY puts on Dec 15 at $45K cost"
  - Outcome by Dec 20: Puts worth $180K (+$135K value)
  - This recommendation was RIGHT
  - This is evidence TAI's logic is sound

**How Measured:**
- [ ] Recommendation tracking spreadsheet (daily)
- [ ] CRO decision log (implementation or rejection + rationale)
- [ ] 1-week post-hoc review: How did each implemented recommendation perform?

**Outcome if PASS:** ✅ Criterion #2 achieved
**Outcome if FAIL:** ⚠️ Investigate what's causing low implementation
- Is TAI too conservative? Too aggressive?
- Are recommendations too expensive?
- Does CRO not understand the reasoning?

---

### Essential Criterion #3: Auditor Compliance Acceptance

**Definition:** Big 4 auditor reviews TAI system and approves for regulatory filing

**Measurement (Week 4):**
- Present TAI system + audit trail to external auditor
- Auditor reviews: Model, data quality, decision logging, compliance
- Auditor provides written confirmation: "Meets FINRA requirements" or "Needs these changes"

**Success Definition:**
```
PASS: "Meets FINRA requirements"
     Auditor signs off on TAI-generated audit trail
     Can be used for official compliance documentation

CONDITIONAL PASS: "Meets FINRA requirements with minor changes"
     1-2 adjustments needed (documentation, logging format, etc.)
     Can be fixed before Year 1 go-live

FAIL: "Does not meet FINRA requirements"
     Significant changes needed (different model, different methodology)
     Cannot use until substantial redesign
```

**How Measured:**
- [ ] Auditor sign-off email or letter
- [ ] List of required changes (if any)
- [ ] Timeline to fix changes (if needed)

**Outcome if PASS:** ✅ Criterion #3 achieved
**Outcome if CONDITIONAL PASS:** ✅ Criterion #3 achieved (with action items for implementation)
**Outcome if FAIL:** ⚠️ Extend POC 2 weeks (work with auditor on changes)

---

### Contract Signature Trigger

**If all 3 criteria achieved by Day 28:**
1. CRO + CFO sign Year 1 commitment
2. Auditor approves audit trail approach
3. Prepare implementation timeline (Week 1: Bloomberg integration, Week 2-4: go-live)

**If any criterion fails by Day 28:**
1. Option A: Extend POC 2 weeks (work through issues)
2. Option B: Exit professionally ("Timing not right, revisit Q2")
3. Option C: Scale down (pilot with 1 strategy instead of full portfolio)

---

## E-Commerce POC: Inventory Optimization

### Success Criteria Framework

**3 Essential Criteria (ALL must pass):**
1. Stock-out event reduction ≥90% (from 47/week to <5/week)
2. Real-time inventory sync accuracy ≥98% across all channels
3. Customer/warehouse team satisfaction (NPS) ≥8/10

**If all 3 essential criteria pass: SIGN CONTRACT**

---

### Essential Criterion #1: Stock-Out Reduction ≥90%

**Definition:** Number of inventory stock-out events across all channels

**Baseline Measurement (Week 1):**
- Collect past 30 days of Shopify order logs
- Find: Orders that said "out of stock" (customer saw unavailability)
- Collect past 30 days of Amazon Seller Central logs
- Find: Products that went "inactive" due to stock-out
- Collect past 30 days of retail partner orders that couldn't be fulfilled
- Calculate: Total weekly stock-out events (baseline: typically 40-50 for $20M GMV)

**TAI System Measurement (Week 2-4):**
- TAI allocation engine prevents stock-outs in real-time
- Track: How many stock-out events would have occurred without TAI?
- Measure: Actual stock-outs that still happened (e.g., supplier delay)
- Calculate: Weekly event count

**Success Definition:**
```
90% reduction = 47 events/week × 0.10 = <5 events/week target

Typical TAI result: 2-3 events/week
(Some events inevitable: supplier fails to deliver on time, sudden demand spike)
```

**How Measured:**
- [ ] Daily stock-out log (Shopify alerts + Amazon seller alerts + retail partner tracking)
- [ ] Weekly summary spreadsheet
- [ ] Root cause for any stock-out event (preventable vs unpreventable)

**Outcome if PASS:** ✅ Criterion #1 achieved
**Outcome if FAIL:** ⚠️ Investigate preventability
- Are stock-outs due to TAI allocation issues? Or supplier delays?
- If supplier delays, that's not TAI's fault (still good win)

---

### Essential Criterion #2: Real-Time Inventory Sync ≥98% Accuracy

**Definition:** TAI's view of channel inventory matches actual channel inventory

**Baseline Measurement (Week 1):**
- Manual audit: Check Shopify inventory count, compare to actual items in warehouse
- Check Amazon FBA count, compare to Amazon's system
- Check retail partner allocation, verify fulfillment status
- Expected accuracy: 85-90% (normal for manual systems)

**TAI System Measurement (Week 2-4):**
- TAI syncs with Shopify API every 5 minutes
- TAI syncs with Amazon API every 15 minutes
- TAI syncs with warehouse management system continuously
- Compare TAI's view to actual inventory at 3 time points per day
- Calculate accuracy: (Correct counts / Total items checked)

**Success Definition:**
```
98% accuracy = Out of 100 inventory checks, TAI is correct 98 times
This is "highly reliable"

Example:
  Shopify has 150 units (TAI sees 150) ✓
  Amazon has 75 units (TAI sees 75) ✓
  Warehouse has 320 units (TAI sees 320) ✓
  Retail pending: 50 units (TAI sees 50) ✓
  Score: 4/4 = 100% (exceeds 98%)
```

**How Measured:**
- [ ] Daily inventory audit spreadsheet (3 time points × 30 days = 90 data points)
- [ ] Running average accuracy tracked
- [ ] Error analysis: When TAI is wrong, why? (data lag? API failure? warehouse scanner error?)

**Outcome if PASS:** ✅ Criterion #2 achieved
**Outcome if FAIL:** ⚠️ Investigate API reliability
- Is Shopify API consistent?
- Is warehouse scanner giving good data?
- Is there systematic data lag?

---

### Essential Criterion #3: Customer Satisfaction (NPS) ≥8/10

**Definition:** VP Ops + Warehouse Manager satisfaction with TAI system

**Measurement (Week 4, Friday afternoon):**
- Survey 3-5 users (VP Ops + warehouse manager + warehouse staff)
- Question: "On a scale of 0-10, how likely are you to recommend this system to other e-commerce companies?"
- Also ask: "What's one thing that would make this perfect?"
- Calculate average NPS

**Success Definition:**
```
NPS ≥8 = Strong positive, would recommend
NPS 6-7 = Cautious, needs improvement
NPS <6 = Negative, system not ready
```

**How Measured:**
- [ ] Google Form or Typeform survey (anonymous)
- [ ] Follow-up interviews with any <8 responses

**Outcome if PASS:** ✅ Criterion #3 achieved
**Outcome if FAIL:** ⚠️ Extend POC 2 weeks to address concerns

---

### Contract Signature Trigger

**If all 3 criteria achieved by Day 28:**
1. Calculate Year 1 value (using their actual stock-out reduction)
2. Prepare contract for signature ($40K annual cost)
3. Schedule 30-minute signature meeting
4. Deliver 60-day implementation timeline

**If any criterion fails by Day 28:**
1. Option A: Extend POC 2 weeks (fix issues)
2. Option B: Exit professionally ("Timing not right, revisit Q2")
3. Option C: Scale down (pilot with 1 channel instead of 3)

---

## POC Measurement Cadence & Reporting

### Weekly Reporting (Every Friday)

**Format:** 1-page summary with:
- Week's progress on each success criterion
- Key metrics snapshot (accuracy, reduction %, satisfaction)
- Risks or blockers identified
- Recommendations for next week

**Audience:** Customer executive sponsor + TAI project manager

**Example (Healthcare):**

```
WEEK 2 PROGRESS REPORT
═══════════════════════════════════════════
Criterion #1: Bed Availability Accuracy
  Current: 92.3% (target: 95%)
  Trend: ↗ Up from 88% last week
  Status: ON TRACK (should hit 95% by Day 20)

Criterion #2: Discharge Processing Time
  Current: 32 min average (target: 27 min)
  Trend: ↘ Down from 38 min last week
  Status: ON TRACK (achieving >40% reduction)

Criterion #3: Satisfaction (pending Week 4)
  Status: PENDING (survey scheduled for Friday)

Risk: Data quality issue in EHR nursing system
  Impact: 3% accuracy loss due to stale data
  Mitigation: IT team identified root cause (cache issue)
  Timeline: Fixed by Tuesday

Recommendation: On track for contract signature by Day 28
═══════════════════════════════════════════
```

### Day 28 Final Report

**Format:** 5-page summary with:
- Executive summary (do we pass or fail?)
- Detailed results for each criterion
- ROI calculation (using their actual metrics)
- Customer testimonials
- Recommendation (sign contract, extend POC, or exit)

**Example:**

```
POC FINAL REPORT - Healthcare Hospital
═════════════════════════════════════════

EXECUTIVE SUMMARY
✅ All 3 success criteria ACHIEVED
✅ Ready for contract signature

CRITERION #1: Bed Availability Accuracy ≥95%
  Result: 96.2% accuracy
  Evidence: 90 daily audits across 5 departments
  Status: ✅ PASS

CRITERION #2: Discharge Processing Time ≥40% Reduction
  Before: 48 minutes average
  After: 9 minutes average
  Reduction: 81% (far exceeds 40% target)
  Status: ✅ PASS

CRITERION #3: Satisfaction (NPS) ≥8/10
  Result: 8.6 average (5 respondents)
  Feedback: "Transformed how we manage beds"
  Status: ✅ PASS

FINANCIAL IMPACT
  Stock-out prevention: $2M/year
  Efficiency gains: $50K/year
  Total Year 1 value: $2.05M
  Cost: $24K/year
  ROI: 85x

RECOMMENDATION
Proceed with Year 1 contract signature.
Implementation timeline: 30 days (Weeks 6-9)
═════════════════════════════════════════
```

---

## Exit Criteria (When to End POC)

### Exit Scenario 1: All criteria pass (Most common)
**Action:** Sign contract, begin Year 1 implementation
**Timeline:** Week 29 onward

### Exit Scenario 2: 2 of 3 criteria pass, 1 is close
**Decision:** Extend POC 2 weeks (give time to fix remaining issue)
**Risk:** Budget overrun (customer may not want to pay for extended POC)
**Mitigation:** Offer extended POC at 50% discount or free

### Exit Scenario 3: Only 1 criterion passes, progress is slow
**Decision:** End POC professionally, offer to revisit Q2
**Talking points:**
- "Your use case is important to us"
- "But based on current progress, we'd recommend waiting for [Q2/after peak season]"
- "We'll stay in touch—revisit Q2 when conditions are better"
- "No cost for this POC"

### Exit Scenario 4: Criterion fails due to technical blocker
**Decision:** If fixable: Extend POC 1-2 weeks. If not fixable: Exit
**Example:** "EHR data quality issues prevent accurate measurement"
- Fixable: "IT team can fix data quality by next week"
- Not fixable: "Your EHR version is too old, integration not possible"

---

## Document Retention

**After POC ends, keep:**
- [ ] All measurement data (spreadsheets)
- [ ] Weekly progress reports
- [ ] Final POC report
- [ ] Customer emails + feedback
- [ ] Success/failure analysis

**Why:** Create case study + learn what works/doesn't work for next customer

---

**Last Updated:** 2026-01-26
**Owner:** VP Sales + Customer Success Lead
