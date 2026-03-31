# Week 3-4: Demo Environment & POC Structure

**Status:** Ready for Execution
**Target:** Close customer demo in Week 4
**Success Metric:** 3 vertical-specific demos ready, 1st POC kickoff Week 5

---

## Mission Statement

Convert technical capability into customer conviction through immersive, outcome-driven demos that answer one question: **"What's the measurable ROI for us?"**

This is not a feature walkthrough—it's a **proof theater** where customers experience their own data transforming into value.

---

## Quick Links

- **Demo Scripts:** See `/DEMO_SCRIPTS/` directory
- **Demo Data:** See `DEMO_DATA_SETUP.md`
- **FAQ Document:** See `DEMO_FAQ.md`
- **ROI Calculator:** See `ROI_CALCULATOR.md`
- **POC Success Criteria:** See `POC_SUCCESS_CRITERIA.md`
- **Post-Demo Follow-up:** See `POST_DEMO_SEQUENCE.md`

---

## Phase 1: Demo Environment Setup (Week 3)

### Objective
Create isolated, customer-safe demo environment that mirrors production architecture without exposing sensitive data.

### Architecture

```
PRODUCTION ENVIRONMENT
├── Live Customer Data
├── Real Erlang Clusters
└── Production Database

DEMO ENVIRONMENT (WEEK 3)
├── Demo Data Layer
│   ├── Healthcare (10 sample hospitals + patient flows)
│   ├── Finance (5 sample investment portfolios + transactions)
│   └── E-commerce (3 sample Shopify stores + inventory)
├── Staging Erlang Clusters
├── Demo Database (PostgreSQL with anonymized data)
├── Metrics & Telemetry (real-time dashboard)
├── Receipt Generation (cryptographic proof system)
└── Replayable Scenarios (deterministic customer journeys)
```

### Deliverables (Week 3)

| Task | Owner | Timeline | Success Metric |
|------|-------|----------|----------------|
| Deploy staging environment | Backend | Mon-Tue | All 3 Erlang nodes healthy |
| Load demo data | Backend | Tue-Wed | 100% data integrity verified |
| Setup monitoring dashboard | DevOps | Wed-Thu | Real-time metrics visible |
| Receipt generation system | Backend | Thu | SHA-256 hashes generated for audit |
| Load testing (baseline) | QA | Thu-Fri | Latency SLA: <200ms p95 |
| Customer data anonymization | Legal/Backend | Fri | GDPR compliance verified |

**Daily Standup (Backend Lead):**
- Mon: "Environment deployed; network configured"
- Tue: "Demo data loaded; initial tests passing"
- Wed: "Monitoring dashboard live; TLS certificates installed"
- Thu: "Receipt generation complete; baseline metrics captured"
- Fri: "Load testing complete; environment ready for demo ✓"

---

## Phase 2: Vertical-Specific Demo Scenarios (Week 3-4)

### Overview

Three deep-dive scenarios targeting primary customer segments. Each demo:
- **Duration:** 15 minutes (5-min problem + 5-min solution + 5-min proof)
- **Format:** Live data transformation (not slides)
- **Audience:** Prospect executives + technical stakeholders
- **Outcome:** Signed POC proposal within 48 hours

### Demo Scenario 1: Healthcare (Hospital Operations)

**Target Prospect:** Hospital operations director managing 300-bed facility

**Problem Statement (2 min):**
- Manual bed allocation across 5 departments takes 45 minutes
- Nurse shift scheduling creates 12-hour gaps
- Patient discharge delays cascade (surgeries get cancelled)
- Current system: spreadsheets + email threads
- Cost of inefficiency: $50K/month in cancelled procedures

**Solution Demo (5 min):**
1. **Load sample hospital data** (10 beds, 3 departments, 50 patients)
   - Show current state: scattered across multiple systems
   - Timestamp: real hospital operations from 6am-6pm

2. **Run TAI autonomic system**
   - Sync patient records → bed availability → nurse assignments
   - Show deterministic decision flow (RDF reasoning)
   - Display receipt with SHA-256 hash (proof of processing)

3. **Show dashboard insights**
   - Bed utilization: from 62% → 89%
   - Nurse assignment gaps: from 12 → 0 hours
   - Discharge processing: from 45min → 8min
   - Surgery cancellation: 0 (vs 3 forecasted without TAI)

4. **Calculate ROI**
   - Cancelled surgery cost: $15K each
   - 3 prevented cancellations = $45K savings/week
   - System cost: $2K/month → 22.5x ROI in Year 1

**Proof (3 min):**
- Show cryptographic receipt (audit trail)
- Demonstrate deterministic replay (same input = same output always)
- Show GDPR compliance (patient data never leaves encryption)
- Explain implementation timeline (30-day go-live)

**Demo Data Location:** `DEMO_DATA/healthcare_hospital_ops.json`
**Demo Script:** `DEMO_SCRIPTS/DEMO_HEALTHCARE.md`

---

### Demo Scenario 2: Finance (Portfolio Risk Management)

**Target Prospect:** Chief Risk Officer at $500M AUM hedge fund

**Problem Statement (2 min):**
- Daily portfolio risk calculations take 8 hours (decision made 4pm, risk measured 12am)
- By next morning, market moved 2% (decisions stale)
- Current system: manual Excel models + Bloomberg terminal
- Missed hedging opportunity in Dec: -$2.1M loss
- CRO spends 60% of time on manual calculations instead of strategy

**Solution Demo (5 min):**
1. **Load sample portfolio data**
   - 50 positions across 8 asset classes
   - Real market data from Jan 22-25, 2026
   - Correlated risk factors: equities, rates, FX, commodities

2. **Run TAI autonomic system**
   - Ingest portfolio + market data
   - Calculate VaR, Greeks, correlation matrix
   - Auto-suggest hedges based on risk policy
   - Generate audit receipt with exact calculation trace

3. **Show dashboard insights**
   - Portfolio VaR: $4.2M (within 1% limit)
   - Risk concentration: 12% in tech (policy: <15%)
   - Recommended hedge: $500K short SPY
   - Execution receipt with timestamp + digital signature

4. **Calculate ROI**
   - Time saved: 7.5 hours/day = 37.5 hrs/week = $50K/week (CRO salary)
   - Hedging improvement: 2% portfolio gain = $10M upside (conservative)
   - Annual ROI: $2.6M savings + upside vs $50K cost = 52x

**Proof (3 min):**
- Show audit receipt (every calculation traceable)
- Demonstrate 8-hour delay eliminated (real-time risk)
- Show FINRA compliance logging (regulatory-grade audit trail)
- Explain SOC 2 Type II certification requirement

**Demo Data Location:** `DEMO_DATA/finance_portfolio_risk.json`
**Demo Script:** `DEMO_SCRIPTS/DEMO_FINANCE.md`

---

### Demo Scenario 3: E-Commerce (Inventory Optimization)

**Target Prospect:** VP Operations at $20M GMV DTC fashion brand

**Problem Statement (2 min):**
- Inventory out-of-stock events cost $500K/month in lost sales
- Manual allocation between Shopify, Amazon, offline stores
- Warehouses run 25% overstock in slow SKUs
- Current system: separate dashboards, no real-time sync
- Competitor just launched smart inventory (potential loss of market share)

**Solution Demo (5 min):**
1. **Load sample inventory data**
   - 3 sales channels (Shopify, Amazon, retail)
   - 500 SKUs with demand patterns
   - Real inventory levels from Jan 2026
   - Supply constraints (limited warehouse space)

2. **Run TAI autonomic system**
   - Sync live inventory across all channels
   - Predict demand with machine learning model
   - Auto-allocate stock to maximize revenue
   - Generate allocation receipt with reasoning chain

3. **Show dashboard insights**
   - Stock-out events: 47 → 2 (96% reduction)
   - Overstock: 25% → 8% (avoiding waste)
   - Revenue uplift: +12% from better allocation
   - Working capital freed: $180K (reduce inventory holding)

4. **Calculate ROI**
   - Prevented lost sales: $500K × 12 months = $6M
   - Reduced overstock waste: $180K/year
   - Working capital improvement: $180K freed
   - Annual ROI: $6.36M upside vs $40K cost = 159x

**Proof (3 min):**
- Show live Shopify sync (real-time connection)
- Demonstrate allocation receipt (why each decision made)
- Show integration complexity handled (3 APIs, real-time)
- Explain 60-day implementation (Shopify API + warehouse sync)

**Demo Data Location:** `DEMO_DATA/ecommerce_inventory_optimization.json`
**Demo Script:** `DEMO_SCRIPTS/DEMO_ECOMMERCE.md`

---

## Phase 3: Demo Script Standards (Week 3)

### Universal Demo Format

Every demo follows this structure:

1. **Setup (1 min)**
   - Greet prospect
   - Confirm objectives for today
   - Explain: "We'll see your data flow through our system in real-time"

2. **Problem (2 min)**
   - Paint current pain (cost, time, risk)
   - Show their metrics before
   - Ask: "Does this match your situation?" (confirm relevance)

3. **Solution (5 min)**
   - Show data transformation live
   - Narrate decision logic (why TAI chose this action)
   - Display metrics after
   - Show cryptographic receipt (proof it happened)

4. **Proof (3 min)**
   - Demonstrate determinism (replay same input)
   - Show compliance/audit trail
   - Explain implementation timeline + cost
   - Calculate ROI specific to their situation

5. **Close (2 min)**
   - "Would a POC for 30 days make sense?"
   - Address immediate concerns
   - If yes: "Let's discuss scope on Friday"

### Facilitation Checklist

**Before Demo (Day Before)**
- [ ] Test all data loads (verify no breakage since yesterday)
- [ ] Verify network connectivity (WiFi backup ready)
- [ ] Check projection equipment (HDMI, audio working)
- [ ] Rehearse talking points (5-min run-through)
- [ ] Confirm prospect attendee list (notify if >5 people)
- [ ] Send pre-demo email (objectives + agenda)

**During Demo (30 min before start)**
- [ ] Load demo environment (refresh data from staging)
- [ ] Have phone number ready for backup (screen share via Zoom)
- [ ] Pull up ROI calculator (custom to their metrics)
- [ ] Have FAQ document on hand (quick reference)
- [ ] Confirm audio/video (ask "Can you see the screen?")

**After Demo (immediately)**
- [ ] Ask NPS question: "On 0-10, how valuable was this?" (target: 8+)
- [ ] Collect feedback: "What would make this perfect?"
- [ ] Schedule follow-up (48-hour proposal delivery)
- [ ] Send thank-you email with recap + next steps

---

## Phase 4: Baseline Measurement Procedure (Week 4)

### Why Measurement Matters

Prospects need proof that TAI delivers value. This requires:
1. **Before metric** (current state)
2. **After metric** (TAI running for 30 days)
3. **Attribution** (how much improvement came from TAI vs other factors?)

### Measurement Framework

#### For Healthcare (Hospital Ops)

**Metric 1: Bed Utilization Rate**
- Definition: (Occupied beds / Total beds) × 100%
- Baseline: Manual tracking from hospital records
- Measurement: Check hospital schedule daily × 30 days
- Expected improvement: +15-25% (fewer gaps)
- ROI: +25% utilization × 5 beds × $5K/week = +$25K/week value

**Metric 2: Discharge Processing Time**
- Definition: Time from "discharge approved" to "bed clean"
- Baseline: Average 45 minutes (from current records)
- Measurement: TAI system tracks with millisecond precision
- Expected improvement: -30 minutes (-67%)
- ROI: 30-min × 10 discharges/day × $500 per hour = +$2,500/day

**Metric 3: Nurse Scheduling Gaps**
- Definition: Hours per shift with insufficient staff
- Baseline: 12+ hours/week (from current scheduling)
- Measurement: Automated detection in TAI system
- Expected improvement: 0 gaps
- ROI: 0 emergency recalls × $200/each = elimination of cost

#### For Finance (Portfolio Risk)

**Metric 1: Risk Calculation Latency**
- Definition: Time from market close to VaR calculation complete
- Baseline: 8 hours (overnight batch process)
- Measurement: TAI system timestamps every calculation
- Expected improvement: <5 minutes (real-time)
- ROI: Earlier decision-making → prevents $2M+ hedging misses

**Metric 2: Hedging Accuracy**
- Definition: % of recommended hedges that CRO implements (confidence metric)
- Baseline: 40% (CRO unsure about Excel model outputs)
- Measurement: Audit trail shows every recommendation + outcome
- Expected improvement: 85%+ (higher confidence from audit trail)
- ROI: 85% × 50 recommendations/month × $100K avg value = $4.25M/month upside

**Metric 3: Compliance Audit Time**
- Definition: Hours per quarter spent on audit trail preparation
- Baseline: 60 hours/quarter (gathering scattered documentation)
- Measurement: TAI generates automated FINRA-grade audit trail
- Expected improvement: 5 hours/quarter (just review + sign-off)
- ROI: 55 hours saved × $250/hour = $13.75K/quarter

#### For E-Commerce (Inventory Ops)

**Metric 1: Stock-Out Frequency**
- Definition: # of inventory stock-out events per week
- Baseline: 47 events/week (from Shopify logs)
- Measurement: TAI allocation engine prevents stock-outs
- Expected improvement: <2 events/week (<5% of baseline)
- ROI: 45 events × $500 lost margin/event × 52 weeks = $1.17M/year

**Metric 2: Overstock Waste**
- Definition: % of inventory that never sells (written off)
- Baseline: 8% of total inventory value ($180K on $2.25M inventory)
- Measurement: TAI allocation minimizes slow-moving SKUs
- Expected improvement: 2% overstock ($45K on same inventory)
- ROI: $135K improved cash flow + reduced waste

**Metric 3: Revenue per Square Foot (Warehouse)**
- Definition: Annual revenue / warehouse space
- Baseline: $450/sqft (typical for 5K sqft warehouse)
- Measurement: Better inventory allocation increases turns
- Expected improvement: $540/sqft (+20%)
- ROI: $45K additional revenue from same space

### Measurement Cadence

**Week 1 (Before TAI):** Capture baseline metrics
- Healthcare: Manually track bed utilization daily
- Finance: Record current risk calculation process
- E-Commerce: Export Shopify stock-out data

**Week 2-4 (TAI System Running):** Capture ongoing metrics
- Healthcare: Daily bed utilization from TAI dashboard
- Finance: Daily risk calculations from TAI system
- E-Commerce: Daily allocation efficiency from TAI system

**End of Week 4:** Compare & calculate ROI
- Side-by-side comparison (before vs after)
- Attribution analysis (what % of improvement is from TAI?)
- Customer testimonial (quote their success metric)
- ROI summary (financial validation)

### Customer-Facing Report Template

See `MEASUREMENT_REPORT_TEMPLATE.md` for detailed structure.

---

## Phase 5: POC Success Criteria (Week 4)

### Why Success Criteria Matter

POC must have **measurable outcomes** that trigger next decision (expand or exit). Vague success ("seems to be working") kills deals.

Each vertical has 3-5 must-have success criteria:

### Healthcare POC Success Criteria

**Essential Criteria (Must have all 3):**
1. ✅ Real-time bed availability ≥95% accuracy
   - Hospital provides actual bed state
   - TAI system matches within 5 minutes
   - Measured continuously for 30 days

2. ✅ Discharge processing documented ≥40% faster
   - Before: 45 min average
   - After: <27 min average
   - Evidence: TAI audit trail + manual spot-checks

3. ✅ NPS feedback ≥8 from operations team
   - Minimum 3 users surveyed
   - Question: "How likely to recommend TAI?" (0-10)
   - Target: Average 8+

**Secondary Criteria (Must have 2 of 3):**
4. ⭕ Nursing staff adoption ≥80%
   - % of shifts using TAI scheduling
   - Measured via system login data
   - Target: 4 out of 5 daily shifts use system

5. ⭕ Patient satisfaction improvement visible
   - Fewer discharge delays → faster time-to-discharge
   - Patient survey: "Wait time to discharge"
   - Baseline + post measurement required

6. ⭕ Budget allocated for Year 1 implementation
   - Hospital finance approves $50K commitment
   - Signed in letter or email
   - Triggers execution phase

**Deal Closure Trigger:**
- If all 3 essential + 2 secondary: **Proceed to contract**
- If 3 essential + 1 secondary: **Extend POC 2 weeks**
- If <3 essential: **Exit POC professionally** (not fit now, revisit in 6 months)

### Finance POC Success Criteria

**Essential Criteria (Must have all 3):**
1. ✅ Risk calculations provided ≥daily (real-time capability proven)
   - TAI generates VaR, Greeks, correlation matrix daily
   - Results match Bloomberg terminal ±0.5%
   - Audit trail shows every calculation

2. ✅ CRO confidence in automated recommendations ≥80%
   - Present 10 hedging recommendations
   - CRO implements ≥8 of them
   - Retrospective: "Would we choose same hedge manually?"

3. ✅ Compliance audit trail accepted by auditors
   - Big 4 auditor review TAI-generated logs
   - Written confirmation: "Meets FINRA requirements"
   - Triggers compliance sign-off

**Secondary Criteria (Must have 2 of 3):**
4. ⭕ Portfolio performance improvement measurable
   - Compare hedging outcomes (TAI vs manual)
   - 30-day backtesting: TAI hedges outperform by ≥1%
   - $500M portfolio = $5M+ value captured

5. ⭕ Integration complexity resolved
   - Bloomberg API connected + stable
   - Risk database syncs in <5 minutes
   - No manual data entry required

6. ⭕ Budget allocated for Year 1 implementation
   - CRO signs $75K annual cost commitment
   - Finance approves in Q1 budget

**Deal Closure Trigger:**
- If all 3 essential + 2 secondary: **Proceed to contract**
- If 3 essential + 1 secondary: **Extend POC 2 weeks** (build auditor confidence)
- If <3 essential: **Exit POC** (timing not right, revisit Q2)

### E-Commerce POC Success Criteria

**Essential Criteria (Must have all 3):**
1. ✅ Real-time inventory sync across all 3 channels ≥98% accuracy
   - Shopify, Amazon, retail POS synchronized
   - Inventory matches within 2 hours
   - No manual adjustments needed

2. ✅ Stock-out events reduced ≥90% (from 47 → <5/week)
   - Measured via Shopify order management
   - TAI allocation prevents out-of-stock scenarios
   - Revenue impact quantified

3. ✅ Implementation roadmap agreed (60-day execution)
   - Technical architect confirms integration feasibility
   - VP Ops signs off on Shopify API access
   - Finance approves $40K annual cost

**Secondary Criteria (Must have 2 of 3):**
4. ⭕ Overstock reduction verified ≥50%
   - Before: 8% of inventory never sells
   - After: <4% waste rate
   - Measured via inventory audits (physical or system)

5. ⭕ Revenue per SKU improvement ≥15%
   - Better allocation → fewer discounts needed
   - Margin per SKU improves from $120 → $138 average
   - Data from order analysis

6. ⭕ Warehouse utilization optimized
   - Same space, 20% more inventory turns
   - Space efficiency metrics improve
   - Finance validates working capital release

**Deal Closure Trigger:**
- If all 3 essential + 2 secondary: **Proceed to contract**
- If 3 essential + 1 secondary: **Fast-track implementation** (strong commitment signals)
- If <3 essential: **Exit POC** (poor-fit segment, move to next prospect)

---

## Phase 6: Receipt Generation Demo (Week 4)

### Purpose

Demonstrate TAI's cryptographic audit trail—the secret weapon that unlocks enterprise trust.

**Demo Talking Points:**

1. **The Problem:** "Automated systems make decisions, but how do you explain them?"
   - Every decision in healthcare, finance, e-commerce must be auditable
   - Current systems: black boxes (why was this SKU allocated there?)
   - Regulators demand: Show your work

2. **TAI Receipt Solution:** "Cryptographic proof of every decision"
   - Input data → Processing logic → Output decision
   - SHA-256 hash of entire calculation
   - Digital signature proving no tampering
   - Timestamp + originating system verified

3. **What Makes It Special:**
   - **Deterministic:** Same input always produces same output
   - **Immutable:** Change one bit, hash changes completely
   - **Auditable:** 100% calculation trace available
   - **Compliant:** Exceeds GDPR, FINRA, HIPAA requirements

### Receipt Generation Flow (Live Demo)

**Step 1: Load sample data**
```
INPUT: Hospital bed allocation
- Patient A: needs ICU (priority 8)
- Patient B: needs general ward (priority 4)
- Available ICU beds: 2
- Available general beds: 5
```

**Step 2: Run decision engine**
```
PROCESSING:
1. Validate inputs (patient data complete? ✓)
2. Apply rules (priority + clinical needs)
3. Generate allocation (Patient A → ICU-1, Patient B → Ward-2)
4. Calculate hash of entire decision
```

**Step 3: Display receipt**
```
RECEIPT:
┌─────────────────────────────────────────────┐
│ TAI DECISION RECEIPT                        │
│ Date: 2026-01-26 14:32:15 UTC              │
│ System: Healthcare Operations               │
│ Decision: Bed Allocation                    │
├─────────────────────────────────────────────┤
│ Inputs:                                     │
│  - 2 patients (clinical profiles)           │
│  - 7 available beds (5 ICU, 2 general)      │
│  - Hospital policy (priority + acuity)      │
├─────────────────────────────────────────────┤
│ Decisions:                                  │
│  - Patient A → ICU-1 (score: 9.2)           │
│  - Patient B → Ward-2 (score: 7.1)          │
├─────────────────────────────────────────────┤
│ Hash: 7a3f2c8e9d1b4e6f...                  │
│ Signature: Valid (TAI Private Key)          │
│ Timestamp: Verified (UTC, no skew)          │
│ Status: IMMUTABLE (hash blocks tampering)   │
└─────────────────────────────────────────────┘
```

**Step 4: Demonstrate immutability**
```
ATTACK TEST: "What if someone changes Patient A's priority?"
Original hash: 7a3f2c8e9d1b4e6f
Tampered hash: 2b4e8c1f3d5a7g9h
Mismatch! ❌ Tampered document detected

This is FINRA/HIPAA-grade audit trail proof.
```

### Why This Wins Deals

**For Healthcare:** "We can prove to regulators exactly why we made each bed allocation. Zero compliance risk."

**For Finance:** "Our hedging decisions are FINRA-auditable. We can defend every trade with cryptographic proof."

**For E-Commerce:** "We can explain inventory allocation to customers & auditors. Complete transparency."

---

## Phase 7: Dashboard Walkthrough (Week 4)

### Dashboard Purpose

Show customer their data, their metrics, their ROI—not ours.

### Dashboard Layout (3 screens)

**Screen 1: Real-Time Operations (Active)**
- Healthcare: Current bed occupancy, discharge queue, nurse assignments
- Finance: Current portfolio VaR, Greeks, top risks
- E-Commerce: Current inventory by channel, stock-out predictions, allocation recommendations

**Screen 2: Historical Trends (Past 7 Days)**
- Healthcare: Bed utilization trend, discharge time trend, policy violations trend
- Finance: VaR trend, hedging recommendations vs outcomes, compliance score
- E-Commerce: Stock-out prevention wins, revenue optimization, overstock reduction

**Screen 3: ROI Calculator (Impact)**
- Healthcare: Bed utilization gain → $ revenue impact, discharge speedup → $ saved
- Finance: Risk reduction → $ hedging savings, confidence gain → strategy hours freed
- E-Commerce: Stock-out prevention → $ revenue, overstock reduction → $ waste prevented

### Walkthrough Script (5 minutes)

1. **"Here's your live data"** (90 sec)
   - Show real-time operations screen
   - Point out key metric (bed utilization, VaR, stock level)
   - Explain what you're looking at

2. **"This is what improved"** (90 sec)
   - Show 7-day trend
   - Compare before vs after
   - Quantify the improvement

3. **"Here's your ROI"** (90 sec)
   - Show ROI calculator
   - Input their metrics (custom to their situation)
   - Show $ impact (annual savings, upside, or risk reduction)

4. **"Questions?"** (30 sec)
   - Pause for customer questions
   - Don't over-explain (let dashboard tell the story)

### Key Metrics by Vertical

**Healthcare:**
- Bed Utilization: Current % + 7-day trend
- Discharge Time: Current avg + distribution chart
- Nurse Gaps: # gaps per day + trend
- ROI: $ saved from faster discharges + prevented cancellations

**Finance:**
- Portfolio VaR: $X with distribution chart
- Hedging Win Rate: % of recommendations CRO acts on
- Risk Events: # of tail risks detected early (saved $ by hedging)
- ROI: $ saved from better hedging + time freed for strategy

**E-Commerce:**
- Stock-Out Events: # per week + trend (downward = winning)
- Inventory Turns: Current rate + optimization target
- Revenue per SKU: Current + post-TAI
- ROI: $ revenue retained from stock-out prevention + $ waste eliminated

---

## Phase 8: FAQ Document (Week 4)

See `DEMO_FAQ.md` (separate file).

Common questions:
- "How long does implementation take?"
- "Will this work with our legacy systems?"
- "How do you ensure data privacy?"
- "What if your AI makes a bad decision?"
- "Can we customize the logic?"
- "How much does it cost?"

---

## Phase 9: ROI Calculator (Week 4)

See `ROI_CALCULATOR.md` (separate file).

Prospect-facing tool to calculate annual ROI for their specific situation:
- Input: Current metrics (bed utilization, portfolio size, GMV)
- Output: Projected savings + upside + ROI percentage
- Formula: (Annual Benefit - Annual Cost) / Annual Cost × 100%

Example for hospital:
- Current state: 62% bed utilization, 45-min discharge time
- Projected with TAI: 89% utilization, 8-min discharge
- Annual value: $450K (3 prevented cancellations × $15K each per week)
- Annual cost: $24K (TAI system)
- ROI: (450K - 24K) / 24K = 1,775% (18.75x)

---

## Phase 10: Case Study Template (Week 4)

See `CASE_STUDY_TEMPLATE.md` (separate file).

Will be filled in post-customer-launch. Template includes:
- Executive summary (1 page)
- Problem statement (their metrics before)
- Solution implementation (30-day timeline)
- Results (metrics after)
- ROI summary ($X value delivered)
- Customer quote (permission required)
- Metrics visual (before/after chart)

---

## Phase 11: Post-Demo Follow-Up Sequence (Week 4)

See `POST_DEMO_SEQUENCE.md` (separate file).

Three-touch follow-up sequence:
1. **48-hour email:** POC proposal + ROI calculator
2. **1-week call:** Address questions, move toward decision
3. **2-week email:** Final push (limited-time offer or next POC cohort closing)

---

## Execution Timeline

### Week 3 (Mon-Fri)

| Day | Task | Owner | Deliverable |
|-----|------|-------|-------------|
| Mon | Environment deployment | Backend | Staging env live |
| Tue | Demo data loading | Backend | All 3 verticals loaded |
| Wed | Monitoring + dashboard setup | DevOps | Real-time visibility |
| Thu | Receipt generation system | Backend | Audit trail working |
| Fri | Demo script rehearsal | Sales | All 3 demos polished |

### Week 4 (Mon-Fri)

| Day | Task | Owner | Deliverable |
|-----|------|-------|-------------|
| Mon | Demo #1 (Healthcare) | Sales | Customer 1 sees proof |
| Tue | Demo #1 follow-up | Sales | POC proposal sent |
| Wed | Demo #2 (Finance) | Sales | Customer 2 sees proof |
| Thu | Demo #2 follow-up | Sales | POC proposal sent |
| Fri | Demo #3 (E-commerce) | Sales | Customer 3 sees proof |

**Exit Criteria:** 3 POC proposals sent, 1+ accepted (ready to start Week 5)

---

## Quality Checklist

Before launching any demo, verify:

- [ ] Demo environment healthy (all services running)
- [ ] Demo data accurate (reflects real customer scenarios)
- [ ] Dashboard displays correctly (no lag, no errors)
- [ ] ROI calculator customizable (easy to input customer metrics)
- [ ] Receipt generation tested (audit trail works)
- [ ] Scripts rehearsed (smooth delivery, no stuttering)
- [ ] FAQ questions covered (confident answers ready)
- [ ] Follow-up sequence prepared (emails drafted, calendar blocked)
- [ ] Fallback plan ready (if demo environment breaks)
- [ ] Customer success team briefed (POC handoff ready)

---

## Success Metrics

### Demo Success Metrics (Week 4)

| Metric | Target | How Measured |
|--------|--------|--------------|
| Demos completed | 3 | Sales calendar + CRM |
| NPS feedback | ≥8/10 | Post-demo survey |
| POC proposals sent | ≥2 | Proposals in CRM |
| POC proposals accepted | ≥1 | Signed agreement |
| Time-to-proposal | <48hr | Email timestamp |

### POC Success Metrics (Weeks 5-8)

| Metric | Target | How Measured |
|--------|--------|--------------|
| Baseline metrics captured | 100% | Measurement report |
| Success criteria tracked | Daily | Dashboard + manual checks |
| Customer satisfaction | NPS ≥8 | Weekly check-in |
| System uptime | 99.5%+ | Monitoring alerts |
| Deal closure | ≥1 signed | Contracts |

---

## Risk Mitigation

### Risk: Demo environment breaks during live demo

**Mitigation:**
- [ ] Have backup environment (ready to switch)
- [ ] Practice failover (test weekly)
- [ ] Have screen recording of demo (show if live demo breaks)
- [ ] Have phone number ready (switch to Zoom screen share)

### Risk: Demo data doesn't reflect customer reality

**Mitigation:**
- [ ] Interview customer before demo (understand their metrics)
- [ ] Use their actual data if possible (with anonymization)
- [ ] Pre-demo call (walk through data scenario)
- [ ] Post-demo review (confirm realism of metrics)

### Risk: Prospect says "This looks great, but we need to think about it"

**Mitigation:**
- [ ] Prepare POC proposal (ready to send same day)
- [ ] Have timeline ready (30-day POC, decision by X date)
- [ ] Offer limited-time incentive (e.g., "POC pricing 20% off if signed this week")
- [ ] Schedule 48-hour follow-up (keep momentum)

---

## Files in This Package

```
/Users/sac/ggen/tai-erlang-autonomics/execution/week-3-4/

├── WEEK_3_4_DEMO_AND_POC.md (THIS FILE)
├── DEMO_SCRIPTS/
│   ├── DEMO_HEALTHCARE.md
│   ├── DEMO_FINANCE.md
│   └── DEMO_ECOMMERCE.md
├── DEMO_DATA/
│   ├── healthcare_hospital_ops.json
│   ├── finance_portfolio_risk.json
│   └── ecommerce_inventory_optimization.json
├── DEMO_DATA_SETUP.md
├── POC_SUCCESS_CRITERIA.md
├── MEASUREMENT_REPORT_TEMPLATE.md
├── DEMO_FAQ.md
├── ROI_CALCULATOR.md
├── CASE_STUDY_TEMPLATE.md
└── POST_DEMO_SEQUENCE.md
```

---

## Next Steps

1. **Week 3:** Execute environment setup + demo script preparation
2. **Week 4:** Execute demos with 3 target prospects
3. **Week 5:** Kick off first POC (measurement + tracking)
4. **Week 6-8:** Execute POC, drive toward deal closure

---

**Last Updated:** 2026-01-26
**Status:** Ready for Execution
**Owner:** VP Sales + Marketing Lead
**Review Schedule:** Weekly (Fridays at 4pm)
