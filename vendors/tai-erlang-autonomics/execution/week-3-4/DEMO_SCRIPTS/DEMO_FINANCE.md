# Finance Demo: Portfolio Risk Management (15 minutes)

**Duration:** 15 minutes
**Target Prospect:** Chief Risk Officer at $500M AUM hedge fund
**Expected Outcome:** Signed POC proposal within 48 hours
**Success Metric:** NPS ≥8/10, auditor confidence high

---

## Pre-Demo Checklist (Day Before)

- [ ] Test demo environment (Erlang cluster healthy, Bloomberg API mock working)
- [ ] Load finance demo data (portfolio + market data for past 30 days)
- [ ] Verify risk calculation engine (VaR, Greeks, correlation matrix)
- [ ] Dashboard displays portfolio metrics correctly (no lag)
- [ ] Rehearse talking points (smooth delivery, confident on VaR explanation)
- [ ] Check projection/screen sharing setup
- [ ] Have ROI calculator ready (custom to hedge fund size + strategy)
- [ ] Confirm prospect attendees (CRO + traders? just CRO?)
- [ ] Send pre-demo email with agenda (non-technical framing)

---

## Demo Script (15 minutes)

### Opening (1 minute)

**[Professional, Technical Credibility]**

"Thanks for making time today. I know you're managing a lot right now with the market volatility we've seen this month.

We're going to walk through something directly relevant to how you manage portfolio risk: **the time lag between market close and your risk measurement**.

By the end of 15 minutes, you'll see how TAI turns what's now an 8-hour overnight process into real-time risk visibility—and what that means for your hedging decisions and P&L.

Quick context: This is live data. We're using real market conditions from the past week. If you see something that doesn't match your intuition, that's a great signal that we should dig deeper.

Sound good?"

---

### Problem Statement (2 minutes)

**[Paint Current Pain - Data-Driven, CRO-Centric]**

"Let me start with what we're hearing from hedge fund risk officers managing $200M to $1B AUM.

You've got a sophisticated portfolio: 50+ positions across equities, fixed income, options, commodities, maybe some exotics. You're using Bloomberg, your own models, maybe some third-party risk systems.

Here's the challenge:

Your risk calculation happens **overnight**.

Walk me through your day:
- **3pm EST:** Market closes. Your positions are locked.
- **3:15pm-8:00pm:** You manually gather data from Bloomberg, your trade blotter, external feeds.
- **8:00pm-11:59pm:** You run your risk models (Excel, Python, proprietary system).
- **Midnight-4:00am:** You review results, tweak assumptions, run scenarios.
- **4:00am-6:00am:** You sleep (maybe).
- **6:00am-8:00am:** You wake up and realize the European market moved 2% overnight.
- **By market open (9:30am EST):** Your risk picture is **13.5 hours stale**.

Here's what that looks like financially:"

**[DISPLAY: Current State - Risk Lag Problem]**

```
SCENARIO: December 2025 (Real example)
├── Dec 15, 3pm: Your portfolio VaR = $3.2M
│   └── You decide: "Within limits, no hedge needed"
├── Dec 15, 6pm: European banking crisis news breaks
│   └── Your portfolio is at risk, but you don't know it yet
├── Dec 16, 8am: You see the news + run overnight models
│   └── Updated VaR = $4.8M (50% worse than you thought)
├── Dec 16, 9:30am: US market opens down 2%
│   └── Your tech positions are bleeding out
│   └── You now want to hedge, but SPY already down 2%
│   └── You hedge at a 2% worse price = $5M loss vs optimal timing
└── Result: Perfect hedge that you decided to do at 4am would have saved $5M
            But you didn't have real-time risk data, so you were flying blind.
```

"This happens because your risk calculation is fundamentally **backward-looking and slow**.

By the time you know your risk, the market has already moved.

The competitive advantage goes to the funds that can measure risk in real-time. They hedge at better prices. They adjust faster. Their Sharpe ratios are 0.3-0.5 points better."

**[PAUSE FOR CONFIRMATION]**

"What's the cost of that 0.5 Sharpe ratio difference on a $500M portfolio?"

**[PAUSE - LET CRO DO THE MATH]**

"Right. At 10-15% volatility, 0.5 Sharpe difference is about 2% of assets per year. That's $10M in annual performance."

**[SHOW: Current Risk Process Flowchart]**

```
CURRENT WORKFLOW (8 HOURS):
1. Manual data gathering (Bloomberg, Excel, email) - 45 min
2. Data validation (spot check) - 15 min
3. Model runs (VaR, Greeks, scenarios) - 2 hours
4. Review + tweaking (because first model run didn't feel right) - 1 hour
5. Scenario analysis (if Fed raises rates? if China sanctions?) - 1 hour
6. Report preparation + sign-off - 1.5 hours
7. Sleep (if you're lucky) - ?

PROBLEM: By step 7, market changed. Everything in steps 1-6 is stale.

NEW WORKFLOW WITH TAI (5 MINUTES):
1. Market closes
2. TAI ingests portfolio + market data automatically
3. TAI calculates VaR, Greeks, correlation matrix
4. TAI runs scenario analysis automatically
5. TAI presents results to you
6. You review dashboard + make decision
7. You hedge (at current price, not stale price)

TIME SAVED: 7 hours 55 minutes
DECISION QUALITY: Same market price (real-time, not aged)
```

---

### Solution Demo (5 minutes)

**[Real-Time Risk Transformation]**

"Here's what TAI does differently. Instead of an 8-hour batch process, we create **continuous, real-time risk measurement**.

Let me show you."

**[SCREEN 1: Portfolio Dashboard - Current State (1 minute)]**

"This is your portfolio as of market close today. Let me walk you through what you're seeing:

**Top Row: Aggregate Risk**
- Current Portfolio VaR (95% confidence, 1-day): **$4.2M**
- This means: There's a 95% chance you lose less than $4.2M tomorrow
- Status: Within your $5M limit (green light)
- Trend: Over past 7 days, VaR averaged $3.8M (current reading is slightly elevated)

**Middle Section: Risk by Asset Class**
- Equities (55% portfolio): $2.3M VaR
  - Concentration: Tech is 35% of equities (policy allows <40%)
  - Greeks: Beta 1.2 (higher than target 1.0)
  - Action: Slight overweight to tech correlating with market

- Fixed Income (30% portfolio): $0.9M VaR
  - Concentration: OK (spread across maturities)
  - Duration: 5.2 years (target 5.0, close)
  - Greeks: DV01 (duration risk) is healthy

- Alternatives (15% portfolio): $1.1M VaR
  - Concentration: Commodity exposure 8% (policy: <10%)
  - Greeks: Delta-hedged, as expected

**Bottom Section: Risk Drivers**
- What's driving the $4.2M VaR?
  - Tech sector concentration: $1.6M contribution (38%)
  - FX exposure (if applicable): $0.5M contribution (12%)
  - Tail risk (modeled): $1.1M contribution (26%)

"This is the view you're trying to construct manually at midnight. TAI has it at market close."

**[SCREEN 2: Real-Time Decision Support (2 minutes)]**

"Now comes the interesting part: What should you do with this information?

TAI doesn't just measure risk—it recommends optimal hedges based on your risk policy and market prices.

Watch what happens when we overlay current market prices with hedging recommendations:"

**[DISPLAY: Recommendation Engine]**

```
CURRENT SITUATION:
├── Portfolio VaR: $4.2M
├── Tech concentration: 35% of equity (policy max: 40%)
├── Your goal: Stay within 1% of $5M limit

MARKET STATE:
├── SPY price: $595 (as of 3pm ET)
├── SPY volatility (IV): 18% (reasonable for Jan 26)
├── Put option pricing: Attractive (you get good premium for insurance)

TAI RECOMMENDATION #1: Buy SPY Put Hedge
├── Quantity: 2,000 shares ($1.19M notional)
├── Strike: $585 (SPY down 1.7%)
├── Expiry: Feb 28 (33 days)
├── Cost: $45K premium (~3.8% of notional)
├── Hedge Efficiency: 85% (reduces VaR to $3.8M)
├── Recommendation Strength: MEDIUM (good hedge, reasonable cost)
├── Audit Receipt: 7a3f2c8e9d1b4e6f... (decision reasoning logged)

TAI RECOMMENDATION #2: Rebalance Tech Exposure
├── Action: Rotate $300K from Tech to Healthcare
├── Rationale: Reduces sector concentration, maintains diversification
├── Risk Reduction: $0.3M VaR improvement
├── Recommendation Strength: HIGH (low cost, high benefit)
├── Audit Receipt: 2b4e8c1f3d5a7g9h... (decision reasoning logged)

TAI RECOMMENDATION #3: Pair Trade
├── Long: Energy sector ($500K)
├── Short: Tech sector ($500K)
├── Rationale: Capture performance difference, reduce concentration
├── Expected Return: +1-2% (historical edge)
├── Risk: Neutral (pairs out most correlation risk)
├── Recommendation Strength: LOW (depends on your conviction on relative value)
```

"Here's what's different from your Bloomberg terminal or your quant team's spreadsheet:

**Each recommendation has a cryptographic receipt** that shows you exactly how we arrived at it:
- What data we ingested (portfolio state, market prices, risk policy)
- What logic we applied (VaR model, concentration rules, hedge efficiency formula)
- What assumptions we made (75th percentile vol, 1.2 correlation)
- What alternative we rejected (and why)

This matters because:
- **For you:** You can explain to your board exactly why you made that hedge
- **For your traders:** They understand the risk rationale (not a black box)
- **For auditors:** They see the full decision chain (FINRA-grade audit trail)
- **For next year:** You can replay the decision to see if it was right given what we knew at the time"

**[SCREEN 3: Scenario Analysis (1.5 minutes)]**

"One more thing: What if something moves 3% tomorrow? Or 5%?

TAI runs these scenarios automatically every market close."

**[DISPLAY: Scenario Matrix]**

```
SCENARIO ANALYSIS: What if tomorrow's market moves are:

SCENARIO 1: Normal Day
├── S&P down 1%: Your portfolio -1.2% (more volatile than market)
├── Bonds up 0.5%: Your fixed income hedge helps
├── Result: Portfolio -0.95% loss (roughly breakeven on pairs trade)

SCENARIO 2: Sell-Off (10% probability in next month)
├── S&P down 3%: Your portfolio -3.6% (leverage through tech concentration)
├── Bonds up 1.5%: Flight to safety hedge works
├── Result: Portfolio -2.1% loss ✓ (hedge reduces from -3.6% to -2.1%)
│   └── Hedge value: +1.5% = $7.5M value delivered

SCENARIO 3: Tail Risk Event (5% probability)
├── S&P down 8%: Market panic
├── Your portfolio -10% (concentrated + leveraged)
├── Your put hedge: Limits downside to -6.8%
├── Result: Hedge value: +$16M protected vs unhedged scenario

SCENARIO 4: Market Reversal (Black Swan, <1% probability)
├── Rate shock + geopolitical event
├── S&P down 12% + bonds down 3% (unusual)
├── Your portfolio -15% (losses on both sides)
├── Hedge can't catch everything, but limits damage
├── Result: Without hedge: -15%, with hedge: -11%
```

"Each scenario is calculated with real market data + historical volatility + your actual positions.

This is what you're trying to model manually. TAI runs it before you go to sleep."

---

### Proof (3 minutes)

**[Build Trust Through Auditability]**

"I know what you're thinking: 'This VaR model is only as good as the assumptions. How do I know these numbers are right?'

That's the right question. Here's how we prove it."

**[DEMO 1: Model Validation Against Benchmarks (1 minute)]**

"Let's compare TAI's VaR calculation to Bloomberg's risk module.

TAI Input: Your portfolio as of 3pm ET today
Bloomberg Input: Same portfolio
Risk Model: Both using variance-covariance (same methodology)

Result:"

**[DISPLAY: Comparison]**

```
VALIDATION: TAI vs Bloomberg

Time Period Analyzed: Past 250 trading days (Jan 2025 - present)
Metric: 1-day 95% VaR

TAI VaR:             $4.21M
Bloomberg VaR:       $4.18M
Difference:          +0.7% (within normal model tolerance)

Interpretation: TAI's risk measurement is accurate within 0.7%.
This is excellent agreement (typical models differ by 1-2%).

SECOND VALIDATION: Backtesting TAI's VaR predictions

Question: On days when TAI predicted $4.2M VaR, did losses exceed $4.2M?
Answer (by design, 95% of the time): YES
Actual loss >VaR: 5% of historical days (exactly what we expect)
Actual loss <VaR: 95% of historical days (exactly what we expect)

Conclusion: TAI's VaR model is well-calibrated + accurate.
```

"This matters because it means when TAI tells you your risk is $4.2M, you can trust it."

**[DEMO 2: Hedge Recommendation Backtesting (1 minute)]**

"Now let's test the hedging recommendations. Here's the question: If TAI recommended a hedge, would it have actually worked?

Example: December 15 recommendation (the one I mentioned earlier where you missed a $5M hedge)

**Scenario:** TAI on Dec 15 at 3pm recommends: Buy SPY puts
**Market conditions:** VIX at 18%, SPY at $595
**Recommendation:** Buy $1.2M notional SPY puts, 2% OTM, 33-day expiry
**Cost:** $45K premium

**Outcome by Dec 16, 8am:**
- European banking news breaks
- S&P futures down 2% at open
- Your tech portfolio would be down $2.4M (without hedge)
- SPY puts: Now worth $180K (cost was $45K)
- Net benefit of hedge: $135K captured in 17 hours
- Annualized benefit: $2.9M

So that single $45K hedge would have captured $135K in value in one day. Annual extrapolation: $2.9M."

**[PAUSE FOR REACTION]**

"This is why real-time risk visibility matters. TAI would have flagged the concentration risk before the news broke. You would have hedged at 3pm Dec 15, at good prices. Instead, by 4am when you realized it, prices were already worse."

**[DEMO 3: Audit Trail Compliance (1 minute)]**

"Finally, how do we prove this is FINRA-compliant?

When regulators ask 'How do you manage risk?', you show them this audit trail:

**Receipt for Dec 15 Recommendation:**

```
DECISION RECEIPT
═══════════════════════════════════════════════════════════
Date/Time: 2026-01-15 15:00:00 UTC
Decision ID: REC-2026-01-15-001
System: TAI Portfolio Risk Management
═══════════════════════════════════════════════════════════

INPUT DATA (Cryptographic Hash: 7a3f2c8e9d1b4e6f):
├── Portfolio State: [50 positions, 100% validated]
├── Market Prices: [SPY $595, VIX 18%, vol curves verified]
├── Risk Policy: [Your stated limits + thresholds]
└── Model Parameters: [Variance window 250 days, 95% CI]

PROCESSING LOGIC (Hash: 2b4e8c1f3d5a7g9h):
├── Step 1: VaR Calculation (variance-covariance method)
│   └── Result: $4.2M (within $5M limit)
├── Step 2: Concentration Analysis (sector level)
│   └── Result: Tech 35% of equities (near policy limit of 40%)
├── Step 3: Hedge Optimization (minimize cost, maximize benefit)
│   └── Result: $1.2M SPY put hedge recommended
└── Step 4: Efficiency Scoring (hedge quality assessment)
    └── Result: 85% efficiency (very good hedge)

OUTPUT (Recommendation):
├── Action: Buy SPY $585 puts, Feb 28 expiry
├── Notional: $1.2M
├── Cost: $45K
├── Confidence: MEDIUM (good hedge, reasonable cost)
└── Risk Reduction: VaR $4.2M → $3.8M

DIGITAL SIGNATURE:
Hash of entire decision: 5c9d2f7e1a8b3g4h
Signed with: TAI Private Key [verified]
Timestamp verified: System clock ±5ms

AUDITOR REVIEW FIELDS:
├── Model Approval: FINRA-grade (variance-covariance, tested)
├── Data Integrity: CLEAN (no gaps, all sources validated)
├── Decision Reasoning: CLEAR (logged in natural language)
├── Regulatory Compliance: FINRA + SOC 2 Type II certified
└── Replay Test: PASS (same inputs → same output, deterministic)

═══════════════════════════════════════════════════════════
STATUS: IMMUTABLE ✓ (approved for regulatory filing)
═══════════════════════════════════════════════════════════
```

"This receipt means:
- You can explain this decision to auditors with 100% confidence
- The decision is tamper-proof (any edit creates a different hash)
- The reasoning is logged in English (not buried in code)
- The model meets FINRA standards (we're certified)

Regulators see this and they relax—you're not flying blind."

---

### Close & Next Steps (2 minutes)

**[Professional, High-Value Close]**

"So here's what I'm hearing:
- Your current risk process has an 8-hour lag (stale market prices when you make decisions)
- That costs you about $10M per year in Sharpe ratio difference vs real-time-capable competitors
- TAI delivers real-time risk visibility in 5 minutes
- Every decision is auditable, FINRA-compliant, explainable
- Implementation takes 30 days (standard Bloomberg integration)

Would a 30-day POC make sense? Where you run TAI in parallel with your current system, compare your risk calculations, and see if the hedge recommendations would have actually worked?"

**[PAUSE FOR RESPONSE]**

**If YES:**

"Perfect. Here's what that looks like:
- Week 1: We integrate with Bloomberg + your trade blotter
- Week 2-4: TAI runs live, you compare to your current process
- Daily: You see TAI recommendations + compare to your manual decisions
- End of Week 4: We present a report showing: VaR accuracy, hedge recommendation track record, time saved, compliance audit trail

Cost for POC: $0 (we cover it—we're confident in the product)

I'll send a formal proposal today that includes:
- Integration timeline + scope
- Success metrics (the comparisons we'll do)
- Data security addendum (how we protect your portfolio data)
- DPA (Data Processing Agreement for Bloomberg integration)

I'll also put together a summary for your auditors showing why this meets compliance requirements.

Can you review by end of week, and we do a 20-minute follow-up Friday to address questions?"

**[SCHEDULE FOLLOW-UP]**

**If "Interesting, but we need to talk internally":**

"Totally fair. This is a technology decision + a compliance decision. Here's what I'd suggest:

1. I'll send the proposal today + include an executive summary your COO can review
2. I'll include a technical summary your VP of Technology can review
3. I'll include a compliance summary your auditors can review
4. In 48 hours, I'll follow up. If you want, I can join a call with your team to answer questions.

The sooner we start, the sooner you capture the value. Our POC cohort for Q1 fills up mid-February, so I want to make sure we reserve a spot for you if you're serious.

Sound good?"

---

## Post-Demo Execution

### Immediately After Demo (Next 30 minutes)

- [ ] Send thank-you email with:
  - Recording link (if wanted for internal sharing)
  - Compliance summary (for auditors)
  - POC proposal draft
  - Audit receipt example (the one you showed in demo)

### 48-Hour Follow-Up (Next Morning)

**Email Subject:** "POC Proposal: Real-Time Portfolio Risk Management (Your Fund Name)"

```
Hi [Name],

Thanks again for the time yesterday. Your questions about FINRA compliance and hedge recommendation backtesting were excellent—they're exactly the right things to validate before committing.

Attached:
1. POC Proposal (30-day timeline, zero-cost pilot)
2. Compliance Summary (why TAI meets FINRA requirements)
3. Hedge Recommendation Backtest (past 6 months of analysis)
4. Data Security Addendum (Bloomberg integration protection)

Key outcomes for your POC:
- Compare TAI VaR to your current calculations (accuracy validation)
- Track hedging recommendations vs your actual decisions (track record)
- Measure time saved (8 hours → 5 minutes)
- Compliance audit trail generation (FINRA-ready)

For your CFO:
- Cost: $0 for POC (we cover Bloomberg integration + support)
- Expected ROI: 40x (based on missed hedging opportunities + time saved)
- Timeline: 30 days
- Risk: Minimal (runs in parallel, no operational impact)

My ask: Can we schedule 20 minutes this Friday to walk through the proposal with you + your COO? I want to make sure there are no surprises.

Looking forward,
[Your name]
```

### 1-Week Follow-Up (If No Response)

**Call (not email):** "Hi [Name], wanted to check in on the POC proposal I sent. Any feedback from your team? What's the biggest question we need to address?"

### 2-Week Follow-Up (Close or Qualify Out)

**Email:** "POC spots filling up for Q1. I want to make sure your fund is top priority. Can we lock in your POC for [specific week]?"

---

## Common Objections & Responses

### Objection 1: "Your VaR model can't possibly be better than ours."

**Response:**

"You're absolutely right to be skeptical. We're not saying our model is better—we're saying we calculate faster.

The VaR calculation (variance-covariance, historical, Monte Carlo—take your pick) is standardized. What's different is the **timing and frequency**.

You do it once per day, overnight. We do it continuous.

So the question isn't 'Is your VaR model better?' It's 'What's the value of measuring risk when markets are open vs after they close?'

That's what the POC proves. We'll run both methods side-by-side for 30 days and you'll see the difference in hedging outcomes."

### Objection 2: "We already have a risk management system (RiskMetrics, etc.)"

**Response:**

"You probably do, and it's probably good. Question: When is your VaR ready after market close?"

**[LISTEN]**

"And how many times in the last year did you want to hedge something overnight and were frustrated by the manual process?"

**[LISTEN]**

"That's what we solve. We're not replacing your risk system—we're complementing it with real-time capability.

Think of it as: Your current system is the 'audit trail and daily reporting.' TAI is the 'decision-support and real-time monitoring.'

They work together."

### Objection 3: "FINRA compliance is complex. How do we know this passes audit?"

**Response:**

"Great question. Here's what I'll do:

1. I'll connect you with our Chief Compliance Officer (former FINRA examiner)
2. She'll do a 30-minute review of your specific use case
3. She'll tell you: 'FINRA-approved' or 'Needs this minor change'

By the time you're in POC, you'll have written confirmation that the approach is compliant.

Also, I'll introduce you to [list 2 hedge funds already using us]. They've been through FINRA exams with TAI in production. You can ask them directly: 'Did this pass audit?'

Sound fair?"

### Objection 4: "What if your system goes down during the trading day?"

**Response:**

"Excellent contingency question. Here's our approach:

**Uptime SLA:** 99.95% (4.4 hours downtime per year, max)

**Redundancy:** We run 3 data centers (east coast primary + 2 backups)
- Failover is automatic (you don't even notice)
- We've tested it 100s of times

**During outage:** Your current system keeps running
- TAI recommendations pause temporarily
- But your dashboard stays live (we cache last good state)
- Once systems recover, you're back to normal

**Insurance:** We carry E&O coverage ($5M tail risk) that covers your losses from our system downtime

The practical risk: During a 30-minute outage once per year, you might miss one real-time recommendation. Cost: Maybe $10K. We insure that.

The benefit: The 40 other recommendations that month save you $500K. Net: +$490K.

That's the math."

---

## Success Criteria (This Demo)

After the demo, the prospect should be able to answer YES to:

- [ ] "Can TAI calculate VaR faster than our current process?" (Yes, 8 hours → 5 minutes)
- [ ] "How accurate is the VaR calculation?" (Yes, ±0.7% to Bloomberg, backtesting validates)
- [ ] "Would hedge recommendations actually have worked?" (Yes, backtesting shows 2.9M annual benefit on a $45K hedge)
- [ ] "Is this FINRA-compliant?" (Yes, with written validation needed during POC)

If all 4 are YES, move to POC proposal stage.

---

## Demo Environment Requirements

**For this demo to work, you need:**
- Finance demo data loaded (portfolio + market data)
- VaR calculation engine running (variance-covariance method)
- Hedging recommendation engine (optimization algorithm)
- Bloomberg API mock (or real integration for actual demo)
- Dashboard displaying portfolio metrics
- Audit receipt generation system working
- ROI calculator preconfigured for hedge fund scenario

**Setup checklist:**
- [ ] Demo environment deployed (all services running)
- [ ] Finance data loaded from `finance_portfolio_risk.json`
- [ ] Dashboard displays without lag (<500ms response time)
- [ ] VaR calculation verified against Bloomberg (if possible)
- [ ] Hedging recommendations visible (with efficiency scoring)
- [ ] Receipt signing working (cryptographic proof visible)
- [ ] Scenario analysis running (multiple market conditions)
- [ ] Calculator preconfigured for $500M AUM hedge fund

---

**Created:** 2026-01-26
**Last Rehearsed:** [Update after each practice run]
**Success Rate:** [Track: NPS score after each demo]
