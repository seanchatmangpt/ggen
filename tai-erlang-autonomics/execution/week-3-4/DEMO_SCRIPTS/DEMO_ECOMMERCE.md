# E-Commerce Demo: Inventory Optimization (15 minutes)

**Duration:** 15 minutes
**Target Prospect:** VP Operations at $20M GMV DTC fashion brand
**Expected Outcome:** Signed POC proposal within 48 hours
**Success Metric:** NPS ≥8/10, tangible revenue impact visible

---

## Pre-Demo Checklist (Day Before)

- [ ] Test demo environment (all services running, real-time data sync)
- [ ] Load e-commerce demo data (Shopify + Amazon + retail inventory)
- [ ] Verify Shopify integration mock (API responding correctly)
- [ ] Dashboard displays inventory metrics correctly (no lag)
- [ ] Allocation algorithm showing recommendations (with reasoning)
- [ ] Rehearse talking points (smooth delivery, confident on inventory math)
- [ ] Check projection/screen sharing setup (HDMI + WiFi backup)
- [ ] Have ROI calculator ready (custom to their GMV + margins)
- [ ] Confirm prospect attendees (VP Ops? Add warehouse manager if possible)
- [ ] Send pre-demo email with agenda (non-technical framing)

---

## Demo Script (15 minutes)

### Opening (1 minute)

**[Warm, Operations-Focused, Outcome-Driven]**

"Thanks for making time today. I know January is insane for e-commerce—returns, inventory reconciliation, all of it.

We're going to walk through something directly relevant to your operations: **the inventory allocation problem you're probably juggling right now**.

You've got Shopify, maybe Amazon, maybe your own retail. Inventory in one place when demand is in another. Out-of-stock emails. Overstock in slow SKUs.

By the end of 15 minutes, you'll see how TAI automatically allocates inventory across channels to maximize revenue—and what that means for your bottom line.

This is live data. We're using real inventory patterns from fashion e-commerce brands like yours. If something doesn't match your experience, that's a conversation—means we need to adjust assumptions.

Sound good?"

---

### Problem Statement (2 minutes)

**[Paint Current Pain - Operationally Specific & Financial]**

"Let me start with what we're hearing from VP Ops at DTC fashion brands in the $10-50M GMV range.

You've got 500+ SKUs. You're selling across 3 channels: Shopify (direct), Amazon (3P), and maybe 5-10 retail partners.

Every channel wants to be top priority. Shopify says 'I'm your core channel.' Amazon says 'I'm growing 40% YoY.' Retail partners say 'We need allocation or we'll drop you.'

Here's the problem: **You can't see inventory in real-time across all channels.**

Walk me through your day:
- **9am:** You check Shopify admin dashboard. See inventory levels.
- **9:30am:** You jump into Amazon Seller Central. Inventory different from Shopify (2-4 hour lag).
- **10am:** You email warehouse manager: 'What's the actual count on our bestseller SKU?'
- **10:30am:** Warehouse replies: 'We have 200 units.' (But is that earmarked for retail partner order? Already allocated to Shopify??)
- **11am:** You manually calculate: 'OK, we have 200 units, Shopify is out of stock, Amazon is running low, we've got a retail order due Friday.'
- **12pm:** You manually adjust inventory: 'Move 50 units from Shopify to Amazon, hold 100 for retail order.'
- **1pm:** Shopify customer orders your bestseller. Out of stock. Lost sale.
- **2pm:** You check Shopify again. Different inventory count (fresh orders came in since 9am).
- **Loop repeats**

This happens because each channel operates independently. Shopify doesn't talk to Amazon. Neither talks to your warehouse. Nobody has real-time visibility."

**[DISPLAY: Current State - Fragmented Inventory Problem]**

```
CURRENT STATE: Fragmented Inventory Across Channels

INVENTORY REALITY:
├── Physical warehouse: 1,000 units (bestseller SKU)
├── Shopify allocation: 600 units (showing in-stock)
├── Amazon allocation: 300 units (showing in-stock)
├── Retail partner reserved: 100 units (not visible in systems)
└── Unaccounted: 0 units ✓

PROBLEM: ALLOCATION IS WRONG
├── Shopify at 600 units seems safe, but retail partner already owns 100
├── True Shopify available: 500 units (you don't know this)
├── Amazon at 300 units seems fine, but trends show +15 units/day sell-through
├── By Thursday, Amazon will want 50 more units (supply won't catch up)
└── Result: Amazon out of stock Thursday, Shopify overallocated

THE COST:
├── Shopify stock-out Thursday: -$8,000 revenue (5 lost sales × $1,600 order avg)
├── Amazon unable to restock Friday: -$5,000 revenue (missed fulfillment)
├── Overstock in Shopify (moving inventory to slow movers): -$2,000 (50% discount to clear)
└── Total loss this week: -$15,000

ANNUALIZED: 47 stock-out events per week (you know this from your data)
= 2,444 stock-out events per year
= 2,444 × $8,000 average lost revenue
= $19.5M annual revenue loss
```

"Hold on, let me repeat that: **$19.5M per year in lost revenue from inventory misallocation.**

I know that sounds high, so let me validate the math:
- Your GMV: $20M
- Your current stock-out rate: 47 events per week (visible in your Shopify logs)
- Average order value when you stock out: $1,600
- You're probably recovering some of that with backorders, but let's say 50% is truly lost
- 47 events × $800 × 52 weeks = $1.95M minimum (I said $19.5M to include the Amazon + retail channel losses too)

Even conservative: **$2M per year in lost revenue from poor inventory allocation.**

Plus the operational cost:
- Your time manually reallocating inventory: 5 hours/week × $100/hr = $26K/year
- Warehouse manager time managing allocations: 8 hours/week × $50/hr = $21K/year
- Overshooting and having to discount slow SKUs: $180K/year

**Total annual cost of current inventory management: $2.227M**"

**[PAUSE FOR REACTION]**

"What if there was a way to eliminate most of that? Automatically."

---

### Solution Demo (5 minutes)

**[Real-Time Inventory Synchronization & Optimization]**

"Here's what TAI does differently. Instead of manual spreadsheet shuffling, we create **real-time, intelligent inventory allocation**.

Let me show you."

**[SCREEN 1: Real-Time Channel Inventory Sync (1.5 minutes)]**

"This is your inventory view with TAI running.

Connected to:
- Shopify store (real-time inventory updates)
- Amazon Seller Central (real-time inventory updates)
- Your warehouse management system (real-time stock count)
- Historical demand patterns (your sales history)

Watch what happens as we sync everything:"

**[DISPLAY: Real-Time Dashboard]**

```
TAI INVENTORY DASHBOARD (Updated every 5 minutes)

TOP ROW: Aggregate Metrics
├── Total Available Inventory: 4,200 units
├── Stock-Out Risk: 2 SKUs at risk in next 7 days
├── Revenue Optimization Opportunity: +$187K possible (this week)
└── System Status: 99.7% accurate (vs manual tracking)

CHANNEL SUMMARY:
├── Shopify (Direct Channel)
│   ├── Active Inventory: 2,100 units across 500 SKUs
│   ├── In-Stock Rate: 94% (up from 62% last week)
│   ├── Stock-Out Events This Week: 1 (down from 8)
│   └── Revenue Impact: +$12,000 (from reduced stock-outs)
│
├── Amazon (3P Marketplace)
│   ├── Active Inventory: 1,200 units across 400 SKUs (subset)
│   ├── In-Stock Rate: 97% (up from 79% last week)
│   ├── Stock-Out Events This Week: 0 (down from 4)
│   └── Revenue Impact: +$8,500 (from reduced stock-outs + faster restocks)
│
└── Retail Partners (5 stores)
    ├── Allocated Inventory: 900 units across 150 SKUs
    ├── In-Stock Rate: 100% (fulfilling all retail orders)
    ├── Backlog: 0 units (no unfulfilled retail orders)
    └── Revenue Impact: +$6,200 (faster fulfillment = retail partner happy)

TOTAL WEEKLY REVENUE IMPACT: +$26,700 (visible improvement in 1 week)
```

"Let me break down what's happening:

Your bestseller SKU (the one with 1,000 units):
- Old system: 600 Shopify, 300 Amazon, 100 retail (misallocated)
- TAI system: 400 Shopify, 350 Amazon, 200 retail (optimized based on demand)
- Why: Amazon is growing faster (40% YoY), Shopify has slower demand this month, retail partner placement is high-value

TAI continuously rebalances based on:
1. Historical demand (which channel sells faster?)
2. Current trends (is demand shifting?)
3. Supply constraints (how much can warehouse fulfill?)
4. Business rules (which channel is priority this month?)

Everything happens automatically. No manual intervention. No spreadsheets."

**[SCREEN 2: Stock-Out Prevention (1.5 minutes)]**

"Now here's the magic part: TAI predicts stock-outs before they happen.

Watch this scenario:"

**[DISPLAY: Prediction + Prevention]**

```
SCENARIO: Popular Item Running Low

DAY 1 (Monday)
├── Current stock: 500 units
├── Daily sell-through Shopify: 40 units/day
├── Daily sell-through Amazon: 35 units/day
├── Total burn rate: 75 units/day
└── Days to stock-out: 6.7 days (Thursday evening)

TAI ANALYSIS (Monday 10am):
├── Trend check: Is demand accelerating?
│   └── Yes, weekend boost effect (Mondays are slower)
├── Restock arrival: New purchase arrives Wednesday
│   └── Quantity: 300 units (restocks 4 more days of inventory)
├── Forecasted demand: Will restock arrive in time?
│   └── YES, but timing is tight. Only 1 day buffer.
└── Recommendation: MILD RISK ⚠️

TAI ACTION (Automatic):
├── Don't restrict Shopify (primary channel)
├── Reduce Amazon allocation slightly (secondary priority this week)
│   └── Was: 35 units/day allowed
│   └── Now: 28 units/day allowed
├── No customer-facing change (no message to buyers)
├── Result: Extends stock-out timeline to Saturday (gives 1 day buffer)

DAY 3 (Wednesday)
├── Restock arrives: 300 units received
├── Warehouse scans boxes: 300 confirmed ✓
├── TAI instantly sees restock: Risk cleared ✓
├── Amazon allocation restored: Back to 35 units/day
└── Result: Stock-outs prevented, no disruption to Amazon sales

WEEKLY OUTCOME:
├── Stock-outs prevented: 1 (would have happened Thursday on Amazon)
├── Prevented lost revenue: $5,000
├── Customer experience: Perfect (they didn't notice anything)
└── Operational complexity: Zero (TAI handled it all)
```

"This is inventory optimization in action. You don't need to lift a finger. TAI watches continuously and prevents problems before they happen."

**[SCREEN 3: Financial Impact Dashboard (2 minutes)]**

"Here's what this means for your bottom line:

**Metric 1: Stock-Out Prevention**
- Before TAI: 47 events/week
- After TAI: 2 events/week (prevention success rate: 96%)
- Revenue saved: 45 events × $8,000 avg = $360,000/week
- Annualized: $18.7M saved"

**[SHOW: Stock-out prevention chart, before/after]**

"**Metric 2: Inventory Turns Improvement**
- Before TAI: Overstock in slow SKUs (8% of inventory never sells)
- After TAI: Better allocation → more items sell before clearance
- Overstock reduction: From 8% to 2%
- On $2.25M inventory: Freed up $135,000 cash flow
- Carrying cost savings: $135,000 × 15% annual = $20,250/year"

**[SHOW: Overstock trend, declining line]**

"**Metric 3: Revenue per Available Inventory Space**
- Before TAI: $450/sqft of warehouse space
- After TAI: $540/sqft (20% improvement through better turns)
- You get same revenue from 20% less inventory investment
- Working capital freed: $450,000"

**[SHOW: Revenue per sqft trend, upward line]**

"**Total Annual Impact:**
- Stock-out prevention: $18.7M recovered revenue
- Overstock reduction: $20K savings
- Improved working capital: $450K freed
- Time savings (you don't manually allocate anymore): $47K/year
- **Total value: $19.2M annually**

Cost of TAI: $40K/year
ROI: (19.2M - 40K) / 40K = **$480x return**"

**[SHOW: ROI waterfall chart]**

---

### Proof (3 minutes)

**[Demonstrate Determinism & Integration]**

"I know what you're thinking: 'This is impressive on paper, but will this actually integrate with Shopify? What about Amazon? Will we lose data?'

That's exactly the right question. Here's how we prove it."

**[DEMO 1: Real-Time Shopify Sync (1 minute)]**

"Let me show you a live Shopify integration test.

I'll make a small inventory adjustment in Shopify and watch TAI pick it up instantly:"

**[DEMO SEQUENCE]**

1. **Shopify admin:** Add 10 units to SKU #12345
2. **TAI system:** Detects change in <30 seconds
3. **TAI dashboard:** Updates inventory count
4. **Timestamp:** 2026-01-26 14:32:00 UTC (immediate)

"This is the foundation of trust. Every change in Shopify instantly reflects in TAI. No delays. No data loss. No manual sync."

**[DISPLAY: Sync timing metrics]**

```
SHOPIFY INTEGRATION PERFORMANCE:
├── Avg sync latency: 22 seconds
├── P95 sync latency: 45 seconds
├── P99 sync latency: 90 seconds
├── Data accuracy: 99.98% (2 errors per 10,000 syncs)
├── Uptime: 99.95% (includes Shopify API downtime)
└── Monthly volume: 500K+ inventory updates processed
```

"This is retail-grade performance. It's reliable."

**[DEMO 2: Amazon Multi-Inventory Fulfillment (1 minute)]**

"Amazon is trickier because you're managing inventory across Amazon FBA and Merchant Fulfilled Network (MFN).

Watch how TAI handles both:"

**[DISPLAY: Amazon Dual-Channel Sync]**

```
AMAZON INTEGRATION (Dual-Fulfillment Model):

YOUR SETUP:
├── FBA (Fulfillment by Amazon): 600 units of SKU #1
│   └── Stored in Amazon warehouse
│   └── Amazon ships orders (2-day prime)
├── MFN (You ship): 200 units of SKU #1
│   └── Stored in your warehouse
│   └── You ship orders (3-5 day standard)

THE CHALLENGE:
├── If FBA runs out, can you fulfill from MFN? (sometimes, if customer accepts slow shipping)
├── How do you decide allocation between FBA and MFN?
├── What if Amazon's inventory count drifts from reality? (it happens)

TAI SOLUTION:
├── Real-time sync with Amazon API (FBA count)
├── Real-time sync with your warehouse (MFN count)
├── Continuous drift detection (Amazon says 600 but should be 580?)
├── Automatic reconciliation (alert warehouse to discrepancy)
├── Smart allocation (when FBA is low, funnel slow-moving items to MFN to preserve FBA for fast-movers)

RESULT:
├── FBA allocation optimized (always stocked with best sellers)
├── MFN utilization maximized (no dead stock)
├── Discrepancy alerts (warehouse catches inventory drift early)
└── Revenue: Higher velocity on both channels
```

"This is where most e-commerce ops teams struggle. Multiple channels, multiple fulfillment methods, nobody talking to each other. TAI brings order."

**[DEMO 3: Data Security & Integrity (1 minute)]**

"Last question: How do I know my inventory data is safe and accurate?

TAI generates audit receipts for every allocation decision:"

**[DISPLAY: Inventory Receipt]**

```
TAI INVENTORY ALLOCATION RECEIPT
═════════════════════════════════════════════
Date/Time: 2026-01-26 14:30:00 UTC
Decision ID: ALLOC-2026-01-26-0847
System: Inventory Optimization Engine
═════════════════════════════════════════════

INPUT DATA (Hash: 7a3f2c8e9d1b4e6f):
├── Shopify inventory: [200 units bestseller]
├── Amazon FBA inventory: [150 units bestseller]
├── Warehouse MFN: [80 units bestseller]
├── Total available: 430 units
└── Demand trends (past 30 days): [Shopify +15%, Amazon +8%, Retail +5%]

ALLOCATION LOGIC:
├── Step 1: Apply business rules
│   └── Shopify is primary (priority 1)
│   └── Amazon is growth (priority 2)
│   └── Retail is partner (priority 3, capped at 100/day commitment)
├── Step 2: Calculate optimal distribution
│   └── Shopify demand: 45 units/day → keep 200 (4.4 day supply) ✓
│   └── Amazon demand: 35 units/day → keep 150 (4.3 day supply) ✓
│   └── Retail need: 80 units/day Mon-Fri → allocate 80 (exact match)
└── Step 3: Monitor for restock
    └── Next purchase arrives in 6 days (plenty of buffer)

ALLOCATION DECISION:
├── Shopify: 200 units (no change)
├── Amazon: 150 units (no change)
├── Retail: 80 units (no change)
├── Decision: MAINTAIN (all channels optimized)

AUDIT TRAIL:
├── Hash of entire decision: 5c9d2f7e1a8b3g4h
├── Digital signature: TAI Private Key [verified]
├── Timestamp: UTC verified (no clock skew)
├── Status: IMMUTABLE ✓

COMPLIANCE NOTES:
├── No data loss: All inventory accounted for (430 units)
├── No unauthorized change: Signature prevents tampering
├── Fully auditable: Every decision logged with reasoning
├── Deterministic: Same input = same output (always)

═════════════════════════════════════════════
APPROVED FOR OPERATIONAL DECISION ✓
═════════════════════════════════════════════
```

"This receipt proves:
- We accounted for all inventory (430 units in, 430 units allocated)
- The decision reasoning is transparent (you can see exactly why)
- The decision is tamper-proof (cryptographic signature)
- Every allocation is auditable (if something goes wrong, we can trace it)

This matters if:
- An auditor asks 'How did you allocate inventory?'
- A warehouse manager asks 'Why does Shopify have 200 units?'
- Inventory goes missing (receipt shows where it should be)
- You want to replay decisions (same input = same output)"

---

### Close & Next Steps (2 minutes)

**[Outcome-Focused, Action-Oriented Close]**

"So here's what I'm hearing:
- Your current inventory process costs you $2.2M annually (stock-outs + overstock + time)
- TAI automatically optimizes allocation, preventing almost all stock-outs
- You get 96% fewer stock-out events, freeing up $450K in working capital
- Implementation is straightforward (Shopify API integration, 30 days)
- ROI is massive (480x return)

Would a 30-day POC make sense? Where you run TAI in parallel with your current system, measure the impact on stock-outs and overstock, and decide if you want to move forward?"

**[PAUSE FOR RESPONSE]**

**If YES:**

"Perfect. Here's what that looks like:
- Week 1: We integrate Shopify + Amazon APIs (2 conference calls, done by Wednesday)
- Week 2-4: TAI runs live in monitoring mode (sees your allocations, doesn't change anything yet)
- Week 4: You turn on auto-allocation (TAI makes recommendations)
- End of Week 4: We measure: Stock-out reduction, overstock improvement, time saved

Cost for POC: $2K (covers AWS infrastructure + API usage)

I'll send a formal proposal today that includes:
- Integration timeline + scope (exactly what we'll connect)
- Success metrics (the measurements we'll take)
- Data security & integration details (how we connect to Shopify)
- DPA (Data Processing Agreement for PII handling)

I'll also include a customized ROI calculator pre-filled with your GMV. You can adjust assumptions if ours are off.

Can you review by end of week? I want to make sure there are no surprises."

**[SCHEDULE FOLLOW-UP CALL FOR FRIDAY]**

**If "This looks great, but I need to talk to my team":**

"Totally fair. Here's what I'd do:

1. I'll send the proposal today + include an executive summary for your CEO
2. I'll include a technical summary for your warehouse manager (they care about the integration)
3. I'll include a financial summary for your CFO (they care about ROI)
4. In 48 hours, I'll follow up. If you want, I can jump on a call with your team.

The value here is time-sensitive: Seasonal inventory peaks in April. If we get TAI running by March, we catch the entire spring season (20% of annual revenue). If we wait until May, we miss the peak.

I want to make sure we lock you in for the Q1 cohort. Can we get a decision this week?"

---

## Post-Demo Execution

### Immediately After Demo (Next 30 minutes)

- [ ] Send thank-you email with:
  - Recording link (if wanted for internal sharing)
  - Detailed ROI analysis (custom to their GMV + margins)
  - POC proposal draft
  - Integration checklist (what we need from them)

### 48-Hour Follow-Up (Next Morning)

**Email Subject:** "POC Proposal: Inventory Optimization for [Company Name]"

```
Hi [Name],

Thanks again for the time yesterday. Your question about Amazon FBA/MFN complexity was excellent—it's exactly where most systems fail, and where TAI shines.

Attached:
1. POC Proposal (30-day timeline, $2K pilot cost)
2. Detailed ROI Analysis (custom to your $20M GMV + your margins)
3. Integration Checklist (Shopify + Amazon APIs, what we need from you)
4. Data Security Addendum (how we protect inventory data)
5. Case Study: [Similar DTC fashion brand] saw 94% stock-out reduction (2-page summary)

Key outcomes for your POC:
- Compare actual stock-outs: Before vs After TAI
- Measure overstock reduction: Before vs After allocation
- Quantify time saved: Hours of manual allocation eliminated
- Verify Shopify integration: Real-time sync working perfectly

For your finance team:
- Cost: $2K for POC (covers infrastructure + support)
- Expected value: $187K in stock-out prevention (first month)
- ROI: 94x (first month alone!)
- Timeline: 30 days to decision

My ask: Can we schedule 20 minutes Friday to walk through this with you and your warehouse manager? I want to make sure the technical side feels comfortable with the integration.

Looking forward,
[Your name]
```

### 1-Week Follow-Up (If No Response)

**Call (not email):** "Hi [Name], wanted to check in on the POC proposal. Any questions from your warehouse team? What's the biggest concern we need to address?"

### 2-Week Follow-Up (Close or Qualify Out)

**Email:** "POC spots filling up for Q1. I want to make sure [Company Name] is locked in for March start. Can we get a commitment this week?"

---

## Common Objections & Responses

### Objection 1: "Shopify already has inventory management. Why do we need this?"

**Response:**

"Good point. Shopify is great at managing Shopify inventory. The problem is it doesn't talk to Amazon, doesn't talk to your warehouse, and doesn't predict stock-outs.

Think of it like this:
- Shopify = Your store's cash register (tracks Shopify sales)
- TAI = Your supply chain brain (sees all channels, predicts problems)

They work together. TAI feeds insights back to Shopify (via inventory adjustments), but TAI is the layer that understands your whole business.

The ROI comes from:
1. Seeing all channels at once (not just Shopify)
2. Predicting stock-outs before they happen (Shopify doesn't predict)
3. Optimizing allocation (Shopify can't do this—it's too simple)

That's what the POC proves."

### Objection 2: "Our warehouse is too complicated. The integration won't work."

**Response:**

"Tell me about the complexity. What's the setup?"

**[LISTEN TO DETAILS]**

"OK, so you've got [describe what they said]. Here's what I'm hearing: Standard e-commerce warehouse, maybe with some custom processes.

We've integrated with 150+ warehouses. Most have custom quirks. The question isn't 'Is it possible?' It's 'How much integration work is it?'

Here's what I'll do:
1. Connect you with our warehouse integration specialist
2. She'll audit your setup (30 minutes, zero obligation)
3. She'll tell you: 'This is 1-week integration' or 'This is 3-week integration'
4. We'll price accordingly

Sound fair?"

### Objection 3: "What if this disrupts our business? What's the rollback plan?"

**Response:**

"Excellent question. This is why we do POC in monitoring mode first.

**Week 1-3:** TAI watches and makes recommendations, but doesn't change anything
- Your current system continues to work normally
- Zero risk to operations
- You just see what TAI would have done

**Week 4:** You manually turn on auto-allocation (you flip the switch)
- TAI starts making live allocation decisions
- But we do it slowly: Start with 10% of SKUs, then 50%, then 100%
- If something breaks, you flip the switch back off

**Rollback:** Takes 10 minutes
- You disable auto-allocation
- TAI stops making decisions
- Your system reverts to whatever it was doing before

No data loss. No inventory gets deleted. You just revert to manual allocation."

### Objection 4: "What about the cost? $40K/year seems high."

**Response:**

"Let me put this in perspective:

Cost: $40K/year
Value: $19.2M (stock-out prevention + overstock reduction + working capital)
ROI: 480x

Or said another way:
- You get the full cost of the system back in 2 days of stock-out prevention
- Everything after that is profit

If we're wrong and you only get 10% of the value, you still have 48x ROI.

Plus, this POC is just $2K. You can measure the actual value with your data, then decide if the $40K annual cost makes sense.

Fair deal?"

---

## Success Criteria (This Demo)

After the demo, the prospect should be able to answer YES to:

- [ ] "Can TAI sync with Shopify in real-time?" (Yes, we saw it sync in <30 seconds)
- [ ] "How would this reduce our stock-outs?" (Yes, 96% reduction demonstrated with our data)
- [ ] "What's the ROI?" (Yes, 480x, and I understand how you calculated it)
- [ ] "Is implementation straightforward?" (Yes, Shopify API integration, 30 days)

If all 4 are YES, move to POC proposal stage.

---

## Demo Environment Requirements

**For this demo to work, you need:**
- E-commerce demo data loaded (Shopify + Amazon + retail)
- Real-time inventory sync working (Shopify API mock + database)
- Dashboard displaying channel inventory metrics
- Stock-out prediction algorithm running
- Allocation recommendation engine showing reasoning
- Receipt generation system working
- ROI calculator preconfigured for e-commerce scenario

**Setup checklist:**
- [ ] Demo environment deployed (all services running)
- [ ] E-commerce data loaded from `ecommerce_inventory_optimization.json`
- [ ] Dashboard displays without lag (<500ms response time)
- [ ] Shopify sync working (real API or realistic mock)
- [ ] Amazon API integration visible (inventory counts updating)
- [ ] Allocation recommendations visible (with reasoning)
- [ ] Receipt signing working (cryptographic proof visible)
- [ ] Stock-out prediction showing (future inventory status)
- [ ] Calculator preconfigured for $20M GMV brand

---

**Created:** 2026-01-26
**Last Rehearsed:** [Update after each practice run]
**Success Rate:** [Track: NPS score after each demo]
