# Customer Contract Negotiation Playbook - TAI Erlang Autonomics

**Purpose**: Equip VP Sales with battle-tested negotiation tactics, objection responses, and closing frameworks

---

## Table of Contents

1. [Pre-Negotiation Preparation](#pre-negotiation-preparation)
2. [Negotiation Frameworks](#negotiation-frameworks)
3. [Objection Response Library](#objection-response-library)
4. [Closing Tactics](#closing-tactics)
5. [Red Flags & Walk-Away Triggers](#red-flags--walk-away-triggers)
6. [Post-Negotiation Follow-Up](#post-negotiation-follow-up)

---

## Pre-Negotiation Preparation

### Discovery Call Prep (Before negotiation call)

**Goal**: Gather maximum intel on Customer's situation before price discussion

**Questions to Ask on Discovery Call** (Week 3-4):

```
PROBLEM VALIDATION:
1. "You mentioned entitlement management is manual. Walk me through your current process."
   → Understand: time/effort, number of people, how often changes happen

2. "How much time does your team spend on this annually?"
   → Validate: 40 hours/month? More? Less? (affects ROI model)

3. "Have you experienced compliance violations or billing errors from entitlement issues?"
   → Determine: Pain severity (high pain = willing to pay more)

4. "What was the cost of [specific incident they mentioned]?"
   → Quantify: Hard dollar impact (critical for ROI justification)

BUDGET & DECISION PROCESS:
5. "What does your annual software infrastructure budget look like?"
   → Understand: Budget availability, spending pattern

6. "Who approves a $[X]K investment? What's the approval process?"
   → Identify: Decision authority, timeline, other influencers

7. "When do you finalize budgets for next year?"
   → Determine: If they can approve today vs. need to wait for budget cycle

COMPETITIVE CONTEXT:
8. "Are you evaluating any other solutions?"
   → Red flag if "yes" - understand competitive pressure

9. "What would an ideal solution look like for you?"
   → Understand: Must-haves vs. nice-to-haves; informs TAI positioning

TIMELINE & COMMITMENT:
10. "What's your timeline to implement a solution?"
    → Understand: Fast (Q1) = will pay premium; slow (Q4) = price-sensitive
```

**Pre-Call Research Checklist**:

- [ ] Pull Customer's recent funding round (if VC-backed) → Signals cash availability
- [ ] Check annual revenue estimates (Crunchbase, PitchBook) → Informs price range
- [ ] Research executive team on LinkedIn → Understand company culture, risk appetite
- [ ] Scan recent news (press releases, analyst reports) → Identify strategic priorities
- [ ] Identify their current entitlement system → Understand integration complexity

---

## Negotiation Frameworks

### The Three-Tier Pricing Framework (Recommended)

**Purpose**: Offer customers choice while anchoring them to your preferred price point

**Strategy**:
- **Anchor High**: Present most expensive tier first (anchors perception)
- **Target Mid**: Your actual target price (what you want them to buy)
- **Safety Low**: Discounted tier (if they push hard, you have fallback)

**Example** (for $150K target):

```
TIER PRESENTATION (in order):

"For a deployment at your scale, we typically see three pricing approaches:

OPTION 1: COMPREHENSIVE DEPLOYMENT (Recommended for your situation)
├─ All 500+ SKUs managed autonomously
├─ Real-time entitlement enforcement
├─ Cryptographic audit trail + compliance reporting
├─ 24/7 support + quarterly optimization reviews
└─ Price: $250K/year

   "This is our standard offering for mid-market SaaS with complex SKU models.
    You get everything included."

OPTION 2: CORE DEPLOYMENT (Most popular - where 60% of customers start)
├─ First 300 SKUs managed
├─ Real-time entitlement enforcement
├─ Cryptographic audit trail
├─ Business hours support + monthly check-ins
└─ Price: $150K/year ← YOUR TARGET

   "This is where most customers start. You can expand to all SKUs in Year 2
    once you see the ROI."

OPTION 3: PILOT DEPLOYMENT (Lowest barrier to entry)
├─ Single product line (50-100 SKUs)
├─ Real-time entitlement enforcement
├─ Audit trail only (no compliance reporting)
├─ Email support only
└─ Price: $50K/year

   "If budget is constrained, we can start here and expand. ROI proof points
    typically come in Month 2-3."
```

**Why This Works**:
1. **Anchoring effect**: $250K makes $150K feel like a deal
2. **Customer choice**: Reduces friction (feels like their decision, not your requirement)
3. **Upsell path**: If they start with Pilot, Year 2 upgrade to Core/Comprehensive is natural
4. **Fallback**: If they negotiate hard, you have Option 3 to land deal

---

### The ROI-Driven Negotiation Framework

**Purpose**: If customer says "too expensive," shift conversation to ROI, not price

**Script** (when customer objects to price):

```
CUSTOMER: "That's more than we budgeted. Can you do $50K instead?"

YOU: "I hear the budget constraint. Before we talk price, help me understand
     the math. You mentioned 40 hours/month of manual work?

     [Pull out ROI model]

     That's 480 hours/year. At your average fully-loaded rate ($[X]/hour),
     that's $[X] of effort annually.

     TAI automates 87% of that, so that's [$X * 0.87] in labor savings alone.

     Plus, you mentioned compliance audit cycles take 12 weeks at [X hours/week].
     That's [X hours]. Compressed to 4 weeks, that's another [X hours] saved.

     So conservatively, we're talking $[X] in annual savings.

     At $150K/year, your payback is [X] months. Does that ROI work for you?"

[Most customers will accept when they see the math]

IF THEY STILL PUSH BACK:
"Let's look at phased approach: Start Year 1 with Core tier ($50K),
 prove ROI in 90 days, expand to Comprehensive in Year 2.

 When can we model that out together?"
```

**ROI Model Template** (build this during discovery):

| Line Item | Baseline (Annual) | TAI Impact | Savings |
|-----------|-------------------|-----------|---------|
| Manual SKU work: 40 hrs/mo × 12 × $[rate] | $[X] | -87% | $[X] |
| Compliance audit work: 12 wks × 40 hrs × $[rate] | $[X] | -67% | $[X] |
| Compliance violations (at $[X]/violation × 2/year) | $[X] | -75% | $[X] |
| **Total Annual Benefit** | | | **$[X]** |
| **TAI Cost** | | | **$150K** |
| **Net Annual Benefit** | | | **$[X]** |
| **Payback Period** | | | **[X] months** |

---

### The Anchoring & Concession Strategy

**Purpose**: Make customer feel like they "won" at negotiation (while you get target price)

**The Setup**:

1. **Anchor High** (Day 1 call)
   - YOU: "For a deployment at your scale, we typically price this at $250K/year"
   - Customer internally freaks out (good - you've reset their expectations)

2. **Small Concessions** (Day 2-3 follow-up)
   - YOU: "I spoke with my CEO about your timeline. We can do $200K if you sign this month."
   - Customer feels like they negotiated down from $250K (even though $200K wasn't your goal)

3. **Final Anchor** (Day 3-4)
   - YOU: "My finance team can approve $150K/year for a 2-year commitment."
   - Customer thinks they negotiated from $200K → $150K (wins in their mind)
   - YOU got your target ($150K) + 2-year commitment (bonus)

**Why This Works**:
- Customer feels like they negotiated down (psychological win)
- You get your target price + better terms
- Multiple "moves" create feeling of progress (vs. single offer feeling like take-it-or-leave-it)

---

## Objection Response Library

### Price-Related Objections

#### Objection 1: "That's more than we budgeted"

**Underlying concern**: Budget constraints OR seeking discount leverage

**Response**:

```
EMPATHY: "I completely understand—budget is always a factor. Let me ask:
         what was your budget range?"

[LISTEN - critical to hear their number]

IF THEY SAY "$50K":
"I appreciate the constraint. $50K would be Phase 1 only (50-100 SKUs).
 But you mentioned needing to manage 500+ SKUs, so let's explore options:

 Option A: Start with Pilot ($50K Year 1, expand Year 2)
 - Pros: Fits your budget, ROI proof in 90 days
 - Cons: Longer overall timeline to full automation

 Option B: 2-year commitment at $130K/year ($260K total, $65K/year)
 - Pros: Better pricing, locks in rate
 - Cons: Multi-year commitment required

 Which path interests you more?"

[Let them choose - creates buy-in]
```

**DO's**:
- [ ] Ask what their budget actually is (vs. assuming)
- [ ] Offer multiple options (Pilot/Core/Comprehensive or multi-year discount)
- [ ] Tie price back to ROI or scope

**DON'Ts**:
- [ ] Don't cut price immediately (appear desperate)
- [ ] Don't justify pricing based on cost (focus on value)
- [ ] Don't say "our lowest price is..." (creates price expectation)

---

#### Objection 2: "Can you match competitor's price of $[X]?"

**Underlying concern**: Customer is shopping competitors OR testing your flexibility

**Response**:

```
CURIOUS: "Interesting—who's the competitor, and what does their solution
        include?"

[LISTEN - understand what they're comparing]

IF THEY NAME REAL COMPETITOR:
"[Competitor name] is good at [their strength], but they don't do
 [TAI's key differentiator: cryptographic receipts].

 That's why [your reference customer] switched from them to us.

 Can I ask: Is price the deciding factor, or is there a feature gap?"

[If price is deciding factor: Use ROI model from above]
[If feature gap: Explain TAI advantage, then circle back to ROI]

IF THEY DON'T NAME A REAL COMPETITOR:
"I'm curious—help me understand the gap. Our pricing reflects [1]
 cryptographic receipt ledger, [2] <50ms latency SLA, [3] 8-week
 implementation included.

 What features would a $[X] competitor remove to hit that price?"

[Usually customer realizes their "competitor" isn't apples-to-apples]
```

**DO's**:
- [ ] Ask questions before defending price (might be misinformation)
- [ ] Focus on value differentiation (not feature-for-feature comparison)
- [ ] Use reference customers as proof

**DON'Ts**:
- [ ] Don't trash-talk competitors (appears insecure)
- [ ] Don't automatically cut price (signals weakness)
- [ ] Don't accept competitor's price as given (question if real)

---

#### Objection 3: "We need to think about it"

**Underlying concern**: Decision paralysis OR lack of budget authority

**Response**:

```
CURIOUS: "I totally understand—this is an important decision. What do you
        need to think through?

        Is it [A] the pricing, [B] the timeline, or [C] internal alignment
        with your team?"

[LISTEN - understanding the real objection]

IF THEY SAY "PRICING":
"If I could work with you on pricing, would you move forward?
 I want to make sure pricing isn't the only blocker."

[If they say yes, you have negotiation leverage]
[If they say no, pricing wasn't the real objection]

IF THEY SAY "TIMELINE":
"When would be ideal for you to start? That might affect pricing—
 early start gets us a better rate."

[Create urgency: incentivize early decision]

IF THEY SAY "INTERNAL ALIGNMENT":
"Smart—involves the right people. Who do we need to get aligned?
 [Finance, Product, Engineering?]

 Could we schedule a 30-min call with them this week to run through
 the ROI model?"

[Move from individual to group sales motion]
```

**DO's**:
- [ ] Always ask what they need to think through (vs. accepting at face value)
- [ ] Create urgency (early-bird pricing, limited availability)
- [ ] Expand deal (include more stakeholders)

**DON'Ts**:
- [ ] Don't accept "I'll get back to you" without a specific date
- [ ] Don't send proposal without commitment to discuss timeline

---

### Implementation/Scope Objections

#### Objection 4: "Your 8-week timeline is too long for us"

**Underlying concern**: They want faster deployment OR timeline is lower priority than they're saying

**Response**:

```
CURIOUS: "When do you need to go live?

        [LISTEN to their ideal date]

        What's driving the urgency?"

[Understand if this is hard deadline or preference]

IF THEY HAVE REAL HARD DEADLINE (e.g., compliance audit in 6 weeks):
"We can absolutely accelerate. Here's what it takes:

 - Dedicate your team (3-4 engineers full-time vs. 1-2 part-time)
 - Provide all SKU definitions day 1 (not phased over 2 weeks)
 - Skip comprehensive UAT (do focused testing only)
 - Cut training to 1 day (not 2 days)

 This could compress timeline to 5-6 weeks, but:
 [PRICE ADJUSTMENT - increased cost to reflect accelerated effort]

 Are you willing to invest more to accelerate?"

[Most will say yes if deadline is real - capture price increase]

IF THEY DON'T HAVE REAL DEADLINE:
"Walk me through your timeline. When does your team have capacity?

 [Often, you'll discover 8 weeks fits their capacity perfectly]

 The 8-week timeline isn't arbitrary—it's designed to give your team
 time to prepare without disrupting operations. Compressed timelines
 often cause problems."
```

**DO's**:
- [ ] Ask questions before accepting their timeline pressure
- [ ] Offer acceleration option (+ charge for it)
- [ ] Educate them on why 8 weeks is actually ideal

**DON'Ts**:
- [ ] Don't immediately compress timeline (signals desperation)
- [ ] Don't accept timeline without questioning (might be artificial pressure)

---

#### Objection 5: "We need custom development / features beyond your standard offering"

**Underlying concern**: They have unique needs OR testing your flexibility

**Response**:

```
CURIOUS: "Tell me more about what you need that's not standard.

        [LISTEN - understand their request]

Let me ask: Is this [A] a must-have for go-live, or [B] nice-to-have
for Year 2?"

[If must-have: significant scope/cost implication]
[If nice-to-have: can be phased]

SCENARIO A: MUST-HAVE CUSTOM FEATURE
"That's a reasonable ask. Custom development typically costs $[X]/week.

 Your request looks like ~[2-4] weeks of work. That's an additional $[X].

 We can add it to your SOW, but that extends timeline to [Week X] and
 increases Year 1 cost to $[Total].

 Is that acceptable?"

[Usually, they'll deprioritize once they see cost]

SCENARIO B: NICE-TO-HAVE
"That's a great idea for Year 2. Let's include it in our roadmap,
 and we can prioritize it based on customer feedback.

 For Year 1, we recommend starting with standard features—you'll get
 better ROI, and we can iterate based on production learnings."

[Push back to Year 2 - saves custom dev, maintains margin]
```

**DO's**:
- [ ] Quantify custom development cost (makes them think twice)
- [ ] Push optional features to Year 2 (most customers accept)
- [ ] Use "roadmap" language (doesn't promise commitment)

**DON'Ts**:
- [ ] Don't agree to custom features easily (erodes margin)
- [ ] Don't build custom features into base offering (sets expectation for all customers)

---

### Relationship/Trust Objections

#### Objection 6: "You're a new company. How do I know you'll be around in 2 years?"

**Underlying concern**: Risk - what if TAI goes out of business?

**Response**:

```
HONEST: "That's a completely fair question. New companies do have risk.
        Here's how we mitigate that:

        [Address with facts, not fluff]:

        ✓ Our founders [mention background: "ex-[FAANG], [Y Combinator]"]
        ✓ We're funded by [investors: Sequoia, Y Combinator, etc.]
        ✓ We have [X] customers live + [X] more in pipeline
        ✓ Our unit economics are healthy (customers already profitable)
        ✓ We have [12+ month] runway

        Plus, your contract includes:
        - 90-day cancellation clause (if unhappy, you can leave)
        - Data export capabilities (you own your data always)
        - Runbook & documentation (you can operate independently)

        Worst case: You have a working system you can operate yourself."

[Most customers accept this level of transparency]

IF THEY STILL PUSH:
"Would a shorter contract term help? We can do 1 year instead of
 multi-year if that reduces your risk."

[Shorter term = less commitment on their side]
```

**DO's**:
- [ ] Be honest about startup risk (don't oversell longevity)
- [ ] Point to concrete evidence (funding, customers, runway)
- [ ] Emphasize customer control (data portability, documentation)
- [ ] Offer contract flexibility (shorter terms, cancellation clauses)

**DON'Ts**:
- [ ] Don't over-promise longevity (erodes credibility)
- [ ] Don't dismiss their concern (shows you don't understand risk)

---

#### Objection 7: "We need SOC 2 certification before we can sign"

**Underlying concern**: Legitimate compliance requirement OR stalling tactic

**Response**:

```
CURIOUS: "When do you need SOC 2 certification? Is this a hard
        requirement for go-live, or can we implement and audit afterward?"

[LISTEN - understand timing]

IF THEY NEED IT FOR GO-LIVE (hard requirement):
"We're actively pursuing SOC 2 Type II audit. [Status: ~[X months] out].

 In the interim, we can provide:
 - SOC 2 readiness report (how we meet SOC 2 controls)
 - Current security certifications (ISO 27001, pending)
 - Security audit by [third party if available]
 - Interim DPA with SOC 2 commitments (in our MSA)

 Many customers proceed with interim controls and finalize audit
 within first 90 days of operation.

 Does that path work for you?"

[Most will accept interim controls if you're transparent]

IF THEY'RE USING AS STALLING TACTIC:
"I understand—security is important. Let's put a timeline on it:

 We provide SOC 2 readiness report by [date]. You review and approve.
 If approved, we proceed with implementation. Fair?"

[Sets deadline, removes ambiguity]
```

**DO's**:
- [ ] Be transparent about SOC 2 status (don't claim you have it if you don't)
- [ ] Provide interim documentation (shows you take security seriously)
- [ ] Set timeline (prevents indefinite stalling)

**DON'Ts**:
- [ ] Don't claim SOC 2 certification you don't have (legal risk)
- [ ] Don't dismiss compliance concerns (legitimate business requirement)

---

## Closing Tactics

### The Trial Close

**Purpose**: Gauge readiness to sign without asking for final commitment

**Script**:

```
NEUTRAL: "Just to make sure we're aligned: If we put together a contract
        with [recap key terms], would you be ready to sign?"

[Listen for hesitation - if there's any, there's still an objection]

IF THEY SAY "YES":
→ Move immediately to contract preparation
  "Great. I'll send you the MSA + SOW by [tomorrow] for your review.
   Can you get legal eyes on it by [Friday]? I'm hoping to have this
   signed by [next Wednesday]."

IF THEY HESITATE:
→ Dig deeper: "I sense some hesitation. What's the concern?"
  [Address the unstated objection before pushing for signature]

IF THEY SAY "NO":
→ Don't panic - ask: "What's still not aligned?"
  [Go back to earlier objection handling]
```

### The Takeaway Close

**Purpose**: Create urgency by implying deal might not be available later

**Script**:

```
"I love your fit for TAI. But I want to be transparent: We're being
 selective with early customers. We have [2-3] other qualified prospects
 in the same stage.

 We typically move fastest with customers who decide within [1-2 weeks].

 If you want to be in our first cohort [reference your first customer
 cohort], I'd recommend moving to contract by [specific date].

 Does that timeline work for you?"

[Creates urgency without sounding desperate]
```

**Important**: Only use if it's genuine - never bluff about other prospects.

### The Committee Close

**Purpose**: Move from individual negotiation to group alignment

**Script**:

```
"I think you're aligned, but let's get buy-in from [Finance, Product, Eng].

 Can we schedule a 30-minute call with [names] to walk through:
 - ROI model
 - Implementation timeline
 - Your success metrics

 [Date/time suggestion]. Would that work?"

[Expand deal, reduce individual's negotiation leverage]
```

---

## Red Flags & Walk-Away Triggers

### Green Lights (Good Signs to Proceed)

- ✓ They provide specific pain metrics (hours/month, $ impact)
- ✓ They ask about implementation timeline (signal of intent)
- ✓ They ask about support/SLA (signal of professionalism)
- ✓ They ask to loop in legal/finance (signal of moving to contract)
- ✓ They commit to UAT timeline (signal of seriousness)

### Yellow Flags (Proceed with Caution)

- ⚠ They say "we need to think about it" without specifying timeline
- ⚠ They want deep discount (20%+ below your floor) without pain to justify it
- ⚠ They push timeline but won't commit resources
- ⚠ They ask for SOC 2 cert but know you don't have it yet
- ⚠ They want long implementation support period (8+ weeks) without paying extra
- ⚠ They ask for 2-3 year contract but negotiate aggressively on Year 1 price

### Red Flags (Consider Walking Away)

- ❌ They demand unlimited liability cap (signals bad faith)
- ❌ They demand exclusive territory/non-compete clause (red flag for startup)
- ❌ They want you to sign NDA but won't disclose their requirements (asymmetry)
- ❌ They want 50%+ discount (suggests they don't really need your product)
- ❌ They won't commit to implementation timeline (suggests low priority)
- ❌ They demand custom features but won't pay for them (entitled buyer)
- ❌ Decision timeline extends beyond 60 days (likely won't close)

### When to Walk Away (Protect Margin & Momentum)

**Walk away if**:

1. **Below margin floor**: If deal is below 50% gross margin, it's not worth the support/integration effort
   - Exception: Strategic account (reference value) - only if you can afford it

2. **Unreasonable scope creep**: If customer demands 5+ custom features/integrations, effort exceeds revenue
   - Exception: If phased (Year 1 core, Year 2+ optional)

3. **Bad customer culture**: If customer is adversarial in negotiation, they'll be difficult post-sale
   - Red flag: Demands, threats, refusing to honor agreed terms

4. **Extended sales cycle**: If prospect takes 60+ days to decide, they're not urgent/committed
   - Exception: Enterprise deals (longer sales cycles are normal)

**Walking-away script**:

```
"I appreciate your interest in TAI. After our discussions, I think
 there's a mismatch between your requirements and our offering.

 [Specific reason: "Your need for custom features exceeds our current
  product roadmap" / "Your budget doesn't support the implementation
  scope you need" / "Your timeline is too compressed for quality"]

 I'd rather be honest with you now than set expectations we can't meet.

 [Graceful exit]: If your situation changes [timeline/budget/scope],
 I'd love to revisit. In the meantime, [recommendation for alternative
 approach or competitor if appropriate]."

[Leaves door open for future, maintains relationship]
```

---

## Post-Negotiation Follow-Up

### Day 1 After Negotiation (Recap Email)

**Purpose**: Confirm agreed terms, remove ambiguity, create momentum

**Email Template**:

```
Subject: Thanks for the great call! Confirming next steps

Hi [Name],

Thanks for taking the time to discuss TAI today. I really enjoyed learning
about [specific insight from call].

I wanted to recap what I heard and confirm next steps:

AGREED TERMS:
- Service: TAI Erlang Autonomics for [X] SKUs
- Price: $[X]/year
- Payment: 50% at signature, 50% at go-live
- Timeline: 8-week implementation starting [Date]
- Go-live target: [Date]

NEXT STEPS:
1. I'll send MSA + SOW + DPA by [tomorrow] for your legal review
2. You review by [Friday] and send comments to [legal contact]
3. [Legal contact] and I coordinate on any proposed changes
4. Target signature date: [next Wednesday]

If I've misunderstood anything, please let me know today. Otherwise,
I'll send docs tomorrow.

Looking forward to working with you!

[Name]
```

**Why this works**:
- Confirms terms in writing (prevents later disputes)
- Creates clarity on next steps (removes ambiguity)
- Sets deadlines (creates urgency)
- Friendly tone (maintains relationship)

### Day 3 (Contract Sent)

**Don't** send contract and disappear. Follow up:

```
Subject: Your TAI contracts are ready for review

Hi [Name],

Per our call, I've sent MSA + SOW + DPA to [email].

If you have questions:
- Technical questions (SOW, implementation): reach out to [CTO]
- Legal questions (MSA, DPA): reach out to [Legal]
- Commercial questions (pricing, payment): reach out to me

Target for feedback: [Friday EOD]. We want to aim for signature by
[next Wednesday].

Any blockers, let me know immediately.

[Name]
```

### Day 5 (Status Check)

**If no response by Day 5**: Make a phone call (vs. email)

```
"Hi [Name], just checking in—did you get the contracts? Any initial
 thoughts or questions from your legal team?

 We're targeting signature by Wednesday. What's the timeline on your
 end?"
```

**Purpose**: Prevent contracts from gathering dust; maintain momentum

---

## Negotiation Closing Checklist

Before you get off the final negotiation call, confirm:

- [ ] **Price agreed upon** (specific number, in writing)
- [ ] **Payment terms agreed** (50/50, Net 30, milestone-based, etc.)
- [ ] **Term length agreed** (12 months, multi-year, auto-renewal terms)
- [ ] **Start date agreed** (exact week for implementation kickoff)
- [ ] **Scope clarity** (what's in-scope vs. out-of-scope clearly defined)
- [ ] **Implementation timeline agreed** (8 weeks or accelerated with cost increase)
- [ ] **Support level agreed** (business hours vs. 24/7, response SLAs)
- [ ] **Key contacts identified** (who approves budget, who's technical contact, who signs contract)
- [ ] **Legal process clear** (do they have legal counsel, timeline for review)
- [ ] **Decision authority confirmed** (who can sign on behalf of customer)
- [ ] **Next meeting scheduled** (kickoff meeting date locked in Week 7)

---

## Key Negotiation Principles

1. **Listen 70%, Talk 30%** - Ask questions, let customer talk. They'll reveal objections and priorities.

2. **Never be first to name price** - If customer names price first, they've set anchor. If you have to, anchor high.

3. **Separate price from value** - Defend value independently of price. "Our features justify $X" ≠ "here's a discount."

4. **Create options, not ultimatums** - "Here are three tiers" is better than "it costs $X, take it or leave it."

5. **Use silence strategically** - After you make offer, stay silent. Let them respond first. Whoever talks first loses leverage.

6. **Document everything** - Every agreed term in writing. Verbal agreements evaporate during legal review.

7. **Know your walk-away price** - Before negotiation, know minimum price that makes deal acceptable. Stick to it.

8. **Expand deal scope** - If customer won't move on price, ask for longer term / larger scope / more features.

9. **Build trust** - Be honest about constraints, timelines, abilities. Transparency builds trust; promises you can't keep destroy it.

10. **Focus on ROI, not features** - Don't sell features; sell outcomes. "87% automation" beats "real-time API."

---

**Document Version**: 1.0
**Last Updated**: 2026-01-26
**Status**: Ready for VP Sales use

