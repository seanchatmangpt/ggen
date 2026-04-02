# TAI Discovery Call Script (Refined for Week 3-4 Execution)

**Duration:** 30-45 minutes
**Format:** Phone or Zoom (video preferred for rapport building)
**Goal:** Qualify prospect on 8 ICP dimensions, identify pain points, secure technical deep-dive

---

## Pre-Call Preparation (15 minutes)

### Research Checklist
- [ ] Find company profile (Crunchbase, LinkedIn, website)
- [ ] Employee count, funding stage, recent news, revenue estimate
- [ ] Tech stack research (G2, StackShare, Glassdoor job postings)
- [ ] Competitor landscape (who else are they likely using?)
- [ ] Mutual connections (any way to build credibility?)
- [ ] Write 2-3 pain point hypotheses based on business model

### Environmental Setup
- [ ] Quiet room, camera on (if video call)
- [ ] Laptop + phone (backup)
- [ ] Prospect info + notes sheet visible
- [ ] Mute notifications
- [ ] CRM/Google Docs open for real-time notes
- [ ] Timer visible (to stay within 45 min)
- [ ] Water bottle + quiet setting

### Calendar Invite Sent
```
Subject: Discovery Call - TAI Erlang Autonomics [VP Sales Name]

Hi [First Name],

Looking forward to our conversation on [DATE] at [TIME]. I'll send Zoom link
10 minutes before the call. Quick agenda:

- Understand your current SKU/entitlement setup
- Explore pain points and compliance requirements
- See if a 30-day POC makes sense for you

No pitch—just discovery. Excited to learn about [Company]'s business.

Best,
[Your Name]
```

---

## Opening (2 minutes)

### Script (Verbatim)

> "Thanks so much for taking the time. I really appreciate it.
>
> At TAI, we've built an autonomous SKU management and entitlement governance system that works on GCP. What we're hearing from companies like [Intercom/Twilio/Wise - pick one similar to theirs] is that their teams spend 40-50 hours a month on manual work around SKUs and tier rules—spreadsheets, ad-hoc fixes, no audit trail.
>
> We built something to solve that. That said, I'm really here to listen and understand if this is even on your radar. I've got some targeted questions, but no pressure—if this doesn't fit, I'll still send you something useful.
>
> Fair?"

### Key Principles
- **No pressure:** Remove objection immediately
- **Social proof:** Mention peer company (similar to theirs)
- **Problem-first:** Lead with problem, not product
- **Permission:** Ask permission to ask questions

---

## Section 1: Current State (10 minutes)

### Purpose
Establish baseline: How much manual effort? What's the system? Is there audit capability?

### Q1: Team & Effort Baseline

**Script:**
> "Let me start with a baseline question. How many people on your team spend time managing SKUs, entitlements, or quotas each week?"

**Listen for:**
- Numbers (0-1 FTE vs 3-5 FTE = high pain)
- Role titles (engineers, product ops, finance)
- What % is manual vs automated

**Follow-up if needed:**
> "What % of their time is manual work versus automated?"
> "What tasks take the most time—provisioning new customers, changing tier limits, auditing compliance?"

**CRM Note:** Capture hours/month and FTE estimate

---

### Q2: Current System Architecture

**Script:**
> "Help me understand your current setup. What does your entitlement or quota system look like today?"

**Listen for (good signals):**
- "We use spreadsheets mostly" = Manual, no audit trail (high pain)
- "Database triggers" = Some automation, but scaling challenges
- "Custom-built system" = Existing investment, might be slow to change
- "Legacy system (SAP, Oracle)" = Rigid, hard to adapt

**Listen for (red flags):**
- "It's all automated, no issues" = Mature/satisfied, low urgency
- "On-prem only, can't move" = Wrong segment

**CRM Note:** Capture current system type

---

### Q3: Change Management & Audit

**Script:**
> "How do you currently track changes to SKU rules, pricing tiers, or quota limits? Like, if someone changes a tier, how do you document that?"

**Listen for (good signals):**
- "Manually in a doc" = No formal process (compliance risk)
- "Git commits, but not auditable" = Technical but incomplete
- "We don't really track it" = MAJOR compliance gap (TAI's breakthrough)
- "Auditors ask, we struggle to answer" = Pain point validated

**Listen for (red flags):**
- "We have a complete audit trail" = System mature, less urgent

**CRM Note:** Capture audit capability level

---

### Q4: Incidents & Business Impact

**Script:**
> "Have you had any incidents where entitlements were set wrong? Over-provisioned, under-provisioned, or misconfigured?"

**Listen for:**
- "Yes, it's cost us money" = STRONG signal
- Specific incident stories (customer got service free, compliance violation, etc.)
- Estimate of impact ($10K? $100K? $1M+?)

**Follow-up:**
> "How often does that happen? What was the cost/impact?"

**CRM Note:** Capture incident frequency and cost

---

### Q5: Tech Stack Deep-Dive

**Script:**
> "Quick tech question—what's your current stack? Database, cloud platform, messaging system?"

**Listen for (good signals):**
- "GCP + Postgres" = Native fit for TAI
- "GCP + Pub/Sub" = Ideal fit (TAI-native)
- "AWS-only" = Possible but more implementation work

**Listen for (red flags):**
- "On-prem only, no cloud" = Not a fit for MVP (possible later)
- "Multi-cloud fragmented" = More complex but potentially higher value

**CRM Note:** Capture tech stack

---

### Q6: GCP Usage (Tactical)

**Script:**
> "Do you use Google Cloud Platform today, or are you on another cloud?"

**Listen for:**
- "Yes, GCP is our primary" = Fastest implementation (8 weeks)
- "Multi-cloud but GCP included" = Manageable (12 weeks)
- "AWS only" = Longer integration but doable (16 weeks)
- "On-prem" = Later phase consideration

**CRM Note:** Capture GCP adoption level (Green / Yellow / Red)

---

## Section 2: Pain Point Validation (10 minutes)

### Purpose
Confirm they have specific pain points TAI solves. This is the "sell" moment.

### Q7: Compliance Requirements

**Script:**
> "Do you have compliance requirements we should know about—SOC2, HIPAA, GDPR, PCI-DSS, or anything like that?"

**Listen for (strong signals):**
- "Yes, SOC2" or "Yes, PCI-DSS" = Enterprise-grade needed
- "Yes, and entitlements are part of our audit" = TAI directly solves
- "Yes, but we're not sure about entitlements" = Opportunity to educate

**Listen for (red flags):**
- "No compliance requirements" = Less urgent (but still valuable)

**Follow-up:**
> "Are entitlements specifically mentioned in your audit scope?"

**CRM Note:** Capture compliance type and whether entitlements are audited

---

### Q8: Audit Trail Maturity

**Script:**
> "When auditors ask you about how entitlements are enforced and changed, what's your answer?"

**Listen for (GOLD SIGNAL):**
- "We don't have one" = Compliance gap, urgency HIGH
- "It's in our code/spreadsheet" = No formal trail, risk
- "Auditors flagged this as a gap" = BREAKING POINT (move to POC immediately)
- "We have logs but not cryptographically verified" = Good but incomplete

**Follow-up if needed:**
> "How long does an audit take? What's the cost?"

**CRM Note:** Capture audit trail maturity and any auditor findings

---

### Q9: Scale & Throughput

**Script:**
> "What kind of throughput do you need? How many concurrent entitlement checks per second—are we talking hundreds, thousands, ten thousand?"

**Listen for:**
- "<1,000/sec" = Moderate scale, TAI still adds value
- "1,000-10,000/sec" = TAI's sweet spot (our competitive advantage)
- ">10,000/sec" = Enterprise scale, major problem if latency is issue

**CRM Note:** Capture throughput requirement

---

### Q10: Performance Issues

**Script:**
> "Have you hit any performance issues with entitlement checks? Latency, timeouts, database locks?"

**Listen for (strong signals):**
- "Yes, frequently" = Latency is a pain point, TAI's <50ms is differentiator
- "Occasionally during peak times" = Performance matters
- "No issues" = Still valuable for audit trail, but less urgent

**Follow-up:**
> "How often does it happen? What's the customer impact?"

**CRM Note:** Capture performance pain level

---

### Q11: Time to Market (Agility)

**Script:**
> "How long does it take to launch a new pricing tier, change a quota, or modify an entitlement rule? Give me a ballpark—days, weeks, months?"

**Listen for (strong signals):**
- "Weeks" = Major blocker (TAI reduces to hours)
- "Days" = Friction (TAI still improves workflow)
- "Minutes" = Already optimized, but TAI adds audit/scale

**Follow-up:**
> "What's the bottleneck—engineering work, QA, compliance review?"

**CRM Note:** Capture time-to-market

---

### Q12: Delayed Launches

**Script:**
> "Have you had to delay product launches because entitlement logic was complex or risky?"

**Listen for (strong signals):**
- "Yes, 2-3 times" = Major business impact
- "Yes, it's happened" = Real friction point

**Listen for (red flag):**
- "No, never" = Might be less of a priority

**CRM Note:** Capture launch delay history

---

## Section 3: Business Impact & ROI (10 minutes)

### Purpose
Quantify the cost of status quo and TAI's potential value. This becomes ROI discussion.

### Q13: Annual Manual Effort (Quantify Waste)

**Script:**
> "Let's quantify this. Roughly how many hours per month does your team spend on manual SKU/entitlement management tasks? Include provisioning, troubleshooting, audits, everything."

**Listen for:**
- "20 hours/month" = Low pain
- "40-60 hours/month" = High pain (good fit)
- "100+ hours/month" = Extreme pain (must-have solution)

**Math on the fly:**
> "So if that's [X] hours per month, and your engineers are roughly $100K/year fully loaded, that's about $[Y]K per year in wasted effort. Is that roughly right?"

**CRM Note:** Capture hours/month and calculated annual cost

---

### Q14: Revenue at Risk

**Script:**
> "Shifting to the business side—how much revenue is at risk if entitlements fail or are misconfigured?"

**Listen for (breakdown):**
- Revenue leakage (customer gets service without paying)
- Customer churn from quota bugs/frustration
- Compliance fines (if applicable)
- Lost deals from entitlement issues

**Conversation:**
> "For example, if a customer accidentally got a premium tier for free, or a quota enforcement bug frustrated a big customer—what would that cost?"

**CRM Note:** Capture revenue at risk ($K)

---

### Q15: Budget Authority

**Script:**
> "Who controls the budget for tools like this? Is it Finance, Product, Engineering, CTO, or someone else?"

**Listen for:**
- "One person" = Fast decision-making
- "Multiple stakeholders" = Longer sales cycle
- "CFO needs approval" = Needs ROI justification
- "No budget allocated" = Not a fit for Q1/Q2

**Follow-up:**
> "Is there a process for unexpected tools, or does it have to wait for budget planning?"

**CRM Note:** Capture budget owner and decision-making process

---

### Q16: Procurement Timeline

**Script:**
> "What's your typical timeline from decision to go-live? Like, how long is approval → contract → implementation?"

**Listen for:**
- "Weeks" = Good fit (can close Q1)
- "Months" = Manageable (might slip to Q2)
- "Months + RFP" = Long sales cycle (prepare for 4-6 month deal)
- "2026 budget is locked" = Wait for 2027 planning

**Follow-up:**
> "Is there already 2026 budget allocated for this category?"

**CRM Note:** Capture procurement timeline and budget status

---

## Section 4: Organizational Readiness (8 minutes)

### Purpose
Understand implementation readiness, competitive landscape, and internal sponsorship.

### Q17: Implementation Readiness

**Script:**
> "From a technical standpoint, do you have an engineering team that can integrate with our API and do the deployment, or would you need professional services support?"

**Listen for:**
- "We have in-house engineers" = Faster implementation (30 days), lower cost
- "We'd need help" = Requires services engagement (60 days), higher revenue
- "We'd need to hire" = Timeline challenge

**CRM Note:** Capture implementation readiness

---

### Q18: Competitive Evaluation

**Script:**
> "Have you evaluated other autonomic governance solutions, or are you looking at TAI for the first time?"

**Listen for:**
- "No, this is new to us" = Good buying signal, early awareness
- "Yes, we looked at [X]" = Understand objections vs competitors
- "We rejected [Y] because..." = Learn from past evaluations

**Follow-up:**
> "What about [X] didn't work for you?"

**CRM Note:** Capture competitive situation

---

### Q19: Executive Sponsorship (CRITICAL)

**Script:**
> "Who's your biggest champion for solving this problem? Who's feeling the pain most acutely?"

**Listen for:**
- "Our VP of Product" = Strong sponsor, likely to move forward
- "CTO" = Technical fit, but might need finance approval
- "VP Finance" = Budget, but slower decision-making
- "Multiple people (Product + Finance + Eng)" = IDEAL (strong deal signal)
- "Nobody yet, just me exploring" = Weaker signal, will need to build consensus

**CRM Note:** Capture sponsor name, title, email

---

### Q20: Value of Freed Time (Vision)

**Script:**
> "If we could reduce your manual effort by 50% and give you a complete audit trail for entitlements, what would you do with the freed-up team time? Like, what's blocked today that you'd prioritize?"

**Listen for:**
- "We'd focus on [new feature development]" = Growth vision
- "We'd handle more regions/customers" = Scaling priority
- "We'd focus on compliance/risk work" = Risk mitigation priority
- "We'd reduce headcount" = Cost-cutting play

**Why this matters:** Shows true value beyond cost savings. Helps build ROI narrative.

**CRM Note:** Capture value proposition

---

## Closing (2 minutes)

### Script (Verbatim)

> "Based on what you've shared, here's what I'm hearing:
>
> 1. You're spending roughly [X] hours/month on manual SKU work—that's [Y] FTE at [Z] cost
> 2. Your main pain is [specific pain point they mentioned: compliance gaps / latency / time-to-market]
> 3. You've got [compliance requirement], and auditors are asking about [specific finding]
> 4. Timeline-wise, you're thinking [Q1 / Q2 / TBD]
>
> We've seen very similar challenges at [peer company name]. And honestly, I think there might be a good fit here.
>
> Would it make sense to spend 30 minutes next week with our technical team? They can walk through how we'd actually solve [main pain point] for you, no pitch—just a technical deep-dive.
>
> If not, no pressure. Either way, I'll send you a summary of what we discussed today."

### Handling Responses

**If Yes:**
> "Perfect. Let me find a time that works. Can you do Tuesday or Wednesday afternoon?"

**If "Maybe, let me check":**
> "Totally fair. Send me back some times that work and I'll coordinate with our team."

**If No:**
> "No problem at all. But I'm thinking we should stay connected. Can I check in with you in 6 months when this might be more urgent?"

---

## Post-Call Actions (5 minutes)

### Within 1 Hour
- [ ] Update CRM with all Q1-Q20 responses
- [ ] Calculate ICP Score (8 dimensions)
- [ ] Send thank you email with attachments
- [ ] Flag for next step (POC proposal, nurture, or pass)

### Thank You Email Template

```
Hi [First Name],

Really appreciated our conversation today about [Company]'s SKU management challenges.

Here's what stood out to me:
- You're managing [specific pain points mentioned]
- Your team is spending [X hours/month], which is significant
- Compliance auditors flagged [specific gap]

I've attached two things:
1. One-pager on how TAI works (for your reference)
2. ROI calculator based on what you shared (rough estimate)

For next steps: If a 30-minute technical deep-dive makes sense, I'll ping you Monday
with some times. If not, I'll stay on your radar and check back in [3-6 months].

Let me know if you have any questions!

Best,
[Your Name]
```

### CRM Entry Template

```
Contact: [Name]
Company: [Company]
Call Date: [Date]
Duration: [Min]
ICP Score: [Overall 1-10]

Key Pain Points:
- Manual effort: [X FTE/month]
- Compliance gap: [Specific finding]
- Performance issue: [Latency/scaling challenge]
- Time-to-market: [Weeks to launch tier]

Budget: [Pre-approved / Request needed / Unknown]
Timeline: [Weeks]
Sponsor: [Name/Title]
Next Step: [POC proposal / Nurture / Pass]
Follow-up Date: [Date]
```

---

## Script Refinement (Based on Feedback)

### If Prospect Pushes Back Early: "We Don't Have This Problem"

**Response (Empathetic):**
> "That's fair. Most companies don't realize they have it until they scale. But let me ask—as you grow and add more products/regions/compliance requirements, how are you planning to scale your current approach?"

### If Prospect Says: "We Have Budget But Timeline Is 2027"

**Response (Reframing):**
> "I totally understand. What if we started a lightweight conversation now—just understand your architecture, maybe do a 7-day trial on a non-prod environment—so when your 2027 budget opens, you're ready to implement immediately? No pressure or commitment."

### If Prospect Says: "Our On-Prem System Is Locked In"

**Response (Honesty):**
> "I appreciate you being straight. Right now TAI is GCP-focused, so if you're on-prem, we might not be the fit today. That said, if you ever think about cloud migration in the next year or two, I'd love to stay connected. Fair?"

### If Prospect Is Highly Technical & Skeptical

**Response (Technical Credibility):**
> "Great question. Here's how we handle that at a technical level: [specific technical detail]. I can have our CTO jump on a call next week if you want to dig into the architecture."

---

## Reminder: What NOT to Do

- ❌ Don't pitch the product (this is discovery)
- ❌ Don't jump to pricing (it's not about price yet)
- ❌ Don't take 60+ minutes (45 min max)
- ❌ Don't talk over the prospect (listen 70%, talk 30%)
- ❌ Don't promise things that aren't ready yet
- ❌ Don't send a generic follow-up (personalize it)
- ❌ Don't prospect to the wrong person (confirm they have authority/influence)

---

## Success Indicators (After Call)

**You nailed it if:**
- [ ] Prospect shared 3+ specific pain points
- [ ] Prospect gave you numbers (hours/month, $ revenue at-risk)
- [ ] You got sponsor's name + permission to involve them
- [ ] Prospect said "Yes" or "Maybe" to technical deep-dive
- [ ] CRM shows ICP Score 75+
- [ ] You sent follow-up within 1 hour with customized ROI
- [ ] You scheduled next step (deep-dive, POC proposal, or nurture)

**You can improve if:**
- [ ] Prospect said "No" to technical deep-dive (probably lower fit or wrong timing)
- [ ] Prospect said "We'll evaluate in 2027" (nurture for later)
- [ ] Prospect couldn't articulate pain quantitatively (might not be ready yet)
- [ ] CRM shows ICP Score <60 (pass to nurture list)

---

## Practice Checklist (Before Week 3 Calls)

- [ ] Read script 3 times (to internalize, not memorize)
- [ ] Practice opening with peer (get feedback on tone/credibility)
- [ ] Do a mock call with CTO (practice objection handling)
- [ ] Record yourself (listen back for clarity, pace, filler words)
- [ ] Create note sheet (one-pager with Q1-Q20 for real-time notes)
- [ ] Set up CRM template (ready to fill post-call)
- [ ] Test Zoom (camera, audio, screen share)

---

## Document Version

- **Version:** 1.0
- **Date:** January 26, 2026
- **Status:** READY FOR EXECUTION
- **Used by:** VP Sales (primary), CTO (technical calls)

