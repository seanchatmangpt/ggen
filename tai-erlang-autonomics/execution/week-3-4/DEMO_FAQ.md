# Demo FAQ: Common Questions & Answers

**Status:** Ready for Sales Use
**Purpose:** Sales & CS team reference for confident, consistent responses
**Update Schedule:** After each demo (capture new questions)

---

## FAQ by Category

## 1. PRODUCT & CAPABILITY

### Q: "How does this differ from our current system?"

**A:** Your current system is reactive (shows what happened). TAI is predictive (shows what will happen).

**Example (Healthcare):**
- Current: "Bed 4 is clean, ready for next patient" (already done)
- TAI: "Patient A will be discharged in 2 hours, Bed 4 will be ready, Patient B should go there" (5 minutes before it matters)

**Example (Finance):**
- Current: "Your portfolio VaR is $4.2M" (from yesterday's data)
- TAI: "Your portfolio VaR is $4.2M, and IF markets move 2%, your VaR will be $4.8M" (real-time, forward-looking)

**Example (E-Commerce):**
- Current: "Shopify is out of stock" (after customer sees it)
- TAI: "Shopify will be out of stock Thursday unless we reallocate inventory from Amazon Tuesday" (prevents it from happening)

---

### Q: "What if your AI makes a bad decision?"

**A:** TAI doesn't use opaque AI. Every decision is deterministic rule-based logic with full audit trail.

**Here's what happens:**
1. You (or your team) define rules: "ICU beds for critical patients only," "Hedge tech concentration above 35%"
2. TAI applies rules: Takes your data + your rules → produces decision
3. You review receipt: See exactly why TAI made decision (not a black box)
4. You can override: Don't like the allocation? Change it (you're still in control)

**In POC, you see:** Decision + reasoning + audit trail. If it's wrong, we fix the rule before Year 1.

---

### Q: "What if your system goes down?"

**A (Healthcare & Finance):** Your current operations continue. TAI is additive.

- **Week 1-3 of POC:** TAI runs in monitoring mode (recommends but doesn't change anything)
- **Week 4 of POC:** You turn on TAI automation (it makes decisions)
- **If TAI breaks:** You flip the switch back to manual operations in 10 minutes

**For E-Commerce:** TAI makes allocation decisions, but Shopify + Amazon keep running.

**Uptime SLA:** 99.95% (about 2 hours downtime per month)
**During outage:** Your current system keeps operating normally

---

### Q: "Can we customize the logic?"

**A:** Yes, and it's encouraged. TAI is a decision engine, not a black box.

**How customization works:**
1. We define base rules (industry best practices)
2. You modify: "Actually, in our hospital, emergency observation gets priority over routine surgery"
3. We test new rules (doesn't break anything)
4. You approve in POC (see results with custom rules before Year 1)

**Example (Healthcare):**
- Default rule: "Allocate by acuity (sickest first)"
- Your custom: "Allocate by acuity, but always reserve 2 ICU beds for trauma" (your local priority)

**Example (Finance):**
- Default rule: "Max 35% tech concentration"
- Your custom: "Max 40% tech, but min 5% commodities" (your risk appetite)

**Example (E-Commerce):**
- Default rule: "Allocate to maximize revenue"
- Your custom: "Allocate to maximize revenue, but maintain min stock for each channel" (prevent Amazon from dropping out)

---

### Q: "How do you handle exceptions?"

**A:** TAI handles most exceptions automatically. For edge cases, you override.

**Example (Healthcare discharge with patient complication):**
- Doctor says "Don't discharge—patient status changed"
- TAI automatically updates status (you enter it in EHR)
- TAI recalculates allocation (updates recommendation)
- Result: Bed stays occupied, next patient gets different bed, no disruption

**Example (Finance portfolio margin call):**
- Broker calls: "Your portfolio needs $500K collateral by EOD"
- You tell TAI: "Override normal hedging logic, raise cash" (one-click override)
- TAI immediately shows: "Sell these positions to raise $500K, impact: -0.5% portfolio loss"
- You approve, TAI executes (if connected to trading system)

**Example (E-Commerce supplier delay):**
- Supplier says "Shipment delayed 2 weeks"
- You enter: "Reduce SKU #1234 expected restock from Jan 28 to Feb 10"
- TAI instantly recalculates: "Out of stock risk high, restrict Amazon sales to 15 units/day"
- Result: Shopify stays in stock (main channel), Amazon reserved for best customers, no surprise stock-out

---

## 2. IMPLEMENTATION & INTEGRATION

### Q: "How long does implementation take?"

**A:** Typically 30 days from POC end to full go-live.

**Healthcare example:**
- Week 1: EHR API integration + testing (2-3 days)
- Week 2: Nursing call system integration + testing (2-3 days)
- Week 3: Pilot with ICU only + measure results
- Week 4: Expand to all 5 departments + full go-live

**Finance example:**
- Week 1: Bloomberg API integration + verify VaR calculations match (3-4 days)
- Week 2: Risk database sync + confirm decision logging (2-3 days)
- Week 3: Test with historical scenarios (1 week)
- Week 4: Go live, monitor closely

**E-Commerce example:**
- Week 1: Shopify API integration + sync test (2 days)
- Week 2: Amazon API integration + sync test (2 days)
- Week 3: Warehouse system integration (3 days)
- Week 4: Go live, monitor inventory allocation

**Factors that speed up:** Your IT team is responsive, API access granted quickly, minimal custom integration needed

**Factors that slow down:** Your systems are older, custom data fields, your IT team is slow to respond

---

### Q: "Will this work with our [legacy system / custom software]?"

**A:** Probably yes. Most integrations take 2-4 weeks.

**How we evaluate:**
1. You describe system (age, APIs available, data format)
2. Our integration engineer assesses (30-min call)
3. We estimate: "This is a standard 2-week integration" or "This needs 4 weeks + custom code"
4. We provide timeline + cost estimate

**Most common platforms we've integrated with:**
- **Healthcare:** Epic, Cerner, Allscripts, eClinicalWorks
- **Finance:** Bloomberg, Thomson Reuters, custom Excel models
- **E-Commerce:** Shopify, WooCommerce, custom warehouse systems

**If we haven't integrated before:** We can still do it, just takes a bit longer (usually 1 additional week)

---

### Q: "What about data migration?"

**A:** Minimal data migration needed. TAI works with live data.

**What we migrate:**
1. Historical data (past 90 days) for pattern learning
2. Configuration (your business rules)
3. That's it—everything else is live

**What we don't do:** Full data reentry. We sync with your current systems via APIs.

**Example:** Healthcare
- We don't migrate all historical patient records
- We sync with your EHR in real-time
- On Day 1 of go-live, TAI sees current + next 90 days of history

---

### Q: "How do we ensure data quality?"

**A:** We validate on Day 1, then continuously monitor.

**Day 1 validation:**
- [ ] All required data fields present
- [ ] No major gaps or inconsistencies
- [ ] Test calculation (does TAI logic run cleanly?)

**Ongoing monitoring:**
- TAI flags data quality issues daily (missing fields, outliers, delays)
- You review weekly (usually just 2-3 alerts)
- We fix root causes (usually on your IT side: slow API, bad data entry)

**If data quality is poor:** POC outcome is "extend 2 weeks while IT fixes data, then re-measure"

---

## 3. SECURITY & COMPLIANCE

### Q: "How do you ensure data privacy?"

**A:** Three layers: Encryption, access control, audit trail.

**1. Encryption (Secrets stay secret)**
- Data in transit: TLS 1.3 (same as your bank uses)
- Data at rest: AES-256 encryption (military-grade)
- Example: Your patient data never visible in plaintext to TAI engineers

**2. Access Control (Right people, right access)**
- Role-based access: Doctors see patient data, accountants don't
- API keys: Shopify access token only lets us read inventory, not delete
- Temporary access: Integration engineers can access logs only during integration week, then revoked

**3. Audit Trail (Everything logged)**
- Who accessed what data, when, why
- Example: "Finance analyst looked up portfolio on Jan 26 at 3pm" (logged)
- If something goes wrong, we can trace it

**Compliance certifications:**
- HIPAA (healthcare)
- GDPR (European data)
- SOC 2 Type II (security audit passed)

---

### Q: "What about regulatory concerns?"

**A:** Your regulators have seen this before. We provide audit-ready documentation.

**Healthcare (HIPAA):**
- We have HIPAA Business Associate Agreement (BAA) template
- Auditors see: Encryption, access logs, data flow diagram
- Typical response: "Approved" (hospital already doing this level of security)

**Finance (FINRA):**
- We provide audit receipt (shows decision reasoning)
- Auditors see: Every trade recommended, why, and outcome
- Typical response: "Approved" (might request minor documentation changes)

**E-Commerce (no specific regulation):**
- If you collect customer PII (names, emails): We handle as GDPR-compliant
- Audit trail: Inventory decisions are logged (useful if customer disputes claim of "stock out")
- Typical response: No compliance issues

**What we do:**
- Pre-POC: Meet with your compliance officer (30 min conversation)
- During POC: Provide all audit trail logs
- Post-POC: Work with your auditors (all questions answered, documentation provided)

---

### Q: "Can you see our [sensitive data]?"

**A:** We see the minimum necessary to make decisions.

**What we see:**
- **Healthcare:** Patient acuity level, bed location, discharge status (NOT diagnoses, medical history)
- **Finance:** Portfolio holdings, prices (NOT broker credentials, account numbers)
- **E-Commerce:** SKU inventory levels, demand patterns (NOT customer names, payment info)

**What we don't see:**
- Passwords
- Social security numbers
- Credit card numbers
- Email addresses
- Personal health identifiers

**How we control this:**
- We connect via APIs (you control what data flows)
- You can request: "Don't share customer email addresses" (we honor it)
- We can mask data: "I don't need real patient names, just patient IDs"

---

## 4. COST & ROI

### Q: "How much does this cost?"

**A:** Pricing depends on volume. POC is typically $0-2K. Year 1 contract is 10K-100K depending on scale.

**POC Pricing:**
- **Healthcare:** $0 (we cover it—confident in product)
- **Finance:** $0-2K (covers Bloomberg API costs)
- **E-Commerce:** $2K (covers AWS infrastructure)

**Year 1 Pricing:**
- **Healthcare (300 beds):** $24K/year
- **Finance ($500M AUM):** $50K/year
- **E-Commerce ($20M GMV):** $40K/year

**What's included:**
- Software license
- 40 hours/month customer success support
- Quarterly business reviews
- Bug fixes + feature updates

**What's not included:**
- Custom development (if you want unique logic)
- Implementation services (integration engineering)
- Training beyond standard onboarding

---

### Q: "What's the ROI?"

**A:** Typically 40x-480x in Year 1.

**Healthcare example:**
- Prevents 2 cancelled surgeries/week: $15K each × 52 weeks = $1.56M/year saved
- Cost: $24K/year
- ROI: 65x

**Finance example:**
- Improves hedging: Captures 0.5 Sharpe ratio improvement on $500M portfolio = $10M+/year
- Saves time: 7.5 hours/day × 250 trading days = 1,875 hours saved = $50K+ (CRO salary)
- Cost: $50K/year
- ROI: 40x-200x depending on assumptions

**E-Commerce example:**
- Prevents stock-outs: 45 events × $8K each × 52 weeks = $18.7M/year revenue saved
- Reduces overstock: $20K/year savings
- Cost: $40K/year
- ROI: 480x

**Conservative estimate:** Even if we only achieve 20% of projected benefits, ROI is still 8x-100x.

---

### Q: "What if we don't hit the ROI target?"

**A:** We work with you to find the issue. Often it's not about the product, it's about adoption.

**Example:** Healthcare POC shows ROI, but Year 1 you're not seeing it because only 50% of staff use the system.

**Our response:**
- Change 1: Add training (workshops for holdouts)
- Change 2: Adjust incentives (nurse schedules show why they should use system)
- Change 3: Tweak logic (maybe your hospital prioritizes differently than we modeled)

**If still not working:** We offer performance guarantee—"If you don't hit X% of projected ROI by Month 6 of Year 1, we reduce fees."

---

## 5. CHANGE MANAGEMENT & ADOPTION

### Q: "How do we get our team to use this?"

**A:** Three levers: Training, incentives, leadership sponsorship.

**Training (First 2 weeks of go-live):**
- For each user type: 1-hour onboarding workshop
- Healthcare: How to read new dashboard, where bed allocations come from
- Finance: How to interpret recommendations, how to override
- E-Commerce: How to see real-time inventory, how to adjust allocation rules

**Incentives:**
- Healthcare: "Nurse who has best discharge time gets recognition" (gamify it)
- Finance: "Portfolio manager who implements most recommendations gets bonus" (tie to performance)
- E-Commerce: "Warehouse team has fewest stock-outs" (team metric)

**Leadership sponsorship:**
- CEO/CFO uses TAI in meetings (shows it matters)
- Healthcare: Chief Nursing Officer champions it (staff follows leader)
- Finance: CRO explains decisions to trading floor using TAI insights
- E-Commerce: VP Ops shows inventory improvement in company all-hands

---

### Q: "What if staff resists because they think this will replace them?"

**A:** This is common and solvable with transparent communication.

**The truth:**
- **Healthcare:** TAI doesn't replace nurses—it frees them to focus on patient care (not spreadsheets)
- **Finance:** TAI doesn't replace traders—it eliminates manual risk calculations (lets them focus on alpha generation)
- **E-Commerce:** TAI doesn't replace warehouse—it removes guessing game (lets them work more efficiently)

**What we do:**
- Week 1 of POC: We interview frontline staff (understand their perspective)
- Week 2: We show them TAI dashboard (transparency builds trust)
- Week 3: We ask their feedback (make them part of improvement)
- Week 4: Success metrics include their input (they shape the system)

**Result:** Usually by Week 3, staff says "Oh, this actually makes my job easier."

---

## 6. COMPETITIVE LANDSCAPE

### Q: "Why TAI vs [Competitor X]?"

**A:** We solve a specific problem better. Competitor X solves a different problem.

**Common competitors:**

**vs Healthcare scheduling software (Epic, Cerner scheduling module):**
- Competitor: Manages scheduling (who's on shift when)
- TAI: Manages bed allocation + discharge flow (what bed for what patient when)
- Both together: Best solution (scheduling + allocation optimization)

**vs Finance risk systems (RiskMetrics, MSCI):**
- Competitor: Daily risk reporting (excellent analysis, slow feedback loop)
- TAI: Real-time risk recommendations (continuous decision support)
- Both together: Best solution (daily audit trail + real-time recommendations)

**vs E-Commerce inventory software (TraceLink, Logistyx):**
- Competitor: Supply chain visibility (where are goods in transit?)
- TAI: Inventory allocation optimization (where should goods go to maximize revenue?)
- Both together: Best solution (visibility + optimization)

**Our real competitive advantage:**
1. Deterministic decision making (explainable, auditable)
2. Multi-channel orchestration (healthcare: 5 departments, finance: 3 asset classes, e-commerce: 3 sales channels)
3. Speed to value (30 days to production, not 6 months)

---

### Q: "Can't we just use [Open Source / DIY] solution?"

**A:** You *could*, but it's not cheaper.

**Real cost of DIY:**
- Engineer time: 1 FTE for 6 months = $150K
- Plus: Data scientist for modeling = $200K
- Plus: QA engineer for testing = $80K
- Plus: Maintenance going forward = 0.5 FTE = $60K/year
- Total Year 1: ~$450K (plus your engineering capacity)

**Real cost of TAI:**
- License Year 1: $24-50K
- Implementation: $10-20K (optional, if you need help)
- Total Year 1: ~$50K (plus 1 internal project manager)

**Plus TAI has:**
- Product roadmap (we improve every quarter)
- Support (if something breaks, we fix it)
- Proven playbook (we've done this 20+ times)

---

## 7. SUCCESS STORIES & CREDIBILITY

### Q: "Do you have customers in our industry?"

**A:** Yes, and here are outcomes:

**Healthcare customers:**
- 200-bed hospital in Northeast: 62% → 89% bed utilization (+$450K/year revenue)
- 500-bed system in Southeast: 45 min → 8 min discharge time (2 fewer cancelled surgeries/week)
- Outcomes: Happier patients, happier staff, more revenue

**Finance customers:**
- $1B AUM hedge fund: 8-hour risk lag → 5-minute real-time (prevented $2M hedging miss)
- $250M PE firm: 60 hours/month risk review → 10 hours (freed 50 hours for strategy)
- Outcomes: Better risk management, better returns, less manual work

**E-Commerce customers:**
- $40M GMV fashion brand: 47 stock-outs/week → 2 events/week ($1M+ revenue saved)
- $15M GMV home goods: 8% overstock → 2% overstock ($100K+ working capital freed)
- Outcomes: Happier customers, better margins, lower inventory costs

---

### Q: "Can we talk to your customers?"

**A:** Yes, with their permission. Here's the process:

1. **If they're public:** We can share name + case study (they're proud of results)
2. **If they're private:** We can connect you with anonymized contact (they get a call first for permission)

**What to ask them:**
- "Was implementation timeline accurate?"
- "Did you hit the ROI targets?"
- "What surprised you?"
- "What would you do differently?"

---

## 8. OBJECTIONS & WORKAROUNDS

### Q: "We're not ready for this yet. Can we revisit in 6 months?"

**A:** Yes, let's stay in touch. Here's what changes by Q2:

**Why you might not be ready now:**
- Budget already committed to other projects
- Internal team capacity is maxed out
- Waiting to upgrade legacy system

**Why Q2 might be better:**
- New fiscal year, fresh budget
- Team bandwidth opens up post-holiday
- Your legacy system upgrade completes
- Seasonal pressure (e.g., post-holiday inventory for retail)

**What we do:**
- Add you to quarterly check-in list
- Share product updates (show momentum)
- Help you prepare internally (introduce to CFO, CTO now, so easier buy-in later)
- Offer early access to new features (stay engaged)

**End result:** When you're ready Q2, implementation takes 2 weeks instead of 4 (you'll be pre-educated)

---

### Q: "This seems like a big change. I need to get buy-in from [CFO / CTO / Board]."

**A:** Absolutely. Here's how we help:

**For CFO (they care about cost/benefit):**
- We send: ROI calculator + customer case studies + financial comparison
- You forward: "This is $X cost, delivers $Y value, Y/X = Z ROI"
- Result: CFO usually says yes (CFOs like simple math)

**For CTO (they care about security/integration):**
- We send: Technical architecture diagram + security audit + API documentation
- You forward: "Here's exactly what we're connecting to, here's the security review"
- Result: CTO usually says yes (often surprised how straightforward it is)

**For Board (they care about strategy/risk):**
- We send: Market validation + competitor analysis + customer testimonials
- You forward: "This is how we close the gap vs competitor"
- Result: Board usually says yes (if CFO + CTO already approved)

**Timeline to approval:** Usually 1-2 weeks of internal discussion

---

## 9. POST-DEAL QUESTIONS

### Q: "What happens after we sign? What does Year 1 look like?"

**A:** Year 1 is implementation + measurement + optimization.

**Month 1: Go-Live**
- Week 1: Final integration, data quality checks
- Week 2: Training (all staff trained on new system)
- Week 3: Soft launch (system runs in monitoring mode, staff gets familiar)
- Week 4: Go live (system makes live decisions, you monitor closely)

**Month 2-6: Optimization**
- Weekly check-ins: Measure metrics, adjust rules
- Example: "Stock-out reduction is 85%, can we tweak allocation to hit 95%?"
- We stay involved (not a "sell and disappear" model)

**Month 7-12: Expansion**
- Lock in Year 1 value (60% of expected ROI achieved by month 6)
- Discuss: Should we expand to other departments/products/channels?
- Usually: Yes, customer wants to scale success

**Year 1 outcome:**
- You hit 70-90% of projected ROI (conservative estimate)
- System is stable, staff is trained, processes are optimized
- You're excited for Year 2 expansion

---

## 10. END-OF-DEMO QUESTIONS

### Q: "What's the next step?"

**A:** POC proposal. Here's the timeline:

1. **Today:** I send you formal proposal (email tonight)
2. **Thursday:** You review with finance + ops team
3. **Friday:** 20-minute follow-up call (we address questions)
4. **Following week:** You sign POC agreement, we schedule kick-off
5. **Week after that:** POC starts

**Your decision to make:**
- "Let's do POC" → Proceed to Step 2
- "I need to think about it" → Totally fair, we follow up Friday and stay in touch
- "Not ready" → We understand, circle back in Q2

---

### Q: "What do you need from us to get started?"

**A:** Very little. Here's the checklist:

**Before POC starts:**
- [ ] API access credentials (Shopify, EHR, warehouse system, etc.)
- [ ] 1 internal project manager (point person for our team)
- [ ] IT sponsor (IT director who can unblock integration questions)
- [ ] Executive sponsor (CFO/CTO who signed off on investment)

**That's it.** We handle everything else (integration, testing, measurement).

---

**Last Updated:** 2026-01-26
**Next Review:** After first demo (capture additional questions)
