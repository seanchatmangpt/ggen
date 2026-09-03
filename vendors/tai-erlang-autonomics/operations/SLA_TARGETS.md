# TAI Erlang Autonomics - SLA Targets & Service Level Agreements

**Version:** 1.0.0
**Date:** January 25, 2026
**Scope:** Support response times, resolution times, and system availability targets
**Audience:** Legal, Sales, Finance, Operations, Engineering

---

## EXECUTIVE SUMMARY

TAI's SLA framework balances customer expectations with operational reality. We commit to:

- **Starter Tier (MVP):** 24-hour response, no guaranteed resolution time
- **Professional Tier:** 4-hour response, 48-hour resolution for most issues
- **Enterprise Tier:** 1-hour response, 8-hour resolution for critical issues

Availability targets: 99.5% (Starter) → 99.9% (Enterprise).

---

## SECTION 1: SERVICE AVAILABILITY SLAs

### 1.1 System Uptime Guarantees

#### By Tier

| Tier | Monthly Uptime Target | Downtime Allowed | Service Credit |
|------|----------------------|------------------|-----------------|
| **Starter** | 99.5% | ~22 minutes/month | 10% of month fee |
| **Professional** | 99.9% | ~4.3 minutes/month | 15% of month fee |
| **Enterprise** | 99.95% | ~2.2 minutes/month | 25% of month fee |

#### What "Uptime" Means

**Included in uptime calculation:**
- Core API endpoints responding with HTTP 200
- Dashboard accessible and functional
- Webhooks delivering to customer endpoints
- Data processing pipeline completing transactions

**Excluded from uptime calculation:**
- Scheduled maintenance (announced 7 days in advance)
- Customer misconfiguration (401 errors caused by invalid API keys)
- DDoS attacks or malicious third-party activity
- Customer's network outage (cannot reach our API)

#### Measurement & Verification

- **Monitoring:** Synthetic monitoring every 10 seconds from 3 global regions
- **Reporting:** Monthly uptime report published by 5th of following month
- **Verification:** Customer can verify uptime on status.tai.ai
- **Disputes:** Report disputes within 30 days of incident

### 1.2 Incident Management & Communication

#### Severity Levels & Communication SLA

| Severity | Description | Internal Page | Customer Notification | Update Frequency |
|----------|-------------|---------------|-----------------------|------------------|
| **P1: Critical** | TAI platform down, all customers affected | Immediate (Slack) | Within 5 min via status page | Every 15 minutes |
| **P2: Major** | Major feature unavailable or severe degradation (>50% customers affected) | Within 5 min | Within 15 min via status page | Every 30 minutes |
| **P3: Significant** | Specific feature degraded or single customer unable to use product | Within 15 min | Within 1 hour via email | Every 2 hours |
| **P4: Minor** | Cosmetic issue, documentation gap, or workaround available | Within 1 hour | No notification required | On resolution |

#### Communication Channels During Incidents

1. **Immediate (< 5 min):** Update status.tai.ai with incident description
2. **Ongoing:** Update status page every 15-30 minutes with progress
3. **Resolution:** Final status post-mortem and action items within 24 hours
4. **Enterprise Only:** Phone call to designated escalation contact for P1/P2 incidents

#### Post-Incident Reporting

Within 24 hours of incident resolution:
- Root cause summary (1 paragraph)
- Timeline of events (minute-by-minute)
- Why it happened (technical details)
- What we're doing to prevent it (fixes + testing)
- Credit issued (if applicable)

Example:
```
INCIDENT: API Latency Spike on 2026-01-15
Duration: 23 minutes (14:47 - 15:10 UTC)
Severity: P2 (Major)
Affected: ~60% of customers (those in us-west region)
Root Cause: Database connection pool exhaustion due to buggy query consuming connections
Timeline:
  14:47 - Synthetic monitoring detects p99 latency spike to 2.3s
  14:52 - On-call engineer paged, starts investigation
  14:58 - Root cause identified (bad query in new code from 2 hours prior)
  15:03 - Buggy code rolled back
  15:10 - Latency returns to normal (<100ms p99)
Prevention:
  - Added query performance testing to CI/CD pipeline
  - Code review checklist now includes: "Did you test with 10K+ transactions?"
  - Automated alerting on connection pool utilization >80%
Service Credits:
  - Affected customers: 10% credit for 1 month (proactive, not requested)
  - Calculated: $[X] credit applied to accounts
```

---

## SECTION 2: SUPPORT RESPONSE SLAs

### 2.1 Response SLA Matrix

**Response SLA = Time from ticket submission to first human response (not resolution)**

#### Starter Tier ($2,500/month, $30K/year)

| Severity | Response SLA | Resolution Target | Priority Queue |
|----------|--------------|-------------------|-----------------|
| **CRITICAL** | 24 hours | 5 business days | Yes (top) |
| **HIGH** | 8 hours | 2 business days | Yes |
| **MEDIUM** | 24 hours | 7 business days | Standard |
| **LOW** | 48 hours | None (best effort) | Standard |

**Achievement Target:** 90% of tickets meet response SLA

**Rationale:**
- Starter customers pay $2.5K/month = ~$200/month per ticket is our cost allowance
- Async (email) is acceptable; same-day response not guaranteed
- 24-hour response means we pick it up in next business day
- 5-day resolution is reasonable for non-critical issues at this tier

#### Professional Tier ($15,000/month, $180K/year)

| Severity | Response SLA | Resolution Target | Priority Queue |
|----------|--------------|-------------------|-----------------|
| **CRITICAL** | 4 hours | 2 business days | Yes (top) |
| **HIGH** | 2 hours | 8 business hours | Yes |
| **MEDIUM** | 4 hours | 1 business day | Yes |
| **LOW** | 24 hours | 3 business days | Standard |

**Achievement Target:** 95% of tickets meet response SLA

**Rationale:**
- Professional customers pay $15K/month = ~$1.2K per ticket cost allowance
- Real-time Slack response is expected for HIGH/CRITICAL
- 2-hour response bridges evening submissions to next morning
- Faster resolution timeline justified by ACV

#### Enterprise Tier ($75,000+/month, $900K+/year)

| Severity | Response SLA | Resolution Target | Priority Queue | Escalation Path |
|----------|--------------|-------------------|------------------|-----------------|
| **CRITICAL** | 1 hour (phone) | 8 hours | Top (paged) | VP Product + CEO |
| **HIGH** | 30 minutes (Slack) | 2 hours | Top (paged) | Engineering + Product |
| **MEDIUM** | 1 hour (Slack) | 4 hours | Top | Support + Product |
| **LOW** | 4 hours (Slack) | 24 hours | Standard | Support |

**Achievement Target:** 99% of tickets meet response SLA

**Rationale:**
- Enterprise customers pay $75K+/month = $6K+ per ticket cost allowance
- Phone response for CRITICAL justified: customer loss is $180K+
- Paging on-call engineer for same-day resolution
- Dedicated CSM ensures continuity and relationship

### 2.2 Definition of "Response" vs "Resolution"

**Response (What counts):**
- Initial human acknowledgment of ticket
- Either a solution is provided, OR
- Investigation is confirmed ("looking into this, will get back within X hours")
- Auto-generated responses do NOT count (only human responses)

**Resolution (What counts):**
- Customer's issue is fixed, OR
- Workaround is provided and customer confirms acceptance, OR
- Feature request is logged and customer is notified they'll be added to roadmap, OR
- Customer confirms they don't need further help

### 2.3 Severity Classification Guide

**CRITICAL:** TAI is completely unavailable for customer or feature is entirely broken

**Example Critical Issues:**
- "My API requests are returning 500 errors for all calls"
- "The webhook endpoint isn't delivering any events"
- "I can't log into my dashboard"
- "My governance rules aren't being executed"
- "My billing shows $0 for the month when it should show $50K"

**HIGH:** Feature is significantly degraded or customer is losing money

**Example High Issues:**
- "API latency is >1 second, it's normally <100ms"
- "Dashboard is slow, taking 10 seconds to load"
- "Some of my webhook events are missing"
- "Custom rule I created isn't working correctly"
- "My cost calculations seem wrong"

**MEDIUM:** Feature is unavailable but workaround exists

**Example Medium Issues:**
- "I can't filter by date in the dashboard, but I can export and filter in Excel"
- "The documentation is unclear on how to use feature X"
- "I need to increase my SKU limit"
- "I want to change my billing method"

**LOW:** Cosmetic or feature request

**Example Low Issues:**
- "The button color looks ugly"
- "Can you add a feature to do X?"
- "Your documentation has a typo on page 5"
- "The error message could be clearer"

---

## SECTION 3: SUPPORT RESPONSE TIME COMMITMENTS

### 3.1 Working Hours Definition

**Standard Business Hours:**
- Monday - Friday, 9 AM - 5 PM Pacific Time (PT)
- Not including US Federal holidays

**Extended Hours (Professional & Enterprise Tiers):**
- Monday - Friday, 9 AM - 6 PM PT
- Limited weekend coverage (9 AM - 5 PM PT) for critical issues only

**24/7 Coverage (Enterprise Tier Only):**
- Phone line available 24/7 for CRITICAL issues
- May route to on-call engineer in any timezone
- Response time: 1 hour for critical escalation at any time

### 3.2 SLA Response Clocks

**Clock Starts:** When ticket is submitted to support system (email received, Slack message sent, etc.)

**Clock Stops:**
- First human response is sent to customer
- Even if response is "We're looking into this, will get back within X hours"

**Clock Pauses (if applicable):**
- If we need more information from customer and ask ("Can you provide your API key so we can debug?"), clock pauses
- Clock resumes when customer provides info
- Never pause for >3 days without notifying customer

**Examples:**

```
Scenario 1: Simple Question (Starter Tier)
├─ 10:00 AM: Customer emails support@tai.ai
├─ 2:00 PM: Support replies with answer (4-hour response)
├─ Result: ✓ Meets 8-hour SLA

Scenario 2: Needs Investigation (Professional Tier)
├─ 3:45 PM (Tuesday): Customer opens support ticket
├─ 11:30 PM (Tuesday): Support replies "Looking into this"
├─ 9:00 AM (Wednesday): Support provides diagnosis and fix
├─ Result: ✓ 4-hour response SLA met (first reply Tuesday 11:30 PM)

Scenario 3: Needs Customer Info (Enterprise Tier)
├─ 2:00 PM (Thursday): Customer reports critical API error
├─ 2:45 PM (Thursday): Support responds "Need more info" (45 min response SLA met)
├─ 6:00 PM (Thursday): Customer replies with details
├─ 6:30 PM (Thursday): Support provides fix
├─ Result: ✓ 1-hour response SLA met, but resolution took 4.5 hours
```

### 3.3 Escalation & Re-prioritization Rules

**Auto-escalate if SLA will be missed:**

```
If HIGH severity ticket hasn't been touched by 1 hour 45 min:
└─ Slack notification: "Professional tier ticket approaching SLA miss"
   └─ Alert assigned support engineer
   └─ If not picked up in 15 min, escalate to support manager
   └─ If still not picked up, page on-call support engineer

If CRITICAL ticket hasn't been touched by 50 minutes:
└─ Immediate Slack notification to #support-critical
   └─ Alert all available support staff
   └─ Page on-call engineer
   └─ Alert VP Product
   └─ This is a code red situation
```

**How to Handle Overload:**

If we cannot meet SLAs due to volume:
1. Reduce scope (defer LOW tickets, focus on CRITICAL/HIGH)
2. Hire temporary contractor (24-hour hiring process)
3. Pull product engineers to support (every engineer handles support 1 week/quarter)
4. Offer paid expedited support option

We do NOT miss SLAs. If we can't staff it, we hire faster.

---

## SECTION 4: AVAILABILITY & INCIDENT SLA DETAILS

### 4.1 Scheduled Maintenance Windows

**Allowed Maintenance Windows (Not Counted as Downtime):**
- Sunday 2:00 AM - 4:00 AM PT (2-hour window)
- 1 additional window per month with 7-day advance notice (up to 4 hours)

**Advance Notice Required:**
- 7 days minimum notice for scheduled maintenance
- Announcement on status.tai.ai + email to all affected customers
- Maintenance window communicated in contract SLA section

**During Maintenance:**
- API is unavailable (HTTP 503 returned)
- Dashboard is read-only or offline
- Webhooks are paused (resume after maintenance)
- SLA clock does not count maintenance downtime

### 4.2 Unplanned Downtime & Service Credits

**Service Credit Calculation (Automatic, No Claim Required):**

If monthly uptime falls below SLA target:

```
Uptime | Service Credit (% of Monthly Charge)
--------|----------------------------------
99.5% - 99.0% (Starter) | 10%
99.0% - 98.5% | 25%
<98.5% | 50%

Example: Starter customer, $2,500/month charge
If uptime = 99.2% (below 99.5% target)
Credit = $2,500 × 10% = $250 credit
Applied to next month's invoice automatically
```

**Enterprise Tier Service Credits:**
- 99.9% - 99.5% uptime: 15% credit
- 99.5% - 99.0% uptime: 25% credit
- <99.0% uptime: 50% credit + incident review meeting

**Non-Transferable:**
- Credits are applied to next month's invoice only
- Cannot be transferred to another customer
- Cannot be combined with other promotions/discounts
- Expire if customer churns (cannot cash out)

---

## SECTION 5: SUPPORT CHANNEL SLAs

### 5.1 Email Support SLAs

**Response Time:** Per severity matrix above

**Email Handling:**
- Dedicated support inbox monitored 9 AM - 6 PM PT
- Daily summary email sent to unresponded tickets at 5 PM PT
- Weekend: Emails answered Monday morning (may exceed SLA if Friday 5 PM submission)

**Email Limits:**
- Max 5 email threads per ticket (escalate to phone if more back-and-forth needed)
- Use Slack if response urgency is high (email not recommended for CRITICAL)

### 5.2 Slack Support SLAs

**Response Time:** Per severity matrix above (typically faster than email)

**Slack Handling:**
- Dedicated @support-team Slack channel monitored 9 AM - 6 PM PT
- Slack notifications enabled (mention @support-oncall for urgent)
- Use threads to keep conversations organized
- Pin important information in thread

**Slack Etiquette:**
- Customer creates ticket via support form OR messages @support
- Support responds in thread with investigation
- Use emoji reactions to show progress (👀 investigating, ✅ resolved, ❓ waiting on customer)
- Don't let conversations go stale (send update every 4 hours minimum)

### 5.3 Phone Support SLAs (Enterprise Only)

**Response Time:** 1 hour for CRITICAL issues, 24-hour schedule

**Phone Availability:**
- Direct phone line: +1-844-TAI-TECH (844-824-8324)
- Answer time: <30 seconds
- Voicemail: "Our support team will call you back within 1 hour"
- No hold queue (callback option available)

**Phone Escalation Path:**
1. Tier 1 Support: Answer phone, triage, resolve if possible
2. Tier 2 Escalation: If needs investigation, page support engineer
3. Tier 3 Escalation: If customer needs VP Product involvement, page VP

---

## SECTION 6: WHAT'S NOT COVERED BY SLA

### 6.1 Excluded Services

The following do NOT have guaranteed SLAs:

1. **Feature Requests:** No response time guaranteed; logged for roadmap consideration
2. **Custom Development/Integration:** Quoted separately; handled by Solutions Engineering (48-hour quote response)
3. **Professional Services:** Billed hourly; scheduled independently
4. **On-Premises Deployments:** Custom support arrangement required
5. **Proof-of-Concepts (POCs):** Handled outside SLA; timing determined ad-hoc
6. **Pre-Sales Demos:** Handled by Sales team, not Support (different SLA)

### 6.2 Customer Responsibility Exclusions

We do NOT guarantee response for issues caused by:

1. **Misconfiguration:**
   - "I entered wrong API key" → 401 error (customer's fault)
   - "I'm sending malformed JSON" → 400 error (customer's fault)
   - Solution: Return error documentation link; does not count against SLA if we explain correctly

2. **Abuse:**
   - "I'm sending 1,000 req/s when limit is 10" → 429 rate limit (expected)
   - "I'm storing 100GB of data in free tier" → Quota exceeded (expected)
   - Solution: Offer upgrade or guidance; does not count against SLA

3. **Customer Network Issues:**
   - "Your API is down but only in my office" → Likely customer's firewall
   - "I can't connect from 203.0.113.0" → IP whitelisting issue
   - Solution: Troubleshooting guide; does not count against SLA

4. **Third-Party Issues:**
   - "Stripe isn't processing my payments" → Stripe issue, not TAI
   - "My webhook endpoint is down" → Customer's endpoint, not our delivery
   - Solution: Help debug but not responsible for third-party uptime

5. **Using Deprecated APIs:**
   - Customer using API we sunset 6 months ago
   - Solution: Provide migration guide but no emergency support

---

## SECTION 7: SLA MEASUREMENT & REPORTING

### 7.1 SLA Dashboard (Public)

**Published at:** status.tai.ai

Shows in real-time:
- Current system status (green/yellow/red)
- Uptime percentage for current month
- Monthly uptime history (rolling 12 months)
- Upcoming maintenance windows
- Incident timeline and root causes

### 7.2 Monthly SLA Report (Customer-Visible)

**Published by:** 5th of following month
**Format:** Automatic email to all customers + visible in dashboard

```
TAI SERVICE LEVEL AGREEMENT REPORT
Month: January 2026
Report Generated: February 5, 2026

YOUR ACCOUNT: [Customer Name]
Tier: [Professional / Enterprise / Starter]

AVAILABILITY METRICS
└─ Your SLA Target: 99.9%
└─ Actual Uptime: 99.92% ✓ (EXCEEDS SLA)
└─ Downtime This Month: 3.5 minutes
└─ Total Availability Hours: 743 hours 56 minutes

UPTIME INCIDENTS
├─ Jan 15, 2:15 AM PT: Database migration (23 minutes)
│  └─ Root cause: Primary DB lock during failover
│  └─ Fix: Added pre-flight validation checks
│
└─ Jan 28, 4:30 PM PT: Webhook delivery delay (5 minutes)
   └─ Root cause: Queue backlog from traffic spike
   └─ Fix: Increased queue capacity

SERVICE CREDITS
└─ You are entitled to: $0 (uptime exceeded SLA)

RESPONSE TIME PERFORMANCE
Avg Response Time: 1.2 hours (SLA: 4 hours)
Avg Resolution Time: 8.5 hours (SLA: 48 hours)
% SLA Met: 100%

CUSTOMER SATISFACTION
Avg CSAT Score: 8.4 / 10.0
NPS Score: 45
Comment: Thank you for choosing TAI!
```

### 7.3 SLA Achievement Tracking

**Internal Dashboard (Support Team Only):**

```
SLA ACHIEVEMENT - Current Month

Tier         | Response SLA | % Met | Resolution SLA | % Met | Notes
-------------|--------------|-------|-----------------|-------|----------
Starter      | 90%          | 92%   | 95%            | 94%   | ✓ Exceeds
Professional | 95%          | 96%   | 95%            | 92%   | ✓ Good
Enterprise   | 99%          | 100%  | 99%            | 98%   | ✓ Exceeds

SEVERE MISS: Enterprise response SLA
├─ Ticket #1847: Missed by 18 minutes
├─ Root cause: Support engineer on leave, backup not assigned
├─ Action: Update on-call scheduling process

TREND: ↑ Improving month-over-month
```

### 7.4 SLA Dispute Resolution

If customer disputes SLA compliance:

1. **Acknowledgment (Within 24 hours):**
   - Review ticket timestamps in our system
   - Check email delivery logs + Slack timestamps
   - Verify actual response time

2. **Response (Within 48 hours):**
   - Provide detailed timeline showing when SLA was met or missed
   - If missed: Explain why and issue service credit (even if disputed)
   - If met: Show evidence of response timestamp

3. **No Further Dispute:**
   - SLA disputes must be filed within 30 days of incident
   - After 30 days, determination is final
   - Service credits expire 60 days after issuance if not applied

---

## SECTION 8: SPECIAL CASES & EXCEPTIONS

### 8.1 Emergency Overrides

**When We Can Override SLA Response Time:**

1. **Customer Initiated Delay:** Customer asks us to wait ("Please respond tomorrow instead")
2. **Information Gathering:** We ask for more info and wait for response (clock pauses)
3. **Force Majeure:** Natural disaster, terrorism, war, act of God (extremely rare)
4. **Infrastructure Provider Issue:** AWS/GCP entire region down (still best-effort response)

**When We CANNOT Override:**
- Support team is busy (hire more staff)
- Customer is in "low priority" tier (still must meet SLA)
- Issue is from 3rd party (still must communicate effort)
- We made a mistake (still must meet SLA, escalate internally)

### 8.2 Enterprise Custom SLAs

**For Enterprise customers with special requirements:**
- Custom SLA must be documented in contract (MSA)
- Can be higher (e.g., "30-minute response for HIGH") or lower
- Must be financially justified (premium support = higher cost)
- Must be reviewed quarterly by VP Customer Success

**Example Enterprise SLA Addendum:**
```
COMPANY: Fortune 500 Financial Services Firm
CUSTOM SLA FOR ACCOUNT:

CRITICAL Issues: 30-minute response (vs standard 1 hour)
Cost: Additional $50K/year
Justification: Our system handles critical payment processing

HIGH Issues: 15-minute response (vs standard 30 minutes)
Cost: Additional $50K/year
Justification: Business impact on daily operations

RESOLUTION: Dedicated support engineer on-site 2 days/week
Cost: Additional $100K/year
Justification: Onboarding + training priority

Total Premium Support: +$200K/year
```

### 8.3 Seasonal Considerations

**During High-Load Periods (Nov-Dec for retail):**
- May adjust hiring/staffing to maintain SLA
- SLA targets remain the same (no reduction)
- Communicate proactively if approaching limits

**During Holidays (Dec 24 - Jan 2):**
- Starter/Professional: May extend response time by 24h (notify in advance)
- Enterprise: Maintain SLA via on-call rotation
- Maintenance windows: Avoid unless critical

---

## SECTION 9: SLA FINANCIAL TERMS

### 9.1 Service Credit Policy

**Service Credits:**
- Applied automatically (customer does NOT need to request)
- Applied to next month's invoice (not cash refund)
- Calculated based on actual downtime that month
- Reported in monthly SLA report

**Service Credit Limitations:**
- Maximum monthly credit: 50% of monthly charge (one month per tier)
- Non-transferable between customers
- Non-cumulative (credits do not roll over months)
- Expires 60 days after issuance if not applied
- Cannot be combined with other discounts or promotions

**Example:**
```
Customer: SmallMart (Professional Tier, $15K/month)
Incident: Feb 5 outage, 52 minutes downtime
Monthly Uptime: 99.76% (Target: 99.9%)
Downtime Below SLA: 52 minutes (over 43,200 minutes in month)
Credit Calculation: 52 minutes / 43,200 minutes = 0.12%
Charge = $15,000
Credit = 15% (applies to HIGH severity) = $2,250
Applied to March invoice: March charge = $15,000 - $2,250 = $12,750
```

### 9.2 SLA Remedies (What We Offer)

**For Service Outages:**
- Service credits (automatic)
- Root cause analysis and prevention plan
- Executive briefing (for Enterprise customers)
- Proactive monitoring & alerts (increased)

**For Support SLA Misses:**
- Expedited resolution (prioritize remaining work)
- Escalation to VP Product
- Service credit (if pattern of misses)

**What We Do NOT Offer:**
- Cash refunds (credits only)
- Penalty fees (from customer; reversed automatically)
- Extra features (cannot be bought with service credits)
- Compensation for downstream business impact

### 9.3 Multi-Year Agreements & SLA Locks

**For customers with 2+ year contracts:**
- SLA targets locked in for contract duration
- Cannot be reduced mid-term
- Can be increased at renewal only
- Pricing adjustments do NOT affect SLA targets

**Example:**
```
Customer signs 2-year Enterprise agreement (Jan 2026 - Dec 2027)
└─ SLA locked: 99.95% uptime, 1-hour response for CRITICAL
└─ If we improve infrastructure in year 2: SLA still 99.95% (not reduced)
└─ Renewal (Jan 2028): Can propose new SLA targets then
```

---

## SECTION 10: SLA CONTINUOUS IMPROVEMENT

### 10.1 Monthly SLA Reviews

**Every month, support team reviews:**
1. Which SLAs were missed (if any)
2. Root cause of each miss
3. Actions taken to prevent recurrence
4. Trend analysis (improving or degrading?)

### 10.2 Quarterly SLA Target Adjustment

**Each quarter, assess whether to adjust targets:**

```
If Achievement > 105% of target for 3 consecutive months:
├─ Consider raising target (we have headroom)
├─ Example: If hitting 100.5% response consistently, raise target to 95%
└─ More aggressive targets = more value to customers

If Achievement < 90% of target for 2 consecutive months:
├─ Do NOT lower targets
├─ Instead: Hire more staff or improve processes
├─ Root cause analysis required
└─ Action plan due within 2 weeks
```

### 10.3 Customer Advisory Board SLA Input

**Quarterly, ask top Enterprise customers:**
1. Are our SLAs still relevant?
2. Would you prefer different targets?
3. Any changes to your usage that affect SLA needs?
4. Are we consistently beating SLAs? (Maybe raise targets)

---

## APPENDIX A: SLA DEFINITIONS QUICK REFERENCE

| Term | Definition |
|------|-----------|
| **Response SLA** | Time from ticket submission to first human acknowledgment |
| **Resolution SLA** | Time from ticket submission to customer-facing fix |
| **Uptime** | % of time API responds with <500ms latency, <1% errors |
| **Downtime** | Any period when uptime drops below target (after maintenance windows excluded) |
| **Service Credit** | % discount on monthly bill, applied automatically |
| **CRITICAL** | Product unusable, customer losing money |
| **HIGH** | Feature degraded or significant impact |
| **MEDIUM** | Feature unavailable, workaround exists |
| **LOW** | Cosmetic or documentation issue |
| **Force Majeure** | Natural disaster, war, act of God (excludes SLA) |

---

## APPENDIX B: SLA TEMPLATE FOR CONTRACTS

**Insert into Master Service Agreement (Section 7: Service Levels):**

```
7. SERVICE LEVELS

7.1 Availability SLA
TAI will maintain the following monthly uptime:
- Starter Tier: 99.5%
- Professional Tier: 99.9%
- Enterprise Tier: 99.95%

Downtime excludes scheduled maintenance (2-4 hours/month, announced 7 days in advance).

7.2 Support Response SLA
TAI will respond to support tickets within:
- CRITICAL: [See SLA matrix above]
- HIGH: [See SLA matrix above]
- MEDIUM: [See SLA matrix above]
- LOW: [See SLA matrix above]

7.3 Service Credits
If monthly uptime falls below SLA:
- 10-25% service credit applied to next month's invoice
- Customer does NOT need to submit claim
- Maximum credit: 50% of monthly charge

7.4 SLA Reporting
TAI publishes monthly SLA report by 5th of following month.
SLA reporting available at status.tai.ai.

7.5 Limitations
TAI is not responsible for:
- Customer misconfiguration (invalid API keys, malformed requests)
- Third-party service outages (payment processors, customer endpoints)
- Customer network issues (firewall, DNS, ISP outages)
- DDoS attacks or malicious activity

[END SLA SECTION]
```

---

## FINAL NOTES

1. **SLAs are commitments, not promises.** We design them to be achievable with discipline.
2. **Service credits are automatic.** Customers should not have to beg for credits.
3. **Transparency builds trust.** Publish real-time status and monthly reports.
4. **Severity classification matters.** Train team to use consistent definitions.
5. **Escalation prevents misses.** Automate alerts when approaching SLA breach.

**Version:** 1.0.0
**Last Updated:** January 25, 2026
**Next Review:** April 25, 2026
**Approvals:** VP Product, VP Customer Success, VP Finance
