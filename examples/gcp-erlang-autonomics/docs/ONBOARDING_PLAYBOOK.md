# GCP Erlang Autonomics: Customer Onboarding Playbook

**Version**: 1.0
**Last Updated**: January 2026
**Audience**: Customer Success, Sales, Support, Product

---

## 🎯 Onboarding Philosophy

The onboarding experience is designed to move customers from **skepticism → understanding → trust → advocacy** across 30 days. Key principles:

- **Quick Wins First**: Customers see value within hours, not weeks
- **Automation Before Human**: Let the Governor do the learning; support only when decisions are needed
- **Transparent Logic**: Every action includes a receipt explaining why (no "black box" decisions)
- **Graduated Autonomy**: Day 1 approval needed for everything; Day 30 automatic governance possible
- **Success Metrics Over Activity**: We measure outcomes (cost saved, false positives, adoption) not just logins

---

## 📊 30-Day Timeline Overview

| Phase | Days | Goal | Outcome |
|-------|------|------|---------|
| **Welcome & Setup** | 1-3 | Deploy Governor, baseline learning | Baseline spend detected, first metrics visible |
| **First Decision** | 5-7 | Customer makes first policy choice, Governor takes first action | Day 7 milestone: First real action taken |
| **Configuration Tuning** | 10-14 | Customer refines policies based on initial feedback | False positive rate <10%, customer confidence high |
| **Expansion** | 21-28 | Upsell, knowledge deepening, advanced policies | Customer adds 2nd governor or advanced features |
| **Assessment & Renewal** | 30 | Success review, tier recommendation, renewal decision | Churn prediction, upsell opportunity |

---

## 📅 Day-by-Day Playbook

### **Day 1: Welcome Email (Automated)**

**Send Within**: 15 minutes of account signup
**Channel**: Email (primary), in-app notification (secondary)
**Subject**: "Your Cost Circuit Breaker is Ready 🚀"

#### Email Template

```
Subject: Your Cost Circuit Breaker is Ready 🚀

Hi [CUSTOMER_NAME],

Welcome to GCP Erlang Autonomics! Your Governor is now watching your GCP account.

🎯 QUICK WIN: Your Cost Circuit Breaker is ready (installed in 2 minutes)

Here's what just happened:
✅ Governor installed on your GCP project
✅ Cost monitoring started
✅ Waiting for your authorization to access billing data

⚡ NEXT STEP (takes 30 seconds):
Click the link below to authorize billing access:
[AUTHORIZE_BUTTON: "Grant Billing Data Access"]

📊 EXPECTED OUTCOME:
Within 1 hour, we'll detect your baseline spend and send you an analysis:
• Typical daily spend
• Peak hourly spend
• Top 5 services by cost
• Suggested throttle thresholds

No configuration needed yet—just authorize, and we'll learn your baseline.

Questions? Reply to this email or visit our chat: [SUPPORT_LINK]

Let's save some money,
The Governor 🤖

---
[Footer: Account: [PROJECT_ID] | Support: [SUPPORT_LINK] | Unsubscribe]
```

#### Required Actions
- [ ] Send email within 15 minutes
- [ ] Verify authorization link is pre-populated with OAuth session
- [ ] Create "Day 1 Email Sent" event in customer timeline
- [ ] Set up reminder for Day 2 (if authorization not completed)

#### Success Metric
- **Target**: 80% customers authorize within 24 hours
- **Track**: `authorization_completion_rate`, `time_to_authorization_hours`

---

### **Day 2-3: Auto-Configuration & Baseline Learning**

**Trigger**: After authorization OR 24 hours (whichever first)
**Timeline**: Governor runs analysis in background
**Duration**: 48-72 hours of observation needed for confident baseline

#### Email: "I've Analyzed Your Last 72 Hours"

**Send**: Day 3, 9:00 AM (customer's local time)
**Subject**: "Your GCP Baseline: Analysis Ready"

```
Subject: Your GCP Baseline: Analysis Ready (3-day summary)

Hi [CUSTOMER_NAME],

I've watched your GCP account for 72 hours. Here's what I found:

📊 YOUR BASELINE METRICS
┌─────────────────────────────────────┐
│ Typical Daily Spend      $[DAILY]    │
│ Peak Hourly Spend        $[HOURLY]   │
│ Lowest Hourly (nighttime) $[MIN]     │
│ Spend Volatility         [TREND]     │
└─────────────────────────────────────┘

💰 TOP 5 SERVICES BY COST
1. [SERVICE_1]  $X/day (Y% of total)
2. [SERVICE_2]  $X/day (Y% of total)
3. [SERVICE_3]  $X/day (Y% of total)
4. [SERVICE_4]  $X/day (Y% of total)
5. [SERVICE_5]  $X/day (Y% of total)

⚙️ RECOMMENDED THRESHOLDS
Based on your baseline, I suggest these throttle triggers:
• Cost threshold: $[DAILY] × 1.5 = $[SPIKE_THRESHOLD]
  (I'll flag unusual spend if it exceeds this)
• Error rate threshold: 1%
  (I'll flag services with error rate >1%)
• Backlog age threshold: 30 seconds
  (I'll shed load if tasks stay queued this long)

🎯 NEXT STEP (pick one):
Option 1: "Approve These Defaults" → Use recommended thresholds
Option 2: "Customize" → Adjust thresholds yourself
Option 3: "I'll Decide Later" → I'll keep watching and remind you in 2 days

[BUTTON: "Approve Defaults"] [BUTTON: "Customize"] [BUTTON: "Remind Me Later"]

💡 WHAT HAPPENS NEXT:
Once you approve, I'll switch to "dry-run" mode for the next 7 days.
If I detect something unusual, I'll send you a notification but won't take action yet.
This gives you time to say "that's normal" or "that's a problem."

Questions? Check out our "Understanding Your Baseline" guide:
[DOCS: "Understanding Your Baseline"]

Looking forward to protecting your budget,
The Governor 🤖

---
Account: [PROJECT_ID]
Last Updated: [TIMESTAMP]
```

#### If Customer Selects "Approve Defaults"
- [ ] Store policy in database
- [ ] Switch Governor to "dry-run" mode (log, notify, but don't act)
- [ ] Create event: `policy_approved`
- [ ] Send confirmation email: "Great! I'm now watching in dry-run mode"
- [ ] Set reminder: Day 5 (first potential dry-run notification)

#### If Customer Selects "Customize"
- [ ] Direct to settings page with guided wizard
- [ ] Highlight critical settings (cost threshold most important)
- [ ] Show real-time impacts: "At this threshold, I would have triggered 3 times in the last week"
- [ ] Create event: `policy_customized`

#### If Customer Selects "Remind Me Later"
- [ ] Set reminder for Day 4
- [ ] Resend with slight variation (urgency: "One more day to set thresholds")

#### Success Metrics
- **Target**: 85% approval rate (either default or custom) by Day 3
- **Track**: `policy_approval_rate`, `customization_rate`, `reminder_needed_rate`

---

### **Day 5: First Real Decision - Dry-Run Notification**

**Trigger**: Governor detects first threshold breach (usually Day 5-7)
**Channel**: Email + in-app notification
**Subject**: "Heads Up: Spend Spike Detected (No Action Taken Yet)"

#### Email Template

```
Subject: Heads Up: Spend Spike Detected (No Action Taken Yet)

Hi [CUSTOMER_NAME],

I've detected something unusual. Before I take any action, I wanted to check with you:

🚨 DETECTION: Cost Spike (Dry-Run Mode)

┌──────────────────────────────────────┐
│ Detected Spend:    $[CURRENT]         │
│ Your Threshold:    $[THRESHOLD]       │
│ Overrun:           [PCT]%             │
│ Services Affected: [TOP_3_SERVICES]   │
│ Duration:          [TIME_WINDOW]      │
└──────────────────────────────────────┘

🤔 WHAT WOULD I DO (in automatic mode)?
I would throttle these services to reduce spend:
• [SERVICE_A]: Reduce concurrent instances 100 → 50
• [SERVICE_B]: Pause [OPTIONAL_FEATURE]
• [SERVICE_C]: Queue requests (reduce load)

Estimated cost reduction: $[SAVINGS]

⚡ YOUR DECISION (pick one):

[BUTTON A] "Yes, Approve This Action"
→ I'll start automatic throttling for similar spikes
→ You can override anytime

[BUTTON B] "This is Normal, Adjust My Baseline"
→ I'll increase my threshold by 30%
→ I'll be less sensitive to future spikes like this

[BUTTON C] "Never Throttle This Service"
→ I'll whitelist [SERVICE_X]
→ Spend on this service won't trigger throttling

[BUTTON D] "Not Sure, Show Me More"
→ I'll send detailed analysis in 30 minutes
→ Takes 5 minutes to review

🔗 NEED MORE INFO?
• "Why did spend spike?" → [ANALYSIS_DASHBOARD]
• "What would happen if I approve?" → [SIMULATION_TOOL]
• "How do I undo this?" → [FAQ: "Stopping the Governor"]

The clock is ticking (this spike is live now), so please decide within the hour.

---
Dry-Run Mode: I'm not taking action yet, just checking with you.
Receipt: [LINK_TO_RECEIPT]
Dashboard: [LINK]
```

#### Customer Response Handling

**If "Yes, Approve This Action"**:
- [ ] Update policy: Enable automatic throttling
- [ ] Create event: `first_policy_approved`
- [ ] Send confirmation: "Great! I'm now in automatic mode for similar spikes"
- [ ] Set Day 7 reminder: Measure impact of first action

**If "This is Normal, Adjust My Baseline"**:
- [ ] Increase threshold by 30% (or custom %)
- [ ] Recalculate and store new baseline
- [ ] Create event: `baseline_adjusted`
- [ ] Send confirmation: "Got it. I'll be less sensitive to [SCENARIO] going forward"
- [ ] Log this feedback for future customers (feature engineering)

**If "Never Throttle This Service"**:
- [ ] Add service to whitelist
- [ ] Create event: `service_whitelisted`
- [ ] Send confirmation: "Understood. [SERVICE] is off-limits"
- [ ] Trigger alert: "Support should review why customer is whitelisting [SERVICE]"

**If "Not Sure, Show Me More"**:
- [ ] Send detailed 5-minute breakdown:
     - Historical spend for this service
     - Impact of proposed throttling (latency, error rate)
     - Similar past incidents and outcomes
- [ ] Set 15-minute follow-up reminder
- [ ] Escalate to support if no response in 2 hours

#### Success Metrics
- **Target**: 80% customers make a decision within 2 hours
- **Target**: <20% choose "Not Sure"
- **Track**: `dry_run_notification_sent`, `decision_time_minutes`, `decision_type_distribution`

---

### **Day 7: First Action Milestone**

**Trigger**: First real automatic action OR 7 days elapsed (whichever first)
**Channel**: Email + in-app celebration + Slack webhook (optional)
**Subject**: "🎉 Your First Action: Governor Saved You $[X]"

#### Email Template

```
Subject: 🎉 Your First Action: Governor Saved You $[X]

Hi [CUSTOMER_NAME],

IT HAPPENED! Your Governor just took its first action. Here's what went down:

📌 THE ACTION
┌────────────────────────────────────────────┐
│ Action Type:  [THROTTLE/ROLLBACK/SHED]     │
│ Service:      [SERVICE_NAME]                │
│ Change:       [OLD_STATE] → [NEW_STATE]    │
│ Trigger Time: [TIMESTAMP]                   │
│ Duration:     [LENGTH] minutes              │
└────────────────────────────────────────────┘

❓ WHY DID I DO THIS?
[REASON]

For example:
"Detected cost spike to $[Z] (150% of your $[Y] baseline) on Cloud Run.
You approved automatic throttling, so I reduced max instances from 100 to 50
until spend returned to normal."

💰 THE IMPACT
┌────────────────────────────────────────────┐
│ Before Action:     $[BEFORE]/hour           │
│ After Action:      $[AFTER]/hour            │
│ Money Saved:       $[SAVINGS]               │
│ User Impact:       Latency +[MS]ms, Errors +[PCT]% │
└────────────────────────────────────────────┘

✅ VERDICT: This was the right call
Your users barely noticed the change. You saved [SAVINGS] without sacrificing quality.

📊 FULL RECEIPT
[RECEIPT_DASHBOARD_LINK]
View exactly what changed, when, and why.

🎯 WHAT'S NEXT?
You've proven the Governor works. Next steps:

Option 1: "Keep Going"
→ I'll continue automatic governance. You can override anytime.

Option 2: "Make it More Aggressive"
→ I can reduce threshold from 1.5× to 1.25× (catch spikes earlier)

Option 3: "Add Deploy Rollback Guard"
→ Automatically roll back deployments that spike costs or errors
→ [UPGRADE: "Add Rollback Guard" $199/mo]

Option 4: "I Need a Pause"
→ I'll switch back to dry-run mode temporarily

[BUTTON: "Keep Going"] [BUTTON: "More Aggressive"]
[BUTTON: "Add Rollback Guard"] [BUTTON: "Pause Automatic Actions"]

🗣️ FEEDBACK
Was this the right call?
[BUTTON: "👍 Yes, great decision"] [BUTTON: "👎 No, I didn't want that"]

If you hit the thumbs-down, I want to know why. Please reply to this email.
Your feedback helps me learn.

---
Receipt: [LINK]
Dashboard: [LINK]
Support: [CHAT_LINK]
```

#### Celebration Event
- [ ] Award badge: "First Action Hero" (if in-app gamification)
- [ ] Create timeline event: `first_action_taken`
- [ ] Log action details in customer success dashboard
- [ ] If thumbs-down received, escalate to support within 30 minutes
- [ ] If no feedback within 24h, send gentle follow-up

#### Success Metrics
- **Target**: First action within 7 days
- **Target**: 70% feedback submitted within 24 hours
- **Target**: <10% negative feedback (thumbs-down)
- **Track**: `first_action_taken_day`, `feedback_submission_rate`, `feedback_sentiment`

---

### **Day 10-14: Configuration Review Checkpoint**

**Trigger**: Day 14 automatically
**Channel**: Email + in-app summary dashboard
**Subject**: "Two Weeks In: How's the Governor Doing?"

#### Email Template

```
Subject: Two Weeks In: How's the Governor Doing? 📊

Hi [CUSTOMER_NAME],

14 days in—time for a checkpoint. Here's how you're doing:

📈 YOUR PROGRESS
┌────────────────────────────────────────────┐
│ Actions Taken:         [N]                  │
│ Total Spend Protected:  $[TOTAL_SAVED]      │
│ Daily Savings:         $[DAILY_SAVED]       │
│ False Positives:       [M] (you rejected)   │
│ Decisions You Overrode: [K]                 │
│ Approval Rate:         [APPROVAL_PCT]%      │
└────────────────────────────────────────────┘

🎯 WHAT THIS MEANS
- Actions Taken [N]: You've gotten [N] spikes and handled them well
- Spend Protected: You've saved approximately $[TOTAL_SAVED] vs. unmanaged baseline
- False Positives [M]: [M] times I flagged something that wasn't actually a problem
  → If [M] is high (>5), we should adjust thresholds. Want to tune?
  → If [M] is low (<2), I'm learning your normal patterns well
- Decisions Overrode [K]: [K] times you said "no, don't do that"
  → This helps me learn what matters to you

💡 ASSESSMENT
┌────────────────────────────────────────────┐
│ Governor Effectiveness:  [GOOD/EXCELLENT]   │
│ Customer Satisfaction:   [GOOD/EXCELLENT]   │
│ Recommended Next Step:   [STEP]              │
└────────────────────────────────────────────┘

🚀 NEXT STEPS (Your Options)

**Option 1: Keep Current Setup** (No change)
→ Keep doing what we're doing

**Option 2: More Aggressive Governance** ($0, requires approval)
→ Tighten thresholds, act faster on anomalies
→ You can approve or customize before I switch

**Option 3: Expand to Multiple Governors** (Growth Tier, $499/mo)
→ Add 2nd and 3rd Governors for critical services
→ Isolate governance: Production vs. Staging, different cost centers
→ Premium feature: Tenant-scoped governors for multi-tenant apps

**Option 4: Deploy Rollback Guard** (Add-on, $199/mo)
→ Automatically roll back deploys that cause cost spikes
→ Roll back if error rate exceeds threshold
→ You approve the first 3 rollbacks; after that, automatic

**Option 5: Advanced Policies** (Premium, $999/mo)
→ Time-window rules (different thresholds during business hours)
→ Service-specific policies (looser on non-critical services)
→ ML-powered anomaly detection (predict spikes before they happen)

[BUTTON: "Keep Current"] [BUTTON: "More Aggressive"]
[BUTTON: "Growth Tier: Multi-Governor"] [BUTTON: "Add Rollback Guard"]
[BUTTON: "Premium Features"]

📞 WANT A HUMAN REVIEW?
If you want to chat through options with an expert, we offer a free 30-minute onboarding call:
[BUTTON: "Schedule Onboarding Call"]

Your success is our success,
The Governor 🤖

---
Dashboard: [LINK]
Full Report: [LINK]
FAQ: [LINK]
Support: [CHAT_LINK]
```

#### If Customer Selects "More Aggressive"
- [ ] Show threshold adjustment wizard
- [ ] Display impact: "At these thresholds, I would have taken 3 additional actions"
- [ ] Get approval before implementing
- [ ] Create event: `policy_tightened`

#### If Customer Selects "Growth Tier"
- [ ] Route to sales process
- [ ] Offer setup assistance
- [ ] Create event: `upgrade_initiated`
- [ ] Schedule follow-up within 24h to confirm purchase

#### If Customer Wants Onboarding Call
- [ ] Schedule within 3 business days
- [ ] Send prep materials before call (10 min read)
- [ ] Use call to identify upsell opportunities, support issues, feature requests
- [ ] Document call notes for future reference

#### Success Metrics
- **Target**: 70% customers engage with checkpoint email
- **Target**: 40% select "More Aggressive" or upgrade path
- **Target**: 20% NPS (Net Promoter Score) at Day 14
- **Track**: `checkpoint_engagement_rate`, `upgrade_consideration_rate`, `nps_day14`

---

### **Day 21: Knowledge Base & Advanced Features**

**Trigger**: Day 21 automatically
**Channel**: Email + in-app learning hub
**Subject**: "Level Up: Advanced Governor Features (Free Tier)"

#### Email Template

```
Subject: Level Up: Advanced Governor Features

Hi [CUSTOMER_NAME],

You've graduated from "Governor 101" to intermediate level.
Time to learn advanced features (all included in your tier):

📚 SELF-SERVICE DOCUMENTATION

**How to Interpret Receipts** (5 min read)
→ Every action includes a detailed receipt. Learn to read it like a pro.
→ [DOCS: "Understanding Receipts"]

**Advanced Policies: Time Windows** (10 min read)
→ Set different thresholds for different times
→ Example: Loose thresholds during business hours (spikes expected),
  strict thresholds at night (every spend matters)
→ [DOCS: "Time Window Policies"]

**Advanced Policies: Service Exclusions** (5 min read)
→ Whitelist critical services (database, payment processor)
→ Blacklist non-essential services (logging, analytics)
→ [DOCS: "Service Policies"]

**Advanced Policies: Progressive Actions** (10 min read)
→ Instead of hard throttles, use progressive escalation:
  1. First spike: Warn (notify, no action)
  2. Second spike: Throttle to 75% (slow it down)
  3. Third spike: Throttle to 50% (reduce impact)
  4. Fourth spike: Pause (stop spending entirely)
→ This prevents false positives while protecting budget
→ [DOCS: "Progressive Action Policies"]

**Incident Response Playbook** (20 min read)
→ What if the Governor takes an action you don't like?
→ How to rollback, override, or pause
→ When to contact support vs. handle yourself
→ [DOCS: "Incident Response"]

**Integration: Slack Notifications** (5 min setup)
→ Get Governor alerts in your Slack channel
→ Requires Slack workspace permission (you control)
→ [GUIDE: "Slack Integration"]

🎓 HANDS-ON LEARNING

**Interactive Dashboard Walkthrough** (video, 8 min)
→ Tour the dashboard, learn every feature
→ [VIDEO: "Dashboard Walkthrough"]

**Simulator: "What If" Scenarios** (interactive, 10 min)
→ Play with thresholds, see how Governor would react to past spikes
→ No real impact—just learning
→ [SIMULATOR: "What If Scenarios"]

**Certification: Governor Expert** (optional, 30 min)
→ Complete 10 exercises, earn "Governor Expert" badge
→ Unlock premium features (advanced policies)
→ [CERTIFICATION: "Governor Expert Program"]

🎯 YOUR KNOWLEDGE TRACKER
You've completed:
✅ Onboarding (Day 1-7)
✅ First Action (Day 7)
⬜ Time Window Policies
⬜ Service Exclusions
⬜ Progressive Actions
⬜ Incident Response
⬜ Slack Integration
⬜ Dashboard Mastery
⬜ What-If Simulator

[BUTTON: "Start Learning"] [BUTTON: "View All Docs"]

💡 BASED ON YOUR USAGE
We've noticed you're using [FEATURE] a lot.
You might love [RELATED_FEATURE]. Here's how to use it:
[SHORT_GUIDE: "[RELATED_FEATURE]"]

🚀 READY FOR PREMIUM?
Advanced time-window policies, ML anomaly detection, and auto-rollback?
Upgrade to Premium at $999/mo:
[BUTTON: "Explore Premium"]

Questions? [CHAT_SUPPORT]

Keep learning,
The Governor 🤖

---
Dashboard: [LINK]
Support: [CHAT_LINK]
```

#### In-App Learning Hub
- [ ] Create curated "Paths" (e.g., "Cost Optimization Master", "Incident Response Expert")
- [ ] Track progress: Show which docs customer has read
- [ ] Recommend docs based on usage (if customer uses throttle action a lot, recommend "Progressive Actions")
- [ ] Gamification: Badges for completing paths, "Governor Expert" certification
- [ ] Create event: `doc_page_viewed` for each resource read

#### Success Metrics
- **Target**: 50% customers read at least 1 doc
- **Target**: 20% complete certification
- **Target**: 15% upgrade to Premium after knowledge deepening
- **Track**: `docs_engagement_rate`, `certification_completion_rate`, `premium_upgrade_rate`

---

### **Day 30: Success Assessment & Renewal Decision**

**Trigger**: Day 30 automatically
**Channel**: Email + in-app success dashboard + optional call
**Subject**: "One Month In: Let's Assess & Plan Your Next 30 Days"

#### Email Template

```
Subject: One Month In: Let's Assess Your Governor Success 🎯

Hi [CUSTOMER_NAME],

30 days ago, you started with the Governor. Today's the day we assess impact and plan next steps.

📊 YOUR 30-DAY IMPACT REPORT
┌────────────────────────────────────────────┐
│ Total Spend Protected:  $[TOTAL_SAVED]      │
│ Total Actions Taken:    [N]                 │
│ False Positive Rate:    [FP_PCT]%           │
│ Average Decision Time:  [AVG_TIME] minutes  │
│ Uptime:                 [UPTIME]%           │
│ NPS (You Rated):        [NPS_SCORE]         │
└────────────────────────────────────────────┘

💭 HOW YOU'RE DOING

Based on these metrics, you fall into: [TIER]

**TIER A: The Optimizers** (You!)
→ High adoption, low false positives, cost-conscious
→ [X]% of customers reach this level by Day 30
→ Next: Expand to multi-governor, advanced policies

**TIER B: The Learners**
→ Good start, still tuning thresholds
→ Next: Schedule onboarding call to optimize

**TIER C: The Cautious**
→ Testing waters, mostly approving actions
→ Next: Review FAQ, custom onboarding

**TIER D: The Observers**
→ Still in dry-run mode, not ready for automation
→ Next: Upgrade timeline is flexible; no pressure

🎯 RENEWAL DECISION

Your subscription expires in [X] days. Here are your options:

**Option 1: Tier 1 (Current) - $99/mo**
→ Single Governor, cost protection, dry-run mode
→ Annual commitment: $1,188 (10% discount)
→ Includes: Email support, documentation, community

[BUTTON: "Renew Tier 1"]

**Option 2: Growth Tier - $499/mo**
→ 3 Governors (isolate by service/environment/cost center)
→ Tenant-scoped governors (multi-tenant apps)
→ Premium features: Advanced policies, time windows
→ Annual commitment: $5,388 (10% discount)
→ Includes: Priority email support, 1 free onboarding call

[BUTTON: "Upgrade to Growth"]

**Option 3: Enterprise Tier - Custom**
→ Unlimited Governors
→ Custom integrations (CI/CD pipelines, PagerDuty, DataDog)
→ Dedicated account manager
→ 24/7 phone support
→ Advanced SLAs

[BUTTON: "Contact Sales"]

🎁 RENEWAL INCENTIVES (Limited Time)

**Loyalty Discount**: Renew today, get 15% off first month
→ Tier 1: $84/mo instead of $99/mo

**Upgrade Bonus**: Switch to Growth Tier, get 2 free months
→ Growth: $499/mo × 12 = $5,988 → Pay for 10 months

**Annual Commitment**: Lock in 20% discount (annual billing)

[BUTTON: "Claim Loyalty Discount"] [BUTTON: "Upgrade + Save 2 Months"]

📞 WANT TO TALK?

Free 30-minute consultation to discuss:
- Is Governor meeting your needs?
- Are there features you wish existed?
- Which tier is right for your company?
- Anything not working well?

[BUTTON: "Schedule Call"]

No pressure—we're here to help you decide.

---

**Your Success Story**
You've already saved $[TOTAL_SAVED] in one month.
At this rate, you'll save $[ANNUAL] per year.
That's [PAYBACK_MULTIPLE]x your annual subscription cost.

We're excited to help you save more,
The Governor 🤖

P.S. If you have feedback (good or bad), we read every reply.
Just hit reply and tell us what's on your mind.

---
Dashboard: [LINK]
Full Report: [LINK]
Support: [CHAT_LINK]
Pricing: [LINK]
```

#### Assessment Logic

| Metric | Good | Excellent | Action |
|--------|------|-----------|--------|
| Spend Protected | >$500 | >$2,000 | Upsell Growth |
| False Positive Rate | <15% | <5% | Offer Premium (better ML) |
| Actions Taken | 5+ | 15+ | Mention multi-governor use cases |
| NPS Score | 40+ | 50+ | Ask for testimonial/case study |
| Doc Completion | <3 docs | >5 docs | Offer certification |

#### Renewal Path

**If Likely to Renew**:
- [ ] Offer 15% loyalty discount
- [ ] Suggest tier upgrade based on usage
- [ ] Create event: `renewal_likely`

**If Uncertain**:
- [ ] Offer free onboarding call
- [ ] Provide customer success checklist
- [ ] Create event: `renewal_at_risk` + assign to success team

**If Likely to Churn**:
- [ ] Investigate: Why is customer not seeing value?
- [ ] Offer free month to reconsider
- [ ] Route to support for deep-dive troubleshooting
- [ ] Create event: `renewal_unlikely` + escalate to leadership

#### Success Metrics
- **Target**: 80% renewal rate (Tier 1 customers)
- **Target**: 25% upgrade rate (Tier 1 → Growth/Enterprise)
- **Target**: 60+ NPS (industry target is 50+)
- **Track**: `renewal_rate`, `upgrade_rate`, `nps_score`, `churn_rate`

---

## 📋 Configuration Templates for Common Scenarios

### **Template 1: Startup (Tight Budget)**

**Profile**: Early-stage company, limited budget, growth-focused

**Configuration**:
```yaml
Name: Startup Budget Guard
Cost Threshold: 120% of baseline (tight: detect 20% spikes)
Error Rate Threshold: 2% (forgiving: some errors expected)
Backlog Age Threshold: 60 seconds (loose: queue latency OK)
Action Type: Progressive
  - First spike: Notify
  - Second spike: Throttle to 75%
  - Third spike: Throttle to 50%
  - Fourth spike: Pause

Whitelisted Services:
  - Cloud SQL (production database—never throttle)
  - Stripe/Payment Processor (never interrupt)
  - Authentication Service

Time Windows:
  - Business Hours (9-5): Full protection
  - Nights (6pm-9am): Pause Governor (humans sleeping anyway)
  - Weekends: Pause Governor (lower spend expected)

Notifications: Daily summary email, Slack integration
```

**Expected Outcomes**:
- False positives: High initially (adjust after 1 week)
- Cost savings: 15-25% on non-essential services
- User impact: Minimal (most spikes caught early)

**Onboarding Messaging**: "You're a bootstrap startup—we'll protect every dollar."

---

### **Template 2: E-Commerce (Traffic Spikes Predictable)**

**Profile**: Online store, traffic spikes during sales/promotions, cost-sensitive but availability crucial

**Configuration**:
```yaml
Name: E-Commerce Peak Protection
Cost Threshold: 150% of baseline (expect spikes during promotions)
Error Rate Threshold: 0.5% (customers notice errors)
Backlog Age Threshold: 15 seconds (latency kills conversions)
Action Type: Aggressive Escalation
  - First spike: Notify
  - Second spike: Throttle to 90% (minimal impact)
  - Third spike: Throttle to 75%
  - Fourth spike: Shed load (queue oldest requests)

Whitelisted Services:
  - Cloud Run (checkout flow—never restrict)
  - Datastore (product catalog)
  - Stripe API (payments)

Graylisted Services (Monitor but don't auto-throttle):
  - Analytics
  - Recommendations
  - Email Service

Deploy Guard: AGGRESSIVE
  - Roll back if error rate >2%
  - Roll back if latency increases >200%
  - Require manual approval for deploys during peaks

Time Windows:
  - Shopping Hours (8am-10pm): Full monitoring
  - Off-Hours (10pm-8am): Relaxed thresholds
  - Holiday Spikes (Black Friday, Cyber Monday): Override rules

Notifications: Slack + SMS (real-time alerts during peaks)
```

**Expected Outcomes**:
- False positives: Very low (<3%)
- Cost savings: 20-30% on redundant capacity
- User impact: Zero (intelligent load shedding)

**Onboarding Messaging**: "We'll protect your margins without losing customers."

---

### **Template 3: SaaS (Multi-Tenant)**

**Profile**: B2B SaaS with multiple customers, per-tenant cost visibility, compliance needs

**Configuration**:
```yaml
Name: Multi-Tenant Cost Guard
Governor Type: Tenant-Scoped (1 Governor per customer)

Per-Tenant Limits:
  - Monthly budget cap: $[X]
  - Daily budget cap: $[X] / 30
  - Spike threshold: 150% of tenant's average

Tenant Actions:
  - When threshold exceeded:
    1. Notify tenant (in-app + email)
    2. After 30 min: Throttle tenant (reduce concurrency)
    3. After 60 min: Pause tenant's non-critical services
    4. After 120 min: Suspend tenant (notify them immediately)

Whitelist by Service Type:
  - All tenants: Database, authentication
  - SLA 99.9%: Always available (never throttle)
  - SLA 99%: Can throttle if tenant over-budget

Notifications:
  - Tenant: "Your usage is [X]% of your monthly budget"
  - Operator: "Tenant [X] approaching budget; may need to pause soon"
  - Internal: Log all tenant throttling for billing/support disputes

Audit Trail: Full compliance logging (who did what, when, why)
```

**Expected Outcomes**:
- False positives: Very low (hard thresholds per tenant)
- Cost savings: 40-60% (prevent runaway tenant abuse)
- Tenant satisfaction: High (predictable pricing)

**Onboarding Messaging**: "Protect your margins while giving customers predictable costs."

---

### **Template 4: Gaming (Evening Spikes Predictable)**

**Profile**: Gaming platform, evening traffic predictable, occasional game launches, tolerance for downtime low

**Configuration**:
```yaml
Name: Gaming Platform Autonomic
Cost Threshold: 200% of baseline (spikes expected, budgeted for)
Error Rate Threshold: 1% (some errors acceptable)
Backlog Age Threshold: 10 seconds (users tolerate brief latency)

Deployment Guard: RELAXED
  - Error threshold for rollback: 5% (gaming tolerates errors)
  - Latency threshold: +500ms OK
  - Only roll back if p95 latency >2 seconds

Backlog Shed: AGGRESSIVE
  - If queue >100ms old: Shed oldest 10%
  - If queue >500ms old: Shed oldest 50%
  - Rationale: Better to lose some players than freeze all

Services by Priority:
  - Critical (never throttle):
    • Game servers (Agones or custom)
    • Player authentication
    • Real-money transactions
  - Optional (OK to throttle):
    • Chat service
    • Leaderboards
    • Achievements
    • Analytics

Time Windows:
  - Peak Hours (7pm-11pm): Full capacity (no throttle)
  - Shoulder Hours (5pm-7pm, 11pm-12am): 80% threshold
  - Off-Peak (12am-5pm): 150% threshold

Deploy Rules:
  - During peak hours: NO deployments (pause auto-deploy)
  - Before peak hours: Only critical fixes (manual approval)
  - Off-peak: Full auto-deploy enabled

Notifications: Slack + webhook to incident management
```

**Expected Outcomes**:
- False positives: Medium (spikes are normal)
- Cost savings: 10-15% (peak costs budgeted, but contingency reduced)
- User impact: Minimal (graceful degradation)

**Onboarding Messaging**: "Your players come first. We'll manage costs without blocking fun."

---

## ⚠️ Common Misconfigurations & Fixes

### **Mistake 1: Too Aggressive (Hair-Trigger Governor)**

**Symptom**: Governor throttles almost every spike, even small normal ones

**Root Cause**:
- Threshold set too low (e.g., 110% instead of 150%)
- Short observation window (baseline not stable)
- Volatile service with high natural variance

**Detection**:
- Week 1: 20+ false positives
- Customer feedback: "Governor is too aggressive"
- Approval rate: <50% (customer rejecting most actions)

**Fix**:
```
1. Increase threshold by 30% (e.g., 130% → 160%)
2. Extend observation window (3 days → 7 days)
3. Switch to progressive actions (notify → throttle → pause)
4. Analyze false positives: Is there a pattern? (E.g., always Mondays?)
5. Whitelist high-variance service or give it special thresholds
```

**Prevention**:
- Show histogram during onboarding: "Your spend ranges $X-$Y. This line is your threshold."
- Recommend thresholds based on data (not one-size-fits-all)
- Default to 150% (not 120%) to reduce false positives

---

### **Mistake 2: Too Conservative (Governor Never Acts)**

**Symptom**: Governor detects nothing, never takes action

**Root Cause**:
- Threshold set too high (e.g., 200% of baseline)
- Whitelist is too permissive (all services whitelisted)
- Time windows too restrictive (Governor paused most hours)

**Detection**:
- Month 1: Zero actions taken
- Customer feedback: "Is the Governor even watching?"
- Approval rate: 0% (no decisions to make)

**Fix**:
```
1. Reduce threshold from 200% → 150%
2. Review whitelist: Remove non-critical services
3. Expand time windows: Governor should be active 24/7 (unless intentional)
4. Enable "Recommendations" mode: Tell customer about potential actions
   (log, notify, but don't throttle automatically)
5. Manually trigger a test: "Here's what I would do if..."
```

**Prevention**:
- Test threshold against historical data: "Would this catch the last 5 spikes?"
- Show whitelist during onboarding: "These services are off-limits [confirm]"
- Warn if threshold too high: "At this level, I'd only trigger during major incidents"

---

### **Mistake 3: Whitelist Too Permissive**

**Symptom**: Customer complains: "Why didn't the Governor act? I thought it was protecting my budget!"

**Root Cause**:
- Customer whitelisted too many services during Day 5 decision
- Onboarding didn't explain whitelist implications
- Customer unsure which services are critical

**Detection**:
- Customer says: "I whitelisted [service] because it seemed important"
- Review whitelist: 30%+ of spend is whitelisted
- Support tickets: "Governor didn't help with our problem"

**Fix**:
```
1. Review whitelist with customer (live conversation or dashboard)
2. Ask: "Why did you whitelist [service]? Can we un-whitelist it?"
3. Replace whitelist with progressive actions (notify → throttle)
   Instead of "never throttle database", try "warn first, then throttle"
4. Show impact: "Removing [service] from whitelist would catch N past spikes"
5. Implement for 1 week and measure false positive rate
```

**Prevention**:
- During Day 5 decision, explain each option:
  - "Approve this action" → Let Governor throttle automatically
  - "Adjust baseline" → Change when Governor triggers
  - "Never throttle" → **Understand implications** (Governor will ignore this service forever)
- Show whitelist impact: "You're currently protecting $[X] of your spend. Throttle rate: [Y]%"
- Suggest smart whitelist: "Critical services (database, auth) only. Optional services (analytics) can be throttled"

---

### **Mistake 4: Time Windows Incorrect**

**Symptom**: Governor takes action at wrong time (e.g., throttles at 3am, wakes up on-call engineer)

**Root Cause**:
- Time zone not set correctly (system time vs. customer time)
- Time windows not aligned with customer's actual business hours
- Unexpected off-hours emergencies

**Detection**:
- Customer reports: "Governor paused our service at 2am!"
- Support ticket: "We want no auto-actions at night"
- Slack notification arrives at odd hours

**Fix**:
```
1. Confirm customer's timezone (ask explicitly during onboarding)
2. Review time windows: Match actual business hours
   - If customer says "9-5", confirm: Means 9am-5pm in [TIMEZONE]?
   - Include on-call rotations: Maybe "24/7 for critical services"
3. Adjust time windows:
   - Business hours: Full auto-governance (approve actions immediately)
   - Off-hours: Dry-run only (notify, wait for human approval)
   - Weekends: Pause Governor entirely (on-call engineer not expecting alerts)
4. Set escalation: If emergency after hours, escalate to human, not automatic action
```

**Prevention**:
- During onboarding, explicitly ask: "What hours does your team work?"
- Show time zone in dashboard prominently
- Default to "pause at night" (better to miss something than wake engineer)
- Offer 24/7 monitoring as premium feature with paid support

---

### **Mistake 5: No Feedback Loop**

**Symptom**: Governor takes actions, customer doesn't know why, can't control future behavior

**Root Cause**:
- Actions not explained (no receipt or reasoning)
- No feedback mechanism (customer can't say "that was wrong")
- Governor doesn't learn from customer overrides

**Detection**:
- Customer asks: "Why did the Governor do that?"
- Support: "I want to understand the Governor's logic"
- Actions taken but customer doesn't trust them

**Fix**:
```
1. Create rich receipts for every action:
   - What triggered the action?
   - What were the alternatives?
   - What's the expected impact?
   - How to rollback if wrong?

2. Add feedback mechanism:
   - "Was this the right action?" (thumbs up/down)
   - "What should I do next time?" (custom feedback)
   - Negative feedback routes to support

3. Log customer overrides:
   - When customer says "no, don't do that", log it
   - Adjust future decisions based on overrides
   - "You've overridden this action 5 times; I'm learning it's normal"

4. Weekly review email:
   - Summary of actions taken
   - Feedback summary (what customer approved vs. rejected)
   - Learnings: "I'm now more conservative with [service]"
```

**Prevention**:
- Receipts are mandatory (not optional)
- Always include: Reason, expected impact, how to undo
- Make feedback easy (single click, not forms)
- Explain logic before taking major actions (dry-run mode for first week)

---

## 📈 Escalation Path

### **Tier 0: Automated Guidance** (Day 1-7)
- Automated welcome emails
- Self-service configuration wizard
- FAQ + documentation
- No human involvement
- **Success Rate**: 70% of customers onboard successfully without support

### **Tier 1: In-App Support** (Day 7+)
- Chat widget (average response <2 hours)
- Knowledge base articles
- Video tutorials
- Common issue templates
- **Escalation Trigger**: Customer asks same question 3+ times in chat → Assign to support agent

### **Tier 2: Email Support** (Day 7+)
- Email support for detailed issues
- Response time: <4 hours (business hours)
- In-depth troubleshooting
- Configuration recommendations
- **Escalation Trigger**: Customer reports issue affecting production → Call support

### **Tier 3: Phone Support** (Growth tier+)
- Free 30-minute onboarding call (included in Growth)
- Proactive outreach if churn risk detected
- Custom configuration assistance
- Account manager (Enterprise only)
- **Escalation Trigger**: Customer wants custom integration or SLA

### **Tier 4: Product/Engineering** (Enterprise only)
- Direct access to product team
- Custom feature requests
- Priority bug fixes
- Quarterly business reviews
- **Escalation Trigger**: Enterprise customer has blocking issue or wants custom build

---

### **Support Escalation Checklist**

When customer contacts support, ask:

**Tier 1 Questions** (Route to chat/FAQ):
- "How do I authorize billing access?" → FAQ
- "What does this notification mean?" → Knowledge base article
- "How do I undo an action?" → Documentation link

**Tier 2 Questions** (Assign to support agent):
- "My false positive rate is high. How do I fix it?" → Troubleshooting guide
- "Can I set different thresholds for different services?" → Configuration session
- "Should I upgrade to Growth tier?" → Assessment conversation

**Tier 3 Questions** (Offer free onboarding call):
- "I want to integrate Governor with our CI/CD pipeline" → Engineering call
- "Can you help us set up multi-tenant governance?" → Architecture discussion
- "Is Governor right for our use case?" → Consultation

**Tier 4 Questions** (Escalate to account manager):
- "We need a custom Governor for [non-standard use case]" → Product meeting
- "We need 99.99% uptime SLA" → Contract negotiation
- "Can you build a [custom feature]?" → Roadmap discussion

---

## 📊 Success Metrics & Tracking

### **Day 1 Metrics**
- `welcome_email_sent` - Email delivery success
- `authorization_completion_rate` - % customers authorizing billing access within 24h
- `authorization_time_hours` - Time from email to authorization
- `target`: >80% authorization rate within 24 hours

### **Day 3 Metrics**
- `baseline_learning_completion` - % of customers with baseline computed
- `policy_approval_rate` - % approving default or custom policy
- `policy_customization_rate` - % customizing defaults
- `target`: >85% policy approval within 3 days

### **Day 5-7 Metrics**
- `dry_run_notification_sent` - % of customers receiving 1st dry-run
- `decision_time_minutes` - How long customer takes to decide
- `decision_type_distribution` - Approve/Reject/Whitelist/Customize breakdown
- `first_action_latency_days` - Days until first real action taken
- `target`: First action within 7 days, >80% decisions made within 2 hours

### **Day 14 Metrics**
- `actions_taken_total` - Total actions by Day 14
- `false_positive_rate` - % actions customer rejected
- `spend_protected_total` - $ saved by Governor
- `spend_protected_daily` - $/day savings rate
- `approval_rate` - % of actions approved
- `override_count` - Times customer said "no"
- `nps_day14` - Net Promoter Score at Day 14
- `target`: NPS >20, false positive rate <10%, spend protected >$500

### **Day 30 Metrics**
- `renewal_rate` - % of customers renewing subscription
- `upgrade_rate` - % upgrading to higher tier
- `churn_rate` - % canceling or not renewing
- `nps_day30` - Net Promoter Score at Day 30
- `docs_engagement_rate` - % reading knowledge base articles
- `certification_completion_rate` - % completing "Governor Expert" cert
- `target`: Renewal >80%, upgrade >25%, churn <5%, NPS >60

### **Lifetime Metrics**
- `customer_lifetime_value` - Total ARR per customer
- `payback_period_months` - Months until Governor saves subscription cost
- `save_multiple` - How many times subscription cost was saved in year 1
- `support_ticket_volume` - Tickets per customer per month
- `support_satisfaction` - % of support interactions rated positive
- `monthly_recurring_revenue` - Total MRR from Governor customers
- `target`: LTV >$12k, payback <4 months, save multiple >2x

---

### **Tracking Dashboard** (Internal Success Team)

Create a dashboard with:

| Metric | Target | Current | Status |
|--------|--------|---------|--------|
| Customers Onboarding This Month | 50 | 47 | 🟢 On Track |
| Day 1 Authorization Rate | 80% | 82% | 🟢 Exceeding |
| Day 3 Policy Approval Rate | 85% | 78% | 🟡 Below Target |
| Day 7 First Action Taken | 90% | 87% | 🟡 Below Target |
| Day 14 NPS | >20 | 32 | 🟢 Exceeding |
| Day 30 Renewal Rate | 80% | 84% | 🟢 Exceeding |
| Day 30 Upgrade Rate | 25% | 19% | 🟡 Below Target |
| Monthly Churn Rate | <5% | 6% | 🟡 Above Target |

---

## 🔄 Continuous Improvement

### **Weekly Onboarding Review**
- Cohort analysis: How are customers from last week doing?
- Problem identification: Are there patterns in drop-offs?
- Iteration: What can we improve for next cohort?

### **Monthly Playbook Updates**
- Extract learnings from support tickets
- Update templates based on customer feedback
- Test new messaging/features with small cohort
- Document successful patterns

### **Quarterly Milestone Reviews**
- Day 30 NPS analysis by cohort
- Churn analysis: Why do customers cancel?
- Expansion analysis: Which customers upgrade? Why?
- Competitive analysis: Are competitors doing better?

### **Annual Strategy Refresh**
- Market research: Customer expectations shifting?
- Feature requests: What do customers want?
- Pricing analysis: Is pricing aligned with value?
- Playbook redesign: Major changes for next year?

---

## 📞 Support Contact Matrix

| Question Category | Channel | Response Time | Escalation |
|-------------------|---------|-------------------|------------|
| Authorization | Chat/Email | <2 hours | Tier 1 → Tier 2 |
| Configuration Help | Chat | <2 hours | Tier 1 → Tier 2 (phone optional) |
| False Positives | Email | <4 hours | Tier 2 → Engineering (if recurring) |
| Production Issue | Phone | <30 min | Tier 3 → Tier 4 |
| Billing/Renewal | Email | <4 hours | Tier 2 → Sales |
| Feature Request | Chat | <24 hours | Tier 1 → Product |
| Bug Report | Chat/Email | <4 hours | Tier 2 → Engineering |
| Custom Integration | Email/Phone | <24 hours | Tier 3 → Engineering |

---

## 🎓 Knowledge Base Structure

Organize support materials by onboarding stage:

### **Stage 1: Getting Started** (Day 1-3)
- [ ] "What is a Cost Circuit Breaker?" (video, 3 min)
- [ ] "How to authorize billing access" (guide, 2 min)
- [ ] "Understanding your baseline" (article, 5 min)

### **Stage 2: Making Your First Decision** (Day 5-7)
- [ ] "Understanding dry-run notifications" (guide, 3 min)
- [ ] "Should I approve this action?" (decision tree)
- [ ] "How to whitelist services" (guide, 3 min)
- [ ] "Understanding receipts" (tutorial, 5 min)

### **Stage 3: Ongoing Management** (Day 10+)
- [ ] "Interpreting your success metrics" (dashboard walkthrough, 5 min)
- [ ] "Advanced policies" (advanced guide, 15 min)
- [ ] "Incident response" (troubleshooting, 10 min)
- [ ] "Integrations: Slack, CI/CD, etc." (integration guides)

### **Stage 4: Optimization** (Day 21+)
- [ ] "Governor Expert Certification" (course, 30 min)
- [ ] "Case studies: How others use Governor" (4-5 case studies)
- [ ] "Best practices by industry" (templates)

---

## ✅ Onboarding Completion Checklist

Before marking customer as "successfully onboarded", verify:

- [ ] **Day 1**: Authorization email sent and customer authorized billing access
- [ ] **Day 3**: Baseline analysis sent and policy approved (default or custom)
- [ ] **Day 5-7**: Customer made first policy decision (approve, whitelist, adjust baseline)
- [ ] **Day 7**: Governor took first real action (or determined no action needed)
- [ ] **Day 14**: Checkpoint email sent and customer engaged (opened email + clicked something)
- [ ] **Day 21**: Knowledge base made available and customer viewed at least 1 doc
- [ ] **Day 30**: Renewal decision made (renew, upgrade, or churn decision made)
- [ ] **NPS**: Day 14 NPS >= 20 (passive or promoter)
- [ ] **Engagement**: Customer took at least 1 action in the Governor UI

---

## 🎯 Success Definition

Customer is **successfully onboarded** when they:
1. **Understand** what the Governor does and how it helps
2. **Trust** the Governor to make decisions (NPS >= 20)
3. **Use** the Governor actively (at least 1 policy configured)
4. **See Value** (spend protected >= $500 or no issues occurring)
5. **Decide** to renew (or understand why they won't)

---

**Document Version**: 1.0
**Last Updated**: January 2026
**Next Review**: April 2026
**Owner**: Customer Success / Product Management
