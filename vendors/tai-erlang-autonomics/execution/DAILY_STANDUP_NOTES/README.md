# Daily Standup Notes: TAI Erlang Autonomics

**Purpose:** Log 15-minute daily standup progress (3:00 PM EST)
**Keeper:** CSM / Project Manager
**Format:** One .md file per day with structured notes

---

## How to Use This Directory

### Daily Process

1. **3:00 PM EST:** 15-minute video call (Slack or Zoom)
2. **During call:** Each person (CEO, CTO, VP Sales, CSM) answers 3 questions
3. **After call:** CSM updates `.md` file with notes
4. **Next morning:** Team reviews yesterday's standup for context

### File Naming Convention

```
week-[N]/
  day-[1-7]-[DAY_NAME].md

Examples:
week-1-2/day-1-monday.md           (Jan 27, 2026)
week-1-2/day-2-tuesday.md          (Jan 28, 2026)
week-1-2/day-3-wednesday.md        (Jan 29, 2026)
week-1-2/day-4-thursday.md         (Jan 30, 2026)
week-1-2/day-5-friday.md           (Jan 31, 2026 + WEEKLY REVIEW)
```

### Standup Template

```markdown
# STANDUP: [Date, Day Name]

📅 **Date:** [Jan 27, 2026 - Monday]
⏰ **Time:** 3:00 PM EST
📍 **Duration:** 15 minutes
🎯 **Attendees:** CEO, CTO, VP Sales, CSM, [optional: advisors]
📝 **Notes By:** [CSM name]

---

## CEO
**What shipped/unblocked yesterday?**
- [Specific outcome]
- [Impact]

**Priority today:**
- [Single most important task]

**Blockers?**
- [If any, escalate immediately]

**Win of the day:**
- [Something to celebrate]

**Notes:** [Additional context]

---

## CTO
[Same format as CEO]

---

## VP Sales
[Same format as CEO]

---

## CSM
[Same format as CEO]

---

## DECISIONS & ACTIONS

| Decision | Owner | Deadline |
|----------|-------|----------|
| [Decision 1] | [Name] | [Date] |
| [Decision 2] | [Name] | [Date] |

---

## BLOCKERS REQUIRING ESCALATION

**Blocker:** [Name]
- **Owner:** [Who]
- **Impact:** [What's blocked]
- **Mitigation:** [How to fix]
- **Timeline:** [When resolved]
- **Status:** 🔴 CRITICAL / 🟡 HIGH

---

## TOMORROW'S FOCUS

**Top 3 priorities for Day [X+1]:**
1. [Priority 1]
2. [Priority 2]
3. [Priority 3]

---

**Next Standup:** [Tomorrow's date]
```

---

## Weekly Review Format (Friday)

Every Friday includes a 30-minute **Weekly Review** after standup (4:00 PM EST):

### Friday Review Template

```markdown
# WEEKLY REVIEW: Week [N]

📅 **Date:** [Friday date]
⏰ **Time:** 4:00 PM EST (30 min, after standup)
📍 **Attendees:** CEO, CTO, VP Sales, CSM, [optional: board/advisors]

---

## 📊 KPI REVIEW (10 min)

### Revenue & Pipeline
- MRR: $X (Target: $Y) [🟢/🟡/🔴]
- Pipeline: $Z (Target: $A) [🟢/🟡/🔴]
- Win Rate: X% [🟢/🟡/🔴]
- Churn: 0% [🟢/🟡/🔴]

### Product & Engineering
- Features Shipped: X [🟢/🟡/🔴]
- Test Coverage: X% [🟢/🟡/🔴]
- Uptime: X% [🟢/🟡/🔴]
- Engineering Velocity: X story points [🟢/🟡/🔴]

### People & Culture
- Team Size: X FTE [🟢/🟡/🔴]
- Team Energy (avg): X/10 [🟢/🟡/🔴]
- Open Hiring Roles: X [🟢/🟡/🔴]

### Financials
- Burn Rate: $X/week (Budget: $Y) [🟢/🟡/🔴]
- Runway: X weeks [🟢/🟡/🔴]
- Cash Position: $X [🟢/🟡/🔴]

---

## 🎉 WINS & CELEBRATIONS (5 min)

**Highlight of the week:**
- [Major win #1]
- [Major win #2]

**Team recognition:**
- [Person name] - [What they did]
- [Person name] - [What they did]

---

## ⚠️ BLOCKERS & ISSUES (10 min)

**Red flags identified:**
- [Issue 1] - Owner: X - Mitigation: [Plan]
- [Issue 2] - Owner: X - Mitigation: [Plan]

**Next week worries:**
- [Potential risk]
- [Potential risk]

**Resource needs:**
- [If any additional resources needed, specify]

---

## 🎯 NEXT WEEK PREVIEW (5 min)

**Major milestones ahead:**
- [Milestone 1]
- [Milestone 2]
- [Milestone 3]

**Team availability:**
- [Any planned time off?]
- [Any big meetings scheduled?]

**Key success factors for next week:**
- [Factor 1]
- [Factor 2]

---

## DECISIONS MADE THIS WEEK

| Decision | Owner | Deadline |
|----------|-------|----------|
| [Decision 1] | [Name] | [Date] |

---

## FOLLOW-UP ITEMS

| Item | Owner | Due |
|------|-------|-----|
| [Item 1] | [Name] | [Date] |

---

**Notes by:** [CSM name]
**Meeting Recording:** [Link if available]
**Next Standup:** Monday [Date]
```

---

## Week Structure

### Week 1-2 (Jan 27 - Feb 7)
**Theme:** Legal Setup & Product Vision

- Day 1 (Mon, Jan 27): Incorporation starts
- Day 2 (Tue, Jan 28): EIN + bank account
- Day 3 (Wed, Jan 29): Legal review
- Day 4 (Thu, Jan 30): Team meetings
- Day 5 (Fri, Jan 31): **WEEKLY REVIEW** + incorporation complete celebration
- Day 6 (Mon, Feb 3): MVP architecture starts
- Day 7 (Tue, Feb 4): Team hiring ongoing
- Day 8 (Wed, Feb 5): Customer research
- Day 9 (Thu, Feb 6): Tech stack review
- Day 10 (Fri, Feb 7): **WEEKLY REVIEW** + MVP shipped

### Week 3-4 (Feb 10 - Feb 21)
**Theme:** MVP Implementation & Sales Launch

- Days 11-15: MVP features, sales playbook
- Day 15 (Fri): **WEEKLY REVIEW**
- Days 16-20: Sales execution begins, discovery calls
- Day 20 (Fri): **WEEKLY REVIEW** + first POC discussions

### Week 5-6 (Feb 24 - Mar 7)
**Theme:** MVP Hardening & First Customer POC

- Days 21-25: Customer #1 POC starts
- Day 25 (Fri): **WEEKLY REVIEW**
- Days 26-30: Implementation, deal negotiation
- Day 30 (Fri): **WEEKLY REVIEW** + Customer #1 signed

### Week 7-9 (Mar 10 - Mar 28)
**Theme:** Customer #1 Implementation & Additional Sales

- Days 31-35: Customer #1 live, Customer #2 POC
- Day 35 (Fri): **WEEKLY REVIEW**
- Days 36-40: Customers #2, #3 POCs, billing setup
- Day 40 (Fri): **WEEKLY REVIEW**
- Days 41-45: Customer #2, #3 signature, implementation
- Day 45 (Fri): **WEEKLY REVIEW** + $125K ARR milestone

### Week 10-13 (Mar 31 - Apr 25)
**Theme:** Production Hardening, Scaling, Series A Prep

- Days 46-50: All 3 customers live, scaling engineering
- Day 50 (Fri): **WEEKLY REVIEW**
- Days 51-55: Case study creation, Series A deck
- Day 55 (Fri): **WEEKLY REVIEW**
- Days 56-60: Investor outreach, $600K pipeline
- Day 60 (Fri): **WEEKLY REVIEW**
- Days 61-65: Q1 success celebration, Q2 planning
- Day 65 (Fri): **FINAL REVIEW** + $125K ARR + 3 live customers + Series A prep

---

## Instructions for CSM/Note Keeper

### During Standup (15 min)
1. ✅ Have template ready
2. ✅ Note each person's points (use shorthand)
3. ✅ Flag blockers in real-time
4. ✅ Confirm decisions/next steps
5. ✅ Ask clarifying questions if needed

### After Standup (15 min)
1. ✅ Expand shorthand into full sentences
2. ✅ Add context/rationale
3. ✅ Verify facts with team (via Slack)
4. ✅ Post to Slack #standup channel
5. ✅ Update master KPI dashboard if numbers changed

### Before Next Standup (5 min)
1. ✅ Review yesterday's notes for context
2. ✅ Note any overnight blockers
3. ✅ Prepare agenda items
4. ✅ Send reminder to team 30 min before standup

---

## Friday Weekly Review Process

### Before Friday Meeting (30 min prep)
1. ✅ Gather all weekly metrics
2. ✅ Calculate KPIs (revenue, pipeline, velocity)
3. ✅ Identify wins this week
4. ✅ Flag risks/issues
5. ✅ Prepare 1-slide summary for board

### During Friday Meeting (30 min)
1. ✅ 10 min: Review KPIs (highlight green/yellow/red)
2. ✅ 5 min: Celebrate wins + recognize people
3. ✅ 10 min: Discuss blockers + mitigation plans
4. ✅ 5 min: Preview next week's major milestones

### After Friday Meeting (30 min)
1. ✅ Finalize notes
2. ✅ Send Friday email to investors/family with summary
3. ✅ Update EXECUTION_TRACKING_LOGS.md
4. ✅ Schedule any follow-up meetings
5. ✅ Prepare Monday agenda with adjusted priorities

---

## Tracking Metrics (Updated Weekly)

Add to Friday review template:

```
## 📈 CUMULATIVE PROGRESS (13-Week Track)

Days Completed: X of 90
Milestones Hit: X of 13
Revenue: $X (Target by Day 90: $125K)
Customers Signed: X of 3
Customers Live: X of 3
Pipeline: $X (Target by Day 90: $600K)
Team Size: X FTE
Burn Rate Status: 🟢 ON BUDGET / 🟡 SLIGHTLY OVER / 🔴 WAY OVER
```

---

## Tools & Integration

### Slack Integration
```
Post standup notes to #standup channel daily @ 3:30 PM
Format:
  🎯 STANDUP UPDATE: [Date]
  ✅ Shipped: [X]
  → Working on: [Y]
  ⚠️ Blockers: [Z or None]
  🎉 Win: [Something positive]
```

### Email Summary (Weekly)
```
Subject: TAI Weekly Update - Week X

Key Metrics:
- Revenue: $X
- Pipeline: $Y
- Team: Z people
- Burn: $A/week

Top 3 Wins:
1. [Win 1]
2. [Win 2]
3. [Win 3]

Top 3 Risks:
1. [Risk 1]
2. [Risk 2]
3. [Risk 3]

Next Week Focus:
1. [Priority 1]
2. [Priority 2]
3. [Priority 3]
```

---

## Document Versions

| Version | Date | Changes |
|---------|------|---------|
| 1.0 | Jan 26, 2026 | Initial standup system |
| 1.1 | Jan 31, 2026 | Week 1 notes, template refinement |
| 2.0 | Feb 28, 2026 | Quarterly review + adjustments |

---

**Last Updated:** January 26, 2026
**Next Review:** January 31, 2026 (Friday Week 1)
