<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Wave 2, Task 5: Ops Engineer 2.0 Training & Pilot Program Roadmap](#wave-2-task-5-ops-engineer-20-training--pilot-program-roadmap)
  - [Executive Summary: The Big Picture](#executive-summary-the-big-picture)
  - [Phase 1: Training (Weeks 9-10, 40 hours total)](#phase-1-training-weeks-9-10-40-hours-total)
    - [Week 9: Foundation (20 hours)](#week-9-foundation-20-hours)
    - [Week 10: Deep Skills + Tools (20 hours)](#week-10-deep-skills--tools-20-hours)
  - [Phase 2: Parallel Run (Week 11)](#phase-2-parallel-run-week-11)
    - [Week 11 Schedule](#week-11-schedule)
    - [Success Metrics (Week 11)](#success-metrics-week-11)
    - [Friday Week 11 Decision Point](#friday-week-11-decision-point)
  - [Phase 3: Full Switchover (Week 12)](#phase-3-full-switchover-week-12)
    - [Week 12 Schedule](#week-12-schedule)
    - [New Ops Engineer Org Chart (Week 12 onwards)](#new-ops-engineer-org-chart-week-12-onwards)
    - [Success Metrics (Week 12)](#success-metrics-week-12)
  - [Training Materials Delivered](#training-materials-delivered)
    - [Module 1: Automation 101 (8 hours, Week 9)](#module-1-automation-101-8-hours-week-9)
    - [Module 2: Exception Handling (12 hours, Week 9-10)](#module-2-exception-handling-12-hours-week-9-10)
    - [Module 3: Process Architecture (6 hours, Week 10)](#module-3-process-architecture-6-hours-week-10)
    - [Module 4: Policy Interpretation (6 hours, Week 10)](#module-4-policy-interpretation-6-hours-week-10)
    - [Module 5: Tools & Systems (8 hours, Week 10)](#module-5-tools--systems-8-hours-week-10)
    - [Supplementary Materials](#supplementary-materials)
  - [How We Address Real Concerns](#how-we-address-real-concerns)
    - [Concern 1: "Will automation replace me?"](#concern-1-will-automation-replace-me)
    - [Concern 2: "Will my skills become obsolete?"](#concern-2-will-my-skills-become-obsolete)
    - [Concern 3: "Is there a future for me in this role?"](#concern-3-is-there-a-future-for-me-in-this-role)
  - [Key Dates & Deadlines](#key-dates--deadlines)
  - [Success Criteria (Exit Gate for Wave 2)](#success-criteria-exit-gate-for-wave-2)
  - [Risks & Mitigation](#risks--mitigation)
  - [How to Use This Roadmap](#how-to-use-this-roadmap)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Wave 2, Task 5: Ops Engineer 2.0 Training & Pilot Program Roadmap

**Execution Period**: Weeks 9-12 (4 weeks total)
**Participants**: 20 ops engineers + 5 mentors (ops leads)
**Pilots**: 3 volunteer risk-takers
**Goal**: Transition ops team from traditional operational model to hybrid human-AI model with ggen automation

---

## Executive Summary: The Big Picture

We are not replacing ops engineers. We are **expanding their role** from "execute deployment configs" to "decide exceptions" and "shape policy."

**The Real Workload**:
- Old role: 100% config execution
- New role: 80% exception handling + 10% process architecture + 10% policy interpretation

**Why this matters**: Exception handling requires human judgment. ggen handles routine. You handle edge cases. You're not replaced; you're redeployed.

---

## Phase 1: Training (Weeks 9-10, 40 hours total)

### Week 9: Foundation (20 hours)

**Daily Schedule** (Monday-Friday, 4 hours/day):
- All 20 engineers + 5 mentors participate
- Blended format: video lectures (30%), interactive labs (50%), mentorship (20%)

| Day | Focus | Modules | Deliverables |
|-----|-------|---------|--------------|
| **Monday** | Automation Fundamentals | Module 1: Automation 101 (morning: video + demo, afternoon: labs 1-3) | Completed labs 1-3 |
| **Tuesday** | Automation + Exception Taxonomy | Finish Module 1 labs, Module 2 start (taxonomy) | Taxonomy poster on wall |
| **Wednesday** | Exception Deep Dives | Module 2: Policy Violations + Config Conflicts | Decision tree worksheet |
| **Thursday** | Exception Deep Dives | Module 2: Customer Exemptions + Rollback Scenarios | Practical framework |
| **Friday** | Capstone + Reflection | Module 2: 20 exception scenarios + lab review | Week 1 reflection submitted |

**Learning Outcomes (Week 9)**:
- [x] Explain what ggen does + 3 things it can't do
- [x] Recognize 7 exception categories (policy violation, config conflict, etc.)
- [x] Practice exception decisions (approve/deny/escalate) with mentor feedback
- [x] Understand: exceptions are where your judgment matters

---

### Week 10: Deep Skills + Tools (20 hours)

**Daily Schedule** (Monday-Friday, 4 hours/day):
- Continued mentorship + hands-on labs
- Introduction to tools + operational systems

| Day | Focus | Modules | Deliverables |
|-----|-------|---------|--------------|
| **Monday** | Live Exception Handling | Module 2 continuation: 5 real exceptions with mentor observing | Mentor sign-off on handling |
| **Tuesday** | Process + Policy | Module 3: Process Architecture + Module 4 start: Policy Interpretation | Process diagram study |
| **Wednesday** | Policy Deep Dive | Module 4: Read policies, spot ambiguities, write feedback | Policy feedback submitted |
| **Thursday** | Tools Introduction | Module 5: Slack workflow, dashboards, audit trail, testing environment | Tools walkthrough complete |
| **Friday** | Final Lab Sprint | Module 5: 20 mock exceptions in testing environment with mentor review | Testing environment sign-off |

**Learning Outcomes (Week 10)**:
- [x] Handle 5+ real exceptions with mentor, make sound decisions
- [x] Understand policy language (YAML + business logic) + spot edge cases
- [x] Use Slack workflow, dashboards, audit trail system
- [x] Practice exception handling in safe testing environment
- [x] **Readiness Assessment**: "Ready for production week 11?" Y/N

---

## Phase 2: Parallel Run (Week 11)

**Structure**: New role + old role, side-by-side (50% each)

### Week 11 Schedule

**Participants**: All 20 engineers (including 3 pilot volunteers)

**Daily Rhythm**:
- **Morning (2 hours)**: New role (handle exceptions, make decisions, document in Slack)
- **Afternoon (2 hours)**: Old role (execute deploy configs, handle on-call incidents)
- **Evening (15 min)**: Daily sync with mentor (what exceptions did you handle? Any blockers?)

**Checkpoints**:
- **Monday 10am**: Kick-off sync. Mentor explains: "You're testing this model for the whole team. Success = >70% satisfaction + 0 escalation reworks."
- **Mon/Wed/Fri 2pm**: Mentor office hours (1 hour). Questions, concerns, escalation guidance.
- **Friday 5pm**: Weekly wrap-up (30 min). What went well? What was hard?

### Success Metrics (Week 11)

| Metric | Target | How Tracked | What If It Fails |
|--------|--------|-------------|-----------------|
| **Exception Handling Rate** | >70% of exceptions handled without escalation | Count solo decisions vs. total | Increase mentorship, review training gaps |
| **Escalation Quality** | 0% rework (no pilot decisions overturned) | Did manager say "actually you should have..."? | Mentor pilots on that category |
| **Pilot Satisfaction** | >70% (survey avg >3.5/5) | Friday survey (5 questions) | Investigate specific concerns |
| **Incident Response SLO** | No degradation (≤5% slower than baseline) | P1/P2 response times week 11 vs. historical | Pilots need more support during incidents |
| **Retention** | 0 attrition (no one quits) | Check: anyone resigned? | Understand why, address before scaling |

### Friday Week 11 Decision Point

**Go/No-Go on Full Switchover (Week 12)**:
- If ALL 5 metrics pass → scale to all 20 ✓
- If 3/5 pass, 2/5 close → short pivot + extra training + retry
- If ANY metric fails significantly → pause, investigate, do not scale

---

## Phase 3: Full Switchover (Week 12)

**Structure**: All 20 engineers fully on new role. 3 leads mentor others.

### Week 12 Schedule

| Day | Focus | Context |
|-----|-------|---------|
| **Monday** | Old role deprecated | All 20 engineers 100% on new role. Old process no longer used. |
| **Mon-Fri** | 3 Leads Mentoring | 3 pilot volunteers (selected from volunteers) mentor other 17 engineers |
| **Daily 15min** | Sync | How's the transition? Escalations? Blockers? |
| **Friday 2pm** | Weekly wrap-up | Lessons learned + discussion |

### New Ops Engineer Org Chart (Week 12 onwards)

```
Operations Director
├─ Lead #1 (Pilot volunteer)
│  ├─ 5-6 ops engineers (mentored)
├─ Lead #2 (Pilot volunteer)
│  ├─ 5-6 ops engineers (mentored)
├─ Lead #3 (Pilot volunteer)
│  └─ 4-5 ops engineers (mentored)
```

**Lead Role**: Visible, expandable position
- Design exception taxonomies for your team
- Mentor other engineers (1-on-1 + group)
- Refine policies based on exception patterns
- First candidates for platform engineering, SRE roles

### Success Metrics (Week 12)

| Metric | Target | Verification |
|--------|--------|--------------|
| **Full Adoption** | 0% old role (100% on new role) | All 20 engineers routing exceptions through Slack |
| **Escalation Stability** | Rate stable or declining | Dashboard: escalations trending down or flat |
| **SLO Maintained** | Incident response ≤30min (P1), ≤4hr (P2) | Post-week 12 audit: no SLO breaches |
| **Retention Baseline** | 0 attrition | Anyone resign or transfer? No. |
| **Team Confidence** | >70% ready for long-term (post-switchover survey) | "I'm confident in this new model" >3.5/5 |

---

## Training Materials Delivered

### Module 1: Automation 101 (8 hours, Week 9)
- **Slide Deck**: 15 slides with visuals + real ggen output examples
- **Videos**: 2x 15-minute lectures (architecture + live demo)
- **Lab Workbook**: 4 labs (workflow, boundary scenarios, group discussion, reflection)
- **Poster**: ggen workflow diagram (hung in ops office)
- **Checklist**: "Can ggen do it?" decision tree (one-pager)

**You'll Be Able To**: Explain what ggen does + 3 things it CANNOT do + why exceptions matter

---

### Module 2: Exception Handling (12 hours, Week 9-10)
- **Slide Deck**: 25 slides covering 7 exception categories + decision framework
- **Exception Taxonomy Poster**: 7 categories, real-world examples
- **Lab Workbook**: Taxonomy drill, decision practice, audit trail writing (20+ scenarios)
- **Decision Matrix**: When to approve/deny/escalate (one-pager)
- **Case Studies**: 20 mixed exception scenarios (low to high complexity)
- **Audit Trail Template**: How to justify your decisions (for legal + learning)

**You'll Be Able To**: Recognize exceptions + make confident decisions (approve/deny/escalate) + write defensible justifications

---

### Module 3: Process Architecture (6 hours, Week 10)
- **Architecture Diagram**: How exceptions flow through system (approval chains, feedback loops)
- **Runbook**: "Who approves what?" (one-pager, laminated)
- **Process Video**: Why design is this way (30 minutes)

**You'll Be Able To**: Understand the "why" behind the process + know escalation paths

---

### Module 4: Policy Interpretation (6 hours, Week 10)
- **Policy Grammar Guide**: How to read YAML policies (20 pages, accessible to non-programmers)
- **5 Annotated Policy Examples**: Real policies from your environment with edge cases highlighted
- **Edge Case Checklist**: Common ambiguities to watch for

**You'll Be Able To**: Read policy rules + spot conflicts + propose clarifications

---

### Module 5: Tools & Systems (8 hours, Week 10)
- **Slack Workflow Guide**: Step-by-step screenshots + common mistakes
- **Dashboard Video Tour**: 10-minute narrated walkthrough of 4 key dashboards
- **Audit Trail Cheat Sheet**: 5 key queries explained in English (one-pager)
- **Testing Environment Quickstart**: Login, mock data, practice flow

**You'll Be Able To**: Use all operational systems confidently (no IT support needed for routine tasks)

---

### Supplementary Materials
- **Mentor Guide**: 30 pages for ops leads running training + parallel run
- **Pilot Guide**: 20 pages for 3 volunteer pilots (what success looks like, what we need from you)

---

## How We Address Real Concerns

### Concern 1: "Will automation replace me?"

**Answer**: No. 80% of new job is exception handling (judgment calls ggen can't make).

**Evidence**:
- ggen handles routine deployment configs (same work you did, now automated)
- You handle exceptions: policy violation detected → you approve/deny + justify
- You decide edge cases: customer exemption? Yes/no/escalate?
- You teach: mentor new engineers, review decisions, shape policies

**What Success Looks Like**:
- Week 9-10: You'll see real ggen output + understand boundaries
- Week 11: You'll handle exceptions + feel the difference (judgment > execution)
- Week 12 onwards: Your job expands; you're architect of exceptions, not typist

---

### Concern 2: "Will my skills become obsolete?"

**Answer**: Your skills evolve, not disappear. Core ops wisdom stays valuable.

**Evidence**:
- Deep deployment knowledge → now: exception rule design
- Config patterns → now: policy interpretation
- Incident judgment → now: exception handling (same judgment, new context)
- Automation literacy → career accelerator for SRE, platform eng

**What Success Looks Like**:
- Week 9: You'll map your existing skills to new role (mentor discussion)
- Week 10: You'll practice applying your wisdom to exception scenarios
- Week 12 onwards: 3 leads design policies + mentor others (visible career path)

---

### Concern 3: "Is there a future for me in this role?"

**Answer**: Yes. 3 leads chosen from volunteers. Leads design policy + mentor others.

**Evidence**:
- Leads shape organizational policies (influence across all teams)
- Leads mentor engineers (respect, mentorship opportunity)
- Leads are first candidates for platform engineering, SRE, policy architecture roles
- Lead role is visible + valuable

**What Success Looks Like**:
- Week 9: Lead role description published. You see the path.
- Week 11: 3 volunteers identified (risk takers get visible roles)
- Week 12 onwards: Leads mentor others + design new policies

---

## Key Dates & Deadlines

| Date | Milestone | Owners |
|------|-----------|--------|
| **End Week 8** | Curriculum finalized + materials delivered to mentors | Program PM + Training Team |
| **Monday Week 9** | Training begins (20 engineers + 5 mentors) | All |
| **Friday Week 9** | Week 9 learnings reflection submitted | All engineers |
| **Friday Week 10** | Post-training assessment (>80% passing) + readiness sign-off | All engineers + mentors |
| **Monday Week 11** | Parallel run kicks off (new + old role, 50% each) | All 20 engineers |
| **Friday Week 11** | Pilot satisfaction survey + go/no-go decision on week 12 | 3 pilots + PM |
| **Monday Week 12** | Full switchover (100% new role) | All 20 engineers |
| **Friday Week 12** | Post-switchover survey + lessons learned discussion | All engineers + mentors + leadership |
| **Following Week** | Post-program report (what worked, what to improve) | Program PM |

---

## Success Criteria (Exit Gate for Wave 2)

**Training (Weeks 9-10)**:
- [x] 20 engineers trained (40 hours) with >85% attendance
- [x] All training modules completed + labs signed off
- [x] Post-training assessment >80% passing
- [x] No engineer reports feeling unprepared (survey >3.5/5)

**Pilot (Week 11)**:
- [x] 3 pilots complete 1-week parallel run
- [x] Pilots handle >70% exceptions without escalation
- [x] 0 escalations reworked (sound judgment)
- [x] Incident response SLO maintained (≤5% variance)
- [x] Pilot satisfaction >70% (survey >3.5/5)

**Switchover (Week 12)**:
- [x] All 20 engineers fully on new role (0% old role)
- [x] 3 leads mentoring others + designing policies
- [x] Escalation rate stable or declining
- [x] Operational SLOs maintained
- [x] 0 attrition (retention baseline established)
- [x] Post-switchover survey >70% confident in new model

---

## Risks & Mitigation

| Risk | Severity | Mitigation |
|------|----------|-----------|
| Engineers feel replaced | HIGH | Module 1: Explicit framing (80% exceptions). Week 11 mentor 1-on-1 addressing fears. |
| Exception handling too complex | HIGH | Module 2 (12 hours): extensive practice + live labs with mentors. Week 11 daily check-ins. |
| Old process faster | MEDIUM | Set expectations: learning curve is OK. Show dashboards + metrics to demonstrate value. |
| Pilot week fails | HIGH | Select pilots carefully (volunteers, risk-takers). Extra mentorship week 9-10. Daily metrics week 11. |
| Skill obsolescence fear | MEDIUM | Module 3: map existing skills to new role. Mentor 1-on-1 career conversations. Lead role visibility. |
| Career path unclear | MEDIUM | Lead role description published pre-launch. Clear selection criteria. Visibility to HR + managers. |

---

## How to Use This Roadmap

1. **Week 8**: Mentors review roadmap + confirm prep work done
2. **Week 9**: Follow week-by-week schedule + hit daily targets
3. **Week 10**: Run final labs + readiness assessments
4. **Week 11**: Track metrics daily + make go/no-go decision Friday
5. **Week 12**: Execute switchover + collect feedback

---

## Next Steps

1. **Confirm 3 pilot volunteers** (identify by end of week 8)
2. **Finalize training materials** (slides, videos, labs ready for mentors)
3. **Brief 5 mentors** (run mentor training on week 9-12 cadence)
4. **Set up testing environment** (accessible, realistic data, safe practice)
5. **Launch communication campaign** (address concerns proactively)
6. **Create evidence directory** (track surveys, metrics, lessons learned)

---

**Contact**: [Program PM]
**Last Updated**: Week 8
**Status**: Ready for execution
