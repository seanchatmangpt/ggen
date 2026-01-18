# Wave 2, Task 5: Ops Engineer 2.0 Training & Pilot Program - Design Summary

**Completion Date**: Week 8 (Pre-Launch)
**Designed By**: Change Management Architect
**Status**: Ready for execution (Weeks 9-12)

---

## What We Built

A comprehensive change management program to transition 20 ops engineers from traditional operational roles ("execute deployment configs") to a hybrid human-AI model where they focus on exception handling, policy interpretation, and operational judgment.

### The Challenge

**Gap 7 Execution**: Disney operations team needs to work alongside ggen automation without fear of replacement. They need to understand:
1. What ggen does (and doesn't do)
2. How to handle the 80% of work that requires human judgment (exceptions)
3. How their skills evolve (not disappear)
4. What career path looks like (3 leads emerge)

### The Solution: 4-Week Program (Weeks 9-12)

| Phase | Week | Duration | Participants | Focus |
|-------|------|----------|--------------|-------|
| **Training** | 9-10 | 40 hrs | 20 engineers + 5 mentors | 5 modules: Automation 101, Exception Handling (12hr), Process Arch, Policy Interpretation, Tools |
| **Parallel Run** | 11 | 1 week | All 20 (3 pilots) | New + old role, 50% each. Validate model works. |
| **Switchover** | 12 | 1 week | All 20 | 100% new role. 3 leads mentor others. |

---

## Key Design Principles

### 1. **Real Concerns, Real Answers**

We didn't ignore fears. We addressed them head-on:

- **Fear**: "Will automation replace me?"
  - **Reality**: 80% of new job is exception handling (judgment only humans can make)
  - **Evidence**: Curriculum explicitly teaches what ggen CANNOT do

- **Fear**: "Will my skills become obsolete?"
  - **Reality**: Existing ops wisdom is MORE valuable in exception handling
  - **Evidence**: Module 3 & 4 show how domain knowledge applies to new role

- **Fear**: "Is there a future?"
  - **Reality**: 3 leads chosen from volunteers. Clear path to SRE, platform engineering
  - **Evidence**: Lead role description published before week 9 (transparency)

### 2. **Learning Outcomes Before Delivery**

We designed learning outcomes FIRST, then curricula:
- Module 1: "Explain what ggen does + 3 things it CANNOT do"
- Module 2: "Make confident exception decisions (approve/deny/escalate)"
- Module 3: "Understand the process architecture and why"
- Module 4: "Read policy rules and spot ambiguities"
- Module 5: "Use all operational tools confidently"

Each outcome has a measurable lab + mentor sign-off.

### 3. **Practical Over Theoretical**

- 50% of training is hands-on labs (not lectures)
- 20% is mentorship (1-on-1 guidance)
- 30% is content (videos, slides, reference)
- Real examples from ops work (not hypotheticals)
- Testing environment for safe practice (not production risk)

### 4. **Risking = Visible**

- 3 pilot volunteers are recognized as risk-takers
- Pilots get lead role opportunities (visible career boost)
- Selection criteria published (transparent, fair)
- Success metrics public (pilots know what success looks like)

### 5. **Evidence Over Hope**

- All metrics quantified (>70% satisfaction, 0% rework, <5% SLO impact)
- Go/no-go decision Friday week 11 (data-driven, not gut feel)
- Pilot scorecard with red/yellow/green (clear decision criteria)
- No scaling without evidence of pilot success

---

## Specification Design (TTL Source of Truth)

### File: `/home/user/ggen/.specify/specs/wave2-task5-ops-engineer-2-0/feature.ttl`

**Structure**:
- Feature definition: Ops Engineer 2.0 Training & Pilot Program
- 5 user stories addressing real concerns (job security, skill relevance, career path)
- 5 training modules (40 hours, week 9-10)
- 4-week program roadmap (weeks 9-12)
- Pilot scorecard with 5 metrics
- 6 failure modes + mitigations (FMEA)
- Completion checklist (week-by-week)

**Key Sections**:
1. **Addressing Real Concerns** (3 concern sections with evidence)
2. **User Stories** (5 stories, one for each stakeholder)
3. **Curriculum** (5 modules, 40 hours total)
4. **Program Roadmap** (4 weeks, week-by-week)
5. **Pilot Scorecard** (5 metrics, go/no-go criteria)
6. **Training Materials** (16 deliverables)
7. **FMEA** (6 failure modes + mitigations)
8. **Completion Checklist** (4 weeks of tasks)

---

## Deliverables Created

### 1. RDF Specification
**File**: `/home/user/ggen/.specify/specs/wave2-task5-ops-engineer-2-0/feature.ttl`
- Complete curriculum specification in Turtle format
- 500+ lines of structured RDF
- Source of truth for all derived documents

### 2. Training Roadmap
**File**: `/home/user/ggen/docs/wave-2-task-5-training-roadmap.md`
- Week-by-week execution plan
- Daily schedules for weeks 9-12
- Learning outcomes for each week
- Success metrics at each phase
- Risk mitigation for each phase
- Key dates & deadlines

### 3. Pilot Scorecard
**File**: `/home/user/ggen/docs/wave-2-task-5-pilot-scorecard.md`
- 5 quantified metrics (>70%, 0%, ≤5%, etc.)
- Real-time tracking dashboard template
- Decision matrix (go/no-go criteria)
- Contingency plans (if pilot fails)
- Survey questions + scoring

### 4. Training Materials Outline
**File**: `/home/user/ggen/docs/wave-2-task-5-training-materials-outline.md`
- Detailed outline of 16 deliverables:
  - 5 slide decks (PowerPoint)
  - 4 videos (MP4, closed captions)
  - 5 lab workbooks (PDF)
  - 7 reference cards (posters, one-pagers)
  - 2 supplementary guides (mentor, pilot)
- What each contains, how to use, success criteria

### 5. Evidence Summary (This File)
**File**: `/home/user/ggen/.specify/specs/wave2-task5-ops-engineer-2-0/evidence/PROGRAM_DESIGN_SUMMARY.md`
- Executive overview
- Design principles
- Stakeholder concerns addressed

---

## Critical Success Factors

### 1. Selection of Pilots (Week 8)
- Must be **volunteers** (not assigned)
- Must be **risk-takers** (willing to try new model)
- Must be **respected** (peers watch what they do)
- Recommended: 1 junior, 1 mid-level, 1 senior (diverse)
- **Outcome**: 3 pilots chosen before week 9 kicks off

### 2. Mentor Training (Week 8)
- 5 ops leads trained on:
  - Module-by-module facilitation
  - Red flags to watch (confidence, job fears, escalation spikes)
  - How to support without giving answers
- **Outcome**: Mentors ready to run week 9 training

### 3. Communication Campaign (Week 8)
- All 20 engineers briefed:
  - What's happening (training + pilot + switchover)
  - Why it matters (automation + judgment = better ops)
  - What success looks like (70%+ satisfaction, 0 escalation reworks)
  - What's in it for them (career path, leads chosen from volunteers)
- **Outcome**: Skepticism acknowledged, but buy-in > 50%

### 4. Infrastructure (Week 8)
- Testing environment set up (realistic data, safe practice)
- LMS configured (materials accessible)
- Dashboards live (metrics trackable real-time)
- Slack workflow configured (exception notifications working)
- **Outcome**: All systems ready for week 9 go-live

---

## How We Address the "Real Concerns"

### Concern 1: Job Security ("Will automation replace me?")

**Our Answer**: No. 80% of new job is exception handling (judgment only humans can make).

**Evidence We Provide**:
- Module 1: Explicit teaching of ggen boundaries ("ggen CANNOT decide customer exemptions, policy trade-offs, etc.")
- Module 2 (12 hours): Exception handling = 80% of new job (focus here shows importance)
- Real exceptions from ops work (show pilots these are judgment calls)
- Lead role visibility (3 leads emerge from volunteers)

**We Check**: Week 10 survey: "Do you feel your job is secure?" Must be >3.5/5

**Escalation**: If concern remains, 1-on-1 with manager + HR (career conversation)

---

### Concern 2: Skill Obsolescence ("Will my expertise disappear?")

**Our Answer**: Your skills evolve, not disappear. Ops wisdom is MORE valuable.

**Evidence We Provide**:
- Module 3 & 4 explicitly map ops expertise to new role:
  - "Deployment patterns knowledge → Exception rule design"
  - "Config troubleshooting → Policy interpretation"
  - "Incident judgment → Exception handling (same judgment, new context)"
- Mentor 1-on-1: "How does YOUR expertise apply to exceptions?"
- Module 2: Exception scenarios use real ops problems (not abstract)

**We Check**: Week 10 survey: "Do you see your skills valued?" Must be >3.5/5

**Escalation**: If concern remains, career development conversation

---

### Concern 3: Career Path ("Is there a future?")

**Our Answer**: Yes. 3 leads chosen from volunteers. Clear path visible.

**Evidence We Provide**:
- Lead role description published PRE-launch (transparency)
- Selection criteria published (fair, merit-based)
- Lead responsibilities: design policies, mentor others, shape org
- Lead as first candidate for platform engineering, SRE roles
- 3 leads visible during week 12 onwards (not hidden)

**We Check**: Week 10 survey: "Do you see a path forward?" Must be >3.5/5

**Escalation**: If concern remains, manager discusses career options

---

## Week 11 Pilot Success Metrics

### Metric 1: Exception Handling Rate (>70%)
- Pilots handle >70% of exceptions solo (approve/deny without escalation)
- If FAIL: Exception handling too complex or training insufficient

### Metric 2: Pilot Satisfaction (>70%, survey 3.5+/5)
- Pilots rate: role achievable, decisions confident, job secure, future clear
- If FAIL: Real concerns not addressed (escalate to management)

### Metric 3: Escalation Quality (0 rework)
- Zero pilot escalations are overturned by manager
- If FAIL: Pilot judgment not sound (training gap identified)

### Metric 4: Incident Response SLO (≤5% slower)
- P1/P2 response times not significantly impacted by new role
- If FAIL: New role distracts from incidents (reduce exception volume week 12)

### Metric 5: Retention (0 attrition)
- No pilots resign or transfer
- If FAIL: Role unsustainable (investigate, fix before scaling to 20)

**Go/No-Go Decision** (Friday Week 11, 5pm):
- All 5 pass → SCALE to 20 (week 12 switchover)
- 4/5 pass → GO with extra support for weak area
- 3/5 pass → CONDITIONAL (extra training Mon-Wed week 12, re-assess Wed)
- <3/5 pass → NO-GO (pause, investigate, redesign)

---

## FMEA Highlights (Risk Management)

We identified 6 failure modes + mitigations:

1. **FM1: Engineer feels replaced** (severity 9)
   - Mitigation: Explicit framing in Module 1 (ggen does routine, you do exceptions)
   - Detection: Week 10 survey question
   - Reaction: 1-on-1 career conversation if concern surfaces

2. **FM2: Exception handling too complex** (severity 8)
   - Mitigation: 12 hours of Module 2 (extensive practice)
   - Detection: Week 11 daily check-in (escalation rate >30% = struggling)
   - Reaction: Extra mentorship + task simplification

3. **FM3: Old process faster** (severity 6)
   - Mitigation: Set expectations (week 11 is learning curve, will be slower)
   - Detection: Week 11 metrics (track time-to-approval)
   - Reaction: Show dashboards (new role is measurable, will improve)

4. **FM4: Pilot fails** (severity 10)
   - Mitigation: Careful pilot selection (volunteers, risk-takers)
   - Detection: Week 11 daily metrics (escalation rate, satisfaction)
   - Reaction: If failure confirmed Friday, DON'T SCALE (investigate)

5. **FM5: Skills seem obsolete** (severity 7)
   - Mitigation: Modules 3 & 4 map existing skills to new role
   - Detection: Week 10 survey
   - Reaction: Manager career conversation

6. **FM6: Career path unclear** (severity 6)
   - Mitigation: Lead role description published PRE-launch
   - Detection: Week 10 survey
   - Reaction: Manager discusses growth opportunities

---

## Why This Works (The Philosophy)

### We Don't Assume
- We don't assume ops engineers will blindly accept ggen
- We don't assume "new role" is obvious
- We don't assume concerns will go away
- **We investigate, then address**

### We Make Real Tradeoffs Clear
- What ggen saves you from: typing configs, manual deployment
- What ggen adds to your plate: exception decisions, policy interpretation
- Net effect: More judgment, less grunt work (not replacement, evolution)

### We Make Success Visible
- 3 leads chosen from volunteers (risk takers get visible rewards)
- Metrics public (everyone knows what "success" looks like)
- Dashboards live (engineers can see their work's impact)
- Mentors present (not alone in changes)

### We Validate Before Scaling
- Week 11 pilots test model for real
- Go/no-go decision based on data (not hope)
- If pilots fail, we investigate (not proceed)
- If pilots succeed, we scale with confidence

---

## Next Steps (Week 8 Checklist)

Before week 9 training kicks off:

- [ ] **Design Finalized**: Stakeholders approve curriculum + metrics
- [ ] **Pilots Selected**: 3 volunteers identified + briefed
- [ ] **Mentors Trained**: 5 ops leads trained on week 9-12 cadence
- [ ] **Materials Created**: All 16 deliverables ready (slides, videos, labs)
- [ ] **Systems Ready**: Testing environment, LMS, dashboards, Slack workflow
- [ ] **Communication**: All 20 engineers briefed (concerns acknowledged)
- [ ] **Leads Published**: Lead role description published (transparency)
- [ ] **Evidence Dir**: Scorecard template, survey template ready

---

## Success Criteria (Wave 2 Exit Gate)

**Training (Weeks 9-10)**: ✓
- 20 engineers trained 40 hours
- All labs completed + mentor signed off
- Post-training assessment >80% passing
- No engineer reports unprepared

**Pilot (Week 11)**: ✓
- 3 pilots complete 1-week parallel run
- Pilots handle >70% exceptions without escalation
- 0 escalations reworked
- SLO maintained (incident response not degraded)
- >70% satisfaction (survey >3.5/5)

**Switchover (Week 12)**: ✓
- All 20 on new role (0% old role)
- 3 leads mentoring others
- Escalation rate stable/declining
- SLOs maintained
- 0 attrition
- >70% confident in new model (post-survey)

**If All Green**: Scale to other operational teams (post-Wave 2)

---

## Evidence Artifacts

### In This Directory
- `PROGRAM_DESIGN_SUMMARY.md` (this file)
- Pilot scorecard dashboard template (TBD, week 11)
- Post-pilot survey results (TBD, Friday week 11)
- Lessons learned report (TBD, after week 12)

### In RDF Specification
- `/home/user/ggen/.specify/specs/wave2-task5-ops-engineer-2-0/feature.ttl`

### In Documentation
- `/home/user/ggen/docs/wave-2-task-5-training-roadmap.md`
- `/home/user/ggen/docs/wave-2-task-5-pilot-scorecard.md`
- `/home/user/ggen/docs/wave-2-task-5-training-materials-outline.md`

---

## Conclusion

This program is **designed to be believable**, not just compliant.

- It addresses real concerns (job security, skill relevance, career)
- It shows real transformation (80% exception handling = real work)
- It validates with data (pilot metrics, not stories)
- It rewards risk-takers (3 leads emerge from volunteers)
- It has a clear path forward (if pilots succeed, scale; if fail, investigate)

**Ready for execution Week 9.**

---

**Program Owner**: [Change Management Lead]
**Design Completion**: Week 8
**Execution Dates**: Weeks 9-12
**Status**: ✓ Ready to Launch
