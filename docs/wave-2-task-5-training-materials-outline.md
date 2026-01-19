# Wave 2, Task 5: Training Materials Outline

**Delivery Timeline**: Weeks 1-2 (complete by end of week 8)
**Format**: Blended (videos, slides, interactive labs, reference materials)
**Audience**: 20 ops engineers + 5 mentors

---

## Overview: What Gets Delivered

| Module | Hours | Primary Format | Key Deliverables | Week |
|--------|-------|-----------------|-----------------|------|
| Module 1: Automation 101 | 8 | Video + Labs | Slides, 2 videos, 4 labs, poster | 9 |
| Module 2: Exception Handling | 12 | Case studies + Labs | Slides, poster, 20 scenarios, workbook | 9-10 |
| Module 3: Process Architecture | 6 | Diagram + Runbook | Architecture diagram, runbook, video | 10 |
| Module 4: Policy Interpretation | 6 | Grammar guide + Examples | Policy guide, 5 annotated policies | 10 |
| Module 5: Tools & Systems | 8 | Demo + Cheat sheets | Slack guide, dashboard video, audit cheat sheet | 10 |
| **Supplementary** | — | Guides | Mentor guide (30 pages), Pilot guide (20 pages) | 8-9 |

**Total Training Hours**: 40 hours (weeks 9-10)
**Delivery Channels**: Internal LMS, email, Slack, in-person mentorship

---

## Module 1: Automation 101 (What ggen Does & Doesn't)

**Duration**: 8 hours (Monday-Tuesday, week 9)
**Goal**: Ops engineer understands ggen boundaries + trusts automation

### Deliverables

#### Slide Deck (PowerPoint)
- **15 slides** | Estimated 90 minutes to present
- Slide 1-3: What is ggen? (Architecture overview)
- Slide 4-6: The 3-step workflow (Template → Generate → Approve → Deploy)
- Slide 7-12: 5 things ggen CANNOT do (with real examples)
- Slide 13-15: Your role in new model + Q&A

**Visuals**:
- Architecture diagram (block diagram, data flow)
- Real ggen output screenshots (deployment config example)
- Before/after: old role vs. new role
- Decision tree: "Can ggen do this?"

**Tone**: Honest, not marketing. Show both power + limitations.

**Delivery**: Presented by mentor Monday morning (60 min) + reference material

---

#### Video Lectures (2x MP4)
- **Video 1: ggen Architecture** (15 min)
  - Topics: 5-stage pipeline (Normalize → Extract → Emit → Canonicalize → Receipt)
  - Audience: any ops engineer (no prerequisites)
  - Format: Screen recording + talking head
  - Closed captions: Yes
  - Hosting: Internal LMS + YouTube (unlisted)

- **Video 2: Live Demo** (15 min)
  - Topics: Deploy microservice end-to-end (real workflow)
  - Show: Policy applied, exception detected, approval flow
  - Format: Screen recording (real ggen system)
  - Closed captions: Yes

**Production**: High-level polish (not home video). Clear audio. Good lighting if talking head.

---

#### Lab Workbook (PDF, 20 pages)
- **Lab 1.1: Workflow Run** (60 minutes, hands-on)
  - Objective: Deploy a sample service using ggen
  - Steps: 1. Set up test environment 2. Create deployment spec 3. Generate config 4. Approve 5. Monitor
  - Success: You can explain each step + answer "What would you do if config violated X?"
  - Format: Step-by-step screenshots + fill-in-the-blank answers

- **Lab 1.2: Boundary Scenarios** (30 minutes, decision-making)
  - Objective: For 5 scenarios, say "ggen handles" or "you handle"
  - Scenarios:
    1. Deployment config exceeds memory limits
    2. Security policy updated, old config violates it
    3. Customer asks for special port number
    4. Performance regression during deployment
    5. New microservice type (not in ggen templates yet)
  - Success: 4/5 correct + reasoning is sound
  - Mentor signs off: "Good thinking"

- **Lab 1.3: Group Discussion** (30 minutes, collaborative)
  - Objective: Small groups (3-4 engineers) discuss: "Where does judgment matter?"
  - Prompts: When would you say no to ggen? When would you modify ggen output?
  - Success: Group identifies ≥3 scenarios where human judgment is essential

- **Lab 1.4: Personal Reflection** (30 minutes, individual)
  - Objective: Write reflection (500 words max)
  - Prompts: "How does this change your job? What excites you? What worries you?"
  - Success: Honest reflection. Mentor discusses with you. Concerns surfaced.
  - Deliverable: Submitted to mentor (personal, not shared)

---

#### Poster (Wall Chart)
- **"ggen Workflow at a Glance"** (11x17, color, laminated)
- Visual: Template → Generate → Policy Check → Approve → Deploy
- Icons for each step (intuitive)
- Three key questions (on back): "Is this ggen's job?" "Is this my judgment?" "What could go wrong?"
- Distribution: One per mentorship group + hung in ops office

---

#### Decision Tree One-Pager (Laminated Reference Card)
- **"Can ggen do it? Decision Tree"**
- Question 1: "Is it routine config?" → YES: ggen. NO: next question
- Question 2: "Does it require customer negotiation?" → YES: you. NO: next question
- Question 3: "Is it a trade-off decision?" → YES: you. NO: ggen
- Examples for each path
- Pocket-sized for quick reference during work

---

## Module 2: Exception Handling (Core 80% of Your Job)

**Duration**: 12 hours (Wednesday-Friday week 9, Monday-Friday week 10)
**Goal**: Ops engineer confidently makes exception decisions + justifies them

### Deliverables

#### Slide Deck (PowerPoint)
- **25 slides** | Estimated 120 minutes to present
- Slides 1-5: Exception taxonomy (7 categories, overview)
- Slides 6-15: Deep dive per category (2-3 slides each with real examples)
  - Policy Violation (slide 6-8)
  - Config Conflict (slide 9-10)
  - Customer Exemption (slide 11-12)
  - Rollback Scenario (slide 13-14)
  - Performance Trade-off (slide 15)
- Slides 16-20: Decision framework (approve/deny/escalate)
- Slides 21-25: Audit trail + escalation paths + wrap-up

**Visual Style**:
- Each category: icon + color + real-world scenario
- Decision framework: flowchart (clear paths)
- Escalation matrix: table showing "who approves what"

---

#### Exception Taxonomy Poster (Wall Chart)
- **"7 Exception Categories"** (11x17, color, laminated)
- Layout: 7 boxes (one per category)
- Each box:
  - Category name (bold)
  - 1-line definition
  - Real example from ops work
  - Frequency (% of exceptions)
  - Typical decision (approve/deny/escalate)
- Distribution: One per mentorship group + ops office

---

#### Lab Workbook (PDF, 50 pages)
- **Lab 2.1: Taxonomy Drill** (90 minutes)
  - Objective: Recognize 7 categories
  - Format: 30 random exceptions, you classify each
  - Examples: "Policy says 'all logs 12mo retention', but this service asks for 6mo" → Policy Violation
  - Success: 28/30 correct (93%+)
  - Self-check key included

- **Lab 2.2: Decision Practice** (90 minutes)
  - Objective: Approve, deny, or escalate (1 per category, increasing difficulty)
  - Format: For each exception: (1) category, (2) decision, (3) 2-sentence justification
  - Example: "Config conflict: service wants port 8080, policy says 8000. Decision? Escalate to service owner. Justification: Service owner owns this trade-off, not ops."
  - Success: Mentor sign-off on 15 decisions (all reasonable + justifications clear)

- **Lab 2.3: Audit Trail Writing** (60 minutes)
  - Objective: Write defensible decision logs (audit trail format)
  - Format: 5 exceptions. For each, write audit entry (100-200 words)
  - What to include: Decision, reasoning, stakeholders involved, trade-offs considered
  - Example format: "[2024-01-18 14:30] Policy Violation: DB retention 6mo vs 12mo policy. Decision: APPROVED (for this customer, 1yr exemption). Reasoning: Customer migration timeline requires 6mo retention. Security reviewed + OK'd. Expires Jan 2025."
  - Success: Mentor sign-off (entries are traceable, would hold up in audit)

- **Lab 2.4: Live Exception Handling** (1-2 hours, supervised)
  - Objective: Handle 5+ real exceptions with mentor watching
  - Format: Real exceptions from Slack during week 10 or week 11 early
  - What mentor evaluates: (1) Did you ask right questions? (2) Did you decide confidently? (3) Did you justify clearly?
  - Success: Mentor sign-off (you handled all 5 correctly)

- **Lab 2.5: Escalation Path Mapping** (30 minutes)
  - Objective: Know exactly who to escalate to, for each category
  - Format: Table: Category → Approval Authority → Name → Email → Slack → Hours
  - Example: "Policy Violation → CTO → Jane Smith → jane@company.com → @cto-oncall → 24/7 on-call"
  - Success: Complete table, no gaps, all contacts verified

---

#### 20 Exception Scenarios Workbook (PDF, 30 pages, perforated)
- **Scenario Set 1-20** (Mixed categories, increasing difficulty)
- Scenario format (for each):
  - Situation (2-3 sentences): "Your service Config X exceeds policy Y. Customer Z says they need it. What do you do?"
  - (a) Category? _______________
  - (b) Decision (approve/deny/escalate)? _______________
  - (c) Justification (2-3 sentences)? _______________
  - (d) Escalation path (if escalating)? _______________
  - Blank space for work

- Answer key (detachable, back of workbook)
  - Mentor uses for grading
  - Can be removed for self-study

- Examples (real scenarios from ops incidents, anonymized):
  - Scenario 5: "Rollback policy says 5min, but prod is in bad state. Incident commander wants 10min to be safe. Your call?"
  - Scenario 14: "Customer exemption: legacy system can't use new API version. Exemption for 12 months?"
  - Scenario 19: "Performance: new logging adds 3% latency. Product team says worth it. Do you approve?"

---

#### Decision Matrix One-Pager (Laminated Card)
- **"When to Approve/Deny/Escalate"**
- Visual flowchart: Start → Question 1 → Question 2 → ... → Decision
- Key decision criteria:
  - **APPROVE**: Low risk, clear exception policy, you have authority
  - **DENY**: High risk, violates core security/SLO, no exception allowed
  - **ESCALATE**: Authority not yours, needs CTO/customer success/security, or edge case
- Examples for each path
- Pocket reference

---

## Module 3: Process Architecture (10% of Your Job)

**Duration**: 6 hours (Tuesday-Wednesday week 10)
**Goal**: Understand HOW the system works + WHY it's designed this way

### Deliverables

#### Architecture Diagram (Wall Poster + Digital)
- **"Exception Flow & Approval Chains"** (11x17, color)
- Flow:
  1. ggen detects exception (in policy check)
  2. Exception notification → Slack (#ops-exceptions)
  3. Ops engineer reviews (in Slack modal)
  4. Decision: approve/deny/escalate
  5. → If approve/deny: recorded in audit log
  6. → If escalate: routed to manager/CTO/customer success (by type)
  7. → Final approval: audit log
  8. → Monthly report: exceptions by category
  9. → Policy team: refines policies based on patterns
  10. → Next month: fewer exceptions (because policy improved)

- Approval chains (nested boxes):
  - Policy violations → CTO (security, compliance)
  - Config conflicts → Service owner (trade-offs)
  - Customer exemptions → Business/customer success (revenue impact)
  - Rollback: Incident commander (time pressure)

- Feedback loop visual: Exceptions → Patterns → Policy refinement → Fewer exceptions

---

#### Process Architecture Runbook (Laminated One-Pager)
- **"Who Approves What? Operational Runbook"**
- Decision matrix:
  | Exception Type | Approval Authority | Name | Email | Slack | Hours |
  | Config Conflict | Service Owner | TBD | TBD | TBD | Business hours |
  | Policy Violation | CTO | Jane Smith | jane@... | @cto-oncall | 24/7 |
  | Customer Exemption | Business Lead | Bob Brown | bob@... | @sales-lead | Business hours |
  | Rollback Override | Incident Commander | On-call | ic@... | @on-call | 24/7 |

- Quick reference: "I have exception type X. Who do I loop in?" → Follow matrix

---

#### Process Video (MP4, 10 min)
- **"How the Exception Flow Works (and Why)"**
- Narration: architect explains design
- Topics:
  - Why escalation chain? (Separation of concerns, accountability)
  - Why Slack? (Speed, visibility, audit trail)
  - Why monthly policy review? (Learn from exceptions, improve policy)
  - Why does this scale? (As volume increases, patterns emerge, automation handles routine approvals)
- Format: Screen recording + talking head (architect at desk, explaining)
- Tone: "This is how we scale ops with AI"

---

## Module 4: Policy Interpretation (10% of Your Job)

**Duration**: 6 hours (Wednesday-Thursday week 10)
**Goal**: Read policy rules + spot ambiguities + contribute to policy refinement

### Deliverables

#### Policy Grammar Guide (PDF, 20 pages)
- **"How to Read ggen Policy Rules"**
- Part 1: Policy YAML Structure (5 pages)
  - Example policy (annotated):
    ```yaml
    name: logging-retention-policy
    scope: all-services
    condition: data_classification == "PCI"
    action: enforce-retention-12-months
    exception:
      allowed: [ "customer-request", "performance-trade-off" ]
      requires-approval: [ "CTO", "business-lead" ]
    ```
  - Explanation: "This policy says: For all services handling PCI data, keep logs 12 months. You can grant exceptions (rare), but requires CTO + business approval."

- Part 2: Translation to English (5 pages)
  - Technique: Read YAML → English sentence
  - Example: "IF scope matches AND condition true THEN action ELSE exception allowed?"
  - Practice: 5 policies, you translate to English

- Part 3: Common Patterns (5 pages)
  - Scope matching (all-services? by-tag? by-team?)
  - Condition matching (data type? performance threshold? customer type?)
  - Action types (enforce, alert, block, audit)
  - Exception patterns (time-limited, approval-required, auto-denied)

- Part 4: Edge Cases & Ambiguities (5 pages)
  - What policies DON'T say (gaps)
  - What happens when two policies conflict?
  - Common ambiguities found in real policies (examples)
  - How to flag ambiguities to policy team

- Appendix: YAML syntax reference (cheat sheet)

---

#### 5 Real Policy Examples (PDF, 15 pages, annotated)
- **Policy Example 1: Logging Retention**
  - YAML policy (full)
  - Side annotations (explain each part)
  - Real scenario (company asks for 6mo instead of 12mo)
  - "What's the right exception decision here?"

- **Policy Example 2: Database Access Control**
  - YAML policy (full)
  - Annotations
  - Real scenario (engineer needs production access temporarily)
  - Question: "Approve exception? Time-limit? Who approves?"

- **Policy Example 3: Configuration Drift Detection**
  - YAML policy (full)
  - Annotations
  - Real scenario (service modifies config post-deployment)
  - Question: "Is this a violation? Or expected behavior?"

- **Policy Example 4: Customer Data Handling**
  - YAML policy (full)
  - Annotations
  - Real scenario (customer wants special data handling)
  - Question: "Approve exception? What questions do you ask?"

- **Policy Example 5: Incident Response Thresholds**
  - YAML policy (full)
  - Annotations
  - Real scenario (during incident, would violate policy to resolve faster)
  - Question: "Override policy? Get approval? Act first, audit later?"

---

## Module 5: Tools & Systems (Operational Fluency)

**Duration**: 8 hours (Thursday-Friday week 10)
**Goal**: Use all tools confidently (Slack, dashboards, audit trail, testing environment)

### Deliverables

#### Slack Workflow Guide (PDF, 10 pages + inline tips)
- **"How to Handle Exceptions in Slack"**
- Part 1: Overview (2 pages)
  - Exception flow: Notification → You review → You decide → Recorded

- Part 2: Step-by-step (5 pages, heavy screenshots)
  - Step 1: Exception notification arrives in #ops-exceptions channel
    - Screenshot: Slack channel with exception card
    - What info is in the card? (Category, severity, service, policy rule, recommendation)
  - Step 2: Click "Review" button
    - Screenshot: Modal opens
    - Fields: Service name, policy violated, ggen recommendation, comments
  - Step 3: Choose decision (radio buttons: Approve / Deny / Escalate)
    - Screenshot: Each option
    - If escalate: Who? (dropdown, auto-filled based on category)
  - Step 4: Write justification (text box, 50-500 characters)
    - Screenshot: Example justifications
    - Tone: Plain English, defensible, brief
    - Template: "[Category] - [Decision] - [Reason] - [If escalated: to whom + why]"
  - Step 5: Click "Submit"
    - Screenshot: Confirmation message
    - What happens next: Decision recorded in audit log, notification sent to relevant parties

- Part 3: Common Mistakes (2 pages)
  - Mistake 1: Vague justification (e.g., "OK"). Result: Audit team confused.
    - Fix: "Policy Violation - APPROVED (customer migration deadline requires 6mo retention vs 12mo policy. CTO + business reviewed + OK'd.)"
  - Mistake 2: Escalating without reason. Result: Manager has to guess.
    - Fix: "Config Conflict - ESCALATE (port conflict between policy 8000 and service request 8080. Service owner should decide.)"
  - Mistake 3: Disapproving without investigation. Result: Looks like you didn't think.
    - Fix: "Customer Exemption - DENIED (requested 12mo exemption, but security policy requires review every 3mo. Suggest: 3-month exemption with monthly review.)"

- Part 4: Slack Tips (1 page)
  - Formatting: Use markdown for clarity
  - Timing: Respond within 2 hours for non-urgent, 30 min for P1
  - Questions: @mention mentor in thread if unsure
  - Escalation: Use thread to keep context (don't lose information)

---

#### Dashboard Video Tour (MP4, 10 min + PDF Guide, 5 pages)
- **Video: "Reading Your Dashboards"**
  - Narration: analyst walks through 4 key dashboards
  - Format: Screen recording (live dashboard system)
  - Closed captions: Yes

- **Dashboard 1: Exception Volume** (3 min)
  - Visual: Line graph over time (by day)
  - Context: "Are exceptions trending up/down? Spike? New category appearing?"
  - What it tells you: "Is the system working? Are policies improving?"
  - Example: "May had spike in policy violations. Why? New policy deployed? Or training issue?" → Investigate

- **Dashboard 2: Your Approval Patterns** (2 min)
  - Visual: Bar chart (approval rate by engineer)
  - Context: "How often do you approve vs. deny?"
  - What it tells you: "Are you too lenient? Too strict? Compared to peers?"
  - Example: "You approve 90% of exemptions. Peers are 70%. Am I being too lenient?"

- **Dashboard 3: Escalation Reasons** (3 min)
  - Visual: Pie chart or bar chart (escalation category breakdown)
  - Context: "What gets escalated most? Is that right?"
  - What it tells you: "Are engineers ready for those decisions? Or do they need training?"
  - Example: "60% of escalations are policy violations. Means Module 4 (policy interpretation) needs improvement." → Mentor gets feedback

- **Dashboard 4: Policy Violation Hotspots** (2 min)
  - Visual: Heatmap (which policies trigger most exceptions)
  - Context: "Which policies need refinement?"
  - What it tells you: "We're seeing a pattern. This policy is too strict / ambiguous / outdated." → Policy team refines

- **PDF Guide** (5 pages):
  - How to access dashboards
  - Common queries ("Show me exceptions for service X", "Show me my decisions in month Y")
  - Interpretation guide (what patterns mean)
  - Red flags (what should trigger investigation)

---

#### Audit Trail Query Cheat Sheet (Laminated One-Pager)
- **"How to Look Up Your Decisions in the Audit Trail"**
- Context: You can query the system to see all YOUR decisions + OTHERS' patterns
- 5 key queries (explained in English, no SQL visible):

  1. **"Show me all my decisions in January"**
     - Result: List of 100+ exceptions you handled, with decisions
     - Use: "What was I thinking about that policy violation?"

  2. **"Show me all policy violations from service X"**
     - Result: All policy violations for one service, sorted by date
     - Use: "Is service X constantly violating? Do they need training? Or is policy wrong?"

  3. **"Show me my decisions where I approved against policy"**
     - Result: Exceptions you approved that violated policy
     - Use: "Did I give too many exemptions? Compare to peers."

  4. **"Show me all escalations by category"**
     - Result: Breakdown: policy violations (40), conflicts (30), exemptions (20)
     - Use: "What's the bottleneck? Do we need more CTO time? More business lead time?"

  5. **"Show me decision patterns: I approve vs. I deny"**
     - Result: Your approval rate (70% approve, 30% deny) vs. team average (65/35)
     - Use: "Am I aligned with team? Am I outlier?"

- Access: Button in dashboard (no SQL needed). Results shown as table.

---

#### Testing Environment Quickstart (PDF, 10 pages + interactive walkthrough)
- **"Welcome to the Testing Environment: Your Safe Practice Space"**
- Part 1: What is this? (2 pages)
  - Testing environment: Replica of production (data, systems), but no real impact
  - Safe: You can approve/deny exceptions, but nothing actually happens
  - Realistic: Same workflow, same decisions, realistic data
  - Goal: Practice until confident → then move to production

- Part 2: How to Access (2 pages)
  - Login URL: _______________
  - Username: your-ops-email
  - Password: _______________
  - (Admin handles access requests)

- Part 3: Mock Exceptions (3 pages)
  - Scenario 1: Policy violation (logging retention)
    - Steps: (1) Review exception in Slack, (2) Decide, (3) Write justification, (4) Submit
    - Expected: Confirmation "Exception recorded in test audit trail"
  - Scenario 2: Config conflict
    - Steps: Similar to Scenario 1
  - Scenario 3: Customer exemption
    - Steps: Similar
  - Your job: Repeat 20 times until confident

- Part 4: Mentor Handoff (2 pages)
  - When are you ready for production? (Mentor assesses after 20 scenarios)
  - Mentor sign-off: "You're ready for week 11"
  - If not ready: More practice + targeted training

- Part 5: Troubleshooting (1 page)
  - Issue: "I can't log in"
  - Issue: "Exception card looks different than training"
  - Issue: "I made a mistake, can I undo it?"
  - Contact: @testing-env-support in Slack

---

## Supplementary Materials

### Mentor Guide (PDF, 30 pages)
**For**: 5 ops leads running training + parallel run mentorship

#### Section 1: Week 9 Training (Week 1 of 2)
- How to run Module 1 (Automation 101)
  - Timeline: Mon-Tue, 8 hours
  - Facilitation tips: Video, then labs, then discussion
  - Watch for: Engineers looking skeptical ("Is this really my job?") → Address fears directly
  - Success: All 4 labs completed + signed off

- How to run Module 2 Weeks 9 Part 1 (Taxonomy)
  - Timeline: Wed-Fri, 12 hours total
  - Facilitation tips: Case studies matter. Use real examples.
  - Watch for: Engineers overwhelmed by 7 categories → Drill 2-3 categories at a time
  - Success: Taxonomy drill 28/30+ correct

#### Section 2: Week 10 Training (Week 2 of 2)
- How to run Module 2 continuation (live exceptions)
  - Timeline: Mon, 4 hours (handle 5 real exceptions with you watching)
  - Facilitation tips: You ask questions, engineer answers. Guide, don't tell.
  - Watch for: Engineer uncertain → Paraphrase decision ("So you're approving because...?")
  - Success: Engineer handled all 5 + mentor sign-off

- How to run Modules 3-5
  - Process architecture (why this way)
  - Policy interpretation (how to read rules)
  - Tools training (hands-on in test environment)
  - Checkpoints: Daily stand-up, Friday reflection

#### Section 3: Week 11 Parallel Run (Mentorship mode)
- Daily rhythm
  - Morning: Engineers do new role (you monitor, available for questions)
  - Afternoon: Engineers do old role (you step back)
  - Evening: 15-min daily sync (what exceptions? Any blockers?)
  - Mon/Wed/Fri: 1-hour office hours (deeper questions)

- Red flags to watch
  - Engineer escalating >30% → Struggling. What category?
  - Engineer frustrated in daily sync → Address fear ("You're doing great")
  - Engineer avoiding certain exception types → Training gap. Extra coaching.
  - Engineer spending >30 min on single exception → Take it as co-mentoring opportunity

- Troubleshooting guide
  - Issue: "I don't know how to decide this exception"
    - Response: Ask guided questions ("What does the policy say?" → "What's the trade-off?" → "Who's the authority?")
  - Issue: "This role is too complex"
    - Response: Reassurance + simpler task allocation + extra training session
  - Issue: "Will automation really replace me?"
    - Response: Honest conversation about role expansion + lead role visibility

#### Section 4: Week 12 Switchover (Leadership mode)
- Your new role (if you're a lead)
  - Mentor other engineers (1-on-1 + group)
  - Design exception taxonomies for your sub-team
  - Shape policy feedback (this category has too many exceptions → policy needs refining)

- Delegating responsibility
  - Which engineers are ready to lead? (Promote 2-3 to co-leads)
  - What decisions can you delegate to them?
  - How to build confidence in junior ops

#### Section 5: Troubleshooting (5 pages)
- Common issues + solutions:
  - "Engineer struggling with decision confidence"
  - "Engineer worried about job security"
  - "Engineer not attending trainings"
  - "Escalation rate spiking"
  - "Incident happens during training"

---

### Pilot Guide (PDF, 20 pages)
**For**: 3 volunteer pilot ops engineers

#### Section 1: You're Special
- Why pilots matter: You're testing the new model for the whole team
- What success looks like: >70% satisfaction + 0 escalation reworks
- What we need from you: Honest feedback + willingness to try

#### Section 2: Week 9-10 Training
- Same curriculum as everyone else (40 hours)
- Extra: 1-on-1 mentor attention
- Goal: Feel confident + ready

#### Section 3: Week 11 Parallel Run
- Your role: 50% new (exceptions) + 50% old (incidents)
- Timeline: Full week (Mon-Fri)
- Metrics: We're tracking satisfaction, escalation quality, SLO impact
- Success: You handle 70%+ exceptions solo

#### Section 4: Week 11 Survey (Friday)
- 5 Likert questions (1-5 scale)
- Open-ended questions (what surprised you? What worried you?)
- Why: Your feedback shapes week 12 for all 20

#### Section 5: Week 12 Onwards (If You Choose)
- Lead role opportunity: Mentor other 17 engineers
- Design policies for your sub-team
- First candidates for platform engineering / SRE
- Career path visibility

#### Section 6: FAQ
- "What if I make a bad exception decision?"
  - Answer: That's why we're piloting. We learn + improve.
- "What if I don't like the new role?"
  - Answer: We want your honest feedback. If role isn't working, we pivot.
- "What if I mess up week 11?"
  - Answer: You won't be alone. Mentor is there. Mistakes OK (within limits).

---

## Delivery & Access

### Hosting
- **LMS (Learning Management System)**: Internal portal
  - All videos, slides, workbooks accessible
  - Progress tracking (who completed what)
  - Discussion forums (questions, peer support)

- **Email**: Checklists + key one-pagers sent to all participants
- **Slack**: Daily reminders, quick reference links
- **Physical**: Posters printed + hung in ops office (wall charts)
- **Cloud Storage**: PDF workbooks (Google Drive link, access for all)

### Timeline
- **Week 1** (design phase): Create materials
- **Week 2** (production phase): Film videos, finalize slides
- **Week 3** (pre-delivery): Upload to LMS, send preview to mentors
- **Week 8** (delivery): All materials ready for mentors to distribute

### Quality Checklist
Before launching:
- [ ] All slides reviewed (by domain expert + ops lead)
- [ ] All videos have closed captions
- [ ] All workbooks field-tested (with 2 pilot mentors)
- [ ] All one-pagers reviewed for clarity
- [ ] All labs have answer keys + mentor guides
- [ ] All materials linked in LMS
- [ ] Mentor training completed (mentors know how to run each module)

---

**Materials Coordinator**: [TBD]
**Production Timeline**: Weeks 1-3 (ready by end of week 8)
**Status**: Ready for development
