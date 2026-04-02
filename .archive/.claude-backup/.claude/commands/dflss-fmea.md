# FMEA (Failure Mode and Effects Analysis) - Multi-Step Workflow

## Purpose

This command guides agents through Failure Mode and Effects Analysis (FMEA), a systematic method for identifying potential failures in processes or systems, assessing their severity, frequency, and detectability, and prioritizing them using Risk Priority Number (RPN). FMEA prevents problems before they occur by identifying and addressing failure modes proactively.

## Workflow Overview

```
Step 1: Define Scope → Step 2: Identify Failure Modes → Step 3: Assess Severity → Step 4: Assess Frequency → Step 5: Assess Detection → Step 6: Calculate RPN → Step 7: Prioritize and Fix
```

## Step-by-Step Instructions

### Step 1: Define Scope

**Action**: Clearly define what process or system is being analyzed.

**Scope definition format**:
- **What**: What process/system is being analyzed?
- **Boundaries**: What's included? What's excluded?
- **Context**: What's the operational context?
- **Goal**: What are we trying to prevent?

**Example scope definition**:
```markdown
## FMEA Scope

**What**: Release preparation workflow
**Boundaries**: 
- Included: Release preparation steps, validation checks, release artifacts
- Excluded: Post-release monitoring, deployment process
**Context**: Preparing v1.1.0 release for production
**Goal**: Prevent release failures (incomplete releases, uncommitted code, missing artifacts)
```

**Principle**: Clear scope ensures focused analysis and prevents scope creep.

---

### Step 2: Identify Failure Modes

**Action**: Identify all potential ways the process/system can fail.

**Failure mode format**:
- **Failure mode**: How can it fail? (What goes wrong?)
- **Component/Step**: Where does it fail? (Which component/step?)
- **Description**: What does the failure look like?

**Failure mode identification techniques**:
1. **Brainstorming**: List all possible failures
2. **Historical data**: Review past failures
3. **Process walkthrough**: Step through process, identify failure points
4. **Expert knowledge**: Consult experts on common failures
5. **Checklists**: Use failure mode checklists

**Example failure modes**:
```markdown
## Failure Modes

**Failure Mode 1**: Release readiness declared despite uncommitted changes
- **Component**: Release preparation workflow
- **Step**: Final validation
- **Description**: Workflow declares "READY" when git state has uncommitted changes

**Failure Mode 2**: Missing release artifacts (CHANGELOG, release notes)
- **Component**: Release preparation workflow
- **Step**: Artifact creation
- **Description**: Release proceeds without required artifacts

**Failure Mode 3**: Tests pass but code has compilation errors in different configuration
- **Component**: Test validation
- **Step**: Test execution
- **Description**: Tests pass in default config but fail in release config
```

**Principle**: Identify all failure modes, not just obvious ones. Use multiple techniques to ensure completeness.

---

### Step 3: Assess Severity

**Action**: Assess the severity of each failure mode's impact.

**Severity scale** (1-10, where 10 is most severe):

| Rating | Severity | Description | Example |
|--------|----------|-------------|---------|
| 10 | Catastrophic | Complete system failure, safety hazard | Release breaks production, data loss |
| 9 | Critical | Major system failure, significant impact | Release causes widespread outages |
| 8 | Serious | System degraded, major functionality lost | Release breaks critical features |
| 7 | Major | System degraded, significant functionality lost | Release breaks important features |
| 6 | Moderate | System degraded, some functionality lost | Release breaks minor features |
| 5 | Minor | System degraded, minimal functionality lost | Release causes minor issues |
| 4 | Low | System degraded, very minor impact | Release causes cosmetic issues |
| 3 | Very Low | System degraded, negligible impact | Release causes documentation issues |
| 2 | Negligible | System degraded, barely noticeable | Release causes minor formatting issues |
| 1 | None | No impact | No noticeable impact |

**Severity assessment questions**:
- What is the worst-case impact if this failure occurs?
- How many users/systems are affected?
- What is the business impact?
- Is there a safety hazard?

**Example severity assessment**:
```markdown
## Severity Assessment

**Failure Mode 1**: Release readiness declared despite uncommitted changes
- **Severity**: 9 (Critical)
- **Rationale**: Could release incomplete/uncommitted code, breaking production, causing widespread outages

**Failure Mode 2**: Missing release artifacts
- **Severity**: 6 (Moderate)
- **Rationale**: Users can't understand changes, but system still works

**Failure Mode 3**: Tests pass but code has compilation errors in different configuration
- **Severity**: 8 (Serious)
- **Rationale**: Release breaks for users with different configurations
```

**Principle**: Assess severity based on worst-case impact, not typical impact.

---

### Step 4: Assess Frequency (Occurrence)

**Action**: Assess how frequently each failure mode occurs or is likely to occur.

**Frequency scale** (1-10, where 10 is most frequent):

| Rating | Frequency | Description | Example |
|--------|-----------|-------------|---------|
| 10 | Very High | Almost certain, >50% of time | Failure occurs in >50% of releases |
| 9 | High | Very likely, 30-50% of time | Failure occurs in 30-50% of releases |
| 8 | Moderate-High | Likely, 20-30% of time | Failure occurs in 20-30% of releases |
| 7 | Moderate | Occasional, 10-20% of time | Failure occurs in 10-20% of releases |
| 6 | Low-Moderate | Unlikely, 5-10% of time | Failure occurs in 5-10% of releases |
| 5 | Low | Rare, 2-5% of time | Failure occurs in 2-5% of releases |
| 4 | Very Low | Very rare, 1-2% of time | Failure occurs in 1-2% of releases |
| 3 | Remote | Extremely rare, 0.5-1% of time | Failure occurs in 0.5-1% of releases |
| 2 | Very Remote | Almost never, 0.1-0.5% of time | Failure occurs in 0.1-0.5% of releases |
| 1 | Extremely Remote | Nearly impossible, <0.1% of time | Failure occurs in <0.1% of releases |

**Frequency assessment questions**:
- How often has this failure occurred historically?
- How likely is this failure given current controls?
- What is the probability of occurrence?
- Are there contributing factors that increase frequency?

**Example frequency assessment**:
```markdown
## Frequency Assessment

**Failure Mode 1**: Release readiness declared despite uncommitted changes
- **Frequency**: 7 (Moderate)
- **Rationale**: Occurs when workflow doesn't check git state (happened once, but likely to recur without fix)

**Failure Mode 2**: Missing release artifacts
- **Frequency**: 4 (Very Low)
- **Rationale**: Rare, but can occur if checklist incomplete

**Failure Mode 3**: Tests pass but code has compilation errors in different configuration
- **Frequency**: 3 (Remote)
- **Rationale**: Very rare, only if feature flags or configs not tested
```

**Principle**: Assess frequency based on likelihood given current controls, not ideal controls.

---

### Step 5: Assess Detection

**Action**: Assess how easily each failure mode can be detected before it causes impact.

**Detection scale** (1-10, where 10 is least detectable):

| Rating | Detection | Description | Example |
|--------|-----------|-------------|---------|
| 10 | Almost Impossible | Cannot detect before impact | Failure only detected after production release |
| 9 | Very Remote | Very difficult to detect | Failure detected only through extensive testing |
| 8 | Remote | Difficult to detect | Failure detected only through specific test scenarios |
| 7 | Very Low | Low chance of detection | Failure detected through comprehensive testing |
| 6 | Low | Moderate chance of detection | Failure detected through normal testing |
| 5 | Moderate | Good chance of detection | Failure detected through standard checks |
| 4 | Moderately High | High chance of detection | Failure detected through automated checks |
| 3 | High | Very high chance of detection | Failure detected through multiple checks |
| 2 | Very High | Almost certain detection | Failure detected through mandatory checks |
| 1 | Almost Certain | Certain detection | Failure detected immediately, impossible to miss |

**Detection assessment questions**:
- How easily can this failure be detected before impact?
- What controls exist to detect this failure?
- How reliable are detection mechanisms?
- Can failure be detected in testing/staging?

**Example detection assessment**:
```markdown
## Detection Assessment

**Failure Mode 1**: Release readiness declared despite uncommitted changes
- **Detection**: 8 (Remote)
- **Rationale**: Difficult to detect - requires manual git status check, not automated

**Failure Mode 2**: Missing release artifacts
- **Detection**: 2 (Very High)
- **Rationale**: Easy to detect - file existence check, automated

**Failure Mode 3**: Tests pass but code has compilation errors in different configuration
- **Detection**: 6 (Low)
- **Rationale**: Moderate detection - requires testing all configurations
```

**Principle**: Assess detection based on current detection mechanisms, not ideal ones.

---

### Step 6: Calculate RPN (Risk Priority Number)

**Action**: Calculate Risk Priority Number for each failure mode.

**RPN formula**: `RPN = Severity × Frequency × Detection`

**RPN interpretation**:
- **RPN 1-100**: Low risk - monitor, low priority
- **RPN 101-300**: Medium risk - address when possible
- **RPN 301-500**: High risk - address soon
- **RPN 501-1000**: Critical risk - address immediately

**Example RPN calculation**:
```markdown
## RPN Calculation

**Failure Mode 1**: Release readiness declared despite uncommitted changes
- **Severity**: 9
- **Frequency**: 7
- **Detection**: 8
- **RPN**: 9 × 7 × 8 = 504 (Critical risk)

**Failure Mode 2**: Missing release artifacts
- **Severity**: 6
- **Frequency**: 4
- **Detection**: 2
- **RPN**: 6 × 4 × 2 = 48 (Low risk)

**Failure Mode 3**: Tests pass but code has compilation errors in different configuration
- **Severity**: 8
- **Frequency**: 3
- **Detection**: 6
- **RPN**: 8 × 3 × 6 = 144 (Medium risk)
```

**Principle**: Higher RPN = higher priority. Focus on high RPN failure modes first.

---

### Step 7: Prioritize and Fix

**Action**: Prioritize failure modes by RPN and implement fixes.

#### 7.1: Prioritize by RPN

**Action**: Sort failure modes by RPN (highest first).

**Prioritization rules**:
1. **Critical (RPN 501-1000)**: Fix immediately
2. **High (RPN 301-500)**: Fix soon
3. **Medium (RPN 101-300)**: Fix when possible
4. **Low (RPN 1-100)**: Monitor, fix if frequency increases

**Example prioritization**:
```markdown
## Prioritized Failure Modes

**Priority 1 (Critical - RPN 504)**:
- Failure Mode 1: Release readiness declared despite uncommitted changes
- **Action**: Fix immediately - add git state verification to workflow

**Priority 2 (Medium - RPN 144)**:
- Failure Mode 3: Tests pass but code has compilation errors in different configuration
- **Action**: Fix when possible - add multi-configuration testing

**Priority 3 (Low - RPN 48)**:
- Failure Mode 2: Missing release artifacts
- **Action**: Monitor - already has detection, low risk
```

#### 7.2: Design Fixes

**Action**: Design fixes that reduce RPN (reduce severity, frequency, or improve detection).

**Fix strategies**:
1. **Reduce Severity**: Make failure less impactful (e.g., add safeguards)
2. **Reduce Frequency**: Prevent failure from occurring (e.g., add controls)
3. **Improve Detection**: Detect failure earlier (e.g., add automated checks)

**Example fix design**:
```markdown
## Fix Design

**Failure Mode 1**: Release readiness declared despite uncommitted changes
- **Current RPN**: 504 (Critical)
- **Fix Strategy**: Reduce frequency (prevent) + Improve detection (catch)
- **Fix**: Add git state verification to release workflow
  - Add "Step 2.6: Git State Verification" to workflow
  - Add git status checks to release checklist
  - Make clean git state a release blocker
- **Expected RPN after fix**: 
  - Severity: 9 (unchanged - impact still critical if occurs)
  - Frequency: 1 (reduced - prevented by workflow check)
  - Detection: 1 (improved - automated check)
  - **New RPN**: 9 × 1 × 1 = 9 (Low risk)
```

#### 7.3: Create Todo List and Execute Fixes

**Action**: Create a 10+ item todo list for all failure modes and execute them.

**CRITICAL**: Do NOT write documents or reports. Create todos and execute them.

**Todo list creation**:
1. Create todos for each failure mode fix (minimum 10 items)
2. Prioritize by RPN (highest RPN first)
3. Include verification steps in todos
4. Execute todos systematically

**Example todo list**:
```markdown
## FMEA Fix Todos (10+ items)

**Priority 1 (Critical - RPN 501-1000)**:
- [ ] Fix Failure Mode 1: Add git state verification to release workflow
- [ ] Add "Step 2.6: Git State Verification" to release-preparation.md
- [ ] Add git status checks to release checklist
- [ ] Make clean git state a release blocker
- [ ] Test: Run workflow with uncommitted changes
- [ ] Verify: Workflow detects uncommitted changes, declares "NOT READY"
- [ ] Recalculate RPN: Verify RPN reduced to <100

**Priority 2 (High - RPN 301-500)**:
- [ ] Fix Failure Mode X: [Description]
- [ ] Implement fix: [Steps]
- [ ] Verify fix works
- [ ] Recalculate RPN

**Priority 3 (Medium - RPN 101-300)**:
- [ ] Fix Failure Mode Y: [Description]
- [ ] Implement fix: [Steps]
- [ ] Verify fix works
- [ ] Recalculate RPN

**Priority 4 (Low - RPN 1-100)**:
- [ ] Monitor Failure Mode Z: [Description]
- [ ] Add detection mechanism
- [ ] Document monitoring process
```

**Execution**:
1. Create todos using `todo_write` tool (10+ items minimum)
2. Execute todos one by one (implement fixes)
3. Mark todos as completed as fixes are implemented
4. Verify each fix works before moving to next
5. Continue until all todos complete

**Principle**: Execute fixes, don't document them. Todos track progress, fixes prevent failures.

---

## Complete Workflow Example

```markdown
# Step 1: Define Scope
Scope: Release preparation workflow
Goal: Prevent release failures

# Step 2: Identify Failure Modes
Failure Mode 1: Release readiness declared despite uncommitted changes
Failure Mode 2: Missing release artifacts
Failure Mode 3: Tests pass but code has compilation errors

# Step 3: Assess Severity
Failure Mode 1: Severity 9 (Critical)
Failure Mode 2: Severity 6 (Moderate)
Failure Mode 3: Severity 8 (Serious)

# Step 4: Assess Frequency
Failure Mode 1: Frequency 7 (Moderate)
Failure Mode 2: Frequency 4 (Very Low)
Failure Mode 3: Frequency 3 (Remote)

# Step 5: Assess Detection
Failure Mode 1: Detection 8 (Remote)
Failure Mode 2: Detection 2 (Very High)
Failure Mode 3: Detection 6 (Low)

# Step 6: Calculate RPN
Failure Mode 1: RPN = 9 × 7 × 8 = 504 (Critical)
Failure Mode 2: RPN = 6 × 4 × 2 = 48 (Low)
Failure Mode 3: RPN = 8 × 3 × 6 = 144 (Medium)

# Step 7: Prioritize and Fix
Priority 1: Failure Mode 1 (RPN 504) - Fix immediately
Priority 2: Failure Mode 3 (RPN 144) - Fix when possible
Priority 3: Failure Mode 2 (RPN 48) - Monitor
```

## FMEA Best Practices

**Guidelines**:
1. **Be thorough** - Identify all failure modes, not just obvious ones
2. **Be realistic** - Assess based on current state, not ideal state
3. **Focus on high RPN** - Prioritize fixes by RPN
4. **Update regularly** - Review FMEA when process changes
5. **Document rationale** - Document why ratings were assigned

**Common mistakes**:
- ❌ Writing documents/reports instead of creating todos and executing fixes (WASTE)
- ❌ Underestimating severity (assessing based on typical, not worst-case)
- ❌ Overestimating detection (assuming ideal detection mechanisms)
- ❌ Ignoring low RPN (even low RPN failures should be monitored)
- ❌ Creating todos but not executing them

## Integration with Other Commands

- **[Root Cause Analysis](./root-cause-analysis.md)** - Use 5 Whys to understand why failure modes occur
- **[DMAIC Problem Solving](./dmaic-problem-solving.md)** - Use DMAIC to systematically fix high RPN failure modes
- **[Poka-Yoke Design](./poka-yoke-design.md)** - Use type system to prevent failure modes (reduce frequency)
- **[Andon Signals](./andon-signals.md)** - Use signals to detect failure modes early (improve detection)
- **[Release Preparation](./release-preparation.md)** - Use FMEA to identify release failure modes

## Expert Insights

**Why this matters**: Preventing failures is better than fixing them. FMEA identifies potential failures before they occur, allowing proactive prevention.

**Key principle**: "Prevent, don't react" - FMEA helps prevent failures by identifying and addressing them proactively, not reactively.

**Remember**: FMEA prevents failures by fixing them, not documenting them. Create todos and execute fixes. Update todos when processes change or new failure modes identified.

**RPN priority**: Focus on high RPN failure modes first, but don't ignore low RPN ones - they can become high RPN if frequency increases.

**DfLSS alignment**: FMEA supports DfLSS (Design for Lean Six Sigma) by preventing both defects (quality) AND waste (efficiency) - identifying failure modes prevents rework (waste) and defects (quality). Don't conflate DfLSS with DFSS (Design for Six Sigma) - DFSS only addresses quality, missing critical waste elimination. See [Root Cause Analysis - DfLSS vs DFSS](./root-cause-analysis.md#dflss-vs-dfss-critical-distinction) for why conflating DfLSS with DFSS is a huge error.

---

## FMEA Execution Pattern

**CRITICAL**: FMEA commands must:
1. **Create 10+ item todo list** - Not documents/reports
2. **Execute todos** - Implement fixes, not document them
3. **Verify fixes** - Test that fixes work
4. **Complete todos** - Mark todos as done as fixes complete

**Example execution**:
1. Define scope (mental model, not document)
2. Identify failure modes (mental model, not document)
3. Assess severity/frequency/detection (mental model, not document)
4. Calculate RPN (mental model, not document)
5. **Create 10+ item todo list** (use `todo_write` tool)
6. **Execute todos** (implement fixes)
7. **Verify fixes** (test that fixes work)
8. **Complete todos** (mark done)

**Principle**: FMEA prevents failures by fixing them, not by documenting them. Todos track progress, fixes prevent failures.

---

End Command ---

