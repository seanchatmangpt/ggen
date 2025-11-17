# FMEA (Failure Mode and Effects Analysis) - Multi-Step Workflow

## Purpose

This command guides agents through Failure Mode and Effects Analysis (FMEA) for ggen processes and systems. FMEA prevents problems before they occur by identifying and addressing failure modes proactively.

## Workflow Overview

```
Step 1: Define Scope → Step 2: Identify Failure Modes → Step 3: Assess Severity → Step 4: Assess Frequency → Step 5: Assess Detection → Step 6: Calculate RPN → Step 7: Prioritize and Fix
```

## Step-by-Step Instructions

### Step 1: Define Scope

**Action**: Clearly define what process or system is being analyzed.

**Example scope definition for ggen**:
```markdown
## FMEA Scope

**What**: RDF processing workflow
**Boundaries**: 
- Included: RDF parsing, processing, validation
- Excluded: Template generation, CLI interface
**Context**: Processing ontologies for code generation
**Goal**: Prevent processing failures (non-deterministic output, memory overflow, invalid graphs)
```

---

### Step 2: Identify Failure Modes

**Action**: Identify all potential ways the process/system can fail.

**Example failure modes for ggen**:
```markdown
## Failure Modes

**Failure Mode 1**: RDF processing produces non-deterministic output
- **Component**: RDF processor
- **Step**: Variable processing
- **Description**: HashMap iteration order causes non-deterministic output

**Failure Mode 2**: Memory overflow with large RDF graphs
- **Component**: RDF processor
- **Step**: Graph loading
- **Description**: Loading entire graph into memory exceeds 100MB SLO

**Failure Mode 3**: Invalid RDF graph passes validation
- **Component**: RDF validator
- **Step**: Validation
- **Description**: Validator doesn't catch all invalid structures
```

---

### Step 3: Assess Severity

**Action**: Assess the severity of each failure mode's impact.

**Severity scale** (1-10, where 10 is most severe):

| Rating | Severity | Description | Example |
|--------|----------|-------------|---------|
| 10 | Catastrophic | Complete system failure | Non-deterministic output breaks reproducibility |
| 9 | Critical | Major system failure | Memory overflow crashes system |
| 8 | Serious | System degraded | Invalid graph produces incorrect code |
| 7 | Major | Significant functionality lost | Processing fails for large graphs |
| 6 | Moderate | Some functionality lost | Processing slow but works |
| 5 | Minor | Minimal functionality lost | Minor performance degradation |

**Example severity assessment**:
```markdown
## Severity Assessment

**Failure Mode 1**: Non-deterministic output
- **Severity**: 10 (Catastrophic)
- **Rationale**: Violates core determinism requirement, breaks reproducibility

**Failure Mode 2**: Memory overflow
- **Severity**: 9 (Critical)
- **Rationale**: Exceeds SLO, crashes system, prevents processing

**Failure Mode 3**: Invalid graph passes validation
- **Severity**: 8 (Serious)
- **Rationale**: Produces incorrect code, breaks downstream processing
```

---

### Step 4: Assess Frequency (Occurrence)

**Action**: Assess how frequently each failure mode occurs or is likely to occur.

**Frequency scale** (1-10, where 10 is most frequent):

| Rating | Frequency | Description | Example |
|--------|-----------|-------------|---------|
| 10 | Very High | >50% of time | Failure occurs in >50% of runs |
| 9 | High | 30-50% of time | Failure occurs in 30-50% of runs |
| 8 | Moderate-High | 20-30% of time | Failure occurs in 20-30% of runs |
| 7 | Moderate | 10-20% of time | Failure occurs in 10-20% of runs |
| 6 | Low-Moderate | 5-10% of time | Failure occurs in 5-10% of runs |
| 5 | Low | 2-5% of time | Failure occurs in 2-5% of runs |
| 4 | Very Low | 1-2% of time | Failure occurs in 1-2% of runs |
| 3 | Remote | 0.5-1% of time | Failure occurs in 0.5-1% of runs |
| 2 | Very Remote | 0.1-0.5% of time | Failure occurs in 0.1-0.5% of runs |
| 1 | Extremely Remote | <0.1% of time | Failure occurs in <0.1% of runs |

**Example frequency assessment**:
```markdown
## Frequency Assessment

**Failure Mode 1**: Non-deterministic output
- **Frequency**: 7 (Moderate)
- **Rationale**: Occurs when HashMap used (happens in 10-20% of code paths)

**Failure Mode 2**: Memory overflow
- **Frequency**: 3 (Remote)
- **Rationale**: Very rare, only with extremely large graphs (>10k triples)

**Failure Mode 3**: Invalid graph passes validation
- **Frequency**: 4 (Very Low)
- **Rationale**: Rare, but can occur with edge cases
```

---

### Step 5: Assess Detection

**Action**: Assess how easily each failure mode can be detected before it causes impact.

**Detection scale** (1-10, where 10 is least detectable):

| Rating | Detection | Description | Example |
|--------|-----------|-------------|---------|
| 10 | Almost Impossible | Cannot detect before impact | Failure only detected after code generation |
| 9 | Very Remote | Very difficult to detect | Failure detected only through extensive testing |
| 8 | Remote | Difficult to detect | Failure detected only through specific test scenarios |
| 7 | Very Low | Low chance of detection | Failure detected through comprehensive testing |
| 6 | Low | Moderate chance of detection | Failure detected through normal testing |
| 5 | Moderate | Good chance of detection | Failure detected through standard checks |
| 4 | Moderately High | High chance of detection | Failure detected through automated checks |
| 3 | High | Very high chance of detection | Failure detected through multiple checks |
| 2 | Very High | Almost certain detection | Failure detected through mandatory checks |
| 1 | Almost Certain | Certain detection | Failure detected immediately, impossible to miss |

**Example detection assessment**:
```markdown
## Detection Assessment

**Failure Mode 1**: Non-deterministic output
- **Detection**: 8 (Remote)
- **Rationale**: Difficult to detect - requires running multiple times and comparing outputs

**Failure Mode 2**: Memory overflow
- **Detection**: 4 (Moderately High)
- **Rationale**: High detection - memory monitoring catches it

**Failure Mode 3**: Invalid graph passes validation
- **Detection**: 6 (Low)
- **Rationale**: Moderate detection - requires comprehensive validation tests
```

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

**Failure Mode 1**: Non-deterministic output
- **Severity**: 10
- **Frequency**: 7
- **Detection**: 8
- **RPN**: 10 × 7 × 8 = 560 (Critical risk)

**Failure Mode 2**: Memory overflow
- **Severity**: 9
- **Frequency**: 3
- **Detection**: 4
- **RPN**: 9 × 3 × 4 = 108 (Medium risk)

**Failure Mode 3**: Invalid graph passes validation
- **Severity**: 8
- **Frequency**: 4
- **Detection**: 6
- **RPN**: 8 × 4 × 6 = 192 (Medium risk)
```

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

**Priority 1 (Critical - RPN 560)**:
- Failure Mode 1: Non-deterministic output
- **Action**: Fix immediately - replace HashMap with BTreeMap

**Priority 2 (Medium - RPN 192)**:
- Failure Mode 3: Invalid graph passes validation
- **Action**: Fix when possible - enhance validation

**Priority 3 (Medium - RPN 108)**:
- Failure Mode 2: Memory overflow
- **Action**: Monitor - add memory checks
```

#### 7.2: Create Todo List and Execute Fixes

**CRITICAL**: Do NOT write documents or reports. Create todos and execute them.

**Action**: Create 10+ item todo list for all failure modes and execute them.

**Example todo list**:
```markdown
## FMEA Fix Todos (10+ items)

**Priority 1 (Critical - RPN 501-1000)**:
- [ ] Fix Failure Mode 1: Replace HashMap with BTreeMap in RDF processor
- [ ] Update code: Use BTreeMap for deterministic iteration
- [ ] Verify compilation: `cargo make check`
- [ ] Run tests: `cargo make test`
- [ ] Verify fix works: Run 100 times, verify deterministic output
- [ ] Recalculate RPN: Verify RPN reduced to <100

**Priority 2 (Medium - RPN 101-300)**:
- [ ] Fix Failure Mode 3: Enhance RDF validation
- [ ] Add validation checks for edge cases
- [ ] Verify fix works
- [ ] Recalculate RPN
```

**Execution**:
1. Create todos using `todo_write` tool (10+ items minimum)
2. Execute todos one by one (implement fixes)
3. Mark todos as completed as fixes are implemented
4. Verify each fix works before moving to next
5. Continue until all todos complete

**Principle**: Execute fixes, don't document them. Todos track progress, fixes prevent failures.

---

## Integration with Other Commands

- **[Root Cause Analysis](./root-cause-analysis.md)** - Use 5 Whys to understand why failure modes occur
- **[DMAIC Problem Solving](./dmaic-problem-solving.md)** - Use DMAIC to systematically fix high RPN failure modes
- **[Poka-Yoke Design](./poka-yoke-design.md)** - Use type system to prevent failure modes (reduce frequency)
- **[Andon Signals](./andon-signals.md)** - Use signals to detect failure modes early (improve detection)

## Expert Insights

**Why this matters**: Preventing failures is better than fixing them. FMEA identifies potential failures before they occur, allowing proactive prevention.

**Key principle**: "Prevent, don't react" - FMEA helps prevent failures by identifying and addressing them proactively, not reactively.

**Remember**: FMEA prevents failures by fixing them, not documenting them. Create todos and execute fixes. Update todos when processes change or new failure modes identified.

**RPN priority**: Focus on high RPN failure modes first, but don't ignore low RPN ones - they can become high RPN if frequency increases.

**DfLSS alignment**: FMEA supports DfLSS (Design for Lean Six Sigma) by preventing both defects (quality) AND waste (efficiency) - identifying failure modes prevents rework (waste) and defects (quality). Don't conflate DfLSS with DFSS (Design for Six Sigma) - DFSS only addresses quality, missing critical waste elimination. See [Root Cause Analysis - DfLSS vs DFSS](./root-cause-analysis.md#dflss-vs-dfss-critical-distinction) for why conflating DfLSS with DFSS is a huge error.

---

## Command Execution Pattern

**CRITICAL**: FMEA commands must:
1. **Create 10+ item todo list** - Not documents/reports
2. **Execute todos** - Implement fixes, not document them
3. **Verify fixes** - Test that fixes work
4. **Complete todos** - Mark todos as done as fixes complete

**Principle**: FMEA prevents failures by fixing them, not by documenting them. Todos track progress, fixes prevent failures.

---

End Command ---
