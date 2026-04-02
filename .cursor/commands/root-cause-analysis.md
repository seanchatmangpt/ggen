# Root Cause Analysis (5 Whys) - Multi-Step Workflow

## Purpose

This command guides agents through root cause analysis using the 5 Whys technique for ggen issues. Root cause analysis finds the underlying cause of problems, not just symptoms. Experts dig deeper to find root causes rather than fixing symptoms.

## Workflow Overview

```
Step 1: Define the Problem (with Measurement) → Step 2: Ask Why #1 → Step 3: Ask Why #2-5 → Step 4: Verify Root Cause → Step 5: Fix Root Cause (with Measurement & Control)
```

## Step-by-Step Instructions

### Step 1: Define the Problem

**Action**: Clearly state the observable problem (symptom).

**Example problem definition**:
```markdown
## Problem Definition

**What**: Template generation produces non-deterministic output
**Where**: `crates/ggen-core/src/templates/generator.rs` - `generate()`
**When**: Approximately 10% of generations, more frequent with large templates
**Impact**: Violates determinism requirement, breaks reproducibility
```

#### 1.1: Collect Baseline Data (DMAIC Measurement)

**Action**: Measure current state to establish baseline.

**Action**: Collect baseline data

```bash
# Measure non-determinism rate
for i in {1..100}; do
    cargo make test --test template_generation > run_$i.txt
done
# Compare outputs: 10 runs produce different output (10% non-determinism)

# Measure when it occurs
# Large templates: 15% non-determinism
# Small templates: 5% non-determinism
# Pattern: More frequent with large templates
```

---

### Step 2: Ask Why #1

**Action**: Ask why the problem occurred (first level).

**Example**:
```markdown
## 5 Whys Analysis

**Problem**: Template generation produces non-deterministic output

**Why #1**: Why did template generation produce non-deterministic output?
**Answer**: Template variables were processed in non-deterministic order
```

---

### Step 3: Ask Why #2-5

**Action**: Continue asking why until root cause found.

**Example continued**:
```markdown
**Why #2**: Why were template variables processed in non-deterministic order?
**Answer**: HashMap iteration order is non-deterministic

**Why #3**: Why was HashMap used for variable storage?
**Answer**: HashMap provides fast lookup for template variables

**Why #4**: Why wasn't deterministic ordering considered?
**Answer**: Performance was prioritized over determinism

**Why #5**: Why wasn't determinism requirement enforced in design?
**Answer**: Design didn't explicitly require deterministic data structures (ROOT CAUSE)

**Root Cause**: Design didn't enforce determinism requirement, allowing non-deterministic data structures
```

---

### Step 4: Verify Root Cause

**Action**: Confirm root cause hypothesis.

**Verification**:
- Does fixing root cause prevent the problem?
- Does data support root cause hypothesis?
- Are there other contributing factors?

**Example verification**:
```rust
// Root cause hypothesis: HashMap causes non-determinism
// Test: Use BTreeMap (deterministic ordering) and verify problem prevented

// Before (root cause present)
let vars: HashMap<String, Value> = ...; // Non-deterministic iteration
for (key, value) in vars.iter() { ... } // Non-deterministic order

// After (root cause fixed)
let vars: BTreeMap<String, Value> = ...; // Deterministic ordering
for (key, value) in vars.iter() { ... } // Deterministic order

// Verification: Run 100 times, should have 0 non-deterministic outputs
```

---

### Step 5: Fix Root Cause

**Action**: Implement fix that addresses root cause.

#### 5.1: Design Fix

**Action**: Design solution that addresses root cause.

**Example fix design**:
```markdown
## Fix Design

**Root Cause**: Design didn't enforce determinism requirement

**Fix**: Use BTreeMap instead of HashMap for template variables
- Maintains fast lookup (O(log n) vs O(1), acceptable trade-off)
- Provides deterministic iteration order
- Enforces determinism at type level

**Prevention**: Add lint rule to flag HashMap usage in deterministic code paths
```

#### 5.2: Implement Fix

**Action**: Implement the fix.

**Example implementation**:
```rust
// Fix: Use BTreeMap for deterministic ordering
use std::collections::BTreeMap;

struct TemplateContext {
    variables: BTreeMap<String, Value>, // Deterministic ordering
}

impl TemplateContext {
    fn process_variables(&self) -> Vec<(String, Value)> {
        // Iteration order is deterministic
        self.variables.iter().map(|(k, v)| (k.clone(), v.clone())).collect()
    }
}
```

#### 5.3: Verify Fix

**Action**: Ensure fix prevents the problem.

**Verification**:
- ✅ Problem doesn't occur: Run 100 times, 0 non-deterministic outputs
- ✅ No regressions: Other tests still pass
- ✅ Root cause addressed: Determinism enforced at type level

#### 5.4: Measure Improvement (DMAIC Measurement)

**Action**: Measure improvement against baseline.

**Measurement**:
- Re-measure non-determinism rate after fix
- Compare to baseline
- Calculate improvement percentage

**Example improvement measurement**:
```markdown
## Improvement Measurement

**Baseline**: 10% non-determinism (10 out of 100 runs)
**After Fix**: 0% non-determinism (0 out of 100 runs)
**Improvement**: 100% (10% → 0%)

**Success Criteria Met**: ✅
- Non-determinism: 0% (target: 0%)
- Determinism requirement: Met (100% reproducible)
- Root cause addressed: Determinism enforced at type level
```

#### 5.5: Create Todo List for Prevention

**CRITICAL**: Do NOT just document prevention. Create todos and implement prevention measures.

**Action**: Create 10+ item todo list for implementing prevention measures.

**Example prevention todo list**:
```markdown
## Root Cause Prevention Todos (10+ items)

**Test Prevention**:
- [ ] Add test: `test_template_generation_deterministic` to catch pattern
- [ ] Verify test fails if non-deterministic data structures used
- [ ] Verify test passes with deterministic data structures
- [ ] Add test to CI pipeline

**Code Review Prevention**:
- [ ] Add checklist item: Use deterministic data structures in deterministic code paths
- [ ] Add checklist item: BTreeMap instead of HashMap for deterministic iteration
- [ ] Update code review process to include determinism checks

**Inline Documentation Prevention**:
- [ ] Add inline comment: Document why BTreeMap is used (determinism)
- [ ] Add inline comment: Document pattern to follow
- [ ] Verify comments are clear and helpful

**Standards Prevention**:
- [ ] Add standard: Use BTreeMap for deterministic iteration in template processing
- [ ] Update team documentation with standard
- [ ] Verify standard is followed
```

**Execution**:
1. Create todos using `todo_write` tool (10+ items minimum)
2. Execute todos one by one (implement prevention measures)
3. Mark todos as completed as prevention is implemented
4. Verify each prevention measure works before moving to next
5. Continue until all prevention measures implemented

**Principle**: Implement prevention measures, don't document them separately. Todos track progress, prevention measures prevent recurrence.

#### 5.6: Establish Controls (DMAIC Control)

**Action**: Set up controls to prevent root cause from returning.

**Controls**:
- **Monitoring**: Track non-determinism rate over time
- **Alerts**: Set up alerts if non-determinism returns
- **Review**: Periodic review of controls effectiveness
- **Adjustment**: Adjust controls if needed

**Action**: Create todo list for controls (10+ items)

```markdown
## Root Cause Control Todos (10+ items)

**Monitoring Controls**:
- [ ] Set up non-determinism tracking dashboard
- [ ] Configure alerts if non-determinism > 0%
- [ ] Review non-determinism trends weekly
- [ ] Document non-determinism patterns

**Test Strategy Controls**:
- [ ] Add determinism check to CI pipeline
- [ ] Configure alert if determinism check fails
- [ ] Verify alerts work correctly
- [ ] Review test strategy monthly

**Code Review Controls**:
- [ ] Add checklist item: Deterministic data structures in deterministic code paths
- [ ] Update code review process to include determinism checks
- [ ] Verify checklist is used in reviews

**Standards Controls**:
- [ ] Add standard: Use BTreeMap for deterministic iteration
- [ ] Update team documentation with standard
- [ ] Verify standard is followed in code reviews
- [ ] Review standards quarterly
```

**Execution**:
1. Create todos using `todo_write` tool (10+ items minimum)
2. Execute todos one by one (implement controls)
3. Mark todos as completed as controls are implemented
4. Verify each control works before moving to next
5. Continue until all controls implemented

**Principle**: Implement controls to prevent root cause recurrence, don't just document them. Todos track progress, controls prevent recurrence.

---

## DfLSS vs DFSS: Critical Distinction

**Why this matters in root cause analysis**: Using the wrong design methodology is itself a root cause. When root cause analysis reveals methodology mismatch, it's not a simple terminology error—it's a fundamental design failure that leads to suboptimal outcomes.

### Definitions

**DFSS (Design for Six Sigma)**:
- Focus: Quality and defect prevention
- Methodology: Primarily DMADV (Define, Measure, Analyze, Design, Verify)
- Goal: Achieve Six Sigma quality levels (3.4 defects per million opportunities)
- Scope: Reduces defects and variability, but may not explicitly address waste elimination

**DfLSS (Design for Lean Six Sigma)**:
- Focus: **Both efficiency (Lean) AND quality (Six Sigma)**
- Methodology: DMADV + Lean tools and techniques integrated throughout
- Goal: Design products/processes that are both efficient (waste-free) and high-quality
- Scope: Addresses both waste elimination (Lean) and defect prevention (Six Sigma) simultaneously

### Key Differences

| Aspect | DFSS | DfLSS |
|--------|------|-------|
| **Primary Focus** | Quality and defect prevention | Efficiency (waste elimination) + Quality (defect prevention) |
| **Waste Elimination** | Not explicitly addressed | Actively identifies and eliminates waste during design |
| **Outcome** | High quality, may have inefficiencies | High quality AND efficient (waste-free) |
| **80/20 Alignment** | Partial (quality focus) | Complete (efficiency + quality = maximum value) |

### Why DfLSS is Superior

**1. Comprehensive Approach**: DfLSS addresses both efficiency and quality from the start, preventing both defects AND waste. DFSS only prevents defects, leaving waste elimination as an afterthought.

**2. Waste Prevention During Design**: DfLSS actively identifies and eliminates waste (muda) during the design phase, not after implementation. This prevents inefficiencies from being built into the system.

**3. Better Alignment with 80/20 Thinking**: DfLSS's dual focus (efficiency + quality) aligns with 80/20 principles—maximizing value by addressing both waste elimination and defect prevention simultaneously.

**4. More Complete Solution**: DfLSS prevents both types of problems:
- **Quality problems**: Defects, variability, failures (Six Sigma)
- **Efficiency problems**: Waste, unnecessary steps, inefficiencies (Lean)

**5. Root Cause Prevention**: By addressing both efficiency and quality during design, DfLSS prevents root causes from being introduced, not just defects.

### Why Conflating Them is a Huge Error

**This is NOT a simple terminology mistake—it's a fundamental methodology error that leads to suboptimal designs.**

#### 5 Whys Example: Methodology Mismatch as Root Cause

```markdown
## Problem Definition

**What**: ggen meets quality targets but is inefficient, causing delays
**Where**: All code generation workflows
**When**: Since initial design phase
**Impact**: Wasted resources, delayed delivery, customer dissatisfaction

## 5 Whys Analysis

**Why #1**: Why is ggen inefficient?
**Answer**: Waste was built into the design (unnecessary steps, redundant processes)

**Why #2**: Why was waste built into the design?
**Answer**: Design methodology didn't address waste elimination

**Why #3**: Why didn't design methodology address waste?
**Answer**: Used DFSS (Design for Six Sigma) instead of DfLSS (Design for Lean Six Sigma)

**Why #4**: Why was DFSS used instead of DfLSS?
**Answer**: Methodology selection process didn't distinguish between DFSS and DfLSS

**Why #5**: Why didn't methodology selection distinguish between DFSS and DfLSS?
**Answer**: Conflated DFSS and DfLSS as equivalent methodologies (ROOT CAUSE)

**Root Cause**: Methodology selection process conflated DFSS and DfLSS, leading to wrong methodology selection and suboptimal design
```

### Best Practices

**1. Always Distinguish DFSS from DfLSS**:
- DFSS = Quality focus (defect prevention)
- DfLSS = Efficiency + Quality focus (waste elimination + defect prevention)

**2. Use DfLSS When**:
- Both efficiency and quality are required
- Waste elimination is critical
- 80/20 thinking applies (maximize value)
- Root cause analysis reveals both waste and quality issues

**3. In Root Cause Analysis**:
- Ask: "Was the right methodology used?"
- Check: "Did methodology address all root causes?"
- Verify: "Does methodology match requirements (efficiency + quality)?"

### Summary

**DfLSS vs DFSS**: DfLSS integrates Lean waste elimination with Six Sigma quality, addressing both efficiency and quality from the start. DFSS focuses only on quality, missing waste elimination.

**Why DfLSS is superior**: Addresses both efficiency and quality, prevents waste during design, aligns with 80/20 thinking, provides more complete solutions.

**Why conflating them is a huge error**: Not a simple terminology mistake—it's a fundamental methodology error that leads to wrong methodology selection, missing waste elimination, suboptimal designs, and process failures. Using wrong methodology is itself a root cause.

**Root cause analysis connection**: When root cause analysis reveals methodology mismatch, it's a critical finding requiring methodology correction, not just terminology clarification.

---

## Integration with Other Commands

- **[DMAIC Problem Solving](./dmaic-problem-solving.md)** - Use DMAIC measurement and control steps integrated into this workflow
- **[Gemba Walk](./gemba-walk.md)** - Go to source to verify root cause
- **[Andon Signals](./andon-signals.md)** - Use 5 Whys when signals appear
- **[Poka-Yoke Design](./poka-yoke-design.md)** - Use type system to prevent root cause

## Expert Insights

**Why this matters**: Fixing symptoms doesn't solve problems. Root cause analysis finds underlying causes that, when fixed, prevent problems from recurring.

**Key principle**: "Ask why five times" - Usually need to dig 3-5 levels deep to find root cause. Surface-level fixes don't prevent recurrence.

**Remember**: Root cause is usually a process or design issue, not a person or one-time event. Focus on fixing the process, not blaming people.

---

## Command Execution Pattern

**CRITICAL**: Root cause analysis commands must:
1. **Create 10+ item todo list** - Not documents/reports
2. **Execute todos** - Implement fixes and prevention, not document them
3. **Verify fixes** - Test that fixes work
4. **Complete todos** - Mark todos as done as fixes complete

**Principle**: Fix root causes and implement prevention, don't document them. Todos track progress, fixes prevent recurrence.

---

End Command ---

