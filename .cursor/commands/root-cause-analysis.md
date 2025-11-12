# Root Cause Analysis (5 Whys) - Multi-Step Workflow

## Purpose

This command guides agents through root cause analysis using the 5 Whys technique. Root cause analysis finds the underlying cause of problems, not just symptoms. Experts dig deeper to find root causes rather than fixing symptoms.

## Workflow Overview

```
Step 1: Define the Problem → Step 2: Ask Why #1 → Step 3: Ask Why #2-5 → Step 4: Verify Root Cause → Step 5: Fix Root Cause
```

## Step-by-Step Instructions

### Step 1: Define the Problem

**Action**: Clearly state the observable problem (symptom).

**Problem definition format**:
- **What**: What is the observable problem?
- **Where**: Where does it occur?
- **When**: When does it occur?
- **Impact**: What is the impact?

**Example problem definition**:
```markdown
## Problem Definition

**What**: Test fails with "assertion failed: expected 1000, got 999"
**Where**: `tests/concurrent_test.rs` - `test_concurrent_increment`
**When**: Approximately 30% of test runs, more frequent in CI
**Impact**: Blocks CI/CD pipeline, causes false negatives
```

**Principle**: Start with the observable symptom, not assumptions about cause.

---

### Step 2: Ask Why #1

**Action**: Ask why the problem occurred (first level).

**Why #1 question**: "Why did [problem] occur?"

**Answer format**:
- Direct cause of the symptom
- Observable fact, not assumption
- Can be verified

**Example**:
```markdown
## 5 Whys Analysis

**Problem**: Test fails with "expected 1000, got 999"

**Why #1**: Why did the test fail?
**Answer**: Counter value was 999 instead of expected 1000

**Verification**: 
- Test output shows actual value 999
- Expected value was 1000
- One increment operation didn't complete
```

---

### Step 3: Ask Why #2-5

**Action**: Continue asking why until root cause found.

**Process**:
- Ask "Why?" for each answer
- Continue until root cause found (usually 3-5 whys)
- Each answer should be deeper than previous
- Root cause is something that, if fixed, prevents the problem

**Example continued**:
```markdown
## 5 Whys Analysis (Continued)

**Why #2**: Why was counter 999 instead of 1000?
**Answer**: One increment operation didn't complete

**Why #3**: Why didn't one increment complete?
**Answer**: Race condition - two threads read same value before incrementing

**Why #4**: Why did race condition occur?
**Answer**: Mutex lock was released too early, allowing concurrent reads

**Why #5**: Why was mutex lock released too early?
**Answer**: Lock scope didn't include the entire increment operation (ROOT CAUSE)

**Root Cause**: Lock scope is too narrow - doesn't protect entire increment operation
```

**Key insight**: Root cause is usually a process or design issue, not a person or one-time event.

---

### Step 4: Verify Root Cause

**Action**: Confirm root cause hypothesis.

#### 4.1: Test Root Cause Hypothesis

**Action**: Verify that fixing root cause prevents the problem.

**Verification questions**:
- If we fix the root cause, will the problem be prevented?
- Does the data support the root cause hypothesis?
- Are there other contributing factors?

**Example verification**:
```rust
// Root cause hypothesis: Lock scope too narrow
// Test: Expand lock scope and verify problem prevented

// Before (root cause present)
let mut value = counter.lock().unwrap();
let current = *value;
drop(value); // Lock released too early
*value = current + 1; // Race condition possible

// After (root cause fixed)
let mut value = counter.lock().unwrap();
*value += 1; // Entire operation protected by lock
// Lock released after operation complete

// Verification: Run test 100 times, should have 0 failures
```

#### 4.2: Check for Contributing Factors

**Action**: Identify other factors that contribute to the problem.

**Contributing factors**:
- Factors that make problem more likely
- Factors that make problem worse
- Factors that prevent detection

**Example**:
```markdown
## Contributing Factors

**Root Cause**: Lock scope too narrow

**Contributing Factors**:
- Test runs in CI with multiple threads (makes race condition more likely)
- No flaky test detection (problem not caught early)
- Insufficient test coverage for concurrent code (problem not prevented)

**Note**: Fix root cause first, then address contributing factors
```

---

### Step 5: Fix Root Cause

**Action**: Implement fix that addresses root cause.

#### 5.1: Design Fix

**Action**: Design solution that addresses root cause.

**Fix criteria**:
- Addresses root cause (not just symptom)
- Prevents problem from recurring
- Doesn't introduce new problems
- Is maintainable

**Example fix design**:
```markdown
## Fix Design

**Root Cause**: Lock scope too narrow

**Fix**: Expand lock scope to include entire increment operation

**Implementation**:
1. Keep lock for entire increment operation
2. Don't release lock until operation complete
3. Verify fix with tests

**Prevention**: Add test that would catch this pattern
```

#### 5.2: Implement Fix

**Action**: Implement the fix.

**Implementation steps**:
1. Make code changes
2. Verify compilation: `cargo make check`
3. Run tests: `cargo make test`
4. Verify fix: Run test multiple times

**Example implementation**:
```rust
// Fix: Expand lock scope
let mut value = counter.lock().unwrap();
*value += 1; // Entire operation protected by lock
// Lock released here, after operation complete
```

#### 5.3: Verify Fix

**Action**: Ensure fix prevents the problem.

**Verification**:
- ✅ Problem doesn't occur: Run test 100 times, 0 failures
- ✅ No regressions: Other tests still pass
- ✅ Root cause addressed: Lock scope now correct

**Example verification**:
```bash
# Verify fix prevents problem
for i in {1..100}; do
    cargo make test test_concurrent_increment
done
# Expected: 0 failures (problem prevented)

# Verify no regressions
cargo make test
# Expected: All tests pass
```

#### 5.4: Create Todo List for Prevention

**CRITICAL**: Do NOT just document prevention. Create todos and implement prevention measures.

**Action**: Create 10+ item todo list for implementing prevention measures.

**Prevention methods** (implement as todos):
- **Tests** - Add test that would catch root cause
- **Code review** - Add checklist items to prevent similar issues
- **Inline comments** - Add comments explaining why fix was needed (in code, not separate doc)
- **Standards** - Implement pattern enforcement

**Example prevention todo list**:
```markdown
## Root Cause Prevention Todos (10+ items)

**Test Prevention**:
- [ ] Add test: `test_lock_scope_covers_operation` to catch pattern
- [ ] Verify test fails if lock scope too narrow
- [ ] Verify test passes with correct lock scope
- [ ] Add test to CI pipeline

**Code Review Prevention**:
- [ ] Add checklist item: Lock scope covers entire critical section
- [ ] Add checklist item: No operations between lock acquire and release
- [ ] Update code review process to include checklist
- [ ] Verify checklist is used in reviews

**Inline Documentation Prevention**:
- [ ] Add inline comment: Document why lock scope is important
- [ ] Add inline comment: Document pattern to follow
- [ ] Verify comments are clear and helpful

**Standards Prevention**:
- [ ] Add standard to coding guidelines: "Lock scope must cover entire operation"
- [ ] Update team documentation with standard
- [ ] Verify standard is followed

**Verification**:
- [ ] Verify prevention works: Test catches pattern
- [ ] Verify no regressions: All tests still pass
- [ ] Verify prevention is sustainable: Process is repeatable
```

**Execution**:
1. Create todos using `todo_write` tool (10+ items minimum)
2. Execute todos one by one (implement prevention measures)
3. Mark todos as completed as prevention is implemented
4. Verify each prevention measure works before moving to next
5. Continue until all prevention measures implemented

**Example implementation**:
```rust
use chicago_tdd_tools::prelude::*;

// Add test to prevent root cause from returning
test!(test_lock_scope_covers_operation, {
    // Test that would fail if lock scope too narrow
    // This prevents root cause from returning
});

// Add inline comment (not separate document)
/// Increment counter with proper lock scope.
/// 
/// **Root Cause Fix**: Lock scope covers entire increment operation.
/// Pattern: Keep lock for entire critical section, not just read.
fn increment_counter(counter: &Mutex<u32>) {
    let mut value = counter.lock().unwrap();
    *value += 1; // Entire operation protected
}
```

**Principle**: Implement prevention measures, don't document them separately. Todos track progress, prevention measures prevent recurrence.

---

## Complete Workflow Example

```markdown
# Step 1: Define the Problem
Problem: Test fails with "expected 1000, got 999"

# Step 2: Ask Why #1
Why #1: Counter value was 999 instead of 1000

# Step 3: Ask Why #2-5
Why #2: One increment operation didn't complete
Why #3: Race condition - two threads read same value
Why #4: Mutex lock released too early
Why #5: Lock scope didn't include entire operation (ROOT CAUSE)

# Step 4: Verify Root Cause
Test: Expand lock scope, verify problem prevented
Result: Problem prevented ✅

# Step 5: Fix Root Cause
Fix: Expand lock scope to include entire increment
Verify: Run test 100 times, 0 failures ✅
Prevent: Add test to catch pattern
```

## 5 Whys Best Practices

**Guidelines**:
1. **Start with symptom** - Begin with observable problem
2. **Ask why, not who** - Focus on process, not blame
3. **Dig deep** - Usually need 3-5 whys to find root cause
4. **Verify root cause** - Test that fixing it prevents problem
5. **Fix root cause** - Not just symptom

**Common mistakes**:
- ❌ Stopping too early (fixing symptom, not cause)
- ❌ Blaming people instead of process
- ❌ Not verifying root cause
- ❌ Fixing symptoms instead of root cause

## Integration with Other Commands

- **[DMAIC Problem Solving](./dmaic-problem-solving.md)** - Use 5 Whys in Analyze step
- **[Gemba Walk](./gemba-walk.md)** - Go to source to verify root cause
- **[Andon Signals](./andon-signals.md)** - Use 5 Whys when signals appear
- **[Poka-Yoke Design](./poka-yoke-design.md)** - Use type system to prevent root cause

## Expert Insights

**Why this matters**: Fixing symptoms doesn't solve problems. Root cause analysis finds underlying causes that, when fixed, prevent problems from recurring.

**Key principle**: "Ask why five times" - Usually need to dig 3-5 levels deep to find root cause. Surface-level fixes don't prevent recurrence.

**Remember**: Root cause is usually a process or design issue, not a person or one-time event. Focus on fixing the process, not blaming people.

**5 Whys principle**: Each "why" should be deeper than the previous. Root cause is something that, if fixed, prevents the entire chain of problems.

---

## DfLSS vs DFSS: Critical Distinction

**Why this matters in root cause analysis**: Using the wrong design methodology is itself a root cause. When root cause analysis reveals methodology mismatch, it's not a simple terminology error—it's a fundamental design failure that leads to suboptimal outcomes.

### Definitions

**DFSS (Design for Six Sigma)**:
- Focus: Quality and defect prevention
- Methodology: Primarily DMADV (Define, Measure, Analyze, Design, Verify)
- Goal: Achieve Six Sigma quality levels (3.4 defects per million opportunities)
- Approach: Design quality into products/processes from the start
- Scope: Reduces defects and variability, but may not explicitly address waste elimination

**DfLSS (Design for Lean Six Sigma)**:
- Focus: **Both efficiency (Lean) AND quality (Six Sigma)**
- Methodology: DMADV + Lean tools and techniques integrated throughout
- Goal: Design products/processes that are both efficient (waste-free) and high-quality
- Approach: Eliminate waste during design phase while ensuring quality
- Scope: Addresses both waste elimination (Lean) and defect prevention (Six Sigma) simultaneously

### Key Differences

| Aspect | DFSS | DfLSS |
|--------|------|-------|
| **Primary Focus** | Quality and defect prevention | Efficiency (waste elimination) + Quality (defect prevention) |
| **Waste Elimination** | Not explicitly addressed | Actively identifies and eliminates waste during design |
| **Methodology** | DMADV (Six Sigma focused) | DMADV + Lean tools integrated |
| **Design Phase** | Quality-focused design | Efficiency + quality-focused design |
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

#### 1. Wrong Methodology Selection

**Error**: Using DFSS when DfLSS is needed (or vice versa) leads to:
- Missing waste elimination when efficiency is critical
- Over-engineering quality when efficiency is the primary concern
- Applying wrong tools and techniques
- Incomplete problem analysis

**Impact**: Suboptimal designs that don't address all root causes.

#### 2. Missing Critical Waste Elimination

**Error**: DFSS doesn't explicitly address waste elimination. If you conflate DFSS with DfLSS, you may:
- Design high-quality systems that are inefficient
- Miss opportunities to eliminate waste during design
- Build waste into the system from the start
- Require expensive rework to eliminate waste later

**Impact**: Systems that meet quality targets but are inefficient, wasting resources and time.

#### 3. Root Cause of Design Failures

**Error**: Using wrong methodology is itself a root cause. Many design failures trace back to methodology mismatch:

```
Why #1: Why did the system fail?
Answer: System was inefficient, causing delays

Why #2: Why was the system inefficient?
Answer: Waste was built into the design

Why #3: Why was waste built into the design?
Answer: Design methodology didn't address waste elimination

Why #4: Why didn't design methodology address waste?
Answer: Used DFSS instead of DfLSS (ROOT CAUSE)
```

**Impact**: Root cause analysis reveals methodology mismatch as the underlying cause.

#### 4. Suboptimal Outcomes

**Error**: Conflating DFSS and DfLSS leads to:
- Designs that meet quality targets but are inefficient
- Designs that are efficient but don't meet quality targets
- Incomplete problem analysis (missing waste or quality issues)
- Wrong tool selection (using Six Sigma tools when Lean tools needed)

**Impact**: Systems that don't achieve optimal outcomes (both efficient AND high-quality).

#### 5. Process Failure, Not Terminology Error

**Error**: This is not a simple naming mistake—it's a process failure:
- Wrong methodology selection process
- Incomplete requirements analysis
- Missing waste elimination in design phase
- Incomplete root cause analysis

**Impact**: Fundamental design process failure, not just terminology confusion.

### Connection to Root Cause Analysis

**Using the wrong methodology is itself a root cause.** When root cause analysis reveals methodology mismatch, it's a critical finding that requires methodology correction, not just terminology clarification.

#### 5 Whys Example: Methodology Mismatch as Root Cause

```markdown
## Problem Definition

**What**: System meets quality targets but is inefficient, causing delays
**Where**: Production system - all workflows
**When**: Since initial design phase
**Impact**: Wasted resources, delayed delivery, customer dissatisfaction

## 5 Whys Analysis

**Why #1**: Why is the system inefficient?
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

#### How to Identify Methodology Mismatch

**Signs that methodology mismatch is the root cause**:
- System meets quality targets but is inefficient (used DFSS when DfLSS needed)
- System is efficient but doesn't meet quality targets (used Lean when DfLSS needed)
- Root cause analysis reveals "design methodology didn't address [waste/quality]"
- Multiple "why" levels point to design phase issues
- Waste elimination or quality prevention missing from design process

**Fix**: Correct methodology selection process, use DfLSS when both efficiency and quality are required.

### Best Practices

**1. Always Distinguish DFSS from DfLSS**:
- DFSS = Quality focus (defect prevention)
- DfLSS = Efficiency + Quality focus (waste elimination + defect prevention)

**2. Use DfLSS When**:
- Both efficiency and quality are required
- Waste elimination is critical
- 80/20 thinking applies (maximize value)
- Root cause analysis reveals both waste and quality issues

**3. Use DFSS When**:
- Quality is primary concern
- Efficiency is not critical
- Waste elimination can be addressed later

**4. In Root Cause Analysis**:
- Ask: "Was the right methodology used?"
- Check: "Did methodology address all root causes?"
- Verify: "Does methodology match requirements (efficiency + quality)?"

**5. Prevention**:
- Document methodology selection criteria
- Distinguish DFSS from DfLSS in requirements
- Use DfLSS by default when both efficiency and quality matter
- Include waste elimination in design phase (DfLSS)

### Summary

**DfLSS vs DFSS**: DfLSS integrates Lean waste elimination with Six Sigma quality, addressing both efficiency and quality from the start. DFSS focuses only on quality, missing waste elimination.

**Why DfLSS is superior**: Addresses both efficiency and quality, prevents waste during design, aligns with 80/20 thinking, provides more complete solutions.

**Why conflating them is a huge error**: Not a simple terminology mistake—it's a fundamental methodology error that leads to wrong methodology selection, missing waste elimination, suboptimal designs, and process failures. Using wrong methodology is itself a root cause.

**Root cause analysis connection**: When root cause analysis reveals methodology mismatch, it's a critical finding requiring methodology correction, not just terminology clarification.


## Command Execution Pattern

**CRITICAL**: Root cause analysis commands must:
1. **Create 10+ item todo list** - Not documents/reports
2. **Execute todos** - Implement fixes and prevention, not document them
3. **Verify fixes** - Test that fixes work
4. **Complete todos** - Mark todos as done as fixes complete

**Principle**: Fix root causes and implement prevention, don't document them. Todos track progress, fixes prevent recurrence.

---
