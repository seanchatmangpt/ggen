# DMAIC Problem Solving - Multi-Step Workflow

## Purpose

This command guides agents through systematic problem-solving using DMAIC methodology. DMAIC (Define, Measure, Analyze, Improve, Control) is a data-driven approach to solving problems. Experts use DMAIC to avoid jumping to solutions and ensure fixes address root causes.

## Workflow Overview

```
Step 1: Define → Step 2: Measure → Step 3: Analyze → Step 4: Improve → Step 5: Control
```

## Step-by-Step Instructions

### Step 1: Define

**Action**: Clearly define the problem, scope, and success criteria.

#### 1.1: Define the Problem

**Action**: Write a clear problem statement.

**Problem statement format**:
- **What**: What is the problem?
- **Where**: Where does it occur?
- **When**: When does it occur?
- **Impact**: What is the impact?
- **Who**: Who is affected?

**Example problem statement**:
```markdown
## Problem Statement

**What**: Tests fail intermittently (flaky tests)
**Where**: `tests/integration_test.rs` - `test_concurrent_access`
**When**: Approximately 30% of test runs, more frequent in CI
**Impact**: Blocks CI/CD pipeline, causes false negatives, wastes developer time
**Who**: All developers, CI/CD system
```

#### 1.2: Define Scope

**Action**: Determine what's in scope and what's out of scope.

**Scope definition**:
- **In scope**: What will be addressed
- **Out of scope**: What won't be addressed (for now)
- **Boundaries**: Limits of the problem-solving effort

**Example scope**:
```markdown
## Scope Definition

**In Scope**:
- Fix flaky `test_concurrent_access` test
- Identify root cause of flakiness
- Prevent similar issues in future

**Out of Scope**:
- Other flaky tests (address separately)
- Performance optimization (not related to flakiness)
- Test framework changes (beyond scope)

**Boundaries**:
- Focus on this specific test
- Solution must not break other tests
- Solution must be maintainable
```

#### 1.3: Define Success Criteria

**Action**: Define what success looks like.

**Success criteria**:
- **Measurable**: Can be quantified
- **Achievable**: Realistic to achieve
- **Time-bound**: When will success be measured?

**Example success criteria**:
```markdown
## Success Criteria

**Primary**:
- Test passes 100% of the time (0% flakiness)
- Measured over 100 consecutive test runs

**Secondary**:
- Test execution time < 1 second (maintain performance)
- No regressions in other tests

**Timeline**:
- Fix implemented within 1 day
- Success verified within 1 week
```

---

### Step 2: Measure

**Action**: Collect data about the problem.

#### 2.1: Collect Baseline Data

**Action**: Measure current state.

**Data to collect**:
- **Failure rate**: How often does it fail?
- **Failure patterns**: When does it fail?
- **Performance metrics**: How long does it take?
- **Error messages**: What errors occur?

**Action**: Run measurements

```bash
# Measure failure rate
for i in {1..100}; do
    cargo make test test_concurrent_access 2>&1 | grep -q "FAILED" && echo "FAILED" || echo "PASSED"
done | sort | uniq -c
# Output: 30 FAILED, 70 PASSED (30% failure rate)

# Measure execution time
time cargo make test test_concurrent_access
# Output: Average 0.5s

# Capture error messages
cargo make test test_concurrent_access 2>&1 | tee error_log.txt
```

#### 2.2: Analyze Data Patterns

**Action**: Look for patterns in the data.

**Patterns to identify**:
- **Frequency**: How often does it occur?
- **Timing**: When does it occur?
- **Conditions**: Under what conditions?
- **Correlations**: What else happens when it fails?

**Example analysis**:
```markdown
## Data Patterns

**Failure Rate**: 30% (30 out of 100 runs)
**Timing**: More frequent in CI (40% vs 20% locally)
**Conditions**: 
- Fails more often with multiple test threads
- Fails more often when system is under load
**Correlations**:
- Always fails with "assertion failed: expected 1000, got 999"
- Suggests race condition
```

---

### Step 3: Analyze

**Action**: Identify root causes using data.

#### 3.1: Root Cause Analysis

**Action**: Use 5 Whys to find root cause.

**5 Whys process**:
1. Why did the test fail? (First why)
2. Why did that happen? (Second why)
3. Why did that happen? (Third why)
4. Why did that happen? (Fourth why)
5. Why did that happen? (Fifth why - root cause)

**Example 5 Whys**:
```markdown
## Root Cause Analysis (5 Whys)

**Problem**: Test fails with "expected 1000, got 999"

1. **Why did the test fail?**
   - Counter value was 999 instead of expected 1000

2. **Why was counter 999 instead of 1000?**
   - One increment operation didn't complete

3. **Why didn't one increment complete?**
   - Race condition - two threads read same value before incrementing

4. **Why did race condition occur?**
   - Mutex lock was released too early, allowing concurrent reads

5. **Why was mutex lock released too early?**
   - Lock scope didn't include the entire increment operation (ROOT CAUSE)

**Root Cause**: Lock scope is too narrow - doesn't protect entire increment operation
```

#### 3.2: Verify Root Cause

**Action**: Confirm root cause with data.

**Verification**:
- Does fixing root cause prevent the problem?
- Does data support root cause hypothesis?
- Are there other contributing factors?

**Action**: Verify root cause

```rust
// Current code (has race condition)
let mut value = counter.lock().unwrap();
let current = *value; // Lock released here
// ... other code ...
*value = current + 1; // Race condition - another thread may have modified value

// Root cause verified: Lock scope too narrow
```

---

### Step 4: Improve

**Action**: Implement solution that addresses root cause.

#### 4.1: Generate Solutions

**Action**: Brainstorm solutions that address root cause.

**Solution criteria**:
- Addresses root cause
- Feasible to implement
- Doesn't break other functionality
- Maintainable
- **DfLSS alignment**: Solutions should address both efficiency (waste elimination) AND quality (defect prevention). See [Root Cause Analysis - DfLSS vs DFSS](./root-cause-analysis.md#dflss-vs-dfss-critical-distinction) for why DfLSS is superior to DFSS.

**Example solutions**:
```markdown
## Solution Options

**Option 1**: Expand lock scope to include entire increment
- Pros: Simple, addresses root cause
- Cons: May reduce parallelism
- Feasibility: High

**Option 2**: Use atomic operations instead of mutex
- Pros: Better performance, no lock needed
- Cons: Requires refactoring
- Feasibility: Medium

**Option 3**: Use single-threaded test execution
- Pros: Eliminates race condition
- Cons: Doesn't test concurrency
- Feasibility: High (but wrong solution - doesn't test what we need)

**Selected**: Option 1 - Expand lock scope (addresses root cause, simple, feasible)
```

#### 4.2: Implement Solution

**Action**: Implement chosen solution.

**Implementation steps**:
1. Make code changes
2. Verify compilation: `cargo make check`
3. Run tests: `cargo make test`
4. Verify fix: Run test multiple times

**Example implementation**:
```rust
// Before (root cause: lock scope too narrow)
let mut value = counter.lock().unwrap();
let current = *value;
drop(value); // Lock released too early
*value = current + 1; // Race condition

// After (fix: expand lock scope)
let mut value = counter.lock().unwrap();
*value += 1; // Entire operation protected by lock
// Lock released here, after operation complete
```

#### 4.3: Verify Improvement

**Action**: Measure improvement against success criteria.

**Verification**:
- Run test multiple times: `for i in {1..100}; do cargo make test test_concurrent_access; done`
- Check failure rate: Should be 0%
- Check performance: Should still be < 1s
- Check other tests: Should still pass

**Success**: If success criteria met, proceed to Step 5. If not, return to Step 3 (Analyze).

---

### Step 5: Control

**Action**: Prevent problem from returning.

#### 5.1: Add Tests

**Action**: Add tests to prevent regression.

**Test strategy**:
- Add test that would catch the problem
- Add test for root cause scenario
- Ensure tests run in CI

**Example**:
```rust
use chicago_tdd_tools::prelude::*;

// Add test that would catch race condition
test!(test_concurrent_increment_no_race, {
    // Test that would fail if race condition returns
    // This test ensures fix prevents regression
});
```

#### 5.2: Create Todo List for Solution Implementation

**CRITICAL**: Do NOT write documents or reports. Create todos and execute them.

**Action**: Create 10+ item todo list for implementing solution and prevention measures.

**Todo list creation**:
1. Create todos for solution implementation steps
2. Create todos for prevention measures (tests, controls)
3. Create todos for verification steps
4. Prioritize by impact (solution first, then prevention)
5. Execute todos systematically

**Example todo list**:
```markdown
## DMAIC Solution Todos (10+ items)

**Solution Implementation**:
- [ ] Implement fix: Expand lock scope to include entire increment operation
- [ ] Update code: Keep lock for entire critical section
- [ ] Verify compilation: `cargo make check`
- [ ] Run tests: `cargo make test`
- [ ] Verify fix works: Run test 100 times, verify 0 failures

**Prevention Measures**:
- [ ] Add test: `test_lock_scope_covers_operation` to catch pattern
- [ ] Add code review checklist item: Lock scope covers entire operation
- [ ] Add inline comment: Document why lock scope is important
- [ ] Verify test catches pattern: Test fails if lock scope too narrow

**Verification**:
- [ ] Verify problem doesn't occur: Run test 100 times, 0 failures
- [ ] Verify no regressions: All other tests still pass
- [ ] Verify prevention works: Test catches pattern
```

**Execution**:
1. Create todos using `todo_write` tool (10+ items minimum)
2. Execute todos one by one (implement solution, add prevention)
3. Mark todos as completed as work is done
4. Verify each step works before moving to next
5. Continue until all todos complete

**Principle**: Execute solution and prevention, don't document them. Todos track progress, fixes prevent recurrence.

#### 5.3: Establish Controls

**Action**: Set up controls to prevent problem recurrence.

**Controls**:
- **Code review**: Check for similar patterns
- **Tests**: Run flaky test detection
- **Monitoring**: Track test failure rates
- **Standards**: Document pattern to avoid

**Example controls** (implement as todos, not markdown):
```markdown
## Controls Todos (10+ items)

**Code Review Controls**:
- [ ] Add checklist item: Lock scope covers entire critical section
- [ ] Add checklist item: No operations between lock acquire and release
- [ ] Update code review process to include checklist

**Test Strategy Controls**:
- [ ] Add flaky test detection to CI pipeline
- [ ] Configure alert if failure rate > 1%
- [ ] Verify alerts work correctly

**Monitoring Controls**:
- [ ] Set up test failure rate tracking
- [ ] Configure monitoring dashboard
- [ ] Set up alerts for failure rate increases

**Standards Controls**:
- [ ] Add standard to coding guidelines: "Lock scope must cover entire operation"
- [ ] Update team documentation with standard
- [ ] Verify standard is followed in code reviews
```

**CRITICAL**: Implement controls as todos and execute them. Don't just document controls - actually implement them.

#### 5.4: Monitor

**Action**: Monitor to ensure problem doesn't return.

**Monitoring**:
- Track metrics over time
- Set up alerts for regression
- Review periodically

**Action**: Set up monitoring

```bash
# Monitor test failure rate
# Run daily: for i in {1..100}; do cargo make test test_concurrent_access; done
# Alert if failure rate > 1%
```

---

## Complete Workflow Example

```bash
# Step 1: Define
# Problem: Flaky test, 30% failure rate
# Scope: Fix this specific test
# Success: 0% failure rate over 100 runs

# Step 2: Measure
for i in {1..100}; do cargo make test test_concurrent_access; done
# Result: 30 failures, 70 passes

# Step 3: Analyze (5 Whys)
# Root cause: Lock scope too narrow

# Step 4: Improve
# Fix: Expand lock scope
cargo make test test_concurrent_access
# Verify: Run 100 times, 0 failures ✅

# Step 5: Control
# Add regression test
# Document solution
# Establish controls
```

## Integration with Other Commands

- **[Root Cause Analysis](./root-cause-analysis.md)** - Use 5 Whys in Analyze step
- **[Gemba Walk](./gemba-walk.md)** - Go to source in Measure and Analyze steps
- **[Poka-Yoke Design](./poka-yoke-design.md)** - Use type system in Improve step to prevent recurrence
- **[Andon Signals](./andon-signals.md)** - Use tests as signals in Control step

## Expert Insights

**Why this matters**: Jumping to solutions without analysis leads to fixing symptoms, not causes. DMAIC ensures systematic problem-solving.

**Key principle**: "Data over assumptions" - Use data to drive decisions, not guesses.

**Remember**: Each step builds on the previous. Don't skip steps. Define clearly, measure accurately, analyze deeply, improve systematically, control consistently.

**DMAIC cycle**: This is iterative. If Improve doesn't work, return to Analyze. If problem returns, strengthen Control.

**DfLSS alignment**: When designing solutions in the Improve step, use DfLSS (Design for Lean Six Sigma) principles - address both efficiency (waste elimination) AND quality (defect prevention) from the start. Don't conflate DfLSS with DFSS (Design for Six Sigma) - DFSS only addresses quality, missing critical waste elimination. Using the wrong methodology is itself a root cause. See [Root Cause Analysis - DfLSS vs DFSS](./root-cause-analysis.md#dflss-vs-dfss-critical-distinction) for detailed explanation.


## Command Execution Pattern

**CRITICAL**: DMAIC commands must:
1. **Create 10+ item todo list** - Not documents/reports
2. **Execute todos** - Implement solutions and controls, not document them
3. **Verify fixes** - Test that solutions work
4. **Complete todos** - Mark todos as done as work completes

**Principle**: Execute solutions and controls, don't document them. Todos track progress, solutions fix problems.

---
