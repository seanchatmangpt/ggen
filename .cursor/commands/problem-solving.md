# Problem Solving - Essential Patterns

## Purpose

This command guides agents through essential problem-solving techniques: **5 Whys for root cause analysis**, **Andon Signals to stop and fix problems**, and **DMAIC for systematic problem-solving**. These three patterns work together to find root causes, prevent problems early, and fix them systematically.

---

## Pattern 1: 5 Whys (Root Cause Analysis)

### Overview

The 5 Whys technique finds underlying causes by asking "why?" repeatedly until reaching the root cause. Experts dig deeper to find root causes rather than fixing symptoms.

### Step-by-Step

#### Step 1: Define the Problem

**Action**: Clearly state the observable problem (symptom).

**Problem definition format**:
- **What**: What is the observable problem?
- **Where**: Where does it occur?
- **When**: When does it occur?
- **Impact**: What is the impact?

**Example**:
```markdown
## Problem Definition

**What**: Test fails with "assertion failed: expected 1000, got 999"
**Where**: `tests/concurrent_test.rs` - `test_concurrent_increment`
**When**: Approximately 30% of test runs, more frequent in CI
**Impact**: Blocks CI/CD pipeline, causes false negatives
```

#### Step 2-5: Ask Why Repeatedly

**Action**: Continue asking why until reaching root cause (usually 3-5 whys).

**Example 5 Whys**:
```markdown
**Why #1**: Why did the test fail?
**Answer**: Counter value was 999 instead of expected 1000

**Why #2**: Why was counter 999 instead of 1000?
**Answer**: One increment operation didn't complete

**Why #3**: Why didn't one increment complete?
**Answer**: Race condition - two threads read same value before incrementing

**Why #4**: Why did race condition occur?
**Answer**: Mutex lock was released too early, allowing concurrent reads

**Why #5**: Why was mutex lock released too early?
**Answer**: Lock scope didn't include the entire increment operation (ROOT CAUSE)
```

#### Step 3: Verify Root Cause

**Action**: Confirm root cause hypothesis by testing fix.

**Verification**:
- If we fix the root cause, will the problem be prevented?
- Does the data support the root cause hypothesis?
- Are there other contributing factors?

#### Step 4: Fix Root Cause

**Action**: Implement fix that addresses root cause, not just symptom.

**Fix Example**:
```rust
// Before (root cause: lock scope too narrow)
let mut value = counter.lock().unwrap();
let current = *value;
drop(value); // Lock released too early
*value = current + 1; // Race condition possible

// After (root cause fixed: entire operation protected by lock)
let mut value = counter.lock().unwrap();
*value += 1; // Entire operation protected by lock
```

#### Step 5: Prevent Recurrence

**Action**: Add tests and controls to prevent root cause from returning.

**Prevention Methods**:
- **Tests** - Add test that would catch the problem
- **Code review** - Add checklist items to prevent similar issues
- **Inline comments** - Document why fix was needed (in code, not separate doc)
- **Standards** - Implement pattern enforcement

### 5 Whys Best Practices

**Guidelines**:
1. **Start with symptom** - Begin with observable problem, not assumptions
2. **Ask why, not who** - Focus on process, not blame
3. **Dig deep** - Usually need 3-5 whys to find root cause
4. **Verify root cause** - Test that fixing it prevents problem
5. **Fix root cause** - Not just symptom

**Common mistakes**:
- ❌ Stopping too early (fixing symptom, not cause)
- ❌ Blaming people instead of process
- ❌ Not verifying root cause
- ❌ Fixing symptoms instead of root cause

---

## Pattern 2: Andon Signals (Stop and Fix)

### Overview

Treat compiler errors, test failures, and warnings as **Andon signals** - visual indicators that work should stop. Andon means "lantern" or "sign". Experts stop and fix problems immediately when signals appear.

### Andon Signal Types

| Signal Type | Severity | Response | Example |
|------------|----------|----------|---------|
| Compiler error | CRITICAL | Stop immediately, fix now | `error[E0425]` |
| Test failure | CRITICAL | Stop immediately, fix now | `test ... FAILED` |
| Compiler warning | HIGH | Stop current work, fix before proceeding | `warning: unused` |
| Linting error | HIGH | Stop current work, fix before proceeding | `clippy::unwrap_used` |
| Performance warning | MEDIUM | Investigate, fix if significant | Test timeout |

### Workflow

#### Step 1: Monitor Signals

**Action**: Watch for visual signals that indicate problems.

```bash
# Monitor compilation signals
cargo make check
# Look for: error[...] or warning: patterns

# Monitor test signals
cargo make test
# Look for: test ... FAILED patterns

# Monitor linting signals
cargo make lint
# Look for: warning: or error: patterns
```

**Principle**: "Andon signals are visual management" - Make problems immediately visible, don't hide them.

#### Step 2: Stop When Signal Appears

**Action**: Immediately stop work when an Andon signal appears.

**Stop the line principles**:
- **Don't ignore** - Never ignore Andon signals
- **Don't proceed** - Don't continue work with signals present
- **Don't hide** - Don't suppress warnings/errors
- **Fix immediately** - Address signal before continuing

#### Step 3: Investigate Root Cause

**Action**: Use 5 Whys (above) to understand why signal appeared.

**Trace Root Cause**:
- Why did this signal appear?
- What changed that caused it?
- Is this a symptom of a deeper problem?

#### Step 4: Fix Root Cause

**Action**: Address the underlying cause, not just the symptom.

**Fix principles**:
- **Fix root cause** - Not just symptom
- **Fix completely** - Don't leave partial fixes
- **Fix safely** - Don't introduce new problems

#### Step 5: Verify Signal Cleared

**Action**: Confirm signal is resolved and won't return.

**Verification**:
- ✅ Compiler errors cleared: `cargo make check`
- ✅ Test failures cleared: `cargo make test`
- ✅ Warnings cleared: `cargo make lint`
- ✅ No new signals appeared

### Andon Culture

**Why this matters**: Ignoring signals leads to accumulating problems. Experts treat every signal as important and fix them immediately.

**Key principle**: "Stop the line" - When an Andon signal appears, stop work and fix the problem immediately. Don't proceed with problems present. This prevents defects from propagating and prevents waste from accumulated problems.

---

## Pattern 3: DMAIC (Systematic Problem-Solving)

### Overview

DMAIC (Define, Measure, Analyze, Improve, Control) is a data-driven approach to solving problems systematically. Use DMAIC when problems are complex and require systematic investigation.

### Workflow

```
Step 1: Define → Step 2: Measure → Step 3: Analyze → Step 4: Improve → Step 5: Control
```

#### Step 1: Define

**Action**: Clearly define problem, scope, and success criteria.

**Problem Statement**:
```markdown
## Problem Statement

**What**: Tests fail intermittently (flaky tests)
**Where**: `tests/integration_test.rs` - `test_concurrent_access`
**When**: Approximately 30% of test runs, more frequent in CI
**Impact**: Blocks CI/CD pipeline, causes false negatives
**Who**: All developers, CI/CD system
```

**Success Criteria**:
- Primary: Test passes 100% of the time (0% flakiness), measured over 100 consecutive test runs
- Secondary: Test execution time < 1 second, no regressions in other tests
- Timeline: Fix implemented within 1 day, success verified within 1 week

#### Step 2: Measure

**Action**: Collect data about the problem.

```bash
# Measure failure rate
for i in {1..100}; do
    cargo make test test_concurrent_access 2>&1 | grep -q "FAILED" && echo "FAILED" || echo "PASSED"
done | sort | uniq -c
# Output: 30 FAILED, 70 PASSED (30% failure rate)

# Analyze data patterns
# Failure Rate: 30% (30 out of 100 runs)
# Timing: More frequent in CI (40% vs 20% locally)
# Conditions: Fails more often with multiple test threads
# Correlations: Always fails with "assertion failed: expected 1000, got 999"
```

#### Step 3: Analyze

**Action**: Identify root causes using 5 Whys (see Pattern 1).

**Root Cause Analysis**:
- Use 5 Whys to drill down
- Verify root cause with data
- Check for contributing factors

#### Step 4: Improve

**Action**: Implement solution that addresses root cause.

**Solution Criteria**:
- Addresses root cause
- Feasible to implement
- Doesn't break other functionality
- Maintainable

**Implementation**:
1. Make code changes
2. Verify compilation: `cargo make check`
3. Run tests: `cargo make test`
4. Verify fix: Run test multiple times
5. Add regression test to prevent recurrence

#### Step 5: Control

**Action**: Prevent problem from returning.

**Control Measures**:
- **Tests** - Add tests to prevent regression
- **Code review** - Add checklist items to prevent similar issues
- **Documentation** - Document pattern to avoid
- **Monitoring** - Track test failure rates

**CRITICAL**: Implement controls as todos and execute them. Don't just document - actually implement.

---

## When to Use Each Pattern

| Pattern | When to Use |
|---------|-----------|
| **5 Whys** | Quick root cause analysis, simple problems, need to dig deeper |
| **Andon Signals** | Immediate signal (compiler error, test failure, warning), need to stop and fix now |
| **DMAIC** | Complex problems, need systematic investigation, multiple factors, measurement important |

### Quick Decision Tree

1. **Signal appeared** (compiler error, test failure, warning)?
   - **YES**: Use **Andon Signals** - Stop and investigate immediately
   - Use **5 Whys** within Andon workflow to find root cause

2. **Quick root cause investigation?**
   - **YES**: Use **5 Whys** - Find root cause in minutes

3. **Complex problem, need systematic approach?**
   - **YES**: Use **DMAIC** - Systematic investigation with measurement and data

---

## Integration

These patterns work together:
- **Andon Signals** (Pattern 2) trigger investigation
- **5 Whys** (Pattern 1) find the root cause quickly
- **DMAIC** (Pattern 3) provides systematic approach for complex problems

**Execution Pattern**:
1. **Monitor** for Andon signals
2. **Use 5 Whys** for quick root cause when signal appears
3. **Escalate to DMAIC** if problem is complex or systematic approach needed
4. **Implement controls** to prevent recurrence (from 5 Whys or DMAIC)
5. **Continue monitoring** to ensure problem doesn't return

---

## Command Execution Pattern

**CRITICAL**: All problem-solving must:
1. **Create 10+ item todo list** - Not documents/reports
2. **Execute todos** - Implement fixes and prevention, not document them
3. **Verify fixes** - Test that fixes work
4. **Complete todos** - Mark todos as done as work completes

**Principle**: Fix root causes and implement prevention, don't document them. Todos track progress, fixes prevent recurrence.

---

## Expert Insights

**Why this matters**: Fixing symptoms doesn't solve problems. Root cause analysis + systematic approaches + signal-based stopping prevents problems from recurring.

**Key principles**:
- "Ask why five times" - Dig 3-5 levels deep to find root cause
- "Stop the line" - When signals appear, stop and fix immediately
- "Data over assumptions" - Use data to drive decisions, not guesses
- "Fix root cause, not symptom" - Prevent problems, don't just address symptoms

**Remember**: Root cause is usually a process or design issue, not a person or one-time event. Focus on fixing the process, not blaming people.

