# Andon Signals (Visual Problem Indicators) - Multi-Step Workflow

## Purpose

This command guides agents to treat compiler errors, test failures, and warnings as Andon signals - visual indicators that something is wrong and work should stop. Andon means "lantern" or "sign" - a visual signal that alerts to problems. Experts stop and fix problems immediately when signals appear.

**ggen-specific**: This workflow is customized for ggen's Rust workspace, cargo make build system, and deterministic output requirements.

## Workflow Overview

```
Step 1: Monitor Andon Signals → Step 2: Stop When Signal Appears → Step 3: Investigate Root Cause → Step 4: Fix Root Cause → Step 5: Verify Signal Cleared
```

## Step-by-Step Instructions

### Step 1: Monitor Andon Signals

**Action**: Watch for visual signals that indicate problems.

**Andon signal types**:

1. **Compiler errors** - Red signals, must stop
   - Pattern: `error[E...]: <description>`
   - Severity: **CRITICAL** - Cannot proceed
   - ggen-specific: Use `cargo make check` (NEVER `cargo check` directly)

2. **Compiler warnings** - Yellow signals, should stop
   - Pattern: `warning: <description>`
   - Severity: **HIGH** - Should fix before proceeding
   - ggen-specific: ggen uses `#![deny(warnings)]` - warnings are errors

3. **Test failures** - Red signals, must stop
   - Pattern: `test ... FAILED`
   - Severity: **CRITICAL** - Cannot proceed
   - ggen-specific: Use `cargo make test` (NEVER `cargo test` directly)

4. **Linting errors** - Yellow/red signals, should stop
   - Pattern: Clippy warnings/errors
   - Severity: **HIGH** - Should fix before proceeding
   - ggen-specific: Use `cargo make lint` (NEVER `cargo clippy` directly)

5. **Performance regressions** - Yellow signals, investigate
   - Pattern: Tests taking longer than expected, SLO violations
   - Severity: **MEDIUM** - Investigate if significant
   - ggen-specific: Use `cargo make slo-check` to verify SLOs

6. **Timeout violations** - Yellow/red signals, should stop
   - Pattern: Commands timing out, build hangs
   - Severity: **HIGH** - Indicates potential deadlock or infinite loop
   - ggen-specific: All commands must have timeout wrappers (see Timeout SLA)

**Action**: Set up signal monitoring

```bash
# Monitor compilation signals (timeout 5s for quick feedback)
cargo make check
# Look for: error[...] or warning: patterns

# Monitor test signals (timeout 10s unit + 30s integration)
cargo make test
# Look for: test ... FAILED patterns

# Monitor linting signals (timeout 5s)
cargo make lint
# Look for: warning: or error: patterns

# Monitor performance SLOs
cargo make slo-check
# Look for: SLO violations

# Verify timeout command exists (before running any tasks)
cargo make timeout-check
```

**Principle**: "Andon signals are visual management" - Make problems immediately visible, don't hide them.

**ggen-specific**: All monitoring commands use `cargo make` with timeout protection. Never use direct `cargo` commands.

---

### Step 2: Stop When Signal Appears

**Action**: Immediately stop work when an Andon signal appears.

#### 2.1: Recognize Signal Severity

**Action**: Determine signal severity and response.

**Signal severity levels**:

- **CRITICAL (Red)** - Must stop immediately
  - Compiler errors (`error[E...]`)
  - Test failures (`test ... FAILED`)
  - **Response**: Stop all work, fix immediately
  - **ggen-specific**: These prevent compilation/testing, blocking all progress

- **HIGH (Yellow)** - Should stop
  - Compiler warnings (`warning:`) - Note: ggen uses `#![deny(warnings)]`, so these are errors
  - Linting errors (clippy warnings/errors)
  - Timeout violations
  - **Response**: Stop current work, fix before proceeding
  - **ggen-specific**: Warnings are treated as errors in ggen due to `#![deny(warnings)]`

- **MEDIUM (Yellow)** - Investigate
  - Performance warnings (SLO violations)
  - Code quality warnings
  - **Response**: Investigate, fix if significant
  - **ggen-specific**: Use `cargo make slo-check` to verify SLOs

**Action**: Classify signal

```markdown
## Andon Signal Classification

### Critical Signals (Stop Immediately)
- [ ] Compiler error: `error[E0425]: cannot find function`
- [ ] Test failure: `test test_name ... FAILED`

### High Signals (Stop and Fix)
- [ ] Compiler warning: `warning: unused variable` (treated as error in ggen)
- [ ] Linting error: `clippy::unwrap_used`
- [ ] Timeout violation: Command exceeded timeout SLA

### Medium Signals (Investigate)
- [ ] Performance warning: SLO violation (first build > 15s, incremental > 2s)
- [ ] Code quality warning: Complexity too high
```

#### 2.2: Stop the Line

**Action**: Stop current work when signal appears.

**Stop the line principles**:
- **Don't ignore** - Never ignore Andon signals
- **Don't proceed** - Don't continue work with signals present
- **Don't hide** - Don't suppress warnings/errors with `#[allow(...)]` without fixing root cause
- **Fix immediately** - Address signal before continuing
- **Never claim completion** - Don't mark work complete with signals present

**Example response**:
```bash
# Signal appeared: Compiler error
cargo make check
# Output: error[E0425]: cannot find function `test_function`

# STOP: Do not proceed with other work
# ACTION: Fix compiler error immediately
# DO NOT: Continue with other tasks, ignore the error, or mark work complete
```

**ggen-specific**: In ggen, warnings are errors due to `#![deny(warnings)]`. Any warning is a CRITICAL signal.

---

### Step 3: Investigate Root Cause

**Action**: Understand why the signal appeared.

#### 3.1: Read Signal Message

**Action**: Carefully read the signal message.

**What to look for**:
- **Error message** - What went wrong?
- **Location** - Where did it occur? (file:line:column)
- **Context** - What was happening when it occurred?
- **ggen-specific**: Check if it's related to cargo make vs direct cargo command usage

**Example**:
```
error[E0425]: cannot find function `test_function` in this scope
  --> crates/ggen-domain/src/graph/export.rs:10:5
   |
10 |     test_function();
   |     ^^^^^^^^^^^^ not found in this scope
```

**Analysis**:
- **What**: Function `test_function` not found
- **Where**: `crates/ggen-domain/src/graph/export.rs:10:5`
- **Why**: Function doesn't exist or not imported
- **ggen-specific**: Check if function was removed during refactoring, verify imports

#### 3.2: Trace Root Cause

**Action**: Use root cause analysis to find why signal appeared.

**Questions to ask**:
- Why did this signal appear?
- What changed that caused it?
- Is this a symptom of a deeper problem?
- **ggen-specific**: Did I use a direct cargo command instead of `cargo make`?

**Example root cause analysis**:
```markdown
## Root Cause Analysis

**Signal**: Compiler error - function not found
**Why #1**: Function `test_function` doesn't exist
**Why #2**: Function was removed during refactoring
**Why #3**: Tests weren't updated after refactoring
**Why #4**: Expert tests were added but function signature changed
**Root Cause**: Missing test update after refactoring export module
```

**Reference**: See [Root Cause Analysis](./root-cause-analysis.md) for detailed 5 Whys process

#### 3.3: Verify Root Cause

**Action**: Confirm root cause hypothesis.

**Verification**:
- Does fixing root cause clear the signal?
- Does data support root cause hypothesis?
- Are there other contributing factors?
- **ggen-specific**: Verify fix doesn't introduce new signals

---

### Step 4: Fix Root Cause

**Action**: Address the underlying cause, not just the symptom.

#### 4.1: Fix the Problem

**Action**: Implement fix that addresses root cause.

**Fix principles**:
- **Fix root cause** - Not just symptom
- **Fix completely** - Don't leave partial fixes
- **Fix safely** - Don't introduce new problems
- **ggen-specific**: Use `cargo make` commands, not direct cargo commands

**Example fix**:
```rust
// Root cause: Function removed, test not updated
// Fix: Add missing function or update test

// Option 1: Add missing function
fn test_function() -> Result<()> {
    // Implementation with proper error handling
    Ok(())
}

// Option 2: Update test to use correct function
// test_function() -> actual_function()
```

**ggen-specific patterns**:
- Use `ggen_utils::error::Result` for error handling (not `anyhow` in libraries)
- Never use `unwrap()` or `expect()` in production code
- Use `cargo make` commands for all operations
- Ensure timeout wrappers are in place

#### 4.2: Verify Fix

**Action**: Ensure fix resolves the signal.

**Verification steps**:
1. Fix the problem
2. Re-run signal check using `cargo make`
3. Verify signal cleared
4. **ggen-specific**: Run full validation suite

**Example**:
```bash
# Fix applied
# Re-check signal
cargo make check
# Expected: No errors, signal cleared ✅

# Verify tests still pass
cargo make test
# Expected: All tests pass ✅

# Verify linting
cargo make lint
# Expected: No warnings ✅
```

**ggen-specific**: Always run full validation before marking complete:
```bash
cargo make timeout-check  # Verify timeout command exists
cargo make check          # No compiler errors
cargo make test           # All tests pass
cargo make lint           # No linting errors
cargo make slo-check      # SLOs met (if applicable)
```

---

### Step 5: Verify Signal Cleared

**Action**: Confirm signal is resolved and won't return.

#### 5.1: Verify Signal Cleared

**Action**: Run checks to confirm signal gone.

**Verification**:
- ✅ Compiler errors cleared: `cargo make check`
- ✅ Test failures cleared: `cargo make test`
- ✅ Warnings cleared: `cargo make lint` (ggen uses `#![deny(warnings)]`)
- ✅ No new signals appeared
- ✅ **ggen-specific**: Timeout command verified: `cargo make timeout-check`

**Example**:
```bash
# Verify all signals cleared
cargo make timeout-check  # Verify timeout command exists
cargo make check          # No errors ✅
cargo make test           # All tests pass ✅
cargo make lint           # No warnings ✅ (warnings are errors in ggen)
cargo make slo-check      # SLOs met ✅ (if applicable)
```

**ggen-specific**: ggen's `#![deny(warnings)]` means warnings are compilation errors. All warnings must be fixed.

#### 5.2: Prevent Signal Return

**Action**: Add controls to prevent signal from returning.

**Prevention methods**:
- **Tests** - Add tests to catch regression
- **Linting** - Enable linting rules in CI
- **Documentation** - Document why fix was needed
- **Code review** - Review to prevent similar issues
- **ggen-specific**: Use Poka-Yoke types to prevent invalid states

**Example**:
```rust
// Add test to prevent regression
#[test]
fn test_function_exists() -> Result<()> {
    // Test that would fail if function removed again
    test_function()?;
    Ok(())
}
```

**ggen-specific**: Use type-level prevention (Poka-Yoke) to prevent entire classes of errors:
```rust
// Poka-Yoke: Use newtypes to prevent invalid states
pub struct ValidatedPackageName(String);

impl ValidatedPackageName {
    pub fn new(name: &str) -> Result<Self> {
        // Validation prevents invalid states at compile time
        if name.is_empty() {
            return Err(Error::new("Package name cannot be empty"));
        }
        // ... more validation
        Ok(Self(name.to_string()))
    }
}
```

#### 5.3: Monitor for New Signals

**Action**: Continue monitoring for new signals.

**Monitoring**:
- Run checks regularly
- Don't ignore warnings (they're errors in ggen)
- Fix signals immediately
- **ggen-specific**: Use `cargo make` commands with timeout protection

---

## Complete Workflow Example

```bash
# Step 1: Monitor Andon Signals
cargo make timeout-check  # Verify timeout command exists
cargo make check
# Signal appeared: error[E0425]: cannot find function

# Step 2: Stop When Signal Appears
# STOP: Do not proceed with other work
# ACTION: Fix compiler error immediately

# Step 3: Investigate Root Cause
# Root cause: Function removed during refactoring, test not updated
# Investigation: Used 5 Whys to trace to root cause

# Step 4: Fix Root Cause
# Fix: Add missing function or update test
# Applied fix using proper error handling

# Step 5: Verify Signal Cleared
cargo make check   # No errors ✅
cargo make test    # All tests pass ✅
cargo make lint    # No warnings ✅ (warnings are errors in ggen)
# Signal cleared, work can proceed
```

## Andon Signal Response Matrix

| Signal Type | Severity | Response | Example | ggen-specific |
|------------|----------|----------|---------|---------------|
| Compiler error | CRITICAL | Stop immediately, fix now | `error[E0425]` | Use `cargo make check` |
| Test failure | CRITICAL | Stop immediately, fix now | `test ... FAILED` | Use `cargo make test` |
| Compiler warning | CRITICAL | Stop immediately, fix now | `warning: unused` | ggen uses `#![deny(warnings)]` - warnings are errors |
| Linting error | HIGH | Stop current work, fix before proceeding | `clippy::unwrap_used` | Use `cargo make lint` |
| Timeout violation | HIGH | Stop current work, fix before proceeding | Command timeout | Verify timeout wrappers |
| Performance warning | MEDIUM | Investigate, fix if significant | SLO violation | Use `cargo make slo-check` |

## ggen-Specific Andon Signal Patterns

### Compiler Errors
- **Pattern**: `error[E...]: <description>`
- **Detection**: `cargo make check`
- **Response**: Stop immediately, fix root cause
- **Never**: Use direct `cargo check` - always use `cargo make check`

### Test Failures
- **Pattern**: `test ... FAILED`
- **Detection**: `cargo make test`
- **Response**: Stop immediately, fix failing test
- **Never**: Use direct `cargo test` - always use `cargo make test`
- **ggen-specific**: Tests must verify observable outputs/state changes (Chicago TDD)

### Compiler Warnings
- **Pattern**: `warning: <description>`
- **Detection**: `cargo make check` (warnings are errors in ggen)
- **Response**: Stop immediately - warnings are errors due to `#![deny(warnings)]`
- **Never**: Suppress with `#[allow(...)]` without fixing root cause

### Linting Errors
- **Pattern**: Clippy warnings/errors
- **Detection**: `cargo make lint`
- **Response**: Stop current work, fix before proceeding
- **Never**: Use direct `cargo clippy` - always use `cargo make lint`

### Timeout Violations
- **Pattern**: Commands timing out, build hangs
- **Detection**: Timeout wrapper failures
- **Response**: Stop current work, investigate deadlock/infinite loop
- **ggen-specific**: All commands must have timeout wrappers (see Timeout SLA)

### Performance SLO Violations
- **Pattern**: Build/test times exceed SLOs
- **Detection**: `cargo make slo-check`
- **Response**: Investigate, fix if significant
- **ggen-specific SLOs**:
  - First build ≤ 15s
  - Incremental ≤ 2s
  - RDF processing ≤ 5s for 1k+ triples
  - Generation memory ≤ 100MB
  - CLI scaffolding ≤ 3s end-to-end

## Integration with Other Commands

- **[Root Cause Analysis](./root-cause-analysis.md)** - Use 5 Whys in Step 3 to find root cause
- **[DMAIC Problem Solving](./dmaic-problem-solving.md)** - Use DMAIC to systematically fix signals
- **[Gemba Walk](./gemba-walk.md)** - Go to source in Step 3 to investigate
- **[Poka-Yoke Design](./poka-yoke-design.md)** - Use type system in Step 4 to prevent signals
- **[Expert Testing Patterns](./expert-testing-patterns.md)** - Add tests in Step 5.2 to prevent regression

## Expert Insights

**Why this matters**: Ignoring signals leads to accumulating problems. Experts treat every signal as important and fix them immediately.

**Key principle**: "Stop the line" - When an Andon signal appears, stop work and fix the problem immediately. Don't proceed with problems present.

**Remember**: Andon signals are visual management. They make problems immediately visible. Don't hide them, don't ignore them, fix them.

**Andon culture**: In Lean manufacturing, any worker can stop the production line if they see a problem. In coding, any developer should stop and fix problems when signals appear. This prevents defects from propagating.

**DfLSS alignment**: Andon signals help prevent both defects (quality) and waste (efficiency) - stopping problems early prevents rework (waste) and defects from propagating (quality). This aligns with DfLSS (Design for Lean Six Sigma) principles. Don't conflate DfLSS with DFSS (Design for Six Sigma) - DFSS only addresses quality, missing critical waste elimination. See [Root Cause Analysis - DfLSS vs DFSS](./root-cause-analysis.md#dflss-vs-dfss-critical-distinction) for why conflating DfLSS with DFSS is a huge error.

**ggen-specific insights**:
- **Warnings are errors**: ggen uses `#![deny(warnings)]`, so all warnings are CRITICAL signals
- **Use cargo make**: Never use direct cargo commands - always use `cargo make` with timeout protection
- **Timeout SLA**: All commands must have timeout wrappers to prevent hangs
- **Test behavior**: Tests must verify observable outputs/state changes, not just that functions exist
- **Never claim completion with signals**: Don't mark work complete until all signals are cleared

## Definition of Done / Completion Workflow

**BEFORE MARKING ANY TASK AS COMPLETE - MANDATORY VALIDATION CHECKS**:

1. **Verify Timeout Command**: Run `cargo make timeout-check` to verify timeout command exists
2. **Check for Compiler Errors (CRITICAL SIGNAL)**: Run `cargo make check`
   - **IF ERRORS FOUND**: STOP THE LINE - Do not proceed. Fix compiler errors immediately.
   - **VERIFY**: No `error[E...]` patterns in output - must be clean
3. **Check for Compiler Warnings (CRITICAL SIGNAL in ggen)**: Review `cargo make check` output
   - **IF WARNINGS FOUND**: STOP THE LINE - Fix warnings before proceeding (warnings are errors in ggen)
   - **VERIFY**: No `warning:` patterns in output - must be clean
4. **Run Tests (CRITICAL SIGNAL)**: Run `cargo make test` to verify all tests pass
   - **IF TESTS FAIL**: STOP THE LINE - Do not proceed. Extract failing test names, create rich todos with:
     * Test name
     * Error message
     * File/line location
     * Root cause analysis (use 5 Whys)
     * Proposed fix
     * Status (pending/in_progress/completed)
   - **VERIFY**: No `test ... FAILED` patterns - all tests must pass
5. **Check for Linting Errors (HIGH SIGNAL)**: Run `cargo make lint`
   - **IF LINTING ERRORS FOUND**: STOP THE LINE - Fix linting errors before proceeding
   - **VERIFY**: No clippy warnings/errors - must be clean
6. **Verify Performance SLOs**: Run `cargo make slo-check` (if applicable)
   - **IF SLOs NOT MET**: Investigate performance regressions
   - **VERIFY**: All SLOs met
7. **Systematic Fixing**: If any signals found:
   - Batch create 5-10+ related todos in single call for systematic fixing
   - Fix systematically: Read failure message → Identify root cause → Fix issue → Run specific check → Verify signal cleared → Update todo status → Remove when fixed
   - Re-run validation checks to verify all signals cleared
   - If still failing, return to step 2
8. **Final Verification - All Signals Cleared**:
   - ✅ `cargo make timeout-check` - Timeout command exists
   - ✅ `cargo make check` - No compiler errors or warnings
   - ✅ `cargo make test` - All tests pass
   - ✅ `cargo make lint` - No linting errors
   - ✅ `cargo make slo-check` - SLOs met (if applicable)
   - ✅ All failing tests fixed and removed from todos
   - ✅ No compilation errors
   - ✅ No test failures
   - ✅ No pending test-related todos

**ONLY mark complete when ALL signals are cleared and ALL validation checks pass**

**NEVER claim completion without running validation checks**

**NEVER ignore signals - they must be fixed before proceeding**

**NEVER proceed with signals present - STOP THE LINE**

**Test results are truth - code doesn't work if tests don't pass**

**Andon Signals are truth - if signals present, work is not complete**

