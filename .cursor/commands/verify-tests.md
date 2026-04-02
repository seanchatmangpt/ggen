# Verify Tests Before Completion - Multi-Step Workflow

## Purpose

This command guides agents through the complete workflow of running tests, identifying failures, fixing issues, and ensuring all tests pass before marking work as complete. It breaks down the complex process into clear, sequential steps with validation checkpoints.

## Workflow Overview

```
Step 1: Run Tests → Step 2: Analyze Results → Step 3: Fix Failures → Step 4: Re-Run Tests → Step 5: Verify Completion
```

## Step-by-Step Instructions

### Step 1: Run Test Suite

**Action**: Run all tests to identify any failures.

```bash
cargo make test
```

**What this does**:
- Runs all unit tests
- Runs all integration tests
- Runs all example tests
- Runs property-based tests (if feature enabled)
- Runs mutation tests (if feature enabled)

**Expected Result**: All tests pass (exit code 0)

**If this step fails**: Proceed to Step 2 (Analyze Results)

**If this step succeeds**: Skip to Step 5 (Verify Completion)

**Note**: Always use `cargo make test`, never `cargo test` directly. See [Build System Practices](../rules/build-system-practices.mdc).

---

### Step 2: Analyze Test Results

**Action**: Parse test output to identify all failures and categorize them.

#### 2.1: Extract Failure Information

**Look for these patterns in output**:

**Compilation Errors**:
```
error[E...]: <description>
  --> src/file.rs:line:column
```

**Test Failures**:
```
test test_name ... FAILED
```

**Panics**:
```
thread 'test_name' panicked at '<message>', src/file.rs:line:column
```

**Timeouts**:
```
test test_name ... timeout
```

#### 2.2: Categorize Failures

**Create failure list**:

```markdown
## Test Failures

### Compilation Errors
- [ ] `src/fixture.rs:123` - Error: `expected type, found ...`

### Test Failures
- [ ] `test_fixture_creation` - Error: `Fixture creation failed`
- [ ] `test_builder_pattern` - Error: `assertion failed: expected 10, got 5`

### Panics
- [ ] `test_async_operation` - Panic: `called Result::unwrap() on an Err value`

### Timeouts
- [ ] `test_slow_operation` - Timeout after 60s
```

#### 2.3: Prioritize Fixes

**Priority Order**:
1. **Compilation errors** - Must fix first (blocks everything)
2. **Test failures** - Fix by test importance (critical path first)
3. **Panics** - Fix immediately (indicates bugs)
4. **Timeouts** - Fix or optimize (may indicate performance issues)

---

### Step 3: Fix Test Failures

**Action**: Systematically fix each failure category.

#### 3.1: Fix Compilation Errors

**For each compilation error**:

**Step 3.1.1**: Read error message carefully
- Understand what the compiler is complaining about
- Identify the root cause

**Step 3.1.2**: Fix the error
- Update code to resolve compilation issue
- Ensure type safety
- Fix import statements if needed

**Step 3.1.3**: Verify fix
```bash
cargo make check
```

**Step 3.1.4**: Repeat until all compilation errors fixed

**Common Fixes**:
- Missing imports: Add `use` statements
- Type mismatches: Fix type annotations
- Missing features: Enable feature flags in `Cargo.toml`
- Syntax errors: Fix syntax issues

**Reference**: See [Chicago TDD Guide](../../docs/testing/chicago-tdd-guide.md) for troubleshooting

#### 3.2: Fix Test Failures

**For each test failure**:

**Step 3.2.1**: Read test failure message
- Understand what the test expected vs. what it got
- Identify the root cause

**Step 3.2.2**: Determine if test or implementation is wrong
- Review test logic
- Review implementation logic
- Check if test needs updating or implementation needs fixing

**Step 3.2.3**: Fix the issue
- Update test if test is wrong
- Update implementation if implementation is wrong
- Ensure test follows AAA pattern (see [Chicago TDD Guide](../../docs/testing/chicago-tdd-guide.md))

**Step 3.2.4**: Verify fix
```bash
cargo make test test_name
```

**Step 3.2.5**: Repeat for each failing test

**Common Fixes**:
- Wrong expected values: Update assertions
- Missing setup: Add Arrange phase
- Async issues: Ensure proper async handling
- Feature flags: Enable required features

#### 3.3: Fix Panics

**For each panic**:

**Step 3.3.1**: Identify panic source
- Read stack trace
- Find the exact line causing panic

**Step 3.3.2**: Fix panic source
- Replace `unwrap()`/`expect()` with proper error handling
- Add null checks
- Fix index out of bounds
- Handle edge cases

**Step 3.3.3**: Verify fix
```bash
cargo make test test_name
```

**Step 3.3.4**: Repeat for each panic

**Common Fixes**:
- `unwrap()` on `None`: Use `match` or `?` operator
- Index out of bounds: Add bounds checking
- Division by zero: Add zero checks
- Null pointer: Add null checks

**Reference**: See [Expert Testing Patterns](./expert-testing-patterns.md) for panic safety testing

#### 3.4: Fix Timeouts

**For each timeout**:

**Step 3.4.1**: Identify slow operation
- Review test code
- Find the operation taking too long

**Step 3.4.2**: Optimize or mock
- Optimize slow code
- Mock external dependencies
- Increase timeout if legitimate
- Use test fixtures for setup

**Step 3.4.3**: Verify fix
```bash
cargo make test test_name
```

**Step 3.4.4**: Repeat for each timeout

**Common Fixes**:
- Mock external APIs
- Use test fixtures
- Optimize algorithms
- Increase timeout for legitimate slow operations

---

### Step 4: Re-Run Tests

**Action**: Run tests again to verify all fixes worked.

```bash
cargo make test
```

**Expected Result**: All tests pass (exit code 0)

**If this step fails**: 
- Return to Step 2
- Identify remaining failures
- Fix them in Step 3
- Repeat until all tests pass

**If this step succeeds**: Proceed to Step 5

**CRITICAL**: Do not mark work as complete until Step 4 passes completely.

---

### Step 5: Verify Completion

**Action**: Final verification that work is complete.

#### 5.1: Verify All Tests Pass

```bash
cargo make test
```

**Expected**: Exit code 0, all tests pass

#### 5.2: Verify Compilation

```bash
cargo make check
```

**Expected**: Exit code 0, no compilation errors

#### 5.3: Verify No Pending Test Fixes

**Check**: Review todo list for any pending test fixes

**Action**: Remove completed test fixes from todo list

**Expected**: No pending test fixes remain

#### 5.4: Mark Work Complete

**Only when**:
- ✅ All tests pass (`cargo make test` exits with code 0)
- ✅ No compilation errors (`cargo make check` succeeds)
- ✅ No test failures
- ✅ No pending test fixes in todo list

**Then**: Mark work as complete

---

## Advanced: Running Specific Test Suites

### Run Unit Tests Only

```bash
cargo make test-unit
```

**Use when**: Quick feedback during development

### Run Example Tests

```bash
cargo make test-examples
```

**Use when**: Verifying example code works

### Run Property-Based Tests

```bash
cargo make test-property
```

**Use when**: Testing with `property-testing` feature enabled

### Run Mutation Tests

```bash
cargo make test-mutation
```

**Use when**: Testing with `mutation-testing` feature enabled

### Run Single-Threaded Tests

```bash
cargo make test-single-threaded
```

**Use when**: 
- Tests are flaky
- Need deterministic execution
- Debugging race conditions

### Run Verbose Tests

```bash
cargo make test-verbose
```

**Use when**: Need detailed output for debugging

---

## Failure Pattern Reference

### Compilation Errors

**Pattern**: `error[E...]: <description>`

**Example**:
```
error[E0425]: cannot find function `test_function` in this scope
  --> src/test.rs:10:5
   |
10 |     test_function();
   |     ^^^^^^^^^^^^ not found in this scope
```

**Fix**: Add missing function or import

### Test Failures

**Pattern**: `test test_name ... FAILED`

**Example**:
```
test test_fixture_creation ... FAILED

---- test_fixture_creation stdout ----
thread 'test_fixture_creation' panicked at 'assertion failed: `(left == right)`
  left: `0`,
 right: `1`', src/fixture.rs:123:5
```

**Fix**: Review assertion, fix test or implementation

### Panics

**Pattern**: `thread 'test_name' panicked`

**Example**:
```
thread 'test_builder' panicked at 'called `Result::unwrap()` on an `Err` value: "error"', src/builders.rs:45:23
```

**Fix**: Replace `unwrap()` with proper error handling

### Timeouts

**Pattern**: `test test_name ... timeout`

**Example**:
```
test test_slow_operation ... timeout
```

**Fix**: Optimize code or increase timeout

---

## Complete Workflow Example

```bash
# Step 1: Run Tests
cargo make test
# Output: 2 tests failed

# Step 2: Analyze Results
# Found:
# - test_fixture_creation: FAILED - assertion failed
# - test_builder: FAILED - panic on unwrap()

# Step 3: Fix Failures
# Fix test_fixture_creation: Update expected value
# Fix test_builder: Replace unwrap() with proper handling

# Step 4: Re-Run Tests
cargo make test
# All tests pass ✅

# Step 5: Verify Completion
cargo make check  # Compilation OK
cargo make test   # All tests pass
# No pending test fixes in todo list
# Mark work complete ✅
```

## Error Handling

### If Tests Fail Repeatedly

**After 3 attempts**:
- Document issue in todo list
- Create detailed failure report
- Consider if test is correct
- Ask for help if stuck

### If Tests Are Flaky

**Action**:
- Add to todo list: "Fix flaky test: `test_name`"
- Use `cargo make test-single-threaded` for deterministic execution
- Review test isolation
- Check for race conditions

### If Compilation Errors Persist

**Action**:
- Review error messages carefully
- Check feature flags
- Verify dependencies
- Consider if architecture change needed

**Reference**: See [Build System Practices](../rules/build-system-practices.mdc)

## Best Practices

1. **Run tests frequently** - Don't wait until the end
2. **Fix immediately** - Address failures as they occur
3. **One fix at a time** - Fix and verify each issue separately
4. **Verify after fixes** - Always re-run tests after fixes
5. **Document failures** - Add to todo list if not immediately fixable
6. **Never skip validation** - All tests must pass before completion

## Documentation References

- **[Build System Practices](../rules/build-system-practices.mdc)** - Build commands
- **[Chicago TDD Guide](../../docs/testing/chicago-tdd-guide.md)** - Testing standards
- **[Expert Testing Patterns](./expert-testing-patterns.md)** - Expert patterns

## Quick Reference

```bash
# Full workflow
cargo make test                    # Step 1: Run tests
# Analyze failures                 # Step 2: Analyze
# Fix failures                     # Step 3: Fix
cargo make test                    # Step 4: Re-run
cargo make check                   # Step 5: Verify compilation
# Mark complete                    # Step 5: Verify completion
```

