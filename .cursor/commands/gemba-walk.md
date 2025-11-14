# Gemba Walk - Multi-Step Workflow

## Purpose

This command guides agents to "go to the source" (Gemba) - work directly with actual code and data, not abstractions or assumptions. Gemba means the actual place where work happens. Experts always verify at the source.

## Workflow Overview

```
Step 1: Go to Gemba → Step 2: Observe Actual Behavior → Step 3: Verify Claims → Step 4: Create Todo List for Fixing Discrepancies → Step 5: Fix at Source
```

## Step-by-Step Instructions

### Step 1: Go to Gemba (The Actual Place)

**Action**: Read actual source code, not documentation or comments.

**Gemba locations**:
- **Source code** (`src/**/*.rs`) - The actual implementation
- **Test code** (`tests/**/*.rs`) - How code is actually used
- **Example code** (`examples/**/*.rs`) - Real usage patterns
- **Build output** (`cargo make check` output) - Actual compilation behavior

**Avoid**:
- ❌ Documentation that may be outdated
- ❌ Comments that may be wrong
- ❌ Assumptions about how code works
- ❌ Second-hand information

**Action**: Navigate to actual source files

```bash
# Read actual source code
cat src/fixture.rs

# Read actual test code
cat tests/fixture_test.rs

# Read actual example usage
cat examples/basic_test.rs
```

**Principle**: "Go see, ask why, show respect" - Go to the source, understand why it works that way, respect the actual implementation.

---

### Step 2: Observe Actual Behavior

**Action**: Run code and observe what actually happens, not what should happen.

#### 2.1: Run Code

**Action**: Execute code to see actual behavior.

```bash
# Run tests to see actual behavior
cargo make test

# Run examples to see actual usage
cargo make test-examples

# Check compilation to see actual errors
cargo make check
```

#### 2.2: Trace Execution

**Action**: Follow code execution path.

**Methods**:
- Read code flow line by line
- Add debug output to trace execution
- Use debugger to step through code
- Examine stack traces

**Purpose**: Understand actual execution path, not assumed path

#### 2.3: Examine Outputs

**Action**: Look at actual outputs, not expected outputs.

**What to examine**:
- Test results (pass/fail, actual vs expected)
- Compiler errors (actual error messages)
- Runtime behavior (actual performance, actual errors)
- Data structures (actual values, actual types)

**Action**: Capture actual outputs

```bash
# Capture test output
cargo make test > test_output.txt 2>&1

# Capture compilation output
cargo make check > check_output.txt 2>&1

# Examine actual outputs
cat test_output.txt
cat check_output.txt
```

---

### Step 3: Verify Claims

**Action**: Test assertions against actual code behavior.

#### 3.1: Identify Claims

**Action**: List all claims about code behavior.

**Sources of claims**:
- Documentation comments
- Test names/comments
- Code comments
- Variable/function names
- Assumptions

**Example claims**:
- "This function returns Ok for valid input"
- "This test verifies fixture creation"
- "This code handles errors properly"

#### 3.2: Verify Against Gemba

**Action**: Check each claim against actual code behavior.

**Verification steps**:
1. Read actual code implementation
2. Run actual code
3. Compare claim to actual behavior
4. Document discrepancies

**Example verification**:
```rust
// Claim: "This function returns Ok for valid input"
// Gemba check:
fn parse_number(input: &str) -> Result<u32, ParseError> {
    if input.is_empty() {
        return Err(ParseError::EmptyInput); // Actual: Returns Err for empty
    }
    // ... rest of implementation
}

// Discrepancy: Claim says "valid input" but doesn't define what's valid
// Actual: Empty string returns Err, which may or may not be expected
```

#### 3.3: Test Claims

**Action**: Write tests to verify claims match actual behavior.

```bash
# Run tests to verify claims
cargo make test

# If tests fail, claim doesn't match actual behavior
# Fix either claim or code to match
```

---

### Step 4: Create Todo List for Fixing Discrepancies

**CRITICAL**: Do NOT just document discrepancies. Create todos and fix them.

**Action**: Create 10+ item todo list for fixing all discrepancies found.

**Discrepancy types**:
1. **Documentation doesn't match code** - Docs say one thing, code does another
2. **Comments don't match behavior** - Comments describe wrong behavior
3. **Test names don't match test behavior** - Test name claims one thing, test does another
4. **Assumptions don't match reality** - Assumed behavior doesn't match actual

**Action**: Create todo list for fixing discrepancies

```markdown
## Gemba Discrepancy Fix Todos (10+ items)

**Documentation vs Code Fixes**:
- [ ] Fix: `docs/testing/chicago-tdd-guide.md` says `parse_number` accepts empty strings, but code returns `Err`
  - File: `src/parser.rs:45`
  - Actual behavior: Returns `ParseError::EmptyInput` for empty string
  - Decision: Code is correct, update documentation
  - Action: Update `docs/testing/chicago-tdd-guide.md` to match code (or `docs/wip/` for work-in-progress documentation)
  - Verify: Documentation now matches code

**Comments vs Behavior Fixes**:
- [ ] Fix: Comment says "handles all errors" but code panics on overflow
  - File: `src/calculator.rs:123`
  - Actual behavior: `unwrap()` on overflow, panics
  - Decision: Code is wrong, fix code to handle errors
  - Action: Replace `unwrap()` with proper error handling
  - Verify: Code handles errors correctly

**Test Names vs Behavior Fixes**:
- [ ] Fix: Test name says "test_valid_input" but test uses invalid input
  - File: `tests/parser_test.rs:34`
  - Actual behavior: Test uses empty string (invalid)
  - Decision: Test name is wrong, update test name
  - Action: Rename test to `test_invalid_input_empty_string`
  - Verify: Test name matches test behavior

**Verification**:
- [ ] Verify all discrepancies fixed: Run `cargo make test`
- [ ] Verify documentation matches code: Review updated docs
- [ ] Verify comments match behavior: Review updated comments
- [ ] Verify test names match behavior: Review updated tests
```

**Execution**:
1. Create todos using `todo_write` tool (10+ items minimum)
2. Execute todos one by one (fix each discrepancy)
3. Mark todos as completed as fixes are implemented
4. Verify each fix works before moving to next
5. Continue until all discrepancies fixed

**Principle**: Fix discrepancies at source, don't just document them. Todos track progress, fixes eliminate discrepancies.

---

### Step 5: Fix at Source

**Action**: Update code, documentation, or tests to match actual behavior.

#### 5.1: Determine What's Correct

**Action**: Decide whether code or claim is correct.

**Decision criteria**:
- **If code is correct**: Update documentation/comments/tests to match code
- **If claim is correct**: Fix code to match claim
- **If both wrong**: Fix both to match intended behavior

**Example decision**:
```rust
// Code: Returns Err for empty string
// Claim: Documentation says empty string is valid
// Decision: Code is correct (empty string should be invalid)
// Action: Update documentation to match code
```

#### 5.2: Fix at Source

**Action**: Make changes at the source of truth.

**Fix locations**:
- **Code** - Fix implementation if code is wrong
- **Documentation** - Fix docs if code is correct
- **Comments** - Fix comments to match actual behavior
- **Tests** - Fix tests to verify actual behavior

**Action**: Make fixes

```bash
# Fix code if needed
# Edit src/file.rs

# Fix documentation if needed
# Edit docs/testing/chicago-tdd-guide.md (or docs/wip/ for work-in-progress documentation)

# Fix tests if needed
# Edit tests/file_test.rs

# Verify fixes
cargo make check
cargo make test
```

#### 5.3: Verify Fixes

**Action**: Ensure fixes resolved discrepancies.

**Verification**:
- ✅ Code matches documentation
- ✅ Comments match behavior
- ✅ Tests verify actual behavior
- ✅ All tests pass: `cargo make test`
- ✅ Code compiles: `cargo make check`

---

## Complete Workflow Example

```bash
# Step 1: Go to Gemba
cat src/parser.rs
# Read actual implementation

# Step 2: Observe Actual Behavior
cargo make test
# See actual test results

# Step 3: Verify Claims
# Claim: "parse_number accepts empty strings"
# Actual: Returns Err for empty string
# Discrepancy found!

# Step 4: Document Discrepancy
# Added to discrepancy list:
# - Documentation says empty string valid, code returns Err

# Step 5: Fix at Source
# Decision: Code is correct, documentation is wrong
# Fix: Update docs/testing/chicago-tdd-guide.md to match code (or docs/wip/ for work-in-progress docs)
# Verify: cargo make check && cargo make test
```

## Integration with Other Commands

- **[Root Cause Analysis](./root-cause-analysis.md)** - Use 5 Whys to understand why discrepancies exist
- **[DMAIC Problem Solving](./dmaic-problem-solving.md)** - Use DMAIC to systematically fix discrepancies
- **[Eliminate Muda](./eliminate-muda.md)** - Remove waste from outdated documentation/comments
- **[Poka-Yoke Design](./poka-yoke-design.md)** - Use type system to prevent discrepancies

## Expert Insights

**Why this matters**: Working from assumptions or outdated information causes bugs. Experts always verify at the source (Gemba).

**Key principle**: "Go see, ask why, show respect" - Go to the actual code, understand why it works that way, respect the actual implementation.

**Remember**: The code is the source of truth. Documentation, comments, and tests should match the code, not the other way around.

**Genchi Genbutsu**: "Go and see for yourself" - Don't trust second-hand information. Verify at the source.

**DfLSS alignment**: Gemba walk (going to source) supports DfLSS (Design for Lean Six Sigma) by preventing both waste (outdated information causes rework) AND defects (wrong assumptions cause bugs). Don't conflate DfLSS with DFSS (Design for Six Sigma) - DFSS only addresses quality, missing critical waste elimination. See [Root Cause Analysis - DfLSS vs DFSS](./root-cause-analysis.md#dflss-vs-dfss-critical-distinction) for why conflating DfLSS with DFSS is a huge error.


## Command Execution Pattern

**CRITICAL**: Gemba walk commands must:
1. **Create 10+ item todo list** - Not documents/reports
2. **Execute todos** - Fix discrepancies at source, not just document them
3. **Verify fixes** - Test that fixes work
4. **Complete todos** - Mark todos as done as fixes complete

**Principle**: Fix discrepancies at source, don't document them. Todos track progress, fixes eliminate discrepancies.

---
