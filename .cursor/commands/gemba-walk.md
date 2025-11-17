# Gemba Walk - Multi-Step Workflow

## Purpose

This command guides agents to "go to the source" (Gemba) - work directly with actual code and data in ggen, not abstractions or assumptions. Gemba means the actual place where work happens. Experts always verify at the source.

## Workflow Overview

```
Step 1: Go to Gemba → Step 2: Observe Actual Behavior (with Measurement) → Step 3: Verify Claims → Step 4: Create Todo List for Fixing Discrepancies → Step 5: Fix at Source (with Measurement & Control)
```

## Step-by-Step Instructions

### Step 1: Go to Gemba (The Actual Place)

**Action**: Read actual source code, not documentation or comments.

**Gemba locations in ggen**:
- **Source code** (`crates/**/src/**/*.rs`) - The actual implementation
- **Test code** (`tests/**/*.rs`) - How code is actually used
- **Example code** (`examples/**/*.rs`) - Real usage patterns
- **Build output** (`cargo make check` output) - Actual compilation behavior
- **RDF files** (`*.ttl`, `*.rdf`) - Actual ontology data
- **Templates** (`templates/**/*.tmpl`) - Actual template files

**Avoid**:
- ❌ Documentation that may be outdated
- ❌ Comments that may be wrong
- ❌ Assumptions about how code works
- ❌ Second-hand information

**Action**: Navigate to actual source files

```bash
# Read actual source code
cat crates/ggen-core/src/rdf/processor.rs

# Read actual test code
cat tests/integration/rdf_tests.rs

# Read actual RDF data
cat ontologies/meta-ontology.ttl

# Read actual templates
cat templates/rust_struct.tmpl
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

# Run RDF processing to see actual output
cargo run -- process-rdf ontologies/meta-ontology.ttl
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
- Generated code (actual output, actual format)
- RDF processing results (actual triples, actual structure)

**Action**: Capture actual outputs

```bash
# Capture test output
cargo make test > test_output.txt 2>&1

# Capture compilation output
cargo make check > check_output.txt 2>&1

# Capture generated code
cargo run -- generate --template rust_struct.tmpl > generated.rs

# Examine actual outputs
cat test_output.txt
cat check_output.txt
cat generated.rs
```

#### 2.4: Collect Baseline Data (DMAIC Measurement)

**Action**: Measure current state to establish baseline for discrepancies.

**Data to collect**:
- **Discrepancy count**: How many discrepancies exist?
- **Discrepancy types**: What types of discrepancies (docs vs code, comments vs behavior, etc.)?
- **Discrepancy locations**: Where do discrepancies occur?
- **Impact**: What is the impact of each discrepancy?

**Action**: Collect baseline data

```bash
# Count discrepancies by type
grep -r "TODO.*discrepancy" . | wc -l
# Output: 15 discrepancies found

# Categorize discrepancies
# Documentation vs code: 5
# Comments vs behavior: 4
# Test names vs behavior: 3
# Assumptions vs reality: 3
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
- "This function processes RDF deterministically"
- "This test verifies template generation"
- "This code handles errors properly"

#### 3.2: Verify Against Gemba

**Action**: Check each claim against actual code behavior.

**Example verification**:
```rust
// Claim: "This function processes RDF deterministically"
// Gemba check:
fn process_rdf(input: &str) -> Result<Graph, ParseError> {
    let mut vars = HashMap::new(); // Actual: Uses HashMap (non-deterministic!)
    // ... rest of implementation
}

// Discrepancy: Claim says "deterministic" but uses HashMap (non-deterministic iteration)
// Actual: HashMap iteration order is non-deterministic
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

**Example todo list**:
```markdown
## Gemba Discrepancy Fix Todos (10+ items)

**Documentation vs Code Fixes**:
- [ ] Fix: `docs/API_REFERENCE.md` says `process_rdf` is deterministic, but code uses HashMap
  - File: `crates/ggen-core/src/rdf/processor.rs:45`
  - Actual behavior: Uses HashMap (non-deterministic iteration)
  - Decision: Code is wrong, fix code to use BTreeMap
  - Action: Replace HashMap with BTreeMap
  - Verify: Code now uses deterministic data structure

**Comments vs Behavior Fixes**:
- [ ] Fix: Comment says "handles all errors" but code panics on invalid RDF
  - File: `crates/ggen-core/src/rdf/parser.rs:123`
  - Actual behavior: `unwrap()` on invalid RDF, panics
  - Decision: Code is wrong, fix code to handle errors
  - Action: Replace `unwrap()` with proper error handling
  - Verify: Code handles errors correctly

**Test Names vs Behavior Fixes**:
- [ ] Fix: Test name says "test_deterministic_generation" but test doesn't verify determinism
  - File: `tests/integration/template_tests.rs:34`
  - Actual behavior: Test doesn't check for determinism
  - Decision: Test is incomplete, add determinism check
  - Action: Add determinism verification to test
  - Verify: Test now verifies determinism

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
// Code: Uses HashMap (non-deterministic)
// Claim: Documentation says deterministic
// Decision: Claim is correct (determinism is requirement), code is wrong
// Action: Fix code to use BTreeMap (deterministic)
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
# Edit crates/ggen-core/src/rdf/processor.rs

# Fix documentation if needed
# Edit docs/API_REFERENCE.md

# Fix tests if needed
# Edit tests/integration/rdf_tests.rs

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
- ✅ Determinism maintained: `cargo make check-determinism`

#### 5.4: Measure Improvement (DMAIC Measurement)

**Action**: Measure improvement against baseline data.

**Measurement**:
- Count remaining discrepancies
- Compare to baseline
- Calculate improvement percentage
- Verify success criteria met

**Action**: Measure improvement

```bash
# Re-count discrepancies after fixes
grep -r "TODO.*discrepancy" . | wc -l
# Output: 0 discrepancies (down from 15)

# Calculate improvement
# Baseline: 15 discrepancies
# After fixes: 0 discrepancies
# Improvement: 100% (15/15 fixed)
```

#### 5.5: Establish Controls (DMAIC Control)

**Action**: Set up controls to prevent discrepancies from returning.

**Controls**:
- **Code review**: Check for documentation/code mismatches
- **Automated checks**: Verify documentation matches code
- **Monitoring**: Track discrepancy rate over time
- **Standards**: Document pattern to prevent discrepancies

**Action**: Create todo list for controls (10+ items)

```markdown
## Gemba Control Todos (10+ items)

**Code Review Controls**:
- [ ] Add checklist item: Documentation matches code implementation
- [ ] Add checklist item: Comments match actual behavior
- [ ] Add checklist item: Test names match test behavior
- [ ] Update code review process to include discrepancy checks

**Automated Checks**:
- [ ] Add CI check: Verify documentation examples compile
- [ ] Add CI check: Verify test names match test behavior
- [ ] Add lint rule: Flag outdated comments
- [ ] Verify automated checks work correctly

**Monitoring Controls**:
- [ ] Set up discrepancy tracking dashboard
- [ ] Configure alerts if discrepancy rate > 0
- [ ] Review discrepancy trends weekly
- [ ] Document discrepancy patterns

**Standards Controls**:
- [ ] Add standard: Documentation must match code
- [ ] Add standard: Comments must match behavior
- [ ] Add standard: Test names must match test behavior
- [ ] Update team documentation with standards
- [ ] Verify standards are followed in code reviews
```

**Execution**:
1. Create todos using `todo_write` tool (10+ items minimum)
2. Execute todos one by one (implement controls)
3. Mark todos as completed as controls are implemented
4. Verify each control works before moving to next
5. Continue until all controls implemented

**Principle**: Implement controls to prevent discrepancies, don't just document them. Todos track progress, controls prevent recurrence.

---

## Integration with Other Commands

- **[Root Cause Analysis](./root-cause-analysis.md)** - Use 5 Whys to understand why discrepancies exist
- **[DMAIC Problem Solving](./dmaic-problem-solving.md)** - Use DMAIC measurement and control steps integrated into this workflow
- **[Eliminate Muda](./eliminate-muda.md)** - Remove waste from outdated documentation/comments
- **[Poka-Yoke Design](./poka-yoke-design.md)** - Use type system to prevent discrepancies

## Expert Insights

**Why this matters**: Working from assumptions or outdated information causes bugs. Experts always verify at the source (Gemba).

**Key principle**: "Go see, ask why, show respect" - Go to the actual code, understand why it works that way, respect the actual implementation.

**Remember**: The code is the source of truth. Documentation, comments, and tests should match the code, not the other way around.

**Genchi Genbutsu**: "Go and see for yourself" - Don't trust second-hand information. Verify at the source.

**DfLSS alignment**: Gemba walk (going to source) supports DfLSS (Design for Lean Six Sigma) by preventing both waste (outdated information causes rework) AND defects (wrong assumptions cause bugs). Don't conflate DfLSS with DFSS (Design for Six Sigma) - DFSS only addresses quality, missing critical waste elimination. See [Root Cause Analysis - DfLSS vs DFSS](./root-cause-analysis.md#dflss-vs-dfss-critical-distinction) for why conflating DfLSS with DFSS is a huge error.

---

## Command Execution Pattern

**CRITICAL**: Gemba walk commands must:
1. **Create 10+ item todo list** - Not documents/reports
2. **Execute todos** - Fix discrepancies at source, not just document them
3. **Verify fixes** - Test that fixes work
4. **Complete todos** - Mark todos as done as fixes complete

**Principle**: Fix discrepancies at source, don't document them. Todos track progress, fixes eliminate discrepancies.

---

End Command ---

