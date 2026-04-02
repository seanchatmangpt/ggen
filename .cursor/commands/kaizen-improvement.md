# Kaizen (Continuous Improvement) - Multi-Step Workflow

## Purpose

This command guides agents to make small, incremental improvements to ggen code rather than big rewrites. Kaizen means "change for better" - continuous small improvements that compound over time. Experts make many small improvements rather than waiting for perfect solutions.

## Workflow Overview

```
Step 1: Identify Opportunity → Step 2: Plan Change (with Success Criteria & Measurement) → Step 3: Do (Implement) → Step 4: Check (Verify with Measurement) → Step 5: Act (Standardize with Control)
```

## Step-by-Step Instructions

### Step 1: Identify Improvement Opportunity

**Action**: Find a small, focused improvement opportunity in ggen.

**Opportunity criteria**:
- **Small**: Can be done in minutes, not hours
- **Focused**: Addresses one specific thing
- **Safe**: Low risk of breaking things
- **Value**: Adds value (clarity, performance, maintainability)

**Types of opportunities for ggen**:
1. **Code clarity** - Make code more readable
2. **Performance** - Small performance improvement
3. **Maintainability** - Easier to maintain
4. **Error prevention** - Prevent a class of errors
5. **Consistency** - Match existing patterns

**Example opportunities**:
```markdown
## Kaizen Opportunities

### Code Clarity
- [ ] Extract magic number to named constant in RDF processor
- [ ] Add clarifying comment to template generator
- [ ] Rename variable for clarity

### Performance
- [ ] Use reference instead of clone in RDF processing
- [ ] Remove unnecessary allocation in template generation
- [ ] Optimize hot path

### Maintainability
- [ ] Extract repeated pattern to function
- [ ] Simplify complex expression
- [ ] Remove dead code
```

---

### Step 2: Plan Change

**Action**: Design minimal change that improves code.

#### 2.1: Define Improvement

**Action**: Clearly define what will improve.

**Example improvement statement**:
```markdown
## Improvement Plan

**What**: Extract magic number `1000` to named constant `MAX_TRIPLES_BATCH_SIZE`
**Why**: Makes code more readable, easier to change, self-documenting
**How**: 
1. Add constant: `const MAX_TRIPLES_BATCH_SIZE: usize = 1000;`
2. Replace `1000` with `MAX_TRIPLES_BATCH_SIZE`
**Risk**: Low - simple refactoring, no logic change
```

#### 2.2: Define Success Criteria (DMAIC Measurement)

**Action**: Define measurable success criteria.

**Example success criteria**:
```markdown
## Success Criteria

**Primary**:
- Code readability improved: Magic number replaced with named constant
- Maintainability improved: Constant can be changed in one place
- Self-documenting: Constant name explains what the value represents

**Measurable**:
- Magic number count: 1 → 0 (100% reduction)
- Named constants: 0 → 1 (added)

**Verification**:
- Code compiles: `cargo make check`
- Tests pass: `cargo make test`
```

#### 2.3: Collect Baseline Data (DMAIC Measurement)

**Action**: Measure current state before improvement.

**Action**: Collect baseline data

```bash
# Count magic numbers
grep -r "\b1000\b" crates/ggen-core/src/rdf/ | wc -l
# Output: 1 magic number found

# Check if constant exists
grep -r "MAX_TRIPLES_BATCH" crates/ggen-core/src/rdf/
# Output: No constant found
```

---

### Step 3: Do (Implement)

**Action**: Implement the improvement.

#### 3.1: Make Change

**Action**: Implement the planned change.

**Example implementation**:
```rust
// Before
fn process_rdf_batch(triples: &[Triple]) -> Result<Graph, ParseError> {
    if triples.len() > 1000 { // Magic number
        return Err(ParseError::BatchTooLarge);
    }
    // ...
}

// After (Kaizen improvement)
const MAX_TRIPLES_BATCH_SIZE: usize = 1000;

fn process_rdf_batch(triples: &[Triple]) -> Result<Graph, ParseError> {
    if triples.len() > MAX_TRIPLES_BATCH_SIZE {
        return Err(ParseError::BatchTooLarge);
    }
    // ...
}
```

#### 3.2: Verify Compilation

**Action**: Ensure code compiles.

```bash
cargo make check
```

**Expected**: Compiles successfully

---

### Step 4: Check (Verify)

**Action**: Verify improvement achieved its goal.

#### 4.1: Verify Functionality

**Action**: Ensure functionality preserved.

```bash
cargo make test
```

**Expected**: All tests pass

#### 4.2: Verify Improvement

**Action**: Check that improvement achieved its goal.

**Verification**:
- **Code clarity**: Is code more readable?
- **Performance**: Is performance improved? (if applicable)
- **Maintainability**: Is code easier to maintain?

#### 4.3: Measure Improvement (DMAIC Measurement)

**Action**: Measure improvement against baseline data.

**Action**: Measure improvement

```bash
# Re-count magic numbers after improvement
grep -r "\b1000\b" crates/ggen-core/src/rdf/ | wc -l
# Output: 0 magic numbers (down from 1)

# Check if constant exists
grep -r "MAX_TRIPLES_BATCH" crates/ggen-core/src/rdf/
# Output: const MAX_TRIPLES_BATCH_SIZE: usize = 1000; (constant added)

# Calculate improvement
# Baseline: 1 magic number, 0 constants
# After improvement: 0 magic numbers, 1 constant
# Improvement: 100% (1/1 magic number eliminated)
```

---

### Step 5: Act (Standardize)

**Action**: Standardize the improvement if successful.

#### 5.1: Apply Pattern Consistently

**Action**: Apply improvement pattern to similar code.

**Example**:
```rust
// Applied improvement pattern to similar code
const MAX_TRIPLES_BATCH_SIZE: usize = 1000;
const MAX_TEMPLATE_VARS: usize = 500; // Applied same pattern
const DEFAULT_CACHE_SIZE: usize = 100; // Applied same pattern
```

#### 5.2: Establish Controls (DMAIC Control)

**Action**: Set up controls to ensure improvement is sustained.

**Controls**:
- **Code review**: Check for magic numbers in reviews
- **Automated checks**: Lint rules to flag magic numbers
- **Monitoring**: Track magic number count over time
- **Standards**: Document pattern in coding standards

**Action**: Create todo list for controls (10+ items)

```markdown
## Kaizen Control Todos (10+ items)

**Code Review Controls**:
- [ ] Add checklist item: No magic numbers in new code
- [ ] Add checklist item: Use named constants for configuration values
- [ ] Update code review process to include checklist

**Automated Checks**:
- [ ] Add lint rule: Flag magic numbers
- [ ] Configure CI check: Fail if magic numbers found
- [ ] Verify automated checks work correctly

**Standards Controls**:
- [ ] Add standard: Use named constants instead of magic numbers
- [ ] Update team documentation with standard
- [ ] Verify standard is followed in code reviews
```

**Execution**:
1. Create todos using `todo_write` tool (10+ items minimum)
2. Execute todos one by one (implement controls)
3. Mark todos as completed as controls are implemented
4. Verify each control works before moving to next
5. Continue until all controls implemented

**Principle**: Implement controls to sustain improvement, don't just document them. Todos track progress, controls prevent regression.

---

## Integration with Other Commands

- **[Eliminate Muda](./eliminate-muda.md)** - Use Kaizen to eliminate waste incrementally
- **[Mura Elimination](./eliminate-mura.md)** - Use Kaizen to standardize patterns
- **[Poka-Yoke Design](./poka-yoke-design.md)** - Use Kaizen to add type safety incrementally
- **[DMAIC Problem Solving](./dmaic-problem-solving.md)** - Use DMAIC measurement and control steps integrated into this workflow

## Expert Insights

**Why this matters**: Big rewrites are risky and slow. Small improvements are safe and fast. Experts make many small improvements rather than waiting for perfect solutions.

**Key principle**: "Better is the enemy of good" - Don't wait for perfect. Make small improvements now.

**Remember**: Kaizen is continuous. Don't stop after one improvement. Keep looking for opportunities. Small improvements compound over time.

**PDCA cycle**: Plan-Do-Check-Act is the Kaizen cycle. Plan small change, do it, check results, act to standardize. Repeat continuously.

**DfLSS alignment**: Kaizen (continuous improvement) aligns with DfLSS (Design for Lean Six Sigma) principles - improvements should address both efficiency (waste elimination) AND quality (defect prevention). Don't conflate DfLSS with DFSS (Design for Six Sigma) - DFSS only addresses quality, missing critical waste elimination. When making improvements, consider both efficiency gains and quality improvements. See [Root Cause Analysis - DfLSS vs DFSS](./root-cause-analysis.md#dflss-vs-dfss-critical-distinction) for why conflating DfLSS with DFSS is a huge error.

---

## Command Execution Pattern

**CRITICAL**: Kaizen commands must:
1. **Create 10+ item todo list** - Not documents/reports
2. **Execute todos** - Implement improvements, not document them
3. **Verify improvements** - Test that improvements work
4. **Complete todos** - Mark todos as done as improvements complete

**Principle**: Execute improvements, don't document them. Todos track progress, improvements deliver value.

---

End Command ---
