# Eliminate Muda (Waste) - Multi-Step Workflow

## Purpose

This command guides agents through identifying and eliminating waste (Muda) in code. Muda refers to any activity that consumes resources without adding value. Experts recognize waste early and eliminate it systematically.

## Workflow Overview

```
Step 1: Identify Muda → Step 2: Measure Waste Impact → Step 3: Eliminate Waste → Step 4: Verify Value Stream → Step 5: Control
```

## Step-by-Step Instructions

### Step 1: Identify Muda (Waste)

**Action**: Scan code for the seven types of waste.

**Types of Muda to identify**:

1. **Over-processing** - Code that does more than necessary
   - Unnecessary abstractions
   - Premature optimization
   - Features not used

2. **Waiting** - Code blocked by dependencies or delays
   - Synchronous operations that could be async
   - Blocking I/O in hot paths
   - Unnecessary serialization

3. **Transportation** - Moving data unnecessarily
   - Unnecessary copies/clones
   - Passing data through multiple layers
   - Redundant data transformations

4. **Inventory** - Code that accumulates without value
   - Dead code
   - Unused dependencies
   - Commented-out code

5. **Motion** - Unnecessary code movement
   - Repeated patterns that could be abstracted
   - Copy-paste code
   - Duplicate logic

6. **Defects** - Code that causes rework
   - Error-prone patterns
   - Missing error handling
   - Incomplete implementations

7. **Over-production** - Code written before needed
   - YAGNI violations
   - Premature abstractions
   - Unused features

**Action**: Create waste inventory list

```markdown
## Muda Inventory

### Over-processing
- [ ] `src/parser.rs:45` - Unnecessary abstraction layer

### Waiting
- [ ] `src/api.rs:123` - Blocking I/O in request handler

### Transportation
- [ ] `src/transform.rs:67` - Unnecessary clone() calls

### Inventory
- [ ] `src/old.rs` - Dead code file

### Motion
- [ ] `src/utils.rs:12` - Duplicate validation logic (also in `src/validator.rs:34`)

### Defects
- [ ] `src/error.rs:89` - Missing error handling

### Over-production
- [ ] `src/future.rs` - Unused feature module
```

---

### Step 2: Measure Waste Impact

**Action**: Quantify the impact of each waste item.

**Metrics to measure**:

- **Lines of code** - How much code is waste?
- **Complexity** - Cyclomatic complexity of waste code
- **Dependencies** - How many dependencies are unused?
- **Performance impact** - Does waste affect performance?
- **Maintenance cost** - How much time spent maintaining waste?

**Action**: Prioritize waste elimination

**Priority order**:
1. **High impact, low effort** - Quick wins
2. **High impact, high effort** - Strategic improvements
3. **Low impact, low effort** - Cleanup tasks
4. **Low impact, high effort** - Defer or eliminate

**Example prioritization**:
```markdown
## Waste Prioritization

### High Impact, Low Effort (Do First)
- Remove dead code (`src/old.rs`) - 200 lines, 0 dependencies
- Remove unnecessary clones - 5 instances, improves performance

### High Impact, High Effort (Plan)
- Refactor duplicate validation logic - Reduces maintenance

### Low Impact, Low Effort (Cleanup)
- Remove unused imports - Reduces noise

### Low Impact, High Effort (Defer)
- Refactor premature abstraction - Works for now
```

---

### Step 3: Eliminate Waste

**Action**: Systematically remove waste, starting with high-priority items.

#### 3.1: Remove Dead Code (Inventory Muda)

**Action**: Remove unused code, files, dependencies.

**Steps**:
1. Identify unused code (dead code analysis)
2. Verify code is truly unused (grep, usage analysis)
3. Remove unused code
4. Remove unused dependencies from `Cargo.toml`
5. Verify removal doesn't break anything: `cargo make check`

**Verification**: `cargo make check` passes, no unused warnings

#### 3.2: Eliminate Duplication (Motion Muda)

**Action**: Extract common patterns, remove copy-paste code.

**Steps**:
1. Identify duplicate code patterns
2. Extract common functionality
3. Replace duplicates with shared code
4. Verify functionality preserved: `cargo make test`
5. Verify no regressions: `cargo make test`

**Verification**: Tests pass, code duplication reduced

#### 3.3: Remove Unnecessary Abstractions (Over-processing Muda)

**Action**: Simplify code by removing premature abstractions.

**Steps**:
1. Identify abstraction that adds no value
2. Inline abstraction if simple
3. Simplify abstraction if needed
4. Verify functionality: `cargo make test`
5. Verify performance not degraded: `cargo make test`

**Verification**: Tests pass, code simpler

#### 3.4: Optimize Data Flow (Transportation Muda)

**Action**: Eliminate unnecessary data movement.

**Steps**:
1. Identify unnecessary clones/copies
2. Use references instead of owned values
3. Eliminate redundant transformations
4. Verify functionality: `cargo make test`
5. Verify performance improved: `cargo make test`

**Verification**: Tests pass, fewer allocations

#### 3.5: Fix Error-Prone Patterns (Defects Muda)

**Action**: Fix patterns that cause rework.

**Steps**:
1. Identify error-prone patterns
2. Add proper error handling
3. Use type system to prevent errors (see [Poka-Yoke Design](./poka-yoke-design.md))
4. Verify errors handled: `cargo make test`
5. Verify no regressions: `cargo make test`

**Verification**: Tests pass, error handling improved

---

### Step 4: Verify Value Stream

**Action**: Ensure waste elimination maintained functionality and improved value.

**Verification checklist**:
- ✅ All tests pass: `cargo make test`
- ✅ Code compiles: `cargo make check`
- ✅ Functionality preserved (tests verify behavior)
- ✅ Performance maintained or improved
- ✅ Code complexity reduced (fewer lines, simpler logic)
- ✅ Dependencies reduced (fewer unused deps)

**If verification fails**:
- Revert changes that broke functionality
- Re-analyze waste (may have removed necessary code)
- Return to Step 1, refine waste identification

**If verification succeeds**:
- Proceed to Step 5 (Control)

---

### Step 5: Control (Prevent Waste from Returning)

**CRITICAL**: Do NOT write documents or reports. Create todos and execute controls.

**Action**: Create 10+ item todo list for implementing controls to prevent waste.

**Control methods** (implement as todos):
- **Tests** - Add tests for refactored code
- **Code review** - Add checklist items to prevent waste
- **Inline comments** - Add comments explaining waste removal (in code, not separate doc)
- **Standards** - Implement waste prevention patterns

**Example control todo list**:
```markdown
## Waste Prevention Control Todos (10+ items)

**Test Controls**:
- [ ] Add tests for refactored code to prevent regression
- [ ] Verify tests catch waste patterns
- [ ] Add tests to CI pipeline

**Code Review Controls**:
- [ ] Add checklist item: No dead code
- [ ] Add checklist item: No unnecessary clones
- [ ] Add checklist item: No unwrap() in production code
- [ ] Update code review process to include checklist

**Inline Documentation Controls**:
- [ ] Add inline comment: Document why waste was removed
- [ ] Add inline comment: Document pattern to avoid
- [ ] Verify comments are clear and helpful

**Standards Controls**:
- [ ] Add standard: Use references over clones when possible
- [ ] Add standard: Extract common logic, don't duplicate
- [ ] Add standard: Remove unused code immediately
- [ ] Add standard: Avoid premature abstractions (YAGNI)

**Monitoring Controls**:
- [ ] Set up unused code detection in CI
- [ ] Configure alerts for waste accumulation
- [ ] Review waste regularly during code review

**Verification**:
- [ ] Verify controls work: Tests catch waste patterns
- [ ] Verify no regressions: All tests still pass
- [ ] Verify controls are sustainable: Process is repeatable
```

**Execution**:
1. Create todos using `todo_write` tool (10+ items minimum)
2. Execute todos one by one (implement controls)
3. Mark todos as completed as controls are implemented
4. Verify each control works before moving to next
5. Continue until all controls implemented

**Principle**: Implement controls, don't document them separately. Todos track progress, controls prevent waste accumulation.

---

## Complete Workflow Example

```bash
# Step 1: Identify Muda
# Found: Dead code in src/old.rs, duplicate validation logic

# Step 2: Measure Impact
# Dead code: 200 lines, 0 dependencies - High impact, low effort
# Duplicate logic: 50 lines duplicated - Medium impact, medium effort

# Step 3: Eliminate Waste
# Remove src/old.rs
cargo make check  # Verify removal

# Extract common validation
# Refactor duplicate logic
cargo make test   # Verify functionality

# Step 4: Verify Value Stream
cargo make test   # All tests pass ✅
cargo make check  # Compiles ✅
# Code reduced by 200 lines ✅

# Step 5: Control
# Add tests for refactored validation
# Document removal decision
# Establish pattern: Extract common logic, don't duplicate
```

## Integration with Other Commands

- **[Gemba Walk](./gemba-walk.md)** - Go to source to verify waste identification
- **[Poka-Yoke Design](./poka-yoke-design.md)** - Prevent defects Muda through type safety
- **[Kaizen Improvement](./kaizen-improvement.md)** - Make small improvements to eliminate waste
- **[Mura Elimination](./eliminate-mura.md)** - Standardize patterns to prevent waste

## Expert Insights

**Why this matters**: Waste accumulates silently. Experts recognize waste early and eliminate it systematically. Most developers don't see waste until it's overwhelming.

**Key principle**: "If it doesn't add value, it's waste. Eliminate it."

**Remember**: Waste elimination is continuous (Kaizen), not one-time. Regular waste elimination prevents technical debt accumulation.

**DfLSS alignment**: Eliminating Muda (waste) is the Lean component of DfLSS (Design for Lean Six Sigma). However, waste elimination alone is incomplete - DfLSS addresses both efficiency (waste elimination) AND quality (defect prevention). Don't conflate DfLSS with DFSS (Design for Six Sigma) - DFSS only addresses quality, missing critical waste elimination. When eliminating waste, also ensure quality is maintained and defects are prevented. See [Root Cause Analysis - DfLSS vs DFSS](./root-cause-analysis.md#dflss-vs-dfss-critical-distinction) for why conflating DfLSS with DFSS is a huge error.

---

## Command Execution Pattern

**CRITICAL**: Eliminate Muda commands must:
1. **Create 10+ item todo list** - Not documents/reports
2. **Execute todos** - Remove waste, not document it
3. **Verify fixes** - Test that waste elimination works
4. **Complete todos** - Mark todos as done as waste is eliminated

**Principle**: Eliminate waste, don't document it. Todos track progress, waste elimination improves efficiency.

---

