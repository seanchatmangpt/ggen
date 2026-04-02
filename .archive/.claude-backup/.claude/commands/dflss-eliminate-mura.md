# Eliminate Mura (Unevenness) - Multi-Step Workflow

## Purpose

This command guides agents to eliminate unevenness (Mura) in code quality, patterns, and style. Mura refers to variability or inconsistency. Experts maintain consistent quality and patterns across the codebase.

## Workflow Overview

```
Step 1: Identify Mura → Step 2: Measure Variability → Step 3: Standardize → Step 4: Apply Consistently → Step 5: Control
```

## Step-by-Step Instructions

### Step 1: Identify Mura (Unevenness)

**Action**: Find inconsistencies in code quality, patterns, and style.

**Types of Mura to identify**:

1. **Code style inconsistency** - Different formatting, naming conventions
   - Example: Some functions use `snake_case`, others use `camelCase`
   - Example: Some use tabs, others use spaces

2. **Pattern inconsistency** - Same problem solved differently
   - Example: Error handling differs across modules
   - Example: Some use `Result`, others use `Option` for similar cases

3. **Quality inconsistency** - Different quality levels
   - Example: Some modules have tests, others don't
   - Example: Some code has error handling, others don't

4. **Complexity inconsistency** - Different complexity levels for similar problems
   - Example: Simple problem solved with complex solution in one place, simple solution in another

5. **Documentation inconsistency** - Different documentation levels
   - Example: Some functions documented, others not
   - Example: Different documentation styles

**Action**: Create Mura inventory

```markdown
## Mura Inventory

### Code Style Inconsistency
- [ ] `src/parser.rs` uses `snake_case` for functions
- [ ] `src/validator.rs` uses `camelCase` for functions
- [ ] Inconsistent naming convention

### Pattern Inconsistency
- [ ] `src/api.rs` uses `Result<T, ApiError>` for errors
- [ ] `src/db.rs` uses `Result<T, DbError>` for errors
- [ ] Different error types for similar operations

### Quality Inconsistency
- [ ] `src/core.rs` has 90% test coverage
- [ ] `src/utils.rs` has 20% test coverage
- [ ] Uneven test coverage

### Complexity Inconsistency
- [ ] `src/simple.rs` uses complex abstraction for simple problem
- [ ] `src/complex.rs` uses simple solution for complex problem
- [ ] Complexity doesn't match problem difficulty

### Documentation Inconsistency
- [ ] Public functions in `src/api.rs` are documented
- [ ] Public functions in `src/db.rs` are not documented
- [ ] Inconsistent documentation
```

---

### Step 2: Measure Variability

**Action**: Quantify the inconsistency.

**Metrics to measure**:

- **Style consistency** - How many style violations?
- **Pattern consistency** - How many different patterns for same problem?
- **Quality consistency** - What's the quality variance?
- **Complexity consistency** - What's the complexity variance?

**Action**: Measure variability

```bash
# Measure style consistency
cargo make fmt --check
# Count violations

# Measure test coverage consistency
cargo make test -- --test-threads 1 --nocapture 2>&1 | grep "test result"
# Compare coverage across modules

# Measure pattern consistency
# Manual review: Count different patterns for same problem
```

**Example measurement**:
```markdown
## Variability Measurement

### Style Consistency
- Formatting violations: 15 files
- Naming convention violations: 8 functions
- Inconsistency score: High

### Pattern Consistency
- Error handling patterns: 3 different patterns
- Validation patterns: 2 different patterns
- Inconsistency score: Medium

### Quality Consistency
- Test coverage range: 20% - 90%
- Coverage variance: 70%
- Inconsistency score: High

### Complexity Consistency
- Cyclomatic complexity range: 2 - 15
- Complexity variance: High
- Inconsistency score: Medium
```

---

### Step 3: Standardize

**Action**: Establish consistent standards.

#### 3.1: Define Standards

**Action**: Define what the standard should be.

**Standard definition**:
- **Style standards** - Formatting, naming conventions
- **Pattern standards** - How to solve common problems
- **Quality standards** - Minimum quality levels
- **Documentation standards** - Documentation requirements

**Example standards**:
```markdown
## Standards Definition

### Style Standards
- **Naming**: Use `snake_case` for functions (Rust convention)
- **Formatting**: Use `cargo make fmt` (enforced)
- **Imports**: Alphabetical, grouped by std/external/local

### Pattern Standards
- **Error handling**: Use `Result<T, E>` with project error types
- **Validation**: Use type-level validation (Poka-yoke) when possible
- **Testing**: Minimum 80% test coverage for all modules

### Quality Standards
- **Test coverage**: Minimum 80% for all modules
- **Error handling**: All fallible operations return `Result`
- **Documentation**: All public APIs documented

### Documentation Standards
- **Public functions**: Must have doc comments
- **Complex logic**: Must have inline comments explaining why
- **Examples**: Public APIs should have usage examples
```

#### 3.2: Choose Reference Implementation

**Action**: Select best example as reference.

**Reference selection**:
- **Best example** - Highest quality, most consistent
- **Most common** - Most frequently used pattern
- **Most maintainable** - Easiest to maintain

**Example**:
```markdown
## Reference Implementation

**Error handling**: Use `src/api.rs` as reference
- Uses `Result<T, ApiError>` consistently
- Proper error propagation
- Good error messages

**Test patterns**: Use `tests/api_test.rs` as reference
- Comprehensive test coverage
- Good test organization
- Clear test names
```

---

### Step 4: Apply Consistently

**Action**: Apply standards across codebase.

#### 4.1: Apply Style Standards

**Action**: Standardize code style.

**Steps**:
1. Run formatter: `cargo make fmt`
2. Fix naming violations
3. Fix import ordering
4. Verify: `cargo make check`

**Example**:
```bash
# Apply formatting standards
cargo make fmt

# Verify style consistency
cargo make check
```

#### 4.2: Apply Pattern Standards

**Action**: Standardize patterns.

**Steps**:
1. Identify inconsistent patterns
2. Refactor to match standard pattern
3. Verify functionality: `cargo make test`
4. Verify no regressions

**Example**:
```rust
// Before: Inconsistent error handling
fn parse(input: &str) -> Option<Value> {
    // Uses Option, inconsistent with other functions
}

// After: Standardized error handling
fn parse(input: &str) -> Result<Value, ParseError> {
    // Uses Result, consistent with other functions
}
```

#### 4.3: Apply Quality Standards

**Action**: Bring all code to minimum quality level.

**Steps**:
1. Identify low-quality code
2. Add tests to reach coverage threshold
3. Add error handling where missing
4. Add documentation where missing
5. Verify: `cargo make test`

**Example**:
```rust
// Before: Low quality (no tests, no error handling)
fn process(data: &str) -> String {
    data.to_uppercase() // No error handling
}

// After: Standardized quality (tests, error handling)
fn process(data: &str) -> Result<String, ProcessingError> {
    if data.is_empty() {
        return Err(ProcessingError::EmptyInput);
    }
    Ok(data.to_uppercase())
}

#[cfg(test)]
mod tests {
    use super::*;
    use chicago_tdd_tools::prelude::*;
    
    test!(test_process_valid_input, {
        let result = process("hello");
        assert_ok!(&result);
        assert_eq!(result.unwrap(), "HELLO");
    });
    
    test!(test_process_empty_input, {
        let result = process("");
        assert_err!(&result);
    });
}
```

#### 4.4: Apply Documentation Standards

**Action**: Standardize documentation.

**Steps**:
1. Identify undocumented public APIs
2. Add documentation following standard
3. Add examples where required
4. Verify: `cargo make docs`

---

### Step 5: Control (Prevent Inconsistency)

**Action**: Establish controls to prevent Mura from returning.

#### 5.1: Automated Checks

**Action**: Use automated tools to enforce standards.

**Automated checks**:
- **Formatting**: `cargo make fmt` in CI
- **Linting**: `cargo make lint` in CI
- **Tests**: `cargo make test` in CI
- **Coverage**: Coverage checks in CI

**Example**:
```bash
# CI checks
cargo make fmt --check  # Fail if not formatted
cargo make lint         # Fail if linting errors
cargo make test         # Fail if tests fail
# Coverage check       # Fail if coverage < 80%
```

#### 5.2: Code Review Checklist

**Action**: Add standards to code review checklist.

**Checklist items**:
- [ ] Code follows style standards
- [ ] Code uses standard patterns
- [ ] Code meets quality standards
- [ ] Code has required documentation

#### 5.3: Documentation

**Action**: Document standards for reference.

**Documentation**:
- Style guide
- Pattern guide
- Quality standards
- Examples

**Example**:
```markdown
## Coding Standards

### Style
- Use `snake_case` for functions
- Run `cargo make fmt` before committing

### Patterns
- Use `Result<T, E>` for error handling
- See `src/api.rs` for reference implementation

### Quality
- Minimum 80% test coverage
- All public APIs documented
```

#### 5.4: Regular Audits

**Action**: Periodically audit for consistency.

**Audit frequency**: Weekly or monthly

**Audit process**:
1. Run consistency checks
2. Identify new inconsistencies
3. Apply standards
4. Update controls if needed

---

## Complete Workflow Example

```bash
# Step 1: Identify Mura
# Found: Inconsistent error handling patterns

# Step 2: Measure Variability
# 3 different error handling patterns found

# Step 3: Standardize
# Standard: Use Result<T, E> with project error types
# Reference: src/api.rs

# Step 4: Apply Consistently
# Refactor all modules to use standard pattern
cargo make test  # Verify functionality

# Step 5: Control
# Add to CI: cargo make lint
# Add to code review checklist
# Document standards
```

## Integration with Other Commands

- **[Kaizen Improvement](./kaizen-improvement.md)** - Use Kaizen to standardize incrementally
- **[Eliminate Muda](./eliminate-muda.md)** - Remove waste while standardizing
- **[Gemba Walk](./gemba-walk.md)** - Go to source to verify standards applied
- **[Andon Signals](./andon-signals.md)** - Use linting as signals of inconsistency

## Expert Insights

**Why this matters**: Inconsistency increases cognitive load and maintenance cost. Consistent code is easier to understand and maintain.

**Key principle**: "Consistency is more important than perfection" - It's better to have consistent, good code than perfect code in some places and poor code in others.

**Remember**: Standardization is continuous. New code should follow standards. Existing code should be gradually standardized. Use Kaizen to standardize incrementally.

**Mura elimination**: Focus on the most impactful inconsistencies first. Don't try to fix everything at once. Use Kaizen to standardize incrementally.

**DfLSS alignment**: Eliminating Mura (inconsistency) supports DfLSS (Design for Lean Six Sigma) by ensuring both efficiency (consistent patterns reduce waste) AND quality (consistent quality standards prevent defects). Don't conflate DfLSS with DFSS (Design for Six Sigma) - DFSS only addresses quality, missing critical waste elimination. When standardizing, ensure consistency supports both efficiency and quality. See [Root Cause Analysis - DfLSS vs DFSS](./root-cause-analysis.md#dflss-vs-dfss-critical-distinction) for why conflating DfLSS with DFSS is a huge error.

