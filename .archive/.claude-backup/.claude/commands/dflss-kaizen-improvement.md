# Kaizen (Continuous Improvement) - Multi-Step Workflow

## Purpose

This command guides agents to make small, incremental improvements rather than big rewrites. Kaizen means "change for better" - continuous small improvements that compound over time. Experts make many small improvements rather than waiting for perfect solutions.

## Workflow Overview

```
Step 1: Identify Opportunity → Step 2: Plan Change → Step 3: Do (Implement) → Step 4: Check (Verify) → Step 5: Act (Standardize)
```

## Step-by-Step Instructions

### Step 1: Identify Improvement Opportunity

**Action**: Find a small, focused improvement opportunity.

**Opportunity criteria**:
- **Small**: Can be done in minutes, not hours
- **Focused**: Addresses one specific thing
- **Safe**: Low risk of breaking things
- **Value**: Adds value (clarity, performance, maintainability)

**Types of opportunities**:
1. **Code clarity** - Make code more readable
2. **Performance** - Small performance improvement
3. **Maintainability** - Easier to maintain
4. **Error prevention** - Prevent a class of errors
5. **Consistency** - Match existing patterns

**Action**: List improvement opportunities

```markdown
## Kaizen Opportunities

### Code Clarity
- [ ] Extract magic number to named constant
- [ ] Add clarifying comment
- [ ] Rename variable for clarity

### Performance
- [ ] Use reference instead of clone
- [ ] Remove unnecessary allocation
- [ ] Optimize hot path

### Maintainability
- [ ] Extract repeated pattern to function
- [ ] Simplify complex expression
- [ ] Remove dead code

### Error Prevention
- [ ] Add type safety (see [Poka-Yoke Design](./poka-yoke-design.md))
- [ ] Add validation
- [ ] Handle edge case

### Consistency
- [ ] Match naming convention
- [ ] Match code style
- [ ] Match error handling pattern
```

**Principle**: "Small improvements, continuously" - Don't wait for perfect. Make small improvements now.

---

### Step 2: Plan Change

**Action**: Design minimal change that improves code.

#### 2.1: Define Improvement

**Action**: Clearly define what will improve.

**Improvement statement**:
- **What**: What will change?
- **Why**: Why is this improvement valuable?
- **How**: How will it be implemented?
- **Risk**: What could go wrong?

**Example improvement statement**:
```markdown
## Improvement Plan

**What**: Extract magic number `42` to named constant `DEFAULT_TIMEOUT_SECONDS`
**Why**: Makes code more readable, easier to change, self-documenting
**How**: 
1. Add constant: `const DEFAULT_TIMEOUT_SECONDS: u64 = 42;`
2. Replace `42` with `DEFAULT_TIMEOUT_SECONDS`
**Risk**: Low - simple refactoring, no logic change
```

#### 2.2: Verify Safety

**Action**: Ensure change is safe.

**Safety checks**:
- ✅ No logic changes (if refactoring)
- ✅ Tests exist for affected code
- ✅ Change is isolated (doesn't affect other code)
- ✅ Can be easily reverted if needed

**Action**: Verify safety

```bash
# Check tests exist
cargo make test

# Verify current behavior
cargo make check
```

---

### Step 3: Do (Implement)

**Action**: Implement the improvement.

#### 3.1: Make Change

**Action**: Implement the planned change.

**Change principles**:
- **Minimal**: Change only what's necessary
- **Focused**: One improvement at a time
- **Clean**: Follow existing patterns

**Example implementation**:
```rust
// Before
fn connect() -> Result<Connection, Error> {
    tokio::time::timeout(Duration::from_secs(42), async_connect()).await?
}

// After (Kaizen improvement)
const DEFAULT_TIMEOUT_SECONDS: u64 = 42;

fn connect() -> Result<Connection, Error> {
    tokio::time::timeout(
        Duration::from_secs(DEFAULT_TIMEOUT_SECONDS),
        async_connect()
    ).await?
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
- **Error prevention**: Are errors prevented? (if applicable)
- **Consistency**: Does code match patterns? (if applicable)

**Example verification**:
```rust
// Improvement: Extract magic number to constant
// Verification:
// ✅ Code more readable: `DEFAULT_TIMEOUT_SECONDS` is clearer than `42`
// ✅ Easier to change: Change constant instead of searching for `42`
// ✅ Self-documenting: Name explains what the number means
// ✅ Functionality preserved: Tests pass
```

#### 4.3: Check for Regressions

**Action**: Ensure no regressions introduced.

**Checks**:
- ✅ All tests pass
- ✅ No performance degradation (if applicable)
- ✅ No new warnings
- ✅ Code still compiles

**If regressions found**:
- Revert change
- Re-plan improvement
- Return to Step 2

**If no regressions**:
- Proceed to Step 5

---

### Step 5: Act (Standardize)

**Action**: Standardize the improvement if successful.

#### 5.1: Apply Pattern Consistently

**Action**: Apply improvement pattern to similar code.

**Pattern application**:
- Find similar code that could benefit
- Apply same improvement
- Verify each application

**Example**:
```rust
// Applied improvement pattern to similar code
const DEFAULT_TIMEOUT_SECONDS: u64 = 42;
const MAX_RETRY_ATTEMPTS: u64 = 3; // Applied same pattern
const CONNECTION_POOL_SIZE: usize = 10; // Applied same pattern
```

#### 5.2: Document Pattern

**Action**: Document improvement pattern for future use.

**Documentation**:
- What pattern was applied
- Why it's beneficial
- When to apply it
- How to apply it

**Example documentation**:
```rust
/// Connection timeout configuration.
/// 
/// **Kaizen improvement**: Extracted magic number to named constant.
/// Pattern: Use named constants instead of magic numbers for:
/// - Configuration values
/// - Repeated literals
/// - Values that may change
const DEFAULT_TIMEOUT_SECONDS: u64 = 42;
```

#### 5.3: Establish Standard

**Action**: Make improvement part of coding standards.

**Standard establishment**:
- Add to code review checklist
- Add to coding standards
- Share with team

**Example standard**:
```markdown
## Coding Standard: Named Constants

**Rule**: Use named constants instead of magic numbers
**Rationale**: Improves readability, maintainability, self-documentation
**Example**: `const DEFAULT_TIMEOUT_SECONDS: u64 = 42;` instead of `42`
**When**: For configuration values, repeated literals, values that may change
```

---

## Complete Workflow Example

```rust
// Step 1: Identify Opportunity
// Opportunity: Extract magic number `42` to named constant

// Step 2: Plan Change
// Plan: Add constant, replace `42` with constant name
// Risk: Low - simple refactoring

// Step 3: Do (Implement)
const DEFAULT_TIMEOUT_SECONDS: u64 = 42;
// Replace `42` with `DEFAULT_TIMEOUT_SECONDS`

// Step 4: Check (Verify)
cargo make check  # Compiles ✅
cargo make test   # Tests pass ✅
// Improvement verified: Code more readable ✅

// Step 5: Act (Standardize)
// Apply pattern to other magic numbers
// Document pattern
// Establish standard
```

## Kaizen Mindset

**Principles**:
1. **Small improvements** - Don't wait for perfect, improve now
2. **Continuous** - Make improvements regularly, not just once
3. **Everyone** - Anyone can suggest improvements
4. **No blame** - Focus on improvement, not blame
5. **Data-driven** - Use data to identify opportunities

**Benefits**:
- **Low risk** - Small changes are safer than big rewrites
- **Fast feedback** - See results quickly
- **Compound effect** - Small improvements add up over time
- **Sustainable** - Easier to maintain than big changes

## Integration with Other Commands

- **[Eliminate Muda](./eliminate-muda.md)** - Use Kaizen to eliminate waste incrementally
- **[Mura Elimination](./eliminate-mura.md)** - Use Kaizen to standardize patterns
- **[Poka-Yoke Design](./poka-yoke-design.md)** - Use Kaizen to add type safety incrementally
- **[DMAIC Problem Solving](./dmaic-problem-solving.md)** - Use Kaizen for small improvements within DMAIC

## Expert Insights

**Why this matters**: Big rewrites are risky and slow. Small improvements are safe and fast. Experts make many small improvements rather than waiting for perfect solutions.

**Key principle**: "Better is the enemy of good" - Don't wait for perfect. Make small improvements now.

**Remember**: Kaizen is continuous. Don't stop after one improvement. Keep looking for opportunities. Small improvements compound over time.

**PDCA cycle**: Plan-Do-Check-Act is the Kaizen cycle. Plan small change, do it, check results, act to standardize. Repeat continuously.

**DfLSS alignment**: Kaizen (continuous improvement) aligns with DfLSS (Design for Lean Six Sigma) principles - improvements should address both efficiency (waste elimination) AND quality (defect prevention). Don't conflate DfLSS with DFSS (Design for Six Sigma) - DFSS only addresses quality, missing critical waste elimination. When making improvements, consider both efficiency gains and quality improvements. See [Root Cause Analysis - DfLSS vs DFSS](./root-cause-analysis.md#dflss-vs-dfss-critical-distinction) for why conflating DfLSS with DFSS is a huge error.

