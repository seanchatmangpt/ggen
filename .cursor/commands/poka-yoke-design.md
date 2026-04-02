# Poka-Yoke Design (Error Prevention) - Multi-Step Workflow

## Purpose

This command guides agents to design ggen code that prevents errors at compile time through type safety and invariants. Poka-yoke means "mistake-proofing" - making errors impossible through design. Experts use Rust's type system to prevent entire classes of errors in ggen.

## Workflow Overview

```
Step 1: Identify Error Modes → Step 2: Design Type-Level Prevention → Step 3: Add Compile-Time Checks → Step 4: Verify Prevention (with Measurement) → Step 5: Document Invariants (with Control)
```

## Step-by-Step Instructions

### Step 1: Identify Error Modes

**Action**: List all ways ggen code can fail at runtime.

**Error mode categories for ggen**:

1. **Invalid state** - States that shouldn't exist
   - Example: Non-deterministic template processing, invalid RDF graph state

2. **Invalid input** - Inputs that cause errors
   - Example: Empty RDF input, invalid template syntax, malformed ontology

3. **Invalid operations** - Operations that fail in certain states
   - Example: Processing RDF after graph closed, modifying immutable template

4. **Resource errors** - Resource-related failures
   - Example: File not found, out of memory, network error

5. **Logic errors** - Errors in program logic
   - Example: Index out of bounds, division by zero, overflow

**Example error mode inventory for ggen**:
```markdown
## Error Modes Inventory

### Invalid State
- [ ] Template processor can be in invalid state after error
- [ ] RDF graph can have invalid triples

### Invalid Input
- [ ] Empty RDF input passed to processor (should be non-empty)
- [ ] Invalid template syntax passed to generator

### Invalid Operations
- [ ] Processing RDF after graph finalized
- [ ] Modifying template after generation started
```

---

### Step 2: Design Type-Level Prevention

**Action**: Use Rust's type system to make errors impossible in ggen.

#### 2.1: Use Newtypes for Validation

**Action**: Create newtypes that enforce invariants.

**Example for ggen**:
```rust
// ❌ BAD: Can have invalid state
struct TemplateContext {
    variables: HashMap<String, Value>, // Non-deterministic iteration!
}

// ✅ GOOD: Type prevents invalid state
#[derive(Debug, Clone)]
struct TemplateContext {
    variables: BTreeMap<String, Value>, // Deterministic ordering enforced by type
}

impl TemplateContext {
    fn new() -> Self {
        Self {
            variables: BTreeMap::new(), // Cannot use HashMap - type prevents it
        }
    }
}
```

#### 2.2: Use Enums for State Machines

**Action**: Use enums to represent valid states only.

**Example for ggen**:
```rust
// ❌ BAD: Can be in invalid state
struct RdfProcessor {
    is_parsing: bool,
    is_processing: bool,
    // Can have both true - invalid state!
}

// ✅ GOOD: Enum prevents invalid states
enum RdfProcessorState {
    Initial,
    Parsing,
    Processing(Graph),
    Complete(Graph),
    Error(ParseError),
}

struct RdfProcessor {
    state: RdfProcessorState, // Only valid states possible
}
```

#### 2.3: Use PhantomData for Type-Level Invariants

**Action**: Use PhantomData to encode invariants in types.

**Example for ggen**:
```rust
use std::marker::PhantomData;

// Type-level invariant: Template<Validated> vs Template<Unvalidated>
struct Validated;
struct Unvalidated;

struct Template<State> {
    content: String,
    _state: PhantomData<State>,
}

impl Template<Unvalidated> {
    fn validate(self) -> Result<Template<Validated>, ValidationError> {
        // Validation logic
        Ok(Template {
            content: self.content,
            _state: PhantomData,
        })
    }
}

impl Template<Validated> {
    fn generate(&self, context: &TemplateContext) -> Result<String, GenerationError> {
        // Can only generate from validated template
    }
}

// Cannot generate from unvalidated template - compiler error!
```

---

### Step 3: Add Compile-Time Checks

**Action**: Leverage Rust's compiler to catch errors in ggen.

#### 3.1: Use Type Bounds

**Action**: Add trait bounds to restrict valid types.

**Example**:
```rust
// Function only accepts types that implement Deterministic
fn process_deterministic<T: Deterministic>(value: T) -> T {
    // Compiler error if type doesn't implement Deterministic
    value
}

trait Deterministic {
    fn is_deterministic(&self) -> bool;
}
```

#### 3.2: Use Const Generics for Sizes

**Action**: Use const generics to prevent size errors.

**Example**:
```rust
// Array size encoded in type - prevents index errors
fn process_triples<const N: usize>(triples: [Triple; N]) -> [Triple; N] {
    // N is known at compile time
    // Cannot index out of bounds - compiler knows size
    triples
}
```

---

### Step 4: Verify Prevention

**Action**: Ensure type system prevents errors in ggen.

#### 4.1: Attempt Invalid Operations

**Action**: Try to write code that should fail to compile.

**Example**:
```rust
// Try to create invalid state - should fail to compile
let context = TemplateContext::new();
context.variables.insert("key".to_string(), value); // Should be compile error if variables is private

// Try to use invalid state - should fail to compile
let unvalidated: Template<Unvalidated> = ...;
unvalidated.generate(&context); // Should be compile error - can't generate from unvalidated
```

**Verification**: Code that should be invalid doesn't compile

```bash
cargo make check
# Should show compile errors for invalid operations
```

#### 4.2: Verify Valid Operations Compile

**Action**: Ensure valid code compiles successfully.

**Verification**: Valid code compiles

```bash
cargo make check
# Should compile successfully
```

#### 4.3: Test Runtime Behavior

**Action**: Verify type-level prevention works at runtime.

```bash
cargo make test
# Tests should pass - type system prevents errors
```

#### 4.4: Measure Error Prevention (DMAIC Measurement)

**Action**: Measure error prevention effectiveness.

**Measurement**:
- Count errors prevented by type system
- Compare to baseline (errors that would occur without types)
- Calculate prevention percentage

**Example error prevention measurement**:
```markdown
## Error Prevention Measurement

**Baseline**: 5 potential runtime errors (without type prevention)
**After Type Prevention**: 0 runtime errors (caught at compile time)
**Prevention**: 100% (5/5 errors prevented)

**By Error Type**:
- Invalid state errors: 2 → 0 (100% prevented)
- Invalid input errors: 2 → 0 (100% prevented)
- Invalid operation errors: 1 → 0 (100% prevented)

**Success Criteria Met**: ✅
- All errors caught at compile time ✅
- No runtime errors ✅
- Type system prevents invalid states ✅
```

---

### Step 5: Document Invariants

**Action**: Explain why design prevents errors in ggen.

#### 5.1: Document Type Invariants

**Action**: Document invariants enforced by types.

**Example**:
```rust
/// Template context with deterministic variable ordering.
/// 
/// **Poka-yoke**: Uses `BTreeMap` instead of `HashMap` to prevent non-deterministic
/// iteration at compile time. The type system makes invalid states impossible.
#[derive(Debug, Clone)]
struct TemplateContext {
    variables: BTreeMap<String, Value>, // Invariant: Always deterministic (enforced by type)
}
```

#### 5.2: Establish Controls (DMAIC Control)

**Action**: Set up controls to ensure error prevention is sustained.

**Controls**:
- **Code review**: Check for type safety in reviews
- **Automated checks**: Lint rules to flag unsafe patterns
- **Monitoring**: Track error prevention effectiveness over time
- **Standards**: Document type safety patterns in coding standards

**Action**: Create todo list for controls (10+ items)

```markdown
## Poka-Yoke Control Todos (10+ items)

**Code Review Controls**:
- [ ] Add checklist item: Use type system to prevent errors
- [ ] Add checklist item: No runtime error handling for invalid states
- [ ] Update code review process to include type safety checks

**Automated Checks**:
- [ ] Add lint rule: Flag unsafe patterns
- [ ] Add lint rule: Flag missing type safety
- [ ] Configure CI check: Verify type safety

**Monitoring Controls**:
- [ ] Set up error prevention tracking dashboard
- [ ] Configure alerts if runtime errors increase
- [ ] Review error prevention trends weekly

**Standards Controls**:
- [ ] Add standard: Use type system to prevent errors
- [ ] Add standard: Make invalid states unrepresentable
- [ ] Update team documentation with standards
```

**Execution**:
1. Create todos using `todo_write` tool (10+ items minimum)
2. Execute todos one by one (implement controls)
3. Mark todos as completed as controls are implemented
4. Verify each control works before moving to next
5. Continue until all controls implemented

**Principle**: Implement controls to sustain error prevention, don't just document them. Todos track progress, controls prevent regression.

---

## Integration with Other Commands

- **[Gemba Walk](./gemba-walk.md)** - Verify actual type behavior matches design
- **[Root Cause Analysis](./root-cause-analysis.md)** - Understand why errors occur, then prevent with types
- **[DMAIC Problem Solving](./dmaic-problem-solving.md)** - Use DMAIC measurement and control steps integrated into this workflow
- **[Eliminate Muda](./eliminate-muda.md)** - Remove error-prone patterns, replace with type-safe designs

## Expert Insights

**Why this matters**: Runtime errors are expensive. Type-level prevention catches errors at compile time, before they reach production in ggen.

**Key principle**: "Make invalid states unrepresentable" - Use types to make errors impossible.

**Remember**: The type system is your friend. Use it to prevent entire classes of errors. If you can't represent an invalid state in your type system, you've prevented that error.

**Poka-yoke principle**: "Prevention is better than detection" - Prevent errors at compile time rather than catching them at runtime.

**DfLSS alignment**: Poka-yoke (defect prevention) is the Six Sigma component of DfLSS (Design for Lean Six Sigma). However, defect prevention alone is incomplete - DfLSS addresses both efficiency (waste elimination) AND quality (defect prevention). Don't conflate DfLSS with DFSS (Design for Six Sigma) - DFSS only addresses quality, missing critical waste elimination. When preventing defects with types, also consider eliminating waste (unnecessary complexity, redundant checks). See [Root Cause Analysis - DfLSS vs DFSS](./root-cause-analysis.md#dflss-vs-dfss-critical-distinction) for why conflating DfLSS with DFSS is a huge error.

---

## Command Execution Pattern

**CRITICAL**: Poka-yoke commands must:
1. **Create 10+ item todo list** - Not documents/reports
2. **Execute todos** - Implement type-level prevention, not document it
3. **Verify prevention** - Test that type system prevents errors
4. **Complete todos** - Mark todos as done as prevention completes

**Principle**: Implement type-level prevention, don't document it. Todos track progress, prevention delivers value.

---

End Command ---

