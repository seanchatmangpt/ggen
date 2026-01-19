# Poka-Yoke Design (Error Prevention) - Multi-Step Workflow

## Purpose

This command guides agents to design code that prevents errors at compile time through type safety and invariants. Poka-yoke means "mistake-proofing" - making errors impossible through design. Experts use the type system to prevent entire classes of errors.

## Workflow Overview

```
Step 1: Identify Error Modes → Step 2: Design Type-Level Prevention → Step 3: Add Compile-Time Checks → Step 4: Verify Prevention → Step 5: Document Invariants
```

## Step-by-Step Instructions

### Step 1: Identify Error Modes

**Action**: List all ways code can fail at runtime.

**Error mode categories**:

1. **Invalid state** - States that shouldn't exist
   - Example: Negative count, empty required field, invalid enum variant

2. **Invalid input** - Inputs that cause errors
   - Example: Empty string when non-empty required, null when non-null required

3. **Invalid operations** - Operations that fail in certain states
   - Example: Reading from closed file, modifying immutable data

4. **Resource errors** - Resource-related failures
   - Example: Out of memory, file not found, network error

5. **Logic errors** - Errors in program logic
   - Example: Division by zero, index out of bounds, overflow

**Action**: Create error mode inventory

```markdown
## Error Modes Inventory

### Invalid State
- [ ] Counter can be negative (should be >= 0)
- [ ] Parser can be in invalid state after error

### Invalid Input
- [ ] Empty string passed to `parse_number` (should be non-empty)
- [ ] Null/None passed where value required

### Invalid Operations
- [ ] Reading from closed file handle
- [ ] Modifying data after finalization

### Resource Errors
- [ ] File not found errors
- [ ] Network connection errors

### Logic Errors
- [ ] Division by zero
- [ ] Index out of bounds
- [ ] Integer overflow
```

---

### Step 2: Design Type-Level Prevention

**Action**: Use Rust's type system to make errors impossible.

#### 2.1: Use Newtypes for Validation

**Action**: Create newtypes that enforce invariants.

**Example**:
```rust
// ❌ BAD: Can have invalid state
struct Counter {
    value: i32, // Can be negative!
}

// ✅ GOOD: Type prevents invalid state
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Counter {
    value: u32, // Cannot be negative - type prevents it
}

impl Counter {
    fn new(value: u32) -> Self {
        Self { value }
    }
    
    fn increment(&mut self) {
        self.value = self.value.saturating_add(1); // Prevents overflow
    }
}
```

#### 2.2: Use Enums for State Machines

**Action**: Use enums to represent valid states only.

**Example**:
```rust
// ❌ BAD: Can be in invalid state
struct Parser {
    is_open: bool,
    is_closed: bool,
    // Can have both true - invalid state!
}

// ✅ GOOD: Enum prevents invalid states
enum ParserState {
    Initial,
    Parsing,
    Complete(Result<Value, ParseError>),
    Error(ParseError),
}

struct Parser {
    state: ParserState, // Only valid states possible
}
```

#### 2.3: Use Option/Result for Nullable Values

**Action**: Use `Option<T>` instead of nullable types.

**Example**:
```rust
// ❌ BAD: Can pass null, causes runtime error
fn process(value: &str) -> u32 {
    value.parse().unwrap() // Panics if empty!
}

// ✅ GOOD: Type forces handling of None
fn process(value: Option<&str>) -> Result<u32, ParseError> {
    match value {
        Some(s) if !s.is_empty() => s.parse().map_err(ParseError::from),
        Some(_) => Err(ParseError::EmptyInput),
        None => Err(ParseError::MissingInput),
    }
}
```

#### 2.4: Use PhantomData for Type-Level Invariants

**Action**: Use PhantomData to encode invariants in types.

**Example**:
```rust
use std::marker::PhantomData;

// Type-level invariant: FileHandle<Open> vs FileHandle<Closed>
struct Open;
struct Closed;

struct FileHandle<State> {
    file: std::fs::File,
    _state: PhantomData<State>,
}

impl FileHandle<Open> {
    fn read(&mut self) -> Result<Vec<u8>, std::io::Error> {
        // Can only read when Open
    }
    
    fn close(self) -> FileHandle<Closed> {
        // Consumes Open handle, returns Closed handle
        FileHandle {
            file: self.file,
            _state: PhantomData,
        }
    }
}

// Cannot read from Closed handle - compiler error!
```

---

### Step 3: Add Compile-Time Checks

**Action**: Leverage Rust's compiler to catch errors.

#### 3.1: Use Type Bounds

**Action**: Add trait bounds to restrict valid types.

**Example**:
```rust
// Function only accepts types that implement Display
fn print_value<T: std::fmt::Display>(value: T) {
    println!("{}", value);
}

// Compiler error if type doesn't implement Display
```

#### 3.2: Use Const Generics for Sizes

**Action**: Use const generics to prevent size errors.

**Example**:
```rust
// Array size encoded in type - prevents index errors
fn process_array<const N: usize>(arr: [u8; N]) -> [u8; N] {
    // N is known at compile time
    // Cannot index out of bounds - compiler knows size
    arr
}
```

#### 3.3: Use Lifetimes to Prevent Use-After-Free

**Action**: Use lifetimes to prevent memory errors.

**Example**:
```rust
// Lifetime ensures reference doesn't outlive data
fn process<'a>(data: &'a str) -> &'a str {
    // Returned reference tied to input lifetime
    // Compiler prevents use-after-free
    data
}
```

#### 3.4: Use Unsafe Blocks Sparingly

**Action**: Mark unsafe code explicitly, use safe abstractions.

**Example**:
```rust
// Safe abstraction over unsafe code
fn safe_operation(input: &[u8]) -> Result<u32, Error> {
    // Internal unsafe code is encapsulated
    // Public API is safe - compiler enforces safety
    unsafe {
        // Unsafe code here, but interface is safe
    }
}
```

---

### Step 4: Verify Prevention

**Action**: Ensure type system prevents errors.

#### 4.1: Attempt Invalid Operations

**Action**: Try to write code that should fail to compile.

**Example**:
```rust
// Try to create invalid state - should fail to compile
let counter = Counter::new(-1); // Should be compile error if Counter uses u32

// Try to use invalid state - should fail to compile
let closed_file: FileHandle<Closed> = ...;
closed_file.read(); // Should be compile error - can't read from closed file
```

**Verification**: Code that should be invalid doesn't compile

```bash
cargo make check
# Should show compile errors for invalid operations
```

#### 4.2: Verify Valid Operations Compile

**Action**: Ensure valid code compiles successfully.

**Example**:
```rust
// Valid operations should compile
let counter = Counter::new(0); // Valid
counter.increment(); // Valid

let open_file: FileHandle<Open> = ...;
open_file.read(); // Valid - can read from open file
```

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

---

### Step 5: Document Invariants

**Action**: Explain why design prevents errors.

#### 5.1: Document Type Invariants

**Action**: Document invariants enforced by types.

**Example**:
```rust
/// Counter that cannot be negative.
/// 
/// **Poka-yoke**: Uses `u32` instead of `i32` to prevent negative values
/// at compile time. The type system makes invalid states impossible.
#[derive(Debug, Clone, Copy)]
struct Counter {
    value: u32, // Invariant: Always >= 0 (enforced by type)
}
```

#### 5.2: Document State Machine Invariants

**Action**: Document valid state transitions.

**Example**:
```rust
/// Parser state machine.
/// 
/// **Poka-yoke**: Enum prevents invalid states. Cannot be both open and closed.
/// Valid transitions:
/// - Initial -> Parsing -> Complete
/// - Initial -> Parsing -> Error
/// - Cannot transition from Complete/Error back to Parsing (type prevents it)
enum ParserState {
    Initial,
    Parsing,
    Complete(Result<Value, ParseError>),
    Error(ParseError),
}
```

#### 5.3: Document Usage Patterns

**Action**: Document how to use types safely.

**Example**:
```rust
/// File handle that prevents use-after-close errors.
/// 
/// **Poka-yoke**: Type-level state prevents reading from closed files.
/// 
/// # Example
/// 
/// ```rust
/// let file = FileHandle::<Open>::open("file.txt")?;
/// let data = file.read()?; // Valid - file is open
/// let closed = file.close();
/// // closed.read() // Compile error - cannot read from closed file
/// ```
```

---

## Complete Workflow Example

```rust
// Step 1: Identify Error Modes
// Error: Counter can be negative
// Error: Can read from closed file

// Step 2: Design Type-Level Prevention
// Counter: Use u32 instead of i32
// FileHandle: Use enum for state (Open/Closed)

// Step 3: Add Compile-Time Checks
struct Counter {
    value: u32, // Prevents negative
}

enum FileState {
    Open(std::fs::File),
    Closed,
}

// Step 4: Verify Prevention
cargo make check
// Attempt invalid operations - should fail to compile
// let counter = Counter { value: -1 }; // Compile error!

// Step 5: Document Invariants
/// Counter that cannot be negative (Poka-yoke: u32 type prevents it)
```

## Integration with Other Commands

- **[Gemba Walk](./gemba-walk.md)** - Verify actual type behavior matches design
- **[Root Cause Analysis](./root-cause-analysis.md)** - Understand why errors occur, then prevent with types
- **[DMAIC Problem Solving](./dmaic-problem-solving.md)** - Use DMAIC to systematically add type-level prevention
- **[Eliminate Muda](./eliminate-muda.md)** - Remove error-prone patterns, replace with type-safe designs

## Expert Insights

**Why this matters**: Runtime errors are expensive. Type-level prevention catches errors at compile time, before they reach production.

**Key principle**: "Make invalid states unrepresentable" - Use types to make errors impossible.

**Remember**: The type system is your friend. Use it to prevent entire classes of errors. If you can't represent an invalid state in your type system, you've prevented that error.

**Poka-yoke principle**: "Prevention is better than detection" - Prevent errors at compile time rather than catching them at runtime.

**DfLSS alignment**: Poka-yoke (defect prevention) is the Six Sigma component of DfLSS (Design for Lean Six Sigma). However, defect prevention alone is incomplete - DfLSS addresses both efficiency (waste elimination) AND quality (defect prevention). Don't conflate DfLSS with DFSS (Design for Six Sigma) - DFSS only addresses quality, missing critical waste elimination. When preventing defects with types, also consider eliminating waste (unnecessary complexity, redundant checks). See [Root Cause Analysis - DfLSS vs DFSS](./root-cause-analysis.md#dflss-vs-dfss-critical-distinction) for why conflating DfLSS with DFSS is a huge error.

