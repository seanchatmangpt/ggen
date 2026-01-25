<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [How-to: Fix Compilation Errors](#how-to-fix-compilation-errors)
  - [Prerequisites](#prerequisites)
  - [Problem Statement](#problem-statement)
  - [Quick Reference](#quick-reference)
  - [E0277: Trait Bound Not Satisfied](#e0277-trait-bound-not-satisfied)
    - [Scenario](#scenario)
    - [Error Message](#error-message)
    - [Solution](#solution)
    - [Troubleshooting](#troubleshooting)
  - [E0308: Mismatched Types](#e0308-mismatched-types)
    - [Scenario](#scenario-1)
    - [Error Message](#error-message-1)
    - [Solution Patterns](#solution-patterns)
      - [Pattern 1: Convert &str → String](#pattern-1-convert-str-%E2%86%92-string)
      - [Pattern 2: Change Function Signature](#pattern-2-change-function-signature)
      - [Pattern 3: Use AsRef Trait](#pattern-3-use-asref-trait)
    - [When to Use Each Pattern](#when-to-use-each-pattern)
  - [E0283: Type Annotations Needed](#e0283-type-annotations-needed)
    - [Scenario](#scenario-2)
    - [Error Message](#error-message-2)
    - [Solution Patterns](#solution-patterns-1)
      - [Pattern 1: Turbofish Syntax](#pattern-1-turbofish-syntax)
      - [Pattern 2: Type Annotation on Variable](#pattern-2-type-annotation-on-variable)
      - [Pattern 3: Specify in Context](#pattern-3-specify-in-context)
  - [E0599: Method Not Found](#e0599-method-not-found)
    - [Scenario](#scenario-3)
    - [Error Message](#error-message-3)
    - [Solution Patterns](#solution-patterns-2)
      - [Pattern 1: Import Missing Trait](#pattern-1-import-missing-trait)
      - [Pattern 2: Check API Version](#pattern-2-check-api-version)
      - [Pattern 3: Correct Method Name](#pattern-3-correct-method-name)
    - [Troubleshooting Steps](#troubleshooting-steps)
  - [Common Patterns Across All Errors](#common-patterns-across-all-errors)
    - [Step 1: Read the Full Error Message](#step-1-read-the-full-error-message)
    - [Step 2: Apply Compiler Suggestion](#step-2-apply-compiler-suggestion)
    - [Step 3: Consult Documentation](#step-3-consult-documentation)
    - [Step 4: Isolate Minimal Reproducible Example](#step-4-isolate-minimal-reproducible-example)
  - [Advanced Techniques](#advanced-techniques)
    - [Technique 1: Use rust-analyzer for Real-Time Fixes](#technique-1-use-rust-analyzer-for-real-time-fixes)
    - [Technique 2: Incremental Compilation](#technique-2-incremental-compilation)
    - [Technique 3: Use Clippy for Prevention](#technique-3-use-clippy-for-prevention)
  - [Error Fix Priority](#error-fix-priority)
  - [Verification Checklist](#verification-checklist)
  - [Troubleshooting](#troubleshooting-1)
    - ["I fixed the error but it still fails"](#i-fixed-the-error-but-it-still-fails)
    - ["Error message doesn't make sense"](#error-message-doesnt-make-sense)
    - ["Compiler suggestion doesn't work"](#compiler-suggestion-doesnt-work)
  - [Related Guides](#related-guides)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# How-to: Fix Compilation Errors

**Handle E0277 (trait bounds), E0308 (type mismatches), E0283 (ambiguity), E0599 (method not found)**

---

## Prerequisites

- Rust compiler installed (rustc 1.70+)
- Basic understanding of Rust types and traits
- Access to project source code

---

## Problem Statement

You run `cargo build` and encounter compilation errors like:

```
error[E0277]: the trait bound `Cli: clap::Parser` is not satisfied
error[E0308]: mismatched types: expected `String`, found `&str`
error[E0283]: type annotations needed: cannot infer type for `T`
error[E0599]: no method named `parse` found for type `App`
```

This guide shows you how to systematically fix each error type.

---

## Quick Reference

| Error Code | Meaning | Common Fix |
|------------|---------|------------|
| **E0277** | Trait not implemented | Add `#[derive(Trait)]` or `impl Trait for Type` |
| **E0308** | Type mismatch | Convert types (`.to_string()`, `.as_str()`, `&`) |
| **E0283** | Type ambiguity | Add type annotations (`: Type`) |
| **E0599** | Method not found | Import trait or update API usage |

---

## E0277: Trait Bound Not Satisfied

### Scenario

```rust
use clap::Parser;

struct Cli {
    verbose: bool,
}

fn main() {
    let cli = Cli::parse();  // ❌ Error!
}
```

### Error Message

```
error[E0277]: the trait bound `Cli: clap::Parser` is not satisfied
  --> src/main.rs:8:15
   |
8  |     let cli = Cli::parse();
   |               ^^^^^^^^^^^ the trait `Parser` is not implemented for `Cli`
```

### Solution

**Add the missing trait implementation:**

```rust
use clap::Parser;

#[derive(Parser)]  // ✅ Implements Parser trait
struct Cli {
    verbose: bool,
}

fn main() {
    let cli = Cli::parse();  // ✅ Works!
}
```

### Troubleshooting

**If derive doesn't work:**

1. Check if the trait supports derive:
   ```bash
   # Search trait documentation
   cargo doc --open
   # Look for "This trait can be derived"
   ```

2. Implement manually if needed:
   ```rust
   impl Parser for Cli {
       fn parse() -> Self {
           // Custom implementation
       }
   }
   ```

---

## E0308: Mismatched Types

### Scenario

```rust
fn greet(name: String) {
    println!("Hello, {}", name);
}

fn main() {
    greet("Alice");  // ❌ Error!
}
```

### Error Message

```
error[E0308]: mismatched types
  --> src/main.rs:6:11
   |
6  |     greet("Alice");
   |     ----- ^^^^^^^- help: try using a conversion method: `.to_string()`
   |     |     |
   |     |     expected `String`, found `&str`
   |     arguments to this function are incorrect
```

### Solution Patterns

#### Pattern 1: Convert &str → String

```rust
// Option A: Use .to_string()
greet("Alice".to_string());

// Option B: Use .to_owned()
greet("Alice".to_owned());

// Option C: Use String::from()
greet(String::from("Alice"));
```

#### Pattern 2: Change Function Signature

```rust
// Accept &str instead (more flexible)
fn greet(name: &str) {  // ✅ Accepts both String and &str
    println!("Hello, {}", name);
}

fn main() {
    greet("Alice");           // ✅ Works
    greet(&String::from("Bob"));  // ✅ Also works
}
```

#### Pattern 3: Use AsRef Trait

```rust
// Generic over string-like types
fn greet<S: AsRef<str>>(name: S) {
    println!("Hello, {}", name.as_ref());
}

fn main() {
    greet("Alice");               // ✅ &str works
    greet(String::from("Bob"));   // ✅ String works
}
```

### When to Use Each Pattern

| Pattern | Use When | Performance |
|---------|----------|-------------|
| `.to_string()` | Caller owns data | Allocates memory |
| Change signature to `&str` | Function only reads | Zero-cost |
| `AsRef<str>` | Generic API | Zero-cost (monomorphization) |

---

## E0283: Type Annotations Needed

### Scenario

```rust
fn parse<T: FromStr>(input: &str) -> Result<T, T::Err> {
    input.parse()
}

fn main() {
    let value = parse("42");  // ❌ Error!
    println!("{}", value);
}
```

### Error Message

```
error[E0283]: type annotations needed
  --> src/main.rs:6:17
   |
6  |     let value = parse("42");
   |                 ^^^^^ cannot infer type for type parameter `T`
```

### Solution Patterns

#### Pattern 1: Turbofish Syntax

```rust
let value: i32 = parse("42");  // ❌ Still ambiguous

// ✅ Use turbofish to specify T
let value = parse::<i32>("42");
println!("{}", value.unwrap());
```

#### Pattern 2: Type Annotation on Variable

```rust
// ✅ Let compiler infer from variable type
let value: i32 = parse("42").unwrap();
println!("{}", value);
```

#### Pattern 3: Specify in Context

```rust
// ✅ Compiler infers from usage
let value = parse("42").unwrap();
let doubled: i32 = value * 2;  // i32 inferred here
```

---

## E0599: Method Not Found

### Scenario

```rust
use clap::App;

fn main() {
    let app = App::new("ggen");
    let matches = app.parse();  // ❌ Error!
}
```

### Error Message

```
error[E0599]: no method named `parse` found for struct `App` in the current scope
  --> src/main.rs:5:23
   |
5  |     let matches = app.parse();
   |                       ^^^^^ method not found in `App<'_>`
```

### Solution Patterns

#### Pattern 1: Import Missing Trait

```rust
// ❌ Method exists but trait not imported
use clap::App;

// ✅ Import trait that provides method
use clap::{App, IntoApp};

fn main() {
    let app = App::new("ggen");
    let matches = app.get_matches();  // ✅ Correct method name
}
```

#### Pattern 2: Check API Version

```rust
// ❌ Using old API (clap 2.x)
let app = App::new("ggen")
    .version("1.0")
    .get_matches();

// ✅ Use new API (clap 4.x derive pattern)
use clap::Parser;

#[derive(Parser)]
#[command(version = "1.0")]
struct Cli {}

fn main() {
    let cli = Cli::parse();  // ✅ Modern API
}
```

#### Pattern 3: Correct Method Name

```rust
// ❌ Wrong method name
app.parse();

// ✅ Check documentation for correct name
app.get_matches();  // Builder pattern
// or
Cli::parse();  // Derive pattern
```

### Troubleshooting Steps

1. **Check if trait is in scope:**
   ```bash
   rg "trait.*parse" --type rust  # Find trait definition
   ```

2. **Verify struct implements trait:**
   ```bash
   cargo doc --open
   # Search for "impl Parser for Cli"
   ```

3. **Check API version:**
   ```bash
   cargo tree | grep clap
   # Ensure version matches documentation
   ```

---

## Common Patterns Across All Errors

### Step 1: Read the Full Error Message

```
error[E0308]: mismatched types
  --> src/main.rs:6:11
   |
6  |     greet("Alice");
   |     ----- ^^^^^^^- help: try using a conversion method: `.to_string()`
   |     |     |
   |     |     expected `String`, found `&str`
   |     arguments to this function are incorrect
```

**Look for:**
- **Location:** `src/main.rs:6:11` (line 6, column 11)
- **Expected type:** `String`
- **Found type:** `&str`
- **Suggested fix:** `.to_string()`

### Step 2: Apply Compiler Suggestion

```rust
// Compiler suggests: try using a conversion method: `.to_string()`
greet("Alice".to_string());  // ✅ Apply suggestion
```

**Success Rate:** 70% of errors fixed by following compiler suggestions.

### Step 3: Consult Documentation

```bash
# Open local documentation
cargo doc --open

# Search online docs
# https://doc.rust-lang.org/error-index.html#E0308
```

### Step 4: Isolate Minimal Reproducible Example

```rust
// ❌ 200-line file with error somewhere

// ✅ Extract to minimal example
fn main() {
    let x: String = "test";  // Reproduce E0308
}
```

**Why?** Easier to experiment with fixes without breaking production code.

---

## Advanced Techniques

### Technique 1: Use rust-analyzer for Real-Time Fixes

```rust
// In VSCode with rust-analyzer:
// 1. Hover over error squiggle
// 2. Click "Quick Fix" (Ctrl+.)
// 3. Select suggested fix
```

**Example:**
```
Error: `Cli` doesn't implement `Parser`
Quick Fix: Add `#[derive(Parser)]` ✅
```

### Technique 2: Incremental Compilation

```bash
# Fix errors one category at a time
cargo build 2>&1 | grep "E0277" | head -5

# Fix first 5 E0277 errors
# Rebuild
cargo build

# Move to next error type
```

### Technique 3: Use Clippy for Prevention

```bash
# Enable clippy lints to catch issues early
cargo clippy -- -D warnings

# Example caught by clippy:
warning: you should consider adding a `Default` implementation for `Cli`
  --> src/main.rs:3:1
   |
3  | impl Cli {
   | ^^^^^^^^^^
```

---

## Error Fix Priority

When facing 100+ errors, fix in this order:

1. **E0599 (method not found)** - Often reveals API version mismatches
2. **E0277 (trait bounds)** - Cascades to other errors when fixed
3. **E0308 (type mismatches)** - Usually localized, easy fixes
4. **E0283 (ambiguity)** - Rare, often indicates design issues

**Rationale:** E0599 → E0277 fixes often eliminate 50%+ of downstream errors.

---

## Verification Checklist

After fixing errors:

- [ ] `cargo build` succeeds with 0 errors
- [ ] `cargo test` passes (no regressions)
- [ ] `cargo clippy` produces 0 warnings
- [ ] Code review requested (second pair of eyes)
- [ ] CI pipeline passes (integration tests)

---

## Troubleshooting

### "I fixed the error but it still fails"

**Cause:** Stale build artifacts.

**Solution:**
```bash
cargo clean
cargo build
```

### "Error message doesn't make sense"

**Cause:** Cascading errors (one error causes 10 others).

**Solution:**
```bash
# Fix first error only
cargo build 2>&1 | head -20

# Rebuild to see if others disappear
cargo build
```

### "Compiler suggestion doesn't work"

**Cause:** Outdated compiler (missing newer diagnostics).

**Solution:**
```bash
rustup update
cargo --version  # Verify 1.70+
```

---

## Related Guides

- [Error Catalog Reference](../reference/error-catalog.md) - Full error code definitions
- [Why Poka-Yoke Prevents Errors](../explanations/why-poka-yoke-prevents-errors.md) - Compile-time mistake-proofing philosophy

---

**Last Updated:** 2025-11-18
