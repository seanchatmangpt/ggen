# Reference: Error Catalog

**Quick lookup for E0277, E0308, E0283, E0599 and common fixes**

---

## E0277: Trait Bound Not Satisfied

**Error Pattern:**
```
error[E0277]: the trait bound `Type: Trait` is not satisfied
```

**Meaning:** Type doesn't implement required trait.

**Common Causes:**
- Missing `#[derive(Trait)]`
- Wrong type passed to generic function
- Trait not imported

**Fix Patterns:**

| Scenario | Fix | Example |
|----------|-----|---------|
| Derivable trait | Add `#[derive(Trait)]` | `#[derive(Parser, Debug, Clone)]` |
| Custom trait | Implement manually | `impl Trait for Type { ... }` |
| Trait not in scope | Import trait | `use clap::Parser;` |

**Quick Fix:**
```rust
// Before
struct Cli { }
let cli = Cli::parse();  // ❌ E0277

// After
#[derive(Parser)]  // ✅ Implement Parser trait
struct Cli { }
```

---

## E0308: Mismatched Types

**Error Pattern:**
```
error[E0308]: mismatched types
expected `Type1`, found `Type2`
```

**Meaning:** Type doesn't match what function/variable expects.

**Common Conversions:**

| From | To | Method | Example |
|------|----| -------|---------|
| `&str` | `String` | `.to_string()` | `"hello".to_string()` |
| `String` | `&str` | `.as_str()` or `&` | `&my_string` |
| `i32` | `u32` | `.try_into()?` | `value.try_into()?` |
| `Option<T>` | `T` | `.unwrap()` or `?` | `opt.unwrap()` or `opt?` |
| `Result<T, E>` | `T` | `.unwrap()` or `?` | `res.unwrap()` or `res?` |

**Quick Fix:**
```rust
// Before
fn greet(name: String) { }
greet("Alice");  // ❌ E0308: expected String, found &str

// After (Option 1: Convert)
greet("Alice".to_string());  // ✅

// After (Option 2: Change signature)
fn greet(name: &str) { }  // ✅ Accepts both String and &str
```

---

## E0283: Type Annotations Needed

**Error Pattern:**
```
error[E0283]: type annotations needed
cannot infer type for type parameter `T`
```

**Meaning:** Compiler cannot determine concrete type for generic.

**Fix Patterns:**

| Scenario | Fix | Example |
|----------|-----|---------|
| Turbofish syntax | `function::<Type>()` | `parse::<i32>("42")` |
| Variable annotation | `let var: Type = ...` | `let num: i32 = parse("42")?` |
| Context inference | Use in typed context | `let x: i32 = parse("42")?; x * 2` |

**Quick Fix:**
```rust
// Before
let value = parse("42");  // ❌ E0283: what type?

// After (Option 1: Turbofish)
let value = parse::<i32>("42");  // ✅

// After (Option 2: Type annotation)
let value: i32 = parse("42");  // ✅
```

---

## E0599: Method Not Found

**Error Pattern:**
```
error[E0599]: no method named `method` found for type `Type`
```

**Meaning:** Method doesn't exist on type or trait not imported.

**Common Causes:**

| Cause | Fix | Example |
|-------|-----|---------|
| Trait not imported | Import trait | `use std::str::FromStr;` |
| Wrong method name | Check docs | `.get_matches()` not `.parse()` |
| Wrong API version | Update code | clap 2.x → 4.x migration |
| Method on &T not T | Add `&` | `&my_value.method()` |

**Quick Fix:**
```rust
// Before
use clap::App;
let app = App::new("ggen");
app.parse();  // ❌ E0599: no method `parse`

// After (Check clap docs)
use clap::Parser;
#[derive(Parser)]
struct Cli { }
Cli::parse();  // ✅ Correct API
```

---

## E0382: Use of Moved Value

**Error Pattern:**
```
error[E0382]: use of moved value: `value`
```

**Meaning:** Value used after ownership transferred.

**Fix Patterns:**

| Scenario | Fix | Example |
|----------|-----|---------|
| Clone value | `.clone()` | `let y = x.clone();` |
| Borrow instead | `&x` | `func(&x); x.method();` |
| Use reference | Change fn to `&T` | `fn f(x: &String)` |

**Quick Fix:**
```rust
// Before
let s = String::from("hello");
consume(s);
println!("{}", s);  // ❌ E0382: s moved

// After (Option 1: Clone)
let s = String::from("hello");
consume(s.clone());
println!("{}", s);  // ✅

// After (Option 2: Borrow)
fn consume(s: &String) { }  // Change signature
consume(&s);
println!("{}", s);  // ✅
```

---

## E0425: Cannot Find Value

**Error Pattern:**
```
error[E0425]: cannot find value `name` in this scope
```

**Meaning:** Variable/function not defined or not imported.

**Fix Patterns:**

| Cause | Fix | Example |
|-------|-----|---------|
| Typo | Fix spelling | `println` → `println!` |
| Not imported | Add `use` | `use std::fs::File;` |
| Out of scope | Move declaration | Define before use |

---

## Quick Diagnostic Flowchart

```
Compilation error?
├─ E0277? → Add #[derive(Trait)] or impl Trait
├─ E0308? → Convert types (.to_string(), &, .into())
├─ E0283? → Add type annotation (: Type or ::<Type>)
├─ E0599? → Import trait or check method name
├─ E0382? → Clone or borrow (&)
└─ E0425? → Import or fix typo
```

---

## Compiler Suggestion Trust Score

| Error Code | Trust Compiler Suggestion | Why |
|------------|---------------------------|-----|
| E0277 | 90% | Usually correct derive/impl suggestion |
| E0308 | 85% | Type conversion suggestions accurate |
| E0283 | 70% | May suggest turbofish when annotation clearer |
| E0599 | 60% | May miss trait import requirement |
| E0382 | 95% | Clone/borrow suggestions very accurate |

**Rule of Thumb:** Try compiler suggestion first. If doesn't work, consult this catalog.

---

## Related Resources

- [How-to: Fix Compilation Errors](../how-to/fix-compilation-errors.md) - Detailed fix procedures
- [Rust Error Index](https://doc.rust-lang.org/error-index.html) - Official error documentation

---

**Last Updated:** 2025-11-18
