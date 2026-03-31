# LSP Hover and Diagnostics Research Report

**Domain:** Getting type information and diagnostics via LSP for Rust code
**Date:** 2026-03-31
**Research Focus:** Understanding rust-analyzer's hover capabilities for complex generic code and async futures

---

## Executive Summary

rust-analyzer's LSP hover provides rich type information, documentation, and diagnostics that are essential for understanding complex Rust code. This research documents hover behavior for:

1. **Complex generic types** - Multi-level trait bounds, lifetime annotations
2. **Async futures** - `Future::Output` types, `Pin<Box<dyn>>` patterns
3. **Type aliases** - `dyn Trait` objects, boxed futures
4. **Diagnostics integration** - Error context, type mismatch explanations

---

## 1. What Information Does Hover Provide?

### 1.1 Basic Type Information

When hovering over any identifier, rust-analyzer reveals:

- **Full type signature** - Including all generic parameters and lifetime annotations
- **Documentation comments** - From the source code (doc comments)
- **Definition location** - Click to navigate to definition (go-to-definition)
- **Type inference results** - What the compiler inferred for complex expressions

**Example from `/Users/sac/ggen/crates/ggen-ai/src/types.rs`:**

```rust
pub struct PolicyId(String);
```

Hovering over `PolicyId` shows:
- Type: `pub struct PolicyId(String)`
- Documentation: "Policy ID newtype for type safety"
- Derived traits: `Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize`

### 1.2 Function Signatures with Generics

Hover reveals complete generic signatures:

**Example from `/Users/sac/ggen/crates/osiris-core/src/crdt/lww_register.rs`:**

```rust
pub fn set(&self, value: T, timestamp: u64) -> bool
where
    T: Clone + PartialEq
```

Hover shows:
- Full signature with trait bounds
- Receiver type (`&self` vs `&mut self` vs `self`)
- Return type with full path
- Documentation including examples

### 1.3 Async Function Return Types

**Critical insight:** Hover on async functions shows the **desugared return type**:

```rust
pub async fn execute_pipeline(
    &self,
    pipeline: &mut ExecutionPipeline,
) -> Result<PipelineResult, ExecutionError>
```

Hover reveals:
- **Desugared type:** `impl Future<Output = Result<PipelineResult, ExecutionError>>`
- The actual return type is a future, not the direct result
- Full `Future::Output` type is visible

This is essential for understanding:
- What type you get when `.await` is called
- How to chain futures
- What error types to handle

---

## 2. Handling Complex Generic Types

### 2.1 Multi-Level Trait Bounds

**Example from `/Users/sac/ggen/crates/ggen-execution/src/pipeline.rs`:**

```rust
pub fn add_rule<F>(mut self, field: &str, rule: F) -> Self
where
    F: Fn(&str) -> Result<String, InputValidationError> + Send + Sync + 'static,
```

Hover reveals:
- `F` is a function pointer with specific signature
- Trait bounds include `Send + Sync + 'static` (thread safety + lifetime)
- Return type is `Self` (builder pattern)
- Full closure signature is visible

### 2.2 Generic Struct Fields

**Example from `/Users/sac/ggen/crates/osiris-core/src/crdt/lww_register.rs`:**

```rust
pub struct LWWRegister<T: Clone + PartialEq> {
    state: std::sync::atomic::AtomicPtr<LWWState<T>>,
}
```

Hover on `state` shows:
- Type: `std::sync::atomic::AtomicPtr<LWWState<T>>`
- Where `T` is the generic parameter from the struct
- `LWWState<T>` is defined inline with its own generics

### 2.3 Complex Type Aliases

**Example from `/Users/sac/ggen/tests/mcp_a2a/mock_mcp_server.rs`:**

```rust
pub type HandlerFn = Arc<dyn Fn(Value) -> HandlerFnFuture + Send + Sync>;
pub type HandlerFnFuture = std::pin::Pin<Box<dyn std::future::Future<Output = HandlerResult> + Send>>;
```

Hover on `HandlerFn` reveals:
- Full expansion: `Arc<dyn Fn(Value) -> Pin<Box<dyn Future<Output = Result<ToolResult, JsonRpcError>> + Send>> + Send + Sync>`
- This is a **boxed async function** that can be shared across threads
- The nested `Future::Output` type is visible in the hover

---

## 3. Async Futures and Future::Output

### 3.1 Desugaring Async Functions

**Key Finding:** Hover on `async fn` shows the **desugared** return type:

```rust
async fn propose_deltas(
    &self,
    patterns: Vec<Pattern>,
    current_snapshot: Arc<SigmaSnapshot>,
    sector: &str,
) -> Result<Vec<DeltaSigmaProposal>, String>
```

Hover reveals:
- **Actual return type:** `impl Future<Output = Result<Vec<DeltaSigmaProposal>, String>>`
- The `async fn` syntax is sugar for returning a future
- When you `.await`, you get `Result<Vec<DeltaSigmaProposal>, String>`

### 3.2 Boxed Pin Futures

**Example from `/Users/sac/ggen/crates/ggen-core/src/ontology/delta_proposer.rs`:**

```rust
pub type ProposalStream = Pin<Box<dyn Stream<Item = DeltaSigmaProposal> + Send>>;
```

Hover reveals:
- This is an **async stream** (not a single future)
- `Stream<Item = T>` is a future of multiple values
- `Pin<Box<...>>` means the stream is heap-allocated and cannot be moved
- `+ Send` means it can be sent across threads

### 3.3 Complex Future Chaining

**Pattern from `/Users/sac/ggen/crates/ggen-execution/src/pipeline.rs`:**

```rust
let handle = tokio::spawn(async move {
    let _permit = sem.acquire().await.unwrap();
    let mut fw = framework.lock().await;
    fw.execute_task(task_clone).await
});
```

Hover on `handle` shows:
- Type: `JoinHandle<Result<TaskResult, ExecutionError>>`
- The inner `async move` block's return type
- How to access the result: `.await?` on the join handle

### 3.4 Box::pin Patterns

**Common pattern for boxing async futures:**

```rust
Box::pin(async move {
    Ok(vec![])
})
```

Hover reveals:
- Type: `Pin<Box<impl Future<Output = Result<Vec<DeltaSigmaProposal>, String>>>>`
- `Box::pin()` converts a future into a pinned, heap-allocated future
- Useful for returning futures from functions (trait object polymorphism)

---

## 4. Accessing Diagnostics via LSP

### 4.1 Error Messages on Hover

When there's a type error, hovering over the error location shows:

- **Type mismatch details** - Expected type vs. actual type
- **Trait implementation suggestions** - "Consider implementing `Trait` for `Type`"
- **Lifetime annotations** - Why a lifetime is required
- **Missing method errors** - What methods are available on the type

**Example (hypothetical):**

```rust
let x: PolicyId = request_id;  // Type error
```

Hover shows:
- `expected PolicyId, found RequestId`
- Suggestion: "Try converting with `PolicyId::from(request_id.as_str())`"
- Note: "These are distinct types to prevent mixing IDs"

### 4.2 Diagnostic Hints

rust-analyzer provides **real-time diagnostics** as you type:

- **Missing trait implementations** - "Trait `Send` is not implemented"
- **Unused variables** - "Variable `x` is never read"
- **Dead code warnings** - "Function is never used"
- **Type inference failures** - "Cannot infer type for `T`"

These appear as:
- **Red squigglies** in the editor (errors)
- **Yellow squigglies** (warnings)
- **Hover to see full message**

### 4.3 Inlay Hints (Optional Feature)

rust-analyzer can show **inlay hints** (inline type annotations):

```rust
fn process<T>(value: T) -> T {
    /* T */ value
}
```

Hover doesn't show inlay hints directly, but they appear in the editor as:
- `value: T` (type annotation inline)
- `-> T` (return type annotation)
- `T:` (generic parameter annotation)

---

## 5. Best Practices for Using Hover

### 5.1 Understanding Complex Generics

**Scenario:** You see this code and don't understand the types:

```rust
pub fn add_rule<F>(mut self, field: &str, rule: F) -> Self
where
    F: Fn(&str) -> Result<String, InputValidationError> + Send + Sync + 'static,
```

**Hover workflow:**
1. Hover over `F` → See it's a function pointer type
2. Hover over `Fn(&str)` → See the signature it must match
3. Hover over `Result<String, InputValidationError>` → See the error type
4. Hover over `Send + Sync + 'static` → Understand thread safety requirements

**Result:** You now understand `add_rule` accepts a closure that:
- Takes a `&str` (field value)
- Returns `Result<String, InputValidationError>` (validated value or error)
- Is thread-safe (`Send + Sync`)
- Has no borrowed data (`'static`)

### 5.2 Tracing Async Return Types

**Scenario:** You're debugging an async function:

```rust
async fn execute_pipeline(&self, pipeline: &mut ExecutionPipeline) -> Result<PipelineResult, ExecutionError>
```

**Hover workflow:**
1. Hover on `execute_pipeline` → See `impl Future<Output = Result<PipelineResult, ExecutionError>>`
2. Now you know `.await` will give you `Result<PipelineResult, ExecutionError>`
3. Hover on `PipelineResult` → See what fields are available
4. Hover on `ExecutionError` → See what error variants to handle

### 5.3 Unboxing Type Aliases

**Scenario:** You encounter this complex type:

```rust
pub type HandlerFnFuture = std::pin::Pin<Box<dyn std::future::Future<Output = HandlerResult> + Send>>;
```

**Hover workflow:**
1. Hover on `HandlerFnFuture` → See full expansion
2. Break it down:
   - `dyn Future<Output = HandlerResult>` → Some future that returns `HandlerResult`
   - `Box<...>` → Heap-allocated (unknown size at compile time)
   - `Pin<...>` → Cannot be moved in memory (required for async)
   - `+ Send` → Thread-safe

**Result:** You now understand this is a **boxed, pinned, thread-safe future** for async handlers.

### 5.4 Debugging Type Errors

**Scenario:** Compiler error: "expected `T`, found `U`"

**Hover workflow:**
1. Hover on the error location → See exact type mismatch
2. Hover on each type → See full definitions
3. Look for trait implementations → "Can `T` be converted to `U`?"
4. Check trait bounds → "Are generics constrained correctly?"

---

## 6. Limitations and Caveats

### 6.1 Macro Expansion

Hover does **NOT** expand macros by default:

```rust
pub async fn execute_pipeline(...) -> Result<PipelineResult, ExecutionError>
```

Hover shows the **desugared** `impl Future<...>` but doesn't show:
- The exact generated code from the `async` macro expansion
- Internal state machine representation

**Workaround:** Use `cargo expand` (requires nightly) to see full expansion.

### 6.2 Type Inference Complexity

For very complex type inference, hover may show:

```
impl Future<Output = Result<...very long type...>>
```

This can be overwhelming. **Best practice:** Break down complex expressions into intermediate variables with explicit type annotations.

### 6.3 Generic Instantiation

Hover on generic code shows the **generic definition**, not the **instantiated type** for a specific call site:

```rust
fn process<T>(value: T) -> T { value }
```

Hover on `process` always shows `<T>`, not `process<String>` even if called with a string.

**Workaround:** Use IDE features like "Inlay Hints" or "Type Hover at Call Site" (varies by editor).

---

## 7. Real-World Examples from ggen Codebase

### 7.1 Complex Generic Builder

**File:** `/Users/sac/ggen/crates/ggen-execution/src/pipeline.rs`

```rust
pub fn add_rule<F>(mut self, field: &str, rule: F) -> Self
where
    F: Fn(&str) -> Result<String, InputValidationError> + Send + Sync + 'static,
{
    self.rules.insert(field.to_string(), Box::new(rule));
    self
}
```

**Hover reveals:**
- `F` is a function pointer/closure
- Must be thread-safe (`Send + Sync`)
- Must own all data (`'static` lifetime)
- Returns a `Result` with specific error type
- `Box::new(rule)` → Converts to trait object for storage

### 7.2 Lock-Free CRDT with Atomics

**File:** `/Users/sac/ggen/crates/osiris-core/src/crdt/lww_register.rs`

```rust
pub struct LWWRegister<T: Clone + PartialEq> {
    state: std::sync::atomic::AtomicPtr<LWWState<T>>,
}
```

**Hover on `state` reveals:**
- `AtomicPtr<LWWState<T>>` → Atomic pointer to heap-allocated state
- `LWWState<T>` → Contains value, timestamp, region_id
- `T: Clone + PartialEq` → Required for value comparisons
- `unsafe impl Send` → Thread-safe despite raw pointer

### 7.3 Async Stream Type Alias

**File:** `/Users/sac/ggen/crates/ggen-core/src/ontology/delta_proposer.rs`

```rust
pub type ProposalStream = Pin<Box<dyn Stream<Item = DeltaSigmaProposal> + Send>>;
```

**Hover reveals:**
- `Stream<Item = T>` → Async iterator (multiple values over time)
- `Pin<Box<...>>` → Heap-allocated, immovable (required for async)
- `+ Send` → Thread-safe (can send across threads)
- `dyn Stream` → Trait object (dynamic dispatch)

### 7.4 Handler Function Type

**File:** `/Users/sac/ggen/tests/mcp_a2a/mock_mcp_server.rs`

```rust
pub type HandlerFn = Arc<dyn Fn(Value) -> HandlerFnFuture + Send + Sync>;
pub type HandlerFnFuture = std::pin::Pin<Box<dyn std::future::Future<Output = HandlerResult> + Send>>;
```

**Hover reveals:**
- `HandlerFn` is a **shared async function** (Arc = reference-counted)
- `dyn Fn` → Trait object (can be any callable)
- `HandlerFnFuture` → **Boxed pinned future**
- Nested expansion shows: `Arc<dyn Fn(Value) -> Pin<Box<dyn Future<Output = Result<ToolResult, JsonRpcError>> + Send>> + Send + Sync>`

This is how you store async functions in a struct for later execution.

---

## 8. Key Takeaways

### 8.1 For Understanding Types

1. **Hover over everything** - Variables, functions, types, trait bounds
2. **Read the full signature** - Don't ignore generic parameters or lifetimes
3. **Follow the types** - Hover on each type in a complex expression
4. **Check trait bounds** - They reveal constraints and capabilities

### 8.2 For Async Code

1. **Hover on async fn** → See `impl Future<Output = T>` (desugared)
2. **Hover on .await** → See what type you actually get
3. **Look for Pin<Box<...>>** → Indicates heap-allocated future
4. **Check for + Send** → Can this future be sent across threads?

### 8.3 For Debugging Errors

1. **Hover on the error** → See exact type mismatch
2. **Hover on each type** → Understand what's expected vs. actual
3. **Check trait implementations** → Is a required trait missing?
4. **Look for suggestions** → rust-analyzer often proposes fixes

### 8.4 For Complex Generics

1. **Break down trait bounds** - Each bound reveals a constraint
2. **Follow type parameters** - Where do they come from?
3. **Check lifetimes** - `'static` means owned data, `'a` means borrowed
4. **Look for where clauses** - Additional constraints not in the signature

---

## 9. Comparison: LSP vs. Grep for Understanding Code

### 9.1 When to Use LSP Hover

**Use LSP hover when:**
- You need **exact type information** for a specific identifier
- You want **documentation** from the source code
- You're **debugging type errors** and need to see mismatch details
- You need **real-time diagnostics** as you edit
- You want **go-to-definition** navigation

**Example:** "What type does this async function return when I `.await` it?"

### 9.2 When to Use Grep

**Use Grep when:**
- You need to **find all usages** of a type or function across files
- You're **searching for patterns** (e.g., "all impls of Future")
- You want **global search** across the entire codebase
- You need **regex-based searches** for complex patterns

**Example:** "Find all places where `LWWRegister` is instantiated"

### 9.3 Combined Workflow

**Best practice for understanding complex code:**

1. **Grep** to find where a type is used across the codebase
2. **LSP hover** on each usage to see the exact type at that location
3. **LSP go-to-definition** to jump to the implementation
4. **LSP hover** on the definition to see full signature and docs
5. **LSP find-references** to see all call sites
6. **Repeat** for related types

---

## 10. Conclusion

rust-analyzer's LSP hover is **essential** for understanding complex Rust code:

- **Complex generics** are fully expanded with all trait bounds
- **Async futures** show `Future::Output` types clearly
- **Type aliases** expand to their full definitions
- **Diagnostics** provide actionable error messages with context

**Best practice:** Make hover your first tool for understanding unfamiliar code. Combine with grep for global searches and go-to-definition for navigation.

**Remember:** Hover shows the **compiler's view** of the code - exactly what rustc sees, including type inference results and desugared async syntax.

---

## Appendix: Code References

### Files Analyzed

1. `/Users/sac/ggen/crates/ggen-execution/src/pipeline.rs` - Complex generic builders, async pipeline execution
2. `/Users/sac/ggen/crates/osiris-core/src/crdt/lww_register.rs` - Lock-free CRDT with atomic generics
3. `/Users/sac/ggen/crates/ggen-ai/src/types.rs` - Newtype pattern for type safety
4. `/Users/sac/ggen/crates/ggen-core/src/ontology/delta_proposer.rs` - Async stream type aliases
5. `/Users/sac/ggen/tests/mcp_a2a/mock_mcp_server.rs` - Boxed future type aliases
6. `/Users/sac/ggen/crates/ggen-api/src/middleware/validation.rs` - Trait object validation rules

### Key Patterns Identified

1. **Async fn desugaring** → `impl Future<Output = T>`
2. **Boxed futures** → `Pin<Box<dyn Future<Output = T> + Send>>`
3. **Trait objects** → `dyn Trait + Send + Sync`
4. **Generic bounds** → `T: Clone + PartialEq + 'static`
5. **Builder pattern** → `-> Self` with chained methods

---

**Research completed:** 2026-03-31
**Researcher:** Claude Code (LSP-focused investigation)
**Tools used:** Grep, Read, LSP (rust-analyzer), codebase analysis
