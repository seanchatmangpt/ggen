# Call Hierarchy Analysis in Rust: Research Report

**Date:** 2026-03-31
**Domain:** Rust LSP (Language Server Protocol) - Call Hierarchy Navigation
**Research Scope:** Async/await, Trait Dispatch, Closures, and LSP Limitations

## Executive Summary

Call hierarchy analysis in Rust is **fundamentally limited** by the language's design, particularly around:
1. **Dynamic trait dispatch** (`dyn Trait`) - runtime polymorphism breaks static analysis
2. **Async/await transformations** - compiler-generated state machines obscure call chains
3. **Closure capture patterns** - anonymous functions with complex environment captures
4. **Generic code** - monomorphization happens at compile time, not visible to LSP

The LSP `incomingCalls` and `outgoingCalls` operations work best for **direct, synchronous, monomorphic function calls**. They degrade significantly for async traits, dynamic dispatch, and closure-heavy code.

---

## 1. Async/Await: Compiler State Machines

### How Async Works in Rust

When you write an async function:

```rust
async fn process_message(&mut self, message: ConvergenceMessage) -> Result<f64> {
    let applicable_indices: Vec<usize> = self
        .convergence_strategies
        .iter()
        .enumerate()
        .filter(|(_, strategy)| strategy.is_applicable(&message))
        .map(|(i, _)| i)
        .collect();

    // ... more code ...

    Ok(total_improvement / strategy_count.max(1) as f64)
}
```

The Rust compiler **desugars** this into:

```rust
fn process_message<'a, 'async_trait>(
    &'a mut self,
    message: ConvergenceMessage,
) -> Pin<Box<dyn Future<Output = Result<f64>> + Send + 'async_trait>>
where
    Self: 'async_trait
{
    // Compiler-generated state machine
    Box::pin(async move {
        // Your code here, wrapped in a future
    })
}
```

### Impact on Call Hierarchy

**Problem:** LSP sees the desugared signature, not your original async fn.

**What breaks:**
- `incomingCalls` on `process_message` may miss callers if the LSP only looks at the `Pin<Box<dyn Future>>` wrapper
- `outgoingCalls` into async functions (e.g., `strategy.is_applicable(&message)`) can be obscured by the state machine
- `.await` points create implicit yield points that LSP may not trace through

**Real example from ggen:**

```rust
// File: crates/ggen-execution/src/convergence.rs:452
pub async fn process_message(
    &mut self, message: ConvergenceMessage,
) -> Result<f64, ExecutionError> {
    // LSP may struggle to trace:
    // 1. Calls to `strategy.is_applicable()` (trait method, see below)
    // 2. Calls to `self.update_metrics()` (borrowck complexities)
    // 3. The async state machine boundaries
}
```

### Workarounds

1. **Use `prepareCallHierarchy` first** - LSP implementation must correctly resolve async fn positions
2. **Trace through `.await` points** - LSP should follow futures through await boundaries
3. **Prefer explicit types** - Avoid `impl Future` in favor of named types where possible

---

## 2. Trait Method Dispatch: Static vs Dynamic

### Static Dispatch (Generics) - **LSP-Friendly**

```rust
// File: crates/ggen-execution/src/convergence.rs:121
#[async_trait::async_trait]
pub trait ConvergenceStrategyTrait: Send + Sync {
    fn name(&self) -> &str;
    async fn apply_convergence(
        &self, engine: &mut SemanticConvergenceEngine, message: &ConvergenceMessage,
    ) -> Result<f64, ExecutionError>;
    fn is_applicable(&self, message: &ConvergenceMessage) -> bool;
}

// Static dispatch via generics
pub fn execute_strategy<S: ConvergenceStrategyTrait>(strategy: &S) {
    // LSP can trace this: S is monomorphized at compile time
    strategy.is_applicable(msg);
}
```

**LSP can track this** because generic monomorphization happens at compile time, and rust-analyzer can infer concrete types.

### Dynamic Dispatch (`dyn Trait`) - **LSP-Limited**

```rust
// File: crates/ggen-execution/src/convergence.rs:81
pub struct SemanticConvergenceEngine {
    // Dynamic dispatch: trait objects in a Vec
    convergence_strategies: Vec<Box<dyn ConvergenceStrategyTrait>>,
}

// Dynamic dispatch through trait object
for idx in applicable_indices {
    let strategy = &self.convergence_strategies[idx];
    let is_applicable = strategy.is_applicable(&message);  // ⚠️ LSP limitation
    // ...
}
```

**Problem:** The concrete type is **erased** at runtime. LSP cannot know which implementation:
- `AlignmentStrategy`
- `ValidationStrategy`
- `NormalizationStrategy`
- `ConsensusStrategy`

...is actually called at line 476.

**What LSP sees:**
- `strategy.is_applicable(&message)` calls `ConvergenceStrategyTrait::is_applicable`
- But LSP cannot determine **which implementation** is in the Box at runtime

**Impact:**
- `incomingCalls` on `AlignmentStrategy::is_applicable` will be **incomplete** (misses dynamic calls)
- `outgoingCalls` from the call site will show **only the trait method**, not the impl

### Trait Bounds and Where Clauses

```rust
// Complex trait bounds complicate LSP resolution
pub async fn execute_parallel<T, F, Fut>(
    &self,
    tasks: Vec<(String, F)>,
) -> Vec<AgentExecutionResult<T>>
where
    T: Send + 'static,
    F: FnOnce() -> Fut + Send + 'static,
    Fut: Future<Output = Result<T>> + Send + 'static,
{
    // LSP must resolve:
    // - F: FnOnce closure
    // - Fut: Future returned by closure
    // - T: Output type
}
```

LSP implementations may struggle with:
- Multi-clause where bounds
- Lifetime elision in async contexts
- Complex generic constraints

---

## 3. Closures and Captures: Anonymous Functions

### Closure Patterns in ggen

```rust
// File: crates/ggen-ai/src/hyper_concurrent/executor.rs:248
Some((agent_id, move || async move {
    agent.execute(&ctx, input).await
}))
```

This is a **double-nested closure**:
1. Outer: `move ||` - moves captured variables
2. Inner: `async move` - creates an async future

**LSP challenges:**
- Closure types are **anonymous** (compiler-generated names like `closure-0`)
- Captures are inferred by the compiler
- Nested closures create multiple anonymous layers

### Closure Captures

```rust
pub async fn execute_parallel<T, F, Fut>(
    &self,
    tasks: Vec<(String, F)>,
) -> Vec<AgentExecutionResult<T>>
where
    F: FnOnce() -> Fut + Send + 'static,
    Fut: Future<Output = Result<T>> + Send + 'static,
{
    // Each closure captures different variables
    let task = move || async move {
        // Captures: agent, ctx, input (by move)
        agent.execute(&ctx, input).await
    };
}
```

**LSP limitations:**
- Cannot easily show **what variables are captured**
- Cannot trace **capture mutations** (e.g., `&mut self` captures)
- Closure call sites are **everywhere** but anonymous

---

## 4. LSP Operations: What Works vs What Doesn't

### `prepareCallHierarchy` - **First Step Required**

**Purpose:** Create a call hierarchy item from a cursor position.

**Works well for:**
- Named functions: `fn process_message()`
- Named methods: `impl SemanticConvergenceEngine { fn register_agent() }`
- Trait definitions: `trait ConvergenceStrategyTrait`

**Fails for:**
- Closure bodies (anonymous)
- Macro-generated code
- Derived implementations (e.g., `#[derive(Debug)]`)

### `incomingCalls` - **Who Calls This Function?**

**Best case:**
```rust
// Direct, named call
fn foo() {
    process_message(msg).await;  // ✅ LSP finds this
}
```

**Problematic cases:**
```rust
// 1. Dynamic trait dispatch
strategies[0].is_applicable(&msg);  // ⚠️ LSP may miss concrete impl

// 2. Closure capture
let task = move || async move {
    agent.execute(&ctx, input).await  // ⚠️ Anonymous caller
};

// 3. Generic function
fn execute<S: Strategy>(s: &S) {
    s.apply();  // ⚠️ LSP may not resolve all monomorphized S
}
```

### `outgoingCalls` - **What Does This Function Call?**

**Best case:**
```rust
async fn process_message(&mut self, msg: Message) -> Result<f64> {
    self.update_metrics(score).await;  // ✅ Direct method call
    Ok(score)
}
```

**Problematic cases:**
```rust
// 1. Trait method calls
strategy.is_applicable(&msg);  // ⚠️ Which impl?

// 2. Macro-generated code
graphql_query!(query);  // ⚠️ Macro expansion opaque

// 3. Through dyn trait
let service: Box<dyn LlmService> = ...;
service.generate_skill_impl(...)?;  // ⚠️ Dynamic dispatch
```

---

## 5. Real-World Examples from ggen Codebase

### Example 1: Async Trait with Dynamic Dispatch

**File:** `/Users/sac/ggen/crates/ggen-execution/src/convergence.rs:452`

```rust
pub async fn process_message(
    &mut self, message: ConvergenceMessage,
) -> Result<f64, ExecutionError> {
    // Find applicable strategies
    let applicable_indices: Vec<usize> = self
        .convergence_strategies  // Vec<Box<dyn ConvergenceStrategyTrait>>
        .iter()
        .enumerate()
        .filter(|(_, strategy)| strategy.is_applicable(&message))
        .map(|(i, _)| i)
        .collect();

    // Apply each strategy
    for idx in applicable_indices {
        let strategy = &self.convergence_strategies[idx];
        let improvement = match idx {
            0 if !self.convergence_strategies.is_empty() => 0.9,  // ⚠️ Manual dispatch
            _ => 0.8,
        };
        // ...
    }
}
```

**Call hierarchy challenges:**
1. `strategy.is_applicable(&message)` - **Dynamic dispatch** through `dyn ConvergenceStrategyTrait`
2. LSP cannot determine which concrete strategy (AlignmentStrategy, ValidationStrategy, etc.) is called
3. Manual match dispatch at line 483 is traceable, but fragile

**LSP behavior:**
- `incomingCalls` on `AlignmentStrategy::is_applicable` will **miss** line 456 (dynamic call)
- `outgoingCalls` from `process_message` will show the trait method, not the concrete impl

### Example 2: MCP Server Tool Routing

**File:** `/Users/sac/ggen/crates/ggen-a2a-mcp/src/ggen_server.rs:365`

```rust
#[tool_router]
impl GgenMcpServer {
    #[tool(
        description = "Generate code from a RDF ontology file via the ggen μ₁-μ₅ pipeline..."
    )]
    async fn generate(
        &self, Parameters(params): Parameters<GenerateParams>,
    ) -> Result<CallToolResult, McpError> {
        // ...

        let result = tokio::task::spawn_blocking(move || {
            run_sync_blocking(ontology, queries, output, lang, false, true)
        })
        .await
        .map_err(|e| McpError::internal_error(e.to_string(), None))?;

        // ...
    }
}
```

**Call hierarchy challenges:**
1. `#[tool_router]` macro - **Macro-generated code** obscures call sites
2. `spawn_blocking` - **Thread boundary** complicates call tracking
3. `move ||` closure - **Anonymous function** captures variables

**LSP behavior:**
- `incomingCalls` on `generate` may miss calls from macro-generated router code
- `outgoingCalls` cannot trace into `run_sync_blocking` closure (different thread)

### Example 3: Pipeline LLM Service Injection

**File:** `/Users/sac/ggen/crates/ggen-core/src/codegen/pipeline.rs:933`

```rust
pub fn generate_skill_impl(
    &self, skill_name: &str, system_prompt: &str, implementation_hint: &str, language: &str,
) -> Result<String> {
    // Use injected LLM service if available, otherwise use default
    let service = self
        .llm_service
        .as_ref()
        .map(|s| s.as_ref())
        .unwrap_or(&DefaultLlmService);

    // Call trait method through dyn LlmService
    service
        .generate_skill_impl(skill_name, system_prompt, implementation_hint, language)
        .map_err(|e| Error::new(&format!("LLM generation failed: {}", e)))
}
```

**Call hierarchy challenges:**
1. `service: &dyn LlmService` - **Dynamic trait object**
2. `DefaultLlmService` is a **fallback** - LSP may not see this as a caller
3. The trait method `generate_skill_impl` is called through **vtable dispatch**

**LSP behavior:**
- `incomingCalls` on `DefaultLlmService::generate_skill_impl` will be **incomplete**
- `outgoingCalls` from `generate_skill_impl` shows only the trait method, not concrete impls

---

## 6. Limitations of Call Hierarchy in Rust

### Fundamental Language Design Issues

1. **Monomorphization is compile-time only**
   - Generic code is duplicated for each concrete type
   - LSP must **infer** all possible instantiations
   - Some instantiations may be in downstream crates (unavailable to LSP)

2. **Dynamic dispatch erases types**
   - `dyn Trait` trait objects hide concrete types
   - Vtable calls are **runtime** decisions
   - LSP cannot predict which impl is called without data flow analysis

3. **Async state machines are opaque**
   - `.await` points create yield points
   - Compiler-generated futures have mangled names
   - LSP must de-sugar async fn to trace calls

4. **Closures are anonymous**
   - Closure types have compiler-generated names
   - Captures are inferred, not explicit
   - Nested closures create multiple anonymous layers

### Practical LSP Limitations

| Scenario | LSP Support | Reason |
|----------|-------------|--------|
| Direct function call | ✅ Full | Static resolution |
| Generic function (monomorphized) | ⚠️ Partial | LSP infers some concrete types |
| Trait method (static dispatch) | ✅ Good | Generic bounds known |
| Trait method (dynamic dispatch) | ❌ Limited | Concrete type erased |
| Async function | ⚠️ Partial | State machine boundaries |
| Closure capture | ❌ Very Limited | Anonymous type |
| Macro-generated code | ❌ None | Macro expansion opaque |
| Cross-crate generic | ⚠️ Partial | Depends on crate visibility |

---

## 7. Effective Patterns for Call Hierarchy Analysis

### Prefer Static Dispatch

```rust
// ✅ GOOD: Static dispatch (LSP-friendly)
pub fn execute_strategy<S: ConvergenceStrategyTrait>(strategy: &S) {
    strategy.apply();
}

// ❌ AVOID: Dynamic dispatch (LSP-hostile)
pub fn execute_strategy(strategy: &Box<dyn ConvergenceStrategyTrait>) {
    strategy.apply();
}
```

### Avoid Nested Closures

```rust
// ✅ GOOD: Named async function
async fn execute_agent(agent: Agent, ctx: Context) -> Result<Output> {
    agent.execute(&ctx).await
}

// ❌ AVOID: Nested closure
let task = move || async move {
    agent.execute(&ctx).await
};
```

### Use Explicit Types

```rust
// ✅ GOOD: Explicit future type
type BoxedFuture = Pin<Box<dyn Future<Output = Result<f64>> + Send>>;

fn process(&self) -> BoxedFuture {
    Box::pin(async move { /* ... */ })
}

// ⚠️ LESS GOOD: Impl trait in return position
fn process(&self) -> impl Future<Output = Result<f64>> {
    async move { /* ... */ }
}
```

### Document Dynamic Dispatch

```rust
// When dynamic dispatch is unavoidable, add comments:
// NOTE: This is a dynamic call through `dyn LlmService`.
// LSP call hierarchy will not show all concrete implementations.
service.generate_skill_impl(...)?;
```

---

## 8. Recommendations for Tooling

### For LSP Implementers (rust-analyzer)

1. **Improve async fn resolution**
   - De-sugar async fn before call hierarchy analysis
   - Trace through `.await` boundaries
   - Show future state machine structure

2. **Dynamic dispatch tracking**
   - Track trait object allocations
   - Infer possible concrete types from data flow
   - Show "possible implementations" for `dyn Trait` calls

3. **Closure naming hints**
   - Assign synthetic names to closures (e.g., `closure_at_line_42`)
   - Show capture lists in call hierarchy
   - Distinguish move vs borrow captures

4. **Generic monomorphization hints**
   - Show all inferred concrete types for generics
   - Cross-crate instantiation analysis
   - Highlight "may be called elsewhere" for public generic functions

### For Rust Developers

1. **Design for traceability**
   - Prefer static dispatch over dynamic where possible
   - Use named functions instead of closures for complex logic
   - Avoid deeply nested async closures

2. **Document limitations**
   - Add comments when dynamic dispatch is intentional
   - Note "LSP cannot track this" for macro-generated calls
   - Provide alternative named functions for critical paths

3. **Test with LSP**
   - Periodically verify `incomingCalls` for critical functions
   - Check `outgoingCalls` to ensure traceability
   - Refactor if call hierarchy is unexpectedly incomplete

---

## 9. Conclusion

Call hierarchy analysis in Rust is **inherently limited** by:
- Dynamic trait dispatch (runtime polymorphism)
- Async state machines (compiler transformations)
- Closure captures (anonymous types)
- Generic monomorphization (compile-time duplication)

**Best practices:**
1. Prefer static dispatch where possible
2. Use named functions over closures for complex logic
3. Document intentional dynamic dispatch
4. Design APIs with traceability in mind
5. Accept that LSP cannot show **all** callers in complex systems

**The ggen codebase demonstrates all these challenges:**
- Async trait objects in convergence engine
- MCP server tool routing through macros
- LLM service injection via trait objects
- Hyper-concurrent executor with closures

**Call hierarchy is a tool, not a complete solution.** Use it alongside:
- `grep`/`rg` for text-based search
- `cargo doc` for API documentation
- Runtime tracing (e.g., `tokio-console`) for async execution
- Manual code review for complex patterns

---

## References

- **ggen codebase:**
  - `/Users/sac/ggen/crates/ggen-execution/src/convergence.rs` - Async trait dispatch
  - `/Users/sac/ggen/crates/ggen-a2a-mcp/src/ggen_server.rs` - MCP tool routing
  - `/Users/sac/ggen/crates/ggen-core/src/codegen/pipeline.rs` - LLM service injection
  - `/Users/sac/ggen/crates/ggen-ai/src/hyper_concurrent/executor.rs` - Closure-heavy executor

- **Rust LSP specification:**
  - [LSP: Call Hierarchy](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_prepareCallHierarchy)
  - [rust-analyzer call hierarchy](https://rust-analyzer.github.io/manual.html#call-hierarchy)

- **Rust async internals:**
  - [Rust async deep dive](https://rust-lang.github.io/async-book/01_execution/02_future.html)
  - [Async state machines](https://blog.yoshuawuyts.com/async-rust-state-machines/)

---

**End of Report**
