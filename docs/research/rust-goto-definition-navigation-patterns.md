# Go-to-Definition Navigation Patterns in Rust

**Research Date:** 2026-03-31
**Context:** ggen v6.0.1 codebase - 30-crate Rust workspace
**Focus:** LSP-based go-to-definition navigation patterns for complex Rust code

## Executive Summary

Rust's go-to-definition navigation through LSP (rust-analyzer) handles multiple complexity patterns well, but has specific behaviors around traits, impls, re-exports, and macros that developers should understand. This research documents patterns found in a production Rust codebase with 30 crates and complex trait/generic hierarchies.

---

## 1. Trait Methods vs Impl Blocks

### Pattern: Trait Definition Navigation

**Navigation Flow:**
```
Usage site → Trait definition → Impl block(s)
```

**Key Finding:** LSP provides two-level navigation:

1. **First-level go-to-definition** (on trait method call):
   - Navigates to **trait definition** (the `trait` block)
   - Shows method signature with default implementation (if any)

2. **Second-level navigation** (from trait definition):
   - Use `findReferences` to locate all `impl Trait for Type` blocks
   - Use `goToImplementation` to jump directly to concrete implementations

**Example from ggen codebase:**

```rust
// In crates/ggen-ai/src/mcp/traits.rs
#[async_trait::async_trait]
pub trait MCPToolServer: Send + Sync {
    async fn list_tools(&self) -> Result<Value>;
    async fn invoke_tool(&self, tool_name: &str, arguments: Value) -> Result<Value>;
}

// In same file, implementation:
#[async_trait::async_trait]
impl MCPToolServer for MCPToolServerImpl {
    async fn list_tools(&self) -> Result<Value> { /* ... */ }
    async fn invoke_tool(&self, tool_name: &str, arguments: Value) -> Result<Value> { /* ... */ }
}
```

**Navigation Behavior:**
- Cursor on `list_tools` in usage site → Trait definition (line 43 in traits.rs)
- From trait definition, use `goToImplementation` → Jump to line 309 (impl block)

### Best Practices

1. **For trait method calls:**
   - Position cursor on method name (not the parentheses)
   - Character position should be on the identifier (first character is safest)

2. **For finding implementations:**
   - Use `goToImplementation` instead of manual search
   - LSP will show list of all impl blocks if multiple exist

3. **For trait hierarchy:**
   - Use `documentSymbol` to see all traits and impls in a file
   - Traits appear under "Trait" node, impls under "Impl" node

---

## 2. Macro Expansions and Navigation

### Pattern: Declarative Macros (`macro_rules!`)

**Key Finding:** LSP does **NOT** navigate into macro expansions by default.

**Example from ggen:**
```rust
// In crates/ggen-utils/src/error.rs
macro_rules! bail {
    ($msg:expr) => {
        return Err(Error::new($msg));
    };
    ($fmt:expr, $($arg:tt)*) => {
        return Err(Error::new(&format!($fmt, $($arg)*)));
    };
}

// Usage:
bail!("Failed to connect");
```

**Navigation Behavior:**
- Cursor on `bail` → Navigates to macro definition (line with `macro_rules!`)
- Does **NOT** expand to show what code the macro generates
- To see expansion: Use `rust-analyzer.expandMacro` command (not in basic LSP)

### Pattern: Procedural Macros (Derive Macros)

**Example from ggen-macros:**
```rust
// In crates/ggen-macros/src/lib.rs
#[proc_macro_derive(Guard, attributes(guard_name, guard_description))]
pub fn derive_guard(input: TokenStream) -> TokenStream {
    // Generates impl Guard for struct
}

// Usage:
#[derive(Guard)]
#[guard_name = "Guard8020"]
pub struct Guard8020 { /* ... */ }
```

**Navigation Behavior:**
- Cursor on `Guard` in `#[derive(Guard)]` → Navigates to `derive_guard` function
- Cannot navigate to generated code (code doesn't exist in source)
- Use `cargo expand` (external tool) to see generated code

### Best Practices

1. **For declarative macros:**
   - Navigation works reliably to `macro_rules!` definition
   - Read macro definition to understand behavior
   - No expansion view in standard LSP

2. **For procedural macros:**
   - Navigates to proc-macro function
   - Use `cargo expand` for generated code
   - Generated code not navigable (not in source)

3. **For macro attributes:**
   - Navigate to attribute proc-macro definition
   - Read implementation to understand transformation

---

## 3. Re-Export Navigation

### Pattern: `pub use` Re-Exports

**Key Finding:** LSP handles re-exports transparently - navigation goes through to original definition.

**Example from ggen-core:**
```rust
// In crates/ggen-core/src/lib.rs
pub use lifecycle::{
    Placeholder, PlaceholderProcessor, PlaceholderRegistry,
    ReadinessCategory, ReadinessReport, ReadinessRequirement,
    ReadinessStatus, ReadinessTracker,
};

// In crates/ggen-core/src/lifecycle.rs (original definition)
pub struct ReadinessTracker { /* ... */ }
```

**Navigation Behavior:**
- Cursor on `ReadinessTracker` in usage → Navigates to `lifecycle.rs` (original definition)
- LSP automatically follows re-exports
- No difference in navigation experience for user

### Pattern: Module-Level Re-Exports

**Example from ggen-domain:**
```rust
// In crates/ggen-domain/src/lib.rs
pub mod ai;
pub mod audit;
// Re-export audit types
pub use audit::security::{
    ConfigAuditor, ConfigIssue, SecurityScanner, Vulnerability,
};

// In crates/ggen-domain/src/audit/security.rs
pub struct SecurityScanner { /* ... */ }
```

**Navigation Behavior:**
- Cursor on `SecurityScanner` → Goes to `audit/security.rs`
- Re-export is transparent to user
- Multiple re-export levels handled correctly

### Best Practices

1. **For finding re-export sources:**
   - Use `findReferences` to see where symbol is re-exported
   - Check `lib.rs` files for `pub use` statements

2. **For ambiguous names:**
   - If symbol exists in multiple modules, LSP shows all locations
   - Use fully qualified path (e.g., `ggen_core::Graph`) to disambiguate

3. **For convenience re-exports:**
   - Prefer re-exports at crate root (`lib.rs`)
   - Makes navigation transparent for users
   - Follow ggen pattern: `pub use module::{Type1, Type2};`

---

## 4. Cursor Positioning for Accurate Results

### Pattern: Identifiers vs Keywords

**Key Finding:** Cursor must be on the **identifier**, not surrounding syntax.

**Examples:**

| Pattern | Correct Position | Incorrect Position |
|---------|------------------|-------------------|
| Function call | `my_function` (on name) | `(` or `)` (parentheses) |
| Struct construction | `MyStruct` (on name) | `{` or `}` (braces) |
| Trait method | `method_name` (on name) | `self` keyword |
| Macro invocation | `macro_name` (on name) | `!` operator |

### Pattern: Generic Types

**Example from ggen:**
```rust
pub struct Graph<T> {
    phantom: PhantomData<T>,
}

let g = Graph::<String>::new();
```

**Navigation Behavior:**
- Cursor on `Graph` → Navigates to struct definition
- Cursor on `String` → Navigates to `std::string::String` definition
- Cursor on `PhantomData` → Navigates to `std::marker::PhantomData`

**Best Practice:**
- Position cursor on type name, not `<` or `>` brackets
- For nested generics, cursor on specific type you want to navigate to

### Pattern: Method Calls on Chains

**Example:**
```rust
registry
    .validate_input(tool_name, &arguments)?
    .map(|result| ok(result))
```

**Navigation Behavior:**
- Cursor on `validate_input` → Navigates to trait method or impl
- Cursor on `map` → Navigates to `Option::map` or `Result::map`
- LSP correctly infers type from chain context

**Best Practice:**
- Cursor anywhere on method name works
- First character of method name is safest

---

## 5. Complex Trait/Impl Patterns

### Pattern: Trait Bounds and Where Clauses

**Example from ggen:**
```rust
pub trait ProposalMiningStrategy: Send + Sync {
    fn mine_proposals(&self, graph: &Graph) -> Vec<Proposal>;
}

pub struct GraphMiner {
    strategies: Vec<Box<dyn ProposalMiningStrategy>>,
}
```

**Navigation Behavior:**
- Cursor on `ProposalMiningStrategy` → Navigates to trait definition
- Cursor on `Send` or `Sync` → Navigates to `std::marker::Send` / `std::marker::Sync`
- Cursor on `Graph` → Navigates to Graph struct definition

### Pattern: Async Traits (`async_trait`)

**Example from ggen:**
```rust
#[async_trait::async_trait]
pub trait LlmClient: Send + Sync {
    async fn complete(&self, prompt: &str) -> Result<LlmResponse>;
}

#[async_trait::async_trait]
impl LlmClient for GenAiClient {
    async fn complete(&self, prompt: &str) -> Result<LlmResponse> {
        // Implementation
    }
}
```

**Navigation Behavior:**
- Cursor on `complete` in usage → Trait definition (line with `async fn complete`)
- Use `goToImplementation` → Jump to `impl LlmClient for GenAiClient`
- `async_trait` macro does not interfere with navigation

### Pattern: Blanket Implementations

**Example:**
```rust
// Trait definition
pub trait MaskApiKey {
    fn mask(&self) -> String;
}

// Blanket impl for all types that implement Display
impl<T: std::fmt::Display> MaskApiKey for T {
    fn mask(&self) -> Self {
        // Implementation
    }
}
```

**Navigation Behavior:**
- Cursor on `mask` method call → Shows trait definition
- `goToImplementation` → Shows blanket impl
- LSP correctly resolves generic blanket impls

---

## 6. Re-Export Chains and Deep Navigation

### Pattern: Multi-Level Re-Exports

**Example from ggen:**
```rust
// crates/ggen-core/src/lib.rs
pub use ontology::{
    OntologyExtractor, OntologyResult, OntologyStats,
};

// crates/ggen-core/src/ontology/mod.rs
pub use extractor::{
    OntologyExtractor, OntologyResult, OntologyStats,
};

// crates/ggen-core/src/ontology/extractor.rs
pub struct OntologyExtractor { /* ... */ }
```

**Navigation Behavior:**
- Cursor on `OntologyExtractor` → Navigates directly to `extractor.rs`
- All intermediate re-exports are transparent
- No difference between direct and re-exported navigation

### Pattern: Prelude Re-Exports

**Example from ggen:**
```rust
// crates/ggen-cli/src/prelude.rs
pub use ggen_domain::prelude::*;
pub use ggen_utils::error::Result;

// crates/ggen-domain/src/lib.rs
pub mod prelude;
```

**Navigation Behavior:**
- Symbols from prelude navigate to original definitions
- Prepend `prelude::` if needed for disambiguation
- Use `workspaceSymbol` to find symbols in preludes

---

## 7. Best Practices for Reliable Navigation

### 7.1 Project Structure

**Recommendations from ggen codebase:**

1. **Organize modules hierarchically:**
   ```
   crates/
     ggen-core/
       src/
         lib.rs         # Crate root with re-exports
         ontology/
           mod.rs       # Module re-exports
           extractor.rs # Concrete definitions
   ```

2. **Use consistent re-export patterns:**
   ```rust
   // In lib.rs
   pub use module::{Type1, Type2, Type3};

   // NOT:
   pub use module::Type1;
   pub use module::Type2;
   pub use module::Type3;
   ```

3. **Avoid circular re-exports:**
   - Module A re-exports from Module B
   - Module B re-exports from Module A
   - LSP can handle but confusing for developers

### 7.2 Naming Conventions

**From ggen patterns:**

1. **Trait naming:**
   - Use descriptive trait names: `MCPToolServer`, `LlmClient`
   - Avoid generic names like `Handler`, `Processor` (too many matches)

2. **Impl naming:**
   - Use `TraitNameImpl` or `TraitName` for implementing structs
   - Example: `MCPToolServer` (trait), `MCPToolServerImpl` (struct)

3. **Re-export organization:**
   - Group by module: `pub use lifecycle::{/*...*/};`
   - Document intent: `// Re-export for convenience`

### 7.3 LSP Configuration

**Recommended LSP settings for Rust:**

```json
{
  "rust-analyzer": {
    "assist.importMergeBehavior": "last",
    "cargo.loadOutDirsFromCheck": true,
    "procMacro.enable": true,
    "rustc.source": "discover"
  }
}
```

**Key settings:**
- `procMacro.enable`: Required for procedural macro navigation
- `cargo.loadOutDirsFromCheck`: Improves navigation accuracy
- `rustc.source`: Enables stdlib navigation

---

## 8. Troubleshooting Navigation Issues

### Issue 1: "Go to definition doesn't work"

**Causes:**
1. **Symbol not in source** (generated by macro)
   - Solution: Use `cargo expand` to see generated code
   - Navigation to macro definition is expected behavior

2. **Multiple definitions exist**
   - Solution: Use `workspaceSymbol` to see all matches
   - Use fully qualified path to disambiguate

3. **LSP not initialized**
   - Solution: Restart LSP server
   - Run `cargo check` to build dependencies

### Issue 2: "Goes to wrong definition"

**Causes:**
1. **Re-export chain confusion**
   - Solution: Check `findReferences` for all locations
   - Verify which definition you want (trait vs impl)

2. **Generic type confusion**
   - Solution: Position cursor on type name, not brackets
   - Use fully qualified path if needed

### Issue 3: "Can't find definition"

**Causes:**
1. **Symbol defined in dependency**
   - Solution: Ensure dependency is in `Cargo.toml`
   - Run `cargo build` to index dependencies

2. **Macro-generated symbol**
   - Solution: Navigate to macro definition instead
   - Use `cargo expand` to see generated code

---

## 9. Advanced LSP Operations

### 9.1 Finding All Implementations

**Operation:** `goToImplementation`

**Use case:** Find all `impl Trait for Type` blocks

**Example from ggen:**
```
Usage: server.list_tools()
↓ goToDefinition
Trait: MCPToolServer trait definition
↓ goToImplementation
Result:
  - impl MCPToolServer for MCPToolServerImpl
  - impl MCPToolServer for MockMCPToolServer (tests)
```

### 9.2 Finding All References

**Operation:** `findReferences`

**Use case:** Find all usages of a symbol

**Example:**
```
Symbol: ReadinessTracker
↓ findReferences
Result:
  - Definition: lifecycle.rs:42
  - Re-export: lib.rs:178
  - Usage 1: domain/state.rs:15
  - Usage 2: core/tests/lifecycle_test.rs:89
```

### 9.3 Finding All Symbols

**Operation:** `workspaceSymbol`

**Use case:** Find symbol by name across workspace

**Example:**
```
Query: "Graph"
↓ workspaceSymbol
Result:
  - ggen_core::graph::Graph
  - ggen_ai::generators::Graph
  - vendors::osiris::graph::Graph
```

### 9.4 Call Hierarchy

**Operations:** `prepareCallHierarchy`, `incomingCalls`, `outgoingCalls`

**Use case:** Trace who calls a method / what a method calls

**Example:**
```
Symbol: GenAiClient::complete()
↓ prepareCallHierarchy
↓ incomingCalls (who calls complete?)
Result:
  - ggen_cli::cmds::generate::run_template()
  - ggen_ai::swarm::agents::CodeGenerator::generate()
  - ggen_core::tests::llm_e2e_test::test_complete()

↓ outgoingCalls (what does complete call?)
Result:
  - genai::Client::chat()
  - tracing::info_span!()
  - std::time::Instant::now()
```

---

## 10. Summary of Navigation Patterns

### Trait Methods
| Action | LSP Operation | Result |
|--------|--------------|--------|
| Find trait definition | `goToDefinition` | Trait definition with method signature |
| Find implementations | `goToImplementation` | All `impl Trait for Type` blocks |
| Find all usages | `findReferences` | All call sites |

### Re-Exports
| Action | LSP Operation | Result |
|--------|--------------|--------|
| Navigate re-exported symbol | `goToDefinition` | Original definition (transparent) |
| Find re-export locations | `findReferences` | All `pub use` statements |
| Disambiguate symbols | `workspaceSymbol` | All definitions in workspace |

### Macros
| Action | LSP Operation | Result |
|--------|--------------|--------|
| Navigate macro | `goToDefinition` | `macro_rules!` or proc-macro function |
| See macro expansion | `cargo expand` | Expanded code (external tool) |
| Find macro uses | `findReferences` | All invocation sites |

### Generics
| Action | Cursor Position | Result |
|--------|----------------|--------|
| Navigate type | Type name | Type definition |
| Navigate trait bound | Trait name | Trait definition |
| Navigate generic param | Parameter name | Where it was declared |

---

## 11. Recommended Workflow

For precise go-to-definition navigation in complex Rust code:

1. **Start at usage site:**
   - Position cursor on identifier (function, type, method)
   - Use `goToDefinition`

2. **If trait method:**
   - First: Navigate to trait definition
   - Then: Use `goToImplementation` to find concrete impls
   - Alternative: Use `findReferences` to see all usages

3. **If re-exported symbol:**
   - Navigation goes directly to original definition
   - Use `findReferences` to see re-export chain

4. **If macro invocation:**
   - Navigate to macro definition
   - Use `cargo expand` to see generated code

5. **If multiple definitions:**
   - Use `workspaceSymbol` to see all matches
   - Use fully qualified path to disambiguate

6. **For call tracing:**
   - Use `prepareCallHierarchy` + `incomingCalls`
   - Use `prepareCallHierarchy` + `outgoingCalls`

---

## 12. Conclusion

**Key Takeaways:**

1. **LSP handles complexity well:** Traits, impls, re-exports, generics all navigate correctly
2. **Position matters:** Cursor must be on identifier, not syntax
3. **Two-level navigation for traits:** Definition → Implementation
4. **Macros are opaque:** Navigate to definition, not expansion (use `cargo expand`)
5. **Re-exports are transparent:** No difference in navigation experience
6. **Use LSP operations:** `goToImplementation`, `findReferences`, `workspaceSymbol`

**Rust's type system and LSP integration make navigation reliable even in complex codebases like ggen with 30 crates, trait hierarchies, and extensive macro usage.**

---

## References

- **LSP Specification:** https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/
- **rust-analyzer:** https://rust-analyzer.github.io/
- **ggen Codebase:** https://github.com/seanchatmangpt/ggen
- **Research Files:**
  - `/Users/sac/ggen/crates/ggen-core/src/lib.rs`
  - `/Users/sac/ggen/crates/ggen-ai/src/lib.rs`
  - `/Users/sac/ggen/crates/ggen-domain/src/lib.rs`
  - `/Users/sac/ggen/crates/ggen-macros/src/lib.rs`
  - `/Users/sac/ggen/crates/ggen-ai/src/mcp/traits.rs`
  - `/Users/sac/ggen/crates/ggen-workflow/src/patterns.rs`
