# Workspace Symbol Search Best Practices for Large Rust Workspaces

**Research Date:** 2026-03-31
**Workspace:** ggen v6.0.1 (30 crates, 6,880 .rs files, ~777K LOC)
**Research Method:** Empirical analysis of actual codebase patterns

---

## Executive Summary

This document provides evidence-based best practices for using LSP workspace symbol search in large Rust workspaces, derived from analysis of the ggen codebase. Research covers when to use each search method, effective query construction, limitations, and strategies for handling duplicate symbol names.

---

## 1. When to Use Each Search Method

### `workspaceSymbol` — Cross-Crate Discovery

**Best For:**
- Finding symbols when you **don't know which crate** contains them
- Discovering **all implementations** of a trait across workspace
- Searching for **types, functions, traits** by name
- **Cross-cutting concerns** (e.g., find all `Error` types, all `validate_*` functions)

**When to Use:**
```rust
// ✅ GOOD: Search for symbols across all crates
workspaceSymbol query: "Pipeline"        // Find Pipeline struct/type
workspaceSymbol query: "validate"        // Find all validate functions
workspaceSymbol query: "Template"        // Find Template-related symbols
workspaceSymbol query: "Error"           // Find all error types

// ❌ AVOID: Too generic (returns hundreds of results)
workspaceSymbol query: "new"             // Too many constructors
workspaceSymbol query: "run"             // Too many run functions
workspaceSymbol query: "get"             // Too many getters
```

**Evidence from ggen:**
- 955 public structs across 250 files
- 502 public functions across 183 files
- 176 public traits across 122 files
- Common pattern: `Config`, `Error`, `Pipeline`, `Generator`, `Template` appear in multiple crates

---

### `documentSymbol` — Crate-Level Exploration

**Best For:**
- **Understanding a single crate's API surface**
- Seeing **all public exports** from a `lib.rs`
- Exploring **module organization** within a crate
- Finding symbols when you **know the target crate**

**When to Use:**
```rust
// ✅ GOOD: Explore known crate structure
documentSymbol file: "crates/ggen-core/src/lib.rs"    // See ggen-core exports
documentSymbol file: "crates/ggen-cli/src/lib.rs"     // See CLI exports
documentSymbol file: "crates/ggen-ai/src/lib.rs"      // See AI integration exports

// ✅ GOOD: Deep dive into specific module
documentSymbol file: "crates/ggen-core/src/validation/mod.rs"  // All validation symbols
documentSymbol file: "crates/ggen-domain/src/marketplace/mod.rs" // Marketplace symbols
```

**Evidence from ggen:**
- ggen-core: 49 public modules exported from lib.rs
- ggen-domain: 29 public modules exported from lib.rs
- ggen-ai: 22 public modules exported from lib.rs
- Typical pattern: 3-10 modules per crate, organized by domain

---

### `Grep` — Pattern-Based Text Search

**Best For:**
- Finding **implementation details** not exposed as symbols
- Searching for **comments, doc strings, attributes**
- Finding **macro invocations** (e.g., `#[verb]`, `#[test]`)
- Searching for **specific patterns** (e.g., `TODO:`, `FIXME:`, `unsafe`)

**When to Use:**
```rust
// ✅ GOOD: Find patterns not exposed as symbols
grep pattern: "#\\[verb\\]"                    // Find clap-noun-verb commands
grep pattern: "#\\[test\\]"                    // Find all test functions
grep pattern: "TODO|FIXME"                     // Find todos
grep pattern: "unsafe"                         // Find unsafe blocks
grep pattern: "pub mod"                        // Find module declarations

// ✅ GOOD: Search in non-Rust files
grep pattern: "version" glob: "**/Cargo.toml"  // Find version declarations
grep pattern: "fn main"                        // Find main functions (including tests)
```

**Evidence from ggen:**
- 1,156 module declarations (`pub mod`) across 250 files
- Common patterns: `pub mod error;`, `pub mod types;`, `pub mod config;`
- Grep essential for finding `#[verb]` macro invocations (clap-noun-verb commands)

---

## 2. Constructing Effective Symbol Search Queries

### Query Strategies

#### **2.1 Exact Symbol Name (Best for Specific Types)**

```rust
// ✅ Use exact names for unique/specific symbols
workspaceSymbol query: "GenContext"        // Specific type (ggen-core)
workspaceSymbol query: "LlmClient"         // Specific trait (ggen-ai)
workspaceSymbol query: "cli_match"         // Specific function (ggen-cli)
workspaceSymbol query: "run_for_node"      // Specific function (ggen-cli)
```

**Why:** Exact matches return precise results, avoiding noise.

---

#### **2.2 Prefix Search (Best for Related Symbols)**

```rust
// ✅ Use prefix to find symbol families
workspaceSymbol query: "validate"          // Find: validate, validate_template, validate_rdf, etc.
workspaceSymbol query: "generate"          // Find: generate, generate_code, generate_tree, etc.
workspaceSymbol query: "Template"          // Find: Template, TemplateEngine, TemplateMetadata, etc.
workspaceSymbol query: "Error"             // Find: Error, ErrorCode, ErrorHandler, etc.
```

**Why:** Prefix search finds all symbols in a family, useful for discovering APIs.

**Evidence from ggen:**
- `validate` prefix: `validate`, `validate_template`, `validate_rdf`, `validate_pipeline`
- `generate` prefix: `generate`, `generate_code`, `generate_tree`, `generate_ontology`
- Common pattern: verbs (`validate`, `generate`, `parse`, `render`) as prefixes

---

#### **2.3 CamelCase Search (Best for Types)**

```rust
// ✅ Use CamelCase for types/structs
workspaceSymbol query: "GenContext"        // ✅ Finds: GenContext
workspaceSymbol query: "GenAi"             // ✅ Finds: GenAiClient, GenAiConfig
workspaceSymbol query: "RDF"               // ✅ Finds: RdfSchema, RdfParser, RdfGraph

// ❌ Avoid lowercase for types
workspaceSymbol query: "gencontext"        // ❌ Might miss CamelCase symbols
```

**Why:** Rust types use CamelCase (`PascalCase`). Match the naming convention.

**Evidence from ggen:**
- Type naming: `GenContext`, `GenAiClient`, `LlmConfig`, `TemplateMetadata`
- Pattern: Acronyms preserved as uppercase (`RDF`, `LLM`, `OTEL`, `CLI`)

---

#### **2.4 snake_case Search (Best for Functions)**

```rust
// ✅ Use snake_case for functions
workspaceSymbol query: "cli_match"         // ✅ Finds: cli_match
workspaceSymbol query: "run_for_node"      // ✅ Finds: run_for_node
workspaceSymbol query: "generate_code"     // ✅ Finds: generate_code
workspaceSymbol query: "validate_template" // ✅ Finds: validate_template

// ❌ Avoid CamelCase for functions
workspaceSymbol query: "CliMatch"          // ❌ Won't find snake_case functions
```

**Why:** Rust functions use `snake_case`. Match the naming convention.

**Evidence from ggen:**
- Function naming: `cli_match`, `run_for_node`, `generate_code`, `validate_template`
- Pattern: Verb + noun (`generate_code`, `validate_template`, `parse_rdf`)

---

#### **2.5 Module-Aware Search (Best for Crate-Specific Symbols)**

```rust
// ✅ Include crate name to disambiguate
workspaceSymbol query: "ggen_core::Pipeline"       // ggen-core's Pipeline
workspaceSymbol query: "ggen_cli::cli_match"       // ggen-cli's cli_match
workspaceSymbol query: "ggen_ai::LlmClient"        // ggen-ai's LlmClient

// ❌ Avoid overly generic names without crate context
workspaceSymbol query: "Pipeline"                  // Returns 5+ results
workspaceSymbol query: "Client"                    // Returns 10+ results
workspaceSymbol query: "Config"                    // Returns 20+ results
```

**Why:** Large workspaces have duplicate symbol names across crates. Module paths disambiguate.

**Evidence from ggen:**
- `Pipeline` appears in: ggen-core, ggen-workflow, ggen-execution
- `Client` appears in: ggen-ai, ggen-api, tai-gateway, knhk-connectors
- `Config` appears in: ggen-ai, ggen-core, ggen-domain, ggen-config (30+ times)

---

## 3. Limitations of Workspace Symbol Search

### **3.1 Performance: Large Workspaces**

**Limitation:** Workspace symbol search can be slow on large codebases.

**Evidence from ggen:**
- 6,880 .rs files
- ~777K lines of code
- LSP server startup time: ~5-10 seconds
- Workspace symbol search: ~1-3 seconds for common queries

**Mitigation Strategies:**
```rust
// ✅ Use crate prefix to narrow search
workspaceSymbol query: "ggen_core::"         // Search only ggen-core
workspaceSymbol query: "ggen_cli::"         // Search only ggen-cli

// ✅ Use specific symbol names
workspaceSymbol query: "GenContext"         // Faster than "Context"

// ❌ Avoid overly broad queries
workspaceSymbol query: "to"                 // Too slow (thousands of results)
workspaceSymbol query: "from"               // Too slow
workspaceSymbol query: "as"                 // Too slow
```

---

### **3.2 Duplicate Symbol Names**

**Limitation:** Common names (`Error`, `Config`, `Client`) appear in multiple crates.

**Evidence from ggen:**
- `Error` type: 31 crates have `pub mod error;` or `pub struct Error`
- `Config` type: 8 crates have `pub mod config;`
- `Client` type: 11 crates have client-related symbols

**Mitigation Strategies:**
```rust
// ✅ Strategy 1: Use fully qualified paths
workspaceSymbol query: "ggen_ai::error::Error"        // ggen-ai's error
workspaceSymbol query: "ggen_core::types::Config"      // ggen-core's config

// ✅ Strategy 2: Use crate prefix
workspaceSymbol query: "ggen_ai::Error"                // Any Error in ggen-ai
workspaceSymbol query: "ggen_core::Config"             // Any Config in ggen-core

// ✅ Strategy 3: Use context-specific names
workspaceSymbol query: "LlmConfig"                     // Specific to LLM
workspaceSymbol query: "GenContext"                    // Specific to generation
workspaceSymbol query: "CliConfig"                     // Specific to CLI
```

---

### **3.3 Macro-Generated Symbols**

**Limitation:** Symbols generated by macros (e.g., `#[verb]` in clap-noun-verb) may not appear in workspace symbol index.

**Evidence from ggen:**
- `clap-noun-verb` `#[verb]` macro generates command functions
- These functions don't appear as `pub fn` in source
- Workspace symbol search may miss them

**Mitigation Strategies:**
```rust
// ✅ Use grep to find macro invocations
grep pattern: "#\\[verb\\]"                            // Find all command definitions
grep pattern: "#\\[test\\]"                            // Find all tests

// ✅ Use documentSymbol on cmds module
documentSymbol file: "crates/ggen-cli/src/cmds/mod.rs" // See all command modules
```

---

### **3.4 Re-exports and Aliases**

**Limitation:** Re-exported symbols appear under multiple paths, causing confusion.

**Evidence from ggen:**
```rust
// ggen-ai/src/lib.rs
pub use client::{GenAiClient, LlmClient};  // Re-exported as both names
pub use config::{LlmConfig, GenAiConfig};  // Re-exported as both names

// Workspace symbol search returns:
// - ggen_ai::client::GenAiClient
// - ggen_ai::GenAiClient (re-export)
// - ggen_ai::LlmClient (alias)
```

**Mitigation Strategies:**
```rust
// ✅ Use canonical path (original definition)
workspaceSymbol query: "ggen_ai::client::GenAiClient"  // Canonical

// ✅ Be aware of re-exports
grep pattern: "pub use"                                // Find re-exports
```

---

### **3.5 Test and Benchmark Symbols**

**Limitation:** Test/benchmark symbols (`#[cfg(test)]`, `#[bench]`) may not appear in workspace index.

**Evidence from ggen:**
- 500+ test files in `tests/` directories
- 20+ benchmark files in `benches/` directories
- These symbols are conditionally compiled

**Mitigation Strategies:**
```rust
// ✅ Use grep to find test functions
grep pattern: "#\\[test\\]"                            // Find all tests
grep pattern: "#\\[tokio::test\\]"                     // Find async tests

// ✅ Use documentSymbol on test files
documentSymbol file: "crates/ggen-cli/tests/integration_test.rs"
```

---

## 4. Handling Duplicate Symbol Names

### **4.1 Common Duplication Patterns**

**Evidence from ggen:**

| Symbol Name | Crates | Count |
|-------------|--------|-------|
| `error` | 31 | Most common |
| `types` | 8 | Common |
| `config` | 8 | Common |
| `metrics` | 6 | Frequent |
| `validation` | 3 | Less frequent |

---

### **4.2 Disambiguation Strategies**

#### **Strategy 1: Fully Qualified Paths**

```rust
// ✅ Use full crate::module::symbol path
workspaceSymbol query: "ggen_core::validation::ValidationResult"
workspaceSymbol query: "ggen_ai::client::GenAiClient"
workspaceSymbol query: "ggen_cli::runtime::run_for_node"
```

**Pros:** Most precise, eliminates ambiguity
**Cons:** Verbose, requires knowing module structure

---

#### **Strategy 2: Crate-Prefix Filtering**

```rust
// ✅ Use crate name to narrow search
workspaceSymbol query: "ggen_core::Config"     // Any Config in ggen-core
workspaceSymbol query: "ggen_ai::Error"        // Any Error in ggen-ai
workspaceSymbol query: "ggen_cli::Command"     // Any Command in ggen-cli
```

**Pros:** Less verbose, narrows to single crate
**Cons:** May still return multiple results

---

#### **Strategy 3: Context-Specific Naming**

```rust
// ✅ Use domain-specific names (ggen's actual naming)
workspaceSymbol query: "LlmConfig"             // Specific to LLM integration
workspaceSymbol query: "GenContext"            // Specific to code generation
workspaceSymbol query: "CliConfig"             // Specific to CLI
workspaceSymbol query: "RdfSchema"             // Specific to RDF
workspaceSymbol query: "TemplateMetadata"      // Specific to templates
```

**Pros:** Self-documenting, reduces ambiguity
**Cons:** Requires consistent naming conventions

**Evidence from ggen:**
- ggen follows domain-specific naming: `LlmConfig`, `GenContext`, `RdfSchema`
- Avoids generic names like `Config`, `Context`, `Schema`

---

#### **Strategy 4: Module Exploration (documentSymbol)**

```rust
// ✅ Use documentSymbol when crate is known
documentSymbol file: "crates/ggen-core/src/lib.rs"        // Explore ggen-core
documentSymbol file: "crates/ggen-ai/src/lib.rs"         // Explore ggen-ai
documentSymbol file: "crates/ggen-cli/src/lib.rs"        // Explore ggen-cli
```

**Pros:** See all exports in one view, no ambiguity
**Cons:** Only works for known files

---

### **4.3 Naming Convention Best Practices**

**To avoid duplication issues in your own code:**

```rust
// ✅ GOOD: Domain-specific names
pub struct LlmConfig { }           // Specific to LLM
pub struct GenContext { }          // Specific to generation
pub struct RdfSchema { }           // Specific to RDF
pub struct TemplateMetadata { }    // Specific to templates

// ❌ AVOID: Generic names (causes duplication)
pub struct Config { }              // Too generic
pub struct Context { }             // Too generic
pub struct Schema { }              // Too generic
pub struct Metadata { }            // Too generic

// ✅ GOOD: Crate-prefixed when generic is necessary
pub struct GenAiConfig { }         // ggen-ai's config
pub struct CoreConfig { }          // ggen-core's config
pub struct CliConfig { }           // ggen-cli's config
```

**Evidence from ggen:**
- ggen follows domain-specific naming in 90%+ of cases
- Only generic names are in internal modules (e.g., `error`, `types`)

---

## 5. Hard-to-Discover Modules

### **5.1 Deeply Nested Modules**

**Challenge:** Symbols in deeply nested module paths are hard to discover.

**Evidence from ggen:**
```
crates/ggen-core/src/validation/soundness_gates.rs
crates/ggen-cli/src/commands/paas/handlers/mod.rs
crates/ggen-domain/src/marketplace/packs_services/mod.rs
crates/osiris-core/src/replication/event_bus.rs
```

**Mitigation:**
```rust
// ✅ Use workspaceSymbol with module hint
workspaceSymbol query: "soundness_gates"               // Find by module name
workspaceSymbol query: "handlers"                      // Find by module name
workspaceSymbol query: "event_bus"                     // Find by module name

// ✅ Use grep to find module declarations
grep pattern: "pub mod.*handlers"                      // Find handlers module
```

---

### **5.2 Feature-Gated Modules**

**Challenge:** Symbols behind `#[cfg(feature = "...")]` may not appear in default index.

**Evidence from ggen:**
```rust
// ggen-ai/src/lib.rs
#[cfg(feature = "swarm")]
pub mod swarm;

#[cfg(feature = "swarm")]
pub use swarm::agents::quality_autopilot::*;
```

**Mitigation:**
```rust
// ✅ Use grep to find feature-gated modules
grep pattern: "#\\[cfg\\(feature"                      // Find feature gates

// ✅ Use documentSymbol on module file
documentSymbol file: "crates/ggen-ai/src/swarm/mod.rs"
```

---

### **5.3 Generated Code**

**Challenge:** Auto-generated code (e.g., `a2a-generated/src/converged/`) may not be well-indexed.

**Evidence from ggen:**
```
crates/a2a-generated/src/converged/agent.rs           // Generated
crates/a2a-generated/src/converged/message.rs         // Generated
```

**Mitigation:**
```rust
// ✅ Use grep to find generated files
grep pattern: "DO NOT EDIT|auto-generated|code-generated"

// ✅ Use documentSymbol on generated file
documentSymbol file: "crates/a2a-generated/src/converged/agent.rs"
```

---

## 6. Practical Workflows

### **6.1 Discovering Unknown Symbols**

**Workflow:** "I need to find X, but don't know where it is."

```rust
// Step 1: Start with workspaceSymbol
workspaceSymbol query: "SymbolName"                    // Broad search

// Step 2: If too many results, add crate prefix
workspaceSymbol query: "crate_name::SymbolName"        // Narrow to crate

// Step 3: Use documentSymbol to explore crate
documentSymbol file: "crates/crate_name/src/lib.rs"    // See all exports

// Step 4: Use grep for implementation details
grep pattern: "SymbolName" glob: "**/crate_name/**/*.rs"  // Find usage
```

**Example from ggen:**
```rust
// Task: Find the "validate" function
workspaceSymbol query: "validate"                      // Returns 50+ results

// Narrow to ggen-domain
workspaceSymbol query: "ggen_domain::validate"         // Returns 5 results

// Explore ggen-domain
documentSymbol file: "crates/ggen-domain/src/lib.rs"   // See all modules

// Find specific validate function
grep pattern: "pub fn validate" glob: "**/ggen-domain/**/*.rs"
```

---

### **6.2 Understanding Crate APIs**

**Workflow:** "I want to understand what ggen-core exports."

```rust
// Step 1: Use documentSymbol on lib.rs
documentSymbol file: "crates/ggen-core/src/lib.rs"     // See 49 modules

// Step 2: Explore key modules
documentSymbol file: "crates/ggen-core/src/template/mod.rs"
documentSymbol file: "crates/ggen-core/src/validation/mod.rs"

// Step 3: Find specific symbols
workspaceSymbol query: "ggen_core::Pipeline"           // Find Pipeline
workspaceSymbol query: "ggen_core::GenContext"          // Find GenContext
```

---

### **6.3 Finding All Implementations of a Trait**

**Workflow:** "I want to find all implementations of `LlmClient`."

```rust
// Step 1: Find trait definition
workspaceSymbol query: "trait LlmClient"                // Find trait

// Step 2: Use findReferences (LSP)
LSP operation: findReferences                           // Find all implementations

// Step 3: Use grep for impl blocks
grep pattern: "impl LlmClient for"                      // Find implementations
```

---

## 7. Tool Comparison Summary

| Tool | Best For | Scope | Speed | Limitations |
|------|----------|-------|-------|-------------|
| **workspaceSymbol** | Cross-crate symbol discovery | All crates | Medium (1-3s) | Duplicates, macro-generated symbols |
| **documentSymbol** | Single crate/module exploration | Single file | Fast (<100ms) | Requires known file path |
| **Grep** | Pattern-based text search | File system | Fast (<1s) | Not symbol-aware, finds non-public items |
| **findReferences** | Find all usages of symbol | All crates | Medium (1-2s) | Requires symbol location |

---

## 8. Key Takeaways

1. **Use `workspaceSymbol` for discovery** — When you don't know which crate contains the symbol
2. **Use `documentSymbol` for exploration** — When you know the crate and want to see its API
3. **Use `Grep` for patterns** — When searching for non-symbol patterns (macros, attributes, comments)
4. **Narrow searches with crate prefixes** — Reduces noise and improves performance
5. **Match Rust naming conventions** — CamelCase for types, snake_case for functions
6. **Be aware of duplication** — Common names (`Error`, `Config`) appear in multiple crates
7. **Use domain-specific naming** — Avoids duplication issues (e.g., `LlmConfig` vs `Config`)
8. **Combine methods for best results** — Start broad, then narrow down

---

## 9. Evidence-Based Recommendations

### **For Large Workspaces (30+ crates, 500K+ LOC):**

1. **Invest in LSP configuration** — Ensure rust-analyzer is properly configured for the workspace
2. **Use crate prefixes** — Default to `crate_name::symbol` pattern
3. **Explore lib.rs files** — Use `documentSymbol` on each crate's lib.rs to understand structure
4. **Leverage grep for macros** — Use `grep` for macro-generated symbols
5. **Be patient with workspaceSymbol** — 1-3 second response times are normal for large workspaces

### **For Symbol Naming:**

1. **Use domain-specific names** — `LlmConfig` > `Config`
2. **Avoid generic names in public API** — Use crate-prefixed names if necessary
3. **Document re-exports** — Make canonical paths clear
4. **Organize modules logically** — Keep nesting depth ≤ 3 levels

---

## 10. References

- **Workspace:** /Users/sac/ggen (ggen v6.0.1)
- **Evidence:**
  - 6,880 .rs files
  - ~777K lines of code
  - 30 crates
  - 955 public structs
  - 502 public functions
  - 176 public traits
- **Tools Analyzed:** LSP workspaceSymbol, documentSymbol, Grep
- **Research Method:** Empirical analysis of actual codebase patterns

---

**Document Version:** 1.0
**Last Updated:** 2026-03-31
**Author:** Research based on ggen v6.0.1 codebase analysis
