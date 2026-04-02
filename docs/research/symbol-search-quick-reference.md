# Symbol Search Quick Reference

**Workspace:** ggen v6.0.1 | **Date:** 2026-03-31

---

## 🚀 Quick Decision Tree

```
Need to find symbol?
│
├─ Don't know which crate?
│  └─ Use workspaceSymbol(query)
│
├─ Know the crate?
│  └─ Use documentSymbol(file)
│
├─ Need macro/test/attribute?
│  └─ Use grep(pattern)
│
└─ Find all implementations?
   └─ Use LSP findReferences
```

---

## 📊 Method Comparison

| Method | Command | Best For | Scope | Speed |
|--------|---------|----------|-------|-------|
| **workspaceSymbol** | `workspaceSymbol query` | Cross-crate discovery | All crates | 1-3s |
| **documentSymbol** | `documentSymbol file` | Crate exploration | Single file | <100ms |
| **Grep** | `grep pattern` | Pattern search | File system | <1s |
| **findReferences** | `LSP findReferences` | Find usages | All crates | 1-2s |

---

## 🔍 Query Patterns

### workspaceSymbol Queries

```rust
// ✅ Exact symbol name
workspaceSymbol query: "GenContext"        // Specific type
workspaceSymbol query: "cli_match"         // Specific function

// ✅ Prefix search
workspaceSymbol query: "validate"          // validate, validate_template, etc.
workspaceSymbol query: "Template"          // Template, TemplateEngine, etc.

// ✅ Crate-prefixed (disambiguates)
workspaceSymbol query: "ggen_core::Pipeline"       // ggen-core's Pipeline
workspaceSymbol query: "ggen_ai::Error"            // ggen-ai's Error

// ❌ Too generic
workspaceSymbol query: "new"               // Thousands of results
workspaceSymbol query: "Config"            // 30+ results
```

### documentSymbol Queries

```rust
// Explore crate exports
documentSymbol file: "crates/ggen-core/src/lib.rs"
documentSymbol file: "crates/ggen-cli/src/lib.rs"
documentSymbol file: "crates/ggen-ai/src/lib.rs"

// Deep dive into module
documentSymbol file: "crates/ggen-core/src/validation/mod.rs"
```

### Grep Queries

```rust
// Find patterns
grep pattern: "#\\[verb\\]"               // clap-noun-verb commands
grep pattern: "#\\[test\\]"               // test functions
grep pattern: "pub mod"                   // module declarations
grep pattern: "unsafe"                    // unsafe blocks

// Search in specific files
grep pattern: "version" glob: "**/Cargo.toml"
```

---

## 🎯 Common Workflows

### Find Unknown Symbol

```rust
// 1. Broad search
workspaceSymbol query: "SymbolName"

// 2. Narrow to crate
workspaceSymbol query: "crate_name::SymbolName"

// 3. Explore crate
documentSymbol file: "crates/crate_name/src/lib.rs"

// 4. Find usage
grep pattern: "SymbolName" glob: "**/crate_name/**/*.rs"
```

### Understand Crate API

```rust
// 1. See exports
documentSymbol file: "crates/ggen-core/src/lib.rs"

// 2. Explore modules
documentSymbol file: "crates/ggen-core/src/template/mod.rs"
documentSymbol file: "crates/ggen-core/src/validation/mod.rs"

// 3. Find symbols
workspaceSymbol query: "ggen_core::Pipeline"
```

### Find Trait Implementations

```rust
// 1. Find trait
workspaceSymbol query: "trait TraitName"

// 2. Find implementations
LSP operation: findReferences

// 3. Grep for impl blocks
grep pattern: "impl TraitName for"
```

---

## ⚠️ Common Pitfalls

### 1. Too Generic

```rust
// ❌ AVOID
workspaceSymbol query: "new"              // Thousands of results
workspaceSymbol query: "get"              // Too many getters
workspaceSymbol query: "run"              // Too many run functions

// ✅ USE
workspaceSymbol query: "GenContext::new"  // Specific constructor
workspaceSymbol query: "cli_match"        // Specific function
```

### 2. Wrong Case

```rust
// ❌ AVOID
workspaceSymbol query: "gencontext"       // Won't find CamelCase
workspaceSymbol query: "CliMatch"         // Won't find snake_case

// ✅ USE
workspaceSymbol query: "GenContext"       // CamelCase for types
workspaceSymbol query: "cli_match"        // snake_case for functions
```

### 3. Ignoring Crate Context

```rust
// ❌ AVOID
workspaceSymbol query: "Config"           // 30+ results

// ✅ USE
workspaceSymbol query: "ggen_ai::Config"  // Single crate
workspaceSymbol query: "LlmConfig"        // Domain-specific
```

---

## 📈 Performance Tips

### Large Workspaces (ggen: 6,880 files, 777K LOC)

```rust
// ✅ Narrow searches with crate prefixes
workspaceSymbol query: "ggen_core::"      // Faster than "Pipeline"

// ✅ Use specific symbol names
workspaceSymbol query: "GenContext"       // Faster than "Context"

// ❌ Avoid overly broad queries
workspaceSymbol query: "to"               // Too slow
workspaceSymbol query: "from"             // Too slow
```

---

## 🏗️ Symbol Naming Best Practices

```rust
// ✅ GOOD: Domain-specific
pub struct LlmConfig { }
pub struct GenContext { }
pub struct RdfSchema { }

// ❌ AVOID: Generic
pub struct Config { }              // Use LlmConfig instead
pub struct Context { }             // Use GenContext instead
pub struct Schema { }              // Use RdfSchema instead

// ✅ GOOD: Crate-prefixed (when generic needed)
pub struct GenAiConfig { }         // ggen-ai's config
pub struct CoreConfig { }          // ggen-core's config
```

---

## 🔗 Evidence from ggen v6.0.1

| Metric | Count |
|--------|-------|
| Total .rs files | 6,880 |
| Total LOC | ~777K |
| Crates | 30 |
| Public structs | 955 |
| Public functions | 502 |
| Public traits | 176 |
| Most duplicated name | `error` (31 crates) |
| Common modules | `types`, `config`, `metrics`, `validation` |

---

## 📚 Quick Links

- **Full Report:** [workspace-symbol-search-best-practices.md](./workspace-symbol-search-best-practices.md)
- **Workspace:** /Users/sac/ggen
- **Version:** ggen v6.0.1

---

**Version:** 1.0 | **Last Updated:** 2026-03-31
