<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Clap + ggen.toml Integration - Module Structure](#clap--ggentoml-integration---module-structure)
  - [Module Hierarchy](#module-hierarchy)
  - [Module Dependencies](#module-dependencies)
    - [Dependency Graph](#dependency-graph)
    - [Build Order](#build-order)
  - [Crate Details](#crate-details)
    - [ggen-cli-derive (NEW)](#ggen-cli-derive-new)
    - [ggen-config (EXTENDED)](#ggen-config-extended)
    - [ggen-cli (MODIFIED)](#ggen-cli-modified)
  - [Data Flow Between Modules](#data-flow-between-modules)
    - [Compile-Time Flow](#compile-time-flow)
    - [Runtime Flow](#runtime-flow)
  - [Public APIs](#public-apis)
    - [ggen-cli-derive](#ggen-cli-derive)
    - [ggen-config](#ggen-config)
    - [ggen-cli validation](#ggen-cli-validation)
  - [Testing Strategy](#testing-strategy)
    - [Unit Tests (Per Crate)](#unit-tests-per-crate)
    - [Integration Tests (Cross-Crate)](#integration-tests-cross-crate)
    - [Property-Based Tests](#property-based-tests)
  - [Build Configuration](#build-configuration)
    - [Workspace Cargo.toml Updates](#workspace-cargotoml-updates)
    - [ggen-cli-derive/Cargo.toml](#ggen-cli-derivecargotoml)
  - [Documentation Structure](#documentation-structure)
  - [Feature Flags](#feature-flags)
    - [ggen-cli-derive](#ggen-cli-derive-1)
    - [ggen-config](#ggen-config-1)
    - [ggen-cli](#ggen-cli)
  - [Performance Characteristics](#performance-characteristics)
    - [Compile-Time](#compile-time)
    - [Runtime](#runtime)
  - [Migration Path (Existing Code)](#migration-path-existing-code)
    - [Phase 1: Add Dependencies](#phase-1-add-dependencies)
    - [Phase 2: Annotate Existing Structs](#phase-2-annotate-existing-structs)
    - [Phase 3: Update main.rs](#phase-3-update-mainrs)
    - [Phase 4: Add Validation](#phase-4-add-validation)
  - [Backwards Compatibility](#backwards-compatibility)
    - [Support for Non-Macro Usage](#support-for-non-macro-usage)
    - [Gradual Adoption](#gradual-adoption)
  - [Error Handling Across Modules](#error-handling-across-modules)
    - [Error Types](#error-types)
    - [Error Conversion](#error-conversion)
  - [Continuous Integration](#continuous-integration)
    - [CI Pipeline](#ci-pipeline)
  - [Success Metrics](#success-metrics)
    - [Technical Metrics](#technical-metrics)
    - [Quality Metrics](#quality-metrics)
    - [User Experience Metrics](#user-experience-metrics)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Clap + ggen.toml Integration - Module Structure

**System Architect**: Claude (Hive Mind Swarm Phase 2)
**Date**: 2025-11-19
**Version**: 1.0.0
**Status**: Module Architecture

---

## Module Hierarchy

```
ggen/ (workspace root)
├── crates/
│   ├── ggen-cli-derive/           ⭐ NEW: Procedural macros
│   │   ├── Cargo.toml
│   │   ├── README.md
│   │   ├── src/
│   │   │   ├── lib.rs             # #[ggen] macro entry point
│   │   │   ├── attr.rs            # Attribute parsing (darling)
│   │   │   ├── config.rs          # TOML loading & validation
│   │   │   ├── ir.rs              # Intermediate representation
│   │   │   ├── generator.rs       # Code generation (quote)
│   │   │   ├── validator.rs       # Schema validation
│   │   │   └── error.rs           # Error types
│   │   ├── tests/
│   │   │   ├── macro_tests.rs     # Basic macro tests
│   │   │   ├── integration/       # Integration tests
│   │   │   └── fixtures/          # Test TOML configs
│   │   └── examples/
│   │       └── basic_usage.rs
│   │
│   ├── ggen-config/               🔄 EXTENDED: Add clap integration
│   │   ├── Cargo.toml
│   │   ├── src/
│   │   │   ├── lib.rs             # Re-export main modules
│   │   │   ├── loader.rs          # Config file loading
│   │   │   ├── merger.rs          # Multi-source config merging
│   │   │   ├── clap.rs            ⭐ NEW: Clap-specific integration
│   │   │   │   ├── mod.rs
│   │   │   │   ├── loader.rs      # ConfigLoader for clap
│   │   │   │   ├── discovery.rs   # ggen.toml discovery
│   │   │   │   ├── precedence.rs  # CLI > Env > Config > Default
│   │   │   │   └── env_expand.rs  # ${VAR} expansion
│   │   │   └── validator.rs       # Runtime validation
│   │   └── tests/
│   │       ├── clap_tests.rs      # Clap integration tests
│   │       └── fixtures/
│   │
│   ├── ggen-cli/                  🔄 MODIFIED: Use new macro & validation
│   │   ├── Cargo.toml             # Add ggen-cli-derive dependency
│   │   ├── src/
│   │   │   ├── main.rs            # Use ConfigLoader::load()
│   │   │   ├── cli.rs             # Root CLI struct
│   │   │   ├── commands/
│   │   │   │   ├── mod.rs
│   │   │   │   ├── graph.rs       # #[ggen(config = "ggen.toml")]
│   │   │   │   ├── ontology.rs    # #[ggen(config = "ggen.toml")]
│   │   │   │   └── template.rs    # #[ggen(config = "ggen.toml")]
│   │   │   ├── validation/        ⭐ NEW: Noun-verb validation
│   │   │   │   ├── mod.rs
│   │   │   │   ├── middleware.rs  # ValidationMiddleware
│   │   │   │   ├── permissions.rs # PermissionValidator
│   │   │   │   ├── constitution.rs # Constitution checker
│   │   │   │   └── audit.rs       # AuditLogger
│   │   │   └── error.rs
│   │   └── tests/
│   │       └── cli_integration.rs
│   │
│   ├── ggen-core/                 🔄 EXTENDED: Add constitution support
│   │   ├── src/
│   │   │   ├── ontology/
│   │   │   │   ├── constitution.rs # Constitution model & checks
│   │   │   │   └── ...
│   │   │   └── ...
│   │   └── ...
│   │
│   └── ...
│
├── docs/
│   ├── clap-ggen-integration-design.md    # This document
│   ├── ggen-cli-macro-design.md           # Macro implementation
│   ├── noun-verb-validation-design.md     # Security & validation
│   └── clap-integration-module-structure.md # Module architecture
│
└── ggen.toml                       # Configuration file
```

---

## Module Dependencies

### Dependency Graph

```
┌─────────────────────────────────────────────────────────┐
│                     ggen-cli                            │
│  (binary crate - main CLI application)                 │
│                                                          │
│  Dependencies:                                           │
│  • ggen-cli-derive (proc-macro)                        │
│  • ggen-config (config loading)                        │
│  • ggen-core (constitution, ontology)                  │
│  • clap (CLI framework)                                │
│  • clap-noun-verb (noun-verb pattern)                  │
└────────────┬────────────────────────────────────────────┘
             │
             ├──────► ggen-cli-derive (proc-macro crate)
             │        └─► Dependencies:
             │            • syn, quote, proc-macro2
             │            • toml, serde
             │            • darling (attr parsing)
             │            • heck (case conversion)
             │
             ├──────► ggen-config (library crate)
             │        └─► Dependencies:
             │            • serde, toml
             │            • clap (for clap module)
             │            • ggen-core (validation)
             │
             └──────► ggen-core (library crate)
                      └─► Dependencies:
                          • oxigraph (SPARQL)
                          • shacl_validation
                          • serde, serde_json
```

### Build Order

```
Phase 1: Foundation
  1. ggen-core (no macro dependencies)

Phase 2: Macros & Config
  2. ggen-cli-derive (proc-macro)
  3. ggen-config (uses ggen-core)

Phase 3: Application
  4. ggen-cli (uses all above)
```

---

## Crate Details

### ggen-cli-derive (NEW)

**Type**: Procedural macro crate
**Purpose**: Auto-generate clap code from ggen.toml

```toml
[package]
name = "ggen-cli-derive"
version = "3.2.0"
edition = "2021"
license = "MIT"
description = "Procedural macros for ggen CLI generation from TOML"

[lib]
proc-macro = true

[dependencies]
syn = { version = "2.0", features = ["full", "extra-traits"] }
quote = "1.0"
proc-macro2 = "1.0"
proc-macro-error = "1.0"
serde = { version = "1.0", features = ["derive"] }
toml = "0.9"
heck = "0.5"
darling = "0.20"

[dev-dependencies]
trybuild = "1.0"
```

**Key APIs**:
```rust
#[proc_macro_attribute]
pub fn ggen(attr: TokenStream, item: TokenStream) -> TokenStream;

pub struct CommandIR { ... }
pub fn generate_clap_code(ir: &CommandIR) -> TokenStream;
```

---

### ggen-config (EXTENDED)

**Type**: Library crate
**Purpose**: Configuration loading, merging, validation

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
toml = "0.9"
serde_json = "1.0"
clap = { version = "4.5", features = ["derive"] }
ggen-core = { workspace = true }
anyhow = "1.0"
thiserror = "2.0"
regex = "1.12"
```

**New Module: `clap.rs`**:
```rust
pub mod clap {
    pub struct ConfigLoader;
    pub struct ConfigDiscovery;
    pub struct PrecedenceResolver;

    impl ConfigLoader {
        pub fn load<T: Parser + Deserialize>() -> Result<T>;
    }
}
```

---

### ggen-cli (MODIFIED)

**Type**: Binary crate
**Purpose**: Main CLI application

```toml
[dependencies]
ggen-cli-derive = { path = "../ggen-cli-derive", version = "3.2.0" }
ggen-config = { workspace = true }
ggen-core = { workspace = true }
clap = { workspace = true }
clap-noun-verb = { workspace = true }
clap-noun-verb-macros = { workspace = true }
tokio = { workspace = true }
anyhow = "1.0"
serde = { workspace = true }
```

**New Module: `validation/`**:
```rust
pub mod validation {
    pub mod middleware;      // ValidationMiddleware
    pub mod permissions;     // PermissionValidator
    pub mod constitution;    // Constitution checks
    pub mod audit;           // AuditLogger
}
```

---

## Data Flow Between Modules

### Compile-Time Flow

```
Developer writes ggen.toml
    │
    ▼
ggen-cli-derive macro reads TOML
    │
    ├─► Parse with toml crate
    ├─► Validate schema
    ├─► Generate IR
    └─► Generate clap code (quote)
    │
    ▼
Rust compiler type-checks generated code
    │
    ▼
Binary includes generated clap structs
```

### Runtime Flow

```
User runs: ggen graph query --sparql "SELECT * WHERE {?s ?p ?o}"
    │
    ▼
main.rs: ConfigLoader::load() (ggen-config)
    │
    ├─► Discover ggen.toml (ConfigDiscovery)
    ├─► Parse TOML (toml crate)
    ├─► Expand env vars (${VAR})
    ├─► Merge CLI args > Env > Config > Defaults
    └─► Return merged config
    │
    ▼
ValidationMiddleware::execute() (ggen-cli/validation)
    │
    ├─► PermissionValidator: Check IO operations
    ├─► Constitution: Check invariants (ggen-core)
    ├─► Execute command handler
    └─► AuditLogger: Record result
    │
    ▼
Command executes (graph query logic)
    │
    ▼
Result returned to user
```

---

## Public APIs

### ggen-cli-derive

```rust
// Public macro
#[ggen(config = "ggen.toml")]
pub struct MyCommands;

// Advanced usage
#[ggen(
    config = "ggen.toml",
    validate = true,
    fallback = "default.toml",
)]
pub struct AdvancedCommands;
```

### ggen-config

```rust
use ggen_config::clap::ConfigLoader;
use clap::Parser;

// Load config with precedence: CLI > Env > File > Defaults
let config: MyCommands = ConfigLoader::load()?;

// Custom discovery
let discovery = ConfigDiscovery::new()
    .add_search_path("./custom.toml")
    .parent_dirs(true);
let config = ConfigLoader::with_discovery(discovery).load()?;
```

### ggen-cli validation

```rust
use ggen_cli::validation::{ValidationMiddleware, IOOperation};

let middleware = ValidationMiddleware::from_config(&config)?;

middleware.execute("graph", "update", args, |ctx| {
    // Validated command execution
    Ok(())
}).await?;
```

---

## Testing Strategy

### Unit Tests (Per Crate)

```
ggen-cli-derive/tests/
├── macro_tests.rs           # Macro expansion tests
├── ir_tests.rs              # IR generation tests
└── generator_tests.rs       # Code generation tests

ggen-config/tests/
├── loader_tests.rs          # Config loading
├── discovery_tests.rs       # File discovery
├── merger_tests.rs          # Config merging
└── validation_tests.rs      # Schema validation

ggen-cli/tests/
├── cli_tests.rs             # CLI parsing
├── validation_tests.rs      # Middleware tests
└── integration_tests.rs     # End-to-end tests
```

### Integration Tests (Cross-Crate)

```
tests/integration/
├── macro_integration.rs     # Macro + config loader
├── cli_integration.rs       # Full CLI workflow
├── validation_integration.rs # Permission + constitution
└── audit_integration.rs     # Audit logging
```

### Property-Based Tests

```rust
// In ggen-cli-derive/tests/
proptest! {
    #[test]
    fn test_any_valid_toml_generates_valid_code(toml in valid_toml_strategy()) {
        let ir = parse_toml(&toml)?;
        let code = generate_code(&ir)?;
        // Should compile
        assert!(code.is_valid_rust());
    }
}
```

---

## Build Configuration

### Workspace Cargo.toml Updates

```toml
[workspace]
members = [
    # ... existing members ...
    "crates/ggen-cli-derive",  # NEW
]

[workspace.dependencies]
# ... existing deps ...
syn = "2.0"
quote = "1.0"
proc-macro2 = "1.0"
darling = "0.20"
```

### ggen-cli-derive/Cargo.toml

```toml
[package]
name = "ggen-cli-derive"
version = "3.2.0"
edition = "2021"
license = "MIT"

[lib]
proc-macro = true

[dependencies]
syn.workspace = true
quote.workspace = true
proc-macro2.workspace = true
toml.workspace = true
serde.workspace = true
heck = "0.5"
darling = "0.20"
proc-macro-error = "1.0"

[dev-dependencies]
trybuild = "1.0"
```

---

## Documentation Structure

```
docs/
├── architecture/
│   ├── clap-ggen-integration-design.md
│   ├── ggen-cli-macro-design.md
│   ├── noun-verb-validation-design.md
│   └── clap-integration-module-structure.md
│
├── guides/
│   ├── getting-started-with-ggen-macro.md
│   ├── writing-ggen-toml.md
│   ├── validation-and-security.md
│   └── audit-logging-guide.md
│
└── api/
    ├── ggen-cli-derive-api.md
    ├── ggen-config-api.md
    └── validation-api.md
```

---

## Feature Flags

### ggen-cli-derive

```toml
[features]
default = []
extra-validation = ["shacl_validation"]  # Enable SHACL validation in macro
debug-output = []                        # Print generated code during compilation
```

### ggen-config

```toml
[features]
default = ["clap"]
clap = ["dep:clap"]                      # Clap integration (optional)
env-expand = ["regex"]                   # Environment variable expansion
validation = ["ggen-core/validation"]    # Runtime validation
```

### ggen-cli

```toml
[features]
default = ["validation", "audit"]
validation = ["ggen-cli/validation"]     # Validation middleware
audit = ["ggen-cli/audit"]               # Audit logging
confirmation = []                         # Interactive confirmation for destructive ops
```

---

## Performance Characteristics

### Compile-Time

| Phase | Time | Notes |
|-------|------|-------|
| Macro expansion | 0.5-1.0s | One-time per crate using `#[ggen]` |
| TOML parsing | <50ms | Cached by cargo |
| Code generation | <100ms | Quote AST generation |
| Type checking | Normal | Generated code is simple |

**Total**: +1-2s compile time for crates using macro

### Runtime

| Phase | Time | Notes |
|-------|------|-------|
| Config discovery | <1ms | Filesystem stat calls |
| TOML parsing | 1-5ms | One-time at startup |
| Config merging | <0.1ms | Zero-cost with proper types |
| Validation | 0.1-1ms | Per command execution |
| Audit logging | 0.1-0.5ms | Async file write |

**Total**: ~5-10ms overhead at startup, <2ms per command

---

## Migration Path (Existing Code)

### Phase 1: Add Dependencies

```toml
# In ggen-cli/Cargo.toml
[dependencies]
ggen-cli-derive = { path = "../ggen-cli-derive", version = "3.2.0" }
```

### Phase 2: Annotate Existing Structs

```rust
// Before:
#[derive(Parser)]
pub struct GraphCommands {
    #[command(subcommand)]
    pub action: GraphAction,
}

// After:
#[ggen(config = "ggen.toml")]
pub struct GraphCommands;
// Auto-generated from TOML
```

### Phase 3: Update main.rs

```rust
// Before:
let cli = Cli::parse();

// After:
use ggen_config::clap::ConfigLoader;
let cli: Cli = ConfigLoader::load()?;
```

### Phase 4: Add Validation

```rust
// Add validation middleware
let middleware = ValidationMiddleware::from_config(&config)?;

middleware.execute("graph", "query", args, |ctx| {
    // Existing command logic
}).await?;
```

---

## Backwards Compatibility

### Support for Non-Macro Usage

```rust
// Users can still use clap directly
#[derive(Parser)]
pub struct ManualCommands {
    // Manual clap definitions
}

// And load config separately
let config = ggen_config::load("ggen.toml")?;
```

### Gradual Adoption

```rust
// Mix macro and manual definitions
#[ggen(config = "ggen.toml", commands = ["graph", "ontology"])]
pub struct GeneratedCommands;

#[derive(Parser)]
pub struct ManualCommands {
    // Custom commands not in ggen.toml
}

#[derive(Parser)]
pub struct AllCommands {
    #[command(flatten)]
    generated: GeneratedCommands,

    #[command(flatten)]
    manual: ManualCommands,
}
```

---

## Error Handling Across Modules

### Error Types

```rust
// ggen-cli-derive/src/error.rs
pub enum MacroError {
    ConfigNotFound { path: String },
    InvalidToml { source: toml::de::Error },
    // ... compile-time errors
}

// ggen-config/src/error.rs
pub enum ConfigError {
    DiscoveryFailed,
    EnvVarNotFound { var: String },
    // ... runtime config errors
}

// ggen-cli/src/validation/error.rs
pub enum ValidationError {
    OperationNotAllowed { /* ... */ },
    ConstitutionViolation { /* ... */ },
    // ... runtime validation errors
}
```

### Error Conversion

```rust
// In ggen-cli
impl From<ConfigError> for CliError {
    fn from(e: ConfigError) -> Self {
        CliError::Config(e)
    }
}

impl From<ValidationError> for CliError {
    fn from(e: ValidationError) -> Self {
        CliError::Validation(e)
    }
}
```

---

## Continuous Integration

### CI Pipeline

```yaml
# .github/workflows/integration.yml
name: Clap Integration Tests

on: [push, pull_request]

jobs:
  test-macro:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - run: cargo test -p ggen-cli-derive

  test-config:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - run: cargo test -p ggen-config

  test-cli:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - run: cargo test -p ggen-cli

  integration:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - run: cargo test --workspace --test '*integration*'
```

---

## Success Metrics

### Technical Metrics

1. ✅ Zero runtime overhead for config access (compile-time types)
2. ✅ <2s additional compile time per crate using macro
3. ✅ <10ms config loading at startup
4. ✅ <2ms validation overhead per command
5. ✅ 100% type safety (no runtime type errors)

### Quality Metrics

1. ✅ >95% test coverage across all modules
2. ✅ All integration tests passing
3. ✅ Zero clippy warnings
4. ✅ Complete API documentation
5. ✅ Example code in all public APIs

### User Experience Metrics

1. ✅ Clear error messages for config errors
2. ✅ IDE autocomplete works for generated code
3. ✅ `cargo doc` generates complete docs
4. ✅ Easy migration path from manual clap
5. ✅ Backwards compatible with existing code

---

**Document Status**: ✅ Complete

**Module Structure Summary**:
- ⭐ 1 NEW crate: `ggen-cli-derive`
- 🔄 3 MODIFIED crates: `ggen-config`, `ggen-cli`, `ggen-core`
- 📦 Total integration: 4 crates working together
- 🧪 Test coverage: Unit + Integration + Property-based
- 📚 Documentation: Architecture + Guides + API

**Next Steps**: Implementation begins with Phase 1 (ggen-cli-derive)
