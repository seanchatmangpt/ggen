<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Clap + ggen.toml Integration Architecture](#clap--ggentoml-integration-architecture)
  - [Executive Summary](#executive-summary)
  - [Architecture Overview](#architecture-overview)
    - [Design Principles](#design-principles)
  - [Layer 1: Schema Bridge](#layer-1-schema-bridge)
    - [Purpose](#purpose)
    - [Architecture](#architecture)
    - [TOML → Rust Type Mapping](#toml-%E2%86%92-rust-type-mapping)
    - [IR (Intermediate Representation)](#ir-intermediate-representation)
  - [Layer 2: Parser Integration](#layer-2-parser-integration)
    - [Purpose](#purpose-1)
    - [Architecture](#architecture-1)
    - [Config File Discovery](#config-file-discovery)
    - [Environment Variable Expansion](#environment-variable-expansion)
  - [Layer 3: Noun-Verb Pattern Integration](#layer-3-noun-verb-pattern-integration)
    - [Purpose](#purpose-2)
    - [Architecture](#architecture-2)
    - [Noun-Verb Benefits for Config Files](#noun-verb-benefits-for-config-files)
    - [IO Operations Validation](#io-operations-validation)
  - [Layer 4: Validation & Type Safety](#layer-4-validation--type-safety)
    - [Purpose](#purpose-3)
    - [Architecture](#architecture-3)
    - [SHACL Integration](#shacl-integration)
    - [Constitution Checks](#constitution-checks)
  - [Component Design](#component-design)
    - [Component 1: ggen-cli-derive (New Crate)](#component-1-ggen-cli-derive-new-crate)
    - [Component 2: ggen-config-clap (Extension Module)](#component-2-ggen-config-clap-extension-module)
    - [Component 3: clap-noun-verb Validation](#component-3-clap-noun-verb-validation)
  - [Data Flow Diagrams](#data-flow-diagrams)
    - [Configuration Loading Flow](#configuration-loading-flow)
    - [Macro Expansion Flow](#macro-expansion-flow)
  - [Error Handling Strategy](#error-handling-strategy)
    - [Error Types](#error-types)
    - [Error Recovery](#error-recovery)
  - [Security Considerations](#security-considerations)
    - [1. Environment Variable Injection](#1-environment-variable-injection)
    - [2. Path Traversal](#2-path-traversal)
    - [3. TOML Bomb (Denial of Service)](#3-toml-bomb-denial-of-service)
  - [Performance Implications](#performance-implications)
    - [Compile-Time Cost](#compile-time-cost)
    - [Runtime Cost](#runtime-cost)
    - [Optimization Strategies](#optimization-strategies)
  - [Module Structure](#module-structure)
  - [Next Steps (Implementation Plan)](#next-steps-implementation-plan)
    - [Phase 1: Schema Bridge (Week 1)](#phase-1-schema-bridge-week-1)
    - [Phase 2: Config Loader (Week 2)](#phase-2-config-loader-week-2)
    - [Phase 3: Validation (Week 3)](#phase-3-validation-week-3)
    - [Phase 4: Integration (Week 4)](#phase-4-integration-week-4)
  - [Success Criteria](#success-criteria)
  - [Appendix A: Example ggen.toml](#appendix-a-example-ggentoml)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Clap + ggen.toml Integration Architecture

**System Architect**: Claude (Hive Mind Swarm Phase 2)
**Date**: 2025-11-19
**Version**: 1.0.0
**Status**: Architecture Design

---

## Executive Summary

This document defines the architecture for integrating `clap` CLI framework with `ggen.toml` configuration files, enabling type-safe, validated command-line interfaces derived from TOML configuration with ontology-backed validation.

## Architecture Overview

### Design Principles

1. **Configuration as Single Source of Truth**: ggen.toml defines commands, options, defaults
2. **Type Safety First**: Compile-time validation via Rust type system + runtime SHACL validation
3. **Layered Precedence**: CLI args > Environment > Config file > Defaults
4. **Zero-Cost Abstractions**: No runtime overhead for configuration merging
5. **Developer Experience**: Auto-generate clap code from ggen.toml with helpful errors

---

## Layer 1: Schema Bridge

### Purpose
Bridge TOML configuration schema to Rust clap Command structures with full type safety.

### Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    ggen.toml                            │
│  [commands.graph.subcommands.query]                     │
│  description = "Query RDF graph"                        │
│  args = [                                                │
│    {name="sparql", type="String", required=true},       │
│    {name="format", type="String", default="turtle"}     │
│  ]                                                       │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│         Schema Bridge (ggen-cli-derive)                 │
│                                                          │
│  • Parse ggen.toml → IR (Intermediate Representation)   │
│  • Validate TOML schema (SHACL constraints)             │
│  • Map TOML types → Rust types                          │
│  • Generate clap::Command AST                           │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│            Generated Clap Code                          │
│                                                          │
│  #[derive(Parser)]                                      │
│  struct GraphQueryArgs {                                │
│      #[arg(long, required = true)]                      │
│      sparql: String,                                    │
│      #[arg(long, default_value = "turtle")]             │
│      format: String,                                    │
│  }                                                       │
└─────────────────────────────────────────────────────────┘
```

### TOML → Rust Type Mapping

| TOML Type | Rust Type | Validation |
|-----------|-----------|------------|
| `String` | `String` | Length constraints from SHACL |
| `Integer` | `i64` | Range constraints |
| `Float` | `f64` | Precision constraints |
| `Boolean` | `bool` | N/A |
| `Array<T>` | `Vec<T>` | Element validation |
| `Path` | `PathBuf` | Existence checks |
| `Url` | `Url` | Format validation |
| `Enum<T>` | `enum T` | Allowed values from ontology |

### IR (Intermediate Representation)

```rust
// Internal representation used by macro system
pub struct CommandIR {
    pub name: String,
    pub description: String,
    pub subcommands: Vec<CommandIR>,
    pub args: Vec<ArgIR>,
    pub validators: Vec<ValidatorIR>,
}

pub struct ArgIR {
    pub name: String,
    pub ty: TypeIR,
    pub required: bool,
    pub default: Option<String>,
    pub env_var: Option<String>,
    pub validators: Vec<ValidatorIR>,
}

pub enum TypeIR {
    String,
    Integer,
    Float,
    Boolean,
    Path,
    Url,
    Array(Box<TypeIR>),
    Enum(Vec<String>),
    Custom(String),
}

pub struct ValidatorIR {
    pub kind: ValidatorKind,
    pub shacl_shape: Option<String>,
}

pub enum ValidatorKind {
    Range { min: Option<i64>, max: Option<i64> },
    Length { min: Option<usize>, max: Option<usize> },
    Pattern { regex: String },
    OneOf { values: Vec<String> },
    Custom { function: String },
}
```

---

## Layer 2: Parser Integration

### Purpose
Merge configuration from multiple sources with well-defined precedence rules.

### Architecture

```
┌─────────────────────────────────────────────────────────┐
│                 Configuration Sources                    │
│                                                          │
│  1. CLI Arguments (highest priority)                    │
│  2. Environment Variables                               │
│  3. ggen.toml Config File                               │
│  4. Defaults from TOML schema                           │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│          Config Loader (ggen-config-clap)               │
│                                                          │
│  1. Discover ggen.toml (current dir → parents)          │
│  2. Parse TOML with serde                               │
│  3. Expand environment variables: ${VAR}                │
│  4. Validate against SHACL shapes                       │
│  5. Build default clap::Command                         │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│              Precedence Resolver                        │
│                                                          │
│  fn resolve<T>(                                          │
│      cli_arg: Option<T>,                                │
│      env_var: Option<T>,                                │
│      config: Option<T>,                                 │
│      default: Option<T>,                                │
│  ) -> Result<T> {                                       │
│      cli_arg                                            │
│          .or(env_var)                                   │
│          .or(config)                                    │
│          .or(default)                                   │
│          .ok_or(Error::MissingRequired)                 │
│  }                                                       │
└─────────────────────────────────────────────────────────┘
```

### Config File Discovery

```rust
pub struct ConfigDiscovery {
    search_paths: Vec<PathBuf>,
}

impl ConfigDiscovery {
    pub fn new() -> Self {
        Self {
            search_paths: vec![
                PathBuf::from("./ggen.toml"),
                PathBuf::from("./config/ggen.toml"),
                PathBuf::from("./.ggen/config.toml"),
            ],
        }
    }

    pub fn discover(&self) -> Result<PathBuf> {
        // Search current dir, then walk up parents
        let mut current = env::current_dir()?;
        loop {
            for path in &self.search_paths {
                let candidate = current.join(path);
                if candidate.exists() {
                    return Ok(candidate);
                }
            }
            if !current.pop() {
                break;
            }
        }

        // Fallback: check Cargo.toml for embedded config
        let cargo_toml = env::current_dir()?.join("Cargo.toml");
        if cargo_toml.exists() {
            return Ok(cargo_toml);
        }

        Err(Error::ConfigNotFound)
    }
}
```

### Environment Variable Expansion

```rust
pub fn expand_env_vars(value: &str) -> Result<String> {
    let re = Regex::new(r"\$\{([A-Z_][A-Z0-9_]*)\}")?;
    let mut result = value.to_string();

    for cap in re.captures_iter(value) {
        let var_name = &cap[1];
        let var_value = env::var(var_name)
            .map_err(|_| Error::EnvVarNotFound(var_name.to_string()))?;
        result = result.replace(&format!("${{{}}}", var_name), &var_value);
    }

    Ok(result)
}
```

---

## Layer 3: Noun-Verb Pattern Integration

### Purpose
Integrate `clap-noun-verb` with ggen.toml for clean command structure and validation.

### Architecture

```
┌─────────────────────────────────────────────────────────┐
│              ggen.toml (Noun-Verb Config)               │
│                                                          │
│  [commands.graph]                                        │
│  noun = "graph"                                          │
│  verbs = ["query", "update", "export", "validate"]      │
│  io_operations = {                                       │
│    query: ["read"],                                      │
│    update: ["read", "write"],                            │
│    export: ["read"],                                     │
│  }                                                       │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│         Noun-Verb Validator (clap-noun-verb)            │
│                                                          │
│  • Validate verb is allowed for noun                    │
│  • Check IO permissions (read/write/delete)             │
│  • Audit log all operations                             │
│  • Constitution checks (hard invariants)                │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│              Generated Command Structure                │
│                                                          │
│  ggen graph query --sparql "SELECT ..."                 │
│  ggen graph update --insert "..."                       │
│  ggen graph export --format turtle                      │
│  ggen graph validate --shape-file shapes.ttl            │
└─────────────────────────────────────────────────────────┘
```

### Noun-Verb Benefits for Config Files

1. **Hierarchical Organization**: Natural grouping of related commands
2. **Permission Modeling**: Define read/write/delete for each verb
3. **Validation**: Ensure verbs match noun capabilities
4. **Discovery**: Auto-generate help from ggen.toml structure
5. **Audit Logging**: Track all noun.verb operations

### IO Operations Validation

```rust
pub struct IOValidator {
    allowed_operations: HashMap<String, Vec<IOOperation>>,
}

pub enum IOOperation {
    Read,
    Write,
    Delete,
    Network,
}

impl IOValidator {
    pub fn validate(&self, noun: &str, verb: &str, operation: IOOperation) -> Result<()> {
        let allowed = self.allowed_operations
            .get(&format!("{}.{}", noun, verb))
            .ok_or(Error::UnknownCommand)?;

        if !allowed.contains(&operation) {
            return Err(Error::OperationNotAllowed {
                noun: noun.to_string(),
                verb: verb.to_string(),
                operation,
            });
        }

        Ok(())
    }
}
```

---

## Layer 4: Validation & Type Safety

### Purpose
Enforce compile-time and runtime validation using SHACL ontology and constitution.

### Architecture

```
┌─────────────────────────────────────────────────────────┐
│              Compile-Time Validation                    │
│                                                          │
│  • Macro validates ggen.toml at compile time            │
│  • Type checking: String, i64, bool, etc.               │
│  • Required args checked                                │
│  • Enum values validated against ontology               │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│              Runtime Validation                         │
│                                                          │
│  • SHACL shape validation (sh:minLength, sh:pattern)    │
│  • Constitution checks (hard invariants)                │
│  • Custom validators from ggen.toml                     │
│  • Cross-field validation                               │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│              Error Reporting                            │
│                                                          │
│  • Helpful error messages with suggestions              │
│  • "Did you mean?" for typos                            │
│  • Show valid values for enums                          │
│  • Pretty-print validation errors                       │
└─────────────────────────────────────────────────────────┘
```

### SHACL Integration

```rust
use shacl_validation::{ShaclValidator, ValidationReport};

pub struct ConfigValidator {
    shacl: ShaclValidator,
}

impl ConfigValidator {
    pub fn validate_arg(&self, arg: &ArgIR, value: &str) -> Result<ValidationReport> {
        if let Some(shacl_shape) = &arg.validators.iter()
            .find_map(|v| v.shacl_shape.as_ref())
        {
            self.shacl.validate(shacl_shape, value)?;
        }
        Ok(ValidationReport::success())
    }
}
```

### Constitution Checks

```rust
pub struct Constitution {
    hard_invariants: Vec<Invariant>,
}

pub struct Invariant {
    pub name: String,
    pub check: Box<dyn Fn(&Config) -> Result<()>>,
}

impl Constitution {
    pub fn check(&self, config: &Config) -> Result<()> {
        for invariant in &self.hard_invariants {
            (invariant.check)(config)
                .map_err(|e| Error::ConstitutionViolation {
                    invariant: invariant.name.clone(),
                    reason: e.to_string(),
                })?;
        }
        Ok(())
    }
}
```

---

## Component Design

### Component 1: ggen-cli-derive (New Crate)

**Purpose**: Procedural macro to auto-generate clap code from ggen.toml

```rust
// crates/ggen-cli-derive/src/lib.rs

#[proc_macro_attribute]
pub fn ggen(attr: TokenStream, item: TokenStream) -> TokenStream {
    // 1. Parse attribute: #[ggen(config = "ggen.toml")]
    let config_path = parse_config_path(attr)?;

    // 2. Load and parse ggen.toml
    let toml_content = std::fs::read_to_string(config_path)?;
    let config: GgenTomlConfig = toml::from_str(&toml_content)?;

    // 3. Validate TOML against schema
    validate_toml_schema(&config)?;

    // 4. Generate IR from TOML
    let ir = build_command_ir(&config)?;

    // 5. Generate clap::Parser code
    let clap_code = generate_clap_parser(&ir)?;

    // 6. Merge with original item
    quote! {
        #clap_code
        #item
    }.into()
}

fn generate_clap_parser(ir: &CommandIR) -> TokenStream {
    let name = &ir.name;
    let description = &ir.description;
    let args = ir.args.iter().map(generate_arg);

    quote! {
        #[derive(Parser, Debug)]
        #[command(name = #name, about = #description)]
        pub struct #name {
            #(#args),*
        }
    }
}

fn generate_arg(arg: &ArgIR) -> TokenStream {
    let name = &arg.name;
    let ty = rust_type(&arg.ty);
    let required = arg.required;
    let default = arg.default.as_ref();

    let attrs = if required {
        quote! { #[arg(long, required = true)] }
    } else if let Some(default) = default {
        quote! { #[arg(long, default_value = #default)] }
    } else {
        quote! { #[arg(long)] }
    };

    quote! {
        #attrs
        pub #name: #ty
    }
}
```

**Usage Example**:

```rust
// In ggen-cli/src/commands/graph.rs

use ggen_cli_derive::ggen;

#[ggen(config = "ggen.toml")]
pub struct GraphCommands;

// Expands to:

#[derive(Parser, Debug)]
#[command(name = "graph", about = "Graph operations")]
pub struct GraphCommands {
    #[command(subcommand)]
    pub action: GraphAction,
}

#[derive(Subcommand, Debug)]
pub enum GraphAction {
    Query(GraphQueryArgs),
    Update(GraphUpdateArgs),
    Export(GraphExportArgs),
}

#[derive(Args, Debug)]
pub struct GraphQueryArgs {
    #[arg(long, required = true)]
    pub sparql: String,

    #[arg(long, default_value = "turtle")]
    pub format: String,
}
```

---

### Component 2: ggen-config-clap (Extension Module)

**Purpose**: Runtime config loader and merger for clap commands

```rust
// crates/ggen-config/src/clap.rs

use clap::Parser;
use serde::Deserialize;

pub struct ConfigLoader {
    discovery: ConfigDiscovery,
}

impl ConfigLoader {
    pub fn load<T: Parser + Deserialize>() -> Result<T> {
        // 1. Discover ggen.toml
        let config_path = ConfigDiscovery::new().discover()?;

        // 2. Parse TOML
        let toml_content = std::fs::read_to_string(config_path)?;
        let toml_config: TomlConfig = toml::from_str(&toml_content)?;

        // 3. Expand environment variables
        let expanded = expand_env_vars(&toml_config)?;

        // 4. Parse CLI args
        let cli_args = T::parse();

        // 5. Merge sources: CLI > Env > Config > Defaults
        let merged = merge_configs(cli_args, expanded)?;

        // 6. Validate final config
        validate_config(&merged)?;

        Ok(merged)
    }
}

fn merge_configs<T>(cli: T, config: TomlConfig) -> Result<T> {
    // Use serde to merge TOML defaults with CLI overrides
    // Fields in CLI (if Some) override config fields
    // This preserves type safety while allowing flexible config

    let cli_json = serde_json::to_value(cli)?;
    let config_json = serde_json::to_value(config)?;

    let merged = merge_json_recursive(cli_json, config_json)?;
    let result = serde_json::from_value(merged)?;

    Ok(result)
}
```

**Usage Example**:

```rust
// In ggen-cli/src/main.rs

use ggen_config::clap::ConfigLoader;

#[tokio::main]
async fn main() -> Result<()> {
    // Load config with CLI args > Env > ggen.toml > Defaults
    let config: GraphCommands = ConfigLoader::load()?;

    match config.action {
        GraphAction::Query(args) => {
            // args.sparql comes from CLI or ggen.toml
            // args.format defaults to "turtle" from ggen.toml
            execute_query(&args).await?;
        }
        // ...
    }

    Ok(())
}
```

---

### Component 3: clap-noun-verb Validation

**Purpose**: Validate noun-verb commands with IO operations and permissions

```rust
// crates/ggen-cli/src/validation.rs

use clap_noun_verb::{Noun, Verb};

pub struct NounVerbValidator {
    config: GgenTomlConfig,
    io_validator: IOValidator,
    audit_log: AuditLog,
}

impl NounVerbValidator {
    pub fn validate_command(&self, noun: &str, verb: &str) -> Result<()> {
        // 1. Check noun exists in ggen.toml
        let noun_config = self.config.commands.get(noun)
            .ok_or(Error::UnknownNoun(noun.to_string()))?;

        // 2. Check verb is allowed for noun
        if !noun_config.verbs.contains(&verb.to_string()) {
            return Err(Error::InvalidVerb {
                noun: noun.to_string(),
                verb: verb.to_string(),
                allowed: noun_config.verbs.clone(),
            });
        }

        // 3. Validate IO operations
        self.io_validator.validate(noun, verb, IOOperation::Read)?;

        // 4. Audit log
        self.audit_log.record(noun, verb)?;

        Ok(())
    }
}

pub struct AuditLog {
    log_file: PathBuf,
}

impl AuditLog {
    pub fn record(&self, noun: &str, verb: &str) -> Result<()> {
        use std::io::Write;

        let entry = AuditEntry {
            timestamp: chrono::Utc::now(),
            noun: noun.to_string(),
            verb: verb.to_string(),
            user: std::env::var("USER").ok(),
        };

        let mut file = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(&self.log_file)?;

        writeln!(file, "{}", serde_json::to_string(&entry)?)?;

        Ok(())
    }
}
```

---

## Data Flow Diagrams

### Configuration Loading Flow

```
User Input
    │
    ├─► CLI Args ──────────────┐
    │                          │
    ├─► Environment Vars ──────┼──► Precedence Resolver
    │                          │
    └─► ggen.toml ─────────────┤
        └─► Defaults ──────────┘
                               │
                               ▼
                         Merged Config
                               │
                               ▼
                    SHACL Validation
                               │
                               ▼
                    Constitution Check
                               │
                               ▼
                     Validated Config
                               │
                               ▼
                    Command Execution
```

### Macro Expansion Flow

```
Compile Time:
    ggen.toml
        │
        ▼
    Parse TOML ──► Validate Schema
        │
        ▼
    Build IR
        │
        ▼
    Generate AST ──► clap::Parser code
        │
        ▼
    Type Check (rustc)
        │
        ▼
    Compiled Binary

Runtime:
    User runs CLI
        │
        ▼
    ConfigLoader::load()
        │
        ▼
    Merge configs
        │
        ▼
    Execute command
```

---

## Error Handling Strategy

### Error Types

```rust
#[derive(Debug, thiserror::Error)]
pub enum ConfigError {
    #[error("Config file not found. Searched: {paths:?}")]
    ConfigNotFound { paths: Vec<PathBuf> },

    #[error("Invalid TOML: {0}")]
    InvalidToml(#[from] toml::de::Error),

    #[error("Environment variable not found: {0}")]
    EnvVarNotFound(String),

    #[error("SHACL validation failed: {report}")]
    ValidationFailed { report: ValidationReport },

    #[error("Constitution violation: {invariant} - {reason}")]
    ConstitutionViolation { invariant: String, reason: String },

    #[error("Unknown noun: {0}. Did you mean: {suggestions:?}")]
    UnknownNoun(String, Vec<String>),

    #[error("Invalid verb '{verb}' for noun '{noun}'. Allowed: {allowed:?}")]
    InvalidVerb { noun: String, verb: String, allowed: Vec<String> },

    #[error("Operation '{operation:?}' not allowed for {noun}.{verb}")]
    OperationNotAllowed { noun: String, verb: String, operation: IOOperation },
}
```

### Error Recovery

```rust
impl ConfigLoader {
    pub fn load_with_fallback<T: Parser + Deserialize>() -> Result<T> {
        match Self::load() {
            Ok(config) => Ok(config),
            Err(ConfigError::ConfigNotFound { .. }) => {
                eprintln!("⚠️  ggen.toml not found, using CLI args only");
                Ok(T::parse())
            }
            Err(e) => Err(e),
        }
    }
}
```

---

## Security Considerations

### 1. Environment Variable Injection

**Risk**: Malicious environment variables could inject code
**Mitigation**: Whitelist allowed environment variables in ggen.toml

```toml
[security]
allowed_env_vars = ["HOME", "USER", "PATH", "GGEN_*"]
```

### 2. Path Traversal

**Risk**: Config paths could escape project directory
**Mitigation**: Canonicalize paths and check bounds

```rust
fn validate_path(path: &Path) -> Result<PathBuf> {
    let canonical = path.canonicalize()?;
    let project_root = env::current_dir()?.canonicalize()?;

    if !canonical.starts_with(&project_root) {
        return Err(Error::PathTraversal);
    }

    Ok(canonical)
}
```

### 3. TOML Bomb (Denial of Service)

**Risk**: Deeply nested TOML could exhaust memory
**Mitigation**: Limit recursion depth and file size

```rust
const MAX_TOML_SIZE: usize = 1_000_000; // 1 MB
const MAX_RECURSION_DEPTH: usize = 10;

fn parse_toml_safe(content: &str) -> Result<TomlConfig> {
    if content.len() > MAX_TOML_SIZE {
        return Err(Error::TomlTooLarge);
    }

    toml::from_str(content)
}
```

---

## Performance Implications

### Compile-Time Cost

- **Macro expansion**: +0.5-1.0s per crate using `#[ggen]`
- **TOML parsing**: Minimal, cached by cargo
- **Trade-off**: Slower compile, faster runtime (zero-cost config)

### Runtime Cost

- **Config loading**: ~1-5ms (one-time cost at startup)
- **Validation**: ~0.1-1ms per command
- **Merging**: Zero-cost (compile-time types)

### Optimization Strategies

1. **Lazy Loading**: Only load config when needed
2. **Caching**: Cache parsed TOML in-memory
3. **Parallel Validation**: Run SHACL checks in parallel
4. **Incremental Parsing**: Stream large TOML files

---

## Module Structure

```
crates/
├── ggen-cli-derive/          # NEW: Procedural macros
│   ├── Cargo.toml
│   └── src/
│       ├── lib.rs            # #[ggen] macro
│       ├── ir.rs             # Intermediate representation
│       ├── generator.rs      # Code generation
│       └── validator.rs      # Schema validation
│
├── ggen-config/              # EXTENDED
│   ├── Cargo.toml
│   └── src/
│       ├── lib.rs
│       ├── clap.rs           # NEW: Clap integration
│       ├── loader.rs         # Config file loading
│       ├── merger.rs         # Multi-source merging
│       └── validator.rs      # Runtime validation
│
└── ggen-cli/                 # MODIFIED
    ├── Cargo.toml
    └── src/
        ├── main.rs           # Use ConfigLoader
        ├── commands/
        │   ├── graph.rs      # #[ggen(config = "ggen.toml")]
        │   └── ontology.rs   # #[ggen(config = "ggen.toml")]
        └── validation.rs     # Noun-verb validation
```

---

## Next Steps (Implementation Plan)

### Phase 1: Schema Bridge (Week 1)
1. Create `ggen-cli-derive` crate
2. Implement IR types
3. Write TOML → IR parser
4. Add basic macro expansion

### Phase 2: Config Loader (Week 2)
1. Extend `ggen-config` with `clap.rs`
2. Implement config discovery
3. Add environment variable expansion
4. Write config merger

### Phase 3: Validation (Week 3)
1. Integrate SHACL validator
2. Add constitution checks
3. Implement noun-verb validation
4. Write audit logging

### Phase 4: Integration (Week 4)
1. Update `ggen-cli` to use macro
2. Write integration tests
3. Performance benchmarks
4. Documentation

---

## Success Criteria

1. ✅ Load ggen.toml for command defaults
2. ✅ Environment variable override support
3. ✅ Type-safe configuration merging
4. ✅ Helpful error messages with suggestions
5. ✅ SHACL validation at runtime
6. ✅ Constitution checks enforced
7. ✅ Noun-verb pattern validated
8. ✅ Zero runtime overhead for config access
9. ✅ Comprehensive test coverage (>90%)
10. ✅ Clear documentation and examples

---

## Appendix A: Example ggen.toml

```toml
# ggen.toml - Complete example

[project]
name = "ggen"
version = "3.2.0"

[commands.graph]
noun = "graph"
description = "RDF graph operations"
verbs = ["query", "update", "export", "validate"]

[commands.graph.subcommands.query]
description = "Query RDF graph with SPARQL"
args = [
    { name = "sparql", type = "String", required = true, help = "SPARQL query string" },
    { name = "format", type = "String", default = "turtle", enum = ["turtle", "ntriples", "jsonld"] },
    { name = "output", type = "Path", env = "GGEN_OUTPUT", help = "Output file path" },
]
validators = [
    { kind = "shacl", shape = "sparql:QueryShape" },
]

[commands.graph.io_operations]
query = ["read"]
update = ["read", "write"]
export = ["read"]

[security]
allowed_env_vars = ["HOME", "USER", "GGEN_*"]
max_config_size = 1_000_000

[constitution]
invariants = [
    { name = "output_must_be_writable", check = "output_path_is_writable" },
    { name = "format_must_be_valid", check = "format_in_enum" },
]
```

---

**Document Status**: ✅ Complete
**Next Document**: ggen-cli-macro-design.md
**Related**: noun-verb-validation-design.md
