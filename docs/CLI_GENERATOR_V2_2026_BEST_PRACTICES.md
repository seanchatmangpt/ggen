# CLI Generator v2 - 2026 Best Practices

## Overview

The CLI Generator v2 implements 2026 best practices for generating Rust CLI applications using the `clap-noun-verb` framework v3.3.0. This document describes the architecture, usage, and benefits of the new generator.

## Architecture

### Workspace Pattern

The generator creates a Cargo workspace with separate crates:

```
{project-name}/
├── Cargo.toml              # Workspace manifest
└── crates/
    ├── {project}-cli/      # CLI presentation layer
    │   ├── Cargo.toml
    │   └── src/
    │       ├── main.rs     # Entry point with clap-noun-verb
    │       ├── lib.rs
    │       ├── runtime.rs  # Async/sync bridge
    │       └── cmds/       # Command definitions
    │           ├── mod.rs
    │           └── {noun}/
    │               ├── mod.rs
    │               └── {verb}.rs
    └── {project}-core/    # Domain/business logic layer
        ├── Cargo.toml
        └── src/
            ├── lib.rs
            └── {noun}/     # Domain modules
                ├── mod.rs
                └── {verb}.rs
```

### Separation of Concerns

#### CLI Layer (`{project}-cli`)

- **Purpose**: Presentation layer for CLI interactions
- **Dependencies**: `clap-noun-verb` v3.3.0, `clap`, `anyhow`
- **Responsibilities**:
  - Parse command-line arguments
  - Convert CLI args to domain input types
  - Call domain functions via stable interface
  - Format output for CLI (including JSON output via v3.0.0)

#### Domain Layer (`{project}-core`)

- **Purpose**: Pure business logic without CLI concerns
- **Dependencies**: `tokio` (for async), `serde`, domain-specific crates
- **Responsibilities**:
  - Implement business logic
  - Provide async functions for operations
  - Define stable input/output types
  - Remain CLI-agnostic (no `clap` dependencies)

### Key Features

#### 1. clap-noun-verb v3.3.0 Integration

- **Attribute Macros**: `#[noun]`, `#[verb]` for auto-discovery
- **Auto-inference**: Type inference for arguments (v3.3.0 feature)
- **JSON Output**: Built-in JSON output support via v3.0.0
- **Enhanced Help**: Improved help generation
- **Display Order**: Control command display order
- **Exclusive Groups**: Define mutually exclusive argument groups

#### 2. Domain Function References

The CLI layer references domain functions via stable paths:

```rust
// CLI layer (crates/{project}-cli/src/cmds/user/create.rs)
use {project}_core::user::create::{Input, execute as domain_create};

#[verb("create", "user")]
pub fn run(args: &CreateArgs) -> Result<()> {
    let input = Input {
        name: args.name.clone(),
        // ... map other args
    };
    
    let result = runtime::execute(async move {
        domain_create(input).await
    })?;
    
    println!("✅ User created: {}", result.id);
    Ok(())
}
```

```rust
// Domain layer (crates/{project}-core/src/user/create.rs)
pub struct Input {
    pub name: String,
    // ... other fields
}

pub struct Output {
    pub id: String,
    // ... other fields
}

pub async fn execute(input: Input) -> Result<Output> {
    // Pure business logic - no CLI concerns
    // ...
}
```

#### 3. Async/Sync Bridge

The CLI layer uses a runtime bridge to execute async domain functions:

```rust
// crates/{project}-cli/src/runtime.rs
pub fn execute<F, T>(future: F) -> Result<T>
where
    F: std::future::Future<Output = Result<T>> + Send + 'static,
{
    tokio::runtime::Runtime::new()
        .context("Failed to create runtime")?
        .block_on(future)
}
```

## Usage

### 1. Define RDF Ontology

Create a Turtle (.ttl) file describing your CLI:

```turtle
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix cli: <http://ggen.dev/schema/cli#> .
@prefix cnv: <http://ggen.dev/schema/clap-noun-verb#> .

<#my-cli> a cli:CliProject ;
    cli:name "my-cli" ;
    cli:version "0.1.0" ;
    cli:description "My CLI application" ;
    cli:edition "2021" ;
    cli:license "MIT" ;
    cli:hasCliCrate "my-cli" ;
    cli:hasDomainCrate "my-core" ;
    cli:hasWorkspaceResolver "2" .

<#user> a cnv:Noun ;
    cnv:name "user" ;
    cnv:description "User management" ;
    cnv:hasVerb <#create-user>, <#list-users> .

<#create-user> a cnv:Verb ;
    cnv:name "create" ;
    cnv:description "Create a new user" ;
    cnv:domainFunction "my_core::user::create" .

<#list-users> a cnv:Verb ;
    cnv:name "list" ;
    cnv:description "List all users" ;
    cnv:domainFunction "my_core::user::list" .
```

### 2. Generate CLI Project

```bash
# Using ggen CLI
ggen project gen --template-ref cli-noun-verb --ttl my-cli.ttl --output ./generated-cli

# Or programmatically
use ggen_ai::rdf::CliGenerator;
use std::path::Path;

let generator = CliGenerator::new(Path::new("templates").to_path_buf());
generator.generate_from_ttl(
    Path::new("my-cli.ttl"),
    Path::new("./generated-cli")
)?;
```

### 3. Build and Run

```bash
cd generated-cli
cargo build
cargo run -- user create --name "Alice"
cargo run -- user list
```

## RDF Ontology Schema

### Required Properties

- `cli:name`: Project name
- `cli:version`: Semantic version
- `cli:description`: Project description
- `cli:hasCliCrate`: CLI crate name (defaults to `{name}-cli`)
- `cli:hasDomainCrate`: Domain crate name (defaults to `{name}-core`)
- `cli:hasWorkspaceResolver`: Cargo workspace resolver version (defaults to `"2"`)

### Verb Properties

- `cnv:domainFunction`: Full path to domain function (e.g., `"my_core::user::create"`)
- `cnv:domainModule`: Domain module path (optional, inferred from noun)

## Benefits

### 1. Clean Separation

- CLI layer can evolve independently
- Domain logic remains testable without CLI
- Easier to add alternative interfaces (web API, library, etc.)

### 2. Type Safety

- Domain functions have explicit input/output types
- Compile-time guarantees for CLI-to-domain contracts
- IDE support for domain function discovery

### 3. Testability

- Domain functions can be tested in isolation
- CLI layer can be tested with mock domain functions
- Integration tests verify the full contract

### 4. Maintainability

- Clear boundaries between layers
- Easier refactoring (change domain without breaking CLI)
- Better code organization

### 5. Future-Proof

- Easy to add web API layer using same domain functions
- Library mode can expose domain functions directly
- Multiple CLI frontends (TUI, GUI) can share domain logic

## Migration from v1

If you have an existing v1 CLI project:

1. **Extract Domain Logic**: Move business logic to a new `{project}-core` crate
2. **Update CLI Layer**: Modify CLI commands to call domain functions
3. **Update Dependencies**: Ensure CLI crate depends on domain crate
4. **Update Workspace**: Add both crates to workspace `Cargo.toml`

## Examples

See `examples/` directory for complete working examples:
- `examples/cli-workspace-example/`: Full workspace with CLI and domain crates
- `examples/domain-layer-example/`: Domain layer with multiple nouns/verbs

## Best Practices

1. **Keep Domain Pure**: Never import `clap` or `clap-noun-verb` in domain crate
2. **Use Stable Interfaces**: Domain functions should have stable signatures
3. **Async Domain, Sync CLI**: Domain functions should be async; CLI layer bridges to sync
4. **Type-Driven**: Use strong types for domain inputs/outputs
5. **Error Handling**: Use `anyhow::Result` in CLI, typed errors in domain

## Troubleshooting

### "Failed to create runtime"

Ensure `tokio` is in CLI crate dependencies with `full` features.

### "Module not found: {domain_function}"

Verify domain function path matches the generated module structure.

### "Circular dependency detected"

Ensure CLI crate depends on domain crate, not vice versa.

## References

- [clap-noun-verb v3.3.0 Documentation](https://crates.io/crates/clap-noun-verb)
- [ggen Architecture Documentation](./architecture/)
- [RDF Ontology Schema](../ontologies/)

