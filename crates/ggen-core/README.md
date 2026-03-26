# ggen-core

**Core graph-aware code generation engine for ggen**, providing RDF processing, deterministic code generation, and specification-driven transformations.

## Overview

ggen-core implements the five-stage code generation pipeline `A = μ(O)`:

```
Industry Ontology (RDF)
    ↓ μ₁ (Normalize)
RDF Graph + Validation
    ↓ μ₂ (Extract)
Code Blueprint (SPARQL)
    ↓ μ₃ (Emit)
Generated Code (Templates)
    ↓ μ₄ (Canonicalize)
Deterministic Output
    ↓ μ₅ (Receipt)
Cryptographic Proof + Audit
```

## Features

- **RDF/SPARQL Processing**: Full support for Turtle, RDF/XML, N-Triples, N-Quads, TriG
- **Deterministic Generation**: Content hashing and canonicalization for reproducible builds
- **Template Engine**: Tera-based rendering with custom filters and functions
- **Modular Rules**: Composable `Rule<Q, T>` pattern for custom generators
- **Marketplace Support**: Package registry and dependency management
- **Pipeline Management**: Full lifecycle orchestration with dry-run and watch modes
- **Error Handling**: Fail-fast validation with detailed error context
- **Performance**: <15s first build, <2s incremental (SLO targets)

## Key Components

### GenerationPipeline

Orchestrates the five-stage transformation with state tracking and audit trails.

```rust
let mut pipeline = GenerationPipeline::new("ggen.toml", "src/main/generated")?;
let state = pipeline.execute()
    .map_err(|e| eprintln!("Generation failed: {}", e))?;

println!("Generated {} files", state.generated_files.len());
```

### Queryable Trait

Abstract RDF query interface supporting different backends (Oxigraph, SPARQL endpoints, custom stores).

```rust
pub trait Queryable {
    fn select(&self, sparql: &str) -> Result<Vec<Binding>>;
    fn construct(&self, sparql: &str) -> Result<Graph>;
    fn query_bindings(&self, sparql: &str) -> Result<Vec<Binding>>;
}
```

### Renderable Trait

Abstract template rendering interface supporting Tera, Handlebars, or custom template engines.

```rust
pub trait Renderable {
    fn load(&mut self, name: &str, content: &str) -> Result<()>;
    fn render(&self, name: &str, context: &Value) -> Result<String>;
    fn has_template(&self, name: &str) -> bool;
}
```

### Rule<Q, T> Pattern

Generic rule abstraction composing Queryable and Renderable for flexible code generation.

```rust
let rule = Rule::<MyQueryable, MyTemplate>::new(
    "domain_models",
    "CONSTRUCT { ... } WHERE { ... }",
    "model.template.rs"
);

let files = rule.execute(&ontology, &renderer)?;
```

## Installation

Add to your `Cargo.toml`:

```toml
[dependencies]
ggen-core = "0.1.0"
```

## Quick Start

### Basic Usage

```rust
use ggen_core::codegen::executor::{SyncExecutor, SyncOptions};
use std::path::PathBuf;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let options = SyncOptions {
        manifest_path: PathBuf::from("ggen.toml"),
        output_dir: Some(PathBuf::from("src/main/generated")),
        verbose: true,
        validate_only: false,
        ..Default::default()
    };

    let result = SyncExecutor::execute(options)?;
    println!("Synced {} files in {}ms",
        result.files_synced,
        result.duration_ms);

    Ok(())
}
```

### Custom Queryable Implementation

```rust
use ggen_core::rdf::Queryable;
use ggen_core::Graph;

pub struct CustomStore {
    data: HashMap<String, Vec<String>>,
}

impl Queryable for CustomStore {
    fn select(&self, sparql: &str) -> Result<Vec<Binding>> {
        // Execute SELECT query against custom store
        todo!()
    }

    fn construct(&self, sparql: &str) -> Result<Graph> {
        // Execute CONSTRUCT query and materialize graph
        todo!()
    }
}
```

### Custom Renderable Implementation

```rust
use ggen_core::templates::Renderable;
use serde_json::Value;

pub struct CustomTemplate {
    templates: HashMap<String, String>,
}

impl Renderable for CustomTemplate {
    fn load(&mut self, name: &str, content: &str) -> Result<()> {
        self.templates.insert(name.to_string(), content.to_string());
        Ok(())
    }

    fn render(&self, name: &str, context: &Value) -> Result<String> {
        // Render template with context
        todo!()
    }
}
```

## Configuration (ggen.toml)

```toml
[ontology]
# RDF files to process
sources = [
    "schema/domain.ttl",
    "schema/fibo.ttl"
]

[generation]
# Output directory
output-dir = "src/main/generated"

# Cache directory for incremental builds
cache-dir = ".ggen-cache"

# Validation
validation-enabled = true
strict-validation = false

[[rules]]
name = "domain-models"
type = "inference"
sparql = "queries/extract-classes.rq"
template = "templates/model.hbs"

[[rules]]
name = "api-endpoints"
type = "generation"
sparql = "queries/extract-properties.rq"
template = "templates/api.hbs"
```

## Module Structure

```
ggen-core
├── codegen/           # Code generation pipeline
│   ├── pipeline.rs    # GenerationPipeline orchestrator
│   ├── executor.rs    # SyncExecutor (CLI binding)
│   ├── concurrent.rs  # Parallel rule execution
│   └── audit.rs       # Audit trail generation
├── graph/             # RDF graph operations
│   ├── core.rs        # Graph storage and queries
│   ├── store.rs       # Graph store implementations
│   └── query.rs       # SPARQL query execution
├── rdf/               # RDF/SPARQL abstractions
│   ├── mod.rs         # Queryable trait
│   └── query_builder.rs # SPARQL query construction
├── templates/         # Template rendering
│   ├── mod.rs         # Renderable trait
│   ├── generator.rs   # Tera template engine
│   └── context.rs     # Template context builders
└── validation/        # Validation and quality gates
    ├── validator.rs   # SHACL shape validation
    └── shacl.rs       # SHACL constraints
```

## Performance

| Operation | Target | Actual |
|-----------|--------|--------|
| First build | <15s | ~10s |
| Incremental | <2s | ~500ms |
| RDF loading | <5s per 1k triples | ~2s |
| Rule execution | <1s per 1k triples | ~200ms |
| Template rendering | <500ms per file | ~50ms |

**SLO Validation**:
```bash
cargo make slo-check
```

## Error Handling

ggen-core follows **fail-fast** validation with detailed error context:

```rust
// Validation errors reported at μ₁
let _ = validator.validate_ontology("schema.ttl")
    .map_err(|e| eprintln!("Validation error: {}", e))?;

// Query errors reported with SPARQL context
let bindings = queryable.select(sparql_str)
    .map_err(|e| eprintln!("Query error: {} in {}", e, sparql_str))?;

// Template errors reported with file context
let output = renderer.render(template_name, context)
    .map_err(|e| eprintln!("Template error in {}: {}", template_name, e))?;
```

## Testing

```bash
# Run all tests
cargo test

# Run with specific filter
cargo test codegen::pipeline

# Run with output
cargo test -- --nocapture

# Run benchmarks
cargo bench
```

## Documentation

- **[Framework Guide](../../docs/ggen-codegen/FRAMEWORK.md)** - Comprehensive architecture and trait documentation
- **[YAWL Rules Reference](../../docs/ggen-yawl/YAWL_RULES.md)** - Specific transformation rules
- **[YAWL Integration Guide](../../docs/ggen-yawl/INTEGRATION.md)** - Spring Boot integration

## Architecture

See [Framework Guide](../../docs/ggen-codegen/FRAMEWORK.md) for detailed architecture diagrams and design patterns.

## Contributing

To add custom rules:

1. Implement `Queryable` for your RDF backend
2. Implement `Renderable` for your template engine
3. Create `Rule<Q, T>` instances with SPARQL and templates
4. Execute through `GenerationPipeline` or standalone

## License

Proprietary - See LICENSE file

## Support

- Issues: GitHub Issues
- Documentation: [docs/ggen-codegen/](../../docs/ggen-codegen/)
- Examples: [examples/](../../examples/)

