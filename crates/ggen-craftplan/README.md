# ggen-craftplan: RDF → Elixir Code Generation

Deterministic, type-safe code generation pipeline for Craftplan ERP.

## Overview

ggen-craftplan implements a five-stage transformation pipeline (μ) that converts RDF ontology specifications into production-ready Elixir code for the Craftplan ERP system.

**Transformation Equation**: A = μ(O)

Where:
- A = Generated Elixir code (Ash resources, LiveViews, contexts)
- μ = Five-stage transformation pipeline
- O = RDF ontology (immutable source of truth)

## Pipeline Stages

1. **μ₁ (Normalize)**: RDF validation, SHACL shapes, dependency resolution
2. **μ₂ (Extract)**: SPARQL queries, OWL inference, entity extraction
3. **μ₃ (Emit)**: Tera template rendering, Elixir code generation
4. **μ₄ (Canonicalize)**: Deterministic formatting, content hashing
5. **μ₅ (Receipt)**: Cryptographic proof generation, audit trail

## Usage

```rust
use ggen_craftplan::pipeline::CodeGenerator;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let generator = CodeGenerator::new("output")?;
    let receipt = generator.generate_from_rdf("ontology.ttl")?;
    
    println!("Generated {} files", receipt.metadata.file_count);
    Ok(())
}
```

## Architecture

- **Type-Safe**: Result<T, E> throughout, no unwrap/expect in production
- **Deterministic**: Same RDF → identical Elixir code (byte-for-byte)
- **Auditable**: Cryptographic receipts verify reproducibility

## Templates

The generator uses Tera templates to produce:

- **Ash Resources**: Database-backed entities with validations
- **Context Modules**: Business logic and data access
- **LiveViews**: Phoenix LiveView UI components

## Development

### Build

```bash
cargo build
```

### Test

```bash
cargo test
```

### Lint

```bash
cargo clippy -- -D warnings
```

## License

MIT
