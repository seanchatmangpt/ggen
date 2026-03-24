# ggen-prompt-mfg

Prompt manufacturing via CONSTRUCT - deterministic compilation from RDF ontologies.

## Overview

This crate implements deterministic prompt compilation from RDF knowledge graphs using SPARQL CONSTRUCT queries. Prompts are not written by hand - they are compiled from ontologies.

**Core Guarantee**: Same input → Same prompt (cryptographically verified)

## Architecture

```
RDF Ontology → CONSTRUCT Query → Prompt IR → Tera Templates → Deterministic Prompt
                                    ↓
                              Hash for deduplication
                              Schema validation
```

### Components

- **`ir`** - Intermediate representation (structured, pre-shaped)
  - Uses `BTreeMap` for deterministic ordering
  - Supports sections, blocks, and variables
  - Serializable for caching

- **`emitter`** - Deterministic Tera-based emission
  - Pre-compiled templates
  - Whitespace normalization
  - Concurrent-safe

- **`validator`** - Schema validation
  - Metadata validation
  - Section structure validation
  - Variable identifier validation

- **`hash`** - Cryptographic hashing (SHA-256)
  - Content-based deduplication
  - Verification of reproducibility
  - IR hash for cache keys

## Usage

```rust
use ggen_prompt_mfg::{PromptCompiler, Result};

fn main() -> Result<()> {
    let compiler = PromptCompiler::new()?;

    // Compile from CONSTRUCT query
    let construct_query = r#"
        CONSTRUCT {
            ?prompt a prompt:Prompt ;
                prompt:hasSection ?section .
            ?section prompt:content ?content .
        }
        WHERE {
            ?prompt a prompt:Prompt ;
                prompt:hasSection ?section .
            ?section prompt:content ?content .
        }
    "#;

    let result = compiler.compile_from_construct(construct_query)?;

    println!("Prompt: {}", result.content());
    println!("Hash: {}", result.hash());

    Ok(())
}
```

## Determinism Guarantees

1. **Stable Ordering**: `BTreeMap` ensures consistent key ordering
2. **Whitespace Normalization**: Trailing spaces and multiple blank lines removed
3. **Template Immutability**: Pre-compiled Tera templates
4. **No Randomness**: No random or time-based generation
5. **Concurrent Safety**: Thread-safe compilation

## Testing

- Unit tests: `cargo test --lib`
- Integration tests: `cargo test --test determinism_tests`
- Property-based tests: Uses `proptest` for fuzzing

All tests verify determinism across:
- Multiple compilations
- Concurrent executions
- Different insertion orders

## Performance

- Compilation: O(n) where n = number of triples
- Hash computation: O(m) where m = prompt length
- Validation: O(s) where s = number of sections

## License

MIT
