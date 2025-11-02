# RDF Module Structure - ggen-ai

## Overview

The RDF module in `ggen-ai` provides functionality to parse RDF/RDFS descriptions of CLI applications and generate complete Rust projects using the Clap library with the noun-verb command pattern.

## Module Structure

```
ggen-ai/src/rdf/
├── mod.rs          # Module exports and documentation
├── types.rs        # Core type definitions (CliProject, Noun, Verb, etc.)
├── parser.rs       # RDF/TTL file parsing using oxigraph
├── query.rs        # SPARQL query execution and data extraction
├── renderer.rs     # Tera template rendering for code generation
├── template.rs     # Template management utilities
└── generator.rs    # High-level orchestration pipeline
```

## Exported Types

All types are re-exported from `ggen_ai` root for convenience:

```rust
use ggen_ai::{
    Argument,         // Command-line argument definition
    ArgumentType,     // Argument type information
    CliProject,       // Complete CLI project structure
    Dependency,       // Cargo dependency definition
    Noun,             // CLI noun (resource/entity)
    TemplateRenderer, // Template rendering engine
    Validation,       // Argument validation rule
    Verb,             // CLI verb (action on noun)
};
```

## Usage Example

```rust
use ggen_ai::rdf::{CliGenerator, TemplateRenderer};
use std::path::{Path, PathBuf};

// Create a CLI generator
let generator = CliGenerator::new(PathBuf::from("templates"));

// Generate from TTL file
generator.generate_from_ttl(
    Path::new("sample-cli.ttl"),
    Path::new("output")
)?;
```

## Build Status

- ✅ Library compiles successfully
- ✅ Module exports work correctly
- ✅ Documentation generates successfully
- ⚠️  Some tests have compilation errors (added by linter, need fixing)
- ⚠️  Uses deprecated oxigraph API (warnings only)

## Integration with ggen-cli-lib

The RDF types and functionality are available through `ggen-ai` which is a dependency of `ggen-cli-lib`, making them accessible throughout the ggen project.

```rust
// In ggen-cli-lib or other workspace members
use ggen_ai::rdf::{CliProject, Noun, Verb};
```

## Files Created/Modified

### Created
- `ggen-ai/src/rdf/mod.rs` - Module structure and exports
- `ggen-ai/src/rdf/types.rs` - Type definitions
- `ggen-ai/src/rdf/parser.rs` - RDF parser implementation
- `ggen-ai/src/rdf/query.rs` - SPARQL query executor
- `ggen-ai/src/rdf/renderer.rs` - Template renderer
- `ggen-ai/src/rdf/generator.rs` - High-level generator
- `ggen-ai/src/rdf/template.rs` - Template utilities
- `ggen-ai/tests/rdf_basic_test.rs` - Basic integration test
- `ggen-ai/tests/rdf_module_test.rs` - Module export test

### Modified
- `ggen-ai/src/lib.rs` - Added `pub mod rdf` and re-exports

## Next Steps

1. Fix test compilation errors in renderer.rs
2. Update oxigraph API usage to remove deprecation warnings
3. Add comprehensive unit tests for each module
4. Create example TTL files and templates
5. Document the complete generation pipeline
