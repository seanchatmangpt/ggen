<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [API Reference](#api-reference)
  - [Rust API](#rust-api)
    - [Core Crate: `ggen-core`](#core-crate-ggen-core)
    - [CLI Crate: `ggen-cli`](#cli-crate-ggen-cli)
    - [Domain Crate: `ggen-domain`](#domain-crate-ggen-domain)
  - [Node.js Bindings](#nodejs-bindings)
    - [Installation](#installation)
    - [Usage](#usage)
    - [API Methods](#api-methods)
  - [Python API](#python-api)
    - [Installation](#installation-1)
    - [Usage](#usage-1)
    - [API Functions](#api-functions)
  - [REST API (If Using ggen Server)](#rest-api-if-using-ggen-server)
    - [Endpoints](#endpoints)
  - [Data Types](#data-types)
    - [RdfGraph](#rdfgraph)
    - [Class](#class)
    - [Property](#property)
    - [ValidationResult](#validationresult)
  - [Error Handling](#error-handling)
    - [Rust Error Types](#rust-error-types)
    - [Python Error Types](#python-error-types)
  - [Version Information](#version-information)
    - [Crate Versions](#crate-versions)
    - [Compatibility](#compatibility)
  - [Examples](#examples)
  - [Getting Help](#getting-help)
  - [Summary](#summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# API Reference

Complete reference for ggen's public API and language bindings.

## Rust API

### Core Crate: `ggen-core`

Link to [Rustdoc](https://docs.rs/ggen-core/latest/ggen_core/): Complete API documentation

**Main modules**:

```rust
use ggen_core::rdf::{RdfGraph, RdfStore};
use ggen_core::template::{TemplateEngine, Template};
use ggen_core::models::{Class, Property};
```

**RDF Operations**:
```rust
let store = RdfStore::new();
let graph = store.load_ttl("schema.ttl")?;

// Query classes
let classes = graph.classes();

// Query properties
let properties = graph.properties_for_class("http://example.org/User")?;

// Export to different formats
let json = graph.to_json()?;
let xml = graph.to_xml()?;
```

**Template Generation**:
```rust
let engine = TemplateEngine::new();
let template = Template::load("api-docs.liquid")?;

let output = engine.render(&template, &graph)?;
```

### CLI Crate: `ggen-cli`

Command-line interface built on `ggen-domain`.

### Domain Crate: `ggen-domain`

Business logic for code generation pipelines.

## Node.js Bindings

### Installation

```bash
npm install ggen
```

### Usage

```typescript
import { loadRdf, generateCode } from 'ggen';

// Load ontology
const graph = await loadRdf('schema.ttl');

// Generate TypeScript
const code = await generateCode(graph, {
  language: 'typescript',
  namespace: 'app.models',
});

console.log(code);
```

### API Methods

**`loadRdf(path: string): Promise<RdfGraph>`**
- Load RDF ontology from file
- Returns: RDF graph object

**`generateCode(graph: RdfGraph, options: GenerateOptions): Promise<string>`**
- Generate code from ontology
- Options: language, namespace, output format
- Returns: Generated code as string

**`validateOntology(graph: RdfGraph): Promise<ValidationResult>`**
- Validate RDF ontology
- Returns: Validation result with errors/warnings

## Python API

### Installation

```bash
pip install ggen
```

### Usage

```python
from ggen import load_rdf, generate_code, validate_ontology

# Load ontology
graph = load_rdf('schema.ttl')

# Generate Python
code = generate_code(graph, {
    'language': 'python',
    'namespace': 'app.models',
})

print(code)
```

### API Functions

**`load_rdf(path: str) -> RdfGraph`**
- Load RDF ontology from file
- Returns: RDF graph object

**`generate_code(graph: RdfGraph, options: dict) -> str`**
- Generate code from ontology
- Options: language, namespace, output format
- Returns: Generated code as string

**`validate_ontology(graph: RdfGraph) -> ValidationResult`**
- Validate RDF ontology
- Returns: Validation result with errors/warnings

**`extract_schema(graph: RdfGraph) -> dict`**
- Extract JSON schema from ontology
- Returns: Schema as dictionary

## REST API (If Using ggen Server)

### Endpoints

**`GET /api/v1/ontologies`**
- List available ontologies

**`POST /api/v1/generate`**
- Generate code from ontology
- Body: `{ "ontology": "schema.ttl", "language": "typescript" }`
- Returns: Generated code

**`POST /api/v1/validate`**
- Validate ontology
- Body: `{ "ontology": "schema.ttl" }`
- Returns: Validation result

## Data Types

### RdfGraph

Represents an RDF knowledge graph.

```rust
pub struct RdfGraph {
    pub classes: Vec<Class>,
    pub properties: Vec<Property>,
    pub namespaces: HashMap<String, String>,
}

impl RdfGraph {
    pub fn classes(&self) -> &[Class];
    pub fn properties(&self) -> &[Property];
    pub fn find_class(&self, uri: &str) -> Option<&Class>;
}
```

### Class

Represents an RDF class (ontology entity type).

```rust
pub struct Class {
    pub uri: String,
    pub label: String,
    pub comment: String,
    pub properties: Vec<String>,  // Property URIs
    pub subclass_of: Vec<String>, // Parent classes
}
```

### Property

Represents an RDF property (field/attribute).

```rust
pub struct Property {
    pub uri: String,
    pub label: String,
    pub comment: String,
    pub domain: Vec<String>,      // Classes this applies to
    pub range: String,             // Value type
    pub cardinality: Cardinality,
}

pub enum Cardinality {
    Optional,                // 0..1
    Single,                  // 1
    Multiple,                // 0..*
    OneOrMore,              // 1..*
}
```

### ValidationResult

Result of ontology validation.

```rust
pub struct ValidationResult {
    pub valid: bool,
    pub errors: Vec<ValidationError>,
    pub warnings: Vec<ValidationWarning>,
}

pub struct ValidationError {
    pub code: String,
    pub message: String,
    pub location: Option<Location>,
}

pub struct Location {
    pub file: String,
    pub line: usize,
    pub column: usize,
}
```

## Error Handling

### Rust Error Types

```rust
use ggen_core::error::{GgenError, Result};

match generate_code(&graph) {
    Ok(code) => println!("{}", code),
    Err(GgenError::ParseError(msg)) => eprintln!("Parse error: {}", msg),
    Err(GgenError::GenerationError(msg)) => eprintln!("Generation error: {}", msg),
    Err(GgenError::IoError(err)) => eprintln!("IO error: {}", err),
    Err(e) => eprintln!("Error: {:?}", e),
}
```

### Python Error Types

```python
from ggen import GgenError, ParseError, GenerationError

try:
    code = generate_code(graph)
except ParseError as e:
    print(f"Parse error: {e}")
except GenerationError as e:
    print(f"Generation error: {e}")
except GgenError as e:
    print(f"Error: {e}")
```

## Version Information

### Crate Versions

- `ggen-core`: Latest version available at [crates.io](https://crates.io/crates/ggen-core)
- `ggen-cli`: Latest version available at [crates.io](https://crates.io/crates/ggen-cli)
- `ggen`: NPM package available at [npmjs.com](https://www.npmjs.com/package/ggen)

### Compatibility

Rust binding supports:
- Rust 1.70+
- Edition 2021

Node.js binding supports:
- Node.js 14.0+
- npm 6.0+

Python binding supports:
- Python 3.8+
- pip 20.0+

## Examples

See [Examples](../../examples) directory for complete working examples in multiple languages.

## Getting Help

- Documentation: See [How-to Guides](../how-to-guides/) for detailed usage
- Issues: Report bugs at [GitHub Issues](https://github.com/seanchatmangpt/ggen/issues)
- Discussions: Ask questions at [GitHub Discussions](https://github.com/seanchatmangpt/ggen/discussions)

## Summary

You now have:
- ✅ Complete API reference for all languages
- ✅ Data type definitions
- ✅ Error handling patterns
- ✅ Links to full documentation
- ✅ Examples in multiple languages
