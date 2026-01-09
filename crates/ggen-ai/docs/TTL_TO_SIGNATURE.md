# TTL to Signature Transpiler

## Overview

The TTL to Signature Transpiler (located in `src/codegen/ttl_to_signature.rs`) is a Rust port of the Python `ttl2dspy.py` transpiler. It converts RDF ontologies defined in Turtle format with SHACL shapes into type-safe DSPy Signature specifications.

This module implements a **core traversal and shape extraction logic** that:

- Discovers all RDF classes with SHACL shapes
- Traverses SHACL property shapes and extracts constraints
- Distinguishes input vs output fields via `cns:outputField` annotations
- Maps XSD datatypes to Rust types
- Generates safe, collision-free field names
- Builds complete Signature objects with proper error handling

## Architecture

### Core Components

#### `TTLToSignatureTranspiler`

The main struct that orchestrates the transpilation process:

```rust
pub struct TTLToSignatureTranspiler {
    /// Track seen field names for collision detection
    seen_field_names: HashSet<String>,
    /// Count of generated signatures
    signature_count: usize,
}
```

Key methods:
- `new()` - Create a new transpiler instance
- `build_signatures(&mut self, store: &Store) -> Result<Vec<Signature>>` - Main entry point
- `find_classes_with_shapes()` - Discover SHACL target classes
- `find_property_shapes()` - Extract property shapes for a class
- `safe_local_name()` - Extract IRI local names safely
- `snake_case()` - Convert names to Rust identifiers
- `check_field_collision()` - Handle naming collisions
- `extract_datatype()` - Map XSD types to Rust types

#### `PropertyShape`

Extracted SHACL property shape metadata:

```rust
pub struct PropertyShape {
    pub iri: String,           // IRI of the shape
    pub path: String,          // Property path
    pub description: Option<String>,  // Field documentation
    pub datatype: Option<String>,     // XSD datatype
    pub is_output: bool,       // Output field marker
}
```

### Transpilation Pipeline

The transpiler follows a 5-stage pipeline:

1. **Class Discovery**: Find all classes with `sh:targetClass` declarations
2. **Property Extraction**: Query SHACL property shapes (2 patterns supported)
3. **Field Classification**: Distinguish inputs vs outputs via `cns:outputField`
4. **Naming Normalization**: Convert IRIs and names to valid Rust identifiers
5. **Signature Assembly**: Build Signature objects with inputs and outputs

## Features

### SHACL Shape Support

The transpiler supports both SHACL patterns:

**Pattern 1: Direct Property Shapes**
```turtle
?propShape sh:targetClass :Class ;
           sh:path :property .
```

**Pattern 2: Node Shapes with Property Links**
```turtle
?nodeShape sh:targetClass :Class ;
           sh:property ?propShape .
?propShape sh:path :property .
```

### Field Classification

Fields are classified as output fields when:

1. They have `cns:outputField "true"` annotation, or
2. Their description contains "output" (case-insensitive)

Example:
```turtle
:EmailProperty
    sh:path :email ;
    cns:outputField "true" ;
    rdfs:comment "Email address (output)" .
```

### Type Mapping

XSD datatypes are automatically mapped to Rust types:

| XSD Type | Rust Type |
|----------|-----------|
| xsd:string | String |
| xsd:integer, xsd:int, xsd:long | i32 |
| xsd:boolean | bool |
| xsd:float, xsd:double, xsd:decimal | f32 |
| (unknown) | String (default) |

### Safe Naming

The transpiler ensures valid Rust identifiers:

1. **Local Name Extraction**: Converts `http://.../#ClassName` → `ClassName`
2. **Snake Case**: `MyPropertyName` → `my_property_name`
3. **Reserved Name Avoidance**: `class` → `custom_class`
4. **Collision Detection**: Multiple `field` → `field`, `field_1`, `field_2`
5. **Numeric Prefix Handling**: `1stProperty` → `field_1st_property`

## Usage

### Basic Example

```rust
use ggen_ai::codegen::TTLToSignatureTranspiler;
use oxigraph::store::Store;
use std::path::Path;

// Create transpiler
let mut transpiler = TTLToSignatureTranspiler::new();

// Load RDF/TTL into store
let mut store = Store::new()?;
let file = std::fs::File::open("ontology.ttl")?;
let reader = std::io::BufReader::new(file);
store.load_from_reader(oxigraph::io::RdfFormat::Turtle, reader)?;

// Build signatures
let signatures = transpiler.build_signatures(&store)?;

// Use signatures
for sig in signatures {
    println!("Generated: {} with {} inputs, {} outputs",
        sig.name,
        sig.inputs.len(),
        sig.outputs.len()
    );
}
```

### Working with Individual Methods

```rust
// Find classes with SHACL shapes
let classes = transpiler.find_classes_with_shapes(&store)?;

// Find property shapes for a specific class
let props = transpiler.find_property_shapes("http://example.com/Person", &store)?;

// Convert names
let snake = transpiler.snake_case("MyProperty");  // "my_property"
let local = transpiler.safe_local_name("http://ex.com#Class");  // "Class"
```

## Error Handling

All fallible operations return `Result<T, GgenAiError>`:

```rust
pub type Result<T> = std::result::Result<T, GgenAiError>;
```

The transpiler gracefully handles:

- Empty RDF stores (no classes found)
- Missing property descriptions (uses default text)
- Malformed IRIs (returns as-is if no delimiter found)
- No output fields (adds default `result` output)
- SPARQL query failures (returns empty results)

No `unwrap()` or `expect()` calls in production code.

## Testing

Comprehensive test suite in `tests/ttl_to_signature_integration.rs`:

### Unit Tests (in module)

```rust
#[test]
fn test_safe_local_name_with_hash() { ... }

#[test]
fn test_snake_case_with_camel_case() { ... }

#[test]
fn test_check_field_collision_handles_duplicates() { ... }
```

### Integration Tests

- Empty store handling
- TTL loading and parsing
- Complex naming scenarios
- Collision detection with edge cases
- Multiple property shape patterns
- Output field classification

Run tests with:
```bash
cargo test --test ttl_to_signature_integration
cargo test --lib codegen::ttl_to_signature
```

## Example TTL Ontology

```turtle
@prefix : <http://example.com/test/> .
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix cns: <http://cns.io/ontology#> .

# Define a class with SHACL constraints
:PersonShape
    a sh:NodeShape ;
    sh:targetClass :Person ;
    sh:property :PersonNameProperty ;
    sh:property :PersonAgeProperty ;
    sh:property :PersonEmailProperty .

# Define individual properties
:PersonNameProperty
    sh:path :name ;
    sh:datatype xsd:string ;
    rdfs:comment "Person's full name" .

:PersonAgeProperty
    sh:path :age ;
    sh:datatype xsd:integer ;
    rdfs:comment "Person's age in years" .

:PersonEmailProperty
    sh:path :email ;
    sh:datatype xsd:string ;
    cns:outputField "true" ;
    rdfs:comment "Email address (output field)" .
```

This generates:
```rust
Signature::new("PersonSignature", "DSPy Signature for Person")
    .with_input(InputField::new("name", "Person's full name", "String"))
    .with_input(InputField::new("age", "Person's age in years", "i32"))
    .with_output(OutputField::new("email", "Email address (output field)", "String"))
```

## Key Differences from Python

| Aspect | Python | Rust |
|--------|--------|------|
| Error Handling | Exceptions | `Result<T, E>` |
| Mutability | Implicit | Explicit `&mut self` |
| Collections | Dynamic | Type-safe `HashSet` |
| String Handling | Built-in methods | Regex crate |
| Type System | Duck typing | Static types |
| Null Safety | None type | `Option<T>` |

## Dependencies

```toml
oxigraph = "0.5"      # RDF store and SPARQL
regex = "latest"      # Pattern matching for naming
serde = "latest"      # Serialization (for Signature)
```

## Performance Characteristics

- **Time Complexity**: O(n*m) where n = classes, m = properties per class
- **Space Complexity**: O(n*m) for storing all signatures
- **Blocking**: No async/concurrency overhead
- **Memory**: Suitable for ontologies with <1000 classes

## Limitations

1. **Single Output Field**: Default behavior if none specified
2. **No Multiple Output Fields**: Follows DSPy convention (one output)
3. **Simple Type Mapping**: Only XSD types supported
4. **No Validation Rules**: SHACL constraints extracted but not enforced
5. **No Recursive Shapes**: Only direct class → properties

## Future Enhancements

- [ ] Support for multiple output fields via `--allow-multi-output`
- [ ] Custom type mapping configuration
- [ ] SHACL constraint preservation (minCount, maxLength, etc.)
- [ ] Class inheritance traversal
- [ ] JSON-LD context support
- [ ] Performance optimization with caching

## Related Code

- **DSPy Module**: `src/dspy/signature.rs` - Signature type definition
- **RDF Module**: `src/rdf/parser.rs` - TTL file loading
- **Error Module**: `src/error.rs` - Error handling
- **Python Original**: `docs/archive/academic/.../ttl2dspy.py`

## Examples

See `tests/ttl_to_signature_integration.rs` for:
- Basic transpiler creation
- IRI extraction examples
- Naming collision scenarios
- Datatype mapping verification
- Complex TTL structure testing
