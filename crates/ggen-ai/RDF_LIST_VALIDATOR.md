# RDF List Validator

## Overview

The RDF List Validator is a specialized module for validating RDF list chains to detect malformed rdf:rest* sequences. It ensures that:

1. **Chain Termination**: All lists properly terminate at rdf:nil
2. **Circular References**: No cycles exist in list chains
3. **Depth Limits**: Chains don't exceed maximum depth (preventing infinite loops)
4. **Structural Integrity**: All required properties (rdf:first, rdf:rest) are present

## Architecture

### Module Location
- **Implementation**: `/home/user/ggen/crates/ggen-ai/src/codegen/rdf_list_validator.rs`
- **Unit Tests**: Embedded in rdf_list_validator.rs (20+ test cases)
- **Integration Tests**: `/home/user/ggen/crates/ggen-ai/tests/rdf_list_validator.rs` (10+ test cases)

### Exports
```rust
pub use codegen::{RdfListValidator, ValidationError};
```

## Usage

### Basic Example

```rust
use ggen_ai::RdfListValidator;
use oxigraph::store::Store;

// Create a validator with default max depth (10,000)
let validator = RdfListValidator::new();

// Or with custom max depth
let validator = RdfListValidator::with_max_depth(100);

// Validate a list chain
let result = validator.validate_list(&store, "http://example.com/myList")?;

// Returns ordered list of members as strings
for member in result {
    println!("List member: {}", member);
}
```

### Error Handling

```rust
use ggen_ai::ValidationError;

match validator.validate_list(&store, head_uri) {
    Ok(members) => {
        // List is valid and contains members
        println!("List has {} members", members.len());
    }
    Err(ValidationError::CircularReference { start, cycle_node }) => {
        eprintln!("Cycle detected: {} -> ... -> {} -> {}", start, cycle_node, start);
    }
    Err(ValidationError::MissingNilTerminator { tail }) => {
        eprintln!("List doesn't end at rdf:nil. Last node: {}", tail);
    }
    Err(ValidationError::MaxDepthExceeded { max, actual }) => {
        eprintln!("List exceeds max depth: {} > {}", actual, max);
    }
    Err(ValidationError::MissingFirstProperty { node }) => {
        eprintln!("Node {} missing rdf:first property", node);
    }
    Err(e) => {
        eprintln!("Validation error: {}", e);
    }
}
```

## RDF List Structure

### Valid List

```turtle
@prefix ex: <http://example.com/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

# List: (apple banana cherry)
ex:myList rdf:first "apple" ;
          rdf:rest [
              rdf:first "banana" ;
              rdf:rest [
                  rdf:first "cherry" ;
                  rdf:rest rdf:nil
              ]
          ] .
```

### Invalid List - Missing nil

```turtle
ex:badList1 rdf:first "item1" ;
            rdf:rest ex:badList2 .
ex:badList2 rdf:first "item2" .
# Error: badList2 has no rdf:rest property
```

### Invalid List - Circular Reference

```turtle
ex:cycleA rdf:first "a" ;
          rdf:rest ex:cycleB .
ex:cycleB rdf:first "b" ;
          rdf:rest ex:cycleA .  # Back to start!
# Error: Circular reference detected
```

## Validation Errors

The validator returns structured error types:

### ValidationError::CircularReference
- **Fields**: `start: String`, `cycle_node: String`
- **Meaning**: Cycle detected in chain - would loop infinitely
- **Example**: "Circular reference in RDF list: list1 -> ... -> list2 (back to list1)"

### ValidationError::MissingNilTerminator
- **Fields**: `tail: String`
- **Meaning**: Chain ends at a node that has no rdf:rest property
- **Example**: "RDF list does not terminate at rdf:nil. Last node: list2"

### ValidationError::InvalidRestProperty
- **Fields**: `node: String`, `reason: String`
- **Meaning**: rdf:rest property is invalid or missing
- **Example**: "Invalid rdf:rest property at list1: multiple rdf:rest properties found"

### ValidationError::MaxDepthExceeded
- **Fields**: `max: usize`, `actual: usize`
- **Meaning**: Chain length exceeds maximum depth limit
- **Example**: "RDF list exceeds maximum depth: 150 (limit: 100)"

### ValidationError::MissingFirstProperty
- **Fields**: `node: String`
- **Meaning**: List node missing rdf:first value
- **Example**: "RDF list node list1 missing rdf:first property"

## Integration with SHACL Parser

The validator should be used in the SHACL parser before extracting list values:

```rust
use ggen_ai::RdfListValidator;

// In SHACLParser::extract_enum_values()
let validator = RdfListValidator::new();

// Validate the list chain before extracting values
let members = validator.validate_list(self.store, list_uri)?;

// Now safely iterate over validated members
for member in members {
    // Process validated member
    values.push(member);
}
```

## Testing

### Unit Tests (embedded in module)
- 20+ test cases covering all validation scenarios
- Chicago TDD pattern (Arrange-Act-Assert)
- Real RDF objects using oxigraph Store

### Integration Tests
**Location**: `/home/user/ggen/crates/ggen-ai/tests/rdf_list_validator.rs`

Tests include:
1. Empty list validation
2. Single and multiple element lists
3. Circular reference detection (2-node and 3-node cycles, self-loops)
4. Missing nil terminator
5. Max depth exceeded
6. URI and literal values
7. Unicode characters (international text, emojis)
8. Duplicate values
9. Long lists (50+ elements)

### Running Tests

```bash
# Run all unit tests
cargo test --lib codegen::rdf_list_validator

# Run integration tests
cargo test --test rdf_list_validator

# Run specific test
cargo test test_integration_circular_reference_detection
```

## Performance Characteristics

- **Time Complexity**: O(n) where n = list length
- **Space Complexity**: O(n) for visited node tracking + result vector
- **Cycle Detection**: Efficient HashSet-based tracking
- **Max Depth Protection**: Prevents infinite loops on malformed data

## API Documentation

### RdfListValidator

```rust
pub struct RdfListValidator {
    max_depth: usize,
}

impl RdfListValidator {
    /// Create validator with default max depth (10,000)
    pub fn new() -> Self;

    /// Create validator with custom max depth
    pub fn with_max_depth(max_depth: usize) -> Self;

    /// Validate list starting from head node
    /// Returns ordered vector of member URIs/literals or ValidationError
    pub fn validate_list(
        &self,
        store: &Store,
        head_uri: &str,
    ) -> std::result::Result<Vec<String>, ValidationError>;
}
```

### ValidationError

```rust
pub enum ValidationError {
    CircularReference { start: String, cycle_node: String },
    MissingNilTerminator { tail: String },
    InvalidRestProperty { node: String, reason: String },
    MaxDepthExceeded { max: usize, actual: usize },
    MissingFirstProperty { node: String },
    InvalidListHead { node: String, reason: String },
    QueryError { message: String },
}
```

## Constants

```rust
const RDF_FIRST: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#first";
const RDF_REST: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest";
const RDF_NIL: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil";
```

## Future Enhancements

1. **Caching**: Cache validation results for frequently checked lists
2. **Partial Validation**: Option to validate only first N elements
3. **Detailed Diagnostics**: Return path to cycle or error location
4. **Async Support**: Non-blocking validation for large lists
5. **Batch Validation**: Validate multiple lists in one operation

## Dependencies

- `oxigraph 0.5.1`: RDF store and SPARQL engine
- `thiserror`: Error type derivation
- Standard library collections (HashSet)

## Related Modules

- **SHACL Parser** (`shacl_parser.rs`): Uses validator for list extraction
- **RDF Module** (`rdf/`): Provides RDF graph utilities
- **Error Handling** (`error.rs`): Global error types and conversions

## Examples

### Example 1: Validating SHACL Enumeration

```rust
use ggen_ai::{RdfListValidator, SHACLParser};
use oxigraph::store::Store;

let store = Store::new()?;
let validator = RdfListValidator::new();
let parser = SHACLParser::new(&store);

// Extract SHACL property shape
let constraints = parser.extract_constraint("http://example.com/EmailProperty", "email")?;

// Validate the enumeration list before using values
if let Some(enum_list_uri) = &constraints.enum_values {
    match validator.validate_list(&store, enum_list_uri) {
        Ok(values) => {
            println!("Valid enum values: {:?}", values);
        }
        Err(e) => {
            eprintln!("Invalid list: {}", e);
        }
    }
}
```

### Example 2: Safe List Processing

```rust
let validator = RdfListValidator::with_max_depth(1000);

match validator.validate_list(&store, list_head) {
    Ok(members) => {
        for (index, member) in members.iter().enumerate() {
            println!("Item {}: {}", index, member);
        }
    }
    Err(ValidationError::CircularReference { start, cycle_node }) => {
        // Handle cycle - may indicate corrupted RDF
        eprintln!("WARNING: Corrupted list structure detected");
    }
    Err(e) => {
        // Handle other validation errors
        return Err(e.into());
    }
}
```

## Test Coverage

**Mutation Score Target**: > 90%

The test suite is designed to catch mutations and verify:
- All error paths are executed
- Boundary conditions (empty list, single element)
- Error conditions (cycles, missing properties)
- Unicode handling
- Large data sets

## Architecture Decisions

1. **No Caching**: Validator keeps state minimal for thread safety
2. **SPARQL Queries**: Uses oxigraph's stable SPARQL interface
3. **String Representation**: Returns list members as strings for flexibility
4. **Custom Errors**: Structured ValidationError for better error handling
5. **Max Depth Default**: 10,000 is reasonable for production use

## Contributing

When modifying the validator:

1. Maintain Chicago TDD pattern for tests
2. Add test cases for any new error variants
3. Update documentation with examples
4. Ensure mutation score remains > 90%
5. Run integration tests: `cargo test --test rdf_list_validator`

## File Manifest

```
crates/ggen-ai/src/codegen/
├── rdf_list_validator.rs         # Main validator implementation
└── mod.rs                        # Module exports

crates/ggen-ai/tests/
├── rdf_list_validator.rs         # Integration tests

crates/ggen-ai/
├── src/lib.rs                    # Public API exports
└── RDF_LIST_VALIDATOR.md         # This file
```
