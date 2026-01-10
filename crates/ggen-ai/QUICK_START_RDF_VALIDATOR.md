# RDF List Validator - Quick Start Guide

## 30-Second Overview

The `RdfListValidator` validates RDF list chains to ensure:
1. All lists terminate at `rdf:nil`
2. No circular references exist
3. Chains don't exceed max depth
4. All required properties are present

## Installation (Already Integrated)

The validator is exported from the main crate:

```rust
use ggen_ai::{RdfListValidator, ValidationError};
```

## Basic Usage

```rust
use ggen_ai::RdfListValidator;
use oxigraph::store::Store;

// Create store and validator
let store = Store::new()?;
let validator = RdfListValidator::new();

// Validate a list
let head_uri = "http://example.com/myList";
match validator.validate_list(&store, head_uri) {
    Ok(members) => {
        for member in members {
            println!("{}", member);
        }
    }
    Err(e) => eprintln!("Validation failed: {}", e),
}
```

## Error Handling Patterns

### Handle Specific Errors
```rust
use ggen_ai::ValidationError;

match validator.validate_list(&store, head_uri) {
    Ok(members) => { /* Process members */ }
    Err(ValidationError::CircularReference { start, cycle_node }) => {
        eprintln!("Cycle detected at: {}", cycle_node);
    }
    Err(ValidationError::MissingNilTerminator { tail }) => {
        eprintln!("List doesn't end: {}", tail);
    }
    Err(ValidationError::MaxDepthExceeded { max, actual }) => {
        eprintln!("Too deep: {} > {}", actual, max);
    }
    Err(e) => eprintln!("Error: {}", e),
}
```

### Generic Error Handling
```rust
if let Err(e) = validator.validate_list(&store, head_uri) {
    return Err(e.into()); // Converts to GgenAiError
}
```

## Common Scenarios

### Validate SHACL Enumeration Before Extraction
```rust
use ggen_ai::RdfListValidator;

let validator = RdfListValidator::new();

// Before using enum values in SHACL parser:
let enum_members = validator.validate_list(&store, enum_list_uri)?;

// Now safe to use:
for value in enum_members {
    // Process validated value
}
```

### Custom Max Depth
```rust
// For smaller lists, use smaller max depth
let validator = RdfListValidator::with_max_depth(100);

// For very large lists:
let validator = RdfListValidator::with_max_depth(50000);
```

### Batch Validation
```rust
let validator = RdfListValidator::new();

let lists = vec![
    "http://example.com/list1",
    "http://example.com/list2",
    "http://example.com/list3",
];

for list_uri in lists {
    match validator.validate_list(&store, list_uri) {
        Ok(members) => println!("✓ {} valid, {} members", list_uri, members.len()),
        Err(e) => println!("✗ {} failed: {}", list_uri, e),
    }
}
```

## RDF Examples

### Valid RDF List
```turtle
@prefix ex: <http://example.com/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:colors rdf:first "red" ;
          rdf:rest [
              rdf:first "green" ;
              rdf:rest [
                  rdf:first "blue" ;
                  rdf:rest rdf:nil
              ]
          ] .
```

### Invalid: Missing nil
```turtle
ex:badList rdf:first "a" ;
           rdf:rest ex:badList2 .
ex:badList2 rdf:first "b" .
# Error: Missing rdf:nil terminator
```

### Invalid: Circular
```turtle
ex:cycleA rdf:first "x" ;
          rdf:rest ex:cycleB .
ex:cycleB rdf:first "y" ;
          rdf:rest ex:cycleA .
# Error: Circular reference
```

## API Reference

### RdfListValidator Methods

```rust
// Create with default max depth (10,000)
let validator = RdfListValidator::new();

// Create with custom max depth
let validator = RdfListValidator::with_max_depth(500);

// Validate a list chain
// Returns: Result<Vec<String>, ValidationError>
let members = validator.validate_list(&store, "http://example.com/list")?;
```

### ValidationError Variants

| Error | Fields | Meaning |
|-------|--------|---------|
| `CircularReference` | `start`, `cycle_node` | Cycle in chain |
| `MissingNilTerminator` | `tail` | No rdf:nil at end |
| `InvalidRestProperty` | `node`, `reason` | Bad rdf:rest |
| `MaxDepthExceeded` | `max`, `actual` | Chain too long |
| `MissingFirstProperty` | `node` | No rdf:first |
| `InvalidListHead` | `node`, `reason` | Bad head |
| `QueryError` | `message` | RDF store error |

## Testing

### Run Unit Tests
```bash
cargo test --lib codegen::rdf_list_validator
```

### Run Integration Tests
```bash
cargo test --test rdf_list_validator
```

### Run Specific Test
```bash
cargo test test_integration_circular_reference_detection
```

## Performance Tips

1. **Reuse Validator**: Create once, use multiple times
   ```rust
   let validator = RdfListValidator::new();
   // Validate many lists with same validator
   ```

2. **Set Appropriate Max Depth**: Use smaller values for expected limits
   ```rust
   // For typical SHACL enums (< 50 items):
   let validator = RdfListValidator::with_max_depth(100);
   ```

3. **Early Error Handling**: Don't process if validation fails
   ```rust
   let members = validator.validate_list(&store, uri)?;
   // Only reaches here if valid
   ```

## Troubleshooting

### "Circular reference detected"
Your RDF data has a cycle. Check for self-referencing nodes.

### "RDF list does not terminate at rdf:nil"
A list node has no `rdf:rest` property. Add proper termination.

### "RDF list exceeds maximum depth"
List is very long. Use `with_max_depth()` with higher limit, or check for infinite loops.

### "Missing rdf:first property"
List node is malformed. Ensure each node has both `rdf:first` and `rdf:rest`.

## Integration Checklist

When integrating with SHACL parser:

- [ ] Import: `use crate::codegen::RdfListValidator;`
- [ ] Create validator: `let validator = RdfListValidator::new();`
- [ ] Add validation before list extraction
- [ ] Handle ValidationError appropriately
- [ ] Test with valid and invalid lists
- [ ] Update documentation

## Common Integration Points

### In SHACL Parser
```rust
// Before extract_enum_values()
let validator = RdfListValidator::new();
let enum_members = validator.validate_list(self.store, list_uri)?;
```

### In List Extraction
```rust
// Validate before returning members
let members = validator.validate_list(&store, head_uri)?;
constraint.enum_values = Some(members);
```

### In Error Handling
```rust
// Convert ValidationError to GgenAiError
match validator.validate_list(&store, uri) {
    Ok(members) => Ok(members),
    Err(e) => Err(e.into()), // Converts using From trait
}
```

## Related Documentation

- Full API docs: `/home/user/ggen/crates/ggen-ai/RDF_LIST_VALIDATOR.md`
- Implementation details: `/home/user/ggen/IMPLEMENTATION_SUMMARY_RDF_LIST_VALIDATOR.md`
- SHACL parser usage: `crates/ggen-ai/src/codegen/shacl_parser.rs`

## Example Files

- Core implementation: `/home/user/ggen/crates/ggen-ai/src/codegen/rdf_list_validator.rs`
- Integration tests: `/home/user/ggen/crates/ggen-ai/tests/rdf_list_validator.rs`

## Key Takeaways

1. **Simple API**: Create validator → call `validate_list()` → handle Result
2. **Structured Errors**: Specific error types for better error handling
3. **Fast Cycle Detection**: O(1) per node with HashSet
4. **Safe Defaults**: 10,000 max depth prevents most infinite loops
5. **Chicago TDD**: All 30+ tests use real objects, no mocks
6. **Production Ready**: Zero unsafe code, comprehensive error handling

## Support

For issues or questions:
1. Check `RDF_LIST_VALIDATOR.md` for detailed documentation
2. Review test examples in `tests/rdf_list_validator.rs`
3. See integration guidance in SHACL parser module
