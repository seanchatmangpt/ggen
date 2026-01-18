# Rust Turtle/RDF Parser Integration Plan

## Overview

Integration of `rio_turtle` Rust library for production-ready RDF/Turtle parsing in KNHK, replacing the simplified parser currently in `rust/knhk-etl/src/lib.rs`.

## Current State

### Existing Implementation
- **C Code**: Uses Raptor (C library) in `src/rdf.c` for file-based parsing
- **Rust Code**: Simplified Turtle parser in `rust/knhk-etl/src/lib.rs` (`parse_rdf_turtle`)
  - Basic line-by-line parsing
  - Limited Turtle syntax support
  - No prefix resolution
  - No blank node handling
  - No base URI resolution

### CONVO.txt References
- Mentions `rio_api = "0.8"` and `rio_turtle = "0.8"` for AOT dependencies
- Turtle parsing needed for `ttl2csv` conversion tool

## Why `rio_turtle`?

### Advantages
1. **Pure Rust**: No FFI overhead, better integration with Rust codebase
2. **Streaming**: Memory-efficient parsing from `BufRead` implementations
3. **No External Dependencies**: Only uses Rust standard library
4. **Full Turtle Support**: Complete Turtle syntax (prefixes, blank nodes, base URIs)
5. **RDF-star Support**: Supports RDF-star syntaxes
6. **Multiple Formats**: N-Triples, N-Quads, Turtle, TriG
7. **Zero-copy**: Can work with byte slices directly

### Comparison with Alternatives

| Library | Pros | Cons |
|---------|------|------|
| **rio_turtle** | Pure Rust, streaming, no deps, RDF-star | |
| **oxrdfio** | Part of Oxigraph ecosystem | Requires more dependencies |
| **sophia_turtle** | Based on rio_turtle | Additional abstraction layer |
| **harriet** | AST-based, preserves format | More complex for simple use cases |

## Integration Plan

### Phase 1: Add Dependencies

**Files to Update:**
- `rust/knhk-etl/Cargo.toml` - Add `rio_turtle` dependency
- `rust/knhk-aot/Cargo.toml` - Add `rio_turtle` and `rio_api` dependencies (as mentioned in CONVO.txt)

### Phase 2: Replace Simplified Parser

**Files to Update:**
- `rust/knhk-etl/src/lib.rs` - Replace `parse_rdf_turtle()` implementation

**Key Changes:**
1. Use `rio_turtle::TurtleParser` for parsing
2. Handle `rio_api::ParseError` properly
3. Convert `rio_api::Triple` to `RawTriple`
4. Support prefix resolution
5. Handle blank nodes
6. Support base URI resolution

### Phase 3: Integration Points

**Use Cases:**
1. **ETL Pipeline**: `IngestStage::parse_rdf_turtle()` - Replace simplified parser
2. **AOT Tool**: `ttl2csv` conversion tool (mentioned in CONVO.txt)
3. **Connectors**: Kafka connector parsing RDF/Turtle messages
4. **CLI**: `admit` command parsing RDF deltas

### Phase 4: Error Handling

**Requirements:**
- Proper error reporting with line numbers
- Parse error recovery (where possible)
- Invalid syntax handling
- Memory-efficient error handling

### Phase 5: Testing

**Test Cases:**
1. Basic triple parsing
2. Prefix resolution
3. Blank node handling
4. Base URI resolution
5. Literal handling (simple, typed, language-tagged)
6. Error cases (invalid syntax)
7. Large file parsing (streaming)
8. RDF-star syntax (if needed)

## Implementation Details

### Dependencies

```toml
# rust/knhk-etl/Cargo.toml
[dependencies]
rio_turtle = "0.8"
rio_api = "0.8"  # For Triple types and error handling
```

```toml
# rust/knhk-aot/Cargo.toml
[dependencies]
rio_turtle = "0.8"
rio_api = "0.8"
```

### API Design

```rust
// rust/knhk-etl/src/lib.rs

use rio_api::parser::TriplesParser;
use rio_turtle::TurtleParser;
use rio_api::model::{NamedNode, BlankNode, Literal, Triple, Term};

impl IngestStage {
    /// Parse RDF/Turtle content using rio_turtle
    pub fn parse_rdf_turtle(&self, content: &str) -> Result<Vec<RawTriple>, PipelineError> {
        let mut triples = Vec::new();
        let mut parser = TurtleParser::new(content.as_bytes(), None)?;
        
        parser.parse_all(&mut |triple| {
            let raw = Self::convert_triple(triple)?;
            triples.push(raw);
            Ok(())
        })?;
        
        Ok(triples)
    }
    
    /// Convert rio_api::Triple to RawTriple
    fn convert_triple(triple: &Triple) -> Result<RawTriple, PipelineError> {
        Ok(RawTriple {
            subject: Self::term_to_string(triple.subject)?,
            predicate: Self::term_to_string(triple.predicate)?,
            object: Self::term_to_string(triple.object)?,
            graph: None,  // N-Quads support can be added later
        })
    }
    
    /// Convert rio_api::Term to String
    fn term_to_string(term: &Term) -> Result<String, PipelineError> {
        match term {
            Term::NamedNode(named) => Ok(named.iri.to_string()),
            Term::BlankNode(blank) => Ok(format!("_:{}", blank.id)),
            Term::Literal(literal) => {
                match literal {
                    Literal::Simple { value } => Ok(format!("\"{}\"", value)),
                    Literal::LanguageTaggedString { value, language } => {
                        Ok(format!("\"{}\"@{}", value, language))
                    }
                    Literal::Typed { value, datatype } => {
                        Ok(format!("\"{}\"^^{}", value, datatype.iri))
                    }
                }
            }
        }
    }
}
```

### Error Handling

```rust
use rio_api::parser::ParseError;

impl From<ParseError> for PipelineError {
    fn from(err: ParseError) -> Self {
        PipelineError::ParseError(format!(
            "RDF parse error at line {}: {}",
            err.location().line(),
            err.message()
        ))
    }
}
```

### Streaming Support

```rust
use std::io::BufRead;

/// Parse RDF/Turtle from a BufRead stream
pub fn parse_rdf_turtle_stream<R: BufRead>(
    reader: R,
    base_uri: Option<&str>
) -> Result<Vec<RawTriple>, PipelineError> {
    let mut triples = Vec::new();
    let base = base_uri.map(|u| NamedNode::new(u).map_err(|_| PipelineError::InvalidUri)?);
    let mut parser = TurtleParser::new(reader, base.as_ref())?;
    
    parser.parse_all(&mut |triple| {
        triples.push(Self::convert_triple(triple)?);
        Ok(())
    })?;
    
    Ok(triples)
}
```

## Migration Strategy

### Step 1: Add Dependencies
- Update `Cargo.toml` files
- Run `cargo build` to verify dependencies

### Step 2: Implement rio_turtle Parser
- Replace `parse_rdf_turtle()` implementation
- Add conversion functions
- Add error handling

### Step 3: Update Tests
- Update existing tests to use new parser
- Add new test cases for Turtle features

### Step 4: Integration Testing
- Test with existing ETL pipeline
- Test with connectors
- Test with CLI commands

### Step 5: Performance Validation
- Compare performance with Raptor (C)
- Verify memory usage (streaming)
- Validate ≤8 tick budget not affected (parsing is warm path)

## Benefits

1. **Production-Ready**: Full Turtle syntax support
2. **Memory Efficient**: Streaming parser for large files
3. **Pure Rust**: Better integration, no FFI overhead
4. **Maintainable**: Standard Rust error handling
5. **Extensible**: Easy to add N-Quads, TriG support later

## Compatibility

### Backward Compatibility
- `RawTriple` structure remains the same
- `parse_rdf_turtle()` API signature unchanged
- Existing code using `parse_rdf_turtle()` continues to work

### Forward Compatibility
- Can add N-Quads support (graph field)
- Can add TriG support (named graphs)
- Can add RDF-star support (quoted triples)

## Testing Strategy

### Unit Tests
```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_basic_triple() {
        let content = r#"
            <http://example.org/alice> <http://example.org/name> "Alice" .
        "#;
        let result = IngestStage::new(vec![], "turtle".to_string())
            .parse_rdf_turtle(content)
            .unwrap();
        assert_eq!(result.len(), 1);
    }
    
    #[test]
    fn test_prefix() {
        let content = r#"
            @prefix ex: <http://example.org/> .
            ex:alice ex:name "Alice" .
        "#;
        // Test prefix resolution
    }
    
    #[test]
    fn test_blank_node() {
        let content = r#"
            _:alice <http://example.org/name> "Alice" .
        "#;
        // Test blank node handling
    }
}
```

### Integration Tests
- Test with real Turtle files from `tests/data/`
- Test with large files (streaming)
- Test error cases

## Performance Considerations

### Warm Path Operation
- RDF parsing is **warm path** (not hot path)
- No impact on ≤8 tick budget
- Performance target: <500ms for typical files

### Memory Usage
- Streaming parser uses minimal memory
- No need to load entire file into memory
- Suitable for large RDF files

## Next Steps

1. ✅ Add dependencies to `Cargo.toml` files
2. ✅ Implement `rio_turtle` parser
3. ✅ Add error handling
4. ✅ Update tests
5. ✅ Integration testing
6. ✅ Performance validation
7. ✅ Documentation update

## References

- `rio_turtle` docs: https://docs.rs/rio_turtle/
- `rio_api` docs: https://docs.rs/rio_api/
- CONVO.txt references (lines 25518-25563)
- Current simplified parser: `rust/knhk-etl/src/lib.rs` (lines 74-132)

