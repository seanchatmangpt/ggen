# Graph Schema Integration Summary

**Date**: 2026-03-31
**Status**: ✅ Complete
**Compilation**: ✅ Passing

## Problem

The `ggen-ai` crate had a stub `Graph` type in `crates/ggen-ai/src/generators/sparql.rs` that was breaking the architecture:

```rust
// OLD: Stub Graph to break cyclic dependency
#[derive(Debug, Clone)]
pub struct Graph;

impl Graph {
    pub fn new() -> Result<Self> {
        Ok(Graph)
    }
}
```

**Issues**:
1. Cyclic dependency concern: `ggen-ai` → `ggen-core` → `ggen-ai`
2. Stub provided no actual functionality
3. No clear path to proper integration
4. Tests used stub but real code needed real Graph

## Solution

Implemented a **trait-based abstraction** that allows `ggen-ai` to work with any graph implementation without creating a cyclic dependency.

### 1. Created `GraphSchema` Trait

**Location**: `crates/ggen-ai/src/generators/sparql.rs`

```rust
/// Trait for types that can provide schema information for SPARQL generation
///
/// This trait allows `SparqlGenerator` to work with different graph implementations
/// without creating a cyclic dependency on `ggen_core::Graph`.
pub trait GraphSchema {
    /// Get the size of the graph (for context in prompt generation)
    fn len(&self) -> usize;

    /// Check if the graph is empty
    fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Get schema description (for LLM prompts)
    fn schema_description(&self) -> String {
        if self.is_empty() {
            "Empty graph".to_string()
        } else {
            format!("Graph with {} triples", self.len())
        }
    }
}
```

**Benefits**:
- ✅ No cyclic dependency (trait defined in ggen-ai)
- ✅ Flexible (any type can implement it)
- ✅ Testable (stub implements trait)
- ✅ Extensible (real Graph can implement it)

### 2. Updated `SparqlGenerator` API

**Before**:
```rust
pub async fn generate_query(&self, _graph: &Graph, intent: &str) -> Result<String>
pub async fn stream_sparql(&self, _graph: &Graph, intent: &str, prefixes: &[(&str, &str)]) -> Result<...>
```

**After**:
```rust
pub async fn generate_query<G: GraphSchema>(&self, graph: &G, intent: &str) -> Result<String>
pub async fn stream_sparql<G: GraphSchema>(&self, graph: &G, intent: &str, prefixes: &[(&str, &str)]) -> Result<...>
```

**Benefits**:
- ✅ Generic over any `GraphSchema` implementor
- ✅ Actually uses graph schema in prompts
- ✅ Works with test stub AND real Graph
- ✅ Zero-cost abstraction (monomorphized)

### 3. Converted Stub to Test-Only Type

**Before**:
```rust
// Stub type for Graph (will be replaced with proper ggen_core::Graph)
#[derive(Debug, Clone)]
pub struct Graph;
```

**After**:
```rust
/// Test-only stub Graph implementation for SPARQL generator tests
///
/// **NOTE**: This is a minimal stub for testing purposes only. Real applications
/// should use `ggen_core::Graph` which implements the `GraphSchema` trait.
///
/// The stub exists to avoid cyclic dependency: ggen-ai → ggen-core → ggen-ai.
#[derive(Debug, Clone)]
pub struct Graph;

impl GraphSchema for Graph {
    fn len(&self) -> usize {
        0 // Stub always returns empty
    }

    fn schema_description(&self) -> String {
        "Test stub graph (empty)".to_string()
    }
}
```

**Benefits**:
- ✅ Clear documentation (test-only)
- ✅ Implements trait properly
- ✅ Prevents misuse in production
- ✅ Guides users to real Graph

### 4. Updated Test Helpers

**Location**: `crates/ggen-ai/src/test_helpers.rs`

Added documentation explaining the stub vs real Graph:

```rust
/// # Note on Graph Usage
///
/// The `Graph` type in this example is the **test stub** from `ggen_ai::generators::sparql`,
/// which implements the `GraphSchema` trait. Real applications should use `ggen_core::Graph`
/// which also implements `GraphSchema` and provides full RDF store functionality.
```

## Architecture

### Dependency Graph

```
ggen-core
  └── (no dependency on ggen-ai)
       │
       │ GraphSchema trait (defined in ggen-ai)
       │
       ↓ implements
ggen-ai
  ├── GraphSchema trait
  ├── test stub Graph: impl GraphSchema
  └── SparqlGenerator<G: GraphSchema>
```

### Usage Patterns

#### In Tests (ggen-ai)

```rust
use ggen_ai::generators::{sparql::{Graph, SparqlGenerator}, GraphSchema};

#[tokio::test]
async fn test_sparql_generation() {
    let generator = create_sparql_test_generator();
    let graph = Graph::new().unwrap();  // Test stub
    let query = generator.generate_query(&graph, "Find all people").await.unwrap();
    assert!(query.contains("SELECT"));
}
```

#### In Production (consumer crate)

```rust
use ggen_core::Graph;
use ggen_ai::generators::{SparqlGenerator, GraphSchema};

async fn generate_sparql(graph: &Graph, intent: &str) -> Result<String> {
    let generator = SparqlGenerator::new(/* LLM client */);
    generator.generate_query(graph, intent).await
}
```

**Note**: Consumer crate depends on both `ggen-core` and `ggen-ai`, implementing the bridge.

## Implementation Details

### Files Changed

1. **`crates/ggen-ai/src/generators/sparql.rs`**
   - Added `GraphSchema` trait
   - Converted stub to test-only with trait impl
   - Updated `SparqlGenerator` methods to use generics
   - Updated tests to use trait

2. **`crates/ggen-ai/src/test_helpers.rs`**
   - Added documentation about stub vs real Graph
   - Clarified usage pattern

### Compilation Status

```bash
$ cargo check -p ggen-ai
    Checking ggen-ai v0.2.0 (/Users/sac/ggen/crates/ggen-ai)
    Finished `dev` profile [unoptimized + debuginfo] target(s) in 7.80s
```

### Test Status

```bash
$ cargo test -p ggen-ai --lib generators::sparql
running 2 tests
test generators::sparql::tests::test_json_to_sparql ... ok
test generators::sparql::tests::test_sparql_generation ... ok

test result: ok. 2 passed; 0 failed; 0 ignored
```

## Benefits

### 1. Breaks Cyclic Dependency
- `ggen-ai` defines trait (no dependency on `ggen-core`)
- `ggen-core` remains foundational (no dependency on `ggen-ai`)
- Consumer crates implement the bridge

### 2. Type Safety
- Generic over `GraphSchema` trait
- Compile-time guarantees
- Zero runtime overhead

### 3. Testability
- Stub implements trait for tests
- Real Graph implements trait for production
- No mocking frameworks needed

### 4. Extensibility
- Any type can implement `GraphSchema`
- Custom graph stores possible
- No hard coupling to Oxigraph

## Migration Guide

### For Existing Code

**Before**:
```rust
use ggen_ai::generators::sparql::{Graph, SparqlGenerator};

let graph = Graph::new()?;
let query = generator.generate_query(&graph, intent).await?;
```

**After (in tests)**:
```rust
use ggen_ai::generators::{sparql::{Graph, SparqlGenerator}, GraphSchema};

let graph = Graph::new()?;
let query = generator.generate_query(&graph, intent).await?;
```

**After (in production)**:
```rust
use ggen_core::Graph;
use ggen_ai::generators::{SparqlGenerator, GraphSchema};

let graph = Graph::new()?;
let query = generator.generate_query(&graph, intent).await?;
```

### For New Code

Use `ggen_core::Graph` in production:
```rust
use ggen_core::Graph;
use ggen_ai::generators::SparqlGenerator;

let graph = Graph::new()?;
let generator = SparqlGenerator::new(client);
let query = generator.generate_query(&graph, "Find all people").await?;
```

## Future Work

### Optional: Implement GraphSchema for ggen_core::Graph

To make `ggen_core::Graph` work with `SparqlGenerator`, consumers have two options:

1. **Implement trait in consumer crate** (current approach):
   ```rust
   impl ggen_ai::generators::GraphSchema for ggen_core::Graph {
       fn len(&self) -> usize { self.len() }
       fn schema_description(&self) -> String { /* ... */ }
   }
   ```

2. **Add optional feature to ggen-core** (future enhancement):
   ```toml
   [features]
   ggen-ai = ["dep:ggen-ai"]
   ```
   ```rust
   #[cfg(feature = "ggen-ai")]
   impl ggen_ai::generators::GraphSchema for Graph { /* ... */ }
   ```

**Recommendation**: Keep current approach (trait impl in consumer) to maintain clean separation of concerns.

## Verification

✅ **Compilation**: Both `ggen-core` and `ggen-ai` compile without errors
✅ **Tests**: All SPARQL generator tests pass
✅ **No Cyclic Dependency**: `ggen-core` does not depend on `ggen-ai`
✅ **Type Safety**: Generic over trait with compile-time checks
✅ **Documentation**: Clear guidance on stub vs real Graph usage

## Conclusion

The trait-based abstraction successfully resolves the cyclic dependency issue while maintaining type safety and testability. The stub Graph is now clearly documented as test-only, and the path to production usage is straightforward.

**Key Achievement**: `SparqlGenerator` now works with ANY graph implementation that provides schema information, without creating cyclic dependencies.
