# GraphSchema Quick Reference

**For**: ggen-ai SPARQL Generator Users
**Updated**: 2026-03-31

## The Problem

You want to use `SparqlGenerator` with a real RDF graph, but:
- `ggen-ai` has a test stub `Graph` type
- `ggen-core::Graph` is the real RDF store
- How do you use them together?

## The Solution

Use the `GraphSchema` trait - it's the bridge between any graph and the SPARQL generator.

## Usage Patterns

### Pattern 1: Test Code (use the stub)

```rust
use ggen_ai::generators::{sparql::{Graph, SparqlGenerator}, GraphSchema};

#[tokio::test]
async fn test_sparql() {
    let generator = create_sparql_test_generator();
    let graph = Graph::new().unwrap();  // Test stub (always empty)
    let query = generator.generate_query(&graph, "Find people").await.unwrap();
}
```

### Pattern 2: Production Code (use real Graph)

```rust
use ggen_core::Graph;
use ggen_ai::generators::{SparqlGenerator, GraphSchema};

async fn generate_query(graph: &Graph, intent: &str) -> Result<String> {
    // Note: You must implement GraphSchema for ggen_core::Graph
    // See "Implementing the Trait" below
    let generator = SparqlGenerator::new(llm_client);
    generator.generate_query(graph, intent).await
}
```

### Pattern 3: Implement the Trait (once per consumer crate)

Add this to your consumer crate (e.g., `crates/your-app/src/graph_schema_impl.rs`):

```rust
use ggen_core::Graph;
use ggen_ai::generators::sparql::GraphSchema;

impl GraphSchema for Graph {
    fn len(&self) -> usize {
        self.len()
    }

    fn schema_description(&self) -> String {
        if self.is_empty() {
            "Empty graph".to_string()
        } else {
            format!("Graph with {} RDF triples", self.len())
        }
    }
}
```

**That's it!** Now `ggen_core::Graph` works with `SparqlGenerator`.

## The GraphSchema Trait

```rust
pub trait GraphSchema {
    /// Get the size of the graph (for context in prompt generation)
    fn len(&self) -> usize;

    /// Check if the graph is empty (default implementation provided)
    fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Get schema description for LLM prompts (default implementation provided)
    fn schema_description(&self) -> String {
        if self.is_empty() {
            "Empty graph".to_string()
        } else {
            format!("Graph with {} triples", self.len())
        }
    }
}
```

## API Examples

### Generate Query

```rust
use ggen_ai::generators::{SparqlGenerator, GraphSchema};

async fn example<G: GraphSchema>(graph: &G) -> Result<String> {
    let generator = SparqlGenerator::new(llm_client);
    let query = generator.generate_query(graph, "Find all person names").await?;
    Ok(query)
}
```

### Stream Query

```rust
use ggen_ai::generators::{SparqlGenerator, GraphSchema};

async fn example_stream<G: GraphSchema>(graph: &G) -> Result<()> {
    let generator = SparqlGenerator::new(llm_client);
    let prefixes = [("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#")];
    let mut stream = generator.stream_sparql(graph, "Find people", &prefixes).await?;

    while let Some(chunk) = stream.next().await {
        let sql = chunk?;
        println!("{}", sql);
    }
    Ok(())
}
```

## Why This Design?

### ✅ No Cyclic Dependency
- `ggen-ai` defines `GraphSchema` trait
- `ggen-core` doesn't depend on `ggen-ai`
- Consumer crates implement the bridge

### ✅ Type Safe
- Generic over trait with compile-time checks
- Zero runtime overhead (monomorphized)

### ✅ Testable
- Stub implements trait for tests
- Real Graph implements trait for production
- No mocking frameworks needed

### ✅ Extensible
- Any type can implement `GraphSchema`
- Custom graph stores possible
- No hard coupling to Oxigraph

## Common Pitfalls

### ❌ Don't use the stub in production

```rust
// BAD: Stub is for tests only
let graph = ggen_ai::generators::sparql::Graph::new()?;
let query = generator.generate_query(&graph, intent).await?;
// ^^^ This always uses an empty graph!
```

### ❌ Don't depend on ggen-core from ggen-ai

```rust
// BAD: Creates cyclic dependency
// In ggen-ai/src/lib.rs:
use ggen_core::Graph;  // ❌ NO!
```

### ✅ Do implement the trait in consumer crate

```rust
// GOOD: Consumer crate bridges the two
// In your-app/src/lib.rs:
use ggen_core::Graph;
use ggen_ai::generators::GraphSchema;

impl GraphSchema for Graph { /* ... */ }  // ✅ YES!
```

## Checklist

- [ ] I'm using `ggen_core::Graph` in production (not the stub)
- [ ] I've implemented `GraphSchema` for `ggen_core::Graph` in my consumer crate
- [ ] My tests use the stub from `ggen_ai::generators::sparql::Graph`
- [ ] I'm not trying to import `ggen_core` from within `ggen-ai`

## Need Help?

- **Full details**: See `GRAPH_SCHEMA_INTEGRATION_SUMMARY.md`
- **Test examples**: See `crates/ggen-ai/src/generators/sparql.rs` (tests module)
- **Real Graph**: See `crates/ggen-core/src/graph/core.rs`
