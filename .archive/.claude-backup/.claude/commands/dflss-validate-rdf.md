# Validate RDF Graphs and Queries

Validate RDF graphs, SPARQL queries, and ensure deterministic processing.

## Commands

### Validate RDF Graphs
```bash
cargo make validate-rdf
```

This command:
- Validates RDF graph syntax
- Checks graph consistency
- Verifies SPARQL query syntax
- Ensures deterministic processing

### Load and Validate Graph
```bash
# Load RDF graph
cargo run --bin ggen -- graph load <rdf-file> --format <turtle|json-ld|rdf-xml>

# Validate graph
cargo make validate-rdf
```

### Validate SPARQL Queries

SPARQL queries must:
- Be syntactically valid
- Extract required template variables
- Produce deterministic results
- Use compile-time checked builders when possible

## RDF Processing

### Graph Loading
- Support multiple formats: Turtle (.ttl), JSON-LD (.jsonld), RDF/XML (.rdf)
- Validate graphs before processing
- Stream large graphs (avoid whole-graph loads)
- SLO: RDF processing ≤ 5s for 1k+ triples

### SPARQL Query Execution
- Extract template variables via SPARQL
- Cache repeated queries for performance
- Validate queries before execution
- Ensure deterministic query results

## Validation Checklist

- [ ] RDF graph syntax is valid
- [ ] Graph is consistent (no contradictions)
- [ ] SPARQL queries are syntactically valid
- [ ] Queries extract required variables
- [ ] Query results are deterministic
- [ ] Graph processing meets SLOs (≤ 5s for 1k+ triples)
- [ ] Large graphs are streamed (not loaded entirely)

## Examples

### Validate Graph File
```rust
use ggen_core::graph::Graph;

let graph = Graph::load("data.ttl")?;
graph.validate()?; // Validates syntax and consistency
```

### Validate SPARQL Query
```rust
use ggen_core::sparql::Query;

let query = Query::parse(&sparql_string)?;
query.validate()?; // Validates syntax
let results = graph.query(&query)?; // Execute query
```

### Check Determinism
```bash
# Run deterministic tests
cargo make deterministic

# Verify same inputs produce same outputs
cargo make test-single-threaded
```

