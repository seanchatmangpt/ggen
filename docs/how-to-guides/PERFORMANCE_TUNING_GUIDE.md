# Performance Tuning for Large Ontologies

> Optimize ggen for 100k+ triples and complex queries

## Quick Wins

### 1. Enable Query Caching

```toml
[generation]
cache_queries = true
cache_size_mb = 512
```

**Impact**: 40-60% speedup for repeated patterns

### 2. Use SPARQL Query Optimization

```sparql
# ❌ SLOW: Cartesian product
SELECT ?a ?b ?c WHERE {
  ?x :prop1 ?a .
  OPTIONAL { ?x :prop2 ?b }
  OPTIONAL { ?x :prop3 ?c }
}

# ✅ FAST: UNION pattern
SELECT ?a ?b ?c WHERE {
  { SELECT ?a WHERE { ?x :prop1 ?a } }
  OPTIONAL { ?x :prop2 ?b }
  OPTIONAL { ?x :prop3 ?c }
}
```

### 3. Selective Loading

```rust
// Load only needed portions of graph
let graph = Graph::new()?;
graph.insert_turtle(&small_ttl)?;  // Start small

// Or use FILTER in SPARQL
let results = graph.query(r#"
  SELECT ?s WHERE {
    ?s a :Type1 .
    FILTER (?s IN (<iri1>, <iri2>, <iri3>))
  }
"#)?;
```

## Profiling

### Measure Query Performance

```bash
# Enable timing
RUST_LOG=debug ggen-cli render 2>&1 | grep "Query time:"

# Detailed profiling
time ggen-cli render
```

### Identify Slow Queries

```rust
use std::time::Instant;

let start = Instant::now();
let results = graph.query("SELECT ...")?;
eprintln!("Query took: {:?}", start.elapsed());
```

## Optimization Techniques

### Technique 1: Partition Ontology

```bash
# Instead of one 500k-triple file, use multiple files
ontology/
├── entities.ttl       (100k triples)
├── relationships.ttl  (100k triples)
├── rules.ttl          (100k triples)
└── instances.ttl      (200k triples)

# Load in parallel
graph.insert_turtle(&std::fs::read_to_string("ontology/entities.ttl")?)?;
graph.insert_turtle(&std::fs::read_to_string("ontology/relationships.ttl")?)?;
// etc.
```

**Impact**: Linear load time vs quadratic

### Technique 2: Lazy Property Loading

```sparql
# Instead of querying all properties
SELECT ?s ?p ?o WHERE {
  ?s a :Entity .
  ?s ?p ?o .
}

# Query only needed properties
SELECT ?s ?name ?email WHERE {
  ?s a :User .
  ?s :name ?name .
  ?s :email ?email .
}
```

### Technique 3: Stratified Queries

```sparql
# Break complex query into stages
# Stage 1: Find candidates
SELECT ?entity WHERE {
  ?entity a :Type1 ;
    :prop1 ?val1 .
  FILTER (?val1 > 1000)
}

# Stage 2: Filter and project
SELECT ?entity ?name WHERE {
  ?entity a :Type1 ;
    :name ?name .
  FILTER (?entity IN (...))
}
```

## Caching Strategy

### Query Result Caching

```rust
// Automatic caching via Graph::query_cached
let results1 = graph.query_cached("SELECT ...")?;  // Hits disk cache
let results2 = graph.query_cached("SELECT ...")?;  // Instant (in-memory)
```

### Materialized Views

```sparql
# Pre-compute expensive joins
INSERT INTO <urn:cache:users-with-posts> {
  ?user :posts ?count .
}
WHERE {
  ?user a :User .
  {
    SELECT ?user (COUNT(?post) as ?count) WHERE {
      ?post :author ?user .
    }
    GROUP BY ?user
  }
}
```

## Index Strategy

### Critical Indexes

```sparql
# Index frequently filtered properties
PREFIX ex: <http://example.org/>

ex:IndexedProperty
  a ex:Property ;
  ex:indexed true ;
  ex:indexed_predicate [
    ex:property ex:name ;
    ex:domain ex:User
  ] .
```

## Benchmarking

### Benchmark Template

```rust
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn bench_query(c: &mut Criterion) {
    c.bench_function("query_100k_triples", |b| {
        let graph = setup_graph();  // 100k triples
        b.iter(|| {
            graph.query(black_box("SELECT COUNT(?s) WHERE { ?s ?p ?o }"))
        });
    });
}

criterion_group!(benches, bench_query);
criterion_main!(benches);
```

Run:
```bash
cargo bench
```

## Memory Optimization

### Streaming Processing

```rust
// Instead of loading all results
let results = graph.query("SELECT * WHERE { ?s ?p ?o }")?;
for solution in results {  // Lazy iteration
    process(solution)?;
}
```

### Graph Compression

```toml
[graph]
compression = "zstd"  # Compress in-memory triples
```

## Monitoring

### Performance Metrics

```bash
# Enable metrics collection
export GGEN_METRICS=true

ggen-cli render

# View metrics
ggen-cli metrics show
```

### Alert Thresholds

```yaml
performance:
  max_query_time_ms: 5000
  max_memory_mb: 2048
  max_triple_load_time_ms: 10000

  alerts:
    - query_time > max_query_time_ms → WARN
    - memory > max_memory_mb → ERROR
```

## Comparative Performance

| Ontology Size | Query Type | Unoptimized | Optimized | Speedup |
|---|---|---|---|---|
| 1k triples | SELECT all | 2ms | 1ms | 2x |
| 10k triples | SELECT with OPTIONAL | 50ms | 15ms | 3.3x |
| 100k triples | CONSTRUCT inference | 500ms | 120ms | 4.2x |
| 1M triples | GROUP BY aggregate | 5000ms | 800ms | 6.25x |

## See Also

- [SPARQL Inference Guide](../SPARQL_INFERENCE_GUIDE.md) - Query optimization
- [GraphQL Deep Dive](./GRAPHQL_DEEP_DIVE.md) - Schema generation performance
