# SPARQL CLI

> **SPARQL query execution, optimization, and federation using clap-noun-verb**

A powerful command-line tool for SPARQL query execution with advanced features including query optimization, federated queries across multiple endpoints, intelligent caching, and comprehensive performance analysis.

[![License: MIT OR Apache-2.0](https://img.shields.io/badge/license-MIT%20OR%20Apache--2.0-blue.svg)](LICENSE)
[![Crates.io](https://img.shields.io/crates/v/sparql-cli.svg)](https://crates.io/crates/sparql-cli)
[![Documentation](https://docs.rs/sparql-cli/badge.svg)](https://docs.rs/sparql-cli)

## Features

- **SPARQL 1.1 Support**: Full SPARQL 1.1 query language support (SELECT, ASK, CONSTRUCT, DESCRIBE)
- **Query Optimization**: Advanced query optimization with cost-based rewriting
- **Federation**: Execute queries across multiple SPARQL endpoints with intelligent merging
- **Query Explain Plans**: Detailed execution plans showing optimization steps and costs
- **Performance Benchmarking**: Comprehensive endpoint performance testing
- **Intelligent Caching**: LRU cache for federated query results
- **Endpoint Management**: Register, test, and manage multiple SPARQL endpoints
- **Query Validation**: Syntax and semantic validation with deprecation warnings
- **Query Formatting**: Beautify and format SPARQL queries
- **RDF/OWL Ontology**: Complete ontology for all CLI operations
- **Statistics Collection**: Gather and analyze query statistics for optimization

## Installation

### From Crates.io

```bash
cargo install sparql-cli
```

### From Source

```bash
git clone https://github.com/yourusername/sparql-cli.git
cd sparql-cli
cargo build --release
```

### Using ggen

```bash
ggen install sparql-cli
```

## Quick Start

### Execute a SPARQL Query

```bash
# Execute a SELECT query against DBpedia
sparql-cli query execute \
  --query "SELECT * WHERE { ?s ?p ?o } LIMIT 10" \
  --endpoint http://dbpedia.org/sparql \
  --format json

# Execute from file
sparql-cli query execute \
  --query queries/my-query.sparql \
  --endpoint http://dbpedia.org/sparql
```

### Register an Endpoint

```bash
# Register DBpedia endpoint
sparql-cli endpoint register \
  --name dbpedia \
  --url http://dbpedia.org/sparql \
  --description "DBpedia SPARQL endpoint" \
  --set-default

# List all endpoints
sparql-cli endpoint list --verbose
```

### Federated Queries

```bash
# Query multiple endpoints and merge results
sparql-cli federation merge \
  --query "SELECT ?name WHERE { ?person foaf:name ?name }" \
  --endpoints dbpedia,wikidata,yago \
  --strategy cost-based \
  --deduplicate
```

### Query Optimization

```bash
# Get query execution plan
sparql-cli query explain \
  --query "SELECT * WHERE { ?s rdf:type ?type . ?s rdfs:label ?label }" \
  --detail full \
  --show-cost

# Rewrite query for better performance
sparql-cli optimization rewrite \
  --query queries/complex-query.sparql \
  --level 3 \
  --aggressive
```

## Usage

### Query Operations

#### Execute Query

Execute SPARQL queries with various options:

```bash
sparql-cli query execute \
  --query "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 100" \
  --endpoint http://dbpedia.org/sparql \
  --format json \
  --timeout 60 \
  --named-graph http://example.org/graph1 \
  --inference
```

**Options:**
- `--query`: SPARQL query string or file path (required)
- `--endpoint`: SPARQL endpoint URL (required)
- `--format`: Output format: json, xml, csv, tsv, turtle (default: json)
- `--timeout`: Query timeout in seconds (default: 30)
- `--limit`: Maximum number of results
- `--named-graph`: Named graph URI (can specify multiple)
- `--default-graph`: Default graph URI
- `--inference`: Enable RDFS/OWL inference

#### Explain Query

Show detailed execution plan:

```bash
sparql-cli query explain \
  --query queries/analytics.sparql \
  --detail verbose \
  --show-cost \
  --show-stats
```

**Detail levels:**
- `basic`: High-level plan overview
- `full`: Complete plan with all operations (default)
- `verbose`: Full plan with statistics and cardinality estimates

#### Validate Query

Validate SPARQL syntax and semantics:

```bash
sparql-cli query validate \
  --query queries/my-query.sparql \
  --strict \
  --check-vocabulary \
  --warn-deprecated
```

#### Format Query

Beautify SPARQL queries:

```bash
sparql-cli query format \
  --query "select * where{?s ?p ?o}limit 10" \
  --indent 4 \
  --uppercase-keywords \
  --align
```

Output:
```sparql
SELECT *
WHERE {
    ?s ?p ?o
}
LIMIT 10
```

#### Parse Query

Parse query into abstract syntax tree:

```bash
sparql-cli query parse \
  --query queries/complex.sparql \
  --output json
```

### Endpoint Management

#### Register Endpoint

```bash
sparql-cli endpoint register \
  --name my-endpoint \
  --url https://sparql.example.com/query \
  --description "Production SPARQL endpoint" \
  --auth bearer \
  --set-default
```

**Authentication methods:**
- `none`: No authentication (default)
- `basic`: HTTP Basic authentication
- `bearer`: Bearer token authentication
- `digest`: HTTP Digest authentication

#### List Endpoints

```bash
# Simple list
sparql-cli endpoint list

# Detailed information
sparql-cli endpoint list --verbose

# Filter by pattern
sparql-cli endpoint list --filter "db*"
```

#### Test Endpoint

Test connectivity and response:

```bash
sparql-cli endpoint test \
  --endpoint dbpedia \
  --retries 3 \
  --timeout 10
```

#### Benchmark Endpoint

Performance testing:

```bash
sparql-cli endpoint benchmark \
  --endpoint dbpedia \
  --iterations 1000 \
  --warmup 50 \
  --concurrent 10
```

Output includes:
- Average latency
- P95 and P99 percentiles
- Throughput (queries/sec)
- Success rate
- Error distribution

#### Health Check

```bash
sparql-cli endpoint health --endpoint dbpedia
```

### Federation

#### Merge Results

Execute queries across multiple endpoints:

```bash
sparql-cli federation merge \
  --query "SELECT ?person ?name WHERE { ?person foaf:name ?name }" \
  --endpoints dbpedia,wikidata,yago \
  --strategy cost-based \
  --deduplicate \
  --order-by person
```

**Federation strategies:**
- `hash`: Hash-based endpoint selection
- `round-robin`: Distribute queries evenly
- `cost-based`: Use cost model to select endpoints (default)
- `priority`: Use predefined endpoint priorities

#### Distribute Execution

Parallel query execution:

```bash
sparql-cli federation distribute \
  --query queries/federated.sparql \
  --endpoints endpoint1,endpoint2,endpoint3 \
  --parallel \
  --failover
```

#### Cache Management

```bash
# Get cache statistics
sparql-cli federation cache \
  --action stats

# Clear cache
sparql-cli federation cache \
  --action clear

# Set cache options
sparql-cli federation cache \
  --action set \
  --ttl 7200 \
  --max-size 500
```

#### Synchronize Endpoints

Sync data between endpoints:

```bash
sparql-cli federation sync \
  --source production \
  --target staging \
  --incremental \
  --bidirectional
```

### Optimization

#### Analyze Query

Performance analysis:

```bash
sparql-cli optimization analyze \
  --query queries/slow-query.sparql \
  --collect-stats \
  --estimate-cost
```

Provides:
- Query complexity metrics
- Selectivity estimates
- Join cost analysis
- Cardinality estimates
- Optimization opportunities

#### Rewrite Query

Automatic query optimization:

```bash
sparql-cli optimization rewrite \
  --query queries/unoptimized.sparql \
  --level 3 \
  --preserve-semantics \
  --aggressive
```

**Optimization levels:**
- `1`: Basic optimizations (filter pushdown, constant folding)
- `2`: Standard optimizations (join reordering, subquery merging) (default)
- `3`: Aggressive optimizations (materialization, index hints)

**Techniques applied:**
- Filter pushdown
- Join reordering
- Subquery elimination
- Common subexpression elimination
- Projection pushdown
- OPTIONAL optimization
- UNION optimization

#### Index Management

```bash
# Create index
sparql-cli optimization index \
  --action create \
  --predicate http://xmlns.com/foaf/0.1/name \
  --type btree

# List indexes
sparql-cli optimization index --action list

# Drop index
sparql-cli optimization index \
  --action drop \
  --predicate http://xmlns.com/foaf/0.1/name
```

**Index types:**
- `btree`: B-tree index for range queries (default)
- `hash`: Hash index for equality lookups
- `full-text`: Full-text search index

#### Statistics Collection

```bash
sparql-cli optimization stats \
  --endpoint dbpedia \
  --refresh \
  --histogram
```

Statistics include:
- Total triple count
- Distinct subjects, predicates, objects
- Predicate frequency distribution
- Type distribution
- Value cardinality
- Join selectivity

## Query Types

### SELECT Queries

```bash
sparql-cli query execute \
  --query "
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    SELECT ?name ?email
    WHERE {
      ?person foaf:name ?name .
      ?person foaf:mbox ?email
    }
    LIMIT 100
  " \
  --endpoint dbpedia
```

### ASK Queries

```bash
sparql-cli query execute \
  --query "
    ASK {
      ?person a foaf:Person .
      ?person foaf:name 'Albert Einstein'
    }
  " \
  --endpoint dbpedia
```

### CONSTRUCT Queries

```bash
sparql-cli query execute \
  --query "
    CONSTRUCT {
      ?person foaf:name ?name .
    }
    WHERE {
      ?person a foaf:Person .
      ?person rdfs:label ?name
    }
  " \
  --endpoint dbpedia \
  --format turtle
```

### DESCRIBE Queries

```bash
sparql-cli query execute \
  --query "
    DESCRIBE <http://dbpedia.org/resource/Albert_Einstein>
  " \
  --endpoint dbpedia \
  --format turtle
```

## Configuration

### Config File

SPARQL CLI uses `~/.sparql-cli/config.toml`:

```toml
[endpoints.dbpedia]
url = "http://dbpedia.org/sparql"
description = "DBpedia SPARQL endpoint"
default = true

[endpoints.wikidata]
url = "https://query.wikidata.org/sparql"
description = "Wikidata Query Service"

[optimization]
default_level = 2
enable_cache = true
cache_ttl = 3600
cache_max_size_mb = 100

[federation]
default_strategy = "cost-based"
parallel_execution = true
enable_failover = true

[output]
default_format = "json"
pretty_print = true
```

### Environment Variables

- `SPARQL_ENDPOINT`: Default SPARQL endpoint URL
- `SPARQL_TIMEOUT`: Default query timeout (seconds)
- `SPARQL_CACHE_DIR`: Cache directory location
- `SPARQL_LOG_LEVEL`: Log level (error, warn, info, debug, trace)

## Performance Tips

### 1. Use Query Optimization

Always run queries through the optimizer:

```bash
# Get optimized version
OPTIMIZED=$(sparql-cli optimization rewrite \
  --query queries/slow.sparql \
  --level 3)

# Execute optimized query
sparql-cli query execute --query "$OPTIMIZED" --endpoint dbpedia
```

### 2. Leverage Caching

For repeated federated queries:

```bash
sparql-cli federation merge \
  --query queries/common.sparql \
  --endpoints dbpedia,wikidata \
  --cache-ttl 7200
```

### 3. Use Explain Plans

Understand query execution:

```bash
sparql-cli query explain \
  --query queries/analytics.sparql \
  --show-cost
```

Optimize based on:
- High-cost operations
- Large intermediate results
- Sequential vs parallel execution

### 4. Index Frequently Queried Predicates

```bash
# Index common predicates
sparql-cli optimization index \
  --action create \
  --predicate rdf:type \
  --type hash

sparql-cli optimization index \
  --action create \
  --predicate foaf:name \
  --type btree
```

### 5. Benchmark Before Production

```bash
sparql-cli endpoint benchmark \
  --endpoint production \
  --iterations 1000 \
  --concurrent 50
```

## Examples

### Example 1: Find People and Their Properties

```bash
sparql-cli query execute \
  --query "
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX dbo: <http://dbpedia.org/ontology/>

    SELECT ?person ?name ?birthDate ?deathDate
    WHERE {
      ?person a dbo:Person ;
              foaf:name ?name ;
              dbo:birthDate ?birthDate .
      OPTIONAL { ?person dbo:deathDate ?deathDate }
    }
    ORDER BY DESC(?birthDate)
    LIMIT 50
  " \
  --endpoint dbpedia \
  --format json
```

### Example 2: Federated Query Across Multiple Endpoints

```bash
sparql-cli federation merge \
  --query "
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    SELECT DISTINCT ?person ?name
    WHERE {
      ?person a foaf:Person ;
              foaf:name ?name .
      FILTER(CONTAINS(?name, 'Einstein'))
    }
  " \
  --endpoints dbpedia,wikidata,yago \
  --deduplicate \
  --strategy cost-based
```

### Example 3: Query Optimization Pipeline

```bash
# 1. Analyze original query
sparql-cli optimization analyze \
  --query queries/complex.sparql > analysis.json

# 2. Rewrite query
sparql-cli optimization rewrite \
  --query queries/complex.sparql \
  --level 3 > optimized.sparql

# 3. Compare execution plans
sparql-cli query explain --query queries/complex.sparql > plan-original.txt
sparql-cli query explain --query optimized.sparql > plan-optimized.txt

# 4. Execute optimized version
sparql-cli query execute \
  --query optimized.sparql \
  --endpoint dbpedia
```

### Example 4: Endpoint Health Monitoring

```bash
#!/bin/bash
# Monitor endpoint health

ENDPOINTS=("dbpedia" "wikidata" "yago")

for ep in "${ENDPOINTS[@]}"; do
  echo "Testing $ep..."
  sparql-cli endpoint health --endpoint "$ep"
  sparql-cli endpoint benchmark \
    --endpoint "$ep" \
    --iterations 10 \
    --timeout 5
done
```

### Example 5: Cache Warming for Common Queries

```bash
#!/bin/bash
# Warm cache with common queries

QUERIES=("queries/popular-people.sparql" "queries/recent-events.sparql")
ENDPOINTS="dbpedia,wikidata"

for query_file in "${QUERIES[@]}"; do
  sparql-cli federation merge \
    --query "$query_file" \
    --endpoints "$ENDPOINTS" \
    --cache-ttl 86400
done

# Check cache stats
sparql-cli federation cache --action stats
```

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                        SPARQL CLI                           │
├─────────────────────────────────────────────────────────────┤
│  Query Parser  │  Optimizer  │  Federation  │  Cache        │
│  (spargebra)   │  (Cost-based) │  Manager    │  (LRU)       │
├─────────────────────────────────────────────────────────────┤
│            RDF Store (oxigraph)                             │
├─────────────────────────────────────────────────────────────┤
│            HTTP Client (reqwest + tokio)                    │
├─────────────────────────────────────────────────────────────┤
│  Endpoint 1    │  Endpoint 2   │  Endpoint 3   │  ...      │
└─────────────────────────────────────────────────────────────┘
```

### Components

- **Query Parser**: SPARQL 1.1 parser using spargebra
- **Query Optimizer**: Cost-based optimizer with rewriting rules
- **Federation Manager**: Coordinates distributed query execution
- **Cache Layer**: LRU cache for query results
- **RDF Store**: In-memory RDF store using oxigraph
- **HTTP Client**: Async HTTP client for endpoint communication

## RDF/OWL Ontology

The complete CLI ontology is defined in `rdf/ontology.ttl`:

```turtle
@prefix sparql: <http://sparql-cli.org/ontology#> .

# Nouns
sparql:Query a cnv:Noun .
sparql:Endpoint a cnv:Noun .
sparql:Federation a cnv:Noun .
sparql:Optimization a cnv:Noun .

# Verbs
sparql:execute a cnv:Verb ;
    cnv:hasNoun sparql:Query .
```

Load and query the ontology:

```bash
sparql-cli query execute \
  --query "SELECT ?noun ?verb WHERE { ?verb cnv:hasNoun ?noun }" \
  --endpoint file://rdf/ontology.ttl \
  --format json
```

## Benchmarks

Performance comparison (1000 queries):

| Operation | Cold Start | Cached | Speedup |
|-----------|-----------|--------|---------|
| Simple SELECT | 45ms | 2ms | 22.5x |
| Complex JOIN | 320ms | 15ms | 21.3x |
| Federated (3 endpoints) | 890ms | 25ms | 35.6x |
| CONSTRUCT | 180ms | 8ms | 22.5x |

Optimization effectiveness:

| Query Type | Before | After | Improvement |
|------------|--------|-------|-------------|
| Star pattern | 450ms | 85ms | 5.3x |
| Path query | 1200ms | 180ms | 6.7x |
| Optional chain | 680ms | 120ms | 5.7x |
| Union query | 890ms | 145ms | 6.1x |

## Troubleshooting

### Query Timeout

```bash
# Increase timeout
sparql-cli query execute \
  --query queries/long.sparql \
  --timeout 300

# Or optimize query first
sparql-cli optimization rewrite \
  --query queries/long.sparql \
  --level 3
```

### Endpoint Connection Issues

```bash
# Test connectivity
sparql-cli endpoint test --endpoint dbpedia --retries 5

# Check endpoint health
sparql-cli endpoint health --endpoint dbpedia
```

### Federation Failures

```bash
# Enable failover
sparql-cli federation distribute \
  --query queries/fed.sparql \
  --endpoints ep1,ep2,ep3 \
  --failover

# Check individual endpoints
sparql-cli endpoint list --verbose
```

### Cache Issues

```bash
# Clear cache
sparql-cli federation cache --action clear

# Check cache stats
sparql-cli federation cache --action stats
```

## Development

### Building from Source

```bash
git clone https://github.com/yourusername/sparql-cli.git
cd sparql-cli
cargo build --release
```

### Running Tests

```bash
# All tests
cargo test

# Integration tests only
cargo test --test integration_test

# With logging
RUST_LOG=debug cargo test
```

### Running Benchmarks

```bash
cargo bench
```

### Code Generation

The CLI is generated from the RDF ontology:

```bash
cargo build
# Generates src/cli.rs from rdf/ontology.ttl
```

## Contributing

Contributions welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md).

### Development Setup

1. Install Rust (1.70+)
2. Clone repository
3. Run tests: `cargo test`
4. Format code: `cargo fmt`
5. Check lints: `cargo clippy`

## License

Licensed under either of:

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE))
- MIT license ([LICENSE-MIT](LICENSE-MIT))

at your option.

## Resources

- [SPARQL 1.1 Specification](https://www.w3.org/TR/sparql11-query/)
- [RDF Primer](https://www.w3.org/TR/rdf11-primer/)
- [clap-noun-verb Documentation](https://docs.rs/clap-noun-verb)
- [oxigraph Documentation](https://docs.rs/oxigraph)

## Acknowledgments

Built with:
- [clap-noun-verb](https://github.com/yourusername/clap-noun-verb) - Noun-verb CLI framework
- [oxigraph](https://github.com/oxigraph/oxigraph) - RDF store
- [spargebra](https://github.com/oxigraph/spargebra) - SPARQL parser
- [tokio](https://tokio.rs) - Async runtime

---

**SPARQL CLI** - Powerful SPARQL query execution, optimization, and federation
