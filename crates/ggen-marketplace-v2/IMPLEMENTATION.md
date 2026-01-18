# ggen Marketplace v2 & v3: RDF-Based Implementation

## Overview

The ggen marketplace v2 and v3 are implemented using **oxigraph** as the primary data store, with all packages, versions, dependencies, and metadata stored as RDF (Resource Description Framework) triples. This provides:

- **Semantic data model**: Flexible, extensible package relationships
- **SPARQL queries**: Intelligent search and discovery
- **Knowledge graph**: All packages as nodes, relationships as edges
- **Version history**: Every state tracked as RDF facts
- **Auditability**: Complete history queryable and reproducible

## Architecture

### Data Layer

```
┌─────────────────────────────────────────┐
│       RDF Triplestore (oxigraph)        │  Single Source of Truth
├─────────────────────────────────────────┤
│  (All packages, versions, metadata)     │
│  (All relationships as triples)         │
└─────────────────────────────────────────┘
           ↓
┌─────────────────────────────────────────┐
│    v2: RdfRegistry + SPARQL Search      │  Foundation (2024-2025)
├─────────────────────────────────────────┤
│  - Direct RDF queries                   │
│  - SPARQL-based search                  │
│  - Full package introspection           │
│  - Cryptographic signing                │
│  - Type-safe models                     │
└─────────────────────────────────────────┘
           ↓
┌─────────────────────────────────────────┐
│   v3: V3OptimizedRegistry + Caching     │  Production (2025-2026)
├─────────────────────────────────────────┤
│  - Two-level caching (hot query, meta)  │
│  - Full-text search indexing            │
│  - Query result caching (5min/1hour)    │
│  - Performance SLOs (<100ms lookup)     │
│  - Distributed query optimization       │
└─────────────────────────────────────────┘
```

## Module Organization

### Core Modules

#### `models.rs`
Type-safe domain models with compile-time validation:
- `PackageId`: Validated package identifier
- `PackageVersion`: Semantic version with validation
- `QualityScore`: 0-100 quality rating
- `Package`: Complete package information
- `Manifest`: Package manifest with dependencies

#### `ontology.rs`
RDF ontology and SPARQL query templates:
- `Namespaces`: Defines RDF namespace URIs
- `Classes`: Package, Version, Dependency, License, etc.
- `Properties`: name, description, hasVersion, hasDependency, etc.
- `Queries`: Pre-built SPARQL query templates for common operations

#### `registry_rdf.rs` (v2)
Core RDF-backed registry:
- `RdfRegistry`: Manages RDF triplestore
- Inserts packages as RDF triples
- Executes SPARQL queries
- Tracks query statistics
- Implements `AsyncRepository` trait

#### `search_sparql.rs` (v2)
SPARQL-powered semantic search:
- `SparqlSearchEngine`: Query engine
- Search by name, description, keyword, author, quality
- Trending and recent package discovery
- `SearchFilters`: Advanced filtering options

#### `v3.rs` (v3)
Production optimizations:
- `V3OptimizedRegistry`: Caching layer over RDF
- Hot query cache (5 minute TTL)
- Metadata cache (1 hour TTL)
- Full-text search index
- Query statistics and monitoring
- <100ms lookup SLO
- <200ms search SLO

### Supporting Modules

#### `error.rs`
Comprehensive error handling:
- Package not found, invalid ID, invalid version
- Dependency resolution failures
- Validation failures
- Security check failures
- Signature verification failures

#### `traits.rs`
Core abstractions:
- `AsyncRepository`: Package lookup interface
- `Queryable`: Generic query execution
- `Installable`: Package installation with dependency resolution
- `Validatable`: Package validation framework
- `Signable`: Cryptographic operations
- `Observable`: Metrics collection

#### `validation.rs`
Pluggable validation system:
- `Validator` trait for custom validators
- Built-in validators: Metadata, License, README, Repository, Author
- Weighted scoring (0-100% quality)
- Production readiness tiers

#### `security.rs`
Cryptographic security:
- Ed25519 key pair generation
- Package signing and verification
- SHA-256 checksum calculation
- Signature receipts with attestation

#### `metrics.rs`
Observability and metrics:
- Search, installation, validation statistics
- Performance metrics
- Cache hit rates
- Event tracking

#### `install.rs`
Installation and dependency resolution:
- Dependency graph resolution
- Circular dependency detection
- Installation manifest creation
- Dry-run installation simulation

#### `builders.rs`
Type-safe builders:
- `PackageBuilder` for safe package construction
- Compile-time required field validation

## v2 Implementation

### RDF Schema

```
Package:
  - has type: ggen:classes/Package
  - has properties:
    - ggen:properties/packageId -> string
    - ggen:properties/name -> string
    - ggen:properties/description -> string
    - ggen:properties/hasVersion -> PackageVersion (1..*)
    - ggen:properties/hasAuthor -> Author (0..*)
    - ggen:properties/hasDependency -> Dependency (0..*)
    - ggen:properties/qualityScore -> integer (0-100)
    - ggen:properties/license -> string
    - ggen:properties/downloads -> integer
    - ggen:properties/createdAt -> datetime
    - ggen:properties/updatedAt -> datetime
    - ggen:properties/signature -> string
    - ggen:properties/checksum -> string

PackageVersion:
  - has type: ggen:classes/PackageVersion
  - has properties:
    - ggen:properties/version -> string
    - ggen:properties/hasDependency -> Dependency (0..*)
    - ggen:properties/releaseNotes -> string
    - ggen:properties/checksum -> string
    - ggen:properties/signature -> string

Dependency:
  - has type: ggen:classes/Dependency
  - references package and version
```

### SPARQL Query Examples

**Search by name**:
```sparql
SELECT ?package WHERE {
  ?package <rdf:type> <ggen:classes/Package> .
  ?package <ggen:properties/name> ?name .
  FILTER(CONTAINS(LCASE(str(?name)), "database"))
}
```

**Get package versions**:
```sparql
SELECT ?version WHERE {
  <ggen:packages/my-package> <ggen:properties/hasVersion> ?version .
}
ORDER BY DESC(?version)
```

**Find high-quality packages**:
```sparql
SELECT ?package WHERE {
  ?package <rdf:type> <ggen:classes/Package> .
  ?package <ggen:properties/qualityScore> ?score .
  FILTER(?score >= 95)
}
ORDER BY DESC(?score)
```

## v3 Optimizations

### Caching Strategy

**Two-level cache hierarchy**:

1. **Hot Query Cache** (5 minute TTL)
   - Stores complete query result sets
   - Covers: all_packages, trending, recent, by_quality
   - Size: 1,000 entries
   - Hit rate target: >70% for production workloads

2. **Metadata Cache** (1 hour TTL)
   - Stores individual package metadata
   - Covers: single package lookups
   - Size: 5,000 entries
   - Hit rate target: >80% for production workloads

### Search Index

Full-text search index over package names and descriptions:
- Built on first startup from RDF data
- Updated incrementally as packages added
- Maps search terms → package URIs
- Enables O(1) prefix search before RDF query

### Query Statistics

Tracked per request:
- Total queries executed
- Cache hits (hot and metadata)
- RDF store queries (cache misses)
- Query latency (microseconds)
- Overall cache hit rate

### Performance Targets

```
v2 SLOs:
  - Package lookup: <1ms (in-memory RDF)
  - Search: ~50ms (SPARQL query)
  - Installation: <5s (resolution + download)

v3 SLOs:
  - Package lookup: <100ms (cached RDF)
  - Search: <200ms (indexed + cached)
  - Installation: <10s (distributed)
  - Uptime: 99.99%
  - Cache hit rate: >75%
```

## Usage Examples

### v2: Basic Usage

```rust
use ggen_marketplace_v2::prelude::*;

#[tokio::main]
async fn main() -> Result<()> {
    // Create RDF-backed registry
    let registry = RdfRegistry::new();

    // Insert a package as RDF triples
    let pkg = Package { /* ... */ };
    registry.insert_package_rdf(&pkg).await?;

    // Search via SPARQL
    let search = SparqlSearchEngine::new(registry.store.clone());
    let results = search.search_by_name("database").await?;

    // Validate package
    let validator = PackageValidator::new();
    let result = validator.validate(&pkg).await?;
    println!("Quality score: {}", result.quality_score);

    // Sign package
    let signer = SignatureVerifier::new(KeyPair::generate());
    let signature = signer.sign(pkg_bytes)?;

    Ok(())
}
```

### v3: With Optimization

```rust
use ggen_marketplace_v2::prelude::*;

#[tokio::main]
async fn main() -> Result<()> {
    // Create RDF store
    let store = Arc::new(Store::new()?);

    // Wrap with v3 optimizations
    let registry = V3OptimizedRegistry::new(store).await;

    // Queries automatically use caching
    let packages = registry.all_packages().await?;

    // Monitor performance
    let stats = registry.stats();
    println!("{}", stats);  // Shows cache hit rates, avg latency

    Ok(())
}
```

## Key Design Decisions

### 1. RDF as Primary Data Store
**Why**: Aligns with ggen's philosophy of treating artifacts as knowledge graphs. Provides semantic flexibility and SPARQL query capabilities. Single source of truth.

### 2. oxigraph for RDF
**Why**: Pure Rust implementation. No external dependencies. Embedded in-process. SPARQL 1.1 compliant. Excellent query performance.

### 3. Two-Level Caching in v3
**Why**: Hot query cache handles repeated searches. Metadata cache handles individual lookups. Minimizes RDF store queries while remaining memory-efficient.

### 4. Type-Safe Models
**Why**: Invalid states unrepresentable at compile time. `PackageId`, `PackageVersion`, `QualityScore` provide runtime guarantees.

### 5. Async-First Design
**Why**: All I/O operations non-blocking. Tokio-based runtime for structured concurrency. Enables handling thousands of concurrent requests.

## Testing Strategy

### Unit Tests
- Model validation (PackageId, Version, QualityScore)
- RDF ontology (URI generation, property definitions)
- Search filters and query building
- Error handling and recovery

### Integration Tests
- RDF store operations (insert, query, update)
- SPARQL query execution and result parsing
- Search across multiple packages
- Caching behavior (hit rates, invalidation)
- Package signing and verification

### Performance Tests
- Lookup latency (<1ms v2, <100ms v3)
- Search latency (<50ms v2, <200ms v3)
- Cache hit rates (target >75%)
- Concurrent query handling

## Migration Path (v2 → v3)

1. **Data Compatibility**: All RDF data valid in both versions
2. **Query Compatibility**: SPARQL queries unchanged
3. **API Compatibility**: Same trait interfaces
4. **Zero Downtime**: Can run v2 and v3 simultaneously
5. **Rollback**: Can revert to v2 if v3 issues arise

## Future Work

### v4 Enhancements
- Multiple RDF replicas for HA
- Sharding by package namespace
- Advanced SPARQL query optimization
- Federated queries across multiple marketplaces
- Byzantine fault tolerance for distributed registry

### v5 Vision
- Universal marketplace across languages (Rust, Python, JS, Go, Java)
- Integration with central software catalogs
- Cross-marketplace federation
- Supply chain security via RDF attestations

## References

- [Oxigraph](https://oxigraph.org/) - RDF/SPARQL implementation
- [SPARQL 1.1](https://www.w3.org/TR/sparql11-query/) - Query language
- [RDF 1.1](https://www.w3.org/TR/rdf11-concepts/) - Data model
- [ggen](https://github.com/seanchatmangpt/ggen) - Code generation framework
