# Maturity Matrix Migration Guide

This guide shows how to progress through all 5 maturity levels, from simple file generation to enterprise-scale deployments.

## Level 1: Simple (1-10 files)

**Goal**: Generate a single file from a basic ontology.

### Setup

```bash
cd level1-simple
```

### Ontology

**File**: `ontology.ttl` (Lines: ~20, Triples: ~10)

Simple RDF ontology with:
- Single class (`ex:User`)
- Two properties (`ex:userName`, `ex:userEmail`)
- One example instance

### Generation

```bash
# Load ontology into graph
ggen graph load ontology.ttl

# Generate single file
ggen template generate template.tmpl
```

**Output**: `generated/user.rs` (1 file)

**Oxigraph Usage**:
- Single `Graph::new()` instance
- In-memory store
- Basic `graph.query()` call
- No caching

**Code References**:
- `pipeline.rs:97-103` - Simple pipeline
- `graph/core.rs:215-220` - `insert_turtle()`
- `template.rs:260-356` - Graph processing

### Expected Output

```rust
// generated/user.rs
pub struct User {
    pub user_name: String,
    pub user_email: String,
}

impl User {
    pub fn new(name: String, email: String) -> Self {
        Self {
            user_name: name,
            user_email: email,
        }
    }
}
```

## Level 2: Small Project (10-100 files)

**Goal**: Generate multiple files with template inheritance.

### Setup

```bash
cd level2-small
```

### Ontology

**File**: `ontology.ttl` (Lines: ~60, Triples: ~50)

Structured RDF with:
- Multiple classes (`User`, `Product`, `Order`)
- Relationships between classes
- Property hierarchies

### Generation

```bash
# Load ontology
ggen graph load ontology.ttl

# Generate multiple files
ggen template generate templates/
```

**Output**: 
- `generated/src/main.rs`
- `generated/src/user.rs`
- `generated/src/product.rs`
- `generated/src/order.rs`
- `generated/Cargo.toml`
- (10-100 files total)

**Oxigraph Usage**:
- Single graph with query caching
- `GraphQuery` for complex queries
- Epoch-based cache invalidation

**Code References**:
- `pipeline.rs:280-291` - RDF file loading
- `graph/core.rs:461-532` - Query caching
- `templates/generator.rs` - File tree generation

## Level 3: Medium Project (100-1,000 files)

**Goal**: Generate workspace structure with lifecycle integration.

### Setup

```bash
cd level3-medium
```

### Ontology

**Files**: 
- `ontology/domain.ttl` (Lines: ~100, Triples: ~500)
- `ontology/api.ttl` (Lines: ~80, Triples: ~300)
- `ontology/database.ttl` (Lines: ~60, Triples: ~200)

Complex ontology with:
- Workspace structure
- Multiple crates
- Module hierarchies
- Function definitions

### Generation

```bash
# Use lifecycle for full workflow
ggen lifecycle run
```

**Output**: Complete Rust workspace with:
- Workspace `Cargo.toml`
- Multiple crate directories
- Source files in each crate
- Test files
- Example files
- (100-1,000 files total)

**Oxigraph Usage**:
- Persistent graph storage
- Multiple named graphs
- Advanced SPARQL queries
- Query result materialization

**Code References**:
- `lifecycle/exec.rs:run_pipeline` - Lifecycle orchestration
- `graph/store.rs` - Persistent storage
- `graph/core.rs:249-286` - Named graph support

## Level 4: Large Project (1,000-10,000 files)

**Goal**: Generate multi-workspace structure with CI/CD.

### Setup

```bash
cd level4-large
```

### Ontology

**Files**:
- `ontologies/core.ttl` (Lines: ~500, Triples: ~5,000)
- `ontologies/services.ttl` (Lines: ~400, Triples: ~3,000)
- `ontologies/infrastructure.ttl` (Lines: ~300, Triples: ~2,000)

Distributed ontology with:
- Core domain model
- Service definitions
- Infrastructure components
- Cross-references

### Generation

```bash
# Incremental generation with caching
ggen lifecycle run --incremental
```

**Output**: Multi-workspace structure with:
- Multiple top-level workspaces
- Service workspaces
- Infrastructure workspaces
- CI/CD configuration
- (1,000-10,000 files total)

**Oxigraph Usage**:
- Federated graph queries
- Query optimization
- Result streaming
- Distributed query execution

**Code References**:
- `graph/query.rs` - Advanced query building
- `graph/export.rs` - Large-scale export
- `lifecycle/cache.rs` - Incremental caching

## Level 5: Enterprise (10,000+ files)

**Goal**: Generate enterprise-scale multi-tenant system.

### Setup

```bash
cd level5-enterprise
```

### Ontology

**Files**:
- `ontologies/shared/core.ttl` (Lines: ~2,000, Triples: ~20,000)
- `ontologies/tenant1/` (Lines: ~1,000, Triples: ~10,000)
- `ontologies/tenant2/` (Lines: ~1,000, Triples: ~10,000)
- `ontologies/shared/services.ttl` (Lines: ~1,500, Triples: ~15,000)

Federated ontologies with:
- Shared core model
- Tenant-specific models
- Service definitions
- Infrastructure as code
- Monitoring and observability

### Generation

```bash
# Distributed generation
ggen lifecycle run --distributed --workers 10
```

**Output**: Enterprise system with:
- Multi-tenant architecture
- Distributed services
- Infrastructure components
- Monitoring stack
- CI/CD pipelines
- (10,000+ files total)

**Oxigraph Usage**:
- Distributed graph stores
- Graph replication
- Sharding strategies
- Query federation
- Advanced performance tuning

**Code References**:
- `graph/store.rs` - Distributed storage
- `graph/query.rs` - Federated queries
- Enterprise extensions (future)

## Migration Checklist

### Level 1 → Level 2
- [ ] Add multiple templates
- [ ] Expand ontology with relationships
- [ ] Enable query caching
- [ ] Add template inheritance

### Level 2 → Level 3
- [ ] Add workspace structure
- [ ] Integrate lifecycle (`make.toml`)
- [ ] Use persistent graph storage
- [ ] Add named graph support

### Level 3 → Level 4
- [ ] Split ontology into multiple files
- [ ] Add CI/CD integration
- [ ] Enable incremental generation
- [ ] Implement advanced caching

### Level 4 → Level 5
- [ ] Add multi-tenant support
- [ ] Implement distributed generation
- [ ] Add monitoring and observability
- [ ] Enable graph federation

## Performance Benchmarks

| Level | Files | Generation Time | Memory | Cache Hit Rate |
|-------|-------|----------------|--------|----------------|
| Level 1 | 1-10 | < 1s | < 10MB | N/A |
| Level 2 | 10-100 | < 5s | < 50MB | 80-90% |
| Level 3 | 100-1K | < 30s | < 200MB | 85-95% |
| Level 4 | 1K-10K | < 2min | < 1GB | 90-95% |
| Level 5 | 10K+ | < 10min | < 5GB | 95%+ |

## Next Steps

1. Start with Level 1 to understand basics
2. Progress to Level 2 as you add features
3. Move to Level 3 when you need lifecycle
4. Scale to Level 4 for large teams
5. Deploy Level 5 for enterprise scale

Each level builds on the previous, ensuring smooth progression.

