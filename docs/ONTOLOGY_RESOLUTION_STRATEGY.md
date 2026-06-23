# Ontology Resolution Strategy

**Date**: 2026-06-23  
**Component**: Core architecture for fallback-based ontology loading

## Overview

The ontology resolution system provides a unified interface for loading ontologies from multiple sources with automatic fallback. This document describes the resolution strategy and integration points.

## Resolution Chain

When ggen needs to load an ontology, it follows this priority chain:

```
1. Embedded Core Bundle (zero-copy, always available)
   ├─ Check namespace URI against CoreOntologyBundle
   ├─ Return static &[u8] content (no allocation)
   └─ Time: <1 μs

2. Local Filesystem (development/custom ontologies)
   ├─ Check project-relative paths
   ├─ Load from ggen.toml ontology-dirs
   └─ Time: 10-100 ms

3. Marketplace Registry (domain packages, on-demand)
   ├─ Query marketplace for package
   ├─ Download if not cached
   ├─ Cache locally for future use
   └─ Time: 100-5000 ms (first use), <10 ms (cached)

4. W3C Canonical Sources (fallback, not recommended)
   ├─ HTTPS fetch from w3.org
   ├─ High latency, unreliable
   └─ Time: 1000-10000 ms
```

## Integration Points

### μ₁ Pipeline Stage (Load)

The pipeline's load stage uses OntologyLoader:

```rust
pub async fn run_load_stage(config: &PipelineConfig) -> Result<Graph> {
    let mut graph = Graph::new();
    
    for ontology_uri in &config.ontologies {
        // OntologyLoader handles fallback chain automatically
        let content = OntologyLoader::load_content(ontology_uri, &config.base_path)?;
        
        graph.load_turtle(&content)?;
    }
    
    Ok(graph)
}
```

### ggen.toml Configuration

```toml
[package]
name = "my-project"
version = "1.0.0"

[ontology]
# These URIs are resolved via the chain
sources = [
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#",        # Core (embedded)
    "http://example.org/domain/my-ontology#",              # Custom (filesystem)
    "registry://medical/fhir@2024.1.0",                    # Marketplace
]

# Alternative: import from ggen.toml in dependency
imports = [
    "medical/fhir@2024.1.0",    # Marketplace package
]

# Custom ontology directories (filesystem search)
ontology-dirs = [
    "ontologies/",              # Project root
    "specifications/",          # Additional paths
]
```

### Namespace URI Resolution

Namespace URIs are resolved using pattern matching:

```rust
// Core bundle lookups
"http://www.w3.org/1999/02/22-rdf-syntax-ns#"  → RDF syntax
"http://www.w3.org/2002/07/owl#"               → OWL
"http://purl.org/dc/terms/"                    → Dublin Core
"http://www.w3.org/ns/prov#"                   → PROV-O

// Marketplace lookups
"http://hl7.org/fhir/"                         → medical/fhir
"https://spec.edmcouncil.org/fibo/"            → finance/fibo
"http://isa95.org/"                            → manufacturing/isa95

// Custom/project ontologies (filesystem)
"http://example.org/domain/"                   → ontologies/domain.ttl
```

## Registry Integration

### Marketplace Query

When resolving `registry://` URIs, the system:

1. Parses the URI: `registry://medical/fhir@2024.1.0`
2. Queries marketplace registry:
   ```sparql
   SELECT ?content WHERE {
       ?package dcat:name "medical/fhir" ;
                dcterms:version "2024.1.0" ;
                dcat:distribution ?dist .
       ?dist dcat:downloadURL ?downloadURL .
   }
   ```
3. Checks local cache: `~/.ggen/ontology-cache/medical/fhir/2024.1.0/`
4. If not cached, downloads from `?downloadURL`
5. Verifies signature and checksum
6. Caches locally
7. Returns content

### Caching Strategy

```
Request for medical/fhir@2024.1.0
    ↓
Check cache: ~/.ggen/ontology-cache/medical/fhir/2024.1.0
    ↓
    ├─ Found? → Load from cache (instant)
    │
    └─ Not found?
        ↓
        Download from marketplace
        ↓
        Verify signature/checksum
        ↓
        Cache locally
        ↓
        Load from cache
```

## Error Handling

### Resolution Errors

| Error | Cause | Resolution |
|-------|-------|-----------|
| `OntologyNotFound` | URI not in any source | Check ggen.toml, ensure marketplace packages installed |
| `CorruptedCache` | Cache file checksum mismatch | `ggen ontology cache --clear <package>` then retry |
| `NetworkError` | Can't reach marketplace | Use `--offline` mode (requires pre-cached packages) |
| `InvalidURI` | Malformed namespace URI | Check ggen.toml syntax |
| `VersionMismatch` | Requested version unavailable | Update version constraint |

### Fallback on Error

```
Try core bundle → failed
  ↓ Continue
Try filesystem → failed
  ↓ Continue
Try cache → failed
  ↓ Continue
Try marketplace download → failed
  ↓
ERROR: Ontology unavailable
      Try:
      1. Check ggen.toml ontology sources
      2. Run: ggen ontology install <package>
      3. Use: ggen sync --offline (if already cached)
```

## Performance Optimization

### Lazy Loading

Ontologies are loaded on-demand:

```rust
// Only loaded when μ₁ stage runs
pipeline.load()?;

// Not loaded until needed
let rdf = OntologyLoader::load_content(uri)?;
```

### Caching Layers

1. **In-Memory**: Loaded ontologies cached in Graph during execution
2. **Filesystem**: Domain packages cached in `~/.ggen/ontology-cache/`
3. **Static**: Core ontologies embedded in binary (no I/O)

### Parallel Loading

Multiple ontologies can be loaded in parallel:

```rust
let mut tasks = vec![];

for uri in ontology_uris {
    let uri = uri.clone();
    let task = tokio::spawn(async move {
        OntologyLoader::load_content(&uri, base_path)
    });
    tasks.push(task);
}

let results = futures::future::join_all(tasks).await;
```

## Offline Mode

Users can opt into offline-only operation:

```bash
# Only use embedded + cached ontologies
ggen sync --offline

# Error if any ontology requires download
ggen sync --offline-strict
```

### Offline Verification

Before execution, verify all dependencies are available:

```bash
# Check if all ontologies are available offline
ggen ontology verify --offline

# Output:
# ✓ http://www.w3.org/1999/02/22-rdf-syntax-ns# (embedded)
# ✓ http://hl7.org/fhir/ (cached: 2024.1.0)
# ✗ http://example.org/missing# (NOT AVAILABLE - download required)
```

## Determinism & Reproducibility

### Version Pinning

All ontology sources must be version-pinned for reproducibility:

```toml
[ontology-dependencies]
# ✓ Valid (explicit version)
medical-fhir = "2024.1.0"
# ✗ Invalid (allows version drift)
medical-fhir = "*"
```

### Checksum Verification

All downloaded ontologies are verified:

```json
{
  "package": "medical/fhir",
  "version": "2024.1.0",
  "download_url": "https://...",
  "checksum": {
    "algorithm": "SHA-256",
    "value": "abc123..."
  },
  "signature": "ed25519-signature"
}
```

### Reproducible Builds

For CI/reproducible builds:

```bash
# 1. Lock all ontology versions
ggen ontology lock --output ggen.lock

# 2. Commit lockfile to version control
git add ggen.lock

# 3. CI uses locked versions
ggen sync --locked

# 4. Same inputs → same outputs (guaranteed)
```

## Integration Timeline

### Phase 1: Core Bundle + Filesystem (DONE ✅)
- ✅ Embedded 12 W3C standard ontologies
- ✅ OntologyLoader with fallback chain
- ✅ Filesystem resolution via OntologyResolver

### Phase 2: Pipeline Integration (IN PROGRESS)
- ⏳ Update μ₁ stage to use OntologyLoader
- ⏳ E2E tests with embedded ontologies
- ⏳ Offline mode support

### Phase 3: Marketplace Integration (NEXT)
- ⏳ Registry query for domain packages
- ⏳ Package installation and caching
- ⏳ Dependency resolution

### Phase 4: CLI & UI (FUTURE)
- ⏳ `ggen ontology` commands
- ⏳ Web UI for package discovery
- ⏳ Version management tools

## Example: Complete Resolution Flow

### Scenario: Healthcare Project with Multiple Ontologies

#### Setup
```toml
# ggen.toml
[package]
name = "healthcare-system"

[ontology-dependencies]
# Core (embedded, no download)
dcterms = { embedded = true }
owl = { embedded = true }

# Domain packages (will be cached)
"medical/fhir" = "2024.1.0"
"medical/snomed" = "2024.1.0"

# Custom (filesystem)
"company/internal-model" = { path = "ontologies/company.ttl" }
```

#### Resolution Execution

```
ggen sync
  ├─ Load dcterms
  │  ├─ Check core bundle → Found ✓
  │  └─ Load static content (0 ns)
  │
  ├─ Load owl
  │  ├─ Check core bundle → Found ✓
  │  └─ Load static content (0 ns)
  │
  ├─ Load medical/fhir
  │  ├─ Check core bundle → Not found
  │  ├─ Check filesystem → Not found
  │  ├─ Check cache → Found ✓
  │  └─ Load from cache (5 ms)
  │
  ├─ Load medical/snomed
  │  ├─ Check core bundle → Not found
  │  ├─ Check filesystem → Not found
  │  ├─ Check cache → Not found
  │  ├─ Query marketplace → Found
  │  ├─ Download → 2.5 MB (1500 ms)
  │  ├─ Verify signature → OK
  │  ├─ Cache locally
  │  └─ Load from cache (10 ms)
  │
  └─ Load company/internal-model
     ├─ Check filesystem → Found ✓
     └─ Load from file (15 ms)

Total time: 1.5 seconds (first run with snomed download)
Total time: 20 ms (cached run)

Generate artifacts → 5 seconds
Total execution: ~6.5 seconds
```

## Testing Strategy

### Resolution Tests
- ✅ Core bundle lookups work
- ✅ Filesystem fallback works
- ⏳ Marketplace download works (needs registry)
- ⏳ Caching works correctly

### Integration Tests
- ⏳ Full pipeline with mixed sources
- ⏳ Offline mode verification
- ⏳ Version conflict detection
- ⏳ Parallel loading performance

### E2E Tests
- ⏳ Real marketplace integration
- ⏳ Multi-package projects
- ⏳ Network failure recovery

---

**See also**:
- [Core Ontology Embedding](./architecture/CORE_ONTOLOGY_EMBEDDING.md)
- [Marketplace Packages](./MARKETPLACE_ONTOLOGY_PACKAGES.md)
- [Pipeline Architecture](./architecture/COMPRESSED_REFERENCE.md)
