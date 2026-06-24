# Core Ontology Embedding: Usage Examples

This document demonstrates how to use the newly embedded core ontologies in ggen v26.5.28+.

## Quick Start

### Accessing Core Ontologies

```rust
use ggen_core::ontology::CoreOntologyBundle;

// List all embedded ontologies
let ontologies = CoreOntologyBundle::all();
println!("Embedded {} ontologies", ontologies.len());

// Look up RDF ontology by namespace
let rdf = CoreOntologyBundle::by_namespace(
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
)?;
println!("RDF ontology size: {} bytes", rdf.size);

// Access raw content
let content: &[u8] = rdf.content;
```

### Using the Unified Loader

```rust
use ggen_core::ontology::OntologyLoader;
use std::path::Path;

// Load ontology content with automatic fallback
let content = OntologyLoader::load_content(
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    Path::new(".")
)?;

// Check if ontology is embedded (zero-copy available)
if OntologyLoader::is_embedded("owl") {
    println!("OWL is available offline");
}

// Get metadata
let meta = OntologyLoader::get_metadata("prov-o")?;
println!("PROV-O: {}", meta.namespace);

// List all offline-available ontologies
let embedded = OntologyLoader::list_embedded();
for (name, uri) in embedded {
    println!("- {} ({})", name, uri);
}
```

## Use Case 1: Offline Code Generation

Generate code without network access for core ontologies:

```rust
use ggen_core::ontology::OntologyLoader;
use ggen_core::pipeline::Pipeline;
use std::path::Path;

// Initialize pipeline
let mut pipeline = Pipeline::new()?;

// Load core ontology (always available offline)
let ontology = OntologyLoader::load_content(
    "http://www.w3.org/2002/07/owl#",
    Path::new("ggen.toml")
)?;

// Run μ₁–μ₅ pipeline without network
let result = pipeline.run(&ontology)?;

// Write generated code
std::fs::write("generated.rs", result)?;
```

## Use Case 2: Checking Embedding Status

```rust
use ggen_core::ontology::OntologyLoader;

// Determine whether to fetch from marketplace
let uri = "http://example.com/domain/healthcare#";

if OntologyLoader::is_embedded(uri) {
    // Use zero-copy embedded version
    println!("Using offline core ontology");
} else {
    // Would fetch from marketplace (future implementation)
    println!("Would install from marketplace: {}", uri);
}
```

## Use Case 3: Iterating Over All Embedded Ontologies

```rust
use ggen_core::ontology::CoreOntologyBundle;

let stats = CoreOntologyBundle::stats();
println!("Total embedded: {} ontologies, {} bytes",
    stats.count,
    stats.total_size_bytes
);

for ontology in CoreOntologyBundle::all() {
    // Parse ontology content
    let lines = ontology.content
        .split(|&b| b == b'\n')
        .count();
    
    println!("{}: {} lines (~{} KB)",
        ontology.name,
        lines,
        ontology.size / 1024
    );
}
```

## Use Case 4: Parsing Core Ontologies with Oxigraph

```rust
use ggen_core::ontology::CoreOntologyBundle;
use oxigraph::store::Store;

// Get RDF ontology content
let rdf_ontology = CoreOntologyBundle::by_name("rdf-syntax-ns")?;

// Load into oxigraph
let store = Store::new()?;
let mut writer = store.bulk_loader().for_loader();

// Parse Turtle content
oxigraph::io::read(
    std::io::Cursor::new(rdf_ontology.content),
    oxigraph::io::RdfFormat::Turtle,
    None,
    None
)?;

// Query using SPARQL
let results = store.query("SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10")?;
```

## Use Case 5: CLI Integration (Future)

```bash
# List embedded ontologies
ggen ontology list --embedded
# Output:
#   rdf-syntax-ns (http://www.w3.org/1999/02/22-rdf-syntax-ns#)
#   owl (http://www.w3.org/2002/07/owl#)
#   ...

# Check if ontology is embedded
ggen ontology status http://www.w3.org/2002/07/owl#
# Output: EMBEDDED (zero-copy, offline available)

# Try to install domain ontology (would use marketplace)
ggen ontology install medical/fhir@2024.1.0
# Output: Downloading from marketplace...

# Generate code without network
ggen sync --offline
# Uses only embedded core ontologies
```

## Use Case 6: Adding Custom Core Ontologies

To embed additional ontologies:

1. Add TTL/RDF file to `ontologies/core/`:
```bash
curl -L https://www.w3.org/ns/example -o ontologies/core/example.ttl
```

2. Update namespace mapping in `build.rs` (if needed):
```rust
"example" => "http://www.w3.org/ns/example#",
```

3. Rebuild:
```bash
cargo build
```

4. Access new ontology:
```rust
let example = CoreOntologyBundle::by_name("example")?;
```

## Performance Characteristics

### Zero-Copy Access

```rust
// This is zero-copy: returns a reference to the embedded bytes
let content = CoreOntologyBundle::by_name("owl")?;
let bytes: &'static [u8] = content.content;  // No allocation

// OntologyLoader falls back to copying when using marketplace
let content = OntologyLoader::load_content("http://...", Path::new("."))?;
// Returns Vec<u8> (allocated for marketplace/local files)
```

### Compilation Time Impact

Adding embedded ontologies adds minimal compile-time overhead:
- Discovery: ~50 ms (scanning ontologies/core/)
- Code generation: ~100 ms (generating ontologies.rs)
- Linking: included in normal linking time

Total impact: < 200 ms on incremental builds.

### Memory Overhead

- Core ontologies in binary: 448 KB (static, shared across processes)
- No runtime allocation for embedded ontologies
- Only allocated for marketplace downloads (cached locally)

## Architecture Decision: Why Embed?

### Pros
- ✅ Offline-first core pipeline (μ₁–μ₅ always work)
- ✅ Zero network latency for standard ontologies
- ✅ Deterministic, reproducible builds
- ✅ No dependency on W3C uptime
- ✅ Smaller than many crate dependencies

### Cons
- ❌ +448 KB binary size (negligible)
- ❌ Can't update without recompiling (acceptable for standards)
- ❌ Limited to read-only access (ontologies are immutable)

### Mitigation

For large domains, use marketplace packages:
- Domain ontologies stay distributed (not embedded)
- Users install on-demand via `ggen ontology install`
- Massive domains (healthcare 28 MB, financial 63.8 MB) stay optional

## Migration: From File-Based to Embedded

### Before (v26.5.27 and earlier)

```rust
// Had to load from files or network
let content = std::fs::read("ontologies/rdf.ttl")?;
```

### After (v26.5.28+)

```rust
// Now uses embedded zero-copy
let rdf = CoreOntologyBundle::by_name("rdf-syntax-ns")?;
let content = rdf.content;  // &'static [u8]
```

### Backward Compatibility

The `OntologyLoader` provides fallback to maintain compatibility:

```rust
// Works with both embedded (new) and file-based (old) loading
let content = OntologyLoader::load_content(uri, base_path)?;
```

## Testing Embedded Ontologies

```bash
# Run core bundle tests
cargo test -p ggen-core --lib ontology::core_bundle

# Run loader tests
cargo test -p ggen-core --lib ontology::loader

# Verify no panic on loading embedded ontologies
cargo test -p ggen-core --doc ontology::CoreOntologyBundle
```

## Troubleshooting

### Q: "Ontology not found" error

**Check 1**: Is it in the core bundle?
```rust
let available = CoreOntologyBundle::available();
if !available.iter().any(|(_, ns)| ns == your_uri) {
    println!("Not in core bundle, use marketplace");
}
```

**Check 2**: Is the namespace URI correct?
```rust
let ontology = CoreOntologyBundle::by_namespace("http://www.w3.org/2002/07/owl#");
assert!(ontology.is_some(), "Standard W3C URI");
```

### Q: "Content is empty" error

This would indicate a broken build. Verify:
```bash
# Rebuild to ensure ontologies are embedded correctly
cargo clean -p ggen-core && cargo build -p ggen-core
```

### Q: How to update an embedded ontology?

```bash
# Download new version from W3C
curl -L https://www.w3.org/ns/prov-o -o ontologies/core/prov-o.ttl

# Verify it's valid Turtle
ggen validate ontologies/core/prov-o.ttl

# Rebuild (ontology will be re-embedded)
cargo build

# Commit changes
git add ontologies/core/prov-o.ttl
git commit -m "chore(ontology): Update PROV-O to latest W3C version"
```

## References

- [ggen Architecture](../architecture/COMPRESSED_REFERENCE.md)
- [Core Ontology Embedding](../architecture/CORE_ONTOLOGY_EMBEDDING.md)
- [API Documentation](https://docs.rs/ggen-core/latest/ggen_core/ontology/)
- [W3C RDF/OWL Specifications](https://www.w3.org/)

---

**Last Updated**: 2026-06-23  
**Applies to**: ggen v26.5.28+
