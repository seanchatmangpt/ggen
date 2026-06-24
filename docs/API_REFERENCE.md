# API Reference

This document describes the public Rust API for ggen ontology embedding.

## Table of Contents

- [CoreOntologyBundle](#coreontologybundle)
- [OntologyLoader](#ontologyloader)
- [OntologyMetadata](#ontologymetadata)
- [OntologyInput](#ontologyinput)
- [Epoch](#epoch)
- [Examples](#examples)

## CoreOntologyBundle

Core API for accessing embedded ontologies.

### Location
```rust
use ggen_core::ontology::CoreOntologyBundle;
```

### Methods

#### `all() -> &'static [OntologyMetadata]`

Get all embedded core ontologies.

**Returns:** Slice of all embedded ontologies (12 total).

**Example:**
```rust
use ggen_core::ontology::CoreOntologyBundle;

let ontologies = CoreOntologyBundle::all();
println!("Available ontologies: {}", ontologies.len());
// Output: Available ontologies: 12

for ont in ontologies {
    println!("{}: {} bytes", ont.name, ont.size);
}
```

---

#### `by_namespace(uri: &str) -> Option<&'static OntologyMetadata>`

Find an ontology by its full namespace URI.

**Parameters:**
- `uri` — Full namespace URI (e.g., `"http://www.w3.org/1999/02/22-rdf-syntax-ns#"`)

**Returns:** `Some(metadata)` if found, `None` otherwise.

**Example:**
```rust
use ggen_core::ontology::CoreOntologyBundle;

let rdf_uri = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
match CoreOntologyBundle::by_namespace(rdf_uri) {
    Some(metadata) => {
        println!("Found: {} ({} bytes)", metadata.name, metadata.size);
    }
    None => {
        println!("Ontology not embedded");
    }
}
```

---

#### `by_name(name: &str) -> Option<&'static OntologyMetadata>`

Find an ontology by its short name.

**Parameters:**
- `name` — Short name (e.g., `"rdf"`, `"owl"`, `"dc"`)

**Returns:** `Some(metadata)` if found, `None` otherwise.

**Example:**
```rust
use ggen_core::ontology::CoreOntologyBundle;

if let Some(metadata) = CoreOntologyBundle::by_name("rdf") {
    println!("RDF namespace: {}", metadata.namespace);
}
```

---

#### `available() -> Vec<(&'static str, &'static str)>`

List all embedded ontologies as (name, namespace) pairs.

**Returns:** Vector of (short_name, full_namespace) tuples.

**Example:**
```rust
use ggen_core::ontology::CoreOntologyBundle;

for (name, namespace) in CoreOntologyBundle::available() {
    println!("{}: {}", name, namespace);
}
// Output:
// rdf: http://www.w3.org/1999/02/22-rdf-syntax-ns#
// rdfs: http://www.w3.org/2000/01/rdf-schema#
// owl: http://www.w3.org/2002/07/owl#
// ... (9 more)
```

---

#### `stats() -> BundleStats`

Get statistics about the core bundle.

**Returns:** `BundleStats` struct with size and count information.

**Example:**
```rust
use ggen_core::ontology::CoreOntologyBundle;

let stats = CoreOntologyBundle::stats();
println!("Total ontologies: {}", stats.count);
println!("Total size: {} KB", stats.total_size_bytes / 1024);
println!("Largest: {} ({} bytes)", stats.largest_name, stats.largest_size);
```

---

## OntologyLoader

Unified interface for loading ontologies from multiple sources with fallback chain.

### Location
```rust
use ggen_core::ontology::OntologyLoader;
```

### Fallback Chain

OntologyLoader tries to load ontologies in this order:

1. **Embedded** (compile-time, fastest) — 12 W3C standard ontologies
2. **File** (local filesystem) — `file://` URIs
3. **Marketplace** (downloaded and cached) — Future: remote registry

If source 1 fails, tries source 2. If source 2 fails, tries source 3. Returns error only if all fail.

### Methods

#### `is_embedded(uri: &str) -> bool`

Check if an ontology is available in the core bundle (offline).

**Parameters:**
- `uri` — Full namespace URI

**Returns:** `true` if embedded, `false` otherwise.

**Example:**
```rust
use ggen_core::ontology::OntologyLoader;

if OntologyLoader::is_embedded("http://www.w3.org/1999/02/22-rdf-syntax-ns#") {
    println!("Can generate code offline!");
} else {
    println!("Requires network or file");
}
```

---

#### `load_content(uri: &str, base_path: &Path) -> Result<Vec<u8>>`

Load ontology content from any available source.

**Parameters:**
- `uri` — Ontology URI (supports schemes: `http://`, `file://`, `marketplace:`)
- `base_path` — Base directory for relative file paths

**Returns:** `Ok(bytes)` on success, `Err(error)` on all sources exhausted.

**Example:**
```rust
use ggen_core::ontology::OntologyLoader;
use std::path::Path;

// Embedded ontology (fast)
let rdf_bytes = OntologyLoader::load_content(
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    Path::new(".")
)?;
println!("Loaded {} bytes", rdf_bytes.len());

// File ontology
let custom_bytes = OntologyLoader::load_content(
    "file:///home/user/my-ontology.ttl",
    Path::new(".")
)?;
```

---

#### `get_metadata(uri: &str) -> Result<OntologyMetadata>`

Get metadata about an ontology without loading full content.

**Parameters:**
- `uri` — Ontology URI

**Returns:** `Ok(metadata)` with size and source info, `Err` if not found.

**Example:**
```rust
use ggen_core::ontology::OntologyLoader;

let metadata = OntologyLoader::get_metadata(
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
)?;

println!("Name: {}", metadata.name);
println!("Size: {} bytes", metadata.size);
println!("Source: {}", metadata.source);  // "embedded", "file", "marketplace"
```

---

#### `list_embedded() -> Vec<&'static OntologyMetadata>`

List all embedded core ontologies.

**Returns:** Vector of metadata for all 12 embedded ontologies.

**Example:**
```rust
use ggen_core::ontology::OntologyLoader;

for metadata in OntologyLoader::list_embedded() {
    println!("{}: {} bytes", metadata.name, metadata.size);
}
```

---

## OntologyMetadata

Structure describing an ontology.

### Location
```rust
use ggen_core::ontology::OntologyMetadata;
```

### Fields

```rust
pub struct OntologyMetadata {
    /// Short name (e.g., "rdf")
    pub name: &'static str,
    
    /// Full namespace URI (e.g., "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    pub namespace: &'static str,
    
    /// Size in bytes
    pub size: usize,
    
    /// Embedded RDF content (for embedded ontologies)
    pub content: &'static [u8],
    
    /// Source: "embedded", "file", or "marketplace"
    pub source: &'static str,
}
```

### Methods

#### `to_string() -> String`

Get human-readable description.

**Example:**
```rust
use ggen_core::ontology::CoreOntologyBundle;

if let Some(metadata) = CoreOntologyBundle::by_name("rdf") {
    println!("{}", metadata);
    // Output: RDF (2.1 KB, embedded)
}
```

---

## OntologyInput

Represents an ontology input for the pipeline.

### Location
```rust
use ggen_core::pipeline_engine::OntologyInput;
```

### Constructor Methods

#### `from_namespace(uri: &str) -> Result<Self>`

Create an OntologyInput from a namespace URI.

Automatically resolves using OntologyLoader fallback chain.

**Parameters:**
- `uri` — Ontology URI (supports `http://`, `file://`, `marketplace:` schemes)

**Returns:** `Ok(input)` on success, `Err` if ontology not found.

**Example:**
```rust
use ggen_core::pipeline_engine::OntologyInput;

// From embedded ontology
let input = OntologyInput::from_namespace(
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
)?;

// From file
let input = OntologyInput::from_namespace(
    "file:///home/user/my-ontology.ttl"
)?;

// From marketplace (future)
let input = OntologyInput::from_namespace(
    "marketplace:financial/banking@1.2.1"
)?;
```

---

#### `from_file(path: &Path) -> Result<Self>`

Create an OntologyInput from a file path.

**Parameters:**
- `path` — Path to ontology file (`.ttl`, `.rdf`, `.n3`, etc.)

**Returns:** `Ok(input)` on success, `Err` if file not found or invalid.

**Example:**
```rust
use ggen_core::pipeline_engine::OntologyInput;
use std::path::Path;

let input = OntologyInput::from_file(
    Path::new("schema/domain.ttl")
)?;

println!("Loaded ontology with {} triples", input.triple_count);
```

---

### Fields

```rust
pub struct OntologyInput {
    /// Source URI or file path
    pub uri: String,
    
    /// SHA-256 hash of content
    pub hash: String,
    
    /// Content size in bytes
    pub size_bytes: usize,
    
    /// Number of RDF triples
    pub triple_count: usize,
    
    /// Parsed RDF graph
    pub graph: Graph,
}
```

---

## Epoch

Represents a code generation run with ontology inputs.

### Location
```rust
use ggen_core::pipeline_engine::Epoch;
```

### Constructor Methods

#### `create_with_fallback(base_path: &Path, ontology_uris: &[&str]) -> Result<Self>`

Create an Epoch using OntologyLoader fallback chain.

Loads multiple ontologies and merges them into a single graph.

**Parameters:**
- `base_path` — Base directory for relative file paths
- `ontology_uris` — Array of ontology URIs to load

**Returns:** `Ok(epoch)` on success, `Err` if any ontology not found.

**Example:**
```rust
use ggen_core::pipeline_engine::Epoch;
use std::path::Path;

// Single embedded ontology (offline)
let epoch = Epoch::create_with_fallback(
    Path::new("."),
    &["http://www.w3.org/1999/02/22-rdf-syntax-ns#"]
)?;

// Multiple ontologies (merge all)
let epoch = Epoch::create_with_fallback(
    Path::new("."),
    &[
        "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
        "http://www.w3.org/2000/01/rdf-schema#",
        "file:///home/user/my-ontology.ttl"
    ]
)?;

println!("Epoch ID: {}", epoch.id);
println!("Total triples: {}", epoch.total_triples);
```

---

### Fields

```rust
pub struct Epoch {
    /// Unique ID (SHA-256 hash of all inputs)
    pub id: String,
    
    /// Mapping of source URIs to OntologyInput metadata
    pub inputs: HashMap<PathBuf, OntologyInput>,
    
    /// Merged RDF graph from all inputs
    pub graph: Graph,
    
    /// Total number of RDF triples across all inputs
    pub total_triples: usize,
    
    /// Creation timestamp
    pub created_at: SystemTime,
}
```

---

### Methods

#### `get_input(&uri: &str) -> Option<&OntologyInput>`

Get metadata for a specific input.

**Example:**
```rust
let input = epoch.get_input("http://www.w3.org/1999/02/22-rdf-syntax-ns#")?;
println!("RDF: {} bytes, {} triples", input.size_bytes, input.triple_count);
```

---

#### `to_hash() -> String`

Compute deterministic hash of all inputs.

**Returns:** SHA-256 hex string (64 characters).

**Example:**
```rust
let hash = epoch.to_hash();
println!("Epoch hash: {}", hash);

// Same inputs always produce same hash
let epoch2 = Epoch::create_with_fallback(Path::new("."), &[...])?;
assert_eq!(hash, epoch2.to_hash());  // Deterministic!
```

---

## Examples

### Example 1: Load Embedded Ontology and Extract Classes

```rust
use ggen_core::ontology::OntologyLoader;
use ggen_core::pipeline_engine::Epoch;
use std::path::Path;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Verify RDF is available
    if !OntologyLoader::is_embedded("http://www.w3.org/1999/02/22-rdf-syntax-ns#") {
        eprintln!("RDF ontology not embedded");
        return Err("RDF not available".into());
    }
    
    // Create epoch with only embedded ontology
    let epoch = Epoch::create_with_fallback(
        Path::new("."),
        &["http://www.w3.org/1999/02/22-rdf-syntax-ns#"]
    )?;
    
    println!("Loaded {} triples", epoch.total_triples);
    println!("Epoch ID: {}", epoch.id);
    
    Ok(())
}
```

---

### Example 2: Mix Embedded + File Ontologies

```rust
use ggen_core::pipeline_engine::Epoch;
use std::path::Path;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Load embedded RDF + custom file ontology
    let epoch = Epoch::create_with_fallback(
        Path::new("."),
        &[
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#",  // embedded
            "file:///home/user/my-schema.ttl"               // file
        ]
    )?;
    
    // Use merged graph
    println!("Total ontologies: {}", epoch.inputs.len());
    println!("Total triples: {}", epoch.total_triples);
    
    // Run SPARQL query on merged graph
    let query = r#"
        SELECT ?resource ?label
        WHERE {
            ?resource a rdf:Property .
            ?resource rdfs:label ?label .
        }
    "#;
    
    let results = epoch.graph.query(query)?;
    println!("Found {} properties", results.len());
    
    Ok(())
}
```

---

### Example 3: Verify Determinism

```rust
use ggen_core::pipeline_engine::Epoch;
use std::path::Path;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // First run
    let epoch1 = Epoch::create_with_fallback(
        Path::new("."),
        &["http://www.w3.org/1999/02/22-rdf-syntax-ns#"]
    )?;
    let hash1 = epoch1.to_hash();
    
    // Second run (same inputs)
    let epoch2 = Epoch::create_with_fallback(
        Path::new("."),
        &["http://www.w3.org/1999/02/22-rdf-syntax-ns#"]
    )?;
    let hash2 = epoch2.to_hash();
    
    // Verify determinism
    assert_eq!(hash1, hash2, "Same inputs should produce same hash");
    println!("✅ Deterministic: {}", hash1);
    
    Ok(())
}
```

---

### Example 4: Get Ontology Metadata

```rust
use ggen_core::ontology::CoreOntologyBundle;

fn main() {
    println!("Core Ontology Bundle:");
    
    for metadata in CoreOntologyBundle::all() {
        println!(
            "  {}: {} bytes",
            metadata.name,
            metadata.size
        );
    }
    
    // Get statistics
    let count = CoreOntologyBundle::all().len();
    let total_size: usize = CoreOntologyBundle::all()
        .iter()
        .map(|m| m.size)
        .sum();
    
    println!("\nTotal: {} ontologies, {} KB",
        count,
        total_size / 1024
    );
}
```

---

## Performance Characteristics

| Operation | Time | Notes |
|-----------|------|-------|
| `is_embedded()` | <1 μs | In-memory lookup |
| `CoreOntologyBundle::by_name()` | <1 μs | Array search |
| `CoreOntologyBundle::by_namespace()` | <1 μs | Array search |
| `load_content()` (embedded) | <1 ms | Read from binary |
| `load_content()` (file) | 5-50 ms | Depends on file size |
| `Epoch::create_with_fallback()` | 10-100 ms | Parse + merge RDF |
| `epoch.to_hash()` | <10 ms | SHA-256 of inputs |

---

## Error Handling

All methods return `Result<T, E>` for proper error handling:

```rust
use ggen_core::ontology::OntologyLoader;

match OntologyLoader::load_content("http://example.com/ontology.ttl", Path::new(".")) {
    Ok(bytes) => {
        println!("Loaded {} bytes", bytes.len());
    }
    Err(e) => {
        eprintln!("Failed to load ontology: {}", e);
        // Handle error...
    }
}
```

Common error types:
- `OntologyNotFound` — Ontology not in embedded bundle or file
- `FileNotFound` — File URL points to non-existent file
- `ParseError` — RDF content is invalid
- `NetworkError` — Marketplace download failed (future)

---

## Thread Safety

All types are thread-safe:
- `CoreOntologyBundle` — Zero heap allocation, immutable
- `OntologyLoader` — Uses `&'static` references
- `Epoch` — `Send + Sync`

Safe to use in concurrent code:

```rust
use std::thread;
use ggen_core::ontology::CoreOntologyBundle;

thread::scope(|s| {
    for _ in 0..10 {
        s.spawn(|| {
            // Thread-safe access to embedded ontologies
            let all = CoreOntologyBundle::all();
            println!("Thread found {} ontologies", all.len());
        });
    }
});
```

---

## See Also

- [GETTING_STARTED.md](./GETTING_STARTED.md) — Quick start guide
- [USAGE_GUIDE.md](./USAGE_GUIDE.md) — Common workflows
- [Source code](../crates/ggen-core/src/ontology/) — Full implementation
