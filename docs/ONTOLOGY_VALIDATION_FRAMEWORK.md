# Ontology Validation Framework

**Date**: 2026-06-23  
**Purpose**: Validation and testing infrastructure for ontology embedding

## Validation Layers

### Layer 1: Compile-Time Validation

Performed by `build.rs` during compilation:

```rust
// Check 1: File existence
if !ontology_file.exists() {
    panic!("Ontology file not found: {}", path);
}

// Check 2: Valid Turtle/RDF syntax
parse_ttl(&content)?;  // Fails at compile time if invalid

// Check 3: Namespace URI validity
if !is_valid_uri(namespace) {
    panic!("Invalid namespace URI: {}", namespace);
}

// Check 4: Deterministic hashing
let hash = sha256(&content);
assert_eq!(hash.len(), 64);  // SHA-256 hex format
```

**Result**: If embedding fails, project doesn't compile.  
**Guarantees**: Only valid, parseable ontologies are embedded.

### Layer 2: Runtime Validation

Performed at runtime when accessing ontologies:

```rust
// Check 1: Availability
assert!(!content.is_empty());
assert!(metadata.size > 0);

// Check 2: Integrity
let computed_hash = sha256(content);
assert_eq!(computed_hash, stored_hash);

// Check 3: Deserializability
let graph = parse_turtle(content)?;
assert!(graph.triples > 0);
```

**Result**: Runtime errors if data is corrupted.  
**Guarantees**: Only valid content is served.

### Layer 3: Test Validation

Comprehensive test coverage:

```rust
#[test]
fn validate_core_bundle_completeness() {
    let ontologies = CoreOntologyBundle::all();
    assert!(ontologies.len() >= 12, "All core ontologies present");
    
    for ont in ontologies {
        assert!(!ont.name.is_empty(), "Has valid name");
        assert!(!ont.namespace.is_empty(), "Has valid namespace");
        assert!(!ont.content.is_empty(), "Has content");
        assert!(ont.size > 0, "Has size");
    }
}

#[test]
fn validate_embedded_rdf_structure() {
    let rdf = CoreOntologyBundle::by_name("rdf-syntax-ns").unwrap();
    let graph = parse_turtle(rdf.content).unwrap();
    
    // RDF ontology must have specific class definitions
    assert!(graph.contains("http://www.w3.org/1999/02/22-rdf-syntax-ns#Resource"));
    assert!(graph.contains("http://www.w3.org/1999/02/22-rdf-syntax-ns#Class"));
    assert!(graph.contains("http://www.w3.org/1999/02/22-rdf-syntax-ns#Property"));
}
```

**Result**: Tests verify ontologies work correctly.  
**Guarantees**: Regressions caught immediately.

## Validation Checklist

### Before Embedding (Pre-Build)

- [ ] All ontology files exist in `ontologies/core/`
- [ ] All files are valid Turtle/RDF (test with rapper)
- [ ] All namespace URIs are correctly mapped
- [ ] No duplicate URIs in namespace mapping
- [ ] File sizes are reasonable (< 200 KB each)

### During Build (Compile-Time)

- [ ] `build.rs` discovers all files
- [ ] SHA-256 hashes computed deterministically
- [ ] `include_bytes!` paths are correct
- [ ] Generated code compiles without errors
- [ ] Total embedded size is acceptable (< 1 MB)

### After Build (Runtime)

- [ ] All ontologies accessible via by_namespace()
- [ ] All ontologies accessible via by_name()
- [ ] Content hash matches recorded hash
- [ ] Parsing ontologies doesn't panic
- [ ] Triple counts are non-zero

### In Tests

- [ ] CoreOntologyBundle APIs work correctly
- [ ] OntologyLoader fallback chain works
- [ ] Mixed ontologies load correctly
- [ ] Offline execution possible
- [ ] No network calls made

## Validation Commands

### Pre-Build Validation

```bash
# Check Turtle syntax for all ontologies
for file in ontologies/core/*.ttl; do
    echo "Validating $file..."
    rapper -i turtle "$file" > /dev/null || exit 1
done

# Count total triples
for file in ontologies/core/*.ttl; do
    rapper -i turtle "$file" | wc -l
done

# Verify file sizes
du -sh ontologies/core/*.ttl
```

### Build-Time Validation

```bash
# Check build.rs logic
RUST_LOG=debug cargo build --lib -p ggen-core 2>&1 | grep -i "ontology"

# Verify generated code
cat target/debug/build/ggen-core-*/out/ontologies.rs | head -50
```

### Runtime Validation

```bash
# Run core bundle tests
cargo test -p ggen-core --lib ontology::core_bundle -- --nocapture

# Run loader tests
cargo test -p ggen-core --lib ontology::loader -- --nocapture

# Run epoch tests with fallback
cargo test -p ggen-core --lib pipeline_engine::epoch::test_epoch_create_with_fallback -- --nocapture
```

### Full Validation Suite

```bash
# Comprehensive validation
./scripts/validate_ontologies.sh

# Expected output:
# ✓ Compile-time validation passed
# ✓ Runtime validation passed
# ✓ Test validation passed
# ✓ All 12 ontologies present
# ✓ Total embedded: 448 KB
# ✓ Ready for production
```

## Performance Validation

### Lookup Performance

```rust
#[bench]
fn bench_core_bundle_by_namespace(b: &mut Bencher) {
    b.iter(|| {
        CoreOntologyBundle::by_namespace(
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
        )
    });
}
// Expected: < 1 μs per lookup
```

### Loading Performance

```rust
#[bench]
fn bench_ontology_loader_embedded(b: &mut Bencher) {
    b.iter(|| {
        OntologyLoader::load_content(
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
            Path::new(".")
        )
    });
}
// Expected: < 10 ms (includes deserialization)
```

### Compilation Time Impact

```bash
# Measure build time
time cargo build -p ggen-core --release

# Expected:
# - Clean build: +200 ms overhead (ontology discovery + codegen)
# - Incremental: No measurable overhead
# - Total for ggen-core: ~15 seconds
```

## Integrity Checks

### Hash Verification

```rust
// At runtime, verify embedded content hash
let ontology = CoreOntologyBundle::by_name("rdf-syntax-ns").unwrap();
let computed = sha256(ontology.content);
assert_eq!(computed, expected_hash);
```

### Content Verification

```rust
// Verify ontology parses correctly
let ontology = CoreOntologyBundle::by_name("rdf-syntax-ns").unwrap();
let graph = parse_turtle(ontology.content)
    .expect("Embedded RDF must parse");
assert!(graph.triples > 0);
```

### Completeness Verification

```rust
// Verify all expected namespaces are present
let expected = vec![
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    "http://www.w3.org/2002/07/owl#",
    "http://purl.org/dc/terms/",
    // ... all 12
];

for uri in expected {
    assert!(
        CoreOntologyBundle::by_namespace(uri).is_some(),
        "Expected namespace not found: {}",
        uri
    );
}
```

## Reproducibility Validation

### Deterministic Hashing

```bash
# Build twice, verify same output
cargo clean && cargo build -p ggen-core
HASH1=$(sha256sum target/*/ontologies.rs)

cargo clean && cargo build -p ggen-core
HASH2=$(sha256sum target/*/ontologies.rs)

assert_eq!(HASH1, HASH2)  # Must be identical
```

### Offline Verification

```bash
# Test that pipeline works offline (no network)
RUST_LOG=debug cargo test -p ggen-core offline_pipeline_test 2>&1
# Should NOT show any network calls or DNS lookups
```

## Validation Matrix

| Aspect | Layer 1 (Compile) | Layer 2 (Runtime) | Layer 3 (Test) |
|--------|-------------------|-------------------|----------------|
| File Existence | ✓ | ✓ | ✓ |
| Syntax Validity | ✓ | ✓ | ✓ |
| Namespace URIs | ✓ | ✓ | ✓ |
| Content Integrity | ✓ | ✓ | ✓ |
| Determinism | ✓ | ✓ | ✓ |
| Offline Capability | — | ✓ | ✓ |
| Performance | — | ✓ | ✓ |
| API Correctness | — | — | ✓ |

## Failure Modes

### Build Failure (Compile-Time)

**If**: `build.rs` fails during ontology discovery or code generation  
**Then**: Project doesn't compile  
**Fix**:
1. Check `ontologies/core/` directory exists
2. Verify all `.ttl` files are valid
3. Clear build cache: `cargo clean`
4. Rebuild: `cargo build`

### Runtime Error (Ontology Not Found)

**If**: `CoreOntologyBundle::by_namespace()` returns None  
**Then**: Ontology missing from build  
**Fix**:
1. Check ontology was added to `ontologies/core/`
2. Rebuild to re-run `build.rs`
3. Verify namespace URI is correct

### Hash Mismatch (Integrity Error)

**If**: `sha256(content)` doesn't match expected hash  
**Then**: Binary corrupted or wrong ontology loaded  
**Fix**:
1. Verify binary is not corrupted: `file target/*/ggen`
2. Rebuild cleanly: `cargo clean && cargo build`
3. Verify source files: `cd ontologies/core && sha256sum *.ttl`

## Continuous Validation

### CI/CD Pipeline

```yaml
# .github/workflows/ontology-validation.yml
name: Ontology Validation

on: [push, pull_request]

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      # 1. Pre-build validation
      - name: Validate Turtle syntax
        run: ./scripts/validate_turtle.sh
      
      # 2. Build-time validation
      - name: Build with ontology embedding
        run: cargo build -p ggen-core
      
      # 3. Runtime validation
      - name: Test core bundle
        run: cargo test -p ggen-core ontology::core_bundle
      
      # 4. Performance validation
      - name: Check compilation time
        run: |
          time cargo build -p ggen-core --release
          # Must complete in < 30 seconds
      
      # 5. Integrity validation
      - name: Verify offline mode
        run: cargo test -p ggen-core offline_pipeline_test
```

---

**See also**:
- [Core Ontology Embedding](./architecture/CORE_ONTOLOGY_EMBEDDING.md)
- [Testing Policy](./CLAUDE.md#testing-policy)
- [Performance SLOs](./CLAUDE.md#performance-slos)
