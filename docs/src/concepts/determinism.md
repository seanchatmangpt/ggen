<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Deterministic Code Generation](#deterministic-code-generation)
  - [Why Determinism Matters](#why-determinism-matters)
  - [The Determinism Guarantee](#the-determinism-guarantee)
  - [How ggen Achieves Determinism](#how-ggen-achieves-determinism)
    - [1. Content Hashing](#1-content-hashing)
    - [2. Sorted RDF Graphs](#2-sorted-rdf-graphs)
    - [3. Ordered SPARQL Results](#3-ordered-sparql-results)
    - [4. Version-Locked Templates](#4-version-locked-templates)
  - [Manifest Key Calculation](#manifest-key-calculation)
    - [For Local Templates](#for-local-templates)
    - [For Marketplace Gpacks](#for-marketplace-gpacks)
  - [Hash Components Explained](#hash-components-explained)
    - [Graph Hash](#graph-hash)
    - [Shapes Hash](#shapes-hash)
    - [Frontmatter Hash](#frontmatter-hash)
    - [Rows Hash](#rows-hash)
  - [Chicago TDD Validation](#chicago-tdd-validation)
    - [The 782-Line End-to-End Test](#the-782-line-end-to-end-test)
    - [What the Test Validates](#what-the-test-validates)
    - [Test Execution](#test-execution)
  - [Determinism in Practice](#determinism-in-practice)
    - [Example 1: Same Inputs → Identical Outputs](#example-1-same-inputs--identical-outputs)
    - [Example 2: Cross-Environment Consistency](#example-2-cross-environment-consistency)
    - [Example 3: Git-Friendly Diffs](#example-3-git-friendly-diffs)
  - [Version Locking with Gpacks](#version-locking-with-gpacks)
    - [Lockfile Structure](#lockfile-structure)
    - [Installing Specific Versions](#installing-specific-versions)
  - [Debugging Determinism Issues](#debugging-determinism-issues)
    - [Enable Trace Logging](#enable-trace-logging)
    - [Compare Manifest Keys](#compare-manifest-keys)
    - [Check SPARQL Ordering](#check-sparql-ordering)
  - [Best Practices for Deterministic Generation](#best-practices-for-deterministic-generation)
  - [The Bottom Line](#the-bottom-line)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Deterministic Code Generation

## Why Determinism Matters

**Determinism** means: **Same inputs always produce byte-identical outputs.**

This is critical for:

1. **Reproducible builds**: CI/CD can verify generated code hasn't changed unexpectedly
2. **Git-friendly**: Only meaningful changes appear in diffs, not random ordering
3. **Cacheable**: Build systems can cache outputs based on input hashes
4. **Trustworthy**: Developers can confidently regenerate without fear of breaking changes
5. **Auditable**: Verify that generated code matches declared inputs

Without determinism, code generation is **unpredictable chaos**:

```bash
# Non-deterministic generator
$ codegen --input schema.json
# Output: model.rs (1,234 bytes, fields in random order)

$ codegen --input schema.json
# Output: model.rs (1,234 bytes, DIFFERENT field order)
# Git diff shows 50 lines changed, but semantically identical!
```

**With determinism:**

```bash
# ggen deterministic generator
$ ggen gen model.tmpl --graph schema.ttl
# Output: model.rs (1,234 bytes, SHA256: abc123...)

$ ggen gen model.tmpl --graph schema.ttl
# Output: model.rs (1,234 bytes, SHA256: abc123...)
# Byte-identical. Git diff shows ZERO changes.
```

## The Determinism Guarantee

ggen provides a **cryptographic determinism guarantee**:

```
Same RDF graph + Same template + Same variables
    ⇒ Byte-identical output
    ⇒ Same SHA-256 hash
```

This guarantee holds across:

- **Machines**: Mac, Linux, Windows produce identical output
- **Environments**: Dev, CI, production generate the same code
- **Time**: Generate today or next year, result is identical
- **Users**: Different developers get the same output

## How ggen Achieves Determinism

### 1. Content Hashing

Every input to code generation is hashed using **SHA-256**:

```rust
use sha2::{Sha256, Digest};

fn hash_content(content: &str) -> String {
    let mut hasher = Sha256::new();
    hasher.update(content.as_bytes());
    format!("{:x}", hasher.finalize())
}
```

This produces a **deterministic fingerprint** of inputs.

### 2. Sorted RDF Graphs

RDF triples are inherently **unordered** (they're a set, not a list). To make them deterministic, ggen:

1. **Serializes** the graph to N-Quads format (canonical RDF syntax)
2. **Sorts** triples lexicographically
3. **Hashes** the sorted output

```turtle
# Input RDF (order may vary)
pc:Product pc:name "Widget" .
pc:Product pc:price 99.99 .

# Sorted N-Quads (deterministic order)
<http://example.org/product_catalog#Product> <http://example.org/product_catalog#name> "Widget" .
<http://example.org/product_catalog#Product> <http://example.org/product_catalog#price> "99.99"^^<http://www.w3.org/2001/XMLSchema#decimal> .
```

**Result:** Same RDF graph → Same hash, regardless of input order.

### 3. Ordered SPARQL Results

SPARQL queries **must include `ORDER BY`** to guarantee deterministic results:

```sparql
# ❌ Non-deterministic (unordered)
SELECT ?property ?datatype WHERE {
    ?property rdfs:domain pc:Product .
    ?property rdfs:range ?datatype .
}

# ✅ Deterministic (ordered)
SELECT ?property ?datatype WHERE {
    ?property rdfs:domain pc:Product .
    ?property rdfs:range ?datatype .
}
ORDER BY ?property
```

ggen **enforces** `ORDER BY` in matrix queries. Templates without `ORDER BY` are rejected.

### 4. Version-Locked Templates

Marketplace gpacks use **semantic versioning** and **lockfiles**:

```toml
# ggen.lock
[gpacks]
"io.ggen.rust.models" = "0.2.1"
"io.ggen.typescript.types" = "1.3.0"

[dependencies]
"io.ggen.rust.models" = {
    version = "0.2.1",
    source = "registry",
    checksum = "sha256:abc123..."
}
```

**Result:** Same gpack version → Same template → Same output.

## Manifest Key Calculation

Every generation operation produces a **manifest key** (SHA-256 hash) that uniquely identifies the inputs.

### For Local Templates

```
K = SHA256(seed || graph_hash || shapes_hash || frontmatter_hash || rows_hash)
```

Where:

- `seed`: Random seed for reproducibility (default: fixed value)
- `graph_hash`: Hash of sorted RDF graph (N-Quads)
- `shapes_hash`: Hash of SHACL validation shapes (N-Quads)
- `frontmatter_hash`: Hash of template frontmatter (YAML)
- `rows_hash`: Hash of SPARQL query results (ordered)

### For Marketplace Gpacks

```
K = SHA256(seed || gpack_version || gpack_deps_hash || graph_hash || shapes_hash || frontmatter_hash || rows_hash)
```

Additional components:

- `gpack_version`: Exact version from `ggen.toml` (e.g., `0.2.1`)
- `gpack_deps_hash`: Hash of all dependency versions

**Key insight:** Changing **any input** changes the manifest key, triggering regeneration.

## Hash Components Explained

### Graph Hash

**Purpose:** Ensure RDF ontology changes are detected.

**Algorithm:**

1. Load RDF graph into Oxigraph
2. Export to N-Quads format (canonical RDF syntax)
3. Sort triples lexicographically
4. Compute SHA-256 of sorted output

**Example:**

```turtle
# Input: product_catalog.ttl
pc:Product a rdfs:Class .
pc:name rdfs:domain pc:Product ; rdfs:range xsd:string .
pc:price rdfs:domain pc:Product ; rdfs:range xsd:decimal .
```

```
# Sorted N-Quads
<http://ex.org/product_catalog#Product> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2000/01/rdf-schema#Class> .
<http://ex.org/product_catalog#name> <http://www.w3.org/2000/01/rdf-schema#domain> <http://ex.org/product_catalog#Product> .
<http://ex.org/product_catalog#name> <http://www.w3.org/2000/01/rdf-schema#range> <http://www.w3.org/2001/XMLSchema#string> .
<http://ex.org/product_catalog#price> <http://www.w3.org/2000/01/rdf-schema#domain> <http://ex.org/product_catalog#Product> .
<http://ex.org/product_catalog#price> <http://www.w3.org/2000/01/rdf-schema#range> <http://www.w3.org/2001/XMLSchema#decimal> .

→ SHA256: a3f2c8b1...
```

### Shapes Hash

**Purpose:** Detect SHACL validation changes.

**Algorithm:** Same as graph hash, but for SHACL shapes file.

```turtle
# shapes.ttl
pc:ProductShape a sh:NodeShape ;
    sh:targetClass pc:Product ;
    sh:property [
        sh:path pc:name ;
        sh:datatype xsd:string ;
        sh:minCount 1 ;
    ] .
```

→ Sorted N-Quads → SHA256

### Frontmatter Hash

**Purpose:** Detect template metadata changes.

**Algorithm:**

1. Extract YAML frontmatter from template
2. Canonicalize YAML (sorted keys)
3. Render Handlebars expressions in frontmatter
4. Compute SHA-256

**Example:**

```yaml
---
to: src/models/{{ class_name }}.rs
vars:
  class_name: Product
matrix:
  query: |
    SELECT ?property WHERE { ... }
    ORDER BY ?property
---
```

→ Rendered frontmatter → SHA256

### Rows Hash

**Purpose:** Detect SPARQL query result changes.

**Algorithm:**

1. Execute SPARQL query from template
2. Serialize results to ordered JSON
3. Compute SHA-256

**Example:**

```sparql
SELECT ?property ?datatype WHERE {
    ?property rdfs:domain pc:Product .
    ?property rdfs:range ?datatype .
}
ORDER BY ?property
```

```json
{
  "results": [
    {"property": "pc:name", "datatype": "xsd:string"},
    {"property": "pc:price", "datatype": "xsd:decimal"}
  ]
}
```

→ SHA256

## Chicago TDD Validation

ggen's determinism is **validated by a comprehensive end-to-end test** using Chicago TDD principles.

### The 782-Line End-to-End Test

**File:** `tests/chicago_tdd/ontology_driven_e2e.rs`

**Test name:** `test_ontology_to_code_generation_workflow`

**What it tests:**

1. Create RDF ontology v1 (Product, Category, Supplier)
2. Generate Rust code from ontology v1
3. Verify generated code contains expected structs and fields
4. Modify ontology to v2 (add SKU, rating, inventory properties)
5. Regenerate Rust code from ontology v2
6. Verify new properties appear in generated code
7. Verify code delta matches ontology delta

**Test principles:**

- **Real RDF graphs** (no mocks) loaded into Oxigraph
- **Real SPARQL queries** executed against Oxigraph
- **Real file I/O** (templates, generated code)
- **Real template rendering** with Handlebars
- **Real code validation** (struct definitions, field types)

### What the Test Validates

**Determinism aspects:**

1. **Reproducibility**: Running generation twice produces identical code
2. **Graph ordering**: RDF graph is processed in deterministic order
3. **Query ordering**: SPARQL results are consistently ordered
4. **Type mapping**: `xsd:string` → `String`, `xsd:decimal` → `f64`
5. **Evolution**: Ontology changes propagate correctly to code

**Example assertions:**

```rust
// V1 ontology generates V1 code
assert_code_contains(&code_v1, "struct Product", "v1 should have Product struct");
assert_code_contains(&code_v1, "name: String", "v1 Product should have name field");
assert_code_contains(&code_v1, "price: f64", "v1 Product should have price field");
assert_code_not_contains(&code_v1, "sku", "v1 should NOT have SKU field yet");

// V2 ontology generates V2 code with NEW fields
assert_code_contains(&code_v2, "struct Product", "v2 should still have Product struct");
assert_code_contains(&code_v2, "sku: String", "v2 should have NEW SKU field from ontology");
assert_code_contains(&code_v2, "rating: f64", "v2 should have NEW rating field from ontology");
assert_code_contains(&code_v2, "inventory_count: i32", "v2 should have NEW inventory field from ontology");

// Verify code delta matches ontology delta
assert_eq!(code_diff.new_fields, 3, "Should have 3 new fields");
assert_eq!(code_diff.new_methods, 1, "Should have 1 new method");
```

### Test Execution

```bash
# Run the Chicago TDD end-to-end test
cargo test --test chicago_tdd ontology_driven_e2e -- --nocapture

# Output shows:
# [1/6] Parsing RDF...
# [2/6] Extracting project structure...
# [3/6] Validating project...
# [4/6] Live Preview...
# [5/6] Generating workspace structure...
# [6/6] Running post-generation hooks...
# ✅ Generation Complete!
```

**Passing this test proves:**

- Deterministic RDF loading (Oxigraph)
- Deterministic SPARQL execution (ordered results)
- Deterministic code generation (same inputs → same outputs)
- Deterministic evolution (ontology changes → code changes)

## Determinism in Practice

### Example 1: Same Inputs → Identical Outputs

```bash
# First generation
$ ggen gen rust/models.tmpl --graph product_catalog.ttl
Generated: src/models/product.rs (1,234 bytes)
Manifest key: sha256:a3f2c8b1e4d5f6a7...

# Second generation (identical inputs)
$ ggen gen rust/models.tmpl --graph product_catalog.ttl
Generated: src/models/product.rs (1,234 bytes)
Manifest key: sha256:a3f2c8b1e4d5f6a7...

# Verify byte-identical
$ sha256sum src/models/product.rs
a3f2c8b1e4d5f6a7... src/models/product.rs
```

**Git diff shows ZERO changes:**

```bash
$ git diff
# No output - files are identical
```

### Example 2: Cross-Environment Consistency

**Developer 1 (Mac):**

```bash
$ ggen gen rust/models.tmpl --graph product_catalog.ttl
Manifest key: sha256:a3f2c8b1...
```

**Developer 2 (Linux):**

```bash
$ ggen gen rust/models.tmpl --graph product_catalog.ttl
Manifest key: sha256:a3f2c8b1...
```

**CI Pipeline (Ubuntu):**

```bash
$ ggen gen rust/models.tmpl --graph product_catalog.ttl
Manifest key: sha256:a3f2c8b1...
```

**All three environments produce byte-identical outputs.**

### Example 3: Git-Friendly Diffs

**Scenario:** Add `rating` field to Product ontology.

```diff
# product_catalog.ttl
pc:Product a rdfs:Class .
pc:name rdfs:domain pc:Product ; rdfs:range xsd:string .
pc:price rdfs:domain pc:Product ; rdfs:range xsd:decimal .
+pc:rating rdfs:domain pc:Product ; rdfs:range xsd:decimal .
```

```bash
$ ggen gen rust/models.tmpl --graph product_catalog.ttl
```

**Git diff shows ONLY the new field:**

```diff
# src/models/product.rs
pub struct Product {
    pub name: String,
    pub price: f64,
+   pub rating: f64,
}
```

**No random reordering. No unrelated changes. Just the semantic diff.**

## Version Locking with Gpacks

Marketplace gpacks use **lockfiles** to ensure version determinism.

### Lockfile Structure

```toml
# ggen.lock
[lockfile]
version = "1.0"

[gpacks]
"io.ggen.rust.models" = "0.2.1"
"io.ggen.typescript.types" = "1.3.0"

[dependencies]
"io.ggen.rust.models" = {
    version = "0.2.1",
    source = "registry",
    checksum = "sha256:abc123def456..."
}
"io.ggen.macros.std" = {
    version = "0.2.0",
    source = "registry",
    checksum = "sha256:789ghi012jkl..."
}
```

### Installing Specific Versions

```bash
# Install exact version
$ ggen add io.ggen.rust.models@0.2.1

# Lockfile records version
$ cat ggen.lock
[gpacks]
"io.ggen.rust.models" = "0.2.1"

# All future generations use locked version
$ ggen gen io.ggen.rust.models:models.tmpl --graph product_catalog.ttl
# Uses version 0.2.1 (locked)
```

**Commit the lockfile:**

```bash
$ git add ggen.lock
$ git commit -m "Lock gpack versions for deterministic builds"
```

**Now CI and other developers use the EXACT same template versions.**

## Debugging Determinism Issues

### Enable Trace Logging

```bash
# Show hash components during generation
$ GGEN_TRACE=1 ggen gen rust/models.tmpl --graph product_catalog.ttl

# Output:
# Manifest key calculation:
#   seed: 0x00000000
#   graph_hash: sha256:a3f2c8b1...
#   shapes_hash: sha256:e4d5f6a7...
#   frontmatter_hash: sha256:b8c9d0e1...
#   rows_hash: sha256:f2a3b4c5...
# → manifest_key: sha256:1234abcd...
```

### Compare Manifest Keys

```bash
# Generate on machine A
$ ggen gen rust/models.tmpl --graph product_catalog.ttl
Manifest key: sha256:1234abcd...

# Generate on machine B
$ ggen gen rust/models.tmpl --graph product_catalog.ttl
Manifest key: sha256:5678efgh...  # ❌ Different!

# Enable tracing to find the difference
$ GGEN_TRACE=1 ggen gen rust/models.tmpl --graph product_catalog.ttl
# Check which hash component differs
```

### Check SPARQL Ordering

**Problem:** Query results in different order.

**Solution:** Add `ORDER BY` to SPARQL query.

```sparql
# Before (non-deterministic)
SELECT ?property ?datatype WHERE {
    ?property rdfs:domain ?class .
    ?property rdfs:range ?datatype .
}

# After (deterministic)
SELECT ?property ?datatype WHERE {
    ?property rdfs:domain ?class .
    ?property rdfs:range ?datatype .
}
ORDER BY ?property ?datatype
```

## Best Practices for Deterministic Generation

1. **Always use `ORDER BY` in SPARQL queries**

   ```sparql
   SELECT ?x ?y WHERE { ... } ORDER BY ?x ?y
   ```

2. **Pin gpack versions in production**

   ```bash
   ggen add io.ggen.rust.models@0.2.1  # Not @latest
   ```

3. **Commit lockfiles to version control**

   ```bash
   git add ggen.lock
   git commit -m "Lock template versions"
   ```

4. **Validate in CI**

   ```bash
   # .github/workflows/codegen.yml
   - name: Verify determinism
     run: |
       ggen gen rust/models.tmpl --graph product_catalog.ttl
       git diff --exit-code src/models/product.rs
   ```

5. **Use canonical RDF formats**

   - Prefer Turtle (`.ttl`) for readability
   - ggen canonicalizes to N-Quads internally

6. **Avoid timestamps in templates**

   ```handlebars
   // ❌ Non-deterministic
   // Generated at: {{ current_timestamp }}

   // ✅ Deterministic
   // Generated from: product_catalog.ttl
   ```

7. **Test with Chicago TDD principles**

   - Use real RDF graphs (no mocks)
   - Verify byte-identical regeneration
   - Test ontology evolution scenarios

## The Bottom Line

**ggen's determinism guarantee:**

```
Same inputs + Same environment = Byte-identical outputs
```

This is **not a goal**. It's a **tested, validated, cryptographically-guaranteed property** of the system.

The 782-line Chicago TDD test proves it. The SHA-256 manifest keys enforce it. The lockfiles preserve it.

**You can trust ggen to generate the exact same code, every single time.**
