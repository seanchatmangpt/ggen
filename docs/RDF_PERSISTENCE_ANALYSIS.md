# RDF Persistence Analysis - Root Cause & Solution

**Date**: 2025-12-11
**Status**: Root cause identified, solution confirmed
**Priority**: CRITICAL - Blocks entire ontology-driven development thesis

---

## 🔍 Root Cause Analysis

### Problem Statement
```bash
$ ggen graph load --file electric-api.ttl
{"triples_loaded":93}  # ✅ Loads successfully

$ ggen graph query --sparql_query "SELECT ?s ?p ?o LIMIT 10"
{"bindings":[], "result_count":0}  # ❌ Returns EMPTY!
```

**Why**: Each command creates a NEW in-memory graph. Nothing persists.

---

## 📋 Code Locations - Current Implementation

### 1. **Graph Load Command** (IN-MEMORY ONLY)
**File**: `crates/ggen-domain/src/graph/load.rs:107`
```rust
// PROBLEM: Creates NEW in-memory graph every time
let graph = Graph::load_from_file(&options.file_path)?;

// After loading: graph is DROPPED (not persisted)
```

### 2. **Graph Query Command** (IN-MEMORY ONLY)
**File**: `crates/ggen-domain/src/graph/query.rs:82-86`
```rust
// PROBLEM: Creates NEW empty graph or loads file into NEW graph
let graph = if let Some(graph_file) = &options.graph_file {
    Graph::load_from_file(graph_file)?  // NEW graph from file
} else {
    Graph::new()?  // NEW empty graph
};

// After query: graph is DROPPED
```

### 3. **Template Rendering** (IN-MEMORY ONLY)
**File**: `crates/ggen-domain/src/template/render_with_rdf.rs:171`
```rust
// PROBLEM: Creates NEW in-memory graph
let mut graph = Graph::new()?;

// Loads RDF files, renders template, then graph is DROPPED
```

---

## ✅ Solution - Persistent Storage (ALREADY EXISTS!)

### **GraphStore API** (Working Implementation)
**File**: `crates/ggen-core/src/graph/store.rs`

```rust
/// Open or create persistent RDF store
let store = GraphStore::open(".ggen/rdf-store")?;

/// Create graph from persistent store
let graph = store.create_graph()?;

// All commands using this graph share the SAME data!
// Data persists across program runs via RocksDB
```

**Proof**: Tests confirm this works (lines 177-204):
```rust
#[test]
fn test_store_persistent_storage() {
    // Create store and add data
    let store1 = GraphStore::open(&store_path).unwrap();
    let graph1 = store1.create_graph().unwrap();
    graph1.insert_turtle(r#"..."#).unwrap();
    drop(graph1);
    drop(store1);

    // Reopen store - data is still there!
    let store2 = GraphStore::open(&store_path).unwrap();
    let graph2 = store2.create_graph().unwrap();
    assert!(!graph2.is_empty());  // ✅ Data persisted!
}
```

---

## 🎯 Commands That ALREADY Use RDF Correctly

### 1. **Template Rendering with RDF** ✅
**File**: `crates/ggen-domain/src/template/render_with_rdf.rs`
**Function**: `render_with_rdf(options: &RenderWithRdfOptions)`

**What it does**:
1. Creates graph: `let mut graph = Graph::new()?;`
2. Loads RDF files from options
3. Renders template with SPARQL context
4. Writes output

**Test**: Line 440 proves it works with inline RDF

### 2. **End-to-End Ontology Test** ✅
**File**: `tests/chicago_tdd/ontology_driven_e2e.rs`
**Function**: `test_ontology_to_code_generation_workflow()`

**What it does**:
1. Creates ontology (Product Catalog v1)
2. Queries with SPARQL: `execute_query(QueryInput {...})`
3. Generates Rust code: `render_with_rdf()`
4. Modifies ontology (v2 with new properties)
5. Regenerates code and verifies changes propagate

**This proves the COMPLETE workflow works!**

### 3. **Metadata Store** ✅
**File**: `crates/ggen-domain/src/rdf/metadata.rs:292-299`

```rust
/// Create persistent metadata store at path
pub fn open<P: AsRef<Path>>(path: P) -> Result<Self> {
    let store = Store::open(path.as_ref())?;
    Ok(Self {
        store: Arc::new(Mutex::new(store)),
        // ...
    })
}
```

**Uses**: Persistent Oxigraph store for template metadata

---

## 🔧 Implementation Plan

### Step 1: Add Persistent Store Helper
**File**: `crates/ggen-domain/src/graph/store_manager.rs` (NEW)

```rust
use ggen_core::graph::GraphStore;
use ggen_utils::error::Result;
use std::path::PathBuf;

/// Get or create the persistent RDF store
pub fn get_persistent_store() -> Result<GraphStore> {
    // Use .ggen/rdf-store as default location
    let store_path = get_store_path()?;
    GraphStore::open(store_path)
}

/// Get the default store path
fn get_store_path() -> Result<PathBuf> {
    // Check for override via environment
    if let Ok(path) = std::env::var("GGEN_RDF_STORE") {
        return Ok(PathBuf::from(path));
    }

    // Use default: .ggen/rdf-store
    let store_dir = PathBuf::from(".ggen");
    std::fs::create_dir_all(&store_dir)?;
    Ok(store_dir.join("rdf-store"))
}
```

### Step 2: Update Graph Load Command
**File**: `crates/ggen-domain/src/graph/load.rs:94-123`

```rust
// BEFORE (in-memory only):
pub fn load_rdf(options: LoadOptions) -> Result<LoadStats> {
    let graph = Graph::load_from_file(&options.file_path)?;
    // ...
}

// AFTER (persistent):
pub fn load_rdf(options: LoadOptions) -> Result<LoadStats> {
    // Get persistent store
    let store = crate::graph::store_manager::get_persistent_store()?;
    let graph = store.create_graph()?;

    // Load RDF file into persistent store
    let ttl_content = std::fs::read_to_string(&options.file_path)?;
    graph.insert_turtle(&ttl_content)?;

    // Count triples
    let total_triples = graph.len();
    // ...
}
```

### Step 3: Update Graph Query Command
**File**: `crates/ggen-domain/src/graph/query.rs:77-93`

```rust
// BEFORE (in-memory only):
pub fn execute_sparql(options: QueryOptions) -> Result<QueryResult> {
    let graph = if let Some(graph_file) = &options.graph_file {
        Graph::load_from_file(graph_file)?
    } else {
        Graph::new()?
    };
    // ...
}

// AFTER (persistent):
pub fn execute_sparql(options: QueryOptions) -> Result<QueryResult> {
    let graph = if let Some(graph_file) = &options.graph_file {
        // One-time query: load file into temporary graph
        Graph::load_from_file(graph_file)?
    } else {
        // Query persistent store
        let store = crate::graph::store_manager::get_persistent_store()?;
        store.create_graph()?
    };
    // ...
}
```

### Step 4: Update Template Rendering (Optional)
**File**: `crates/ggen-domain/src/template/render_with_rdf.rs:170-174`

```rust
// OPTION 1: Keep in-memory for templates (current behavior)
let mut graph = Graph::new()?;

// OPTION 2: Add flag to use persistent store
let mut graph = if options.use_persistent_store {
    let store = crate::graph::store_manager::get_persistent_store()?;
    store.create_graph()?
} else {
    Graph::new()?
};
```

---

## 🧪 Testing Strategy

### Test 1: Load Persists
```bash
$ ggen graph load --file examples/electric-schema/electric-api.ttl
{"triples_loaded":93}

$ ggen graph query --sparql_query "SELECT ?s ?p ?o LIMIT 10"
{"result_count":10}  # ✅ NOW WORKS!
```

### Test 2: Multiple Loads Merge
```bash
$ ggen graph load --file schema1.ttl
{"triples_loaded":50}

$ ggen graph load --file schema2.ttl --merge
{"triples_loaded":30, "total_triples":80}
```

### Test 3: Template Generation from Persistent Store
```bash
$ ggen graph load --file electric-api.ttl

$ ggen template generate-rdf \
    --use-persistent-store \
    --template jsdoc-api.tera \
    --output electric-client.js

# ✅ Generates JSDoc from persistent RDF data
```

---

## 📊 Coverage Matrix

| Component | Current Status | After Fix | Test Status |
|-----------|----------------|-----------|-------------|
| **GraphStore::open()** | ✅ Working | N/A | ✅ test_store_persistent_storage |
| **GraphStore::create_graph()** | ✅ Working | N/A | ✅ test_store_multiple_graphs_share_data |
| **graph load** | ❌ In-memory only | ✅ Persistent | ⚠️ Needs new test |
| **graph query** | ❌ In-memory only | ✅ Persistent | ⚠️ Needs new test |
| **template render** | ✅ Works (in-memory) | ✅ Optional persistent | ✅ test_render_with_inline_rdf |
| **End-to-end workflow** | ✅ Works (files) | ✅ Works (persistent) | ✅ ontology_driven_e2e |

---

## 🚀 Expected Workflow (After Fix)

### Scenario 1: Incremental Development
```bash
# Day 1: Load base ontology
$ ggen graph load --file domain-model.ttl
{"triples_loaded":200}

# Day 2: Add more concepts
$ ggen graph load --file extensions.ttl --merge
{"triples_loaded":50, "total_triples":250}

# Day 3: Generate code
$ ggen template generate-rdf \
    --template rust-models.tera \
    --output src/models.rs
# ✅ Uses all 250 triples from persistent store
```

### Scenario 2: Query-Driven Development
```bash
# Load schema once
$ ggen graph load --file electric-api.ttl

# Iteratively refine queries
$ ggen graph query --sparql_query "SELECT ?class WHERE { ?class a owl:Class }"
$ ggen graph query --sparql_query "SELECT ?prop WHERE { ?prop a owl:ObjectProperty }"
$ ggen graph query --sparql_query "SELECT ?method WHERE { ?method a ex:Method }"

# Generate when ready
$ ggen template generate-rdf --template jsdoc-api.tera --output api.js
```

### Scenario 3: Electric Schema → JSDoc (Original Use Case)
```bash
# 1. Load ElectricSQL schema
$ ggen graph load --file examples/electric-schema/electric-api.ttl
{"triples_loaded":93}

# 2. Verify data (optional)
$ ggen graph query --sparql_query "SELECT ?class ?label WHERE { ?class a owl:Class . ?class rdfs:label ?label }"
{"result_count":4}  # ElectricClient, Database, SyncEngine, Shape

# 3. Generate JSDoc
$ ggen template generate-rdf \
    --template examples/electric-schema/jsdoc-api.tera \
    --output generated/electric-client.js

# 4. Output: Beautiful JSDoc from RDF!
```

---

## 🎯 Benefits of Persistent Storage

1. **Incremental Development**: Load schemas once, query many times
2. **Multi-File Projects**: Load multiple ontologies, query across all
3. **Interactive Exploration**: Build complex queries iteratively
4. **Template Reuse**: Generate multiple outputs from same ontology
5. **Ontology Evolution**: Modify schemas, regenerate code automatically
6. **Knowledge Accumulation**: Store domain knowledge across sessions

---

## 🔗 Related Files

- `crates/ggen-core/src/graph/core.rs` - Graph API
- `crates/ggen-core/src/graph/store.rs` - Persistent storage ✅
- `crates/ggen-domain/src/graph/load.rs` - Load command (needs fix)
- `crates/ggen-domain/src/graph/query.rs` - Query command (needs fix)
- `crates/ggen-domain/src/template/render_with_rdf.rs` - Template rendering ✅
- `tests/chicago_tdd/ontology_driven_e2e.rs` - End-to-end test ✅
- `examples/rdf_template_integration.rs` - RDF + Template example ✅

---

## ⚠️ Critical Notes

1. **GraphStore ALREADY EXISTS and WORKS** - Tests prove it
2. **Template rendering with RDF ALREADY EXISTS** - Just uses in-memory graphs
3. **End-to-end tests ALREADY PASS** - Workflow is proven
4. **Only missing**: Wire persistent store into CLI commands

This is NOT a major refactor - it's a **10-line fix per command**!

---

## 🎉 Validation Report Update

**Before Fix**:
```
| RDF Store Persistence  | ⚠️ Needs work | Each command uses separate in-memory store |
| Template Generation    | ❌ Missing    | generate command not implemented |
```

**After Fix**:
```
| RDF Store Persistence  | ✅ Working    | All commands use shared persistent store |
| Template Generation    | ✅ Working    | template generate-rdf integrates RDF + Tera |
```

---

**Status**: Ready to implement
**Estimated Effort**: 2-3 hours
**Risk**: Low (proven patterns exist, just need wiring)
**Impact**: HIGH - Unblocks entire ontology-driven development thesis
