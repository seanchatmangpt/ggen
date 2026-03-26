# YAWL v6 Code Generation Architecture

**Version:** 6.0.0
**Status:** Production
**Updated:** 2026-03-26

## Overview

YAWL v6 implements specification-driven code generation through a five-stage deterministic pipeline (μ-calculus). Code precipitates from RDF ontologies via SPARQL transformations and template rendering, guaranteeing reproducible builds.

```
RDF Ontology
    ↓ μ₁ (Normalize)
Validated Graph
    ↓ μ₂ (Extract)
SPARQL Results
    ↓ μ₃ (Emit)
Generated Code
    ↓ μ₄ (Canonicalize)
Deterministic Output
    ↓ μ₅ (Receipt)
Cryptographic Proof
```

---

## The μ-Calculus Pipeline

### Stage μ₁: Normalize (RDF Validation)

**Purpose:** Load and validate ontology, establish transformation context

**Input:** RDF document (Turtle, RDF/XML, JSON-LD)

**Process:**
1. Parse RDF using oxigraph library
2. Register SPARQL PREFIX declarations from manifest
3. Validate SHACL shapes (if provided)
4. Build in-memory RDF store (oxigraph::MemoryStore)

**Output:** Validated `oxigraph::Store` ready for querying

**Determinism:** Prefix ordering from BTreeMap (alphabetical)

### Stage μ₂: Extract (SPARQL Reasoning & Selection)

**Purpose:** Enrich ontology with inferred knowledge, extract facts for code generation

**Divided into two substages:**

#### μ₂a: Inference Rules (SPARQL CONSTRUCT)

```toml
[[inference.rules]]
name = "infer_workflow_tasks"
construct = """
  PREFIX wf: <http://workflow.org/>
  CONSTRUCT { ?task wf:hasSequence ?next . }
  WHERE { ?task wf:precedes ?next . }
"""
order = 1
when = "ASK { ?workflow a wf:Workflow . }"  # Optional guard
```

**Process:**
- Iterate inference rules in `order` (ascending)
- Check `when` condition (SPARQL ASK) if present
- Execute CONSTRUCT query → new triples added to store
- Record metadata: rule name, query hash, execution time, triples added

**Result:** Enriched RDF graph (code_graph)

#### μ₂b: Generation Rules (SPARQL SELECT)

```toml
[[generation.rules]]
name = "generate_task_handler"
query = { file = "queries/extract_tasks.sparql" }  # or inline
template = { file = "templates/task_handler.tera" }  # or inline
output_file = "src/handlers/{{ task_name }}.rs"
skip_empty = true
when = "ASK { ?workflow a hl7:ClinicalWorkflow . }"
```

**Process:**
- For each generation rule (in manifest order)
- Check `when` condition if present
- Execute SELECT query → bindings for each result row
- Skip if `skip_empty=true` and no results
- Render output filename from `output_file` template
- Render code from `template` with bindings as context

**Result:** Generated source files + metadata

### Stage μ₃: Emit (Template Rendering)

**Engine:** Tera (Jinja2-compatible)

**Context Variables:** Bindings from SPARQL SELECT query

**Example:**

Input query (SPARQL):
```sparql
SELECT ?taskName ?taskType WHERE {
  ?task rdfs:label ?taskName ;
        rdf:type ?taskType .
}
```

Result bindings:
```json
[
  { "taskName": "VerifyIdentity", "taskType": "hl7:Task" },
  { "taskName": "ProcessPayment", "taskType": "hl7:Task" }
]
```

Template (Tera):
```jinja2
pub struct {{ taskName | to_pascal_case }} {
    task_type: String,  // {{ taskType }}
}
```

Output:
```rust
pub struct VerifyIdentity {
    task_type: String,  // hl7:Task
}

pub struct ProcessPayment {
    task_type: String,  // hl7:Task
}
```

**Features:**
- For-loops over result rows
- Conditionals based on result fields
- Custom Rust filters (to_pascal_case, to_snake_case, etc.)
- SPARQL filter access via `sparql()` function

### Stage μ₄: Canonicalize (Deterministic Formatting)

**Purpose:** Normalize output to guarantee reproducibility

**Process:**
1. Normalize whitespace (consistent indentation, line endings)
2. Sort collections (imports, properties, constants)
3. Compute SHA256 hash of canonical form
4. Record file metadata: path, hash, size, source rule

**Output:** Deterministically formatted code + content hash

### Stage μ₅: Receipt (Cryptographic Proof)

**Purpose:** Create verifiable audit trail for reproducibility

**Generates:** `audit.json` manifest containing:

```json
{
  "project": {
    "name": "ggen",
    "version": "6.0.0"
  },
  "generated_at": "2026-03-26T12:34:56Z",
  "executed_rules": [
    {
      "name": "infer_workflow_tasks",
      "rule_type": "Inference",
      "triples_added": 42,
      "query_hash": "sha256:abc123...",
      "duration_ms": 125
    }
  ],
  "generated_files": [
    {
      "path": "src/handlers/verify_identity.rs",
      "content_hash": "sha256:def456...",
      "size_bytes": 1024,
      "source_rule": "generate_task_handler"
    }
  ],
  "validation_results": [...]
}
```

**Receipt Verification:**
```bash
ggen verify audit.json  # Recompute hashes, compare with stored values
```

---

## Rule Execution Engine

### Data Structures

```rust
// Configuration (from ggen.toml)
pub struct GenerationRule {
    pub name: String,              // "generate_task_handler"
    pub query: QuerySource,        // SPARQL SELECT location
    pub template: TemplateSource,  // Tera template location
    pub output_file: String,       // "src/handlers/{{ task_name }}.rs"
    pub skip_empty: bool,          // Skip if query returns no results
    pub mode: GenerationMode,      // Create/Overwrite/Append
    pub when: Option<String>,      // Optional guard condition
}

// Execution state
pub struct GeneratedFile {
    pub path: PathBuf,
    pub content_hash: String,
    pub size_bytes: usize,
    pub source_rule: String,
}

pub struct ExecutedRule {
    pub name: String,
    pub rule_type: RuleType,       // Inference or Generation
    pub triples_added: usize,
    pub duration_ms: u64,
    pub query_hash: String,
}
```

### Execution Algorithm

```rust
pub async fn execute_pipeline(manifest: &GgenManifest) -> Result<AuditTrail> {
    // μ₁: Normalize
    let ontology_graph = load_and_validate(&manifest.ontology)?;
    let mut code_graph = ontology_graph.clone();

    // μ₂a: Execute inference rules (in order)
    for rule in sorted_by(&manifest.inference.rules, |r| r.order) {
        if let Some(when) = &rule.when {
            if !ask_query(&code_graph, when)? { continue; }
        }
        let new_triples = construct_query(&code_graph, &rule.construct)?;
        code_graph.merge(new_triples)?;
        audit.record_executed_rule(rule.name, "Inference", ...)?;
    }

    // μ₂b/μ₃: Execute generation rules
    for rule in &manifest.generation.rules {
        if let Some(when) = &rule.when {
            if !ask_query(&code_graph, when)? { continue; }
        }

        let bindings = select_query(&code_graph, &rule.query)?;
        if rule.skip_empty && bindings.is_empty() { continue; }

        for row in bindings {
            let output_path = render_template(&rule.output_file, &row)?;
            let code = render_template(&rule.template, &row)?;

            // μ₄: Canonicalize
            let canonical = canonicalize(&code)?;
            let hash = sha256(&canonical);

            // Write file (respecting mode)
            match rule.mode {
                GenerationMode::Create => {
                    if !output_path.exists() {
                        write_file(&output_path, &canonical)?;
                        audit.record_generated_file(&output_path, hash, ...)?;
                    }
                }
                GenerationMode::Overwrite => {
                    write_file(&output_path, &canonical)?;
                    audit.record_generated_file(&output_path, hash, ...)?;
                }
                GenerationMode::Append => {
                    append_file(&output_path, &canonical)?;
                    audit.record_generated_file(&output_path, hash, ...)?;
                }
            }
        }
    }

    // μ₅: Receipt
    audit.write_to_file("audit.json")?;
    Ok(audit)
}
```

---

## Separation of Concerns

### ggen-core

**Purpose:** Implement μ-calculus pipeline generically

**Modules:**
- `graph` - RDF operations (oxigraph wrapper)
- `pipeline` - Tera template rendering + SPARQL integration
- `codegen::pipeline` - Rule execution orchestration
- `manifest` - TOML manifest parsing + validation

**Key Responsibility:** Abstract pipeline implementation, reusable across domains

### ggen-yawl

**Purpose:** Apply pipeline to YAWL workflow generation

**Modules:**
- `ontology::loader` - Industry ontology loaders (FIBO, HL7)
- `transform::executor` - SPARQL CONSTRUCT patterns for workflow inference
- `template::renderer` - Tera templates for YAWL XML + Erlang
- `codegen::yawl_xml` - YAWL-specific validation + canonicalization

**Key Responsibility:** Domain-specific patterns and templates

### Dependency Flow

```
┌─────────────────────────┐
│  ggen-yawl              │  Domain-specific
│  (YAWL generation)      │
└────────┬────────────────┘
         │
         └─► ggen-core    ◄─ Generic framework
             ├─ Graph
             ├─ Pipeline
             └─ codegen
                 │
                 └─► oxigraph (SPARQL)
                 └─► tera (templates)
```

---

## Determinism Guarantees

### Reproducible Output

**Goal:** Same RDF + Rules = Identical output (bit-for-bit)

**Mechanisms:**

1. **Deterministic Rule Ordering**
   - Inference: Explicit `order` field (ascending sort)
   - Generation: Manifest declaration order
   - Within results: SPARQL query result ordering

2. **Sorted Collections**
   - Manifest prefixes: BTreeMap (alphabetical)
   - Generated files: Sorted by path before hashing
   - Query results: Deterministic SPARQL engine (oxigraph)

3. **Content Hashing**
   - SHA256 of canonical code
   - Stored in audit.json for verification
   - Allows "diff" between versions

4. **Determinism Salt** (optional)
   ```toml
   [generation]
   determinism_salt = "v6.0.0-2026-03"  # Version-specific generation
   ```

### Verification Command

```bash
ggen verify audit.json                    # Recompute hashes
ggen compare audit1.json audit2.json      # Diff between runs
```

---

## Error Handling

### Error Categories

| Category | Source | Handling |
|----------|--------|----------|
| Ontology loading | File I/O, parsing | Halt with context |
| SPARQL errors | Query syntax, timeout | Log + audit trail |
| Template errors | Tera rendering | Report line + snippet |
| Validation failures | SHACL, manifest checks | Halt or warn |
| File I/O errors | Permissions, disk full | Rollback generated files |

### Error Propagation

1. **I/O Errors:** Wrapped in `anyhow::Error` for CLI
2. **Validation Errors:** Stop pipeline, detailed message
3. **SPARQL Timeout:** Log query + increase timeout
4. **Template Syntax:** Report line number + context

### Dry-run Mode

```bash
ggen sync --manifest ggen.toml --dry-run
```

Detects errors without modifying filesystem.

---

## Performance Characteristics

### Build SLOs

| Operation | Target | Notes |
|-----------|--------|-------|
| First build | ≤15s | Full μ₁-μ₅ pipeline |
| Incremental build | ≤2s | Skip unchanged inference rules |
| SPARQL query | ≤50ms | Indexed store, small ontologies |
| Template render | ≤100ms | Per-file, Tera pre-compiled |
| Large SPARQL | ≤5s | 1000+ triple ontologies |

### Memory Usage

| Component | Typical |
|-----------|---------|
| RDF Store | 10-100MB |
| Template engine | 1-5MB |
| Result bindings | <1MB |
| Generated code | Varies |

### Caching Strategies

1. **Tera template caching:** Implicit (compiled templates cached)
2. **Incremental builds:** Cache inference graph hashes
3. **Query result caching:** Optional (disabled by default)
4. **Proof archive:** Store old audit.json files

---

## Known Limitations

| Limitation | Workaround |
|-----------|-----------|
| No SPARQL FEDERATION | Pre-merge ontologies |
| No recursive Tera macros | Use iteration instead |
| Large ontologies (>1M triples) | Partition + multiple pipelines |
| No real-time SPARQL updates | Reload ontology per generation |
| Sequential rule execution | Split rules into separate invocations |

---

## Extension Points

### Adding Domain Support

1. Create `ggen-<domain>` crate
2. Implement ontology loaders
3. Define SPARQL patterns (inference rules)
4. Create Tera templates (generation rules)
5. Compose via manifest

### Adding Template Engines

1. Implement renderer trait
2. Register SPARQL filters
3. Extend TemplateSource enum

### Custom Validation

```toml
[validation]
sparql_constraints = ["constraints/workflow_rules.sparql"]
shacl_shapes = ["ontology/shapes.ttl"]
```

---

## Version History

| Version | Date | Highlights |
|---------|------|-----------|
| 6.0.0 | 2026-03-26 | μ-calculus pipeline, manifest-driven rules |
| 5.0.0 | 2025-12-01 | Template-first, SPARQL integration |
