# YAWL v6 Architecture Diagrams

**Version:** 6.0.0
**Date:** 2026-03-26

Comprehensive visual representations of YAWL system architecture.

---

## 1. Five-Stage Pipeline (μ-Calculus)

```
┌─────────────────────────────────────────────────────────┐
│ RDF Ontology (Input)                                    │
│ - Industry standards (FIBO, HL7, ISO)                  │
│ - Domain-specific classes and relationships             │
└────────────────────┬────────────────────────────────────┘
                     │
                     ↓
┌─────────────────────────────────────────────────────────┐
│ μ₁: NORMALIZE (Load & Validate)                         │
├─────────────────────────────────────────────────────────┤
│ Input:  RDF document (Turtle, RDF/XML, JSON-LD)       │
│ Ops:    Parse RDF → oxigraph::Store                    │
│         Register SPARQL PREFIXes                        │
│         Validate SHACL shapes                           │
│ Output: Validated oxigraph::Store                       │
│ Time:   ~500ms                                          │
└────────────────────┬────────────────────────────────────┘
                     │
                     ↓
┌─────────────────────────────────────────────────────────┐
│ μ₂a: EXTRACT - INFERENCE (SPARQL CONSTRUCT)           │
├─────────────────────────────────────────────────────────┤
│ Input:  Validated ontology graph                        │
│ For each InferenceRule (sorted by .order):             │
│   └─ Check .when guard (if present)                    │
│   └─ Execute SPARQL CONSTRUCT query                    │
│   └─ Merge results into code_graph                     │
│   └─ Record metadata (rule name, triples added, time)  │
│ Output: Enriched code_graph                            │
│ Time:   ~1000ms (typical)                              │
└────────────────────┬────────────────────────────────────┘
                     │
                     ↓
┌─────────────────────────────────────────────────────────┐
│ μ₂b: EXTRACT - GENERATION (SPARQL SELECT)              │
├─────────────────────────────────────────────────────────┤
│ Input:  Enriched code_graph                            │
│ For each GenerationRule (manifest order):              │
│   ├─ Check .when guard (if present)                    │
│   ├─ Execute SPARQL SELECT query → bindings           │
│   ├─ Skip if .skip_empty and no results               │
│   └─ Pass bindings to template rendering              │
│ Output: Vec<HashMap<String, Value>>                    │
│ Time:   ~500ms (typical)                               │
└────────────────────┬────────────────────────────────────┘
                     │
                     ↓
┌─────────────────────────────────────────────────────────┐
│ μ₃: EMIT (Tera Template Rendering)                     │
├─────────────────────────────────────────────────────────┤
│ Input:  SPARQL bindings + Tera templates               │
│ For each result row:                                    │
│   ├─ Render output_file path (templated)              │
│   ├─ Render template body with row variables          │
│   ├─ Respect .mode (Create/Overwrite/Append)          │
│   └─ Track execution metadata                          │
│ Output: Generated source code                          │
│ Time:   ~5000ms (typical)                              │
└────────────────────┬────────────────────────────────────┘
                     │
                     ↓
┌─────────────────────────────────────────────────────────┐
│ μ₄: CANONICALIZE (Deterministic Formatting)           │
├─────────────────────────────────────────────────────────┤
│ Input:  Generated source code                          │
│ Ops:    Normalize whitespace                           │
│         Sort collections (imports, properties)         │
│         Compute SHA256 hash                            │
│ Output: Canonical code + content_hash                  │
│ Time:   ~100ms                                          │
└────────────────────┬────────────────────────────────────┘
                     │
                     ↓
┌─────────────────────────────────────────────────────────┐
│ μ₅: RECEIPT (Cryptographic Proof)                      │
├─────────────────────────────────────────────────────────┤
│ Input:  Canonical code + execution metadata            │
│ Ops:    Create audit.json with:                        │
│         - ExecutedRule entries (what ran)              │
│         - GeneratedFile entries (hashes)               │
│         - Timestamps and versions                      │
│ Output: audit.json (cryptographic proof)               │
│ Time:   ~50ms                                           │
└────────────────────┬────────────────────────────────────┘
                     │
                     ↓
        ┌────────────────────────────┐
        │ Generated Code (Artifact)   │
        │ + audit.json (Proof)        │
        └────────────────────────────┘
```

---

## 2. Component Architecture

```
┌─────────────────────────────────────────────────────────┐
│                   ggen CLI (User Interface)             │
│  $ ggen sync --manifest ggen.toml                       │
└────────────────────┬────────────────────────────────────┘
                     │
        ┌────────────┴────────────┐
        │                         │
        ↓                         ↓
┌──────────────────┐      ┌──────────────────┐
│  ggen-yawl       │      │  ggen-core       │ (Generic)
│  (Domain)        │      │  (Framework)     │
├──────────────────┤      ├──────────────────┤
│ ├─ ontology/     │      │ ├─ manifest/     │
│ │  └─ loader.rs  │      │ │  └─ types.rs   │
│ │                │      │ │                │
│ ├─ transform/    │      │ ├─ graph/        │
│ │  └─ executor   │      │ │  └─ (oxigraph) │
│ │                │      │ │                │
│ ├─ template/     │      │ ├─ pipeline/     │
│ │  └─ renderer   │      │ │  └─ (Tera)     │
│ │                │      │ │                │
│ └─ codegen/      │      │ └─ codegen/      │
│    └─ yawl_xml   │      │    └─ executor   │
└──────────────────┘      └─────────┬────────┘
        │                           │
        └───────────────┬───────────┘
                        │
        ┌───────────────┴───────────────┐
        │                               │
        ↓                               ↓
┌──────────────────┐            ┌──────────────────┐
│   oxigraph       │            │    Tera          │
│   (RDF/SPARQL)   │            │  (Templates)     │
└──────────────────┘            └──────────────────┘
```

---

## 3. Data Flow Through Pipeline

```
                    ggen.toml
                        │
        ┌───────────────┼───────────────┐
        │               │               │
    Ontology      Inference Rules  Generation Rules
    fibo.ttl      SPARQL CONSTRUCT  SPARQL SELECT
        │               │               │
        │               ↓               │
        │        ┌─────────────────┐   │
        │        │ code_graph      │   │
        │        │ (enriched RDF)  │←──┘
        │        └────────┬────────┘
        │                 │
        ↓                 ↓
    ┌────────────────────────────┐
    │  Query Bindings            │
    │  { taskName: "Verify",     │
    │    taskType: "HL7:Task",   │
    │    ... }                   │
    └────────┬───────────────────┘
             │
             ↓
    ┌────────────────────────────┐
    │  Tera Template             │
    │  pub struct                │
    │    {{ taskName }} { ... }  │
    └────────┬───────────────────┘
             │
             ↓
    ┌────────────────────────────┐
    │  Generated Code            │
    │  pub struct                │
    │    Verify { ... }          │
    └────────┬───────────────────┘
             │
             ↓
    ┌────────────────────────────┐
    │  Canonical Code            │
    │  (normalized format)       │
    │  hash: SHA256              │
    └────────┬───────────────────┘
             │
             ↓
    ┌────────────────────────────┐
    │  audit.json                │
    │  - generated_files         │
    │  - executed_rules          │
    │  - hashes + timestamps     │
    └────────────────────────────┘
```

---

## 4. Trait Relationships

```
┌─────────────────────────────────────────┐
│  YawlGenerator (ggen-yawl)              │
├─────────────────────────────────────────┤
│ ├─ executor: ConstructExecutor          │
│ ├─ renderer: TemplateRenderer           │
│ └─ validate_output: bool                │
│                                          │
│ Methods:                                │
│ ├─ new() -> YawlGenerator               │
│ ├─ with_validation(bool) -> Self        │
│ └─ generate(&str) -> Result<String>     │
└────────────────┬────────────────────────┘
                 │ uses
        ┌────────┴────────┬────────────────┐
        │                 │                │
        ↓                 ↓                ↓
   ┌─────────┐     ┌──────────┐     ┌─────────┐
   │Construct│     │ Tera     │     │Oxigraph │
   │Executor │     │Renderer  │     │ Store   │
   └──┬──────┘     └────┬─────┘     └────┬────┘
      │ executes        │ renders         │ queries
      │                 │                │
      ↓                 ↓                ↓
   ┌──────────────────────────────────────────┐
   │ SPARQL CONSTRUCT                         │
   │ → Inference Rules                        │
   │ → New Triples                            │
   └──────────────────────────────────────────┘
```

---

## 5. Rule Execution Flow

```
START: ggen sync --manifest ggen.toml
   │
   ↓
┌────────────────────────────────────┐
│ Parse Manifest (TOML)              │
│ ├─ Project metadata                │
│ ├─ Ontology config                 │
│ ├─ Inference rules                 │
│ └─ Generation rules                │
└────────┬───────────────────────────┘
         │
         ↓
┌────────────────────────────────────┐
│ Load Ontology                      │
│ └─ oxigraph::Store::from_turtle()  │
└────────┬───────────────────────────┘
         │
         ↓
┌────────────────────────────────────┐
│ FOR each InferenceRule (by order)  │
├────────────────────────────────────┤
│ ├─ if .when condition: skip if     │
│ │       ASK returns false          │
│ │                                  │
│ ├─ Execute CONSTRUCT query         │
│ │ └─ Merge new triples into graph  │
│ │                                  │
│ └─ Record ExecutedRule metadata    │
└────────┬───────────────────────────┘
         │
         ↓
┌────────────────────────────────────┐
│ FOR each GenerationRule (order)    │
├────────────────────────────────────┤
│ ├─ if .when condition: skip if     │
│ │       ASK returns false          │
│ │                                  │
│ ├─ Execute SELECT query            │
│ │ └─ Get bindings for each row     │
│ │                                  │
│ ├─ if .skip_empty && empty:        │
│ │   continue to next rule          │
│ │                                  │
│ ├─ FOR each result row:            │
│ │   ├─ Render output_file path     │
│ │   ├─ Render template with binds  │
│ │   ├─ Canonicalize output         │
│ │   ├─ Compute content_hash        │
│ │   ├─ Write file (.mode handling) │
│ │   └─ Record GeneratedFile        │
│ │                                  │
│ └─ Next rule                       │
└────────┬───────────────────────────┘
         │
         ↓
┌────────────────────────────────────┐
│ Generate audit.json                │
│ ├─ List ExecutedRules              │
│ ├─ List GeneratedFiles             │
│ ├─ Timestamps + versions           │
│ └─ Verification hashes             │
└────────┬───────────────────────────┘
         │
         ↓
    ✓ SUCCESS
    └─ Generated files written
       audit.json written
       Summary printed to stdout
```

---

## 6. Determinism Verification

```
┌─────────────────────────────────────────┐
│ Input Inputs (Determinism Sources)      │
├─────────────────────────────────────────┤
│ ├─ ggen.toml (manifest)                 │
│ │  └─ hash: H₁                          │
│ ├─ fibo.ttl (ontology)                  │
│ │  └─ hash: H₂                          │
│ ├─ *.sparql (queries)                   │
│ │  └─ hashes: H₃...Hₙ                   │
│ ├─ *.tera (templates)                   │
│ │  └─ hashes: Hₙ₊₁...Hₘ                 │
│ └─ ggen binary version                  │
│    └─ hash: Hᵥ                          │
└────────────┬────────────────────────────┘
             │
             ↓ (combined as seed)
       ┌──────────────┐
       │  μ₁-μ₅       │
       │  Pipeline    │
       │  Execution   │
       └──────────────┘
             │
             ↓
┌─────────────────────────────────────────┐
│ Outputs (Deterministic Results)         │
├─────────────────────────────────────────┤
│ ├─ Generated file 1                     │
│ │  └─ content_hash: H'₁                 │
│ ├─ Generated file 2                     │
│ │  └─ content_hash: H'₂                 │
│ └─ Generated file N                     │
│    └─ content_hash: H'ₙ                 │
└────────────┬────────────────────────────┘
             │
             ↓
    ┌────────────────────┐
    │  audit.json        │
    │  ├─ hashes         │
    │  ├─ timestamps     │
    │  └─ rule provenance│
    └────────┬───────────┘
             │
             ↓
    ggen verify audit.json
             │
             ↓
    Compare: H'₁, H'₂, ... H'ₙ
    with stored values in audit.json
             │
             ├─ All match? → ✓ DETERMINISTIC
             └─ Mismatch?  → ✗ NON-DETERMINISTIC BUG
```

---

## 7. Error Handling Flow

```
START: ggen sync
   │
   ├─→ Parse manifest
   │   ├─ TOML syntax error → ERROR: line X
   │   └─ Missing field → ERROR: field "X" required
   │
   ├─→ Load ontology
   │   ├─ File not found → ERROR: "fibo.ttl" not found
   │   ├─ Invalid RDF → ERROR: RDF parse error at line X
   │   └─ SHACL validation fails → ERROR: shape X violated
   │
   ├─→ Execute inference rules
   │   ├─ SPARQL syntax error → ERROR: query X syntax
   │   ├─ Query timeout → ERROR: query timeout (>5s)
   │   └─ Guard condition fails → SKIP (if .when false)
   │
   ├─→ Execute generation rules
   │   ├─ SPARQL syntax error → ERROR: query X syntax
   │   ├─ Template syntax error → ERROR: template line X
   │   ├─ File write error → ERROR: permission denied
   │   └─ Protected file → ERROR: use --force to overwrite
   │
   ├─→ Validate output
   │   ├─ Invalid generated code → ERROR: validation failed
   │   └─ Audit trail creation → ERROR: JSON write failed
   │
   └─→ Generate audit.json
       └─ Hash verification fails → WARN: non-deterministic output

END: Success or Error (with context + suggestions)
```

---

## 8. Caching Strategy (Incremental Builds)

```
First Build:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
manifest: ggen.toml
ontology: fibo.ttl
  │
  ├─→ Hash(manifest) = H_manifest
  ├─→ Hash(ontology) = H_ontology
  │
  ├─→ μ₁ Load ontology        [~500ms]
  ├─→ μ₂a Inference rules     [~1000ms] ← Cache hash
  ├─→ μ₂b Generation rules    [~500ms]
  ├─→ μ₃ Template rendering   [~5000ms]
  ├─→ μ₄ Canonicalization     [~100ms]
  └─→ μ₅ Receipt generation   [~50ms]
        Total: ~7150ms
        Cache: .ggen/inference_graph_hash = H_infer


Incremental Build (ontology unchanged):
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
manifest: ggen.toml (updated)
ontology: fibo.ttl (unchanged)
  │
  ├─→ Hash(manifest) = H_manifest [CHANGED]
  ├─→ Hash(ontology) = H_ontology [SAME]
  │
  ├─→ Check cache: H_infer matches?
  │   └─ YES! Skip μ₂a (inference)
  │
  ├─→ μ₂b Generation rules    [~500ms]
  ├─→ μ₃ Template rendering   [~5000ms]
  ├─→ μ₄ Canonicalization     [~100ms]
  └─→ μ₅ Receipt generation   [~50ms]
        Total: ~5650ms (25% faster)


Incremental Build (all unchanged):
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  ├─→ Hash(manifest) = H_manifest [SAME]
  ├─→ Hash(ontology) = H_ontology [SAME]
  │
  ├─→ Compare with audit.json
  │   └─ All hashes match?
  │       └─ YES! Skip full generation
  │
  └─→ Output: audit.json (unchanged)
        Total: ~50ms (100x faster)
```

---

## 9. Separation of Concerns

```
Developer                    Ontologist                    Operations
    │                            │                              │
    ├─ Write Rust code       ├─ Design RDF ontology       ├─ Deploy CLI
    ├─ Impl SPARQL filters   ├─ Define domain concepts    ├─ Run ggen sync
    ├─ Compose rules         ├─ Create SPARQL patterns    └─ Monitor output
    └─ Test in unit tests    └─ Validate with SHACL
         │                         │                         │
         ↓                         ↓                         ↓
    ┌──────────────────────────────────────────────────────────┐
    │           ggen Manifest (ggen.toml)                      │
    │  ┌────────────┬────────────┬────────────────────────┐   │
    │  │ Ontology   │ Inference  │ Generation Rules       │   │
    │  │ source:    │ rules:     │ rules:                 │   │
    │  │ fibo.ttl   │ [...]      │ [{name: "gen_*.rs",   │   │
    │  │ base_iri:  │            │   query: "...",        │   │
    │  │ ...        │            │   template: "...",     │   │
    │  │            │            │   output_file: "..."}] │   │
    │  └────────────┴────────────┴────────────────────────┘   │
    └────────────────────┬───────────────────────────────────────┘
                         │
                         ↓ (source of truth)
                    git commit
                         │
                         ↓
    ┌──────────────────────────────────────────────────────────┐
    │         CI/CD Pipeline (Automated)                       │
    │  1. ggen validate ggen.toml                             │
    │  2. ggen sync --manifest ggen.toml --audit true        │
    │  3. Verify audit.json hashes                            │
    │  4. Commit generated code (if changed)                  │
    │  5. Run tests                                            │
    │  6. Deploy                                               │
    └──────────────────────────────────────────────────────────┘
```

---

## 10. Extension Points

```
Core Framework (ggen-core)
    ├─→ Graph (Oxigraph wrapper)
    │   └─ Future: Plugin for different RDF stores
    │
    ├─→ Pipeline (Tera wrapper)
    │   └─ Future: Plugin system for template engines
    │
    └─→ Codegen (Rule executor)
        └─ Future: Custom validation plugins


Domain Specializations
    ├─ ggen-yawl (YAWL workflows)
    ├─ ggen-craftplan (Elixir)
    ├─ ggen-terraform (Infrastructure)
    ├─ ggen-kubernetes (K8s manifests)
    └─ ggen-api (REST APIs)


Custom Extensions
    ├─ SPARQL filters (Rust closures)
    ├─ Tera filters (custom rendering)
    ├─ Validation rules (SHACL + custom)
    └─ Manifest plugins (external rules)
```

---

## 11. Performance Model

```
Build Time Breakdown (Typical Project):

   Ontology size    Time    Component
   ─────────────────────────────────────
   1KB              500ms   μ₁ Load RDF
   10KB             500ms   (oxigraph parsing)

   Inference rules: 5 rules, avg ~100KB code_graph
   Total time:     1000ms   μ₂a Inference
                            (SPARQL CONSTRUCT execution)

   Generation rules: 10 rules, 100 queries total
   Results: 500 rows total
   Total time:      500ms   μ₂b Query (SELECT)

   Template rendering: 500 result rows
   Avg template size: 100 lines
   Avg render time: ~10ms per row
   Total time:     5000ms   μ₃ Emit

   Canonicalization: 50KB total code
   Total time:       100ms   μ₄ Canonicalize

   Receipt generation: audit.json creation
   Total time:        50ms    μ₅ Receipt

   ─────────────────────────────────────
   TOTAL:            6650ms   First build


Incremental Build (ontology unchanged):
   Skip μ₂a inference (cached)
   Total:            5650ms   (25% faster)


Incremental Build (all unchanged):
   Compare hashes
   Total:            50ms     (100x faster)
```

---

## 12. Determinism Guarantee Chain

```
Input                    Determinism Point            Output
─────────────────────────────────────────────────────────────────
manifest.toml       →   Parse + validate     →   Manifest
                        (stable TOML parser)

ontology.ttl        →   Load with oxigraph   →   Validated
                        (deterministic store)       Graph

Inference Rules     →   Execute by order     →   Enriched
(ordered by .order)     (explicit sort)           Graph

SPARQL patterns     →   Query + canonicalize →   Bindings
                        (deterministic engine)    (sorted)

Templates + Context →   Render via Tera      →   Generated
                        (compiled templates)      Code

Generated Code      →   Canonicalize         →   Canonical
                        (sort + normalize)       Code

Canonical Code      →   SHA256()             →   Hash
                        (cryptographic)          (proof)

Execution Metadata  →   JSON serialization   →   audit.json
                        (deterministic)         (verifiable)
```

---

## 13. Domain Integration Pattern

```
Step 1: Design Domain Ontology
    Input: Industry requirements
    Output: domain.ttl (RDF specification)


Step 2: Create SPARQL Patterns
    Input: domain.ttl
    Output: .sparql files (CONSTRUCT + SELECT queries)
    ├─ inference_*.sparql (CONSTRUCT for enrichment)
    └─ extract_*.sparql (SELECT for facts)


Step 3: Write Tera Templates
    Input: SPARQL result structure
    Output: .tera files
    ├─ class_template.tera
    ├─ function_template.tera
    └─ config_template.tera


Step 4: Compose ggen.toml
    Input: ontology, queries, templates
    Output: ggen.toml (manifest)
    ├─ [ontology]
    ├─ [[inference.rules]]
    └─ [[generation.rules]]


Step 5: Generate Code
    Input: ggen.toml + all sources
    Process: μ₁-μ₅ pipeline
    Output: generated code + audit.json


Step 6: Version & Distribute
    Input: generated code + audit.json
    Process: Reproducibility verification
    Output: Deployable artifacts
```

---

## Related Documentation

- **[YAWL_ARCHITECTURE.md](./YAWL_ARCHITECTURE.md)** - Detailed architecture description
- **[YAWL_DESIGN_DECISIONS.md](./YAWL_DESIGN_DECISIONS.md)** - Why these patterns
- **[YAWL_RATIONALE.md](./YAWL_RATIONALE.md)** - Business and technical goals
