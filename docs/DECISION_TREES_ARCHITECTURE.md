# Decision Trees & Architecture Reference

> Navigate ggen's complexity with decision trees. Find the right pattern in 3 questions.

---

## Decision Tree 1: "What should I do?"

```
START
  │
  ├─ Need to generate code?
  │  │
  │  ├─ From scratch?
  │  │  ├─ YES → Define ontology (RDF/Turtle)
  │  │  │        → Write templates (Tera)
  │  │  │        → Configure ggen.toml
  │  │  │        → Run: ggen sync
  │  │  │
  │  │  └─ Have existing ontology?
  │  │     ├─ YES → Skip to: ggen sync
  │  │     └─ NO  → See "I have code, want ontology?"
  │  │
  │  ├─ Generating REST API?
  │  │  → Use SPARQL: Select all Classes with properties
  │  │  → Template: Render endpoint handlers
  │  │  → Rule: Capability detection (canCreate, canRead, etc.)
  │  │
  │  ├─ Generating data models?
  │  │  → Use SPARQL: Inheritance closure (rdfs:subClassOf+)
  │  │  → Template: Struct generation
  │  │  → Rule: Property expansion to subclasses
  │  │
  │  └─ Generating tests?
  │     → Use SPARQL: Extract user stories from ontology
  │     → Template: Test case boilerplate
  │     → Rule: Scenario enumeration
  │
  ├─ Need to learn/master something?
  │  │
  │  ├─ How to write SPARQL rules? → Read SPARQL_INFERENCE_GUIDE.md
  │  ├─ How to query RDF? → Read GRAPH_QUERYING_API.md
  │  ├─ How to build agents? → Read AI_AGI_INTEGRATION_GUIDE.md
  │  ├─ How to configure? → Read GGEN_TOML_REFERENCE.md
  │  └─ All CLI commands? → Read CLI_REFERENCE.md
  │
  ├─ Need to integrate into CI/CD?
  │  │
  │  ├─ Validate specs only?
  │  │  → ggen sync --validate-only --format json
  │  │
  │  ├─ Generate + run tests?
  │  │  → ggen sync --audit audit.json
  │  │  → cargo test
  │  │
  │  └─ Multi-crate workspace?
  │     → Create [[generation.rules]] per crate
  │     → Order by dependencies
  │     → Set lifecycle.phases.post_generation to test
  │
  └─ Need to build agents/intelligence?
     │
     ├─ Natural language → code?
     │  → Agent: Parse requirement → Create RDF ontology
     │  → Agent: Call ggen sync (Layer 2)
     │  → Agent: Run tests (Layer 3)
     │  → Agent: Refine ontology based on failures (Layer 4)
     │
     ├─ Self-improving code generation?
     │  → Enable feedback loops with audit trails
     │  → Use SPARQL to analyze what was generated
     │  → Update ontology based on test results
     │
     └─ Semantic package discovery?
        → Use SPARQL queries on marketplace RDF
        → Example: Find packages that depend on JWT
```

---

## Decision Tree 2: "Which pattern should I use?"

```
Need to MATERIALIZE facts?
  │
  ├─ Class hierarchies?
  │  → CONSTRUCT: Transitive closure (rdfs:subClassOf+)
  │  → Query: ?sub :allSuperclasses ?super
  │  → Impact: Enables downcast/upcasting in code
  │
  ├─ Properties on subclasses?
  │  → CONSTRUCT: Domain expansion (parent props → children)
  │  → Query: ?prop rdfs:domain ?subclass
  │  → Impact: All entities automatically get parent fields
  │
  ├─ Capabilities/roles?
  │  → CONSTRUCT: Aggregate :canCreate, :canUpdate, :canDelete
  │  → Query: ?class :hasCapability ?cap
  │  → Impact: Auto-generate CRUD interfaces
  │
  ├─ Validation rules?
  │  → CONSTRUCT: Extract SHACL constraints
  │  → Query: ?prop :minLength, :maxLength, :pattern
  │  → Impact: Auto-generate validation code
  │
  ├─ Cross-cutting concerns?
  │  → CONSTRUCT: Mark :isAuditable, :isVersioned, :isSoftDeletable
  │  → Query: Materialized as boolean fields
  │  → Impact: Auto-add audit fields, versioning, soft delete
  │
  └─ Derived facts (compute once, use many)?
     → CONSTRUCT: Any computed property
     → Cache: Store in graph, query in templates
     → Performance: O(1) lookup instead of re-computing
```

---

## Decision Tree 3: "Which flags should I use?"

```
ggen sync [?] ...
      │
      ├─ Development
      │  └─ --watch                 (auto-regenerate on changes)
      │
      ├─ Testing/Validation
      │  ├─ --validate-only         (just check, no generate)
      │  ├─ --format json           (machine-readable)
      │  └─ --audit audit.json      (capture everything)
      │
      ├─ Debugging
      │  ├─ -vvv                    (max verbosity)
      │  ├─ --dry-run               (no file writes)
      │  └─ --timeout 120000        (extend timeout)
      │
      ├─ CI/CD
      │  ├─ --validate-only         (fast check)
      │  ├─ --format json           (parse results)
      │  └─ --audit audit.json      (audit trail)
      │
      ├─ Selective
      │  └─ --rule rule_name        (one rule only)
      │
      └─ Config
         ├─ --config custom.toml    (alt config file)
         ├─ --ontology-override dir (alt ontology dir)
         └─ --output-override dir   (alt output dir)
```

---

## Architecture Reference: Data Flow

```
┌─────────────────────────────────────────────────────────────────────┐
│                      GGEN v5.0.2 ARCHITECTURE                        │
├─────────────────────────────────────────────────────────────────────┤
│                                                                       │
│  INPUT                 PROCESSING              OUTPUT                │
│  ─────────────────────────────────────────────────────────────       │
│                                                                       │
│  ggen.toml ──┐                                                        │
│              ├─→ [Config Parser] ──→ Config object                   │
│              │                                                        │
│  *.ttl ──────┼─→ [RDF Loader] ─────→ RDF Graph                       │
│              │       ↓                                                │
│  (ontology)  │   [SHACL Validator]                                   │
│              │       ↓                                                │
│              └─→ [Inference Engine] ──→ Enriched Graph               │
│                    ↓                                                  │
│              [CONSTRUCT Rules]                                        │
│                    ↓                                                  │
│  *.tera ────→ [Template Engine] ───→ [File Writer] ──→ *.rs, *.py.. │
│  (templates)  - Tera renderer                                         │
│                - SPARQL SELECT                      [Poka-Yoke] ─┐  │
│                - Result processing              (Protection)     │  │
│                                                    ↓              │  │
│              [Audit Trail]                   Protected paths     │  │
│                ↓                             Regenerate paths    │  │
│              audit.json ────────────────────────────────────────┘   │
│                                                                       │
│  KEY COMPONENTS:                                                     │
│  • Config Parser: TOML → internal representation                    │
│  • RDF Loader: Turtle/RDF/JSON-LD → RDF graph                      │
│  • Inference Engine: SPARQL CONSTRUCT rules → materialized facts    │
│  • Template Engine: Tera + SPARQL SELECT → code                    │
│  • Poka-Yoke: Protected/regenerate paths, warning headers          │
│                                                                       │
│  DECISION POINTS:                                                    │
│  • Load ontology? (validation happens here)                         │
│  • Run rules? (if inference.enabled)                                │
│  • Process all rules or specific? (--rule filter)                   │
│  • Write files? (unless --dry-run or --validate-only)              │
│                                                                       │
└─────────────────────────────────────────────────────────────────────┘
```

---

## Pattern Selection Matrix

| Goal | Rule Type | Template Lang | Query |
|------|-----------|---------------|-------|
| **REST API** | Capability detection | Tera | SELECT with FILTER |
| **Data Models** | Inheritance closure | Tera | CONSTRUCT transitive |
| **Tests** | Scenario extraction | Tera | SELECT with GROUP BY |
| **Validation** | SHACL expansion | Tera | CONSTRUCT constraints |
| **Interfaces** | Trait materialization | Tera | CONSTRUCT roles |
| **Docs** | Comment extraction | Tera/Markdown | SELECT with OPTIONAL |
| **Migrations** | Schema diff | Tera | CONSTRUCT deltas |

---

## Performance Tiers

| Ontology Size | Rule Complexity | Execution Time | Recommendation |
|---------------|-----------------|----------------|-----------------|
| <100 classes | Simple (transitive) | <50ms | Direct execution |
| 100-500 classes | Medium (multi-union) | 100-300ms | Acceptable |
| 500-2000 classes | Complex (nested) | 500ms-2s | Pre-materialize |
| >2000 classes | Any | >2s | Split into phases |

---

## When to Use What

| Scenario | Use | Why |
|----------|-----|-----|
| First time learning | SPARQL_INFERENCE_GUIDE.md | Concrete examples |
| Building agents | AI_AGI_INTEGRATION_GUIDE.md | 4-layer arch + code |
| Querying RDF | GRAPH_QUERYING_API.md | Full API reference |
| Configuring ggen | GGEN_TOML_REFERENCE.md | Complete schema |
| All CLI options | CLI_REFERENCE.md | Every flag documented |
| Quick lookup | SPARQL_REFERENCE_CARD.md | 2-page cheat sheet |
| Choosing pattern | Decision Trees (this page) | Navigate complexity |

