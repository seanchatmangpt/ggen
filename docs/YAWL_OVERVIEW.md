# YAWL v6 Architecture Documentation Overview

**Version:** 6.0.0
**Status:** Complete
**Generated:** 2026-03-26

This document provides a roadmap to the YAWL v6 architecture documentation suite.

---

## Quick Start: Which Document Should I Read?

### For System Users (Want to understand how generation works)
→ **Start with: [YAWL_ARCHITECTURE.md](./YAWL_ARCHITECTURE.md)**
- Five-stage μ-calculus pipeline
- Rule execution flow
- Data transformations
- Error handling

### For Architects (Want to understand why it's designed this way)
→ **Start with: [YAWL_DESIGN_DECISIONS.md](./YAWL_DESIGN_DECISIONS.md)**
- Why μ-calculus over monolithic
- Why manifest-driven over programmatic
- Why SPARQL over custom DSLs
- Trade-offs explained

### For Business Stakeholders (Want to understand the value)
→ **Start with: [YAWL_RATIONALE.md](./YAWL_RATIONALE.md)**
- Business goals (faster time-to-code, consistency)
- Technical goals (determinism, reusability)
- Cost-benefit analysis
- Future evolution

### For Developers (Want visual understanding)
→ **Start with: [YAWL_DIAGRAMS.md](./YAWL_DIAGRAMS.md)**
- Pipeline flow diagram
- Component architecture
- Data flow visualization
- Execution flowchart

### For Problem Solvers (Hitting limitations)
→ **Start with: [YAWL_LIMITATIONS_AND_EXTENSIONS.md](./YAWL_LIMITATIONS_AND_EXTENSIONS.md)**
- Known limitations (RDF size, recursion, federation)
- Workarounds for each limitation
- Extension points for future features
- Roadmap for feature additions

---

## Document Structure

```
YAWL Documentation Suite
├─ YAWL_OVERVIEW.md (this file)
│  └─ Navigation guide + document index
│
├─ YAWL_ARCHITECTURE.md
│  ├─ Five-stage μ-calculus pipeline (μ₁-μ₅)
│  ├─ Rule composition pattern (Inference + Generation)
│  ├─ Separation of concerns (ggen-core vs ggen-yawl)
│  ├─ Data flow through system
│  ├─ Determinism guarantees
│  ├─ Error handling strategy
│  ├─ Performance characteristics
│  └─ Known limitations + workarounds
│
├─ YAWL_DESIGN_DECISIONS.md
│  ├─ 1. μ-calculus over direct generation
│  ├─ 2. Manifest-driven over programmatic API
│  ├─ 3. SPARQL SELECT over custom DSL
│  ├─ 4. Tera templates over Handlebars/Mustache
│  ├─ 5. Content hashing for determinism
│  ├─ 6. Oxigraph for RDF/SPARQL
│  ├─ 7. Sequential execution for determinism
│  ├─ 8. Poka-yoke error proofing
│  ├─ 9. Manifest version pinning
│  └─ Summary table + principles
│
├─ YAWL_RATIONALE.md
│  ├─ Business goals
│  │  ├─ Reduce time-to-code for domain experts
│  │  ├─ Ensure consistency across artifacts
│  │  ├─ Enable rapid evolution of rules
│  │  ├─ Achieve reproducible builds
│  │  └─ Support multi-domain code generation
│  │
│  ├─ Technical goals
│  │  ├─ Zero-cost abstractions
│  │  ├─ Reusability via composition
│  │  ├─ Cognitive load minimization
│  │  ├─ Maintenance burden reduction
│  │  └─ Security posture
│  │
│  ├─ Architectural principles
│  ├─ Trade-off analysis
│  ├─ Constraints and assumptions
│  └─ Future evolution roadmap
│
├─ YAWL_DIAGRAMS.md
│  ├─ 1. Five-stage pipeline flow
│  ├─ 2. Component architecture
│  ├─ 3. Data flow visualization
│  ├─ 4. Trait relationships
│  ├─ 5. Rule execution flow
│  ├─ 6. Determinism verification chain
│  ├─ 7. Error handling flow
│  ├─ 8. Caching strategy (incremental)
│  ├─ 9. Separation of concerns
│  ├─ 10. Extension points
│  ├─ 11. Performance model
│  ├─ 12. Determinism guarantee chain
│  └─ 13. Domain integration pattern
│
└─ YAWL_LIMITATIONS_AND_EXTENSIONS.md
   ├─ Known Limitations (with workarounds)
   │  ├─ 1. RDF store size limit (~100MB)
   │  ├─ 2. No recursive Tera macros
   │  ├─ 3. SPARQL federation not supported
   │  ├─ 4. Sequential rule execution (no parallelism)
   │  └─ 5. Limited SPARQL feature support
   │
   ├─ Extension Points
   │  ├─ 1. Custom SPARQL filters
   │  ├─ 2. Custom Tera filters
   │  ├─ 3. Validation rules
   │  ├─ 4. Custom RDF stores (future)
   │  ├─ 5. Template engine plugins (future)
   │  ├─ 6. Lifecycle hooks
   │  ├─ 7. Manifest composition
   │  └─ 8. External rule plugins
   │
   ├─ Feature roadmap (v6.1 → v8.0)
   ├─ Migration guide
   └─ Problem-solving matrix
```

---

## The Five Core Concepts

### 1. The μ-Calculus Pipeline

YAWL uses a five-stage transformation pipeline (inspired by μ-calculus):

```
μ₁ Normalize    → Load RDF, validate with SHACL
μ₂ Extract      → Enrichment (inference) + querying (generation)
μ₃ Emit         → Render templates with SPARQL results
μ₄ Canonicalize → Deterministic formatting + hashing
μ₅ Receipt      → Cryptographic proof + audit trail
```

Each stage is independent, composable, and deterministic.

**Read:** [YAWL_ARCHITECTURE.md - μ-Calculus Pipeline](./YAWL_ARCHITECTURE.md#the-μ-calculus-pipeline)

### 2. Rules as Declarative Specifications

Two types of rules, both declarative (TOML):

```toml
# Inference Rule: Enrich RDF graph
[[inference.rules]]
name = "infer_workflow_tasks"
construct = "SPARQL CONSTRUCT query"
order = 1  # Execution order (deterministic)

# Generation Rule: Extract facts + render code
[[generation.rules]]
name = "generate_task_handler"
query = "SPARQL SELECT query"
template = "Tera template file"
output_file = "src/handlers/{{ task_name }}.rs"
```

Rules are version-controlled, composable, and reusable.

**Read:** [YAWL_DESIGN_DECISIONS.md - Manifest-Driven Rules](./YAWL_DESIGN_DECISIONS.md#2-manifest-driven-rules-over-programmatic-api)

### 3. Determinism as First-Class Concern

Same inputs → Same outputs (always)

Mechanisms:
- Explicit rule ordering (no races)
- Content hashing (cryptographic proof)
- Audit trail (complete provenance)
- Reproducible verification (ggen verify audit.json)

**Read:** [YAWL_ARCHITECTURE.md - Determinism Guarantees](./YAWL_ARCHITECTURE.md#determinism-guarantees)

### 4. Separation of Concerns

Three roles, three tools:

- **Ontologists:** Design RDF specifications
- **Developers:** Write SPARQL + Tera templates
- **Operations:** Run `ggen sync`, deploy results

Each role works independently, final composition via manifest.

**Read:** [YAWL_ARCHITECTURE.md - Separation of Concerns](./YAWL_ARCHITECTURE.md#separation-of-concerns)

### 5. Pragmatic Trade-offs

No perfect solution; explicit trade-offs:

- Speed vs Reproducibility: Choose reproducibility
- Expressiveness vs Simplicity: Choose simplicity
- Performance vs Genericity: Choose genericity

Workarounds provided for all limitations.

**Read:** [YAWL_LIMITATIONS_AND_EXTENSIONS.md](./YAWL_LIMITATIONS_AND_EXTENSIONS.md)

---

## Key Architectural Decisions at a Glance

| Decision | Benefit | Trade-off | Reference |
|----------|---------|-----------|-----------|
| μ-calculus pipeline | Composability, clear stages | Complexity | [Design Decisions](./YAWL_DESIGN_DECISIONS.md#1-μ-calculus-pipeline-over-direct-code-generation) |
| Manifest-driven rules | Non-developer accessible, auditable | Less flexible | [Design Decisions](./YAWL_DESIGN_DECISIONS.md#2-manifest-driven-rules-over-programmatic-api) |
| SPARQL SELECT | Industry standard, reusable | Steeper learning curve | [Design Decisions](./YAWL_DESIGN_DECISIONS.md#3-sparql-select-queries-over-custom-query-dsl) |
| Tera templates | Rust-native, Jinja2 compatible | Limited macros | [Design Decisions](./YAWL_DESIGN_DECISIONS.md#4-tera-templates-over-handlersmustache) |
| Content hashing | Reproducibility proof | Canonicalization overhead | [Design Decisions](./YAWL_DESIGN_DECISIONS.md#5-content-hashing-for-determinism-verification) |
| Oxigraph RDF | Rust-native, in-memory, self-contained | Limited to ~100MB | [Design Decisions](./YAWL_DESIGN_DECISIONS.md#6-oxigraph-for-rdfsparql) |

**Full table:** [YAWL_DESIGN_DECISIONS.md - Summary Table](./YAWL_DESIGN_DECISIONS.md#summary-table)

---

## Reading Paths by Role

### Software Engineer (Building Code Generators)

1. **YAWL_ARCHITECTURE.md** - Understand pipeline (30 min)
2. **YAWL_DIAGRAMS.md** - Visualize data flow (20 min)
3. **YAWL_DESIGN_DECISIONS.md** - Understand trade-offs (30 min)
4. **YAWL_LIMITATIONS_AND_EXTENSIONS.md** - Know constraints (20 min)

**Total:** ~2 hours (sufficient to build production systems)

### Architect/Technical Lead

1. **YAWL_RATIONALE.md** - Business + technical goals (30 min)
2. **YAWL_DESIGN_DECISIONS.md** - Design trade-offs (45 min)
3. **YAWL_ARCHITECTURE.md** - Deep understanding (45 min)
4. **YAWL_LIMITATIONS_AND_EXTENSIONS.md** - Roadmap (30 min)

**Total:** ~2.5 hours (sufficient for strategic decisions)

### Product Manager / Stakeholder

1. **YAWL_RATIONALE.md** - Why this approach (30 min)
2. **YAWL_DIAGRAMS.md** - How it works (visual) (20 min)
3. **YAWL_LIMITATIONS_AND_EXTENSIONS.md** - What's possible/impossible (30 min)

**Total:** ~1.5 hours (sufficient for business decisions)

### Domain Expert / Ontologist

1. **YAWL_ARCHITECTURE.md** - Stages μ₂a onwards (focus on SPARQL) (30 min)
2. **YAWL_DIAGRAMS.md** - Data flow (20 min)
3. **YAWL_LIMITATIONS_AND_EXTENSIONS.md** - SPARQL constraints (20 min)

**Total:** ~1.5 hours (sufficient to design ontologies)

---

## Completeness Assessment

### Coverage

- ✓ Overall system architecture
- ✓ Pipeline stages (μ₁-μ₅)
- ✓ Rule composition (Inference + Generation)
- ✓ Data flow and transformations
- ✓ Separation of concerns
- ✓ Error handling strategy
- ✓ Determinism mechanisms
- ✓ Performance characteristics
- ✓ Design decision rationale
- ✓ Business and technical goals
- ✓ Known limitations and workarounds
- ✓ Extension points and roadmap
- ✓ Visual diagrams (13 comprehensive diagrams)

### Gap Analysis

**Documented:**
- Architecture (100%)
- Design decisions (100%)
- Business rationale (100%)
- Limitations & workarounds (100%)
- Diagrams (100%)

**Future Documentation:**
- API reference (will be auto-generated from source)
- Example projects (will be created as ggen-examples)
- Tutorial for building domain generators (in progress)
- SPARQL pattern library (in progress)
- Tera template cookbook (planned)

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 6.0.0 | 2026-03-26 | Initial documentation suite (YAWL_OVERVIEW, YAWL_ARCHITECTURE, YAWL_DESIGN_DECISIONS, YAWL_RATIONALE, YAWL_DIAGRAMS, YAWL_LIMITATIONS_AND_EXTENSIONS) |

---

## How to Use This Documentation

### For Learning
1. Start with your role's "Reading Path" (above)
2. Skim headings first to find relevant sections
3. Use cross-references to dive deeper

### For Reference
1. Use document table of contents (generated)
2. Search for keywords (Ctrl+F)
3. Check index of concepts (below)

### For Verification
1. Review relevant architectural diagrams
2. Check design decisions table
3. Confirm limitations and workarounds

---

## Concept Index

### Architectural Concepts
- μ-calculus pipeline: [YAWL_ARCHITECTURE.md](./YAWL_ARCHITECTURE.md#the-μ-calculus-pipeline)
- Five stages (μ₁-μ₅): [YAWL_ARCHITECTURE.md](./YAWL_ARCHITECTURE.md#stage-μ1-normalize)
- Inference rules (CONSTRUCT): [YAWL_ARCHITECTURE.md](./YAWL_ARCHITECTURE.md#μ2a-inference-rules-sparql-construct)
- Generation rules (SELECT): [YAWL_ARCHITECTURE.md](./YAWL_ARCHITECTURE.md#μ2b-generation-rules-sparql-select)

### Design Decisions
- Why manifest-driven: [YAWL_DESIGN_DECISIONS.md](./YAWL_DESIGN_DECISIONS.md#2-manifest-driven-rules-over-programmatic-api)
- Why SPARQL: [YAWL_DESIGN_DECISIONS.md](./YAWL_DESIGN_DECISIONS.md#3-sparql-select-queries-over-custom-query-dsl)
- Why Tera: [YAWL_DESIGN_DECISIONS.md](./YAWL_DESIGN_DECISIONS.md#4-tera-templates-over-handlersmustache)
- Why determinism: [YAWL_DESIGN_DECISIONS.md](./YAWL_DESIGN_DECISIONS.md#5-content-hashing-for-determinism-verification)

### Business Goals
- Faster time-to-code: [YAWL_RATIONALE.md](./YAWL_RATIONALE.md#1-reduce-time-to-code-for-domain-experts)
- Consistency: [YAWL_RATIONALE.md](./YAWL_RATIONALE.md#2-ensure-consistency-across-generated-artifacts)
- Reproducible builds: [YAWL_RATIONALE.md](./YAWL_RATIONALE.md#4-achieve-reproducible-builds)

### Limitations
- RDF size limit: [YAWL_LIMITATIONS_AND_EXTENSIONS.md](./YAWL_LIMITATIONS_AND_EXTENSIONS.md#1-rdf-store-size-limit-100mb)
- No recursion: [YAWL_LIMITATIONS_AND_EXTENSIONS.md](./YAWL_LIMITATIONS_AND_EXTENSIONS.md#2-no-recursive-tera-macros)
- No federation: [YAWL_LIMITATIONS_AND_EXTENSIONS.md](./YAWL_LIMITATIONS_AND_EXTENSIONS.md#3-sparql-federation-not-supported)

### Visualizations
- Pipeline diagram: [YAWL_DIAGRAMS.md](./YAWL_DIAGRAMS.md#1-five-stage-pipeline-μ-calculus)
- Component architecture: [YAWL_DIAGRAMS.md](./YAWL_DIAGRAMS.md#2-component-architecture)
- Data flow: [YAWL_DIAGRAMS.md](./YAWL_DIAGRAMS.md#3-data-flow-through-pipeline)
- Execution flow: [YAWL_DIAGRAMS.md](./YAWL_DIAGRAMS.md#5-rule-execution-flow)

---

## Contributing to Documentation

Found an error? Want to add clarification?

1. Fork repository
2. Edit relevant document
3. Verify cross-references still work
4. Submit pull request

Documentation maintainers: @seanchatmangpt

---

## Related Resources

- **Repository:** https://github.com/seanchatmangpt/ggen
- **C4 Architecture Diagrams:** [docs/10-architecture/](../10-architecture/)
- **API Reference:** (Auto-generated from source code)
- **Examples:** ggen-examples repository (coming soon)

---

## Feedback

Documentation quality is important. Please report:
- Unclear explanations
- Missing examples
- Outdated information
- Broken cross-references
- Suggestions for improvement

Via: GitHub Issues + email (documentation@ggen.dev)

---

**Documentation Version:** 6.0.0
**Last Updated:** 2026-03-26
**Status:** Complete (Comprehensive architectural documentation)
