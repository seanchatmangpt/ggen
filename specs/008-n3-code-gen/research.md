# Research: N3/CONSTRUCT Semantic Code Generator

**Branch**: `008-n3-code-gen` | **Date**: 2024-12-14 | **Status**: Complete

---

## Executive Summary

This research phase investigates unknowns for implementing N3-based semantic code generation in ggen v5. Key findings: oxigraph 0.5.1 provides sufficient SPARQL/CONSTRUCT support; N3 rule syntax requires custom implementation due to limited oxigraph N3 rule support; existing ggen infrastructure (Graph, template, frontmatter) provides 80% foundation.

---

## Research Questions & Findings

### RQ-001: oxigraph N3/CONSTRUCT Capabilities

**Question**: What N3/CONSTRUCT features does oxigraph 0.5.1 support?

**Finding**:
- **CONSTRUCT queries**: Fully supported via `QueryResults::Graph` variant
- **N3 parsing**: Limited - oxigraph supports N3 **syntax** for loading triples, but NOT N3 **rules** (implications)
- **Evidence**: `crates/ggen-core/src/graph/types.rs:15` shows `Graph(Vec<String>)` for CONSTRUCT results

**Implication**: N3 rules (`{ premises } => { conclusions }`) require custom implementation or alternative approach.

**Decision**: Implement rules in Turtle-compatible format using SPARQL CONSTRUCT patterns. Store rules as named CONSTRUCT queries in `ggen.toml` rather than N3 rule files.

---

### RQ-002: Existing CONSTRUCT Infrastructure

**Question**: What CONSTRUCT support already exists in ggen-core?

**Finding**:
- `CachedResult::Graph(Vec<String>)` holds CONSTRUCT output (types.rs:68)
- `GraphQuery` provides query execution with caching (query.rs:63-65)
- Template system supports SPARQL results via `sparql_results` (template.rs:131)
- Code ontology defines `code:Struct`, `code:Trait`, `code:Method`, etc. (code_ontology.ttl)

**Implication**: CONSTRUCT → Code Graph → Template pipeline foundation exists. Need to:
1. Add CONSTRUCT-specific execution method
2. Parse CONSTRUCT results back into Graph for chaining
3. Add code graph materialization step

---

### RQ-003: N3 Rule Alternatives (STRATEGIC DECISION)

**Question**: How can we achieve N3-style inference without full N3 rule support?

**Finding**: Three approaches evaluated:

| Approach | Pros | Cons | Decision |
|----------|------|------|----------|
| **A. SPARQL CONSTRUCT chains** | Native oxigraph support, composable, testable, deterministic | Verbose for complex rules | **CANONICAL** |
| B. External reasoner (EYE) | Full N3 support | External dependency, process spawn, non-deterministic | Rejected |
| C. Custom N3 parser | Full control | Significant implementation effort, duplicates SPARQL capability | Rejected |

**STRATEGIC DECISION**: Named CONSTRUCT IS the semantic IR. Do NOT implement N3 rule execution.

**Rationale** (validated by stakeholder review):
1. **Runtime Simplicity**: oxigraph provides mature CONSTRUCT; N3 engines add complexity without benefit
2. **Determinism**: CONSTRUCT order is explicit; N3 fixpoint semantics introduce non-determinism
3. **Testability**: Each rule is independently testable via SPARQL
4. **Explainability**: Pipeline steps map 1:1 to queries in audit trail
5. **Performance**: No forward-chaining overhead; direct graph transformation

**Future Option**: If N3 surface syntax desired, implement as transpiler to CONSTRUCT (source language only).

**Decision**: Encode inference rules as named CONSTRUCT queries in `ggen.toml`. Example:

```toml
[[inference.rules]]
name = "auditable_fields"
description = "Add created_at/updated_at to auditable entities"
construct = """
PREFIX code: <http://ggen.dev/code#>
PREFIX : <http://ggen.dev/model#>
CONSTRUCT {
  ?struct code:structFields ?created_field .
  ?struct code:structFields ?updated_field .
}
WHERE {
  ?struct a code:Struct ;
          :auditable true .
  BIND(IRI(CONCAT(STR(?struct), "_created_at")) AS ?created_field)
  BIND(IRI(CONCAT(STR(?struct), "_updated_at")) AS ?updated_field)
}
"""
```

---

### RQ-004: RDF Format Support Matrix

**Question**: What RDF formats are supported for input/output?

**Finding**: From `crates/ggen-domain/src/graph/load.rs`:

| Format | Load | Export | Extension |
|--------|------|--------|-----------|
| Turtle | Yes | Yes | .ttl |
| N-Triples | Yes | Yes | .nt |
| RDF/XML | Yes | Yes | .rdf, .xml |
| N3 | Yes* | No | .n3 |
| JSON-LD | Partial | No | .jsonld |

*N3 loads as triples only (no rule execution)

**Implication**: Recommend Turtle as primary format for both ontologies and code graphs.

---

### RQ-005: CONSTRUCT Chaining Strategy

**Question**: How should multiple CONSTRUCT queries compose?

**Finding**: Two viable patterns:

**Pattern A: Sequential Materialization** (Selected)
```
ontology → CONSTRUCT₁ → materialize → CONSTRUCT₂ → materialize → code_graph
```
- Each CONSTRUCT output materialized (inserted) into working graph
- Next CONSTRUCT can query previous results
- Clear dependency order
- Testable at each stage

**Pattern B: SPARQL Subqueries**
```
ontology → CONSTRUCT { ... WHERE { { nested SELECT } } }
```
- Single query, complex nesting
- Harder to debug/test
- Limited composability

**Decision**: Pattern A with configurable materialization strategy.

---

### RQ-006: ggen.toml Manifest Schema

**Question**: What schema should ggen.toml use for code generation?

**Finding**: Analyzed existing patterns in codebase and industry (cargo, pyproject.toml).

**Proposed Schema**:
```toml
[project]
name = "my-domain"
version = "1.0.0"

[ontology]
source = "domain/model.ttl"          # Primary ontology
imports = ["domain/base.ttl"]        # Additional imports
base_iri = "http://example.org/"     # Default base IRI

[[inference.rules]]                   # Named inference rules (CONSTRUCT)
name = "auditable_fields"
construct = "..."
order = 1                            # Execution order

[[generation.rules]]                  # Code generation rules
query = "queries/structs.sparql"     # SPARQL SELECT/CONSTRUCT
template = "templates/struct.tera"   # Tera template
output_file = "src/models/{{name}}.rs"  # Output path pattern
skip_empty = true                    # Skip if no results

[validation]
shacl = ["shapes/domain.ttl"]        # SHACL shape files
validate_syntax = true               # Rust syntax check
no_unsafe = true                     # Reject unsafe code

[generation]
max_sparql_timeout_ms = 5000
max_reasoning_timeout_ms = 5000
require_audit_trail = true
determinism_salt = "stable-v1"       # For deterministic IRI generation
```

---

### RQ-007: Deterministic Output Strategy

**Question**: How to ensure 100% byte-identical output across runs?

**Finding**: Non-determinism sources identified:

| Source | Mitigation |
|--------|------------|
| HashMap iteration order | Use `BTreeMap` everywhere |
| SPARQL result ordering | Add `ORDER BY` to all queries |
| IRI generation | Use deterministic CONCAT with stable salt |
| Timestamp fields | Omit or use fixed epoch |
| Template rendering | Tera is deterministic |
| File write order | Sort files before writing |

**Decision**: Enforce deterministic patterns:
1. All internal maps use `BTreeMap` (already done in ggen-core)
2. Manifest validation rejects queries without `ORDER BY`
3. Code graph IRIs use hash-based generation
4. Audit trail records all inputs for verification

---

### RQ-008: Error Handling & Exit Codes

**Question**: How do existing semantic exit codes integrate?

**Finding**: From `crates/ggen-cli/src/error.rs`:

| Code | GgenError Variant | New Usage |
|------|-------------------|-----------|
| 0 | Success | Generation complete |
| 1 | ValidationError | SHACL/ontology validation failed |
| 2 | SparqlError | Query syntax/execution error |
| 3 | TemplateError | Tera rendering failed |
| 4 | OutputInvalid | Generated Rust fails validation |
| 5 | Timeout | SPARQL/reasoning timeout |
| 127 | Internal | Unexpected error |

**Implication**: No new exit codes needed. Map new errors to existing categories.

---

### RQ-009: Template Integration Points

**Question**: How do CONSTRUCT results flow to templates?

**Finding**: Current template system (`template.rs`) supports:
- `sparql_results` BTreeMap for query results
- `sparql_first()`, `sparql_values()` functions
- Frontmatter `query:` field for inline SPARQL

**Proposed Flow**:
```
CONSTRUCT query → CachedResult::Graph → parse back to Graph
→ SELECT from code graph → Solutions → Tera context
→ template.render() → Rust code
```

**New Template Fields** (extend Frontmatter):
```yaml
---
# Existing
query: structs_query
# New
code_graph: true           # Load code graph instead of domain ontology
construct_chain:           # Pre-execute CONSTRUCT chain
  - infer_derives
  - infer_audit_fields
---
```

---

### RQ-010: Performance Benchmarks (Existing)

**Question**: What are current performance characteristics?

**Finding**: From existing benchmarks:

| Operation | Current SLO | Measured |
|-----------|-------------|----------|
| SPARQL SELECT | <10ms | <5ms (cached) |
| Template parse | <5ms | 115ns |
| Graph insert (1k triples) | <100ms | ~50ms |
| Full generation | <5s | ~2s (estimate) |

**Implication**: Current infrastructure meets SC-001 (<5s for 10-50 entities). Concern: CONSTRUCT chain with 500+ entities may approach 30s limit (SC-009).

**Mitigation**: Add progressive materialization, query result caching, parallel template rendering.

---

## Risk Mitigation Decisions

| Risk | Mitigation Implemented |
|------|------------------------|
| oxigraph N3 rules incomplete | Use CONSTRUCT chains instead |
| CONSTRUCT performance | Sequential materialization with caching |
| Non-deterministic output | BTreeMap + ORDER BY + deterministic IRIs |
| N3 rule cycles | Manifest validation + 5s timeout per rule |
| Template complexity | Leverage existing template.rs infrastructure |

---

## Architectural Decisions

### AD-001: Rules as CONSTRUCT Queries
**Context**: oxigraph doesn't execute N3 rules
**Decision**: Encode rules as named CONSTRUCT queries in ggen.toml
**Consequence**: Simpler implementation, native SPARQL, composable, but less expressive than full N3

### AD-002: Sequential Materialization
**Context**: Need to chain multiple CONSTRUCT queries
**Decision**: Execute CONSTRUCT, materialize to graph, repeat
**Consequence**: Clear debugging, testable stages, potential performance overhead

### AD-003: ggen.toml as Single Source of Truth
**Context**: Need unified configuration for generation pipeline
**Decision**: All paths, queries, templates, validation in ggen.toml
**Consequence**: Discoverable, version-controlled, agent-friendly

### AD-004: Code Graph as RDF
**Context**: Need intermediate representation before template rendering
**Decision**: Use code ontology (code:Struct, etc.) as in-memory RDF graph
**Consequence**: Query-able, validatable, but requires parse/serialize overhead

---

## Next Steps

1. **Phase 1**: Create data-model.md defining GgenManifest, CodeGraph, InferenceRule, SyncOptions types ✓
2. **Phase 1**: Create CLI contract for `ggen sync` (THE ONLY command in v5) ✓
3. **Phase 1**: Create quickstart.md with minimal working example using `ggen sync` ✓
4. **Phase 2**: Generate tasks.md via `/speckit.tasks` ✓

**Note**: ggen v5 uses `ggen sync` as the ONLY command. All legacy commands removed for fresh start.

---

## References

- oxigraph SPARQL support: https://github.com/oxigraph/oxigraph
- N3 specification: https://www.w3.org/DesignIssues/Notation3.html
- SHACL specification: https://www.w3.org/TR/shacl/
- Existing ggen-core infrastructure: `crates/ggen-core/src/`
