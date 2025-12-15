# Implementation Plan: N3/CONSTRUCT Semantic Code Generator

**Branch**: `008-n3-code-gen` | **Date**: 2024-12-14 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/008-n3-code-gen/spec.md`

## Summary

Transform ggen from template-centric code generation to semantic graph transformation. Leverage the existing 80% infrastructure (oxigraph 0.5.1, Tera 1.20, Graph with SPARQL caching) to implement the 20% that unlocks the full paradigm: ggen.toml parsing, CONSTRUCT-based inference rules, Code-Graph pipeline, SHACL validation, and audit trail generation.

**Core Innovation**: Code generation becomes `ontology → named CONSTRUCT rules → code graph → Tera serialization → Rust code`, enabling inference-driven generation, semantic validation, and agent-safe determinism.

## Strategic Decision: Named CONSTRUCT as Canonical Rule Language

**Decision**: Do NOT implement full N3 rule execution. Named CONSTRUCT queries ARE the semantic IR.

**Rationale**:
1. **Runtime Simplicity**: oxigraph provides mature CONSTRUCT support; N3 rule engines add complexity without benefit
2. **Determinism**: CONSTRUCT execution order is explicit and controllable; N3 fixpoint semantics introduce non-determinism
3. **Testability**: Each CONSTRUCT rule is independently testable via SPARQL
4. **Explainability**: Pipeline steps map 1:1 to CONSTRUCT queries in audit trail
5. **Performance**: No forward-chaining overhead; direct graph transformation

**Architecture**:
```
Complex logic     → Rust (type-safe, tested)
Graph shaping     → SPARQL CONSTRUCT (semantic, composable)
Determinism       → Pipeline orchestration (ordered execution)
```

**Future Option**: If N3 surface syntax is desired, implement as transpiler to CONSTRUCT (source language only, not runtime).

## Technical Context

**Language/Version**: Rust 1.75+ (edition 2021), existing ggen v4.0.0 workspace
**Primary Dependencies**:
- oxigraph 0.5.1 (SPARQL engine, CONSTRUCT support, partial N3)
- Tera 1.20 (template engine with custom filters)
- toml 0.9 (manifest parsing)
- serde 1.0 + serde_json (serialization)
- thiserror 2.0 (error handling)

**Storage**: File system (ggen.toml, .ttl ontologies, generated .rs files, audit.json)
**Testing**: cargo test via cargo make, Chicago TDD, 1,168+ tests baseline
**Target Platform**: Linux, macOS, Windows (cross-platform Rust CLI)
**Project Type**: Workspace crate addition (ggen-core enhancement + CLI thin wrapper)

**Performance Goals**:
- Generation: <5s for 10-50 entities, <30s for 500+ entities
- SPARQL queries: <10ms (existing SLO met)
- Template parsing: <5ms (existing SLO: 115ns measured)
- Determinism: 100% byte-identical output

**Constraints**:
- max_sparql_timeout_ms: 5000ms (configurable)
- max_reasoning_timeout_ms: 5000ms (configurable)
- no_unsafe: true (poka-yoke compliance)
- Binary size: <5MB (existing: 2.8MB)

**Scale/Scope**:
- Target: 10-50 entities typical, 500+ entities enterprise
- Existing codebase: 15,260 lines in ggen-core, 2,098 lines in ggen-cli

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

Verify compliance with ggen Constitution v1.0.0 (`.specify/memory/constitution.md`):

- [x] **I. Crate-First Architecture**: Feature extends ggen-core (existing crate) with new modules. No new crate needed - leverages existing Graph, template, frontmatter infrastructure.
- [x] **II. Deterministic RDF Projections**: Core design goal. Same ontology + rules = byte-identical output. Audit trail enables verification. Dependencies version-locked in Cargo.lock.
- [x] **III. Chicago TDD**: All new code tested with state-based tests using real oxigraph, real file system. 80%+ coverage on critical paths (N3 loading, CONSTRUCT execution, template rendering).
- [x] **IV. cargo make Protocol**: All development uses `cargo make check`, `cargo make test-unit`, `cargo make lint`. SLOs: <5s check, <30s full test.
- [x] **V. Type-First Thinking**: Manifest parsed into strongly-typed GgenManifest. Code graph uses typed code:: ontology. APIs use Result<T, GgenError>.
- [x] **VI. Andon Signal Protocol**: Monitor cargo make output for RED (compilation error, test failure). YELLOW signals addressed before release.
- [x] **VII. Error Handling**: All production code uses Result<T, GgenError>. Semantic exit codes (0-5) provide agent-friendly error categories. No unwrap/expect outside tests.
- [x] **VIII. Concurrent Execution**: Implementation batches file writes. No root folder saves. Code in crates/ggen-core/src/, tests in crates/ggen-core/tests/.
- [x] **IX. Lean Six Sigma Quality**: Existing workspace lints enforced (deny all, pedantic, nursery, cargo). Pre-commit hooks run cargo make pre-commit.

**Quality Gates Pass?**: [x] YES

## Project Structure

### Documentation (this feature)

```text
specs/008-n3-code-gen/
├── spec.md              # Feature specification (complete)
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output (CLI contract)
│   └── cli-contract.md  # CLI interface specification
├── checklists/
│   └── requirements.md  # Quality validation (complete)
└── tasks.md             # Phase 2 output (via /speckit.tasks)
```

### Source Code (repository root)

```text
crates/ggen-core/src/
├── rdf/
│   ├── mod.rs                    # RDF module (existing)
│   ├── code_ontology.ttl         # Code vocabulary (existing, 394 lines)
│   └── construct_pipeline.rs     # NEW: CONSTRUCT → Code Graph pipeline
├── manifest/
│   ├── mod.rs                    # NEW: ggen.toml parsing module
│   ├── parser.rs                 # NEW: TOML → GgenManifest
│   ├── types.rs                  # NEW: Manifest type definitions
│   └── validation.rs             # NEW: Manifest schema validation
├── codegen/
│   ├── mod.rs                    # Existing codegen module
│   ├── pipeline.rs               # NEW: Full generation pipeline
│   ├── code_graph.rs             # NEW: Code graph operations
│   └── audit.rs                  # NEW: Audit trail generation
├── graph/
│   ├── core.rs                   # Existing: Graph with SPARQL caching
│   ├── query.rs                  # Existing: SPARQL query execution
│   └── construct.rs              # NEW: CONSTRUCT-specific operations
├── template.rs                   # Existing: Frontmatter + Tera (1,114 lines)
└── error.rs                      # Existing: Semantic exit codes (185 lines)

crates/ggen-cli/src/
├── commands/
│   ├── mod.rs                    # Existing command registration
│   ├── generate.rs               # NEW: `ggen generate` command
│   └── validate.rs               # NEW: `ggen validate` command
└── error.rs                      # Existing: CLI error handling

crates/ggen-core/tests/
├── manifest/
│   ├── parser_tests.rs           # ggen.toml parsing tests
│   └── validation_tests.rs       # Schema validation tests
├── codegen/
│   ├── pipeline_tests.rs         # Full pipeline integration tests
│   ├── code_graph_tests.rs       # Code graph operation tests
│   └── audit_tests.rs            # Audit trail tests
└── construct/
    ├── query_tests.rs            # CONSTRUCT query tests
    └── materialization_tests.rs  # Sequential materialization tests
```

**Structure Decision**: Extends existing ggen-core crate with new modules (manifest/, codegen enhancements). No new crates needed. N3 loader removed per strategic decision - rules expressed as named CONSTRUCT queries in ggen.toml.

## Complexity Tracking

> No constitution violations. All principles satisfied.

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| None | N/A | N/A |

## Implementation Phases

### Phase 0: Research (Complete)

See [research.md](./research.md) for resolved unknowns.

### Phase 1: Design & Contracts (This Phase)

1. **Data Model**: Code graph entities, manifest schema, N3 rule structure
2. **API Contracts**: CLI interface, generation pipeline, error handling
3. **Quickstart**: Minimal working example for developers

### Phase 2: Task Breakdown (Next)

Generated via `/speckit.tasks` after Phase 1 artifacts complete.

## Dependencies & Integration Points

### Existing Infrastructure (Leverage)

| Component | Location | Status | Leverage |
|-----------|----------|--------|----------|
| Graph (oxigraph wrapper) | `ggen-core/src/graph/core.rs` | Complete | Query caching, SPARQL execution |
| Template (Tera + frontmatter) | `ggen-core/src/template.rs` | Complete | SPARQL integration, code rendering |
| Code Ontology | `ggen-core/src/rdf/code_ontology.ttl` | Complete | code:Struct, code:Trait vocabulary |
| Error Types | `ggen-cli/src/error.rs` | Complete | Semantic exit codes (0-5) |
| Poka-yoke Guards | `ggen-core/src/poka_yoke/` | Complete | Timeout, validation, dry-run |

### New Components (Implement)

| Component | Location | Priority | Dependencies |
|-----------|----------|----------|--------------|
| Manifest Parser | `ggen-core/src/manifest/parser.rs` | P1 | toml 0.9, serde |
| Manifest Types | `ggen-core/src/manifest/types.rs` | P1 | serde |
| CONSTRUCT Pipeline | `ggen-core/src/rdf/construct_pipeline.rs` | P1 | Graph |
| Code Graph Ops | `ggen-core/src/codegen/code_graph.rs` | P1 | Graph, Code Ontology |
| Audit Trail | `ggen-core/src/codegen/audit.rs` | P2 | Pipeline |
| CLI Generate | `ggen-cli/src/commands/generate.rs` | P2 | Pipeline, Manifest |
| CLI Validate | `ggen-cli/src/commands/validate.rs` | P3 | Manifest, SHACL |

**Removed**: `n3_loader.rs` - N3 rule execution not implemented per strategic decision. Rules expressed as named CONSTRUCT queries in ggen.toml.

## Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| CONSTRUCT performance on large graphs | Low | Medium | Query caching already implemented, add materialized views |
| Rule dependency cycles | Low | Medium | Manifest validation detects circular `order` dependencies |
| Template complexity | Low | Low | Existing template.rs handles SPARQL results |
| SHACL validation performance | Low | Low | Validate before CONSTRUCT chain, not after each rule |

**Risks Eliminated by Strategic Decision**:
- ~~oxigraph N3 support incomplete~~ → Not using N3 rules
- ~~N3 fixpoint non-termination~~ → Explicit rule ordering
- ~~N3 engine complexity~~ → Native SPARQL only

## Success Metrics

Track against specification success criteria:

- SC-001: <5s generation for 10-50 entities
- SC-002: 100% determinism (hash comparison)
- SC-003: 60% annotation reduction via N3 inference
- SC-005: 0 syntax errors in generated code
- SC-010: Error messages identify exact IRI/property
