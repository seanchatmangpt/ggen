# Implementation Plan: ggen v5 - Unified Sync Command

**Branch**: `008-n3-code-gen` | **Date**: 2024-12-14 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/008-n3-code-gen/spec.md`

## Summary

ggen v5 is a **fresh start** with a single command: `ggen sync`. This replaces ALL existing commands with one unified code synchronization pipeline.

**Core Innovation**: Code generation becomes `ggen.toml → ontology → CONSTRUCT inference → SELECT + Template → Rust code`, enabling inference-driven generation, semantic validation, and agent-safe determinism.

**Breaking Change**: All existing commands are removed. v5 is `ggen sync` only.

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

## Technical Context

**Language/Version**: Rust 1.75+ (edition 2021), existing ggen v4.0.0 workspace
**Primary Dependencies**:
- oxigraph 0.5.1 (SPARQL engine, CONSTRUCT support)
- Tera 1.20 (template engine with custom filters)
- toml 0.9 (manifest parsing)
- serde 1.0 + serde_json (serialization)
- thiserror 2.0 (error handling)

**Storage**: File system (ggen.toml, .ttl ontologies, generated .rs files, audit.json)
**Testing**: cargo test via cargo make, Chicago TDD, 1,168+ tests baseline
**Target Platform**: Linux, macOS, Windows (cross-platform Rust CLI)
**Project Type**: Workspace crate modification (remove old commands, add sync)

**Performance Goals**:
- Sync: <5s for 10-50 entities, <30s for 500+ entities
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

- [x] **I. Crate-First Architecture**: Feature modifies ggen-cli (command removal + sync) and ggen-core (pipeline). Leverages existing Graph, template, frontmatter infrastructure.
- [x] **II. Deterministic RDF Projections**: Core design goal. Same ontology + rules = byte-identical output. Audit trail enables verification. Dependencies version-locked in Cargo.lock.
- [x] **III. Chicago TDD**: All new code tested with state-based tests using real oxigraph, real file system. 80%+ coverage on critical paths.
- [x] **IV. cargo make Protocol**: All development uses `cargo make check`, `cargo make test-unit`, `cargo make lint`. SLOs: <5s check, <30s full test.
- [x] **V. Type-First Thinking**: Manifest parsed into strongly-typed GgenManifest. Code graph uses typed code:: ontology. APIs use Result<T, GgenError>.
- [x] **VI. Andon Signal Protocol**: Monitor cargo make output for RED (compilation error, test failure). YELLOW signals addressed before release.
- [x] **VII. Error Handling**: All production code uses Result<T, GgenError>. Semantic exit codes (0-6) provide agent-friendly error categories. No unwrap/expect outside tests.
- [x] **VIII. Concurrent Execution**: Implementation batches file writes. No root folder saves. Code in crates/ggen-core/src/, tests in crates/ggen-core/tests/.
- [x] **IX. Lean Six Sigma Quality**: Existing workspace lints enforced (deny all, pedantic, nursery, cargo). Pre-commit hooks run cargo make pre-commit.

**Quality Gates Pass?**: [x] YES

## Data Flow

```
ggen.toml
    │
    ├── [ontology] ──────────────────────────────────────┐
    │   source: domain/model.ttl                         │
    │   imports: [base.ttl]                              ▼
    │   prefixes: {...}                           ┌─────────────┐
    │                                             │  GRAPH      │
    ├── [[inference.rules]] ──────────────────────│  (oxigraph) │
    │   CONSTRUCT queries add triples ──────────→ │  domain +   │
    │                                             │  code graph │
    └── [[generation.rules]]                      └──────┬──────┘
        query: SELECT ...  ←─────────────────────────────┘
        template: struct.tera                            │
        output_file: src/{{name}}.rs                     ▼
                                              ┌─────────────────┐
                                              │ Generated Files │
                                              │ + audit.json    │
                                              └─────────────────┘
```

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
│   └── construct_pipeline.rs     # CONSTRUCT → Code Graph pipeline (existing)
├── manifest/
│   ├── mod.rs                    # ggen.toml parsing module (existing)
│   ├── parser.rs                 # TOML → GgenManifest (existing)
│   ├── types.rs                  # Manifest type definitions (existing)
│   └── validation.rs             # Manifest schema validation (existing)
├── codegen/
│   ├── mod.rs                    # Codegen module (existing)
│   ├── pipeline.rs               # Full generation pipeline (existing)
│   ├── code_graph.rs             # Code graph operations (existing)
│   └── audit.rs                  # Audit trail generation
├── graph/
│   ├── core.rs                   # Graph with SPARQL caching (existing)
│   ├── query.rs                  # SPARQL query execution (existing)
│   └── construct.rs              # CONSTRUCT-specific operations (existing)
├── template.rs                   # Frontmatter + Tera (1,114 lines) - process_graph() for graph-driven context
└── error.rs                      # Semantic exit codes (185 lines)

crates/ggen-cli/src/
├── cmds/
│   ├── mod.rs                    # Command router - REWRITE for sync only
│   ├── sync.rs                   # NEW: `ggen sync` command (THE ONLY COMMAND)
│   └── helpers.rs                # Shared CLI utilities (keep)
└── main.rs                       # Entry point

COMMANDS TO DELETE:
├── cmds/generate.rs              # DELETE - replaced by sync
├── cmds/template.rs              # DELETE - replaced by sync
├── cmds/project.rs               # DELETE - add back later
├── cmds/graph.rs                 # DELETE - add back later
├── cmds/ontology.rs              # DELETE - add back later
├── cmds/marketplace.rs           # DELETE - add back later
├── cmds/ai.rs                    # DELETE - add back later
├── cmds/test.rs                  # DELETE - add back later
├── cmds/utils.rs                 # DELETE - add back later
├── cmds/ci.rs                    # DELETE - add back later
└── cmds/workflow.rs              # DELETE - add back later

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

**Structure Decision**: Fresh start for v5. Only `ggen sync` command exists. All legacy commands removed. Utilities can be added back incrementally in future versions.

## Complexity Tracking

> No constitution violations. All principles satisfied.

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| None | N/A | N/A |

## Implementation Phases

### Phase 1: Core Sync Command

1. **T1**: Create `crates/ggen-cli/src/cmds/sync.rs` - `#[verb("sync", "ggen")]` with all CLI options
2. **T2**: Register sync in `crates/ggen-cli/src/cmds/mod.rs` - Add `pub mod sync;`
3. **T3**: Implement `SyncOptions` type in `crates/ggen-core/src/codegen/mod.rs`

### Phase 2: Complete Pipeline Implementation

4. **T4**: Implement CONSTRUCT execution in `pipeline.rs` - `execute_inference_rule()`
5. **T5**: Implement SELECT + Template generation - `execute_generation_rule()`
6. **T6**: Integrate `Template::process_graph()` - Pass SPARQL row bindings as template context
7. **T7**: Implement file writing with modes - Handle GenerationMode::Create/Overwrite/Merge

### Phase 3: Features

8. **T8**: Implement `--dry-run` mode - Preview files without writing
9. **T9**: Implement `--watch` mode - Use notify crate for file watching
10. **T10**: Implement audit trail generation - Generate audit.json with hashes
11. **T11**: Implement `--validate-only` - Validate manifest and ontology without generating

### Phase 4: Remove ALL Old Commands (Fresh Start)

12. **T12**: Delete all command modules except sync and helpers
    - Delete: `generate.rs`, `template.rs`, `project.rs`, `graph.rs`, `ontology.rs`, `marketplace.rs`, `ai.rs`, `test.rs`, `utils.rs`, `ci.rs`, `workflow.rs`
    - Keep: `sync.rs` (new), `helpers.rs` (internal utilities), `mod.rs` (router)
13. **T13**: Rewrite `mod.rs` - Only register `sync` module
14. **T14**: Update bin entry point - Ensure clean CLI with only `ggen sync`

### Phase 5: Testing

15. **T15**: Unit tests for pipeline - Test CONSTRUCT execution, SELECT + Template rendering, output path expansion
16. **T16**: Integration test for ggen sync - End-to-end test with sample ggen.toml

## Dependencies & Integration Points

### Existing Infrastructure (Leverage)

| Component | Location | Status | Leverage |
|-----------|----------|--------|----------|
| Graph (oxigraph wrapper) | `ggen-core/src/graph/core.rs` | Complete | Query caching, SPARQL execution |
| Template (Tera + frontmatter) | `ggen-core/src/template.rs` | Complete | SPARQL integration, process_graph() |
| Code Ontology | `ggen-core/src/rdf/code_ontology.ttl` | Complete | code:Struct, code:Trait vocabulary |
| Error Types | `ggen-core/src/error.rs` | Complete | Semantic exit codes (0-6) |
| Poka-yoke Guards | `ggen-core/src/poka_yoke/` | Complete | Timeout, validation, dry-run |
| Manifest Types | `ggen-core/src/manifest/types.rs` | Complete | GgenManifest, rules |
| CONSTRUCT Executor | `ggen-core/src/graph/construct.rs` | Complete | Sequential materialization |
| Code Graph | `ggen-core/src/codegen/code_graph.rs` | Complete | Code entity types |

### New Components (Implement)

| Component | Location | Priority | Dependencies |
|-----------|----------|----------|--------------|
| Sync CLI | `ggen-cli/src/cmds/sync.rs` | P1 | Pipeline, Manifest |
| SyncOptions | `ggen-core/src/codegen/mod.rs` | P1 | None |
| Command Removal | `ggen-cli/src/cmds/*.rs` | P1 | None |

## Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Breaking change for existing users | High | High | Clear documentation, version bump to v5.0.0 |
| Missing utility commands | Medium | Low | Commands can be added back incrementally |
| CONSTRUCT performance on large graphs | Low | Medium | Query caching already implemented |
| Template complexity | Low | Low | Existing template.rs handles SPARQL results |

## CLI Interface

```bash
ggen sync [OPTIONS]

OPTIONS:
  -m, --manifest <PATH>     Manifest path [default: ggen.toml]
  -o, --output-dir <PATH>   Override output directory
  -n, --dry-run             Preview changes without writing
  -v, --verbose             Verbose output
  -w, --watch               Watch for changes and regenerate
  -r, --rule <NAME>         Run specific rule(s) only
  -f, --force               Overwrite protected files
  -a, --audit               Generate audit.json
      --validate-only       Validate manifest only
      --format <FMT>        Output format: text, json [default: text]
```

## Exit Codes (Semantic)

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | Manifest validation error |
| 2 | Ontology load error |
| 3 | SPARQL query error |
| 4 | Template rendering error |
| 5 | File I/O error |
| 6 | Timeout exceeded |

## Success Metrics

Track against specification success criteria:

- SC-001: <5s sync for 10-50 entities
- SC-002: 100% determinism (hash comparison)
- SC-003: 60% annotation reduction via inference
- SC-004: 0 syntax errors in generated code
- SC-005: Audit trail enables verification
- SC-010: `ggen sync` is the ONLY command available

## Commands Summary

### v5.0 Command (THE ONLY COMMAND)

| Command | Description |
|---------|-------------|
| `ggen sync` | Unified code synchronization pipeline |

### Commands Removed in v5.0

| Module | Reason |
|--------|--------|
| generate | Replaced by sync |
| template | Replaced by sync |
| project | Add back in v5.1+ |
| graph | Add back in v5.1+ |
| ontology | Add back in v5.1+ |
| marketplace | Add back in v5.1+ |
| ai | Add back in v5.1+ |
| test | Add back in v5.1+ |
| utils | Add back in v5.1+ |
| ci | Add back in v5.1+ |
| workflow | Add back in v5.1+ |
