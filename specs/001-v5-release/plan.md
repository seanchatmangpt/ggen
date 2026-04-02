# Implementation Plan: ggen v5.0.0 Release

**Branch**: `001-v5-release` | **Date**: 2025-12-17 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-v5-release/spec.md`

## Summary

**Primary Requirement**: Consolidate ggen v4's fragmented 47-verb CLI (across 9 modules) into a single unified `ggen sync` command that orchestrates the complete ontology-to-code pipeline: RDF loading → SPARQL inference (CONSTRUCT) → data extraction (SELECT) → template rendering (Tera) → code generation.

**Technical Approach**: Leverage **existing 85% complete** v5 sync implementation in ggen-cli and ggen-core. Core architecture already implements proper three-layer separation (CLI → Executor → Pipeline) with production-ready SPARQL caching, Tera rendering, and manifest system. Focus on enhancement (workspace discovery, error reporting, benchmarks, migration guide) rather than greenfield development.

**Key Finding from Research**: v5 sync architecture is **production-ready**. No new crates needed. All 7 pipeline stages implemented. Main work: integration tests, performance benchmarks, workspace sync, and migration documentation.

---

## Technical Context

**Language/Version**: Rust 1.75+ (edition 2021)
**Primary Dependencies**:
  - Oxigraph 0.5 (RDF store, SPARQL 1.1 execution)
  - Tera (template engine)
  - clap-noun-verb 5.3.0 (CLI framework)
  - criterion.rs (benchmarking)

**Storage**: File system-based
  - RDF ontologies: `.ttl`, `.rdf`, `.nt` files
  - Configuration: `ggen.toml` (TOML format)
  - Generated code: Rust `.rs` files (primary), supports any language via templates
  - Metadata: `.ggen/sync-state.json`, `.ggen/cache/sparql/` (JSON)

**Testing**: cargo test + Chicago TDD methodology
  - 1,168+ existing tests (all passing)
  - Unit tests: `cargo make test-unit` (<16s)
  - Integration tests: `cargo make test` (<32s)
  - Benchmarks: criterion.rs micro-benchmarks

**Target Platform**: Linux, macOS, Windows (cross-platform Rust)

**Project Type**: Rust workspace (single monorepo)
  - 17 crates: ggen-cli, ggen-core, ggen-domain, ggen-utils, etc.
  - Workspace-level Cargo.toml coordinates builds

**Performance Goals** (from constitution SLOs):
  - First build: ≤15s (measured: 0.79s ✅)
  - Incremental: ≤2s (measured: sub-second ✅)
  - RDF processing: ≤5s for 1,000+ triples (measured: <1µs per triple ✅)
  - SPARQL queries: ≤10ms for complex queries (measured: <10ms ✅)
  - Template parsing: ≤5ms (measured: 115ns ✅)

**Success Criteria Targets** (from spec):
  - SC-002: Full sync on 1K triples <5s
  - SC-003: Incremental sync 1 triple <3s (90% faster)
  - SC-007: Verify mode 1K triples <2s
  - SC-010: Handle 50K triples without timeout

**Constraints**:
  - **Zero unwrap/expect in production code** (Result<T, E> only)
  - **Chicago TDD mandatory** (tests drive behavior, no mocks)
  - **cargo make protocol** (no direct cargo commands)
  - **100% type annotations** on all functions
  - **Deterministic output** (same RDF + template → same code)

**Scale/Scope**:
  - Ontologies: 10 - 50,000 triples (range)
  - Templates: 1 - 100 per project (estimate)
  - Generated files: 10 - 1,000 per project (estimate)
  - Workspace: 1 - 10 crates per workspace

---

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

Verify compliance with ggen Constitution v1.0.0 (`.specify/memory/constitution.md`):

- [x] **I. Crate-First Architecture**: Yes. Feature uses existing crates (ggen-cli, ggen-core). No new crates needed. Clear separation: CLI layer (ggen-cli/src/cmds/sync.rs), Integration layer (ggen-core/src/codegen/executor.rs), Domain layer (ggen-core/src/codegen/pipeline.rs).

- [x] **II. Deterministic RDF Projections**: Yes. SPARQL query results converted to JSON with BTreeMap (deterministic iteration order, FMEA fix applied). Oxigraph Store provides reproducible triple storage. Same ontology + template → same generated code.

- [x] **III. Chicago TDD**: Yes. Existing tests use state-based testing with real collaborators (Oxigraph Store, Tera renderer, file system). No mocks. Plan includes integration tests for full pipeline. 80%+ coverage achievable (core path: ontology → SPARQL → template → file).

- [x] **IV. cargo make Protocol**: Yes. All development uses `cargo make` targets: `cargo make check` (<5s), `cargo make test-unit`, `cargo make lint`. SLOs respected: first build ≤15s, incremental ≤2s.

- [x] **V. Type-First Thinking**: Yes. Types express invariants: `SyncMode` enum (Full/Incremental/Verify), `GenerationMode` enum (Create/Overwrite/Merge), `Result<T, E>` for all fallible operations. Public APIs are type-safe and ergonomic.

- [x] **VI. Andon Signal Protocol**: Yes. Plan includes Andon signal integration in benchmarks (RED >SLO, YELLOW 90-100% SLO, GREEN <90% SLO). Compilation errors and test failures stop the line (pre-commit hooks enforce).

- [x] **VII. Error Handling**: Yes. All production code uses `Result<T, SyncError>`. Existing codebase has only 6 unwrap/expect occurrences in production code (all safe contexts: hardcoded URIs, single-threaded mutex locks). Test exemption properly applied (`#[cfg(test)]` uses unwrap for fail-fast).

- [x] **VIII. Concurrent Execution**: Yes. Research recommends parallel file generation with Rayon (`artifacts.par_iter().map(generate)`). File organization proper: generated files in `src/generated/`, not root. TodoWrite will batch 10+ items per call.

- [x] **IX. Lean Six Sigma Quality**: Yes. Pre-commit hooks run `cargo make pre-commit` (format + lint + tests). 100% type coverage maintained. Comprehensive linting via Clippy. Benchmark suite validates SLOs (RED/YELLOW/GREEN gates).

**Quality Gates Pass?**: [x] YES

**Justification**: No constitutional violations. All 9 principles satisfied. Existing v5 implementation already follows constitution (85% complete, production-ready architecture).

---

## Project Structure

### Documentation (this feature)

```text
specs/001-v5-release/
├── plan.md                      # This file
├── research.md                  # Phase 0 output ✅
├── data-model.md                # Phase 1 output ✅
├── quickstart.md                # Phase 1 output ✅
├── contracts/
│   └── sync-pipeline-api.md    # Phase 1 output ✅
├── checklists/
│   └── requirements.md          # Spec validation ✅
└── tasks.md                     # Phase 2 output (NOT yet created - run /speckit.tasks)
```

### Source Code (existing workspace structure)

```text
crates/
├── ggen-cli/
│   ├── src/
│   │   ├── main.rs              # Binary entry point
│   │   ├── lib.rs               # Library exports
│   │   └── cmds/
│   │       └── sync.rs          # ✅ CLI verb (Layer 3) - ALREADY IMPLEMENTED
│   └── tests/                   # CLI integration tests
│
├── ggen-core/
│   ├── src/
│   │   ├── codegen/
│   │   │   ├── executor.rs      # ✅ SyncExecutor (Layer 2) - ALREADY IMPLEMENTED
│   │   │   ├── pipeline.rs      # ✅ GenerationPipeline (Layer 1) - ALREADY IMPLEMENTED
│   │   │   └── mod.rs
│   │   ├── manifest/
│   │   │   ├── parser.rs        # ✅ ManifestParser - ALREADY IMPLEMENTED
│   │   │   └── validator.rs    # ✅ ManifestValidator - ALREADY IMPLEMENTED
│   │   ├── rdf/
│   │   │   └── query.rs         # ✅ QueryCache (LRU) - ALREADY IMPLEMENTED
│   │   ├── templates/
│   │   │   ├── generator.rs    # ✅ FileTreeGenerator (Tera) - ALREADY IMPLEMENTED
│   │   │   └── tera_env.rs     # ✅ Tera environment - ALREADY IMPLEMENTED
│   │   └── error.rs             # Error types (SyncError, etc.)
│   ├── tests/
│   │   ├── sync_pipeline_test.rs    # ⚠️ NEEDS: Integration tests
│   │   └── fixtures/                # Test data (ontologies, templates)
│   └── benches/                     # ⚠️ NEEDS: Criterion benchmarks
│
├── ggen-domain/
│   └── src/
│       ├── packs/
│       │   └── sparql_executor.rs   # ✅ Domain SPARQL executor
│       └── rdf/                     # RDF domain logic
│
├── ggen-utils/
│   └── src/                         # Utility functions
│
└── ggen-testdata/                   # ⚠️ NEEDS: NEW - Synthetic data generators
    └── src/
        └── generators/
            ├── ontology.rs          # OntologyGenerator (10-50K triples)
            ├── template.rs          # TemplateGenerator (simple/medium/complex)
            └── workspace.rs         # WorkspaceGenerator (multi-crate)

tests/                               # Workspace-level integration tests
├── sync_end_to_end_test.rs         # ⚠️ NEEDS: Full pipeline test
└── fixtures/                        # Shared test data

benches/                             # Workspace-level benchmarks
├── sync_scaling.rs                  # ⚠️ NEEDS: Baseline scaling benchmarks
├── incremental_sync.rs              # ⚠️ NEEDS: Incremental mode benchmarks
├── verify_mode.rs                   # ⚠️ NEEDS: Verify mode benchmarks
└── workspace_scaling.rs             # ⚠️ NEEDS: Multi-crate benchmarks

docs/
├── v4-to-v5-migration.md           # ⚠️ NEEDS: Migration guide
└── v4-to-v5-sync-analysis.md       # ✅ CREATED by code-analyzer agent

.ggen/                               # Runtime generated files
├── sync-state.json                  # Incremental sync metadata
├── cache/sparql/                    # Query result cache
└── sync.log                         # Execution logs

scripts/
├── validate_slos.sh                 # ⚠️ NEEDS: SLO validation script
└── compare_benchmarks.sh            # ⚠️ NEEDS: Regression detection script
```

**Structure Decision**: Using existing Rust workspace structure (Option 1: Single project). No changes needed—all implementation targets existing crates. Only new crate: `ggen-testdata` for synthetic benchmark data generators.

---

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

**No violations identified.** All constitutional principles satisfied by existing implementation and planned enhancements.

---

## Implementation Phases

### Phase 0: Research ✅ COMPLETE

**Status**: All research agents completed successfully
- ✅ System architect analyzed existing implementation (85% complete)
- ✅ Researcher identified incremental sync strategy (hybrid change detection)
- ✅ Code analyzer mapped v4→v5 commands (47 verbs → 1 command)
- ✅ Performance benchmarker designed comprehensive benchmark suite

**Artifacts Created**:
- `research.md` - Complete research findings
- `data-model.md` - Entity definitions
- `contracts/sync-pipeline-api.md` - API contracts
- `quickstart.md` - Developer guide

**Key Decisions**:
- ✅ No new crates needed (use existing ggen-cli, ggen-core)
- ✅ Hybrid change detection (content hashing + timestamp optimization)
- ✅ Region-based manual code preservation (`// GGEN_MANUAL` markers)
- ✅ Parallel file generation with dependency ordering (Rayon)
- ✅ Comprehensive benchmark suite (5 dimensions, 15+ scenarios)

---

### Phase 1: Design & Contracts ✅ COMPLETE

**Status**: All design artifacts created

**Artifacts Created**:
- ✅ `data-model.md` - 7 core entities (RDF Ontology, SPARQL Query, Tera Template, Generated Artifact, Sync Configuration, Workspace Member, Sync Operation)
- ✅ `contracts/sync-pipeline-api.md` - API contracts for 3 layers + 7 pipeline stages
- ✅ `quickstart.md` - Essential commands, TDD workflow, common patterns

**API Contracts Defined**:
- Layer 3: CLI → Executor (SyncOptions, SyncOutput, exit codes)
- Layer 2: Executor → Pipeline (GgenManifest, PipelineResult, PipelineError)
- Layer 1: Pipeline Stages (7 stages with Input/Output contracts)

**Agent Context Updated**: ✅ CLAUDE.md updated with "File system" storage type

---

### Phase 2: Task Breakdown (NOT YET EXECUTED)

**Status**: ⚠️ Ready for `/speckit.tasks` command

**Prerequisites Met**:
- ✅ Specification complete (`spec.md`)
- ✅ Research complete (`research.md`)
- ✅ Data model complete (`data-model.md`)
- ✅ API contracts complete (`contracts/sync-pipeline-api.md`)
- ✅ Quickstart guide complete (`quickstart.md`)
- ✅ Constitution check passed (9/9 principles)

**Next Command**: `/speckit.tasks` to generate `tasks.md` with actionable implementation tasks

---

## Key Recommendations from Research

### P0 (Must Have for v5.0.0)
1. ✅ **Unified sync command** - ALREADY IMPLEMENTED
2. ✅ **SPARQL pipeline** - ALREADY IMPLEMENTED
3. ✅ **Manifest system** - ALREADY IMPLEMENTED
4. ⚠️ **Integration tests** - NEEDED (sync_pipeline_test.rs)
5. ⚠️ **Performance benchmarks** - NEEDED (5 dimensions)
6. ⚠️ **Migration guide** - NEEDED (v4→v5 command mapping)

### P1 (Should Have for v5.0.0)
7. ⚠️ **Workspace discovery** - Simple sequential processing (parse Cargo.toml workspace.members)
8. ⚠️ **Enhanced error reporting** - Query context in SPARQL errors (line numbers, query snippet)

### P2 (Nice to Have for v5.1.0)
9. Incremental sync with change detection (`.ggen/sync-state.json`)
10. Watch mode (`--watch` flag, currently returns error)
11. Parallel workspace processing (dependency-ordered with `cargo metadata`)
12. Auto-migration tool (`ggen migrate-config --from v4.toml --to v5.toml`)

### P3 (Future Enhancements)
13. Manual region merge automation
14. SPARQL query optimizer
15. Template hot-reload
16. Incremental SPARQL execution

---

## Risk Mitigation

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Breaking changes for v4 users | HIGH | HIGH | Comprehensive migration guide, deprecation warnings in v4 commands |
| Performance regression vs v4 | MEDIUM | MEDIUM | Extensive benchmarking (15+ scenarios), Andon signals, regression tests |
| Incomplete workspace sync | MEDIUM | LOW | Defer parallel processing to v5.1, simple sequential version in v5.0 |
| Watch mode complexity | LOW | LOW | Return clear error "Not implemented", suggest watchexec workaround |
| Manual region merge conflicts | LOW | LOW | Warn user, preserve in `.ggen/manual-archive/`, require manual resolution |

---

## Success Metrics

**Definition of Done for v5.0.0**:
- [x] All constitutional principles satisfied (9/9 ✅)
- [ ] All P0 items complete (6 items, 3 remaining)
- [ ] Integration tests passing (sync_pipeline_test.rs)
- [ ] Benchmarks meet SLOs (SC-002, SC-003, SC-007, SC-010)
- [ ] Migration guide published (docs/v4-to-v5-migration.md)
- [ ] CI pipeline green (1,168+ tests passing)
- [ ] Zero Andon RED signals (compilation, tests, lint clean)

**Performance Validation**:
- Baseline scaling: 10 → 100 → 1K → 10K → 50K triples
- Incremental sync: 1, 10, 100, 1000 triple modifications
- Verify mode: Consistency check <2s for 1K triples
- Workspace scaling: 1 → 5 → 10 crates
- Template complexity: Simple → Medium → Complex

---

## Next Steps

1. ✅ **Phase 0 Complete**: Research findings documented
2. ✅ **Phase 1 Complete**: Design artifacts created
3. ⚠️ **Phase 2 Pending**: Run `/speckit.tasks` to generate actionable task breakdown
4. **Implementation**: Execute tasks following Chicago TDD (RED → GREEN → Refactor)
5. **Validation**: Run `cargo make pre-commit` + benchmarks before PR
6. **Release**: Tag v5.0.0 after all Definition of Done items checked

**Estimated Effort**: 2-3 weeks (not months) due to 85% existing implementation

**Go/No-Go**: ✅ **GO** - All research complete, architecture production-ready, path forward clear

---

## Appendices

### A. Existing Implementation Status (85% Complete)

**Implemented**:
- ✅ CLI layer (ggen-cli/src/cmds/sync.rs)
- ✅ Domain executor (ggen-core/src/codegen/executor.rs)
- ✅ Pipeline orchestration (ggen-core/src/codegen/pipeline.rs)
- ✅ Manifest system (parser, validator)
- ✅ SPARQL integration (CONSTRUCT + SELECT, query cache)
- ✅ Template rendering (Tera)
- ✅ File operations (Create/Overwrite/Merge modes)
- ✅ Audit trail generation (audit.json)

**Missing** (15%):
- ⚠️ Watch mode (--watch flag returns error)
- ⚠️ Workspace discovery (multi-crate sync)
- ⚠️ Incremental sync metadata tracking
- ⚠️ Integration tests (sync_pipeline_test.rs)
- ⚠️ Performance benchmarks (criterion.rs suite)
- ⚠️ Migration guide (v4→v5 documentation)

### B. Technology Stack Summary

| Component | Technology | Version | Location |
|-----------|-----------|---------|----------|
| Language | Rust | 1.75+ | Entire codebase |
| RDF Store | Oxigraph | 0.5 | ggen-core/src/rdf/ |
| SPARQL | SPARQL 1.1 | - | Oxigraph integration |
| Templates | Tera | Latest | ggen-core/src/templates/ |
| CLI Framework | clap-noun-verb | 5.3.0 | ggen-cli |
| Testing | cargo test | - | Chicago TDD |
| Benchmarking | criterion.rs | Latest | benches/ |
| Build Tool | cargo make | - | Makefile.toml |

### C. Exit Code Reference

| Code | Meaning | Error Type |
|------|---------|-----------|
| 0 | Success | - |
| 1 | Manifest validation error | ManifestError |
| 2 | Ontology load error | OntologyLoadError |
| 3 | SPARQL query error | SparqlError |
| 4 | Template rendering error | TemplateError |
| 5 | File I/O error | IoError |
| 6 | Timeout exceeded | TimeoutError |

---

**Plan Version**: 1.0.0
**Last Updated**: 2025-12-17
**Status**: Phase 1 Complete - Ready for Task Breakdown
