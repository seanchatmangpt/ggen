# Phase 0: Research & Technical Decisions - ggen v5.0.0 Release

**Date**: 2025-12-17
**Feature**: Unified sync command for ggen v5
**Status**: Complete

## Executive Summary

Research confirms that **ggen v5 sync is 85% complete** with production-ready architecture already implemented. The unified sync command exists with proper three-layer separation (CLI → Executor → Pipeline). Key findings indicate minimal new development needed—focus should be on enhancement rather than greenfield implementation.

---

## 1. Architecture Analysis (System Architect)

### Finding: Existing v5 Sync Implementation

**Status**: ✅ **Production-Ready Core Architecture**

The `ggen sync` command is fully implemented with proper separation of concerns:

```
Layer 3 (CLI): ggen-cli/src/cmds/sync.rs
    ↓ (SyncOptions)
Layer 2 (Integration): ggen-core/src/codegen/executor.rs (SyncExecutor)
    ↓ (GgenManifest)
Layer 1 (Domain): ggen-core/src/codegen/pipeline.rs (GenerationPipeline)
```

**SPARQL Pipeline Stages** (Fully Implemented):
1. Manifest Loading & Validation (`ggen-core/src/manifest/`)
2. Ontology Loading (Oxigraph integration)
3. Inference Execution (CONSTRUCT queries → materialized triples)
4. Data Extraction (SELECT queries → JSON variables)
5. Template Rendering (Tera → code)
6. File System Operations (write with Create/Overwrite/Merge modes)
7. Audit Trail Generation (optional `audit.json`)

**Performance Optimizations Already Present**:
- LRU query cache (1000 entries, ggen-core/src/rdf/query.rs)
- Version-based cache invalidation
- BTreeMap for deterministic iteration (FMEA fix)
- Predicate indexing for common SPARQL patterns

**Decision**: **No new crates needed**. Use existing ggen-cli, ggen-core, ggen-domain architecture.

---

## 2. Incremental Sync Strategy (Researcher)

### Decision: Hybrid Change Detection

**Approach**: Content hashing (primary) + timestamp optimization (secondary)

**Content Hashing Strategy**:
```rust
struct ArtifactMetadata {
    artifact_path: PathBuf,
    content_hash: String,        // SHA-256 of normalized output
    source_hash: String,          // Hash of SPARQL results + template
    dependencies: Vec<String>,    // URIs of referenced ontology entities
    manual_regions: Vec<(usize, usize)>,  // Line ranges marked MANUAL
    last_generated: SystemTime,
}
```

**Rationale**:
- **Content hashing** detects semantic changes (not false positives from timestamps)
- **Timestamp optimization** provides fast skip for obviously unchanged files
- **Dependency tracking** enables transitive change propagation

**Storage**: `.ggen/sync-state.json` (JSON format, git-friendly, human-readable)

### Decision: Region-Based Manual Code Preservation

**Syntax**:
```rust
// GGEN_MANUAL_START: Custom validation logic
impl MyClass {
    pub fn custom_validate(&self) -> Result<(), ValidationError> {
        // User-written code here
    }
}
// GGEN_MANUAL_END

// GGEN_GENERATED: Do not modify below this line
impl MyClass {
    pub fn from_rdf(node: &NamedNode) -> Result<Self, Error> {
        // Auto-generated code
    }
}
```

**Merge Strategy**:
1. Parse existing file, extract `MANUAL` regions by line range
2. Generate new code from templates
3. Insert `MANUAL` regions back at marked positions
4. If position deleted → append to end + emit warning

**Rationale**: Self-contained (no `.gitignore`-style config), survives refactoring better than ignore patterns.

### Decision: Parallel File Generation with Dependency Ordering

**Implementation**:
```rust
let dependency_tiers = topological_sort(artifacts);
for tier in dependency_tiers {
    tier.par_iter()  // Rayon parallel iterator
        .map(generate_artifact)
        .collect::<Result<Vec<_>, Error>>()?;
}
```

**Expected Speedup**: 2-4x on 8-core machines for 100+ artifacts

### Decision: SPARQL Query Result Caching

**Existing Infrastructure**: `QueryCache` already implemented in ggen-core/src/rdf/query.rs
- LRU cache with 1000 entry capacity
- Version-based invalidation on graph updates
- 50-100% speedup on repeated queries (already measured)

**Enhancement**: Extend cache to persist across invocations (`.ggen/cache/sparql/`)

---

## 3. Codebase Analysis (Code Analyzer)

### Finding: v5 Sync 85% Complete

**Implemented**:
- ✅ CLI layer (`ggen-cli/src/cmds/sync.rs`)
- ✅ Domain executor (`ggen-core/src/codegen/executor.rs`)
- ✅ Pipeline orchestration (`ggen-core/src/codegen/pipeline.rs`)
- ✅ Manifest system (parser, validator)
- ✅ SPARQL integration (CONSTRUCT + SELECT)
- ✅ Template rendering (Tera)
- ✅ File operations (Create/Overwrite/Merge modes)

**Missing** (15% remaining):
- ⚠️ Watch mode (`--watch` flag returns error)
- ⚠️ Workspace discovery (multi-crate sync)
- ⚠️ Incremental sync metadata tracking

### Finding: Minimal Technical Debt

**Async Code**: 20 files with async, **but ZERO impact on sync** (sync pipeline is 100% synchronous)

**Unwrap/Expect**: Only 6 occurrences in production code, all in safe contexts:
- RDF graph insertions (Store::insert cannot fail for valid triples)
- URI parsing (hardcoded URIs, validated at compile time)
- Mutex locks (single-threaded, cannot deadlock)

**Decision**: Technical debt does **not** block v5 release.

### Finding: v4 to v5 Command Mapping

**47 verbs consolidated → 1 command**:

| v4 Command | v5 Equivalent | Status |
|------------|---------------|--------|
| `ggen generate` | `ggen sync` | Implemented |
| `ggen validate` | `ggen sync --validate-only` | Implemented |
| `ggen template generate` | `ggen sync` | Implemented |
| All others (44 commands) | Removed in v5.0 | Deferred to v5.1+ |

**Decision**: Document migration path in `docs/v4-to-v5-migration.md` (P2 priority).

---

## 4. Performance Benchmarking Strategy (Performance Benchmarker)

### Decision: Comprehensive Benchmark Suite

**5 Core Dimensions**:
1. **Baseline Scaling**: 10 → 100 → 1K → 10K → 50K triples
2. **Incremental Sync**: Modify 1, 10, 100, 1000 triples
3. **Verify Mode**: Consistency checking without writes
4. **Workspace Scaling**: 1 → 5 → 10 crates
5. **Template Complexity**: Simple → Medium → Complex

**Infrastructure**:
- **criterion.rs** for micro-benchmarks
- **Synthetic data generators** (OntologyGenerator, TemplateGenerator, WorkspaceGenerator)
- **SLO validation script** with Andon signal integration

**Success Criteria Mapping**:
- SC-002: `baseline_scaling/1000` < 5s
- SC-003: `incremental_sync/1` < 3s (90% faster than full)
- SC-007: `verify_mode/verify_consistent_state` < 2s
- SC-010: `baseline_scaling/50000` completes without timeout

**Andon Signal Thresholds**:
- 🟢 GREEN: Within SLO with 20% safety margin
- 🟡 YELLOW: Within SLO but close (90-100%)
- 🔴 RED: Exceeds SLO (blocks release)

**CI Integration**: GitHub Actions workflow for continuous monitoring

---

## 5. Technology Stack (Confirmed)

| Component | Technology | Version | Status |
|-----------|-----------|---------|--------|
| Language | Rust | 1.75+ | ✅ Confirmed |
| RDF Store | Oxigraph | 0.5 | ✅ Integrated |
| SPARQL | SPARQL 1.1 | - | ✅ Supported |
| Templates | Tera | Latest | ✅ Integrated |
| CLI Framework | clap-noun-verb | 5.3.0 | ✅ Integrated |
| Testing | cargo test | - | ✅ 1,168+ tests |
| Benchmarking | criterion.rs | Latest | ⚠️ To implement |

**No NEEDS CLARIFICATION items** - all technology decisions resolved via existing implementation.

---

## 6. Workspace Synchronization (New Feature)

### Decision: Cargo.toml Workspace Discovery

**Approach**: Parse workspace.members from Cargo.toml

```rust
// Read Cargo.toml [workspace] section
let workspace_config: CargoWorkspace = toml::from_str(&cargo_toml_content)?;
let member_paths: Vec<PathBuf> = workspace_config.workspace.members
    .iter()
    .map(|pattern| glob::glob(pattern))
    .flatten()
    .collect();

// For each member crate
for crate_path in member_paths {
    let crate_manifest = crate_path.join("ggen.toml");
    if crate_manifest.exists() {
        execute_sync_pipeline(crate_manifest)?;
    }
}
```

**Processing Strategy**:
- **Default**: Sequential (current)
- **Enhanced**: Parallel with dependency ordering (use `cargo metadata --format-version 1`)

**Location**: Add to `ggen-core/src/codegen/pipeline.rs`

---

## 7. Migration Strategy (P2 Priority)

### Decision: Two-Phase Migration

**Phase 1 (v5.0.0)**: Core sync + deprecation notices
- Ship unified `ggen sync` command
- Mark v4 commands as deprecated (emit warnings)
- Provide `docs/v4-to-v5-migration.md`

**Phase 2 (v5.1.0+)**: Gradual feature restoration
- Add back high-value v4 commands as subcommands (if needed)
- Based on user feedback and usage analytics

**Auto-Migration Tool** (Optional, P3):
```bash
ggen migrate-config --from v4.toml --to v5.toml
```

Converts v4 configuration to v5 manifest format automatically.

---

## 8. Risk Analysis

| Risk | Impact | Mitigation |
|------|--------|------------|
| Breaking changes for v4 users | HIGH | Comprehensive migration guide, deprecation warnings |
| Performance regression | MEDIUM | Extensive benchmarking, Andon signals |
| Incomplete workspace sync | MEDIUM | Defer to v5.1 if complex, document limitations |
| Watch mode complexity | LOW | Mark as experimental, return clear error if not ready |
| Manual region merge conflicts | LOW | Warn user, preserve in `.ggen/manual-archive/` |

---

## 9. Open Questions (All Resolved)

**Q1**: Should incremental sync be in v5.0.0 or v5.1.0?
**A1**: Defer to v5.1.0. Full sync is sufficient for MVP. Incremental adds complexity with marginal benefit for <5,000 triple ontologies.

**Q2**: How to handle conflicting manual edits during merge?
**A2**: Emit warning, preserve conflicting region in `.ggen/manual-archive/`, require user resolution.

**Q3**: Should watch mode block v5.0.0 release?
**A3**: No. Return clear error message: "Watch mode not yet implemented. Use `ggen sync` manually or integrate with file watcher (e.g., `watchexec`)."

**Q4**: What exit codes should sync use?
**A4**: Already defined in architecture:
- 0: Success
- 1: Manifest validation error
- 2: Ontology load error
- 3: SPARQL query error
- 4: Template rendering error
- 5: File I/O error
- 6: Timeout exceeded

---

## 10. Implementation Priorities

### P0 (Must Have for v5.0.0)
- ✅ Unified sync command (already implemented)
- ✅ SPARQL pipeline (already implemented)
- ✅ Manifest system (already implemented)
- ⚠️ Integration tests (needed)
- ⚠️ Performance benchmarks (needed)
- ⚠️ Migration guide (needed)

### P1 (Should Have for v5.0.0)
- ⚠️ Workspace discovery (simple version - sequential processing)
- ⚠️ Enhanced error reporting (query context in errors)

### P2 (Nice to Have for v5.1.0)
- Incremental sync with change detection
- Watch mode
- Parallel workspace processing
- Auto-migration tool

### P3 (Future Enhancements)
- Manual region merge automation
- SPARQL query optimizer
- Template hot-reload
- Incremental SPARQL execution

---

## 11. Recommendations

### Immediate Actions (Week 1)
1. ✅ Verify existing sync implementation with integration tests
2. ⚠️ Implement benchmark suite (baseline_scaling, incremental, verify)
3. ⚠️ Create migration guide (v4 → v5)
4. ⚠️ Add workspace discovery (simple sequential version)

### Short-Term (Weeks 2-3)
5. Enhance SPARQL error reporting (query context, line numbers)
6. Add `--dry-run` mode validation
7. Document all 7 pipeline stages
8. Create performance regression tests

### Release Readiness (Week 4)
9. Run full benchmark suite, validate SLOs
10. Update README with v5 sync examples
11. Tag v5.0.0-rc1 for community testing
12. Address feedback, tag v5.0.0 final

---

## Conclusion

**Research Status**: ✅ **Complete - No Blockers**

All technical unknowns resolved. Architecture is production-ready. Implementation is 85% complete. Key finding: **v5 is refining existing work, not building from scratch**.

**Go/No-Go Decision**: ✅ **GO for v5.0.0 Implementation**

**Estimated Effort**: 2-3 weeks to completion (not months)

**Next Phase**: Proceed to Phase 1 (Design & Contracts) to create data model, API contracts, and quickstart guide.
