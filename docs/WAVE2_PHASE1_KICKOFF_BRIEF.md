# Wave 2 Phase 1 Kickoff Brief
## ggen v26.5.28 Critical Path: P0 Blockers

**Date:** 2026-05-29  
**Status:** READY FOR IMPLEMENTATION  
**Orchestration:** Phase 5 Wave 2 Phase 1 (P0 Blocker Elimination)  
**Branch:** `feat/autonomic-actuation` → `main` (merged)  
**Success Criteria:** All 4 P0 blockers fixed, tested with Chicago TDD, OTEL spans verified

---

## Executive Summary

Phase 1 is a **29.5-hour critical path** elimination of the 4 P0 blockers that ship wrong behavior or block other work. These are not nice-to-have features — they are correctness violations that affect the entire codebase.

**What Phase 1 Accomplishes:**
- Fixes SHACL validation (currently a no-op, needs real shape enforcement)
- Fixes pipeline architecture divergence (wrong stages detected, receipts tracked)
- Eliminates namespace conflicts (SPARQL silent data loss prevention)
- Standardizes CLI error handling (three competing Result types consolidated)

**Phase 1 Unblocks:**
- Phase 2 P1 work (search, list, deps, gates, receipt — 26h)
- Capability marketplace features
- Quality gate validation system
- Receipt provenance and audit chain

**Outcome:** After Phase 1, the system ships correct behavior for quality gates, search, and error handling. All 4 P0 blockers converted to passing tests and verified OTEL spans.

---

## P0 Blockers to Fix

All 4 P0 blockers derived from audit dashboard (2026-04-01), verified in memory (2026-05-27).

### P0-01: SHACL Validation (14 hours) — CRITICAL PATH

**What:** SHACL shape loader returns empty shape sets; validation always passes (no-op).

**Where:**
- Implementation: `crates/ggen-core/src/validation/shacl.rs`
- Call site: `Pipeline::run()` in `crates/ggen-core/src/codegen/pipeline.rs`

**Impact:**
- Blocks quality gates (Gate 2: ontology-validation-success)
- Blocks marketplace validation (prevents bad packages from being installed)
- All ontology shapes silently ignored — violations undetected

**What Needs to Happen:**
1. Implement `ShapeLoader::load()` to actually read SHACL shape definitions from `.specify/shapes/`
2. Build shape graph from TTL files (use Oxigraph for graph construction)
3. Implement shape validation against artifact RDF (node shapes, property constraints, cardinality)
4. Capture violations in `ShaclViolationSet` (currently empty)
5. Emit `quality_gate.validate` span with `gate.name="shacl"` and `gate.result=pass/fail`
6. Block pipeline if violations detected (in strict mode)

**Test Requirements:**
- Chicago TDD: real shape files, real RDF artifacts, real shape violations
- Negative case: inject invalid RDF, verify detection
- OTEL span: `quality_gate.validate` with `gate.result=fail` when shapes violated
- Coverage: ≥80% of shape validation paths

**Estimated Effort:** 14 hours
- Shape loader + graph construction: 4h
- Shape validation engine: 6h
- Test harness + OTEL spans: 3h
- Integration: 1h

### P0-02: Pipeline Architecture (7 hours) — MEDIUM PRIORITY

**What:** Two pipeline implementations exist (`GenerationPipeline` vs `StagedPipeline`); wrong one used at runtime.

**Where:**
- Implementation: `crates/ggen-core/src/codegen/pipeline.rs`
- Issue: Pipeline divergence between declared stages (μ₁–μ₅) and actual execution

**Impact:**
- Blocks receipt provenance (stages not correctly recorded)
- Blocks epoch verification (phase boundaries undefined)
- Blocks staged governance (state transitions not tracked)
- Generated artifacts lack complete stage history

**What Needs to Happen:**
1. Audit which pipeline is actually used in production (`ggen sync` command)
2. If wrong pipeline: switch to correct one (likely `StagedPipeline` with μ₁–μ₅)
3. If both needed: consolidate into single canonical pipeline with pluggable stages
4. Record stage entry/exit in receipt with timestamps
5. Emit `pipeline.*` spans: `pipeline.load`, `pipeline.extract`, `pipeline.generate`, `pipeline.validate`, `pipeline.emit`
6. Each span includes `pipeline.stage` and `pipeline.duration_ms`

**Test Requirements:**
- Chicago TDD: real ontology→code generation with real input
- Verify all 5 stages execute in order
- OTEL spans: all pipeline stages present in trace
- Negative case: skip a stage, verify detection
- Coverage: ≥80% of pipeline paths

**Estimated Effort:** 7 hours
- Pipeline audit: 1h
- Consolidation (if needed): 3h
- OTEL instrumentation: 2h
- Tests + integration: 1h

### P0-03: Namespace Conflicts (5 hours) — HIGHEST PRIORITY (Silent Data Loss)

**What:** Three competing URIs for the same semantic concept; SPARQL queries return empty silently.

**Where:**
- Implementation: `crates/ggen-marketplace/src/ontology.rs`
- Issue: Namespace bindings inconsistent across ontology layers

**Impact:**
- MOST INSIDIOUS: Silent data loss (queries return 0 results instead of error)
- Blocks package search (finds 0 packages)
- Blocks marketplace install validation
- Blocks dependency resolution
- Breaks SPARQL consistency guarantees

**What Needs to Happening:**
1. Identify the three competing URIs (likely in prefixes, graph construction, or query headers)
2. Establish canonical namespace mapping (one URI per concept)
3. Add namespace validation step in ontology loader
4. Emit `sparql.query` span with namespace context
5. Fail hard if ambiguous namespace detected (not silent loss)
6. Update all SPARQL queries to use canonical namespaces

**Test Requirements:**
- Chicago TDD: real SPARQL queries with namespace bindings
- Test each of the 3 conflicting URIs (verify only canonical works)
- OTEL span: `sparql.query` with `namespace.count` and `sparql.result_count`
- Negative case: query with wrong namespace, verify hard failure (not silent 0)
- Coverage: ≥80% of namespace paths

**Estimated Effort:** 5 hours
- Namespace audit: 1.5h
- Canonicalization + validation: 2h
- OTEL instrumentation: 1h
- Tests: 0.5h

### P0-04: Error Type Chaos (3.5 hours) — LOW PRIORITY (Ergonomics)

**What:** Three different Result types in CLI prelude; inconsistent error conversion.

**Where:**
- Implementation: `crates/ggen-cli/src/prelude.rs`
- Issue: Error enums not uniformly convertible; error context lost at boundaries

**Impact:**
- CLI error messages incomplete (stack trace context missing)
- Hard to debug failures (root cause obscured)
- Error handling unpredictable across command boundaries

**What Needs to Happening:**
1. Identify the 3 Result types in prelude
2. Create single unified error type (newtype over anyhow::Error or custom enum)
3. Implement lossless conversion (`From<T>` for each source error type)
4. Emit `error.*` span on failure with error code and message
5. Use `tracing::error!` macros with context

**Test Requirements:**
- Chicago TDD: real CLI command execution with error conditions
- Test each error type conversion path
- OTEL span: `error.*` with `error.type`, `error.message`, `error.code`
- Negative case: error at each stage (load, validate, emit), verify context preserved
- Coverage: ≥80% of error paths

**Estimated Effort:** 3.5 hours
- Error type audit: 0.5h
- Unification + conversions: 1.5h
- OTEL instrumentation: 0.5h
- Tests: 1h

---

## Test Harness References

All tests follow **Chicago TDD** (no mocks, no test doubles). Reference: [TESTING.md](./TESTING.md)

Each P0 blocker has a corresponding test file:

| Blocker | Test File | Coverage Target | OTEL Spans |
|---------|-----------|-----------------|-----------|
| **P0-01 SHACL** | `crates/ggen-core/tests/shacl_validation_test.rs` | ≥80% | `quality_gate.validate`, `quality_gate.pass_fail` |
| **P0-02 Pipeline** | `crates/ggen-core/tests/pipeline_architecture_test.rs` | ≥80% | `pipeline.load`, `pipeline.extract`, `pipeline.generate`, `pipeline.validate`, `pipeline.emit` |
| **P0-03 Namespace** | `crates/ggen-marketplace/tests/namespace_conflicts_test.rs` | ≥80% | `sparql.query`, `sparql.bindings` |
| **P0-04 Error** | `crates/ggen-cli/tests/error_handling_test.rs` | ≥80% | `error.*`, `error.conversion` |

### Test Harness Structure (AAA Pattern)

Each test follows **Arrange → Act → Assert** with real collaborators:

```rust
#[tokio::test]
async fn test_shacl_validation_detects_violations() {
    // Arrange: Load real SHACL shapes + real RDF artifact
    let shapes = ShapeLoader::load("tests/fixtures/shapes.ttl").await.unwrap();
    let artifact = load_rdf_artifact("tests/fixtures/invalid-artifact.rdf").await.unwrap();
    
    // Act: Run real validation
    let result = validate_artifact(&artifact, &shapes).await;
    
    // Assert: Violations detected, not silently passed
    assert!(result.is_err());
    assert_eq!(result.violations.len(), 2);
    
    // OTEL verification: span exists with failure
    let spans = capture_otel_spans()?;
    assert_contains(&spans, "quality_gate.validate");
    assert_contains(&spans, r#""gate.result":"fail""#);
}
```

### Negative Path Testing (Sabotage Tests)

Each blocker has sabotage tests that inject impossible conditions:

| Blocker | Sabotage | Expected |
|---------|----------|----------|
| **SHACL** | Violate cardinality constraint (2 values when max=1) | `quality_gate.validate` span with `gate.result=fail` |
| **Pipeline** | Delete intermediate stage output | Exit non-zero, receipt incomplete |
| **Namespace** | Query with wrong URI prefix | Hard error, not silent 0 results |
| **Error** | Error at each stage (load, extract, generate, validate) | All errors propagate with context intact |

---

## Success Criteria

Phase 1 is complete when ALL of the following are true:

### Functional Correctness
- [ ] P0-01: SHACL validation executes (not no-op), violations detected
- [ ] P0-02: Pipeline uses canonical μ₁–μ₅ stages, all stages recorded in receipt
- [ ] P0-03: Namespace canonicalized, SPARQL queries return results or hard error
- [ ] P0-04: CLI errors propagate with context, lossless conversion

### Test Coverage
- [ ] All 4 P0 blockers have Chicago TDD test files (≥80% coverage each)
- [ ] All tests pass: `cargo make test -- p0`
- [ ] No ignored tests, no `todo!()` in tests, no mocks/test doubles

### OTEL Validation
- [ ] P0-01: `quality_gate.validate` spans with `gate.result` attribute
- [ ] P0-02: `pipeline.*` spans for all 5 stages with `pipeline.stage` and `pipeline.duration_ms`
- [ ] P0-03: `sparql.query` spans with namespace context
- [ ] P0-04: `error.*` spans on failure with `error.type`, `error.message`, `error.code`
- [ ] All spans captured and verified: `RUST_LOG=trace cargo make test -- p0 2>&1 | grep -E "(quality_gate|pipeline|sparql|error)"`

### Build Quality
- [ ] `cargo make check` passes (no compiler errors)
- [ ] `cargo make lint` passes (no clippy warnings)
- [ ] All 4 P0 fixes in committed code (no dangling branches)

---

## Implementation Roadmap

**Timeline:** 29.5 hours critical path (5 business days at 6h/day)

```
Day 1 (6h):         P0-01 SHACL (4h) + P0-03 Namespace (2h)
Day 2 (6h):         P0-01 SHACL tests (3h) + P0-03 tests (2h) + P0-02 Pipeline (1h)
Day 3 (6h):         P0-02 Pipeline (4h) + P0-02 tests (2h)
Day 4 (6h):         P0-02 tests (2h) + P0-04 Error (3h) + P0-04 tests (1h)
Day 5 (5.5h):       Buffer + OTEL validation (2h) + Commit/Merge (0.5h)

Total Critical Path: 29.5 hours
Parallel Work (if agents available):
  - P0-03 and P0-01 can run in parallel (independent codebases)
  - Tests can run while implementation continues on next blocker
```

### Phase 1 → Phase 2 Transition

After Phase 1 completes:

**Phase 2 (Days 6–10):** P1 High-Priority (26 hours)
- Search + list (4h)
- Dependency resolution (3h)  
- Quality gates 5-11 (8h)
- Receipt management v2 (6h)
- Watch mode / monitor (5h)

**Phase 3 (Days 11–15):** Dead Code Deletion + Integration (25 hours)
- Phase 1 deletions (~5,726 lines) — 6h
- Phase 2 deletions (~2,530 lines) — 4h
- Phase 3 deletions (~600 lines) — 2h
- Full test suite + integration — 8h
- Publish validation — 5h

---

## Agent Assignments

**Placeholder for implementation phase.** When Phase 1 launches:

| Agent Role | Tasks | Assignment |
|-----------|-------|------------|
| **P0-01 SHACL Engineer** | Implement ShapeLoader + validation engine + tests | `(TBD)` |
| **P0-03 Namespace Engineer** | Audit namespaces + canonicalize + SPARQL fixes | `(TBD)` |
| **P0-02 Pipeline Engineer** | Audit pipelines + consolidate + receipt tracking | `(TBD)` |
| **P0-04 Error Engineer** | Unify error types + conversions + OTEL | `(TBD)` |
| **Test Harness Lead** | Coordinate test files, ensure Chicago TDD, OTEL verification | `(TBD)` |
| **Integration Lead** | Merge + validate full test suite + commit signoff | `(TBD)` |

**Parallelization Strategy:**
- P0-01 and P0-03 run in parallel (different crates, no shared state)
- P0-02 and P0-04 sequential (OTEL depends on pipeline clarity)
- Tests run in parallel with next blocker implementation
- Integration lead monitors build gates continuously

---

## Next Steps

### For Implementation Agents

1. **Read This Document** — Understand 4 P0 blockers, success criteria, OTEL requirements
2. **Read Test Harness References** — [TESTING.md](./TESTING.md), [Chicago TDD Rules](./../.claude/rules/rust/testing.md)
3. **Read Audit Dashboard** — [AUDIT_DASHBOARD.md](./crate-audits/AUDIT_DASHBOARD.md) for context
4. **Claim Blocker** — "I will implement P0-01 SHACL" (prevents duplicate work)
5. **Create Test File** — Write failing test first (RED), then implement (GREEN)
6. **Verify OTEL** — Run with `RUST_LOG=trace` and capture spans
7. **Commit with Evidence** — Link OTEL output + test results in commit message

### For Human Orchestrator

1. **Launch Agents** — Assign 4-6 agents to Phase 1 blockers
2. **Monitor Build Gates** — Watch for compiler errors, test failures (Andon protocol)
3. **Verify OTEL Spans** — Confirm spans exist after each blocker completion
4. **Merge Daily** — Pull Phase 1 work to main branch each day
5. **Transition to Phase 2** — Plan P1 work (search, list, deps) in parallel

---

## Key References

| Document | Purpose |
|----------|---------|
| [Audit Dashboard](./crate-audits/AUDIT_DASHBOARD.md) | Full context on all 54 stubs, 4 P0 blockers, dead code |
| [TESTING.md](./TESTING.md) | Chicago TDD principles, real collaborators, AAA pattern |
| [Testing Rules](./../.claude/rules/rust/testing.md) | Forbidden London TDD patterns, required coverage |
| [OTEL Validation Rules](./../.claude/rules/otel-validation.md) | Span capture, attribute verification |
| [Andon Protocol](./../.claude/rules/andon/signals.md) | Stop the line on compiler errors, test failures |
| [Memory Index](./../.claude/projects/-Users-sac-ggen/memory/MEMORY.md) | Phase 5 Wave 1 discovery summary, quick reference |
| [Wave Orchestration](./../wave-orchestration.md) | Broader context (v26.5.19 release goals) |

---

## Metrics & Health Dashboard

**Tracked metrics for Phase 1 completion:**

| Metric | Target | Current | Status |
|--------|--------|---------|--------|
| **Compiler Errors** | 0 | TBD | After P0-01 complete |
| **Test Pass Rate** | 100% | TBD | After all tests complete |
| **Code Coverage (P0 fixes)** | ≥80% | TBD | After test merge |
| **OTEL Spans Present** | 100% | TBD | After OTEL verification |
| **Build Time** | <15s (first), <2s (incr) | TBD | Continuous monitoring |
| **Hours Spent** | ≤29.5 | TBD | Daily tracking |

### Definition of Done (DoD)

Phase 1 is done when:
1. All 4 P0 blockers have passing tests
2. OTEL spans verified for each blocker
3. `cargo make check` + `cargo make test` + `cargo make lint` all pass
4. Commit message links OTEL evidence
5. No open TODOs or ignored tests
6. Code reviewed and merged to `main`

---

## Support & Escalation

**If stuck:**
1. Read Audit Dashboard for detailed context on your blocker
2. Check TESTING.md for Chicago TDD patterns
3. Verify OTEL spans capture: `RUST_LOG=trace cargo test -- <blocker_name>`
4. Ask human orchestrator if:
   - Blocker definition unclear (message with quote from this doc)
   - OTEL span not appearing (show rust log output)
   - Test harness not working (show test failure)

**Andon Signal:** Stop immediately if you see:
- Compiler error (`error[E...]`) — fix before proceeding
- Test failure (`FAILED`) — investigate root cause
- Clippy warning — address before commit

---

**Document Version:** 1.0  
**Created:** 2026-05-29  
**Status:** READY FOR PHASE 1 IMPLEMENTATION  
**Next Review:** After P0-01 completion (approximately 2026-05-31)
