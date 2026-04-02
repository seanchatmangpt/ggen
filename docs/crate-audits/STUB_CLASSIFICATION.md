# Stub Classification — Execution-Trace Verified

**Date:** 2026-04-01
**Method:** Four execution-trace agents traced `ggen sync`, `ggen marketplace install`, CLI reachability, and library API call chains. Every stub was classified by whether it sits on a reachable execution path.

---

## Summary

| Category | Count | Est. Lines |
|----------|:-----:|:----------:|
| MUST IMPLEMENT (on real paths, users hit) | 13 | — |
| CAN DELETE (not on any execution path) | 38 | ~8,900 |
| PARTIALLY IMPLEMENTED (exists but critical logic unused) | 2 | — |
| BY DESIGN (intentional behavior) | 1 | — |
| **Total classified** | **54** | |

---

## MUST IMPLEMENT — On Real Execution Paths

Users invoke these commands/features and get incorrect or silent results.

### P0 — Blocks or silently corrupts

| # | Stub | Location | Reachable From | Impact |
|---|------|----------|---------------|--------|
| 1 | `check_conflicts()` | ggen-marketplace/install.rs:234 | Every `ggen marketplace install` | Always Ok — missing semver conflict detection. Two packs with same major version can coexist silently |
| 2 | `get_marketplace_public_key()` | ggen-marketplace/install.rs | Every `ggen marketplace install` | Blocks install without `MARKETPLACE_PUBLIC_KEY` env var. No config file fallback |
| 3 | Ontology namespace conflict | ggen-marketplace/ontology.rs:14 vs rdf/ontology.rs:23 | Every SPARQL query in marketplace | Three competing URIs cause silent empty results. `https://ggen.io/marketplace/` vs `http://ggen.dev/ontology#` vs `http://ggen.dev/marketplace#` |
| 4 | Prelude triple-Result | ggen-cli/prelude.rs | Every CLI command | Three `Result` types in scope; commands must pick correctly |

### P1 — Feature doesn't work when invoked

| # | Stub | Location | Reachable From | Impact |
|---|------|----------|---------------|--------|
| 5 | `ShapeLoader::load()` | ggen-core/validation/shacl.rs:132 | v6/pipeline.rs:198 (NormalizationPass) | Returns empty ShaclShapeSet — SHACL validation is a complete no-op |
| 6 | `SparqlValidator::validate()` | ggen-core/validation/validator.rs:113 | v6/pipeline.rs:225 (NormalizationPass) | Runs SPARQL query but discards all violations — always returns pass |
| 7 | `ThreeWayMerger` FailOnConflict | ggen-core/merge/mod.rs:398 | ggen-domain/template/regenerate.rs:32 | `has_conflicts()` always false — merge conflicts silently accepted |
| 8 | `RegionAwareMerger::merge_with_regions` | ggen-core/merge/mod.rs:491 | ggen-domain/template/regenerate.rs:32 | Naive line-by-line comparison; no proper diff3 algorithm |
| 9 | `Attestation::verify` | ggen-core/cleanroom/attestation.rs:302 | cleanroom attestation chain | Always returns Ok(true) — no actual crypto verification |
| 10 | `construct create` / `construct validate` | ggen-cli/cmds/construct.rs:286,327 | User-invocable CLI | Returns "not_implemented". Blocked on ggen_ai::llm_construct module |
| 11 | `ggen watch yawl` | ggen-cli/cmds/yawl.rs:268 | User-invocable CLI | Error: "Watch mode not yet fully implemented" |

> **NOTE:** A false completion report (`docs/YAWL_WATCH_MODE_REPORT.md`) claimed this stub was implemented on 2026-04-01. This was **incorrect** — the report was deleted after verification. The stub remains unimplemented. See `docs/YAWL_WATCH_MODE_IMPLEMENTATION_PLAN.md` for the real implementation roadmap.
| 12 | `ggen start-server mcp --background` | ggen-cli/cmds/mcp.rs:820 | User-invocable CLI | "Background mode not yet implemented" |
| 13 | `PackInstaller integrity` | ggen-domain/packs/installer.rs:215 | Pack install path | SHA256 digest always None — no integrity verification |

---

## CAN DELETE — Not On Any Execution Path

These stubs, modules, and dead code have zero production callers. Deleting them improves signal-to-noise without losing functionality.

### Lowest Risk — Dead modules with zero consumers

| # | Item | Location | Est. Lines | Why Safe |
|---|------|----------|:----------:|----------|
| 1 | AHI subsystem (8 modules) | ggen-domain/ahi_*.rs, capability_system.rs, proof_types.rs, action_types.rs, temporal_fabric.rs, etc. | ~4,200 | Zero callers in any workspace crate |
| 2 | `ggen-testing` crate | crates/ggen-testing/ | ~276 | Zero consumers. No test uses TestHarness |
| 3 | `ggen-macros` dead macros (3 of 5) | crates/ggen-macros/ (include_*, require_guards) | ~200 | Deferred to v4.0, zero consumers |
| 4 | `ggen-config-clap` bridge | crates/ggen-config-clap/ | ~100 | Trait with zero implementors |
| 5 | `config/hive_coordinator.rs` | ggen-core/config/ | ~350 | All #[allow(dead_code)], never wired |
| 6 | Entire `commands/paas/` | ggen-cli/commands/paas/ | ~500 | Feature-gated, never invoked |
| 7 | `cmds/git_hooks.rs` | ggen-cli/cmds/ | ~150 | No #[verb] annotation, unreachable |
| 8 | `cmds/packs_old.rs` | ggen-cli/cmds/ | ~200 | Legacy code, no #[verb] |
| 9 | `cmds/packs_receipt.rs` | ggen-cli/cmds/ | ~100 | Superseded by ggen-receipt crate |

### Medium Risk — Stubs on dead code paths only

| # | Item | Location | Est. Lines | Why Safe |
|---|------|----------|:----------:|----------|
| 10 | SHACL validation stubs | ggen-core/validation/shacl.rs, validator.rs | ~600 | NOT called from `ggen sync` (only from v6 pipeline, which is test-only) |
| 11 | RdfControlPlane v2 stubs | ggen-marketplace/rdf/control.rs | ~800 | search/list/dependencies/dashboard NOT on install path |
| 12 | V3OptimizedRegistry stubs | ggen-marketplace/v3.rs | ~400 | Never used by Installer |
| 13 | DMAIC gates 7-11 | ggen-core/pipeline/ | ~300 | Re-check fields already validated by gates 1-6 |
| 14 | `IncrementalCache` invalidation | ggen-core/codegen/incremental_cache.rs | ~200 | Loaded/saved but invalidation never consulted |
| 15 | `PqcSigner` / `PqcVerifier` | ggen-core/pqc.rs | ~200 | Functional but zero production callers |
| 16 | `LlmCache` | ggen-ai/cache.rs | ~150 | Complete implementation, zero consumers |
| 17 | `ToolRegistry` | ggen-ai/tool_registry.rs | ~200 | Complete implementation, zero production consumers |
| 18 | `marketplace.rs` redirect errors | ggen-domain/marketplace.rs | ~130 | All functions return Err("moved to ggen-cli") |
| 19 | graph/export.rs deprecated formats | ggen-domain/graph/export.rs | ~200 | Hardcoded data, superseded by Oxigraph serializer |
| 20 | `DefaultLlmService` | ggen-core/codegen/pipeline.rs:130 | ~50 | Intentional fallback, emits TODO comments. Keep only if v6 pipeline is adopted |
| 21 | `ultrathink/core.rs` channels | ggen-ai/ultrathink/core.rs | ~50 | Dead fields, no callers |
| 22 | `microframework/` dead fields | ggen-ai/microframework/ | ~50 | results/config stored, never used |
| 23 | Commented-out modules (secrets, supply_chain) | ggen-utils/lib.rs:58-59 | 2 lines | Broken since Week 8/9 |

### Dead code fields (within otherwise-live structs)

| # | Item | Location | Why Safe |
|---|------|----------|----------|
| 24 | `CachedPackage` struct | ggen-marketplace/rdf/control.rs:62 | All fields dead_code |
| 25 | `ReceiptBuilder` | ggen-core/v6/passes/receipt_gen.rs:138 | dead_code, no callers |
| 26 | `Plan` struct | ggen-core/pipeline.rs:384 | dead_code, no callers |
| 27 | `stage_pack_templates` | ggen-core/v6/pipeline.rs:34 | dead_code, never called |
| 28 | `V6Pipeline`, `PipelineStage`, `V6PipelineConfig` | ggen-core/v6/pipeline.rs:204,310 | dead_code, earlier iteration |
| 29 | `FileProgressBar.enabled` | ggen-core/codegen/ux.rs:79 | dead field |
| 30 | `ImpactAnalyzer.template_queries` | ggen-core/delta.rs:659 | Never populated or read |
| 31 | `PackResolver.lockfile_path` | ggen-core/pack_resolver.rs:170 | dead field |
| 32 | `SigmaRuntime.overlays` | ggen-core/ontology/sigma_runtime.rs:380 | Reserved, never used |
| 33 | `CircuitBreaker` HalfOpen variant | ggen-core/poka_yoke/network_retry.rs:16 | Never constructed |
| 34 | `LockfileGuard.lock_file` | ggen-core/poka_yoke/lockfile_guard.rs:40 | RAII-only |
| 35 | `LifecycleHook` validation metadata | ggen-core/lifecycle/hooks.rs:50 | dead field |
| 36 | `poc.rs merged_ctx` | ggen-core/poc.rs:321 | dead_code, no callers |
| 37 | `SparqlGenerator` test stub graph | ggen-ai/generators/sparql.rs:40 | Test-only |
| 38 | `PolicyEngine::evaluate_custom_condition` | ggen-ai/governance/policy.rs:346 | Always Ok(false), plugin system not needed |

### Ignored tests (permanently dead)

| # | File | Count | Reason |
|---|------|:-----:|--------|
| 39 | tests/marketplace/install_tests.rs | 26 | "Phase 2" — dead since creation |

---

## PARTIALLY IMPLEMENTED

| # | Item | Location | Issue |
|---|------|----------|-------|
| 1 | `IncrementalCache` | ggen-core/codegen/ | Hash computation and save/load work. `check_invalidation` and `get_rules_to_rerun` are implemented but no caller uses the result to skip work. Cache is loaded/saved as a no-op. |
| 2 | `validate_sparql` LLM validation | ggen-ai/sparql_validator.rs:118 | `fast_syntax_check` is real and always conclusive. `llm_validation` path is unreachable because fast check returns Some() first. |

---

## BY DESIGN

| # | Item | Location | Explanation |
|---|------|----------|-------------|
| 1 | Tera template `todo!()` emission | ggen-core/rdf/templates/impl.tera:16 | When a method body is missing in the template context, `todo!()` is emitted into generated code. This is intentional — it marks unimplemented generated methods. |

---

## Recommended Deletion Order

Execute in this order to minimize risk and maximize clarity:

### Phase 1: Dead crates and modules (lowest risk, highest signal)
1. Delete `ggen-testing` crate (~276 lines, zero consumers)
2. Delete `ggen-config-clap` crate (~100 lines, zero implementors)
3. Delete ggen-macros dead macros (3 of 5, ~200 lines)
4. Delete ggen-cli `commands/paas/` module (~500 lines)
5. Delete ggen-cli `cmds/git_hooks.rs` (~150 lines)
6. Delete ggen-cli `cmds/packs_old.rs` (~200 lines)
7. Delete ggen-cli `cmds/packs_receipt.rs` (~100 lines)
8. Delete ggen-domain AHI subsystem (~4,200 lines, 8 modules)

**Phase 1 total: ~5,726 lines removed**

### Phase 2: Dead stubs and dead code fields
9. Delete ggen-core `config/hive_coordinator.rs` (~350 lines)
10. Delete ggen-core v6 dead structs (~200 lines)
11. Delete ggen-marketplace RdfControlPlane v2 stubs (~800 lines)
12. Delete ggen-marketplace V3OptimizedRegistry stubs (~400 lines)
13. Delete ggen-domain `marketplace.rs` (~130 lines)
14. Delete ggen-domain `graph/export.rs` deprecated functions (~200 lines)
15. Delete ggen-ai `LlmCache` (~150 lines)
16. Delete ggen-ai `ultrathink/core.rs` dead fields (~50 lines)
17. Delete ggen-utils commented-out modules (2 lines)
18. Delete 26 permanently-ignored install tests

**Phase 2 total: ~2,530 lines removed**

### Phase 3: Dead code fields within live structs (requires careful editing)
19. Remove dead fields from ggen-core structs (items 25-36 above)
20. Remove dead fields from ggen-marketplace CachedPackage
21. Remove dead fields from ggen-ai microframework

**Phase 3 total: ~600 lines of dead fields removed**

### Grand total estimated deletion: ~8,900 lines

---

## Open Decisions

These items need a core team decision before classifying:

| Item | Question |
|------|----------|
| `StagedPipeline` (v6) | Wire into `ggen sync` as canonical, or keep as test-only? If test-only, many v6 stubs become CAN DELETE |
| `DefaultLlmService` | Keep as intentional fallback, or delete if v6 pipeline is deprioritized? |
| AHI subsystem | Archive to a branch for future reference, or delete permanently? |
| JSON-LD/N3 export | Implement or remove the error-returning stubs? |
| `ggen-macros` active macros (Guard, Bundle) | Only used in own tests. Keep for v4.0 or remove? |
