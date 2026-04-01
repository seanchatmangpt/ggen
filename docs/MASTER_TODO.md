# ggen Rust Core Team — Master TODO List

**Generated:** 2026-04-01
**Source:** LSP-surveyed audit of all 31 workspace crates
**Scope:** Everything in `crates/` and `tests/` — excludes `vendors/`

---

## P0 — CRITICAL (Blocks other work or ships wrong behavior)

### P0-01. SHACL validation is entirely stubbed — always passes

| Detail | Value |
|--------|-------|
| **Blocker** | T014 — Graph::query() wrapper API unclear |
| **Files** | `ggen-core/src/validation/shacl.rs`, `validator.rs`, `tests.rs` |
| **Lines** | shacl.rs:132-155, validator.rs:6-12, tests.rs:3-5 |
| **Impact** | All RDF-based validation is a no-op. Normalization pass passes dummy empty graph. No SHACL constraints are ever enforced. |
| **Action** | Restore 628-line SPARQL implementation from git history. Restore 22 Chicago TDD tests (715 lines). Resolve Graph::query() iteration pattern. |
| **Downstream** | Blocks quality gates, blocks ontology validation, blocks marketplace package validation |

### P0-02. Production sync uses wrong pipeline

| Detail | Value |
|--------|-------|
| **Current** | `ggen sync` → `GenerationPipeline::run()` at `codegen/pipeline.rs:811` |
| **Expected** | Documentation and CLAUDE.md describe `StagedPipeline::run()` at `v6/pipeline.rs:329` |
| **Impact** | Two parallel pipelines exist. v6 constitutional pipeline (μ₀-μ₅) has receipt provenance, epoch verification, and staged governance. Production pipeline has none of these. |
| **Action** | Decide: either wire v6 pipeline into sync command, or update all documentation to reflect GenerationPipeline as canonical. |
| **Files** | `ggen-cli/src/cmds/sync.rs`, `ggen-core/src/codegen/executor.rs:589`, `ggen-core/src/v6/pipeline.rs` |

### P0-03. Three competing ontology namespaces cause silent data loss

| Detail | Value |
|--------|-------|
| **URI 1** | `https://ggen.io/marketplace/` — used by `ontology.rs`, `v3.rs`, `registry_rdf.rs`, `rdf/control.rs` |
| **URI 2** | `http://ggen.dev/ontology#` — used by `rdf/ontology.rs`, config TTL files, tests |
| **URI 3** | `http://ggen.dev/marketplace#` — used in `rdf/ontology.rs:24` |
| **Impact** | SPARQL queries with one namespace return empty results against triples stored with another. No error, just silent empty data. |
| **Action** | Consolidate to single canonical namespace. Audit every SPARQL query, every TTL file, every URI constructor. |
| **Files** | `ggen-marketplace/src/ontology.rs:14`, `ggen-marketplace/src/rdf/ontology.rs:23-24`, `ggen-core/src/rdf/schema.rs:46`, `ggen-domain/src/rdf/schema.rs:9` |

### P0-04. CLI error type chaos — three Result types in prelude

| Detail | Value |
|--------|-------|
| **File** | `ggen-cli/src/prelude.rs` |
| **Problem** | `AnyhowResult` (anyhow), `Result` (clap_noun_verb), `UtilsResult` (ggen_utils) — three incompatible error types. `Context` imported twice under different names. |
| **Impact** | Every CLI command file must choose between incompatible error types. Error conversion chains silently swallow context. |
| **Action** | Standardize on one Result type for the CLI layer. Remove anyhow and clap_noun_verb Results from prelude. |
| **Scale** | 23 crates define local error modules. Only 2 re-export shared type. |

---

## P1 — HIGH (Stubs that need real implementation)

### P1-01. Construct command returns "not_implemented"

| File | `ggen-cli/src/cmds/construct.rs:286,327` |
|------|------|
| **Blocker** | `ggen_ai::llm_construct` module commented out due to dspy dependency |
| **Action** | Re-enable llm_construct in `ggen-ai/src/lib.rs`, resolve dspy dependency, wire `LLMConstructBuilder::build()` + `LLMConstructCodeGen::generate_rust_module()` |

### P1-02. Marketplace v3 registry — core methods return empty/error

| File | `ggen-marketplace/src/v3.rs:249-273` |
|------|------|
| **Stubs** | `get_package()` → Err, `get_package_version()` → Err, `all_packages()` → empty Vec, `list_versions()` → empty Vec |
| **Action** | Implement two-level cache lookup (metadata cache + RDF store) and Package reconstruction |

### P1-03. Marketplace RdfControlPlane — half the methods are stubs

| File | `ggen-marketplace/src/rdf/control.rs` |
|------|------|
| **Stubs** | `search_packages()` → empty, `list_packages()` → empty, `get_dependencies()` → empty, `get_published_package()` → NotImplemented, `get_maturity_metrics()` → hardcoded, `get_dashboard_stats()` → zeros |
| **Working** | `create_package()`, `publish_package()`, `get_package_state()` |
| **Action** | Implement SPARQL-backed queries for search, listing, dependency trees, and published package reconstruction |

### P1-04. Pack install — 29 tests blocked on Phase 2

| File | `ggen-cli/tests/marketplace/install_tests.rs` |
|------|------|
| **Count** | 26 `#[ignore]` tests marked "TODO: Enable in Phase 2" |
| **Blocker** | `install_package` not implemented at CLI level |
| **Action** | Wire CLI install command through `ggen-marketplace::install::Installer`, enable all 26 tests |

### P1-05. MCP tool execution returns placeholder JSON

| File | `ggen-ai/src/mcp/traits.rs:338-345` |
|------|------|
| **Current** | Returns `{"status": "placeholder", "message": "Tool execution not yet implemented"}` |
| **Action** | Dispatch through `ToolRegistry` via `export_openapi()` definitions |

### P1-06. JSON Schema parser returns error

| File | `ggen-core/src/schema/parser.rs:281-283` |
|------|------|
| **Current** | `from_json_schema()` → `Err("JSON Schema parsing not yet implemented")` |
| **Action** | Implement JSON Schema → `Schema` struct conversion |
| **Downstream** | Blocks A2A schema support, Python code generation |

### P1-07. Python code generation not implemented

| File | `ggen-core/src/schema/generators.rs:601-609` |
|------|------|
| **Current** | Returns comment string "# Python code generation not yet implemented..." |
| **Action** | Implement Python dataclass generation from Schema |

### P1-08. Graph evolution agent — AI client returns error

| File | `ggen-ai/src/agents/core/graph_evolution.rs:358-361` |
|------|------|
| **Current** | `get_ai_client()` → `Err("AI client not implemented")` |
| **Action** | Initialize and return appropriate LLM client for graph analysis |

### P1-09. PaaS deploy, logs follow, explain spec/pipeline — all not implemented

| File | `ggen-cli/src/commands/paas/handlers/` |
|------|------|
| **deploy.rs:47** | "Deployment not yet implemented" |
| **logs.rs:60** | "Log following not yet implemented" |
| **explain.rs:66,78** | "Specification tracing not yet implemented", "Pipeline details not yet implemented" |
| **Action** | Implement PaaS deployment, log streaming, and provenance tracing |

### P1-10. YAWL watch mode returns error

| File | `ggen-cli/src/cmds/yawl.rs:268-275` |
|------|------|
| **Current** | Returns error "Watch mode is not yet fully implemented" |
| **Action** | Implement file-watching with automatic regeneration on workflow changes |

### P1-11. MCP server background mode not implemented

| File | `ggen-cli/src/cmds/mcp.rs:820-828` |
|------|------|
| **Current** | "Background mode not yet implemented. Use foreground mode." |
| **Action** | Implement daemon/background process mode with pid tracking |

### P1-12. Program of Thought — only JavaScript code generation works

| File | `ggen-dspy/src/modules/program_of_thought.rs:168-170` |
|------|------|
| **Current** | Non-JavaScript languages return `Err("Code generation not implemented for {:?}", language)` |
| **Action** | Implement Python, Rust, and other language code generation |

### P1-13. DSPy optimizers are pass-through stubs

| File | `ggen-dspy/src/optimizers/bootstrap.rs:28-29`, `mipro.rs:28-29` |
|------|------|
| **Current** | `compile()` returns `Ok(module)` unchanged. Config fields stored but unused. |
| **Action** | Implement BootstrapFewShot (demo selection) and MIPRO (instruction optimization) algorithms |

---

## P2 — MEDIUM (Mock implementations returning wrong answers)

### P2-01. Sigma runtime invariants — 4 of 7 are empty branches

| File | `ggen-core/src/ontology/validators.rs:162-172` |
|------|------|
| **Stubs** | TypeSoundness, GuardSoundness, ProjectionDeterminism, SLOPreservation — all "mock implementation" with no-op bodies |
| **Action** | Implement actual validation logic for each invariant |

### P2-02. Delta proposer returns mock proposals instead of calling LLM

| File | `ggen-core/src/ontology/delta_proposer.rs:345-350` |
|------|------|
| **Action** | Call actual genai service with streaming for real delta-sigma proposals |

### P2-03. WeakProof::is_valid() always returns true

| File | `ggen-domain/src/action_types.rs:397-398` |
|------|------|
| **Action** | Implement expiry checking and revocation status verification |

### P2-04. Marketplace install conflict check always passes

| File | `ggen-marketplace/src/install.rs:234-235` |
|------|------|
| **Current** | Always returns Ok — "would check semantic version constraints" |
| **Action** | Implement semver constraint satisfaction checking |

### P2-05. Marketplace README validator only checks description non-empty

| File | `ggen-marketplace/src/validation.rs:270-271` |
|------|------|
| **Action** | Check for actual README.md/README.rst files in package |

### P2-06. Execution proof verification not implemented

| File | `ggen-core/src/codegen/execution_proof.rs:116-118` |
|------|------|
| **Current** | Comment describes what verification should do; no code executes |
| **Action** | Implement deterministic build verification and hash chain integrity checks |

### P2-07. Regeneration cache/dependency validation returns Ok/None

| File | `ggen-ai/src/agents/core/regeneration.rs:459,465` |
|------|------|
| **Current** | `check_artifact_cache()` → `Ok(None)`, `validate_dependencies()` → `Ok(true)` |
| **Action** | Implement cache freshness checking and dependency staleness detection |

### P2-08. Template validation only checks for empty content

| File | `ggen-core/src/prevention/state_machine.rs:289-290` |
|------|------|
| **Action** | Implement full Tera/Handlebars template syntax validation |

### P2-09. Pack generator logs "Would generate template" — no actual generation

| File | `ggen-domain/src/packs/generator.rs:70-75` |
|------|------|
| **Action** | Call template domain functions for actual rendering |

### P2-10. Pack composition Custom strategy returns error

| File | `ggen-domain/src/packs/compose.rs:57-61` |
|------|------|
| **Action** | Implement user-defined custom composition strategies |

### P2-11. Pack registry unpublish just logs a warning

| File | `ggen-domain/src/packs/registry.rs:130-141` |
|------|------|
| **Action** | Implement permission check, CDN removal, registry index update, user notification |

### P2-12. Graph export functions return hardcoded example data

| File | `ggen-domain/src/graph/export.rs:217-293` |
|------|------|
| **Stubs** | `generate_turtle()`, `generate_ntriples()`, `generate_rdfxml()`, `generate_jsonld()`, `generate_n3()` — all `#[allow(dead_code)]` returning hardcoded strings |
| **Action** | Implement actual RDF serialization using oxigraph for each format |

### P2-13. Bundle dependency resolution returns empty Vec

| File | `ggen-macros/src/lib.rs:140-142` |
|------|------|
| **Action** | Implement lazy loading and dependency resolution for bundles |

### P2-14. Windows lockfile not implemented

| File | `ggen-core/src/poka_yoke/lockfile_guard.rs:74-79` |
|------|------|
| **Current** | Empty block for non-Unix platforms |
| **Action** | Implement using `winapi` or `windows-sys` crate |

---

## P3 — ARCHITECTURAL CLEANUP (Dead code, unused infrastructure, type debris)

### P3-01. ggen-testing — complete framework, zero consumers

| Detail | Value |
|--------|-------|
| **Size** | 276 lines: TestHarness, StateVerifier, 16 assertions, property/snapshot testing, 4 fixture types |
| **Consumers** | Zero crates depend on it. Only its own self-test references it. |
| **Action** | Either integrate into workspace tests or remove from workspace |

### P3-02. ggen-macros — 3 of 5 macros dead, 2 never used

| Detail | Value |
|--------|-------|
| **Dead** | `include_ontology!`, `include_templates!`, `include_examples!` — all `#[allow(dead_code)]` marked "FUTURE: v4.0" |
| **Unused** | `#[derive(Guard)]`, `#[derive(Bundle)]` — only used in own tests |
| **Action** | Remove or schedule for v4.0 milestone |

### P3-03. ~4,200 lines of dead AHI subsystem code in ggen-domain

| Modules | Lines | Consumers |
|---------|-------|-----------|
| `ahi_contract.rs` | 502 | 0 |
| `auto_promotion_pipeline.rs` | 426 | 0 |
| `doctrine_engine.rs` | 366 | 0 |
| `proof_carrier.rs` | 631 | 0 |
| `temporal_fabric.rs` | 606 | 0 |
| `capability_system.rs` | 527 | 0 |
| `proof_types.rs` | 552 | 0 |
| `action_types.rs` | 607 | 0 |
| **Action** | Remove or gate behind feature flag |

### P3-04. Hive coordinator — entire module is dead code

| File | `ggen-core/src/config/hive_coordinator.rs` |
|------|------|
| **Size** | HiveCoordinator, HiveState, ConsensusTopic, PackageConflict, ConflictType, CompatibilityMatrix — all `#[allow(dead_code)]` |
| **Action** | Remove or implement |

### P3-05. V6 pipeline types dead-code gated

| File | `ggen-core/src/v6/pipeline.rs:34,204,310` |
|------|------|
| **Dead** | `V6Pipeline`, `PipelineStage`, `V6PipelineConfig` — all `#[allow(dead_code)]` |
| **Note** | `StagedPipeline` is the active type. These appear to be earlier iterations. |
| **Action** | Remove dead types or merge into StagedPipeline |

### P3-06. Lifecycle module has disabled submodules

| File | `ggen-core/src/lifecycle/mod.rs:42,45,47` |
|------|------|
| **Disabled** | `template_phase` — "TEMPORARILY DISABLED: needs ggen_template crate which doesn't exist yet" |
| **Dead** | `dag` module, `dx` module — `#[allow(dead_code)]` |
| **Action** | Remove or create missing ggen_template crate |

### P3-07. LoadConfigFromGgenToml trait — zero implementors

| File | `ggen-config-clap/src/loader.rs:10-19` |
|------|------|
| **Action** | Implement on CLI argument structs, or remove trait if pattern abandoned |

### P3-08. ggen-domain marketplace module — all functions return errors

| File | `ggen-domain/src/marketplace.rs:90-130` |
|------|------|
| **Functions** | `list_all()`, `get_package()`, `resolve_dependencies()`, `execute_install()` — all return Err |
| **Action** | Remove or redirect to ggen-marketplace crate |

### P3-09. ggen-core pack install — bails immediately

| File | `ggen-core/src/packs/install.rs:33-38` |
|------|------|
| **Action** | Remove and update all call sites to use `ggen_marketplace::install::Installer` |

### P3-10. 4 stale workspace exclude entries

| File | `Cargo.toml` (workspace root) |
|------|------|
| **Stale** | `crates/ggen-workflow`, `crates/ggen-workflow/native`, `examples/marketplace-demo/generated`, `examples/api-endpoint`, `examples/advanced-lifecycle-demo` — directories no longer exist |
| **Action** | Remove stale entries |

---

## P3 — TEST QUALITY (120 ignored tests, mock violations, 5K+ unwraps)

### TQ-01. 120 `#[ignore]` tests — never run in CI

**Top offenders:**

| File | Count | Reason |
|------|:---:|--------|
| `ggen-cli/tests/marketplace/install_tests.rs` | 26 | "Phase 2" — permanently dead |
| `tests/integration/marketplace_nextjs_ontology_e2e.rs` | 15 | "Long-running" |
| `ggen-core/tests/production_validation.rs` | 11 | "Requires Docker" |
| `tests/integration/otel_validation_tests.rs` | 11 | "Requires Docker" |
| `tests/integration/full_workflow_scenarios.rs` | 9 | "Run with --ignored" |
| `tests/factory_paas/load_tests.rs` | 9 | "Ignore by default" |
| `tests/security/rate_limit_integration_tests.rs` | 7 | "Requires running API" |
| `ggen-cli/tests/integration.rs` | 7 | Various features not implemented |
| **Action** | Enable, fix, or delete. Phase 2 tests are dead code. |

### TQ-02. 8 files with hand-rolled mock structs (Chicago TDD violation)

| File | Mock Types |
|------|-----------|
| `tests/tool_discovery_integration_tests.rs` | MockA2AServer, MockMCPClient (64 usages) |
| `tests/mcp_a2a/tool_discovery_tests.rs` | MockA2AServer, MockMCPClient (102 usages) — **duplicate of above** |
| `tests/mcp_a2a/transport_tests.rs` | MockHttpServer, MockWebSocketServer, MockHttpClient, MockWebSocketClient, MockWebSocketConnection |
| `tests/workflow_tests.rs` | MockServices |
| `tests/a2a_integration_tests.rs` | MockA2AServer, MockMCPServer |
| `tests/ontology_systems_tests.rs` | MockLLMProposer, MockStaticValidator, MockPerformanceValidator, MockDynamicValidator |
| `ggen-ai/tests/dspy_optimizer_test.rs` | MockPredictor |
| **Action** | Replace with real collaborators per Chicago TDD mandate. Delete duplicate `tool_discovery_tests.rs`. |

### TQ-03. 5,046 `.unwrap()` + 2,069 `.expect()` in test code

**Worst offenders:**

| File | unwrap | expect |
|------|:---:|:---:|
| `tests/graph_core_tests.rs` | 278 | — |
| `ggen-core/tests/integration/lifecycle_tests.rs` | 133 | — |
| `ggen-core/tests/pipeline_builder_test.rs` | 118 | 58 |
| `ggen-marketplace/tests/unit/rdf_turtle_test.rs` | 103 | — |
| `ggen-core/tests/pipeline/canonicalization_tests.rs` | 98 | — |
| `ggen-receipt/tests/chain_integrity_tests.rs` | — | 113 |
| **Action** | Convert to `.expect("descriptive context")` or proper assertion macros for diagnostic failures |

---

## SUMMARY

| Priority | Category | Count |
|----------|----------|:---:|
| **P0** | Critical — blocks other work or ships wrong behavior | 4 |
| **P1** | High — stubs needing real implementation | 13 |
| **P2** | Medium — mock implementations returning wrong answers | 14 |
| **P3** | Architectural — dead code, unused infrastructure | 10 |
| **P3** | Test quality — ignored tests, mock violations, unwrap cleanup | 3 |
| **Total action items** | | **44** |

| Metric | Value |
|--------|-------|
| Explicit stubs (bail/err "not implemented") | 20 |
| Mock/placeholder implementations | 9 |
| `#[ignore]` tests | 120 |
| Dead code structs/modules | ~100+ annotations |
| Dead infrastructure crates | 2 (ggen-testing, ggen-macros) |
| Dead AHI subsystem lines | ~4,200 |
| Ontology namespace conflicts | 3 competing URIs |
| Error type definitions | 23 local error modules across crates |
| Total test markers | ~12,514 |
| Test `.unwrap()` calls | 5,046 |
| Test `.expect()` calls | 2,069 |
