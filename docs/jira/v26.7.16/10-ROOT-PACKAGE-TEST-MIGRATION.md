# Root Package + Test Migration

Part of [00-OVERVIEW](00-OVERVIEW.md) — Phase 5, depends on
[08-GGEN-CLI-MIGRATION](08-GGEN-CLI-MIGRATION.md) step 11 and
[09-GGEN-LSP-MIGRATION](09-GGEN-LSP-MIGRATION.md) step 6 both completing.

## File reference table

`/Users/sac/ggen/src/lib.rs` — 13 lines total. Line 10: `pub use ggen_core as core;` —
confirmed. This is the root `ggen` package published on crates.io (see
[01-PUBLISH-SAFETY-AND-CRATE-RENAME](01-PUBLISH-SAFETY-AND-CRATE-RENAME.md)) — a real
consumer, not just the two inner crates.

### Full list: 82 `.rs` files referencing `ggen_core::` (re-verified this session)

`grep -rl "ggen_core::" tests examples benches` returns 89 files, but 7 are Markdown docs
merely mentioning `ggen_core::` in prose, not compiling against it — the real count,
confirmed exactly this session, is **82** (3 benches + 23 examples + 56 tests).

**benches/ (3 files):**
- `/Users/sac/ggen/benches/canonical_bench.rs`
- `/Users/sac/ggen/benches/ggen_benchmarks.rs`
- `/Users/sac/ggen/benches/receipt_bench.rs`

**examples/ (23 files):**
- `/Users/sac/ggen/examples/_archive/a2a_tasks.rs`
- `/Users/sac/ggen/examples/_archive/full_pipeline.rs`
- `/Users/sac/ggen/examples/_archive/packet_routing.rs`
- `/Users/sac/ggen/examples/_archive/receipt_chain.rs`
- `/Users/sac/ggen/examples/advanced-cache-registry/src/cache_demo.rs`
- `/Users/sac/ggen/examples/advanced-cache-registry/src/registry_demo.rs`
- `/Users/sac/ggen/examples/advanced-error-handling/src/errors.rs`
- `/Users/sac/ggen/examples/advanced-fullstack-integration/src/architect.rs`
- `/Users/sac/ggen/examples/drift-detection-example.rs`
- `/Users/sac/ggen/examples/ggen-usage-wrapping/examples/basic-usage.rs`
- `/Users/sac/ggen/examples/ggen-usage-wrapping/examples/batch-processor.rs`
- `/Users/sac/ggen/examples/ggen-usage-wrapping/examples/custom-pipeline.rs`
- `/Users/sac/ggen/examples/ggen-usage-wrapping/examples/graph-operations.rs`
- `/Users/sac/ggen/examples/ggen-usage-wrapping/examples/template-validation.rs`
- `/Users/sac/ggen/examples/ggen-usage-wrapping/wrappers/custom-cli/src/main.rs`
- `/Users/sac/ggen/examples/ggen-usage-wrapping/wrappers/rest-api/src/main.rs`
- `/Users/sac/ggen/examples/knowledge-graph-builder/src/main.rs`
- `/Users/sac/ggen/examples/lifecycle-complete/src/main.rs`
- `/Users/sac/ggen/examples/path_validation_example.rs`
- `/Users/sac/ggen/examples/rdf_metadata_example.rs`
- `/Users/sac/ggen/examples/rdf_template_integration.rs`
- `/Users/sac/ggen/examples/sparql-engine/src/main.rs`
- `/Users/sac/ggen/examples/test_lockfile.rs`

**tests/ (56 files):**
- `/Users/sac/ggen/tests/a2a_integration_tests.rs`
- `/Users/sac/ggen/tests/chicago_tdd/expert_patterns/boundaries.rs`
- `/Users/sac/ggen/tests/chicago_tdd/expert_patterns/concurrency.rs`
- `/Users/sac/ggen/tests/chicago_tdd/expert_patterns/error_paths.rs`
- `/Users/sac/ggen/tests/chicago_tdd/expert_patterns/resources.rs`
- `/Users/sac/ggen/tests/chicago_tdd/ontology_driven_e2e.rs`
- `/Users/sac/ggen/tests/cli.rs`
- `/Users/sac/ggen/tests/common/fixtures.rs`
- `/Users/sac/ggen/tests/contract/manifest.rs`
- `/Users/sac/ggen/tests/contract/pipeline.rs`
- `/Users/sac/ggen/tests/domain/graph/query_tests.rs`
- `/Users/sac/ggen/tests/e2e_v2/test_helpers.rs`
- `/Users/sac/ggen/tests/fixture_validation_proof.rs`
- `/Users/sac/ggen/tests/generator_core_tests.rs`
- `/Users/sac/ggen/tests/graph_core_tests.rs`
- `/Users/sac/ggen/tests/integration/cache_tests.rs`
- `/Users/sac/ggen/tests/integration/clap_noun_verb_ontology_test.rs`
- `/Users/sac/ggen/tests/integration/code_generation_tests.rs`
- `/Users/sac/ggen/tests/integration/graph/core_operations_test.rs`
- `/Users/sac/ggen/tests/integration/graph/export_operations_test.rs`
- `/Users/sac/ggen/tests/integration/lifecycle_tests.rs`
- `/Users/sac/ggen/tests/integration/nextjs_ontology_sync.rs`
- `/Users/sac/ggen/tests/integration/packs/pack_cli_integration_test.rs`
- `/Users/sac/ggen/tests/integration/packs/pack_e2e_workflows_test.rs`
- `/Users/sac/ggen/tests/integration/packs/user_workflow_multi_pack_test.rs`
- `/Users/sac/ggen/tests/integration/packs/user_workflow_single_pack_test.rs`
- `/Users/sac/ggen/tests/integration/packs/user_workflow_template_reuse_test.rs`
- `/Users/sac/ggen/tests/integration/template_tests/test_template_regenerate.rs`
- `/Users/sac/ggen/tests/integration/test_determinism.rs`
- `/Users/sac/ggen/tests/integration/test_manifest.rs`
- `/Users/sac/ggen/tests/integration/test_rdf.rs`
- `/Users/sac/ggen/tests/mcp_a2a/message_handling_test.rs`
- `/Users/sac/ggen/tests/mcp_a2a/mock_a2a_server.rs`
- `/Users/sac/ggen/tests/mcp_a2a/stdio_transport_test.rs`
- `/Users/sac/ggen/tests/mcp_a2a/transport_tests.rs`
- `/Users/sac/ggen/tests/otel_validation/mod.rs`
- `/Users/sac/ggen/tests/performance/packs_performance_test.rs`
- `/Users/sac/ggen/tests/performance/packs/pack_benchmarks.rs`
- `/Users/sac/ggen/tests/prevention_integration_tests.rs`
- `/Users/sac/ggen/tests/security/e2e_vulnerability_tests.rs`
- `/Users/sac/ggen/tests/security/mod.rs`
- `/Users/sac/ggen/tests/security/supply_chain_tests.rs`
- `/Users/sac/ggen/tests/security/week4_security_hardening_tests.rs`
- `/Users/sac/ggen/tests/template_systems_tests.rs`
- `/Users/sac/ggen/tests/tracing.rs`
- `/Users/sac/ggen/tests/transport/registry_client_tests.rs`
- `/Users/sac/ggen/tests/unit/marketplace_critical_tests.rs`
- `/Users/sac/ggen/tests/unit/packs/gpack_manifest_test.rs`
- `/Users/sac/ggen/tests/unit/packs/pack_composer_test.rs`
- `/Users/sac/ggen/tests/unit/packs/pack_core_domain_test.rs`
- `/Users/sac/ggen/tests/unit/packs/pack_edge_cases_test.rs`
- `/Users/sac/ggen/tests/unit/packs/pack_generator_test.rs`
- `/Users/sac/ggen/tests/unit/packs/pack_installer_test.rs`
- `/Users/sac/ggen/tests/unit/packs/pack_validation_test.rs`
- `/Users/sac/ggen/tests/unit/packs/pack_validator_test.rs`
- `/Users/sac/ggen/tests/validate_marketplace_rdf.rs`

Symbol tally (`grep -rhoE "ggen_core::[a-z_0-9]+"`): `utils` 35, `domain` 24, `graph` 14,
`gpack` 13, `template_cache` 7, `lifecycle` 7, `simple_tracing` 6, `security` 4, then a long
tail (`receipt` 3, `manifest` 1, `telemetry` 1, `packs` 1) — **zero** hits for `config_lib`
or `marketplace`. None of the sub-package example projects
(`advanced-cache-registry`, `sparql-engine`, `knowledge-graph-builder`,
`lifecycle-complete`, `advanced-error-handling`, `advanced-fullstack-integration`,
`ggen-usage-wrapping`) are workspace members — they don't gate `cargo build --workspace` or
the pre-commit/pre-push hooks; root `tests/*.rs`/`benches/*.rs` do.

## Recommendation: batch at the end, with a narrow exception

Not a blanket incremental migration. Since `utils`/`domain` dominate the symbol tally and
most files touch several buckets at once, only the 3 `receipt`-only and 1 `manifest`-only
candidate files are worth checking for early movement (immediately after
[08-GGEN-CLI-MIGRATION](08-GGEN-CLI-MIGRATION.md) steps 2 and 4 land, respectively) —
everything else should wait for that ticket's step 11 and
[09-GGEN-LSP-MIGRATION](09-GGEN-LSP-MIGRATION.md)'s step 6 to both complete, since that's
when their real transitive dependencies exist in the new engine. The 4
`/Users/sac/ggen/examples/_archive/` files and the 7 non-member sub-package example
directories listed above carry no build-gate urgency and can be deferred or deleted as a
separate decision.

## Definition of done for this ticket

- All 82 files listed above re-pointed away from `ggen_core::` (or explicitly deleted, for
  the non-member/archived examples — record which decision was made per file group).
- `/Users/sac/ggen/src/lib.rs:10`'s `pub use ggen_core as core;` updated to the new engine
  crate.
- `cargo test --workspace` (via `just test`) passes with these files exercising the new
  engine.
