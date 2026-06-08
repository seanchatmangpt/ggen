## 2026-06-06T20:48:03Z
Create the integration test file `/Users/sac/ggen/crates/ggen-projection/tests/t3_pairwise.rs` containing exactly 5 tests for Tier 3: Cross-Feature Pairwise Interaction Testing:
   - `test_t3_sync_under_cyclic_dependencies`: Interaction between Feature 1 (dependencies) and Feature 2 (projection maps). Syncing a cyclic project plan must fail.
   - `test_t3_staging_gate_on_untracked_drift`: Interaction between Feature 2 (projection maps) and Feature 3 (pack proving). Staging gate check on modified files with no receipt entry.
   - `test_t3_lsp_diagnostic_drift_after_sync`: Interaction between Feature 3 (proving) and Feature 4 (diagnostics). Run a sync, modify a file, and verify that the LSP publishes a drift diagnostic (`GGEN-DRIFT-001`).
   - `test_t3_composite_routing_of_missing_dependency_diagnostics`: Interaction between Feature 4 (diagnostics) and Feature 5 (composite routing). Missing dependency diagnostics are routed and attribute-tagged correctly.
   - `test_t3_customization_override_verification`: Interaction between Feature 2 (customization map overrides) and Feature 3 (proving). Verify that file overrides in `CustomizationMap` are synced and verified correctly without marking them as dirty.

2. Create the integration test file `/Users/sac/ggen/crates/ggen-projection/tests/t4_scenarios.rs` containing exactly 5 tests for Tier 4: Real-World Application Scenarios:
   - `test_t4_clap_noun_verb_lsp_projection_flow`: Simulates the complete setup, generation, and verification flow for a durable pack template (representing the `clap-noun-verb-lsp` pack). Verify projection resolution order, sync to disk, and receipt verification.
   - `test_t4_modifying_generated_files_drift_loop`: Simulates a developer modifying generated files (drift). Verify that the LSP detects it, issues a `GGEN-DRIFT-001` diagnostic, the developer forces sync, and new receipts are generated, clearing the diagnostic.
   - `test_t4_composite_lsp_diagnostic_merge_attribution`: Simulates multiple LSPs (a mock pack LSP and `ggen-lsp`) publishing diagnostics on the same file. Verify that the composed server merges them, sorts them, and preserves proper source attribution.
   - `test_t4_dynamic_manifest_reload`: Simulates modifying a `pack.toml` file dynamically. Verify that the LSP observes the change, re-resolves the `PackPlan`, and updates diagnostics (such as detecting new missing dependencies).
   - `test_t4_incremental_sync_and_validation`: Simulates an incremental update to a customization map. Verify that only the affected files are re-projected and verified, while other receipts remain untouched and valid.

3. Run `cargo test -p ggen-projection --test t3_pairwise` and `cargo test -p ggen-projection --test t4_scenarios` to verify that they compile and pass successfully.
