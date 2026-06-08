## 2026-06-06T13:49:25-07:00
You are a developer worker. Your working directory is `/Users/sac/ggen/.agents/teamwork_preview_worker_m2_3`.
Your task is to write the Tier 3 (Cross-Feature Combinations) and Tier 4 (Real-World Application Scenarios) E2E test cases under `crates/ggen-projection/tests/`.

Instructions:
1. Create `crates/ggen-projection/tests/tier3_cross_feature.rs` and implement the 6 test cases from TEST_INFRA.md:
   - `test_t3_sync_during_manual_edit_drift`: Sync gate refuses to overwrite files that have drifted unless force/override is true.
   - `test_t3_dependency_removal_breaks_diagnostic_chain`: Removing a pack dependency triggers missing evidence diagnostics downstream.
   - `test_t3_slow_generation_timeout_in_lsp`: LSP proxy handles timeouts for sync/generation while maintaining previous diagnostics/snapshots.
   - `test_t3_customization_point_completed_resolves_diagnostic`: Completing customization points resolves customization diagnostics and updates receipts.
   - `test_t3_signature_change_invalidates_opportunity`: Modifying opportunity signatures invalidates matching scan opportunities.
   - `test_t3_corrupt_receipt_blocks_export`: Receipt index corruption blocks export to wasm4pm packages.
2. Create `crates/ggen-projection/tests/tier4_real_world.rs` and implement the 5 scenarios from TEST_INFRA.md:
   - Scenario 1: End-to-End Pack Creation and Sync
   - Scenario 2: Modification and Drift Diagnostics
   - Scenario 3: Customization Point Completion
   - Scenario 4: Upstream Language Server Crash Recovery
   - Scenario 5: Process Audit and wasm4pm Verification
3. Use the common test harness (`common/mod.rs`), models (`PackDescriptor`, `PackPlan`, `ProjectionMap`, etc.), and helper client (`TestLspClient`) to perform real execution of the scenarios.
4. Run `cargo check --all-targets` and `cargo test -p ggen-projection` to ensure that all unit and E2E tests compile cleanly and pass.
5. Write your handoff report to `/Users/sac/ggen/.agents/teamwork_preview_worker_m2_3/handoff.md` and send a message back.

MANDATORY INTEGRITY WARNING:
DO NOT CHEAT. All implementations must be genuine. DO NOT hardcode test results, create dummy/facade implementations, or circumvent the intended task. A Forensic Auditor will independently verify your work. Integrity violations WILL be detected and your work WILL be rejected.
