## 2026-06-06T20:45:36Z
You are teamwork_preview_worker_impl_f4_f5_1.
Your working directory is: /Users/sac/ggen/.agents/teamwork_preview_worker_impl_f4_f5_1
Your task is to:
1. Create the file `/Users/sac/tower-lsp-max/tests/e2e/test_f4_diagnostics.rs` containing exactly 10 tests for Feature 4: LSP Diagnostics & Opportunity. Use the existing `TestHarness` and `MockServer` helpers.
   The 10 tests must cover:
   Tier 1: Feature Coverage (5 tests):
   - `test_f4_t1_publish_diagnostics_forwarding`: Upstream publishes diagnostics, composed server forwards to client.
   - `test_f4_t1_diagnose_drift_code`: Client receives diagnostic code `GGEN-DRIFT-001` (drifted content).
   - `test_f4_t1_diagnose_missing_receipt`: Client receives diagnostic code `GGEN-EVIDENCE-001` (missing receipts).
   - `test_f4_t1_diagnose_incomplete_customization`: Client receives diagnostic code `GGEN-CUSTOMIZE-001` (incomplete customization points).
   - `test_f4_t1_detect_projection_opportunity`: Client receives diagnostic code `GGEN-PROJECT-OPPORTUNITY-001`.
   Tier 2: Boundary and Corner Cases (5 tests):
   - `test_f4_t2_diagnostics_clear_on_save`: Diagnostics are cleared or updated when file is modified.
   - `test_f4_t2_diagnostics_invalid_syntax`: Graceful handling of invalid diagnostic structures.
   - `test_f4_t2_diagnostics_empty_array`: Composed server forwards empty diagnostics to clear client markers.
   - `test_f4_t2_diagnostics_non_standard_severity`: Correct mapping of custom severity fields.
   - `test_f4_t2_diagnostics_maximum_limit`: Handling large payload arrays of diagnostics without crash.

2. Create the file `/Users/sac/tower-lsp-max/tests/e2e/test_f5_composite_routing.rs` containing exactly 10 tests for Feature 5: Composite LSP Routing. Use the existing `TestHarness` and `MockServer` helpers.
   The 10 tests must cover:
   Tier 1: Feature Coverage (5 tests):
   - `test_f5_t1_compose_initialize_capabilities`: Composite server capabilities intersection from upstreams.
   - `test_f5_t1_routing_document_sync_did_open`: `didOpen` is routed to all upstreams.
   - `test_f5_t1_routing_hover_first_success`: `hover` returns first success response.
   - `test_f5_t1_routing_completion_merge`: `completion` lists are merged and ranked.
   - `test_f5_t1_routing_definition_merge`: `definition` lists are merged and deduplicated.
   Tier 2: Boundary and Corner Cases (5 tests):
   - `test_f5_t2_routing_slow_upstream_timeout`: Composed server returns results within timeout even if one upstream is slow.
   - `test_f5_t2_routing_crashed_upstream_resilience`: Composed server returns results even if one upstream crashes/closes.
   - `test_f5_t2_routing_unsupported_method`: Routing invalid method returns MethodNotFound.
   - `test_f5_t2_routing_concurrent_stress`: Composed server handles multiple concurrent queries without deadlock.
   - `test_f5_t2_routing_empty_upstreams`: Initialize composed server with empty upstreams list compiles and syncs without error.

3. Edit `/Users/sac/tower-lsp-max/tests/e2e/mod.rs` to register the two new modules.
4. Run `cargo test --test e2e` in `/Users/sac/tower-lsp-max/` to compile and verify all tests pass.
