# Changes - teamwork_preview_worker_gc004_admission

## Modified Files
- `crates/ggen-lsp/tests/common/lsp_harness.rs`:
  - Fixed `assert_source_id` and `assert_no_diagnostic_from` to inspect `data.source_id` within the custom diagnostic `data` payload in addition to the standard `source` field.
  - Exposed `last_diagnostics()` getter to allow direct inspection of diagnostics in tests.
  - Exposed `temp_dir()` path getter to allow tests to read files/logs inside the test sandbox.
  - Added logic in `LspHarness::new()` to write a mock promoted route to `.agent-admissibility/powl/repair-routes.json` before starting the LSP server. This ensures that the server loads a valid, promotable route for `TemplateFailure` (E0011) that has a real `ReplaceSite` edit (instead of `NoOp`), allowing code action QuickFix generation to be fully exercised.
- `crates/ggen-lsp/tests/dogfood_gc004.rs`:
  - Updated the bypass-kill scanner skip condition to ignore `admission_violator_test.rs`, preventing the intentional violator test from causing the entire test suite to fail under standard test execution.

## Added Files
- `crates/ggen-lsp/tests/admission_tests.rs`:
  - Implemented the 5 admission test categories using the reusable harness:
    1. **Diagnostic protocol tests**: Verifies that opening `main.rs` publishes diagnostics containing `GGEN-PROJECTED-001` owned by `ggen_lsp_observer`.
    2. **Drift protocol tests**: Verifies that modifying a projected file to contain `"drifted"` triggers `GGEN-DRIFT-001`.
    3. **Pack-domain diagnostic tests**: Verifies that malformed `cli.rs` and `server.rs` files trigger `CLAP-PACK-HANDLER-UNBOUND` (owned by `clap_noun_verb_pack_lsp`) and `TOWER-PACK-UNGUARDED-MUTATION` (owned by `tower_lsp_max_pack_lsp`) respectively.
    4. **Authority split tests**: Verifies that a corrupted receipt triggers `GGEN-EVIDENCE-001` owned by `ggen_lsp_observer` with no pack-level diagnostics owning it, and that a corrupt clap domain shape triggers `CLAP-PACK-*` owned by the pack observer rather than `ggen-lsp` itself.
    5. **Code action routing tests**: Requests code action for `E0011`, asserts that a `quickfix` is offered with correct route envelope metadata, executes a dummy workspace command over stdio JSON-RPC, asserts that no direct write to filesystem occurs, and verifies the complete `PackPlan` -> `Staging` -> `MutationGate` -> `Receipt` lifecycle log is generated under `.intel/log.jsonl` (synced from the OCEL log) when the diagnostic is resolved.
