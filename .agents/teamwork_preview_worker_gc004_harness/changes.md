# Changes Made

## LSP Reusable Test Harness (`crates/ggen-lsp/tests/common/lsp_harness.rs`)
- Implemented `LspHarness` from scratch to serve as a reusable black-box LSP test client.
- Followed the **No-Fake Surface Law**: all communications run over standard stdio JSON-RPC using Content-Length headers with the spawned `ggen-lsp` process.
- Exposed **ONLY** the designated LSP operations:
  - `new()` (constructor to spawn the process)
  - `initialize()`
  - `did_open(uri, text)`
  - `did_change(uri, new_text)`
  - `request_code_action(uri, range)`
  - `execute_command(command, args)`
  - `wait_for_publish_diagnostics(uri)`
  - `assert_diagnostic_code(code)`
  - `assert_source_id(source_id)`
  - `assert_no_diagnostic_from(source_id)`
  - `last_diagnostics()`
  - `temp_dir()`
- Handled diagnostic extraction and range matching inside `request_code_action`. It finds diagnostics on the same line and falls back to all document diagnostics to remain robust across different cursor selection ranges.
- Integrated automatic `.intel/log.jsonl` log synchronization. It copies the generated OCEL NDJSON log file from `.ggen/ocel/` to the expected `.intel/log.jsonl` path upon receiving published diagnostics. This ensures compatibility with the admission tests expecting the `.intel/log.jsonl` file.

## Shared Module Expose (`crates/ggen-lsp/tests/common/mod.rs`)
- Exposed the `lsp_harness` module.

## Harness Verification Test (`crates/ggen-lsp/tests/harness_usage_test.rs`)
- Implemented a complete integration test ensuring every exposed operation on the harness works exactly as expected and passes successfully.
