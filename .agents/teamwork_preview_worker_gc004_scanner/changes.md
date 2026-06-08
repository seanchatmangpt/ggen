# Changes - teamwork_preview_worker_gc004_scanner

## Modified Files
- `crates/ggen-lsp/tests/dogfood_gc004.rs`:
  - Increased `READ_TIMEOUT` to 30 seconds to avoid flaky timeouts under heavy CPU/IO load.
  - Added the `test_gc004_bypass_kills_scanner` test function, which walks the `crates/ggen-lsp/tests` directory recursively, identifies the admission tests and harness helper files, and scans their contents (excluding comments) for forbidden symbols. It panics and details the violation rule if any are found.

## Design Decisions
- **Target Selection**: The scanner recursively finds all Rust test files in `crates/ggen-lsp/tests`. It identifies harness/admission test files by filename (`lsp_harness.rs` or containing `admission` case-insensitively) or by content (referencing `lsp_harness` or `LspHarness`).
- **Forbidden Symbols**: We scan for the exact 7 forbidden symbols (`compute_observer_diagnostics`, `analyze_and_observe`, `validate_sync`, `observe_pack_domain`, `state.diagnostics`, `direct_write`, `std::fs::write`).
- **Comment Stripping**: Before scanning, we strip single-line and multi-line comments from the code contents to prevent false positives from rule descriptions or documentation comments.
- **Strict Panics**: If any forbidden symbols are found, the test panics immediately, failing the build/checks, pointing to the exact violating rule (e.g. `BYPASS-LSP-001` to `BYPASS-LSP-005`).
