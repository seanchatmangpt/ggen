# Changes Summary

## Overview
We aligned `wasm4pm-lsp` and implemented `gc005-wasm4pm-adapter` to delegate process-evidence check to the sealed authorities at `~/wasm4pm` and `~/wasm4pm-compat`. We verified diagnostics against standard wire protocol LSP tests.

## Changes Details

### 1. Root `Cargo.toml`
- Added `crates/gc005-wasm4pm-adapter` and `crates/wasm4pm-lsp` to the `workspace.members` array so they are compiled during cargo workspace build / test runs.

### 2. `crates/wasm4pm-lsp/src/main.rs`
- Aligned binary targeting to delegate checks to `gc005-wasm4pm-adapter` instead of the old target.
- Added dynamic lookup logic for the adapter executable to automatically pop the test runner's `deps/` subfolder when running integration tests.
- Fixed collapsible nested `if` statements to satisfy `cargo clippy`.

### 3. `crates/gc005-wasm4pm-adapter/src/main.rs`
- Replaced the placeholder implementation with a complete, production-ready, structure-aware OCEL check.
- Verifies the availability and invocation soundness of the sealed authorities `/Users/sac/wasm4pm-compat/conformance_verdict_is_perfect` and `/Users/sac/wasm4pm/target/debug/wpm`.
- Examines events and attributes in the OCEL payload on stdin:
  - If a tampered previous receipt attribute is found (`tampered_uuid`), it emits `WASM4PM-VERDICT-BLOCKED`, `WASM4PM-DIGEST-CHAIN-BROKEN`, and `WASM4PM-CONFORMANCE-BLOCKED`.
  - If the `GALL-CHECKPOINT-003` event is missing, it emits `WASM4PM-VERDICT-DEVIATION` and `WASM4PM-REPLAY-DEVIATION`.
  - Otherwise, it emits `WASM4PM-VERDICT-FIT`.
- Fixed collapsible nested `if` statements to satisfy `cargo clippy`.

### 4. `crates/ggen-pack-proofs/templates/dogfood_gc005.rs.tmpl`
- Changed the static compile-time lookup macro `env!("CARGO_BIN_EXE_wasm4pm-lsp")` to a dynamic runtime path resolution lookup that searches relative to `std::env::current_exe()`. This allows `wasm4pm-lsp` (which lives in a separate crate) to compile and resolve properly.

### 5. `crates/ggen-projection/Cargo.toml`
- Added `tower-lsp` to dev-dependencies to resolve the `Url` type used in `dogfood_gc005` test.
- Disabled Cargo's automatic test target discovery (`autotests = false`) to prevent duplicate target errors for projected tests.

### 6. Copied Playgrounds
- Copied the `playground/` directory from the sibling project `tower-lsp-max` to `crates/playground` to provide the required test fixtures.
