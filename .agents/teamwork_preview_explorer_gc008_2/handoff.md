# Handoff Report — teamwork_preview_explorer_gc008_2

This report outlines the detailed exploration of invariants, checkpoints, dependencies, LSP 3.18 features, and anti-cheating static surveillance patterns.

## 1. Observation

- **Crate dependencies and imports**:
  - `crates/ggen-lsp/Cargo.toml:7`: `tower-lsp = "0.20"`
  - `crates/ggen-pack-clap-noun-verb/Cargo.toml:8`: `tower-lsp = "0.20.0"`
  - `crates/ggen-pack-tower-lsp-max/Cargo.toml:8`: `tower-lsp = "0.20.0"`
  - `crates/ggen-projection/Cargo.toml:30`: `tower-lsp = "0.20.0"`
  - `crates/gc005-wasm4pm-adapter/Cargo.toml:15`: `tower-lsp = "0.20"` (dev-dependency)
  - `Cargo.lock:2780`: `name = "tower-lsp"` version `0.20.0`
  - In `wasm4pm/crates/wasm4pm-lsp/Cargo.toml` and `wasm4pm-compat/wasm4pm-compat-lsp/Cargo.toml`, dependencies have been modified to point to `tower-lsp-max` via local path.

- **Compile-time Errors in LSP implementations under `tower-lsp-max`**:
  Running `cargo check -p wasm4pm-lsp` in `wasm4pm` yields:
  ```
  error[E0425]: cannot find type `Url` in this scope
    --> crates/wasm4pm-lsp/src/main.rs:68:45
     |
  68 |     async fn check_diagnostics(&self, uri: &Url, content: &str) {
     |                                             ^^^
  ...
  error[E0063]: missing field `offset_encoding` in initializer of `tower_lsp_max::lsp_types_max::InitializeResult`
    --> crates/wasm4pm-lsp/src/main.rs:15:12
     |
  15 |         Ok(InitializeResult {
     |            ^^^^^^^^^^^^^^^^ missing `offset_encoding`
  ...
  error[E0308]: mismatched types
     --> crates/wasm4pm-lsp/src/main.rs:53:39
      |
   53 |                           command: Some(Command {
      |  __________________________________----_^
  ...
  ```
  Running `cargo check -p wasm4pm-compat-lsp` in `wasm4pm-compat` yields:
  ```
  error[E0425]: cannot find type `Url` in this scope
    --> wasm4pm-compat-lsp/src/main.rs:46:26
     |
  46 |     files: Mutex<HashMap<Url, String>>,
     |                          ^^^
  ...
  error[E0063]: missing field `offset_encoding` in initializer of `tower_lsp_max::lsp_types_max::InitializeResult`
     --> wasm4pm-compat-lsp/src/main.rs:179:12
      |
  179 |         Ok(InitializeResult {
      |            ^^^^^^^^^^^^^^^^ missing `offset_encoding`
  ```

- **LSP 3.18 Features**:
  - `LanguageServer::inline_completion` exists in `src/language_server.rs:531`.
  - `LanguageServer::text_document_content` exists in `src/language_server.rs:540`.
  - `Client::folding_range_refresh` exists in `src/service/client/lsp_methods.rs:305`.
  - `LanguageServer::ranges_formatting` exists in `src/language_server.rs:559`.
  - Upgraded types `InitializeResult` (demanding `offset_encoding`), `RelativePattern`, `GlobPattern`, `WorkspaceEdit` metadata exist in `lsp_3_18.rs` and `lsp-types-max`.
  - Extensive tests exist in `tests/test_lsp318_capabilities.rs` proving dispatcher execution.

- **Forbidden static surveillance patterns**:
  - `std::fs::write(&path, &content)` is present in `tower-lsp-max/src/workspace_edit.rs:44` under `apply_workspace_edit`.
  - `std::fs::write(&path, old_content)` is present in `tower-lsp-max/src/language_server/impls/repair.rs:164` (for rolling back changes).
  - `std::fs::read_to_string(&path)` is present in `tower-lsp-max/src/workspace_edit.rs:19` and `tower-lsp-max/src/language_server/impls/repair.rs:132`.
  - Other forbidden patterns (`wasm4pm.bind_receipt`, `bind_conformance_receipt`, mutating `execute_command`, `WorkspaceEdit` receipt binding, `ocel.events.push` in adapter, hardcoded fitness returns, shadow crates) are **ABSENT** from active implementation and negatively asserted in tests.

---

## 2. Logic Chain

1. In the modified codebase, `wasm4pm-lsp` and `wasm4pm-compat-lsp` have been switched to depend on `tower-lsp-max`.
2. This switch causes compiler type errors because `Url` is not imported/exposed correctly (it needs to be imported or changed to `Uri`), `InitializeResult` demands `offset_encoding` (which was not present in the legacy `tower-lsp` initializer blocks), and `std::process::Command` conflicts/shadows the LSP `Command` structure.
3. As a result, compilation is currently broken and blocking.
4. All 15 LSP 3.18 features are fully supported by `tower-lsp-max` and its model. None are refused by law or blocked, as they are standard editor features.
5. Surveillance pattern checks show that file mutation paths (`std::fs::write` / `read_to_string` to apply repairs/rollbacks) exist in `tower-lsp-max/src/workspace_edit.rs` and `repair.rs` (under control-plane commands), but no other illegal mutation or cheating paths are present.

---

## 3. Caveats

- We assumed that `TOWER_LSP_MAX_DB_PATH` is correctly configured or resolves to temp directory for tests.
- We did not implement any fixes since this is a read-only investigation.

---

## 4. Conclusion

Replacing standard `tower-lsp` with `tower-lsp-max` is structurally feasible and re-exports compatible `lsp_types` aliases. However, it is **currently blocked** by compiler errors in `wasm4pm-lsp` (6 errors) and `wasm4pm-compat-lsp` (4 errors). Resolving this requires modifying imports (importing `url::Url` or using `Uri`), satisfying `offset_encoding` in `InitializeResult` initializers, and qualifying the `Command` struct.

---

## 5. Verification Method

To verify the compile errors:
1. Run `cargo check -p wasm4pm-lsp` in `/Users/sac/wasm4pm`.
2. Run `cargo check -p wasm4pm-compat-lsp` in `/Users/sac/wasm4pm-compat`.
Both commands will fail with the exact errors documented above.
