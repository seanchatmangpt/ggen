# Investigation and Exploration Report

This report outlines the findings from the exploration of `/Users/sac/ggen`, `/Users/sac/tower-lsp-max`, `/Users/sac/wasm4pm`, and `/Users/sac/wasm4pm-compat`.

## 1. plain `tower-lsp` or `tower_lsp` occurrences

We scanned the workspaces for any occurrence of plain `tower-lsp` or `tower_lsp` in `Cargo.toml`, Rust source files, and `Cargo.lock` files.

### 1.1 In `/Users/sac/ggen`:
- `Cargo.toml` (Line 35): `"crates/ggen-pack-tower-lsp-max",`
- `crates/ggen-lsp/Cargo.toml` (Line 7): `tower-lsp = "0.20"`
- `crates/ggen-pack-clap-noun-verb/Cargo.toml` (Line 8): `tower-lsp = "0.20.0"`
- `crates/ggen-pack-tower-lsp-max/Cargo.toml` (Line 8): `tower-lsp = "0.20.0"`
- `crates/ggen-projection/Cargo.toml` (Line 30): `tower-lsp = "0.20.0"`
- Various test files (`ggen_tpl_001_did_close_clear.rs`, `ggen_tpl_001_living_loop.rs`, `ggen_tpl_001_stale_clear.rs`, `lsp_protocol_test.rs`, `lsp_sabotage_test.rs`) contain references to `tower-lsp` in comments.

### 1.2 In `/Users/sac/tower-lsp-max`:
- `crates/gc005-wasm4pm-adapter/Cargo.toml` (Line 15): `tower-lsp = "0.20"` (dev-dependency)
- `Cargo.lock` (Line 2780): Package `tower-lsp` version `0.20.0` is registered.
- Various code comments referencing standard `tower-lsp` repository and documentation.

### 1.3 In `/Users/sac/wasm4pm`:
- `crates/wasm4pm-lsp/Cargo.toml` (Line 8): Previously `tower-lsp = "0.20.0"`, currently modified in workspace to `tower-lsp-max = { path = "../../../tower-lsp-max", features = ["proposed"] }`.
- `crates/wasm4pm-lsp/src/main.rs`: Previously used `tower_lsp::*`, currently updated to use `tower_lsp_max::*`.

### 1.4 In `/Users/sac/wasm4pm-compat`:
- `wasm4pm-compat-lsp/Cargo.toml` (Line 17): Previously `tower-lsp = "0.20"`, currently modified in workspace to `tower-lsp-max = { path = "../../tower-lsp-max", features = ["proposed"] }`.
- `wasm4pm-compat-lsp/src/main.rs`: Previously used `tower_lsp::*`, currently updated to use `tower_lsp_max::*`.

---

## 2. Feasibility of replacing plain `tower-lsp` with `tower-lsp-max`

### 2.1 Feasibility
Replacing standard `tower-lsp` with `tower-lsp-max` is feasible structurally since both expose the same `LanguageServer` trait and standard RPC routing architecture. Moreover, `tower-lsp-max` explicitly re-exports its upgraded types crate (`lsp_types_max`) as `lsp_types` (`pub use lsp_types_max as lsp_types;` in `tower-lsp-max/src/lib.rs`). This allows files importing `tower_lsp::lsp_types::*` to remain mostly compatible.

### 2.2 Current Blocking Issues
However, replacing plain `tower-lsp` with `tower-lsp-max` is **currently blocking** due to syntax and type compatibility errors in the modified files:

1. **`Url` type mismatch / missing scope in `wasm4pm-lsp` & `wasm4pm-compat-lsp`**:
   `lsp_types_max` uses `Uri` (wrapping `fluent_uri::Uri`) rather than `url::Url` as standard `lsp-types` did. Consequently, the LSP main files fail to resolve the type `Url` which is not imported or exported in the same way.
   * `wasm4pm-lsp/src/main.rs:68:45`: `cannot find type Url in this scope`
   * `wasm4pm-compat-lsp/src/main.rs:46,59,72`: `cannot find type Url in this scope`

2. **Missing `offset_encoding` field in `InitializeResult`**:
   `InitializeResult` under `lsp-types-max` requires the `offset_encoding` field which was not required by standard `tower-lsp`.
   * `wasm4pm-lsp/src/main.rs:15:12`: `missing field offset_encoding in initializer of tower_lsp_max::lsp_types_max::InitializeResult`
   * `wasm4pm-compat-lsp/src/main.rs:179:12`: `missing field offset_encoding in initializer of tower_lsp_max::lsp_types_max::InitializeResult`

3. **Shadowing of `Command` struct in `wasm4pm-lsp`**:
   * `wasm4pm-lsp/src/main.rs:53:39`: Shadows `lsp_types::Command` with `std::process::Command` due to import `use std::process::{Command, Stdio};` on line 5. Under the strict checks of `tower-lsp-max`, this results in compile errors.

Because of these errors, neither package compiles successfully, blocking execution and dogfood test suites.

---

## 3. Investigation of the 15 LSP 3.18 features

We investigated the 15 features defined in the meta-model under the workspaces.

| Feature ID | Feature Name | Status | Location of tests / code |
|---|---|---|---|
| **LSP318-001** | inline completions | **SUPPORTED** | `LanguageServer::inline_completion` in `src/language_server.rs:531` and default impl in `src/language_server/impls/text_document.rs:165`. Tested in `tests/test_lsp318_capabilities.rs:561`. |
| **LSP318-002** | dynamic text document content | **SUPPORTED** | `LanguageServer::text_document_content` in `src/language_server.rs:540`. Tested in `tests/test_lsp318_capabilities.rs:1540`. |
| **LSP318-003** | folding range refresh | **SUPPORTED** | `Client::folding_range_refresh` in `src/service/client/lsp_methods.rs:305`. Tested in `tests/test_lsp318_capabilities.rs:821`. |
| **LSP318-004** | multi-range formatting | **SUPPORTED** | `LanguageServer::ranges_formatting` in `src/language_server.rs:559`. Tested in `tests/test_lsp318_capabilities.rs:774`. |
| **LSP318-005** | snippets in workspace edits | **SUPPORTED** | Supported in type definition `lsp_3_18.rs` and applied via `apply_workspace_edit` in `src/workspace_edit.rs`. |
| **LSP318-006** | relative patterns in document filters | **SUPPORTED** | Supported via `GlobPattern` and `RelativePattern` types defined in `lsp_3_18.rs:112`. |
| **LSP318-007** | relative patterns in notebook filters | **SUPPORTED** | Supported via `NotebookDocumentFilter` type in `lsp_3_18.rs:120`. |
| **LSP318-008** | code action kind documentation | **SUPPORTED** | Supported via `CodeActionKind` metadata in `lsp_3_18.rs`. |
| **LSP318-009** | activeParameter on SignatureHelp | **SUPPORTED** | Supported via `SignatureHelp` metadata in `lsp_3_18.rs`. |
| **LSP318-010** | command tooltips | **SUPPORTED** | Supported via `Command` tooltip field in `lsp_3_18.rs`. |
| **LSP318-011** | workspace edit metadata | **SUPPORTED** | Supported via `WorkspaceEdit` metadata field in `lsp_3_18.rs`. |
| **LSP318-012** | snippets in text document edits | **SUPPORTED** | Supported via `TextDocumentEdit` type in `lsp_3_18.rs`. |
| **LSP318-013** | debug message kind | **SUPPORTED** | `MessageType::Debug` variant defined in `lsp_3_18.rs:503`. |
| **LSP318-014** | code lens resolve properties | **SUPPORTED** | Supported via `CodeLens` type and `codeLens/resolve` RPC method in `language_server.rs:177`. |
| **LSP318-015** | completionList.applyKind | **SUPPORTED** | Supported via `CompletionList` properties in `lsp_3_18.rs`. |

All 15 features are **SUPPORTED** by `tower-lsp-max` through its meta-model re-exports (`lsp_3_18.rs`) and server trait/client methods. None are refused by law or blocked, as they are standard editor features and do not violate strict process boundary rules.

---

## 4. Anti-Cheating Static Surveillance list

We scanned the codebases for forbidden patterns.

1. **`wasm4pm.bind_receipt`**: **ABSENT** from active implementation. Test files in `ggen` negatively assert its absence (e.g. `assert!(!lsp_src.contains("\"wasm4pm.bind_receipt\""), "Invented command wasm4pm.bind_receipt is forbidden");`).
2. **`bind_conformance_receipt`**: **ABSENT** from active implementation. Checked by `dogfood_gc008.rs` negatively (e.g. `assert!(!adapter_src.contains("pub fn bind_conformance_receipt"), "bind_conformance_receipt found in adapter; adapter cannot authorize mutation");`).
3. **`execute_command` mutation path**: **ABSENT**. The `wasm4pm-lsp` crate does not implement `execute_command`. The `wasm4pm-compat-lsp` implements `execute_command` only for non-mutating query commands (`explainRefusal`, `listRefusalLaws`).
4. **`WorkspaceEdit` receipt binding**: **ABSENT**. Workspace edits are used only for text-editing quickfixes in `wasm4pm-compat-lsp`, not for receipt binding.
5. **`std::fs::write` or `tokio::fs::write` in LSP mutation path**: **PRESENT**.
   - `std::fs::write(&path, &content)` is present in `tower-lsp-max/src/workspace_edit.rs:44` under `apply_workspace_edit`.
   - `std::fs::write(&path, old_content)` is present in `tower-lsp-max/src/language_server/impls/repair.rs:164` (for rolling back transactions).
   - No occurrences of `tokio::fs::write` in the active LSP path.
6. **`std::fs::read_to_string` in receipt-binding path**: **PRESENT** but only for backups and edits:
   - `std::fs::read_to_string(&path)` is present in `tower-lsp-max/src/workspace_edit.rs:19` (to read the file before editing it) and `tower-lsp-max/src/language_server/impls/repair.rs:132` (to make a backup copy of the file before applying the transaction).
   - In `tower-lsp-max/src/gate.rs`, it reads security/auth receipts for verification, not binding.
7. **`ocel.events.push` in adapter**: **ABSENT**. Negatively asserted by `dogfood_gc008.rs:29`.
8. **manual FIT / ADMITTED returns, v1.0.0 or version = "1.0.0"**: **ABSENT**. No fake or hardcoded fitness score or version returns are present in the active adapter.
9. **fake shadow crates (crates/wasm4pm, crates/wasm4pm-lsp, crates/wasm4pm-compat)**: **ABSENT**.
