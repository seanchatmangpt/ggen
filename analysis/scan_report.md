# Forbidden Patterns Integrity Scan Report

This report presents the findings of a scan across all active workspaces for forbidden patterns.

## Workspaces Scanned
- **ggen**: `/Users/sac/ggen`
- **wasm4pm**: `/Users/sac/wasm4pm`
- **wasm4pm-compat**: `/Users/sac/wasm4pm-compat`
- **tower-lsp-max**: `/Users/sac/tower-lsp-max`

## 1. Shadow Crates
✓ No forbidden shadow crates found.

## 2. Forbidden Cargo Crate Versions (`version = '1.0.0'`)
✓ No forbidden version constraints found.

## 3. wasm4pm.bind_receipt Occurrences
### Violations (LSP or source code context)
✓ No violations of `wasm4pm.bind_receipt` found in active source code paths.

### References (Tests, templates, or documentation)
<details>
<summary>Click to view references</summary>

| Workspace | File | Line | Content |
|---|---|---|---|
| ggen | `ORIGINAL_REQUEST.md` | 2194 | `Prove that `conformance-receipt.bind` is admitted by the actual CLAP noun/verb authority, while `...` |
| ggen | `ORIGINAL_REQUEST.md` | 2291 | `CLAPValidate(command=\text{wasm4pm.bind_receipt}) = REFUSED` |
| ggen | `ORIGINAL_REQUEST.md` | 2387 | `* `wasm4pm.bind_receipt`` |
| ggen | `ORIGINAL_REQUEST.md` | 2528 | `3. Refuse `wasm4pm.bind_receipt`.` |
| ggen | `ORIGINAL_REQUEST.md` | 2693 | `CLAPValidate(command="wasm4pm.bind_receipt") = REFUSED` |
| ggen | `ORIGINAL_REQUEST.md` | 2823 | `* `wasm4pm.bind_receipt`` |
| ggen | `ORIGINAL_REQUEST.md` | 2956 | `Prove that `conformance-receipt.bind` is admitted by the actual CLAP noun/verb authority, while `...` |
| ggen | `ORIGINAL_REQUEST.md` | 2962 | `CLAPValidate(command="wasm4pm.bind_receipt") = REFUSED` |
| ggen | `ORIGINAL_REQUEST.md` | 3014 | `- `wasm4pm.bind_receipt`, `bind_conformance_receipt`, `execute_command` mutation path` |
| ggen | `crates/ggen-projection/tests/dogfood_clap_command_route.rs` | 19 | `if content.contains("wasm4pm.bind_receipt") {` |
| ggen | `crates/ggen-projection/tests/dogfood_clap_command_route.rs` | 20 | `violations.push(format!("Forbidden wasm4pm.bind_receipt in wasm4pm-lsp {:?}", path));` |
| ggen | `crates/ggen-projection/tests/dogfood_gc008_route.rs` | 35 | `// a. wasm4pm.bind_receipt is refused` |
| ggen | `crates/ggen-projection/tests/dogfood_gc008_route.rs` | 39 | `assert!(!output.status.success(), "wasm4pm.bind_receipt should be refused by CLAP noun/verb autho...` |
| ggen | `crates/ggen-pack-clap-noun-verb/tests/dogfood_clap_command_route.rs` | 19 | `if content.contains("wasm4pm.bind_receipt") {` |
| ggen | `crates/ggen-pack-clap-noun-verb/tests/dogfood_clap_command_route.rs` | 20 | `violations.push(format!("Forbidden wasm4pm.bind_receipt in wasm4pm-lsp {:?}", path));` |
| ggen | `crates/ggen-pack-clap-noun-verb/tests/dogfood_gc008_b_c.rs` | 115 | `"command": "wasm4pm.bind_receipt",` |
| ggen | `crates/ggen-pack-clap-noun-verb/tests/dogfood_gc008_route.rs` | 35 | `// a. wasm4pm.bind_receipt is refused` |
| ggen | `crates/ggen-pack-clap-noun-verb/tests/dogfood_gc008_route.rs` | 39 | `assert!(!output.status.success(), "wasm4pm.bind_receipt should be refused by CLAP noun/verb autho...` |
| ggen | `crates/ggen-pack-proofs/templates/dogfood_gc008_route.rs.tmpl` | 35 | `// a. wasm4pm.bind_receipt is refused` |
| ggen | `crates/ggen-pack-proofs/templates/dogfood_gc008_route.rs.tmpl` | 39 | `assert!(!output.status.success(), "wasm4pm.bind_receipt should be refused by CLAP noun/verb autho...` |
| ggen | `crates/ggen-pack-proofs/templates/dogfood_gc008.rs.tmpl` | 32 | `assert!(!lsp_src.contains("\"wasm4pm.bind_receipt\""), "Invented command wasm4pm.bind_receipt is ...` |
| ggen | `crates/ggen-pack-proofs/templates/dogfood_gc008_b_c.rs.tmpl` | 115 | `"command": "wasm4pm.bind_receipt",` |
| ggen | `crates/ggen-pack-proofs/templates/dogfood_clap_command_route.rs.tmpl` | 19 | `if content.contains("wasm4pm.bind_receipt") {` |
| ggen | `crates/ggen-pack-proofs/templates/dogfood_clap_command_route.rs.tmpl` | 20 | `violations.push(format!("Forbidden wasm4pm.bind_receipt in wasm4pm-lsp {:?}", path));` |
| tower-lsp-max | `crates/gc005-wasm4pm-adapter/tests/dogfood_gc008.rs` | 37 | `assert!(!lsp_src.contains("\"wasm4pm.bind_receipt\""), "Invented command wasm4pm.bind_receipt is ...` |

</details>

## 4. execute_command in LSP Source Files
| Workspace | File | Line | Content |
|---|---|---|---|
| ggen | `examples/clap-noun-verb-lsp/src/server.rs` | 77 | `execute_command_provider: Some(ExecuteCommandOptions {` |
| wasm4pm | `crates/pm4py-lsp/src/lib.rs` | 346 | `execute_command_provider: Some(ExecuteCommandOptions {` |
| wasm4pm | `crates/pm4py-lsp/src/lib.rs` | 446 | `async fn execute_command(&self, params: ExecuteCommandParams) -> Result<Option<Value>> {` |
| wasm4pm-compat | `wasm4pm-compat-lsp/src/main.rs` | 189 | `execute_command_provider: Some(ExecuteCommandOptions {` |
| wasm4pm-compat | `wasm4pm-compat-lsp/src/main.rs` | 296 | `async fn execute_command(&self, params: ExecuteCommandParams) -> Result<Option<Value>> {` |
| tower-lsp-max | `crates/playground/src/capabilities.rs` | 92 | `execute_command_provider: Some(ExecuteCommandOptions {` |
| tower-lsp-max | `crates/playground/src/handlers/completions/table/workspace_window.rs` | 3 | `fn_name: "execute_command",` |
| tower-lsp-max | `crates/playground/src/handlers/completions/table/workspace_window.rs` | 4 | `lsp_method: "workspace/executeCommand",` |
| tower-lsp-max | `crates/playground/src/handlers/completions/table/workspace_window.rs` | 7 | `capability_field: Some("execute_command_provider"),` |
| tower-lsp-max | `crates/playground/src/handlers/completions/table/capability_fields.rs` | 130 | `"execute_command_provider",` |
| tower-lsp-max | `crates/playground/src/handlers/completions/table/capability_fields.rs` | 132 | `&["execute_command"],` |
| tower-lsp-max | `crates/tower-lsp-max-client/src/server_handle.rs` | 201 | `pub async fn execute_command(` |
| tower-lsp-max | `examples/clap-noun-verb-lsp/src/server.rs` | 77 | `execute_command_provider: Some(ExecuteCommandOptions {` |
| tower-lsp-max | `tower-lsp-max-protocol/src/lsp_3_18.rs` | 4012 | `pub execute_command_options_base: ExecuteCommandOptions,` |
| tower-lsp-max | `tower-lsp-max-protocol/src/lsp_3_18.rs` | 5172 | `#[serde(rename = "executeCommandProvider")]` |
| tower-lsp-max | `tower-lsp-max-protocol/src/lsp_3_18.rs` | 5174 | `pub execute_command_provider: Option<ExecuteCommandOptions>,` |
| tower-lsp-max | `tower-lsp-max-protocol/src/lsp_3_18.rs` | 6464 | `///Capabilities specific to the `workspace/executeCommand` request.` |
| tower-lsp-max | `tower-lsp-max-protocol/src/lsp_3_18.rs` | 6465 | `#[serde(rename = "executeCommand")]` |
| tower-lsp-max | `tower-lsp-max-protocol/src/lsp_3_18.rs` | 6467 | `pub execute_command: Option<ExecuteCommandClientCapabilities>,` |
| tower-lsp-max | `tower-lsp-max-protocol/src/lsp_3_18.rs` | 9146 | `const METHOD: &'static str = "workspace/executeCommand";` |
| tower-lsp-max | `src/language_server.rs` | 400 | `/// Handler for the `execute_command` endpoint.` |
| tower-lsp-max | `src/language_server.rs` | 401 | `#[rpc(name = "workspace/executeCommand")]` |
| tower-lsp-max | `src/language_server.rs` | 402 | `async fn execute_command(&self, params: ExecuteCommandParams) -> Result<Option<Value>> {` |
| tower-lsp-max | `src/language_server.rs` | 403 | `impls::execute_command(params).await` |
| tower-lsp-max | `src/composition.rs` | 322 | `| "workspace/executeCommand" => CompositionStrategy::TransactionalEditGate,` |
| tower-lsp-max | `src/composition.rs` | 2527 | `async fn execute_command(&self, params: ExecuteCommandParams) -> Result<Option<Value>> {` |
| tower-lsp-max | `src/composition.rs` | 2528 | `self.route_request("workspace/executeCommand", params).await.map(|o| o.flatten())` |
| tower-lsp-max | `src/composition.rs` | 3237 | `"workspace/executeCommand",` |
| tower-lsp-max | `src/language_server/impls/text_document.rs` | 132 | `pub async fn execute_command(params: ExecuteCommandParams) -> Result<Option<Value>> {` |

## 5. WorkspaceEdit in LSP Source Files (Direct Receipt Binding)
| Workspace | File | Line | Content |
|---|---|---|---|
| ggen | `crates/ggen-lsp/src/server.rs` | 322 | `/// concrete `WorkspaceEdit`. Only routes with a computable edit (no unfilled` |
| ggen | `crates/ggen-lsp/src/server.rs` | 392 | `async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {` |
| ggen | `crates/ggen-lsp/src/analyzers/rdf_analyzer.rs` | 10 | `WorkspaceEdit,` |
| ggen | `crates/ggen-lsp/src/analyzers/rdf_analyzer.rs` | 275 | `pub fn rename_symbol(&self, position: Position, new_name: &str) -> Option<WorkspaceEdit> {` |
| ggen | `crates/ggen-lsp/src/analyzers/rdf_analyzer.rs` | 302 | `Some(WorkspaceEdit {` |
| ggen | `crates/ggen-lsp/src/analyzers/tera_analyzer.rs` | 11 | `InlayHint, Location, NumberOrString, Position, Range, SymbolKind, TextEdit, WorkspaceEdit,` |
| ggen | `crates/ggen-lsp/src/analyzers/tera_analyzer.rs` | 254 | `pub fn rename_symbol(&self, _position: Position, _new_name: &str) -> Option<WorkspaceEdit> {` |
| ggen | `crates/ggen-lsp/src/analyzers/mod.rs` | 14 | `WorkspaceEdit,` |
| ggen | `crates/ggen-lsp/src/analyzers/mod.rs` | 429 | `pub fn rename_symbol(&self, position: Position, new_name: &str) -> Option<WorkspaceEdit> {` |
| ggen | `crates/ggen-lsp/src/analyzers/toml_analyzer.rs` | 11 | `MarkupContent, MarkupKind, Position, Range, SymbolKind, TextEdit, WorkspaceEdit,` |
| ggen | `crates/ggen-lsp/src/analyzers/toml_analyzer.rs` | 257 | `pub fn rename_symbol(&self, _position: Position, _new_name: &str) -> Option<WorkspaceEdit> {` |
| ggen | `crates/ggen-lsp/src/analyzers/sparql_analyzer.rs` | 13 | `Location, Position, Range, SymbolKind, TextEdit, WorkspaceEdit,` |
| ggen | `crates/ggen-lsp/src/analyzers/sparql_analyzer.rs` | 164 | `pub fn rename_symbol(&self, _position: Position, _new_name: &str) -> Option<WorkspaceEdit> {` |
| ggen | `crates/ggen-lsp/src/route/edit.rs` | 6 | `//! `WorkspaceEdit` and the agent `RoutePlan`, so both apply identical repairs.` |
| ggen | `crates/ggen-lsp/src/route/edit.rs` | 9 | `use tower_lsp::lsp_types::{Position, Range, TextEdit, Url, WorkspaceEdit};` |
| ggen | `crates/ggen-lsp/src/route/edit.rs` | 74 | `/// Build a `WorkspaceEdit` applying all of a route's steps to `uri`/`doc`.` |
| ggen | `crates/ggen-lsp/src/route/edit.rs` | 78 | `) -> WorkspaceEdit {` |
| ggen | `crates/ggen-lsp/src/route/edit.rs` | 85 | `WorkspaceEdit {` |
| wasm4pm | `crates/pm4py-lsp/src/lib.rs` | 484 | `let workspace_edit = WorkspaceEdit {` |
| wasm4pm-compat | `wasm4pm-compat-lsp/src/main.rs` | 254 | `edit: Some(WorkspaceEdit {` |
| wasm4pm-compat | `wasm4pm-compat-lsp/src/main.rs` | 276 | `edit: Some(WorkspaceEdit {` |
| tower-lsp-max | `crates/playground/src/bin/dogfood_harness.rs` | 347 | `async fn rename(&self, params: RenameParams) -> tower_lsp_max::jsonrpc::Result<Option<WorkspaceEdit>> {` |
| tower-lsp-max | `crates/playground/src/bin/dogfood_harness.rs` | 381 | `Ok(Some(WorkspaceEdit {` |
| tower-lsp-max | `crates/playground/src/handlers/hover.rs` | 111 | `- `apply_edit(WorkspaceEdit)` — `workspace/applyEdit`"` |
| tower-lsp-max | `crates/playground/src/handlers/hover.rs` | 152 | `"WorkspaceEdit" => {` |
| tower-lsp-max | `crates/playground/src/handlers/hover.rs` | 153 | `"## `WorkspaceEdit`\n\nCollection of `TextEdit`s across multiple documents, optionally \` |
| tower-lsp-max | `crates/playground/src/handlers/hover.rs` | 179 | `- `edit: Option<WorkspaceEdit>` — changes to apply\n\` |
| tower-lsp-max | `crates/playground/src/handlers/completions/table/workspace_window.rs` | 46 | `return_type: "Result<Option<WorkspaceEdit>>",` |
| tower-lsp-max | `crates/playground/src/handlers/completions/table/workspace_window.rs` | 62 | `return_type: "Result<Option<WorkspaceEdit>>",` |
| tower-lsp-max | `crates/playground/src/handlers/completions/table/workspace_window.rs` | 78 | `return_type: "Result<Option<WorkspaceEdit>>",` |
| tower-lsp-max | `crates/playground/src/handlers/completions/table/editing_diagnostics.rs` | 54 | `return_type: "Result<Option<WorkspaceEdit>>",` |
| tower-lsp-max | `crates/playground/src/handlers/diagnostics/actions.rs` | 60 | `edit: Some(WorkspaceEdit {` |
| tower-lsp-max | `crates/playground/src/handlers/diagnostics/actions.rs` | 106 | `edit: Some(WorkspaceEdit {` |
| tower-lsp-max | `crates/playground/src/handlers/diagnostics/actions.rs` | 308 | `fn workspace_edit_insert(uri: &Uri, pos: Position, text: &str) -> WorkspaceEdit {` |
| tower-lsp-max | `crates/playground/src/handlers/diagnostics/actions.rs` | 318 | `WorkspaceEdit {` |
| tower-lsp-max | `crates/tower-lsp-max-client/src/client.rs` | 94 | `params: ApplyWorkspaceEditParams,` |
| tower-lsp-max | `crates/tower-lsp-max-client/src/client.rs` | 95 | `) -> Result<ApplyWorkspaceEditResponse, ClientError> {` |
| tower-lsp-max | `crates/tower-lsp-max-client/src/client.rs` | 97 | `Ok(ApplyWorkspaceEditResponse {` |
| tower-lsp-max | `crates/tower-lsp-max-client/src/server_handle.rs` | 154 | `) -> Result<Option<WorkspaceEdit>, ClientError> {` |
| tower-lsp-max | `tower-lsp-max-protocol/src/lsp_3_18.rs` | 2401 | `pub struct WorkspaceEdit {` |
| tower-lsp-max | `tower-lsp-max-protocol/src/lsp_3_18.rs` | 3719 | `pub edit: Option<WorkspaceEdit>,` |
| tower-lsp-max | `tower-lsp-max-protocol/src/lsp_3_18.rs` | 4017 | `pub struct ApplyWorkspaceEditParams {` |
| tower-lsp-max | `tower-lsp-max-protocol/src/lsp_3_18.rs` | 4024 | `pub edit: WorkspaceEdit,` |
| tower-lsp-max | `tower-lsp-max-protocol/src/lsp_3_18.rs` | 4029 | `pub metadata: Option<WorkspaceEditMetadata>,` |
| tower-lsp-max | `tower-lsp-max-protocol/src/lsp_3_18.rs` | 4033 | `@since 3.17 renamed from ApplyWorkspaceEditResponse*/` |
| tower-lsp-max | `tower-lsp-max-protocol/src/lsp_3_18.rs` | 4036 | `pub struct ApplyWorkspaceEditResult {` |
| tower-lsp-max | `tower-lsp-max-protocol/src/lsp_3_18.rs` | 5914 | `pub struct WorkspaceEditMetadata {` |
| tower-lsp-max | `tower-lsp-max-protocol/src/lsp_3_18.rs` | 6449 | `///Capabilities specific to `WorkspaceEdit`s.` |
| tower-lsp-max | `tower-lsp-max-protocol/src/lsp_3_18.rs` | 6452 | `pub workspace_edit: Option<WorkspaceEditClientCapabilities>,` |
| tower-lsp-max | `tower-lsp-max-protocol/src/lsp_3_18.rs` | 6947 | `pub struct WorkspaceEditClientCapabilities {` |
| tower-lsp-max | `tower-lsp-max-protocol/src/lsp_3_18.rs` | 6948 | `///The client supports versioned document changes in `WorkspaceEdit`s` |
| tower-lsp-max | `tower-lsp-max-protocol/src/lsp_3_18.rs` | 6983 | `/**Whether the client supports `WorkspaceEditMetadata` in `WorkspaceEdit`s.` |
| tower-lsp-max | `tower-lsp-max-protocol/src/lsp_3_18.rs` | 8858 | `type Result = Option<WorkspaceEdit>;` |
| tower-lsp-max | `tower-lsp-max-protocol/src/lsp_3_18.rs` | 8864 | `type Result = Option<WorkspaceEdit>;` |
| tower-lsp-max | `tower-lsp-max-protocol/src/lsp_3_18.rs` | 8870 | `type Result = Option<WorkspaceEdit>;` |
| tower-lsp-max | `tower-lsp-max-protocol/src/lsp_3_18.rs` | 9133 | `type Result = Option<WorkspaceEdit>;` |
| tower-lsp-max | `tower-lsp-max-protocol/src/lsp_3_18.rs` | 9148 | `pub struct ApplyWorkspaceEditRequest;` |
| tower-lsp-max | `tower-lsp-max-protocol/src/lsp_3_18.rs` | 9149 | `impl LspRequest for ApplyWorkspaceEditRequest {` |
| tower-lsp-max | `tower-lsp-max-protocol/src/lsp_3_18.rs` | 9150 | `type Params = ApplyWorkspaceEditParams;` |
| tower-lsp-max | `tower-lsp-max-protocol/src/lsp_3_18.rs` | 9151 | `type Result = ApplyWorkspaceEditResult;` |
| tower-lsp-max | `tower-lsp-max-protocol/src/lsp_3_18.rs` | 9224 | `ApplyWorkspaceEditRequest,` |
| tower-lsp-max | `src/diagnostics.rs` | 78 | `edit: Some(WorkspaceEdit {` |
| tower-lsp-max | `src/diagnostics.rs` | 167 | `edit: Some(WorkspaceEdit {` |
| tower-lsp-max | `src/diagnostics.rs` | 244 | `edit: Some(WorkspaceEdit {` |
| tower-lsp-max | `src/workspace_edit.rs` | 3 | `/// Applies a list of changes defined in a `WorkspaceEdit` to files in the local filesystem.` |
| tower-lsp-max | `src/workspace_edit.rs` | 6 | `pub fn apply_workspace_edit(edit: &lsp_types_max::WorkspaceEdit) -> Result<(), String> {` |
| tower-lsp-max | `src/language_server.rs` | 323 | `async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {` |
| tower-lsp-max | `src/language_server.rs` | 367 | `async fn will_create_files(&self, params: CreateFilesParams) -> Result<Option<WorkspaceEdit>> {` |
| tower-lsp-max | `src/language_server.rs` | 377 | `async fn will_rename_files(&self, params: RenameFilesParams) -> Result<Option<WorkspaceEdit>> {` |
| tower-lsp-max | `src/language_server.rs` | 387 | `async fn will_delete_files(&self, params: DeleteFilesParams) -> Result<Option<WorkspaceEdit>> {` |
| tower-lsp-max | `src/composition.rs` | 2499 | `async fn will_create_files(&self, params: CreateFilesParams) -> Result<Option<WorkspaceEdit>> {` |
| tower-lsp-max | `src/composition.rs` | 2507 | `async fn will_rename_files(&self, params: RenameFilesParams) -> Result<Option<WorkspaceEdit>> {` |
| tower-lsp-max | `src/composition.rs` | 2515 | `async fn will_delete_files(&self, params: DeleteFilesParams) -> Result<Option<WorkspaceEdit>> {` |
| tower-lsp-max | `src/composition.rs` | 2572 | `async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {` |
| tower-lsp-max | `src/language_server/impls/repair.rs` | 7 | `use lsp_types_max::{CodeAction, CodeActionKind, DiagnosticSeverity, TextEdit, WorkspaceEdit};` |
| tower-lsp-max | `src/language_server/impls/file_ops_ext.rs` | 7 | `pub async fn will_create_files(params: CreateFilesParams) -> Result<Option<WorkspaceEdit>> {` |
| tower-lsp-max | `src/language_server/impls/file_ops_ext.rs` | 18 | `pub async fn will_rename_files(params: RenameFilesParams) -> Result<Option<WorkspaceEdit>> {` |
| tower-lsp-max | `src/language_server/impls/file_ops_ext.rs` | 29 | `pub async fn will_delete_files(params: DeleteFilesParams) -> Result<Option<WorkspaceEdit>> {` |
| tower-lsp-max | `src/language_server/impls/text_document.rs` | 115 | `pub async fn rename(params: RenameParams) -> Result<Option<WorkspaceEdit>> {` |
| tower-lsp-max | `src/service/client/lsp_methods.rs` | 439 | `edit: WorkspaceEdit,` |
| tower-lsp-max | `src/service/client/lsp_methods.rs` | 440 | `) -> jsonrpc::Result<ApplyWorkspaceEditResponse> {` |
| tower-lsp-max | `src/service/client/lsp_methods.rs` | 441 | `use lsp_types_max::request::ApplyWorkspaceEdit;` |
| tower-lsp-max | `src/service/client/lsp_methods.rs` | 442 | `self.send_request::<ApplyWorkspaceEdit>(ApplyWorkspaceEditParams { edit, label: None })` |

## 6. std::fs::write in LSP Source Files
| Workspace | File | Line | Content |
|---|---|---|---|
| ggen | `crates/ggen-lsp/src/harness_index.rs` | 241 | `std::fs::write(p, content).expect("write");` |
| ggen | `crates/ggen-lsp/src/check.rs` | 668 | `fs::write(root.join(".specify/specs/feature.ttl"), "@prefix ex: <x> .").expect("w");` |
| ggen | `crates/ggen-lsp/src/check.rs` | 669 | `fs::write(root.join("ggen.toml"), "[project]\nname=\"x\"").expect("w");` |
| ggen | `crates/ggen-lsp/src/check.rs` | 670 | `fs::write(root.join("target/junk/ignored.ttl"), "@prefix ex: <x> .").expect("w");` |
| ggen | `crates/ggen-lsp/src/check.rs` | 671 | `fs::write(root.join("readme.md"), "# not a law surface").expect("w");` |
| ggen | `crates/ggen-lsp/src/check.rs` | 713 | `fs::write(root.join("row.tera"), r#"{{ row["title"] }}"#).expect("write template");` |
| ggen | `crates/ggen-lsp/src/check.rs` | 728 | `fs::write(root.join("ggen.toml"), manifest).expect("write manifest");` |
| ggen | `crates/ggen-lsp/src/check.rs` | 760 | `fs::write(root.join("row.tera"), r#"{{ row["name"] }}"#).expect("write template");` |
| ggen | `crates/ggen-lsp/src/check.rs` | 775 | `fs::write(root.join("ggen.toml"), manifest).expect("write manifest");` |
| ggen | `crates/ggen-lsp/src/check.rs` | 815 | `fs::write(root.join("ggen.toml"), manifest).expect("write manifest");` |
| ggen | `crates/ggen-lsp/src/check.rs` | 841 | `fs::write(root.join("row.tera"), r#"{{ row["title"] }}"#).expect("write template");` |
| ggen | `crates/ggen-lsp/src/check.rs` | 856 | `fs::write(root.join("ggen.toml"), manifest).expect("write manifest");` |
| ggen | `crates/ggen-lsp/src/check.rs` | 880 | `fs::write(&cfg, "[logging]\nlevel = \"verbose\"\n").expect("write");` |
| ggen | `crates/ggen-lsp/src/project_index.rs` | 178 | `std::fs::write(dir.path().join("ggen.toml"), manifest).expect("write manifest");` |
| ggen | `crates/ggen-lsp/src/project_index.rs` | 205 | `std::fs::write(` |
| ggen | `crates/ggen-lsp/src/project_index.rs` | 224 | `std::fs::write(dir.path().join("ggen.toml"), manifest).expect("write manifest");` |
| ggen | `crates/ggen-lsp/src/project_index.rs` | 256 | `std::fs::write(dir.path().join("ggen.toml"), manifest).expect("write manifest");` |
| ggen | `crates/ggen-lsp/src/project_index.rs` | 284 | `std::fs::write(dir.path().join("ggen.toml"), manifest).expect("write manifest");` |
| ggen | `crates/ggen-lsp/src/init.rs` | 151 | `std::fs::write(path, contents)?;` |
| ggen | `crates/ggen-lsp/src/init.rs` | 184 | `std::fs::write(dir.path().join(".helix/languages.toml"), "custom").expect("seed");` |
| ggen | `crates/ggen-lsp/src/rule_index.rs` | 246 | `std::fs::write(` |
| ggen | `crates/ggen-lsp/src/intel/log.rs` | 9 | `use std::fs::{create_dir_all, OpenOptions};` |
| ggen | `crates/ggen-lsp/src/intel/log.rs` | 60 | `let mut file = OpenOptions::new()` |
| ggen | `crates/ggen-lsp/src/intel/log.rs` | 156 | `let mut f = OpenOptions::new()` |
| ggen | `crates/ggen-lsp/src/intel/mine.rs` | 346 | `let _ = std::fs::write(path, json);` |
| ggen | `crates/ggen-lsp/src/intel/mine.rs` | 378 | `std::fs::write(path, md).map_err(|e| ggen_graph::GraphError::Other(e.to_string()))` |
| ggen | `crates/ggen-lsp/src/intel/history.rs` | 8 | `use std::fs::{create_dir_all, OpenOptions};` |
| ggen | `crates/ggen-lsp/src/intel/history.rs` | 83 | `let mut file = OpenOptions::new()` |
| ggen | `crates/ggen-lsp/src/intel/replay.rs` | 247 | `let mut f = std::fs::OpenOptions::new()` |
| ggen | `crates/ggen-lsp/src/route/promoted.rs` | 85 | `std::fs::write(&tmp, json)?;` |
| ggen | `crates/ggen-lsp/src/route/promoted.rs` | 157 | `std::fs::write(&path, r#"{"version":99,"source_log_hash":"x","routes":[]}"#).expect("w");` |
| ggen | `crates/ggen-lsp/src/features/workspace_symbol.rs` | 171 | `fs::write(root.join(".specify/specs/people.ttl"), ttl).expect("write ttl");` |
| ggen | `crates/ggen-lsp/src/features/workspace_symbol.rs` | 174 | `fs::write(` |
| ggen | `crates/ggen-lsp/src/pack/mod.rs` | 251 | `let _ = std::fs::write(root.join(PROVENANCE_FILE), json);` |
| ggen | `crates/ggen-lsp/src/pack/mod.rs` | 258 | `let _ = std::fs::write(root.join(MANIFEST_FILE), json);` |
| ggen | `crates/ggen-lsp/src/pack/mod.rs` | 328 | `let _ = std::fs::write(dir.join(format!("pack-{}.json", &sig[..16])), json);` |
| ggen | `crates/ggen-lsp/src/pack/mod.rs` | 492 | `std::fs::write(path, contents)?;` |
| wasm4pm | `crates/pm4py-lsp/src/receipts.rs` | 55 | `fs::write(receipt_path, content)?;` |
| wasm4pm | `crates/pm4py-lsp/src/lib.rs` | 516 | `fs::write(receipt_path, receipt_json)` |
| wasm4pm | `crates/pm4py-lsp/src/lib.rs` | 555 | `fs::write(fixture_path, &fixture_json)` |
| wasm4pm | `crates/pm4py-lsp/src/lib.rs` | 578 | `fs::write(receipt_path, receipt_json)` |
| wasm4pm | `crates/pm4py-lsp/src/lib.rs` | 838 | `fs::write(receipt_path, receipt_json)` |
| wasm4pm | `crates/pm4py-lsp/src/lib.rs` | 875 | `fs::write(receipt_path, receipt_json)` |
| wasm4pm | `crates/pm4py-lsp/src/fixtures.rs` | 25 | `fs::write(fixture_path, content)?;` |
| tower-lsp-max | `crates/playground/src/bin/gc005_wasm4pm_adapter.rs` | 124 | `fs::write(output_path, json)?;` |
| tower-lsp-max | `crates/playground/src/bin/dogfood_harness.rs` | 904 | `std::fs::write(&root_file_path, &content).unwrap();` |
| tower-lsp-max | `crates/playground/src/bin/dogfood_harness.rs` | 905 | `std::fs::write(&crate_file_path, &content).unwrap();` |
| tower-lsp-max | `crates/tower-lsp-max-specgen/src/main.rs` | 40 | `fs::write(&args.output, rendered)` |
| tower-lsp-max | `crates/tower-lsp-max-cli/src/nouns/diagnostics.rs` | 303 | `std::fs::write(&path, json_str)` |
| tower-lsp-max | `crates/tower-lsp-max-cli/src/nouns/client.rs` | 50 | `std::fs::write(&path, content).map_err(|e| e.to_string())?;` |
| tower-lsp-max | `crates/tower-lsp-max-cli/src/nouns/config.rs` | 57 | `fs::write(path, content).map_err(|e| e.to_string())?;` |
| tower-lsp-max | `crates/tower-lsp-max-cli/src/nouns/server.rs` | 90 | `std::fs::write(&path, content).map_err(|e| e.to_string())?;` |
| tower-lsp-max | `crates/tower-lsp-max-cli/src/nouns/agent.rs` | 59 | `std::fs::write(&path, content).map_err(|e| e.to_string())?;` |
| tower-lsp-max | `crates/tower-lsp-max-cli/src/nouns/plugin.rs` | 52 | `std::fs::write(&path, content).map_err(|e| e.to_string())?;` |
| tower-lsp-max | `tower-lsp-max-runtime/src/mesh.rs` | 141 | `std::fs::write(path, serialized)?;` |
| tower-lsp-max | `tower-lsp-max-runtime/src/mesh.rs` | 316 | `if let Err(e) = std::fs::write(&file_path, content) {` |
| tower-lsp-max | `tower-lsp-max-runtime/src/control_plane/receipts.rs` | 259 | `std::fs::write(path, self.primary_key.to_bytes())?;` |
| tower-lsp-max | `src/workspace_edit.rs` | 44 | `std::fs::write(&path, &content)` |
| tower-lsp-max | `src/language_server/impls/repair.rs` | 164 | `let _ = std::fs::write(&path, old_content);` |

