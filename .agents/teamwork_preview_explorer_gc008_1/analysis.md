# GC008 Exploration Analysis Report

This report documents the read-only exploration of the GC008 verification target (`crates/ggen-pack-clap-noun-verb`).

---

## 1. Executive Summary
The `crates/ggen-pack-clap-noun-verb` package represents the template structure and Language Server Protocol (LSP) validation logic for the **Clap Noun-Verb** code generation pack. During compilation verification, we discovered that the test suite fails to compile out of the box due to missing `[dev-dependencies]` in the package's local `Cargo.toml`. Additionally, the test suite has physical dependencies on sibling workspace `/Users/sac/wasm4pm` for source-level scanning and binary execution.

---

## 2. CLAP Noun/Verb Validation Server (`src/main.rs`)
The validation server (`clap-noun-verb-pack-lsp`) is a lightweight Language Server Protocol (LSP) server implemented using the `tower-lsp` framework. It runs over `stdin` and `stdout`.

### Key Capabilities
- **Text Document Sync**: Fully synchronized (`TextDocumentSyncKind::FULL`).
- **Command Provider**: Exposes one command: `"conformance-receipt.bind"`.

### Logic & Implementation Details
- **Diagnostics (`check_diagnostics`)**:
  - Automatically triggers on document open (`did_open`) and change (`did_change`).
  - Scans files ending with `cli.rs` or `main.rs`.
  - **Poka-Yoke Constraint**: If the code contains `struct Opts` (defining command line arguments) but does *not* contain a handling function (`fn handle`), it publishes an LSP Diagnostic:
    - **Code**: `"CLAP-PACK-HANDLER-UNBOUND"`
    - **Message**: `"CLI domain missing handler"`
    - **Severity**: `DiagnosticSeverity::ERROR`
    - **Data**: `{ "source_id": "clap_noun_verb_pack_lsp" }`
- **Command Execution (`execute_command`)**:
  - Handles the custom `"conformance-receipt.bind"` command.
  - On validation of the command, it logs info messages representing admission through the lawful spine:
    - `CLAP Validated: noun=conformance-receipt verb=bind`
    - `Routing to PackPlan -> Staging -> MutationGate`
  - Rejects any other commands with a `CLAP Rejected: unknown command` error log.

---

## 3. Test Suite Structure and Implementation
The package includes 7 integration/dogfooding test files in `tests/`:

### 1. `tests/dogfood_gc008_b_c.rs` (CLAP Command Admission & Lawful Spine)
- **Goal**: Verifies CLAP command grammar admission and routing through the MutationGate.
- **Mechanism**:
  - Spawns the compiled `clap-noun-verb-pack-lsp` binary as a child process.
  - Drives LSP initialization, then notifies `workspace/executeCommand` with `"conformance-receipt.bind"`.
  - Asserts that the LSP server returns logs containing `"CLAP Validated"` and `"Routing to PackPlan -> Staging -> MutationGate"`.
  - Asserts that invalid commands (e.g., `"wasm4pm.bind_receipt"`) are rejected with `"CLAP Rejected"`.

### 2. `tests/dogfood_clap_command_route.rs` (Command Route Lock)
- **Goal**: Ensures no unauthorized command routing exists in `wasm4pm-lsp`.
- **Mechanism**:
  - Scans the directory `/Users/sac/wasm4pm/crates/wasm4pm-lsp/src` if it exists.
  - Searches for forbidden keywords: `"WorkspaceEdit"`, `"wasm4pm.bind_receipt"`, and direct file writes containing receipts.
  - Verifies that `conformance-receipt.bind` is the active command in `main.rs`.

### 3. `tests/dogfood_gc004.rs` (LSP Heuristics Separation)
- **Goal**: Assures that the LSPs are non-monolithic and strictly respect the "no-observer-write" law.
- **Mechanism**:
  - Asserts that both `ggen-pack-clap-noun-verb/src/main.rs` and `ggen-pack-tower-lsp-max/src/main.rs` exist.
  - Scans their contents to verify that they do not contain forbidden write functions (`fs::write`, `std::fs::write`, or `tokio::fs::write`).
  - Scans `crates/ggen-lsp/src/handlers/diagnostics.rs` to ensure pack-specific diagnostic codes (e.g. `CLAP-CUSTOMIZE-001`) are stripped, but generic states like `GGEN-PROJECTED` and `GGEN-DRIFT` remain.

### 4. `tests/dogfood_gc005.rs` (WASM4PM LSP Diagnostics)
- **Goal**: Verifies process-conformance diagnostic output from `wasm4pm-lsp`.
- **Mechanism**:
  - Spawns the `wasm4pm-lsp` binary.
  - Sends `textDocument/didOpen` notifications with modified OCEL logs representing:
    - **FIT**: Raw log matches process model. Returns `WASM4PM-VERDICT-FIT`.
    - **DEVIATION**: Missing checkpoints (removes `GALL-CHECKPOINT-003`). Returns `WASM4PM-VERDICT-DEVIATION`.
    - **BLOCKED**: Receipt tampering (injects `tampered_uuid`). Returns `WASM4PM-VERDICT-BLOCKED`.

### 5. `tests/dogfood_gc006_calver.rs` (CalVer Version Lock)
- **Goal**: Hard enforcement of CalVer versioning.
- **Mechanism**:
  - Recursively walks `ggen` and `tower-lsp-max` directories.
  - Reads `Cargo.toml`, `pack.toml`, and `sync_target.rs` files.
  - Panics if any file contains `"1.0.0"` or `"v1.0.0"`.

### 6. `tests/dogfood_no_lsp_mutation.rs` (No LSP Write Lock)
- **Goal**: Enforces the no-write policy for `wasm4pm-lsp`.
- **Mechanism**:
  - Walks `/Users/sac/wasm4pm/crates/wasm4pm-lsp/src` and checks for any `fs::write` patterns in the Rust source.

### 7. `tests/f8_equation_enforcement.rs` (Cryptographic Proof Verification)
- **Goal**: Tests the `ggen_projection` equation context boundary check.
- **Mechanism**:
  - Constructs mock `ReceiptIndex`, `EquationContext`, and `ProjectionMap`.
  - Performs validation sync and asserts that changing any parameter of the equation (`boundary_digest`, `workspace_digest`, `pack_plan_digest`, `pack_descriptor_digest`, `customization_digest`, `staging_digest`, `mutation_gate_decision`, `verification_result`, `projection_engine_version`) invalidates the receipt check.
  - Verifies that after-the-fact tampering of files on disk fails validation.

---

## 4. Metadata and Config Files
Inside `tests/`, there are configuration files defining how the code generator is set up:
- **`pack-plan.json`**: Resolves `ggen-pack-proofs` as a required pack dependency with its Blake3 checksum.
- **`projection-map.json`**: Maps each test file in `tests/` to its corresponding template template path in `ggen-pack-proofs`.
- **`receipts.json` / `tests_receipts/receipts.jsonl`**: The cryptographically bound receipt histories for the projected test files, ensuring they conform to the `26.6.6` engine version.

---

## 5. Dependency Gaps and Test Execution Issues
When running `cargo test -p clap-noun-verb-pack-lsp` inside the `ggen` workspace, the build fails with unresolved import errors:
1. `walkdir` is unresolved in `dogfood_clap_command_route.rs` and `dogfood_no_lsp_mutation.rs`.
2. `tempfile` and `ggen_projection` are unresolved in `f8_equation_enforcement.rs`.

These dependencies are defined at the workspace level or elsewhere, but the crate's `Cargo.toml` (`crates/ggen-pack-clap-noun-verb/Cargo.toml`) does not import them.

### Cross-Workspace Dependencies
The tests also rely on:
- Sibling directory `/Users/sac/wasm4pm` containing `crates/wasm4pm-lsp` source files and the compiled `wasm4pm-lsp` binary.
- If the sibling workspace `/Users/sac/wasm4pm` has not been compiled, or if the `wasm4pm-lsp` binary is missing from the target directory, `dogfood_gc005.rs` will fail at runtime.

---

## 6. Proposed Configuration Fix
To resolve the compile-time errors in `cargo test -p clap-noun-verb-pack-lsp`, we propose updating `crates/ggen-pack-clap-noun-verb/Cargo.toml` with the following patch:

```patch
diff --git a/crates/ggen-pack-clap-noun-verb/Cargo.toml b/crates/ggen-pack-clap-noun-verb/Cargo.toml
index 193c780..13f412c 100644
--- a/crates/ggen-pack-clap-noun-verb/Cargo.toml
+++ b/crates/ggen-pack-clap-noun-verb/Cargo.toml
@@ -9,3 +9,9 @@ [dependencies]
 serde_json = "1.0"
 tokio = { version = "1.0", features = ["rt-multi-thread", "macros", "io-std"] }
 
+[dev-dependencies]
+walkdir = "2.5"
+tempfile = { workspace = true }
+ggen-projection = { workspace = true }
```
