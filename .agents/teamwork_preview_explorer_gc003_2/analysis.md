# Codebase Analysis: crates/ggen-lsp interaction with Equation Enforcement Receipts

## Executive Summary
This report analyzes how the language server protocol (LSP) implementation in `crates/ggen-lsp/` interacts with equation enforcement receipts ($R_B \vdash A = \mu(O^*_B)$) and verifies whether diagnostic triggers (such as `GGEN-EVIDENCE-001` and `GGEN-DRIFT-001`) correctly intercept and read `receipts.json` / `receipts.jsonl`.

Our key findings indicate that **`ggen-lsp` does not interact with the projection engine's equation enforcement receipts or validation logic**. The diagnostic triggers in the LSP are implemented as **heuristic pattern-matching rules on the content of the active editor buffers** rather than actual cryptographic validation against `receipts.json` or `receipts.jsonl`.

---

## 1. Interaction with Equation Enforcement Receipts ($R_B \vdash A = \mu(O^*_B)$)
The equation $R_B \vdash A = \mu(O^*_B)$ specifies that a boundary receipt $R_B$ certifies that an artifact $A$ matches the projection mapping $\mu$ applied to the observed boundary behavior $O^*_B$. 

* **Theoretical Verification Location**: This verification is implemented via `ReceiptIndex::validate_sync` in `crates/ggen-projection/src/receipt.rs`. It validates file digests, template digests, boundary digests, and signature chain integrity.
* **LSP Integration**: While `crates/ggen-lsp/Cargo.toml` lists `ggen-projection` as a dependency, the codebase of `ggen-lsp` **never references or calls** `ReceiptIndex` or `validate_sync`. There are zero occurrences of these symbols within the `crates/ggen-lsp/` crate.
* **Telemetric Receipts vs. Projection Receipts**: `crates/ggen-lsp` contains its own concept of "receipts" under `src/check.rs` and `src/state.rs`, which append `ReceiptEmitted` events to the append-only event log `.ggen/ocel/agent-edit-events.ocel.jsonl`. These represent process-mining telemetry of applied repairs inside the editor, rather than project-level boundary projection receipts.

---

## 2. Verification of Diagnostic Triggers (`GGEN-EVIDENCE-001`, `GGEN-DRIFT-001`)
We investigated how these diagnostic triggers are registered and resolved within `crates/ggen-lsp/src/handlers/diagnostics.rs` (in `compute_observer_diagnostics`).

### 2.1 Heuristic Heuristics vs. Real Filesystem Reads
The language server performs purely local, pattern-based checks on the active document contents passed by the editor (via `didOpen` or `didChange` notifications). It does not intercept or read `receipts.json` or `receipts.jsonl` from the filesystem to diagnose other files:

| Diagnostic Code | Target Files | Trigger Rule | LSP Action / Message |
| --- | --- | --- | --- |
| **`GGEN-EVIDENCE-001`** (Missing receipt) | `lib.rs` | Content does **not** contain `"pub mod cli"` | `"Artifact lacks projection receipt"` |
| **`GGEN-EVIDENCE-001`** (Corrupt index) | `receipts.json`, `receipts.jsonl` | Content contains `"random_corrupt_bytes"` OR fails to parse as valid JSON | `"Receipt index is corrupted"` |
| **`GGEN-DRIFT-001`** (Drifted content) | `main.rs`, `server.rs` | Content contains the substring `"drifted"` | `"Projected content has drifted from template"` |
| **`GGEN-OVERRIDE-001`** (Unreceipted override) | `main.rs`, `server.rs` | Content contains the substring `"ggen:override"` | `"Override exists but not receipted"` |
| **`GGEN-CUSTOMIZE-001`** (Incomplete customization) | `customization-map.json` | Content is empty, `"{}"`, or contains `"TODO"` | `"Required customization point incomplete"` |
| **`GGEN-PROJECT-OPPORTUNITY-001`** | `pack.toml`, `manual_parser.rs` | `pack.toml` contains `"no_sigs"` OR file contains `pub fn parse()` | Opportunity to project file / missing signatures |

### 2.2 Proof from Integration Tests
The integration tests in `crates/ggen-projection/tests/f4_lsp_diagnostics.rs` confirm these exact triggers:
* `test_f4_t1_drift_diagnostic` sends `"pub fn main() { println!(\"drifted\"); }"` to trigger `GGEN-DRIFT-001`.
* `test_f4_t2_corrupt_receipt_file` sends `"{random_corrupt_bytes}"` to a virtual `receipts.json` to trigger `GGEN-EVIDENCE-001` (corrupted index).
* `test_f4_t1_evidence_diagnostic` opens `lib.rs` with `"pub fn lib() {}"` (missing `"pub mod cli"`) to trigger `GGEN-EVIDENCE-001` (missing receipt).

---

## 3. Conclusion
The diagnostic triggers in `crates/ggen-lsp/` do not intercept, read, or validate `receipts.json` / `receipts.jsonl` files on the disk. They act as **lightweight editor-buffer pattern matchers** designed to satisfy conformance test suites in a mock-like manner without incurring the overhead of running a full projection synchronizer or executing cryptographic validation inside the editor event loop.
