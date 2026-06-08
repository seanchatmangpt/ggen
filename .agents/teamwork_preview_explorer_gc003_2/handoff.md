# Handoff Report: crates/ggen-lsp Receipt & Diagnostic Verification

## 1. Observation
We observed the following definitions and implementations in `crates/ggen-lsp/` and `crates/ggen-projection/`:

* **LSP Diagnostic Rules**: Inside `crates/ggen-lsp/src/handlers/diagnostics.rs` (lines 4-133), the function `compute_observer_diagnostics` contains the entire logic for generating diagnostics. It matches filenames and their string content:
  - **Receipt Map Check (lines 39-51)**:
    ```rust
    // 2. Receipts map check
    if path_str.ends_with("receipts.json") || path_str.ends_with("receipts.jsonl") {
        if content.contains("random_corrupt_bytes") || (content.trim().starts_with('{') && serde_json::from_str::<serde_json::Value>(content).is_err()) {
            diags.push(make_diag(
                "GGEN-EVIDENCE-001",
                "Receipt index is corrupted",
                DiagnosticSeverity::ERROR,
                0,
                0,
                "ggen_lsp_observer",
            ));
        }
    }
    ```
  - **Drift Check (lines 95-105)**:
    ```rust
    // Check for drift
    if content.contains("drifted") {
        diags.push(make_diag(
            "GGEN-DRIFT-001",
            "Projected content has drifted from template",
            DiagnosticSeverity::WARNING,
            start,
            end,
            "ggen_lsp_observer",
        ));
    }
    ```
  - **Missing Receipt Check (lines 120-130)**:
    ```rust
    // 6. Missing receipt check for files in projected directory
    if path_str.ends_with("lib.rs") && !content.contains("pub mod cli") {
        diags.push(make_diag(
            "GGEN-EVIDENCE-001",
            "Artifact lacks projection receipt",
            DiagnosticSeverity::ERROR,
            0,
            0,
            "ggen_lsp_observer",
        ));
    }
    ```

* **No Reference to ReceiptIndex**: Running a grep search for `ReceiptIndex` or `validate_sync` within `crates/ggen-lsp/` returns absolutely zero results.

* **F4 LSP Diagnostics Tests**: In `crates/ggen-projection/tests/f4_lsp_diagnostics.rs`, the tests simulate these checks using virtual files and string matching:
  - `test_f4_t1_drift_diagnostic` sends `"pub fn main() { println!(\"drifted\"); }"` to trigger `GGEN-DRIFT-001`.
  - `test_f4_t2_corrupt_receipt_file` sends `"{random_corrupt_bytes}"` to trigger `GGEN-EVIDENCE-001` (corrupted receipt index).
  - `test_f4_t1_evidence_diagnostic` opens `lib.rs` with `"pub fn lib() {}"` (missing `"pub mod cli"`) to trigger `GGEN-EVIDENCE-001` (lacks projection receipt).

---

## 2. Logic Chain
1. **Fact**: The function `compute_observer_diagnostics` in `crates/ggen-lsp/src/handlers/diagnostics.rs` only performs local string checks on the active file name and file content arguments passed into it. (Supported by Observation 1).
2. **Fact**: There are no references to the projection validation logic (`ReceiptIndex` or `validate_sync`) anywhere in the `ggen-lsp` crate. (Supported by Observation 2).
3. **Fact**: The integration tests (`crates/ggen-projection/tests/f4_lsp_diagnostics.rs`) trigger diagnostic codes (`GGEN-DRIFT-001`, `GGEN-EVIDENCE-001`) by passing mock-like string patterns (e.g. `"drifted"`, `"{random_corrupt_bytes}"`, or a `lib.rs` content without `"pub mod cli"`). (Supported by Observation 3).
4. **Conclusion**: Therefore, `crates/ggen-lsp/` does not interact with the projection engine's actual equation enforcement receipts ($R_B \vdash A = \mu(O^*_B)$). Furthermore, `ggen-lsp` does not read or intercept `receipts.json` / `receipts.jsonl` from the filesystem to perform actual validation; it relies strictly on editor-buffer content-based heuristics.

---

## 3. Caveats
No caveats. The implementation was fully audited, and the tests were executed and passed successfully.

---

## 4. Conclusion
* `crates/ggen-lsp/` is completely decoupled from the mathematical projection validation and equation enforcement receipts ($R_B \vdash A = \mu(O^*_B)$) of `crates/ggen-projection`.
* Diagnostic triggers in `crates/ggen-lsp/` (such as `GGEN-EVIDENCE-001` and `GGEN-DRIFT-001`) are simple text-matching heuristics executed on current editor buffers. They do not load or inspect `receipts.json` / `receipts.jsonl` from the project directory.

---

## 5. Verification Method
To independently verify:
1. Examine the diagnostics logic in `crates/ggen-lsp/src/handlers/diagnostics.rs`.
2. Run a global search for `ReceiptIndex` or `validate_sync` in `crates/ggen-lsp/` to confirm they are absent.
3. Run the LSP unit and integration tests using:
   ```bash
   cargo test -p ggen-lsp
   cargo test -p ggen-projection --test f4_lsp_diagnostics
   ```
   Both commands should complete successfully.
