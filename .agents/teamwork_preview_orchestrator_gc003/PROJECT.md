# Project: GC003/004/005 Boundary-Receipted Equation Enforcement & Conformance Verification

## Architecture
- Module boundaries: crates/ggen-projection/, crates/ggen-lsp/, crates/ggen-pack-gall-checkpoint-proof/
- Data flow: sync_target CLI generates targets, staging, and receipts. The receipt validation engine enforces $R_B \vdash A = \mu(O^*_B)$.
- LSP Pipeline: Stdio client communicating over stdin/stdout with ggen-lsp server executing full JSON-RPC method routing to publishDiagnostics.
- OCEL Ledger (wasm4pm/wasm4pm-compat): Deterministic emission of workspace, boundary ledger, checkpoint, pack, template, plan, staging, diagnostics, LSP, tests, and verifications to `crates/playground/ocel/` in both `~/wasm4pm` and `~/wasm4pm-compat` in append-only JSONL format with BLAKE3 cryptographic chain linkage.

## Milestones
| # | Name | Scope | Dependencies | Status |
|---|------|-------|-------------|--------|
| 1 | Exploration | Inspect current repo, branch, tests, and proof pack manifest (GC003) | none | DONE |
| 2 | Code Alignment & Baseline | Check compilation/tests and clean tower-lsp-max workspace status | M1 | IN_PROGRESS |
| 3 | Reusable LSP Test Harness | Implement the harness at crates/ggen-lsp/tests/common/lsp_harness.rs exposing only LSP client ops | M2 | PLANNED |
| 4 | Protocol Path & Admission Categories | Implement 5 admission test categories using full LSP client/server path | M3 | PLANNED |
| 5 | Source-Level Bypass-Kills (Anti-Bypass) | Implement generated dogfood test scanning for forbidden symbols | M4 | PLANNED |
| 6 | OCEL Schema & Receipt Reclassification | Reclassify old receipts under warning fence in wasm4pm/compat. Implement OCEL object/event schema. | M5 | PLANNED |
| 7 | Deterministic OCEL Event Emission | Emit append-only JSONL logs for events, objects, digests, and verdicts in wasm4pm/compat | M6 | PLANNED |
| 8 | Cryptographic Digest Chain & Verdict Verifier | Link events via BLAKE3 hashes and implement wasm4pm verifier returning conformance verdict | M7 | PLANNED |
| 9 | Execution & Verification | Run workspace compile/tests and ensure sandboxed boundaries under feat/ggen-lsp-source-laws | M8 | PLANNED |
| 10 | Forensic Integrity Audit | Run Forensic Integrity Auditor checks to verify clean status | M9 | PLANNED |

## Interface Contracts
- $R_B \vdash A = \mu(O^*_B)$ is verified via `ReceiptIndex::validate_sync`.
- Reusable LSP test harness exposes only LSP operations (`initialize`, `did_open`, `did_change`, `request_code_action`, `execute_command`, `wait_for_publish_diagnostics`, etc.) communicates over stdio with no internal server state inspection.
- Anti-bypass forbidden symbols to scan: `compute_observer_diagnostics`, `analyze_and_observe`, `validate_sync`, `observe_pack_domain`, `state.diagnostics`, `direct_write`, `std::fs::write`.
- OCEL logs format: JSONL file formats under `crates/playground/ocel/` in `~/wasm4pm` and `~/wasm4pm-compat` for events, objects, digests, and verdicts.
- Cryptographic hashing: BLAKE3 hashes chain events, verified by conformance replay verifier returning conformance verdict: `FIT | DEVIATION | BLOCKED | INCONCLUSIVE`.
