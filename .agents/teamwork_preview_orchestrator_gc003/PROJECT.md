# Project: GC003/004/005 Boundary-Receipted Equation Enforcement & Conformance Verification

## Architecture
- Module boundaries: crates/ggen-projection/, crates/ggen-lsp/, crates/ggen-pack-gall-checkpoint-proof/
- Data flow: sync_target CLI generates targets, staging, and receipts. The receipt validation engine enforces $R_B \vdash A = \mu(O^*_B)$.
- LSP Pipeline: Stdio client communicating over stdin/stdout with ggen-lsp server executing full JSON-RPC method routing to publishDiagnostics.
- wasm4pm Neutral Adapter: `gc005-wasm4pm-adapter` calls the sealed read-only authorities at `~/wasm4pm` and `~/wasm4pm-compat` without performing conformance/replay itself.
- wasm4pm-lsp Server: Process-evidence observer owning only `WASM4PM-*` diagnostics, delegating evaluation to `gc005-wasm4pm-adapter` and publishing mapped diagnostics over stdio JSON-RPC.

## Milestones
| # | Name | Scope | Dependencies | Status |
|---|------|-------|-------------|--------|
| 1 | Exploration | Inspect current repo, branch, tests, and proof pack manifest (GC003) | none | DONE |
| 2 | Code Alignment & Baseline | Check compilation/tests and clean tower-lsp-max workspace status | M1 | IN_PROGRESS |
| 3 | Reusable LSP Test Harness | Implement the harness at crates/ggen-lsp/tests/common/lsp_harness.rs exposing only LSP client ops | M2 | PLANNED |
| 4 | Protocol Path & Admission Categories | Implement 5 admission test categories using full LSP client/server path | M3 | PLANNED |
| 5 | Source-Level Bypass-Kills (Anti-Bypass) | Implement generated dogfood test scanning for forbidden symbols | M4 | PLANNED |
| 6 | Sealed Boundaries & wasm4pm Adapter | Ensure no writes/fake local crates in ~/wasm4pm or ~/wasm4pm-compat. Implement gc005-wasm4pm-adapter | M5 | PLANNED |
| 7 | wasm4pm-lsp Server Implementation | Implement or align wasm4pm-lsp to act as observer and delegate to gc005-wasm4pm-adapter | M6 | PLANNED |
| 8 | Proof Projection Harness (dogfood_gc005) | Ensure sync_target projects dogfood_gc005.rs from template and runs stdio LSP tests | M7 | PLANNED |
| 9 | Execution & Conformance Verification | Show verifier validation command output proving FIT/DEVIATION/BLOCKED over evidence | M8 | PLANNED |
| 10 | Forensic Integrity Audit | Run Forensic Integrity Auditor checks to verify clean status | M9 | PLANNED |

## Interface Contracts
- $R_B \vdash A = \mu(O^*_B)$ is verified via `ReceiptIndex::validate_sync`.
- Reusable LSP test harness exposes only LSP operations (`initialize`, `did_open`, `did_change`, `request_code_action`, `execute_command`, `wait_for_publish_diagnostics`, etc.) communicates over stdio with no internal server state inspection.
- Anti-bypass forbidden symbols to scan: `compute_observer_diagnostics`, `analyze_and_observe`, `validate_sync`, `observe_pack_domain`, `state.diagnostics`, `direct_write`, `std::fs::write`.
- `gc005-wasm4pm-adapter` calls sealed authorities in read-only folders `~/wasm4pm` and `~/wasm4pm-compat`.
- `wasm4pm-lsp` owns only `WASM4PM-*` diagnostics.
