# Project: GC003/006 Boundary-Receipted Equation Enforcement & wasm4pm Workspace Remediation

## Architecture
- Module boundaries: crates/ggen-projection/, crates/ggen-lsp/
- Data flow: sync_target CLI projects templates and generates receipts. The receipt validation engine enforces $R_B \vdash A = \mu(O^*_B)$.
- Sealed wasm4pm Workspace: Sibling directory `/Users/sac/wasm4pm` must remain clean and match its `.gc-sealed-baseline` baseline signature.

## Milestones
| # | Name | Scope | Dependencies | Status |
|---|------|-------|-------------|--------|
| 1 | Context Recovery & Initialization | Recover state from previous orchestrator, set up agent metadata | none | DONE |
| 2 | wasm4pm Workspace Remediation | Revert unauthorized changes in `/Users/sac/wasm4pm` to align with the sealed baseline | M1 | DONE |
| 3 | Verification of GC003/GC006 Test Suite | Run tests in `ggen` to ensure all tests pass (including `dogfood_gc003` and `dogfood_gc006`) | M2 | DONE |
| 4 | Forensic Integrity Audit | Perform a forensic audit to verify conformance and clean status | M3 | DONE |

## Interface Contracts
- Sibling workspaces `/Users/sac/wasm4pm` and `/Users/sac/wasm4pm-compat` must match their baselines.
- The `sync_target` binary generates correct receipts.
- Verification commands compile and run without errors.
