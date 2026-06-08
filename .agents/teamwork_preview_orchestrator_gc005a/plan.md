# Plan for GC005A Implementation & Workspace Sterility Baselines

## Objective
Implement GC005A: Sealed wasm4pm Replay Surface Contract, LSP Integration Testing, and Sealed Workspace Sterility Baselines.

## Milestones & Status
| Milestone | Description | Status |
|-----------|-------------|--------|
| M1: Investigate | Explore existing tests, files, and signatures in ggen and tower-lsp-max | DONE (explorer_gc005a_1) |
| M2: Repair & Manifests | Fix syntax/build/dependency errors, write `.gc-sealed-baseline` manifests, ignore locally via git status exclude, update `dogfood_gc006.rs` to parse manifests | IN_PROGRESS (worker_gc005a_1) |
| M3: Review | Verify implementation correctness and completeness against architecture constraints | PLANNED |
| M4: Challenge & Audit | Run adversarial and forensic verification checks | PLANNED |

## Verification Plan
1. Run E2E dogfood_gc005 test under tower-lsp-max integration boundary.
2. Run sterility checks under dogfood_gc006.rs verifying that baseline manifests enforce read-only status and detect new changes.
