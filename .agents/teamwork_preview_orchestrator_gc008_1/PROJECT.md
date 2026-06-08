# Project: GC008 Verification

## Architecture
- `crates/ggen-pack-clap-noun-verb` acts as the LSP server providing CLAP command grammar admission.
- The LSP server checks commands via `execute_command`, and admits `conformance-receipt.bind` while rejecting others.
- The execution flow simulates:
  `CodeAction -> PackActionIntent -> PackPlan -> Staging -> MutationGate -> Receipt`
  by logging specific status messages and routing checks.
- Verification is done by running integration/dogfood tests that cross the stdio LSP protocol boundary, asserting correct validation messages and checking negative controls.

## Milestones
| # | Name | Scope | Dependencies | Status |
|---|------|-------|-------------|--------|
| 1 | Exploration | Inspect existing GC008 tests and server implementation | none | PLANNED |
| 2 | Verification Execution | Run integration/dogfood tests and retrieve execution evidence | M1 | PLANNED |
| 3 | Verification Reporting | Format final verification evidence and output victory claim | M2 | PLANNED |

## Interface Contracts
- LSP Client <-> clap-noun-verb-pack-lsp via JSON-RPC stdin/stdout.
- Command admission requests: `workspace/executeCommand` with `command: "conformance-receipt.bind"` and `command: "wasm4pm.bind_receipt"`.
