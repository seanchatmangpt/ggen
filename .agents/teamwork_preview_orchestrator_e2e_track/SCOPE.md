# Scope: E2E Testing Track

## Architecture
The E2E test suite will perform opaque-box validation of the ggen projection pipeline and LSP observer servers. All tests are located under `crates/ggen-projection/tests/` (and/or `crates/tower-lsp-max/tests/` if created) and execute via real CLI invocation or public API boundary crossings.

Data flow:
1. E2E Test Suite -> Invokes `cargo` or CLI commands / public library entry points.
2. `ggen-projection` -> Generates projection-map.json, customization-map.json, receipts.jsonl.
3. `ggen-lsp` / `tower-lsp-max` -> Consumes generated files and publishes diagnostics / autocompletion / hover hints.
4. E2E Test Suite -> Inspects filesystem outputs and parses JSON-RPC/OTel telemetry responses to corroborate correctness.

## Milestones
| # | Name | Scope | Dependencies | Status |
|---|---|---|---|---|
| 1 | Test Harness Scaffolding | Define common helper modules, integration test targets, and shared validation logic without mocks. | None | DONE |
| 2 | Tier 1: Feature Coverage | Implement $\ge 5$ happy-path integration tests for each of Features F1 to F6. | M1 | DONE |
| 3 | Tier 2: Boundary & Corner Cases | Implement $\ge 5$ boundary/negative/robustness tests for each of Features F1 to F6. | M2 | DONE |
| 4 | Tier 3: Cross-Feature Combinations | Implement $\ge 5$ pairwise interaction tests combining features (e.g., drift during sync). | M3 | DONE |
| 5 | Tier 4: Real-World Scenarios | Implement 5 complete application workload scenarios. | M4 | DONE |
| 6 | TEST_READY.md & Handoff | Verify all tests compile/run, publish `/Users/sac/ggen/TEST_READY.md`, and complete handoff to parent. | M5 | DONE |

## Interface Contracts
- **Telemetry Surface**: Tests will subscribe to/verify real OpenTelemetry spans.
- **Evidence Surface**: Validation of BLAKE3 receipts and cryptographic signatures.
- **Causality Surface**: Verification of downstream diagnostic changes upon template/ontology modifications.
