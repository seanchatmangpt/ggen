# Original User Request

## Initial Request — 2026-06-06T13:22:53-07:00

Implement the "80/20 Projection Core and Pack LSPs" (ggen Projection Intelligence) based on the PRD/ARD, establishing a workspace configuration and implementation in `~/ggen` and `~/tower-lsp-max`.

Working directory: /Users/sac/ggen
Integrity mode: benchmark

## Requirements

### R1. Pack Model and 80/20 Distribution
- Implement `PackDescriptor` (representing `pack.toml`), `PackPlan`, `ProjectionMap`, `CustomizationMap`, and `ReceiptIndex` structures according to the PRD/ARD schemas.
- Ensure host language package manager (Cargo/crates) handles dependency resolution.
- Place the packs inside `/Users/sac/ggen`.

### R2. First Proof Generation
- Implement two durable packs: `ggen-pack-clap-noun-verb` and `ggen-pack-tower-lsp-max` (which depends on `clap-noun-verb`).
- Ensure `tower-lsp-max-pack` can project/generate target files into `/Users/sac/tower-lsp-max/examples/clap-noun-verb-lsp/`.
- The generation must produce `ProjectionMap`, `CustomizationMap`, and `ReceiptIndex` in the output directories.
- Receipts should be exportable toward `wasm4pm`.

### R3. `ggen-lsp` Meta-Observer & Pack LSP Surfaces
- Implement/extend `ggen-lsp` in `~/ggen/crates/ggen-lsp` to read the maps and receipts, producing the following diagnostics:
  - `GGEN-PROJECTED-001` (file/range projected by pack)
  - `GGEN-DRIFT-001` (projected range differs from template/expected output)
  - `GGEN-EVIDENCE-001` (artifact lacks projection receipt)
  - `GGEN-CUSTOMIZE-001` (required customization point incomplete)
  - `GGEN-OVERRIDE-001` (override exists but not receipted)
- Support projection opportunity detection:
  - `CouldBeProjected` / `ShouldBeProjected` diagnostics (e.g. matching signatures/patterns).
- Provide Pack LSP surfaces (declarative or typed providers) for both packs.
- In `tower-lsp-max` (located in `/Users/sac/tower-lsp-max`), compose the `ggen-lsp` diagnostics/hints and the Pack LSP surfaces, preserving source attribution (no anonymous merge).

## Acceptance Criteria

### Verification
- [ ] **Positive Validation**:
  - Run pack descriptor validation and dependency resolution.
  - Successfully construct a `PackPlan` and generate the projected code into `examples/clap-noun-verb-lsp`.
  - Verify `ProjectionMap`, `CustomizationMap`, and `ReceiptIndex` are correctly written.
  - Run `ggen-lsp` diagnostics on the generated project to ensure files are marked as projected and customization/receipt states are correct.
  - Verify `tower-lsp-max` correctly routes/composes diagnostics and inlay hints with correct source attribution.
- [ ] **Bypass-Kill Validation**:
  - Removing the `clap-noun-verb-pack` dependency must cause the proof resolution or build to fail.
  - Deleting/tampering with the `ProjectionMap` or `ReceiptIndex` must trigger `GGEN-EVIDENCE-001` or `GGEN-DRIFT-001` diagnostics.
  - Modifying any projected code line manually must trigger a `GGEN-DRIFT-001` diagnostic.
  - Disabling projection signatures must cause opportunity detection diagnostics to fail to fire.
  - Striping source attribution from composite output must fail the composition unit tests.

## Follow-up — 2026-06-06T20:23:30Z

Implement ggen Projection Intelligence, introducing durable versioned pack artifacts, declarative/provider Pack LSPs, the `ggen-lsp` meta-observer, and the composite `tower-lsp-max` LSP. The primary proof of completion is `ggen-pack-tower-lsp-max` (which depends on `ggen-pack-clap-noun-verb`) generating the functional `examples/clap-noun-verb-lsp` containing projection maps, customization maps, receipt indexes, and diagnostics.

Working directory: /Users/sac/ggen
Integrity mode: benchmark

Reference Material:
- Existing hand-authored tower-lsp-max implementation: `/Users/sac/tower-lsp-max`
- Target hand-authored examples/clap-noun-verb-lsp to project: `/Users/sac/tower-lsp-max/examples/clap-noun-verb-lsp`

## Requirements

### R1. Pack & Projection Core Model
Implement `PackDescriptor`, `PackPlan`, `ProjectionMap`, `CustomizationMap`, `ReceiptIndex`, and staging/sync write gate in the `ggen` pipeline. Pack metadata will be defined in a `pack.toml` file containing projection signatures and customization points.

### R2. Pack LSP Contract & ggen-lsp Meta-Observer
Implement the `ggen-lsp` meta-observer server which watches projection boundaries, diagnosing projected files, required customization points, drift from templates, and missing receipts. It will also detect manual code that fits known pack patterns ("should/could be projected").

### R3. tower-lsp-max Composition
Implement `tower-lsp-max` to compose multiple LSP surfaces (`ggen-lsp`, pack-specific LSPs, and downstream language servers like `rust-analyzer`), maintaining proper source attribution (source_id) for every diagnostic, code action, and inlay hint.

### R4. Process Evidence & wasm4pm Export
Support exporting receipts in a process-evidence shape for future ingestion and process mining by `wasm4pm`.

### R5. E2E Proof of Concept
Verify that `ggen-pack-tower-lsp-max` depends on `ggen-pack-clap-noun-verb` and generates a fully functioning `examples/clap-noun-verb-lsp` with projection maps, customization maps, receipt indexes, and active LSP diagnostics.

## Acceptance Criteria

### Execution & Integration
- [ ] `ggen-pack-clap-noun-verb` and `ggen-pack-tower-lsp-max` exist as durable packs with valid `pack.toml` metadata.
- [ ] Running the projection sync generates `examples/clap-noun-verb-lsp/` with valid `projection-map.json`, `customization-map.json`, and `receipts.jsonl`.
- [ ] The generated CLI and LSP applications in `examples/clap-noun-verb-lsp/` are fully buildable and run correctly.
- [ ] `tower-lsp-max` correctly initializes, routes, and composes diagnostics from `ggen-lsp`, `rust-analyzer`, and the pack-specific LSPs with source attribution.

### Diagnostics & Inlay Hints
- [ ] `ggen-lsp` reports diagnostics for projected code (`GGEN-PROJECTED-001`), drifted content (`GGEN-DRIFT-001`), missing receipts (`GGEN-EVIDENCE-001`), and incomplete customization points (`GGEN-CUSTOMIZE-001`).
- [ ] `ggen-lsp` identifies manual files that match projection signatures and reports them as projection opportunities (`GGEN-PROJECT-OPPORTUNITY-001`).

### Robustness & Bypass-Kill Verification
- [ ] A test suite verifies that removing dependencies, omitting receipts, or manual editing of projected files without an override triggers the appropriate compiler or diagnostic errors.

## Follow-up — 2026-06-06T22:57:44Z

Execute the GC003 team for Boundary-Receipted Equation Enforcement inside the projection engine.

Working directory: `/Users/sac/ggen`
Integrity mode: benchmark

## Requirements

### R1. Boundary-Receipted Equation Enforcement
Implement or verify the equation $R_B \vdash A = \mu(O^*_B)$ inside the projection engine using the owning surfaces in `~/ggen`:
- `crates/ggen-projection/`
- `crates/ggen-lsp/`
- `crates/ggen-pack-gall-checkpoint-proof/`

The producing workspace boundary must be `~/ggen` and branch `feat/ggen-lsp-source-laws`.

### R2. Downstream Export & Mutation Restrictions
Do not use `~/tower-lsp-max` as the authority for GC003. Do not mutate `~/tower-lsp-max` unless the mutation is declared as an exported receipt artifact with the following metadata:
- `producing_workspace = ~/ggen`
- `storing_workspace = ~/tower-lsp-max`
- `export_reason = checkpoint_receipt_archive | downstream_playground_receipt`
- `exported_artifact_digest`
- `export_receipt_digest`

No checkpoint status may be admitted from `~/tower-lsp-max` for GC003.

### R3. Clean Sandboxed Boundary
All execution must use the following clean boundary paths strictly:
- `workspace = ~/ggen`
- `target = ~/ggen/.tmp_gc003/target`
- `staging = ~/ggen/.tmp_gc003/staging`
- `receipt_sink = ~/ggen/.tmp_gc003/receipts`
- `proof_pack = crates/ggen-pack-gall-checkpoint-proof`

### R4. Complete & Verifiable Implementation
No stubs, mocks, or placeholder hashes. All tests must be real Chicago-style tests using actual cryptographic derivations (BLAKE3) and real OpenTelemetry traces where applicable. Follow the AGENTS.md constitution.

## Acceptance Criteria

### Execution & Isolation
- [ ] The entire execution runs within `~/ggen`. No mutations are made to `~/tower-lsp-max` unless declared as an exported receipt artifact containing the five required fields.
- [ ] Target outputs are generated into `.tmp_gc003/target`, staging into `.tmp_gc003/staging`, and receipts into `.tmp_gc003/receipts`.
- [ ] No checkpoint status is read or admitted from `~/tower-lsp-max` for GC003.

### Mathematical Correctness & Verification
- [ ] The equation $R_B \vdash A = \mu(O^*_B)$ is successfully enforced, and all 12 proofs in `crates/ggen-pack-gall-checkpoint-proof/manifest.toml` are verified.
- [ ] All tests in `crates/ggen-projection` (including `dogfood_gc003.rs` and `f8_equation_enforcement.rs`) compile and pass successfully.

## Follow-up — 2026-06-06T23:07:25Z

Please execute the following GC004 capability requirements as an addition to the active GC003 execution.

## Requirements

### R1. Reusable LSP Test Harness
Build a reusable test harness in `crates/ggen-lsp/tests/common/lsp_harness.rs` that exposes only LSP operations:
- `initialize()`
- `did_open(uri, text)`
- `did_change(uri, new_text)`
- `request_code_action(uri, range)`
- `execute_command(command, args)`
- `wait_for_publish_diagnostics(uri)`
- `assert_diagnostic_code(code)`
- `assert_source_id(source_id)`
- `assert_no_diagnostic_from(source_id)`

The test harness must NOT expose internal server state or call functions like `compute_observer_diagnostics()`, `validate_sync()`, etc.

### R2. Protocol Path Enforcement
Ensure every GC004 proof goes through the full LSP pipeline:
`Test Harness → JSON-RPC initialize → textDocument/didOpen → textDocument/didChange → server handler → ggen-lsp observer registry → pack-domain observer → diagnostic merger → textDocument/publishDiagnostics → test assertion`

And for code actions:
`textDocument/codeAction → PackActionIntent → PackPlan → Staging → MutationGate → Receipt`

### R3. Admission Test Categories
Implement the following five protocol test categories:
1. **Diagnostic protocol tests**: initialize → didOpen generated file → wait publishDiagnostics → assert `GGEN-PROJECTED-001` and source_id `ggen_lsp_observer`.
2. **Drift protocol tests**: didOpen projected file → didChange modified text → wait publishDiagnostics → assert `GGEN-DRIFT-001`.
3. **Pack-domain diagnostic tests**:
   - open malformed clap command file → wait publishDiagnostics → assert `CLAP-PACK-*` and source_id `clap_noun_verb_pack_lsp`.
   - open malformed tower LSP composition file → wait publishDiagnostics → assert `TOWER-PACK-*` and source_id `tower_lsp_max_pack_lsp`.
4. **Authority split tests**: corrupt receipt → expect `GGEN-RECEIPT-*` (no `CLAP-*` or `TOWER-*` diagnostic owns it); corrupt clap domain shape → expect `CLAP-*` (ggen-lsp must not invent `CLAP-*` itself).
5. **Code action routing tests**: request codeAction → assert kind/intention → executeCommand → assert no direct write → assert PackPlan/Staging/MutationGate/Receipt path.

### R4. Source-Level Bypass-Kills (Anti-Bypass)
Add a generated dogfood test to scan admission tests and enforce that:
- `BYPASS-LSP-001`: Admission tests do not call `compute_observer_diagnostics` directly.
- `BYPASS-LSP-002`: Admission tests do not call `validate_sync` directly.
- `BYPASS-LSP-003`: Admission tests do not inspect private server state.
- `BYPASS-LSP-004`: Admission tests do not write files except through LSP codeAction → PackPlan → MutationGate.
- `BYPASS-LSP-005`: Diagnostics are asserted from `publishDiagnostics` payloads only.

Forbidden symbols to scan: `compute_observer_diagnostics`, `analyze_and_observe`, `validate_sync`, `observe_pack_domain`, `state.diagnostics`, `direct_write`, `std::fs::write`.

## Acceptance Criteria

### Verification & Harness Implementation
- [ ] Reusable LSP harness exists in `crates/ggen-lsp/tests/common/lsp_harness.rs`.
- [ ] Dogfood-generated tests use only the harness and make no internal server calls or private state assertions.
- [ ] All 4 proofs in the GC004 proof manifest are fully implemented and pass successfully.
- [ ] Direct file writes (except for explicitly marked fixture setup) are blocked or fail verification.

## Follow-up — 2026-06-06T23:25:50Z

Please update your execution parameters to enforce the following "No-Fake Surface Law":

A system may not claim a capability by naming its intended abstraction. It must actuate the concrete surface that makes the abstraction real. 
- "LSP tested" is fake unless LSPClient → stdio → tower_lsp → publishDiagnostics happens.
- "Validated" is fake unless SHACL runs over RDF/TTL graph material.
- "Receipt" is fake unless BLAKE3 binds the canonical equation context.
- "Admitted" is fake unless the checkpoint admission bar is passed.

Please ensure the Project Orchestrator implements this doctrine strictly, rejecting any vibe-based, simulated, or placeholder validations.

Also, classify the current operational status parameters exactly as follows:
- GC004 doctrine pressure: ALIVE
- No-fake surface law: ALIVE
- GC003/GC004 task merge: PARTIAL_ALIVE
- Template mapping: PARTIAL_ALIVE
- LSP stdio client setup: IN_PROGRESS
- Heartbeat/liveness claim: UNSUPPORTED until receipted

## Follow-up — 2026-06-06T23:35:02Z

Please execute the following GC005 capability requirements as an addition to the active execution task.

## Requirements

### R1. Reclassify Current Receipts
Reclassify existing `GALL-CHECKPOINT-*.txt` and `*.receipt.json` files in `crates/playground/receipts/` as reports/human-readable narratives or legacy receipt-shaped summaries, not admission authorities. Add a fence declaring they are not admission receipts.

### R2. Canonical OCEL Schema & Objects
Create an OCEL object and event schema supporting the minimum universe:
- **Object Types**: `Workspace`, `BoundaryLedger`, `Checkpoint`, `Pack`, `Template`, `PackPlan`, `StagingArea`, `MutationGate`, `Artifact`, `Diagnostic`, `LspServer`, `LspClient`, `Observer`, `ReceiptLedger`, `TestRun`, `Commit`, `ExportRecord`, `Refusal`, `ConformanceModel`, `ReplayVerdict`.
- **Event Types**: `BoundaryDeclared`, `WorkspaceAdmitted`, `PackLoaded`, `TemplateResolved`, `TemplateRendered`, `PackPlanConstructed`, `StagingPrepared`, `MutationGateAdmitted`, `MutationGateDenied`, `ArtifactWritten`, `DigestComputed`, `ReceiptEventAppended`, `ReceiptChainLinked`, `ValidationPassed`, `ValidationFailed`, `LspServerSpawned`, `LspClientInitialized`, `DocumentOpened`, `DocumentChanged`, `DiagnosticPublished`, `DiagnosticAttributed`, `CodeActionReturned`, `CommandExecuted`, `ObserverWriteScanPerformed`, `ObserverWriteScanPassed`, `ObserverWriteScanFailed`, `DogfoodTestProjected`, `DogfoodTestDeleted`, `DogfoodTestReprojected`, `TestRunStarted`, `TestRunPassed`, `TestRunFailed`, `ExportRecordCreated`, `CheckpointCandidateDeclared`, `CheckpointAdmitted`, `CheckpointRefused`, `ReplayStarted`, `ReplayCompleted`, `ConformanceVerdictEmitted`.

### R3. Deterministic OCEL Event Emission
Implement deterministic event emission matching the above event types into append-only JSON/JSONL format in `/Users/sac/ggen`:
- `crates/playground/ocel/events.jsonl`
- `crates/playground/ocel/objects.jsonl`
- `crates/playground/ocel/digests.jsonl`
- `crates/playground/ocel/verdicts.jsonl`

### R4. Cryptographic Digest Chain & Verdict verification
Compute BLAKE3 hashes over canonical events and link each event sequence. Implement a verifier that reads these OCEL logs and produces a conformance verdict (`FIT | DEVIATION | BLOCKED | INCONCLUSIVE`).

## Acceptance Criteria

### Execution & Demotion
- [ ] Existing narrative and markdown receipts are successfully demoted and marked with a warning fence.
- [ ] A canonical OCEL ledger exists with JSON/JSONL events, objects, digests, and verdicts under `crates/playground/ocel/`.
- [ ] No checkpoint admission is accepted without the required OCEL event coverage and valid cryptographic BLAKE3 chaining.
- [ ] The verifier accurately computes replay verdicts over the OCEL logs.

## Follow-up — 2026-06-06T23:47:46Z

Correction to working directories for GC005:

Please execute the GC005 capability requirements using `~/wasm4pm` and `~/wasm4pm-compat` as the working directories, not `~/ggen`. The target files (events.jsonl, objects.jsonl, digests.jsonl, verdicts.jsonl) must be written under `crates/playground/ocel/` in both `~/wasm4pm` and `~/wasm4pm-compat`, and the wasm4pm replay verifications must execute in those directories.

Here is the corrected specification:

## Requirements

### R1. Reclassify Current Receipts
Reclassify existing `GALL-CHECKPOINT-*.txt` and `*.receipt.json` files in `crates/playground/receipts/` or legacy directories in `~/wasm4pm` and `~/wasm4pm-compat` as reports/human-readable narratives or legacy receipt-shaped summaries, not admission authorities. Add a fence declaring they are not admission receipts.

### R2. Canonical OCEL Schema & Objects
Create an OCEL object and event schema supporting the minimum universe:
- **Object Types**: `Workspace`, `BoundaryLedger`, `Checkpoint`, `Pack`, `Template`, `PackPlan`, `StagingArea`, `MutationGate`, `Artifact`, `Diagnostic`, `LspServer`, `LspClient`, `Observer`, `ReceiptLedger`, `TestRun`, `Commit`, `ExportRecord`, `Refusal`, `ConformanceModel`, `ReplayVerdict`.
- **Event Types**: `BoundaryDeclared`, `WorkspaceAdmitted`, `PackLoaded`, `TemplateResolved`, `TemplateRendered`, `PackPlanConstructed`, `StagingPrepared`, `MutationGateAdmitted`, `MutationGateDenied`, `ArtifactWritten`, `DigestComputed`, `ReceiptEventAppended`, `ReceiptChainLinked`, `ValidationPassed`, `ValidationFailed`, `LspServerSpawned`, `LspClientInitialized`, `DocumentOpened`, `DocumentChanged`, `DiagnosticPublished`, `DiagnosticAttributed`, `CodeActionReturned`, `CommandExecuted`, `ObserverWriteScanPerformed`, `ObserverWriteScanPassed`, `ObserverWriteScanFailed`, `DogfoodTestProjected`, `DogfoodTestDeleted`, `DogfoodTestReprojected`, `TestRunStarted`, `TestRunPassed`, `TestRunFailed`, `ExportRecordCreated`, `CheckpointCandidateDeclared`, `CheckpointAdmitted`, `CheckpointRefused`, `ReplayStarted`, `ReplayCompleted`, `ConformanceVerdictEmitted`.

### R3. Deterministic OCEL Event Emission in wasm4pm/wasm4pm-compat
Implement deterministic event emission matching the above event types into append-only JSON/JSONL format in `~/wasm4pm` and `~/wasm4pm-compat`:
- `crates/playground/ocel/events.jsonl`
- `crates/playground/ocel/objects.jsonl`
- `crates/playground/ocel/digests.jsonl`
- `crates/playground/ocel/verdicts.jsonl`

### R4. Cryptographic Digest Chain & Verdict verification via wasm4pm
Compute BLAKE3 hashes over canonical events and link each event sequence. Implement a wasm4pm verifier that reads these OCEL logs and produces a conformance verdict (`FIT | DEVIATION | BLOCKED | INCONCLUSIVE`).

## Acceptance Criteria

### Execution & Demotion
- [ ] Existing narrative and markdown receipts are successfully demoted and marked with a warning fence.
- [ ] A canonical OCEL ledger exists with JSON/JSONL events, objects, digests, and verdicts under `crates/playground/ocel/` in `~/wasm4pm` and `~/wasm4pm-compat`.
- [ ] No checkpoint admission is accepted without the required OCEL event coverage and valid cryptographic BLAKE3 chaining.
- [ ] The wasm4pm replay verifier accurately computes replay verdicts over the OCEL logs.
- [ ] All check/test runs compile and pass successfully inside `~/wasm4pm` and `~/wasm4pm-compat`.
