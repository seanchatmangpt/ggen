# Original User Request

## 2026-06-06T22:58:06Z

You are the Project Orchestrator for the GC003 milestone (Boundary-Receipted Equation Enforcement).

Your working directory: `/Users/sac/ggen/.agents/teamwork_preview_orchestrator_gc003/`
Workspace path: `/Users/sac/ggen`

Your tasks:
1. Initialize your BRIEFING.md and plan.md in your working directory.
2. Read the latest request in `/Users/sac/ggen/.agents/ORIGINAL_REQUEST.md` and check files under `/Users/sac/ggen` (specifically crates/ggen-projection/, crates/ggen-lsp/, and crates/ggen-pack-gall-checkpoint-proof/).
3. Orchestrate the implementation/verification of $R_B \vdash A = \mu(O^*_B)$ inside the projection engine.
4. Ensure all execution, staging, target, receipts, and proof pack paths strictly follow R3.
5. Adhere to the AGENTS.md constitution: no stubs, mocks, or placeholder hashes; write real Chicago-style tests.
6. Actively update progress.md in your working directory so that the Sentinel can monitor your progress.
7. Report to the Sentinel when the milestone is complete.

## 2026-06-06T23:07:55Z

Hello Project Orchestrator,

The parent agent has requested the addition of the following GC004 capability requirements to your active execution. Please incorporate these requirements into your plan, milestones, and work items:

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
- [ ] `cargo make check` and `cargo make test` pass successfully on the `feat/ggen-lsp-source-laws` branch.

## 2026-06-06T23:26:03Z

Hello Project Orchestrator,

Please update your execution parameters to enforce the following "No-Fake Surface Law" in your active milestones and execution:

A system may not claim a capability by naming its intended abstraction. It must actuate the concrete surface that makes the abstraction real. 
- "LSP tested" is fake unless LSPClient → stdio → tower_lsp → publishDiagnostics happens.
- "Validated" is fake unless SHACL runs over RDF/TTL graph material.
- "Receipt" is fake unless BLAKE3 binds the canonical equation context.
- "Admitted" is fake unless the checkpoint admission bar is passed.

Please implement this doctrine strictly, rejecting any vibe-based, simulated, or placeholder validations.

Also, classify the current operational status parameters exactly as follows:
- GC004 doctrine pressure: ALIVE
- No-fake surface law: ALIVE
- GC003/GC004 task merge: PARTIAL_ALIVE
- Template mapping: PARTIAL_ALIVE
- LSP stdio client setup: IN_PROGRESS
- Heartbeat/liveness claim: UNSUPPORTED until receipted
- GC004_ADMITTED_BY_DOGFOOD: NOT SHOWN

## 2026-06-06T23:35:20Z

Hello Project Orchestrator,

Please incorporate the following GC005 capability requirements into your active milestones and execution:

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
- [ ] `cargo make check` and `cargo make test` pass successfully on the `feat/ggen-lsp-source-laws` branch.

## 2026-06-06T23:48:04Z

Hello Project Orchestrator,

Please note the following correction to the working directories for the GC005 capability requirements:

You must execute the GC005 capability requirements using `~/wasm4pm` and `~/wasm4pm-compat` as the working directories, not `~/ggen`. The target files (events.jsonl, objects.jsonl, digests.jsonl, verdicts.jsonl) must be written under `crates/playground/ocel/` in both `~/wasm4pm` and `~/wasm4pm-compat`, and the wasm4pm replay verifications must execute in those directories.

Here is the corrected specification:

## Requirements

### R1. Reclassify Current Receipts
Reclassify existing `GALL-CHECKPOINT-*.txt` and `*.receipt.json` files in `crates/playground/receipts/` or legacy directories in `~/wasm4pm` and `~/wasm4pm-compat` as reports/human-readable narratives or legacy receipt-shaped summaries, not admission authorities. Add a fence declaring they are not admission receipts.

### R3. Deterministic OCEL Event Emission in wasm4pm/wasm4pm-compat
Implement deterministic event emission matching the event types into append-only JSON/JSONL format in `~/wasm4pm` and `~/wasm4pm-compat`:
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




