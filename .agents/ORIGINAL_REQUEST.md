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

## Follow-up — 2026-06-07T00:12:48Z

Status accepted as partial operational evidence.

Corrections:
  - Remove or receipt the liveness heartbeat claim.
  - Update stale status from MIGRATING_TO_OCEL_ONLY to the current GC005 state.
  - Do not claim verifier completion until the verifier output is shown.
  - Preserve the admitted distinction:
      wasm4pm-lsp dogfood integration is alive;
      final verifier validation remains pending until concrete command output is provided.

Required next receipt:
  Show the verifier validation command and output proving FIT / DEVIATION / BLOCKED

## Follow-up — 2026-06-07T00:31:15Z

Please update your execution parameters to enforce the "GC005 Correct Architecture — C4 + Filesystem Law" immediately:

## Requirements

### R1. Sealed Read-Only Workspace Boundaries
`~/wasm4pm` and `~/wasm4pm-compat` are sealed read-only authority workspaces.
- Do NOT write into `~/wasm4pm` or `~/wasm4pm-compat`.
- Do NOT create fake local `wasm4pm`, `wasm4pm-proper`, or `wasm4pm-compat` crates/directories in `~/ggen` or `~/tower-lsp-max`.
- If a local compatibility adapter exists, rename/fence it as `gc005-wasm4pm-adapter`, `ggen-wasm4pm-adapter`, or `tower-wasm4pm-adapter`.

### R2. Neutral wasm4pm Adapter
Implement `gc005-wasm4pm-adapter` to act as a neutral adapter:
- It may translate inputs, call sealed libraries/CLIs, and normalize returned issues/verdicts.
- It must NOT implement replay/conformance itself, emit FIT from local logic, or write to sealed workspaces.
- If the required sealed authority is unavailable, it must emit BLOCKED.

### R3. wasm4pm-lsp Server
Implement or align `wasm4pm-lsp` to act as a process-evidence observer only:
- Owns only `WASM4PM-*` diagnostics (e.g., `WASM4PM-EVIDENCE-MISSING`, `WASM4PM-OCEL-SHAPE-INVALID`, `WASM4PM-DIGEST-CHAIN-BROKEN`, `WASM4PM-CONFORMANCE-BLOCKED`, `WASM4PM-REPLAY-DEVIATION`, `WASM4PM-VERDICT-FIT`, `WASM4PM-VERDICT-DEVIATION`, `WASM4PM-VERDICT-BLOCKED`, `WASM4PM-VERDICT-INCONCLUSIVE`).
- It does NOT own `GGEN-*`, `CLAP-*`, or `TOWER-*` diagnostics.
- It delegates evaluation to `gc005-wasm4pm-adapter` using a document buffer and adapter client, then publishes mapped diagnostics over standard stdio JSON-RPC.

### R4. Proof Projection Harness
Ensure the projection engine (`sync_target`) projects the protocol-only proof test `dogfood_gc005.rs` from `dogfood_gc005.rs.tmpl` template in `ggen-pack-gall-checkpoint-proof`.
- Test must use LSP wire (`initialize` → `textDocument/didOpen` → `textDocument/publishDiagnostics`).
- No hardcoded `/Users/sac/` paths in the generated tests.
- FIT fixture must return `WASM4PM-VERDICT-FIT`.
- Missing boundary fixture must return `WASM4PM-CONFORMANCE-BLOCKED`.
- Artifact-before-staging fixture must return `WASM4PM-REPLAY-DEVIATION`.
- Broken receipt chain fixture must return `WASM4PM-DIGEST-CHAIN-BROKEN` or `WASM4PM-CONFORMANCE-BLOCKED`.

## Acceptance Criteria

### Verification & Alignment
- [ ] No writes to `~/wasm4pm` or `~/wasm4pm-compat` are performed by the agents.
- [ ] No local fake or shadow `wasm4pm` or `wasm4pm-compat` crates exist in `~/ggen` or `~/tower-lsp-max`.
- [ ] `wasm4pm-lsp` delegates to `gc005-wasm4pm-adapter` which calls sealed authorities.
- [ ] `dogfood_gc005.rs` executes real LSP protocol flow over stdio and asserts the required verdicts correctly.

## Follow-up — 2026-06-07T00:34:04Z

Please spawn 5 subagents concurrently in your orchestrator swarm to close the remaining gaps for GC003/GC004/GC005:

1. **Worker 1 (LSP Test Harness)**: Implement the reusable LSP test harness in `crates/ggen-lsp/tests/common/lsp_harness.rs`.
2. **Worker 2 (Admission Tests & Protocol Paths)**: Implement the 5 admission test categories using the harness and verify the full LSP pipeline flow.
3. **Worker 3 (Anti-Bypass Scanner)**: Implement the generated dogfood test to scan for forbidden symbols and enforce bypass-kills.
4. **Worker 4 (OCEL Logs & Emitters)**: Implement deterministic OCEL logging in `~/wasm4pm` and `~/wasm4pm-compat`.
5. **Worker 5 (Verifier & Verdicts)**: Implement the wasm4pm verifier and verdict logic (`FIT | DEVIATION | BLOCKED | INCONCLUSIVE`).


## Follow-up — 2026-06-07T00:39:29Z

Please execute the following GC006 capability requirements as the next milestone:

## Requirements

### R1. Authority Surface Manifest
Add an `authority_surface.toml` (or equivalent manifest) in `~/ggen` and `~/tower-lsp-max` declaring the following parameters:
- `sealed_workspace: ~/wasm4pm`
- `sealed_compat_workspace: ~/wasm4pm-compat`
- `conformance_function: wasm4pm_algos::gall::check_gall_conformance`
- `observer: wasm4pm-lsp`
- `adapter: gc005-wasm4pm-adapter`

### R2. Forbidden Local Shadow Crate Guard
Add tests that fail if any forbidden local shadow workspace member/crate exists under `~/ggen/crates/` or `~/tower-lsp-max/crates/` with names:
- `wasm4pm`
- `wasm4pm-proper`
- `wasm4pm-compat`

### R3. Neutral Adapter & Local Replay Check
Add tests verifying that:
- `wasm4pm-lsp` does NOT perform conformance/replay logic locally.
- `gc005-wasm4pm-adapter` fails (produces `BLOCKED` instead of `FIT`) if the sealed authority is unavailable or if adapter logic tries to manufacture local verdicts without calling the sealed surface.

### R4. Read-Only Workspace Enforcement
Add checks that verify sealed workspaces (`~/wasm4pm` and `~/wasm4pm-compat`) remain unmodified and contain no generated files from the GC005/GC006 pipeline execution.

### R5. C4 Architecture Compliance
Add a C4 architecture compliance verification test that verifies the layout matches the architecture contract (ggen manufactures proof surfaces, tower-lsp-max hosts integration surfaces, wasm4pm-lsp observes process evidence, neutral adapter calls sealed authorities, and wasm4pm/compat decide replay/conformance).

## Acceptance Criteria

### Verification & Manifest Checks
- [ ] `authority_surface.toml` exists and correctly declares workspace/function paths.
- [ ] Anti-regression tests fail if local shadow crates are added or if authority is mutated.
- [ ] Adapter failure tests confirm it refuses to generate `FIT` verdicts locally when the sealed authority is missing.
- [ ] Sealed workspaces remain 100% read-only and free of generated files.

## Follow-up — 2026-06-07T00:45:56Z

STOP.

The swarm status is checkpoint-stale.

Do not continue OCEL logging/verifier/verdict implementation unless it is strictly
inside the sealed wasm4pm authority path.

Do not implement append-only JSONL verifier authority inside ggen.

Do not report GC004 LSP harness workers as GC006 progress.

Do not set or claim heartbeat timers unless there is a real scheduler receipt.

Current task is GC006 only:

  Authority Surface Lock.

Required:
  1. Use the existing C4 architecture contract.
  2. Resolve the canonical proof pack path.
  3. Register dogfood_gc006.rs.tmpl in the real proof pack.
  4. Remove all hardcoded /Users/sac paths from the template.
  5. Project dogfood_gc006.rs through sync_target.
  6. Run dogfood_gc006.
  7. Prove:
       no shadow wasm4pm crates
       wasm4pm-lsp does not own conformance logic
       adapter does not emit fake FIT
       adapter calls sealed wasm4pm authority
       sealed workspaces are read-only

Status must remain:
  GC006_DRAFT / PROJECTION_BLOCKED


## Follow-up — 2026-06-07T00:47:25Z

Status corrections:
  - Remove or receipt any liveness heartbeat claims.
  - Awaiting next concrete worker output.
  - No completion claim will be made until command output, projected files, and test results are available.

Please align your internal briefing and handoff records with the following canonical state:

Checkpoint state:
  GC001-GC004:
    ADMITTED per prior checkpoint scopes

  GC005:
    CONDITIONALLY_ADMITTED_BY_DOGFOOD
    valid only under sealed-authority path:
      wasm4pm-lsp
      → gc005-wasm4pm-adapter
      → wasm4pm_algos::gall::check_gall_conformance

  GC006:
    DRAFT / PROJECTION_BLOCKED

Rejected:
  - heartbeat claims
  - stale verifier-pending status
  - ad hoc OCEL logging/verifier workers
  - GC004 work reported as GC006 progress
  - local fake wasm4pm / wasm4pm-proper / wasm4pm-compat authority

## Follow-up — 2026-06-07T00:57:59Z

# Teamwork Project Prompt — Draft

Implement **GC005A: Sealed wasm4pm Replay Surface Contract** and establish **Sealed Repo Integrity Baselines** to completely close the verification gate.

Working directories:
- `/Users/sac/ggen` (mutable)
- `/Users/sac/tower-lsp-max` (mutable)
- `/Users/sac/wasm4pm` (sealed read-only)
- `/Users/sac/wasm4pm-compat` (sealed read-only)

Integrity mode: benchmark

## Requirements

### R1. Sealed wasm4pm Replay Surface (GC005A)
Ensure that the sealed authority `~/wasm4pm` contains a real, callable replay/conformance library (`wasm4pm_algos::gall::check_gall_conformance`) that:
- Accepts OCEL/JSONL process evidence.
- Returns a structured enum verdict: `FIT` / `DEVIATION` / `BLOCKED` / `INCONCLUSIVE`.
The adapter `gc005-wasm4pm-adapter` must only act as a neutral forwarding/normalizing transport, and `wasm4pm-lsp` must only observe and publish diagnostics.

### R2. LSP Integration Testing
Verify through `dogfood_gc005.rs` that the complete path (`wasm4pm-lsp` → `gc005-wasm4pm-adapter` → sealed `wasm4pm` replay API) is executed under a real stdio tower-lsp integration boundary, publishing diagnostics with `WASM4PM-*` error codes.

### R3. Sealed Workspace Sterility Baselines
Create baseline manifest files `.gc-sealed-baseline` in the root of `~/wasm4pm` and `~/wasm4pm-compat`. The baseline manifest must record:
- Tracked git status.
- Ignored artifact inventory (e.g., target, node_modules, log/csv outputs).
- Allowed ignored directories and forbidden generated paths.
- A cryptographic digest over the baseline manifest content.

The integration tests (such as `dogfood_gc006.rs` or workspace checks) must parse these manifests and assert that:
- There are no new tracked changes.
- There are no new untracked non-baselined files.
- There are no new ignored files outside the baselined inventory.
- No writes have occurred from `ggen` or `tower-lsp-max` into the sealed repositories.

## Acceptance Criteria

### GC005A Replay Contract
- [ ] `wasm4pm_algos::gall::check_gall_conformance` parses raw OCEL/JSONL and evaluates conformance deterministically.
- [ ] `dogfood_gc005` test passes and proves end-to-end execution of the real replay surface via Tower LSP diagnostics.

### Workspace Baselines & Cleanliness
- [ ] `.gc-sealed-baseline` manifests exist and are valid inside `/Users/sac/wasm4pm` and `/Users/sac/wasm4pm-compat`.
- [ ] Git status verification asserts zero new tracked, untracked, or non-baselined ignored modifications against the baseline manifests.
- [ ] All 7 closure gate commands run and pass successfully.

## Follow-up — 2026-06-07T01:00:00Z

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
- [ ] `cargo make check` and `cargo make test` pass successfully on the `feat/ggen-lsp-source-laws` branch.

## Follow-up — 2026-06-06T20:52:29-07:00

# Teamwork Project Prompt — Draft

Verify the CLAP command grammar admission (GC008B) and lawful mutation route (GC008C) under Gall cash-busting mode.

Working directory: `/Users/sac/ggen`
Integrity mode: benchmark

## Requirements

### R1. GC008B — CLAP Command Grammar Admission
Prove that `conformance-receipt.bind` is admitted by the actual CLAP noun/verb authority, while `wasm4pm.bind_receipt` and other malformed commands are refused.

### R2. GC008C — PackPlan / Staging / MutationGate Route
Prove that a valid `conformance-receipt.bind` intent flows strictly through:
```text
CodeAction → PackActionIntent → PackPlan → Staging → MutationGate → Receipt
```
with negative controls verifying that direct command executions or direct file writes are blocked/refused.

## Acceptance Criteria

### Verification Structures
- `GC008B_STATUS` must be reported as one of: `BLOCKED`, `CANDIDATE`, `REPORTED_ADMITTED_BY_DOGFOOD`.
- `GC008C_STATUS` must be reported as one of: `BLOCKED`, `CANDIDATE`, `REPORTED_ADMITTED_BY_DOGFOOD`.
- The final output must match the exact YAML/markdown block schema requested, providing a `FAILSET` list, `RAW_PROOFS` with commands and digests, `LSP_TRANSCRIPTS`, `NEGATIVE_CONTROLS`, and `FORBIDDEN_CLAIMS`.

### Anti-Cheating & Static Surveillance
- No shadow crates or fake returns allowed.
- No direct file mutations inside LSP observer path.
- No `v1.0.0` or `version = "1.0.0"` placeholders.

## Follow-up — 2026-06-07T03:54:40Z

# Final Teamwork Project Prompt — GC008B / GC008C

Status: Launched
Goal: Swarm verification in progress
Working directory: `/Users/sac/ggen`
Integrity mode: benchmark
Operating mode: Gall cash-busting
Success predicate: external evaluator only

## Mission

Verify the CLAP command grammar admission for **GC008B** and the lawful mutation route for **GC008C**.

Do not produce victory language.
Do not claim DONE.
Do not claim ADMITTED unless the proof boundary is actually satisfied.
Return failures, blockers, raw proofs, transcripts, and receipts only.

Current target:

| Checkpoint                                      | Current Status               |
| ----------------------------------------------- | ---------------------------- |
| GC008A: No LSP mutation lock                    | REPORTED_ADMITTED_BY_DOGFOOD |
| GC008B: CLAP validation                         | CANDIDATE                    |
| GC008C: PackPlan / Staging / MutationGate route | OPEN                         |

---

## Core Gall Law

All work must preserve:

[
R_B \vdash A = \mu(O^{*}_{B})
]

The LLM/team does not declare success.

External success predicate:

[
Done_B(A)
=========

[FailSet_B(A)=\varnothing]
\land
[R_B \vdash A=\mu(O^{*}_{B})]
]

Your job is to reduce the failset, not narrate victory.

---

# Requirements

## R1. GC008B — CLAP Command Grammar Admission

Prove that:

`conformance-receipt.bind`

is admitted by the actual CLAP noun/verb authority.

It is not sufficient for the string to merely look CLAP-shaped.

Required positive proof:

[
CLAPValidate(noun=\text{conformance-receipt}, verb=\text{bind}) = ADMITTED
]

Required negative proof:

[
CLAPValidate(command=\text{wasm4pm.bind_receipt}) = REFUSED
]

Also prove refusal for malformed or unauthorized variants:

* `receipt.bind`
* `wasm4pm.bind`
* `conformance.bind_receipt`
* `conformance-receipt.write`
* `conformance-receipt.apply`
* `conformance-receipt.mutate`
* `receipt.conformance-bind`

Accepted GC008B statuses only:

* `BLOCKED`
* `CANDIDATE`
* `REPORTED_ADMITTED_BY_DOGFOOD`

Forbidden GC008B claim:

* “CLAP governance proven” unless the actual CLAP authority validates the command and refuses the malformed controls.

---

## R2. GC008C — PackPlan / Staging / MutationGate Route

Prove that a valid `conformance-receipt.bind` intent flows strictly through:

`CodeAction → PackActionIntent → PackPlan → Staging → MutationGate → Receipt`

Required positive proof:

1. Real LSP protocol emits a `CodeAction`.
2. The returned action contains `conformance-receipt.bind`.
3. The action is converted into `PackActionIntent`.
4. `PackActionIntent` resolves into `PackPlan`.
5. `PackPlan` creates or requires `Staging`.
6. `Staging` requires `MutationGate`.
7. MutationGate denial prevents write.
8. Receipt emission requires admitted mutation.

Required negative controls:

* direct `executeCommand` is refused or absent
* direct `WorkspaceEdit` receipt binding is impossible
* command without CLAP admission is refused
* PackPlan bypass is refused
* Staging bypass is refused
* MutationGate denial blocks write
* receipt emission without admitted mutation refuses
* adapter cannot append receipt events
* LSP cannot write files
* LSP cannot manufacture receipt strings

Accepted GC008C statuses only:

* `BLOCKED`
* `CANDIDATE`
* `REPORTED_ADMITTED_BY_DOGFOOD`

Forbidden GC008C claim:

* “All mutations flow through PackPlan / Staging / MutationGate” unless the positive route and negative controls are both proven.

---

# Protocol Proof Requirement

At least one proof must use the real LSP boundary:

`initialize → didOpen → publishDiagnostics → textDocument/codeAction → returned conformance-receipt.bind intent`

Forbidden proof shortcuts:

* direct internal function calls as final proof
* private state inspection as final proof
* unit-only proof for protocol behavior
* string-only proof for CLAP validation
* static scan only for lawful mutation route

Static scans are allowed as supporting evidence, not final route proof.

---

# Anti-Cheating Static Surveillance

Scan all relevant boundaries:

* `/Users/sac/ggen`
* `/Users/sac/tower-lsp-max`
* `/Users/sac/wasm4pm`
* `/Users/sac/wasm4pm-compat`

Forbidden patterns:

* `wasm4pm.bind_receipt`
* `bind_conformance_receipt`
* `execute_command` mutation path
* `WorkspaceEdit` receipt binding
* `std::fs::write` in LSP mutation path
* `tokio::fs::write` in LSP mutation path
* `std::fs::read_to_string` in receipt-binding path
* `ocel.events.push` in adapter
* manual `FIT` return
* manual `ADMITTED` return
* `v1.0.0`
* `version = "1.0.0"`
* fake `wasm4pm` shadow crate
* fake `wasm4pm-lsp` shadow crate
* fake `wasm4pm-compat` shadow crate

If no forbidden patterns are found, include the exact scan commands and raw output.

---

# Acceptance Criteria

## Required Output Schema

Final output must match this structure:

```yaml
GC008B_STATUS: BLOCKED | CANDIDATE | REPORTED_ADMITTED_BY_DOGFOOD

GC008C_STATUS: BLOCKED | CANDIDATE | REPORTED_ADMITTED_BY_DOGFOOD

FAILSET:
  - failure_id:
    checkpoint:
    law:
    evidence:
    blocking:
    required_next_proof:

RAW_PROOFS:
  - proof_id:
    command:
    output_excerpt:
    receipt_path:
    digest:

LSP_TRANSCRIPTS:
  - transcript_id:
    path:
    covers:
      - initialize
      - didOpen
      - publishDiagnostics
      - textDocument/codeAction
    digest:

NEGATIVE_CONTROLS:
  - control_id:
    expected:
    observed:
    status:
    raw_output_excerpt:

ANTI_CHEAT_SCAN:
  commands:
    - command:
      output_excerpt:
      digest:
  forbidden_matches:
    - pattern:
      path:
      line:
      status:

FORBIDDEN_CLAIMS:
  - claim:
    reason:
```

---

# Status Rules

## Allowed

`GC008B_STATUS: REPORTED_ADMITTED_BY_DOGFOOD`

Only if:

* actual CLAP authority admits `conformance-receipt.bind`
* malformed controls are refused
* raw proof and receipt paths are provided

`GC008C_STATUS: CANDIDATE`

Allowed if:

* no LSP mutation lock still holds
* CodeAction intent exists
* route design is partially wired
* full PackPlan/Staging/MutationGate receipt path is not yet fully proven

`GC008C_STATUS: REPORTED_ADMITTED_BY_DOGFOOD`

Only if:

* LSP protocol proof exists
* CLAP validation proof exists
* PackActionIntent proof exists
* PackPlan proof exists
* Staging proof exists
* MutationGate denial proof exists
* receipt emission proof exists
* negative controls pass

## Forbidden

Do not say:

* “Victory confirmed”
* “All gaps resolved”
* “GC008 fully admitted”
* "CLAP governance proven”
* “All mutations flow through the gate”
* “The path is guaranteed”
* “No future bypass is possible”

unless the raw proof structure explicitly supports the claim.

---

# Agent Assignments

## Agent 1 — CLAP Grammar Auditor

Target: GC008B

Tasks:

1. Locate the actual CLAP noun/verb authority.
2. Validate `conformance-receipt.bind` through that authority.
3. Refuse `wasm4pm.bind_receipt`.
4. Refuse malformed command variants.
5. Return raw commands, outputs, receipts, and failset.

Do not accept string shape as validation.

---

## Agent 2 — LSP Protocol Auditor

Target: protocol evidence

Tasks:

1. Run real `tower-lsp` stdio boundary.
2. Send `initialize`.
3. Send `didOpen`.
4. Observe `publishDiagnostics`.
5. Request `textDocument/codeAction`.
6. Verify returned CodeAction contains `conformance-receipt.bind`.
7. Capture transcript and digest.

Do not use internal function calls as final proof.

---

## Agent 3 — Mutation Route Auditor

Target: GC008C

Tasks:

1. Trace CodeAction into `PackActionIntent`.
2. Trace `PackActionIntent` into `PackPlan`.
3. Prove `Staging` is required.
4. Prove `MutationGate` is required.
5. Prove denial prevents write.
6. Prove receipt emission requires admitted mutation.

Do not add shortcuts.

---

## Agent 4 — Anti-Cheating Auditor

Target: forbidden paths

Tasks:

1. Scan all relevant workspaces.
2. Report forbidden patterns.
3. Verify no shadow crates.
4. Verify no `v1.0.0`.
5. Verify no fake return paths.
6. Return raw scan commands and digests.

No prose victory summary.

---

# Final Instruction

Operate in Gall cash-busting mode.

Return only:

* statuses
* failset
* raw proofs
* LSP transcripts
* negative controls
* anti-cheat scans
* forbidden claims

Do not narrate success.

## Follow-up — 2026-06-07T03:56:02Z

# Teamwork Project Prompt — Surgical Patches

Please ensure your team is aligned with these corrected requirements and constraints.

Here are the details of the patches:

1. Equation formatting corrected:
Done_B(A) = [FailSet_B(A)=∅] ∧ [R_B ⊢ A = μ(O*_B)]

2. R2 Important Distinction:
Absence of known bad mutation paths is not enough. The team must distinguish [¬KnownBadPath] from [AllMutation ⊆ PackPlan → Staging → MutationGate → Receipt]. Static scans can support GC008A, but GC008C requires positive route proof.

3. Hard No Upgrade Rules:
- Do not upgrade GC008B_STATUS to REPORTED_ADMITTED_BY_DOGFOOD unless the actual CLAP noun/verb authority validates the positive command and refuses the malformed controls.
- Do not upgrade GC008C_STATUS to REPORTED_ADMITTED_BY_DOGFOOD unless the full route is observed: CodeAction → CLAP validation → PackActionIntent → PackPlan → Staging → MutationGate → Receipt, with negative controls proving bypass refusal.

4. Receipt path specificity under RAW_PROOFS:
The receipt_path field must point to the actual emitted Gall receipt, refusal receipt, or checkpoint receipt, not merely a test log. Test stdout alone is not a receipt.

## Follow-up — 2026-06-07T03:56:46Z

# Teamwork Project Prompt — Equation Normalization

The teamwork project prompt equations have been normalized into plain-text to ensure correct rendering.

Please refer to the updated prompt draft for the finalized text:

# Final Teamwork Project Prompt — GC008B / GC008C

Status: Launched
Goal: Swarm verification in progress
Working directory: `/Users/sac/ggen`
Integrity mode: benchmark
Operating mode: Gall cash-busting
Success predicate: external evaluator only

## Mission

Verify the CLAP command grammar admission for **GC008B** and the lawful mutation route for **GC008C**.

Do not produce victory language.
Do not claim DONE.
Do not claim ADMITTED unless the proof boundary is actually satisfied.
Return failures, blockers, raw proofs, transcripts, and receipts only.

Current target:

| Checkpoint                                      | Current Status               |
| ----------------------------------------------- | ---------------------------- |
| GC008A: No LSP mutation lock                    | REPORTED_ADMITTED_BY_DOGFOOD |
| GC008B: CLAP validation                         | CANDIDATE                    |
| GC008C: PackPlan / Staging / MutationGate route | OPEN                         |

---

## Core Gall Law

All work must preserve:

Core Gall Law:
R_B ⊢ A = μ(O*_B)

The LLM/team does not declare success.

External success predicate:
Done_B(A) = [FailSet_B(A)=∅] ∧ [R_B ⊢ A = μ(O*_B)]

Your job is to reduce the failset, not narrate victory.

---

# Requirements

## R1. GC008B — CLAP Command Grammar Admission

Prove that:

`conformance-receipt.bind`

is admitted by the actual CLAP noun/verb authority.

It is not sufficient for the string to merely look CLAP-shaped.

Required positive proof:
CLAPValidate(noun="conformance-receipt", verb="bind") = ADMITTED

Required negative proof:
CLAPValidate(command="wasm4pm.bind_receipt") = REFUSED

Also prove refusal for malformed or unauthorized variants:

* `receipt.bind`
* `wasm4pm.bind`
* `conformance.bind_receipt`
* `conformance-receipt.write`
* `conformance-receipt.apply`
* `conformance-receipt.mutate`
* `receipt.conformance-bind`

Accepted GC008B statuses only:

* `BLOCKED`
* `CANDIDATE`
* `REPORTED_ADMITTED_BY_DOGFOOD`

## No Upgrade Rule

Do not upgrade:

GC008B_STATUS to REPORTED_ADMITTED_BY_DOGFOOD

unless the actual CLAP noun/verb authority validates the positive command and refuses the malformed controls.

Forbidden GC008B claim:

* “CLAP governance proven” unless the actual CLAP authority validates the command and refuses the malformed controls.

---

## R2. GC008C — PackPlan / Staging / MutationGate Route

Prove that a valid `conformance-receipt.bind` intent flows strictly through:

`CodeAction → PackActionIntent → PackPlan → Staging → MutationGate → Receipt`

Required positive proof:

1. Real LSP protocol emits a `CodeAction`.
2. The returned action contains `conformance-receipt.bind`.
3. The action is converted into `PackActionIntent`.
4. `PackActionIntent` resolves into `PackPlan`.
5. `PackPlan` creates or requires `Staging`.
6. `Staging` requires `MutationGate`.
7. MutationGate denial prevents write.
8. Receipt emission requires admitted mutation.

Required negative controls:

* direct `executeCommand` is refused or absent
* direct `WorkspaceEdit` receipt binding is impossible
* command without CLAP admission is refused
* PackPlan bypass is refused
* Staging bypass is refused
* MutationGate denial blocks write
* receipt emission without admitted mutation refuses
* adapter cannot append receipt events
* LSP cannot write files
* LSP cannot manufacture receipt strings

Accepted GC008C statuses only:

* `BLOCKED`
* `CANDIDATE`
* `REPORTED_ADMITTED_BY_DOGFOOD`

## Important Distinction

Absence of known bad mutation paths is not enough.

The team must distinguish:

Absence of known bad paths:
¬KnownBadPath

is not equivalent to:

AllMutation ⊆ PackPlan → Staging → MutationGate → Receipt

Static scans can support GC008A, but GC008C requires positive route proof.

## No Upgrade Rule

Do not upgrade:

GC008C_STATUS to REPORTED_ADMITTED_BY_DOGFOOD

unless the full route is observed:

CodeAction → CLAP validation → PackActionIntent → PackPlan → Staging → MutationGate → Receipt

with negative controls proving bypass refusal.

Forbidden GC008C claim:

* “All mutations flow through PackPlan / Staging / MutationGate” unless the positive route and negative controls are both proven.

---

# Protocol Proof Requirement

At least one proof must use the real LSP boundary:

`initialize → didOpen → publishDiagnostics → textDocument/codeAction → returned conformance-receipt.bind intent`

Forbidden proof shortcuts:

* direct internal function calls as final proof
* private state inspection as final proof
* unit-only proof for protocol behavior
* string-only proof for CLAP validation
* static scan only for lawful mutation route

Static scans are allowed as supporting evidence, not final route proof.

---

# Anti-Cheating Static Surveillance

Scan all relevant boundaries:

* `/Users/sac/ggen`
* `/Users/sac/tower-lsp-max`
* `/Users/sac/wasm4pm`
* `/Users/sac/wasm4pm-compat`

Forbidden patterns:

* `wasm4pm.bind_receipt`
* `bind_conformance_receipt`
* `execute_command` mutation path
* `WorkspaceEdit` receipt binding
* `std::fs::write` in LSP mutation path
* `tokio::fs::write` in LSP mutation path
* `std::fs::read_to_string` in receipt-binding path
* `ocel.events.push` in adapter
* manual `FIT` return
* manual `ADMITTED` return
* `v1.0.0`
* `version = "1.0.0"`
* fake `wasm4pm` shadow crate
* fake `wasm4pm-lsp` shadow crate
* fake `wasm4pm-compat` shadow crate

If no forbidden patterns are found, include the exact scan commands and raw output.

---

# Acceptance Criteria

## Required Output Schema

Final output must match this structure:

```yaml
GC008B_STATUS: BLOCKED | CANDIDATE | REPORTED_ADMITTED_BY_DOGFOOD

GC008C_STATUS: BLOCKED | CANDIDATE | REPORTED_ADMITTED_BY_DOGFOOD

FAILSET:
  - failure_id:
    checkpoint:
    law:
    evidence:
    blocking:
    required_next_proof:

RAW_PROOFS:
  - proof_id:
    command:
    output_excerpt:
    receipt_path: # Must point to the emitted Gall receipt, refusal receipt, or checkpoint receipt, not merely a test log. Test stdout alone is not a receipt.
    digest:

LSP_TRANSCRIPTS:
  - transcript_id:
    path:
    covers:
      - initialize
      - didOpen
      - publishDiagnostics
      - textDocument/codeAction
    digest:

NEGATIVE_CONTROLS:
  - control_id:
    expected:
    observed:
    status:
    raw_output_excerpt:

ANTI_CHEAT_SCAN:
  commands:
    - command:
      output_excerpt:
      digest:
  forbidden_matches:
    - pattern:
      path:
      line:
      status:

FORBIDDEN_CLAIMS:
  - claim:
    reason:
```

## Follow-up — 2026-06-07T04:01:13Z

# Final Teamwork Project Prompt — GC008B / GC008C / LSP 3.18

Status: Launched
Goal: Swarm verification in progress
Working directory: `/Users/sac/ggen`
Integrity mode: benchmark
Operating mode: Gall cash-busting
Success predicate: external evaluator only

## Mission

Verify the CLAP command grammar admission for **GC008B**, the lawful mutation route for **GC008C**, and the strict LSP 3.18 protocol boundary (GC004B, GC004C, GC004D, GC008D).

Do not produce victory language.
Do not claim DONE.
Do not claim ADMITTED unless the proof boundary is actually satisfied.
Return failures, blockers, raw proofs, transcripts, and receipts only.

Current target:

| Checkpoint | Current Status |
| :--- | :--- |
| **GC004B: No plain tower-lsp lock** | OPEN / BLOCKING |
| **GC004C: LSP 3.18 capability negotiation** | OPEN |
| **GC004D: LSP 3.18 feature exercise** | OPEN |
| **GC008A: No LSP mutation lock** | REPORTED_ADMITTED_BY_DOGFOOD |
| **GC008B: CLAP validation** | CANDIDATE |
| **GC008C: PackPlan / Staging / MutationGate route** | OPEN |
| **GC008D: GC008 over LSP 3.18 surface** | OPEN |
| **GC009** | OPEN |

---

## Core Gall Invariants

All work must preserve:

Core Gall Law:
R_B ⊢ A = μ(O*_B)

The LLM/team does not declare success.

External success predicate:
Done_B(A) = [FailSet_B(A)=∅] ∧ [R_B ⊢ A = μ(O*_B)]

Your job is to reduce the failset, not narrate victory.

---

# Requirements

## R1. GC008B — CLAP Command Grammar Admission
Prove that `conformance-receipt.bind` is admitted by the actual CLAP noun/verb authority, while `wasm4pm.bind_receipt` and other malformed commands are refused.

Required positive proof:
CLAPValidate(noun="conformance-receipt", verb="bind") = ADMITTED

Required negative proof:
CLAPValidate(command="wasm4pm.bind_receipt") = REFUSED

Also prove refusal for malformed or unauthorized variants:
* `receipt.bind`, `wasm4pm.bind`, `conformance.bind_receipt`
* `conformance-receipt.write`, `conformance-receipt.apply`, `conformance-receipt.mutate`, `receipt.conformance-bind`

## R2. GC008C & GC008D — Lawful Mutation Route over LSP 3.18
Prove that a valid `conformance-receipt.bind` intent flows strictly through:
```text
CodeAction → PackActionIntent → PackPlan → Staging → MutationGate → Receipt
```
This route must be observed over the LSP 3.18 runtime surface (GC008D), not the old plain `tower-lsp` surface.

### Important Distinction
Absence of known bad mutation paths is not enough. The team must distinguish:
Absence of known bad paths: ¬KnownBadPath
is not equivalent to: AllMutation ⊆ PackPlan → Staging → MutationGate → Receipt
Static scans can support GC008A, but GC008C requires positive route proof.

## R3. GC004B, GC004C, GC004D — LSP 3.18 Surface Lock
- **LAW-LSP-001 (No plain tower-lsp)**: Remove all `tower-lsp` / `tower_lsp` dependencies, imports, and lock entries from `/Users/sac/ggen`, `/Users/sac/tower-lsp-max`, `/Users/sac/wasm4pm`, and `/Users/sac/wasm4pm-compat`.
- **LAW-LSP-002 (LSP 3.18 only)**: The actual runtime, server, and test harness must use the LSP 3.18 protocol model (advertised capability negotiation in `initialize` payload and server response).
- **LSP 3.18 Feature Matrix**: Verify that the 15 required LSP 3.18 features are supported, refused by law, or blocked by missing surface (but not "not tested").

---

# LSP 3.18 Feature Matrix

| ID | LSP 3.18 Feature | Status Codomain |
| :--- | :--- | :--- |
| **LSP318-001** | inline completions | SUPPORTED \| REFUSED_BY_LAW \| BLOCKED |
| **LSP318-002** | dynamic text document content | SUPPORTED \| REFUSED_BY_LAW \| BLOCKED |
| **LSP318-003** | folding range refresh | SUPPORTED \| REFUSED_BY_LAW \| BLOCKED |
| **LSP318-004** | multi-range formatting | SUPPORTED \| REFUSED_BY_LAW \| BLOCKED |
| **LSP318-005** | snippets in workspace edits | SUPPORTED \| REFUSED_BY_LAW \| BLOCKED |
| **LSP318-006** | relative patterns in document filters | SUPPORTED \| REFUSED_BY_LAW \| BLOCKED |
| **LSP318-007** | relative patterns in notebook filters | SUPPORTED \| REFUSED_BY_LAW \| BLOCKED |
| **LSP318-008** | code action kind documentation | SUPPORTED \| REFUSED_BY_LAW \| BLOCKED |
| **LSP318-009** | activeParameter on SignatureHelp | SUPPORTED \| REFUSED_BY_LAW \| BLOCKED |
| **LSP318-010** | command tooltips | SUPPORTED \| REFUSED_BY_LAW \| BLOCKED |
| **LSP318-011** | workspace edit metadata | SUPPORTED \| REFUSED_BY_LAW \| BLOCKED |
| **LSP318-012** | snippets in text document edits | SUPPORTED \| REFUSED_BY_LAW \| BLOCKED |
| **LSP318-013** | debug message kind | SUPPORTED \| REFUSED_BY_LAW \| BLOCKED |
| **LSP318-014** | code lens resolve properties | SUPPORTED \| REFUSED_BY_LAW \| BLOCKED |
| **LSP318-015** | completionList.applyKind | SUPPORTED \| REFUSED_BY_LAW \| BLOCKED |

---

# Anti-Cheating Static Surveillance

Scan all relevant boundaries for forbidden patterns:
- `tower-lsp`, `tower_lsp`, `lsp-types` or `lsp_types` version lacking 3.18 coverage
- `wasm4pm.bind_receipt`, `bind_conformance_receipt`, `execute_command` mutation path
- `WorkspaceEdit` receipt binding
- `std::fs::write` or `tokio::fs::write` in LSP mutation path
- `std::fs::read_to_string` in receipt-binding path
- `ocel.events.push` in adapter
- manual `FIT` / `ADMITTED` returns, `v1.0.0` or `version = "1.0.0"`
- fake shadow crates (`crates/wasm4pm`, `crates/wasm4pm-lsp`, `crates/wasm4pm-compat`)

---

# Acceptance Criteria

## Required Output Schema

```yaml
GC004B_STATUS: BLOCKED | REPORTED_ADMITTED_BY_DOGFOOD
GC004C_STATUS: BLOCKED | CANDIDATE | REPORTED_ADMITTED_BY_DOGFOOD
GC004D_STATUS: BLOCKED | CANDIDATE | REPORTED_ADMITTED_BY_DOGFOOD
GC008B_STATUS: BLOCKED | CANDIDATE | REPORTED_ADMITTED_BY_DOGFOOD
GC008C_STATUS: BLOCKED | CANDIDATE | REPORTED_ADMITTED_BY_DOGFOOD
GC008D_STATUS: BLOCKED | CANDIDATE | REPORTED_ADMITTED_BY_DOGFOOD

FAILSET:
  - failure_id:
    checkpoint:
    law:
    evidence:
    blocking:
    required_next_proof:

LSP318_FEATURE_MATRIX:
  - feature_id: LSP318-001
    name: inline completions
    status: SUPPORTED | REFUSED_BY_LAW | BLOCKED
    protocol_proof:
    negative_control:
    receipt:

RAW_PROOFS:
  - proof_id:
    command:
    output_excerpt:
    receipt_path: # Must point to the emitted Gall receipt, refusal receipt, or checkpoint receipt, not merely a test log. Test stdout alone is not a receipt.
    digest:

LSP_TRANSCRIPTS:
  - transcript_id:
    path:
    covers:
      - initialize
      - didOpen
      - publishDiagnostics
      - textDocument/codeAction
    digest:

NEGATIVE_CONTROLS:
  - control_id:
    expected:
    observed:
    status:
    raw_output_excerpt:

ANTI_CHEAT_SCAN:
  commands:
    - command:
      output_excerpt:
      digest:
  forbidden_matches:
    - pattern:
      path:
      line:
      status:

FORBIDDEN_CLAIMS:
  - claim:
    reason:
```

---

# Status & Upgrade Rules

- **Do not upgrade GC008B_STATUS to `REPORTED_ADMITTED_BY_DOGFOOD`** unless the actual CLAP noun/verb authority validates the positive command and refuses the malformed controls.
- **Do not upgrade GC008C_STATUS to `REPORTED_ADMITTED_BY_DOGFOOD`** unless the full route is observed: `CodeAction` → CLAP validation → `PackActionIntent` → `PackPlan` → `Staging` → `MutationGate` → `Receipt` (with negative controls proving bypass refusal).
- **Do not claim GC008C admitted** while any proof depends on plain `tower-lsp`.
- **Do not claim LSP 3.18** because `tower-lsp-max-pack-lsp` is present. Pack observer dependency is not runtime protocol authority.
- **Forbidden Claims**: Do not say "Victory confirmed", "All gaps resolved", "GC008 fully admitted", "CLAP governance proven", or "All mutations flow through the gate" unless the raw proof structure explicitly supports the claim.

## Follow-up — 2026-06-07T04:12:49Z

# Final Teamwork Project Prompt — Strange Stuff / Code Smell / Poor Practice Audit

Verify the CLAP command grammar admission for **GC008B** and the lawful mutation route for **GC008C**.

Do not produce victory language.
Do not claim DONE.
Do not claim ADMITTED unless the proof boundary is actually satisfied.
Return failures, blockers, raw proofs, transcripts, and receipts only.

Working directories:
- `/Users/sac/ggen`
- `/Users/sac/tower-lsp-max`
- `/Users/sac/wasm4pm`
- `/Users/sac/wasm4pm-compat`

Operating mode: Gall cash-busting
Success predicate: external evaluator only
Output mode: failset only

---

# Requirements

## R1. Strange Code / Suspicious Behavior
Find code that looks like debugging, scaffolding, cargo-culting, or “make the test pass” logic.
Flag examples like:
- diagnostics named `DEBUG`
- diagnostics that print raw file content or paths
- warnings emitted for non-error states
- substring checks used as authority
- fake validated log messages
- placeholder code that sounds authoritative

## R2. clap-noun-verb Correctness
Search for any invented abstraction around `CLAP` (like `CLAP` as an acronym or `CLAPValidate` unless it actually exists). Verify where `clap-noun-verb` actually lives, which binary admits noun/verb shapes, and if invalid shapes are refused.

## R3. LSP Surface Audit
No `tower-lsp` anywhere. Search for old protocol crates in dependencies or lockfiles, and old tower-lsp imports. Verify that the runtime does not depend on plain `tower-lsp`.

## R4. LSP 3.18 Feature Coverage Audit
Verify if every LSP 3.18 feature (LSP318-001 through LSP318-015) is implemented, tested, or lawfully refused.

## R5. Receipt, Checkpoint, and Mutation Authority Audit
Find places where logs/summaries are treated as receipts. Verify CalVer versioning. Verify that no mutation route bypasses `CodeAction` -> `clap-noun-verb` -> `PackActionIntent` -> `PackPlan` -> `Staging` -> `MutationGate` -> `Receipt`. Check for poor Rust practices (unwraps in authority path, silent defaults, etc.).

---

# Acceptance Criteria

Final output must match the requested YAML structure:

```yaml
GC_STRANGENESS_AUDIT_STATUS: FAILSET_NONEMPTY | NO_FAILURES_REPORTED_WITH_RECEIPTS

FAILSET:
  - failure_id:
    category:
    checkpoint_impacted:
    law:
    path:
    line:
    evidence:
    why_strange:
    blocking:
    required_correction:
    required_next_proof:

CODE_SMELLS:
  - smell_id:
    category:
    path:
    line:
    snippet:
    severity:
    reason:
    suggested_correction:

POOR_PRACTICES:
  - practice_id:
    path:
    line:
    evidence:
    severity:
    why_poor:
    required_correction:

TEST_SMELLS:
  - test_smell_id:
    path:
    line:
    evidence:
    why_test_can_pass_for_wrong_reason:
    required_correction:

LSP_SURFACE_AUDIT:
  no_tower_lsp_status: BLOCKED | REPORTED_CLEAN_WITH_RAW_SCAN
  tower_lsp_matches:
    - path:
      line:
      evidence:
  lsp318_matrix:
    - feature_id:
      feature:
      status: SUPPORTED | REFUSED_BY_LAW | BLOCKED | NOT_FOUND
      evidence:
      required_next_proof:

CLAP_NOUN_VERB_AUDIT:
  status: BLOCKED | CANDIDATE | REPORTED_ADMITTED_BY_DOGFOOD
  fake_clap_matches:
    - path:
      line:
      evidence:
  noun_verb_validation_evidence:
    - command:
      expected:
      observed:
      proof_path:
      digest:

MUTATION_ROUTE_AUDIT:
  status: BLOCKED | CANDIDATE | REPORTED_ADMITTED_BY_DOGFOOD
  bypass_matches:
    - path:
      line:
      evidence:
      bypass_type:
  route_proofs:
    - step:
      evidence:
      receipt:

RECEIPT_AND_VERSION_AUDIT:
  calver_status: BLOCKED | REPORTED_CLEAN_WITH_RAW_SCAN
  receipt_status: BLOCKED | CANDIDATE | REPORTED_RECEIPTED
  bad_version_matches:
    - path:
      line:
      evidence:
  receipt_smells:
    - path:
      line:
      evidence:
      reason:

RAW_COMMANDS:
  - command:
    exit_code:
    output_excerpt:
    digest:

FORBIDDEN_CLAIMS:
  - claim:
    reason:
```
