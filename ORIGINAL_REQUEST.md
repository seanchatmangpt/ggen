# Original User Request

## Initial Request — 2026-05-26T16:09:23-07:00

Implement `ggen-graph` as a one-package Rust substrate for public-ontology-governed RDF graphs, Knowledge Hooks, deterministic deltas, OCEL/PROV evidence, and replayable BLAKE3 receipts, satisfying the Vision 2030 GALL checkpoint PRD/ARD.

Working directory: /Users/sac/ggen/crates/ggen-graph
Integrity mode: benchmark

## Requirements

### R1. One-Crate Package Boundary and No Feature Flags
- The substrate must be implemented as a single, standalone Rust package at `crates/ggen-graph`.
- No feature flags or optional capability selection configurations are permitted.

### R2. Public Ontology Governance and Vocabulary Emission
- Rust vocabulary constants must be emitted directly from public ontology profiles (RDF, RDFS, OWL, XSD, PROV, SKOS, SHACL, OCEL, skos, etc.).

### R3. Deterministic Graph Substrate and Delta Conservation
- Graph parsing, serialization, canonical byte generation, and BLAKE3 hashing must be completely deterministic.
- Graph transitions must map to deterministic `RdfDelta` structs conserving quad additions/deletions.

### R4. Knowledge Hook Runtime & Deterministic Scheduling
- Turtle-based hook packs must load dynamically as graph laws.
- Hook execution ordering must be scheduled deterministically according to priority, dependencies, and cycle-free sequences.
- SPARQL ASK/CONSTRUCT queries must actuate graph transformations.

### R5. OCEL/PROV Evidence Projection & Replayable Receipts
- Every graph transition must produce a replayable cryptographic receipt bundle capturing the input hash, delta, output hash, and execution evidence.
- Provenance details (PROV-O) and object-centric process event logs (OCEL) must be projected for auditability.

### R6. Compliance Checks & Anti-Fake Gates
- Must pass structural analyses checking for forbidden execution surfaces (no hidden subprocess shells, no arbitrary HTTP clients).
- Must contain zero placeholder, stub, mock, or fake-success patterns (no `Ok(())` placeholding or dummy stubs).

## Acceptance Criteria

### Compilation and Testing
- [ ] Crate compiles cleanly via `cargo check -p ggen-graph --all-targets`.
- [ ] Full suite of unit and integration tests passes via `cargo test -p ggen-graph`.
- [ ] Compliance scripts `scripts/gall/forbidden_surface.sh` and `scripts/gall/anti_fake_implementation.sh` return exit code 0 indicating zero violations.
- [ ] Replay verification tests demonstrate that graph state transitions can be replayed and validated from receipts.

## Follow-up — 2026-05-26T23:27:58Z

Implement the OCEL v2 self-audit log requirement for `ggen-graph`. The agent team must convert their own implementation, testing, scanning, and GALL promotion activities into a qualified, object-centric process log (OCEL v2) and derive the final Vision 2030 GALL Checkpoint Proof Report from this audit log.

Working directory: /Users/sac/ggen/crates/ggen-graph
Integrity mode: benchmark

## Requirements

### R1. OCEL v2 Self-Audit Log Emission
- Implement self-audit log generator in `crates/ggen-graph/src/ocel/self_audit.rs` and `crates/ggen-graph/src/ocel/gall_projection.rs` emitting `crates/ggen-graph/audit/vision2030.self_audit.ocel.json`.
- Log must include required Object Types (`RustCrate`, `PRDRequirement`, `ARDRequirement`, `GALLCheckpoint`, `PublicOntology`, `OntologyTerm`, `SourceFile`, `TestFile`, `ExampleFile`, `FixtureFile`, `ScriptFile`, `Command`, `CommandRun`, `EvidenceArtifact`, `GraphReceipt`, `CoverageMatrix`, `PromotionDecision`, `UnsupportedCapability`).
- Log must include required Event Types (`RequirementDeclared`, `OntologyMapped`, `FileEmitted`, `ImplementationChanged`, `FixtureCreated`, `CommandExecuted`, `TestPassed`, `TestFailed`, `ForbiddenSurfaceScanned`, `AntiFakeScanned`, `ReceiptEmitted`, `ReplayVerified`, `CoverageEvaluated`, `CheckpointEvaluated`, `CheckpointPromoted`, `CheckpointRefused`, `UnsupportedCapabilityDeclared`).
- Relationships must use qualifiers (e.g., `--checks-->`, `--produces-->`, `--verifies-->`, `--satisfied_by-->`, `--decides-->`).

### R2. Coverage Matrix & Verification Scripts
- Implement coverage mapping in `crates/ggen-graph/src/ocel/coverage.rs` emitting `crates/ggen-graph/audit/vision2030.coverage.json`.
- Add integration tests `crates/ggen-graph/tests/ocel_self_audit.rs` and `crates/ggen-graph/tests/vision2030_coverage.rs`.
- Write validation scripts at `scripts/gall/emit_ocel_self_audit.sh` and `scripts/gall/verify_ocel_self_audit.sh`.
- `verify_ocel_self_audit.sh` must enforce the 5 Completeness Rules (Requirements have evidence, Checkpoints have Command evidence, prior evaluations exist, anti-fake is audited, unsupported capabilities are linked).

### R3. Derived GALL Checkpoint Proof Report
- Author/re-write `crates/ggen-graph/audit/vision2030.self_audit.summary.md` and the final `docs/VISION_2030_GALL_PROOF.md` such that the promotion decision is explicitly derived from the audit JSON files.

## Acceptance Criteria

### Execution & Verification
- [ ] Crate compiles cleanly with the new modules.
- [ ] All unit and integration tests under `cargo test -p ggen-graph` pass successfully.
- [ ] `bash scripts/gall/verify_ocel_self_audit.sh` runs and exits with 0, confirming zero completeness violations in the audit JSON.
- [ ] `bash scripts/gall/forbidden_surface.sh` and `bash scripts/gall/anti_fake_implementation.sh` run and exit with 0.

## Follow-up — 2026-05-26T23:44:16Z

Implement the External Lifecycle Evaluation Doctrine for `ggen-graph`. The agent team must construct an external observer script ring at `scripts/gall/external/` that independently observes, hashes, validates, and adjudicates the crate lifecycle and GALL checkpoints, ensuring the Rust package or agent does not self-verify.

Working directory: /Users/sac/ggen

## Requirements

### R1. External Observer Script Ring
- Implement scripts under `scripts/gall/external/` (00 to 13) to capture baseline, extract requirements, verify package constraints, check feature flags, run tests, scan forbidden surfaces, verify OCEL self-audits, detect contradictions, and perform final promotion adjudication.
- The verifier scripts must capture script digests and file digests to prevent verifier bypass or mutation.

### R2. Verification Logic & Contradiction Detection
- Implement `12_detect_contradictions.sh` to check for conflicting checkpoint events (e.g. both `CheckpointPromoted` and `CheckpointRefused`) or missing evaluations.
- Implement `09_verify_ocel_self_audit.sh` to verify complete requirement coverage, linking requirements, events, and script validations.
- Implement `13_adjudicate_gall_promotion.sh` to run all validation steps sequentially and write the external adjudication results to `crates/ggen-graph/audit/vision2030.external_adjudication.json`.

### R3. CI and Report Updates
- Integrate verifier scripts into the CI command flows.
- Rewrite `docs/VISION_2030_GALL_PROOF.md` such that the promotion decision is strictly declared as dependent on the external verifier scripts' adjudication results.

## Acceptance Criteria

### Execution & Verification
- [ ] All external scripts (00 to 13) are successfully written and executable.
- [ ] `bash scripts/gall/external/13_adjudicate_gall_promotion.sh` executes successfully, producing `crates/ggen-graph/audit/vision2030.external_adjudication.json`.
- [ ] All Rust tests pass cleanly under `cargo test -p ggen-graph`.
- [ ] Verification script scans return 0 indicating complete requirements coverage and zero contradictions.

## Follow-up — 2026-05-27T00:14:14Z

Implement the Agent Truthfulness GALL protocol for `ggen-graph`. Build a complete adversarial verifier ring (`scripts/gall/external/00_` through `99_`) to ensure that all checks are non-trivial, corrupted state mutations result in expected script failures (refusal), command transcripts capture duration/hashes, and the entire worktree is indexed under `crates/ggen-graph/audit/worktree_inventory.json`.

Working directory: /Users/sac/ggen
Integrity mode: benchmark

## Requirements

### R1. Negative-Control Sabotage Suite & Script Adequacy
- Implement the negative-control sabotage sweep in `scripts/gall/external/23_run_sabotage_suite.sh` applying temporary corrupted worktree mutations (features in Cargo.toml, TODO in source, std::process::Command, receipt tampering, missing requirement link, file deletion) and proving verification refusal.
- Validate verifier script adequacy, ensuring they contain failure branches, real check commands, and generate execution transcripts with non-zero durations.

### R2. Full Worktree Inventory & Transcript Logging
- Build `scripts/gall/external/20_capture_full_worktree_inventory.sh` to generate the worktree inventory JSON.
- Implement command transcript capture in `crates/ggen-graph/audit/transcripts/` with metadata (duration, environment, exit code, stdout/stderr hashes) for every executed verifier command.

### R3. Causal Sufficiency and Adjudication Decision
- Implement checks validating minimum evidence cardinality and causal completion in the OCEL log.
- Output the final external adjudication JSON `crates/ggen-graph/audit/agent_truthfulness.external_adjudication.json` via `scripts/gall/external/99_adjudicate_truthfulness.sh` only if T0–T9 checks pass.

## Acceptance Criteria

### Execution & Refusal Tests
- [ ] All verifier scripts and sabotage tests run successfully.
- [ ] Sabotage script returns exit code 0 indicating that every injected mutation correctly triggered a verification refusal.
- [ ] Adjudication JSON is produced and verdict is set to `"Promoted"` under a clean worktree, but results in refusal under any sabotage mutation.
- [ ] `verify_agent_truthfulness.sh` runs successfully and exits with 0.

## Follow-up — 2026-05-27T00:49:55Z

Implement the **Witnessed Agent Truthfulness GALL** protocol in the `ggen` repository to prevent narrative/counterfeit agent completion claims by requiring transcript-bearing, sabotage-tested, full-worktree, OCEL-backed external adjudication.

Working directory: /Users/sac/ggen

## Requirements

### R1. External Witness Adjudication Scripts
Implement the following external verification scripts under `scripts/gall/external/`:
- `20_capture_full_worktree_inventory.sh`: Capture details (path, sha256, blake3, size, inclusion status/reason) for all source, test, example, schema, script, and documentation files.
- `21_verify_command_transcripts.sh`: Validate that all executed commands have an accompanying transcript JSON (containing stdout, stderr, exit code, duration, argv, cwd, etc.).
- `22_verify_script_adequacy.sh`: Verify that every verifier script is adequate (proves failure behavior, has negative control cases, exits non-zero on failure).
- `23_run_sabotage_suite.sh`: Mutate/sabotage the workspace with the 12 required cases to verify that the corresponding verifier gates fail.
- `24_run_clean_room_rebuild.sh`: Rebuild and run tests in a clean-room temporary directory.
- `25_verify_cross_artifact_consistency.sh`: Verify all cross-artifact links (inventory, transcripts, OCEL log, receipts).
- `26_verify_ocel_causal_sufficiency.sh`: Verify the causal lifecycle chain of requirements in OCEL.
- `27_verify_contradiction_supersession.sh`: Ensure contradiction events (promoted vs refused) default to refused unless resolved by a supersession event.
- `99_adjudicate_witnessed_truthfulness.sh`: The final external witness script that produces `witnessed_truthfulness.external_adjudication.json`.

### R2. Verification Artifacts & Evidence
Generate the following durably structured audit artifacts in `crates/ggen-graph/audit/`:
- `worktree_inventory.full.json`
- `transcripts/` (containing command JSONs, stdout, stderr files)
- `script_adequacy.json`
- `sabotage_results.json`
- `clean_room_rebuild.json`
- `cross_artifact_consistency.json`
- `ocel_causal_sufficiency.json`
- `contradiction_supersession.json`
- `witnessed_truthfulness.external_adjudication.json`

### R3. Structural Proof Document
Update `docs/VISION_2030_GALL_PROOF.md` to shift from the old 5-rule shape to the W0–W9/T0-T10 Witnessed Agent Truthfulness GALL checkpoints.

## Acceptance Criteria

### Execution & Evidence Integrity
- [ ] No narrative promotion: Promotion is denied if any claim is not backed by a transcript artifact.
- [ ] Full Worktree Coverage: Every source, test, example, schema, query, script, doc, and audit artifact is inventoried and verified.
- [ ] Sabotage Suite Completeness: All 12 negative-control sabotage cases must run, fail their expected gates, and restore correctly.
- [ ] Contradiction Supersession: Promoted/Refused conflicts default to Refused unless superseded by a resolved event.
- [ ] External Adjudication: Final promotion must exclusively be driven by the external witness adjudication file.

## Follow-up — 2026-05-27T00:53:55Z

The specifications have been updated to pivot the implementation to match the updated prompt draft:

# Teamwork Project Prompt — Draft

Implement the **Witnessed Agent Truthfulness GALL** protocol in the `ggen` repository to prevent narrative/counterfeit agent completion claims by requiring transcript-bearing, sabotage-tested, full-worktree, OCEL-backed external adjudication using Rust witness binaries.

Working directory: /Users/sac/ggen

## Requirements

### R1. Rust Witness Binaries, Not Shell Validators
Implement the Witnessed Agent Truthfulness GALL verifier logic using Rust binary targets under `crates/ggen-graph/src/bin/`:
- `gall_capture_full_worktree_inventory.rs`: Capture details (path, sha256, blake3, size, inclusion status/reason) for all source, test, example, schema, script, and documentation files.
- `gall_verify_command_transcripts.rs`: Validate that all executed commands have an accompanying transcript JSON (containing stdout, stderr, exit code, duration, argv, cwd, etc.).
- `gall_verify_witness_adequacy.rs`: Verify that every verifier/witness binary is adequate (proves failure behavior, has negative control cases, exits non-zero on failure).
- `gall_run_sabotage_suite.rs`: Mutate/sabotage the workspace with the 12 required cases to verify that the corresponding verifier gates fail.
- `gall_run_clean_room_rebuild.rs`: Rebuild and run tests in a clean-room temporary directory.
- `gall_verify_cross_artifact_consistency.rs`: Verify all cross-artifact links (inventory, transcripts, OCEL log, receipts).
- `gall_verify_ocel_causal_sufficiency.rs`: Verify the causal lifecycle chain of requirements in OCEL.
- `gall_verify_contradiction_supersession.rs`: Ensure contradiction events (promoted vs refused) default to refused unless resolved by a supersession event.
- `gall_emit_docs_tree.rs`: Build and output the docs tree metadata.
- `gall_adjudicate_witnessed_truthfulness.rs`: The final external witness executable that produces `witnessed_truthfulness.external_adjudication.json`.

Shell files are allowed only as thin launchers (e.g. `scripts/gall/run_witnessed_truthfulness.sh`). They must not contain verification law.
Each Rust witness binary must:
- Emit structured JSON artifacts.
- Emit command transcripts where applicable.
- Return non-zero on refusal.
- Include unit tests or integration tests.
- Be represented in OCEL evidence.
- Be included in the full worktree inventory.

### R2. Critical Boundary / Authority Zones
- **Production library surface** (`crates/ggen-graph/src/lib.rs`, `crates/ggen-graph/src/{graph,delta,receipt,hooks,ocel,diagnostics}/**`) must forbid: `std::process::Command`, shell execution, filesystem deletion, network calls, and LLM calls.
- **Witness binary surface** (`crates/ggen-graph/src/bin/gall_*.rs`) may use: `std::process::Command`, `tempfile`, `walkdir`, file hashing, command transcript capture, and temporary worktree mutation.

### R3. Verification Artifacts & Evidence
Generate the following durably structured audit artifacts in `crates/ggen-graph/audit/`:
- `worktree_inventory.full.json`
- `transcripts/` (containing command JSONs, stdout, stderr files)
- `script_adequacy.json`
- `sabotage_results.json`
- `clean_room_rebuild.json`
- `cross_artifact_consistency.json`
- `ocel_causal_sufficiency.json`
- `contradiction_supersession.json`
- `witnessed_truthfulness.external_adjudication.json`

### R4. Emitted Docs Tree
The documentation tree is part of the GALL evidence surface.
Emit:
- `docs/docs.tree.json`
- `docs/docs.tree.ttl`

The docs tree must include every document under `docs/`, its checkpoint mapping, source requirement IDs, hash, and verification status.
Required docs:
- `docs/GGEN_GRAPH_INDEX.md`
- `docs/VISION_2030_GALL_PRD_ARD.md`
- `docs/VISION_2030_GALL_PROOF.md`
- `docs/GGEN_GRAPH_DX_QOL.md`
- `docs/PUBLIC_ONTOLOGY_GOVERNANCE.md`
- `docs/WITNESSED_AGENT_TRUTHFULNESS_GALL.md`
- `docs/gall/W0_NO_NARRATIVE_PROMOTION.md`
- `docs/gall/W1_FULL_WORKTREE_INVENTORY.md`
- `docs/gall/W2_TRANSCRIPT_COMMAND_EVIDENCE.md`
- `docs/gall/W3_SCRIPT_ADEQUACY.md`
- `docs/gall/W4_NEGATIVE_CONTROL_SABOTAGE.md`
- `docs/gall/W5_CLEAN_ROOM_REBUILD.md`
- `docs/gall/W6_CROSS_ARTIFACT_CONSISTENCY.md`
- `docs/gall/W7_OCEL_CAUSAL_SUFFICIENCY.md`
- `docs/gall/W8_CONTRADICTION_SUPERSESSION.md`
- `docs/gall/W9_EXTERNAL_WITNESS_ADJUDICATION.md`

Promotion is refused if docs are not inventoried, hash-bound, and mapped to requirements/checkpoints.

### R5. Doctest-Governed DX/QoL
The public developer experience must be proven by Rust doctests, not prose.
Every public module must include examples that compile with `cargo test -p ggen-graph --doc`.
Required doctest coverage:
- Dataset load from Turtle
- Dataset stable hash
- RdfDelta construction
- RdfDelta application
- GraphReceipt creation
- GraphReceipt verification
- HookPack load from Turtle
- HookRuntime SPARQL ASK / CONSTRUCT path
- OCEL / PROV projection
- Error handling for unsupported capability

Promotion is refused if public API examples are not doctested.

## Acceptance Criteria

### Execution & Evidence Integrity
- [ ] No narrative promotion: Promotion is denied if any claim is not backed by a transcript artifact.
- [ ] Full Worktree Coverage: Every source, test, example, schema, query, script, doc, and audit artifact is inventoried and verified.
- [ ] Sabotage Suite Completeness: All 12 negative-control sabotage cases must run, fail their expected gates, and restore correctly.
- [ ] Contradiction Supersession: Promoted/Refused conflicts default to Refused unless superseded by a resolved event.
- [ ] External Adjudication: Final promotion must exclusively be driven by the external witness adjudication file.

## Follow-up — 2026-05-27T00:54:50Z

The user has refined the specification to adopt the "Witnessed Code Evaluation / Knowledge Hook Actuation" model. Please pivot the implementation to match the updated prompt draft:

# Teamwork Project Prompt — Draft

Implement the **Witnessed Agent Truthfulness GALL** protocol in the `ggen` repository using **Witnessed Code Evaluation / Knowledge Hook Actuation** over OCEL / PROV / RDF evidence emitted by external boundary observers.

Working directory: /Users/sac/ggen

## Requirements

### R1. Boundary Observation Binaries
Implement Rust boundary observers under `crates/ggen-graph/src/bin/`:
- `gall_observe_worktree.rs`: Observe worktree inventory (paths, size, SHA256, BLAKE3).
- `gall_observe_commands.rs`: Observe commands, capture transcripts (stdout, stderr, argv, cwd, exit code, etc.).
- `gall_observe_sabotage.rs`: Observe negative-control sabotage runs.
- `gall_observe_clean_room.rs`: Observe clean-room build/test executions.
- `gall_observe_docs_tree.rs`: Observe documentation tree files.
- `gall_observe_doctests.rs`: Observe doctest coverage/verification.
- `gall_materialize_evidence_graph.rs`: Materialize individual observer outputs into a cohesive RDF/Turtle evidence graph (`crates/ggen-graph/audit/gall_evidence.ttl` and `crates/ggen-graph/audit/gall_evidence.ocel.json`).
- `gall_actuate_code_evaluation.rs`: Actuate the evaluation runtime. Loads the Knowledge Hook Pack and the evidence graph, runs the trigger query evaluation, and outputs the decision delta/receipt.

*Rule: Observer binaries must not decide final promotion. They only observe and materialize facts/evidence.*

### R2. Knowledge Hook Pack
Create:
- `crates/ggen-graph/hooks/gall-code-evaluation.ttl`

The hook pack defines the evaluation law for W0–W9/T0-T10 checkpoints:
- **W0 No Narrative Promotion**: Blocks promotion if narrative claims exist without transcripts.
- **W1 Full Worktree Inventory**: Blocks promotion if required files lack digest evidence.
- **W2 Transcript-Bearing Command**: Blocks promotion if commands lack transcript metadata.
- **W3 Witness Adequacy**: Refuses promotion if any witness lacks negative-control sabotage validation.
- **W4 Negative-Control Sabotage**: Refuses promotion if expected/actual sabotage failure gates mismatch.
- **W5 Clean-Room Rebuild**: Blocks promotion if clean-room rebuild fails.
- **W6 Cross-Artifact Consistency**: Blocks promotion on reference mismatches.
- **W7 OCEL Causal Sufficiency**: Blocks promotion if requirement declaration lacks causal path to check.
- **W8 Contradiction Supersession**: CheckpointPromoted + CheckpointRefused defaults to Refused unless superseded by ContradictionResolved.
- **W9 Final Promotion**: Promotion to CheckpointPromoted / VerifiedState iff W0-W8 pass and receipt is verified.

Each hook has an IRI, trigger (SPARQL ASK/SELECT/SHACL), action (CONSTRUCT decision delta), checkpoint mapping, and receipt projection.

### R3. Decision Deltas, Receipts & Final Adjudication
Generate decision/audit artifacts in `crates/ggen-graph/audit/`:
- `gall_evidence.ttl` / `gall_evidence.ocel.json`
- `gall_decision.delta.ttl` (RDFDelta containing facts like `gall:CheckpointPromoted`, `gall:CheckpointRefused`, `gall:PromotionBlocked`, `gall:VerifiedState`)
- `gall_code_evaluation.receipt.ttl` (binds input evidence hash, hook pack hash, decision delta hash, output graph hash, timestamp, etc.)
- `gall_code_evaluation.final.ttl` (final hook-actuated status)

### R4. Critical Boundary / Authority Zones
- **Production library surface** (`crates/ggen-graph/src/lib.rs`, `crates/ggen-graph/src/{graph,delta,receipt,hooks,ocel,diagnostics}/**`) must forbid: `std::process::Command`, shell execution, filesystem deletion, network calls, and LLM calls.
- **Witness binary surface** (`crates/ggen-graph/src/bin/gall_*.rs`) may use: `std::process::Command`, `tempfile`, `walkdir`, file hashing, command transcript capture, and temporary worktree mutation.
- **Knowledge Hooks** (`crates/ggen-graph/hooks/gall-code-evaluation.ttl`) evaluate the evidence graph to actuate decisions but must not mutate the filesystem directly.

### R5. Emitted Docs Tree Index
Emit `docs/docs.tree.json` and `docs/docs.tree.ttl` listing every doc under `docs/`, its checkpoint, source requirements, hash, and status.

### R6. Doctest-Governed DX/QoL
All developer experience paths must be proven by Rust doctests (`cargo test -p ggen-graph --doc`).

## Acceptance Criteria

### Execution & Evidence Integrity
- [ ] No imperative scripts or witness binaries can directly declare final promotion; it must be actuated by Knowledge Hooks over the evidence graph.
- [ ] Every decision is emitted as an RDFDelta and bound by a cryptographic receipt.
- [ ] Same evidence + same hooks must replay to the same promotion/refusal.
- [ ] Full Worktree Coverage: Every source, test, example, schema, query, script, doc, and audit artifact is inventoried and verified.
- [ ] Sabotage Suite Completeness: All 12 negative-control sabotage cases must run, fail their expected gates, and restore correctly.
- [ ] Contradiction Supersession: Promoted/Refused conflicts default to Refused unless superseded by a resolved event.

## Follow-up — 2026-05-27T01:05:32Z

The user has further refined the specification:
"Use public vocabularies only. No private vocabulary namespace."

Please pivot the implementation to match the updated prompt draft:

# Teamwork Project Prompt — Draft

Implement the **Witnessed Agent Truthfulness GALL** protocol in the `ggen` repository using **Witnessed Code Evaluation / Knowledge Hook Actuation** over OCEL / PROV / RDF evidence using **public vocabularies only** (no private vocabulary namespaces).

Working directory: /Users/sac/ggen

## Requirements

### R1. Boundary Observation Binaries
Implement Rust boundary observers under `crates/ggen-graph/src/bin/`:
- `gall_observe_worktree.rs`: Observe worktree inventory (paths, size, SHA256, BLAKE3).
- `gall_observe_commands.rs`: Observe commands, capture transcripts (stdout, stderr, argv, cwd, exit code, etc.).
- `gall_observe_sabotage.rs`: Observe negative-control sabotage runs.
- `gall_observe_clean_room.rs`: Observe clean-room build/test executions.
- `gall_observe_docs_tree.rs`: Observe documentation tree files.
- `gall_observe_doctests.rs`: Observe doctest coverage/verification.
- `gall_materialize_evidence_graph.rs`: Materialize individual observer outputs into a cohesive RDF/Turtle evidence graph using public vocabularies.
- `gall_actuate_code_evaluation.rs`: Actuate the evaluation runtime. Loads the Hook Pack and the evidence graph, runs the trigger query evaluation, and outputs the decision delta/receipt.

*Rule: Observer binaries must not decide final promotion. They only observe and materialize facts/evidence.*

### R2. Public-Vocabulary-Only Knowledge Hook Pack
Create:
- `crates/ggen-graph/hooks/gall-code-evaluation.ttl`

The hook pack must use public vocabularies only.
Allowed prefixes: `rdf:`, `rdfs:`, `owl:`, `xsd:`, `prov:`, `dcat:`, `dcterms:`, `skos:`, `sh:`, `time:`, `spdx:`, `ocel:`.
Forbidden prefixes: `gall:`, `gg:`, `kh:`, `pcst:`, `truex:`, or any project-private namespaces.

The hook pack expresses W0-W9 evaluation law using:
- `prov:Plan`
- `sh:NodeShape`
- `sh:SPARQLConstraint`
- `sh:ValidationReport`
- `sh:ValidationResult`
- `dcat:Dataset`
- `dcterms:identifier`
- `dcterms:title`
- `dcterms:description`
- `prov:used`
- `prov:wasGeneratedBy`
- `prov:wasDerivedFrom`

No private class or predicate may be introduced.

### R3. Public-Vocabulary Decision Deltas, Receipts & Final Adjudication
Generate decision/audit artifacts in `crates/ggen-graph/audit/`:
- `gall_evidence.ttl` / `gall_evidence.ocel.json`
- `gall_decision.delta.ttl`
- `gall_code_evaluation.receipt.ttl`
- `gall_code_evaluation.final.ttl`

The decision delta must not contain private terms. Instead, final status must be represented as public vocabulary facts:
- **Promotion**: `sh:ValidationReport` and `sh:conforms true` linked to execution via `prov:Activity` and `prov:wasGeneratedBy`.
- **Refusal**: `sh:ValidationReport` and `sh:conforms false` with one or more `sh:ValidationResult` objects with `sh:resultSeverity sh:Violation` and `sh:resultMessage`.

The final decision must include a receipt binding the evidence hash, hook pack hash, decision delta hash, output graph hash, timestamp, etc.

### R4. Critical Boundary / Authority Zones
- **Production library surface** (`crates/ggen-graph/src/lib.rs`, `crates/ggen-graph/src/{graph,delta,receipt,hooks,ocel,diagnostics}/**`) must forbid: `std::process::Command`, shell execution, filesystem deletion, network calls, and LLM calls.
- **Witness binary surface** (`crates/ggen-graph/src/bin/gall_*.rs`) may use: `std::process::Command`, `tempfile`, `walkdir`, file hashing, command transcript capture, and temporary worktree mutation.
- **Knowledge Hooks** (`crates/ggen-graph/hooks/gall-code-evaluation.ttl`) evaluate the evidence graph to actuate decisions but must not mutate the filesystem directly.

### R5. Emitted Docs Tree Index
Emit `docs/docs.tree.json` and `docs/docs.tree.ttl` listing every doc under `docs/`, its checkpoint, source requirements, hash, and status. Must use public vocabularies.

### R6. Doctest-Governed DX/QoL
All developer experience paths must be proven by Rust doctests (`cargo test -p ggen-graph --doc`).

## Acceptance Criteria

### Execution & Evidence Integrity
- [ ] No private namespace appears in hook packs, evidence graphs, decision deltas, final reports, docs tree TTL, or receipts.
- [ ] Promotion/refusal is represented using `sh:ValidationReport` and `sh:conforms`.
- [ ] Refusal reasons are represented as `sh:ValidationResult` objects.
- [ ] Hook execution is represented as `prov:Activity`.
- [ ] Evidence artifacts are represented as `prov:Entity` and/or `dcat:Dataset`.
- [ ] Any occurrence of `gall:`, `gg:`, `kh:`, `pcst:`, or project-private prefixes refuses promotion.
- [ ] No imperative scripts or witness binaries can directly declare final promotion; it must be actuated by Knowledge Hooks over the evidence graph.
- [ ] Every decision is emitted as an RDFDelta and bound by a cryptographic receipt.
- [ ] Same evidence + same hooks must replay to the same promotion/refusal.
- [ ] Full Worktree Coverage: Every source, test, example, schema, query, script, doc, and audit artifact is inventoried and verified.
- [ ] Sabotage Suite Completeness: All 12 negative-control sabotage cases must run, fail their expected gates, and restore correctly.
- [ ] Contradiction Supersession: Promoted/Refused conflicts default to Refused unless superseded by a resolved event.

## Follow-up — 2026-05-27T01:07:23Z

The user has finalized the specification for Genesis Day 6:
- The string `gall` is allowed in filenames/binary names but strictly banned in RDF namespaces/URIs.
- Added R7: Public Vocabulary Gate executable rules.
- Specified the 12 Sabotage Cases inline.
- Added the Receipt Vocabulary Rule (using `prov:Entity`, `dcat:Dataset`, `spdx:checksum` structures).

Please pivot the implementation to match the finalized prompt draft:

# Teamwork Project Prompt — Draft

Implement the **Witnessed Agent Truthfulness GALL** protocol in the `ggen` repository using **Witnessed Code Evaluation / Knowledge Hook Actuation** over OCEL / PROV / RDF evidence using **public vocabularies only** (no private vocabulary namespaces).

Working directory: /Users/sac/ggen

## Requirements

### R1. Boundary Observation Binaries
Implement Rust boundary observers under `crates/ggen-graph/src/bin/`:
- `gall_observe_worktree.rs`: Observe worktree inventory (paths, size, SHA256, BLAKE3).
- `gall_observe_commands.rs`: Observe commands, capture transcripts (stdout, stderr, argv, cwd, exit code, etc.).
- `gall_observe_sabotage.rs`: Observe negative-control sabotage runs.
- `gall_observe_clean_room.rs`: Observe clean-room build/test executions.
- `gall_observe_docs_tree.rs`: Observe documentation tree files.
- `gall_observe_doctests.rs`: Observe doctest coverage/verification.
- `gall_materialize_evidence_graph.rs`: Materialize individual observer outputs into a cohesive RDF/Turtle evidence graph using public vocabularies.
- `gall_actuate_code_evaluation.rs`: Actuate the evaluation runtime. Loads the Hook Pack and the evidence graph, runs the trigger query evaluation, and outputs the decision delta/receipt.

*Rule: Observer binaries must not decide final promotion. They only observe and materialize facts/evidence.*

### R2. Public-Vocabulary-Only Knowledge Hook Pack
Create:
- `crates/ggen-graph/hooks/gall-code-evaluation.ttl`

The hook pack must use public vocabularies only.
Allowed prefixes: `rdf:`, `rdfs:`, `owl:`, `xsd:`, `prov:`, `dcat:`, `dcterms:`, `skos:`, `sh:`, `time:`, `spdx:`, `ocel:`.
Forbidden prefixes: `gall:`, `gg:`, `kh:`, `pcst:`, `truex:`, or any project-private namespaces.

The hook pack expresses W0-W9 evaluation law using:
- `prov:Plan`
- `sh:NodeShape`
- `sh:SPARQLConstraint`
- `sh:ValidationReport`
- `sh:ValidationResult`
- `dcat:Dataset`
- `dcterms:identifier`
- `dcterms:title`
- `dcterms:description`
- `prov:used`
- `prov:wasGeneratedBy`
- `prov:wasDerivedFrom`

No private class or predicate may be introduced.

### R3. Public-Vocabulary Decision Deltas, Receipts & Final Adjudication
Generate decision/audit artifacts in `crates/ggen-graph/audit/`:
- `gall_evidence.ttl` / `gall_evidence.ocel.json`
- `gall_decision.delta.ttl`
- `gall_code_evaluation.receipt.ttl`
- `gall_code_evaluation.final.ttl`

The decision delta must not contain private terms. Instead, final status must be represented as public vocabulary facts:
- **Promotion**: `sh:ValidationReport` and `sh:conforms true` linked to execution via `prov:Activity` and `prov:wasGeneratedBy`.
- **Refusal**: `sh:ValidationReport` and `sh:conforms false` with one or more `sh:ValidationResult` objects with `sh:resultSeverity sh:Violation` and `sh:resultMessage`.

The final decision must include a receipt binding the evidence hash, hook pack hash, decision delta hash, output graph hash, timestamp, etc.

### R4. Critical Boundary / Authority Zones
- **Production library surface** (`crates/ggen-graph/src/lib.rs`, `crates/ggen-graph/src/{graph,delta,receipt,hooks,ocel,diagnostics}/**`) must forbid: `std::process::Command`, shell execution, filesystem deletion, network calls, and LLM calls.
- **Witness binary surface** (`crates/ggen-graph/src/bin/gall_*.rs`) may use: `std::process::Command`, `tempfile`, `walkdir`, file hashing, command transcript capture, and temporary worktree mutation.
- **Knowledge Hooks** (`crates/ggen-graph/hooks/gall-code-evaluation.ttl`) evaluate the evidence graph to actuate decisions but must not mutate the filesystem directly.

### R5. Emitted Docs Tree Index
Emit `docs/docs.tree.json` and `docs/docs.tree.ttl` listing every doc under `docs/`, its checkpoint, source requirements, hash, and status. Must use public vocabularies.

### R6. Doctest-Governed DX/QoL
All developer experience paths must be proven by Rust doctests (`cargo test -p ggen-graph --doc`).

### R7. Public Vocabulary Gate
Implement an executable public-vocabulary-only gate in the hook-actuated evaluation flow.
The gate must scan:
- `crates/ggen-graph/hooks/**/*.ttl`
- `crates/ggen-graph/audit/**/*.ttl`
- `docs/**/*.ttl`
- `schema/**/*.ttl`

Refuse promotion if any RDF prefix, predicate, or class uses a project-private namespace.
Allowed namespaces: RDF, RDFS, OWL, XSD, PROV-O, DCAT, DCTERMS, SKOS, SHACL, OWL-Time, SPDX, OCEL.
Emit refusal as:
- `sh:ValidationReport`
- `sh:conforms false`
- `sh:ValidationResult`
- `sh:resultSeverity sh:Violation`
- `sh:resultMessage` (explaining the private prefix violation using public terms only)

## Rules & Clarifications

### Namespace Clarification
The string `gall` may appear in filenames, binary names, directory names, and document titles as project terminology.
- **Allowed**: `gall_observe_worktree.rs`, `gall-code-evaluation.ttl`, `gall_evidence.ttl`, `VISION_2030_GALL_PROOF.md`.
- **Forbidden**: `@prefix gall:`, `gall:CheckpointPromoted`, `gall:PromotionBlocked`, `gall:VerifiedState`, or any private predicate/class.
Project terminology may name files; public vocabularies must carry RDF meaning.

### Receipt Vocabulary Rule
`gall_code_evaluation.receipt.ttl` must use public vocabularies only. The receipt must be represented as a `prov:Entity` and/or `dcat:Dataset`.
Required linkage:
- `prov:wasGeneratedBy` for the hook actuation activity
- `prov:used` for evidence graph and hook pack inputs
- `prov:wasDerivedFrom` for command transcripts, docs tree, coverage matrix, and sabotage evidence
- `dcterms:identifier` for stable receipt identity
- `dcterms:created` for receipt time
- `dcterms:description` for human-readable summary
- Hash values represented as literals using public predicates only, preferably `spdx:checksum` / SPDX checksum structures when represented in RDF.

## Required Sabotage Cases
All sabotage must occur in temporary copied worktrees.
1. Add `[features]` to `crates/ggen-graph/Cargo.toml`.
2. Add `TODO` to production source.
3. Add `std::process::Command` to production library surface.
4. Tamper receipt hash.
5. Remove requirement link from coverage or OCEL evidence.
6. Delete a source file referenced by coverage.
7. Add both promoted and refused checkpoint evidence without supersession.
8. Replace command transcript with empty file.
9. Alter OCEL timestamp order so promotion precedes evaluation.
10. Mutate observer/witness binary or launcher to unconditional success.
11. Remove required test fixture.
12. Replace SPARQL query fixture with invalid syntax.

Each sabotage case must emit:
- mutation description
- temp worktree path
- expected failed check
- actual failed check
- transcript artifact
- public-vocabulary evidence
- SHACL validation result

## Acceptance Criteria

### Execution & Evidence Integrity
- [ ] No private namespace appears in hook packs, evidence graphs, decision deltas, final reports, docs tree TTL, or receipts.
- [ ] Promotion/refusal is represented using `sh:ValidationReport` and `sh:conforms`.
- [ ] Refusal reasons are represented as `sh:ValidationResult` objects.
- [ ] Hook execution is represented as `prov:Activity`.
- [ ] Evidence artifacts are represented as `prov:Entity` and/or `dcat:Dataset`.
- [ ] Any occurrence of `gall:`, `gg:`, `kh:`, `pcst:`, or project-private prefixes refuses promotion.
- [ ] No imperative scripts or witness binaries can directly declare final promotion; it must be actuated by Knowledge Hooks over the evidence graph.
- [ ] Every decision is emitted as an RDFDelta and bound by a cryptographic receipt.
- [ ] Same evidence + same hooks must replay to the same promotion/refusal.
- [ ] Full Worktree Coverage: Every source, test, example, schema, query, script, doc, and audit artifact is inventoried and verified.
- [ ] Sabotage Suite Completeness: All 12 negative-control sabotage cases must run, fail their expected gates, and restore correctly.
- [ ] Contradiction Supersession: Promoted/Refused conflicts default to Refused unless superseded by a resolved event.

## Follow-up — 2026-05-27T01:08:19Z

The user has added the Dialect Completeness Matrix requirements. Please pivot the implementation to match the updated prompt draft:

# Teamwork Project Prompt — Draft

Implement the **Witnessed Agent Truthfulness GALL** protocol in the `ggen` repository using **Witnessed Code Evaluation / Knowledge Hook Actuation** over OCEL / PROV / RDF evidence using **public vocabularies only** (no private vocabulary namespaces), with a fully proven **Dialect Completeness Matrix**.

Working directory: /Users/sac/ggen

## Requirements

### R1. Boundary Observation Binaries
Implement Rust boundary observers under `crates/ggen-graph/src/bin/`:
- `gall_observe_worktree.rs`: Observe worktree inventory (paths, size, SHA256, BLAKE3).
- `gall_observe_commands.rs`: Observe commands, capture transcripts (stdout, stderr, argv, cwd, exit code, etc.).
- `gall_observe_sabotage.rs`: Observe negative-control sabotage runs.
- `gall_observe_clean_room.rs`: Observe clean-room build/test executions.
- `gall_observe_docs_tree.rs`: Observe documentation tree files.
- `gall_observe_doctests.rs`: Observe doctest coverage/verification.
- `gall_materialize_evidence_graph.rs`: Materialize individual observer outputs into a cohesive RDF/Turtle evidence graph using public vocabularies.
- `gall_actuate_code_evaluation.rs`: Actuate the evaluation runtime. Loads the Hook Pack and the evidence graph, runs the trigger query evaluation, and outputs the decision delta/receipt.

*Rule: Observer binaries must not decide final promotion. They only observe and materialize facts/evidence.*

### R2. Public-Vocabulary-Only Knowledge Hook Pack
Create:
- `crates/ggen-graph/hooks/gall-code-evaluation.ttl`

The hook pack must use public vocabularies only.
Allowed prefixes: `rdf:`, `rdfs:`, `owl:`, `xsd:`, `prov:`, `dcat:`, `dcterms:`, `skos:`, `sh:`, `time:`, `spdx:`, `ocel:`.
Forbidden prefixes: `gall:`, `gg:`, `kh:`, `pcst:`, `truex:`, or any project-private namespaces.

The hook pack expresses W0-W9 evaluation law using:
- `prov:Plan`
- `sh:NodeShape`
- `sh:SPARQLConstraint`
- `sh:ValidationReport`
- `sh:ValidationResult`
- `dcat:Dataset`
- `dcterms:identifier`
- `dcterms:title`
- `dcterms:description`
- `prov:used`
- `prov:wasGeneratedBy`
- `prov:wasDerivedFrom`

No private class or predicate may be introduced.

### R3. Public-Vocabulary Decision Deltas, Receipts & Final Adjudication
Generate decision/audit artifacts in `crates/ggen-graph/audit/`:
- `gall_evidence.ttl` / `gall_evidence.ocel.json`
- `gall_decision.delta.ttl`
- `gall_code_evaluation.receipt.ttl`
- `gall_code_evaluation.final.ttl`

The decision delta must not contain private terms. Instead, final status must be represented as public vocabulary facts:
- **Promotion**: `sh:ValidationReport` and `sh:conforms true` linked to execution via `prov:Activity` and `prov:wasGeneratedBy`.
- **Refusal**: `sh:ValidationReport` and `sh:conforms false` with one or more `sh:ValidationResult` objects with `sh:resultSeverity sh:Violation` and `sh:resultMessage`.

The final decision must include a receipt binding the evidence hash, hook pack hash, decision delta hash, output graph hash, timestamp, etc.

### R4. Critical Boundary / Authority Zones
- **Production library surface** (`crates/ggen-graph/src/lib.rs`, `crates/ggen-graph/src/{graph,delta,receipt,hooks,ocel,diagnostics}/**`) must forbid: `std::process::Command`, shell execution, filesystem deletion, network calls, and LLM calls.
- **Witness binary surface** (`crates/ggen-graph/src/bin/gall_*.rs`) may use: `std::process::Command`, `tempfile`, `walkdir`, file hashing, command transcript capture, and temporary worktree mutation.
- **Knowledge Hooks** (`crates/ggen-graph/hooks/gall-code-evaluation.ttl`) evaluate the evidence graph to actuate decisions but must not mutate the filesystem directly.

### R5. Emitted Docs Tree Index
Emit `docs/docs.tree.json` and `docs/docs.tree.ttl` listing every doc under `docs/`, its checkpoint, source requirements, hash, and status. Must use public vocabularies.

### R6. Doctest-Governed DX/QoL
All developer experience paths must be proven by Rust doctests (`cargo test -p ggen-graph --doc`).

### R7. Public Vocabulary Gate
Implement an executable public-vocabulary-only gate in the hook-actuated evaluation flow.
The gate must scan:
- `crates/ggen-graph/hooks/**/*.ttl`
- `crates/ggen-graph/audit/**/*.ttl`
- `docs/**/*.ttl`
- `schema/**/*.ttl`

Refuse promotion if any RDF prefix, predicate, or class uses a project-private namespace.
Allowed namespaces: RDF, RDFS, OWL, XSD, PROV-O, DCAT, DCTERMS, SKOS, SHACL, OWL-Time, SPDX, OCEL.
Emit refusal as:
- `sh:ValidationReport`
- `sh:conforms false`
- `sh:ValidationResult`
- `sh:resultSeverity sh:Violation`
- `sh:resultMessage` (explaining the private prefix violation using public terms only)

### R8. Dialect Completeness Matrix
The Witnessed Code Evaluation runtime must prove every supported dialect surface.
Supported dialects:
1. SPARQL ASK
2. SPARQL SELECT
3. SPARQL CONSTRUCT
4. SHACL
5. N3
6. Datalog
7. ShEx

For each dialect, the following must exist: positive fixture, negative fixture, malformed fixture, sabotage fixture, hook-pack trigger example, command transcript, OCEL event evidence, RDF evidence graph projection, receipt-bound decision delta, replay verification, and typed unsupported/refusal behavior where applicable.

#### Required Fixtures:
Under `crates/ggen-graph/tests/fixtures/dialects/`:
- `sparql/`: `ask_pass.rq`, `ask_fail.rq`, `select_pass.rq`, `construct_pass.rq`, `malformed.rq`
- `shacl/`: `conforms.ttl`, `violates.ttl`, `malformed.ttl`
- `n3/`: `rule_pass.n3`, `rule_fail.n3`, `malformed.n3`
- `datalog/`: `rule_pass.dl`, `rule_fail.dl`, `malformed.dl`
- `shex/`: `conforms.shex`, `violates.shex`, `malformed.shex`

#### Required Tests:
- `crates/ggen-graph/tests/dialect_completeness.rs`
- `crates/ggen-graph/tests/dialect_sabotage.rs`
- `crates/ggen-graph/tests/dialect_receipts.rs`
- `crates/ggen-graph/tests/dialect_replay.rs`

Each dialect must prove: valid input passes, invalid input refuses, malformed input errors, sabotaged input fails expected gate, decision appears in public-vocabulary SHACL report, receipt binds inputs/outputs, and replay returns same result.

The hook pack must contain one W-check hook per dialect family (`W-DIALECT-SPARQL`, `W-DIALECT-SHACL`, `W-DIALECT-N3`, `W-DIALECT-DATALOG`, `W-DIALECT-SHEX`) represented as public `prov:Plan` / `sh:NodeShape` definitions.

## Rules & Clarifications

### Namespace Clarification
The string `gall` may appear in filenames, binary names, directory names, and document titles as project terminology.
- **Allowed**: `gall_observe_worktree.rs`, `gall-code-evaluation.ttl`, `gall_evidence.ttl`, `VISION_2030_GALL_PROOF.md`.
- **Forbidden**: `@prefix gall:`, `gall:CheckpointPromoted`, `gall:PromotionBlocked`, `gall:VerifiedState`, or any private predicate/class.
Project terminology may name files; public vocabularies must carry RDF meaning.

### Receipt Vocabulary Rule
`gall_code_evaluation.receipt.ttl` must use public vocabularies only. The receipt must be represented as a `prov:Entity` and/or `dcat:Dataset`.
Required linkage:
- `prov:wasGeneratedBy` for the hook actuation activity
- `prov:used` for evidence graph and hook pack inputs
- `prov:wasDerivedFrom` for command transcripts, docs tree, coverage matrix, and sabotage evidence
- `dcterms:identifier` for stable receipt identity
- `dcterms:created` for receipt time
- `dcterms:description` for human-readable summary
- Hash values represented as literals using public predicates only, preferably `spdx:checksum` / SPDX checksum structures when represented in RDF.

## Required Sabotage Cases
All sabotage must occur in temporary copied worktrees.
1. Add `[features]` to `crates/ggen-graph/Cargo.toml`.
2. Add `TODO` to production source.
3. Add `std::process::Command` to production library surface.
4. Tamper receipt hash.
5. Remove requirement link from coverage or OCEL evidence.
6. Delete a source file referenced by coverage.
7. Add both promoted and refused checkpoint evidence without supersession.
8. Replace command transcript with empty file.
9. Alter OCEL timestamp order so promotion precedes evaluation.
10. Mutate observer/witness binary or launcher to unconditional success.
11. Remove required test fixture.
12. Replace SPARQL query fixture with invalid syntax.

Each sabotage case must emit:
- mutation description
- temp worktree path
- expected failed check
- actual failed check
- transcript artifact
- public-vocabulary evidence
- SHACL validation result

## Acceptance Criteria

### Execution & Evidence Integrity
- [ ] SPARQL ASK / SELECT / CONSTRUCT are tested through positive, negative, malformed, sabotage, receipt, and replay paths.
- [ ] SHACL is tested through conforming, violating, malformed, sabotage, receipt, and replay paths.
- [ ] N3 is tested through pass, fail, malformed, sabotage, receipt, and replay paths.
- [ ] Datalog is tested through pass, fail, malformed, sabotage, receipt, and replay paths.
- [ ] ShEx is tested through conforming, violating, malformed, sabotage, receipt, and replay paths.
- [ ] No dialect is marked supported unless its evidence appears in the OCEL log, RDF evidence graph, decision delta, receipt, and replay result.
- [ ] If a dialect is intentionally not executable yet, it must be represented as a bounded unsupported capability with `sh:conforms false`, not silently omitted.
- [ ] No private namespace appears in hook packs, evidence graphs, decision deltas, final reports, docs tree TTL, or receipts.
- [ ] Promotion/refusal is represented using `sh:ValidationReport` and `sh:conforms`.
- [ ] Refusal reasons are represented as `sh:ValidationResult` objects.
- [ ] Hook execution is represented as `prov:Activity`.
- [ ] Evidence artifacts are represented as `prov:Entity` and/or `dcat:Dataset`.
- [ ] Any occurrence of `gall:`, `gg:`, `kh:`, `pcst:`, or project-private prefixes refuses promotion.
- [ ] No imperative scripts or witness binaries can directly declare final promotion; it must be actuated by Knowledge Hooks over the evidence graph.
- [ ] Every decision is emitted as an RDFDelta and bound by a cryptographic receipt.
- [ ] Same evidence + same hooks must replay to the same promotion/refusal.
- [ ] Full Worktree Coverage: Every source, test, example, schema, query, script, doc, and audit artifact is inventoried and verified.
- [ ] Sabotage Suite Completeness: All 12 negative-control sabotage cases must run, fail their expected gates, and restore correctly.
- [ ] Contradiction Supersession: Promoted/Refused conflicts default to Refused unless superseded by a resolved event.

## Follow-up — 2026-05-27T01:09:26Z

The user has added the OFMF-to-Public-Vocabulary hook schema mapping requirements. Please pivot the implementation to match the updated prompt draft:

# Teamwork Project Prompt — Draft

Implement the **Witnessed Agent Truthfulness GALL** protocol in the `ggen` repository using **Witnessed Code Evaluation / Knowledge Hook Actuation** over OCEL / PROV / RDF evidence using **public vocabularies only** (no private vocabulary namespaces), with a fully proven **Dialect Completeness Matrix** and an **OFMF-to-Public-Vocabulary hook schema mapping**.

Working directory: /Users/sac/ggen

## Requirements

### R1. Boundary Observation Binaries
Implement Rust boundary observers under `crates/ggen-graph/src/bin/`:
- `gall_observe_worktree.rs`: Observe worktree inventory (paths, size, SHA256, BLAKE3).
- `gall_observe_commands.rs`: Observe commands, capture transcripts (stdout, stderr, argv, cwd, exit code, etc.).
- `gall_observe_sabotage.rs`: Observe negative-control sabotage runs.
- `gall_observe_clean_room.rs`: Observe clean-room build/test executions.
- `gall_observe_docs_tree.rs`: Observe documentation tree files.
- `gall_observe_doctests.rs`: Observe doctest coverage/verification.
- `gall_materialize_evidence_graph.rs`: Materialize individual observer outputs into a cohesive RDF/Turtle evidence graph using public vocabularies.
- `gall_actuate_code_evaluation.rs`: Actuate the evaluation runtime. Loads the Hook Pack and the evidence graph, runs the trigger query evaluation, and outputs the decision delta/receipt.

*Rule: Observer binaries must not decide final promotion. They only observe and materialize facts/evidence.*

### R2. Public-Vocabulary-Only Knowledge Hook Pack
Create:
- `crates/ggen-graph/hooks/gall-code-evaluation.ttl`

The hook pack must use public vocabularies only.
Allowed prefixes: `rdf:`, `rdfs:`, `owl:`, `xsd:`, `prov:`, `dcat:`, `dcterms:`, `skos:`, `sh:`, `time:`, `spdx:`, `ocel:`.
Forbidden prefixes: `gall:`, `gg:`, `kh:`, `pcst:`, `truex:`, or any project-private namespaces.

The hook pack expresses W0-W9 evaluation law by mapping the OFMF hook schema to public vocabularies:
- **OFMF HookPack**: Maps to `dcat:Dataset` + `prov:Entity`.
- **OFMF KnowledgeHook**: Maps to `prov:Plan` + `sh:NodeShape`.
- **OFMF HookTrigger**: Maps to `sh:SPARQLConstraint` / SHACL / N3 / Datalog / ShEx artifact.
- **OFMF HookAction**: Maps to SPARQL CONSTRUCT public-vocabulary delta.
- **OFMF DiagnosticEmitter**: Maps to `sh:ValidationReport` + `sh:ValidationResult`.
- **OFMF Receipt**: Maps to `prov:Entity` + `dcat:Dataset` + checksum evidence.
- **OFMF Runtime**: Maps to hook actuation over the evidence graph.

No private class or predicate may be introduced.

### R3. Public-Vocabulary Decision Deltas, Receipts & Final Adjudication
Generate decision/audit artifacts in `crates/ggen-graph/audit/`:
- `gall_evidence.ttl` / `gall_evidence.ocel.json`
- `gall_decision.delta.ttl`
- `gall_code_evaluation.receipt.ttl`
- `gall_code_evaluation.final.ttl`

The decision delta must not contain private terms. Instead, final status must be represented as public vocabulary facts:
- **Promotion**: `sh:ValidationReport` and `sh:conforms true` linked to execution via `prov:Activity` and `prov:wasGeneratedBy`.
- **Refusal**: `sh:ValidationReport` and `sh:conforms false` with one or more `sh:ValidationResult` objects with `sh:resultSeverity sh:Violation` and `sh:resultMessage`.

The final decision must include a receipt binding the evidence hash, hook pack hash, decision delta hash, output graph hash, timestamp, etc.

### R4. Critical Boundary / Authority Zones
- **Production library surface** (`crates/ggen-graph/src/lib.rs`, `crates/ggen-graph/src/{graph,delta,receipt,hooks,ocel,diagnostics}/**`) must forbid: `std::process::Command`, shell execution, filesystem deletion, network calls, and LLM calls.
- **Witness binary surface** (`crates/ggen-graph/src/bin/gall_*.rs`) may use: `std::process::Command`, `tempfile`, `walkdir`, file hashing, command transcript capture, and temporary worktree mutation.
- **Knowledge Hooks** (`crates/ggen-graph/hooks/gall-code-evaluation.ttl`) evaluate the evidence graph to actuate decisions but must not mutate the filesystem directly.

### R5. Emitted Docs Tree Index
Emit `docs/docs.tree.json` and `docs/docs.tree.ttl` listing every doc under `docs/`, its checkpoint, source requirements, hash, and status. Must use public vocabularies.

### R6. Doctest-Governed DX/QoL
All developer experience paths must be proven by Rust doctests (`cargo test -p ggen-graph --doc`).

### R7. Public Vocabulary Gate
Implement an executable public-vocabulary-only gate in the hook-actuated evaluation flow.
The gate must scan:
- `crates/ggen-graph/hooks/**/*.ttl`
- `crates/ggen-graph/audit/**/*.ttl`
- `docs/**/*.ttl`
- `schema/**/*.ttl`

Refuse promotion if any RDF prefix, predicate, or class uses a project-private namespace.
Allowed namespaces: RDF, RDFS, OWL, XSD, PROV-O, DCAT, DCTERMS, SKOS, SHACL, OWL-Time, SPDX, OCEL.
Emit refusal as:
- `sh:ValidationReport`
- `sh:conforms false`
- `sh:ValidationResult`
- `sh:resultSeverity sh:Violation`
- `sh:resultMessage` (explaining the private prefix violation using public terms only)

### R8. Dialect Completeness Matrix
The Witnessed Code Evaluation runtime must prove every supported dialect surface.
Supported dialects:
1. SPARQL ASK
2. SPARQL SELECT
3. SPARQL CONSTRUCT
4. SHACL
5. N3
6. Datalog
7. ShEx

For each dialect, the following must exist: positive fixture, negative fixture, malformed fixture, sabotage fixture, hook-pack trigger example, command transcript, OCEL event evidence, RDF evidence graph projection, receipt-bound decision delta, replay verification, and typed unsupported/refusal behavior where applicable.

#### Required Fixtures:
Under `crates/ggen-graph/tests/fixtures/dialects/`:
- `sparql/`: `ask_pass.rq`, `ask_fail.rq`, `select_pass.rq`, `construct_pass.rq`, `malformed.rq`
- `shacl/`: `conforms.ttl`, `violates.ttl`, `malformed.ttl`
- `n3/`: `rule_pass.n3`, `rule_fail.n3`, `malformed.n3`
- `datalog/`: `rule_pass.dl`, `rule_fail.dl`, `malformed.dl`
- `shex/`: `conforms.shex`, `violates.shex`, `malformed.shex`

#### Required Tests:
- `crates/ggen-graph/tests/dialect_completeness.rs`
- `crates/ggen-graph/tests/dialect_sabotage.rs`
- `crates/ggen-graph/tests/dialect_receipts.rs`
- `crates/ggen-graph/tests/dialect_replay.rs`

Each dialect must prove: valid input passes, invalid input refuses, malformed input errors, sabotaged input fails expected gate, decision appears in public-vocabulary SHACL report, receipt binds inputs/outputs, and replay returns same result.

The hook pack must contain one W-check hook per dialect family (`W-DIALECT-SPARQL`, `W-DIALECT-SHACL`, `W-DIALECT-N3`, `W-DIALECT-DATALOG`, `W-DIALECT-SHEX`) represented as public `prov:Plan` / `sh:NodeShape` definitions.

## Rules & Clarifications

### Namespace Clarification
The string `gall` may appear in filenames, binary names, directory names, and document titles as project terminology.
- **Allowed**: `gall_observe_worktree.rs`, `gall-code-evaluation.ttl`, `gall_evidence.ttl`, `VISION_2030_GALL_PROOF.md`.
- **Forbidden**: `@prefix gall:`, `gall:CheckpointPromoted`, `gall:PromotionBlocked`, `gall:VerifiedState`, or any private predicate/class.
Project terminology may name files; public vocabularies must carry RDF meaning.

### Receipt Vocabulary Rule
`gall_code_evaluation.receipt.ttl` must use public vocabularies only. The receipt must be represented as a `prov:Entity` and/or `dcat:Dataset`.
Required linkage:
- `prov:wasGeneratedBy` for the hook actuation activity
- `prov:used` for evidence graph and hook pack inputs
- `prov:wasDerivedFrom` for command transcripts, docs tree, coverage matrix, and sabotage evidence
- `dcterms:identifier` for stable receipt identity
- `dcterms:created` for receipt time
- `dcterms:description` for human-readable summary
- Hash values represented as literals using public predicates only, preferably `spdx:checksum` / SPDX checksum structures when represented in RDF.

## Required Sabotage Cases
All sabotage must occur in temporary copied worktrees.
1. Add `[features]` to `crates/ggen-graph/Cargo.toml`.
2. Add `TODO` to production source.
3. Add `std::process::Command` to production library surface.
4. Tamper receipt hash.
5. Remove requirement link from coverage or OCEL evidence.
6. Delete a source file referenced by coverage.
7. Add both promoted and refused checkpoint evidence without supersession.
8. Replace command transcript with empty file.
9. Alter OCEL timestamp order so promotion precedes evaluation.
10. Mutate observer/witness binary or launcher to unconditional success.
11. Remove required test fixture.
12. Replace SPARQL query fixture with invalid syntax.

Each sabotage case must emit:
- mutation description
- temp worktree path
- expected failed check
- actual failed check
- transcript artifact
- public-vocabulary evidence
- SHACL validation result

## Acceptance Criteria

### Execution & Evidence Integrity
- [ ] SPARQL ASK / SELECT / CONSTRUCT are tested through positive, negative, malformed, sabotage, receipt, and replay paths.
- [ ] SHACL is tested through conforming, violating, malformed, sabotage, receipt, and replay paths.
- [ ] N3 is tested through pass, fail, malformed, sabotage, receipt, and replay paths.
- [ ] Datalog is tested through pass, fail, malformed, sabotage, receipt, and replay paths.
- [ ] ShEx is tested through conforming, violating, malformed, sabotage, receipt, and replay paths.
- [ ] No dialect is marked supported unless its evidence appears in the OCEL log, RDF evidence graph, decision delta, receipt, and replay result.
- [ ] If a dialect is intentionally not executable yet, it must be represented as a bounded unsupported capability with `sh:conforms false`, not silently omitted.
- [ ] No private namespace appears in hook packs, evidence graphs, decision deltas, final reports, docs tree TTL, or receipts.
- [ ] Promotion/refusal is represented using `sh:ValidationReport` and `sh:conforms`.
- [ ] Refusal reasons are represented as `sh:ValidationResult` objects.
- [ ] Hook execution is represented as `prov:Activity`.
- [ ] Evidence artifacts are represented as `prov:Entity` and/or `dcat:Dataset`.
- [ ] Any occurrence of `gall:`, `gg:`, `kh:`, `pcst:`, or project-private prefixes refuses promotion.
- [ ] No imperative scripts or witness binaries can directly declare final promotion; it must be actuated by Knowledge Hooks over the evidence graph.
- [ ] Every decision is emitted as an RDFDelta and bound by a cryptographic receipt.
- [ ] Same evidence + same hooks must replay to the same promotion/refusal.
- [ ] Full Worktree Coverage: Every source, test, example, schema, query, script, doc, and audit artifact is inventoried and verified.
- [ ] Sabotage Suite Completeness: All 12 negative-control sabotage cases must run, fail their expected gates, and restore correctly.
- [ ] Contradiction Supersession: Promoted/Refused conflicts default to Refused unless superseded by a resolved event.

## Follow-up — 2026-05-27T01:10:34Z

The user has specified that hooks must not directly execute boundary work. Instead, they trigger it indirectly by materializing a receipted boundary request (BoundaryExecutionRequest) in the evidence graph. The boundary adapters then consume these request facts, run the actual processes, and emit transcript evidence.

Please pivot the implementation to match the updated prompt draft:

# Teamwork Project Prompt — Draft

Implement the **Witnessed Agent Truthfulness GALL** protocol in the `ggen` repository using **Witnessed Code Evaluation / Knowledge Hook Actuation** over OCEL / PROV / RDF evidence using **public vocabularies only** (no private vocabulary namespaces), with a fully proven **Dialect Completeness Matrix**, an **OFMF-to-Public-Vocabulary hook schema mapping**, and an **Indirect Hook Boundary Request / Boundary Adapter** architecture.

Working directory: /Users/sac/ggen

## Requirements

### R1. Boundary Observation Binaries & Boundary Adapters
Implement Rust boundary observers and adapters under `crates/ggen-graph/src/bin/`:
- `gall_observe_worktree.rs`: Observe worktree inventory (paths, size, SHA256, BLAKE3).
- `gall_observe_commands.rs`: Observe commands, capture transcripts (stdout, stderr, argv, cwd, exit code, etc.).
- `gall_observe_sabotage.rs`: Observe negative-control sabotage runs.
- `gall_observe_clean_room.rs`: Observe clean-room build/test executions.
- `gall_observe_docs_tree.rs`: Observe documentation tree files.
- `gall_observe_doctests.rs`: Observe doctest coverage/verification.
- `gall_materialize_evidence_graph.rs`: Materialize individual observer outputs into a cohesive RDF/Turtle evidence graph using public vocabularies.
- `gall_actuate_code_evaluation.rs`: Actuate the evaluation runtime. Loads the Hook Pack and the evidence graph, runs the trigger query evaluation, and outputs the decision delta/receipt.
- **Boundary Adapters**: Separate binaries or adapters that watch for and consume `BoundaryExecutionRequest` activities. They validate that requests are valid, receipted, generated by hook actuation, use public vocabularies, and correspond to known capabilities. They execute the requested external action (e.g. running cargo tests or command runs) and emit the resulting command transcripts, PROV execution activities, and OCEL events back to the evidence graph.

*Rule: Observer binaries and boundary adapters must not decide final promotion. Hooks trigger boundary execution by emitting request facts; adapters materialize the boundary acts and emit evidence.*

### R2. Public-Vocabulary-Only Knowledge Hook Pack
Create:
- `crates/ggen-graph/hooks/gall-code-evaluation.ttl`

The hook pack must use public vocabularies only.
Allowed prefixes: `rdf:`, `rdfs:`, `owl:`, `xsd:`, `prov:`, `dcat:`, `dcterms:`, `skos:`, `sh:`, `time:`, `spdx:`, `ocel:`.
Forbidden prefixes: `gall:`, `gg:`, `kh:`, `pcst:`, `truex:`, or any project-private namespaces.

The hook pack expresses W0-W9 evaluation law by mapping the OFMF hook schema to public vocabularies:
- **OFMF HookPack**: Maps to `dcat:Dataset` + `prov:Entity`.
- **OFMF KnowledgeHook**: Maps to `prov:Plan` + `sh:NodeShape`.
- **OFMF HookTrigger**: Maps to `sh:SPARQLConstraint` / SHACL / N3 / Datalog / ShEx artifact.
- **OFMF HookAction**: Maps to SPARQL CONSTRUCT public-vocabulary delta.
- **OFMF DiagnosticEmitter**: Maps to `sh:ValidationReport` + `sh:ValidationResult`.
- **OFMF Receipt**: Maps to `prov:Entity` + `dcat:Dataset` + checksum evidence.
- **OFMF Runtime**: Maps to hook actuation over the evidence graph.

*Rule: Hooks must not directly execute boundary work (filesystem mutation, shell execution, etc.). They trigger boundary execution by emitting `BoundaryExecutionRequest` facts in the delta.*

### R3. Public-Vocabulary Decision Deltas, Receipts & Final Adjudication
Generate decision/audit artifacts in `crates/ggen-graph/audit/`:
- `gall_evidence.ttl` / `gall_evidence.ocel.json`
- `gall_decision.delta.ttl`
- `gall_code_evaluation.receipt.ttl`
- `gall_code_evaluation.final.ttl`

The decision delta must not contain private terms. Instead, final status must be represented as public vocabulary facts:
- **Promotion**: `sh:ValidationReport` and `sh:conforms true` linked to execution via `prov:Activity` and `prov:wasGeneratedBy`.
- **Refusal**: `sh:ValidationReport` and `sh:conforms false` with one or more `sh:ValidationResult` objects with `sh:resultSeverity sh:Violation` and `sh:resultMessage`.

The final decision must include a receipt binding the evidence hash, hook pack hash, decision delta hash, output graph hash, timestamp, etc.

### R4. Critical Boundary / Authority Zones
- **Production library surface** (`crates/ggen-graph/src/lib.rs`, `crates/ggen-graph/src/{graph,delta,receipt,hooks,ocel,diagnostics}/**`) must forbid: `std::process::Command`, shell execution, filesystem deletion, network calls, and LLM calls.
- **Witness binary surface / Boundary Adapters** (`crates/ggen-graph/src/bin/gall_*.rs`) may use: `std::process::Command`, `tempfile`, `walkdir`, file hashing, command transcript capture, and temporary worktree mutation to fulfill authorized `BoundaryExecutionRequest` activities.
- **Knowledge Hooks** (`crates/ggen-graph/hooks/gall-code-evaluation.ttl`) evaluate the evidence graph to actuate decisions but must not mutate the filesystem directly or run commands.

### R5. Emitted Docs Tree Index
Emit `docs/docs.tree.json` and `docs/docs.tree.ttl` listing every doc under `docs/`, its checkpoint, source requirements, hash, and status. Must use public vocabularies.

### R6. Doctest-Governed DX/QoL
All developer experience paths must be proven by Rust doctests (`cargo test -p ggen-graph --doc`).

### R7. Public Vocabulary Gate
Implement an executable public-vocabulary-only gate in the hook-actuated evaluation flow.
The gate must scan:
- `crates/ggen-graph/hooks/**/*.ttl`
- `crates/ggen-graph/audit/**/*.ttl`
- `docs/**/*.ttl`
- `schema/**/*.ttl`

Refuse promotion if any RDF prefix, predicate, or class uses a project-private namespace.
Allowed namespaces: RDF, RDFS, OWL, XSD, PROV-O, DCAT, DCTERMS, SKOS, SHACL, OWL-Time, SPDX, OCEL.
Emit refusal as:
- `sh:ValidationReport`
- `sh:conforms false`
- `sh:ValidationResult`
- `sh:resultSeverity sh:Violation`
- `sh:resultMessage` (explaining the private prefix violation using public terms only)

### R8. Dialect Completeness Matrix
The Witnessed Code Evaluation runtime must prove every supported dialect surface.
Supported dialects:
1. SPARQL ASK
2. SPARQL SELECT
3. SPARQL CONSTRUCT
4. SHACL
5. N3
6. Datalog
7. ShEx

For each dialect, the following must exist: positive fixture, negative fixture, malformed fixture, sabotage fixture, hook-pack trigger example, command transcript, OCEL event evidence, RDF evidence graph projection, receipt-bound decision delta, replay verification, and typed unsupported/refusal behavior where applicable.

#### Required Fixtures:
Under `crates/ggen-graph/tests/fixtures/dialects/`:
- `sparql/`: `ask_pass.rq`, `ask_fail.rq`, `select_pass.rq`, `construct_pass.rq`, `malformed.rq`
- `shacl/`: `conforms.ttl`, `violates.ttl`, `malformed.ttl`
- `n3/`: `rule_pass.n3`, `rule_fail.n3`, `malformed.n3`
- `datalog/`: `rule_pass.dl`, `rule_fail.dl`, `malformed.dl`
- `shex/`: `conforms.shex`, `violates.shex`, `malformed.shex`

#### Required Tests:
- `crates/ggen-graph/tests/dialect_completeness.rs`
- `crates/ggen-graph/tests/dialect_sabotage.rs`
- `crates/ggen-graph/tests/dialect_receipts.rs`
- `crates/ggen-graph/tests/dialect_replay.rs`

Each dialect must prove: valid input passes, invalid input refuses, malformed input errors, sabotaged input fails expected gate, decision appears in public-vocabulary SHACL report, receipt binds inputs/outputs, and replay returns same result.

The hook pack must contain one W-check hook per dialect family (`W-DIALECT-SPARQL`, `W-DIALECT-SHACL`, `W-DIALECT-N3`, `W-DIALECT-DATALOG`, `W-DIALECT-SHEX`) represented as public `prov:Plan` / `sh:NodeShape` definitions.

## Rules & Clarifications

### Namespace Clarification
The string `gall` may appear in filenames, binary names, directory names, and document titles as project terminology.
- **Allowed**: `gall_observe_worktree.rs`, `gall-code-evaluation.ttl`, `gall_evidence.ttl`, `VISION_2030_GALL_PROOF.md`.
- **Forbidden**: `@prefix gall:`, `gall:CheckpointPromoted`, `gall:PromotionBlocked`, `gall:VerifiedState`, or any private predicate/class.
Project terminology may name files; public vocabularies must carry RDF meaning.

### Receipt Vocabulary Rule
`gall_code_evaluation.receipt.ttl` must use public vocabularies only. The receipt must be represented as a `prov:Entity` and/or `dcat:Dataset`.
Required linkage:
- `prov:wasGeneratedBy` for the hook actuation activity
- `prov:used` for evidence graph and hook pack inputs
- `prov:wasDerivedFrom` for command transcripts, docs tree, coverage matrix, and sabotage evidence
- `dcterms:identifier` for stable receipt identity
- `dcterms:created` for receipt time
- `dcterms:description` for human-readable summary
- Hash values represented as literals using public predicates only, preferably `spdx:checksum` / SPDX checksum structures when represented in RDF.

## Required Sabotage Cases
All sabotage must occur in temporary copied worktrees.
1. Add `[features]` to `crates/ggen-graph/Cargo.toml`.
2. Add `TODO` to production source.
3. Add `std::process::Command` to production library surface.
4. Tamper receipt hash.
5. Remove requirement link from coverage or OCEL evidence.
6. Delete a source file referenced by coverage.
7. Add both promoted and refused checkpoint evidence without supersession.
8. Replace command transcript with empty file.
9. Alter OCEL timestamp order so promotion precedes evaluation.
10. Mutate observer/witness binary or launcher to unconditional success.
11. Remove required test fixture.
12. Replace SPARQL query fixture with invalid syntax.

Each sabotage case must emit:
- mutation description
- temp worktree path
- expected failed check
- actual failed check
- transcript artifact
- public-vocabulary evidence
- SHACL validation result

## Acceptance Criteria

### Execution & Evidence Integrity
- [ ] Boundary execution is indirect: Hooks trigger boundary execution by emitting `prov:Activity` records labeled with `dcterms:type "BoundaryExecutionRequest"`; boundary adapters consume those records and emit transcript evidence back. No hook directly performs boundary work.
- [ ] SPARQL ASK / SELECT / CONSTRUCT are tested through positive, negative, malformed, sabotage, receipt, and replay paths.
- [ ] SHACL is tested through conforming, violating, malformed, sabotage, receipt, and replay paths.
- [ ] N3 is tested through pass, fail, malformed, sabotage, receipt, and replay paths.
- [ ] Datalog is tested through pass, fail, malformed, sabotage, receipt, and replay paths.
- [ ] ShEx is tested through conforming, violating, malformed, sabotage, receipt, and replay paths.
- [ ] No dialect is marked supported unless its evidence appears in the OCEL log, RDF evidence graph, decision delta, receipt, and replay result.
- [ ] If a dialect is intentionally not executable yet, it must be represented as a bounded unsupported capability with `sh:conforms false`, not silently omitted.
- [ ] No private namespace appears in hook packs, evidence graphs, decision deltas, final reports, docs tree TTL, or receipts.
- [ ] Promotion/refusal is represented using `sh:ValidationReport` and `sh:conforms`.
- [ ] Refusal reasons are represented as `sh:ValidationResult` objects.
- [ ] Hook execution is represented as `prov:Activity`.
- [ ] Evidence artifacts are represented as `prov:Entity` and/or `dcat:Dataset`.
- [ ] Any occurrence of `gall:`, `gg:`, `kh:`, `pcst:`, or project-private prefixes refuses promotion.
- [ ] No imperative scripts or witness binaries can directly declare final promotion; it must be actuated by Knowledge Hooks over the evidence graph.
- [ ] Every decision is emitted as an RDFDelta and bound by a cryptographic receipt.
- [ ] Same evidence + same hooks must replay to the same promotion/refusal.
- [ ] Full Worktree Coverage: Every source, test, example, schema, query, script, doc, and audit artifact is inventoried and verified.
- [ ] Sabotage Suite Completeness: All 12 negative-control sabotage cases must run, fail their expected gates, and restore correctly.
- [ ] Contradiction Supersession: Promoted/Refused conflicts default to Refused unless superseded by a resolved event.

## Follow-up — 2026-05-27T01:11:28Z

The user has finalized the requirements for delegation by adding three critical hardening patches:
1. Custom operational labels (like "BoundaryExecutionRequest") are allowed only as literal values of public predicates, never as private RDF classes/predicates.
2. Dialects must either be fully executable (fixture, transcript, receipt, replay) or yield a bounded unsupported capability SHACL report.
3. Boundary adapters must consume requests only once, verifying they are receipted and contain stable identifiers/hashes to prevent replay attacks.

Please pivot the implementation to match the finalized prompt draft:

# Teamwork Project Prompt — Draft

Implement the **Witnessed Agent Truthfulness GALL** protocol in the `ggen` repository using **Witnessed Code Evaluation / Knowledge Hook Actuation** over OCEL / PROV / RDF evidence using **public vocabularies only** (no private vocabulary namespaces), with a fully proven **Dialect Completeness Matrix**, an **OFMF-to-Public-Vocabulary hook schema mapping**, and an **Indirect Hook Boundary Request / Boundary Adapter** architecture.

Working directory: /Users/sac/ggen

## Requirements

### R1. Boundary Observation Binaries & Boundary Adapters
Implement Rust boundary observers and adapters under `crates/ggen-graph/src/bin/`:
- `gall_observe_worktree.rs`: Observe worktree inventory (paths, size, SHA256, BLAKE3).
- `gall_observe_commands.rs`: Observe commands, capture transcripts (stdout, stderr, argv, cwd, exit code, etc.).
- `gall_observe_sabotage.rs`: Observe negative-control sabotage runs.
- `gall_observe_clean_room.rs`: Observe clean-room build/test executions.
- `gall_observe_docs_tree.rs`: Observe documentation tree files.
- `gall_observe_doctests.rs`: Observe doctest coverage/verification.
- `gall_materialize_evidence_graph.rs`: Materialize individual observer outputs into a cohesive RDF/Turtle evidence graph using public vocabularies.
- `gall_actuate_code_evaluation.rs`: Actuate the evaluation runtime. Loads the Hook Pack and the evidence graph, runs the trigger query evaluation, and outputs the decision delta/receipt.
- **Boundary Adapters**: Separate binaries or adapters that watch for and consume `BoundaryExecutionRequest` activities. They validate that requests are valid, receipted, generated by hook actuation, use public vocabularies, and correspond to known capabilities. They execute the requested external action (e.g. running cargo tests or command runs) and emit the resulting command transcripts, PROV execution activities, and OCEL events back to the evidence graph.

*Rule: Observer binaries and boundary adapters must not decide final promotion. Hooks trigger boundary execution by emitting request facts; adapters materialize the boundary acts and emit evidence.*

### R2. Public-Vocabulary-Only Knowledge Hook Pack
Create:
- `crates/ggen-graph/hooks/gall-code-evaluation.ttl`

The hook pack must use public vocabularies only.
Allowed prefixes: `rdf:`, `rdfs:`, `owl:`, `xsd:`, `prov:`, `dcat:`, `dcterms:`, `skos:`, `sh:`, `time:`, `spdx:`, `ocel:`.
Forbidden prefixes: `gall:`, `gg:`, `kh:`, `pcst:`, `truex:`, or any project-private namespaces.

The hook pack expresses W0-W9 evaluation law by mapping the OFMF hook schema to public vocabularies:
- **OFMF HookPack**: Maps to `dcat:Dataset` + `prov:Entity`.
- **OFMF KnowledgeHook**: Maps to `prov:Plan` + `sh:NodeShape`.
- **OFMF HookTrigger**: Maps to `sh:SPARQLConstraint` / SHACL / N3 / Datalog / ShEx artifact.
- **OFMF HookAction**: Maps to SPARQL CONSTRUCT public-vocabulary delta.
- **OFMF DiagnosticEmitter**: Maps to `sh:ValidationReport` + `sh:ValidationResult`.
- **OFMF Receipt**: Maps to `prov:Entity` + `dcat:Dataset` + checksum evidence.
- **OFMF Runtime**: Maps to hook actuation over the evidence graph.

*Rule: Hooks must not directly execute boundary work (filesystem mutation, shell execution, etc.). They trigger boundary execution by emitting `BoundaryExecutionRequest` facts in the delta.*

### R3. Public-Vocabulary Decision Deltas, Receipts & Final Adjudication
Generate decision/audit artifacts in `crates/ggen-graph/audit/`:
- `gall_evidence.ttl` / `gall_evidence.ocel.json`
- `gall_decision.delta.ttl`
- `gall_code_evaluation.receipt.ttl`
- `gall_code_evaluation.final.ttl`

The decision delta must not contain private terms. Instead, final status must be represented as public vocabulary facts:
- **Promotion**: `sh:ValidationReport` and `sh:conforms true` linked to execution via `prov:Activity` and `prov:wasGeneratedBy`.
- **Refusal**: `sh:ValidationReport` and `sh:conforms false` with one or more `sh:ValidationResult` objects with `sh:resultSeverity sh:Violation` and `sh:resultMessage`.

The final decision must include a receipt binding the evidence hash, hook pack hash, decision delta hash, output graph hash, timestamp, etc.

### R4. Critical Boundary / Authority Zones
- **Production library surface** (`crates/ggen-graph/src/lib.rs`, `crates/ggen-graph/src/{graph,delta,receipt,hooks,ocel,diagnostics}/**`) must forbid: `std::process::Command`, shell execution, filesystem deletion, network calls, and LLM calls.
- **Witness binary surface / Boundary Adapters** (`crates/ggen-graph/src/bin/gall_*.rs`) may use: `std::process::Command`, `tempfile`, `walkdir`, file hashing, command transcript capture, and temporary worktree mutation to fulfill authorized `BoundaryExecutionRequest` activities.
- **Knowledge Hooks** (`crates/ggen-graph/hooks/gall-code-evaluation.ttl`) evaluate the evidence graph to actuate decisions but must not mutate the filesystem directly or run commands.

### R5. Emitted Docs Tree Index
Emit `docs/docs.tree.json` and `docs/docs.tree.ttl` listing every doc under `docs/`, its checkpoint, source requirements, hash, and status. Must use public vocabularies.

### R6. Doctest-Governed DX/QoL
All developer experience paths must be proven by Rust doctests (`cargo test -p ggen-graph --doc`).

### R7. Public Vocabulary Gate
Implement an executable public-vocabulary-only gate in the hook-actuated evaluation flow.
The gate must scan:
- `crates/ggen-graph/hooks/**/*.ttl`
- `crates/ggen-graph/audit/**/*.ttl`
- `docs/**/*.ttl`
- `schema/**/*.ttl`

Refuse promotion if any RDF prefix, predicate, or class uses a project-private namespace.
Allowed namespaces: RDF, RDFS, OWL, XSD, PROV-O, DCAT, DCTERMS, SKOS, SHACL, OWL-Time, SPDX, OCEL.
Emit refusal as:
- `sh:ValidationReport`
- `sh:conforms false`
- `sh:ValidationResult`
- `sh:resultSeverity sh:Violation`
- `sh:resultMessage` (explaining the private prefix violation using public terms only)

### R8. Dialect Completeness Matrix
The Witnessed Code Evaluation runtime must prove every supported dialect surface.
Supported dialects:
1. SPARQL ASK
2. SPARQL SELECT
3. SPARQL CONSTRUCT
4. SHACL
5. N3
6. Datalog
7. ShEx

For each dialect, the following must exist: positive fixture, negative fixture, malformed fixture, sabotage fixture, hook-pack trigger example, command transcript, OCEL event evidence, RDF evidence graph projection, receipt-bound decision delta, replay verification, and typed unsupported/refusal behavior where applicable.

#### Required Fixtures:
Under `crates/ggen-graph/tests/fixtures/dialects/`:
- `sparql/`: `ask_pass.rq`, `ask_fail.rq`, `select_pass.rq`, `construct_pass.rq`, `malformed.rq`
- `shacl/`: `conforms.ttl`, `violates.ttl`, `malformed.ttl`
- `n3/`: `rule_pass.n3`, `rule_fail.n3`, `malformed.n3`
- `datalog/`: `rule_pass.dl`, `rule_fail.dl`, `malformed.dl`
- `shex/`: `conforms.shex`, `violates.shex`, `malformed.shex`

#### Required Tests:
- `crates/ggen-graph/tests/dialect_completeness.rs`
- `crates/ggen-graph/tests/dialect_sabotage.rs`
- `crates/ggen-graph/tests/dialect_receipts.rs`
- `crates/ggen-graph/tests/dialect_replay.rs`

Each dialect must prove: valid input passes, invalid input refuses, malformed input errors, sabotaged input fails expected gate, decision appears in public-vocabulary SHACL report, receipt binds inputs/outputs, and replay returns same result.

The hook pack must contain one W-check hook per dialect family (`W-DIALECT-SPARQL`, `W-DIALECT-SHACL`, `W-DIALECT-N3`, `W-DIALECT-DATALOG`, `W-DIALECT-SHEX`) represented as public `prov:Plan` / `sh:NodeShape` definitions.

## Rules & Clarifications

### Namespace Clarification
The string `gall` may appear in filenames, binary names, directory names, and document titles as project terminology.
- **Allowed**: `gall_observe_worktree.rs`, `gall-code-evaluation.ttl`, `gall_evidence.ttl`, `VISION_2030_GALL_PROOF.md`.
- **Forbidden**: `@prefix gall:`, `gall:CheckpointPromoted`, `gall:PromotionBlocked`, `gall:VerifiedState`, or any private predicate/class.
Project terminology may name files; public vocabularies must carry RDF meaning.

### Public Vocabulary Literal Clarification
Custom operational labels such as `"BoundaryExecutionRequest"`, `"CommandTranscript"`, `"DialectEvidence"`, or `"Knowledge Hook Actuation"` may appear only as literal values of public predicates such as `dcterms:type`, `dcterms:identifier`, `dcterms:title`, or `skos:prefLabel`.
They must not appear as RDF classes, RDF predicates, private-prefix terms, or project-private URI terms.
- **Allowed**: `dcterms:type "BoundaryExecutionRequest"`
- **Forbidden**: `gall:BoundaryExecutionRequest`, `<https://example.com/gall#BoundaryExecutionRequest>`, or `a <.../BoundaryExecutionRequest>`

### Dialect Execution Requirement
For each listed dialect, the runtime must prove one of two states:
1. **Executable Support**: positive fixture passes, negative fixture refuses, malformed fixture errors, sabotage fixture fails expected gate, transcript evidence exists, and receipt/replay exists.
2. **Bounded Unsupported Capability**: represented as a `sh:ValidationReport` where `sh:conforms false`, includes `sh:ValidationResult` explaining unsupported status using `sh:resultMessage`, and appears in OCEL, RDF evidence, decision delta, and receipt.
A dialect may not be listed as supported merely because fixtures or enum variants exist.

### Boundary Request Consumption Rule
A boundary adapter may consume a `BoundaryExecutionRequest` only once.
Each request must include:
- stable `dcterms:identifier`
- request hash
- hook-actuation receipt reference
- freshness timestamp
- capability label
- target command or observation category
- expected output artifact path

The adapter must refuse if the request was already consumed, lacks a receipt, was not materialized by hook actuation, contains private RDF terms, or maps to an unknown capability.

### Receipt Vocabulary Rule
`gall_code_evaluation.receipt.ttl` must use public vocabularies only. The receipt must be represented as a `prov:Entity` and/or `dcat:Dataset`.
Required linkage:
- `prov:wasGeneratedBy` for the hook actuation activity
- `prov:used` for evidence graph and hook pack inputs
- `prov:wasDerivedFrom` for command transcripts, docs tree, coverage matrix, and sabotage evidence
- `dcterms:identifier` for stable receipt identity
- `dcterms:created` for receipt time
- `dcterms:description` for human-readable summary
- Hash values represented as literals using public predicates only, preferably `spdx:checksum` / SPDX checksum structures when represented in RDF.

## Required Sabotage Cases
All sabotage must occur in temporary copied worktrees.
1. Add `[features]` to `crates/ggen-graph/Cargo.toml`.
2. Add `TODO` to production source.
3. Add `std::process::Command` to production library surface.
4. Tamper receipt hash.
5. Remove requirement link from coverage or OCEL evidence.
6. Delete a source file referenced by coverage.
7. Add both promoted and refused checkpoint evidence without supersession.
8. Replace command transcript with empty file.
9. Alter OCEL timestamp order so promotion precedes evaluation.
10. Mutate observer/witness binary or launcher to unconditional success.
11. Remove required test fixture.
12. Replace SPARQL query fixture with invalid syntax.

Each sabotage case must emit:
- mutation description
- temp worktree path
- expected failed check
- actual failed check
- transcript artifact
- public-vocabulary evidence
- SHACL validation result

## Acceptance Criteria

### Execution & Evidence Integrity
- [ ] Boundary execution is indirect: Hooks trigger boundary execution by emitting `prov:Activity` records labeled with `dcterms:type "BoundaryExecutionRequest"`; boundary adapters consume those records and emit transcript evidence back. No hook directly performs boundary work.
- [ ] SPARQL ASK / SELECT / CONSTRUCT are tested through positive, negative, malformed, sabotage, receipt, and replay paths.
- [ ] SHACL is tested through conforming, violating, malformed, sabotage, receipt, and replay paths.
- [ ] N3 is tested through pass, fail, malformed, sabotage, receipt, and replay paths.
- [ ] Datalog is tested through pass, fail, malformed, sabotage, receipt, and replay paths.
- [ ] ShEx is tested through conforming, violating, malformed, sabotage, receipt, and replay paths.
- [ ] No dialect is marked supported unless its evidence appears in the OCEL log, RDF evidence graph, decision delta, receipt, and replay result.
- [ ] If a dialect is intentionally not executable yet, it must be represented as a bounded unsupported capability with `sh:conforms false`, not silently omitted.
- [ ] No private namespace appears in hook packs, evidence graphs, decision deltas, final reports, docs tree TTL, or receipts.
- [ ] Promotion/refusal is represented using `sh:ValidationReport` and `sh:conforms`.
- [ ] Refusal reasons are represented as `sh:ValidationResult` objects.
- [ ] Hook execution is represented as `prov:Activity`.
- [ ] Evidence artifacts are represented as `prov:Entity` and/or `dcat:Dataset`.
- [ ] Any occurrence of `gall:`, `gg:`, `kh:`, `pcst:`, or project-private prefixes refuses promotion.
- [ ] No imperative scripts or witness binaries can directly declare final promotion; it must be actuated by Knowledge Hooks over the evidence graph.
- [ ] Every decision is emitted as an RDFDelta and bound by a cryptographic receipt.
- [ ] Same evidence + same hooks must replay to the same promotion/refusal.
- [ ] Full Worktree Coverage: Every source, test, example, schema, query, script, doc, and audit artifact is inventoried and verified.
- [ ] Sabotage Suite Completeness: All 12 negative-control sabotage cases must run, fail their expected gates, and restore correctly.
- [ ] Contradiction Supersession: Promoted/Refused conflicts default to Refused unless superseded by a resolved event.


## Follow-up — 2026-05-27T01:26:40Z

The user has ordered a Public Interop Purge:
1. Ban private RDF terms completely (e.g. gall:, gg:, kh:, pcst:, truex:, ggen:, doc:, doctest:).
2. Ban namespace laundering. Custom terms (such as BoundaryExecutionRequest or R7) must not be placed inside public namespaces like prov: (e.g. prov:BoundaryExecutionRequest is forbidden). Use literal values on public predicates instead (e.g. dcterms:type "BoundaryExecutionRequest").
3. Emit actual SHACL validation reports (*.validation.ttl) to the audit/ directory.
4. Replace custom document, status, and receipt terms with standard structures from PROV, DCAT, SHACL, and SPDX.

Please pivot the implementation to match the updated prompt draft:

# Teamwork Project Prompt — Draft

Implement the **Witnessed Agent Truthfulness GALL** protocol in the `ggen` repository using **Witnessed Code Evaluation / Knowledge Hook Actuation** over OCEL / PROV / RDF evidence using **public vocabularies only** (no private vocabulary namespaces, and no namespace laundering), with a fully proven **Dialect Completeness Matrix**, and an **OFMF-to-Public-Vocabulary hook schema mapping**.

Working directory: /Users/sac/ggen

## Requirements

### R1. Boundary Observation Binaries & Boundary Adapters
Implement Rust boundary observers and adapters under `crates/ggen-graph/src/bin/`:
- `gall_observe_worktree.rs`: Observe worktree inventory (paths, size, SHA256, BLAKE3).
- `gall_observe_commands.rs`: Observe commands, capture transcripts (stdout, stderr, argv, cwd, exit code, etc.).
- `gall_observe_sabotage.rs`: Observe negative-control sabotage runs.
- `gall_observe_clean_room.rs`: Observe clean-room build/test executions.
- `gall_observe_docs_tree.rs`: Observe documentation tree files.
- `gall_observe_doctests.rs`: Observe doctest coverage/verification.
- `gall_materialize_evidence_graph.rs`: Materialize individual observer outputs into a cohesive RDF/Turtle evidence graph using public vocabularies.
- `gall_actuate_code_evaluation.rs`: Actuate the evaluation runtime. Loads the Hook Pack and the evidence graph, runs the trigger query evaluation, and outputs the decision delta/receipt.
- **Boundary Adapters**: Separate binaries or adapters that watch for and consume `BoundaryExecutionRequest` activities. They validate that requests are valid, receipted, generated by hook actuation, use public vocabularies, and correspond to known capabilities. They execute the requested external action (e.g. running cargo tests or command runs) and emit the resulting command transcripts, PROV execution activities, and OCEL events back to the evidence graph.

*Rule: Observer binaries and boundary adapters must not decide final promotion. Hooks trigger boundary execution by emitting request facts; adapters materialize the boundary acts and emit evidence.*

### R2. Public-Vocabulary-Only Knowledge Hook Pack (No Namespace Laundering)
Create:
- `crates/ggen-graph/hooks/gall-code-evaluation.ttl`

The hook pack must use public vocabularies only.
Allowed prefixes: `rdf:`, `rdfs:`, `owl:`, `xsd:`, `prov:`, `dcat:`, `dcterms:`, `skos:`, `sh:`, `time:`, `spdx:`, `ocel:`.
Forbidden prefixes: `gall:`, `gg:`, `kh:`, `pcst:`, `truex:`, `ggen:`, `doc:`, `doctest:`, or any project-private namespaces.

*Namespace Laundering Ban*: Public namespaces may only contain terms actually defined by those vocabularies. Custom terms (such as `BoundaryExecutionRequest` or `w0_worktree`) must not be placed inside public namespaces (e.g. `prov:BoundaryExecutionRequest` is forbidden). Custom operational labels must only be literal values of public predicates (e.g., `dcterms:type "BoundaryExecutionRequest"`).

The hook pack expresses W0-W9 evaluation law by mapping the OFMF hook schema to public vocabularies:
- **OFMF HookPack**: Maps to `dcat:Dataset` + `prov:Entity`.
- **OFMF KnowledgeHook**: Maps to `prov:Plan` + `sh:NodeShape`.
- **OFMF HookTrigger**: Maps to `sh:SPARQLConstraint` / SHACL / N3 / Datalog / ShEx artifact.
- **OFMF HookAction**: Maps to SPARQL CONSTRUCT public-vocabulary delta.
- **OFMF DiagnosticEmitter**: Maps to `sh:ValidationReport` + `sh:ValidationResult`.
- **OFMF Receipt**: Maps to `prov:Entity` + `dcat:Dataset` + checksum evidence.
- **OFMF Runtime**: Maps to hook actuation over the evidence graph.

*Rule: Hooks must not directly execute boundary work (filesystem mutation, shell execution, etc.). They trigger boundary execution by emitting `BoundaryExecutionRequest` facts in the delta.*

### R3. Public-Vocabulary Decision Deltas, Receipts & Final Adjudication
Generate decision/audit artifacts in `crates/ggen-graph/audit/`:
- `gall_evidence.ttl` / `gall_evidence.ocel.json`
- `gall_decision.delta.ttl`
- `gall_code_evaluation.receipt.ttl`
- `gall_code_evaluation.final.ttl`
- **Durable SHACL reports**: `audit/public_vocab.validation.ttl`, `audit/hook_actuation.validation.ttl`, `audit/dialect_completeness.validation.ttl`, `audit/sabotage.validation.ttl`, and `audit/final.validation.ttl`.

The decision delta must not contain private terms. Instead, final status must be represented as public vocabulary facts:
- **Promotion**: `sh:ValidationReport` and `sh:conforms true` linked to execution via `prov:Activity` and `prov:wasGeneratedBy`.
- **Refusal**: `sh:ValidationReport` and `sh:conforms false` with one or more `sh:ValidationResult` objects with `sh:resultSeverity sh:Violation` and `sh:resultMessage`.

The final decision must include a receipt binding the evidence hash, hook pack hash, decision delta hash, output graph hash, timestamp, etc.

### R4. Critical Boundary / Authority Zones
- **Production library surface** (`crates/ggen-graph/src/lib.rs`, `crates/ggen-graph/src/{graph,delta,receipt,hooks,ocel,diagnostics}/**`) must forbid: `std::process::Command`, shell execution, filesystem deletion, network calls, and LLM calls.
- **Witness binary surface / Boundary Adapters** (`crates/ggen-graph/src/bin/gall_*.rs`) may use: `std::process::Command`, `tempfile`, `walkdir`, file hashing, command transcript capture, and temporary worktree mutation to fulfill authorized `BoundaryExecutionRequest` activities.
- **Knowledge Hooks** (`crates/ggen-graph/hooks/gall-code-evaluation.ttl`) evaluate the evidence graph to actuate decisions but must not mutate the filesystem directly or run commands.

### R5. Emitted Docs Tree Index
Emit `docs/docs.tree.json` and `docs/docs.tree.ttl` listing every doc under `docs/`, its checkpoint, source requirements, hash, and status. Must use public vocabularies. Custom doc structures are forbidden (use `prov:Entity`, `dcat:Distribution`, `dcterms:title`, etc. instead).

### R6. Doctest-Governed DX/QoL
All developer experience paths must be proven by Rust doctests (`cargo test -p ggen-graph --doc`).

### R7. Public Vocabulary Gate (No Laundering)
Implement an executable public-vocabulary-only gate in the hook-actuated evaluation flow.
The gate must scan:
- `crates/ggen-graph/hooks/**/*.ttl`
- `crates/ggen-graph/audit/**/*.ttl`
- `docs/**/*.ttl`
- `schema/**/*.ttl`

Refuse promotion if any RDF prefix, predicate, or class uses a project-private namespace, or if any public namespace prefix contains a term not defined in that standard vocabulary (e.g. `prov:R7` or `prov:BoundaryExecutionRequest`).
Allowed namespaces: RDF, RDFS, OWL, XSD, PROV-O, DCAT, DCTERMS, SKOS, SHACL, OWL-Time, SPDX, OCEL.
Emit refusal as:
- `sh:ValidationReport`
- `sh:conforms false`
- `sh:ValidationResult`
- `sh:resultSeverity sh:Violation`
- `sh:resultMessage` (explaining the private prefix or laundered term violation using public terms only)

### R8. Dialect Completeness Matrix
The Witnessed Code Evaluation runtime must prove every supported dialect surface.
Supported dialects:
1. SPARQL ASK
2. SPARQL SELECT
3. SPARQL CONSTRUCT
4. SHACL
5. N3
6. Datalog
7. ShEx

For each dialect, the following must exist: positive fixture, negative fixture, malformed fixture, sabotage fixture, hook-pack trigger example, command transcript, OCEL event evidence, RDF evidence graph projection, receipt-bound decision delta, replay verification, and typed unsupported/refusal behavior where applicable.

#### Required Fixtures:
Under `crates/ggen-graph/tests/fixtures/dialects/`:
- `sparql/`: `ask_pass.rq`, `ask_fail.rq`, `select_pass.rq`, `construct_pass.rq`, `malformed.rq`
- `shacl/`: `conforms.ttl`, `violates.ttl`, `malformed.ttl`
- `n3/`: `rule_pass.n3`, `rule_fail.n3`, `malformed.n3`
- `datalog/`: `rule_pass.dl`, `rule_fail.dl`, `malformed.dl`
- `shex/`: `conforms.shex`, `violates.shex`, `malformed.shex`

#### Required Tests:
- `crates/ggen-graph/tests/dialect_completeness.rs`
- `crates/ggen-graph/tests/dialect_sabotage.rs`
- `crates/ggen-graph/tests/dialect_receipts.rs`
- `crates/ggen-graph/tests/dialect_replay.rs`

Each dialect must prove: valid input passes, invalid input refuses, malformed input errors, sabotaged input fails expected gate, decision appears in public-vocabulary SHACL report, receipt binds inputs/outputs, and replay returns same result.

The hook pack must contain one W-check hook per dialect family (`W-DIALECT-SPARQL`, `W-DIALECT-SHACL`, `W-DIALECT-N3`, `W-DIALECT-DATALOG`, `W-DIALECT-SHEX`) represented as public `prov:Plan` / `sh:NodeShape` definitions.

## Rules & Clarifications

### Namespace Clarification
The string `gall` may appear in filenames, binary names, directory names, and document titles as project terminology.
- **Allowed**: `gall_observe_worktree.rs`, `gall-code-evaluation.ttl`, `gall_evidence.ttl`, `VISION_2030_GALL_PROOF.md`.
- **Forbidden**: `@prefix gall:`, `gall:CheckpointPromoted`, `gall:PromotionBlocked`, `gall:VerifiedState`, or any private predicate/class.
Project terminology may name files; public vocabularies must carry RDF meaning.

### Public Vocabulary Literal Clarification
Custom operational labels such as `"BoundaryExecutionRequest"`, `"CommandTranscript"`, `"DialectEvidence"`, or `"Knowledge Hook Actuation"` may appear only as literal values of public predicates such as `dcterms:type`, `dcterms:identifier`, `dcterms:title`, or `skos:prefLabel`.
They must not appear as RDF classes, RDF predicates, private-prefix terms, or project-private URI terms.
- **Allowed**: `dcterms:type "BoundaryExecutionRequest"`
- **Forbidden**: `gall:BoundaryExecutionRequest`, `<https://example.com/gall#BoundaryExecutionRequest>`, or `a <.../BoundaryExecutionRequest>`

### Dialect Execution Requirement
For each listed dialect, the runtime must prove one of two states:
1. **Executable Support**: positive fixture passes, negative fixture refuses, malformed fixture errors, sabotage fixture fails expected gate, transcript evidence exists, and receipt/replay exists.
2. **Bounded Unsupported Capability**: represented as a `sh:ValidationReport` where `sh:conforms false`, includes `sh:ValidationResult` explaining unsupported status using `sh:resultMessage`, and appears in OCEL, RDF evidence, decision delta, and receipt.
A dialect may not be listed as supported merely because fixtures or enum variants exist.

### Boundary Request Consumption Rule
A boundary adapter may consume a `BoundaryExecutionRequest` only once.
Each request must include:
- stable `dcterms:identifier`
- request hash
- hook-actuation receipt reference
- freshness timestamp
- capability label
- target command or observation category
- expected output artifact path

The adapter must refuse if the request was already consumed, lacks a receipt, was not materialized by hook actuation, contains private RDF terms, or maps to an unknown capability.

### Receipt Vocabulary Rule
`gall_code_evaluation.receipt.ttl` must use public vocabularies only. The receipt must be represented as a `prov:Entity` and/or `dcat:Dataset`.
Required linkage:
- `prov:wasGeneratedBy` for the hook actuation activity
- `prov:used` for evidence graph and hook pack inputs
- `prov:wasDerivedFrom` for command transcripts, docs tree, coverage matrix, and sabotage evidence
- `dcterms:identifier` for stable receipt identity
- `dcterms:created` for receipt time
- `dcterms:description` for human-readable summary
- Hash values represented as literals using public predicates only, preferably `spdx:checksum` / SPDX checksum structures when represented in RDF.

## Required Sabotage Cases
All sabotage must occur in temporary copied worktrees.
1. Add `[features]` to `crates/ggen-graph/Cargo.toml`.
2. Add `TODO` to production source.
3. Add `std::process::Command` to production library surface.
4. Tamper receipt hash.
5. Remove requirement link from coverage or OCEL evidence.
6. Delete a source file referenced by coverage.
7. Add both promoted and refused checkpoint evidence without supersession.
8. Replace command transcript with empty file.
9. Alter OCEL timestamp order so promotion precedes evaluation.
10. Mutate observer/witness binary or launcher to unconditional success.
11. Remove required test fixture.
12. Replace SPARQL query fixture with invalid syntax.

Each sabotage case must emit:
- mutation description
- temp worktree path
- expected failed check
- actual failed check
- transcript artifact
- public-vocabulary evidence
- SHACL validation result

## Acceptance Criteria

### Execution & Evidence Integrity
- [ ] No project-private RDF terms, and no namespace laundering (putting custom terms into public namespaces like `prov:BoundaryExecutionRequest` or `prov:R7`).
- [ ] Durable SHACL reports must be written to disk under the `audit/` directory.
- [ ] Boundary execution is indirect: Hooks trigger boundary execution by emitting `prov:Activity` records labeled with `dcterms:type "BoundaryExecutionRequest"`; boundary adapters consume those records and emit transcript evidence back. No hook directly performs boundary work.
- [ ] SPARQL ASK / SELECT / CONSTRUCT are tested through positive, negative, malformed, sabotage, receipt, and replay paths.
- [ ] SHACL is tested through conforming, violating, malformed, sabotage, receipt, and replay paths.
- [ ] N3 is tested through pass, fail, malformed, sabotage, receipt, and replay paths.
- [ ] Datalog is tested through pass, fail, malformed, sabotage, receipt, and replay paths.
- [ ] ShEx is tested through conforming, violating, malformed, sabotage, receipt, and replay paths.
- [ ] No dialect is marked supported unless its evidence appears in the OCEL log, RDF evidence graph, decision delta, receipt, and replay result.
- [ ] If a dialect is intentionally not executable yet, it must be represented as a bounded unsupported capability with `sh:conforms false`, not silently omitted.
- [ ] No private namespace appears in hook packs, evidence graphs, decision deltas, final reports, docs tree TTL, or receipts.
- [ ] Promotion/refusal is represented using `sh:ValidationReport` and `sh:conforms`.
- [ ] Refusal reasons are represented as `sh:ValidationResult` objects.
- [ ] Hook execution is represented as `prov:Activity`.
- [ ] Evidence artifacts are represented as `prov:Entity` and/or `dcat:Dataset`.
- [ ] Any occurrence of `gall:`, `gg:`, `kh:`, `pcst:`, or project-private prefixes refuses promotion.
- [ ] No imperative scripts or witness binaries can directly declare final promotion; it must be actuated by Knowledge Hooks over the evidence graph.
- [ ] Every decision is emitted as an RDFDelta and bound by a cryptographic receipt.
- [ ] Same evidence + same hooks must replay to the same promotion/refusal.
- [ ] Full Worktree Coverage: Every source, test, example, schema, query, script, doc, and audit artifact is inventoried and verified.
- [ ] Sabotage Suite Completeness: All 12 negative-control sabotage cases must run, fail their expected gates, and restore correctly.
- [ ] Contradiction Supersession: Promoted/Refused conflicts default to Refused unless superseded by a resolved event.


## Follow-up — 2026-05-27T01:30:12Z

The user has introduced the "Agent K — Open Ontologies External Oracle" requirement:
1. Ensure all Turtle artifacts (.ttl files under hooks/, audit/, docs/, and schema/) can be parsed, loaded, and linted successfully by an independent RDF/OWL/SHACL validation toolchain (e.g. Oxigraph or Open Ontologies).
2. Fix all syntax errors (such as unescaped spaces in file URIs in audit/worktree_inventory.ttl).
3. Validate that standard RDF canonicalization (like URDNA2015) is used for receipt replay.
4. Verify the entire artifact set through Open Ontologies before claiming completion.

Please pivot the implementation to match the updated prompt draft:

# Teamwork Project Prompt — Draft

Implement the **Witnessed Agent Truthfulness GALL** protocol in the `ggen` repository using **Witnessed Code Evaluation / Knowledge Hook Actuation** over OCEL / PROV / RDF evidence using **public vocabularies only** (no private vocabulary namespaces, and no namespace laundering), with a fully proven **Dialect Completeness Matrix**, an **OFMF-to-Public-Vocabulary hook schema mapping**, and an independent **Agent K — Open Ontologies External Oracle** validator.

Working directory: /Users/sac/ggen

## Requirements

### R1. Boundary Observation Binaries & Boundary Adapters
Implement Rust boundary observers and adapters under `crates/ggen-graph/src/bin/`:
- `gall_observe_worktree.rs`: Observe worktree inventory (paths, size, SHA256, BLAKE3).
- `gall_observe_commands.rs`: Observe commands, capture transcripts (stdout, stderr, argv, cwd, exit code, etc.).
- `gall_observe_sabotage.rs`: Observe negative-control sabotage runs.
- `gall_observe_clean_room.rs`: Observe clean-room build/test executions.
- `gall_observe_docs_tree.rs`: Observe documentation tree files.
- `gall_observe_doctests.rs`: Observe doctest coverage/verification.
- `gall_materialize_evidence_graph.rs`: Materialize individual observer outputs into a cohesive RDF/Turtle evidence graph using public vocabularies.
- `gall_actuate_code_evaluation.rs`: Actuate the evaluation runtime. Loads the Hook Pack and the evidence graph, runs the trigger query evaluation, and outputs the decision delta/receipt.
- **Boundary Adapters**: Separate binaries or adapters that watch for and consume `BoundaryExecutionRequest` activities. They validate that requests are valid, receipted, generated by hook actuation, use public vocabularies, and correspond to known capabilities. They execute the requested external action (e.g. running cargo tests or command runs) and emit the resulting command transcripts, PROV execution activities, and OCEL events back to the evidence graph.

*Rule: Observer binaries and boundary adapters must not decide final promotion. Hooks trigger boundary execution by emitting request facts; adapters materialize the boundary acts and emit evidence.*

### R2. Public-Vocabulary-Only Knowledge Hook Pack (No Namespace Laundering)
Create:
- `crates/ggen-graph/hooks/gall-code-evaluation.ttl`

The hook pack must use public vocabularies only.
Allowed prefixes: `rdf:`, `rdfs:`, `owl:`, `xsd:`, `prov:`, `dcat:`, `dcterms:`, `skos:`, `sh:`, `time:`, `spdx:`, `ocel:`.
Forbidden prefixes: `gall:`, `gg:`, `kh:`, `pcst:`, `truex:`, `ggen:`, `doc:`, `doctest:`, or any project-private namespaces.

*Namespace Laundering Ban*: Public namespaces may only contain terms actually defined by those vocabularies. Custom terms (such as `BoundaryExecutionRequest` or `w0_worktree`) must not be placed inside public namespaces (e.g. `prov:BoundaryExecutionRequest` is forbidden). Custom operational labels must only be literal values of public predicates (e.g., `dcterms:type "BoundaryExecutionRequest"`).

The hook pack expresses W0-W9 evaluation law by mapping the OFMF hook schema to public vocabularies:
- **OFMF HookPack**: Maps to `dcat:Dataset` + `prov:Entity`.
- **OFMF KnowledgeHook**: Maps to `prov:Plan` + `sh:NodeShape`.
- **OFMF HookTrigger**: Maps to `sh:SPARQLConstraint` / SHACL / N3 / Datalog / ShEx artifact.
- **OFMF HookAction**: Maps to SPARQL CONSTRUCT public-vocabulary delta.
- **OFMF DiagnosticEmitter**: Maps to `sh:ValidationReport` + `sh:ValidationResult`.
- **OFMF Receipt**: Maps to `prov:Entity` + `dcat:Dataset` + checksum evidence.
- **OFMF Runtime**: Maps to hook actuation over the evidence graph.

*Rule: Hooks must not directly execute boundary work (filesystem mutation, shell execution, etc.). They trigger boundary execution by emitting `BoundaryExecutionRequest` facts in the delta.*

### R3. Public-Vocabulary Decision Deltas, Receipts & Final Adjudication
Generate decision/audit artifacts in `crates/ggen-graph/audit/`:
- `gall_evidence.ttl` / `gall_evidence.ocel.json`
- `gall_decision.delta.ttl`
- `gall_code_evaluation.receipt.ttl`
- `gall_code_evaluation.final.ttl`
- **Durable SHACL reports**: `audit/public_vocab.validation.ttl`, `audit/hook_actuation.validation.ttl`, `audit/dialect_completeness.validation.ttl`, `audit/sabotage.validation.ttl`, and `audit/final.validation.ttl`.

The decision delta must not contain private terms. Instead, final status must be represented as public vocabulary facts:
- **Promotion**: `sh:ValidationReport` and `sh:conforms true` linked to execution via `prov:Activity` and `prov:wasGeneratedBy`.
- **Refusal**: `sh:ValidationReport` and `sh:conforms false` with one or more `sh:ValidationResult` objects with `sh:resultSeverity sh:Violation` and `sh:resultMessage`.

The final decision must include a receipt binding the evidence hash, hook pack hash, decision delta hash, output graph hash, timestamp, etc.

### R4. Critical Boundary / Authority Zones
- **Production library surface** (`crates/ggen-graph/src/lib.rs`, `crates/ggen-graph/src/{graph,delta,receipt,hooks,ocel,diagnostics}/**`) must forbid: `std::process::Command`, shell execution, filesystem deletion, network calls, and LLM calls.
- **Witness binary surface / Boundary Adapters** (`crates/ggen-graph/src/bin/gall_*.rs`) may use: `std::process::Command`, `tempfile`, `walkdir`, file hashing, command transcript capture, and temporary worktree mutation to fulfill authorized `BoundaryExecutionRequest` activities.
- **Knowledge Hooks** (`crates/ggen-graph/hooks/gall-code-evaluation.ttl`) evaluate the evidence graph to actuate decisions but must not mutate the filesystem directly or run commands.

### R5. Emitted Docs Tree Index
Emit `docs/docs.tree.json` and `docs/docs.tree.ttl` listing every doc under `docs/`, its checkpoint, source requirements, hash, and status. Must use public vocabularies. Custom doc structures are forbidden (use `prov:Entity`, `dcat:Distribution`, `dcterms:title`, etc. instead).

### R6. Doctest-Governed DX/QoL
All developer experience paths must be proven by Rust doctests (`cargo test -p ggen-graph --doc`).

### R7. Public Vocabulary Gate (No Laundering)
Implement an executable public-vocabulary-only gate in the hook-actuated evaluation flow.
The gate must scan:
- `crates/ggen-graph/hooks/**/*.ttl`
- `crates/ggen-graph/audit/**/*.ttl`
- `docs/**/*.ttl`
- `schema/**/*.ttl`

Refuse promotion if any RDF prefix, predicate, or class uses a project-private namespace, or if any public namespace prefix contains a term not defined in that standard vocabulary (e.g. `prov:R7` or `prov:BoundaryExecutionRequest`).
Allowed namespaces: RDF, RDFS, OWL, XSD, PROV-O, DCAT, DCTERMS, SKOS, SHACL, OWL-Time, SPDX, OCEL.
Emit refusal as:
- `sh:ValidationReport`
- `sh:conforms false`
- `sh:ValidationResult`
- `sh:resultSeverity sh:Violation`
- `sh:resultMessage` (explaining the private prefix or laundered term violation using public terms only)

### R8. Dialect Completeness Matrix
The Witnessed Code Evaluation runtime must prove every supported dialect surface.
Supported dialects:
1. SPARQL ASK
2. SPARQL SELECT
3. SPARQL CONSTRUCT
4. SHACL
5. N3
6. Datalog
7. ShEx

For each dialect, the following must exist: positive fixture, negative fixture, malformed fixture, sabotage fixture, hook-pack trigger example, command transcript, OCEL event evidence, RDF evidence graph projection, receipt-bound decision delta, replay verification, and typed unsupported/refusal behavior where applicable.

#### Required Fixtures:
Under `crates/ggen-graph/tests/fixtures/dialects/`:
- `sparql/`: `ask_pass.rq`, `ask_fail.rq`, `select_pass.rq`, `construct_pass.rq`, `malformed.rq`
- `shacl/`: `conforms.ttl`, `violates.ttl`, `malformed.ttl`
- `n3/`: `rule_pass.n3`, `rule_fail.n3`, `malformed.n3`
- `datalog/`: `rule_pass.dl`, `rule_fail.dl`, `malformed.dl`
- `shex/`: `conforms.shex`, `violates.shex`, `malformed.shex`

#### Required Tests:
- `crates/ggen-graph/tests/dialect_completeness.rs`
- `crates/ggen-graph/tests/dialect_sabotage.rs`
- `crates/ggen-graph/tests/dialect_receipts.rs`
- `crates/ggen-graph/tests/dialect_replay.rs`

Each dialect must prove: valid input passes, invalid input refuses, malformed input errors, sabotaged input fails expected gate, decision appears in public-vocabulary SHACL report, receipt binds inputs/outputs, and replay returns same result.

The hook pack must contain one W-check hook per dialect family (`W-DIALECT-SPARQL`, `W-DIALECT-SHACL`, `W-DIALECT-N3`, `W-DIALECT-DATALOG`, `W-DIALECT-SHEX`) represented as public `prov:Plan` / `sh:NodeShape` definitions.

### R9. Agent K — Open Ontologies External Oracle
Use Open Ontologies as an independent public-vocabulary evidence consumer.
Validate that all relevant artifacts can be loaded, queried, linted, and checked by an external RDF/OWL/SHACL toolchain:
- `crates/ggen-graph/hooks/**/*.ttl`
- `crates/ggen-graph/audit/**/*.ttl`
- `docs/**/*.ttl`
- `schema/**/*.ttl`

Required checks:
1. All Turtle parses successfully (e.g. no unescaped spaces in file URIs).
2. No private RDF namespace appears.
3. No local project term is laundered into a public namespace.
4. SHACL decision reports are durable on disk.
5. PROV chains link activities, evidence, decisions, and receipts.
6. DCAT identifies datasets/evidence bundles.
7. Receipts are externally reproducible (e.g. using URDNA2015 canonicalization or standardized RDF hashing instead of home-grown string sorts).
8. Sabotage evidence is externally inspectable.

If Open Ontologies cannot ingest and query the evidence surface, external interoperability is failed.

## Rules & Clarifications

### Namespace Clarification
The string `gall` may appear in filenames, binary names, directory names, and document titles as project terminology.
- **Allowed**: `gall_observe_worktree.rs`, `gall-code-evaluation.ttl`, `gall_evidence.ttl`, `VISION_2030_GALL_PROOF.md`.
- **Forbidden**: `@prefix gall:`, `gall:CheckpointPromoted`, `gall:PromotionBlocked`, `gall:VerifiedState`, or any private predicate/class.
Project terminology may name files; public vocabularies must carry RDF meaning.

### Public Vocabulary Literal Clarification
Custom operational labels such as `"BoundaryExecutionRequest"`, `"CommandTranscript"`, `"DialectEvidence"`, or `"Knowledge Hook Actuation"` may appear only as literal values of public predicates such as `dcterms:type`, `dcterms:identifier`, `dcterms:title`, or `skos:prefLabel`.
They must not appear as RDF classes, RDF predicates, private-prefix terms, or project-private URI terms.
- **Allowed**: `dcterms:type "BoundaryExecutionRequest"`
- **Forbidden**: `gall:BoundaryExecutionRequest`, `<https://example.com/gall#BoundaryExecutionRequest>`, or `a <.../BoundaryExecutionRequest>`

### Dialect Execution Requirement
For each listed dialect, the runtime must prove one of two states:
1. **Executable Support**: positive fixture passes, negative fixture refuses, malformed fixture errors, sabotage fixture fails expected gate, transcript evidence exists, and receipt/replay exists.
2. **Bounded Unsupported Capability**: represented as a `sh:ValidationReport` where `sh:conforms false`, includes `sh:ValidationResult` explaining unsupported status using `sh:resultMessage`, and appears in OCEL, RDF evidence, decision delta, and receipt.
A dialect may not be listed as supported merely because fixtures or enum variants exist.

### Boundary Request Consumption Rule
A boundary adapter may consume a `BoundaryExecutionRequest` only once.
Each request must include:
- stable `dcterms:identifier`
- request hash
- hook-actuation receipt reference
- freshness timestamp
- capability label
- target command or observation category
- expected output artifact path

The adapter must refuse if the request was already consumed, lacks a receipt, was not materialized by hook actuation, contains private RDF terms, or maps to an unknown capability.

### Receipt Vocabulary Rule
`gall_code_evaluation.receipt.ttl` must use public vocabularies only. The receipt must be represented as a `prov:Entity` and/or `dcat:Dataset`.
Required linkage:
- `prov:wasGeneratedBy` for the hook actuation activity
- `prov:used` for evidence graph and hook pack inputs
- `prov:wasDerivedFrom` for command transcripts, docs tree, coverage matrix, and sabotage evidence
- `dcterms:identifier` for stable receipt identity
- `dcterms:created` for receipt time
- `dcterms:description` for human-readable summary
- Hash values represented as literals using public predicates only, preferably `spdx:checksum` / SPDX checksum structures when represented in RDF.

## Required Sabotage Cases
All sabotage must occur in temporary copied worktrees.
1. Add `[features]` to `crates/ggen-graph/Cargo.toml`.
2. Add `TODO` to production source.
3. Add `std::process::Command` to production library surface.
4. Tamper receipt hash.
5. Remove requirement link from coverage or OCEL evidence.
6. Delete a source file referenced by coverage.
7. Add both promoted and refused checkpoint evidence without supersession.
8. Replace command transcript with empty file.
9. Alter OCEL timestamp order so promotion precedes evaluation.
10. Mutate observer/witness binary or launcher to unconditional success.
11. Remove required test fixture.
12. Replace SPARQL query fixture with invalid syntax.

Each sabotage case must emit:
- mutation description
- temp worktree path
- expected failed check
- actual failed check
- transcript artifact
- public-vocabulary evidence
- SHACL validation result

## Acceptance Criteria

### Execution & Evidence Integrity
- [ ] Open Ontologies validation check: All RDF/Turtle files load, query, and lint successfully with no syntax-level failures, namespace laundering, or unescaped spaces.
- [ ] No project-private RDF terms, and no namespace laundering (putting custom terms into public namespaces like `prov:BoundaryExecutionRequest` or `prov:R7`).
- [ ] Durable SHACL reports must be written to disk under the `audit/` directory.
- [ ] Boundary execution is indirect: Hooks trigger boundary execution by emitting `prov:Activity` records labeled with `dcterms:type "BoundaryExecutionRequest"`; boundary adapters consume those records and emit transcript evidence back. No hook directly performs boundary work.
- [ ] SPARQL ASK / SELECT / CONSTRUCT are tested through positive, negative, malformed, sabotage, receipt, and replay paths.
- [ ] SHACL is tested through conforming, violating, malformed, sabotage, receipt, and replay paths.
- [ ] N3 is tested through pass, fail, malformed, sabotage, receipt, and replay paths.
- [ ] Datalog is tested through pass, fail, malformed, sabotage, receipt, and replay paths.
- [ ] ShEx is tested through conforming, violating, malformed, sabotage, receipt, and replay paths.
- [ ] No dialect is marked supported unless its evidence appears in the OCEL log, RDF evidence graph, decision delta, receipt, and replay result.
- [ ] If a dialect is intentionally not executable yet, it must be represented as a bounded unsupported capability with `sh:conforms false`, not silently omitted.
- [ ] No private namespace appears in hook packs, evidence graphs, decision deltas, final reports, docs tree TTL, or receipts.
- [ ] Promotion/refusal is represented using `sh:ValidationReport` and `sh:conforms`.
- [ ] Refusal reasons are represented as `sh:ValidationResult` objects.
- [ ] Hook execution is represented as `prov:Activity`.
- [ ] Evidence artifacts are represented as `prov:Entity` and/or `dcat:Dataset`.
- [ ] Any occurrence of `gall:`, `gg:`, `kh:`, `pcst:`, or project-private prefixes refuses promotion.
- [ ] No imperative scripts or witness binaries can directly declare final promotion; it must be actuated by Knowledge Hooks over the evidence graph.
- [ ] Every decision is emitted as an RDFDelta and bound by a cryptographic receipt.
- [ ] Same evidence + same hooks must replay to the same promotion/refusal.
- [ ] Full Worktree Coverage: Every source, test, example, schema, query, script, doc, and audit artifact is inventoried and verified.
- [ ] Sabotage Suite Completeness: All 12 negative-control sabotage cases must run, fail their expected gates, and restore correctly.
- [ ] Contradiction Supersession: Promoted/Refused conflicts default to Refused unless superseded by a resolved event.


## Follow-up — 2026-05-27T15:43:25Z

# Teamwork Project Prompt

Audit, find, and fill any remaining gaps in the `ggen` codebase under the Reimagined Vision 2030 interchangeable parts specification. 

Working directory: /Users/sac/ggen
Integrity mode: development

## Requirements

### R1. Reimagined Vision 2030 Architecture
Ensure that the codebase adheres to the new model:
- **ggen** manufactures Genesis-bearing interchangeable parts and provides the membrane, packaging, adapter surface, and projection layer.
- **Genesis** acts as the lawful construction kernel embedded inside every interchangeable part (owning O*, μ, RelationPage, Pair2, Construct8, Receipt, Replay, and Refusal).
- **Interchangeable parts** are runnable across AtomVM, WASM, Rust native, browsers, and edge environments.

### R2. Complete Test Coverage & Execution
Verify all in-process integration tests (`otel_validation_tests` and `ggen-graph` suite) execute successfully under strict rules, with zero stubs or placeholders.

### R3. Adjudication & Integrity Validation
Ensure that all verifier gates (W0-W9/T0-T10) pass and produce a verified, promoted `witnessed_truthfulness.external_adjudication.json` receipt via the external witness scripts.

## Acceptance Criteria

### Test Integrity
- [ ] `cargo test --all` executes successfully.
- [ ] No stubs or london TDD mocks exist in the codebase.

### Adjudication Validation
- [ ] The external witness adjudication script (`99_adjudicate_witnessed_truthfulness.sh`) executes and produces a promoted verdict.

---
## Vision 2030 Context Specification

### The new doctrine
- **Genesis is the lawful construction kernel inside every interchangeable part.**
- **ggen is the foundry, membrane, packaging system, adapter surface, and projection layer that manufactures and surrounds those parts.**

### Layer Mapping
- **Outside world**: devices, actors, files, APIs, logs, tools, databases, humans, workflows
- **ggen membrane**: adapts messy reality into bounded operating surfaces
- **AtomVM / Erlang shell**: actor custody, mailbox discipline, supervision, edge event ownership
- **WASM shell**: portable execution body for interchangeable parts
- **Genesis core inside the part**: O*, μ, Pair2, RelationPage, Construct8, receipt, replay, refusal
- **ggen projection membrane**: expands receipts and relation matter into OCEL, RDF, PROV, DCAT, SQL, reports
- **Downstream systems**: QLever, DuckDB, lakehouse, audit systems, wasm4pm, dashboards, AI context

## Follow-up — 2026-05-27T15:46:25Z

Here is the approved DFLSS Project Charter for the Genesis-Bearing Interchangeable Parts, Vision 2030 project:

# DFLSS Project Charter

## Genesis-Bearing Interchangeable Parts, Vision 2030

### 1. Project Name
**Genesis-Bearing Interchangeable Parts**

### 2. Charter Statement
Design and validate a production-grade architecture where ggen manufactures interchangeable operating parts that carry Genesis inside them.
Each part must be able to operate at the riverhead of enterprise motion: edge, IoT, CI runner, factory cell, clinic station, document processor, lakehouse listener, or browser/edge worker.
Each part locally performs:
- Runtime custody: AtomVM / Erlang shell
- Portable execution: WASM / Rust body
- External contact: ggen membrane
- Lawful construction: Genesis core
- Evidence rollup: receipts, replay, refusal
- External projection: ggen projection layer

The design objective is to manufacture replaceable operating parts that locally construct receipted relation matter before downstream systems consume it.

### 3. Business Case (Blue River Dam)
Value is captured at the first lawful construction of operational consequence (riverhead), preventing context decay before data reaches the lakehouse.

### 4. Scope
- **In Scope (Genesis Core)**: O*, μ, Pair2, RelationPage, Construct8, receipts, replay, refusal. Pure-core kernel with zero-overhead layout and no external dependency sprawl.
- **In Scope (ggen)**: Membrane, adapters, symbol-page builders, context binders, projections (OCEL, RDF, PROV, DCAT, SQL, reports), validation (QLever, DuckDB, SHACL, OCEL).
- **In Scope (AtomVM/WASM)**: Edge custody shell, actor identity, restart evidence, portable body.

### 5. Primary CTQs & Metrics
- Active Pair2 tuples per Construct8 packet: <= 8
- Bytes per hot Pair2 tuple: 2
- Local page domains: <= 256 left and <= 256 right active symbols.
- No page/packet overflow accepted.
- Refusal artifact coverage: 100% for invalid construction cases.
- ggen/Genesis boundary violations: 0
- Pure-core external dependency violations: 0

Please incorporate this DFLSS charter into the current gap analysis and resolution phase.

## Follow-up — 2026-05-27T16:33:53Z

Please apply the following Rust Core Team Best Practices to the active implementation and verification of the Vision 2030 interchangeable parts modules:

1. **Memory-Safety & Idiomatic Design**: Use pure safe Rust. Follow idiomatic design patterns (e.g., using TryFrom/From for node representations, clean module structure).
2. **Robust Error Handling**: Avoid `unwrap()` or `panic!` inside library code (`crates/ggen-core/src/`). Propagate errors cleanly using the crate's custom Result/Error/bail/ensure patterns.
3. **Clippy & Warn-Free Execution**: Ensure all codebase modifications pass `#![deny(warnings)]` and `cargo clippy` cleanly.
4. **Performance & Heap-Free**: Maintain heap-free zero-overhead layout for the hot execution path in the Genesis core.

Make sure the active workers align their current code fixes and test structures with these principles.


## Follow-up — 2026-05-27T16:47:26Z

Here is the Swarm Operating System Prompt Pack for documenting all interop contracts, specifications, and creating the finish plan. Please transition to this phase:

# Swarm Operating System Prompt Pack

## Mission
Document the full Genesis-bearing interchangeable parts architecture for interop and finish.
Create the directory `docs/interop/` and produce the following files:
1. `00_INDEX.md` (Main index linking every report, executive summary, finish plan, top risks, final architecture status judgment)
2. `01_PORTFOLIO_MAP.md` (Portfolio classification table)
3. `02_BOUNDARY_DOCTRINE.md` (Expose boundaries between Core, Membrane, Body, and Projections)
4. `03_INTEROP_CONTRACTS.md` (Boundary interfaces table mapping Inputs, Outputs, Proof, Replay, Refusal, and Validators)
5. `04_GENESIS_CORE_SPEC.md` (Primitives, Page split laws, set/bag/stream multiplicity law, context authority)
6. `05_GGEN_FOUNDRY_SPEC.md` (ggen foundry, membrane adapters, projection output formats)
7. `06_PART_RUNTIME_SPEC.md` (AtomVM custody, WASM portability, Rust physical discipline, part lifecycle states)
8. `07_PROOF_SURFACES_SPEC.md` (Receipt types, Replay log, Refusal cases: Need9/257, invalid context, sabotage gates)
9. `09_DATA_ALGEBRA_GALL.md` (GALL data algebra checklist, relation page bounds, join correctness check)
10. `09_EXTERNAL_VALIDATION_SPEC.md` (DuckDB, QLever, SHACL, OCEL validation bridge mapping)
11. `10_PUBLIC_VOCABULARY_GALL.md` (Open Ontologies survivability checkpoint mapping)
12. `13_DEFINITION_OF_DONE.md` (v0.1, v0.2, and Vision 2030 standards)
13. `14_AGENT_WORK_QUEUE.md` (Work packets with owners, inputs, outputs, tests, and risks)
14. `15_FINISH_PLAN.md` (Finish backlog, dependency graph, observed-vs-planned matrix)

## Swarm Contract Constraints
- Do not put outside-world dependencies inside Genesis.
- Pair2 is left byte + right byte under a predicate-fixed RelationPage context (not compressed RDF).
- Every claim must be tagged with status (IMPLEMENTED, PARTIAL, MISSING, etc.) and backed by file evidence in the repo.

Align the Project Orchestrator and all subagent workers to execute this specification.

## Follow-up — 2026-05-27T19:32:55Z

# Teamwork Project Prompt — Draft

> Status: Launched
> Goal: Craft prompt → get user approval → delegate to teamwork_preview

Build and finish `capability-map` (`cpmp`) in `/Users/sac/capability-map` leveraging `open-ontologies` as the primary catalog store, ensuring all code, capabilities, patterns, tests, docs, and symbols are discoverable by LLM coding agents.

Working directory: `/Users/sac/capability-map`
Integrity mode: development

## Requirements

### R1. Non-Destructive Scanner & Receipt Generation
Scan filesystem paths read-only, respect ignore files, guess languages, compute cryptographic file hashes (BLAKE3), and inventory codebases. Build scan receipts (`scan-<timestamp>.receipt.toml`) containing the file lists, hashes, and sizes. Implement `verify-no-deletion` to detect changes without modifying target repositories.

### R2. RDF Graph Generation and Public Vocabulary
Generate Turtle (`cpmp-catalog.ttl`) and N-Quads (`cpmp-catalog.nq`) project graphs under `~/.cpmp/catalog/` using public vocabulary terms:
- `prov:Activity` for the scan runs and file generation lineage
- `dcat:Catalog` and `dcat:Distribution` for the computer catalog and distribution formats
- `doap:Project` and `doap:Repository` for project mappings
- `spdx:File`, `spdx:Package` and SPDX checksums for file/checksum entities
- `skos:Concept` for capabilities and taxonomy classifications
- `sh:NodeShape` for SHACL shapes (`cpmp-shapes.ttl`)

### R3. Open Ontologies Integration & Validation
Leverage `open-ontologies` CLI or MCP tools (`onto_validate`, `onto_load`, `onto_shacl`, `onto_version`, etc.) to:
1. Validate generated Turtle graphs
2. Load graphs into Open Ontologies store
3. Run SHACL verification
4. Version the catalog state (`v-<receipt-id>`)
SQLite is allowed ONLY as an implementation cache or report acceleration layer, not as the primary source store.

### R4. SPARQL Reports
Emit markdown reports (`CAPABILITY_INVENTORY.md`, `PROJECT_ATLAS.md`, `PATTERN_ATLAS.md`) generated from querying Open Ontologies using SPARQL or local memory graphs.

## Acceptance Criteria

### Execution & Verification
- [ ] Programmatic scan compiles successfully and runs on a fixture repository.
- [ ] Scanning generates the required RDF files in `~/.cpmp/catalog/` (or the custom output path).
- [ ] `cpmp-catalog.ttl` is validated successfully using `open-ontologies validate` or equivalent local validation.
- [ ] SHACL shapes are verified using `open-ontologies shacl` and pass successfully.
- [ ] Scan receipts and reports are generated correctly under `receipts/` and `reports/` respectively.
- [ ] `verify-no-deletion` subcommand correctly reports any deletion, modification, or additions between receipts.
- [ ] The entire scan operation is completely read-only on the target repository directories.

## Follow-up — 2026-05-27T19:33:40Z

The user has supplied an update and correction to the architecture. We are extending CPMP and ggen to include the Enterprise Wrapper Architecture:

# CPMP + ggen + Open Ontologies Enterprise Wrapper Architecture

Please incorporate these requirements into your implementation and documentation goals:

1. Architecture:
   - cpmp: discovers projects, files, capabilities, tests, docs, runtimes.
   - ggen enterprise membrane: normalizes, projects, validates, receipts, redacts, packages.
   - Open Ontologies: RDF/OWL store, SPARQL, SHACL, lint, reason, diff, version, lineage.
   - Enterprise Control Plane (Wrapper): auth, tenancy, policy, approvals, audit, retention, backups, exports.

2. Create/Write the required enterprise documentation in `docs/enterprise/`:
   - docs/enterprise/ARCHITECTURE.md
   - docs/enterprise/CONTROL_PLANE.md
   - docs/enterprise/TENANCY.md
   - docs/enterprise/AUTHZ.md
   - docs/enterprise/POLICY_PACKS.md
   - docs/enterprise/AUDIT_AND_LINEAGE.md
   - docs/enterprise/RETENTION_AND_BACKUP.md
   - docs/enterprise/OPEN_ONTOLOGIES_ADAPTER.md
   - docs/enterprise/GGEN_PROJECTION_MEMBRANE.md
   - docs/enterprise/PUBLIC_VOCABULARY_FIREWALL.md
   - docs/enterprise/GAP_CLOSURE_MATRIX.md (Be sure to include all specified gaps: invalid Turtle, illegal URI syntax, namespace laundering, private predicate authority, missing SHACL reports, missing PROV lineage, missing checksum evidence, missing canonical graph hash, JSON-only evidence, stale report emission, unversioned graph mutation, unscoped tenant access, missing no-deletion check, missing backup/export path, missing policy refusal artifact).
   - docs/enterprise/ENTERPRISE_DEFINITION_OF_DONE.md

3. Implement or stub the CLI nouns and commands:
   - cpmp computer discover
   - cpmp graph project
   - cpmp graph validate
   - cpmp graph load
   - cpmp graph query
   - cpmp graph version
   - cpmp graph drift
   - cpmp policy check
   - cpmp policy enforce
   - cpmp tenant create
   - cpmp tenant list
   - cpmp audit lineage
   - cpmp receipt emit
   - cpmp receipt verify-no-deletion
   - cpmp enterprise doctor

4. Create stubs for the enterprise modules:
   - cpmp-enterprise-auth
   - cpmp-enterprise-tenancy
   - cpmp-enterprise-policy
   - cpmp-enterprise-audit
   - cpmp-enterprise-retention
   - cpmp-enterprise-backup
   - cpmp-enterprise-redaction
   - cpmp-enterprise-approval
   - cpmp-enterprise-observability
   - cpmp-open-ontologies-adapter
   - cpmp-ggen-projection
   - cpmp-public-vocabulary-firewall

5. Ensure all gates are enforced in the scanning pipeline. A scan must be refused if:
   - RDF does not parse cleanly or contains illegal URI syntax.
   - Private predicates carry public authority where standard predicates exist.
   - Local IDs are laundered into prov:, sh:, dcat:, or other standard namespaces.
   - Validation fails or missing sh:ValidationReport.
   - Output does not link to the scan activity via prov:wasGeneratedBy.
   - Files lack checksums.
   - Reports lack source graph hashes.
   - Please update the project plan, build and implement these structures, verify the tests pass, and report back.

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
* “CLAP governance proven”
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







