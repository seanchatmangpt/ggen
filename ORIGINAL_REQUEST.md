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
   - Open Ontologies load, validation, or versioning fails.

Please update the project plan, build and implement these structures, verify the tests pass, and report back.

## Follow-up — 2026-06-09T04:32:45Z

Verify that the `ggen-marketplace` crate and atomic pack taxonomy are ready for the v26.6.9 release, updating the workspace versions and integrating the `wasm4pm-compat` library.

Working directory: /Users/sac/ggen
Integrity mode: benchmark

## Requirements

### R1. Workspace Version Bump
Upgrade the workspace package version and workspace dependency versions to `26.6.9` in all `Cargo.toml` files within the `ggen` repository.

### R2. wasm4pm-compat Integration
Integrate `wasm4pm-compat = { version = "26.6.9", path = "/Users/sac/wasm4pm-compat" }` as an active dependency in `crates/ggen-graph/Cargo.toml` and ensure that it compiles successfully within the workspace.

### R3. Compile, Test, and Clippy Verification
Ensure the entire workspace compiles successfully (`cargo build --all-targets`), passes all unit and integration tests (`cargo test --all-targets`), and passes clippy checks (`cargo clippy --all-targets --all-features -- -D warnings`).

### R4. Marketplace and Pack Consistency
Verify that the `ggen-marketplace` package, its taxonomy structures (like atomic pack classifications in `crates/ggen-marketplace/src/marketplace/atomic.rs`), and metadata are correct and structurally ready for release.

## Acceptance Criteria

### Build & Release Readiness
- [ ] Every Cargo.toml in the workspace has its version set to `26.6.9`.
- [ ] `crates/ggen-graph/Cargo.toml` has an active, compiling dependency on `wasm4pm-compat` pointing to `/Users/sac/wasm4pm-compat`.
- [ ] `cargo check --all-targets` exits with code 0.
- [ ] `cargo test --all-targets` exits with code 0.
- [ ] `cargo clippy --all-targets --all-features -- -D warnings` exits with code 0.

## Follow-up — 2026-06-11T19:18:48Z

An audit of the ggen tool from the perspective of a completely new user to identify usability gaps, setup friction, and documentation clarity.

Working directory: /Users/sac/ggen
Integrity mode: demo

### Requirements

#### R1. Installation and Local Environment Setup Audit
Test and document the onboarding workflow, installation processes, dependencies, and environment setup friction points for a first-time user starting in this repository.

#### R2. CLI Interface & Error Handling Evaluation
Evaluate the CLI interface clarity, command discovery, help messages, argument parsing, and error behavior. Identify areas where error messages are unhelpful or command usage is counter-intuitive.

#### R3. Documentation & Walkthrough Verification
Audit the completeness, accuracy, and ease-of-following of the documentation (such as `README.md`, developer guides, or onboarding docs) by comparing the instructions directly with reality.

#### R4. Code Architecture & Developer Onboarding Assessment
Evaluate the project's codebase structure, dependency layout, readability, compile times, and general developer experience when first trying to understand or modify the codebase.

### Acceptance Criteria

#### Audit Scope
- [ ] Attempt every onboarding and setup step documented in the README/onboarding guides, noting where commands succeed, fail, or hang.
- [ ] Run and document the input, stdout, stderr, and exit codes for at least 5 distinct ggen CLI commands or workflows.
- [ ] Review the top-level README and major docs for typos, outdated commands, or missing prerequisites.

#### Deliverables
- [ ] Produce a structured markdown report at `/Users/sac/ggen/audit_report.md` outlining findings, friction levels, and specific actionable recommendations for improvement across setup, CLI, docs, and codebase onboarding.
- [ ] Include a detailed `Verification Log` section in the report containing raw terminal transcripts, input command lines, and output snippets from testing the setup and CLI.

## Follow-up — 2026-06-11T19:19:46Z

The user has added a new requirement to the audit prompt:

### R5. Ruby on Rails-Inspired Architectural and DX Evaluation
Provide a comparative critique of ggen from the perspective of a Ruby on Rails Core Team member, drawing lessons on:
1. **Convention over Configuration (CoC)**: Standardizing Turtle class-to-code mapping to eliminate verbose configuration.
2. **Generators & Scaffolding**: Standardizing model/controller/service templates and generator commands (e.g. adding to ontology and files concurrently).
3. **Ontology Migrations**: Designing a system to evolve ontologies over time incrementally, similar to Rails ActiveRecord migrations.
4. **Developer Happiness (DX)**: Providing interactive REPLs/consoles (like `rails console` but for Oxigraph/Turtle query and template playground) and rich context-aware CLI diagnostic error messages.
5. **The Golden Path**: Standard schemas and templates that provide an out-of-the-box working application with minimal friction.

### Updated Acceptance Criteria
- [ ] Include a dedicated section in the audit report comparing ggen with Rails philosophies and outlining 3-5 high-impact Rails-inspired lessons for ggen.

## Follow-up — 2026-06-22T22:54:41Z

Document the state of all `.md` files in the repository by reading their contents and producing a comprehensive report detailing the status, purpose, and completeness of each file.

Working directory: /Users/sac/ggen
Integrity mode: development

## Requirements

### R1. Markdown File Discovery
Find all `.md` files in the repository root and subdirectories (excluding standard ignore folders like `target`, `.git`, `.venv_shacl`, and node_modules).

### R2. Content Analysis
For each discovered `.md` file, analyze its content to determine its primary purpose, structural state (e.g., whether it contains placeholders, stubs, TODOs, or is fully complete), and any potential alignment with repository rules/AGENTS.md.

### R3. Executive Report Generation
Generate a comprehensive, structured markdown report documenting the state of the repository's documentation. The report must be saved as `DOCUMENTATION_AUDIT_REPORT.md` in the workspace root.

## Acceptance Criteria

### Audit Scope & Verification
- [ ] Every `.md` file in the workspace (excluding ignored directories like target, .git, etc.) must be identified and visited.
- [ ] A final audit report file `DOCUMENTATION_AUDIT_REPORT.md` must be created in the workspace root.
- [ ] The generated report must contain a table covering all discovered `.md` files with columns for:
  - File Path (relative to workspace root)
  - Primary Purpose (brief summary of what the file describes)
  - Completeness State (e.g., Complete, Draft, Placeholder/Stub, Has TODOs)
  - Action Items / Recommendations
- [ ] A validation check must prove that the number of files listed in the report matches the number of `.md` files found by a standard shell command like `find . -name "*.md"` (excluding ignored directories).

## Follow-up — 2026-06-23T00:12:24Z

Replace the custom TOML parsing and validation system in `ggen-config` with the `star-toml` library, extending `star-toml` with any necessary validation helpers first.

Working directory: /Users/sac/ggen
Integrity mode: development

## Requirements

### R1. Star-TOML Validation Extensions
Extend the `star-toml` crate's `Validator` struct (in `crates/star-toml`) with built-in validation helper methods to cover all parsing/validating needs in `ggen-config`. Specifically, add:
- Semver validation check (basic semver structure)
- IP/Domain hostname validation check
- Path validation check (non-empty or safe directory/file paths)

### R2. GgenConfig Validate Trait Implementation
Implement the `star_toml::Validate` trait for the `GgenConfig` structure and all of its sub-configuration structures (`ProjectConfig`, `AiConfig`, `TelemetryConfig`, `TemplatesConfig`, `McpConfig`, `A2AConfig`, etc.). The trait implementations must cover all checks previously implemented in `ConfigValidator`.

### R3. Migration of ggen-config
Refactor `crates/ggen-config` to use `star-toml` for loading, env-var expansion, and validation:
- Modify `ConfigLoader` to use `star_toml::Loader` or `star_toml::load_file`.
- Modify `ConfigValidator` to delegate validation to `star_toml`'s validation engine.
- Map or wrap `star_toml::ValidationErrors` in `ConfigError`.
- Ensure all existing workspace tests pass cleanly.

## Acceptance Criteria

### Correctness & Compatibility
- [ ] `star-toml` compiles successfully with the new validation helpers.
- [ ] `ggen-config` is fully migrated to use `star-toml` for loading and validating configuration files.
- [ ] Running `cargo check` and `cargo test` in the workspace finishes with a green pass.
- [ ] No regression is introduced in existing `ggen-config` or workspace tests.

## Follow-up — 2026-06-23T04:17:11Z

Implement a prototype of the 1000x praxis active self-healing and validation system in `/Users/sac/praxis/playground`.

Working directory: /Users/sac/praxis/playground
Integrity mode: development

## Requirements

### R1. Active Hygiene Reconciler (`praxis-reconciler`)
Build a Rust tool/daemon (`praxis-reconciler`) that actively monitors standard repository configurations (e.g. `rustfmt.toml`, `deny.toml`, `.editorconfig`) against a reference set (from `/Users/sac/praxis/template`). If any of the monitored files are modified, tampered with, or deleted, the tool must automatically detect the drift and overwrite the files back to match the reference state.

### R2. Cryptographic Compliance Guard (`praxis-guard`)
Build a Rust tool (`praxis-guard`) that verifies project checks (compilation and tests). Upon a successful run, it must compute a BLAKE3 content-addressed digest of the project source files, sign it, and emit an unforgeable compliance receipt (`receipt.json`) detailing the state of verification.

### R3. Demonstration Playground
Set up a sample workspace/crate structure in `/Users/sac/praxis/playground` demonstrating the integration. Provide a script or execution path that:
1. Simulates drift (e.g. modifying `rustfmt.toml`) and shows `praxis-reconciler` restoring it.
2. Runs `praxis-guard` to generate a valid cryptographic receipt on a clean project, and verifies that the receipt fails if any source files are modified or check commands fail.

## Acceptance Criteria

### Correctness & Compliance
- [ ] Both `praxis-reconciler` and `praxis-guard` are implemented in Rust and compile cleanly.
- [ ] An automated test suite in `/Users/sac/praxis/playground` verifies that `praxis-reconciler` successfully restores modified/deleted config files within seconds.
- [ ] An automated test verifies that `praxis-guard` produces a valid `receipt.json` on success, and rejects execution or fails verification if source files do not match the receipt's BLAKE3 hashes.
- [ ] Running the workspace tests with `cargo test` returns a clean, green pass.

## Follow-up — 2026-06-24T06:16:46Z

# Teamwork Project Prompt

> Status: Launched

# Vision 2030 — Rust as a Manufactured Language Surface

By 2030, GGEN should be able to manufacture complete Rust software systems from admitted ontology state.

Not snippets.
Not helper files.
Not scaffolds.
Complete Rust codebases.

```text
Ontology facts
→ SPARQL projection
→ Tera rendering
→ Cargo verification
→ artifact receipts
→ replayable generation lineage
```

The goal is to move Rust development from manual file authorship into semantic manufacturing.

A Rust workspace should be describable as a bounded product space:

```text
workspace
× crate
× target
× module
× item
× type
× trait
× impl
× feature
× cfg
× test
× bench
× doc
× receipt
```

GGEN becomes the manufacturing system that expands that product space into verified Rust artifacts.

---

## 2030 Operating Claim

By 2030, a developer should be able to declare a Rust system at the ontology level and run:

```bash
ggen sync
cargo check --workspace --all-targets
cargo test --workspace
```

and receive a complete, formatted, verified, receipted Rust workspace.

The generated workspace may contain:

```text
multiple crates
library targets
binary targets
examples
tests
benchmarks
module trees
traits
impl blocks
structs
enums
type aliases
constants
feature flags
cfg-gated targets
documentation
release metadata
artifact receipts
verification reports
```

The ontology is the admitted source of structure.

Tera renders the surfaces.

Cargo verifies the result.

Receipts prove lineage.

---

## Core Vision

The Rust source tree becomes an output artifact.

The ontology becomes the source of architectural truth.

```text
Rust source is not where structure is invented.
Rust source is where admitted structure is rendered.
```

This does not remove human authorship.

It relocates human authorship to the correct level:

```text
manual coding
→ semantic specification
→ template design
→ verification strategy
→ generated artifact review
```

The human does not hand-maintain every repeated Rust surface.

The human defines the manufacturing law.

---

## Why This Matters

Rust is structurally rich:

```text
traits
impls
generics
lifetimes
features
cfg targets
modules
visibility boundaries
Cargo targets
tests
benches
docs
examples
```

That richness is power, but it also creates repetition.

A serious Rust system often contains the same structural pattern across many surfaces:

```text
API item
test item
bench item
doc item
example item
registry item
feature-gated item
target-specific item
```

GGEN should manufacture that entire constellation from one admitted semantic object.

The value is not fewer keystrokes.

The value is structural integrity.

---

## Combinatorial Maximalism

The system must not be designed around the smallest useful Rust generation case.

It must be designed around the largest lawful Rust product space.

The ontology should support generation across:

```text
workspace × package
package × target
target × module
module × item
item × attribute
item × visibility
item × generic form
function × parameter set
function × return type
struct × field set
enum × variant set
trait × required/provided item set
impl × target type
impl × trait
crate × feature
crate × cfg target
item × test surface
item × bench surface
item × doc surface
artifact × receipt
```

The point is not to generate one Rust shape.

The point is to generate all lawful Rust shapes that the ontology can admit.

---

## 2030 Capability Targets

### 1. Full Workspace Manufacturing

GGEN can generate complete Cargo workspaces, including:

```text
workspace manifest
workspace dependencies
member crates
profiles
features
lints
crate manifests
library targets
binary targets
test targets
bench targets
examples
build scripts
```

### 2. Full Rust Item Manufacturing

GGEN can generate every major Rust item category:

```text
modules
use trees
functions
structs
enums
unions
traits
impl blocks
type aliases
constants
statics
macros
extern blocks
```

### 3. Full Type Surface Manufacturing

GGEN can represent and render:

```text
primitive types
path types
generic types
tuple types
array types
slice types
references
mutable references
raw pointers
function pointers
dyn trait types
impl trait types
associated type projections
qualified paths
unit type
never type
```

### 4. Full Generics Manufacturing

GGEN can generate:

```text
type parameters
lifetime parameters
const generics
trait bounds
lifetime bounds
where clauses
associated type constraints
higher-ranked trait bounds
default generic parameters
```

### 5. Full Attribute Manufacturing

GGEN can render attributes across:

```text
crate
module
item
field
variant
function
parameter
impl block
trait item
```

Including:

```text
derive
cfg
cfg_attr
allow
warn
deny
forbid
repr
inline
must_use
deprecated
doc
test
ignore
should_panic
macro_export
path
```

### 6. Full Verification Manufacturing

Every generated structure can carry matching verification surfaces:

```text
unit tests
integration tests
doc tests
compile tests
fixtures
golden files
benchmarks
examples
cargo commands
verifier reports
```

### 7. Full Documentation Manufacturing

GGEN can generate:

```text
crate docs
module docs
item docs
field docs
variant docs
examples
README files
API indexes
CHANGELOG files
release notes
usage guides
```

### 8. Full Receipt Manufacturing

Every generated artifact has lineage:

```text
source ontology IRI
source graph
SPARQL projection
Tera template
output path
content hash
generation timestamp
generator version
verification command
verification status
```

No artifact without receipt.

---

## 2030 Maturity Ladder

### Level 1 — File Generation
GGEN emits individual Rust files from TTL.

### Level 2 — Module Generation
GGEN emits coherent module trees with imports, exports, and documentation.

### Level 3 — Crate Generation
GGEN emits full Cargo packages with source, tests, examples, benches, and docs.

### Level 4 — Workspace Generation
GGEN emits multi-crate workspaces with shared dependencies, features, profiles, and verification commands.

### Level 5 — Product-Space Generation
GGEN emits entire families of crates across features, targets, generic forms, and artifact surfaces.

### Level 6 — Verified Manufacturing
GGEN refuses generation unless SHACL, SPARQL projection, Tera provision, Cargo verification, and artifact receipts all pass.

### Level 7 — Self-Describing Rust Systems
Generated Rust workspaces include their own ontology references, generation receipts, artifact manifests, and replay instructions.

---

## 2030 Success Metrics

By 2030, the project succeeds if:

```text
A full Rust workspace can be generated from ontology facts.
Every generated file is receipted.
Every template variable is projected by SPARQL.
Every generated crate passes cargo check.
Every generated workspace passes cargo test.
Manual registry drift is eliminated.
Generated docs remain synchronized with generated code.
New item families can be added by ontology + template extension, not one-off scripts.
```

The strongest metric:
```text
A new Rust product family can be introduced by adding ontology individuals and templates, without changing the generator core.
```

---

## Strategic End State

GGEN becomes a Rust manufacturing compiler.
Tera becomes the rendering surface.
SPARQL becomes the projection law.
SHACL becomes the admission gate.
Cargo becomes the verification boundary.
Receipts become the proof of artifact lineage.
The generated Rust codebase becomes a consequence of admitted structure.

---

## Crown Statement

By 2030, GGEN should not merely generate Rust files.
It should manufacture Rust systems.
A Rust project should be expressible as ontology, rendered through Tera, verified by Cargo, and proven by receipts.

That is the 2030 vision:
```text
Rust as manufacturable architecture.
```

---

## Execution Requirements

Build a comprehensive RDF/TTL ontology (`rustlang-ontology`) for representing Rust code as manufacturable semantic structure. The goal is to provide the `.ttl` definitions, SHACL shapes, SPARQL queries, and Tera templates that an external GGEN engine will use to manufacture complete Rust software systems according to the PRD/ARD provided.

Working directory: ~/teamwork_projects/rustlang_ontology
Integrity mode: development

### R1. Complete Ontology Definition
Define the full set of TTL ontology layers (Project, Rust Syntax, Generation, Verification, Documentation) as specified in the ARD. This includes exhaustive modeling of Cargo workspaces, crates, Rust modules, items, traits, generics, visibility, etc. The implementation must cover all specifications in the PRD from the start.

### R2. SHACL Validation
Define SHACL shapes to rigorously validate the ontology, ensuring no malformed TTL is admitted (e.g., function without a name, template without a projection query).

### R3. SPARQL Projections
Write SPARQL queries that project facts from the ontology into variables consumed by Tera templates. This must enforce the Template Provision Law (no unprojected variables).

### R4. Tera Templates
Create exhaustive Tera templates that render the projected facts into valid Rust code (Cargo.toml, lib.rs, structs, enums, traits, functions, tests, benches, docs, etc.), along with generated receipts for every artifact.

## Acceptance Criteria

### Objective Verification
- [ ] All TTL files pass SHACL validation against the defined shapes using a standard SHACL validator.
- [ ] All SPARQL queries successfully parse and map to the template requirements.
- [ ] All Tera templates successfully parse without syntax errors.
- [ ] **End-to-End Test (if GGEN runner or mock is available):** Given a complete TTL graph describing a workspace and crate, the templates render a Rust codebase that passes `cargo fmt --check`, `cargo check --workspace --all-targets`, and `cargo test --workspace`.
- [ ] No manual registry drift: templates generate registries entirely from ontology facts.
- [ ] Every generated file specifies a receipt identifying the source entity, template, query, output path, hash, and verification command.
