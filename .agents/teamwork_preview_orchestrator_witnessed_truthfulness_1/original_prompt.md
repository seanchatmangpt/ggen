## 2026-05-27T00:50:14Z

You are the teamwork_preview_orchestrator for the Witnessed Agent Truthfulness GALL Protocol task.
Your role: Coordinate specialists, plan milestones, monitor progress, verify implementation, and report completion.
Your working directory is: /Users/sac/ggen/.agents/teamwork_preview_orchestrator_witnessed_truthfulness_1/

Read the original user request and requirements in `/Users/sac/ggen/ORIGINAL_REQUEST.md`, specifically the follow-up request dated 2026-05-27T00:49:55Z.

Your key tasks are:
1. Implement the Witnessed Agent Truthfulness GALL protocol in the `ggen` repository to prevent narrative/counterfeit agent completion claims.
2. Develop the external witness adjudication scripts under `scripts/gall/external/` (20_... to 99_).
3. Generate the required structured verification artifacts & evidence in `crates/ggen-graph/audit/`.
4. Update the structural proof document `docs/VISION_2030_GALL_PROOF.md`.
5. Strictly adhere to all global user rules (GEMINI.md and AGENTS.md, e.g., no mocks, no placeholder laundering, no stubs, real boundaries).
6. Create `plan.md` and keep updating `progress.md` in your working directory.
7. Output a victory claim message back to the Sentinel when all checkpoints and verification checks are completed and passing.

Proceed with the task. You are authorized to invoke specialists to analyze and implement.

## 2026-05-27T00:54:06Z

The user has updated the specifications. Please pivot the implementation to match the updated prompt draft:

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

## 2026-05-27T00:55:06Z

The user has refined the specification to adopt the "Witnessed Code Evaluation / Knowledge Hook Actuation" model. Please update the team's requirements to match this prompt draft:

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

## 2026-05-27T01:05:47Z

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

## 2026-05-27T01:07:40Z

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

## 2026-05-27T01:08:38Z

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

## 2026-05-27T01:09:46Z

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

## 2026-05-27T01:10:34Z

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
- [ ] Boundary execution is indirect: Hooks trigger boundary execution by emitting `prov:Activity` request facts; boundary adapters consume those requests and perform the actual runs, emitting transcript evidence back. No hook directly performs boundary work.
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

## 2026-05-27T01:26:40Z

## Pivoted Requirements: Public Interop Purge

The user has ordered a Public Interop Purge follow-up (dated 2026-05-27T01:26:40Z). Please pivot the implementation immediately to meet these requirements:

### 1. Ban Private RDF Terms
- Completely ban private RDF terms and namespaces: `gall:`, `gg:`, `kh:`, `pcst:`, `truex:`, `ggen:`, `doc:`, `doctest:`, or any other project-private prefixes.
- Private terms must NOT appear in hook packs, evidence graphs, decision deltas, final reports, docs tree TTL, or receipts. Banned in all RDF namespaces/URIs.
- Project terminology (such as `gall`) may only appear in filenames, binary names, directory names, and document titles.

### 2. Ban Namespace Laundering
- Public namespaces may only contain terms actually defined by those vocabularies.
- Custom terms (such as `BoundaryExecutionRequest` or `R7` or `w0_worktree`) must NOT be placed inside public namespaces (e.g. `prov:BoundaryExecutionRequest` or `prov:R7` is strictly forbidden).
- Custom operational labels must only be literal values of public predicates (e.g., `dcterms:type "BoundaryExecutionRequest"` or `dcterms:identifier "R7"`).

### 3. Emit Actual SHACL Validation Reports
- The verifier suite must emit actual SHACL validation report files (`*.validation.ttl`) to the `audit/` directory.
- Required files:
  - `audit/public_vocab.validation.ttl`
  - `audit/hook_actuation.validation.ttl`
  - `audit/dialect_completeness.validation.ttl`
  - `audit/sabotage.validation.ttl`
  - `audit/final.validation.ttl`

### 4. Use Standard Public Structures
- Replace all custom document, status, and receipt terms with standard structures from PROV, DCAT, SHACL, and SPDX. For example, docs tree TTL must use `prov:Entity`, `dcat:Distribution`, `dcterms:title`, etc.
- Final promotion/refusal must be represented as:
  - **Promotion**: `sh:ValidationReport` and `sh:conforms true` linked to execution via `prov:Activity` and `prov:wasGeneratedBy`.
  - **Refusal**: `sh:ValidationReport` and `sh:conforms false` with one or more `sh:ValidationResult` objects with `sh:resultSeverity sh:Violation` and `sh:resultMessage`.

## 2026-05-27T01:30:12Z

## Pivoted Requirements: Agent K — Open Ontologies External Oracle

The user has introduced the "Agent K — Open Ontologies External Oracle" requirement (dated 2026-05-27T01:30:12Z). Please pivot the implementation immediately to meet these requirements:

### 1. External Interoperability / Toolchain Compliance
- Ensure all Turtle artifacts (`.ttl` files under `hooks/`, `audit/`, `docs/`, and `schema/`) can be parsed, loaded, and linted successfully by an independent RDF/OWL/SHACL validation toolchain (e.g. Oxigraph or Open Ontologies).
- Fix syntax errors (such as unescaped spaces in file URIs, e.g. in `audit/worktree_inventory.ttl` or other generated artifacts). All URIs must be standard, valid RFC 3986 URIs.

### 2. Standard RDF Canonicalization
- Receipts and replay verification must use standard RDF canonicalization (such as URDNA2015/URDNA2012 or standard quad canonical serialization and hashing) to ensure external reproducibility, rather than home-grown string sorts.

### 3. Open Ontologies Gate Validation
- All Turtle artifacts must be clean and compliant, verified through this oracle check. Refuse promotion if any validation or parsing fails.

## 2026-05-27T02:24:04Z

You are the teamwork_preview_orchestrator for the Witnessed Agent Truthfulness GALL Protocol task.
Your role: Resume the work of the previous orchestrator which stopped due to API quota exhaustion.
Your working directory is: /Users/sac/ggen/.agents/teamwork_preview_orchestrator_witnessed_truthfulness_1/

Read the plans, progress, and original_prompt files in your working directory.
Your current objectives are to implement the Witnessed Agent Truthfulness GALL Protocol, incorporating:
1. The "Public Interop Purge" (ban private RDF terms, ban namespace laundering, use standard structures from PROV, DCAT, SHACL, and SPDX).
2. The "Agent K — Open Ontologies External Oracle" (fix syntax errors in TTL files like spaces in URIs, use standard RDF canonicalization like URDNA2015 for receipts, load/lint all Turtle artifacts with external validator).
3. Generate the 5 durable SHACL validation reports in `audit/` (public_vocab.validation.ttl, hook_actuation.validation.ttl, dialect_completeness.validation.ttl, sabotage.validation.ttl, final.validation.ttl).

Verify all requirements, run the clean rebuild and all 12 sabotage validation cases, and report completion back to the Sentinel.
