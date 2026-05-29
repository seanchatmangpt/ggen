# Plan: Witnessed Agent Truthfulness GALL Protocol (Public Interop Purge)

## Architecture & Design
Implement the Witnessed Agent Truthfulness GALL verifier using a declarative Witnessed Code Evaluation / Knowledge Hook Actuation model that uses **public vocabularies only** (no private vocabulary namespaces), with a fully proven **Dialect Completeness Matrix**, an **OFMF-to-Public-Vocabulary hook schema mapping**, and an **Indirect Hook Boundary Request / Boundary Adapter** architecture.

### Public Interop Purge & Agent K constraints:
1. **Ban Private RDF Terms**: Completely ban private RDF terms and namespaces: `gall:`, `gg:`, `kh:`, `pcst:`, `truex:`, `ggen:`, `doc:`, `doctest:`, or any other project-private prefixes. Private terms must NOT appear in hook packs, evidence graphs, decision deltas, final reports, docs tree TTL, or receipts. Banned in all RDF namespaces/URIs. The string `gall` is allowed ONLY in filenames/binary names/titles.
2. **Ban Namespace Laundering**: Public namespaces may only contain terms actually defined by those vocabularies. Custom terms (such as `BoundaryExecutionRequest` or `R7` or `w0_worktree`) must NOT be placed inside public namespaces (e.g. `prov:BoundaryExecutionRequest` or `prov:R7` is strictly forbidden). Custom operational labels must only be literal values of public predicates (e.g., `dcterms:type "BoundaryExecutionRequest"^^xsd:string` or `dcterms:identifier "R7"^^xsd:string`).
3. **Emit Actual SHACL Validation Reports**: Emit the following 5 validation reports in SHACL turtle format (`*.validation.ttl`) under `crates/ggen-graph/audit/`:
   - `audit/public_vocab.validation.ttl`
   - `audit/hook_actuation.validation.ttl`
   - `audit/dialect_completeness.validation.ttl`
   - `audit/sabotage.validation.ttl`
   - `audit/final.validation.ttl`
4. **Standard Public Structures**: Replace custom terms with standard structures from PROV, DCAT, SHACL, and SPDX (e.g., docs tree uses `prov:Entity`, `dcat:Distribution`, `dcterms:title`, etc.).
5. **Final Status**:
   - Promotion: `sh:ValidationReport` + `sh:conforms true` linked via `prov:wasGeneratedBy` to evaluation `prov:Activity`.
   - Refusal: `sh:ValidationReport` + `sh:conforms false` containing `sh:ValidationResult` with `sh:resultSeverity sh:Violation` and `sh:resultMessage`.
6. **External Interoperability & RFC 3986 Compliance**: Fix all syntax errors (unescaped spaces in URIs). All URIs in generated and static `.ttl` files must be valid RFC 3986 URIs (using percent-encoding for spaces, e.g., `%20`).
7. **Standard RDF Canonicalization**: Receipts and replay must use standard RDF canonicalization (such as URDNA2015/URDNA2012 or standard quad canonical serialization and hashing) instead of custom string sorts to guarantee external reproducibility.

Allowed prefixes: `rdf:`, `rdfs:`, `owl:`, `xsd:`, `prov:`, `dcat:`, `dcterms:`, `skos:`, `sh:`, `time:`, `spdx:`, `ocel:`.
OFMF components map to standard public ontologies:
- **OFMF HookPack**: `dcat:Dataset` + `prov:Entity`
- **OFMF KnowledgeHook**: `prov:Plan` + `sh:NodeShape`
- **OFMF HookTrigger**: `sh:SPARQLConstraint` / SHACL / N3 / Datalog / ShEx
- **OFMF HookAction**: SPARQL CONSTRUCT query (emits request delta or final status)
- **OFMF DiagnosticEmitter**: `sh:ValidationReport` + `sh:ValidationResult`
- **OFMF Receipt**: `prov:Entity` + `dcat:Dataset` + checksum evidence
- **OFMF Runtime**: Hook actuation over evidence graph

## Milestones

### Milestone 1: Planning and Setup
- [x] Create/update `plan.md`
- [x] Update `progress.md`
- [x] Update `BRIEFING.md`

### Milestone 2: Boundary Observation Binaries
Implement under `crates/ggen-graph/src/bin/` using public vocabularies:
- [ ] `gall_observe_worktree.rs`
- [ ] `gall_observe_commands.rs`
- [ ] `gall_observe_sabotage.rs`
- [ ] `gall_observe_clean_room.rs`
- [ ] `gall_observe_docs_tree.rs`
- [ ] `gall_observe_doctests.rs`
- [ ] `gall_materialize_evidence_graph.rs`
- [ ] `gall_actuate_code_evaluation.rs`
- [ ] `gall_adjudicate_witnessed_truthfulness.rs`

### Milestone 3: Public Vocabulary Knowledge Hook Pack
- [ ] Implement `crates/ggen-graph/hooks/gall-code-evaluation.ttl` using only public vocabulary constructs, mapping OFMF hook concepts to PROV-O, SHACL, and DCAT. Include one W-check hook per dialect family (`W-DIALECT-SPARQL`, `W-DIALECT-SHACL`, `W-DIALECT-N3`, `W-DIALECT-DATALOG`, `W-DIALECT-SHEX`).

### Milestone 4: Dialect Completeness Matrix Fixtures and Tests
- [ ] Create fixtures under `crates/ggen-graph/tests/fixtures/dialects/`.
- [ ] Implement integration tests under `crates/ggen-graph/tests/` (`dialect_completeness.rs`, `dialect_sabotage.rs`, `dialect_receipts.rs`, `dialect_replay.rs`).
- [ ] Represent unsupported capabilities correctly in OCEL and SHACL report with `sh:conforms false` for non-executable dialects.

### Milestone 5: Docs Tree and Required Documentation
- [ ] Verify required docs presence.
- [ ] Emit `docs/docs.tree.json` and `docs/docs.tree.ttl` (using public vocabularies).
- [ ] Update `docs/VISION_2030_GALL_PROOF.md` to map checkpoints to public SHACL validation reports.

### Milestone 6: Doctest-Governed DX/QoL
- [ ] Ensure full public API doctest coverage (R6).

### Milestone 7: Launchers and workspace orchestrator
- [ ] Implement/update `/verify_agent_truthfulness.sh` at workspace root.
- [ ] Implement `scripts/gall/run_witnessed_truthfulness.sh` as thin launcher.

### Milestone 8: Execution, Sabotage Verification & Evidence Generation
- [ ] Run verification suite, verify clean promotion and all 12 sabotage cases refusal.

### Milestone 9: Final Reporting & Handoff
- [ ] Write `handoff.md` and complete progress reports.
- [ ] Communicate success back to the Sentinel.
