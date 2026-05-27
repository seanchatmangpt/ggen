# Action Plan: ggen External Lifecycle Evaluation

## Milestone 1: Exploration & Analysis [DONE]
- **Goal**: Verify compilation of existing code, locate source code integration points.
- **Worker**: teamwork_preview_explorer

## Milestone 2: Self-Audit Implementation [DONE]
- **Goal**: Implement `crates/ggen-graph/src/ocel/self_audit.rs` and `crates/ggen-graph/src/ocel/gall_projection.rs` emitting the OCEL v2 self-audit log.
- **Worker**: teamwork_preview_worker

## Milestone 3: Coverage Matrix & Verification Scripts [DONE]
- **Goal**: Implement coverage mapping, integration tests, and scripts `verify_ocel_self_audit.sh`.
- **Worker**: teamwork_preview_worker

## Milestone 4: Derived Checkpoint Proof Report [DONE]
- **Goal**: Author `crates/ggen-graph/audit/vision2030.self_audit.summary.md` and `docs/VISION_2030_GALL_PROOF.md`.
- **Worker**: teamwork_preview_worker

## Milestone 5: Forensic Audit & Validation [DONE]
- **Goal**: Execute verification scripts and run the Forensic Auditor.
- **Worker**: teamwork_preview_auditor

## Milestone 6: External Observer Script Ring
- **Goal**: Implement the 14 scripts (`00_` to `13_`) under `scripts/gall/external/` to capture baseline, extract requirements, verify package constraints, check feature flags, run tests, scan forbidden surfaces, verify OCEL self-audits (09_verify_ocel_self_audit.sh), detect contradictions (12_detect_contradictions.sh), and perform final promotion adjudication (13_adjudicate_gall_promotion.sh).
- **Digests Requirement**: Capture script digests and file digests in scripts to prevent bypass/mutation.
- **Worker**: teamwork_preview_worker
- **Reviewer**: teamwork_preview_reviewer

## Milestone 7: CI Integration & Docs Rewrite
- **Goal**: Integrate external scripts into CI flows and rewrite `docs/VISION_2030_GALL_PROOF.md` such that the promotion decision is strictly declared as dependent on the external verifier scripts' adjudication results.
- **Worker**: teamwork_preview_worker
- **Reviewer**: teamwork_preview_reviewer

## Milestone 8: E2E External Adjudication Verification
- **Goal**: Execute the full script ring `13_adjudicate_gall_promotion.sh` to produce `crates/ggen-graph/audit/vision2030.external_adjudication.json`. Ensure all tests pass and there are no violations. Run the Forensic Auditor on the final codebase to confirm compliance.
- **Auditor**: teamwork_preview_auditor
