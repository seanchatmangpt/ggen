# Project: ggen External Lifecycle Evaluation

## Architecture
- `crates/ggen-graph` is a standalone crate containing the public-ontology-governed RDF graph substrate.
- The OCEL v2 self-audit log generator is implemented in `crates/ggen-graph/src/ocel/self_audit.rs` and integrated into `crates/ggen-graph/src/ocel/gall_projection.rs`.
- It emits the audit log at `crates/ggen-graph/audit/vision2030.self_audit.ocel.json`.
- The coverage matrix is implemented in `crates/ggen-graph/src/ocel/coverage.rs` and emits coverage details to `crates/ggen-graph/audit/vision2030.coverage.json`.
- Verification scripts check correctness and compile/test/audit compliance.
- The External Observer Script Ring under `scripts/gall/external/` (00 to 13) independently observes, hashes, validates, and adjudicates the crate lifecycle and GALL checkpoints, producing `crates/ggen-graph/audit/vision2030.external_adjudication.json`.

## Milestones
| # | Name | Scope | Dependencies | Status | Conv ID |
|---|------|-------|-------------|--------|---------|
| 1 | Exploration & Analysis | Run initial compilation and explore files/interfaces via Explorer | none | DONE | 5bb19ed9-8e95-476b-839f-2b47534cb9a1 |
| 2 | Self-Audit Implementation | Implement `src/ocel/self_audit.rs` and `src/ocel/gall_projection.rs` | M1 | DONE | 525c2af3-abe2-43b3-a237-6c9581510ff3 |
| 3 | Coverage & Integration Tests | Implement `src/ocel/coverage.rs`, integration tests, and verification scripts | M2 | DONE | 21375ab0-1b5d-47a9-8c03-29312351aa1f |
| 4 | Documentation | Author self_audit.summary.md and VISION_2030_GALL_PROOF.md | M3 | DONE | b7043402-c171-446e-9aa2-0beba6ac5c85 |
| 5 | Forensic Audit & Validation | Run verification scripts and Forensic Auditor to ensure clean verdict | M4 | DONE | ad2ec82d-4ed9-4961-a9b0-c0737130d5a7 |
| 6 | External Observer Script Ring | Implement scripts `00_` to `13_` under `scripts/gall/external/` with contradiction detection and audit verification | M5 | DONE | 09a4063b-399c-45ec-9849-7b9b65753ee5 |
| 7 | CI Integration & Docs Rewrite | Integrate external scripts into CI flows and rewrite `docs/VISION_2030_GALL_PROOF.md` | M6 | DONE | 09a4063b-399c-45ec-9849-7b9b65753ee5 |
| 8 | E2E External Adjudication Verification | Run promotion adjudication, verify external_adjudication.json output, and ensure all tests pass | M7 | DONE | ad2ec82d-4ed9-4961-a9b0-c0737130d5a7 |

## Interface Contracts
### `SelfAudit` ↔ `OcelLog`
- Self-audit maps execution events/objects to OCEL v2 format.
- Output files must be saved at specific locations:
  - `crates/ggen-graph/audit/vision2030.self_audit.ocel.json`
  - `crates/ggen-graph/audit/vision2030.coverage.json`
  - `crates/ggen-graph/audit/vision2030.self_audit.summary.md`
  - `docs/VISION_2030_GALL_PROOF.md`

### External Observer Adjudication
- Sequential execution of scripts 00 to 12.
- Adjudicator (13) validates all outputs and digests.
- Emits results to `crates/ggen-graph/audit/vision2030.external_adjudication.json`.
