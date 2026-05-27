# Finish Plan

## 1. Executive Summary
The Genesis-bearing interchangeable parts architecture represents a radical shift from centralized lakehouses to edge-based lawful construction. This plan dictates the path to finishing the v0.2 milestone and closing the critical interop gaps.

## 2. Phase A: Structural Consolidation
The initial priority is code consolidation. The reference materials imported from KNHK and `wasm4pm` must be cleanly distilled into a `genesis-core` crate that strictly enforces the data algebra (Pair2, Construct8, Need9, Need257) with zero IO.

## 3. Phase B: Membrane & Projection Verification
Once the core is isolated, ggen must be wired to accept messy inputs (JSON/CSV) and adapt them to `RelationPage` structures. Simultaneously, the output of Genesis must be projected by ggen into OCEL and N-Quads, proving the system is useful to the outside world.

## 4. Phase C: Proof Surface Hardening
The true value of the architecture relies on its proof surfaces. Sabotage tests must be written to intentionally violate limits and ensure Refusal artifacts are correctly generated. The Replay Cursor must be proven functional on a million-tuple stream.

## 5. Phase D: External Integration
With projections working, external validators (`wasm4pm`, DuckDB, QLever, SHACL CLI) will be integrated directly into the CI/CD pipeline (the GALL checkpoints).

## 6. Observed vs Planned Matrix

| Objective | Planned Status | Observed Status | Action Required |
| :--- | :--- | :--- | :--- |
| **Core Architecture Docs** | Complete | Complete | N/A - Swarm docs generated. |
| **genesis-core Crate** | Isolated | Scattered (KNHK/ggen) | Execute WP-01. |
| **Data Algebra Limits** | Enforced | Unenforced | Execute WP-02, WP-03. |
| **Receipt Generation** | Hashing Packets | Partial | Execute WP-05. |
| **External Projections** | Validating | Unverified | Execute WP-06, WP-07. |

## 7. Next Steps
1. Review this Finish Plan and the associated Agent Work Queue.
2. Approve the Work Queue.
3. Dispatch an agent to execute **WP-01: Isolate `genesis-core`**.
