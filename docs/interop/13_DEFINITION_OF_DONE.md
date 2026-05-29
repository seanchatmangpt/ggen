# 13 DEFINITION OF DONE

This document formalizes the completion criteria for the Genesis-bearing interchangeable parts architecture across three horizons.

## 1. v0.1 Definition of Done

*   **Genesis Core Spec:** [IMPLEMENTED] `docs/interop/04_GENESIS_CORE_SPEC.md` exists and covers A=μ(O).
*   **ggen Foundry Spec:** [IMPLEMENTED] `docs/interop/05_GGEN_FOUNDRY_SPEC.md` exists.
*   **Interchangeable Part Spec:** [IMPLEMENTED] `docs/interop/06_PART_RUNTIME_SPEC.md` covers AtomVM/WASM boundaries.
*   **Pair2/RelationPage/Construct8 Algebra:** [PARTIAL] Documented in `docs/interop/09a_DATA_ALGEBRA_GALL.md`, but requires Rust `genesis-core` implementation evidence.
*   **Proof Surfaces:** [IMPLEMENTED] `docs/interop/07_PROOF_SURFACES_SPEC.md` covers receipts and refusal artifacts.
*   **External Validation:** [IMPLEMENTED] `docs/interop/09_EXTERNAL_VALIDATION_SPEC.md` documents QLever/DuckDB bounds.
*   **Process Interop:** [IMPLEMENTED] `docs/interop/08_PROCESS_INTELLIGENCE_SPEC.md` covers OCEL/wasm4pm.
*   **Public Vocabulary GALL:** [IMPLEMENTED] `docs/interop/10_PUBLIC_VOCABULARY_GALL.md` specifies `open-ontologies` checkpoints.
*   **Repo Claims Evidence-Backed:** [MISSING] Requires full source code scan (`src/**/*.rs`) matching architectural documents.
*   **No Broad Rename Performed:** [IMPLEMENTED] Naming taxonomy stable without unplanned broad renames.
*   **Final Doc Index:** [IMPLEMENTED] `docs/interop/00_INDEX.md` points to every artifact.

## 2. v0.2 Definition of Done

*   **Rust Validation:** [MISSING] `genesis-core` must compile and pass cargo tests for all Pair2 operations (`cargo test --package genesis-core`).
*   **Integration Boundary:** [MISSING] `ggen` must physically produce an AtomVM binary via `cargo build --target wasm32-wasi` and log receipts.
*   **Validation Path:** [MISSING] `pictl` or `wasm4pm` must parse a true runtime trace (OCEL 2.0) and assert Truex lifecycle promotion.
*   **Proof Surfaces:** [TEST_ONLY] `tests/receipt_validation.rs` must reject tampered Pair2 payloads.
*   **Finish Recommendation DoD:** All recommendations must have explicit measurable tests and CI gates assigned.

## 3. Vision 2030 Definition of Done

*   **Total Falsifiability:** [DOC_ONLY] All capabilities have strict Chicago TDD boundaries. No test doubles for boundary logic.
*   **Real Collaborators Only:** [DOC_ONLY] `tests/` explicitly asserts on OCEL logs or database row mutations, not memory structures.
*   **Strict Performance SLOs:** [DOC_ONLY] `cargo make slo-check` must execute and pass within predefined latency boundaries.
*   **E2E Boundary Crossing:** [MISSING] Every layer (Genesis -> ggen -> AtomVM -> WASM -> Truex -> wasm4pm) covered by a unified E2E integration test suite (`tests/e2e/`).

## Finish Recommendation: Complete the Core Extraction
*   **Recommendation:** Isolate Genesis pure logic from `ggen`.
*   **Definition of Done:** 
    1. `genesis-core` crate is independent.
    2. `cargo test --package genesis-core` passes.
    3. `src/ostar/` imports `genesis-core` without cyclic dependencies.
    4. Receipts are validated using `genesis-core`'s pure functions.
    5. No JSON processing occurs within `genesis-core`.
