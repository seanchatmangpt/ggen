//! `wasm4pm-verify` — committed, real consumer of `wasm4pm-pack`.
//!
//! Wires the pack's generated crate-catalog (`src/wasm4pm_crate_catalog.rs`)
//! and dependency-graph (`src/wasm4pm_crate_deps.rs`) modules via the pack's
//! own `include!` contract (see the header comment of the generated
//! `src/wasm4pm_crate_lib_wiring.rs`), exercised by all three proof suites
//! under `tests/`: the two pre-existing hand-transcribed proofs
//! (`wasm4pm_crate_catalog_proof.rs`, `wasm4pm_crate_deps_proof.rs`) and the
//! new SPARQL-derived catalog proof
//! (`wasm4pm_crate_catalog_sparql_derived_proof.rs`).

include!("wasm4pm_crate_lib_wiring.rs");
