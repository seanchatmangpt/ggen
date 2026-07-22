//! `praxis-core-verify` — committed, real consumer of `praxis-core-pack`.
//!
//! Wires the pack's generated refusal-taxonomy table
//! (`src/praxis_core_refusal_table.rs`) and, with the (default-on)
//! `praxis-core-live` feature, compiles and runs its proof tests against
//! the REAL `praxis-core` crate rather than only checking this pack's two
//! generated artifacts agree with each other. See `tests/` for the actual
//! proof; the pack's own consumer contract (see the header comment of the
//! generated `src/praxis_core_lib_wiring.rs`) is exactly one permanent
//! `include!` line here.

include!("praxis_core_lib_wiring.rs");
