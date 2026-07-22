//! `cargo-cicd-verify` — committed, real consumer of `cargo-cicd-pack`.
//!
//! Wires the pack's generated command catalog
//! (`src/cargo_cicd_catalog.rs`) and its real `std::process::Command`
//! dispatch shim (`src/cargo_cicd_dispatch.rs`, closing the handler-gap
//! dimension named in `.specify/maturity.ttl`'s `cg_010` row). See `tests/`
//! for the actual proof; the pack's own consumer contract (see the header
//! comment of the generated `src/cargo_cicd_lib_wiring.rs`) is exactly one
//! permanent `include!` line here.

include!("cargo_cicd_lib_wiring.rs");
