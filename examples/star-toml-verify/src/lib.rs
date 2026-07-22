//! `star-toml-verify` — committed, real consumer of `star-toml-pack`.
//!
//! Wires the pack's generated config-admission module
//! (`src/star_toml_config.rs`) and, via `cargo test`, both of its proof
//! tests (`tests/star_toml_config_proof.rs`, hand-transcribed;
//! `tests/star_toml_config_sparql_derived_proof.rs`, SPARQL-derived)
//! against the real `star-toml` crate rather than only checking this
//! pack's generated artifacts agree with each other. See `tests/` for the
//! actual proof; the pack's own consumer contract (see the header comment
//! of the generated `src/star_toml_lib_wiring.rs`) is exactly one
//! permanent `include!` line here.

include!("star_toml_lib_wiring.rs");
