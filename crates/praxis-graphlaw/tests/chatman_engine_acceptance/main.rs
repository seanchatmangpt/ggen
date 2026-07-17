//! Integration-test target for the chatman engine acceptance harness.
//!
//! Cargo only builds `tests/*.rs` and `tests/*/main.rs`; this file exists so
//! `harness/` and `properties.rs` compile and run as one test target named
//! `chatman_engine_acceptance`. Generated per-suite fixtures and the engine
//! wiring land in later CE-* lanes; this file stays a module ledger.

pub mod harness;
mod properties;
