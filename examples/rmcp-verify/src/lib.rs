//! `rmcp-verify` — committed, real consumer of `rmcp-pack`.
//!
//! Wires the pack's generated catalogs (`src/rmcp_handler_catalog.rs`,
//! `src/rmcp_macro_catalog.rs`, `src/rmcp_transport_catalog.rs`,
//! `src/rmcp_feature_catalog.rs`) and its real, compiling
//! `impl rmcp::ServerHandler` (`src/rmcp_stub_handler.rs`) and, via
//! `cargo test`, both of its proof tests
//! (`tests/rmcp_handler_catalog_proof.rs`, hand-transcribed;
//! `tests/rmcp_stub_handler_proof.rs`, real-behavior) against the actual
//! `rmcp` crate rather than only checking this pack's generated artifacts
//! agree with each other. See `tests/` for the actual proof; the pack's own
//! consumer contract (see the header comment of the generated
//! `src/rmcp_lib_wiring.rs`) is exactly one permanent `include!` line here.

include!("rmcp_lib_wiring.rs");
