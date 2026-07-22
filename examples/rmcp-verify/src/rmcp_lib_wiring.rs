// NOTE (same fix as mcpp-pack/cargo-cicd-pack/praxis-core-pack/star-toml-pack's
// own identical, independently-converged-on templates): this template
// targets a pack-unique file, not `to: src/lib.rs` with `inject: true` --
// that pattern hits this engine's own FM-WRITE-008 duplicate-output guard
// the moment a second pack sharing it is wired into the same real consumer.
// The consumer needs one `include!("rmcp_lib_wiring.rs");` line in their own
// `src/lib.rs` to mount it -- a real, disclosed remaining consumer-effort
// gap, not "consumer wires ggen.toml, done".

#[path = "rmcp_handler_catalog.rs"]
pub mod rmcp_handler_catalog;
#[path = "rmcp_macro_catalog.rs"]
pub mod rmcp_macro_catalog;
#[path = "rmcp_transport_catalog.rs"]
pub mod rmcp_transport_catalog;
#[path = "rmcp_feature_catalog.rs"]
pub mod rmcp_feature_catalog;
#[path = "rmcp_stub_handler.rs"]
pub mod rmcp_stub_handler;
