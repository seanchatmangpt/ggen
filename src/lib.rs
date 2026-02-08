//! ggen - Ontology-Driven Code Generation
//!
//! This is a workspace crate that provides unified access to all ggen modules.
//! The actual implementation is in the workspace crates under `crates/`.
//!
//! For the CLI tool, use `ggen-cli-lib`.
//! For the core library, use `ggen-core`.

// Re-export core functionality
pub use ggen_core as core;

/// Workspace version information
pub const VERSION: &str = env!("CARGO_PKG_VERSION");
