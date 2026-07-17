//! Pack Receipt Generation (CLI adapter).
//!
//! The authoritative pack-install receipt logic now lives in
//! [`crate::agent::receipt`] (root-parameterized and shared with the agent /
//! MCP surface; ported from `ggen_core::agent::receipt` in task T041 — see that
//! module's doc comment). This module is a thin CLI adapter: it preserves the
//! [`PackInstallClosure`] / [`generate_pack_install_receipt`] surface that
//! `pack add` already calls, and delegates the actual signing to the single
//! authoritative implementation. There is therefore exactly one place that
//! decides what a lawful pack receipt looks like — the CLI and the agent surface
//! cannot emit divergent receipts.

use std::path::PathBuf;

// Re-export the authoritative closure + error types so existing call sites
// (`crate::cmds::packs_receipt::PackInstallClosure`) keep compiling unchanged.
pub use crate::agent::{PackInstallClosure, PackReceiptError};

/// Result type for pack receipt operations (alias over the core error).
pub type Result<T> = std::result::Result<T, PackReceiptError>;

/// Generate a cryptographic receipt for a SUCCESSFUL pack installation, rooted
/// at the current working directory (the project root for a CLI `pack add`).
///
/// This delegates to [`crate::agent::emit_install_receipt`]; see that
/// function for the fail-closed contract (an empty digest is refused) and the
/// input/output-hash closure binding.
pub fn generate_pack_install_receipt(closure: &PackInstallClosure<'_>) -> Result<PathBuf> {
    let root = std::env::current_dir().map_err(|e| {
        PackReceiptError::Runtime(format!("Failed to resolve project directory: {}", e))
    })?;
    crate::agent::emit_install_receipt(&root, closure)
}
