//! Pack installation logic for Phase 1
//!
//! This module provides type definitions for pack installation.
//! The actual installer implementation is in `ggen-marketplace/src/install.rs`.

// NOTE: The real installer is in ggen-marketplace/src/install.rs with full
// cryptographic verification, caching, and pack extraction support.
// This module only provides type definitions for compatibility.

use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Result of pack installation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackInstallResult {
    /// Pack identifier that was installed
    pub pack_id: String,
    /// Version that was installed
    pub version: String,
    /// Number of packages in this pack
    pub packages_installed: usize,
    /// Path to the lockfile
    pub lockfile_path: PathBuf,
    /// Human-readable confirmation message
    pub message: String,
}

/// Install a pack (stub - redirects to ggen-marketplace)
///
/// This is a compatibility stub. The real implementation is in
/// `ggen-marketplace::install::Installer` with full cryptographic verification.
pub async fn install_pack(
    _pack_id: &str,
    _version: Option<&str>,
    _project_dir: &std::path::Path,
    _force: bool,
) -> Result<PackInstallResult> {
    // This is a stub. The real installer is in ggen-marketplace.
    // Use `ggen_marketplace::install::Installer` instead.
    ggen_utils::bail!(
        "install_pack is a stub. Use ggen_marketplace::install::Installer instead."
    )
}
