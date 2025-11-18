//! Pack Installation System for ggen v4.0
//!
//! This module implements Phase 1 of the Pack Installation System roadmap,
//! providing core data structures and serialization for pack management.
//!
//! ## Overview
//!
//! The Pack Installation System enables:
//! - Installing packs from multiple sources (Registry, GitHub, Local)
//! - Tracking installed packs with versions and dependencies
//! - Verifying pack integrity with checksums
//! - Managing pack dependencies and preventing circular references
//!
//! ## Modules
//!
//! - [`lockfile`] - Pack lockfile management (`.ggen/packs.lock`)
//! - [`install`] - Pack installation logic
//!
//! ## Quick Start
//!
//! ```rust
//! use ggen_core::packs::lockfile::{PackLockfile, LockedPack, PackSource};
//! use chrono::Utc;
//!
//! // Create a new lockfile
//! let mut lockfile = PackLockfile::new("4.0.0");
//!
//! // Add a pack from registry
//! let pack = LockedPack {
//!     version: "1.0.0".to_string(),
//!     source: PackSource::Registry {
//!         url: "https://registry.ggen.io".to_string()
//!     },
//!     integrity: Some("sha256-abc123".to_string()),
//!     installed_at: Utc::now(),
//!     dependencies: vec![],
//! };
//!
//! lockfile.add_pack("io.ggen.rust.cli", pack);
//! ```

pub mod install;
pub mod lockfile;

// Re-export commonly used types
pub use install::{install_pack, PackInstallResult};
pub use lockfile::{LockedPack, PackLockfile, PackSource};
