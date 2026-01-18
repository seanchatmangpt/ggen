//! Drift Detection Module
//!
//! Detects when ontology changes make generated code stale by tracking
//! SHA256 hashes of ontology files, manifests, and inference rules.
//!
//! ## Overview
//!
//! The drift detector:
//! - Stores SHA256 hashes after each sync
//! - Compares current hashes against stored state
//! - Warns users when ontology changes since last sync
//! - Tracks timestamps for age-based warnings
//!
//! ## Usage
//!
//! ```rust,no_run
//! use ggen_core::drift::{DriftDetector, DriftStatus};
//! use std::path::Path;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let detector = DriftDetector::new(Path::new(".ggen"))?;
//! let status = detector.check_drift(
//!     Path::new("ontology.ttl"),
//!     Path::new("ggen.toml"),
//! )?;
//!
//! match status {
//!     DriftStatus::Clean => println!("No drift detected"),
//!     DriftStatus::Drifted { changes } => {
//!         for change in changes {
//!             eprintln!("⚠️  {}", change);
//!         }
//!     }
//! }
//! # Ok(())
//! # }
//! ```
//!
//! ## Constitutional Requirements
//!
//! - No unwrap/expect in production code
//! - Result<T,E> throughout
//! - Non-blocking warnings (don't fail commands)
//! - Performance: drift check <100ms

pub mod detector;
pub mod sync_state;

pub use detector::{DriftDetector, DriftStatus, DriftChange, ChangeType};
pub use sync_state::{SyncState, FileHashState};
