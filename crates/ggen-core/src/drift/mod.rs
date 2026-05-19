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
//! use mcpp_core::drift::{DriftDetector, DriftStatus};
//! use std::path::Path;
//!
//! # fn main() -> mcpp_utils::error::Result<()> {
//! let detector = DriftDetector::new(Path::new(".mcpp"))?;
//! let status = detector.check_drift(
//!     Path::new("ontology.ttl"),
//!     Path::new("mcpp.toml"),
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

pub use detector::{ChangeType, DriftChange, DriftDetector, DriftStatus};
pub use sync_state::{FileHashState, SyncState};
