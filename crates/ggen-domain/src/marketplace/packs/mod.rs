//! GGEN Packs Commands
//!
//! Commands for intelligent package management across the system.
//! Each command is in its own module file following the noun-verb pattern.
//!
//! # Command Categories
//!
//! **Discovery Commands:**
//! - `list` - Show all installed packages with filtering/sorting
//! - `search` - Find packages by name/description/author
//! - `info` - Show detailed package information
//! - `stats` - Show adoption metrics and trends
//!
//! **Management Commands:**
//! - `install` - Install a package
//! - `uninstall` - Remove a package
//! - `update` - Update package(s)
//!
//! **Manifest Commands:**
//! - `manifest` - Manage dependency manifests (lock versions, apply, compare)
//!
//! **Compliance Commands:**
//! - `audit` - Show operation history
//! - `verify` - Check consistency
//! - `security_scan` - Find vulnerabilities
//! - `deprecate` - Mark packages obsolete
//!
//! **Analytics Commands:**
//! - `dependents` - Find packages that depend on a package
//! - `stats` - Show adoption trends

pub mod list;

pub use list::ListPackagesOutput;
