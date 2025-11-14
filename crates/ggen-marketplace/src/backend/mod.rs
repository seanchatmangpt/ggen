//! Registry backend implementations
//!
//! This module provides implementations of the `Registry` trait for different
//! storage backends. Currently supports local file-system based registries
//! with plans for remote registry support.
//!
//! ## Implementations
//!
//! - **LocalRegistry**: File-system based registry for offline-first operation
//!   - JSON-based package index
//!   - Thread-safe concurrent access
//!   - Version management
//!   - Search and discovery
//!
//! ## Examples
//!
//! ### Using Local Registry
//!
//! ```rust,no_run
//! use ggen_marketplace::backend::LocalRegistry;
//! use std::path::PathBuf;
//!
//! # async fn example() -> anyhow::Result<()> {
//! let registry = LocalRegistry::new(PathBuf::from("~/.ggen/registry")).await?;
//! let packages = registry.list_packages().await?;
//! # Ok(())
//! # }
//! ```

mod local;
pub use local::LocalRegistry;
