//! Package storage implementations
//!
//! This module provides implementations of the `PackageStore` trait for
//! content-addressable storage of package binaries. Supports both filesystem
//! and in-memory storage backends.
//!
//! ## Features
//!
//! - **Content-addressable**: Packages stored by content hash (SHA-256)
//! - **Streaming support**: Efficient streaming for large files
//! - **Metadata tracking**: Content metadata (size, type, creation time)
//! - **Multiple backends**: Filesystem and in-memory implementations
//! - **Deduplication**: Automatic deduplication via content addressing
//! - **Integrity verification**: Hash-based content verification
//!
//! ## Implementations
//!
//! - **FilesystemStore**: Persistent storage on local filesystem
//!   - Two-level directory structure for efficient access
//!   - Automatic directory creation
//!   - Metadata storage
//! - **MemoryStore**: In-memory storage for testing and caching
//!   - Fast access with no I/O
//!   - Thread-safe concurrent access
//!   - Ephemeral storage
//!
//! ## Examples
//!
//! ### Using Filesystem Storage
//!
//! ```rust,no_run
//! use ggen_marketplace::storage::FilesystemStore;
//! use std::path::PathBuf;
//!
//! # async fn example() -> anyhow::Result<()> {
//! let store = FilesystemStore::new(PathBuf::from("/tmp/storage")).await?;
//! let content = b"package content";
//! let content_id = store.store(content).await?;
//! # Ok(())
//! # }
//! ```
//!
//! ### Using Memory Storage
//!
//! ```rust,no_run
//! use ggen_marketplace::storage::MemoryStore;
//!
//! # async fn example() -> anyhow::Result<()> {
//! let store = MemoryStore::new();
//! let content = b"test content";
//! let content_id = store.store(content).await?;
//! # Ok(())
//! # }
//! ```

mod filesystem;
mod memory;

pub use filesystem::FilesystemStore;
pub use memory::MemoryStore;
