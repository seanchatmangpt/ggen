//! # ggen-utils - Shared utilities for ggen project
//!
//! This crate provides common utilities used across the ggen codebase, including:
//! - Error handling types and utilities
//! - Application configuration management
//! - Logging infrastructure
//! - Alert system for critical notifications
//! - Project configuration types
//! - Time utilities
//! - Type definitions and helpers
//! - User level management
//!
//! ## Examples
//!
//! ### Error Handling
//!
//! ```rust,no_run
//! use ggen_utils::error::Result;
//! use ggen_utils::error::Error;
//!
//! fn process_data() -> Result<()> {
//!     // Operations that may fail
//!     Ok(())
//! }
//!
//! # fn main() -> Result<()> {
//! process_data()?;
//! # Ok(())
//! # }
//! ```
//!
//! ### Configuration
//!
//! ```rust,no_run
//! use ggen_utils::app_config::AppConfig;
//!
//! let config = AppConfig::load()?;
//! println!("Config loaded: {:?}", config);
//! # Ok::<(), Box<dyn std::error::Error>>(())
//! ```

// backtrace is stable since 1.65.0, no feature flag needed

#![deny(warnings)] // Poka-Yoke: Prevent warnings at compile time - compiler enforces correctness

pub mod alert;
pub mod app_config;
pub mod cli;
pub mod enhanced_error;
pub mod error;
pub mod logger;
pub mod path_validator;
pub mod project_config;
pub mod safe_path;
pub mod time;
pub mod types;
pub mod user_level;
pub mod versioning;

// Re-export error handling utilities
// Note: bail! and ensure! macros are exported via #[macro_export] in error.rs
// They are available as ggen_utils::bail! and ggen_utils::ensure!
pub use error::{Context, Error, Result};

// Re-export SafePath for easy migration from PathBuf
// Note: For enterprise-grade validation with workspace bounds, use path_validator::PathValidator
pub use safe_path::SafePath;
