//! Advanced Rust API - Production-ready REST API
//!
//! Generated with ggen lifecycle + AI-powered code generation

pub mod error;

// Re-export main types
pub use error::AppError;

/// Application result type
pub type Result<T> = std::result::Result<T, AppError>;
