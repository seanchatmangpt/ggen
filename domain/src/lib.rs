//! Domain Layer - Business Logic
//!
//! This module contains async business logic implementations.
//! Each domain module is called by the commands layer via runtime_helper.
//!
//! All async functions return Result<T, String> for compatibility with runtime_helper.

pub mod hook;

// Re-export commonly used types
pub mod prelude {
    // Domain layer uses Result<T, String> for runtime_helper compatibility
    pub type Result<T> = std::result::Result<T, String>;
}
