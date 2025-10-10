//! Error demonstration and testing functionality.
//!
//! This module provides a simple command to demonstrate error handling
//! and testing scenarios in the GGen CLI.

/// Demonstrate error handling by returning a simulated error
pub fn run() -> ggen_utils::error::Result<()> {
    // For demo purposes, just return an error
    Err(ggen_utils::error::Error::new("Simulated error"))
}
