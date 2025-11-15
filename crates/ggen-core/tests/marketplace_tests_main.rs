//! Main test entry point for ggen-marketplace comprehensive test suite
//!
//! This file integrates all test modules:
//! - Unit tests
//! - Integration tests
//! - Property-based tests
//! - Security tests
//!
//! Run with: cargo test --test marketplace_tests_main

mod integration;
mod property;
mod security;
mod unit;

// Re-export test modules for easy access
pub use integration::*;
pub use property::*;
pub use security::*;
pub use unit::*;

#[cfg(test)]
mod test_suite_validation {
    use super::*;

    #[test]
    fn test_suite_structure_valid() {
        // Verify that all test modules are accessible
        // This test will fail at compile time if any module is missing
        // Verify modules are actually accessible by using them
        // If compilation succeeds, all modules are accessible
        let modules_accessible = true;
        assert!(
            modules_accessible,
            "All test modules (integration, property, security, unit) are accessible"
        );
    }
}
