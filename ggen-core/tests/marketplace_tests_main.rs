//! Main test entry point for ggen-marketplace comprehensive test suite
//!
//! This file integrates all test modules:
//! - Unit tests
//! - Integration tests
//! - Property-based tests
//! - Security tests
//!
//! Run with: cargo test --test marketplace_tests_main

mod unit;
mod integration;
mod property;
mod security;

// Re-export test modules for easy access
pub use unit::*;
pub use integration::*;
pub use property::*;
pub use security::*;

#[cfg(test)]
mod test_suite_validation {
    #[test]
    fn test_suite_structure_valid() {
        // Verify that all test modules are accessible
        // This test will fail at compile time if any module is missing
        assert!(true);
    }
}
