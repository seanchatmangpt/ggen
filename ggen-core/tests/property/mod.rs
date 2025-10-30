//! Property-based tests for ggen-marketplace
//!
//! These tests use proptest to verify properties that should hold
//! for all valid inputs, catching edge cases and invariant violations.

pub mod package_properties;
pub mod search_properties;
pub mod version_properties;
pub mod serialization_properties;
