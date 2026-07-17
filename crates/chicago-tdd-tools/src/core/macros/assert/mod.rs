//! Assertion Macros for Chicago TDD Testing
//!
//! Comprehensive assertion macros organized by category for better maintainability.
//!
//! # Modules
//!
//! - [`result`] - Result assertions (`assert_ok`, `assert_err`, `assert_fail`)
//! - [`equality`] - Equality assertions (`assert_eq_msg`, `assert_eq_enhanced`, `assert_approx_eq`)
//! - [`collections`] - Collection assertions (`assert_contains`, `assert_not_contains`, `assert_subset`, `assert_superset`) - v1.3.0
//! - [`json`] - JSON assertions (`assert_json_eq`) - v1.3.0
//! - [`patterns`] - Pattern matching assertions (`assert_matches`) - v1.3.0
//! - [`performance`] - Performance and constraint assertions (`assert_within_tick_budget`, `assert_in_range`, `assert_guard_constraint`)
//!
//! # Organization
//!
//! This module is organized into logical sub-modules for better maintainability:
//! - Result assertions handle `Result<T, E>` testing
//! - Equality assertions provide enhanced comparison capabilities
//! - Collection assertions (v1.3.0) simplify common collection testing scenarios
//! - JSON assertions (v1.3.0) provide semantic JSON comparison
//! - Pattern assertions (v1.3.0) enable pattern matching in tests
//! - Performance assertions validate timing and constraint compliance
//!
//! All macros are re-exported at the crate root for backward compatibility.

// Result assertions
pub mod result;

// Equality assertions
pub mod equality;

// Collection assertions (v1.3.0)
pub mod collections;

// JSON assertions (v1.3.0)
pub mod json;

// Pattern matching assertions (v1.3.0)
pub mod patterns;

// Performance and constraint assertions
pub mod performance;

// Re-export all assertion macros for convenient access
// These are already exported by the individual modules via #[macro_export],
// so they're available at the crate root. This module just organizes them.
