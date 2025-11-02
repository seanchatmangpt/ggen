//! Chicago TDD Refactoring Validation Suite
//!
//! Validates that v1→v2 refactoring preserves all functionality.
//!
//! APPROACH: Chicago TDD (Classicist School)
//! - Test REAL refactored code, not mocks
//! - Verify actual behavior and state
//! - Test end-to-end workflows
//! - Only mock external APIs
//!
//! TEST SUITES:
//! 1. Regression Tests - Ensure v1 functionality still works
//! 2. Migration Tests - Validate v2 architecture works
//! 3. Integration Tests - End-to-end workflows
//! 4. Performance Tests - No regressions

mod refactoring_validation {
    pub mod regression;
    pub mod migration;
    pub mod integration;
    pub mod performance;
    pub mod helpers;
}
