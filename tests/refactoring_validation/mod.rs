//! Refactoring Validation Test Suite
//!
//! This module organizes all refactoring validation tests using Chicago TDD.
//!
//! Test Structure:
//! - helpers: Shared test utilities
//! - regression: v1 functionality preservation tests
//! - migration: v2 architecture validation tests
//! - integration: End-to-end workflow tests
//! - performance: Performance regression tests

pub mod helpers;
pub mod regression;
pub mod migration;
pub mod integration;
pub mod performance;
