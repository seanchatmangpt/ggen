//! Unit tests for v2.0 architecture components
//!
//! Tests pure functions without external dependencies:
//! - runtime::execute (async→sync bridge)
//! - Error handling utilities
//! - Argument parsing and validation
//!
//! Target: <100ms total, <10ms per test

pub mod runtime_bridge_test;
pub mod error_handling_test;
