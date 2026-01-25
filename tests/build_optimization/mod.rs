//! Chicago TDD (Classicist School) Tests for Build Optimization
//!
//! This module provides comprehensive state-based tests for build optimization validation.
//! Tests verify:
//! - Cargo.toml profile configuration correctness
//! - Feature flag combinations and defaults
//! - Dependency resolution and consolidation
//! - Performance regression detection
//! - Binary compatibility validation
//!
//! All tests follow the AAA pattern (Arrange/Act/Assert) with real objects and state verification.

pub mod profiles;
pub mod feature_flags;
pub mod dependencies;
pub mod performance;
pub mod binary_compat;
