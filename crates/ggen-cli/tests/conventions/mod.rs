//! Conventions Tests Module
//!
//! Tests for conventional behaviors like watch mode, auto-regeneration, etc.
//! London TDD approach: Define contracts through mocks, verify interactions

pub mod watch_tests;
pub mod e2e_tests;

// V2.2.0 Convention-based routing tests
pub mod resolver_tests;
pub mod planner_tests;
pub mod integration_tests;
pub mod fixtures;
