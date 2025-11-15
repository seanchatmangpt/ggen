//! Chicago TDD (Classicist School) tests
//!
//! These tests verify actual behavior with real objects and state,
//! following the Classicist School of TDD.

pub mod marketplace;
pub mod ontology_driven_e2e;
pub mod expert_patterns;

// Core team major tests - high-impact production readiness tests
#[path = "core_team_major_tests.rs"]
pub mod core_team_major_tests;
