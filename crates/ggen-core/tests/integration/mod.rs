//! Integration tests for ggen-marketplace and lifecycle
//!
//! These tests validate end-to-end functionality including:
//! - Complete package publish/retrieve flows
//! - Multi-node registry scenarios
//! - Search engine integration with real tantivy index
//! - Clnrm-based test harness for isolated testing
//! - Lifecycle phase transitions and deployment workflows
//! - Production readiness validation

pub mod clnrm_harness;
pub mod clnrm_harness_examples;
pub mod end_to_end_flow;
pub mod marketplace_validation;
pub mod multi_node_scenario;
pub mod registry_api_integration;
pub mod search_integration;

// Lifecycle integration tests
pub mod lifecycle_clnrm_tests;
pub mod lifecycle_tests;

// Performance benchmarks
pub mod performance_benchmarks;
