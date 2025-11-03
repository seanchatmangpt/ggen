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
pub mod search_integration;
pub mod multi_node_scenario;
pub mod registry_api_integration;
pub mod marketplace_validation;
pub mod marketplace_p2p_tests;

// Lifecycle integration tests
pub mod lifecycle_tests;
pub mod lifecycle_clnrm_tests;

// Performance benchmarks
pub mod performance_benchmarks;
