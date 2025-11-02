// End-to-end validation for ggen v2.0.0
// Chicago TDD approach: Test REAL workflows, not mocks

pub mod complete_user_journey;
pub mod rdf_template_workflow;
pub mod marketplace_discovery;
pub mod error_handling;
pub mod multilang_generation;
pub mod rdf_query_workflow;
pub mod template_versioning;
pub mod deterministic_output;
pub mod performance_validation;

// Shared test utilities
pub mod test_helpers;
