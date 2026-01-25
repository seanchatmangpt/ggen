//! TPS (Toyota Production System) Integration Tests
//!
//! Comprehensive test suite for all TPS principle interactions:
//! - **Jidoka**: Autonomic response to quality issues
//! - **Kanban**: Flow optimization and WIP management
//! - **Andon**: Visibility and alerting
//! - **Kaizen**: Continuous improvement through monitoring
//!
//! Test Organization:
//! - `end_to_end_workflow_tests`: Complete payment → deployment → monitoring flows
//! - `smoke_tests`: Fast validation (<30s) for CI/CD gates
//! - `regression_tests`: Snapshot-based deterministic output verification
//! - `performance_regression_tests`: SLO validation with baseline metrics
//! - `chaos_tests`: Resilience under failure conditions
//! - `test_data_generation`: Realistic scenario data builders
//!
//! All tests use Chicago TDD pattern:
//! - AAA (Arrange/Act/Assert) structure
//! - Real objects, not mocks
//! - State-based verification
//! - Observable outputs and behavior

pub mod end_to_end_workflow_tests;
pub mod smoke_tests;
pub mod regression_tests;
pub mod performance_regression_tests;
pub mod chaos_tests;
pub mod test_data_generation;

// Re-export test data utilities for convenient access
pub use test_data_generation::{
    AllTestDataProvider, CrossPrincipleScenario, CrossPrincipleScenarioBuilder, DeploymentTestCase,
    DeploymentTestDataBuilder, FailureScenario, FailureScenarioBuilder, LoadDataPoint,
    LoadProfileBuilder, PaymentTestCase, PaymentTestDataBuilder, QueueWorkloadBuilder, WorkItem,
};

use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

/// Global test counter for tracking test execution
static TEST_EXECUTION_COUNT: AtomicUsize = AtomicUsize::new(0);

/// Increment test execution counter
pub fn increment_test_count() {
    TEST_EXECUTION_COUNT.fetch_add(1, Ordering::SeqCst);
}

/// Get current test execution count
pub fn get_test_count() -> usize {
    TEST_EXECUTION_COUNT.load(Ordering::SeqCst)
}

/// Test environment configuration
#[derive(Debug, Clone)]
pub struct TestEnvironment {
    pub enable_logging: bool,
    pub enable_metrics: bool,
    pub enable_tracing: bool,
    pub timeout_secs: u64,
}

impl Default for TestEnvironment {
    fn default() -> Self {
        Self {
            enable_logging: true,
            enable_metrics: true,
            enable_tracing: true,
            timeout_secs: 30,
        }
    }
}

impl TestEnvironment {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_timeout(mut self, secs: u64) -> Self {
        self.timeout_secs = secs;
        self
    }

    pub fn with_logging(mut self, enabled: bool) -> Self {
        self.enable_logging = enabled;
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_data_provider_comprehensive() {
        let report = AllTestDataProvider::generate_report();

        assert!(
            report.payment_cases > 0,
            "Should have comprehensive payment test data"
        );
        assert!(
            report.deployment_cases > 0,
            "Should have comprehensive deployment test data"
        );
        assert!(
            report.queue_items > 0,
            "Should have comprehensive queue test data"
        );
        assert!(
            report.failure_scenarios > 0,
            "Should have comprehensive failure scenarios"
        );
    }

    #[test]
    fn test_environment_configuration() {
        let env = TestEnvironment::new()
            .with_timeout(60)
            .with_logging(false);

        assert_eq!(env.timeout_secs, 60);
        assert!(!env.enable_logging);
        assert!(env.enable_metrics);
    }
}
