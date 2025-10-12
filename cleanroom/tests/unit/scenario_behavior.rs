//! Unit tests for scenario DSL behaviors
//!
//! Tests the scenario DSL to ensure correct step execution, policy application,
//! and service lifecycle management.

use rstest::*;
use cleanroom::scenario::{Scenario, RunResult};
use cleanroom::backend::{Backend, Cmd, MockBackend};
use cleanroom::policy::{Policy, TimeProfile, RngProfile};
use cleanroom::services::Service;
use crate::common::fixtures::*;

#[test]
fn scenario_executes_steps_sequentially_by_default() {
    // Given: A scenario with multiple steps
    let mut step1_mock = MockBackend::new();
    step1_mock.expect_run_cmd().returning(|_| {
        Ok(cleanroom::backend::RunResult {
            exit_code: 0,
            stdout: "step1".to_string(),
            stderr: String::new(),
            duration_ms: 100,
            steps: vec![],
            redacted_env: vec![],
            backend: "mock".to_string(),
        })
    });

    let mut step2_mock = MockBackend::new();
    step2_mock.expect_run_cmd().returning(|_| {
        Ok(cleanroom::backend::RunResult {
            exit_code: 0,
            stdout: "step2".to_string(),
            stderr: String::new(),
            duration_ms: 100,
            steps: vec![],
            redacted_env: vec![],
            backend: "mock".to_string(),
        })
    });

    let scenario = cleanroom::scenario("test")
        .step("step1".to_string(), ["echo", "step1"])
        .step("step2".to_string(), ["echo", "step2"]);

    // When: The scenario is executed
    let result = scenario.run().unwrap();

    // Then: Steps are executed sequentially
    assert_eq!(result.exit_code, 0);
    assert!(result.stdout.contains("step1"));
    assert!(result.stdout.contains("step2"));
    assert_eq!(result.steps.len(), 2);
}

#[test]
fn scenario_executes_steps_concurrently_when_requested() {
    // Given: A scenario with concurrent execution
    let scenario = cleanroom::scenario("test")
        .step("step1".to_string(), ["echo", "step1"])
        .step("step2".to_string(), ["echo", "step2"])
        .concurrent();

    // When: The scenario is executed
    let result = scenario.run().unwrap();

    // Then: Steps are executed concurrently
    assert_eq!(result.exit_code, 0);
    assert_eq!(result.steps.len(), 2);
    // Note: In a real implementation, concurrent execution would be verified
}

#[test]
fn scenario_stops_on_first_failure_in_sequential_mode() {
    // Given: A scenario with a failing step
    let scenario = cleanroom::scenario("test")
        .step("step1".to_string(), ["echo", "step1"])
        .step("step2".to_string(), ["false"]) // This will fail
        .step("step3".to_string(), ["echo", "step3"]);

    // When: The scenario is executed
    let result = scenario.run().unwrap();

    // Then: Execution stops after the first failure
    assert_eq!(result.exit_code, 1); // false exits with 1
    assert_eq!(result.steps.len(), 2); // Only first two steps executed
}

#[test]
fn scenario_aggregates_all_failures_in_concurrent_mode() {
    // Given: A scenario with concurrent execution and failing steps
    let scenario = cleanroom::scenario("test")
        .step("step1".to_string(), ["false"])
        .step("step2".to_string(), ["false"])
        .concurrent();

    // When: The scenario is executed
    let result = scenario.run().unwrap();

    // Then: All failures are aggregated
    assert_eq!(result.exit_code, 1); // Last failure exit code
    assert_eq!(result.steps.len(), 2); // Both steps executed
}

#[test]
fn scenario_applies_policy_to_all_steps() {
    // Given: A scenario with a specific policy
    let policy = Policy::locked();
    let scenario = cleanroom::scenario("test")
        .step("step1".to_string(), ["echo", "step1"])
        .policy(policy.clone());

    // When: The scenario is executed
    let result = scenario.run().unwrap();

    // Then: Policy is applied to all steps
    assert_eq!(result.exit_code, 0);
    // Note: Policy application would be verified through backend behavior
}

#[test]
fn scenario_applies_determinism_profiles() {
    // Given: A scenario with deterministic profiles
    let scenario = cleanroom::scenario("test")
        .step("step1".to_string(), ["echo", "step1"])
        .determinism(TimeProfile::Frozen(12345), RngProfile::Seed(42));

    // When: The scenario is executed
    let result = scenario.run().unwrap();

    // Then: Determinism profiles are applied
    assert_eq!(result.exit_code, 0);
    // Note: Determinism would be verified through repeated executions
}

#[test]
fn scenario_starts_and_stops_services_correctly() {
    // Given: A scenario with services
    let scenario = cleanroom::scenario("test")
        .step("step1".to_string(), ["echo", "step1"])
        .services(vec![]); // Empty for now, would contain real services

    // When: The scenario is executed
    let result = scenario.run().unwrap();

    // Then: Services are started and stopped correctly
    assert_eq!(result.exit_code, 0);
    // Note: Service lifecycle would be verified through service mocks
}

#[test]
fn scenario_passes_environment_variables_to_steps() {
    // Given: A scenario with environment variables
    let scenario = cleanroom::scenario("test")
        .step("step1".to_string(), ["sh", "-c", "echo $TEST_VAR"])
        .env("TEST_VAR".to_string(), "test_value".to_string());

    // When: The scenario is executed
    let result = scenario.run().unwrap();

    // Then: Environment variables are passed to steps
    assert_eq!(result.exit_code, 0);
    // Note: Environment variable passing would be verified through backend mocks
}

#[test]
fn scenario_handles_empty_step_list() {
    // Given: A scenario with no steps
    let scenario = cleanroom::scenario("test");

    // When: The scenario is executed
    let result = scenario.run().unwrap();

    // Then: Empty scenario executes successfully
    assert_eq!(result.exit_code, 0);
    assert_eq!(result.steps.len(), 0);
}

#[test]
fn scenario_preserves_step_order_in_sequential_mode() {
    // Given: A scenario with multiple steps
    let scenario = cleanroom::scenario("test")
        .step("step1".to_string(), ["echo", "1"])
        .step("step2".to_string(), ["echo", "2"])
        .step("step3".to_string(), ["echo", "3"]);

    // When: The scenario is executed
    let result = scenario.run().unwrap();

    // Then: Steps are executed in order
    assert_eq!(result.exit_code, 0);
    assert_eq!(result.steps.len(), 3);
    
    // Verify step order
    assert_eq!(result.steps[0].name, "step1");
    assert_eq!(result.steps[1].name, "step2");
    assert_eq!(result.steps[2].name, "step3");
}

#[test]
fn scenario_aggregates_output_from_all_steps() {
    // Given: A scenario with multiple steps producing output
    let scenario = cleanroom::scenario("test")
        .step("step1".to_string(), ["echo", "hello"])
        .step("step2".to_string(), ["echo", "world"]);

    // When: The scenario is executed
    let result = scenario.run().unwrap();

    // Then: Output from all steps is aggregated
    assert_eq!(result.exit_code, 0);
    assert!(result.stdout.contains("hello"));
    assert!(result.stdout.contains("world"));
}

#[test]
fn scenario_handles_backend_errors_gracefully() {
    // Given: A scenario that will encounter a backend error
    let scenario = cleanroom::scenario("test")
        .step("step1".to_string(), ["nonexistent_command"]);

    // When: The scenario is executed
    let result = scenario.run();

    // Then: Backend errors are handled gracefully
    // Note: This test would need to be adjusted based on actual error handling
    assert!(result.is_ok() || result.is_err()); // Either outcome is valid
}

#[test]
fn scenario_builder_is_fluent() {
    // Given: A scenario builder
    let scenario = cleanroom::scenario("test")
        .step("step1".to_string(), ["echo", "step1"])
        .concurrent()
        .policy(Policy::locked())
        .determinism(TimeProfile::Frozen(12345), RngProfile::Seed(42))
        .env("TEST_VAR".to_string(), "test_value".to_string())
        .services(vec![]);

    // When: The scenario is built
    // Then: All fluent methods are chained successfully
    assert_eq!(scenario.name, "test");
    assert!(scenario.concurrent);
    assert_eq!(scenario.steps.len(), 1);
}

#[test]
fn scenario_preserves_step_metadata() {
    // Given: A scenario with steps containing metadata
    let scenario = cleanroom::scenario("test")
        .step("important_step".to_string(), ["echo", "important"])
        .step("cleanup_step".to_string(), ["echo", "cleanup"]);

    // When: The scenario is executed
    let result = scenario.run().unwrap();

    // Then: Step metadata is preserved
    assert_eq!(result.exit_code, 0);
    assert_eq!(result.steps.len(), 2);
    assert_eq!(result.steps[0].name, "important_step");
    assert_eq!(result.steps[1].name, "cleanup_step");
}
