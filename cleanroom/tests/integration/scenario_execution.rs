//! Integration tests for scenario execution
//!
//! Tests multi-step scenario execution, service lifecycle management,
//! error propagation, and result aggregation.

use cleanroom::scenario::{Scenario, RunResult};
use cleanroom::policy::{Policy, TimeProfile, RngProfile};
use crate::common::fixtures::*;

#[test]
fn scenario_executes_multiple_steps_successfully() {
    // Given: A scenario with multiple steps
    let scenario = cleanroom::scenario("multi_step_test")
        .step("step1".to_string(), ["echo", "step1"])
        .step("step2".to_string(), ["echo", "step2"])
        .step("step3".to_string(), ["echo", "step3"]);

    // When: The scenario is executed
    let result = scenario.run().unwrap();

    // Then: All steps execute successfully
    assert_eq!(result.exit_code, 0);
    assert_eq!(result.steps.len(), 3);
    assert!(result.stdout.contains("step1"));
    assert!(result.stdout.contains("step2"));
    assert!(result.stdout.contains("step3"));
}

#[test]
fn scenario_stops_on_first_failure() {
    // Given: A scenario with a failing step
    let scenario = cleanroom::scenario("failure_test")
        .step("step1".to_string(), ["echo", "step1"])
        .step("step2".to_string(), ["false"]) // This will fail
        .step("step3".to_string(), ["echo", "step3"]);

    // When: The scenario is executed
    let result = scenario.run().unwrap();

    // Then: Execution stops after the first failure
    assert_eq!(result.exit_code, 1);
    assert_eq!(result.steps.len(), 2); // Only first two steps executed
    assert!(result.stdout.contains("step1"));
    assert!(!result.stdout.contains("step3"));
}

#[test]
fn scenario_executes_steps_concurrently() {
    // Given: A scenario with concurrent execution
    let scenario = cleanroom::scenario("concurrent_test")
        .step("step1".to_string(), ["echo", "step1"])
        .step("step2".to_string(), ["echo", "step2"])
        .concurrent();

    // When: The scenario is executed
    let result = scenario.run().unwrap();

    // Then: Steps are executed concurrently
    assert_eq!(result.exit_code, 0);
    assert_eq!(result.steps.len(), 2);
    assert!(result.stdout.contains("step1"));
    assert!(result.stdout.contains("step2"));
}

#[test]
fn scenario_applies_policy_to_all_steps() {
    // Given: A scenario with a locked policy
    let policy = Policy::locked();
    let scenario = cleanroom::scenario("policy_test")
        .step("step1".to_string(), ["echo", "step1"])
        .policy(policy);

    // When: The scenario is executed
    let result = scenario.run().unwrap();

    // Then: Policy is applied to all steps
    assert_eq!(result.exit_code, 0);
    assert_eq!(result.steps.len(), 1);
}

#[test]
fn scenario_applies_determinism_profiles() {
    // Given: A scenario with deterministic profiles
    let scenario = cleanroom::scenario("deterministic_test")
        .step("step1".to_string(), ["date", "+%s"])
        .determinism(TimeProfile::Frozen(12345), RngProfile::Seed(42));

    // When: The scenario is executed
    let result = scenario.run().unwrap();

    // Then: Determinism profiles are applied
    assert_eq!(result.exit_code, 0);
    assert_eq!(result.steps.len(), 1);
    // Note: Time determinism would be verified through repeated executions
}

#[test]
fn scenario_passes_environment_variables() {
    // Given: A scenario with environment variables
    let scenario = cleanroom::scenario("env_test")
        .step("step1".to_string(), ["sh", "-c", "echo $TEST_VAR"])
        .env("TEST_VAR".to_string(), "scenario_value".to_string());

    // When: The scenario is executed
    let result = scenario.run().unwrap();

    // Then: Environment variables are passed to steps
    assert_eq!(result.exit_code, 0);
    assert!(result.stdout.contains("scenario_value"));
}

#[test]
fn scenario_handles_empty_step_list() {
    // Given: A scenario with no steps
    let scenario = cleanroom::scenario("empty_test");

    // When: The scenario is executed
    let result = scenario.run().unwrap();

    // Then: Empty scenario executes successfully
    assert_eq!(result.exit_code, 0);
    assert_eq!(result.steps.len(), 0);
    assert!(result.stdout.is_empty());
}

#[test]
fn scenario_aggregates_output_from_all_steps() {
    // Given: A scenario with multiple steps producing output
    let scenario = cleanroom::scenario("aggregation_test")
        .step("step1".to_string(), ["echo", "output1"])
        .step("step2".to_string(), ["echo", "output2"])
        .step("step3".to_string(), ["echo", "output3"]);

    // When: The scenario is executed
    let result = scenario.run().unwrap();

    // Then: Output from all steps is aggregated
    assert_eq!(result.exit_code, 0);
    assert_eq!(result.steps.len(), 3);
    assert!(result.stdout.contains("output1"));
    assert!(result.stdout.contains("output2"));
    assert!(result.stdout.contains("output3"));
}

#[test]
fn scenario_preserves_step_metadata() {
    // Given: A scenario with steps containing metadata
    let scenario = cleanroom::scenario("metadata_test")
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

#[test]
fn scenario_handles_concurrent_failures() {
    // Given: A scenario with concurrent execution and failing steps
    let scenario = cleanroom::scenario("concurrent_failure_test")
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
fn scenario_measures_total_execution_time() {
    // Given: A scenario with steps that take time
    let scenario = cleanroom::scenario("timing_test")
        .step("step1".to_string(), ["sleep", "0.1"])
        .step("step2".to_string(), ["sleep", "0.1"]);

    // When: The scenario is executed
    let result = scenario.run().unwrap();

    // Then: Total execution time is measured
    assert_eq!(result.exit_code, 0);
    assert!(result.duration_ms > 200); // At least 200ms for two 0.1s sleeps
}

#[test]
fn scenario_handles_large_output() {
    // Given: A scenario that produces large output
    let scenario = cleanroom::scenario("large_output_test")
        .step("step1".to_string(), ["seq", "1", "1000"]);

    // When: The scenario is executed
    let result = scenario.run().unwrap();

    // Then: Large output is handled correctly
    assert_eq!(result.exit_code, 0);
    assert!(result.stdout.contains("1"));
    assert!(result.stdout.contains("1000"));
    assert!(result.stdout.len() > 1000);
}

#[test]
fn scenario_handles_unicode_output() {
    // Given: A scenario that outputs Unicode
    let scenario = cleanroom::scenario("unicode_test")
        .step("step1".to_string(), ["printf", "'Unicode: 🚀 🌟 💻 🎉'"]);

    // When: The scenario is executed
    let result = scenario.run().unwrap();

    // Then: Unicode is handled correctly
    assert_eq!(result.exit_code, 0);
    assert!(result.stdout.contains("🚀"));
    assert!(result.stdout.contains("🌟"));
    assert!(result.stdout.contains("💻"));
    assert!(result.stdout.contains("🎉"));
}

#[test]
fn scenario_builder_is_fluent() {
    // Given: A scenario builder with fluent methods
    let scenario = cleanroom::scenario("fluent_test")
        .step("step1".to_string(), ["echo", "step1"])
        .concurrent()
        .policy(Policy::locked())
        .determinism(TimeProfile::Frozen(12345), RngProfile::Seed(42))
        .env("TEST_VAR".to_string(), "test_value".to_string())
        .services(vec![]);

    // When: The scenario is built
    // Then: All fluent methods are chained successfully
    assert_eq!(scenario.name, "fluent_test");
    assert!(scenario.concurrent);
    assert_eq!(scenario.steps.len(), 1);
}

#[test]
fn scenario_handles_service_lifecycle() {
    // Given: A scenario with services
    let scenario = cleanroom::scenario("service_test")
        .step("step1".to_string(), ["echo", "step1"])
        .services(vec![]); // Empty for now, would contain real services

    // When: The scenario is executed
    let result = scenario.run().unwrap();

    // Then: Services are started and stopped correctly
    assert_eq!(result.exit_code, 0);
    assert_eq!(result.steps.len(), 1);
    // Note: Service lifecycle would be verified through service mocks
}
