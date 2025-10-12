//! Integration tests for determinism verification
//!
//! Tests that identical inputs produce identical outputs using snapshot
//! testing with insta for output verification.

use cleanroom::scenario::{Scenario, RunResult};
use cleanroom::policy::{TimeProfile, RngProfile};
use cleanroom::backend::{Backend, LocalBackend, Cmd};
use crate::common::assertions;
use insta::assert_snapshot;

#[test]
fn identical_inputs_produce_identical_outputs() {
    // Given: A scenario with deterministic profiles
    let scenario = cleanroom::scenario("deterministic_test")
        .step("step1".to_string(), ["echo", "deterministic"])
        .determinism(TimeProfile::Frozen(12345), RngProfile::Seed(42));

    // When: The scenario is executed multiple times
    let result1 = scenario.clone().run().unwrap();
    let result2 = scenario.clone().run().unwrap();
    let result3 = scenario.run().unwrap();

    // Then: All results are identical
    assertions::assert_results_identical(&result1, &result2);
    assertions::assert_results_identical(&result2, &result3);
    assertions::assert_results_identical(&result1, &result3);
}

#[test]
fn seeded_rng_produces_same_values() {
    // Given: A scenario with seeded RNG
    let scenario = cleanroom::scenario("rng_test")
        .step("step1".to_string(), ["sh", "-c", "echo $RANDOM"])
        .determinism(TimeProfile::System, RngProfile::Seed(42));

    // When: The scenario is executed multiple times
    let result1 = scenario.clone().run().unwrap();
    let result2 = scenario.clone().run().unwrap();
    let result3 = scenario.run().unwrap();

    // Then: RNG produces the same values
    assertions::assert_results_identical(&result1, &result2);
    assertions::assert_results_identical(&result2, &result3);
    assertions::assert_results_identical(&result1, &result3);
}

#[test]
fn frozen_time_produces_consistent_timestamps() {
    // Given: A scenario with frozen time
    let scenario = cleanroom::scenario("time_test")
        .step("step1".to_string(), ["date", "+%s"])
        .determinism(TimeProfile::Frozen(12345), RngProfile::System);

    // When: The scenario is executed multiple times
    let result1 = scenario.clone().run().unwrap();
    let result2 = scenario.clone().run().unwrap();
    let result3 = scenario.run().unwrap();

    // Then: Timestamps are consistent
    assertions::assert_results_identical(&result1, &result2);
    assertions::assert_results_identical(&result2, &result3);
    assertions::assert_results_identical(&result1, &result3);
}

#[test]
fn snapshot_testing_verifies_deterministic_output() {
    // Given: A deterministic scenario
    let scenario = cleanroom::scenario("snapshot_test")
        .step("step1".to_string(), ["echo", "snapshot test"])
        .step("step2".to_string(), ["date", "+%Y-%m-%d"])
        .determinism(TimeProfile::Frozen(12345), RngProfile::Seed(42));

    // When: The scenario is executed
    let result = scenario.run().unwrap();

    // Then: The output matches the expected snapshot
    assertions::snapshot_result(&result, "deterministic_scenario");
}

#[test]
fn different_seeds_produce_different_outputs() {
    // Given: Scenarios with different RNG seeds
    let scenario1 = cleanroom::scenario("seed1_test")
        .step("step1".to_string(), ["sh", "-c", "echo $RANDOM"])
        .determinism(TimeProfile::System, RngProfile::Seed(42));

    let scenario2 = cleanroom::scenario("seed2_test")
        .step("step1".to_string(), ["sh", "-c", "echo $RANDOM"])
        .determinism(TimeProfile::System, RngProfile::Seed(123));

    // When: Both scenarios are executed
    let result1 = scenario1.run().unwrap();
    let result2 = scenario2.run().unwrap();

    // Then: Different seeds produce different outputs
    assert_ne!(result1.stdout, result2.stdout);
}

#[test]
fn different_timestamps_produce_different_outputs() {
    // Given: Scenarios with different frozen timestamps
    let scenario1 = cleanroom::scenario("time1_test")
        .step("step1".to_string(), ["date", "+%s"])
        .determinism(TimeProfile::Frozen(12345), RngProfile::System);

    let scenario2 = cleanroom::scenario("time2_test")
        .step("step1".to_string(), ["date", "+%s"])
        .determinism(TimeProfile::Frozen(54321), RngProfile::System);

    // When: Both scenarios are executed
    let result1 = scenario1.run().unwrap();
    let result2 = scenario2.run().unwrap();

    // Then: Different timestamps produce different outputs
    assert_ne!(result1.stdout, result2.stdout);
}

#[test]
fn system_time_produces_different_outputs() {
    // Given: A scenario with system time
    let scenario = cleanroom::scenario("system_time_test")
        .step("step1".to_string(), ["date", "+%s"])
        .determinism(TimeProfile::System, RngProfile::System);

    // When: The scenario is executed with delays
    let result1 = scenario.clone().run().unwrap();
    std::thread::sleep(std::time::Duration::from_millis(100));
    let result2 = scenario.clone().run().unwrap();
    std::thread::sleep(std::time::Duration::from_millis(100));
    let result3 = scenario.run().unwrap();

    // Then: System time produces different outputs
    assert_ne!(result1.stdout, result2.stdout);
    assert_ne!(result2.stdout, result3.stdout);
    assert_ne!(result1.stdout, result3.stdout);
}

#[test]
fn backend_execution_is_deterministic() {
    // Given: A backend with deterministic behavior
    let backend = LocalBackend::new();
    let cmd = Cmd::new("echo").args(["backend", "test"]);

    // When: The same command is executed multiple times
    let result1 = backend.run_cmd(cmd.clone()).unwrap();
    let result2 = backend.run_cmd(cmd.clone()).unwrap();
    let result3 = backend.run_cmd(cmd).unwrap();

    // Then: Results are identical
    assertions::assert_results_identical(&result1, &result2);
    assertions::assert_results_identical(&result2, &result3);
    assertions::assert_results_identical(&result1, &result3);
}

#[test]
fn environment_variables_affect_determinism() {
    // Given: Scenarios with different environment variables
    let scenario1 = cleanroom::scenario("env1_test")
        .step("step1".to_string(), ["sh", "-c", "echo $TEST_VAR"])
        .env("TEST_VAR".to_string(), "value1".to_string())
        .determinism(TimeProfile::Frozen(12345), RngProfile::Seed(42));

    let scenario2 = cleanroom::scenario("env2_test")
        .step("step1".to_string(), ["sh", "-c", "echo $TEST_VAR"])
        .env("TEST_VAR".to_string(), "value2".to_string())
        .determinism(TimeProfile::Frozen(12345), RngProfile::Seed(42));

    // When: Both scenarios are executed
    let result1 = scenario1.run().unwrap();
    let result2 = scenario2.run().unwrap();

    // Then: Different environment variables produce different outputs
    assert_ne!(result1.stdout, result2.stdout);
    assert!(result1.stdout.contains("value1"));
    assert!(result2.stdout.contains("value2"));
}

#[test]
fn step_order_affects_determinism() {
    // Given: Scenarios with different step orders
    let scenario1 = cleanroom::scenario("order1_test")
        .step("step1".to_string(), ["echo", "first"])
        .step("step2".to_string(), ["echo", "second"])
        .determinism(TimeProfile::Frozen(12345), RngProfile::Seed(42));

    let scenario2 = cleanroom::scenario("order2_test")
        .step("step1".to_string(), ["echo", "second"])
        .step("step2".to_string(), ["echo", "first"])
        .determinism(TimeProfile::Frozen(12345), RngProfile::Seed(42));

    // When: Both scenarios are executed
    let result1 = scenario1.run().unwrap();
    let result2 = scenario2.run().unwrap();

    // Then: Different step orders produce different outputs
    assert_ne!(result1.stdout, result2.stdout);
}

#[test]
fn concurrent_execution_maintains_determinism() {
    // Given: A scenario with concurrent execution
    let scenario = cleanroom::scenario("concurrent_deterministic_test")
        .step("step1".to_string(), ["echo", "step1"])
        .step("step2".to_string(), ["echo", "step2"])
        .concurrent()
        .determinism(TimeProfile::Frozen(12345), RngProfile::Seed(42));

    // When: The scenario is executed multiple times
    let result1 = scenario.clone().run().unwrap();
    let result2 = scenario.clone().run().unwrap();
    let result3 = scenario.run().unwrap();

    // Then: Concurrent execution is still deterministic
    assertions::assert_results_identical(&result1, &result2);
    assertions::assert_results_identical(&result2, &result3);
    assertions::assert_results_identical(&result1, &result3);
}

#[test]
fn large_output_remains_deterministic() {
    // Given: A scenario that produces large output
    let scenario = cleanroom::scenario("large_output_deterministic_test")
        .step("step1".to_string(), ["seq", "1", "1000"])
        .determinism(TimeProfile::Frozen(12345), RngProfile::Seed(42));

    // When: The scenario is executed multiple times
    let result1 = scenario.clone().run().unwrap();
    let result2 = scenario.clone().run().unwrap();
    let result3 = scenario.run().unwrap();

    // Then: Large output remains deterministic
    assertions::assert_results_identical(&result1, &result2);
    assertions::assert_results_identical(&result2, &result3);
    assertions::assert_results_identical(&result1, &result3);
}

#[test]
fn unicode_output_remains_deterministic() {
    // Given: A scenario that outputs Unicode
    let scenario = cleanroom::scenario("unicode_deterministic_test")
        .step("step1".to_string(), ["printf", "'Unicode: 🚀 🌟 💻 🎉'"])
        .determinism(TimeProfile::Frozen(12345), RngProfile::Seed(42));

    // When: The scenario is executed multiple times
    let result1 = scenario.clone().run().unwrap();
    let result2 = scenario.clone().run().unwrap();
    let result3 = scenario.run().unwrap();

    // Then: Unicode output remains deterministic
    assertions::assert_results_identical(&result1, &result2);
    assertions::assert_results_identical(&result2, &result3);
    assertions::assert_results_identical(&result1, &result3);
}
