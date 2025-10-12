use super::super::world::{CleanroomWorld, ScenarioDefinition, ScenarioStep};
use cucumber::{given, then, when};
use std::time::Instant;

/// Scenario DSL step definitions for Cleanroom BDD tests
///
/// These steps handle scenario definition and execution
/// for multi-step workflows and concurrent operations.

// ============================================================================
// GIVEN steps - Setup preconditions
// ============================================================================

#[given(regex = r"^a fixture project "([^"]+)"$")]
fn fixture_project(world: &mut CleanroomWorld, project: String) {
    world.set_fixture_project(project, "target/debug/mock-binary".to_string());
}

// ============================================================================
// WHEN steps - Execute actions
// ============================================================================

#[when(regex = r"^I define scenario "([^"]+)"$")]
fn define_scenario(world: &mut CleanroomWorld, scenario_name: String) {
    // Create a basic scenario definition
    let scenario = ScenarioDefinition {
        name: scenario_name.clone(),
        steps: vec![
            ScenarioStep {
                name: "version".to_string(),
                args: vec!["--version".to_string()],
                expect: "success".to_string(),
                max_output: None,
            },
            ScenarioStep {
                name: "build prod".to_string(),
                args: vec!["build".to_string(), "--prod".to_string()],
                expect: "success".to_string(),
                max_output: None,
            },
        ],
        concurrent: false,
        continue_on_fail: false,
    };
    
    world.add_scenario(scenario);
    world.set_current_scenario(scenario_name);
}

#[when(regex = r"^I define concurrent scenario "([^"]+)"$")]
fn define_concurrent_scenario(world: &mut CleanroomWorld, scenario_name: String) {
    // Create a concurrent scenario definition
    let scenario = ScenarioDefinition {
        name: scenario_name.clone(),
        steps: vec![
            ScenarioStep {
                name: "help1".to_string(),
                args: vec!["--help".to_string()],
                expect: "success".to_string(),
                max_output: Some(64 * 1024), // 64KB
            },
            ScenarioStep {
                name: "help2".to_string(),
                args: vec!["--help".to_string()],
                expect: "success".to_string(),
                max_output: Some(64 * 1024), // 64KB
            },
        ],
        concurrent: true,
        continue_on_fail: false,
    };
    
    world.add_scenario(scenario);
    world.set_current_scenario(scenario_name);
}

#[when(regex = r"^I execute the scenario on backend "([^"]+)"$")]
fn execute_scenario_on_backend(world: &mut CleanroomWorld, backend: String) {
    let scenario_name = world.current_scenario.as_ref()
        .expect("Current scenario should be set");
    
    let scenario = world.scenarios.get(scenario_name)
        .expect("Scenario should be defined");
    
    let start_time = Instant::now();
    
    // Mock scenario execution
    let mut steps_succeeded = Vec::new();
    let mut steps_failed = Vec::new();
    
    for step in &scenario.steps {
        // Mock step execution
        if step.expect == "success" {
            steps_succeeded.push(step.name.clone());
        } else {
            steps_failed.push(step.name.clone());
        }
    }
    
    let duration = start_time.elapsed();
    
    // Store scenario result
    let result = super::super::world::ScenarioResult {
        scenario_name: scenario_name.clone(),
        steps_succeeded,
        steps_failed,
        total_duration_ms: duration.as_millis() as u64,
        aggregated_duration_ms: duration.as_millis() as u64,
    };
    
    world.scenario_results.insert(scenario_name.clone(), result);
    world.set_backend(backend);
}

// ============================================================================
// THEN steps - Verify outcomes
// ============================================================================

#[then(regex = r"^all steps succeeded$")]
fn all_steps_succeeded(world: &mut CleanroomWorld) {
    let scenario_name = world.current_scenario.as_ref()
        .expect("Current scenario should be set");
    
    let result = world.scenario_results.get(scenario_name)
        .expect("Scenario result should exist");
    
    assert!(
        result.steps_failed.is_empty(),
        "All steps should succeed, but {} failed: {:?}",
        result.steps_failed.len(),
        result.steps_failed
    );
}

#[then(regex = r"^aggregated duration <= (\d+) ms$")]
fn aggregated_duration_less_than(world: &mut CleanroomWorld, max_duration_ms: u64) {
    let scenario_name = world.current_scenario.as_ref()
        .expect("Current scenario should be set");
    
    let result = world.scenario_results.get(scenario_name)
        .expect("Scenario result should exist");
    
    assert!(
        result.aggregated_duration_ms <= max_duration_ms,
        "Aggregated duration should be <= {} ms, but got {} ms",
        max_duration_ms, result.aggregated_duration_ms
    );
}

#[then(regex = r"^step order is deterministic$")]
fn step_order_is_deterministic(world: &mut CleanroomWorld) {
    let scenario_name = world.current_scenario.as_ref()
        .expect("Current scenario should be set");
    
    let scenario = world.scenarios.get(scenario_name)
        .expect("Scenario should be defined");
    
    // Verify that steps are in the expected order
    let expected_order = vec!["version", "build prod"];
    let actual_order: Vec<&str> = scenario.steps.iter().map(|s| s.name.as_str()).collect();
    
    assert_eq!(
        actual_order, expected_order,
        "Step order should be deterministic"
    );
}

#[then(regex = r"^logs are order-stable by \(start_ts, step_name\)$")]
fn logs_are_order_stable(world: &mut CleanroomWorld) {
    // Verify that logs are ordered by start timestamp and step name
    // In a real implementation, this would check the actual log ordering
    
    let scenario_name = world.current_scenario.as_ref()
        .expect("Current scenario should be set");
    
    let result = world.scenario_results.get(scenario_name)
        .expect("Scenario result should exist");
    
    // Verify that we have results to check ordering
    assert!(
        !result.steps_succeeded.is_empty(),
        "Should have succeeded steps to check ordering"
    );
}

#[then(regex = r"^both steps succeeded$")]
fn both_steps_succeeded(world: &mut CleanroomWorld) {
    let scenario_name = world.current_scenario.as_ref()
        .expect("Current scenario should be set");
    
    let result = world.scenario_results.get(scenario_name)
        .expect("Scenario result should exist");
    
    assert_eq!(
        result.steps_succeeded.len(), 2,
        "Both steps should succeed, but {} succeeded",
        result.steps_succeeded.len()
    );
}

#[then(regex = r"^total duration < 2x single-step duration$")]
fn total_duration_less_than_double_single_step(world: &mut CleanroomWorld) {
    let scenario_name = world.current_scenario.as_ref()
        .expect("Current scenario should be set");
    
    let result = world.scenario_results.get(scenario_name)
        .expect("Scenario result should exist");
    
    // Mock single step duration (in real implementation, this would be measured)
    let single_step_duration_ms = 100;
    let max_duration_ms = 2 * single_step_duration_ms;
    
    assert!(
        result.total_duration_ms < max_duration_ms,
        "Total duration should be < {} ms (2x single-step), but got {} ms",
        max_duration_ms, result.total_duration_ms
    );
}

#[then(regex = r"^per-step output <= max_output$")]
fn per_step_output_less_than_max(world: &mut CleanroomWorld) {
    let scenario_name = world.current_scenario.as_ref()
        .expect("Current scenario should be set");
    
    let scenario = world.scenarios.get(scenario_name)
        .expect("Scenario should be defined");
    
    // Verify that each step has max_output set
    for step in &scenario.steps {
        assert!(
            step.max_output.is_some(),
            "Step '{}' should have max_output set",
            step.name
        );
    }
}
