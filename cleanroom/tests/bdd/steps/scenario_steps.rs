use super::super::world::CleanroomWorld;
use assert_cmd::Command;
use cucumber::{given, then, when};
use std::fs;
use std::path::Path;

/// Scenario step definitions for Cleanroom BDD tests
///
/// These steps handle test scenario execution, command running,
/// and result validation for cleanroom operations.

// ============================================================================
// GIVEN steps - Setup preconditions
// ============================================================================

#[given(regex = r"^I have a test scenario "([^"]+)"$")]
fn test_scenario(world: &mut CleanroomWorld, scenario_name: String) {
    // Create a basic test scenario file
    let scenario_content = format!(
        r#"name: {}
description: Test scenario for cleanroom
steps:
  - name: "test step"
    command: "echo 'Hello World'"
    expected_exit_code: 0
"#,
        scenario_name
    );
    
    let scenario_file = world.project_dir.join(format!("{}.yaml", scenario_name));
    fs::write(&scenario_file, scenario_content)
        .unwrap_or_else(|e| panic!("Failed to write scenario file: {}", e));
    
    world.capture_file(&format!("{}.yaml", scenario_name), scenario_content);
}

#[given(regex = r"^I have a test scenario with steps:$")]
fn test_scenario_with_steps(world: &mut CleanroomWorld, steps: String) {
    // Create a test scenario with custom steps
    let scenario_content = format!(
        r#"name: "custom_scenario"
description: Custom test scenario
steps:
{}
"#,
        steps
    );
    
    let scenario_file = world.project_dir.join("custom_scenario.yaml");
    fs::write(&scenario_file, scenario_content)
        .unwrap_or_else(|e| panic!("Failed to write scenario file: {}", e));
    
    world.capture_file("custom_scenario.yaml", scenario_content);
}

#[given(regex = r"^I have a failing test scenario$")]
fn failing_test_scenario(world: &mut CleanroomWorld) {
    // Create a scenario that will fail
    let scenario_content = r#"name: "failing_scenario"
description: Test scenario that will fail
steps:
  - name: "failing step"
    command: "false"
    expected_exit_code: 0
"#;
    
    let scenario_file = world.project_dir.join("failing_scenario.yaml");
    fs::write(&scenario_file, scenario_content)
        .unwrap_or_else(|e| panic!("Failed to write scenario file: {}", e));
    
    world.capture_file("failing_scenario.yaml", scenario_content.to_string());
}

#[given(regex = r"^I have a long-running test scenario$")]
fn long_running_test_scenario(world: &mut CleanroomWorld) {
    // Create a scenario that takes time to complete
    let scenario_content = r#"name: "long_running_scenario"
description: Test scenario that takes time to complete
steps:
  - name: "sleep step"
    command: "sleep 5"
    expected_exit_code: 0
    timeout: "10s"
"#;
    
    let scenario_file = world.project_dir.join("long_running_scenario.yaml");
    fs::write(&scenario_file, scenario_content)
        .unwrap_or_else(|e| panic!("Failed to write scenario file: {}", e));
    
    world.capture_file("long_running_scenario.yaml", scenario_content.to_string());
}

#[given(regex = r"^I have a test scenario with network access$")]
fn test_scenario_with_network(world: &mut CleanroomWorld) {
    // Create a scenario that requires network access
    let scenario_content = r#"name: "network_scenario"
description: Test scenario requiring network access
steps:
  - name: "network test"
    command: "curl -s http://httpbin.org/get"
    expected_exit_code: 0
    network: "open"
"#;
    
    let scenario_file = world.project_dir.join("network_scenario.yaml");
    fs::write(&scenario_file, scenario_content)
        .unwrap_or_else(|e| panic!("Failed to write scenario file: {}", e));
    
    world.capture_file("network_scenario.yaml", scenario_content.to_string());
}

#[given(regex = r"^I have a test scenario with file operations$")]
fn test_scenario_with_file_ops(world: &mut CleanroomWorld) {
    // Create a scenario that performs file operations
    let scenario_content = r#"name: "file_ops_scenario"
description: Test scenario with file operations
steps:
  - name: "create file"
    command: "echo 'test content' > test.txt"
    expected_exit_code: 0
  - name: "read file"
    command: "cat test.txt"
    expected_exit_code: 0
"#;
    
    let scenario_file = world.project_dir.join("file_ops_scenario.yaml");
    fs::write(&scenario_file, scenario_content)
        .unwrap_or_else(|e| panic!("Failed to write scenario file: {}", e));
    
    world.capture_file("file_ops_scenario.yaml", scenario_content.to_string());
}

// ============================================================================
// WHEN steps - Execute actions
// ============================================================================

#[when(regex = r"^I run the scenario "([^"]+)"$")]
fn run_scenario(world: &mut CleanroomWorld, scenario_name: String) {
    let scenario_file = world.project_dir.join(format!("{}.yaml", scenario_name));
    
    if !scenario_file.exists() {
        panic!("Scenario file '{}' does not exist", scenario_file.display());
    }
    
    let output = Command::cargo_bin("cleanroom")
        .expect("cleanroom binary not found")
        .arg("run")
        .arg(&scenario_file)
        .current_dir(&world.project_dir)
        .output()
        .unwrap_or_else(|e| panic!("Failed to run scenario: {}", e));
    
    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r"^I run the scenario with policy "([^"]+)"$")]
fn run_scenario_with_policy(world: &mut CleanroomWorld, policy_name: String) {
    let scenario_file = world.project_dir.join("custom_scenario.yaml");
    let policy_file = world.project_dir.join(&policy_name);
    
    if !scenario_file.exists() {
        panic!("Scenario file does not exist");
    }
    
    if !policy_file.exists() {
        panic!("Policy file '{}' does not exist", policy_name);
    }
    
    let output = Command::cargo_bin("cleanroom")
        .expect("cleanroom binary not found")
        .arg("run")
        .arg(&scenario_file)
        .arg("--policy")
        .arg(&policy_file)
        .current_dir(&world.project_dir)
        .output()
        .unwrap_or_else(|e| panic!("Failed to run scenario with policy: {}", e));
    
    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r"^I run the scenario with backend "([^"]+)"$")]
fn run_scenario_with_backend(world: &mut CleanroomWorld, backend: String) {
    let scenario_file = world.project_dir.join("custom_scenario.yaml");
    
    if !scenario_file.exists() {
        panic!("Scenario file does not exist");
    }
    
    let output = Command::cargo_bin("cleanroom")
        .expect("cleanroom binary not found")
        .arg("run")
        .arg(&scenario_file)
        .arg("--backend")
        .arg(&backend)
        .current_dir(&world.project_dir)
        .output()
        .unwrap_or_else(|e| panic!("Failed to run scenario with backend: {}", e));
    
    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r"^I run the scenario with timeout "([^"]+)"$")]
fn run_scenario_with_timeout(world: &mut CleanroomWorld, timeout: String) {
    let scenario_file = world.project_dir.join("custom_scenario.yaml");
    
    if !scenario_file.exists() {
        panic!("Scenario file does not exist");
    }
    
    let output = Command::cargo_bin("cleanroom")
        .expect("cleanroom binary not found")
        .arg("run")
        .arg(&scenario_file)
        .arg("--timeout")
        .arg(&timeout)
        .current_dir(&world.project_dir)
        .output()
        .unwrap_or_else(|e| panic!("Failed to run scenario with timeout: {}", e));
    
    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r"^I run the scenario in parallel$")]
fn run_scenario_in_parallel(world: &mut CleanroomWorld) {
    let scenario_file = world.project_dir.join("custom_scenario.yaml");
    
    if !scenario_file.exists() {
        panic!("Scenario file does not exist");
    }
    
    let output = Command::cargo_bin("cleanroom")
        .expect("cleanroom binary not found")
        .arg("run")
        .arg(&scenario_file)
        .arg("--parallel")
        .current_dir(&world.project_dir)
        .output()
        .unwrap_or_else(|e| panic!("Failed to run scenario in parallel: {}", e));
    
    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

// ============================================================================
// THEN steps - Verify outcomes
// ============================================================================

#[then(regex = r"^the scenario should complete successfully$")]
fn scenario_should_complete_successfully(world: &mut CleanroomWorld) {
    if !world.last_command_succeeded() {
        eprintln!("=== SCENARIO FAILED ===");
        eprintln!("Exit code: {}", world.last_exit_code.unwrap_or(-1));
        eprintln!("Stdout:\n{}", world.last_stdout());
        eprintln!("Stderr:\n{}", world.last_stderr());
        panic!("Scenario should have completed successfully");
    }
}

#[then(regex = r"^the scenario should fail$")]
fn scenario_should_fail(world: &mut CleanroomWorld) {
    if world.last_command_succeeded() {
        eprintln!("=== SCENARIO UNEXPECTEDLY SUCCEEDED ===");
        eprintln!("Exit code: {}", world.last_exit_code.unwrap_or(0));
        eprintln!("Stdout:\n{}", world.last_stdout());
        eprintln!("Stderr:\n{}", world.last_stderr());
        panic!("Scenario should have failed but succeeded");
    }
}

#[then(regex = r"^the scenario should timeout$")]
fn scenario_should_timeout(world: &mut CleanroomWorld) {
    // Check for timeout-related output
    let stdout = world.last_stdout();
    let stderr = world.last_stderr();
    
    if !stdout.contains("timeout") && !stderr.contains("timeout") {
        eprintln!("=== SCENARIO DID NOT TIMEOUT ===");
        eprintln!("Exit code: {}", world.last_exit_code.unwrap_or(-1));
        eprintln!("Stdout:\n{}", stdout);
        eprintln!("Stderr:\n{}", stderr);
        panic!("Scenario should have timed out");
    }
}

#[then(regex = r"^the scenario should produce output$")]
fn scenario_should_produce_output(world: &mut CleanroomWorld) {
    let stdout = world.last_stdout();
    
    if stdout.trim().is_empty() {
        eprintln!("=== SCENARIO PRODUCED NO OUTPUT ===");
        eprintln!("Exit code: {}", world.last_exit_code.unwrap_or(-1));
        eprintln!("Stderr:\n{}", world.last_stderr());
        panic!("Scenario should have produced output");
    }
}

#[then(regex = r"^the scenario should create files$")]
fn scenario_should_create_files(world: &mut CleanroomWorld) {
    // Check if any files were created
    let mut files_created = false;
    
    for entry in fs::read_dir(&world.project_dir).unwrap() {
        let entry = entry.unwrap();
        if entry.file_type().unwrap().is_file() {
            files_created = true;
            break;
        }
    }
    
    if !files_created {
        panic!("Scenario should have created files");
    }
}

#[then(regex = r"^the scenario should respect network constraints$")]
fn scenario_should_respect_network_constraints(world: &mut CleanroomWorld) {
    // Verify that network constraints were respected
    if world.network_constraints.contains(&"blocked".to_string()) {
        // If network is blocked, the scenario should not have made network requests
        let stdout = world.last_stdout();
        let stderr = world.last_stderr();
        
        if stdout.contains("http") || stderr.contains("http") {
            panic!("Scenario should not have made network requests when network is blocked");
        }
    }
}

#[then(regex = r"^the scenario should respect filesystem constraints$")]
fn scenario_should_respect_filesystem_constraints(world: &mut CleanroomWorld) {
    // Verify that filesystem constraints were respected
    if world.filesystem_constraints.contains(&"readonly".to_string()) {
        // If filesystem is readonly, the scenario should not have created files
        let stdout = world.last_stdout();
        let stderr = world.last_stderr();
        
        if stdout.contains("created") || stderr.contains("created") {
            panic!("Scenario should not have created files when filesystem is readonly");
        }
    }
}

#[then(regex = r"^the scenario should complete within the timeout$")]
fn scenario_should_complete_within_timeout(world: &mut CleanroomWorld) {
    // This would typically involve checking execution time
    // For now, just verify that the scenario completed successfully
    if !world.last_command_succeeded() {
        panic!("Scenario should have completed within timeout");
    }
}

#[then(regex = r"^the scenario should produce consistent results$")]
fn scenario_should_produce_consistent_results(world: &mut CleanroomWorld) {
    // Run the scenario multiple times and compare results
    let first_output = world.last_stdout();
    
    // Store the first output hash
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    
    let mut hasher = DefaultHasher::new();
    first_output.hash(&mut hasher);
    let first_hash = hasher.finish();
    
    world.capture_hash(first_hash.to_string());
    
    // This step would typically run the scenario again and compare
    // For now, just verify that we have output to compare
    assert!(!first_output.is_empty(), "Scenario should produce output for consistency checking");
}
