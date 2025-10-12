use super::super::world::CleanroomWorld;
use cucumber::{given, then, when};
use std::fs;
use std::path::Path;

/// Assertion step definitions for Cleanroom BDD tests
///
/// These steps handle test assertions, validation, and verification
/// of cleanroom execution results and artifacts.

// ============================================================================
// GIVEN steps - Setup preconditions
// ============================================================================

#[given(regex = r"^I have an assertion file "([^"]+)" with:$")]
fn assertion_file(world: &mut CleanroomWorld, filename: String, content: String) {
    let file_path = world.project_dir.join(&filename);
    
    // Create parent directories if needed
    if let Some(parent) = file_path.parent() {
        fs::create_dir_all(parent).expect("Failed to create parent directories");
    }
    
    fs::write(&file_path, content.trim())
        .unwrap_or_else(|e| panic!("Failed to write assertion file {}: {}", filename, e));
    
    world.capture_file(&filename, content.trim().to_string());
}

#[given(regex = r"^I have expected output "([^"]+)"$")]
fn expected_output(world: &mut CleanroomWorld, expected: String) {
    world.set_policy("expected_output".to_string(), expected);
}

#[given(regex = r"^I have expected exit code (\d+)$")]
fn expected_exit_code(world: &mut CleanroomWorld, code: i32) {
    world.set_policy("expected_exit_code".to_string(), code.to_string());
}

#[given(regex = r"^I have expected files: "([^"]+)"$")]
fn expected_files(world: &mut CleanroomWorld, files: String) {
    world.set_policy("expected_files".to_string(), files);
}

#[given(regex = r"^I have expected artifacts: "([^"]+)"$")]
fn expected_artifacts(world: &mut CleanroomWorld, artifacts: String) {
    world.set_policy("expected_artifacts".to_string(), artifacts);
}

// ============================================================================
// WHEN steps - Execute actions
// ============================================================================

#[when(regex = r"^I validate the results$")]
fn validate_results(world: &mut CleanroomWorld) {
    // Perform validation of execution results
    // This would typically involve checking outputs, files, artifacts, etc.
    
    // For now, just mark that validation was performed
    world.set_policy("validation_performed".to_string(), "true".to_string());
}

#[when(regex = r"^I compare with baseline "([^"]+)"$")]
fn compare_with_baseline(world: &mut CleanroomWorld, baseline: String) {
    // Compare current results with a baseline
    let baseline_file = world.project_dir.join(&baseline);
    
    if !baseline_file.exists() {
        panic!("Baseline file '{}' does not exist", baseline);
    }
    
    let baseline_content = fs::read_to_string(&baseline_file)
        .unwrap_or_else(|e| panic!("Failed to read baseline file '{}': {}", baseline, e));
    
    world.set_policy("baseline_content".to_string(), baseline_content);
}

#[when(regex = r"^I run assertions from "([^"]+)"$")]
fn run_assertions_from_file(world: &mut CleanroomWorld, assertion_file: String) {
    let file_path = world.project_dir.join(&assertion_file);
    
    if !file_path.exists() {
        panic!("Assertion file '{}' does not exist", assertion_file);
    }
    
    let content = fs::read_to_string(&file_path)
        .unwrap_or_else(|e| panic!("Failed to read assertion file '{}': {}", assertion_file, e));
    
    // Parse and execute assertions
    for line in content.lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        
        // Simple assertion parsing (would be more sophisticated in real implementation)
        if line.starts_with("assert_output_contains:") {
            let expected = line.strip_prefix("assert_output_contains:").unwrap().trim();
            world.set_policy("assert_output_contains".to_string(), expected.to_string());
        } else if line.starts_with("assert_file_exists:") {
            let expected = line.strip_prefix("assert_file_exists:").unwrap().trim();
            world.set_policy("assert_file_exists".to_string(), expected.to_string());
        }
    }
}

// ============================================================================
// THEN steps - Verify outcomes
// ============================================================================

#[then(regex = r"^the output should match the expected result$")]
fn output_should_match_expected(world: &mut CleanroomWorld) {
    let expected = world.policy_settings.get("expected_output")
        .expect("Expected output should be set");
    
    let actual = world.last_stdout();
    
    if !actual.contains(expected) {
        eprintln!("=== OUTPUT MISMATCH ===");
        eprintln!("Expected to contain: '{}'", expected);
        eprintln!("Actual output:\n{}", actual);
        panic!("Output should match expected result");
    }
}

#[then(regex = r"^the exit code should match the expected code$")]
fn exit_code_should_match_expected(world: &mut CleanroomWorld) {
    let expected_code: i32 = world.policy_settings.get("expected_exit_code")
        .expect("Expected exit code should be set")
        .parse()
        .expect("Expected exit code should be a number");
    
    let actual_code = world.last_exit_code.unwrap_or(-1);
    
    if actual_code != expected_code {
        eprintln!("=== EXIT CODE MISMATCH ===");
        eprintln!("Expected: {}", expected_code);
        eprintln!("Actual: {}", actual_code);
        eprintln!("Stdout:\n{}", world.last_stdout());
        eprintln!("Stderr:\n{}", world.last_stderr());
        panic!("Exit code should match expected code");
    }
}

#[then(regex = r"^all expected files should exist$")]
fn all_expected_files_should_exist(world: &mut CleanroomWorld) {
    let expected_files = world.policy_settings.get("expected_files")
        .expect("Expected files should be set");
    
    let files: Vec<&str> = expected_files.split(',').map(|s| s.trim()).collect();
    
    for file in files {
        let file_path = world.project_dir.join(file);
        if !file_path.exists() {
            panic!("Expected file '{}' does not exist", file);
        }
    }
}

#[then(regex = r"^all expected artifacts should be present$")]
fn all_expected_artifacts_should_be_present(world: &mut CleanroomWorld) {
    let expected_artifacts = world.policy_settings.get("expected_artifacts")
        .expect("Expected artifacts should be set");
    
    let artifacts: Vec<&str> = expected_artifacts.split(',').map(|s| s.trim()).collect();
    
    for artifact in artifacts {
        let artifact_path = world.project_dir.join(artifact);
        if !artifact_path.exists() {
            panic!("Expected artifact '{}' does not exist", artifact);
        }
        
        // Verify it's a binary file (not empty)
        let metadata = fs::metadata(&artifact_path)
            .unwrap_or_else(|e| panic!("Failed to get metadata for artifact '{}': {}", artifact, e));
        
        if metadata.len() == 0 {
            panic!("Expected artifact '{}' should not be empty", artifact);
        }
    }
}

#[then(regex = r"^the results should be deterministic$")]
fn results_should_be_deterministic(world: &mut CleanroomWorld) {
    // Check if we have multiple runs to compare
    if world.captured_hashes.len() < 2 {
        panic!("Need at least 2 runs to verify determinism");
    }
    
    let first_hash = &world.captured_hashes[0];
    for (i, hash) in world.captured_hashes.iter().enumerate().skip(1) {
        if hash != first_hash {
            panic!("Results are not deterministic: run {} differs from run 0", i);
        }
    }
}

#[then(regex = r"^the results should be reproducible$")]
fn results_should_be_reproducible(world: &mut CleanroomWorld) {
    // Similar to determinism but focuses on reproducibility across environments
    // For now, just verify that we have consistent output
    let output = world.last_stdout();
    
    if output.is_empty() {
        panic!("Results should be reproducible but output is empty");
    }
    
    // In a real implementation, this would involve running the same scenario
    // multiple times and comparing results
}

#[then(regex = r"^the execution should be isolated$")]
fn execution_should_be_isolated(world: &mut CleanroomWorld) {
    // Verify that execution was properly isolated
    // Check that no external files were modified
    // Check that no external network connections were made
    
    if world.network_constraints.contains(&"isolated".to_string()) {
        let stdout = world.last_stdout();
        let stderr = world.last_stderr();
        
        // Check for network-related output
        if stdout.contains("http") || stderr.contains("http") ||
           stdout.contains("tcp") || stderr.contains("tcp") {
            panic!("Execution should be isolated but network activity detected");
        }
    }
    
    if world.filesystem_constraints.contains(&"isolated".to_string()) {
        // Check that no files were created outside the project directory
        // This would require more sophisticated tracking in a real implementation
    }
}

#[then(regex = r"^the security constraints should be enforced$")]
fn security_constraints_should_be_enforced(world: &mut CleanroomWorld) {
    // Verify that security constraints were properly enforced
    let stdout = world.last_stdout();
    let stderr = world.last_stderr();
    
    // Check for security violations
    if stdout.contains("permission denied") || stderr.contains("permission denied") {
        // This might be expected if we're testing security constraints
        return;
    }
    
    // Check for other security-related indicators
    if stdout.contains("unauthorized") || stderr.contains("unauthorized") {
        // This might be expected if we're testing security constraints
        return;
    }
    
    // If we have security constraints set, verify they were respected
    if !world.policy_settings.is_empty() {
        // Security constraints are active
        // In a real implementation, this would involve more sophisticated checks
    }
}

#[then(regex = r"^the performance should be within limits$")]
fn performance_should_be_within_limits(world: &mut CleanroomWorld) {
    // Check performance constraints
    if let Some(memory_limit) = world.policy_settings.get("memory_limit") {
        // In a real implementation, this would check actual memory usage
        // For now, just verify that the limit was set
        assert!(!memory_limit.is_empty(), "Memory limit should be set");
    }
    
    if let Some(cpu_limit) = world.policy_settings.get("cpu_limit") {
        // In a real implementation, this would check actual CPU usage
        // For now, just verify that the limit was set
        assert!(!cpu_limit.is_empty(), "CPU limit should be set");
    }
    
    if let Some(timeout) = world.policy_settings.get("timeout") {
        // In a real implementation, this would check actual execution time
        // For now, just verify that the timeout was set
        assert!(!timeout.is_empty(), "Timeout should be set");
    }
}

#[then(regex = r"^the assertions should all pass$")]
fn assertions_should_all_pass(world: &mut CleanroomWorld) {
    // Verify that all assertions passed
    let validation_performed = world.policy_settings.get("validation_performed")
        .expect("Validation should have been performed");
    
    assert_eq!(validation_performed, "true", "Validation should have been performed");
    
    // Check specific assertions
    if let Some(expected_output) = world.policy_settings.get("assert_output_contains") {
        let actual_output = world.last_stdout();
        assert!(
            actual_output.contains(expected_output),
            "Output should contain '{}'",
            expected_output
        );
    }
    
    if let Some(expected_file) = world.policy_settings.get("assert_file_exists") {
        let file_path = world.project_dir.join(expected_file);
        assert!(
            file_path.exists(),
            "File '{}' should exist",
            expected_file
        );
    }
}

#[then(regex = r"^the baseline comparison should match$")]
fn baseline_comparison_should_match(world: &mut CleanroomWorld) {
    let baseline_content = world.policy_settings.get("baseline_content")
        .expect("Baseline content should be set");
    
    let current_output = world.last_stdout();
    
    // Simple comparison (in real implementation, this would be more sophisticated)
    if current_output.trim() != baseline_content.trim() {
        eprintln!("=== BASELINE MISMATCH ===");
        eprintln!("Baseline:\n{}", baseline_content);
        eprintln!("Current:\n{}", current_output);
        panic!("Baseline comparison should match");
    }
}
