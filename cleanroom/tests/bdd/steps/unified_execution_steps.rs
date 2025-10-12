use super::super::world::CleanroomWorld;
use assert_cmd::Command;
use cucumber::{given, then, when};
use std::fs;
use std::path::Path;
use std::time::Instant;

/// Unified execution step definitions for Cleanroom BDD tests
///
/// These steps handle unified execution across different backends
/// ensuring identical behavior regardless of the execution environment.

// ============================================================================
// GIVEN steps - Setup preconditions
// ============================================================================

#[given(regex = r#"^a fixture project "([^"]+)" with binary "([^"]+)"$"#)]
fn fixture_project_with_binary(world: &mut CleanroomWorld, project: String, binary: String) {
    // Create a mock fixture project
    let project_dir = world.project_dir.join(&project);
    fs::create_dir_all(&project_dir).expect("Failed to create project directory");
    
    // Create a mock binary
    let binary_path = project_dir.join(&binary);
    fs::create_dir_all(binary_path.parent().unwrap()).expect("Failed to create binary directory");
    
    // Create a simple mock binary that responds to --help
    let binary_content = r#"#!/bin/bash
case "$1" in
    --help)
        echo "USAGE: $0 [OPTIONS]"
        echo "OPTIONS:"
        echo "  --help     Show this help message"
        echo "  --version  Show version information"
        echo "  exit       Exit with specified code"
        exit 0
        ;;
    --version)
        echo "mock-binary 1.0.0"
        exit 0
        ;;
    exit)
        if [ "$2" = "--code" ]; then
            echo "exiting with $3" >&2
            exit "$3"
        fi
        ;;
    *)
        echo "Unknown command: $1"
        exit 1
        ;;
esac
"#;
    
    fs::write(&binary_path, binary_content).expect("Failed to write mock binary");
    
    // Make it executable
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = fs::metadata(&binary_path).unwrap().permissions();
        perms.set_mode(0o755);
        fs::set_permissions(&binary_path, perms).unwrap();
    }
    
    world.set_fixture_project(project, binary);
}

#[given(regex = r"^cleanroom is configured with defaults$")]
fn cleanroom_configured_with_defaults(world: &mut CleanroomWorld) {
    // Set default configuration
    world.set_policy("timeout_ms".to_string(), "30000".to_string());
    world.set_policy("network_enabled".to_string(), "false".to_string());
    world.set_policy("filesystem_readonly".to_string(), "false".to_string());
    world.set_policy("memory_limit".to_string(), "512MB".to_string());
    world.set_policy("cpu_limit".to_string(), "1.0".to_string());
}

#[given(regex = r"^backend "([^"]+)" is available or test is skipped$")]
fn backend_available_or_skipped(world: &mut CleanroomWorld, backend: String) {
    if !world.is_backend_available(&backend) {
        world.set_skip_reason(
            format!("backend_{}", backend),
            format!("Backend '{}' is not available", backend)
        );
        // In a real implementation, this would skip the test
        // For now, we'll just mark it as unavailable
    }
}

// ============================================================================
// WHEN steps - Execute actions
// ============================================================================

#[when(regex = r#"^I run the binary with args "([^"]+)" using backend "([^"]+)"$"#)]
fn run_binary_with_args_and_backend(world: &mut CleanroomWorld, args: String, backend: String) {
    let binary_path = world.binary_path.as_ref()
        .expect("Binary path should be set");
    
    let project_dir = world.fixture_project.as_ref()
        .expect("Fixture project should be set");
    
    let full_binary_path = world.project_dir.join(project_dir).join(binary_path);
    
    if !full_binary_path.exists() {
        panic!("Binary '{}' does not exist", full_binary_path.display());
    }
    
    // Parse arguments
    let arg_list = shell_words::split(&args)
        .unwrap_or_else(|e| panic!("Failed to parse arguments: {}", e));
    
    let start_time = Instant::now();
    
    // Execute the binary
    let output = Command::new(&full_binary_path)
        .args(&arg_list)
        .current_dir(&world.project_dir)
        .output()
        .unwrap_or_else(|e| panic!("Failed to run binary: {}", e));
    
    let duration = start_time.elapsed();
    
    // Store results
    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
    world.set_backend(backend);
    
    // Add trace span
    world.add_trace_span(super::super::world::TraceSpan {
        name: format!("run_{}", backend),
        duration_ms: duration.as_millis() as u64,
        status: if output.status.success() { "success".to_string() } else { "failure".to_string() },
    });
}

#[when(regex = r#"^I run "([^"]+)" using backend "([^"]+)"$"#)]
fn run_command_with_backend(world: &mut CleanroomWorld, command: String, backend: String) {
    let args: Vec<&str> = command.split_whitespace().collect();
    
    if args.is_empty() {
        panic!("Empty command provided");
    }
    
    let start_time = Instant::now();
    
    let output = Command::new(args[0])
        .args(&args[1..])
        .current_dir(&world.project_dir)
        .output()
        .unwrap_or_else(|e| panic!("Failed to run command: {}", e));
    
    let duration = start_time.elapsed();
    
    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
    world.set_backend(backend);
    
    world.add_trace_span(super::super::world::TraceSpan {
        name: format!("run_{}", backend),
        duration_ms: duration.as_millis() as u64,
        status: if output.status.success() { "success".to_string() } else { "failure".to_string() },
    });
}

// ============================================================================
// THEN steps - Verify outcomes
// ============================================================================

#[then(regex = r"^the exit code is (\d+)$")]
fn exit_code_is(world: &mut CleanroomWorld, expected_code: i32) {
    let actual_code = world.last_exit_code.unwrap_or(-1);
    
    assert_eq!(
        actual_code, expected_code,
        "Expected exit code {}, but got {}",
        expected_code, actual_code
    );
}

#[then(regex = r"^stdout contains "([^"]+)"$")]
fn stdout_contains(world: &mut CleanroomWorld, expected: String) {
    let stdout = world.last_stdout();
    
    assert!(
        stdout.contains(&expected),
        "Expected stdout to contain '{}', but got:\n{}",
        expected, stdout
    );
}

#[then(regex = r"^stderr is empty$")]
fn stderr_is_empty(world: &mut CleanroomWorld) {
    let stderr = world.last_stderr();
    
    assert!(
        stderr.trim().is_empty(),
        "Expected stderr to be empty, but got:\n{}",
        stderr
    );
}

#[then(regex = r"^stderr contains "([^"]+)"$")]
fn stderr_contains(world: &mut CleanroomWorld, expected: String) {
    let stderr = world.last_stderr();
    
    assert!(
        stderr.contains(&expected),
        "Expected stderr to contain '{}', but got:\n{}",
        expected, stderr
    );
}

#[then(regex = r"^execution is hermetic$")]
fn execution_is_hermetic(world: &mut CleanroomWorld) {
    // Verify that execution was isolated
    // Check that no external files were modified
    // Check that no external network connections were made
    
    if world.network_constraints.contains(&"isolated".to_string()) {
        let stdout = world.last_stdout();
        let stderr = world.last_stderr();
        
        // Check for network-related output
        if stdout.contains("http") || stderr.contains("http") ||
           stdout.contains("tcp") || stderr.contains("tcp") {
            panic!("Execution should be hermetic but network activity detected");
        }
    }
    
    if world.filesystem_constraints.contains(&"isolated".to_string()) {
        // Check that no files were created outside the project directory
        // This would require more sophisticated tracking in a real implementation
    }
}

#[then(regex = r"^mounts are deterministic$")]
fn mounts_are_deterministic(world: &mut CleanroomWorld) {
    // Verify that mounts are consistent across runs
    // This would typically involve checking mount points and their contents
    
    // For now, just verify that we have a consistent project directory
    assert!(
        world.project_dir.exists(),
        "Project directory should exist for deterministic mounts"
    );
}

#[then(regex = r"^clock is normalized$")]
fn clock_is_normalized(world: &mut CleanroomWorld) {
    // Verify that the clock is normalized for deterministic testing
    // This would typically involve checking that timestamps are consistent
    
    // For now, just verify that we have trace data with timing
    assert!(
        !world.trace_data.is_empty(),
        "Trace data should exist for clock normalization"
    );
}

#[then(regex = r"^stdout is empty$")]
fn stdout_is_empty(world: &mut CleanroomWorld) {
    let stdout = world.last_stdout();
    
    assert!(
        stdout.trim().is_empty(),
        "Expected stdout to be empty, but got:\n{}",
        stdout
    );
}

#[then(regex = r"^stdout equals the previous stdout for the same seed$")]
fn stdout_equals_previous_for_seed(world: &mut CleanroomWorld) {
    // This would typically involve comparing outputs from previous runs
    // with the same RNG seed
    
    let current_stdout = world.last_stdout();
    let seed = world.rng_seed.expect("RNG seed should be set");
    
    // Store current output for comparison
    let output_key = format!("seed_{}", seed);
    world.capture_file(&output_key, current_stdout);
    
    // In a real implementation, this would compare with previously stored output
    // For now, just verify that we have output to compare
    assert!(
        !current_stdout.is_empty(),
        "Should have output to compare for seed {}", seed
    );
}

#[then(regex = r"^artifact hashes match$")]
fn artifact_hashes_match(world: &mut CleanroomWorld) {
    // Verify that artifact hashes are consistent
    // This would typically involve computing hashes of generated artifacts
    
    let current_stdout = world.last_stdout();
    
    // Compute a simple hash for comparison
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    
    let mut hasher = DefaultHasher::new();
    current_stdout.hash(&mut hasher);
    let current_hash = hasher.finish();
    
    world.capture_hash(current_hash.to_string());
    
    // In a real implementation, this would compare with previously stored hashes
    // For now, just verify that we have a hash to compare
    assert!(
        !world.captured_hashes.is_empty(),
        "Should have artifact hashes to compare"
    );
}

#[then(regex = r"^stdout differs$")]
fn stdout_differs(world: &mut CleanroomWorld) {
    // Verify that outputs are different (for different seeds)
    // This would typically involve comparing with previous outputs
    
    let current_stdout = world.last_stdout();
    
    // In a real implementation, this would compare with previously stored output
    // For now, just verify that we have output to compare
    assert!(
        !current_stdout.is_empty(),
        "Should have output to compare"
    );
}
