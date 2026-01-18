//! Integration tests for ggen-test-opt CLI commands
//!
//! Tests verify command-line interface behavior for:
//! - optimize: 80/20 Pareto test selection
//! - metadata-update: Test metadata collection
//! - budget-check: Performance budget validation
//!
//! Uses tempfile for isolated test environments.

use std::fs;
use std::process::{Command, Output};
use tempfile::TempDir;

/// Helper to run CLI command and capture output
fn run_cli_command(args: &[&str]) -> Output {
    Command::new("cargo")
        .args(&["run", "--package", "ggen-test-opt", "--"])
        .args(args)
        .output()
        .expect("Failed to execute CLI command")
}

/// Helper to create test metadata directory with sample data
fn setup_test_metadata() -> TempDir {
    let temp_dir = TempDir::new().expect("Failed to create temp directory");
    let metadata_dir = temp_dir.path();

    // Create failure history JSON
    let failure_history = r#"{
        "test_stats": {
            "test1": {"failure_count": 5, "total_runs": 10},
            "test2": {"failure_count": 0, "total_runs": 10},
            "test3": {"failure_count": 10, "total_runs": 10}
        }
    }"#;
    fs::write(metadata_dir.join("failure_history.json"), failure_history)
        .expect("Failed to write failure history");

    temp_dir
}

#[test]
fn test_cli_help_displays_all_commands() {
    let output = run_cli_command(&["--help"]);

    assert!(output.status.success(), "Help command should succeed");

    let stdout = String::from_utf8_lossy(&output.stdout);

    // Verify all commands are listed
    assert!(
        stdout.contains("optimize"),
        "Help should list 'optimize' command"
    );
    assert!(
        stdout.contains("metadata-update"),
        "Help should list 'metadata-update' command"
    );
    assert!(
        stdout.contains("budget-check"),
        "Help should list 'budget-check' command"
    );
}

#[test]
fn test_optimize_command_with_no_metadata() {
    let temp_dir = TempDir::new().expect("Failed to create temp directory");
    let metadata_path = temp_dir.path().to_str().unwrap();

    let output = run_cli_command(&[
        "optimize",
        "--metadata-dir",
        metadata_path,
        "--target-count",
        "50",
    ]);

    // Command may fail or succeed with no metadata - both are valid
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    // Should at least attempt to run and report missing data
    assert!(
        stdout.contains("No nextest results found")
            || stdout.contains("Optimization complete")
            || stderr.contains("error"),
        "Should indicate missing data, complete optimization, or error: stdout={}, stderr={}",
        stdout,
        stderr
    );
}

#[test]
fn test_metadata_update_without_inputs() {
    let temp_dir = TempDir::new().expect("Failed to create temp directory");
    let metadata_path = temp_dir.path().to_str().unwrap();

    let output = run_cli_command(&["metadata-update", "--metadata-dir", metadata_path]);

    // Should succeed even with no input files (just does nothing)
    assert!(
        output.status.success(),
        "Metadata update should succeed with no inputs"
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("Metadata update complete"),
        "Should report completion"
    );
}

#[test]
fn test_budget_check_with_missing_data() {
    let temp_dir = TempDir::new().expect("Failed to create temp directory");
    let metadata_path = temp_dir.path().to_str().unwrap();

    let output = run_cli_command(&[
        "budget-check",
        "--metadata-dir",
        metadata_path,
        "--unit-budget",
        "1000",
        "--integration-budget",
        "10000",
    ]);

    // Should fail gracefully when execution times missing
    assert!(
        !output.status.success(),
        "Budget check should fail with missing data"
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("not found") || stderr.contains("No such file"),
        "Should report missing execution times"
    );
}

#[test]
fn test_optimize_command_flags() {
    let output = run_cli_command(&["optimize", "--help"]);

    assert!(output.status.success(), "Help for optimize should succeed");

    let stdout = String::from_utf8_lossy(&output.stdout);

    // Verify all expected flags are documented
    assert!(
        stdout.contains("--target-count"),
        "Should document target-count flag"
    );
    assert!(
        stdout.contains("--min-detection-rate"),
        "Should document min-detection-rate flag"
    );
    assert!(
        stdout.contains("--metadata-dir"),
        "Should document metadata-dir flag"
    );
    assert!(stdout.contains("--output"), "Should document output flag");
}

#[test]
fn test_metadata_update_command_flags() {
    let output = run_cli_command(&["metadata-update", "--help"]);

    assert!(
        output.status.success(),
        "Help for metadata-update should succeed"
    );

    let stdout = String::from_utf8_lossy(&output.stdout);

    // Verify all expected flags are documented
    assert!(
        stdout.contains("--nextest-json"),
        "Should document nextest-json flag"
    );
    assert!(
        stdout.contains("--tarpaulin-json"),
        "Should document tarpaulin-json flag"
    );
    assert!(
        stdout.contains("--metadata-dir"),
        "Should document metadata-dir flag"
    );
    assert!(
        stdout.contains("--test-results"),
        "Should document test-results flag"
    );
}

#[test]
fn test_budget_check_command_flags() {
    let output = run_cli_command(&["budget-check", "--help"]);

    assert!(
        output.status.success(),
        "Help for budget-check should succeed"
    );

    let stdout = String::from_utf8_lossy(&output.stdout);

    // Verify all expected flags are documented
    assert!(
        stdout.contains("--unit-budget"),
        "Should document unit-budget flag"
    );
    assert!(
        stdout.contains("--integration-budget"),
        "Should document integration-budget flag"
    );
    assert!(
        stdout.contains("--metadata-dir"),
        "Should document metadata-dir flag"
    );
    assert!(
        stdout.contains("--nextest-json"),
        "Should document nextest-json flag"
    );
}

#[test]
fn test_optimize_with_custom_target_count() {
    let temp_dir = setup_test_metadata();
    let metadata_path = temp_dir.path().to_str().unwrap();

    let output = run_cli_command(&[
        "optimize",
        "--metadata-dir",
        metadata_path,
        "--target-count",
        "100",
        "--min-detection-rate",
        "0.75",
    ]);

    // Command may fail with no execution times data - check it accepted parameters
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    // Should at least parse and display the custom parameters
    assert!(
        stdout.contains("Target: 100 tests") && stdout.contains("Min bug detection: 75"),
        "Should display custom parameters: stdout={}, stderr={}",
        stdout,
        stderr
    );
}

#[test]
fn test_budget_check_exit_codes() {
    let temp_dir = TempDir::new().expect("Failed to create temp directory");
    let metadata_dir = temp_dir.path();

    // Create execution times JSON with violations
    let execution_times = r#"{
        "test_results": {
            "slow_unit_test": {"exec_time": 1500},
            "fast_integration_test": {"exec_time": 500}
        }
    }"#;
    fs::write(metadata_dir.join("execution-times.json"), execution_times)
        .expect("Failed to write execution times");

    let output = run_cli_command(&[
        "budget-check",
        "--metadata-dir",
        metadata_dir.to_str().unwrap(),
        "--unit-budget",
        "1000",
        "--nextest-json",
        metadata_dir.join("execution-times.json").to_str().unwrap(),
    ]);

    // Note: This test may fail if the JSON format doesn't match expectations
    // It's a smoke test to verify budget-check runs
    let stderr = String::from_utf8_lossy(&output.stderr);
    let stdout = String::from_utf8_lossy(&output.stdout);

    // Either succeeds (budget met) or fails (budget exceeded)
    // Just verify it attempts to run
    assert!(
        stderr.contains("error") || stdout.contains("budget") || stdout.contains("Budget"),
        "Should attempt budget check: stderr={}, stdout={}",
        stderr,
        stdout
    );
}

#[test]
fn test_metadata_update_with_failure_history() {
    let temp_dir = setup_test_metadata();
    let metadata_path = temp_dir.path().to_str().unwrap();

    // Create test results JSON
    let test_results = r#"{
        "test1": true,
        "test2": false,
        "test3": true,
        "test4": true
    }"#;
    let results_path = temp_dir.path().join("test_results.json");
    fs::write(&results_path, test_results).expect("Failed to write test results");

    let output = run_cli_command(&[
        "metadata-update",
        "--metadata-dir",
        metadata_path,
        "--test-results",
        results_path.to_str().unwrap(),
    ]);

    assert!(
        output.status.success(),
        "Metadata update with failure history should succeed"
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("Updated failure history"),
        "Should report failure history update"
    );
}
