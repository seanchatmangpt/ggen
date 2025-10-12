//! End-to-end integration tests for lifecycle commands
//!
//! Tests the complete user journey through the lifecycle management system:
//! - Listing phases from make.toml
//! - Showing phase details
//! - Running individual phases
//! - Executing phase pipelines
//! - State persistence and tracking
//! - Error handling for edge cases

use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

/// Helper to create a test project with make.toml
fn create_test_project() -> TempDir {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");

    let make_toml = r#"
[project]
name = "test-project"
type = "webapp"
version = "1.0.0"
description = "Test project for lifecycle e2e tests"

[lifecycle.validate]
description = "Validate environment"
commands = ["echo '🔍 Validating environment...'"]

[lifecycle.lint]
description = "Lint code"
commands = ["echo '🔍 Linting code...'"]

[lifecycle.init]
description = "Initialize project structure"
commands = [
    "mkdir -p src tests docs",
    "echo 'Project initialized' > .initialized"
]

[lifecycle.setup]
description = "Install dependencies and configure environment"
commands = [
    "echo 'Installing dependencies...'",
    "echo 'Setup complete' > .setup"
]
watch = true

[lifecycle.dev]
description = "Start development server"
commands = ["echo 'Dev server running on port 3000'"]
port = 3000
watch = true

[lifecycle.build]
description = "Build production artifacts"
commands = [
    "echo 'Building project...'",
    "mkdir -p dist",
    "echo 'Build complete' > dist/output.txt"
]
cache = true

[lifecycle.test]
description = "Run test suite"
commands = [
    "echo 'Running tests...'",
    "echo 'All tests passed' > .test-results"
]

[lifecycle.deploy]
description = "Deploy to production"
commands = ["echo 'Deploying to production...'"]

[lifecycle.cleanup]
description = "Clean up temporary files"
commands = ["echo '🧹 Cleaning up...'"]

[hooks]
before_build = ["lint"]
after_build = ["test"]
"#;

    fs::write(temp_dir.path().join("make.toml"), make_toml)
        .expect("Failed to write make.toml");

    temp_dir
}

/// Helper to get the ggen binary path
fn ggen_cmd() -> Command {
    Command::cargo_bin("ggen").expect("Failed to find ggen binary")
}

/// Helper to assert state.json exists and is valid JSON
fn assert_state_exists(root: &std::path::Path) {
    let state_path = root.join(".ggen/state.json");
    assert!(
        state_path.exists(),
        "State file should exist at {}",
        state_path.display()
    );

    let state_content = fs::read_to_string(&state_path)
        .expect("Failed to read state.json");
    serde_json::from_str::<serde_json::Value>(&state_content)
        .expect("State file should be valid JSON");
}

#[test]
fn test_lifecycle_list_shows_all_phases() {
    let temp_dir = create_test_project();

    ggen_cmd()
        .arg("lifecycle")
        .arg("list")
        .arg("--root")
        .arg(temp_dir.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("Available lifecycle phases"))
        .stdout(predicate::str::contains("init"))
        .stdout(predicate::str::contains("setup"))
        .stdout(predicate::str::contains("dev"))
        .stdout(predicate::str::contains("build"))
        .stdout(predicate::str::contains("test"))
        .stdout(predicate::str::contains("deploy"))
        .stdout(predicate::str::contains("Initialize project structure"));
}

#[test]
fn test_lifecycle_list_without_make_toml() {
    let temp_dir = TempDir::new().unwrap();

    ggen_cmd()
        .arg("lifecycle")
        .arg("list")
        .arg("--root")
        .arg(temp_dir.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("No make.toml found"));
}

#[test]
fn test_lifecycle_show_displays_phase_details() {
    let temp_dir = create_test_project();

    ggen_cmd()
        .arg("lifecycle")
        .arg("show")
        .arg("init")
        .arg("--root")
        .arg(temp_dir.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("Phase: init"))
        .stdout(predicate::str::contains("Initialize project structure"))
        .stdout(predicate::str::contains("Commands:"))
        .stdout(predicate::str::contains("mkdir -p src tests docs"));
}

#[test]
fn test_lifecycle_show_with_metadata() {
    let temp_dir = create_test_project();

    ggen_cmd()
        .arg("lifecycle")
        .arg("show")
        .arg("dev")
        .arg("--root")
        .arg(temp_dir.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("Watch mode: true"))
        .stdout(predicate::str::contains("Port: 3000"));

    ggen_cmd()
        .arg("lifecycle")
        .arg("show")
        .arg("build")
        .arg("--root")
        .arg(temp_dir.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("Caching: true"))
        .stdout(predicate::str::contains("Before hooks:"))
        .stdout(predicate::str::contains("lint"))
        .stdout(predicate::str::contains("After hooks:"))
        .stdout(predicate::str::contains("test"));
}

#[test]
fn test_lifecycle_show_missing_phase() {
    let temp_dir = create_test_project();

    ggen_cmd()
        .arg("lifecycle")
        .arg("show")
        .arg("nonexistent")
        .arg("--root")
        .arg(temp_dir.path())
        .assert()
        .failure()
        .stderr(predicate::str::contains("Phase 'nonexistent' not found"));
}

#[test]
fn test_lifecycle_run_executes_phase() {
    let temp_dir = create_test_project();

    ggen_cmd()
        .arg("lifecycle")
        .arg("run")
        .arg("init")
        .arg("--root")
        .arg(temp_dir.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("Running phase: init"));

    // Verify files were created
    assert!(temp_dir.path().join("src").exists());
    assert!(temp_dir.path().join("tests").exists());
    assert!(temp_dir.path().join("docs").exists());
    assert!(temp_dir.path().join(".initialized").exists());
}

#[test]
fn test_lifecycle_run_creates_state_file() {
    let temp_dir = create_test_project();

    ggen_cmd()
        .arg("lifecycle")
        .arg("run")
        .arg("init")
        .arg("--root")
        .arg(temp_dir.path())
        .assert()
        .success();

    assert_state_exists(temp_dir.path());

    // Verify state content
    let state_path = temp_dir.path().join(".ggen/state.json");
    let state_content = fs::read_to_string(&state_path).unwrap();
    let state: serde_json::Value = serde_json::from_str(&state_content).unwrap();

    assert_eq!(state["last_phase"], "init");
    assert!(state["completed_phases"].as_array().unwrap().contains(&serde_json::json!("init")));
}

#[test]
fn test_lifecycle_run_with_environment() {
    let temp_dir = create_test_project();

    ggen_cmd()
        .arg("lifecycle")
        .arg("run")
        .arg("setup")
        .arg("--root")
        .arg(temp_dir.path())
        .arg("--env")
        .arg("production")
        .assert()
        .success()
        .stdout(predicate::str::contains("Running phase: setup"));

    // State should track the execution
    assert_state_exists(temp_dir.path());
}

#[test]
fn test_lifecycle_run_missing_phase() {
    let temp_dir = create_test_project();

    ggen_cmd()
        .arg("lifecycle")
        .arg("run")
        .arg("invalid-phase")
        .arg("--root")
        .arg(temp_dir.path())
        .assert()
        .failure()
        .stderr(predicate::str::contains("Phase 'invalid-phase' not found"));
}

#[test]
fn test_lifecycle_pipeline_sequential_execution() {
    let temp_dir = create_test_project();

    ggen_cmd()
        .arg("lifecycle")
        .arg("pipeline")
        .arg("init")
        .arg("setup")
        .arg("build")
        .arg("--root")
        .arg(temp_dir.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("Running phase: init"))
        .stdout(predicate::str::contains("Running phase: setup"))
        .stdout(predicate::str::contains("Running phase: build"))
        .stdout(predicate::str::contains("Pipeline completed: init → setup → build"));

    // Verify all phases executed
    assert!(temp_dir.path().join(".initialized").exists());
    assert!(temp_dir.path().join(".setup").exists());
    assert!(temp_dir.path().join("dist/output.txt").exists());

    // Verify state tracks all phases
    let state_path = temp_dir.path().join(".ggen/state.json");
    let state_content = fs::read_to_string(&state_path).unwrap();
    let state: serde_json::Value = serde_json::from_str(&state_content).unwrap();

    assert_eq!(state["last_phase"], "build");
    let completed = state["completed_phases"].as_array().unwrap();
    assert!(completed.contains(&serde_json::json!("init")));
    assert!(completed.contains(&serde_json::json!("setup")));
    assert!(completed.contains(&serde_json::json!("build")));
}

#[test]
fn test_lifecycle_pipeline_with_environment() {
    let temp_dir = create_test_project();

    ggen_cmd()
        .arg("lifecycle")
        .arg("pipeline")
        .arg("init")
        .arg("setup")
        .arg("--root")
        .arg(temp_dir.path())
        .arg("--env")
        .arg("staging")
        .assert()
        .success();

    assert_state_exists(temp_dir.path());
}

#[test]
fn test_lifecycle_pipeline_stops_on_missing_phase() {
    let temp_dir = create_test_project();

    ggen_cmd()
        .arg("lifecycle")
        .arg("pipeline")
        .arg("init")
        .arg("nonexistent")
        .arg("setup")
        .arg("--root")
        .arg(temp_dir.path())
        .assert()
        .failure()
        .stderr(predicate::str::contains("Phase 'nonexistent' not found"));
}

#[test]
fn test_state_persistence_across_runs() {
    let temp_dir = create_test_project();

    // First run
    ggen_cmd()
        .arg("lifecycle")
        .arg("run")
        .arg("init")
        .arg("--root")
        .arg(temp_dir.path())
        .assert()
        .success();

    // Second run - state should persist
    ggen_cmd()
        .arg("lifecycle")
        .arg("run")
        .arg("setup")
        .arg("--root")
        .arg(temp_dir.path())
        .assert()
        .success();

    // Verify state includes both phases
    let state_path = temp_dir.path().join(".ggen/state.json");
    let state_content = fs::read_to_string(&state_path).unwrap();
    let state: serde_json::Value = serde_json::from_str(&state_content).unwrap();

    assert_eq!(state["last_phase"], "setup");
    let completed = state["completed_phases"].as_array().unwrap();
    assert_eq!(completed.len(), 2);
}

#[test]
fn test_lifecycle_list_shows_last_executed_phase() {
    let temp_dir = create_test_project();

    // Run a phase first
    ggen_cmd()
        .arg("lifecycle")
        .arg("run")
        .arg("build")
        .arg("--root")
        .arg(temp_dir.path())
        .assert()
        .success();

    // List should show last executed
    ggen_cmd()
        .arg("lifecycle")
        .arg("list")
        .arg("--root")
        .arg(temp_dir.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("Last executed: build"));
}

#[test]
fn test_lifecycle_help_output() {
    ggen_cmd()
        .arg("lifecycle")
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("Universal lifecycle management"))
        .stdout(predicate::str::contains("list"))
        .stdout(predicate::str::contains("show"))
        .stdout(predicate::str::contains("run"))
        .stdout(predicate::str::contains("pipeline"));
}

#[test]
fn test_lifecycle_list_help() {
    ggen_cmd()
        .arg("lifecycle")
        .arg("list")
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("List all available lifecycle phases"))
        .stdout(predicate::str::contains("--root"));
}

#[test]
fn test_lifecycle_show_help() {
    ggen_cmd()
        .arg("lifecycle")
        .arg("show")
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("Show details of a specific phase"))
        .stdout(predicate::str::contains("<PHASE>"))
        .stdout(predicate::str::contains("--root"));
}

#[test]
fn test_lifecycle_run_help() {
    ggen_cmd()
        .arg("lifecycle")
        .arg("run")
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("Run a single lifecycle phase"))
        .stdout(predicate::str::contains("<PHASE>"))
        .stdout(predicate::str::contains("--root"))
        .stdout(predicate::str::contains("--env"));
}

#[test]
fn test_lifecycle_pipeline_help() {
    ggen_cmd()
        .arg("lifecycle")
        .arg("pipeline")
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("Run multiple phases in sequence"))
        .stdout(predicate::str::contains("<PHASES>"))
        .stdout(predicate::str::contains("--root"))
        .stdout(predicate::str::contains("--env"));
}

#[test]
fn test_hooks_execution_order() {
    let temp_dir = create_test_project();

    let output = ggen_cmd()
        .arg("lifecycle")
        .arg("run")
        .arg("build")
        .arg("--root")
        .arg(temp_dir.path())
        .output()
        .expect("Failed to execute command");

    let stdout = String::from_utf8_lossy(&output.stdout);

    // Verify hooks run in correct order: lint (before) -> build -> test (after)
    let lint_pos = stdout.find("Running phase: lint").unwrap();
    let build_pos = stdout.find("Running phase: build").unwrap();
    let test_pos = stdout.find("Running phase: test").unwrap();

    assert!(lint_pos < build_pos, "Lint should run before build");
    assert!(build_pos < test_pos, "Test should run after build");
}

#[test]
fn test_multiple_commands_in_phase() {
    let temp_dir = create_test_project();

    ggen_cmd()
        .arg("lifecycle")
        .arg("run")
        .arg("build")
        .arg("--root")
        .arg(temp_dir.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("Building project..."));

    // Verify both commands executed
    assert!(temp_dir.path().join("dist").exists());
    assert!(temp_dir.path().join("dist/output.txt").exists());
}

#[test]
fn test_empty_phase_list() {
    let temp_dir = TempDir::new().unwrap();

    // Create make.toml with no phases
    let make_toml = r#"
[project]
name = "test-project"
"#;
    fs::write(temp_dir.path().join("make.toml"), make_toml).unwrap();

    ggen_cmd()
        .arg("lifecycle")
        .arg("list")
        .arg("--root")
        .arg(temp_dir.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("(no phases defined)"));
}

#[test]
fn test_state_directory_creation() {
    let temp_dir = create_test_project();

    // State directory shouldn't exist initially
    assert!(!temp_dir.path().join(".ggen").exists());

    ggen_cmd()
        .arg("lifecycle")
        .arg("run")
        .arg("init")
        .arg("--root")
        .arg(temp_dir.path())
        .assert()
        .success();

    // State directory and file should be created
    assert!(temp_dir.path().join(".ggen").exists());
    assert!(temp_dir.path().join(".ggen").is_dir());
    assert_state_exists(temp_dir.path());
}

#[test]
fn test_concurrent_safe_state_updates() {
    let temp_dir = create_test_project();

    // Run multiple phases quickly
    ggen_cmd()
        .arg("lifecycle")
        .arg("run")
        .arg("init")
        .arg("--root")
        .arg(temp_dir.path())
        .assert()
        .success();

    ggen_cmd()
        .arg("lifecycle")
        .arg("run")
        .arg("setup")
        .arg("--root")
        .arg(temp_dir.path())
        .assert()
        .success();

    ggen_cmd()
        .arg("lifecycle")
        .arg("run")
        .arg("test")
        .arg("--root")
        .arg(temp_dir.path())
        .assert()
        .success();

    // State should be valid and contain all phases
    let state_path = temp_dir.path().join(".ggen/state.json");
    let state_content = fs::read_to_string(&state_path).unwrap();
    let state: serde_json::Value = serde_json::from_str(&state_content).unwrap();

    let completed = state["completed_phases"].as_array().unwrap();
    assert_eq!(completed.len(), 3);
}

#[test]
fn test_phase_without_hooks() {
    let temp_dir = create_test_project();

    // Init phase has no hooks defined
    let output = ggen_cmd()
        .arg("lifecycle")
        .arg("run")
        .arg("init")
        .arg("--root")
        .arg(temp_dir.path())
        .output()
        .expect("Failed to execute command");

    let stdout = String::from_utf8_lossy(&output.stdout);

    // Should run directly without hooks
    assert!(stdout.contains("Running phase: init"));
    // Should not run other phases as hooks
    assert!(!stdout.contains("Running phase: lint"));
    assert!(!stdout.contains("Running phase: test"));
}

#[test]
fn test_performance_fast_execution() {
    let temp_dir = create_test_project();

    let start = std::time::Instant::now();

    ggen_cmd()
        .arg("lifecycle")
        .arg("pipeline")
        .arg("init")
        .arg("setup")
        .arg("build")
        .arg("test")
        .arg("--root")
        .arg(temp_dir.path())
        .assert()
        .success();

    let duration = start.elapsed();

    // All tests should complete in under 5 seconds
    assert!(
        duration.as_secs() < 5,
        "Pipeline took too long: {:?}",
        duration
    );
}
