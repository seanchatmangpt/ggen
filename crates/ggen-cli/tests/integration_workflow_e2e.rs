//! End-to-end integration tests for workflow commands
//!
//! **Chicago TDD Principles**:
//! - REAL CLI process execution
//! - REAL file system operations for workflow files
//! - REAL state verification of workflow state
//! - NO mocking of workflow processing
//!
//! **Critical User Workflows (80/20)**:
//! 1. Initialize workflow tracking
//! 2. Discover process patterns
//! 3. Analyze workflow events
//! 4. Track workflow events
//! 5. Generate workflow reports

use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;
use tempfile::TempDir;

/// Helper to create ggen command
fn ggen() -> Command {
    Command::cargo_bin("ggen").expect("Failed to find ggen binary")
}

/// Helper to create test workflow file
fn create_test_workflow(temp_dir: &TempDir) -> std::path::PathBuf {
    let workflow_file = temp_dir.path().join("workflow.json");

    let workflow_content = r#"{
        "workflow_name": "test-workflow",
        "events": [
            {
                "case_id": "case-1",
                "activity": "Start",
                "timestamp": "2024-01-01T00:00:00Z"
            },
            {
                "case_id": "case-1",
                "activity": "Process",
                "timestamp": "2024-01-01T01:00:00Z"
            }
        ]
    }"#;

    fs::write(&workflow_file, workflow_content).expect("Failed to write workflow file");
    workflow_file
}

#[test]
fn test_workflow_init_creates_workflow() {
    // Chicago TDD: Verify workflow initialization
    let temp_dir = TempDir::new().unwrap();

    ggen()
        .arg("workflow")
        .arg("init")
        .arg("--name")
        .arg("test-workflow")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Verify state: Workflow file created
    let workflow_path = temp_dir.path().join(".workflows/test-workflow.json");
    assert!(workflow_path.exists(), "Workflow file should be created");
}

#[test]
fn test_workflow_init_with_type() {
    // Chicago TDD: Verify workflow type specification
    let temp_dir = TempDir::new().unwrap();

    ggen()
        .arg("workflow")
        .arg("init")
        .arg("--name")
        .arg("research-workflow")
        .arg("--type")
        .arg("research")
        .current_dir(&temp_dir)
        .assert()
        .success();
}

#[test]
fn test_workflow_init_with_output_dir() {
    // Chicago TDD: Verify custom output directory
    let temp_dir = TempDir::new().unwrap();

    let output_dir = temp_dir.path().join("custom-workflows");

    ggen()
        .arg("workflow")
        .arg("init")
        .arg("--name")
        .arg("custom-workflow")
        .arg("--output-dir")
        .arg(output_dir.to_str().unwrap())
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Verify state: Custom output directory used
    assert!(output_dir.exists(), "Custom output directory should be created");
}

#[test]
fn test_workflow_analyze() {
    // Chicago TDD: Verify workflow analysis
    let temp_dir = TempDir::new().unwrap();
    let workflow_file = create_test_workflow(&temp_dir);

    ggen()
        .arg("workflow")
        .arg("analyze")
        .arg("--workflow-file")
        .arg(workflow_file.to_str().unwrap())
        .current_dir(&temp_dir)
        .assert()
        .success()
        .stdout(
            predicate::str::contains("cases")
                .or(predicate::str::contains("events"))
                .or(predicate::str::contains("workflow")),
        );
}

#[test]
fn test_workflow_analyze_summary() {
    // Chicago TDD: Verify summary analysis
    let temp_dir = TempDir::new().unwrap();
    let workflow_file = create_test_workflow(&temp_dir);

    ggen()
        .arg("workflow")
        .arg("analyze")
        .arg("--workflow-file")
        .arg(workflow_file.to_str().unwrap())
        .arg("--summary")
        .current_dir(&temp_dir)
        .assert()
        .success();
}

#[test]
fn test_workflow_discover() {
    // Chicago TDD: Verify process discovery
    let temp_dir = TempDir::new().unwrap();
    let workflow_file = create_test_workflow(&temp_dir);

    ggen()
        .arg("workflow")
        .arg("discover")
        .arg("--workflow-file")
        .arg(workflow_file.to_str().unwrap())
        .current_dir(&temp_dir)
        .assert()
        .success()
        .stdout(
            predicate::str::contains("graph")
                .or(predicate::str::contains("edges"))
                .or(predicate::str::contains("path")),
        );
}

#[test]
fn test_workflow_discover_pareto() {
    // Chicago TDD: Verify Pareto analysis (80/20 rule)
    let temp_dir = TempDir::new().unwrap();
    let workflow_file = create_test_workflow(&temp_dir);

    ggen()
        .arg("workflow")
        .arg("discover")
        .arg("--workflow-file")
        .arg(workflow_file.to_str().unwrap())
        .arg("--pareto")
        .current_dir(&temp_dir)
        .assert()
        .success();
}

#[test]
fn test_workflow_discover_export_mermaid() {
    // Chicago TDD: Verify Mermaid diagram export
    let temp_dir = TempDir::new().unwrap();
    let workflow_file = create_test_workflow(&temp_dir);

    ggen()
        .arg("workflow")
        .arg("discover")
        .arg("--workflow-file")
        .arg(workflow_file.to_str().unwrap())
        .arg("--export")
        .arg("mermaid")
        .current_dir(&temp_dir)
        .assert()
        .success()
        .stdout(predicate::str::contains("graph").or(predicate::str::contains("mermaid")));
}

#[test]
fn test_workflow_event() {
    // Chicago TDD: Verify event tracking
    let temp_dir = TempDir::new().unwrap();
    let workflow_file = create_test_workflow(&temp_dir);

    ggen()
        .arg("workflow")
        .arg("event")
        .arg("--workflow-file")
        .arg(workflow_file.to_str().unwrap())
        .arg("--case-id")
        .arg("case-2")
        .arg("--activity")
        .arg("Complete")
        .current_dir(&temp_dir)
        .assert()
        .success()
        .stdout(predicate::str::contains("Event recorded").or(predicate::str::contains("timestamp")));
}

#[test]
fn test_workflow_event_with_resource() {
    // Chicago TDD: Verify event with resource tracking
    let temp_dir = TempDir::new().unwrap();
    let workflow_file = create_test_workflow(&temp_dir);

    ggen()
        .arg("workflow")
        .arg("event")
        .arg("--workflow-file")
        .arg(workflow_file.to_str().unwrap())
        .arg("--case-id")
        .arg("case-3")
        .arg("--activity")
        .arg("Review")
        .arg("--resource")
        .arg("reviewer-1")
        .current_dir(&temp_dir)
        .assert()
        .success();
}

#[test]
fn test_workflow_report() {
    // Chicago TDD: Verify report generation
    let temp_dir = TempDir::new().unwrap();
    let workflow_file = create_test_workflow(&temp_dir);

    ggen()
        .arg("workflow")
        .arg("report")
        .arg("--workflow-file")
        .arg(workflow_file.to_str().unwrap())
        .current_dir(&temp_dir)
        .assert()
        .success()
        .stdout(predicate::str::contains("Report generated").or(predicate::str::contains("status")));
}

#[test]
fn test_workflow_report_html_format() {
    // Chicago TDD: Verify HTML report generation
    let temp_dir = TempDir::new().unwrap();
    let workflow_file = create_test_workflow(&temp_dir);

    let output_file = temp_dir.path().join("report.html");

    ggen()
        .arg("workflow")
        .arg("report")
        .arg("--workflow-file")
        .arg(workflow_file.to_str().unwrap())
        .arg("--format")
        .arg("html")
        .arg("--output")
        .arg(output_file.to_str().unwrap())
        .current_dir(&temp_dir)
        .assert()
        .success();
}

#[test]
fn test_workflow_report_json_format() {
    // Chicago TDD: Verify JSON report generation
    let temp_dir = TempDir::new().unwrap();
    let workflow_file = create_test_workflow(&temp_dir);

    let output_file = temp_dir.path().join("report.json");

    ggen()
        .arg("workflow")
        .arg("report")
        .arg("--workflow-file")
        .arg(workflow_file.to_str().unwrap())
        .arg("--format")
        .arg("json")
        .arg("--output")
        .arg(output_file.to_str().unwrap())
        .current_dir(&temp_dir)
        .assert()
        .success();
}

#[test]
fn test_workflow_help_shows_verbs() {
    // Chicago TDD: Verify help state is comprehensive
    ggen()
        .arg("workflow")
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("init"))
        .stdout(predicate::str::contains("analyze"))
        .stdout(predicate::str::contains("discover"))
        .stdout(predicate::str::contains("event"))
        .stdout(predicate::str::contains("report"));
}

#[test]
fn test_workflow_invalid_verb() {
    // Chicago TDD: Verify error handling for invalid verbs
    ggen()
        .arg("workflow")
        .arg("invalid-verb")
        .assert()
        .failure()
        .stderr(predicate::str::contains("error").or(predicate::str::contains("invalid")));
}

#[test]
fn test_workflow_analyze_missing_file() {
    // Chicago TDD: Verify error state for missing workflow file
    let temp_dir = TempDir::new().unwrap();

    ggen()
        .arg("workflow")
        .arg("analyze")
        .arg("--workflow-file")
        .arg("/nonexistent/workflow.json")
        .current_dir(&temp_dir)
        .assert()
        .failure()
        .stderr(predicate::str::contains("not found").or(predicate::str::contains("error")));
}
