//! CI Validate Command Tests
//!
//! Test suite for CI/CD workflow validation command.
//! Follows 80/20 principle: focus on critical functionality only.

use assert_cmd::Command;
use assert_fs::prelude::*;
use predicates::prelude::*;

#[test]
fn test_ci_validate_requires_workflow_or_all() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args(["ci", "validate"]);

    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("Either --workflow or --all must be specified"));
}

#[test]
fn test_ci_validate_single_workflow_not_found() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args(["ci", "validate", "--workflow", "nonexistent.yml"]);

    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("Failed to read workflow"));
}

#[test]
fn test_ci_validate_all_no_workflows_directory() {
    let temp = assert_fs::TempDir::new().unwrap();
    std::env::set_current_dir(&temp).unwrap();

    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args(["ci", "validate", "--all"]);

    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("No .github/workflows directory found"));
}

#[test]
fn test_ci_validate_valid_yaml_workflow() {
    let temp = assert_fs::TempDir::new().unwrap();
    let workflows_dir = temp.child(".github/workflows");
    workflows_dir.create_dir_all().unwrap();

    let workflow_file = workflows_dir.child("test.yml");
    workflow_file.write_str(
        r#"
name: Test Workflow
on: [push]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - run: echo "test"
"#,
    ).unwrap();

    std::env::set_current_dir(&temp).unwrap();

    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args(["ci", "validate", "--workflow", ".github/workflows/test.yml"]);

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Valid YAML syntax"))
        .stdout(predicate::str::contains("All workflows are valid"));
}

#[test]
fn test_ci_validate_invalid_yaml() {
    let temp = assert_fs::TempDir::new().unwrap();
    let workflows_dir = temp.child(".github/workflows");
    workflows_dir.create_dir_all().unwrap();

    let workflow_file = workflows_dir.child("invalid.yml");
    workflow_file.write_str(
        r#"
invalid: yaml: syntax:
  - this is: [broken
"#,
    ).unwrap();

    std::env::set_current_dir(&temp).unwrap();

    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args(["ci", "validate", "--workflow", ".github/workflows/invalid.yml"]);

    cmd.assert()
        .failure()
        .stdout(predicate::str::contains("Invalid YAML"));
}

#[test]
fn test_ci_validate_all_workflows() {
    let temp = assert_fs::TempDir::new().unwrap();
    let workflows_dir = temp.child(".github/workflows");
    workflows_dir.create_dir_all().unwrap();

    // Create multiple valid workflows
    for i in 1..=3 {
        let workflow = workflows_dir.child(format!("workflow{}.yml", i));
        workflow.write_str(&format!(
            r#"
name: Workflow {}
on: [push]
jobs:
  job{}:
    runs-on: ubuntu-latest
    steps:
      - run: echo "test"
"#,
            i, i
        )).unwrap();
    }

    std::env::set_current_dir(&temp).unwrap();

    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args(["ci", "validate", "--all"]);

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Validating 3 workflow(s)"))
        .stdout(predicate::str::contains("All workflows are valid"));
}

#[test]
fn test_ci_validate_with_verbose() {
    let temp = assert_fs::TempDir::new().unwrap();
    let workflows_dir = temp.child(".github/workflows");
    workflows_dir.create_dir_all().unwrap();

    let workflow_file = workflows_dir.child("test.yml");
    workflow_file.write_str("name: Test\non: [push]").unwrap();

    std::env::set_current_dir(&temp).unwrap();

    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args(["ci", "validate", "--workflow", ".github/workflows/test.yml", "--verbose"]);

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Size:"));
}

#[test]
fn test_ci_validate_with_security() {
    let temp = assert_fs::TempDir::new().unwrap();
    let workflows_dir = temp.child(".github/workflows");
    workflows_dir.create_dir_all().unwrap();

    let workflow_file = workflows_dir.child("test.yml");
    workflow_file.write_str("name: Test\non: [push]").unwrap();

    std::env::set_current_dir(&temp).unwrap();

    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args(["ci", "validate", "--workflow", ".github/workflows/test.yml", "--security"]);

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Security check: passed"));
}
