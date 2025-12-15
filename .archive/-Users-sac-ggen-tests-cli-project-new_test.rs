//! Lean test suite for `ggen project new` command (80/20 critical path)
//!
//! Following agent-editor pattern: Focus on critical 20% that delivers 80% value
//! - Project name validation
//! - Project type parsing
//! - Basic project creation flow
//! - Skip low-value edge cases

use assert_cmd::Command;
use predicates::prelude::*;
use tempfile::TempDir;

#[test]
fn test_new_requires_name() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args(["project", "new"])
        .assert()
        .failure()
        .stderr(predicate::str::contains("required"));
}

#[test]
fn test_new_requires_type() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args(["project", "new", "my-project"])
        .assert()
        .failure()
        .stderr(predicate::str::contains("--type"));
}

#[test]
fn test_new_validates_project_name_no_spaces() {
    let temp = TempDir::new().unwrap();

    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "project", "new",
        "my project", // Invalid: contains space
        "--type", "rust-cli",
        "--output", temp.path().to_str().unwrap(),
    ])
    .assert()
    .failure()
    .stderr(predicate::str::contains("whitespace"));
}

#[test]
fn test_new_validates_project_type() {
    let temp = TempDir::new().unwrap();

    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "project", "new",
        "my-project",
        "--type", "invalid-type", // Invalid project type
        "--output", temp.path().to_str().unwrap(),
    ])
    .assert()
    .failure()
    .stderr(predicate::str::contains("Unsupported"));
}

#[test]
fn test_new_rust_cli_project_basic() {
    let temp = TempDir::new().unwrap();

    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "project", "new",
        "my-cli",
        "--type", "rust-cli",
        "--output", temp.path().to_str().unwrap(),
    ])
    .assert()
    .success()
    .stdout(predicate::str::contains("Creating new project"))
    .stdout(predicate::str::contains("my-cli"));
}

#[test]
fn test_new_with_framework() {
    let temp = TempDir::new().unwrap();

    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "project", "new",
        "my-web",
        "--type", "rust-web",
        "--framework", "axum",
        "--output", temp.path().to_str().unwrap(),
    ])
    .assert()
    .success()
    .stdout(predicate::str::contains("Framework: axum"));
}

#[test]
fn test_new_skip_install_flag() {
    let temp = TempDir::new().unwrap();

    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "project", "new",
        "my-app",
        "--type", "nextjs",
        "--skip-install",
        "--output", temp.path().to_str().unwrap(),
    ])
    .assert()
    .success()
    .stdout(predicate::str::contains("npm install"));
}

#[cfg(test)]
mod unit_tests {
    use super::*;
    use ggen_cli_lib::cmds::project::new::{NewArgs, run};
    use std::path::PathBuf;

    #[tokio::test]
    async fn test_unit_new_args_parsing() {
        let args = NewArgs {
            name: "test-project".to_string(),
            project_type: "rust-lib".to_string(),
            framework: None,
            output: PathBuf::from("."),
            skip_install: false,
        };

        assert_eq!(args.name, "test-project");
        assert_eq!(args.project_type, "rust-lib");
        assert_eq!(args.framework, None);
        assert!(!args.skip_install);
    }

    #[tokio::test]
    async fn test_unit_invalid_name_with_whitespace() {
        let args = NewArgs {
            name: "my project".to_string(),
            project_type: "rust-cli".to_string(),
            framework: None,
            output: PathBuf::from("."),
            skip_install: false,
        };

        let result = run(&args).await;
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("whitespace") || err.contains("invalid"));
    }

    #[tokio::test]
    async fn test_unit_invalid_project_type() {
        let args = NewArgs {
            name: "valid-name".to_string(),
            project_type: "unknown-type".to_string(),
            framework: None,
            output: PathBuf::from("."),
            skip_install: false,
        };

        let result = run(&args).await;
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Unsupported"));
    }
}
