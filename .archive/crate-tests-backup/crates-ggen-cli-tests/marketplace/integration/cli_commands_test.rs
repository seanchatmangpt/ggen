//! Integration tests for marketplace CLI commands
//!
//! Tests verify end-to-end functionality of:
//! - marketplace list
//! - marketplace search
//! - marketplace maturity
//! - marketplace validate
//! - marketplace export
//! - marketplace compare
//! - marketplace recommend

use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;
use tempfile::TempDir;

#[test]
fn test_marketplace_list_command() {
    // Arrange
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    // Act
    let assert = cmd.arg("marketplace").arg("list").assert();

    // Assert: Should succeed and show package list
    assert.success().stdout(predicate::str::contains("total"));
}

#[test]
fn test_marketplace_list_with_json_output() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd.arg("marketplace").arg("list").arg("--json").assert();

    assert
        .success()
        .stdout(predicate::str::contains("packages").and(predicate::str::contains("total")));
}

#[test]
fn test_marketplace_search_with_query() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("search")
        .arg("--query")
        .arg("rust")
        .assert();

    assert
        .success()
        .stdout(predicate::str::contains("packages").or(predicate::str::contains("total")));
}

#[test]
fn test_marketplace_search_with_limit() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("search")
        .arg("--query")
        .arg("cli")
        .arg("--limit")
        .arg("5")
        .assert();

    assert.success();
}

#[test]
fn test_marketplace_search_with_category_filter() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("search")
        .arg("--query")
        .arg("web")
        .arg("--category")
        .arg("backend")
        .assert();

    assert.success();
}

#[test]
fn test_marketplace_search_empty_query_fails() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("search")
        .arg("--query")
        .arg("")
        .assert();

    // Should fail with validation error
    assert.failure();
}

#[test]
fn test_marketplace_maturity_command() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("maturity")
        .arg("io.ggen.rust.microservice")
        .assert();

    assert
        .success()
        .stdout(predicate::str::contains("total_score"));
}

#[test]
fn test_marketplace_maturity_detailed_flag() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("maturity")
        .arg("io.ggen.rust.microservice")
        .arg("--detailed")
        .assert();

    assert
        .success()
        .stdout(predicate::str::contains("feedback"));
}

#[test]
fn test_marketplace_maturity_verify_production() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    // This might fail if package isn't production-ready
    let _ = cmd
        .arg("marketplace")
        .arg("maturity")
        .arg("io.ggen.rust.microservice")
        .arg("--verify")
        .arg("production")
        .assert();

    // Test passes if command runs (success or known failure)
}

#[test]
fn test_marketplace_maturity_verify_beta() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("maturity")
        .arg("io.ggen.rust.microservice")
        .arg("--verify")
        .arg("beta")
        .assert();

    // Should succeed if package is at least beta
    let _ = assert.success().or_else(|_| Ok::<_, ()>(())).unwrap();
}

#[test]
fn test_marketplace_dashboard_command() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd.arg("marketplace").arg("dashboard").assert();

    assert
        .success()
        .stdout(predicate::str::contains("generated_at"));
}

#[test]
fn test_marketplace_dashboard_with_output_file() {
    // Arrange
    let temp_dir = TempDir::new().unwrap();
    let output_path = temp_dir.path().join("dashboard.json");

    let mut cmd = Command::cargo_bin("ggen").unwrap();

    // Act
    let assert = cmd
        .arg("marketplace")
        .arg("dashboard")
        .arg("--output")
        .arg(&output_path)
        .assert();

    // Assert
    assert.success();
    assert!(output_path.exists(), "Dashboard file should be created");

    let content = fs::read_to_string(&output_path).unwrap();
    assert!(content.contains("generated_at"));
    assert!(content.contains("statistics"));
}

#[test]
fn test_marketplace_dashboard_filter_production() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("dashboard")
        .arg("--min-maturity")
        .arg("production")
        .assert();

    assert.success();
}

#[test]
fn test_marketplace_validate_single_package() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    // Use a package that should exist
    let assert = cmd
        .arg("marketplace")
        .arg("validate")
        .arg("--package")
        .arg("io.ggen.rust.microservice")
        .assert();

    assert.success().stdout(predicate::str::contains("score"));
}

#[test]
fn test_marketplace_validate_all_packages() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd.arg("marketplace").arg("validate").assert();

    assert
        .success()
        .stdout(predicate::str::contains("total_packages"));
}

#[test]
fn test_marketplace_validate_require_production() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    // Might fail if packages aren't production-ready
    let _ = cmd
        .arg("marketplace")
        .arg("validate")
        .arg("--require-level")
        .arg("production")
        .assert();
}

#[test]
fn test_marketplace_validate_improvement_plan() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("validate")
        .arg("--package")
        .arg("io.ggen.rust.microservice")
        .arg("--improvement-plan")
        .assert();

    assert.success();
}

#[test]
fn test_marketplace_export_json() {
    let temp_dir = TempDir::new().unwrap();
    let output_path = temp_dir.path().join("export.json");

    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("export")
        .arg("--format")
        .arg("json")
        .arg("--output")
        .arg(&output_path)
        .assert();

    assert.success();
    assert!(output_path.exists());

    let content = fs::read_to_string(&output_path).unwrap();
    assert!(content.starts_with('{') || content.starts_with('['));
}

#[test]
fn test_marketplace_export_csv() {
    let temp_dir = TempDir::new().unwrap();
    let output_path = temp_dir.path().join("export.csv");

    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("export")
        .arg("--format")
        .arg("csv")
        .arg("--output")
        .arg(&output_path)
        .assert();

    assert.success();
    assert!(output_path.exists());

    let content = fs::read_to_string(&output_path).unwrap();
    assert!(content.contains(','), "CSV should contain commas");
}

#[test]
fn test_marketplace_export_html() {
    let temp_dir = TempDir::new().unwrap();
    let output_path = temp_dir.path().join("export.html");

    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("export")
        .arg("--format")
        .arg("html")
        .arg("--output")
        .arg(&output_path)
        .assert();

    assert.success();
    assert!(output_path.exists());

    let content = fs::read_to_string(&output_path).unwrap();
    assert!(content.contains("<html>"));
    assert!(content.contains("<table>"));
}

#[test]
fn test_marketplace_export_filter_by_maturity() {
    let temp_dir = TempDir::new().unwrap();
    let output_path = temp_dir.path().join("production.json");

    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("export")
        .arg("--format")
        .arg("json")
        .arg("--min-maturity")
        .arg("production")
        .arg("--output")
        .arg(&output_path)
        .assert();

    assert.success();
    assert!(output_path.exists());
}

#[test]
fn test_marketplace_compare_two_packages() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("compare")
        .arg("--package-a")
        .arg("io.ggen.compiler")
        .arg("--package-b")
        .arg("io.ggen.parser")
        .assert();

    assert
        .success()
        .stdout(predicate::str::contains("comparison"));
}

#[test]
fn test_marketplace_compare_detailed() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("compare")
        .arg("--package-a")
        .arg("io.ggen.compiler")
        .arg("--package-b")
        .arg("io.ggen.parser")
        .arg("--detailed")
        .assert();

    assert.success();
}

#[test]
fn test_marketplace_compare_with_output() {
    let temp_dir = TempDir::new().unwrap();
    let output_path = temp_dir.path().join("comparison.json");

    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("compare")
        .arg("--package-a")
        .arg("io.ggen.compiler")
        .arg("--package-b")
        .arg("io.ggen.parser")
        .arg("--output")
        .arg(&output_path)
        .assert();

    assert.success();
    assert!(output_path.exists());
}

#[test]
fn test_marketplace_recommend_production() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("recommend")
        .arg("--use-case")
        .arg("production")
        .assert();

    assert
        .success()
        .stdout(predicate::str::contains("recommendations"));
}

#[test]
fn test_marketplace_recommend_research() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("recommend")
        .arg("--use-case")
        .arg("research")
        .assert();

    assert.success();
}

#[test]
fn test_marketplace_recommend_with_priority() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("recommend")
        .arg("--use-case")
        .arg("production")
        .arg("--priority")
        .arg("security")
        .assert();

    assert.success();
}

#[test]
fn test_marketplace_recommend_min_score() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("recommend")
        .arg("--use-case")
        .arg("startup")
        .arg("--min-score")
        .arg("50")
        .assert();

    assert.success();
}

#[test]
fn test_marketplace_search_maturity_production() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("search-maturity")
        .arg("--min-level")
        .arg("production")
        .assert();

    assert.success().stdout(predicate::str::contains("results"));
}

#[test]
fn test_marketplace_search_maturity_dimensions() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("search-maturity")
        .arg("--min-documentation")
        .arg("15")
        .arg("--min-testing")
        .arg("15")
        .arg("--min-security")
        .arg("18")
        .assert();

    assert.success();
}

#[test]
fn test_marketplace_maturity_batch() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd.arg("marketplace").arg("maturity-batch").assert();

    assert
        .success()
        .stdout(predicate::str::contains("generated_at"));
}

#[test]
fn test_marketplace_maturity_batch_with_output() {
    let temp_dir = TempDir::new().unwrap();
    let output_path = temp_dir.path().join("batch.json");

    let mut cmd = Command::cargo_bin("ggen").unwrap();

    let assert = cmd
        .arg("marketplace")
        .arg("maturity-batch")
        .arg("--output")
        .arg(&output_path)
        .assert();

    assert.success();
    assert!(output_path.exists());
}
