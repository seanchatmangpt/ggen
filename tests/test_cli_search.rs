use assert_cmd::Command;
use predicates::prelude::*;
use tempfile::TempDir;
use std::fs;

#[test]
fn test_search_command_help() {
    let mut cmd = Command::cargo_bin("rgen").unwrap();
    cmd.arg("search").arg("--help");
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Search for rpacks in registry"))
        .stdout(predicate::str::contains("--category"))
        .stdout(predicate::str::contains("--keyword"))
        .stdout(predicate::str::contains("--author"))
        .stdout(predicate::str::contains("--stable"))
        .stdout(predicate::str::contains("--limit"))
        .stdout(predicate::str::contains("--detailed"))
        .stdout(predicate::str::contains("--json"));
}

#[test]
fn test_categories_command_help() {
    let mut cmd = Command::cargo_bin("rgen").unwrap();
    cmd.arg("categories").arg("--help");
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Show popular categories and keywords"))
        .stdout(predicate::str::contains("--keywords"))
        .stdout(predicate::str::contains("--detailed"))
        .stdout(predicate::str::contains("--json"));
}

#[test]
fn test_search_command_basic_usage() {
    let mut cmd = Command::cargo_bin("rgen").unwrap();
    cmd.arg("search").arg("rust");
    // This will fail because there's no registry, but we can test the argument parsing
    cmd.assert()
        .failure() // Expected to fail without registry
        .stderr(predicate::str::contains("Failed to fetch registry index"));
}

#[test]
fn test_search_command_with_filters() {
    let mut cmd = Command::cargo_bin("rgen").unwrap();
    cmd.arg("search")
        .arg("api")
        .arg("--category")
        .arg("rust")
        .arg("--keyword")
        .arg("database")
        .arg("--author")
        .arg("test-author")
        .arg("--stable")
        .arg("--limit")
        .arg("5")
        .arg("--detailed");
    
    // This will fail because there's no registry, but we can test the argument parsing
    cmd.assert()
        .failure() // Expected to fail without registry
        .stderr(predicate::str::contains("Failed to fetch registry index"));
}

#[test]
fn test_search_command_json_output() {
    let mut cmd = Command::cargo_bin("rgen").unwrap();
    cmd.arg("search")
        .arg("test")
        .arg("--json");
    
    // This will fail because there's no registry, but we can test the argument parsing
    cmd.assert()
        .failure() // Expected to fail without registry
        .stderr(predicate::str::contains("Failed to fetch registry index"));
}

#[test]
fn test_categories_command_basic() {
    let mut cmd = Command::cargo_bin("rgen").unwrap();
    cmd.arg("categories");
    
    // This will fail because there's no registry, but we can test the argument parsing
    cmd.assert()
        .failure() // Expected to fail without registry
        .stderr(predicate::str::contains("Failed to fetch registry index"));
}

#[test]
fn test_categories_command_keywords() {
    let mut cmd = Command::cargo_bin("rgen").unwrap();
    cmd.arg("categories")
        .arg("--keywords")
        .arg("--detailed");
    
    // This will fail because there's no registry, but we can test the argument parsing
    cmd.assert()
        .failure() // Expected to fail without registry
        .stderr(predicate::str::contains("Failed to fetch registry index"));
}

#[test]
fn test_categories_command_json() {
    let mut cmd = Command::cargo_bin("rgen").unwrap();
    cmd.arg("categories")
        .arg("--json");
    
    // This will fail because there's no registry, but we can test the argument parsing
    cmd.assert()
        .failure() // Expected to fail without registry
        .stderr(predicate::str::contains("Failed to fetch registry index"));
}

#[test]
fn test_cli_help_includes_search_commands() {
    let mut cmd = Command::cargo_bin("rgen").unwrap();
    cmd.arg("--help");
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("search"))
        .stdout(predicate::str::contains("categories"));
}

#[test]
fn test_search_command_invalid_args() {
    let mut cmd = Command::cargo_bin("rgen").unwrap();
    cmd.arg("search")
        .arg("--limit")
        .arg("0"); // Invalid limit
    
    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("error"));
}

#[test]
fn test_search_command_missing_query() {
    let mut cmd = Command::cargo_bin("rgen").unwrap();
    cmd.arg("search");
    
    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("error"));
}
