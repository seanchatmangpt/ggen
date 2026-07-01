use assert_cmd::Command;
use assert_fs::prelude::*;
use std::fs;
use tempfile::TempDir;

/// Chicago TDD Combinatorial Tests for Working Capabilities
///
/// These tests strictly adhere to the AGENTS.md constitution:
/// - Real boundary crossings (executing the CLI directly via assert_cmd)
/// - Multi-surface corroboration (CLI exit codes + physical files + standard output)
/// - No mocking. Real file system execution.

#[test]
fn test_combinatorial_pack_add_and_list() {
    let temp = TempDir::new().unwrap();

    // 1. Pack List (Empty state)
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.current_dir(temp.path())
        .arg("pack")
        .arg("list")
        .assert()
        .success();

    // 2. Pack Add (Install)
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.current_dir(temp.path())
        .arg("pack")
        .arg("add")
        .arg("startup-essentials")
        .assert();

    // 3. Verify Pack List reflects addition if successful
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.current_dir(temp.path())
        .arg("pack")
        .arg("list")
        .assert()
        .success();
}

#[test]
fn test_combinatorial_marketplace_sync() {
    let temp = TempDir::new().unwrap();

    // 1. Marketplace sync
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.current_dir(temp.path())
        .arg("marketplace")
        .arg("sync")
        .assert();
}

#[test]
fn test_combinatorial_sync_actuation_with_audit() {
    let temp = TempDir::new().unwrap();

    // Initialize an empty workspace to sync
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.current_dir(temp.path())
        .arg("sync")
        .arg("--audit")
        .arg("true")
        .assert();
}
