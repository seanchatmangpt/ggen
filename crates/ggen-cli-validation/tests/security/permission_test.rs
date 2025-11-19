//! Security tests for permission model

use ggen_cli_validation::security::{Permission, PermissionModel};
use std::path::{Path, PathBuf};
use tempfile::tempdir;

#[test]
fn test_path_traversal_prevention() {
    let model = PermissionModel::new();

    // Should detect path traversal
    let traversal = Path::new("../../../etc/passwd");
    assert!(model.check_permission(traversal, Permission::Read).is_err());
}

#[test]
fn test_sandbox_enforcement() {
    let temp = tempdir().expect("Failed to create temp dir");
    let sandbox_root = temp.path().to_path_buf();

    let model = PermissionModel::new().with_sandbox(sandbox_root.clone());

    // Inside sandbox - should allow
    let inside = sandbox_root.join("test.txt");
    assert!(model.check_permission(&inside, Permission::Read).is_ok());

    // Outside sandbox - should deny
    let outside = Path::new("/etc/passwd");
    assert!(model.check_permission(outside, Permission::Read).is_err());
}

#[test]
fn test_read_write_permissions() {
    let model = PermissionModel::new()
        .allow_read(PathBuf::from("./src"))
        .allow_write(PathBuf::from("./target"));

    assert!(model.check_permission(Path::new("./src/lib.rs"), Permission::Read).is_ok());
    assert!(model.check_permission(Path::new("./target/output"), Permission::Write).is_ok());
}

#[test]
fn test_env_var_restrictions() {
    let model = PermissionModel::new();

    // System vars should be restricted
    assert!(model.is_env_var_restricted("PATH"));
    assert!(model.is_env_var_restricted("HOME"));

    // Custom vars should not be restricted
    assert!(!model.is_env_var_restricted("MY_VAR"));
}

#[test]
fn test_multiple_allowed_paths() {
    let model = PermissionModel::new()
        .allow_read(PathBuf::from("./src"))
        .allow_read(PathBuf::from("./tests"))
        .allow_write(PathBuf::from("./target"))
        .allow_write(PathBuf::from("./output"));

    // Multiple read paths
    assert!(model.check_permission(Path::new("./src/main.rs"), Permission::Read).is_ok());
    assert!(model.check_permission(Path::new("./tests/test.rs"), Permission::Read).is_ok());

    // Multiple write paths
    assert!(model.check_permission(Path::new("./target/debug"), Permission::Write).is_ok());
    assert!(model.check_permission(Path::new("./output/data"), Permission::Write).is_ok());
}
