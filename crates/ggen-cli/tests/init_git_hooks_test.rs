//! Integration test for git hooks installation in init command
//!
//! Tests that `ggen init` automatically installs git hooks.

use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

#[test]
fn test_init_installs_git_hooks_in_git_repo() {
    // Arrange: Create a temp directory with .git
    let temp = TempDir::new().expect("Failed to create temp dir");
    let project_path = temp.path();
    let git_dir = project_path.join(".git");
    fs::create_dir_all(&git_dir).expect("Failed to create .git directory");

    // Act: Run init (skip_hooks = false)
    let result = ggen_cli_lib::cmds::git_hooks::install_git_hooks(project_path, false);

    // Assert: Hooks should be installed
    assert!(result.is_ok(), "Hook installation should succeed");
    let output = result.unwrap();
    assert!(output.git_repo_detected, "Should detect git repository");
    assert_eq!(output.hooks_installed.len(), 2, "Should install 2 hooks");

    // Verify files exist
    let hooks_dir = git_dir.join("hooks");
    assert!(hooks_dir.join("pre-commit").exists(), "pre-commit hook should exist");
    assert!(hooks_dir.join("pre-push").exists(), "pre-push hook should exist");
}

#[test]
fn test_init_skips_hooks_with_flag() {
    // Arrange
    let temp = TempDir::new().expect("Failed to create temp dir");
    let project_path = temp.path();
    let git_dir = project_path.join(".git");
    fs::create_dir_all(&git_dir).expect("Failed to create .git directory");

    // Act: Run init with skip_hooks = true
    let result = ggen_cli_lib::cmds::git_hooks::install_git_hooks(project_path, true);

    // Assert: Hooks should be skipped
    assert!(result.is_ok());
    let output = result.unwrap();
    assert!(!output.git_repo_detected, "Should report not detected when skipped");
    assert_eq!(output.hooks_installed.len(), 0, "Should not install hooks");
    assert!(!output.warnings.is_empty(), "Should have warning about skipping");
}

#[test]
fn test_init_handles_non_git_repo_gracefully() {
    // Arrange: Create temp directory WITHOUT .git
    let temp = TempDir::new().expect("Failed to create temp dir");
    let project_path = temp.path();

    // Act: Run init
    let result = ggen_cli_lib::cmds::git_hooks::install_git_hooks(project_path, false);

    // Assert: Should succeed but skip hooks
    assert!(result.is_ok());
    let output = result.unwrap();
    assert!(!output.git_repo_detected, "Should not detect git repo");
    assert_eq!(output.hooks_installed.len(), 0);
    assert!(!output.warnings.is_empty(), "Should warn about non-git repo");
}

#[test]
fn test_hooks_are_executable_on_unix() {
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;

        // Arrange
        let temp = TempDir::new().expect("Failed to create temp dir");
        let project_path = temp.path();
        let git_dir = project_path.join(".git");
        fs::create_dir_all(&git_dir).expect("Failed to create .git directory");

        // Act
        let result = ggen_cli_lib::cmds::git_hooks::install_git_hooks(project_path, false);
        assert!(result.is_ok());

        // Assert: Hooks should be executable
        let hooks_dir = git_dir.join("hooks");
        let pre_commit = hooks_dir.join("pre-commit");
        let pre_push = hooks_dir.join("pre-push");

        let pre_commit_metadata = fs::metadata(&pre_commit).expect("Failed to get metadata");
        let pre_push_metadata = fs::metadata(&pre_push).expect("Failed to get metadata");

        let pre_commit_mode = pre_commit_metadata.permissions().mode();
        let pre_push_mode = pre_push_metadata.permissions().mode();

        assert!(
            pre_commit_mode & 0o111 != 0,
            "pre-commit should be executable"
        );
        assert!(
            pre_push_mode & 0o111 != 0,
            "pre-push should be executable"
        );
    }
}

#[test]
fn test_hook_content_includes_cargo_make() {
    // Arrange
    let temp = TempDir::new().expect("Failed to create temp dir");
    let project_path = temp.path();
    let git_dir = project_path.join(".git");
    fs::create_dir_all(&git_dir).expect("Failed to create .git directory");

    // Act
    ggen_cli_lib::cmds::git_hooks::install_git_hooks(project_path, false)
        .expect("Installation should succeed");

    // Assert: Hook content should reference cargo check/fmt/make
    let hooks_dir = git_dir.join("hooks");
    let pre_commit_content = fs::read_to_string(hooks_dir.join("pre-commit"))
        .expect("Failed to read pre-commit");
    let pre_push_content = fs::read_to_string(hooks_dir.join("pre-push"))
        .expect("Failed to read pre-push");

    assert!(
        pre_commit_content.contains("cargo check"),
        "pre-commit should run cargo check"
    );
    assert!(
        pre_commit_content.contains("cargo fmt"),
        "pre-commit should run cargo fmt"
    );
    assert!(
        pre_push_content.contains("cargo make pre-commit") || pre_push_content.contains("cargo clippy"),
        "pre-push should run cargo make pre-commit or cargo clippy"
    );
}

#[test]
fn test_existing_hooks_are_not_overwritten() {
    // Arrange
    let temp = TempDir::new().expect("Failed to create temp dir");
    let project_path = temp.path();
    let git_dir = project_path.join(".git");
    let hooks_dir = git_dir.join("hooks");
    fs::create_dir_all(&hooks_dir).expect("Failed to create hooks directory");

    let existing_hook_content = "#!/bin/bash\n# Existing hook\necho 'existing'";
    fs::write(hooks_dir.join("pre-commit"), existing_hook_content)
        .expect("Failed to write existing hook");

    // Act
    let result = ggen_cli_lib::cmds::git_hooks::install_git_hooks(project_path, false);

    // Assert: Existing hook should be preserved
    assert!(result.is_ok());
    let output = result.unwrap();

    let pre_commit_result = output.hooks_installed
        .iter()
        .find(|h| h.hook_name == "pre-commit")
        .expect("Should have pre-commit result");

    assert!(pre_commit_result.skipped, "Should skip existing hook");
    assert!(!pre_commit_result.installed, "Should not overwrite");

    // Verify content unchanged
    let content = fs::read_to_string(hooks_dir.join("pre-commit"))
        .expect("Failed to read hook");
    assert_eq!(content, existing_hook_content, "Content should be unchanged");
}
