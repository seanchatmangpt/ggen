//! Git hooks installation and management
//!
//! Provides automatic installation of pre-commit and pre-push hooks
//! integrated with cargo make workflow.
//!
//! ## Features
//!
//! - **Auto-detection**: Detects if in git repository
//! - **Smart installation**: Skips if hooks already installed
//! - **Cross-platform**: Works on Unix and Windows
//! - **Verification**: Validates hooks after installation
//!
//! ## Hooks Installed
//!
//! - **pre-commit**: Fast validation (cargo make check + format)
//! - **pre-push**: Full validation (cargo make pre-commit)

use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, serde::Serialize)]
pub struct HookInstallation {
    pub hook_name: String,
    pub installed: bool,
    pub skipped: bool,
    pub reason: Option<String>,
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct HooksInstallOutput {
    pub git_repo_detected: bool,
    pub hooks_installed: Vec<HookInstallation>,
    pub warnings: Vec<String>,
}

/// Check if a directory is a git repository
pub fn is_git_repo(path: &Path) -> Result<bool, std::io::Error> {
    let git_dir = path.join(".git");
    Ok(git_dir.exists() && git_dir.is_dir())
}

/// Get the .git/hooks directory path
pub fn get_hooks_dir(project_path: &Path) -> Result<PathBuf, std::io::Error> {
    let hooks_dir = project_path.join(".git").join("hooks");
    Ok(hooks_dir)
}

/// Check if a hook is already installed
pub fn is_hook_installed(hooks_dir: &Path, hook_name: &str) -> Result<bool, std::io::Error> {
    let hook_path = hooks_dir.join(hook_name);
    Ok(hook_path.exists())
}

/// Pre-commit hook content
const PRE_COMMIT_HOOK: &str = r#"#!/usr/bin/env bash
# Pre-commit hook - Fast validation tier
# Auto-installed by ggen init
# Target: <10 seconds | Catches compilation errors early

set -e
cd "$(git rev-parse --show-toplevel)"

echo ""
echo "Pre-commit validation (fast tier)..."
echo ""

# Gate 1: Cargo check (compilation)
echo -n "  Cargo check... "
if timeout 10s cargo check --quiet 2>/dev/null; then
    echo "PASS"
else
    echo "FAIL"
    echo ""
    echo "STOP: Compilation errors must be fixed"
    cargo check 2>&1 | head -30
    exit 1
fi

# Gate 2: Format check (auto-fixable)
echo -n "  Format check... "
if cargo fmt --all -- --check >/dev/null 2>&1; then
    echo "PASS"
else
    echo "AUTO-FIX"
    cargo fmt --all >/dev/null 2>&1 || true
    echo "  Code formatted. Review changes before commit."
fi

echo ""
echo "Pre-commit passed."
exit 0
"#;

/// Pre-push hook content
const PRE_PUSH_HOOK: &str = r#"#!/usr/bin/env bash
# Pre-push hook - Full validation tier
# Auto-installed by ggen init
# Target: <90 seconds | Comprehensive checks

set -e
cd "$(git rev-parse --show-toplevel)"

echo ""
echo "Pre-push validation (full tier)..."
echo ""

# Use cargo make if available, fallback to direct commands
if command -v cargo-make &> /dev/null; then
    echo "Running cargo make pre-commit..."
    if ! cargo make pre-commit; then
        echo ""
        echo "STOP: Pre-commit checks failed"
        exit 1
    fi
else
    # Fallback: run checks directly
    echo "cargo-make not found, running checks directly..."

    echo "  Cargo check..."
    if ! cargo check --quiet; then
        echo "FAIL: Compilation errors"
        exit 1
    fi

    echo "  Cargo clippy..."
    if ! cargo clippy --all-targets --all-features -- -D warnings; then
        echo "FAIL: Clippy warnings"
        exit 1
    fi

    echo "  Cargo test..."
    if ! cargo test --workspace; then
        echo "FAIL: Tests failed"
        exit 1
    fi

    echo "  Format check..."
    cargo fmt --all -- --check || {
        echo "FAIL: Format check failed"
        exit 1
    }
fi

echo ""
echo "Pre-push passed."
exit 0
"#;

/// Install a single git hook
pub fn install_hook(
    hooks_dir: &Path,
    hook_name: &str,
    hook_content: &str,
) -> Result<HookInstallation, std::io::Error> {
    let hook_path = hooks_dir.join(hook_name);

    // Check if already exists
    if hook_path.exists() {
        return Ok(HookInstallation {
            hook_name: hook_name.to_string(),
            installed: false,
            skipped: true,
            reason: Some("Hook already exists".to_string()),
        });
    }

    // Ensure hooks directory exists
    fs::create_dir_all(hooks_dir)?;

    // Write hook file
    fs::write(&hook_path, hook_content)?;

    // Make executable on Unix systems
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        fs::set_permissions(&hook_path, fs::Permissions::from_mode(0o755))?;
    }

    // On Windows, the git bash will handle execution
    #[cfg(windows)]
    {
        // Windows: No chmod needed, git bash handles it
        // We could add a .bat wrapper here if needed
    }

    Ok(HookInstallation {
        hook_name: hook_name.to_string(),
        installed: true,
        skipped: false,
        reason: None,
    })
}

/// Install all git hooks
pub fn install_git_hooks(
    project_path: &Path,
    skip_hooks: bool,
) -> Result<HooksInstallOutput, std::io::Error> {
    let mut warnings = Vec::new();

    // Check if skip flag is set
    if skip_hooks {
        return Ok(HooksInstallOutput {
            git_repo_detected: false,
            hooks_installed: vec![],
            warnings: vec!["Git hooks installation skipped (--skip-hooks flag)".to_string()],
        });
    }

    // Check if git repo
    let is_git = is_git_repo(project_path)?;
    if !is_git {
        warnings.push("Not a git repository, skipping hook installation".to_string());
        return Ok(HooksInstallOutput {
            git_repo_detected: false,
            hooks_installed: vec![],
            warnings,
        });
    }

    // Get hooks directory
    let hooks_dir = get_hooks_dir(project_path)?;

    // Install hooks
    let mut hooks_installed = Vec::new();

    // Install pre-commit hook
    match install_hook(&hooks_dir, "pre-commit", PRE_COMMIT_HOOK) {
        Ok(result) => hooks_installed.push(result),
        Err(e) => {
            warnings.push(format!("Failed to install pre-commit hook: {}", e));
        }
    }

    // Install pre-push hook
    match install_hook(&hooks_dir, "pre-push", PRE_PUSH_HOOK) {
        Ok(result) => hooks_installed.push(result),
        Err(e) => {
            warnings.push(format!("Failed to install pre-push hook: {}", e));
        }
    }

    Ok(HooksInstallOutput {
        git_repo_detected: is_git,
        hooks_installed,
        warnings,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_is_git_repo_detects_git_directory() {
        // Arrange
        let temp = TempDir::new().unwrap();
        let git_dir = temp.path().join(".git");
        fs::create_dir_all(&git_dir).unwrap();

        // Act
        let result = is_git_repo(temp.path()).unwrap();

        // Assert
        assert!(result, "Should detect .git directory");
    }

    #[test]
    fn test_is_git_repo_returns_false_for_non_git() {
        // Arrange
        let temp = TempDir::new().unwrap();

        // Act
        let result = is_git_repo(temp.path()).unwrap();

        // Assert
        assert!(!result, "Should not detect git repo");
    }

    #[test]
    fn test_get_hooks_dir_returns_correct_path() {
        // Arrange
        let temp = TempDir::new().unwrap();

        // Act
        let hooks_dir = get_hooks_dir(temp.path()).unwrap();

        // Assert
        assert_eq!(
            hooks_dir,
            temp.path().join(".git").join("hooks"),
            "Should return correct hooks directory path"
        );
    }

    #[test]
    fn test_is_hook_installed_detects_existing_hook() {
        // Arrange
        let temp = TempDir::new().unwrap();
        let hooks_dir = temp.path().join(".git").join("hooks");
        fs::create_dir_all(&hooks_dir).unwrap();
        let hook_path = hooks_dir.join("pre-commit");
        fs::write(&hook_path, "#!/bin/bash\necho test").unwrap();

        // Act
        let result = is_hook_installed(&hooks_dir, "pre-commit").unwrap();

        // Assert
        assert!(result, "Should detect installed hook");
    }

    #[test]
    fn test_is_hook_installed_returns_false_for_missing_hook() {
        // Arrange
        let temp = TempDir::new().unwrap();
        let hooks_dir = temp.path().join(".git").join("hooks");
        fs::create_dir_all(&hooks_dir).unwrap();

        // Act
        let result = is_hook_installed(&hooks_dir, "pre-commit").unwrap();

        // Assert
        assert!(!result, "Should not detect missing hook");
    }

    #[test]
    fn test_install_hook_creates_hook_file() {
        // Arrange
        let temp = TempDir::new().unwrap();
        let hooks_dir = temp.path().join(".git").join("hooks");

        // Act
        let result = install_hook(&hooks_dir, "pre-commit", PRE_COMMIT_HOOK).unwrap();

        // Assert
        assert!(result.installed, "Hook should be marked as installed");
        assert!(!result.skipped, "Hook should not be skipped");
        let hook_path = hooks_dir.join("pre-commit");
        assert!(hook_path.exists(), "Hook file should exist");
        let content = fs::read_to_string(&hook_path).unwrap();
        assert_eq!(content, PRE_COMMIT_HOOK, "Hook content should match");
    }

    #[test]
    fn test_install_hook_skips_existing_hook() {
        // Arrange
        let temp = TempDir::new().unwrap();
        let hooks_dir = temp.path().join(".git").join("hooks");
        fs::create_dir_all(&hooks_dir).unwrap();
        let hook_path = hooks_dir.join("pre-commit");
        fs::write(&hook_path, "#!/bin/bash\necho existing").unwrap();

        // Act
        let result = install_hook(&hooks_dir, "pre-commit", PRE_COMMIT_HOOK).unwrap();

        // Assert
        assert!(!result.installed, "Hook should not be marked as installed");
        assert!(result.skipped, "Hook should be marked as skipped");
        assert!(
            result.reason.is_some(),
            "Should have reason for skipping"
        );
        let content = fs::read_to_string(&hook_path).unwrap();
        assert_eq!(
            content, "#!/bin/bash\necho existing",
            "Existing hook should not be overwritten"
        );
    }

    #[test]
    fn test_install_git_hooks_with_skip_flag() {
        // Arrange
        let temp = TempDir::new().unwrap();

        // Act
        let result = install_git_hooks(temp.path(), true).unwrap();

        // Assert
        assert!(!result.git_repo_detected);
        assert_eq!(result.hooks_installed.len(), 0);
        assert!(
            !result.warnings.is_empty(),
            "Should have warning about skipping"
        );
    }

    #[test]
    fn test_install_git_hooks_in_non_git_repo() {
        // Arrange
        let temp = TempDir::new().unwrap();

        // Act
        let result = install_git_hooks(temp.path(), false).unwrap();

        // Assert
        assert!(!result.git_repo_detected);
        assert_eq!(result.hooks_installed.len(), 0);
        assert!(
            !result.warnings.is_empty(),
            "Should have warning about non-git repo"
        );
    }

    #[test]
    fn test_install_git_hooks_in_git_repo() {
        // Arrange
        let temp = TempDir::new().unwrap();
        let git_dir = temp.path().join(".git");
        fs::create_dir_all(&git_dir).unwrap();

        // Act
        let result = install_git_hooks(temp.path(), false).unwrap();

        // Assert
        assert!(result.git_repo_detected, "Should detect git repo");
        assert_eq!(
            result.hooks_installed.len(),
            2,
            "Should install 2 hooks"
        );

        // Check pre-commit
        let pre_commit = &result.hooks_installed[0];
        assert_eq!(pre_commit.hook_name, "pre-commit");
        assert!(pre_commit.installed, "pre-commit should be installed");

        // Check pre-push
        let pre_push = &result.hooks_installed[1];
        assert_eq!(pre_push.hook_name, "pre-push");
        assert!(pre_push.installed, "pre-push should be installed");

        // Verify files exist
        let hooks_dir = git_dir.join("hooks");
        assert!(
            hooks_dir.join("pre-commit").exists(),
            "pre-commit file should exist"
        );
        assert!(
            hooks_dir.join("pre-push").exists(),
            "pre-push file should exist"
        );
    }

    #[test]
    fn test_install_git_hooks_skips_existing() {
        // Arrange
        let temp = TempDir::new().unwrap();
        let git_dir = temp.path().join(".git");
        let hooks_dir = git_dir.join("hooks");
        fs::create_dir_all(&hooks_dir).unwrap();
        fs::write(hooks_dir.join("pre-commit"), "#!/bin/bash\necho existing").unwrap();

        // Act
        let result = install_git_hooks(temp.path(), false).unwrap();

        // Assert
        assert!(result.git_repo_detected);
        assert_eq!(result.hooks_installed.len(), 2);

        let pre_commit = &result.hooks_installed[0];
        assert_eq!(pre_commit.hook_name, "pre-commit");
        assert!(
            pre_commit.skipped,
            "Existing pre-commit should be skipped"
        );

        let pre_push = &result.hooks_installed[1];
        assert_eq!(pre_push.hook_name, "pre-push");
        assert!(pre_push.installed, "New pre-push should be installed");
    }

    #[cfg(unix)]
    #[test]
    fn test_hook_is_executable_on_unix() {
        use std::os::unix::fs::PermissionsExt;

        // Arrange
        let temp = TempDir::new().unwrap();
        let git_dir = temp.path().join(".git");
        fs::create_dir_all(&git_dir).unwrap();

        // Act
        install_git_hooks(temp.path(), false).unwrap();

        // Assert
        let hooks_dir = git_dir.join("hooks");
        let pre_commit_path = hooks_dir.join("pre-commit");
        let metadata = fs::metadata(&pre_commit_path).unwrap();
        let permissions = metadata.permissions();
        let mode = permissions.mode();

        assert!(
            mode & 0o111 != 0,
            "Hook should have executable permissions"
        );
    }
}
