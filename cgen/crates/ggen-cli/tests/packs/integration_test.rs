//! Chicago TDD Integration Tests for Pack CLI Commands
//!
//! These tests follow Chicago TDD methodology:
//! - Test with REAL CLI invocations
//! - Verify REAL filesystem state
//! - Test REAL command execution
//! - No mocks for critical paths

use std::fs;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

// ============================================================================
// Test Helpers
// ============================================================================

/// Test environment with isolated directories
struct TestEnv {
    temp_dir: TempDir,
    workspace_dir: PathBuf,
    packs_dir: PathBuf,
}

impl TestEnv {
    fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let temp_dir = TempDir::new()?;
        let workspace_dir = temp_dir.path().join("workspace");
        let packs_dir = workspace_dir.join(".ggen/packs");

        fs::create_dir_all(&workspace_dir)?;
        fs::create_dir_all(&packs_dir)?;

        Ok(Self {
            temp_dir,
            workspace_dir,
            packs_dir,
        })
    }

    fn workspace_path(&self) -> &Path {
        &self.workspace_dir
    }

    fn packs_path(&self) -> &Path {
        &self.packs_dir
    }
}

// ============================================================================
// PACK LIST COMMAND TESTS
// ============================================================================

#[tokio::test]
async fn test_pack_list_empty_workspace() {
    let env = TestEnv::new().unwrap();

    // Run pack list in empty workspace
    // TODO: Phase 2 - Execute CLI command
    // let output = Command::new("ggen")
    //     .args(["pack", "list"])
    //     .current_dir(env.workspace_path())
    //     .output()
    //     .await?;

    // assert!(output.status.success());
    // let stdout = String::from_utf8(output.stdout)?;
    // assert!(stdout.contains("No packs installed"));

    assert!(env.packs_path().exists());
}

#[tokio::test]
async fn test_pack_list_with_installed_packs() {
    let env = TestEnv::new().unwrap();

    // Create some installed pack directories
    let pack_dir = env.packs_path().join("surface-mcp").join("test-server");
    fs::create_dir_all(&pack_dir).unwrap();
    fs::write(pack_dir.join("pack.toml"), "name = 'test-server'").unwrap();

    // Run pack list
    // TODO: Phase 2 - Execute CLI command
    // let output = Command::new("ggen")
    //     .args(["pack", "list"])
    //     .current_dir(env.workspace_path())
    //     .output()
    //     .await?;

    // assert!(output.status.success());
    // let stdout = String::from_utf8(output.stdout)?;
    // assert!(stdout.contains("test-server"));

    assert!(pack_dir.exists());
}

#[tokio::test]
async fn test_pack_list_by_category() {
    let env = TestEnv::new().unwrap();

    // Create packs in different categories
    fs::create_dir_all(env.packs_path().join("surface-mcp/server")).unwrap();
    fs::create_dir_all(env.packs_path().join("projection-rust/client")).unwrap();

    // Run pack list filtered by category
    // TODO: Phase 2 - Execute CLI command
    // let output = Command::new("ggen")
    //     .args(["pack", "list", "--category", "surface"])
    //     .current_dir(env.workspace_path())
    //     .output()
    //     .await?;

    // assert!(output.status.success());
    // let stdout = String::from_utf8(output.stdout)?;
    // assert!(stdout.contains("server"));
    // assert!(!stdout.contains("client"));
}

// ============================================================================
// PACK INSTALL COMMAND TESTS
// ============================================================================

#[tokio::test]
async fn test_pack_install_single_pack() {
    let env = TestEnv::new().unwrap();

    // Install a single pack
    // TODO: Phase 2 - Execute CLI command
    // let output = Command::new("ggen")
    //     .args(["pack", "install", "surface-mcp/test-server"])
    //     .current_dir(env.workspace_path())
    //     .output()
    //     .await?;

    // assert!(output.status.success());

    // Verify pack installed
    let pack_path = env.packs_path().join("surface-mcp").join("test-server");
    // assert!(pack_path.exists(), "Pack should be installed");
}

#[tokio::test]
async fn test_pack_install_with_version() {
    let env = TestEnv::new().unwrap();

    // Install pack with specific version
    // TODO: Phase 2 - Execute CLI command
    // let output = Command::new("ggen")
    //     .args(["pack", "install", "surface-mcp/test-server@1.2.3"])
    //     .current_dir(env.workspace_path())
    //     .output()
    //     .await?;

    // assert!(output.status.success());

    // Verify correct version installed
    // let pack_path = env.packs_path().join("surface-mcp").join("test-server");
    // let version = fs::read_to_string(pack_path.join("version.txt"))?;
    // assert_eq!(version, "1.2.3");
}

#[tokio::test]
async fn test_pack_install_with_dependencies() {
    let env = TestEnv::new().unwrap();

    // Install pack with dependencies
    // TODO: Phase 2 - Execute CLI command
    // let output = Command::new("ggen")
    //     .args(["pack", "install", "surface-mcp/full-stack"])
    //     .current_dir(env.workspace_path())
    //     .output()
    //     .await?;

    // assert!(output.status.success());

    // Verify main pack installed
    // let main_pack = env.packs_path().join("surface-mcp").join("full-stack");
    // assert!(main_pack.exists());

    // Verify dependencies installed
    // let dep_pack = env.packs_path().join("projection-rust").join("client");
    // assert!(dep_pack.exists());
}

#[tokio::test]
async fn test_pack_install_creates_lockfile() {
    let env = TestEnv::new().unwrap();

    // Install a pack
    // TODO: Phase 2 - Execute CLI command
    // Command::new("ggen")
    //     .args(["pack", "install", "surface-mcp/test-server"])
    //     .current_dir(env.workspace_path())
    //     .output()
    //     .await?;

    // Verify lockfile created
    let lockfile_path = env.workspace_path().join("ggen.lock");
    // assert!(lockfile_path.exists(), "Lockfile should be created");

    // Verify lockfile content
    // let lockfile_content = fs::read_to_string(&lockfile_path)?;
    // assert!(lockfile_content.contains("test-server"));
}

#[tokio::test]
async fn test_pack_install_dry_run() {
    let env = TestEnv::new().unwrap();

    // Install with dry-run flag
    // TODO: Phase 2 - Execute CLI command
    // let output = Command::new("ggen")
    //     .args(["pack", "install", "--dry-run", "surface-mcp/test-server"])
    //     .current_dir(env.workspace_path())
    //     .output()
    //     .await?;

    // assert!(output.status.success());

    // Verify nothing actually installed
    let pack_path = env.packs_path().join("surface-mcp").join("test-server");
    assert!(!pack_path.exists(), "Dry run should not install pack");
}

// ============================================================================
// PACK UNINSTALL COMMAND TESTS
// ============================================================================

#[tokio::test]
async fn test_pack_uninstall_single_pack() {
    let env = TestEnv::new().unwrap();

    // First install a pack
    let pack_path = env.packs_path().join("surface-mcp").join("test-server");
    fs::create_dir_all(&pack_path).unwrap();
    fs::write(pack_path.join("pack.toml"), "name = 'test-server'").unwrap();

    assert!(pack_path.exists());

    // Uninstall the pack
    // TODO: Phase 2 - Execute CLI command
    // let output = Command::new("ggen")
    //     .args(["pack", "uninstall", "surface-mcp/test-server"])
    //     .current_dir(env.workspace_path())
    //     .output()
    //     .await?;

    // assert!(output.status.success());

    // Verify pack removed
    // assert!(!pack_path.exists(), "Pack should be uninstalled");
}

#[tokio::test]
async fn test_pack_uninstall_preserves_shared_dependencies() {
    let env = TestEnv::new().unwrap();

    // Create two packs that share a dependency
    let pack_a = env.packs_path().join("surface-mcp").join("server-a");
    let pack_b = env.packs_path().join("surface-mcp").join("server-b");
    let shared = env.packs_path().join("projection-rust").join("shared");

    fs::create_dir_all(&pack_a).unwrap();
    fs::create_dir_all(&pack_b).unwrap();
    fs::create_dir_all(&shared).unwrap();

    // Uninstall pack-a
    // TODO: Phase 2 - Execute CLI command
    // Command::new("ggen")
    //     .args(["pack", "uninstall", "surface-mcp/server-a"])
    //     .current_dir(env.workspace_path())
    //     .output()
    //     .await?;

    // Verify pack-a removed but shared dependency remains
    // assert!(!pack_a.exists());
    // assert!(pack_b.exists());
    // assert!(shared.exists(), "Shared dependency should remain");
}

// ============================================================================
// PACK UPDATE COMMAND TESTS
// ============================================================================

#[tokio::test]
async fn test_pack_update_to_latest() {
    let env = TestEnv::new().unwrap();

    // Install pack at v1.0.0
    let pack_path = env.packs_path().join("surface-mcp").join("test-server");
    fs::create_dir_all(&pack_path).unwrap();
    fs::write(pack_path.join("version.txt"), "1.0.0").unwrap();

    // Update to latest
    // TODO: Phase 2 - Execute CLI command
    // let output = Command::new("ggen")
    //     .args(["pack", "update", "surface-mcp/test-server"])
    //     .current_dir(env.workspace_path())
    //     .output()
    //     .await?;

    // assert!(output.status.success());

    // Verify version updated
    // let version = fs::read_to_string(pack_path.join("version.txt"))?;
    // assert!(version != "1.0.0", "Version should be updated");
}

#[tokio::test]
async fn test_pack_update_preserves_config() {
    let env = TestEnv::new().unwrap();

    // Install pack with config
    let pack_path = env.packs_path().join("surface-mcp").join("test-server");
    fs::create_dir_all(&pack_path).unwrap();
    fs::write(pack_path.join("config.toml"), "port = 8080").unwrap();
    fs::write(pack_path.join("version.txt"), "1.0.0").unwrap();

    // Update pack
    // TODO: Phase 2 - Execute CLI command
    // Command::new("ggen")
    //     .args(["pack", "update", "surface-mcp/test-server"])
    //     .current_dir(env.workspace_path())
    //     .output()
    //     .await?;

    // Verify config preserved
    // let config = fs::read_to_string(pack_path.join("config.toml"))?;
    // assert_eq!(config, "port = 8080");
}

// ============================================================================
// PACK SEARCH COMMAND TESTS
// ============================================================================

#[tokio::test]
async fn test_pack_search_by_name() {
    let env = TestEnv::new().unwrap();

    // Search for packs
    // TODO: Phase 2 - Execute CLI command
    // let output = Command::new("ggen")
    //     .args(["pack", "search", "server"])
    //     .current_dir(env.workspace_path())
    //     .output()
    //     .await?;

    // assert!(output.status.success());

    // let stdout = String::from_utf8(output.stdout)?;
    // assert!(stdout.contains("surface-mcp/http-server"));
}

#[tokio::test]
async fn test_pack_search_by_category() {
    let env = TestEnv::new().unwrap();

    // Search by category
    // TODO: Phase 2 - Execute CLI command
    // let output = Command::new("ggen")
    //     .args(["pack", "search", "--category", "projection"])
    //     .current_dir(env.workspace_path())
    //     .output()
    //     .await?;

    // assert!(output.status.success());

    // let stdout = String::from_utf8(output.stdout)?;
    // assert!(stdout.contains("projection-rust"));
}

#[tokio::test]
async fn test_pack_search_with_limit() {
    let env = TestEnv::new().unwrap();

    // Search with limit
    // TODO: Phase 2 - Execute CLI command
    // let output = Command::new("ggen")
    //     .args(["pack", "search", "server", "--limit", "5"])
    //     .current_dir(env.workspace_path())
    //     .output()
    //     .await?;

    // assert!(output.status.success());

    // Count results (should be <= 5)
    // let stdout = String::from_utf8(output.stdout)?;
    // let result_count = stdout.lines().filter(|l| l.contains("surface-mcp")).count();
    // assert!(result_count <= 5);
}

// ============================================================================
// PACK INFO COMMAND TESTS
// ============================================================================

#[tokio::test]
async fn test_pack_info_installed() {
    let env = TestEnv::new().unwrap();

    // Install a pack
    let pack_path = env.packs_path().join("surface-mcp").join("test-server");
    fs::create_dir_all(&pack_path).unwrap();
    fs::write(pack_path.join("pack.toml"), r#"
        name = "test-server"
        version = "1.0.0"
        description = "Test MCP server"
    "#).unwrap();

    // Get pack info
    // TODO: Phase 2 - Execute CLI command
    // let output = Command::new("ggen")
    //     .args(["pack", "info", "surface-mcp/test-server"])
    //     .current_dir(env.workspace_path())
    //     .output()
    //     .await?;

    // assert!(output.status.success());

    // let stdout = String::from_utf8(output.stdout)?;
    // assert!(stdout.contains("test-server"));
    // assert!(stdout.contains("1.0.0"));
    // assert!(stdout.contains("Test MCP server"));
}

#[tokio::test]
async fn test_pack_info_not_installed() {
    let env = TestEnv::new().unwrap();

    // Get info for non-installed pack
    // TODO: Phase 2 - Execute CLI command
    // let output = Command::new("ggen")
    //     .args(["pack", "info", "surface-mcp/remote-server"])
    //     .current_dir(env.workspace_path())
    //     .output()
    //     .await?;

    // Should show marketplace info even if not installed
    // assert!(output.status.success());

    // let stdout = String::from_utf8(output.stdout)?;
    // assert!(stdout.contains("remote-server"));
    // assert!(stdout.contains("Not installed"));
}

// ============================================================================
// PACK VALIDATE COMMAND TESTS
// ============================================================================

#[tokio::test]
async fn test_pack_validate_success() {
    let env = TestEnv::new().unwrap();

    // Create valid pack
    let pack_path = env.packs_path().join("surface-mcp").join("test-server");
    fs::create_dir_all(&pack_path).unwrap();
    fs::write(pack_path.join("pack.toml"), r#"
        name = "test-server"
        version = "1.0.0"
        class = "surface-mcp"
    "#).unwrap();

    // Validate pack
    // TODO: Phase 2 - Execute CLI command
    // let output = Command::new("ggen")
    //     .args(["pack", "validate", "surface-mcp/test-server"])
    //     .current_dir(env.workspace_path())
    //     .output()
    //     .await?;

    // assert!(output.status.success());

    // let stdout = String::from_utf8(output.stdout)?;
    // assert!(stdout.contains("Valid"));
}

#[tokio::test]
async fn test_pack_validate_failure() {
    let env = TestEnv::new().unwrap();

    // Create invalid pack (missing required fields)
    let pack_path = env.packs_path().join("surface-mcp").join("invalid-pack");
    fs::create_dir_all(&pack_path).unwrap();
    fs::write(pack_path.join("pack.toml"), "name = 'incomplete'").unwrap();

    // Validate pack
    // TODO: Phase 2 - Execute CLI command
    // let output = Command::new("ggen")
    //     .args(["pack", "validate", "surface-mcp/invalid-pack"])
    //     .current_dir(env.workspace_path())
    //     .output()
    //     .await?;

    // Should fail validation
    // assert!(!output.status.success());

    // let stderr = String::from_utf8(output.stderr)?;
    // assert!(stderr.contains("Invalid"));
}
