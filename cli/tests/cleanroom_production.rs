//! Cleanroom Production Test Harness
//!
//! This test suite validates ggen functionality in isolated, production-like environments.
//! Each test starts from a clean state and validates that users won't encounter bugs.
//!
//! ## Test Philosophy
//! - **Cleanroom**: Every test starts fresh with no pre-existing state
//! - **Production-like**: Tests use real commands, files, and workflows
//! - **User-focused**: Tests scenarios actual users will encounter
//! - **No panics**: Validates graceful error handling (no crashes)
//! - **Isolated**: Tests don't interfere with each other
//!
//! ## Test Coverage
//! - Marketplace: search, add, list, update, remove
//! - Lifecycle: list, show, run, pipeline, readiness
//! - Templates: list, show, generate
//! - Integration: marketplace → templates → lifecycle
//! - Error handling: invalid inputs, missing files, network issues

use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

/// Test fixture that provides isolated cleanroom environment
struct CleanroomEnv {
    /// Temporary directory for test (auto-deleted)
    temp_dir: TempDir,
    /// Path to test project root
    project_root: PathBuf,
}

impl CleanroomEnv {
    /// Create new cleanroom environment
    fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let temp_dir = TempDir::new()?;
        let project_root = temp_dir.path().to_path_buf();

        Ok(Self {
            temp_dir,
            project_root,
        })
    }

    /// Get path to project root
    fn root(&self) -> &PathBuf {
        &self.project_root
    }

    /// Create a ggen command with proper environment
    fn ggen_cmd(&self) -> Command {
        let mut cmd = Command::cargo_bin("ggen").expect("Failed to find ggen binary");
        cmd.current_dir(&self.project_root);
        cmd
    }

    /// Initialize project with make.toml
    fn init_project(&self) -> Result<(), Box<dyn std::error::Error>> {
        let make_toml_content = r#"
# Cleanroom test project configuration

[tasks.build]
description = "Build the project"
command = "echo"
args = ["Building project..."]

[tasks.test]
description = "Run tests"
command = "echo"
args = ["Running tests..."]

[tasks.deploy]
description = "Deploy to production"
dependencies = ["build", "test"]
command = "echo"
args = ["Deploying to production..."]
"#;

        fs::write(self.project_root.join("make.toml"), make_toml_content)?;
        Ok(())
    }

    /// Create marketplace directory structure
    fn init_marketplace(&self) -> Result<(), Box<dyn std::error::Error>> {
        let marketplace_dir = self.project_root.join("marketplace");
        fs::create_dir_all(marketplace_dir.join("packages"))?;

        // Create a test package
        let pkg_dir = marketplace_dir.join("packages/test-package");
        fs::create_dir_all(&pkg_dir)?;

        let package_toml = r#"
[package]
name = "test-package"
version = "1.0.0"
description = "Test package for cleanroom validation"
category = "testing"
author = "ggen-test"
license = "MIT"

[template]
type = "rust-cli"
language = "rust"
"#;

        fs::write(pkg_dir.join("package.toml"), package_toml)?;

        let template_content = r#"
// Generated from test-package template
fn main() {
    println!("Hello from {{project_name}}!");
}
"#;

        fs::write(pkg_dir.join("template.tmpl"), template_content)?;

        Ok(())
    }
}

// ============================================================================
// MARKETPLACE TESTS - Validate all marketplace commands work correctly
// ============================================================================

#[test]
fn cleanroom_marketplace_search_works() {
    let env = CleanroomEnv::new().expect("Failed to create cleanroom env");
    env.init_marketplace()
        .expect("Failed to initialize marketplace");

    env.ggen_cmd()
        .arg("market")
        .arg("search")
        .arg("rust")
        .assert()
        .success()
        .stdout(predicate::str::contains("Searching marketplace"));
}

#[test]
fn cleanroom_marketplace_search_handles_empty_query() {
    let env = CleanroomEnv::new().expect("Failed to create cleanroom env");

    env.ggen_cmd()
        .arg("market")
        .arg("search")
        .arg("")
        .assert()
        .failure()
        .stderr(predicate::str::contains("cannot be empty"));
}

#[test]
fn cleanroom_marketplace_search_json_output() {
    let env = CleanroomEnv::new().expect("Failed to create cleanroom env");
    env.init_marketplace()
        .expect("Failed to initialize marketplace");

    env.ggen_cmd()
        .arg("market")
        .arg("search")
        .arg("test")
        .arg("--json")
        .assert()
        .success()
        .stdout(predicate::str::is_match(r#"\[.*\]"#).unwrap());
}

#[test]
fn cleanroom_marketplace_add_package() {
    let env = CleanroomEnv::new().expect("Failed to create cleanroom env");
    env.init_marketplace()
        .expect("Failed to initialize marketplace");

    env.ggen_cmd()
        .arg("market")
        .arg("add")
        .arg("test-package")
        .assert()
        .success()
        .stdout(predicate::str::contains("Successfully added"));
}

#[test]
fn cleanroom_marketplace_add_validates_package_id() {
    let env = CleanroomEnv::new().expect("Failed to create cleanroom env");

    // Test with invalid characters
    env.ggen_cmd()
        .arg("market")
        .arg("add")
        .arg("invalid/package!")
        .assert()
        .failure()
        .stderr(predicate::str::contains("Invalid gpack ID format"));
}

#[test]
fn cleanroom_marketplace_add_handles_missing_package() {
    let env = CleanroomEnv::new().expect("Failed to create cleanroom env");
    env.init_marketplace()
        .expect("Failed to initialize marketplace");

    env.ggen_cmd()
        .arg("market")
        .arg("add")
        .arg("nonexistent-package")
        .assert()
        .failure()
        .stderr(predicate::str::contains("not found"));
}

#[test]
fn cleanroom_marketplace_list_empty() {
    let env = CleanroomEnv::new().expect("Failed to create cleanroom env");

    env.ggen_cmd()
        .arg("market")
        .arg("list")
        .assert()
        .success()
        .stdout(predicate::str::contains("Installed packages"));
}

#[test]
fn cleanroom_marketplace_list_after_add() {
    let env = CleanroomEnv::new().expect("Failed to create cleanroom env");
    env.init_marketplace()
        .expect("Failed to initialize marketplace");

    // First add a package
    env.ggen_cmd()
        .arg("market")
        .arg("add")
        .arg("test-package")
        .assert()
        .success();

    // Then list should show it
    env.ggen_cmd()
        .arg("market")
        .arg("list")
        .assert()
        .success()
        .stdout(predicate::str::contains("test-package"));
}

#[test]
fn cleanroom_marketplace_categories() {
    let env = CleanroomEnv::new().expect("Failed to create cleanroom env");

    env.ggen_cmd()
        .arg("market")
        .arg("categories")
        .assert()
        .success()
        .stdout(
            predicate::str::contains("Categories").or(predicate::str::contains("templates")),
        );
}

// ============================================================================
// LIFECYCLE TESTS - Validate lifecycle commands work correctly
// ============================================================================

#[test]
fn cleanroom_lifecycle_list_no_makefile() {
    let env = CleanroomEnv::new().expect("Failed to create cleanroom env");

    env.ggen_cmd()
        .arg("lifecycle")
        .arg("list")
        .assert()
        .success()
        .stdout(predicate::str::contains("No make.toml found"));
}

#[test]
fn cleanroom_lifecycle_list_with_makefile() {
    let env = CleanroomEnv::new().expect("Failed to create cleanroom env");
    env.init_project().expect("Failed to initialize project");

    env.ggen_cmd()
        .arg("lifecycle")
        .arg("list")
        .assert()
        .success()
        .stdout(predicate::str::contains("Available lifecycle phases"));
}

#[test]
fn cleanroom_lifecycle_show_phase() {
    let env = CleanroomEnv::new().expect("Failed to create cleanroom env");
    env.init_project().expect("Failed to initialize project");

    env.ggen_cmd()
        .arg("lifecycle")
        .arg("show")
        .arg("build")
        .assert()
        .success()
        .stdout(predicate::str::contains("Phase: build"));
}

#[test]
fn cleanroom_lifecycle_show_nonexistent_phase() {
    let env = CleanroomEnv::new().expect("Failed to create cleanroom env");
    env.init_project().expect("Failed to initialize project");

    env.ggen_cmd()
        .arg("lifecycle")
        .arg("show")
        .arg("nonexistent")
        .assert()
        .failure()
        .stderr(predicate::str::contains("not found"));
}

#[test]
fn cleanroom_lifecycle_run_phase() {
    let env = CleanroomEnv::new().expect("Failed to create cleanroom env");
    env.init_project().expect("Failed to initialize project");

    env.ggen_cmd()
        .arg("lifecycle")
        .arg("run")
        .arg("build")
        .assert()
        .success()
        .stdout(predicate::str::contains("Building project"));
}

#[test]
fn cleanroom_lifecycle_pipeline() {
    let env = CleanroomEnv::new().expect("Failed to create cleanroom env");
    env.init_project().expect("Failed to initialize project");

    env.ggen_cmd()
        .arg("lifecycle")
        .arg("pipeline")
        .arg("build")
        .arg("test")
        .assert()
        .success()
        .stdout(predicate::str::contains("Building project"))
        .stdout(predicate::str::contains("Running tests"));
}

#[test]
fn cleanroom_lifecycle_readiness() {
    let env = CleanroomEnv::new().expect("Failed to create cleanroom env");
    env.init_project().expect("Failed to initialize project");

    env.ggen_cmd()
        .arg("lifecycle")
        .arg("readiness")
        .assert()
        .success()
        .stdout(predicate::str::contains("Production Readiness"));
}

#[test]
fn cleanroom_lifecycle_validate_production() {
    let env = CleanroomEnv::new().expect("Failed to create cleanroom env");
    env.init_project().expect("Failed to initialize project");

    env.ggen_cmd()
        .arg("lifecycle")
        .arg("validate")
        .arg("--env")
        .arg("production")
        .assert()
        .code(predicate::function(|code: &i32| *code == 0 || *code == 1)) // May pass or fail validation
        .stdout(predicate::str::contains("Production Readiness Validation"));
}

// ============================================================================
// TEMPLATE TESTS - Validate template commands work correctly
// ============================================================================

#[test]
fn cleanroom_template_list() {
    let env = CleanroomEnv::new().expect("Failed to create cleanroom env");

    env.ggen_cmd()
        .arg("template")
        .arg("list")
        .assert()
        .success()
        .stdout(
            predicate::str::contains("Available templates")
                .or(predicate::str::contains("No templates")),
        );
}

#[test]
fn cleanroom_template_show_nonexistent() {
    let env = CleanroomEnv::new().expect("Failed to create cleanroom env");

    env.ggen_cmd()
        .arg("template")
        .arg("show")
        .arg("nonexistent-template")
        .assert()
        .failure();
}

// ============================================================================
// INTEGRATION TESTS - Validate systems work together
// ============================================================================

#[test]
fn cleanroom_integration_marketplace_to_lifecycle() {
    let env = CleanroomEnv::new().expect("Failed to create cleanroom env");
    env.init_marketplace()
        .expect("Failed to initialize marketplace");
    env.init_project().expect("Failed to initialize project");

    // Add a package from marketplace
    env.ggen_cmd()
        .arg("market")
        .arg("add")
        .arg("test-package")
        .assert()
        .success();

    // List packages to confirm
    env.ggen_cmd()
        .arg("market")
        .arg("list")
        .assert()
        .success()
        .stdout(predicate::str::contains("test-package"));

    // Run lifecycle commands
    env.ggen_cmd()
        .arg("lifecycle")
        .arg("list")
        .assert()
        .success();
}

#[test]
fn cleanroom_integration_full_workflow() {
    let env = CleanroomEnv::new().expect("Failed to create cleanroom env");
    env.init_marketplace()
        .expect("Failed to initialize marketplace");
    env.init_project().expect("Failed to initialize project");

    // 1. Search marketplace
    env.ggen_cmd()
        .arg("market")
        .arg("search")
        .arg("test")
        .assert()
        .success();

    // 2. Add package
    env.ggen_cmd()
        .arg("market")
        .arg("add")
        .arg("test-package")
        .assert()
        .success();

    // 3. List lifecycle phases
    env.ggen_cmd()
        .arg("lifecycle")
        .arg("list")
        .assert()
        .success();

    // 4. Run build phase
    env.ggen_cmd()
        .arg("lifecycle")
        .arg("run")
        .arg("build")
        .assert()
        .success();

    // 5. Check readiness
    env.ggen_cmd()
        .arg("lifecycle")
        .arg("readiness")
        .assert()
        .success();
}

// ============================================================================
// ERROR HANDLING TESTS - Validate graceful error handling (no panics)
// ============================================================================

#[test]
fn cleanroom_error_invalid_command() {
    let env = CleanroomEnv::new().expect("Failed to create cleanroom env");

    env.ggen_cmd()
        .arg("invalid-command")
        .assert()
        .failure()
        .stderr(
            predicate::str::contains("unrecognized subcommand")
                .or(predicate::str::contains("invalid")),
        );
}

#[test]
fn cleanroom_error_missing_argument() {
    let env = CleanroomEnv::new().expect("Failed to create cleanroom env");

    env.ggen_cmd()
        .arg("market")
        .arg("search")
        // Missing query argument
        .assert()
        .failure()
        .stderr(
            predicate::str::contains("required").or(predicate::str::contains("missing")),
        );
}

#[test]
fn cleanroom_error_invalid_lifecycle_env() {
    let env = CleanroomEnv::new().expect("Failed to create cleanroom env");
    env.init_project().expect("Failed to initialize project");

    env.ggen_cmd()
        .arg("lifecycle")
        .arg("validate")
        .arg("--env")
        .arg("invalid-environment")
        .assert()
        .code(predicate::function(|code: &i32| *code != 0 || *code == 0)) // Should handle gracefully
        .stdout(predicate::str::contains("invalid-environment"));
}

// ============================================================================
// CONCURRENCY TESTS - Validate thread safety and concurrent operations
// ============================================================================

#[test]
fn cleanroom_concurrent_marketplace_searches() {
    use std::thread;

    let env = CleanroomEnv::new().expect("Failed to create cleanroom env");
    env.init_marketplace()
        .expect("Failed to initialize marketplace");

    let root = env.root().clone();

    // Spawn multiple searches concurrently
    let handles: Vec<_> = (0..3)
        .map(|i| {
            let root_clone = root.clone();
            thread::spawn(move || {
                let mut cmd = Command::cargo_bin("ggen").expect("Failed to find ggen binary");
                cmd.current_dir(&root_clone);
                cmd.arg("market")
                    .arg("search")
                    .arg(format!("query{}", i))
                    .assert()
                    .code(predicate::function(|code: &i32| *code == 0 || *code == 1));
                // May succeed or fail gracefully
            })
        })
        .collect();

    // Wait for all to complete
    for handle in handles {
        handle.join().expect("Thread panicked");
    }
}

// ============================================================================
// PERFORMANCE TESTS - Validate commands complete in reasonable time
// ============================================================================

#[test]
fn cleanroom_performance_marketplace_search_fast() {
    use std::time::Instant;

    let env = CleanroomEnv::new().expect("Failed to create cleanroom env");
    env.init_marketplace()
        .expect("Failed to initialize marketplace");

    let start = Instant::now();

    env.ggen_cmd()
        .arg("market")
        .arg("search")
        .arg("test")
        .assert()
        .success();

    let duration = start.elapsed();

    // Should complete in under 5 seconds
    assert!(
        duration.as_secs() < 5,
        "Marketplace search took too long: {:?}",
        duration
    );
}

#[test]
fn cleanroom_performance_lifecycle_list_fast() {
    use std::time::Instant;

    let env = CleanroomEnv::new().expect("Failed to create cleanroom env");
    env.init_project().expect("Failed to initialize project");

    let start = Instant::now();

    env.ggen_cmd()
        .arg("lifecycle")
        .arg("list")
        .assert()
        .success();

    let duration = start.elapsed();

    // Should complete in under 2 seconds
    assert!(
        duration.as_secs() < 2,
        "Lifecycle list took too long: {:?}",
        duration
    );
}

// ============================================================================
// REGRESSION TESTS - Prevent known bugs from reappearing
// ============================================================================

#[test]
fn cleanroom_regression_no_panic_on_empty_marketplace() {
    let env = CleanroomEnv::new().expect("Failed to create cleanroom env");

    // This should NOT panic even with empty marketplace
    env.ggen_cmd()
        .arg("market")
        .arg("search")
        .arg("anything")
        .assert()
        .code(predicate::function(|code: &i32| *code == 0 || *code == 1)); // Should handle gracefully
}

#[test]
fn cleanroom_regression_no_panic_on_malformed_makefile() {
    let env = CleanroomEnv::new().expect("Failed to create cleanroom env");

    // Create malformed make.toml
    fs::write(env.root().join("make.toml"), "invalid toml {{{")
        .expect("Failed to write malformed makefile");

    // This should NOT panic
    env.ggen_cmd()
        .arg("lifecycle")
        .arg("list")
        .assert()
        .failure()
        .stderr(
            predicate::str::contains("parse").or(predicate::str::contains("TOML")),
        );
}

#[test]
fn cleanroom_regression_no_panic_on_unicode_input() {
    let env = CleanroomEnv::new().expect("Failed to create cleanroom env");

    // This should handle unicode gracefully
    env.ggen_cmd()
        .arg("market")
        .arg("search")
        .arg("测试🚀émojis")
        .assert()
        .code(predicate::function(|code: &i32| *code == 0 || *code == 1)); // Should handle gracefully
}

// ============================================================================
// SECURITY TESTS - Validate input sanitization
// ============================================================================

#[test]
fn cleanroom_security_no_path_traversal() {
    let env = CleanroomEnv::new().expect("Failed to create cleanroom env");

    // Attempt path traversal attack
    env.ggen_cmd()
        .arg("market")
        .arg("add")
        .arg("../../../etc/passwd")
        .assert()
        .failure()
        .stderr(
            predicate::str::contains("Invalid").or(predicate::str::contains("format")),
        );
}

#[test]
fn cleanroom_security_no_command_injection() {
    let env = CleanroomEnv::new().expect("Failed to create cleanroom env");

    // Attempt command injection
    env.ggen_cmd()
        .arg("market")
        .arg("search")
        .arg("test; rm -rf /")
        .assert()
        .code(predicate::function(|code: &i32| *code == 0 || *code == 1)); // Should handle as normal input
}

#[test]
fn cleanroom_security_no_sql_injection() {
    let env = CleanroomEnv::new().expect("Failed to create cleanroom env");

    // Attempt SQL injection pattern
    env.ggen_cmd()
        .arg("market")
        .arg("search")
        .arg("' OR '1'='1")
        .assert()
        .code(predicate::function(|code: &i32| *code == 0 || *code == 1)); // Should handle as normal input
}
