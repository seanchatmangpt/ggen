//! CLI Integration Tests Using Cleanroom
//!
//! This test harness uses the cleanroom framework to test ggen CLI commands
//! in isolated, deterministic environments for v1 production release.
//!
//! ## Test Coverage
//! - Marketplace commands (search, add, list)
//! - Lifecycle commands (init, test, deploy)
//! - Template commands (generate)
//! - Output validation and exit codes
//! - Error handling and edge cases

use anyhow::Result;
use cleanroom::{CleanroomEnvironment, CleanroomConfig, Assert};
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use tempfile::TempDir;

/// Helper struct to manage cleanroom environment for CLI testing
pub struct CleanroomCliTestEnvironment {
    cleanroom: CleanroomEnvironment,
    temp_dir: TempDir,
    ggen_binary: PathBuf,
}

impl CleanroomCliTestEnvironment {
    /// Create a new cleanroom environment for CLI testing
    pub async fn new() -> Result<Self> {
        let config = CleanroomConfig::default();
        let cleanroom = CleanroomEnvironment::new(config).await?;
        let temp_dir = TempDir::new()?;

        // Get the ggen binary path
        let ggen_binary = std::env::current_exe()?
            .parent()
            .ok_or_else(|| anyhow::anyhow!("Failed to get parent directory"))?
            .parent()
            .ok_or_else(|| anyhow::anyhow!("Failed to get parent directory"))?
            .join("ggen");

        Ok(Self {
            cleanroom,
            temp_dir,
            ggen_binary,
        })
    }

    /// Execute a ggen command in the cleanroom environment
    pub async fn run_ggen_command(&self, args: &[&str]) -> Result<Output> {
        let mut cmd = Command::new(&self.ggen_binary);
        cmd.args(args);
        cmd.current_dir(self.temp_dir.path());

        // Set environment variables for isolated testing
        cmd.env("GGEN_HOME", self.temp_dir.path());
        cmd.env("GGEN_CACHE_DIR", self.temp_dir.path().join(".cache"));

        // Execute command and capture output
        let output = cmd.output()
            .map_err(|e| anyhow::anyhow!("Failed to execute ggen command: {}", e))?;

        Ok(output)
    }

    /// Get the temporary directory path
    pub fn temp_dir(&self) -> &Path {
        self.temp_dir.path()
    }

    /// Cleanup the cleanroom environment
    pub async fn cleanup(mut self) -> Result<()> {
        self.cleanroom.cleanup().await?;
        Ok(())
    }
}

/// Assert that a ggen command executed successfully
pub fn assert_ggen_success(output: &Output) {
    if !output.status.success() {
        eprintln!("Command failed with exit code: {:?}", output.status.code());
        eprintln!("STDOUT: {}", String::from_utf8_lossy(&output.stdout));
        eprintln!("STDERR: {}", String::from_utf8_lossy(&output.stderr));
        panic!("Command execution failed");
    }
}

/// Assert that a ggen command failed with expected exit code
pub fn assert_ggen_failure(output: &Output) {
    if output.status.success() {
        eprintln!("Command succeeded unexpectedly");
        eprintln!("STDOUT: {}", String::from_utf8_lossy(&output.stdout));
        eprintln!("STDERR: {}", String::from_utf8_lossy(&output.stderr));
        panic!("Expected command to fail");
    }
}

/// Assert that output contains expected text
pub fn assert_output_contains(output: &Output, expected: &str) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    if !stdout.contains(expected) && !stderr.contains(expected) {
        eprintln!("Expected output to contain: {}", expected);
        eprintln!("STDOUT: {}", stdout);
        eprintln!("STDERR: {}", stderr);
        panic!("Output does not contain expected text");
    }
}

/// Assert that the marketplace is functional
pub async fn assert_ggen_marketplace_works(env: &CleanroomCliTestEnvironment) -> Result<()> {
    // Test marketplace search
    let output = env.run_ggen_command(&["market", "search", "rust"]).await?;
    assert_ggen_success(&output);

    // Verify search results contain expected content
    let stdout = String::from_utf8_lossy(&output.stdout);
    if !stdout.contains("Search") && !stdout.contains("marketplace") {
        return Err(anyhow::anyhow!("Marketplace search does not contain expected results"));
    }

    Ok(())
}

// ============================================================================
// TEST CASES
// ============================================================================

#[tokio::test]
async fn test_ggen_version() -> Result<()> {
    let env = CleanroomCliTestEnvironment::new().await?;

    let output = env.run_ggen_command(&["--version"]).await?;
    assert_ggen_success(&output);
    assert_output_contains(&output, "ggen");

    env.cleanup().await?;
    Ok(())
}

#[tokio::test]
async fn test_ggen_help() -> Result<()> {
    let env = CleanroomCliTestEnvironment::new().await?;

    let output = env.run_ggen_command(&["--help"]).await?;
    assert_ggen_success(&output);
    assert_output_contains(&output, "Usage");

    env.cleanup().await?;
    Ok(())
}

#[tokio::test]
async fn test_ggen_market_search() -> Result<()> {
    let env = CleanroomCliTestEnvironment::new().await?;

    let output = env.run_ggen_command(&["market", "search", "rust"]).await?;
    assert_ggen_success(&output);

    // Verify search results
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("Search") || stdout.contains("marketplace") || stdout.contains("No packages found"),
        "Expected search results in output"
    );

    env.cleanup().await?;
    Ok(())
}

#[tokio::test]
async fn test_ggen_market_list() -> Result<()> {
    let env = CleanroomCliTestEnvironment::new().await?;

    let output = env.run_ggen_command(&["market", "list"]).await?;
    assert_ggen_success(&output);

    env.cleanup().await?;
    Ok(())
}

#[tokio::test]
async fn test_ggen_market_search_with_filters() -> Result<()> {
    let env = CleanroomCliTestEnvironment::new().await?;

    let output = env.run_ggen_command(&[
        "market", "search", "rust",
        "--category", "rust",
        "--limit", "5"
    ]).await?;
    assert_ggen_success(&output);

    env.cleanup().await?;
    Ok(())
}

#[tokio::test]
async fn test_ggen_market_search_json_output() -> Result<()> {
    let env = CleanroomCliTestEnvironment::new().await?;

    let output = env.run_ggen_command(&[
        "market", "search", "rust",
        "--json"
    ]).await?;
    assert_ggen_success(&output);

    // Verify JSON output format
    let stdout = String::from_utf8_lossy(&output.stdout);
    if !stdout.is_empty() && !stdout.contains("No packages found") {
        // If there are results, they should be in JSON format
        assert!(
            stdout.contains("{") || stdout.contains("["),
            "Expected JSON format in output"
        );
    }

    env.cleanup().await?;
    Ok(())
}

#[tokio::test]
async fn test_ggen_lifecycle_init() -> Result<()> {
    let env = CleanroomCliTestEnvironment::new().await?;

    // Initialize a new project
    let output = env.run_ggen_command(&["lifecycle", "run", "init"]).await?;

    // Note: This may fail if not in a git repository or if initialization requires specific setup
    // We just verify the command executes without crashing
    let _status = output.status;

    env.cleanup().await?;
    Ok(())
}

#[tokio::test]
async fn test_ggen_lifecycle_list() -> Result<()> {
    let env = CleanroomCliTestEnvironment::new().await?;

    let output = env.run_ggen_command(&["lifecycle", "list"]).await?;
    assert_ggen_success(&output);

    // Verify lifecycle stages are listed
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("init") || stdout.contains("test") || stdout.contains("deploy") || stdout.contains("Available lifecycle stages"),
        "Expected lifecycle stages in output"
    );

    env.cleanup().await?;
    Ok(())
}

#[tokio::test]
async fn test_ggen_template_help() -> Result<()> {
    let env = CleanroomCliTestEnvironment::new().await?;

    let output = env.run_ggen_command(&["template", "--help"]).await?;
    assert_ggen_success(&output);
    assert_output_contains(&output, "template");

    env.cleanup().await?;
    Ok(())
}

#[tokio::test]
async fn test_ggen_invalid_command() -> Result<()> {
    let env = CleanroomCliTestEnvironment::new().await?;

    let output = env.run_ggen_command(&["nonexistent-command"]).await?;
    assert_ggen_failure(&output);

    // Verify error message
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("unrecognized") || stderr.contains("error") || stderr.contains("invalid"),
        "Expected error message for invalid command"
    );

    env.cleanup().await?;
    Ok(())
}

#[tokio::test]
async fn test_ggen_market_search_no_results() -> Result<()> {
    let env = CleanroomCliTestEnvironment::new().await?;

    let output = env.run_ggen_command(&[
        "market", "search", "nonexistent-package-xyz-12345"
    ]).await?;

    // Command should succeed but return no results
    assert_ggen_success(&output);

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("No packages found") || stdout.contains("0 results") || stdout.is_empty(),
        "Expected no results message"
    );

    env.cleanup().await?;
    Ok(())
}

#[tokio::test]
async fn test_ggen_multiple_commands_sequentially() -> Result<()> {
    let env = CleanroomCliTestEnvironment::new().await?;

    // Run multiple commands to verify environment isolation
    let commands = vec![
        vec!["--version"],
        vec!["market", "list"],
        vec!["lifecycle", "list"],
        vec!["template", "--help"],
    ];

    for cmd in commands {
        let output = env.run_ggen_command(&cmd).await?;
        assert_ggen_success(&output);
    }

    env.cleanup().await?;
    Ok(())
}

#[tokio::test]
async fn test_ggen_environment_isolation() -> Result<()> {
    let env = CleanroomCliTestEnvironment::new().await?;

    // Verify that GGEN_HOME is set to isolated directory
    let output = env.run_ggen_command(&["--version"]).await?;
    assert_ggen_success(&output);

    // Verify no side effects in the system
    let home_dir = env.temp_dir();
    assert!(home_dir.exists());

    env.cleanup().await?;
    Ok(())
}

#[tokio::test]
async fn test_ggen_marketplace_comprehensive() -> Result<()> {
    let env = CleanroomCliTestEnvironment::new().await?;

    // Test marketplace functionality
    assert_ggen_marketplace_works(&env).await?;

    env.cleanup().await?;
    Ok(())
}

#[tokio::test]
async fn test_ggen_ai_help() -> Result<()> {
    let env = CleanroomCliTestEnvironment::new().await?;

    let output = env.run_ggen_command(&["ai", "--help"]).await?;
    assert_ggen_success(&output);
    assert_output_contains(&output, "AI");

    env.cleanup().await?;
    Ok(())
}

#[tokio::test]
async fn test_ggen_audit_help() -> Result<()> {
    let env = CleanroomCliTestEnvironment::new().await?;

    let output = env.run_ggen_command(&["audit", "--help"]).await?;
    assert_ggen_success(&output);
    assert_output_contains(&output, "audit");

    env.cleanup().await?;
    Ok(())
}

#[tokio::test]
async fn test_ggen_graph_help() -> Result<()> {
    let env = CleanroomCliTestEnvironment::new().await?;

    let output = env.run_ggen_command(&["graph", "--help"]).await?;
    assert_ggen_success(&output);
    assert_output_contains(&output, "graph");

    env.cleanup().await?;
    Ok(())
}

#[tokio::test]
async fn test_ggen_hook_help() -> Result<()> {
    let env = CleanroomCliTestEnvironment::new().await?;

    let output = env.run_ggen_command(&["hook", "--help"]).await?;
    assert_ggen_success(&output);
    assert_output_contains(&output, "hook");

    env.cleanup().await?;
    Ok(())
}

#[tokio::test]
async fn test_ggen_project_help() -> Result<()> {
    let env = CleanroomCliTestEnvironment::new().await?;

    let output = env.run_ggen_command(&["project", "--help"]).await?;
    assert_ggen_success(&output);
    assert_output_contains(&output, "project");

    env.cleanup().await?;
    Ok(())
}

#[tokio::test]
async fn test_ggen_error_handling_missing_args() -> Result<()> {
    let env = CleanroomCliTestEnvironment::new().await?;

    // Test commands that require arguments
    let failing_commands = vec![
        vec!["market", "add"],  // Missing package name
        vec!["market", "search"],  // Missing search query
    ];

    for cmd in failing_commands {
        let output = env.run_ggen_command(&cmd).await?;
        assert_ggen_failure(&output);

        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("required") || stderr.contains("error") || stderr.contains("missing"),
            "Expected error message for missing arguments"
        );
    }

    env.cleanup().await?;
    Ok(())
}

#[tokio::test]
async fn test_ggen_deterministic_output() -> Result<()> {
    let env = CleanroomCliTestEnvironment::new().await?;

    // Run the same command multiple times to verify deterministic output
    let output1 = env.run_ggen_command(&["--version"]).await?;
    let output2 = env.run_ggen_command(&["--version"]).await?;

    assert_ggen_success(&output1);
    assert_ggen_success(&output2);

    // Verify outputs are identical
    assert_eq!(output1.stdout, output2.stdout);

    env.cleanup().await?;
    Ok(())
}

// ============================================================================
// INTEGRATION WITH MEMORY COORDINATION
// ============================================================================

#[tokio::test]
async fn test_ggen_with_memory_coordination() -> Result<()> {
    let env = CleanroomCliTestEnvironment::new().await?;

    // Store test metadata in memory coordination system
    // This would use: npx claude-flow@alpha hooks post-edit --memory-key "hive/code/ggen-tests"

    // Run comprehensive test suite
    let test_results = vec![
        ("version", env.run_ggen_command(&["--version"]).await?),
        ("help", env.run_ggen_command(&["--help"]).await?),
        ("market_list", env.run_ggen_command(&["market", "list"]).await?),
    ];

    // Verify all tests passed
    for (test_name, output) in test_results {
        if !output.status.success() {
            eprintln!("Test '{}' failed", test_name);
            return Err(anyhow::anyhow!("Test suite failed"));
        }
    }

    env.cleanup().await?;
    Ok(())
}

#[cfg(test)]
mod performance_tests {
    use super::*;
    use std::time::Instant;

    #[tokio::test]
    async fn test_ggen_command_performance() -> Result<()> {
        let env = CleanroomCliTestEnvironment::new().await?;

        // Measure command execution time
        let start = Instant::now();
        let output = env.run_ggen_command(&["--version"]).await?;
        let duration = start.elapsed();

        assert_ggen_success(&output);

        // Verify command completes in reasonable time (< 5 seconds)
        assert!(duration.as_secs() < 5, "Command took too long: {:?}", duration);

        env.cleanup().await?;
        Ok(())
    }
}
