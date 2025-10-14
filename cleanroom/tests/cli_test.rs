//! Comprehensive CLI testing without Docker containers
//!
//! This test demonstrates cleanroom's ability to test CLI applications
//! using process isolation without requiring container infrastructure.

use anyhow::{Context, Result};
use cleanroom::{Cleanroom, ExecutionResult, RuntimeConfig};
use std::path::PathBuf;

/// Helper to build the test CLI binary
fn build_test_cli() -> Result<PathBuf> {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let cli_source = manifest_dir.join("examples").join("test_cli.rs");

    // Build the CLI binary
    let output = std::process::Command::new("rustc")
        .arg(&cli_source)
        .arg("-o")
        .arg(manifest_dir.join("target").join("test_cli"))
        .output()
        .context("Failed to compile test CLI")?;

    if !output.status.success() {
        anyhow::bail!(
            "Failed to build test CLI: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    Ok(manifest_dir.join("target").join("test_cli"))
}

/// Helper to create a cleanroom instance with standard config
fn create_cleanroom() -> Result<Cleanroom> {
    let config = RuntimeConfig {
        max_memory_mb: 100,
        timeout_secs: 5,
        network_enabled: false,
        filesystem_readonly: false,
        max_processes: 10,
    };

    Cleanroom::new(config).context("Failed to create cleanroom")
}

/// Helper to execute CLI and check results
fn execute_cli(
    cleanroom: &Cleanroom,
    cli_path: &PathBuf,
    args: &[&str],
) -> Result<ExecutionResult> {
    let mut command = vec![cli_path.to_str().context("Invalid CLI path")?];
    command.extend(args);

    cleanroom.execute_command(&command).context("Failed to execute CLI")
}

#[test]
fn test_cli_echo_command() -> Result<()> {
    let cli_path = build_test_cli()?;
    let cleanroom = create_cleanroom()?;

    // Test basic echo
    let result = execute_cli(&cleanroom, &cli_path, &["echo", "hello", "world"])?;

    assert_eq!(result.exit_code, 0, "Echo command should succeed");
    assert_eq!(
        result.stdout.trim(),
        "hello world",
        "Echo should output the message"
    );
    assert!(result.stderr.is_empty(), "Echo should have no stderr output");

    Ok(())
}

#[test]
fn test_cli_echo_single_word() -> Result<()> {
    let cli_path = build_test_cli()?;
    let cleanroom = create_cleanroom()?;

    let result = execute_cli(&cleanroom, &cli_path, &["echo", "test"])?;

    assert_eq!(result.exit_code, 0);
    assert_eq!(result.stdout.trim(), "test");
    assert!(result.stderr.is_empty());

    Ok(())
}

#[test]
fn test_cli_echo_missing_argument() -> Result<()> {
    let cli_path = build_test_cli()?;
    let cleanroom = create_cleanroom()?;

    let result = execute_cli(&cleanroom, &cli_path, &["echo"])?;

    assert_eq!(result.exit_code, 1, "Echo without message should fail");
    assert!(
        result.stderr.contains("echo requires a message"),
        "Should show error message"
    );

    Ok(())
}

#[test]
fn test_cli_add_command() -> Result<()> {
    let cli_path = build_test_cli()?;
    let cleanroom = create_cleanroom()?;

    // Test positive numbers
    let result = execute_cli(&cleanroom, &cli_path, &["add", "5", "3"])?;

    assert_eq!(result.exit_code, 0, "Add command should succeed");
    assert_eq!(result.stdout.trim(), "8", "Should output correct sum");
    assert!(result.stderr.is_empty());

    Ok(())
}

#[test]
fn test_cli_add_negative_numbers() -> Result<()> {
    let cli_path = build_test_cli()?;
    let cleanroom = create_cleanroom()?;

    let result = execute_cli(&cleanroom, &cli_path, &["add", "-5", "3"])?;

    assert_eq!(result.exit_code, 0);
    assert_eq!(result.stdout.trim(), "-2");

    Ok(())
}

#[test]
fn test_cli_add_zero() -> Result<()> {
    let cli_path = build_test_cli()?;
    let cleanroom = create_cleanroom()?;

    let result = execute_cli(&cleanroom, &cli_path, &["add", "0", "0"])?;

    assert_eq!(result.exit_code, 0);
    assert_eq!(result.stdout.trim(), "0");

    Ok(())
}

#[test]
fn test_cli_add_missing_arguments() -> Result<()> {
    let cli_path = build_test_cli()?;
    let cleanroom = create_cleanroom()?;

    // Missing both arguments
    let result = execute_cli(&cleanroom, &cli_path, &["add"])?;
    assert_eq!(result.exit_code, 1);
    assert!(result.stderr.contains("requires two numbers"));

    // Missing one argument
    let result = execute_cli(&cleanroom, &cli_path, &["add", "5"])?;
    assert_eq!(result.exit_code, 1);
    assert!(result.stderr.contains("requires two numbers"));

    Ok(())
}

#[test]
fn test_cli_add_invalid_numbers() -> Result<()> {
    let cli_path = build_test_cli()?;
    let cleanroom = create_cleanroom()?;

    let result = execute_cli(&cleanroom, &cli_path, &["add", "abc", "def"])?;

    assert_eq!(result.exit_code, 1, "Invalid numbers should fail");
    assert!(
        result.stderr.contains("must be numbers"),
        "Should show error about numbers"
    );

    Ok(())
}

#[test]
fn test_cli_version_command() -> Result<()> {
    let cli_path = build_test_cli()?;
    let cleanroom = create_cleanroom()?;

    let result = execute_cli(&cleanroom, &cli_path, &["version"])?;

    assert_eq!(result.exit_code, 0, "Version command should succeed");
    assert!(
        result.stdout.contains("version 1.0.0"),
        "Should display version"
    );
    assert!(result.stderr.is_empty());

    Ok(())
}

#[test]
fn test_cli_help_command() -> Result<()> {
    let cli_path = build_test_cli()?;
    let cleanroom = create_cleanroom()?;

    let result = execute_cli(&cleanroom, &cli_path, &["help"])?;

    assert_eq!(result.exit_code, 0, "Help command should succeed");
    assert!(result.stdout.contains("Commands:"), "Should list commands");
    assert!(result.stdout.contains("echo"), "Should mention echo command");
    assert!(result.stdout.contains("add"), "Should mention add command");
    assert!(result.stdout.contains("version"), "Should mention version command");
    assert!(result.stderr.is_empty());

    Ok(())
}

#[test]
fn test_cli_no_arguments() -> Result<()> {
    let cli_path = build_test_cli()?;
    let cleanroom = create_cleanroom()?;

    let result = execute_cli(&cleanroom, &cli_path, &[])?;

    assert_eq!(result.exit_code, 1, "No arguments should fail");
    assert!(result.stderr.contains("Usage:"), "Should show usage");
    assert!(result.stderr.contains("Commands:"), "Should list commands");

    Ok(())
}

#[test]
fn test_cli_unknown_command() -> Result<()> {
    let cli_path = build_test_cli()?;
    let cleanroom = create_cleanroom()?;

    let result = execute_cli(&cleanroom, &cli_path, &["unknown"])?;

    assert_eq!(result.exit_code, 1, "Unknown command should fail");
    assert!(
        result.stderr.contains("unknown command"),
        "Should indicate unknown command"
    );
    assert!(
        result.stderr.contains("help"),
        "Should suggest help command"
    );

    Ok(())
}

#[test]
fn test_cli_multiple_executions() -> Result<()> {
    let cli_path = build_test_cli()?;
    let cleanroom = create_cleanroom()?;

    // Execute multiple commands to ensure no state leakage
    for i in 1..=5 {
        let result = execute_cli(&cleanroom, &cli_path, &["add", &i.to_string(), "1"])?;
        assert_eq!(result.exit_code, 0);
        assert_eq!(result.stdout.trim(), (i + 1).to_string());
    }

    Ok(())
}

#[test]
fn test_cli_resource_limits() -> Result<()> {
    let cli_path = build_test_cli()?;
    let cleanroom = create_cleanroom()?;

    // Verify resource limits are enforced
    let metrics = cleanroom.get_metrics().context("Failed to get metrics")?;

    assert!(
        metrics.memory_used_mb <= 100,
        "Memory usage should be within limits"
    );
    assert!(
        metrics.execution_time_secs < 5.0,
        "Execution should complete within timeout"
    );

    Ok(())
}

#[test]
fn test_cli_parallel_execution() -> Result<()> {
    let cli_path = build_test_cli()?;

    // Test parallel execution of multiple commands
    let handles: Vec<_> = (0..10)
        .map(|i| {
            let cli_path_clone = cli_path.clone();
            std::thread::spawn(move || -> Result<()> {
                let cleanroom = create_cleanroom()?;
                let result = execute_cli(&cleanroom, &cli_path_clone, &["add", &i.to_string(), "1"])?;
                assert_eq!(result.exit_code, 0);
                Ok(())
            })
        })
        .collect();

    // Wait for all threads and check results
    for handle in handles {
        handle.join().map_err(|_| anyhow::anyhow!("Thread panicked"))??;
    }

    Ok(())
}

#[test]
fn test_cli_exit_codes() -> Result<()> {
    let cli_path = build_test_cli()?;
    let cleanroom = create_cleanroom()?;

    // Successful commands should return 0
    let success_commands = vec![
        vec!["echo", "test"],
        vec!["add", "1", "2"],
        vec!["version"],
        vec!["help"],
    ];

    for args in success_commands {
        let result = execute_cli(&cleanroom, &cli_path, &args)?;
        assert_eq!(
            result.exit_code, 0,
            "Command {:?} should return exit code 0",
            args
        );
    }

    // Error commands should return 1
    let error_commands = vec![
        vec![],
        vec!["unknown"],
        vec!["echo"],
        vec!["add"],
        vec!["add", "1"],
        vec!["add", "abc", "def"],
    ];

    for args in error_commands {
        let result = execute_cli(&cleanroom, &cli_path, &args)?;
        assert_eq!(
            result.exit_code, 1,
            "Command {:?} should return exit code 1",
            args
        );
    }

    Ok(())
}

#[test]
fn test_cli_cleanup() -> Result<()> {
    let cli_path = build_test_cli()?;
    let cleanroom = create_cleanroom()?;

    // Execute command
    let _ = execute_cli(&cleanroom, &cli_path, &["version"])?;

    // Verify cleanup
    cleanroom.cleanup().context("Failed to cleanup")?;

    // Ensure resources are freed
    let metrics = cleanroom.get_metrics().context("Failed to get metrics")?;
    assert_eq!(metrics.active_processes, 0, "All processes should be cleaned up");

    Ok(())
}
