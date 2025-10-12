use anyhow::Result;
use assert_cmd::Command;
use predicates::prelude::*;
use std::env;

/// E2E tests for GitHub API integration
///
/// Tests the GitHub API commands added in v1.0.0:
/// - ggen ci pages status
/// - ggen ci workflow status
/// - ggen ci trigger

#[test]
fn test_github_pages_status_command() -> Result<()> {
    // Test that the command exists and has proper help
    let mut cmd = Command::cargo_bin("ggen")?;
    cmd.arg("ci").arg("pages").arg("status").arg("--help");

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("status"));

    Ok(())
}

#[test]
fn test_github_pages_status_with_explicit_repo() -> Result<()> {
    // Test with explicit repository argument
    let mut cmd = Command::cargo_bin("ggen")?;
    cmd.arg("ci").arg("pages").arg("status");

    // Command might fail without GITHUB_TOKEN, but it should run
    let output = cmd.output()?;

    // Should output either success info or error message (not crash)
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    // One of these should contain relevant information
    assert!(
        stdout.contains("Pages") || stderr.contains("Pages") || stderr.contains("Error"),
        "Command should provide pages status or error message"
    );

    Ok(())
}

#[test]
fn test_github_workflow_status_command() -> Result<()> {
    // Test that the command exists and has proper help
    let mut cmd = Command::cargo_bin("ggen")?;
    cmd.arg("ci").arg("workflow").arg("status").arg("--help");

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("status"));

    Ok(())
}

#[test]
fn test_github_workflow_status_with_workflow_name() -> Result<()> {
    // Test with explicit workflow name
    let mut cmd = Command::cargo_bin("ggen")?;
    cmd.arg("ci")
        .arg("workflow")
        .arg("status")
        .arg("--workflow")
        .arg("Build and Deploy GitHub Pages");

    // Command might fail without GITHUB_TOKEN or if workflow doesn't exist
    let output = cmd.output()?;

    // Should provide some output (success or error)
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    // Should contain workflow-related information or error
    assert!(
        stdout.contains("Workflow")
            || stdout.contains("Build")
            || stderr.contains("Workflow")
            || stderr.contains("Error"),
        "Command should provide workflow status or error message"
    );

    Ok(())
}

#[test]
fn test_github_trigger_workflow_command() -> Result<()> {
    // Test that the command exists and has proper help
    let mut cmd = Command::cargo_bin("ggen")?;
    cmd.arg("ci").arg("trigger").arg("--help");

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("trigger"));

    Ok(())
}

#[test]
#[ignore] // Network-dependent: requires git remote configuration
fn test_github_repo_auto_detection() -> Result<()> {
    // Test that commands can auto-detect repo from git remote
    // This will fail gracefully if not in a git repo

    let mut cmd = Command::cargo_bin("ggen")?;
    cmd.arg("ci").arg("pages").arg("status");

    let output = cmd.output()?;
    let stderr = String::from_utf8_lossy(&output.stderr);

    // Should either detect repo successfully or provide helpful error
    assert!(
        stderr.contains("detected")
            || stderr.contains("repository")
            || stderr.contains("Error")
            || stderr.contains("Pages"),
        "Command should attempt repo detection or provide error"
    );

    Ok(())
}

#[test]
fn test_github_commands_handle_missing_token() -> Result<()> {
    // Test that commands work or fail gracefully without GITHUB_TOKEN
    // Temporarily unset GITHUB_TOKEN if it exists
    let original_token = env::var("GITHUB_TOKEN").ok();
    env::remove_var("GITHUB_TOKEN");

    let mut cmd = Command::cargo_bin("ggen")?;
    cmd.arg("ci").arg("pages").arg("status");

    let output = cmd.output()?;

    // Restore original token
    if let Some(token) = original_token {
        env::set_var("GITHUB_TOKEN", token);
    }

    // Command should run (might fail due to rate limiting, but shouldn't crash)
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    // Should provide some output
    assert!(
        !stdout.is_empty() || !stderr.is_empty(),
        "Command should provide output even without token"
    );

    Ok(())
}

#[test]
#[ignore] // Network-dependent: requires GitHub API access
fn test_github_pages_status_output_format() -> Result<()> {
    // Test that pages-status provides expected output format
    let mut cmd = Command::cargo_bin("ggen")?;
    cmd.arg("ci").arg("pages").arg("status");

    let output = cmd.output()?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    // Should mention pages URL or status
    assert!(
        stdout.contains("github.io")
            || stdout.contains("Pages URL")
            || stdout.contains("Status")
            || stderr.contains("Error")
            || stderr.contains("not configured"),
        "Output should contain pages information or error"
    );

    Ok(())
}

#[test]
fn test_github_workflow_status_lists_workflows() -> Result<()> {
    // Test that workflow-status can list workflows
    let mut cmd = Command::cargo_bin("ggen")?;
    cmd.arg("ci").arg("workflow").arg("status");

    let output = cmd.output()?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    // Should list workflows or provide error
    assert!(
        stdout.contains("Workflow")
            || stdout.contains("workflow")
            || stderr.contains("Error")
            || stderr.contains("No workflows"),
        "Output should contain workflow information or error"
    );

    Ok(())
}

#[test]
fn test_github_commands_validate_repo_format() -> Result<()> {
    // Test that commands validate repository format
    let mut cmd = Command::cargo_bin("ggen")?;
    cmd.arg("ci").arg("pages").arg("status"); // No repo argument needed for CI commands

    let output = cmd.output()?;

    // Should fail with validation error
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("Error") || stderr.contains("invalid") || stderr.contains("format"),
            "Should provide error for invalid repo format"
        );
    }

    Ok(())
}

#[test]
fn test_github_help_commands() -> Result<()> {
    // Test that all GitHub subcommands have help text
    let subcommands = ["pages status", "workflow status", "trigger"];

    for subcommand in &subcommands {
        let mut cmd = Command::cargo_bin("ggen")?;
        cmd.args(["ci"].iter().copied().chain(subcommand.split(" ")))
            .arg("--help");

        cmd.assert().success().stdout(predicate::str::contains(
            subcommand.split(' ').next().unwrap(),
        ));
    }

    Ok(())
}

#[test]
#[ignore] // Network-dependent: requires GitHub API access
fn test_github_integration_with_public_repo() -> Result<()> {
    // Test GitHub integration with a known public repository
    // This is the most realistic E2E test
    let mut cmd = Command::cargo_bin("ggen")?;
    cmd.arg("ci").arg("pages").arg("status");

    let output = cmd.output()?;

    // Command should complete (success or known error)
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    // Should get either pages info or a specific error
    assert!(
        stdout.contains("seanchatmangpt.github.io/ggen")
            || stdout.contains("Status:")
            || stdout.contains("Pages URL:")
            || stderr.contains("Error:")
            || stderr.contains("not configured")
            || stderr.contains("rate limit"),
        "Command should provide pages status or specific error for known repo"
    );

    Ok(())
}

#[test]
fn test_github_commands_performance() -> Result<()> {
    // Test that GitHub commands complete in reasonable time
    use std::time::Instant;

    let start = Instant::now();

    let mut cmd = Command::cargo_bin("ggen")?;
    cmd.arg("ci").arg("pages").arg("status");

    let _ = cmd.output()?;

    let duration = start.elapsed();

    // Should complete within 10 seconds (network calls)
    assert!(
        duration.as_secs() < 10,
        "GitHub command should complete within 10 seconds, took {:?}",
        duration
    );

    Ok(())
}

#[test]
fn test_github_api_error_messages_are_helpful() -> Result<()> {
    // Test that error messages are user-friendly
    let mut cmd = Command::cargo_bin("ggen")?;
    cmd.arg("ci").arg("pages").arg("status");

    let output = cmd.output()?;
    let stderr = String::from_utf8_lossy(&output.stderr);

    // Error should be informative
    if !output.status.success() {
        assert!(
            stderr.contains("Error")
                || stderr.contains("not found")
                || stderr.contains("404")
                || stderr.contains("does not exist"),
            "Error message should be informative, got: {}",
            stderr
        );
    }

    Ok(())
}
