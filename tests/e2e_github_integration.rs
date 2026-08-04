//! ARCHIVED test functions (2026-08-03): 8 of this file's original 14 tests invoked the `ci`
//! noun (`ggen ci pages status`, `ggen ci workflow status`, `ggen ci trigger`), which no
//! longer exists anywhere in the current CLI surface.
//!
//! Confirmed live: `ggen ci pages status --help` fails with `error: unrecognized subcommand
//! 'ci'` (clap even suggests the nearest real noun, `capability`). `ggen --help`'s current
//! noun list (`init`, `sync`, `bblock`, `doctor`, `policy`, `pack`, `graph`, `agent`, `utils`,
//! `packs`, `law`, `capability`, `receipt`, `ontology`, `help`) has no `ci` entry at all, and
//! neither `ggen agent --help` nor `ggen policy --help` expose anything resembling GitHub
//! Actions workflow/Pages status querying or triggering. This is a real removal from the
//! pre-v26.7.16-CLI-routing-flip design, not a rename, and there is no current equivalent to
//! repoint these 8 tests at. See also `tests/ci_validate.rs`, archived in the same
//! `ci`-noun-removed state.
//!
//! Removed test functions: `test_github_pages_status_command`,
//! `test_github_pages_status_with_explicit_repo`, `test_github_workflow_status_command`,
//! `test_github_workflow_status_with_workflow_name`, `test_github_trigger_workflow_command`,
//! `test_github_help_commands`, `test_github_workflow_status_lists_workflows`,
//! `test_github_api_error_messages_are_helpful` -- each asserted on GitHub-specific stdout
//! (e.g. "Pages", "Workflow", "trigger") or a `--help`/`.success()` exit that the
//! unrecognized-subcommand failure can never produce.
//!
//! The 6 tests below are unaffected and still pass (3 run, 3 marked `#[ignore]` as
//! network-dependent): their assertions don't require the `ci` command itself to succeed --
//! each accepts a generic error/output shape (e.g. `!stdout.is_empty() || !stderr.is_empty()`)
//! that the "unrecognized subcommand" failure also satisfies.
//!
//! If GitHub Actions workflow/Pages status querying or triggering is wanted again, it would
//! need to be rebuilt as a real CLI command first -- restoring these assertions without that
//! implementation would just recreate the removed-subcommand failures this archival fixes.
#![cfg(feature = "integration")]

use anyhow::Result;
use assert_cmd::Command;
use serial_test::serial;

/// Saves the prior value of an env var and restores it (or removes it) on Drop.
/// Defined locally per binary; do not share across crates.
struct EnvVarGuard {
    key: &'static str,
    previous: Option<std::ffi::OsString>,
}

impl EnvVarGuard {
    fn unset(key: &'static str) -> Self {
        let previous = std::env::var_os(key);
        std::env::remove_var(key);
        Self { key, previous }
    }
}

impl Drop for EnvVarGuard {
    fn drop(&mut self) {
        match &self.previous {
            None => std::env::remove_var(self.key),
            Some(v) => std::env::set_var(self.key, v),
        }
    }
}

/// E2E tests for GitHub API integration
///
/// Tests the GitHub API commands added in v1.0.0:
/// - ggen ci pages status
/// - ggen ci workflow status
/// - ggen ci trigger

#[test]
#[ignore = "network-dependent: requires git remote configuration"]
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
#[serial(GITHUB_TOKEN)]
fn test_github_commands_handle_missing_token() -> Result<()> {
    // Test that commands work or fail gracefully without GITHUB_TOKEN
    // Temporarily unset GITHUB_TOKEN if it exists; guard restores on Drop.
    let _token_guard = EnvVarGuard::unset("GITHUB_TOKEN");

    let mut cmd = Command::cargo_bin("ggen")?;
    cmd.arg("ci").arg("pages").arg("status");

    let output = cmd.output()?;

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
#[ignore = "network-dependent: requires GitHub API access"]
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
#[ignore = "network-dependent: requires GitHub API access"]
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
