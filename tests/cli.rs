// NOTE: This test file tests unimplemented CLI commands (audit, market, lifecycle, shell, ci,
// etc.) and uses wrong version expectations ("ggen 1.2.0" vs actual "6.x"). Gated behind
// the `integration` feature until the full CLI surface is implemented.
#![cfg(feature = "integration")]

use assert_cmd::Command;
use predicates::prelude::*;
use serial_test::serial;
// use url::Url; // Not available in test dependencies

/// Saves the prior value of an env var and restores it (or removes it) on Drop.
/// Defined locally per binary; do not share across crates.
struct EnvVarGuard {
    key: &'static str,
    previous: Option<std::ffi::OsString>,
}

impl EnvVarGuard {
    fn set(key: &'static str, value: &str) -> Self {
        let previous = std::env::var_os(key);
        std::env::set_var(key, value);
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

#[test]
fn test_cli_basic() {
    let mut cmd = Command::cargo_bin("ggen").expect("Calling binary failed");
    cmd.assert().failure();
}

#[test]
fn test_version() {
    let expected_version = "ggen 1.2.0\n";
    let mut cmd = Command::cargo_bin("ggen").expect("Calling binary failed");
    cmd.arg("--version").assert().stdout(expected_version);
}

#[test]
fn test_hazard_exit_code() {
    let mut cmd = Command::cargo_bin("ggen").expect("Calling binary failed");
    cmd.arg("audit")
        .arg("hazard")
        .arg("scan")
        .assert()
        .failure();
}

#[test]
fn test_hazard_stdout() {
    let mut cmd = Command::cargo_bin("ggen").expect("Calling binary failed");
    cmd.arg("audit")
        .arg("hazard")
        .arg("scan")
        .assert()
        .failure()
        .stdout(predicate::str::contains("Scanning"));
}

#[test]
fn test_cli_help_commands() {
    // Batch test all help commands to reduce process spawning
    let commands = [
        ("market", "Marketplace operations"),
        ("ai", "AI-powered template generation"),
        ("audit", "Security and performance auditing"),
        ("ci", "CI/CD operations"),
        ("graph", "RDF graph operations"),
        ("hook", "Knowledge hooks"),
        ("lifecycle", "Universal lifecycle management"),
        ("project", "Project scaffolding"),
        ("shell", "Shell integration"),
        ("template", "Template management"),
    ];

    for (cmd_name, expected_text) in &commands {
        let mut cmd = Command::cargo_bin("ggen").unwrap();
        cmd.arg(cmd_name).arg("--help");
        cmd.assert()
            .success()
            .stdout(predicate::str::contains(*expected_text));
    }
}

#[test]
#[serial(GGEN_REGISTRY_URL)]
fn test_search_command_basic_usage() {
    // Set up local registry URL for testing
    let registry_path = std::env::current_dir().unwrap().join("registry");
    let registry_url = format!("file://{}/", registry_path.to_string_lossy());
    let _guard = EnvVarGuard::set("GGEN_REGISTRY_URL", &registry_url);

    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("market").arg("search").arg("rust");
    // Search now works with local mock registry
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("rig-mcp"));
}

#[test]
#[serial(GGEN_REGISTRY_URL)]
fn test_search_command_with_filters() {
    // Set up local registry URL for testing
    let registry_path = std::env::current_dir().unwrap().join("registry");
    let registry_url = format!("file://{}/", registry_path.to_string_lossy());
    let _guard = EnvVarGuard::set("GGEN_REGISTRY_URL", &registry_url);

    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("market")
        .arg("search")
        .arg("rust")
        .arg("--category")
        .arg("rust")
        .arg("--limit")
        .arg("5")
        .arg("--detailed");
    // Search now works with local mock registry
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("rig-mcp"));
}

// Individual help tests removed - now batched in test_cli_help_commands

#[test]
fn test_cli_error_handling() {
    // Test invalid command
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("invalid-command");
    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("unrecognized subcommand"));

    // Test missing required arguments
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("market").arg("add");
    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("required"));

    // Test invalid arguments
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("market").arg("search").arg("--invalid-flag");
    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("unexpected argument"));
}

#[test]
#[serial(GGEN_REGISTRY_URL)]
fn test_cli_output_formats() {
    // Set up local registry URL for testing
    let registry_path = std::env::current_dir().unwrap().join("registry");
    let registry_url = format!("file://{}/", registry_path.to_string_lossy());
    let _guard = EnvVarGuard::set("GGEN_REGISTRY_URL", &registry_url);

    // Test JSON output - now works with local mock registry
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("market").arg("search").arg("rust").arg("--json");
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("\"id\""));

    // Test detailed output - now works with local mock registry
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("market")
        .arg("search")
        .arg("rust")
        .arg("--detailed");
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("rig-mcp"));
}

#[test]
fn test_cli_environment_variables() {
    // Test with GGEN_TRACE environment variable
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.env("GGEN_TRACE", "debug");
    cmd.arg("audit").arg("hazard").arg("scan");
    cmd.assert()
        .failure()
        .stdout(predicate::str::contains("Scanning"));

    // Test with different trace levels
    let trace_levels = ["error", "warn", "info", "debug", "trace"];
    for level in &trace_levels {
        let mut cmd = Command::cargo_bin("ggen").unwrap();
        cmd.env("GGEN_TRACE", level);
        cmd.arg("audit").arg("hazard").arg("scan");
        cmd.assert()
            .failure()
            .stdout(predicate::str::contains("Scanning"));
    }
}
