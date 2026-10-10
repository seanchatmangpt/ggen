#![allow(
    dead_code,
    unused_imports,
    unused_variables,
    deprecated,
    clippy::all,
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    unused_mut
)]

//! End-to-end integration tests for utils commands
//!
//! **Chicago TDD Principles**:
//! - REAL CLI process execution
//! - REAL system checks (Rust, Cargo, Git)
//! - REAL environment inspection
//! - NO mocking of system state
//!
//! **Critical User Workflows (80/20)**:
//! 1. Run system diagnostics (doctor)
//! 2. Check environment variables
//! 3. Verify tool installation

use assert_cmd::Command;
use predicates::prelude::*;
use tempfile::TempDir;

/// Resolve the real `ggen` binary the same way `cli_boundary.rs::ggen_bin`
/// does: `CARGO_BIN_EXE_ggen` (set by `cargo test -p ggen-cli-lib`, never by
/// `-p ggen-engine`/`-p ggen-cli` — the root package is `autobins = false`),
/// then the workspace `target/{debug,release}/ggen`, then `PATH`. Panics
/// (loudly) if no candidate resolves, so failure happens at binary
/// resolution, not at first spawn.
fn ggen_bin() -> std::path::PathBuf {
    if let Ok(path) = std::env::var("CARGO_BIN_EXE_ggen") {
        let p = std::path::PathBuf::from(path);
        if p.exists() {
            return p;
        }
    }

    let target_root = std::env::var_os("CARGO_TARGET_DIR")
        .map(std::path::PathBuf::from)
        .or_else(|| {
            let manifest_dir =
                std::env::var_os("CARGO_MANIFEST_DIR").map(std::path::PathBuf::from)?;
            let mut dir: &std::path::Path = manifest_dir.as_path();
            loop {
                if dir.join("Cargo.lock").exists() {
                    return Some(dir.join("target"));
                }
                match dir.parent() {
                    Some(p) => dir = p,
                    None => return None,
                }
            }
        });

    if let Some(target) = target_root {
        for profile in &["debug", "release"] {
            let candidate = target.join(profile).join("ggen");
            if candidate.is_file() {
                return candidate;
            }
            let candidate_exe = target.join(profile).join("ggen.exe");
            if candidate_exe.is_file() {
                return candidate_exe;
            }
        }
    }

    panic!(
        "could not resolve the `ggen` binary: CARGO_BIN_EXE_ggen unset and no \
         target/debug/ggen found; build it with `cargo build -p ggen-cli-lib --bin ggen`"
    );
}

/// Helper to create ggen command
fn ggen() -> Command {
    Command::new(ggen_bin())
}

#[test]
#[ignore = "utils doctor command consolidated to root ggen doctor (v26.7.3)"]
fn test_utils_doctor_runs() {
    // Chicago TDD: Verify system diagnostics execute
    ggen().arg("utils").arg("doctor").assert().success().stdout(
        predicate::str::contains("Rust")
            .or(predicate::str::contains("Cargo"))
            .or(predicate::str::contains("checks")),
    );
}

#[test]
#[ignore = "utils doctor command consolidated to root ggen doctor (v26.7.3)"]
fn test_utils_doctor_all() {
    // Chicago TDD: Verify all checks mode
    ggen()
        .arg("utils")
        .arg("doctor")
        .arg("--all")
        .assert()
        .success();
}

#[test]
#[ignore = "utils doctor command consolidated to root ggen doctor (v26.7.3)"]
fn test_utils_doctor_env_format() {
    // Chicago TDD: Verify environment format output
    ggen()
        .arg("utils")
        .arg("doctor")
        .arg("--format")
        .arg("env")
        .assert()
        .success();
}

#[test]
#[ignore = "utils doctor command consolidated to root ggen doctor (v26.7.3)"]
fn test_utils_doctor_json_format() {
    // Chicago TDD: Verify JSON format output
    let output = ggen()
        .arg("utils")
        .arg("doctor")
        .arg("--format")
        .arg("json")
        .output()
        .expect("Failed to execute");

    // Command should complete successfully
    assert!(output.status.success(), "Doctor should succeed");
}

#[test]
fn test_utils_env_lists() {
    // Chicago TDD: Verify environment listing
    // Note: env command is stubbed, returns empty
    ggen()
        .arg("utils")
        .arg("env")
        .arg("--list")
        .assert()
        .success();
}

#[test]
fn test_utils_env_get() {
    // Chicago TDD: Verify environment variable get
    // Note: env command is stubbed, returns empty
    ggen()
        .arg("utils")
        .arg("env")
        .arg("--get")
        .arg("PATH")
        .assert()
        .success();
}

#[test]
fn test_utils_env_set() {
    // Chicago TDD: Verify environment variable set
    // Note: env command is stubbed
    ggen()
        .arg("utils")
        .arg("env")
        .arg("--set")
        .arg("TEST_VAR=test")
        .assert()
        .success();
}

#[test]
fn test_utils_help_shows_verbs() {
    // Chicago TDD: Verify help state is comprehensive
    ggen()
        .arg("utils")
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("env"));
}

#[test]
#[ignore = "utils doctor command consolidated to root ggen doctor (v26.7.3)"]
fn test_utils_doctor_help() {
    // Chicago TDD: Verify verb-specific help
    ggen()
        .arg("utils")
        .arg("doctor")
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("diagnostic").or(predicate::str::contains("check")));
}

#[test]
fn test_utils_invalid_verb() {
    // Chicago TDD: Verify error handling for invalid verbs
    ggen()
        .arg("utils")
        .arg("invalid-verb")
        .assert()
        .failure()
        .stderr(predicate::str::contains("error").or(predicate::str::contains("invalid")));
}

#[test]
#[ignore = "utils doctor command consolidated to root ggen doctor (v26.7.3)"]
fn test_utils_doctor_checks_system_tools() {
    // Chicago TDD: Verify doctor checks for required tools
    let output = ggen()
        .arg("utils")
        .arg("doctor")
        .output()
        .expect("Failed to execute");

    let stdout = String::from_utf8_lossy(&output.stdout);

    // Verify system tools are checked
    // Output should contain information about Rust, Cargo, or Git
    assert!(
        stdout.contains("Rust")
            || stdout.contains("Cargo")
            || stdout.contains("Git")
            || stdout.contains("checks"),
        "Doctor should check system tools"
    );
}

#[test]
#[ignore = "utils doctor command consolidated to root ggen doctor (v26.7.3)"]
fn test_utils_doctor_reports_health_status() {
    // Chicago TDD: Verify doctor reports overall health
    let output = ggen()
        .arg("utils")
        .arg("doctor")
        .output()
        .expect("Failed to execute");

    let stdout = String::from_utf8_lossy(&output.stdout);

    // Verify health status reported
    assert!(
        stdout.contains("healthy")
            || stdout.contains("needs attention")
            || stdout.contains("status"),
        "Doctor should report health status"
    );
}
