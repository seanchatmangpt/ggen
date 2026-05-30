//! T0 Layer: Smoke Tests — CLI Boot & Core Response
//!
//! **Purpose**: The T0 (smoke) layer is the fastest gate — it proves the CLI
//! boots and core commands respond. No real synthesis, no artifacts, no receipts.
//! Just: command exists, exits with expected status, responds to --help.

use assert_cmd::Command;
use tempfile::TempDir;
use std::fs;

type TestResult = Result<(), Box<dyn std::error::Error>>;

/// Helper: Create a minimal ggen.toml in a temp directory
fn setup_minimal_ggen_toml(dir: &TempDir) -> TestResult {
    let ggen_toml = r#"[project]
name = "test-project"
version = "0.1.0"

[ontology]
source = "ontology.ttl"
"#;
    fs::write(dir.path().join("ggen.toml"), ggen_toml)?;
    Ok(())
}

/// Helper: Create a minimal valid TTL file
fn setup_minimal_ttl(dir: &TempDir) -> TestResult {
    let ttl = r#"@prefix ex: <http://example.org/> .
ex:Thing a rdfs:Class .
"#;
    fs::write(dir.path().join("ontology.ttl"), ttl)?;
    Ok(())
}

/// T0-SMOKE-01: ggen --help exits 0
#[test]
fn smoke_01_help_command_exits_zero() -> TestResult {
    let _ = Command::cargo_bin("ggen")?
        .arg("--help")
        .assert()
        .success();
    Ok(())
}

/// T0-SMOKE-02: ggen doctor succeeds on valid workspace
#[test]
fn smoke_02_doctor_boot_succeeds() -> TestResult {
    let temp = TempDir::new()?;
    setup_minimal_ggen_toml(&temp)?;
    setup_minimal_ttl(&temp)?;

    let _ = Command::cargo_bin("ggen")?
        .arg("doctor")
        .current_dir(temp.path())
        .assert()
        .success();
    Ok(())
}
