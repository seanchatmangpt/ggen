//! T0 Layer: Smoke Tests — CLI Boot & Core Response
//!
//! **Purpose**: The T0 (smoke) layer is the fastest gate — it proves the CLI
//! boots and core commands respond. No real synthesis, no artifacts, no receipts.
//! Just: command exists, exits with expected status, responds to --help.

use assert_cmd::Command;
use std::fs;
use tempfile::TempDir;

type TestResult = Result<(), Box<dyn std::error::Error>>;

/// Helper: Create a minimal ggen.toml in a temp directory.
///
/// Live frontmatter schema (`ggen_engine::config::GgenConfig`,
/// `deny_unknown_fields`): `[project]` is name-only, `[templates]` required.
fn setup_minimal_ggen_toml(dir: &TempDir) -> TestResult {
    let ggen_toml = r#"[project]
name = "test-project"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;
    fs::write(dir.path().join("ggen.toml"), ggen_toml)?;
    fs::create_dir_all(dir.path().join("templates"))?;
    fs::write(
        dir.path().join("templates/one.tmpl"),
        "---\nto: out/names.txt\nforce: true\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{% for row in results %}{{ row.name }}\n{% endfor %}",
    )?;
    Ok(())
}

/// Helper: Create a minimal valid TTL file
fn setup_minimal_ttl(dir: &TempDir) -> TestResult {
    let ttl = r#"@prefix ex: <http://example.org/> .
ex:Alice ex:name "Alice" .
"#;
    fs::write(dir.path().join("ontology.ttl"), ttl)?;
    Ok(())
}

/// T0-SMOKE-01: ggen --help exits 0
#[test]
fn smoke_01_help_command_exits_zero() -> TestResult {
    let _ = Command::cargo_bin("ggen")?.arg("--help").assert().success();
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
