//! Chicago TDD integration tests for the headless admission gate.
//!
//! Real files on disk → real analyzers → real diagnostics. No mocks. These prove
//! the bridge that generated hooks rely on: ERROR diagnostics produce a non-zero
//! exit code, clean law-surface files produce exit 0.

use std::fs;
use tempfile::TempDir;

use ggen_lsp::check_files;

#[test]
fn values_in_external_rq_is_refused() {
    let dir = TempDir::new().expect("tempdir");
    let rq = dir.path().join("rule.rq");
    fs::write(&rq, "SELECT ?s WHERE { VALUES ?s { <http://x> } }").expect("write");

    let report = check_files(&[rq]);

    assert!(report.has_errors(), "VALUES in .rq must be refused");
    assert_eq!(report.exit_code(), 1);
    assert!(report.files[0]
        .diagnostics
        .iter()
        .any(|d| d.code
            == Some(tower_lsp::lsp_types::NumberOrString::String("E0010".into()))));
}

#[test]
fn clean_turtle_passes_the_gate() {
    let dir = TempDir::new().expect("tempdir");
    let ttl = dir.path().join("spec.ttl");
    fs::write(
        &ttl,
        "@prefix ex: <http://example.org/> .\nex:Thing a ex:Class .\n",
    )
    .expect("write");

    let report = check_files(&[ttl]);

    assert!(!report.has_errors(), "clean turtle must pass");
    assert_eq!(report.exit_code(), 0);
}

#[test]
fn malformed_turtle_is_refused_with_location() {
    let dir = TempDir::new().expect("tempdir");
    let ttl = dir.path().join("broken.ttl");
    fs::write(&ttl, "@prefix ex: <http://example.org/> .\nex:a ex:b \"oops\n").expect("write");

    let report = check_files(&[ttl]);

    assert!(report.has_errors());
    let diag = &report.files[0].diagnostics[0];
    assert_eq!(diag.severity, Some(tower_lsp::lsp_types::DiagnosticSeverity::ERROR));
}

#[test]
fn invalid_config_enum_is_refused() {
    let dir = TempDir::new().expect("tempdir");
    let cfg = dir.path().join("ggen.toml");
    fs::write(&cfg, "[logging]\nlevel = \"verbose\"\n").expect("write");

    let report = check_files(&[cfg]);

    assert!(report.has_errors(), "invalid enum value is an error");
    assert!(report.files[0]
        .diagnostics
        .iter()
        .any(|d| d.message.contains("invalid value") && d.message.contains("level")));
}

#[test]
fn ggen_does_not_flag_llm_sections() {
    // ggen supplies LSP/MCP/A2A infrastructure; it is not in the LLM business.
    let dir = TempDir::new().expect("tempdir");
    let cfg = dir.path().join("ggen.toml");
    fs::write(&cfg, "[ai]\nprovider = \"openai\"\n").expect("write");

    let report = check_files(&[cfg]);

    assert!(!report.has_errors(), "an [ai] section is not a ggen concern");
}

#[test]
fn multiple_files_aggregate_and_one_bad_fails_the_batch() {
    let dir = TempDir::new().expect("tempdir");
    let good = dir.path().join("good.ttl");
    let bad = dir.path().join("bad.rq");
    fs::write(&good, "@prefix ex: <http://example.org/> .\nex:A a ex:B .\n").expect("write");
    fs::write(&bad, "SELECT ?s WHERE { VALUES ?s { <http://x> } }").expect("write");

    let report = check_files(&[good, bad]);

    assert_eq!(report.files.len(), 2);
    assert!(report.has_errors(), "one bad file fails the whole batch");
}
