#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::duration_suboptimal_units,
    clippy::branches_sharing_code,
    clippy::used_underscore_binding,
    clippy::single_char_pattern,
    clippy::ignore_without_reason,
    clippy::cloned_ref_to_slice_refs,
    clippy::doc_overindented_list_items,
    clippy::match_wildcard_for_single_variants,
    clippy::ignored_unit_patterns,
    clippy::needless_collect,
    clippy::unnecessary_map_or,
    clippy::manual_flatten,
    clippy::manual_strip,
    clippy::future_not_send,
    clippy::unnested_or_patterns,
    clippy::no_effect_underscore_binding,
    clippy::literal_string_with_formatting_args
)]

//! End-to-end integration tests for the `graph` CLI noun.
//!
//! **Chicago TDD Principles**:
//! - REAL RDF validation via the real `ggen` binary
//! - REAL file system operations (Turtle fixtures written to a TempDir)
//! - REAL state verification of exit codes and output
//! - NO mocking of the RDF engine
//!
//! **Surface note (v26.11.8, lane V8-GGEN-R1)**: the `graph` noun survives,
//! but only the `validate` sub-verb remains — `load`/`query`/`export`/
//! `snapshot`/`stats`/`diff` were removed from the CLI surface (the
//! capability retreated into the ggen-graph lib). Tests pinning removed
//! verbs were DELETED (loud absence registered in
//! docs/jira/v26.11.8/receipts/ggen-e2e-repair-survey.md); the surviving
//! surface (`graph validate`, help) is re-pinned here.

use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;
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

/// Helper to create a valid test RDF (Turtle) fixture
fn create_test_rdf(temp_dir: &TempDir) -> std::path::PathBuf {
    let rdf_file = temp_dir.path().join("test.ttl");

    let rdf_content = r#"
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:Person a rdfs:Class ;
    rdfs:label "Person" ;
    rdfs:comment "A human being" .

ex:name a rdf:Property ;
    rdfs:domain ex:Person ;
    rdfs:range rdfs:Literal ;
    rdfs:label "name" .

ex:age a rdf:Property ;
    rdfs:domain ex:Person ;
    rdfs:range rdfs:Literal ;
    rdfs:label "age" .

ex:john a ex:Person ;
    ex:name "John Doe" ;
    ex:age "30" .

ex:jane a ex:Person ;
    ex:name "Jane Smith" ;
    ex:age "28" .
"#;

    fs::write(&rdf_file, rdf_content).expect("Failed to write RDF file");
    rdf_file
}

/// Helper to create an invalid Turtle fixture
fn create_invalid_rdf(temp_dir: &TempDir) -> std::path::PathBuf {
    let bad_file = temp_dir.path().join("invalid.ttl");
    fs::write(&bad_file, "not valid RDF @!#$%").expect("Failed to write fixture");
    bad_file
}

#[test]
fn test_graph_validate_structure() {
    // Chicago TDD: REAL validation of a valid Turtle ontology via `graph validate`.
    // Re-pinned 2026-11-08: `graph load` removed; validate now takes `--files`.
    let temp_dir = TempDir::new().unwrap();
    let rdf_file = create_test_rdf(&temp_dir);

    ggen()
        .arg("graph")
        .arg("validate")
        .arg("--files")
        .arg(rdf_file.to_str().unwrap())
        .current_dir(&temp_dir)
        .assert()
        .success();
}

#[test]
fn test_graph_validate_reports_violations() {
    // Chicago TDD: REAL error state for invalid RDF — validate exits non-zero
    // and names the parse failure. Re-pinned 2026-11-08 from the removed
    // `graph load` error-path tests (witnessed rc=1 + "invalid" on stderr).
    let temp_dir = TempDir::new().unwrap();
    let bad_file = create_invalid_rdf(&temp_dir);

    ggen()
        .arg("graph")
        .arg("validate")
        .arg("--files")
        .arg(bad_file.to_str().unwrap())
        .current_dir(&temp_dir)
        .assert()
        .failure()
        .stderr(
            predicate::str::contains("invalid")
                .or(predicate::str::contains("Parser error"))
                .or(predicate::str::contains("error")),
        );
}

#[test]
fn test_graph_validate_missing_file() {
    // Chicago TDD: error state for a nonexistent file argument.
    let temp_dir = TempDir::new().unwrap();

    ggen()
        .arg("graph")
        .arg("validate")
        .arg("--files")
        .arg("/nonexistent/file.ttl")
        .current_dir(&temp_dir)
        .assert()
        .failure()
        .stderr(
            predicate::str::contains("not found")
                .or(predicate::str::contains("invalid"))
                .or(predicate::str::contains("error")),
        );
}

#[test]
fn test_graph_help_output() {
    // Chicago TDD: help reflects the CURRENT surface (validate only).
    // Re-pinned 2026-11-08: load/query/export verbs removed from the noun.
    ggen()
        .arg("graph")
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("Validate"))
        .stdout(predicate::str::contains("validate"))
        .stdout(predicate::str::contains("Usage"));
}

#[test]
fn test_graph_validate_help() {
    // Chicago TDD: verb-specific help for the surviving verb.
    // Re-pinned 2026-11-08 from `graph query --help` (query verb removed).
    ggen()
        .arg("graph")
        .arg("validate")
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("Usage"))
        .stdout(predicate::str::contains("validate"));
}

#[test]
fn test_graph_invalid_verb() {
    // Chicago TDD: unknown sub-verb under the surviving noun still errors.
    ggen()
        .arg("graph")
        .arg("invalid-verb")
        .assert()
        .failure()
        .stderr(
            predicate::str::contains("error")
                .or(predicate::str::contains("invalid"))
                .or(predicate::str::contains("unrecognized")),
        );
}
