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
//! End-to-end integration tests for ontology commands (R2 repair).
//!
//! The ontology verb surface moved (2026-11-08 R2 repair):
//! - `ontology validate <file>` -> `graph validate --files <file>`
//! - `ontology generate <file> --language X` -> `ggen init` scaffold + `sync run`
//!   (code generation is now manifest-driven, not one-shot file->language)
//! - `ontology init` -> root `ggen init`
//!
//! Deleted with no equivalent: `--strict`, `--language`, `--output`, `--zod`,
//! `--utilities` flags (removed with the old verbs).
//!
//! **Chicago TDD Principles**:
//! - REAL CLI process execution
//! - REAL file system operations for ontology files
//! - REAL state verification of generated artifacts
//! - NO mocking of ontology processing

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

/// Helper to create test ontology file
fn create_test_ontology(temp_dir: &TempDir) -> std::path::PathBuf {
    let ontology_file = temp_dir.path().join("test.ttl");

    let ontology_content = r#"
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .

ex:Person a owl:Class ;
    rdfs:label "Person" ;
    rdfs:comment "A human being" .

ex:name a rdf:Property ;
    rdfs:domain ex:Person ;
    rdfs:range rdfs:Literal ;
    rdfs:label "name" .
"#;

    fs::write(&ontology_file, ontology_content).expect("Failed to write ontology file");
    ontology_file
}

/// Helper: scaffold a real ggen project. The scaffold's example rule now
/// carries ORDER BY, so default strict_mode needs no relaxation.
fn init_project(temp_dir: &TempDir) {
    ggen()
        .arg("init")
        .current_dir(temp_dir.path())
        .assert()
        .success();
}

// ---------------------------------------------------------------------------
// Current ontology noun surface (lock/info/install/namespaces/search/list/status)
// ---------------------------------------------------------------------------

#[test]
fn test_ontology_help_shows_verbs() {
    // Chicago TDD: Verify help state matches the current verb surface
    ggen()
        .arg("ontology")
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("lock"))
        .stdout(predicate::str::contains("info"))
        .stdout(predicate::str::contains("install"))
        .stdout(predicate::str::contains("namespaces"));
}

#[test]
fn test_ontology_invalid_verb() {
    // Chicago TDD: Verify error handling for invalid verbs
    ggen()
        .arg("ontology")
        .arg("invalid-verb")
        .assert()
        .failure()
        .stderr(predicate::str::contains("error").or(predicate::str::contains("invalid")));
}

// ---------------------------------------------------------------------------
// validate -> `graph validate --files`
// ---------------------------------------------------------------------------

#[test]
fn test_ontology_validate_passes() {
    // Re-pinned: validation moved to `graph validate --files`
    let temp_dir = TempDir::new().unwrap();
    let ontology_file = create_test_ontology(&temp_dir);

    ggen()
        .arg("graph")
        .arg("validate")
        .arg("--files")
        .arg(ontology_file.to_str().unwrap())
        .current_dir(&temp_dir)
        .assert()
        .success();
}

#[test]
fn test_ontology_validate_missing_file() {
    // Re-pinned: validation moved to `graph validate --files`
    let temp_dir = TempDir::new().unwrap();

    ggen()
        .arg("graph")
        .arg("validate")
        .arg("--files")
        .arg("/nonexistent/file.ttl")
        .current_dir(&temp_dir)
        .assert()
        .failure();
}

// ---------------------------------------------------------------------------
// init -> root `ggen init`
// ---------------------------------------------------------------------------

#[test]
fn test_ontology_init_creates_project() {
    // Re-pinned: project init moved to the root `init` verb
    let temp_dir = TempDir::new().unwrap();

    ggen().arg("init").current_dir(&temp_dir).assert().success();

    // Verify state: manifest + ontology scaffold created
    assert!(
        temp_dir.path().join("ggen.toml").exists(),
        "ggen.toml should be created"
    );
    assert!(
        temp_dir.path().join("schema/domain.ttl").exists(),
        "schema/domain.ttl should be created"
    );
}

// ---------------------------------------------------------------------------
// generate -> manifest-driven `sync run`
// ---------------------------------------------------------------------------

#[test]
fn test_ontology_generate_creates_code() {
    // Re-pinned: generation moved to `sync run` over a ggen.toml manifest
    let temp_dir = TempDir::new().unwrap();
    init_project(&temp_dir);

    ggen()
        .arg("sync")
        .arg("run")
        .arg("--skip-ea-admission")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Verify state: rule output file rendered on disk
    assert!(
        temp_dir.path().join("ontology-summary.txt").exists(),
        "sync run should render ontology-summary.txt from the scaffold rule"
    );
}

#[test]
fn test_ontology_generate_missing_file() {
    // Re-pinned: generation now requires a ggen.toml manifest; without one,
    // `sync run` must fail loudly.
    let temp_dir = TempDir::new().unwrap();

    ggen()
        .arg("sync")
        .arg("run")
        .current_dir(&temp_dir)
        .assert()
        .failure();
}
