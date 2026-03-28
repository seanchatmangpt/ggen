//! Chicago-TDD tests for the `sync` orchestrator
//!
//! Each test follows the Red-Green-Refactor cycle and is:
//! - **Fast**: no external processes, no network, no real file-system writes
//!   (dry_run or temp directories)
//! - **Independent**: each test sets up its own fixtures
//! - **Repeatable**: fully deterministic — no random data
//! - **Self-Checking**: explicit `assert!` / `assert_eq!` on the observable claim
//! - **Timely**: written alongside the implementation

use super::{sync, SyncConfig, SyncLanguage};
use std::fs;
use tempfile::TempDir;

// ---------------------------------------------------------------------------
// Minimal Turtle ontology used in tests
// ---------------------------------------------------------------------------

/// A minimal valid Turtle document defining one Service.
const MINIMAL_TTL: &str = r#"
@prefix bos: <https://chatmangpt.com/businessos#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@base <https://chatmangpt.com/businessos/> .

<OrderService> a bos:Service ;
    rdfs:label "Order API" ;
    bos:port 8001 ;
    bos:language "Go" .
"#;

/// A SPARQL SELECT that extracts service names.
const SERVICES_QUERY: &str = r#"
PREFIX bos: <https://chatmangpt.com/businessos#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?service WHERE {
  ?s a bos:Service .
  BIND(REPLACE(STR(?s), ".*[/#]", "") AS ?service)
}
"#;

// ---------------------------------------------------------------------------
// Helper: create a temp directory with ontology + query files
// ---------------------------------------------------------------------------

fn setup_fixture(
    ttl: &str, queries: &[(&str, &str)],
) -> (TempDir, std::path::PathBuf, std::path::PathBuf) {
    let dir = TempDir::new().expect("temp dir");
    let ont_path = dir.path().join("ontology.ttl");
    let queries_dir = dir.path().join("queries");
    fs::write(&ont_path, ttl).expect("write ttl");
    fs::create_dir_all(&queries_dir).expect("queries dir");
    for (name, rq) in queries {
        fs::write(queries_dir.join(format!("{}.rq", name)), rq).expect("write rq");
    }
    (dir, ont_path, queries_dir)
}

// ---------------------------------------------------------------------------
// Test 1: dry_run produces no files on disk
// ---------------------------------------------------------------------------

/// RED claim: `sync()` with `dry_run: true` must not write any files to disk,
/// yet must still return the paths that *would* have been written.
#[test]
fn test_sync_dry_run_produces_no_files() {
    let (_dir, ont_path, queries_dir) = setup_fixture(MINIMAL_TTL, &[("services", SERVICES_QUERY)]);

    let output_dir = _dir.path().join("output");

    let config = SyncConfig {
        ontology_path: ont_path,
        queries_dir,
        output_dir: output_dir.clone(),
        language: SyncLanguage::Go,
        validate: false,
        dry_run: true,
    };

    let result = sync(config).expect("sync must succeed in dry-run mode");

    // Claim: the pipeline ran and identified at least one file to generate
    assert!(
        !result.files_generated.is_empty(),
        "dry_run should still report files that would be generated"
    );

    // Claim: no file was actually written — the output directory must not exist
    assert!(
        !output_dir.exists(),
        "dry_run must not create the output directory on disk"
    );

    // Claim: every reported path lives under the (non-existent) output_dir
    for path in &result.files_generated {
        assert!(
            path.starts_with(&output_dir),
            "reported path {:?} should be under output_dir",
            path
        );
    }
}

// ---------------------------------------------------------------------------
// Test 2: Go generation produces a service struct
// ---------------------------------------------------------------------------

/// RED claim: after `sync()` with `language: Go`, the generated `.go` file
/// must contain a Go `struct` definition.
#[test]
fn test_sync_go_generates_service_struct() {
    let (_dir, ont_path, queries_dir) = setup_fixture(MINIMAL_TTL, &[("services", SERVICES_QUERY)]);

    let output_dir = _dir.path().join("output");

    let config = SyncConfig {
        ontology_path: ont_path,
        queries_dir,
        output_dir: output_dir.clone(),
        language: SyncLanguage::Go,
        validate: false,
        dry_run: false,
    };

    let result = sync(config).expect("sync must succeed");

    // Claim: at least one file was generated
    assert!(
        !result.files_generated.is_empty(),
        "sync must produce at least one Go file"
    );

    // Claim: at least one generated file has `.go` extension
    let go_files: Vec<_> = result
        .files_generated
        .iter()
        .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("go"))
        .collect();
    assert!(!go_files.is_empty(), "expected at least one .go file");

    // Claim: the first Go file contains a `struct` keyword — proving a struct was generated
    let content = fs::read_to_string(&go_files[0]).expect("read generated go file");
    assert!(
        content.contains("struct"),
        "generated Go file should contain a struct definition, got:\n{}",
        content
    );
}

// ---------------------------------------------------------------------------
// Test 3: receipt is deterministic across two identical invocations
// ---------------------------------------------------------------------------

/// RED claim: two `sync()` calls with identical inputs must produce the
/// identical receipt string (sha256 is a pure function of the inputs).
#[test]
fn test_sync_receipt_is_deterministic() {
    // Run #1
    let (_dir1, ont1, qdir1) = setup_fixture(MINIMAL_TTL, &[("services", SERVICES_QUERY)]);
    let out1 = _dir1.path().join("out1");

    let receipt1 = sync(SyncConfig {
        ontology_path: ont1,
        queries_dir: qdir1,
        output_dir: out1,
        language: SyncLanguage::Go,
        validate: false,
        dry_run: false,
    })
    .expect("first sync")
    .receipt;

    // Run #2 — identical inputs in a fresh temp directory
    let (_dir2, ont2, qdir2) = setup_fixture(MINIMAL_TTL, &[("services", SERVICES_QUERY)]);
    let out2 = _dir2.path().join("out2");

    let receipt2 = sync(SyncConfig {
        ontology_path: ont2,
        queries_dir: qdir2,
        output_dir: out2,
        language: SyncLanguage::Go,
        validate: false,
        dry_run: false,
    })
    .expect("second sync")
    .receipt;

    // Claim: same content always produces the same receipt
    assert_eq!(
        receipt1, receipt2,
        "receipt must be deterministic for identical inputs"
    );

    // Claim: receipt is a non-empty hex string (sha256 = 64 hex chars)
    assert_eq!(
        receipt1.len(),
        64,
        "sha256 receipt must be 64 hex characters"
    );
    assert!(
        receipt1.chars().all(|c| c.is_ascii_hexdigit()),
        "receipt must be a valid hex string"
    );
}
