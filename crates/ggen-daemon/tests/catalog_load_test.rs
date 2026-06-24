//! Chicago TDD integration tests for `catalog::load_catalog` and
//! `catalog::filter_by_language`.
//!
//! These tests use real file I/O against either the project's checked-in
//! `.specify/specs/repos-catalog.ttl` or small in-test TTL fixtures written
//! to a `TempDir`.  No mocks or test doubles are used.

use std::path::PathBuf;
use tempfile::TempDir;

use ggen_daemon::catalog::{filter_by_language, load_catalog};

// ---------------------------------------------------------------------------
// Helper: locate the ggen workspace root by walking up from cwd until a
// directory with `.specify/` is found.
// ---------------------------------------------------------------------------
fn ggen_root() -> PathBuf {
    let cwd = std::env::current_dir().expect("current_dir must be readable");
    cwd.ancestors()
        .find(|p| p.join(".specify").exists())
        .map(|p| p.to_path_buf())
        .unwrap_or(cwd)
}

fn real_catalog_path() -> PathBuf {
    ggen_root().join(".specify/specs/repos-catalog.ttl")
}

// ---------------------------------------------------------------------------
// A minimal TTL fixture that includes `rdfs:label` triples so that
// `filter_by_language` can match.  The real repos-catalog.ttl uses bare named
// nodes (e.g. `ggen:Rust`) with no `rdfs:label`, so language filtering must
// be tested with this synthetic fixture.
// ---------------------------------------------------------------------------
const FIXTURE_TTL: &str = r#"
@prefix doap: <http://usefulinc.com/ns/doap#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ggen: <https://ggen.dev/ontology/core#> .
@prefix repo: <https://github.com/example/> .

ggen:Rust   rdfs:label "Rust" .
ggen:Python rdfs:label "Python" .
ggen:Go     rdfs:label "Go" .

repo:alpha a doap:Project ;
    doap:name "alpha" ;
    doap:homepage <https://github.com/example/alpha> ;
    doap:shortdesc "Alpha project" ;
    ggen:primaryLanguage ggen:Rust .

repo:beta a doap:Project ;
    doap:name "beta" ;
    doap:homepage <https://github.com/example/beta> ;
    doap:shortdesc "Beta project" ;
    ggen:primaryLanguage ggen:Python .

repo:gamma a doap:Project ;
    doap:name "gamma" ;
    doap:homepage <https://github.com/example/gamma> ;
    doap:shortdesc "Gamma project" ;
    ggen:primaryLanguage ggen:Rust .

repo:delta a doap:Project ;
    doap:name "delta" ;
    doap:homepage <https://github.com/example/delta> ;
    doap:shortdesc "Delta project" ;
    ggen:primaryLanguage ggen:Go .
"#;

fn write_fixture(dir: &TempDir) -> PathBuf {
    let path = dir.path().join("fixture.ttl");
    std::fs::write(&path, FIXTURE_TTL).expect("fixture write must succeed");
    path
}

// ---------------------------------------------------------------------------
// Tests against the real repos-catalog.ttl
// ---------------------------------------------------------------------------

#[test]
fn real_catalog_has_at_least_100_entries() {
    let path = real_catalog_path();
    if !path.exists() {
        eprintln!("SKIP: repos-catalog.ttl not found at {}", path.display());
        return;
    }

    let entries = load_catalog(&path)
        .expect("load_catalog must not fail on a valid TTL");

    assert!(
        entries.len() >= 100,
        "expected >= 100 catalog entries, got {}",
        entries.len()
    );
}

#[test]
fn every_real_catalog_entry_has_non_empty_name_and_github_url() {
    let path = real_catalog_path();
    if !path.exists() {
        eprintln!("SKIP: repos-catalog.ttl not found at {}", path.display());
        return;
    }

    let entries = load_catalog(&path)
        .expect("load_catalog must not fail on a valid TTL");

    assert!(
        !entries.is_empty(),
        "catalog must contain at least one entry"
    );

    for entry in &entries {
        assert!(
            !entry.name.is_empty(),
            "entry name must not be empty: {:?}",
            entry
        );
        assert!(
            !entry.github_url.is_empty(),
            "entry github_url must not be empty for repo '{}'",
            entry.name
        );
        // Catalog entries may have http:// or https:// homepages; the fallback
        // is always https://github.com/... — accept both schemes.
        assert!(
            entry.github_url.starts_with("http://") || entry.github_url.starts_with("https://"),
            "github_url must be an http(s) URL, got '{}' for repo '{}'",
            entry.github_url,
            entry.name
        );
    }
}

#[test]
fn load_catalog_on_missing_file_returns_error_not_panic() {
    let path = PathBuf::from("/tmp/this-file-does-not-exist-ggen-daemon-test.ttl");
    assert!(!path.exists(), "test precondition: file must not exist");

    let result = load_catalog(&path);
    assert!(
        result.is_err(),
        "load_catalog on a missing file must return Err, not Ok"
    );
}

// ---------------------------------------------------------------------------
// Tests against the synthetic TTL fixture (required for language filtering)
// ---------------------------------------------------------------------------

#[test]
fn fixture_catalog_loads_correct_entry_count() {
    let dir = TempDir::new().unwrap();
    let path = write_fixture(&dir);

    let entries = load_catalog(&path).expect("fixture load must succeed");
    assert_eq!(
        entries.len(),
        4,
        "fixture has 4 projects; got {}",
        entries.len()
    );
}

#[test]
fn fixture_entries_have_populated_name_and_url() {
    let dir = TempDir::new().unwrap();
    let path = write_fixture(&dir);

    let entries = load_catalog(&path).expect("fixture load must succeed");
    for entry in &entries {
        assert!(!entry.name.is_empty(), "name must not be empty");
        assert!(
            !entry.github_url.is_empty(),
            "github_url must not be empty for '{}'",
            entry.name
        );
    }
}

#[test]
fn fixture_entries_carry_primary_language_from_rdfs_label() {
    let dir = TempDir::new().unwrap();
    let path = write_fixture(&dir);

    let entries = load_catalog(&path).expect("fixture load must succeed");

    // All entries in the fixture have a language node with an rdfs:label, so
    // primary_language must be Some(_) for every row.
    for entry in &entries {
        assert!(
            entry.primary_language.is_some(),
            "primary_language must be Some for '{}'; the fixture TTL defines rdfs:label for all language nodes",
            entry.name
        );
    }
}

#[test]
fn filter_by_language_returns_only_rust_entries() {
    let dir = TempDir::new().unwrap();
    let path = write_fixture(&dir);
    let entries = load_catalog(&path).expect("fixture load must succeed");

    let rust = filter_by_language(&entries, "Rust");

    assert_eq!(
        rust.len(),
        2,
        "fixture has 2 Rust repos (alpha, gamma); got {}",
        rust.len()
    );
    let names: Vec<&str> = rust.iter().map(|e| e.name.as_str()).collect();
    assert!(names.contains(&"alpha"), "alpha must be in Rust results");
    assert!(names.contains(&"gamma"), "gamma must be in Rust results");
}

#[test]
fn filter_by_language_is_case_insensitive() {
    let dir = TempDir::new().unwrap();
    let path = write_fixture(&dir);
    let entries = load_catalog(&path).expect("fixture load must succeed");

    let lower = filter_by_language(&entries, "rust");
    let upper = filter_by_language(&entries, "RUST");
    let mixed = filter_by_language(&entries, "Rust");

    assert_eq!(
        lower.len(),
        mixed.len(),
        "lowercase 'rust' and 'Rust' must return same count"
    );
    assert_eq!(
        upper.len(),
        mixed.len(),
        "uppercase 'RUST' and 'Rust' must return same count"
    );
}

#[test]
fn filter_by_language_returns_empty_for_unknown_language() {
    let dir = TempDir::new().unwrap();
    let path = write_fixture(&dir);
    let entries = load_catalog(&path).expect("fixture load must succeed");

    let results = filter_by_language(&entries, "COBOL");
    assert!(
        results.is_empty(),
        "filter for 'COBOL' must return empty slice; got {}",
        results.len()
    );
}

#[test]
fn filter_by_language_on_empty_slice_returns_empty() {
    let results = filter_by_language(&[], "Rust");
    assert!(results.is_empty(), "empty input must produce empty output");
}

#[test]
fn load_catalog_on_invalid_ttl_returns_error() {
    let dir = TempDir::new().unwrap();
    let bad = dir.path().join("bad.ttl");
    std::fs::write(&bad, "this is not valid turtle syntax !!! @@@").unwrap();

    let result = load_catalog(&bad);
    assert!(
        result.is_err(),
        "load_catalog on malformed TTL must return Err"
    );
}
