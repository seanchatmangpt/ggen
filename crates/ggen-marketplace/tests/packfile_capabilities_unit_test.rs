//! Unit-fixture tests for `PackFile.capabilities` refusal/edge paths.
//!
//! Lane: packfile-tests. Complements `packfile_capabilities_test.rs` (happy
//! paths + corpus) with the adversarial edges: unknown keys, wrong-typed
//! entries, empty arrays, absent table, duplicate URNs.
//!
//! Chicago discipline: real TempDir fixtures on real disk parsed by the real
//! `star_toml` parser — the same parser `metadata.rs::load_pack_metadata`
//! uses. No mocks, no doubles.


#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)] // Chicago TDD: real-IO tests
use ggen_marketplace::packs_registry::types::PackFile;
use std::fs;
use tempfile::TempDir;

/// Write a pack.toml fixture to a real TempDir and parse it with the real
/// parser (same `star_toml::from_str::<PackFile>` call as metadata.rs).
fn parse_fixture(toml_body: &str) -> Result<PackFile, String> {
    let dir = TempDir::new().expect("tempdir");
    let path = dir.path().join("pack.toml");
    fs::write(&path, toml_body).expect("write fixture");
    let content = fs::read_to_string(&path).expect("read fixture back");
    star_toml::from_str(&content).map_err(|e| format!("parse refused: {e}"))
}

const MINIMAL: &str = r#"
[pack]
id = "acme/caps-edge"
name = "caps-edge"
version = "1.0.0"
description = "edge fixture"
category = "test"
packages = []
"#;

// (a) Unknown key inside [capabilities]
//
// PARITY (lane caps-strict-parity): `PackCapabilitiesFile` is now
// `#[serde(deny_unknown_fields)]`, matching the engine's
// `ggen-engine::pack::PackCapabilities`. The same TOML is REJECTED in both
// surfaces: a typo'd capability key (e.g. `provide = [...]`) is a parse
// error, never a silent drop into an absent field.
#[test]
fn capabilities_unknown_key_is_refused() {
    let result = parse_fixture(&format!(
        "{MINIMAL}\n[capabilities]\nprovide = [\"urn:ggen:pack:typos\"]\n"
    ));
    let err = result.expect_err("unknown key must be refused (deny_unknown_fields)");
    assert!(
        err.to_lowercase().contains("unknown")
            || err.to_lowercase().contains("provide"),
        "refusal should name the unknown field, got: {err}"
    );
}

// (b) provides with a non-string entry -> typed parse refusal.
#[test]
fn provides_with_non_string_entry_is_refused() {
    let result = parse_fixture(&format!(
        "{MINIMAL}\n[capabilities]\nprovides = [\"urn:ggen:pack:ok\", 42]\n"
    ));
    assert!(result.is_err(), "non-string provides entry must be refused");
}

#[test]
fn requires_with_non_string_entry_is_refused() {
    let result = parse_fixture(&format!("{MINIMAL}\n[capabilities]\nrequires = [true]\n"));
    assert!(result.is_err(), "non-string requires entry must be refused");
}

#[test]
fn types_with_non_string_entry_is_refused() {
    let result = parse_fixture(&format!(
        "{MINIMAL}\n[capabilities]\ntypes = [[\"nested\"]]\n"
    ));
    assert!(result.is_err(), "non-string types entry must be refused");
}

// (b') The refusal carries a parse-error message, not a silent truncation:
// the same fixture without the bad entry must parse, proving the refusal is
// caused by the type error itself.
#[test]
fn refusal_is_caused_by_bad_entry_not_surrounding_shape() {
    let ok = parse_fixture(&format!(
        "{MINIMAL}\n[capabilities]\nprovides = [\"urn:ggen:pack:ok\"]\n"
    ));
    assert!(ok.is_ok(), "control fixture must parse");

    let bad = parse_fixture(&format!(
        "{MINIMAL}\n[capabilities]\nprovides = [\"urn:ggen:pack:ok\", {{ table = true }}]\n"
    ));
    let err = bad.expect_err("inline-table entry must be refused");
    assert!(
        err.contains("parse refused"),
        "error should come from star_toml, got: {err}"
    );
}

// (c) Empty arrays -> Ok with empty sets (Some, not None).
#[test]
fn empty_arrays_parse_ok_as_empty() {
    let parsed = parse_fixture(&format!(
        "{MINIMAL}\n[capabilities]\nprovides = []\nrequires = []\n"
    ))
    .expect("empty arrays must parse");
    let caps = parsed.capabilities.expect("Some");
    assert!(caps.provides.as_ref().expect("provides Some").is_empty());
    assert!(caps.requires.as_ref().expect("requires Some").is_empty());
}

// (d) Absent [capabilities] -> None. (TOML has no null literal; `capabilities
// = null` is invalid TOML and refused by the parser itself — covered by the
// invalid-toml case below.)
#[test]
fn absent_capabilities_is_none() {
    let parsed = parse_fixture(MINIMAL).expect("must parse");
    assert!(parsed.capabilities.is_none());
}

#[test]
fn empty_capabilities_table_is_some_default() {
    let parsed = parse_fixture(&format!("{MINIMAL}\n[capabilities]\n")).expect("must parse");
    let caps = parsed.capabilities.expect("empty table is Some, not None");
    assert!(caps.types.is_none());
    assert!(caps.provides.is_none());
    assert!(caps.requires.is_none());
}

#[test]
fn invalid_toml_null_capabilities_is_refused() {
    // TOML has no null; the closest authoring mistake is refused at the TOML
    // layer, before serde ever sees it.
    assert!(parse_fixture(&format!("{MINIMAL}\ncapabilities = null\n")).is_err());
}

// (e) Duplicate URN within one provides array.
//
// PARITY (lane caps-btree-parity): `PackCapabilitiesFile` now uses
// `BTreeSet<String>`, matching the engine's `PackCapabilities` set semantics.
// Duplicates parse without refusal and DEDUP silently to one element — the
// same observed behavior on both surfaces. No divergence remains.
#[test]
fn duplicate_urn_in_provides_dedup_like_engine_set() {
    let parsed = parse_fixture(&format!(
        "{MINIMAL}\n[capabilities]\nprovides = [\"urn:ggen:pack:dup\", \"urn:ggen:pack:dup\"]\n"
    ))
    .expect("duplicates parse without refusal");
    let caps = parsed.capabilities.expect("Some");
    let provides = caps.provides.expect("provides Some");
    assert_eq!(
        provides.len(),
        1,
        "BTreeSet dedups (engine parity): one entry, not two"
    );
    assert!(provides.contains("urn:ggen:pack:dup"));
}
