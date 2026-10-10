//! Court for build-time provenance (build.rs + `build_provenance` module +
//! the Stage-5 receipt's `build_provenance` payload field).
//!
//! Chicago: real git build state via env-embedded consts, a real sync
//! pipeline over a real `TempDir` fixture (pattern reused from
//! `sync_closure_golden_test.rs`), and — when the workspace `ggen` binary
//! is present — a real subprocess `ggen --version --verbose`. No mocks.
//!
//! Fail-soft honesty: in a git build (this checkout) the commit sha is a
//! real 40-hex SHA, never "unknown"; in a non-git build (crates.io) the
//! constants must instead record `source: crates.io` — the court asserts
//! the pairing, not a particular source.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::path::Path;

use ggen_engine::build_provenance::{
    describe, BUILD_COMMIT_SHA, BUILD_ONTOLOGY_TRIPLES, BUILD_PROVENANCE_BLAKE3,
    BUILD_PROVENANCE_HEX, BUILD_SOURCE, BUILD_STRATA_PACKS,
};
use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

fn is_git_build() -> bool {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .ancestors()
        .nth(2)
        .expect("manifest sits at <repo>/crates/ggen-engine")
        .join(".git")
        .exists()
}

/// The embedded digest is present and non-zero in a git build, and is a
/// well-formed 64-char lowercase hex BLAKE3 in every build.
#[test]
fn provenance_digest_present_nonzero_and_hex() {
    assert_eq!(BUILD_PROVENANCE_HEX.len(), 64);
    assert!(BUILD_PROVENANCE_HEX.chars().all(|c| c.is_ascii_hexdigit() && !c.is_ascii_uppercase()));
    assert_ne!(BUILD_PROVENANCE_BLAKE3, [0u8; 32], "all-zero digest means build.rs hashing failed");
}

/// Source/sha honesty pairing: git build ⇒ 40-hex real SHA; non-git build
/// ⇒ "unknown" + "crates.io". Never a faked SHA over a non-git source.
#[test]
fn source_and_sha_pairing_is_honest() {
    if is_git_build() {
        assert_eq!(BUILD_SOURCE, "git", "git checkout must record source: git");
        assert_eq!(BUILD_COMMIT_SHA.len(), 40);
        assert!(BUILD_COMMIT_SHA.chars().all(|c| c.is_ascii_hexdigit()));
        assert_ne!(BUILD_COMMIT_SHA, "unknown");
    } else {
        assert_eq!(BUILD_SOURCE, "crates.io");
        assert_eq!(BUILD_COMMIT_SHA, "unknown");
    }
}

/// The 6 canonical strata packs are always named, each with a 64-hex
/// digest (real BLAKE3 or the documented zero placeholder).
#[test]
fn strata_pack_bindings_well_formed() {
    let mut names: Vec<&str> = BUILD_STRATA_PACKS.split(';').map(|b| b.split('=').next().unwrap()).collect();
    names.sort_unstable();
    assert_eq!(names, ["strata-cas-pack", "strata-protocol-pack", "strata-signer-pack", "strata-stratus-pack", "strata-temprun-pack", "strata-valve-pack"]);
    for binding in BUILD_STRATA_PACKS.split(';') {
        let hex = binding.split('=').nth(1).expect("binding has =");
        assert_eq!(hex.len(), 64);
        assert!(hex.chars().all(|c| c.is_ascii_hexdigit()));
    }
}

/// The verbose block carries every component, and the `ggen --version
/// --verbose` CLI surface exits 0 and prints it — via the real binary
/// when the workspace build has produced one.
#[test]
fn verbose_surface_prints_provenance() {
    let block = describe();
    for needle in [
        BUILD_COMMIT_SHA,
        BUILD_PROVENANCE_HEX,
        BUILD_STRATA_PACKS,
        &format!("ontology_triples: {BUILD_ONTOLOGY_TRIPLES}"),
        &format!("source: {BUILD_SOURCE}"),
    ] {
        assert!(block.contains(needle), "verbose block missing {needle:?}");
    }

    // Real subprocess against the workspace `ggen` binary when present
    // (built by `cargo test`'s workspace compile in a canonical checkout).
    let bin = Path::new(env!("CARGO_MANIFEST_DIR"))
        .ancestors()
        .nth(2)
        .map(|root| root.join("target/debug/ggen"))
        .filter(|p| p.exists());
    if let Some(bin) = bin {
        let out = std::process::Command::new(&bin).args(["--version", "--verbose"]).output().expect("spawn ggen");
        assert!(out.status.success(), "ggen --version --verbose exited {:?}", out.status);
        let stdout = String::from_utf8_lossy(&out.stdout);
        assert!(stdout.contains(BUILD_PROVENANCE_HEX), "binary stdout missing provenance hex");
    }
}

const GGEN_TOML: &str = r#"
[project]
name = "provenance-court"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

const ONTOLOGY: &str = r#"
@prefix ex: <http://example.org/> .
ex:rex a ex:Dog ; ex:name "Rex" .
"#;

const TEMPLATE: &str = "---\nto: out.txt\n---\ndog: rex\n";

/// After a real sync, the Stage-5 receipt payload carries the
/// `build_provenance` field equal to the embedded digest hex.
#[test]
fn receipt_carries_build_provenance_after_real_sync() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY).expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::write(root.join("templates/dog.tmpl"), TEMPLATE).expect("write template");

    sync(root, SyncOptions::default()).expect("sync");

    let raw = std::fs::read_to_string(root.join(".ggen-v2/receipt.json")).expect("read receipt");
    let json: serde_json::Value = serde_json::from_str(&raw).expect("parse receipt");
    let provenance = json["payload"]["build_provenance"].as_str().expect("build_provenance field present");
    assert_eq!(provenance, BUILD_PROVENANCE_HEX);
}
