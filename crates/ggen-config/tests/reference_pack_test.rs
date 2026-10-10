//! REFERENCE fixture test: parses the committed reference_pack fixture
//! through the real ManifestParser and asserts the v26.10.10 §4.1
//! `[rules]`/`[pack_sources]` sections behave as documented in the
//! fixture's README (including the deny_unknown_fields guard).

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use ggen_config::manifest::{GgenManifest, ManifestParser};
use std::path::PathBuf;

const FIXTURE_DIR: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/reference_pack");
const FIXTURE_MANIFEST: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/tests/fixtures/reference_pack/ggen.toml"
);

#[test]
fn reference_fixture_parses_with_expected_rules_and_pack_sources() {
    let raw = std::fs::read_to_string(FIXTURE_MANIFEST).expect("fixture manifest readable");
    let manifest: GgenManifest = ManifestParser::parse_str(&raw).expect("fixture parses");

    let rules = manifest.rules.as_ref().expect("[rules] present");
    assert_eq!(rules.n3, vec![PathBuf::from("rules.n3")]);
    assert_eq!(rules.datalog, vec![PathBuf::from("rules.datalog")]);

    let sources = manifest
        .pack_sources
        .as_ref()
        .expect("[pack_sources] present");
    assert_eq!(sources.len(), 2);
    let core = sources.get("core").expect("core binding");
    assert!(matches!(
        core.source,
        ggen_config::manifest::PackSourceKind::Path
    ));
    assert_eq!(core.location, "../core-pack");
    let remote = sources.get("remote").expect("remote binding");
    assert!(matches!(
        remote.source,
        ggen_config::manifest::PackSourceKind::Git
    ));
    assert_eq!(remote.location, "https://example.com/pack.git");

    assert_eq!(manifest.project.name, "reference-pack");
    assert_eq!(manifest.ontology.source, PathBuf::from("ontology.ttl"));
}

#[test]
fn reference_fixture_round_trips_stably() {
    let raw = std::fs::read_to_string(FIXTURE_MANIFEST).expect("fixture readable");
    let manifest: GgenManifest = ManifestParser::parse_str(&raw).expect("parses");
    let reserialized = toml::to_string(&manifest).expect("serializes");
    let reparsed: GgenManifest = ManifestParser::parse_str(&reserialized).expect("re-parses");
    assert_eq!(reparsed.rules, manifest.rules);
    assert_eq!(reparsed.pack_sources, manifest.pack_sources);
}

#[test]
fn mutated_fixture_copy_with_unknown_rules_key_is_refused() {
    let mut tmp = std::env::temp_dir();
    tmp.push(format!("ref_pack_test_{}", std::process::id()));
    std::fs::create_dir_all(&tmp).expect("tmpdir");
    let mutated = tmp.join("ggen.toml");
    let raw = std::fs::read_to_string(FIXTURE_MANIFEST).expect("fixture readable");
    let tampered = raw.replace("n3 = [\"rules.n3\"]", "n3 = [\"rules.n3\"]\nbogus_key = 1");
    assert_ne!(tampered, raw, "mutation must apply");
    std::fs::write(&mutated, tampered).expect("write mutated copy");

    let text = std::fs::read_to_string(&mutated).expect("read back");
    let err = ManifestParser::parse_str(&text).expect_err("unknown key must refuse");
    assert!(
        err.to_string().to_lowercase().contains("bogus_key"),
        "refusal must name the unknown field, got: {err}"
    );
    std::fs::remove_file(&mutated).ok();
}

#[test]
fn fixture_rule_and_ontology_files_exist() {
    for rel in ["ontology.ttl", "rules.n3", "rules.datalog", "README.md"] {
        let p = PathBuf::from(FIXTURE_DIR).join(rel);
        assert!(p.is_file(), "fixture file missing: {}", p.display());
    }
}

// Step 4: ontology validity is checked out-of-band (ggen binary if present).
#[test]
fn ontology_parses_if_ggen_binary_available() {
    let ggen = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../target/debug/ggen");
    if !ggen.is_file() {
        eprintln!("SKIP: no built ggen binary at {}", ggen.display());
        return;
    }
    let out = std::process::Command::new(&ggen)
        .args([
            "graph",
            "validate",
            "--files",
            &format!("{FIXTURE_DIR}/ontology.ttl"),
        ])
        .output()
        .expect("spawn ggen");
    assert!(
        out.status.success(),
        "ontology.ttl must validate: {}",
        String::from_utf8_lossy(&out.stderr)
    );
}
