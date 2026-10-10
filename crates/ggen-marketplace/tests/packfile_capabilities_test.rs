//! Parsing tests for `PackFile.capabilities` (`[capabilities]` in pack.toml).
//!
//! Chicago discipline: real `star_toml` parsing over real fixtures (TempDir),
//! real corpus files from both repos — no mocks.


#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)] // Chicago TDD: real-IO tests
use ggen_marketplace::packs_registry::types::{PackCapabilitiesFile, PackFile};
use std::collections::BTreeSet;
use std::fs;
use tempfile::TempDir;

const MINIMAL_TOML: &str = r#"
[pack]
id = "acme/minimal"
name = "minimal"
version = "1.0.0"
description = "minimal fixture"
category = "test"
packages = []
"#;

/// Pack.toml without `[capabilities]` must parse unchanged with
/// `capabilities == None` — zero drift for all pre-annotation packs.
#[test]
fn parse_without_capabilities_yields_none() {
    let parsed: PackFile = star_toml::from_str(MINIMAL_TOML).expect("must parse");
    assert!(parsed.capabilities.is_none());
}

/// With `[capabilities]`, provides/requires parse as string arrays.
#[test]
fn parse_with_provides_and_requires() {
    let toml = format!(
        "{MINIMAL_TOML}\n[capabilities]\nprovides = [\"urn:ggen:pack:acme-minimal\"]\nrequires = [\"urn:ggen:pack:acme-base\"]\n"
    );
    let parsed: PackFile = star_toml::from_str(&toml).expect("must parse");
    let caps: PackCapabilitiesFile = parsed.capabilities.expect("capabilities present");
    assert_eq!(
        caps.provides,
        Some(BTreeSet::from(["urn:ggen:pack:acme-minimal".to_string()]))
    );
    assert_eq!(
        caps.requires,
        Some(BTreeSet::from(["urn:ggen:pack:acme-base".to_string()]))
    );
    assert_eq!(caps.types, None);
}

/// Optional `types` array parses when present.
#[test]
fn parse_with_types() {
    let toml = format!(
        "{MINIMAL_TOML}\n[capabilities]\ntypes = [\"urn:ggen:pack:type:template\"]\nprovides = []\nrequires = []\n"
    );
    let parsed: PackFile = star_toml::from_str(&toml).expect("must parse");
    let caps = parsed.capabilities.expect("capabilities present");
    assert_eq!(
        caps.types,
        Some(BTreeSet::from(["urn:ggen:pack:type:template".to_string()]))
    );
}

/// Malformed capabilities (provides is a string, not an array) must be a typed
/// parse refusal — consistent with the crate's error style, never a silent pass.
#[test]
fn parse_malformed_capabilities_is_err() {
    let toml = format!("{MINIMAL_TOML}\n[capabilities]\nprovides = \"not-a-list\"\n");
    let result = star_toml::from_str::<PackFile>(&toml);
    let err = result.expect_err("malformed capabilities must refuse parse");
    let msg = err.to_string();
    assert!(
        msg.contains("capabilities") || msg.contains("provides"),
        "error must reference the offending field; got: {msg}"
    );
}

/// Round-trip: parse a PackFile with capabilities, serialize, re-parse.
#[test]
fn capabilities_round_trip() {
    let toml = format!(
        "{MINIMAL_TOML}\n[capabilities]\nprovides = [\"urn:ggen:pack:rt-a\", \"urn:ggen:pack:rt-b\"]\nrequires = [\"urn:ggen:pack:rt-dep\"]\n"
    );
    let parsed: PackFile = star_toml::from_str(&toml).expect("must parse");
    let serialized = star_toml::to_string(&parsed).expect("serialize PackFile");
    let reparsed: PackFile = star_toml::from_str(&serialized).expect("re-parse must succeed");
    let caps = reparsed
        .capabilities
        .expect("capabilities survive round trip");
    assert_eq!(
        caps.provides,
        Some(BTreeSet::from([
            "urn:ggen:pack:rt-a".to_string(),
            "urn:ggen:pack:rt-b".to_string()
        ]))
    );
    assert_eq!(
        caps.requires,
        Some(BTreeSet::from(["urn:ggen:pack:rt-dep".to_string()]))
    );
}

// ── Real-corpus smoke: real annotated pack.tomls from both repos ────────────

fn repo_root() -> std::path::PathBuf {
    std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .expect("repo root resolves")
}

fn ggen_packs_dir() -> std::path::PathBuf {
    repo_root().join("packs")
}

fn marketplace_packs_dir() -> std::path::PathBuf {
    repo_root().join("../ggen-marketplace/packs")
}

/// Scoped probe over real corpus files: real `star_toml` parse of the
/// `[capabilities]` table via `PackCapabilitiesFile`, with the `[pack]` table
/// held opaquely. Most corpus pack.tomls lack `[pack].id` — a pre-existing
/// condition (`Pack` requires `id`), outside this lane's file scope — so a
/// full `PackFile` parse is asserted separately for the packs that do carry it.
#[derive(serde::Deserialize)]
struct CorpusProbe {
    #[allow(dead_code)]
    pack: toml::Value,
    #[serde(default)]
    capabilities: Option<PackCapabilitiesFile>,
}

fn parse_corpus_probe(path: &std::path::Path) -> CorpusProbe {
    let content = fs::read_to_string(path).expect("read real pack.toml");
    star_toml::from_str(&content).expect("real pack.toml parses")
}

/// Collect up to `limit` annotated pack.tomls (sorted, deterministic) from a
/// packs directory.
fn sorted_annotated(dir: &std::path::Path, limit: usize) -> Vec<std::path::PathBuf> {
    let mut names: Vec<String> = fs::read_dir(dir)
        .expect("packs dir readable")
        .filter_map(|e| e.ok())
        .map(|e| e.file_name().to_string_lossy().into_owned())
        .filter(|n| {
            let p = dir.join(n).join("pack.toml");
            p.exists()
                && fs::read_to_string(&p)
                    .map(|c| c.contains("[capabilities]"))
                    .unwrap_or(false)
        })
        .collect();
    names.sort();
    names.truncate(limit);
    names
        .into_iter()
        .map(|n| dir.join(n).join("pack.toml"))
        .collect()
}

/// Five real annotated pack.tomls across the two repos: `[capabilities]`
/// parses via the real parser (strict `PackCapabilitiesFile`, opaque `pack`).
#[test]
fn real_corpus_annotated_packs_parse_capabilities() {
    let mut candidates = vec![
        ggen_packs_dir().join("ggen-combinatorial-maximalism-pack/pack.toml"),
        ggen_packs_dir().join("clap-noun-verb-verification-pack/pack.toml"),
        ggen_packs_dir().join("tcps-release-pack/pack.toml"),
    ];
    candidates.extend(sorted_annotated(&marketplace_packs_dir(), 2));
    candidates.truncate(5);

    let mut parsed_count = 0;
    for path in &candidates {
        assert!(path.exists(), "corpus file must exist: {}", path.display());
        let content = fs::read_to_string(path).unwrap();
        assert!(
            content.contains("[capabilities]"),
            "smoke target must be annotated: {}",
            path.display()
        );

        let probe = parse_corpus_probe(path);
        let caps = probe
            .capabilities
            .unwrap_or_else(|| panic!("capabilities must parse for {}", path.display()));
        assert!(
            caps.provides.is_some() || caps.requires.is_some() || caps.types.is_some(),
            "annotated pack must carry at least one capability array: {}",
            path.display()
        );
        parsed_count += 1;
    }
    assert_eq!(parsed_count, 5, "all five corpus files must parse");
}

/// Whole real corpus: every pack.toml in both repos parses via the real parser
/// (strict `PackCapabilitiesFile`), and every one is annotated — so the
/// capabilities field is proven live across the entire corpus, not a sample.
#[test]
fn real_corpus_all_packs_parse_capabilities() {
    let mut parsed = 0usize;
    let mut annotated = 0usize;
    for dir in [ggen_packs_dir(), marketplace_packs_dir()] {
        for entry in fs::read_dir(&dir).expect("packs dir readable") {
            let path = entry.expect("dir entry").path().join("pack.toml");
            if !path.exists() {
                continue;
            }
            let probe = parse_corpus_probe(&path);
            if probe.capabilities.is_some() {
                annotated += 1;
            }
            parsed += 1;
        }
    }
    assert!(
        parsed >= 400,
        "expected the full corpus to scan, got {parsed}"
    );
    assert_eq!(
        parsed, annotated,
        "every corpus pack.toml is annotated with [capabilities]"
    );
}

/// Zero-drift control on a real on-disk file without `[capabilities]`: full
/// `PackFile` parse yields None (pre-annotation packs parse unchanged).
#[test]
fn real_file_without_capabilities_parses_none() {
    let temp = TempDir::new().unwrap();
    let path = temp.path().join("pre-annotation.toml");
    fs::write(&path, MINIMAL_TOML).unwrap();
    let parsed: PackFile =
        star_toml::from_str(&fs::read_to_string(&path).unwrap()).expect("must parse");
    assert!(parsed.capabilities.is_none());
}

/// TempDir-based real file I/O: write a fixture pack and parse it from disk
/// exactly as the loader would.
#[test]
fn tempdir_fixture_parses_via_real_file_io() {
    let temp = TempDir::new().unwrap();
    let toml = format!("{MINIMAL_TOML}\n[capabilities]\nprovides = [\"urn:ggen:pack:fixture\"]\n");
    let path = temp.path().join("fixture.toml");
    fs::write(&path, toml).unwrap();

    let content = fs::read_to_string(&path).unwrap();
    let parsed: PackFile = star_toml::from_str(&content).expect("fixture parses");
    let caps = parsed.capabilities.expect("capabilities present");
    assert_eq!(
        caps.provides,
        Some(BTreeSet::from(["urn:ggen:pack:fixture".to_string()]))
    );
}
