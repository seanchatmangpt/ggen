//! Corpus smoke: every pack.toml in the local and marketplace pack dirs must
//! parse through the STRICT loader shape that `ggen sync` uses
//! (`crates/ggen-engine/src/pack.rs` `PackToml` -> `star_toml`).
//!
//! `PackToml`/`PackCapabilities` are private, so this test mirrors their serde
//! contract exactly: closed `[capabilities]` (types/provides/requires, all
//! string arrays), open `[pack]` and open top-level tables. Any drift between
//! this mirror and pack.rs is a test bug — keep them in sync.

use std::collections::BTreeMap;
use std::fs;
use std::path::Path;

use serde::Deserialize;

#[derive(Debug, Deserialize)]
struct SmokePackToml {
    #[allow(dead_code)]
    pack: BTreeMap<String, toml::Value>,
    #[serde(default)]
    #[allow(dead_code)]
    dependencies: BTreeMap<String, String>,
    #[serde(default)]
    capabilities: SmokePackCapabilities,
    #[serde(flatten)]
    #[allow(dead_code)]
    extra: BTreeMap<String, toml::Value>,
}

#[derive(Debug, Default, Deserialize)]
#[serde(deny_unknown_fields)]
struct SmokePackCapabilities {
    #[serde(default)]
    #[allow(dead_code)]
    types: Vec<String>,
    #[serde(default)]
    #[allow(dead_code)]
    provides: Vec<String>,
    #[serde(default)]
    #[allow(dead_code)]
    requires: Vec<String>,
}

fn corpus_roots() -> Vec<&'static Path> {
    vec![
        Path::new("/Users/sac/ggen/packs"),
        Path::new("/Users/sac/ggen-marketplace/packs"),
    ]
}

#[test]
fn every_pack_toml_in_corpus_parses_with_strict_loader_shape() {
    let mut parsed = 0usize;
    let mut refusals: Vec<String> = Vec::new();

    for root in corpus_roots() {
        let entries = fs::read_dir(root)
            .unwrap_or_else(|e| panic!("pack corpus dir unreadable at {}: {e}", root.display()));
        for entry in entries {
            let entry = entry.expect("readdir entry");
            let manifest = entry.path().join("pack.toml");
            if !manifest.is_file() {
                continue;
            }
            let raw = match fs::read_to_string(&manifest) {
                Ok(raw) => raw,
                Err(e) => {
                    refusals.push(format!("{}: unreadable: {e}", manifest.display()));
                    continue;
                }
            };
            match star_toml::from_str::<SmokePackToml>(&raw) {
                Ok(_) => parsed += 1,
                Err(e) => refusals.push(format!("{}: {e}", manifest.display())),
            }
        }
    }

    assert!(
        refusals.is_empty(),
        "pack.toml refusals under strict loader shape ({}):\n{}",
        refusals.len(),
        refusals.join("\n")
    );
    println!("parsed {parsed} pack.tomls, 0 refusals");
    assert!(parsed > 400, "expected >400 packs, got {parsed}");
}
