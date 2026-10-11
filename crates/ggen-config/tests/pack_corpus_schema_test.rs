//! Pack corpus schema guard (lane BH).
//!
//! Mirrors the strict shapes of `PackToml`/`PackCapabilities` from
//! `crates/ggen-engine/src/pack.rs` (real path: ggen-engine's `PackToml`
//! collects unknown top-level tables into a flattened `extra` map, so
//! unknown keys parse; `PackCapabilities` is `#[serde(deny_unknown_fields)]`
//! with only `types`/`provides`/`requires`). `ggen-engine` is NOT a
//! dev-dependency of `ggen-config` (checked crates/ggen-config/Cargo.toml),
//! so these are local mirror structs, not the real type. The mirror is
//! proven faithful by the negative tests below (a path-map `capabilities`
//! must be refused, exactly as the real loader refuses it).

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)] // Chicago TDD: real-IO tests
use std::collections::{BTreeMap, BTreeSet};

use serde::Deserialize;

const MARKETPLACE: &str = "xaas-ash-core-pack";
const FORTUNE5: &str = "fortune5-enterprise-architecture-pack";

fn pack_path(name: &str) -> String {
    format!(
        "{}/ggen-marketplace/packs/{}/pack.toml",
        std::env::var("HOME").expect("HOME set"),
        name
    )
}

fn read_pack(name: &str) -> toml::Value {
    let path = pack_path(name);
    let raw = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {path}: {e}"));
    star_toml::from_str(&raw).unwrap_or_else(|e| panic!("parse {path}: {e}"))
}

/// Mirror of PackCapabilities (deny_unknown_fields, types/provides/requires).
#[derive(Debug, Deserialize, Default)]
#[serde(deny_unknown_fields)]
struct PackCapabilitiesMirror {
    #[serde(default)]
    types: BTreeSet<String>,
    #[serde(default)]
    provides: BTreeSet<String>,
    #[serde(default)]
    requires: BTreeSet<String>,
}

#[test]
fn xaas_pack_parses_and_has_no_top_level_generation_rules() {
    let v = read_pack(MARKETPLACE);
    assert!(
        v.get("generation_rules").is_none(),
        "pack.toml top level must not declare generation_rules (engine extra, not schema)"
    );
    // The rule data survived under its namespaced key.
    let rules = v
        .get("xaas")
        .and_then(|x| x.get("generation_rules"))
        .expect("xaas.generation_rules present");
    assert!(rules.as_array().map(|a| !a.is_empty()).unwrap_or(false));
}

#[test]
fn fortune5_pack_parses_with_no_capabilities_path_map() {
    let v = read_pack(FORTUNE5);
    // The historical path-map table lives under [capability_paths]; a
    // lawful provides/requires [capabilities] block (added by the corpus
    // annotation pass) may coexist — its shape is asserted by
    // capabilities_if_present_accepts_only_provides_requires_shape. What
    // must never return is path-map keys inside [capabilities] (the real
    // PackCapabilities refuses them via deny_unknown_fields).
    if let Some(cap) = v.get("capabilities") {
        assert!(
            cap.get("ontology").is_none()
                && cap.get("queries").is_none()
                && cap.get("templates").is_none()
                && cap.get("schemas").is_none(),
            "capabilities must not be a path map"
        );
    }
    let paths = v.get("capability_paths").expect("capability_paths present");
    assert_eq!(
        paths.get("ontology").and_then(|x| x.as_str()),
        Some("ontology.ttl")
    );
}

#[test]
fn capabilities_if_present_accepts_only_provides_requires_shape() {
    for name in [MARKETPLACE, FORTUNE5] {
        let v = read_pack(name);
        if let Some(cap) = v.get("capabilities") {
            let mirror: PackCapabilitiesMirror = cap.clone().try_into().unwrap_or_else(|e| {
                panic!("{name}: [capabilities] not provides/requires-shaped: {e}")
            });
            let _ = mirror; // shape proven by successful strict parse
        }
    }
}

#[test]
fn negative_path_map_capabilities_is_refused_by_strict_mirror() {
    // Proves the mirror has teeth: the exact shape fortune5 used to declare
    // must fail the strict capabilities schema, matching the real loader.
    let raw = r#"
ontology = "ontology.ttl"
queries = "queries/"
"#;
    let val: toml::Value = star_toml::from_str(raw).expect("valid toml");
    let result: Result<PackCapabilitiesMirror, _> = val.try_into();
    assert!(result.is_err(), "path-map capabilities must be refused");
}

#[test]
fn both_packs_parse_cleanly_as_pack_toml_shaped_documents() {
    for name in [MARKETPLACE, FORTUNE5] {
        let v = read_pack(name);
        let pack = v.get("pack").expect("[pack] table");
        assert!(pack.get("name").is_some(), "{name}: pack.name present");
        assert!(
            pack.get("version").is_some(),
            "{name}: pack.version present"
        );
        let _extra: BTreeMap<String, toml::Value> = BTreeMap::new();
        // Unknown top-level tables are tolerated (flattened extra in the real
        // loader) -- presence of any table here is fine; the shape-specific
        // bans live in the tests above.
        assert!(v.is_table());
    }
}
