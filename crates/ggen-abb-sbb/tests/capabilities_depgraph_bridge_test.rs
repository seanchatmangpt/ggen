//! WO-03 pack-import bridge courts: `[capabilities]` `urn:ggen:pack:<id>` → depgraph facts.
//!
//! Chicago TDD — real `pack.toml` documents, `extract_pack_manifest` / `resolve_sync_order`.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use ggen_abb_sbb::depgraph::{extract_pack_manifest, resolve_sync_order, PackManifest};
use std::collections::BTreeSet;

fn manifest(name: &str) -> PackManifest {
    PackManifest {
        name: name.into(),
        depends_on: BTreeSet::new(),
        provides: BTreeSet::new(),
        requires: BTreeSet::new(),
        artifacts: BTreeSet::new(),
    }
}

#[test]
fn capabilities_only_pack_toml_projects_pack_import_urns() {
    let src = r#"
[pack]
name = "consumer-pack"

[capabilities]
provides = ["urn:ggen:pack:consumer-pack", "urn:ggen:cap:extra"]
requires = ["urn:ggen:pack:provider-pack"]
"#;
    let m = extract_pack_manifest(src).expect("test fixture: constructed value cannot fail");
    assert_eq!(m.name, "consumer-pack");
    assert!(m.depends_on.contains("provider-pack"));
    assert!(m.requires.contains("provider-pack"));
    assert!(m.provides.contains("consumer-pack"));
    assert!(!m.depends_on.contains("urn:ggen:cap:extra"));
}

#[test]
fn capabilities_requirer_and_provider_resolve_sync_order_succeeds() {
    let provider_src = r#"
[pack]
name = "provider-pack"

[capabilities]
provides = ["urn:ggen:pack:provider-pack"]
"#;
    let requirer_src = r#"
[pack]
name = "requirer-pack"

[capabilities]
requires = ["urn:ggen:pack:provider-pack"]
"#;
    let provider =
        extract_pack_manifest(provider_src).expect("test fixture: constructed value cannot fail");
    let requirer =
        extract_pack_manifest(requirer_src).expect("test fixture: constructed value cannot fail");
    let plan = resolve_sync_order(&[provider, requirer], None)
        .expect("test fixture: constructed value cannot fail");
    assert_eq!(plan.order, vec!["provider-pack", "requirer-pack"]);
}

#[test]
fn capabilities_missing_provider_dep_refuses_dangling_reference() {
    let requirer_src = r#"
[pack]
name = "lonely-pack"

[capabilities]
requires = ["urn:ggen:pack:ghost-pack"]
"#;
    let requirer =
        extract_pack_manifest(requirer_src).expect("test fixture: constructed value cannot fail");
    let err = resolve_sync_order(&[requirer], None).unwrap_err();
    assert!(err.to_string().contains("DANGLING_REFERENCE"), "got: {err}");
}

#[test]
fn capabilities_unbound_port_refuses_when_provider_lacks_matching_provide() {
    let packs = vec![
        manifest("provider-pack"),
        PackManifest {
            depends_on: std::iter::once("provider-pack".to_string()).collect(),
            requires: std::iter::once("provider-pack".to_string()).collect(),
            ..manifest("requirer-pack")
        },
    ];
    let err = resolve_sync_order(&packs, None).unwrap_err();
    assert!(err.to_string().starts_with("UNBOUND_PORT"), "got: {err}");
}

#[test]
fn graph_and_capabilities_union_additively() {
    let src = r#"
[pack]
name = "merged-pack"

[graph]
depends_on = ["graph-base"]
provides = ["port-a"]
requires = ["port-b"]

[capabilities]
provides = ["urn:ggen:pack:merged-pack"]
requires = ["urn:ggen:pack:cap-base"]
"#;
    let m = extract_pack_manifest(src).expect("test fixture: constructed value cannot fail");
    assert!(m.depends_on.contains("graph-base"));
    assert!(m.depends_on.contains("cap-base"));
    assert!(m.provides.contains("port-a"));
    assert!(m.provides.contains("merged-pack"));
    assert!(m.requires.contains("port-b"));
    assert!(m.requires.contains("cap-base"));
}

#[test]
fn legacy_pack_without_graph_or_capabilities_stays_dependency_free() {
    let src = "[pack]\nname = \"legacy-pack\"\n";
    let m = extract_pack_manifest(src).expect("test fixture: constructed value cannot fail");
    assert_eq!(m.name, "legacy-pack");
    assert!(m.depends_on.is_empty());
    assert!(m.provides.is_empty());
    assert!(m.requires.is_empty());
}

#[test]
fn non_pack_capability_urns_ignored_for_depgraph() {
    let src = r#"
[pack]
name = "urn-mix-pack"

[capabilities]
provides = ["urn:strata:contract:abc", "urn:ggen:cap:extra"]
requires = ["urn:strata:pack:need"]
"#;
    let m = extract_pack_manifest(src).expect("test fixture: constructed value cannot fail");
    assert!(m.depends_on.is_empty());
    assert!(m.provides.is_empty());
    assert!(m.requires.is_empty());
}

#[test]
fn non_canonical_pack_id_in_capability_urn_refuses() {
    let src = r#"
[pack]
name = "bad-pack"

[capabilities]
provides = ["urn:ggen:pack:BadPack"]
"#;
    let err = extract_pack_manifest(src).unwrap_err();
    match err {
        ggen_abb_sbb::Refusal::NonCanonicalPort { pack, field, port } => {
            assert_eq!(pack, "bad-pack");
            assert_eq!(field, "capabilities.provides");
            assert_eq!(port, "BadPack");
        }
        other => panic!("wrong refusal: {other:?}"),
    }
}
