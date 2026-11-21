//! WO-03 acceptance §3 contradiction court (Chicago TDD).
#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]
use ggen_abb_sbb::depgraph::resolve_sync_order_from_pack_tomls;
use ggen_marketplace::packs_registry::composer::{compose, CompositionRefusal};
use ggen_marketplace::packs_registry::types::PackFile;
use std::fs;
use tempfile::TempDir;

fn parse_fixture(toml_body: &str) -> PackFile {
    let dir = TempDir::new().expect("tempdir");
    let path = dir.path().join("pack.toml");
    fs::write(&path, toml_body).expect("write");
    star_toml::from_str(&fs::read_to_string(&path).expect("read")).expect("parse")
}

fn pack_toml(id: &str, provides: &[&str], requires: &[&str]) -> String {
    format!(
        "[pack]\nid = \"{id}\"\nname = \"{id}\"\nversion = \"1.0.0\"\n\
         description = \"fixture\"\ncategory = \"test\"\npackages = []\n\
         [capabilities]\nprovides = [{}]\nrequires = [{}]\n",
        provides.iter().map(|p| format!("\"{p}\"")).collect::<Vec<_>>().join(", "),
        requires.iter().map(|p| format!("\"{p}\"")).collect::<Vec<_>>().join(", ")
    )
}

fn assert_gates_agree(bodies: &[&str]) {
    let packs: Vec<PackFile> = bodies.iter().map(|b| parse_fixture(b)).collect();
  match (&compose(&packs), &resolve_sync_order_from_pack_tomls(bodies, None)) {
        (Ok(c), Ok(k)) => {
            let order: Vec<_> = c.order.iter().filter(|id| c.pack_ids.contains(*id)).cloned().collect();
            assert_eq!(order, k.order);
        }
        (Err(_), Err(_)) => {}
        (Ok(_), Err(k)) => panic!("contradiction: composer ok, kernel {k:?}"),
        (Err(c), Ok(_)) => panic!("contradiction: composer {c:?}, kernel ok"),
    }
}

#[test]
fn three_pack_urn_capability_chain_admits_with_matching_order() {
    let base = pack_toml("base-pack", &["urn:ggen:pack:base-pack"], &[]);
    let mid = pack_toml("mid-pack", &["urn:ggen:pack:mid-pack"], &["urn:ggen:pack:base-pack"]);
    let top = pack_toml("top-pack", &["urn:ggen:pack:top-pack"], &["urn:ggen:pack:mid-pack"]);
    assert_gates_agree(&[base.as_str(), mid.as_str(), top.as_str()]);
}

#[test]
fn diamond_urn_requires_admits_with_matching_order() {
    let base = pack_toml("base-pack", &["urn:ggen:pack:base-pack"], &[]);
    let left = pack_toml("left-pack", &["urn:ggen:pack:left-pack"], &["urn:ggen:pack:base-pack"]);
    let right = pack_toml("right-pack", &["urn:ggen:pack:right-pack"], &["urn:ggen:pack:base-pack"]);
    let top = pack_toml(
        "top-pack",
        &["urn:ggen:pack:top-pack"],
        &["urn:ggen:pack:left-pack", "urn:ggen:pack:right-pack"],
    );
    assert_gates_agree(&[base.as_str(), left.as_str(), right.as_str(), top.as_str()]);
}

#[test]
fn unbound_pack_import_urn_refuses_on_both_gates() {
    let lonely = pack_toml("lonely-pack", &[], &["urn:ggen:pack:ghost-pack"]);
    let packs = vec![parse_fixture(&lonely)];
    assert!(matches!(compose(&packs), Err(CompositionRefusal::UnboundRequirement { .. })));
    let k = resolve_sync_order_from_pack_tomls(&[lonely.as_str()], None).unwrap_err();
    assert!(k.to_string().contains("DANGLING_REFERENCE") || k.to_string().contains("UNBOUND_PORT"));
}
