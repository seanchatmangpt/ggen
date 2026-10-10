//! Experiment: capability-topology ordering edges (cap-topology, 2026-10-09).
//!
//! URN-form `requires` (`urn:ggen:pack:<name>`) between packs of the declared
//! universe create ORDERING edges in `dependency_scope` only when the combined
//! graph stays acyclic. Satisfaction stays consumer-declaration-based
//! (FM-PACK-018 H2): mutual URN requires never refuse; a real
//! `[dependencies]` cycle still refuses via sync.

use std::collections::{BTreeMap, BTreeSet};
use std::path::PathBuf;

use tempfile::TempDir;

use ggen_engine::pack::{dependency_scope, Pack, ScopeDepth};
use ggen_engine::sync::{sync, SyncOptions};

fn exp_pack(name: &str, dependencies: &[&str], requires: &[&str]) -> Pack {
    Pack {
        name: name.to_string(),
        version: "1.0.0".to_string(),
        description: String::new(),
        dependencies: dependencies
            .iter()
            .map(|d| ((*d).to_string(), "1.0.0".to_string()))
            .collect::<BTreeMap<_, _>>(),
        semantic_types: BTreeSet::new(),
        provides: BTreeSet::new(),
        requires: requires.iter().map(|r| (*r).to_string()).collect(),
        root: PathBuf::from("/tmp"),
        ontology_path: PathBuf::from("/tmp/ontology.ttl"),
        extra_ontology_paths: Vec::new(),
        template_paths: Vec::new(),
        lock: true,
    }
}

fn urn(pack: &str) -> String {
    format!("urn:ggen:pack:{pack}")
}

fn names<'a>(scoped: &[&'a Pack]) -> Vec<&'a str> {
    scoped.iter().map(|p| p.name.as_str()).collect()
}

#[test]
fn urn_requires_order_providers_after_requirers() {
    // a -> (URN) -> b -> (URN) -> c: scope of `a` must visit b then c.
    let packs = vec![
        exp_pack("c", &[], &[]),
        exp_pack("b", &[], &[&urn("c")]),
        exp_pack("a", &[], &[&urn("b")]),
    ];
    let scoped = dependency_scope(&packs, "a", ScopeDepth::Transitive).expect("scope resolves");
    assert_eq!(names(&scoped), vec!["a", "b", "c"]);
}

#[test]
fn mutual_urn_requires_do_not_refuse_and_fall_back_to_dependency_order() {
    // Mutual URN requires would form a capability cycle: ordering edges are
    // ignored (H2 preserved — consumer-advice, never closure), scope reduces
    // to the dependencies-only traversal, and nothing refuses.
    let packs = vec![
        exp_pack("self-monitoring", &[], &[&urn("dogfood-lifecycle")]),
        exp_pack("dogfood-lifecycle", &[], &[&urn("self-monitoring")]),
    ];
    let scoped = dependency_scope(&packs, "self-monitoring", ScopeDepth::Transitive)
        .expect("mutual URN requires must not refuse");
    assert_eq!(names(&scoped), vec!["self-monitoring"]);
    let scoped_other = dependency_scope(&packs, "dogfood-lifecycle", ScopeDepth::Transitive)
        .expect("mutual URN requires must not refuse");
    assert_eq!(names(&scoped_other), vec!["dogfood-lifecycle"]);
}

/// Minimal on-disk pack with an empty ontology and one template.
fn write_pack(root: &PathBuf, name: &str, dependencies: &[&str], requires: &[&str]) -> PathBuf {
    let dir = root.join("packs").join(name);
    std::fs::create_dir_all(dir.join("templates")).expect("mkdir pack");
    let mut toml =
        format!("[pack]\nname = \"{name}\"\nversion = \"1.0.0\"\ndescription = \"{name}\"\n");
    if !dependencies.is_empty() {
        toml.push_str("\n[dependencies]\n");
        for dependency in dependencies {
            toml.push_str(&format!("{dependency} = \"1.0.0\"\n"));
        }
    }
    if !requires.is_empty() {
        toml.push_str("\n[capabilities]\nrequires = [");
        let items: Vec<String> = requires.iter().map(|r| format!("\"{r}\"")).collect();
        toml.push_str(&items.join(", "));
        toml.push_str("]\n");
    }
    std::fs::write(dir.join("pack.toml"), toml).expect("write pack.toml");
    std::fs::write(dir.join("ontology.ttl"), "").expect("write ontology.ttl");
    std::fs::write(
        dir.join("templates").join(format!("{name}.tmpl")),
        format!("---\nto: out/{name}.txt\n---\n{name}\n"),
    )
    .expect("write template");
    dir
}

fn write_manifest(dir: &TempDir, pack_names: &[&str]) {
    let mut manifest = String::from(
        "[project]\nname = \"cap-topology-exp\"\n\n[ontology]\nsource = \"ontology.ttl\"\n\n[templates]\ndir = \"templates\"\n",
    );
    for name in pack_names {
        manifest.push_str(&format!("\n[packs.{name}]\npath = \"packs/{name}\"\n"));
    }
    std::fs::write(dir.path().join("ggen.toml"), manifest).expect("write ggen.toml");
    std::fs::write(dir.path().join("ontology.ttl"), "").expect("write ontology.ttl");
    std::fs::create_dir_all(dir.path().join("templates")).expect("mkdir templates");
}

#[test]
fn mutual_urn_requires_sync_green() {
    let dir = TempDir::new().expect("tempdir");
    write_pack(
        &dir.path().to_path_buf(),
        "self-monitoring",
        &[],
        &[&urn("dogfood-lifecycle")],
    );
    write_pack(
        &dir.path().to_path_buf(),
        "dogfood-lifecycle",
        &[],
        &[&urn("self-monitoring")],
    );
    write_manifest(&dir, &["self-monitoring", "dogfood-lifecycle"]);
    sync(dir.path(), SyncOptions::default()).expect("mutual URN requires must stay green");
    assert!(dir.path().join("out/self-monitoring.txt").exists());
    assert!(dir.path().join("out/dogfood-lifecycle.txt").exists());
}

#[test]
fn declared_dependency_cycle_still_refuses() {
    let dir = TempDir::new().expect("tempdir");
    write_pack(&dir.path().to_path_buf(), "p1", &["p2"], &[]);
    write_pack(&dir.path().to_path_buf(), "p2", &["p1"], &[]);
    write_manifest(&dir, &["p1", "p2"]);
    let err = sync(dir.path(), SyncOptions::default())
        .expect_err("declared dependency cycle must refuse");
    let msg = err.to_string();
    assert!(
        msg.contains("CYCLIC_PACK_DEPENDENCY") || msg.contains("cyclic"),
        "expected cycle refusal, got: {msg}"
    );
}
