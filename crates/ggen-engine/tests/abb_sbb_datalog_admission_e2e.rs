//! E2E integration test proving ABB/SBB and cross-pack Datalog admission
//! inside the sync pipeline:
//! 1. EA graph with qualified SBB admits cleanly.
//! 2. Unqualified or port-missing SBB refuses sync fail-closed.
//! 3. Cyclic pack dependencies refuse sync fail-closed.
//! 4. Unbound required port across packs refuses sync fail-closed.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use ggen_engine::sync::{sync, SyncOptions};
use std::path::Path;
use tempfile::TempDir;

const GGEN_TOML: &str = r#"
[project]
name = "abbdemo"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

const ONTOLOGY: &str = r#"
@prefix ex: <http://example.org/> .
ex:subject a ex:Item .
"#;

const TEMPLATE: &str = "---\nto: out/result.txt\n---\nhello\n";

fn scaffold(root: &Path) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY).expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::write(root.join("templates/main.tmpl"), TEMPLATE).expect("write template");
}

#[test]
fn qualified_ea_graph_admits_cleanly() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    let g = ggen_abb_sbb::synthetic_graph(1, 1);
    let ea_json = serde_json::to_string_pretty(&g).expect("serialize EA graph");
    std::fs::write(dir.path().join("ea.graph.json"), ea_json).expect("write ea.graph.json");

    let report =
        sync(dir.path(), SyncOptions::default()).expect("sync must admit qualified EA graph");
    assert_eq!(
        report.written,
        vec![std::path::PathBuf::from("out/result.txt")]
    );
}

#[test]
fn unqualified_sbb_refuses_sync() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    let mut g = ggen_abb_sbb::synthetic_graph(1, 1);
    // Clear qualifications to make the candidate SBB unqualified
    g.qualifications.clear();
    let ea_json = serde_json::to_string_pretty(&g).expect("serialize EA graph");
    std::fs::write(dir.path().join("ea.graph.json"), ea_json).expect("write ea.graph.json");

    let err = sync(dir.path(), SyncOptions::default()).expect_err("unqualified SBB must refuse");
    let msg = err.to_string();
    assert!(
        msg.contains("SBB_UNQUALIFIED") || msg.contains("admission refused"),
        "{msg}"
    );
    assert!(
        !dir.path().join("out/result.txt").exists(),
        "refused sync must write nothing"
    );
}

#[test]
fn cyclic_pack_dependency_refuses_sync() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());

    // Create pack A depending on B, and pack B depending on A
    let pack_a = dir.path().join("packs/pack-a");
    let pack_b = dir.path().join("packs/pack-b");
    std::fs::create_dir_all(&pack_a).expect("mkdir pack-a");
    std::fs::create_dir_all(&pack_b).expect("mkdir pack-b");

    let pack_a_toml = r#"
[pack]
name = "pack-a"
version = "1.0.0"
description = "Pack A"

[graph]
depends_on = ["pack-b"]
"#;
    let pack_b_toml = r#"
[pack]
name = "pack-b"
version = "1.0.0"
description = "Pack B"

[graph]
depends_on = ["pack-a"]
"#;
    std::fs::write(pack_a.join("pack.toml"), pack_a_toml).expect("write pack-a toml");
    std::fs::write(pack_b.join("pack.toml"), pack_b_toml).expect("write pack-b toml");
    std::fs::write(pack_a.join("ontology.ttl"), "").expect("write pack-a ttl");
    std::fs::write(pack_b.join("ontology.ttl"), "").expect("write pack-b ttl");
    std::fs::create_dir_all(pack_a.join("templates")).expect("mkdir pack-a templates");
    std::fs::create_dir_all(pack_b.join("templates")).expect("mkdir pack-b templates");
    std::fs::write(
        pack_a.join("templates/a.tmpl"),
        "---\nto: out/a.txt\n---\na\n",
    )
    .expect("write a.tmpl");
    std::fs::write(
        pack_b.join("templates/b.tmpl"),
        "---\nto: out/b.txt\n---\nb\n",
    )
    .expect("write b.tmpl");

    let ggen_toml = r#"
[project]
name = "cyclic-demo"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"

[packs.pack-a]
path = "packs/pack-a"

[packs.pack-b]
path = "packs/pack-b"
"#;
    std::fs::write(dir.path().join("ggen.toml"), ggen_toml).expect("write ggen.toml");

    let err = sync(dir.path(), SyncOptions::default()).expect_err("cyclic dependency must refuse");
    let msg = err.to_string();
    assert!(msg.contains("CYCLIC_PACK_DEPENDENCY"), "{msg}");
    assert!(
        !dir.path().join("out/result.txt").exists(),
        "refused sync must write nothing"
    );
}

#[test]
fn unbound_pack_port_refuses_sync() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());

    let pack_a = dir.path().join("packs/pack-a");
    std::fs::create_dir_all(&pack_a).expect("mkdir pack-a");

    let pack_a_toml = r#"
[pack]
name = "pack-a"
version = "1.0.0"
description = "Pack A"

[graph]
requires = ["port:missing-event-stream"]
"#;
    std::fs::write(pack_a.join("pack.toml"), pack_a_toml).expect("write pack-a toml");
    std::fs::write(pack_a.join("ontology.ttl"), "").expect("write pack-a ttl");
    std::fs::create_dir_all(pack_a.join("templates")).expect("mkdir pack-a templates");
    std::fs::write(
        pack_a.join("templates/a.tmpl"),
        "---\nto: out/a.txt\n---\na\n",
    )
    .expect("write a.tmpl");

    let ggen_toml = r#"
[project]
name = "port-demo"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"

[packs.pack-a]
path = "packs/pack-a"
"#;
    std::fs::write(dir.path().join("ggen.toml"), ggen_toml).expect("write ggen.toml");

    let err = sync(dir.path(), SyncOptions::default()).expect_err("unbound port must refuse");
    let msg = err.to_string();
    assert!(msg.contains("UNBOUND_PORT"), "{msg}");
    assert!(
        !dir.path().join("out/result.txt").exists(),
        "refused sync must write nothing"
    );
}
