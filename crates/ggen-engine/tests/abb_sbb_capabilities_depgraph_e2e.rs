//! Stage 2c e2e: `[capabilities]` `urn:ggen:pack:` requires/provides (no `[graph]`)
//! admitted/refused through the real `sync()` depgraph gate (`resolve_sync_order`
//! → FM-PACK-015). Chicago TDD: temp fixtures, real kernel, no mocks.

#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::similar_names
)]

use ggen_engine::error::AppError;
use ggen_engine::sync::{sync, SyncOptions};
use std::path::Path;
use tempfile::TempDir;

const GGEN_TOML_BASE: &str = r#"
[project]
name = "caps-depgraph"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

const ONTOLOGY: &str = r"
@prefix ex: <http://example.org/> .
ex:subject a ex:Item .
";

const TEMPLATE: &str = "---\nto: out/result.txt\n---\nhello\n";

const EMPTY_EA_GRAPH: &str = r#"{
  "schema": "ggen.ea.graph.v1",
  "strategy": { "id": "s", "title": "t" },
  "capabilities": [],
  "abbs": [],
  "contracts": [],
  "candidate_sbbs": [],
  "qualifications": [],
  "packs": []
}"#;

fn scaffold_consumer(root: &Path) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML_BASE).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY).expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::write(root.join("templates/main.tmpl"), TEMPLATE).expect("write template");
    std::fs::write(root.join("ea.graph.json"), EMPTY_EA_GRAPH).expect("write ea.graph.json");
}

fn write_capability_pack(root: &Path, name: &str, provides: &[&str], requires: &[&str]) {
    let pack_dir = root.join("packs").join(name);
    std::fs::create_dir_all(pack_dir.join("templates")).expect("mkdir pack templates");
    let mut manifest = format!(
        "[pack]\nname = \"{name}\"\nversion = \"1.0.0\"\ndescription = \"capabilities-only fixture\"\n"
    );
    if !provides.is_empty() || !requires.is_empty() {
        manifest.push_str("\n[capabilities]\n");
        if !provides.is_empty() {
            let items: Vec<String> = provides.iter().map(|p| format!("\"{p}\"")).collect();
            manifest.push_str(&format!("provides = [{}]\n", items.join(", ")));
        }
        if !requires.is_empty() {
            let items: Vec<String> = requires.iter().map(|r| format!("\"{r}\"")).collect();
            manifest.push_str(&format!("requires = [{}]\n", items.join(", ")));
        }
    }
    std::fs::write(pack_dir.join("pack.toml"), manifest).expect("write pack.toml");
    std::fs::write(pack_dir.join("ontology.ttl"), "").expect("write pack ontology");
}

fn wire_packs(root: &Path, pack_names: &[&str]) {
    let mut ggen = GGEN_TOML_BASE.to_string();
    for name in pack_names {
        ggen.push_str(&format!("\n[packs.{name}]\npath = \"packs/{name}\"\n"));
    }
    std::fs::write(root.join("ggen.toml"), ggen).expect("write ggen.toml");
}

#[test]
fn capabilities_urn_depgraph_admits_when_both_packs_wired() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    scaffold_consumer(root);
    write_capability_pack(root, "pack-a", &["urn:ggen:pack:pack-a"], &[]);
    write_capability_pack(
        root,
        "pack-b",
        &["urn:ggen:pack:pack-b"],
        &["urn:ggen:pack:pack-a"],
    );
    wire_packs(root, &["pack-a", "pack-b"]);

    let report = sync(
        root,
        SyncOptions {
            skip_ea_admission: true,
            ..SyncOptions::default()
        },
    )
    .expect("capabilities-only depgraph must admit when provider is in [packs]");
    assert_eq!(
        report.written,
        vec![std::path::PathBuf::from("out/result.txt")]
    );
}

#[test]
fn capabilities_missing_provider_refuses_fm_pack_015_dangling() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    scaffold_consumer(root);
    write_capability_pack(
        root,
        "pack-b",
        &["urn:ggen:pack:pack-b"],
        &["urn:ggen:pack:pack-a"],
    );
    wire_packs(root, &["pack-b"]);

    let err = sync(
        root,
        SyncOptions {
            skip_ea_admission: true,
            ..SyncOptions::default()
        },
    )
    .expect_err("missing capabilities provider must refuse at Stage 2c");
    let msg = err.to_string();
    assert!(msg.contains("FM-PACK-015"), "expected FM-PACK-015, got: {msg}");
    assert!(
        msg.contains("DANGLING_REFERENCE"),
        "expected DANGLING_REFERENCE, got: {msg}"
    );
    if let AppError::AbbRefusal { refusal, .. } = &err {
        assert!(
            matches!(refusal, ggen_abb_sbb::Refusal::DanglingReference { .. }),
            "expected typed DanglingReference, got: {refusal:?}"
        );
    } else {
        panic!("expected AppError::AbbRefusal, got: {err:?}");
    }
    assert!(
        !root.join("out/result.txt").exists(),
        "refused sync must write nothing"
    );
}
