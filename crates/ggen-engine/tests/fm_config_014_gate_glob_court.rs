//! Court: `[FM-CONFIG-014]` — a `[validation].gates` glob that matches no
//! files is a typed, fail-closed sync refusal (`GLOB_NO_MATCHES`), never a
//! silent pass of zero gates. Key: `[law].gates` (the engine
//! frontmatter-schema gate surface), not `[validation].gates`.
//!
//! Chicago: real temp-dir project (manifest + ontology), the real
//! `ggen_engine::sync::sync` pipeline entry, no mocks. Emission site:
//! `crates/ggen-engine/src/sync.rs` `resolve_gate_entries` (`fm_config(14, ...)`).
//! Closes GAP-3 for FM-CONFIG-014 (CARRY-09, zd-48).

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

#[test]
fn gate_glob_matching_zero_files_refuses_sync_with_fm_config_014() {
    let dir = TempDir::new().expect("tempdir");
    std::fs::write(
        dir.path().join("ggen.toml"),
        "[project]\nname = \"gateglob\"\n\n[ontology]\nsource = \"ontology.ttl\"\n\n[templates]\ndir = \"templates\"\n\n[law]\ngates = [\"gates/*.norq\"]\n",
    )
    .expect("write ggen.toml");
    std::fs::write(
        dir.path().join("ontology.ttl"),
        "@prefix ex: <http://example.org/> .\nex:alice ex:name \"alice\" .\n",
    )
    .expect("write ontology.ttl");
    std::fs::create_dir_all(dir.path().join("templates")).expect("mkdir templates");
    std::fs::write(
        dir.path().join("templates/one.tmpl"),
        "---\nto: out/names.txt\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{% for row in results %}{{ row.name }}\n{% endfor %}",
    )
    .expect("write template");
    // Deliberately NO gates/ directory: the glob must match nothing.

    let err = sync(
        dir.path(),
        SyncOptions {
            skip_ea_admission: true,
            ..Default::default()
        },
    )
    .expect_err("a typo'd gate glob must refuse the sync, not pass zero gates");
    let msg = err.to_string();
    assert!(msg.contains("FM-CONFIG-014"), "{msg}");
    assert!(msg.contains("GLOB_NO_MATCHES"), "{msg}");
    assert!(
        msg.contains("gates/*.norq"),
        "refusal must name the offending pattern: {msg}"
    );
    assert!(
        !dir.path().join("out").exists(),
        "nothing may be written on refusal"
    );
}
