//! Court: `[FM-TPL-006]` — a template file that fails to read/parse at sync
//! time is a typed refusal naming the file (the sync-time wrapper around the
//! inner FM-TPL-001/012/013 codes).
//!
//! Chicago: real temp-dir project (frontmatter-schema manifest + ontology +
//! a malformed `.tmpl`), the real `ggen_engine::sync::sync` pipeline entry,
//! no mocks. Emission site: `crates/ggen-engine/src/sync.rs`
//! `parse_template_file` (`fm_tpl(6, ...)`).
//! Closes GAP-3 for FM-TPL-006 (CARRY-09, zd-48).

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

#[test]
fn malformed_template_file_refuses_sync_with_fm_tpl_006_naming_the_file() {
    let dir = TempDir::new().expect("tempdir");
    std::fs::write(
        dir.path().join("ggen.toml"),
        "[project]\nname = \"badtpl\"\n\n[ontology]\nsource = \"ontology.ttl\"\n\n[templates]\ndir = \"templates\"\n",
    )
    .expect("write ggen.toml");
    std::fs::write(
        dir.path().join("ontology.ttl"),
        "@prefix ex: <http://example.org/> .\nex:alice ex:name \"alice\" .\n",
    )
    .expect("write ontology.ttl");
    std::fs::create_dir_all(dir.path().join("templates")).expect("mkdir templates");
    std::fs::write(
        dir.path().join("templates/broken.tmpl"),
        "---\nto: out/x.txt\nunterminated frontmatter, no closing delimiter\n",
    )
    .expect("write broken template");

    let err = sync(
        dir.path(),
        SyncOptions {
            skip_ea_admission: true,
            ..Default::default()
        },
    )
    .expect_err("a malformed template must refuse the sync");
    let msg = err.to_string();
    assert!(msg.contains("FM-TPL-006"), "{msg}");
    assert!(
        msg.contains("broken.tmpl"),
        "refusal must name the offending template file: {msg}"
    );
    assert!(
        !dir.path().join("out").exists(),
        "nothing may be written on refusal"
    );
}
