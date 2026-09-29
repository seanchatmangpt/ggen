//! Chicago-TDD end-to-end tests for `[ontology].imports` in the frontmatter
//! (`ggen-engine`) `ggen.toml` schema: extra Turtle files, resolved relative
//! to the manifest, are unioned into the graph alongside `source`.
//! Real tempdir, real TOML, real Turtle, real sync; no mocks.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::Path;

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

fn write(root: &Path, rel: &str, content: &str) {
    let path = root.join(rel);
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).expect("mkdir parent");
    }
    std::fs::write(path, content).expect("write file");
}

fn project(imports: &str) -> TempDir {
    let dir = TempDir::new().expect("tempdir");
    write(
        dir.path(),
        "ggen.toml",
        &format!(
            "[project]\nname = \"demo\"\n\n[ontology]\nsource = \"ontology.ttl\"\nimports = {imports}\n\n[templates]\ndir = \"templates\"\n"
        ),
    );
    write(
        dir.path(),
        "ontology.ttl",
        "@prefix ex: <http://example.org/> .\nex:alice ex:name \"alice\" .\n",
    );
    write(
        dir.path(),
        "vocab/extra.ttl",
        "@prefix ex: <http://example.org/> .\nex:bob ex:name \"bob\" .\n",
    );
    write(
        dir.path(),
        "templates/one.tmpl",
        "---\nto: out/names.txt\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{% for row in results %}{{ row.name }}\n{% endfor %}",
    );
    dir
}

#[test]
fn imported_ontology_triples_are_visible_to_sparql() {
    let dir = project("[\"vocab/extra.ttl\"]");
    sync(dir.path(), SyncOptions::default()).expect("sync with imports must succeed");
    let out = std::fs::read_to_string(dir.path().join("out/names.txt")).expect("read output");
    assert_eq!(out, "alice\nbob\n");
}

#[test]
fn without_imports_only_source_triples_are_visible() {
    let dir = project("[]");
    sync(dir.path(), SyncOptions::default()).expect("sync must succeed");
    let out = std::fs::read_to_string(dir.path().join("out/names.txt")).expect("read output");
    assert_eq!(out, "alice\n");
}

#[test]
fn missing_import_file_is_typed_refusal() {
    let dir = project("[\"vocab/absent.ttl\"]");
    let err = sync(dir.path(), SyncOptions::default()).expect_err("missing import must fail");
    let msg = err.to_string();
    assert!(msg.contains("vocab/absent.ttl"), "names the file: {msg}");
    assert!(msg.contains("[ontology].imports"), "remediation: {msg}");
    assert!(!dir.path().join("out/names.txt").exists());
}
