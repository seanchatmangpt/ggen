//! Fortune 5 frontmatter hardening: bounded multiplication, admitted paths,
//! receipt-bound governing inputs, coherent authority, and fail-closed ownership.

use std::path::Path;

use ggen_engine::sync::{sync, SyncOptions, MAX_QUERY_RESULT_ROWS};
use tempfile::TempDir;

const GGEN_TOML: &str = r#"
[project]
name = "fortune5"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

fn scaffold(root: &Path, ontology: &str) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("manifest");
    std::fs::write(root.join("ontology.ttl"), ontology).expect("ontology");
    std::fs::create_dir_all(root.join("templates")).expect("templates");
}

fn template(root: &Path, name: &str, source: &str) {
    std::fs::write(root.join("templates").join(name), source).expect("template");
}

#[test]
fn excessive_query_rows_refuse_before_hook_or_write() {
    let dir = TempDir::new().expect("tempdir");
    let mut ontology = String::from("@prefix ex: <http://example.org/> .\n");
    for index in 0..=MAX_QUERY_RESULT_ROWS {
        ontology.push_str(&format!("ex:e{index} ex:name \"n{index}\" .\n"));
    }
    scaffold(dir.path(), &ontology);
    template(
        dir.path(),
        "rows.tmpl",
        r#"---
to: "out/{{ row.name }}.txt"
sparql:
  entities: |
    PREFIX ex: <http://example.org/>
    SELECT ?name WHERE { ?s ex:name ?name } ORDER BY ?name
for_each: entities
sh_before: "echo ran > hook.log"
---
{{ row.name }}
"#,
    );

    let error = sync(dir.path(), SyncOptions::default()).expect_err("must refuse");
    assert!(error.to_string().contains("FM-TPL-021"), "{error}");
    assert!(!dir.path().join("hook.log").exists());
    assert!(!dir.path().join("out").exists());
}

#[test]
fn unsafe_skip_empty_target_is_admitted_before_skip() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), "@prefix ex: <http://example.org/> .\n");
    template(
        dir.path(),
        "unsafe.tmpl",
        "---\nto: ../escaped.txt\nskip_empty: true\n---\n   \n",
    );

    let error = sync(dir.path(), SyncOptions::default()).expect_err("must refuse");
    assert!(error.to_string().contains("FM-WRITE-002"), "{error}");
    assert!(!dir
        .path()
        .parent()
        .expect("parent")
        .join("escaped.txt")
        .exists());
}

#[test]
fn canonical_target_aliases_are_duplicate_outputs() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), "@prefix ex: <http://example.org/> .\n");
    template(dir.path(), "a.tmpl", "---\nto: out.txt\n---\na\n");
    template(dir.path(), "b.tmpl", "---\nto: ./out.txt\n---\nb\n");

    let error = sync(dir.path(), SyncOptions::default()).expect_err("must refuse");
    assert!(error.to_string().contains("FM-WRITE-008"), "{error}");
    assert!(!dir.path().join("out.txt").exists());
}

#[test]
fn shape_is_regular_safe_and_receipt_bound() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), "@prefix ex: <http://example.org/> .\n");
    std::fs::create_dir_all(dir.path().join("shapes")).expect("shape dir");
    let shape = b"@prefix sh: <http://www.w3.org/ns/shacl#> .\n";
    std::fs::write(dir.path().join("shapes/policy.ttl"), shape).expect("shape");
    template(
        dir.path(),
        "shape.tmpl",
        "---\nto: out.txt\nshape:\n  - shapes/policy.ttl\n---\nbody\n",
    );

    let report = sync(dir.path(), SyncOptions::default()).expect("sync");
    let expected = blake3::hash(shape).to_hex().to_string();
    assert_eq!(report.closure.get("shapes/policy.ttl"), Some(&expected));
}

#[test]
fn shape_directory_refuses_before_hook() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), "@prefix ex: <http://example.org/> .\n");
    std::fs::create_dir_all(dir.path().join("shapes")).expect("shape dir");
    template(
        dir.path(),
        "shape-dir.tmpl",
        "---\nto: out.txt\nshape:\n  - shapes\nsh_before: \"echo ran > hook.log\"\n---\nbody\n",
    );

    let error = sync(dir.path(), SyncOptions::default()).expect_err("must refuse");
    assert!(error.to_string().contains("FM-TPL-014"), "{error}");
    assert!(!dir.path().join("hook.log").exists());
    assert!(!dir.path().join("out.txt").exists());
}

#[test]
fn placement_without_injection_refuses_before_hook() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), "@prefix ex: <http://example.org/> .\n");
    std::fs::write(dir.path().join("out.txt"), "// SLOT\n").expect("host");
    template(
        dir.path(),
        "selector.tmpl",
        "---\nto: out.txt\nbefore: \"// SLOT\"\nsh_before: \"echo ran > hook.log\"\n---\nbody\n",
    );

    let error = sync(dir.path(), SyncOptions::default()).expect_err("must refuse");
    assert!(error.to_string().contains("FM-WRITE-010"), "{error}");
    assert!(!dir.path().join("hook.log").exists());
}

#[test]
fn unreadable_checksum_state_refuses_before_hook() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), "@prefix ex: <http://example.org/> .\n");
    std::fs::write(dir.path().join("out.txt"), "old\n").expect("target");
    std::fs::create_dir_all(dir.path().join(".ggen-v2/freeze/out.txt.blake3"))
        .expect("directory where checksum file must be");
    template(
        dir.path(),
        "checksum.tmpl",
        "---\nto: out.txt\nforce: true\nfreeze_policy: checksum\nfreeze_slots_dir: .ggen-v2/freeze\nsh_before: \"echo ran > hook.log\"\n---\nnew\n",
    );

    let error = sync(dir.path(), SyncOptions::default()).expect_err("must refuse");
    assert!(error.to_string().contains("FM-WRITE-011"), "{error}");
    assert!(!dir.path().join("hook.log").exists());
    assert_eq!(
        std::fs::read_to_string(dir.path().join("out.txt")).unwrap(),
        "old\n"
    );
}
