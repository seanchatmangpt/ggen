//! Dedicated Chicago-TDD court for `TemplateSource::Pack` (FM-GEN-007
//! family): the template half of pack-referenced resolution, isolated from
//! `QuerySource::Pack` (which `tests/generation_rules_e2e.rs` already
//! covers). Real filesystem, real SPARQL, real Tera — no mocks — driven
//! through the public [`ggen_engine::sync::sync`] entry point.
//!
//! Pins three cells:
//! 1. **positive**: a locally-pathed pack's template resolves through the
//!    pack's `package.toml` `[pack.outputs]` table and renders real query
//!    results to disk — the capability 26.7.59 refused with
//!    `[FM-GEN-007] TemplateSource::Pack is not implemented yet`.
//! 2. **fail-closed, undeclared pack**: a rule referencing a pack absent
//!    from `[[packs]]` refuses loudly, writing nothing.
//! 3. **fail-closed, non-local pack**: a pack declared with a registry
//!    (no local `path`) refuses loudly, writing nothing.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::Path;

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

const ONTOLOGY_ALICE_BOB: &str = r#"
@prefix ex: <http://example.org/> .
ex:alice ex:name "alice" .
ex:bob   ex:name "bob" .
"#;

fn write_manifest(root: &Path, packs_body: &str, rule_body: &str) {
    let manifest = format!(
        "[project]\nname = \"tpl-pack-demo\"\nversion = \"0.1.0\"\n\n[ontology]\nsource = \"ontology.ttl\"\n\n{packs_body}\n{rule_body}"
    );
    std::fs::write(root.join("ggen.toml"), manifest).expect("write ggen.toml");
}

fn write_ontology(root: &Path) {
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY_ALICE_BOB).expect("write ontology.ttl");
}

/// Writes a minimal vendored pack whose `templates` output key maps to a
/// real on-disk directory via `package.toml`'s `[pack.outputs]` table.
fn write_pack_with_template(pack_root: &Path, template_body: &str) {
    std::fs::create_dir_all(pack_root.join("real-templates-dir")).expect("mkdir pack templates");
    std::fs::write(
        pack_root.join("package.toml"),
        "[pack.outputs]\ntemplates = \"real-templates-dir\"\n",
    )
    .expect("write package.toml");
    std::fs::write(
        pack_root.join("real-templates-dir").join("names.tmpl"),
        template_body,
    )
    .expect("write pack template");
}

/// Cell 1 (positive): `template = { pack = ... }` resolves through the
/// pack's `[pack.outputs]` table and renders real query results end to
/// end. Query stays inline so this isolates the TEMPLATE half of pack
/// resolution.
#[test]
fn template_source_pack_renders_real_query_results_through_package_toml_outputs_table() {
    let dir = TempDir::new().expect("tempdir");
    let pack_root = dir.path().join("vendor").join("lsp-max");
    write_pack_with_template(
        &pack_root,
        "{% for row in results %}{{ row.name }}\n{% endfor %}",
    );

    write_manifest(
        dir.path(),
        "[[packs]]\nname = \"lsp-max\"\nregistry = \"local\"\npath = \"vendor/lsp-max\"\n",
        "[[generation.rules]]\nname = \"names_via_pack_template\"\nquery = { inline = \"SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\" }\ntemplate = { pack = \"lsp-max\", output = \"templates\", file = \"names.tmpl\" }\noutput_file = \"out/names.txt\"\n",
    );
    write_ontology(dir.path());

    let report = sync(dir.path(), SyncOptions { skip_ea_admission: true, ..Default::default() })
        .expect("TemplateSource::Pack must resolve and render, not refuse FM-GEN-007");

    assert_eq!(
        report.written,
        vec![std::path::PathBuf::from("out/names.txt")],
        "exactly the pack-templated output must be written"
    );
    let content = std::fs::read_to_string(dir.path().join("out/names.txt")).expect("read output");
    assert_eq!(
        content, "alice\nbob\n",
        "rendered content must be the REAL query results rendered through the PACK's template"
    );

    // The pack template file must be bound into the receipt closure under
    // its real resolved path — proof it was read from the pack, not inlined.
    assert!(
        report
            .closure
            .keys()
            .any(|k| k.contains("vendor/lsp-max") && k.ends_with("names.tmpl")),
        "pack template must be bound into the input closure; got: {:?}",
        report.closure.keys().collect::<Vec<_>>()
    );
}

/// Cell 2 (fail-closed): a template referencing a pack that is not declared
/// in `[[packs]]` refuses the whole sync loudly and writes nothing. Never a
/// silent skip or empty output.
#[test]
fn template_source_pack_with_undeclared_pack_is_a_typed_refusal_not_a_silent_skip() {
    let dir = TempDir::new().expect("tempdir");
    // A pack directory exists on disk, but the manifest never declares it —
    // resolution must go through the manifest's [[packs]], not directory
    // luck.
    write_pack_with_template(&dir.path().join("vendor").join("ghost-pack"), "unused");

    write_manifest(
        dir.path(),
        "",
        "[[generation.rules]]\nname = \"ghost\"\nquery = { inline = \"SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\" }\ntemplate = { pack = \"ghost-pack\", output = \"templates\", file = \"names.tmpl\" }\noutput_file = \"out/names.txt\"\n",
    );
    write_ontology(dir.path());

    let err = sync(dir.path(), SyncOptions { skip_ea_admission: true, ..Default::default() })
        .expect_err("undeclared pack must refuse, not silently skip");
    let msg = err.to_string();
    assert!(
        msg.contains("ghost-pack"),
        "refusal must name the missing pack; got: {msg}"
    );
    assert!(
        msg.contains("FM-GEN-006") || msg.contains("E0014"),
        "refusal must be a typed pack-resolution error (FM-GEN-006 or E0014); got: {msg}"
    );
    assert!(
        !dir.path().join("out/names.txt").exists(),
        "no output may be written on refusal"
    );
}

/// Cell 3 (fail-closed): a pack declared with a non-local registry (no
/// `path`) has no on-disk root to resolve a template against — loud typed
/// refusal with remediation, nothing written.
#[test]
fn template_source_pack_with_non_local_registry_is_a_typed_refusal() {
    let dir = TempDir::new().expect("tempdir");
    write_manifest(
        dir.path(),
        "[[packs]]\nname = \"remote-pack\"\nregistry = \"marketplace\"\n",
        "[[generation.rules]]\nname = \"remote\"\nquery = { inline = \"SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\" }\ntemplate = { pack = \"remote-pack\", output = \"templates\", file = \"names.tmpl\" }\noutput_file = \"out/names.txt\"\n",
    );
    write_ontology(dir.path());

    let err = sync(dir.path(), SyncOptions { skip_ea_admission: true, ..Default::default() })
        .expect_err("non-local pack must refuse, not silently skip");
    let msg = err.to_string();
    assert!(msg.contains("FM-GEN-006"), "got: {msg}");
    assert!(msg.contains("no local"), "got: {msg}");
    assert!(msg.contains("remote-pack"), "got: {msg}");
    assert!(!dir.path().join("out/names.txt").exists());
}
