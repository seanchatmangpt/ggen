//! Chicago-TDD end-to-end tests for datatype-aware RDF term → Tera value
//! coercion (`graph::term_to_engine_value` / `template::solutions_to_values`,
//! specs/014-ggen-core-replacement Phase 7, Task 1). Real filesystem, real
//! oxigraph store, real Tera rendering, real `ggen_engine::sync::sync()`
//! pipeline — no mocks. Mirrors `frontmatter_fields_e2e.rs`'s conventions.

use std::path::Path;

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

const GGEN_TOML: &str = r#"
[project]
name = "demo"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

/// One subject per datatype under test, plus one deliberately malformed
/// literal (`ex:bad ex:n "not-a-number"^^xsd:integer`) and one plain
/// untyped string literal for the backward-compatibility regression guard.
const ONTOLOGY: &str = r#"
@prefix ex: <http://example.org/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:answer ex:n "42"^^xsd:integer .
ex:flagTrue ex:flag "true"^^xsd:boolean .
ex:flagFalse ex:flag "false"^^xsd:boolean .
ex:price ex:amount "19.99"^^xsd:decimal .
ex:bad ex:n "not-a-number"^^xsd:integer .
ex:plain ex:label "hello" .
"#;

fn scaffold(root: &Path) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY).expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
}

fn write_template(root: &Path, name: &str, content: &str) {
    std::fs::write(root.join("templates").join(name), content).expect("write template");
}

fn run_sync(root: &Path) -> ggen_engine::sync::SyncReport {
    sync(
        root,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync")
}

/// `xsd:integer` renders correctly through a real Tera arithmetic
/// expression (`{{ row.n + 1 }}`) — proving the cell is a real
/// `tera::Value::Number`, not a string (`"42" + 1` would be a Tera render
/// error, not `43`, if the coercion regressed to plain strings).
#[test]
fn xsd_integer_renders_through_arithmetic_proving_it_is_a_real_number() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    write_template(
        dir.path(),
        "int.tmpl",
        "---\nto: out/int.txt\nsparql:\n  default: SELECT ?n WHERE { <http://example.org/answer> <http://example.org/n> ?n }\n---\n{% for row in results %}{{ row.n + 1 }}{% endfor %}",
    );

    run_sync(dir.path());
    let out = std::fs::read_to_string(dir.path().join("out/int.txt")).expect("read output");
    assert_eq!(
        out, "43",
        "xsd:integer must arithmetic-add as a real number: {out:?}"
    );
}

/// `xsd:boolean` correctly gates `{% if %}`/`{% else %}` for BOTH `true`
/// and `false` — proving the string `"false"` is not truthy (a Tera
/// `{% if %}` on a non-empty string is always true, so this is a real
/// regression guard against the pre-Task-1 all-strings behavior).
#[test]
fn xsd_boolean_gates_if_else_correctly_for_both_true_and_false() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    write_template(
        dir.path(),
        "bool.tmpl",
        "---\nto: out/bool.txt\nsparql:\n  default: SELECT ?flag WHERE { ?s <http://example.org/flag> ?flag } ORDER BY ?flag\n---\n{% for row in results %}{% if row.flag %}YES{% else %}NO{% endif %};{% endfor %}",
    );

    run_sync(dir.path());
    let out = std::fs::read_to_string(dir.path().join("out/bool.txt")).expect("read output");
    // ORDER BY ?flag sorts lexically: "false" < "true", so false-row first.
    assert_eq!(
        out, "NO;YES;",
        "xsd:boolean \"false\" must be falsy and \"true\" truthy: {out:?}"
    );
}

/// `xsd:decimal` renders correctly through `{{ row.price * 2 }}` — proving
/// it's a real float, not a string.
#[test]
fn xsd_decimal_renders_through_multiplication_proving_it_is_a_real_number() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    write_template(
        dir.path(),
        "dec.tmpl",
        "---\nto: out/dec.txt\nsparql:\n  default: SELECT ?amount WHERE { <http://example.org/price> <http://example.org/amount> ?amount }\n---\n{% for row in results %}{{ row.amount * 2 }}{% endfor %}",
    );

    run_sync(dir.path());
    let out = std::fs::read_to_string(dir.path().join("out/dec.txt")).expect("read output");
    assert_eq!(
        out, "39.98",
        "xsd:decimal must arithmetic-multiply as a real number: {out:?}"
    );
}

/// A literal declared `xsd:integer` with malformed content
/// (`"not-a-number"`) falls back to a plain string instead of crashing the
/// sync — the value round-trips as the original lexical text, and the sync
/// as a whole still succeeds (proving the fallback is graceful, not a hard
/// error).
#[test]
fn malformed_integer_literal_falls_back_to_string_without_crashing_sync() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    write_template(
        dir.path(),
        "bad.tmpl",
        "---\nto: out/bad.txt\nsparql:\n  default: SELECT ?n WHERE { <http://example.org/bad> <http://example.org/n> ?n }\n---\n{% for row in results %}{{ row.n }}{% endfor %}",
    );

    // Must not error/panic -- the malformed literal is a data-quality issue
    // in the source graph, not a reason to fail the whole sync.
    let report = run_sync(dir.path());
    assert!(
        report.written.iter().any(|p| p.ends_with("out/bad.txt")),
        "sync must still write the output despite the malformed literal: {:?}",
        report.written
    );
    let out = std::fs::read_to_string(dir.path().join("out/bad.txt")).expect("read output");
    assert_eq!(
        out, "not-a-number",
        "malformed xsd:integer literal must round-trip as its original lexical string: {out:?}"
    );
}

/// An untyped plain-string literal renders exactly as before Task 1
/// (backward-compatibility regression guard for every existing
/// `{{ row.field }}`-style template in the repo).
#[test]
fn untyped_plain_string_literal_still_renders_as_before() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    write_template(
        dir.path(),
        "plain.tmpl",
        "---\nto: out/plain.txt\nsparql:\n  default: SELECT ?label WHERE { <http://example.org/plain> <http://example.org/label> ?label }\n---\n{% for row in results %}{{ row.label }}{% endfor %}",
    );

    run_sync(dir.path());
    let out = std::fs::read_to_string(dir.path().join("out/plain.txt")).expect("read output");
    assert_eq!(
        out, "hello",
        "untyped plain-string literal must render unchanged: {out:?}"
    );
}
