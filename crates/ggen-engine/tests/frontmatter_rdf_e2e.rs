//! Chicago-TDD end-to-end tests for the Gap 1 frontmatter-level RDF loading
//! fields (`rdf:`, `rdf_inline:`, `prefixes:`, `base:`) and their per-template
//! overlay-graph semantics in `ggen_engine::sync`. Real filesystem, real sync
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

/// Base project ontology: present in every template's overlay (overlay =
/// base graph's triples PLUS the template's own rdf/rdf_inline content).
const ONTOLOGY: &str = r#"
@prefix ex: <http://example.org/> .
ex:alice ex:name "alice" .
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

/// (a) `rdf: [other.ttl]` — the template sees triples from that file via a
/// SPARQL query, layered over (not replacing) the base project graph.
#[test]
fn rdf_file_field_loads_sibling_ttl_and_layers_over_base_graph() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    std::fs::write(
        dir.path().join("templates/other.ttl"),
        "@prefix ex: <http://example.org/> .\nex:bob ex:city \"Paris\" .\n",
    )
    .expect("write other.ttl");
    write_template(
        dir.path(),
        "a.tmpl",
        "---\nto: a_out.txt\nrdf:\n  - other.ttl\nsparql:\n  city: \"PREFIX ex: <http://example.org/> SELECT ?city WHERE { ex:bob ex:city ?city }\"\n  base_check: \"PREFIX ex: <http://example.org/> SELECT ?name WHERE { ex:alice ex:name ?name }\"\n---\n{% for row in city %}{{ row.city }}{% endfor %}|{% for row in base_check %}{{ row.name }}{% endfor %}",
    );

    run_sync(dir.path());

    assert_eq!(
        std::fs::read_to_string(dir.path().join("a_out.txt")).expect("output"),
        "Paris|alice",
        "the overlay must contain both the rdf: file's triple (Paris) and \
         the base project graph's triple (alice)"
    );
}

/// (b) `rdf_inline:` with literal inline Turtle text is queryable.
#[test]
fn rdf_inline_field_is_queryable() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    write_template(
        dir.path(),
        "b.tmpl",
        "---\nto: b_out.txt\nrdf_inline:\n  - \"@prefix ex: <http://example.org/> . ex:carol ex:city \\\"Berlin\\\" .\"\nsparql:\n  city: \"PREFIX ex: <http://example.org/> SELECT ?city WHERE { ex:carol ex:city ?city }\"\n---\n{% for row in city %}{{ row.city }}{% endfor %}",
    );

    run_sync(dir.path());

    assert_eq!(
        std::fs::read_to_string(dir.path().join("b_out.txt")).expect("output"),
        "Berlin"
    );
}

/// (c) `prefixes:` + `base:` correctly expand a prefixed/relative IRI in
/// `rdf_inline:` content that declares neither itself — proving the
/// expansion comes from the frontmatter fields, not from the inline text.
#[test]
fn prefixes_and_base_expand_relative_and_prefixed_iris_in_rdf_inline() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    write_template(
        dir.path(),
        "c.tmpl",
        "---\nto: c_out.txt\nbase: \"http://example.org/\"\nprefixes:\n  ex: \"http://example.org/vocab#\"\nrdf_inline: \"<dave> ex:city \\\"Tokyo\\\" .\"\nsparql:\n  city: \"PREFIX ex: <http://example.org/vocab#> SELECT ?city WHERE { <http://example.org/dave> ex:city ?city }\"\n---\n{% for row in city %}{{ row.city }}{% endfor %}",
    );

    run_sync(dir.path());

    assert_eq!(
        std::fs::read_to_string(dir.path().join("c_out.txt")).expect("output"),
        "Tokyo",
        "`<dave>` must resolve against `base:` to <http://example.org/dave>, and \
         `ex:city` must resolve against `prefixes:` to <http://example.org/vocab#city>"
    );
}

/// (d) An `rdf:` path traversal escape attempt is refused loudly — never a
/// silent skip — with the same class of error `from:` produces.
#[test]
fn rdf_path_traversal_escape_is_refused() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    std::fs::write(
        dir.path().join("secret.ttl"),
        "@prefix ex: <http://example.org/> .\nex:s ex:p ex:o .\n",
    )
    .expect("write secret");
    write_template(
        dir.path(),
        "d.tmpl",
        "---\nto: d_out.txt\nrdf:\n  - ../secret.ttl\n---\nbody\n",
    );

    let err = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("traversal outside the template's own directory must be refused");
    let msg = err.to_string();
    assert!(msg.contains("FM-TPL-018"), "{msg}");
    assert!(
        msg.contains("rdf:"),
        "error should name the rdf: field: {msg}"
    );
    assert!(
        !dir.path().join("d_out.txt").exists(),
        "no output must be written on refusal"
    );
}

/// (e) Overlay isolation: two templates in the same sync run declaring
/// DIFFERENT `rdf:` files must not see each other's extra triples.
#[test]
fn different_templates_rdf_overlays_are_isolated_from_each_other() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    std::fs::write(
        dir.path().join("templates/e1_extra.ttl"),
        "@prefix ex: <http://example.org/> .\nex:thing1 ex:marker \"e1\" .\n",
    )
    .expect("write e1 extra");
    std::fs::write(
        dir.path().join("templates/e2_extra.ttl"),
        "@prefix ex: <http://example.org/> .\nex:thing2 ex:marker \"e2\" .\n",
    )
    .expect("write e2 extra");
    write_template(
        dir.path(),
        "e1.tmpl",
        "---\nto: e1_out.txt\nrdf:\n  - e1_extra.ttl\nsparql:\n  own: \"PREFIX ex: <http://example.org/> SELECT ?s WHERE { ?s ex:marker \\\"e1\\\" }\"\n  other: \"PREFIX ex: <http://example.org/> SELECT ?s WHERE { ?s ex:marker \\\"e2\\\" }\"\n---\nown={{ own | length }};other={{ other | length }}",
    );
    write_template(
        dir.path(),
        "e2.tmpl",
        "---\nto: e2_out.txt\nrdf:\n  - e2_extra.ttl\nsparql:\n  own: \"PREFIX ex: <http://example.org/> SELECT ?s WHERE { ?s ex:marker \\\"e2\\\" }\"\n  other: \"PREFIX ex: <http://example.org/> SELECT ?s WHERE { ?s ex:marker \\\"e1\\\" }\"\n---\nown={{ own | length }};other={{ other | length }}",
    );

    run_sync(dir.path());

    assert_eq!(
        std::fs::read_to_string(dir.path().join("e1_out.txt")).expect("e1 output"),
        "own=1;other=0",
        "e1's overlay must see its own extra triple but not e2's"
    );
    assert_eq!(
        std::fs::read_to_string(dir.path().join("e2_out.txt")).expect("e2 output"),
        "own=1;other=0",
        "e2's overlay must see its own extra triple but not e1's"
    );
}
