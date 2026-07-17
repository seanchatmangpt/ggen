//! Chicago-TDD end-to-end tests for SPARQL constructs `ggen_engine::graph`
//! must refuse loudly rather than silently mishandle
//! (specs/014-ggen-core-replacement Phase 7, Tasks 2 & 3): `GRAPH` clauses
//! (structurally detected — Task 2), SPARQL UPDATE attempts (message
//! quality on an already-unreachable path — Task 3), and SPARQL 1.1
//! federated `SERVICE` calls (already fail closed at evaluation time with
//! no service handler configured — Task 3). Real filesystem, real oxigraph
//! store, real `ggen_engine::sync::sync()` pipeline — no mocks.

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

/// A `GRAPH <...> { ... }` clause is refused loudly with `[FM-GRAPH-008]`,
/// not silently executed to an (always-empty, since this crate never loads
/// named graphs) zero-row result.
#[test]
fn graph_clause_is_refused_loudly_not_silently_empty() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    write_template(
        dir.path(),
        "graph.tmpl",
        "---\nto: out.txt\nsparql:\n  default: \"SELECT ?s ?p ?o WHERE { GRAPH <http://example.org/named> { ?s ?p ?o } }\"\n---\n{{ results | length }}",
    );

    let err = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("GRAPH clause must be refused");
    assert!(err.to_string().contains("FM-GRAPH-008"), "{err}");
    assert!(
        err.to_string().contains("GRAPH"),
        "error must name the refused construct: {err}"
    );
}

/// A `GRAPH` clause nested inside `FILTER EXISTS { ... }` is also caught —
/// not just a top-level `GRAPH` block. Regression guard for the recursive
/// expression walk in `graph::expression_has_graph_clause`.
#[test]
fn graph_clause_nested_inside_filter_exists_is_also_refused() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    write_template(
        dir.path(),
        "graph_exists.tmpl",
        "---\nto: out.txt\nsparql:\n  default: \"SELECT ?s WHERE { ?s ?p ?o . FILTER EXISTS { GRAPH <http://example.org/named> { ?s ?p ?o } } }\"\n---\n{{ results | length }}",
    );

    let err = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("GRAPH clause nested inside FILTER EXISTS must be refused");
    assert!(err.to_string().contains("FM-GRAPH-008"), "{err}");
}

/// A query with no `GRAPH` clause at all (the overwhelming common case)
/// still executes normally — regression guard that the structural
/// pre-check never false-positives on an ordinary query, including one
/// whose literal *text* happens to contain the substring `"Graph"` inside
/// a string literal (the exact false-positive a Debug-string heuristic
/// would have hit, per this task's investigation).
#[test]
fn query_with_graph_substring_inside_a_string_literal_is_not_a_false_positive() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    write_template(
        dir.path(),
        "no_graph.tmpl",
        "---\nto: out.txt\nsparql:\n  default: \"SELECT ?s WHERE { ?s ?p \\\"a Graph { thing }\\\" }\"\n---\nrows={{ results | length }}",
    );

    let report = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("must NOT be refused -- no real GRAPH clause is present, only a string literal containing the text");
    assert!(report.written.iter().any(|p| p.ends_with("out.txt")));
    let out = std::fs::read_to_string(dir.path().join("out.txt")).expect("read output");
    assert_eq!(out, "rows=0", "no match expected, but the query itself must run: {out:?}");
}

/// A SPARQL UPDATE attempt (`INSERT DATA { ... }`) is refused with a clear,
/// named message ("SPARQL UPDATE is not supported...") rather than
/// oxigraph's raw grammar-mismatch parse-error text. UPDATE is already
/// structurally unreachable via this crate's `Query`-only parse path
/// (confirmed: `spargebra::Query` has no `Update` variant) -- this test
/// proves the *message quality* improvement, not a new refusal mechanism.
#[test]
fn sparql_update_attempt_is_refused_with_a_clear_named_message() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    write_template(
        dir.path(),
        "update.tmpl",
        "---\nto: out.txt\nsparql:\n  default: \"INSERT DATA { <http://example.org/x> <http://example.org/y> <http://example.org/z> }\"\n---\n{{ results | length }}",
    );

    let err = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("SPARQL UPDATE must be refused");
    assert!(err.to_string().contains("FM-GRAPH-009"), "{err}");
    assert!(
        err.to_string().contains("SPARQL UPDATE is not supported"),
        "error must name the refused construct, not just leak oxigraph's raw grammar-mismatch text: {err}"
    );
}

/// A `DELETE WHERE { ... }` UPDATE attempt is caught by the same keyword
/// sniff (covers the `DELETE`, not just `INSERT`, branch of the
/// best-effort keyword list).
#[test]
fn sparql_delete_update_attempt_is_also_refused_with_a_clear_message() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    write_template(
        dir.path(),
        "delete_update.tmpl",
        "---\nto: out.txt\nsparql:\n  default: \"DELETE WHERE { <http://example.org/alice> ?p ?o }\"\n---\n{{ results | length }}",
    );

    let err = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("SPARQL UPDATE (DELETE) must be refused");
    assert!(err.to_string().contains("FM-GRAPH-009"), "{err}");
    assert!(err.to_string().contains("SPARQL UPDATE is not supported"), "{err}");
}

/// A SPARQL 1.1 federated `SERVICE` call fails closed at evaluation time
/// with oxigraph's own clear `UnsupportedService` error (no HTTP
/// service-handler feature or custom handler is configured on this
/// crate's evaluator) -- already a hard, clear error naturally, so this
/// test proves that behavior rather than adding a new refusal path.
#[test]
fn service_clause_fails_closed_with_a_clear_evaluation_error() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    write_template(
        dir.path(),
        "service.tmpl",
        "---\nto: out.txt\nsparql:\n  default: \"SELECT ?s ?p ?o WHERE { SERVICE <http://example.org/remote-endpoint> { ?s ?p ?o } }\"\n---\n{{ results | length }}",
    );

    let err = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("SERVICE call must fail closed (no handler configured, no live network dependency)");
    assert!(err.to_string().contains("FM-GRAPH-003"), "{err}");
    assert!(
        err.to_string().contains("not supported") || err.to_string().contains("example.org/remote-endpoint"),
        "error must surface oxigraph's own clear UnsupportedService message: {err}"
    );
}
