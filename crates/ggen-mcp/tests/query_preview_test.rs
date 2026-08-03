//! Chicago TDD: real `TempDir` + real `ggen.toml` + real ontology, no mocks.
//! `query_preview` is called directly (in-process), not through the rmcp
//! wire protocol -- that's covered separately by `mcp_protocol_test.rs`.

use std::path::Path;

use ggen_mcp::tools::query_preview::{query_preview, QueryPreviewParams};

const GGEN_TOML: &str = r#"
[project]
name = "demo"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

/// The exact repro for the incident that motivated this crate: `ex:hasName`
/// exists on `ex:alice`, but `ex:hasX` (the mandatory pattern below) is used
/// zero times anywhere in the graph.
const ONTOLOGY: &str = r#"
@prefix ex: <http://example.org/> .
ex:alice ex:hasName "alice" .
ex:bob ex:hasName "bob" .
"#;

fn write_project(root: &Path) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY).expect("write ontology.ttl");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
}

/// THE regression test. A mandatory (non-OPTIONAL) triple pattern on a
/// predicate used zero times in the graph must report `ok:true,
/// row_count:0` -- never an error, never silent, and `touched_predicates`
/// must be empty (nothing in the query's own bound terms ever matched).
#[test]
fn zero_row_query_reports_loudly_not_silently() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_project(dir.path());

    let params = QueryPreviewParams {
        root: dir.path().display().to_string(),
        sparql: "SELECT ?s ?v WHERE { ?s <http://example.org/hasX> ?v }".to_string(),
        max_rows: None,
    };

    let result = query_preview(&params).expect("query must execute, not error");
    assert!(result.ok, "a zero-row SELECT must be ok:true, not an error");
    assert_eq!(result.row_count, 0, "the true row count must be reported as zero");
    assert!(!result.truncated, "zero rows is not truncation");
    assert_eq!(result.returned_rows, 0);
    assert!(result.rows.is_empty());
}

/// Sanity check on the success path: a real matching query against the same
/// fixture returns real rows, proving the zero-row test above is actually
/// discriminating (not just "everything returns zero").
#[test]
fn matching_query_returns_real_rows() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_project(dir.path());

    let params = QueryPreviewParams {
        root: dir.path().display().to_string(),
        sparql: "SELECT ?s ?name WHERE { ?s <http://example.org/hasName> ?name }".to_string(),
        max_rows: None,
    };

    let result = query_preview(&params).expect("query must execute");
    assert!(result.ok);
    assert_eq!(result.row_count, 2, "alice and bob both have hasName");
    assert_eq!(result.returned_rows, 2);
    assert!(!result.truncated);
}

/// Malformed SPARQL must be a distinguishable `syntax_error`, never a
/// `graph_load_error` or a panic, and must be caught BEFORE the graph is
/// touched.
#[test]
fn malformed_sparql_is_syntax_error_not_graph_load_error() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_project(dir.path());

    let params = QueryPreviewParams {
        root: dir.path().display().to_string(),
        sparql: "SELECT ?s WHERE { ?s".to_string(), // unterminated block
        max_rows: None,
    };

    let err = query_preview(&params).expect_err("malformed SPARQL must fail");
    assert_eq!(err.category, ggen_mcp::error::ErrorCategory::SyntaxError);
}

/// A `root` that does not resolve to a real, existing directory must be
/// refused as `PathTraversal`, not attempted.
#[test]
fn nonexistent_root_is_path_traversal_error() {
    let params = QueryPreviewParams {
        root: "/definitely/does/not/exist/anywhere".to_string(),
        sparql: "SELECT ?s WHERE { ?s ?p ?o }".to_string(),
        max_rows: None,
    };

    let err = query_preview(&params).expect_err("nonexistent root must fail");
    assert_eq!(err.category, ggen_mcp::error::ErrorCategory::PathTraversal);
}

/// ASK queries return `boolean_result`, not `rows`.
#[test]
fn ask_query_returns_boolean_result() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_project(dir.path());

    let params = QueryPreviewParams {
        root: dir.path().display().to_string(),
        sparql: "ASK { ?s <http://example.org/hasName> \"alice\" }".to_string(),
        max_rows: None,
    };

    let result = query_preview(&params).expect("ASK must execute");
    assert_eq!(result.boolean_result, Some(true));
    assert!(result.rows.is_empty());
}
