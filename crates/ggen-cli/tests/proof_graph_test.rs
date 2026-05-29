//! Executable-proof integration tests for the `ggen graph` CLI verbs.
//!
//! Chicago TDD — REAL binary, REAL Oxigraph, REAL filesystem. NO mocks, NO doubles.
//!
//! Each test invokes the real `ggen` binary via `assert_cmd`, sets up REAL RDF
//! state in a `TempDir`, and asserts on OBSERVABLE results:
//!   - process exit code
//!   - stdout JSON content (the verb returns a `#[derive(Serialize)]` struct which
//!     clap-noun-verb v26.5.19 serializes to JSON and prints to stdout — see
//!     `cli/registry.rs:739` `let json = output.to_json()?; println!("{}", json);`)
//!   - files actually written to disk
//!
//! These tests only pass if the command genuinely performs the work. The asserted
//! values (triple counts, query bindings, written file contents) cannot be produced
//! without real Oxigraph ingestion, real SPARQL execution, and real serialization.
//!
//! ## CLI surface (derived from source, not guessed)
//!
//! Verb fns live in `crates/ggen-cli/src/cmds/graph.rs`. clap-noun-verb-macros
//! v26.5.19 maps every fn parameter to a `--long` flag using the *verbatim
//! snake_case identifier* (see `cli/registry.rs:513` `clap::Arg::new(arg_name).long(arg_name)`
//! and macro `lib.rs:1040` `arg_name = ident.to_string()`). Parameters are flags,
//! not positionals, unless explicitly annotated `#[arg(positional = N)]` — none of
//! the graph verbs are. Hence the flag names below use underscores.
//!
//!   graph load     --file <PATH> [--format <FMT>]                 (graph.rs:100)
//!   graph query    --sparql_query <Q> [--graph_file <PATH>] [--format <FMT>]  (graph.rs:132)
//!   graph export   --input_file <PATH> --output <PATH> --format <FMT>         (graph.rs:164)
//!   graph visualize --input_file <PATH> [--format <FMT>]          (graph.rs:196)
//!   graph validate --schema_file <PATH> [--strict]                (graph.rs:59)

use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;
use tempfile::TempDir;

/// Build a fresh invocation of the real `ggen` binary.
fn ggen() -> Command {
    Command::cargo_bin("ggen").expect("ggen binary should build")
}

/// Write a small, real RDF/Turtle file with a known, countable number of triples.
///
/// Five triples total:
///   ex:alice a foaf:Person          (1)
///   ex:alice foaf:name "Alice"      (2)
///   ex:alice foaf:knows ex:bob      (3)
///   ex:bob   a foaf:Person          (4)
///   ex:bob   foaf:name "Bob"        (5)
fn write_data_ttl(dir: &TempDir) -> std::path::PathBuf {
    let path = dir.path().join("data.ttl");
    let ttl = r#"@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:alice a foaf:Person ;
    foaf:name "Alice" ;
    foaf:knows ex:bob .

ex:bob a foaf:Person ;
    foaf:name "Bob" .
"#;
    fs::write(&path, ttl).expect("write data.ttl");
    path
}

/// Write a real OWL ontology in the `http://example.org#` namespace that the
/// validate verb extracts against (it hardcodes that namespace at graph.rs:67).
///
/// Two owl:Class declarations (Product, Order) and three properties so that
/// `classes_count` and `properties_count` are provably non-zero.
fn write_ontology_ttl(dir: &TempDir) -> std::path::PathBuf {
    let path = dir.path().join("schema.ttl");
    let ttl = r#"@prefix ex: <http://example.org#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:Product a owl:Class ;
    rdfs:label "Product" ;
    rdfs:comment "A product in the catalog" .

ex:Order a owl:Class ;
    rdfs:label "Order" ;
    rdfs:comment "A customer order" .

ex:name a owl:DatatypeProperty ;
    rdfs:domain ex:Product ;
    rdfs:range xsd:string ;
    rdfs:label "Name" .

ex:price a owl:DatatypeProperty ;
    rdfs:domain ex:Product ;
    rdfs:range xsd:decimal ;
    rdfs:label "Price" .

ex:quantity a owl:DatatypeProperty ;
    rdfs:domain ex:Order ;
    rdfs:range xsd:integer ;
    rdfs:label "Quantity" .
"#;
    fs::write(&path, ttl).expect("write schema.ttl");
    path
}

// ============================================================================
// load — real Oxigraph ingestion, reports real triple count
// ============================================================================

/// PROOF: `graph load` ingests the Turtle file through Oxigraph and reports the
/// REAL triple count. The fixture has exactly 5 triples; the verb's `LoadOutput`
/// (graph.rs:14) is serialized to compact JSON on stdout, so it must contain
/// `"triples_loaded":5` and `"total_triples":5`. A stub that fakes success
/// without parsing could not produce the correct count.
#[test]
fn graph_load_imports_ttl_and_reports_triple_count() {
    let dir = TempDir::new().unwrap();
    let data = write_data_ttl(&dir);

    ggen()
        .arg("graph")
        .arg("load")
        .arg("--file")
        .arg(data.to_str().unwrap())
        .current_dir(&dir)
        .assert()
        .success()
        .stdout(predicate::str::contains("\"triples_loaded\":5"))
        .stdout(predicate::str::contains("\"total_triples\":5"))
        .stdout(predicate::str::contains("Turtle"));
}

/// PROOF (negative path): loading a non-existent file must FAIL loudly with a
/// non-zero exit and a "not found" message (load.rs:96 `bail!("RDF file not found")`),
/// not fail-open with a fake success.
#[test]
fn graph_load_missing_file_fails_loudly() {
    let dir = TempDir::new().unwrap();

    ggen()
        .arg("graph")
        .arg("load")
        .arg("--file")
        .arg(dir.path().join("does-not-exist.ttl").to_str().unwrap())
        .current_dir(&dir)
        .assert()
        .failure()
        .stderr(predicate::str::contains("not found").or(predicate::str::contains("Load failed")));
}

// ============================================================================
// query — real SPARQL execution against a real loaded graph
// ============================================================================

/// PROOF: `graph query` loads the file given via `--graph_file` and runs a REAL
/// SPARQL SELECT through Oxigraph (query.rs:80 `execute_sparql`). The fixture has
/// two foaf:Person subjects each with a foaf:name, so the SELECT returns exactly
/// 2 bindings. The serialized `QueryOutput` (graph.rs:22) must contain
/// `"result_count": 2` and the real literal values "Alice" and "Bob". An empty /
/// faked graph would yield `result_count: 0` and fail this test.
#[test]
fn graph_query_returns_bindings_from_loaded_graph() {
    let dir = TempDir::new().unwrap();
    let data = write_data_ttl(&dir);

    let sparql = r#"PREFIX foaf: <http://xmlns.com/foaf/0.1/>
SELECT ?name WHERE { ?p a foaf:Person ; foaf:name ?name } ORDER BY ?name"#;

    ggen()
        .arg("graph")
        .arg("query")
        .arg("--sparql_query")
        .arg(sparql)
        .arg("--graph_file")
        .arg(data.to_str().unwrap())
        .current_dir(&dir)
        .assert()
        .success()
        .stdout(predicate::str::contains("\"result_count\":2"))
        .stdout(predicate::str::contains("Alice"))
        .stdout(predicate::str::contains("Bob"));
}

/// PROOF: an ASK query over the real graph returns a real boolean. The graph
/// contains foaf:knows triples, so `ASK { ?s foaf:knows ?o }` is true and the
/// serialized binding must contain "result" => "true" (query.rs:127).
#[test]
fn graph_query_ask_returns_real_boolean() {
    let dir = TempDir::new().unwrap();
    let data = write_data_ttl(&dir);

    ggen()
        .arg("graph")
        .arg("query")
        .arg("--sparql_query")
        .arg("PREFIX foaf: <http://xmlns.com/foaf/0.1/> ASK { ?s foaf:knows ?o }")
        .arg("--graph_file")
        .arg(data.to_str().unwrap())
        .current_dir(&dir)
        .assert()
        .success()
        .stdout(predicate::str::contains("result"))
        .stdout(predicate::str::contains("true"));
}

// ============================================================================
// export — real RDF serialization, writes a real file
// ============================================================================

/// PROOF: `graph export` loads the input graph, serializes it via Oxigraph's
/// RdfSerializer, and WRITES a real file (export.rs:113 `export_graph`). We assert
/// on durable state: the output file exists, is non-empty, contains the real
/// subject IRIs from the source graph, and stdout reports `triples_exported: 5`.
/// A decorative stub that printed success without writing would fail the
/// `output.exists()` assertion.
#[test]
fn graph_export_writes_serialized_file_with_real_triples() {
    let dir = TempDir::new().unwrap();
    let data = write_data_ttl(&dir);
    let out = dir.path().join("exported.ttl");

    ggen()
        .arg("graph")
        .arg("export")
        .arg("--input_file")
        .arg(data.to_str().unwrap())
        .arg("--output")
        .arg(out.to_str().unwrap())
        .arg("--format")
        .arg("turtle")
        .current_dir(&dir)
        .assert()
        .success()
        .stdout(predicate::str::contains("\"triples_exported\":5"));

    // Durable state: the export file was actually written.
    assert!(out.exists(), "export must write a real output file");
    let content = fs::read_to_string(&out).expect("read exported file");
    assert!(!content.is_empty(), "exported file must have content");
    // The serialized graph must reference the real subjects from the source.
    assert!(
        content.contains("http://example.org/alice") || content.contains("alice"),
        "export must contain real serialized triples, got:\n{content}"
    );
    assert!(
        content.contains("http://example.org/bob") || content.contains("bob"),
        "export must contain real serialized triples, got:\n{content}"
    );
}

/// PROOF: `graph export` to N-Triples writes a real N-Triples file. N-Triples
/// uses full-IRI angle-bracket syntax, so the file must contain the absolute IRIs.
#[test]
fn graph_export_ntriples_writes_full_iris() {
    let dir = TempDir::new().unwrap();
    let data = write_data_ttl(&dir);
    let out = dir.path().join("exported.nt");

    ggen()
        .arg("graph")
        .arg("export")
        .arg("--input_file")
        .arg(data.to_str().unwrap())
        .arg("--output")
        .arg(out.to_str().unwrap())
        .arg("--format")
        .arg("ntriples")
        .current_dir(&dir)
        .assert()
        .success();

    assert!(out.exists(), "n-triples export file must exist");
    let content = fs::read_to_string(&out).expect("read nt file");
    assert!(
        content.contains("<http://example.org/alice>"),
        "n-triples must use full IRIs, got:\n{content}"
    );
    assert!(
        content.contains("<http://xmlns.com/foaf/0.1/name>"),
        "n-triples must contain the foaf:name predicate IRI, got:\n{content}"
    );
}

// ============================================================================
// visualize — real graph traversal, writes a real DOT/JSON artifact
// ============================================================================

/// PROOF: `graph visualize` loads the graph, extracts nodes/edges via real SPARQL
/// (visualize.rs:210/252), generates DOT, and WRITES it. The CLI passes
/// `output: None` (graph.rs:202), so the artifact is written next to the input with
/// the format extension swapped (visualize.rs:178 / execute.rs:489): `data.dot`.
/// We assert the file exists and contains real Graphviz DOT (`digraph RDF`) plus
/// the real subject IRIs, and that stdout reports a non-zero node count.
#[test]
fn graph_visualize_writes_dot_artifact_with_real_nodes() {
    let dir = TempDir::new().unwrap();
    let data = write_data_ttl(&dir);

    ggen()
        .arg("graph")
        .arg("visualize")
        .arg("--input_file")
        .arg(data.to_str().unwrap())
        .arg("--format")
        .arg("dot")
        .current_dir(&dir)
        .assert()
        .success()
        // Two distinct subjects (ex:alice, ex:bob) => 2 nodes rendered.
        .stdout(predicate::str::contains("\"nodes_rendered\":2"));

    // Durable state: DOT artifact written next to input (extension swapped).
    let dot = dir.path().join("data.dot");
    assert!(
        dot.exists(),
        "visualize must write a real .dot artifact at {dot:?}"
    );
    let content = fs::read_to_string(&dot).expect("read dot file");
    assert!(
        content.contains("digraph RDF"),
        "artifact must be real Graphviz DOT, got:\n{content}"
    );
    assert!(
        content.contains("http://example.org/alice"),
        "DOT must contain real graph node IRIs, got:\n{content}"
    );
}

/// PROOF: `graph visualize --format json` writes a real D3-style JSON artifact
/// (visualize.rs:352 `generate_json`) with `nodes` and `edges` arrays containing
/// the real subjects/predicates.
#[test]
fn graph_visualize_json_writes_node_edge_json() {
    let dir = TempDir::new().unwrap();
    let data = write_data_ttl(&dir);

    ggen()
        .arg("graph")
        .arg("visualize")
        .arg("--input_file")
        .arg(data.to_str().unwrap())
        .arg("--format")
        .arg("json")
        .current_dir(&dir)
        .assert()
        .success();

    let json = dir.path().join("data.json");
    assert!(
        json.exists(),
        "visualize json must write a real artifact at {json:?}"
    );
    let content = fs::read_to_string(&json).expect("read json artifact");
    assert!(content.contains("\"nodes\""), "json must have nodes array");
    assert!(content.contains("\"edges\""), "json must have edges array");
    assert!(
        content.contains("http://example.org/alice"),
        "json must contain real node IRIs, got:\n{content}"
    );
}

// ============================================================================
// validate — real ontology extraction + schema validation
// ============================================================================

/// PROOF: `graph validate` extracts the ontology schema via real Oxigraph + SPARQL
/// (graph.rs:67 -> ontology::extract_ontology_schema, namespace "http://example.org#")
/// and validates it. The fixture declares 2 owl:Class and 3 properties in that exact
/// namespace, so the serialized `ValidateOutput` (graph.rs:45) must report
/// `"classes_count": 2`, `"properties_count": 3`, and `"is_valid": true`. A stub
/// that fabricated success without extracting could not report the correct counts.
#[test]
fn graph_validate_extracts_schema_and_reports_counts() {
    let dir = TempDir::new().unwrap();
    let schema = write_ontology_ttl(&dir);

    ggen()
        .arg("graph")
        .arg("validate")
        .arg("--schema_file")
        .arg(schema.to_str().unwrap())
        .current_dir(&dir)
        .assert()
        .success()
        .stdout(predicate::str::contains("\"classes_count\":2"))
        .stdout(predicate::str::contains("\"properties_count\":3"))
        .stdout(predicate::str::contains("\"is_valid\":true"));
}

/// PROOF: `graph validate --strict` runs the same real extraction plus the
/// strict-mode reference checks (ontology/mod.rs:101). The fixture has no dangling
/// class references, so strict validation still reports `is_valid: true` with the
/// real class count. The `--strict` flag is the `strict: bool` parameter
/// (graph.rs:60) mapped to a SetTrue flag.
#[test]
fn graph_validate_strict_mode_runs_reference_checks() {
    let dir = TempDir::new().unwrap();
    let schema = write_ontology_ttl(&dir);

    ggen()
        .arg("graph")
        .arg("validate")
        .arg("--schema_file")
        .arg(schema.to_str().unwrap())
        .arg("--strict")
        .current_dir(&dir)
        .assert()
        .success()
        .stdout(predicate::str::contains("\"classes_count\":2"))
        .stdout(predicate::str::contains("\"is_valid\":true"));
}
