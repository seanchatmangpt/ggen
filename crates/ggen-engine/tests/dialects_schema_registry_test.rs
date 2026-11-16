#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)] // Chicago TDD: unwrap/expect/panic allowed in test code
//! Court: every dialect in the engine dispatch matrix (`dialect_for_path`)
//! has an entry in the RDF dialects schema (`.specify/dialects-schema.ttl`).
//! Real file, real graph, real SPARQL; negative case removes an entry.

use std::path::Path;

use ggen_engine::graph::{
    dialect_for_path, DeterministicGraph, EngineQueryResults, GraphEngine,
};

/// The dispatch matrix as exercised against the engine: (extension, dialect).
const MATRIX: &[(&str, &str)] = &[
    ("ttl", "turtle"),
    ("nt", "ntriples"),
    ("nq", "nquads"),
    ("trig", "trig"),
    ("rdf", "rdf-xml"),
    ("xml", "rdf-xml"),
    ("owl", "rdf-xml"),
    ("jsonld", "json-ld"),
    ("json", "json-ld"),
];

fn schema_text() -> String {
    let p = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../.specify/dialects-schema.ttl");
    std::fs::read_to_string(&p).unwrap_or_else(|e| panic!("read {}: {e}", p.display()))
}

fn has_entry(g: &DeterministicGraph, dialect: &str, ext: &str) -> bool {
    let q = format!(
        "PREFIX dia: <http://seanchatmangpt.github.io/ggen/dialects#> \
         ASK {{ ?d a dia:Dialect ; dia:name \"{dialect}\" ; dia:extension \"{ext}\" }}"
    );
    match GraphEngine::query(g, &q).expect("ask") {
        EngineQueryResults::Boolean(b) => b,
        other => panic!("expected boolean, got {other:?}"),
    }
}

/// Dialects/extensions in the matrix lacking a schema entry.
fn missing(ttl: &str) -> Vec<String> {
    let g = DeterministicGraph::new().expect("graph");
    g.insert_turtle(ttl).expect("schema parses");
    MATRIX
        .iter()
        .filter_map(|(ext, dialect)| {
            // the matrix constant must itself agree with the live dispatcher
            let live = dialect_for_path(Path::new(&format!("x.{ext}"))).expect("dispatch");
            assert_eq!(live, *dialect, "matrix drift for .{ext}");
            (!has_entry(&g, dialect, ext)).then(|| format!("{dialect}/.{ext}"))
        })
        .collect()
}

#[test]
fn every_dispatch_dialect_has_a_schema_entry() {
    assert_eq!(missing(&schema_text()), Vec::<String>::new());
}

#[test]
fn dialect_without_schema_entry_fails() {
    let stripped: String = schema_text()
        .lines()
        .filter(|l| !l.starts_with("dia:jsonld"))
        .collect::<Vec<_>>()
        .join("\n");
    assert_eq!(missing(&stripped), vec!["json-ld/.jsonld", "json-ld/.json"]);
}
