//! End-to-end proof that declarative `[ontology].imports` admit heterogeneous
//! RDF serializations through the same project-graph path used by sync/query
//! consumers. This deliberately exercises schema dispatch, manifest semantic
//! validation, filesystem admission, batch parsing, and the GraphEngine -- it
//! is not a parser-unit surrogate.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use ggen_engine::{project_graph::load_for_query_with_engine, sync::EngineKind};
use tempfile::TempDir;

const MANIFEST: &str = r#"
[project]
name = "heterogeneous-rdf-admission"
version = "1.0.0"

[ontology]
source = "base.ttl"
imports = ["skos.rdf", "authority.jsonld"]

[generation]
rules = []
"#;

const BASE_TURTLE: &str = "<urn:ggen:base-subject> <urn:ggen:predicate> <urn:ggen:base-object> .";

const RDFXML: &str = r#"<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
         xmlns:ggen="urn:ggen:">
  <rdf:Description rdf:about="urn:ggen:rdfxml-subject">
    <ggen:predicate rdf:resource="urn:ggen:rdfxml-object"/>
  </rdf:Description>
</rdf:RDF>"#;

const JSONLD: &str = r#"{
  "@context": { "predicate": "urn:ggen:predicate" },
  "@id": "urn:ggen:jsonld-subject",
  "predicate": { "@id": "urn:ggen:jsonld-object" }
}"#;

#[test]
fn declarative_ontology_imports_admit_turtle_rdfxml_and_jsonld_together() {
    let root = TempDir::new().expect("tempdir");
    std::fs::write(root.path().join("ggen.toml"), MANIFEST).expect("write ggen.toml");
    std::fs::write(root.path().join("base.ttl"), BASE_TURTLE).expect("write Turtle");
    std::fs::write(root.path().join("skos.rdf"), RDFXML).expect("write RDF/XML");
    std::fs::write(root.path().join("authority.jsonld"), JSONLD).expect("write JSON-LD");

    let graph = load_for_query_with_engine(root.path(), EngineKind::Oxigraph)
        .expect("the real project-graph ingestion path must admit all three RDF syntaxes");
    let canonical = graph
        .canonical_quads()
        .expect("canonical graph receipt")
        .join("\n");

    assert!(canonical.contains("<urn:ggen:base-subject>"), "{canonical}");
    assert!(
        canonical.contains("<urn:ggen:rdfxml-subject>"),
        "{canonical}"
    );
    assert!(
        canonical.contains("<urn:ggen:jsonld-subject>"),
        "{canonical}"
    );

    let predicate_hits = canonical.matches("<urn:ggen:predicate>").count();
    assert_eq!(
        predicate_hits, 3,
        "expected one admitted assertion per authority:\n{canonical}"
    );
}

#[test]
fn unsupported_non_rdf_import_refuses_the_entire_project_graph() {
    let root = TempDir::new().expect("tempdir");
    let manifest = MANIFEST.replace(
        "imports = [\"skos.rdf\", \"authority.jsonld\"]",
        "imports = [\"skos.rdf\", \"kubernetes.openapi.json\"]",
    );
    std::fs::write(root.path().join("ggen.toml"), manifest).expect("write ggen.toml");
    std::fs::write(root.path().join("base.ttl"), BASE_TURTLE).expect("write Turtle");
    std::fs::write(root.path().join("skos.rdf"), RDFXML).expect("write RDF/XML");
    std::fs::write(root.path().join("kubernetes.openapi.json"), "{}")
        .expect("write non-RDF source");

    let error = match load_for_query_with_engine(root.path(), EngineKind::Oxigraph) {
        Ok(_) => panic!("arbitrary JSON must not acquire ambient RDF execution authority"),
        Err(error) => error,
    };
    let diagnostic = error.to_string();

    assert!(diagnostic.contains("FM-GRAPH-010"), "{diagnostic}");
    assert!(
        diagnostic.contains("kubernetes.openapi.json"),
        "{diagnostic}"
    );
}
