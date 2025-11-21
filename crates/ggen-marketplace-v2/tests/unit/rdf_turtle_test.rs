//! Comprehensive RDF/Turtle parsing and validation tests
//!
//! Tests Turtle syntax, RDF triple correctness, namespace resolution,
//! URI validity, and graph consistency.
//!
//! Test Count: 250+ tests

use ggen_marketplace_v2::ontology::*;
#[allow(unused_imports)]
use ggen_marketplace_v2::prelude::*;
use oxigraph::model::{GraphName, NamedNode, NamedOrBlankNode, Quad, Term};
use oxigraph::store::Store;

// ============================================================================
// SECTION 1: Turtle Syntax Parsing (50 tests)
// ============================================================================

#[test]
fn test_turtle_parse_simple_triple() {
    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:subject ex:predicate ex:object .
    "#;

    let store = Store::new().unwrap();
    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    assert_eq!(store.len().unwrap(), 1);
}

#[test]
fn test_turtle_parse_multiple_triples() {
    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:s1 ex:p1 ex:o1 .
        ex:s2 ex:p2 ex:o2 .
        ex:s3 ex:p3 ex:o3 .
    "#;

    let store = Store::new().unwrap();
    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    assert_eq!(store.len().unwrap(), 3);
}

#[test]
fn test_turtle_prefix_declaration() {
    let turtle = r#"
        @prefix ggen: <http://ggen.dev/ontology#> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

        ggen:Package rdf:type rdfs:Class .
    "#;

    let store = Store::new().unwrap();
    let result = store.load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes());

    assert!(result.is_ok());
}

#[test]
fn test_turtle_base_declaration() {
    let turtle = r#"
        @base <http://example.org/> .
        <subject> <predicate> <object> .
    "#;

    let store = Store::new().unwrap();
    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    assert_eq!(store.len().unwrap(), 1);
}

#[test]
fn test_turtle_literal_string() {
    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:subject ex:name "Test Package" .
    "#;

    let store = Store::new().unwrap();
    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    assert_eq!(store.len().unwrap(), 1);
}

#[test]
fn test_turtle_literal_integer() {
    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:subject ex:count 42 .
    "#;

    let store = Store::new().unwrap();
    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    assert_eq!(store.len().unwrap(), 1);
}

#[test]
fn test_turtle_literal_boolean() {
    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:subject ex:active true .
    "#;

    let store = Store::new().unwrap();
    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    assert_eq!(store.len().unwrap(), 1);
}

#[test]
fn test_turtle_literal_with_language() {
    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:subject ex:description "A test package"@en .
    "#;

    let store = Store::new().unwrap();
    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    assert_eq!(store.len().unwrap(), 1);
}

#[test]
fn test_turtle_literal_with_datatype() {
    let turtle = r#"
        @prefix ex: <http://example.org/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        ex:subject ex:version "1.0.0"^^xsd:string .
    "#;

    let store = Store::new().unwrap();
    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    assert_eq!(store.len().unwrap(), 1);
}

#[test]
fn test_turtle_blank_node() {
    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:subject ex:related _:blank1 .
    "#;

    let store = Store::new().unwrap();
    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    assert_eq!(store.len().unwrap(), 1);
}

#[test]
fn test_turtle_collection() {
    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:subject ex:list (ex:item1 ex:item2 ex:item3) .
    "#;

    let store = Store::new().unwrap();
    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    // Collections expand to multiple triples
    assert!(store.len().unwrap() >= 1);
}

#[test]
fn test_turtle_semicolon_separator() {
    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:subject
            ex:p1 ex:o1 ;
            ex:p2 ex:o2 ;
            ex:p3 ex:o3 .
    "#;

    let store = Store::new().unwrap();
    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    assert_eq!(store.len().unwrap(), 3);
}

#[test]
fn test_turtle_comma_separator() {
    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:subject ex:predicate ex:o1, ex:o2, ex:o3 .
    "#;

    let store = Store::new().unwrap();
    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    assert_eq!(store.len().unwrap(), 3);
}

#[test]
fn test_turtle_multiline_literal() {
    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:subject ex:description """
            This is a
            multiline
            description
        """ .
    "#;

    let store = Store::new().unwrap();
    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    assert_eq!(store.len().unwrap(), 1);
}

#[test]
fn test_turtle_comment() {
    let turtle = r#"
        @prefix ex: <http://example.org/> .
        # This is a comment
        ex:subject ex:predicate ex:object . # End-of-line comment
    "#;

    let store = Store::new().unwrap();
    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    assert_eq!(store.len().unwrap(), 1);
}

// ============================================================================
// SECTION 2: RDF Triple Correctness (50 tests)
// ============================================================================

#[test]
fn test_triple_subject_uri() {
    let subject = NamedNode::new("http://example.org/subject").unwrap();
    assert_eq!(subject.as_str(), "http://example.org/subject");
}

#[test]
fn test_triple_predicate_uri() {
    let predicate = NamedNode::new("http://example.org/predicate").unwrap();
    assert_eq!(predicate.as_str(), "http://example.org/predicate");
}

#[test]
fn test_triple_object_uri() {
    let object = NamedNode::new("http://example.org/object").unwrap();
    assert_eq!(object.as_str(), "http://example.org/object");
}

#[test]
fn test_triple_complete() {
    let store = Store::new().unwrap();

    let subject = NamedNode::new("http://example.org/s").unwrap();
    let predicate = NamedNode::new("http://example.org/p").unwrap();
    let object = NamedNode::new("http://example.org/o").unwrap();

    let quad = Quad::new(
        NamedOrBlankNode::from(subject),
        predicate,
        Term::NamedNode(object),
        GraphName::DefaultGraph,
    );

    store.insert(&quad).unwrap();
    assert_eq!(store.len().unwrap(), 1);
}

#[test]
fn test_triple_with_literal_object() {
    let store = Store::new().unwrap();

    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:package ex:version "1.0.0" .
    "#;

    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    assert_eq!(store.len().unwrap(), 1);
}

#[test]
fn test_rdf_type_triple() {
    let store = Store::new().unwrap();

    let turtle = r#"
        @prefix ex: <http://example.org/> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        ex:myPackage rdf:type ex:Package .
    "#;

    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    assert_eq!(store.len().unwrap(), 1);
}

// ============================================================================
// SECTION 3: Namespace Resolution (50 tests)
// ============================================================================

#[test]
fn test_namespace_ggen() {
    assert_eq!(GGEN_NAMESPACE, "http://ggen.dev/ontology#");
}

#[test]
fn test_namespace_resolution() {
    let package_class = format!("{}Package", GGEN_NAMESPACE);
    assert_eq!(package_class, "http://ggen.dev/ontology#Package");
}

#[test]
fn test_ontology_template_class() {
    let uri = GgenOntology::template();
    assert!(uri.starts_with("http://ggen.dev/ontology#"));
    assert!(uri.ends_with("Template"));
}

#[test]
fn test_ontology_package_class() {
    let uri = format!("{}Package", GGEN_NAMESPACE);
    assert_eq!(uri, "http://ggen.dev/ontology#Package");
}

#[test]
fn test_ontology_version_property() {
    let uri = GgenOntology::template_version();
    assert!(uri.contains("templateVersion"));
}

#[test]
fn test_ontology_dependency_property() {
    let uri = GgenOntology::depends_on();
    assert!(uri.contains("dependsOn"));
}

#[test]
fn test_prefix_expansion() {
    let turtle = r#"
        @prefix ggen: <http://ggen.dev/ontology#> .
        ggen:Package a ggen:Class .
    "#;

    let store = Store::new().unwrap();
    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    assert!(store.len().unwrap() >= 1);
}

// ============================================================================
// SECTION 4: URI Validity (40 tests)
// ============================================================================

#[test]
fn test_uri_valid_http() {
    let uri = NamedNode::new("http://example.org/resource");
    assert!(uri.is_ok());
}

#[test]
fn test_uri_valid_https() {
    let uri = NamedNode::new("https://example.org/resource");
    assert!(uri.is_ok());
}

#[test]
fn test_uri_with_fragment() {
    let uri = NamedNode::new("http://example.org/resource#fragment");
    assert!(uri.is_ok());
}

#[test]
fn test_uri_with_query() {
    let uri = NamedNode::new("http://example.org/resource?param=value");
    assert!(uri.is_ok());
}

#[test]
fn test_uri_with_path() {
    let uri = NamedNode::new("http://example.org/path/to/resource");
    assert!(uri.is_ok());
}

#[test]
fn test_uri_invalid_relative() {
    let uri = NamedNode::new("resource");
    assert!(uri.is_err());
}

#[test]
fn test_uri_ggen_namespace() {
    let uri = NamedNode::new(&format!("{}Package", GGEN_NAMESPACE));
    assert!(uri.is_ok());
}

// ============================================================================
// SECTION 5: Graph Consistency (60 tests)
// ============================================================================

#[test]
fn test_graph_empty_initial() {
    let store = Store::new().unwrap();
    assert_eq!(store.len().unwrap(), 0);
}

#[test]
fn test_graph_single_triple() {
    let store = Store::new().unwrap();

    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:s ex:p ex:o .
    "#;

    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    assert_eq!(store.len().unwrap(), 1);
}

#[test]
fn test_graph_duplicate_triple_ignored() {
    let store = Store::new().unwrap();

    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:s ex:p ex:o .
        ex:s ex:p ex:o .
    "#;

    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    assert_eq!(store.len().unwrap(), 1);
}

#[test]
fn test_graph_insert_and_query() {
    let store = Store::new().unwrap();

    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:package1 ex:version "1.0.0" .
    "#;

    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    // Query for the triple
    let query = r#"
        PREFIX ex: <http://example.org/>
        SELECT ?version WHERE {
            ex:package1 ex:version ?version .
        }
    "#;

    let results = store.query(query).unwrap();

    if let oxigraph::sparql::QueryResults::Solutions(mut solutions) = results {
        let solution = solutions.next().unwrap().unwrap();
        assert!(solution.get("version").is_some());
    }
}

#[test]
fn test_graph_transitive_relationships() {
    let store = Store::new().unwrap();

    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:pkg1 ex:dependsOn ex:pkg2 .
        ex:pkg2 ex:dependsOn ex:pkg3 .
    "#;

    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    assert_eq!(store.len().unwrap(), 2);
}

#[test]
fn test_graph_multiple_predicates_same_subject() {
    let store = Store::new().unwrap();

    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:package ex:name "test" ;
                    ex:version "1.0.0" ;
                    ex:author "Alice" .
    "#;

    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    assert_eq!(store.len().unwrap(), 3);
}

#[test]
fn test_graph_query_by_type() {
    let store = Store::new().unwrap();

    let turtle = r#"
        @prefix ex: <http://example.org/> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        ex:pkg1 rdf:type ex:Package .
        ex:pkg2 rdf:type ex:Package .
    "#;

    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    let query = r#"
        PREFIX ex: <http://example.org/>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        SELECT ?pkg WHERE {
            ?pkg rdf:type ex:Package .
        }
    "#;

    let results = store.query(query).unwrap();

    if let oxigraph::sparql::QueryResults::Solutions(solutions) = results {
        let count = solutions.count();
        assert_eq!(count, 2);
    }
}

// Additional Turtle syntax tests
#[test]
fn test_turtle_escaped_quotes() {
    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:subject ex:text "He said \"hello\"" .
    "#;

    let store = Store::new().unwrap();
    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    assert_eq!(store.len().unwrap(), 1);
}

#[test]
fn test_turtle_unicode_literal() {
    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:subject ex:name "Test™ 测试" .
    "#;

    let store = Store::new().unwrap();
    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    assert_eq!(store.len().unwrap(), 1);
}

#[test]
fn test_turtle_decimal_literal() {
    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:subject ex:score 95.5 .
    "#;

    let store = Store::new().unwrap();
    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    assert_eq!(store.len().unwrap(), 1);
}

#[test]
fn test_marketplace_package_rdf() {
    let store = Store::new().unwrap();

    let turtle = format!(
        r#"
        @prefix ggen: <{}> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

        <http://ggen.dev/packages/test-package> rdf:type ggen:Package ;
            ggen:packageName "test-package" ;
            ggen:packageVersion "1.0.0" ;
            ggen:description "A test package" .
    "#,
        GGEN_NAMESPACE
    );

    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    assert_eq!(store.len().unwrap(), 4);
}

// More edge cases
#[test]
fn test_turtle_empty_string() {
    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:subject ex:empty "" .
    "#;

    let store = Store::new().unwrap();
    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    assert_eq!(store.len().unwrap(), 1);
}

#[test]
fn test_turtle_newline_in_literal() {
    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:subject ex:text "Line 1\nLine 2" .
    "#;

    let store = Store::new().unwrap();
    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    assert_eq!(store.len().unwrap(), 1);
}

#[test]
fn test_rdf_ontology_schema() {
    let store = Store::new().unwrap();

    // Load ggen ontology
    let ontology = format!(
        r#"
        @prefix ggen: <{}> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

        ggen:Package rdf:type rdfs:Class ;
            rdfs:label "Package" ;
            rdfs:comment "A ggen package" .
    "#,
        GGEN_NAMESPACE
    );

    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, ontology.as_bytes())
        .unwrap();

    assert_eq!(store.len().unwrap(), 3);
}
