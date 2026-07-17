//! Interaction tests for `sh:sparql`/`sh:SPARQLTarget` combined with
//! ordinary constraints (targets the specifically named low-confidence
//! gap: this surface was newer and never touched by the NK combinatorial
//! study, which only covered the 11 non-SPARQL constraint families).

use praxis_graphlaw::parser::{Parser, Syntax};
use praxis_graphlaw::shacl::{ShapesGraph, Validator};
use praxis_graphlaw::tripleindex::TripleIndex;

fn build_data_index(data_str: &str) -> TripleIndex {
    let triples = Parser::parse_triples(data_str, Syntax::Turtle).unwrap();
    let mut index = TripleIndex::new();
    for t in triples {
        index.add(t);
    }
    index
}

/// A `sh:sparql` constraint combined with 2 ordinary constraints
/// (`sh:targetClass` implicit via node shape + `sh:property` minCount) on
/// the same shape -- all three must be independently enforced; violating
/// any one must produce a violation, satisfying all three must conform.
#[test]
fn test_sparql_constraint_combined_with_ordinary_constraints() {
    let shapes_str = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .
        ex:S a sh:NodeShape ;
            sh:targetClass ex:Person ;
            sh:property [ sh:path ex:age ; sh:minCount 1 ] ;
            sh:sparql [
                sh:message "age must be non-negative" ;
                sh:select "SELECT $this WHERE { $this <http://example.org/age> ?age . FILTER (?age < 0) }" ;
            ] .
    "#;
    let shapes = ShapesGraph::parse(shapes_str).unwrap();

    // Both satisfied: has age, non-negative.
    let data_ok = build_data_index(
        "@prefix ex: <http://example.org/> .\nex:alice a ex:Person ; ex:age 30 .\n",
    );
    assert!(
        Validator::validate(&data_ok, &shapes).conforms,
        "valid non-negative age with age present must conform"
    );

    // sh:sparql violated (negative age), sh:minCount satisfied.
    let data_negative =
        build_data_index("@prefix ex: <http://example.org/> .\nex:bob a ex:Person ; ex:age -5 .\n");
    let report_negative = Validator::validate(&data_negative, &shapes);
    assert!(
        !report_negative.conforms,
        "negative age must violate the sh:sparql constraint even though minCount is satisfied"
    );

    // sh:minCount violated (no age at all), sh:sparql trivially has no rows to flag.
    let data_missing =
        build_data_index("@prefix ex: <http://example.org/> .\nex:carol a ex:Person .\n");
    let report_missing = Validator::validate(&data_missing, &shapes);
    assert!(!report_missing.conforms, "missing age must violate sh:minCount even though the SPARQL constraint finds no negative-age rows to flag");
}

/// `sh:target`/`sh:SPARQLTarget` combined with `sh:targetClass` on the
/// same shape -- targets must UNION (a focus node reached via either
/// mechanism gets validated), not conflict or silently pick only one.
#[test]
fn test_sparql_target_unions_with_target_class() {
    let shapes_str = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .
        ex:S a sh:NodeShape ;
            sh:targetClass ex:Person ;
            sh:target [
                a sh:SPARQLTarget ;
                sh:select "SELECT ?this WHERE { ?this <http://example.org/flaggedForReview> true }" ;
            ] ;
            sh:property [ sh:path ex:age ; sh:minInclusive 0 ] .
    "#;
    let shapes = ShapesGraph::parse(shapes_str).unwrap();

    // Reached only via sh:targetClass -- must still be validated.
    let data1 = build_data_index(
        "@prefix ex: <http://example.org/> .\nex:alice a ex:Person ; ex:age -1 .\n",
    );
    assert!(
        !Validator::validate(&data1, &shapes).conforms,
        "a focus node reached via sh:targetClass alone must still be validated"
    );

    // Reached ONLY via the SPARQL target (not a ex:Person at all) -- must
    // still be validated, confirming the SPARQL target isn't silently
    // ignored when sh:targetClass is also present on the same shape.
    let data2 = build_data_index(
        "@prefix ex: <http://example.org/> .\nex:widget ex:flaggedForReview true ; ex:age -1 .\n",
    );
    let report2 = Validator::validate(&data2, &shapes);
    assert!(!report2.conforms, "a focus node reached ONLY via sh:SPARQLTarget must still be validated (targets union, not override)");

    // Neither target applies -- must conform trivially (not a focus node at all).
    let data3 = build_data_index("@prefix ex: <http://example.org/> .\nex:unrelated ex:age -1 .\n");
    assert!(
        Validator::validate(&data3, &shapes).conforms,
        "a node reached by neither target mechanism must not be validated at all"
    );
}
