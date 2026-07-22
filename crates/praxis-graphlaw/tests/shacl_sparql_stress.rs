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
/// the same shape.
///
/// NOTE: `SHACL_SPARQL_BOUNDARY = "CORE_ONLY"` (`src/shacl/model.rs`,
/// PROJ-407 Step 2 -- a deliberate v26.7.8 threat-model decision, pinned by
/// its own `shacl_test.rs::test_...boundary` unit test) makes
/// `check_sparql_boundary` skip evaluating EVERY `sh:sparql` constraint node
/// entirely (`src/shacl/validate.rs:572`). The shape still loads
/// successfully (a real, separately-tracked doc/implementation drift against
/// the model.rs comment's "rejected at shape load time" phrasing -- fixing
/// that is out of scope here since a real production shapes file,
/// `ontologies/shapes/process-conformance.shacl.ttl`, uses `sh:sparql`, and
/// this bounded pass does not have evidence that would be safe to reject at
/// load time). So `sh:sparql` here is INERT: only the ordinary `sh:minCount`
/// constraint is actually enforced, and this test now verifies exactly
/// that -- the inert `sh:sparql` constraint's presence must not spuriously
/// cause violations OR spuriously suppress the real ones.
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

    // Both would be satisfied even if sh:sparql were live: has age, non-negative.
    let data_ok = build_data_index(
        "@prefix ex: <http://example.org/> .\nex:alice a ex:Person ; ex:age 30 .\n",
    );
    assert!(
        Validator::validate(&data_ok, &shapes).conforms,
        "valid non-negative age with age present must conform"
    );

    // Would violate sh:sparql (negative age) if it were live; sh:minCount is
    // satisfied. Since sh:sparql is inert under CORE_ONLY, this must conform.
    let data_negative =
        build_data_index("@prefix ex: <http://example.org/> .\nex:bob a ex:Person ; ex:age -5 .\n");
    let report_negative = Validator::validate(&data_negative, &shapes);
    assert!(
        report_negative.conforms,
        "sh:sparql is inert under SHACL_SPARQL_BOUNDARY = CORE_ONLY, so a negative age (which \
         only the inert sh:sparql constraint would flag) must still conform: {:?}",
        report_negative.results
    );

    // sh:minCount violated (no age at all) -- this is SHACL Core, not gated
    // by CORE_ONLY, so it must still be enforced regardless of sh:sparql.
    let data_missing =
        build_data_index("@prefix ex: <http://example.org/> .\nex:carol a ex:Person .\n");
    let report_missing = Validator::validate(&data_missing, &shapes);
    assert!(!report_missing.conforms, "missing age must violate sh:minCount (SHACL Core, unaffected by the sh:sparql boundary) regardless of the inert SPARQL constraint");
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
