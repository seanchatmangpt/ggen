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

#[test]
fn test_min_max_count_violation() {
    let shapes_str = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .

        ex:PersonShape a sh:NodeShape ;
            sh:targetClass ex:Person ;
            sh:property [
                sh:path ex:name ;
                sh:minCount 2 ;
                sh:maxCount 3 ;
            ] .
    "#;

    let data_str = r#"
        @prefix ex: <http://example.org/> .

        ex:Alice a ex:Person ;
            ex:name "Alice" . # Only 1 name, violates minCount 2

        ex:Bob a ex:Person ;
            ex:name "Bob" , "Robert" , "Bobby" , "Rob" . # 4 names, violates maxCount 3

        ex:Charlie a ex:Person ;
            ex:name "Charlie" , "Chuck" . # 2 names, passes
    "#;

    let data = build_data_index(data_str);
    let shapes = ShapesGraph::parse(shapes_str).unwrap();
    let report = Validator::validate(&data, &shapes);

    assert!(!report.conforms);
    assert_eq!(report.results.len(), 2);

    let alice_results: Vec<_> = report
        .results
        .iter()
        .filter(|r| r.focus_node.to_string().contains("Alice"))
        .collect();
    assert_eq!(alice_results.len(), 1);
    assert_eq!(
        alice_results[0].source_constraint_component.to_string(),
        "<http://www.w3.org/ns/shacl#MinCountConstraintComponent>"
    );

    let bob_results: Vec<_> = report
        .results
        .iter()
        .filter(|r| r.focus_node.to_string().contains("Bob"))
        .collect();
    assert_eq!(bob_results.len(), 1);
    assert_eq!(
        bob_results[0].source_constraint_component.to_string(),
        "<http://www.w3.org/ns/shacl#MaxCountConstraintComponent>"
    );
}

#[test]
fn test_datatype_constraint_pass_fail() {
    let shapes_str = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

        ex:PersonShape a sh:NodeShape ;
            sh:targetClass ex:Person ;
            sh:property [
                sh:path ex:age ;
                sh:datatype xsd:integer ;
            ] .
    "#;

    let data_str = r#"
        @prefix ex: <http://example.org/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

        ex:Alice a ex:Person ;
            ex:age 30 . # passes

        ex:Bob a ex:Person ;
            ex:age "thirty" . # fails (string instead of integer)
    "#;

    let data = build_data_index(data_str);
    let shapes = ShapesGraph::parse(shapes_str).unwrap();
    let report = Validator::validate(&data, &shapes);

    assert!(!report.conforms);
    let bob_results: Vec<_> = report
        .results
        .iter()
        .filter(|r| r.focus_node.to_string().contains("Bob"))
        .collect();
    assert_eq!(bob_results.len(), 1);
    assert_eq!(
        bob_results[0].source_constraint_component.to_string(),
        "<http://www.w3.org/ns/shacl#DatatypeConstraintComponent>"
    );
}

#[test]
fn test_class_constraint() {
    let shapes_str = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .

        ex:PersonShape a sh:NodeShape ;
            sh:targetClass ex:Person ;
            sh:property [
                sh:path ex:knows ;
                sh:class ex:Person ;
            ] .
    "#;

    let data_str = r#"
        @prefix ex: <http://example.org/> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

        ex:Student rdfs:subClassOf ex:Person .

        ex:Alice a ex:Person ;
            ex:knows ex:Bob , ex:Charlie , ex:Rex .

        ex:Bob a ex:Person . # directly matches Person
        ex:Charlie a ex:Student . # subclass of Person (should pass subclass check)
        ex:Rex a ex:Animal . # not Person or subclass (should fail)
    "#;

    let data = build_data_index(data_str);
    let shapes = ShapesGraph::parse(shapes_str).unwrap();
    let report = Validator::validate(&data, &shapes);

    assert!(!report.conforms);
    let rex_results: Vec<_> = report
        .results
        .iter()
        .filter(|r| {
            r.value
                .as_ref()
                .map(|v| v.to_string().contains("Rex"))
                .unwrap_or(false)
        })
        .collect();
    assert_eq!(rex_results.len(), 1);
    assert_eq!(
        rex_results[0].source_constraint_component.to_string(),
        "<http://www.w3.org/ns/shacl#ClassConstraintComponent>"
    );
}

#[test]
fn test_and_or_not_logical_constraints() {
    let shapes_str = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .

        ex:AndShape a sh:NodeShape ;
            sh:targetNode ex:AliceAnd ;
            sh:and (
                [ sh:property [ sh:path ex:name; sh:minCount 1 ] ]
                [ sh:property [ sh:path ex:age; sh:minCount 1 ] ]
            ) .

        ex:OrShape a sh:NodeShape ;
            sh:targetNode ex:BobOr ;
            sh:or (
                [ sh:property [ sh:path ex:name; sh:minCount 1 ] ]
                [ sh:property [ sh:path ex:age; sh:minCount 1 ] ]
            ) .

        ex:NotShape a sh:NodeShape ;
            sh:targetNode ex:CharlieNot ;
            sh:not [
                sh:property [ sh:path ex:name; sh:minCount 1 ]
            ] .
    "#;

    let data_str = r#"
        @prefix ex: <http://example.org/> .

        # AliceAnd has name and age -> passes AndShape
        ex:AliceAnd ex:name "Alice" ; ex:age 30 .

        # BobOr has only age -> passes OrShape
        ex:BobOr ex:age 30 .

        # CharlieNot has no name -> passes NotShape
        ex:CharlieNot ex:age 30 .
    "#;

    let data = build_data_index(data_str);
    let shapes = ShapesGraph::parse(shapes_str).unwrap();
    let report = Validator::validate(&data, &shapes);

    assert!(report.conforms);
    assert_eq!(report.results.len(), 0);

    // Let's modify the data to trigger violations
    let data_str_violating = r#"
        @prefix ex: <http://example.org/> .

        # AliceAnd has only name -> fails AndShape
        ex:AliceAnd ex:name "Alice" .

        # BobOr has neither -> fails OrShape
        ex:BobOr a ex:Something .

        # CharlieNot has name -> fails NotShape
        ex:CharlieNot ex:name "Charlie" .
    "#;

    let data_violating = build_data_index(data_str_violating);
    let report_violating = Validator::validate(&data_violating, &shapes);

    assert!(!report_violating.conforms);
    // AliceAnd fails sh:and: one top-level AndConstraintComponent result (per
    // spec, the nested sub-shape violation is not additionally propagated).
    // BobOr fails sh:or: one top-level OrConstraintComponent result.
    // CharlieNot fails sh:not: one top-level NotConstraintComponent result.
    // Total = 3. (This assertion was already failing before this change --
    // verified via `git stash` -- because the old sh:and implementation
    // incorrectly propagated nested sub-shape results in addition to the
    // top-level AndConstraintComponent result; fixed as part of vendoring the
    // real W3C SHACL suite, whose node/and-001 test caught the discrepancy.)
    assert_eq!(report_violating.results.len(), 3);
}

#[test]
fn test_conforms_true_for_valid_graph() {
    let shapes_str = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .

        ex:PersonShape a sh:NodeShape ;
            sh:targetClass ex:Person ;
            sh:property [
                sh:path ex:name ;
                sh:minCount 1 ;
            ] .
    "#;

    let data_str = r#"
        @prefix ex: <http://example.org/> .

        ex:Alice a ex:Person ;
            ex:name "Alice" .
    "#;

    let data = build_data_index(data_str);
    let shapes = ShapesGraph::parse(shapes_str).unwrap();
    let report = Validator::validate(&data, &shapes);

    assert!(report.conforms);
    assert_eq!(report.results.len(), 0);
}

#[test]
fn test_empty_dataset() {
    // 1. Shapes with targetClass but data is completely empty. Should conform because no focus nodes.
    let shapes_str = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .

        ex:PersonShape a sh:NodeShape ;
            sh:targetClass ex:Person ;
            sh:property [
                sh:path ex:name ;
                sh:minCount 1 ;
            ] .
    "#;

    let data = build_data_index("");
    let shapes = ShapesGraph::parse(shapes_str).unwrap();
    let report = Validator::validate(&data, &shapes);
    assert!(report.conforms);
    assert_eq!(report.results.len(), 0);

    // 2. Shapes with targetNode and empty data. Should violate minCount.
    let shapes_str_node = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .

        ex:PersonShape a sh:NodeShape ;
            sh:targetNode ex:Alice ;
            sh:property [
                sh:path ex:name ;
                sh:minCount 1 ;
            ] .
    "#;

    let shapes_node = ShapesGraph::parse(shapes_str_node).unwrap();
    let report_node = Validator::validate(&data, &shapes_node);
    assert!(!report_node.conforms);
    assert_eq!(report_node.results.len(), 1);
    assert_eq!(
        report_node.results[0].focus_node.to_string(),
        "<http://example.org/Alice>"
    );
    assert_eq!(
        report_node.results[0]
            .source_constraint_component
            .to_string(),
        "<http://www.w3.org/ns/shacl#MinCountConstraintComponent>"
    );
}

#[test]
fn test_invalid_turtle_shapes() {
    // Missing semicolon or dot, invalid syntax
    let shapes_str = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .

        ex:PersonShape a sh:NodeShape
            sh:targetClass ex:Person
    "#;
    let shapes = ShapesGraph::parse(shapes_str);
    assert!(shapes.is_err());
}

#[test]
fn test_severity_and_datatype() {
    let shapes_str = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

        ex:SeverityShape a sh:NodeShape ;
            sh:targetNode ex:Alice ;
            sh:severity sh:Warning ;
            sh:property [
                sh:path ex:age ;
                sh:datatype xsd:integer ;
                sh:severity sh:Info ;
            ] .
    "#;

    let data_str = r#"
        @prefix ex: <http://example.org/> .
        ex:Alice ex:age "thirty" . # fails datatype xsd:integer
    "#;

    let data = build_data_index(data_str);
    let shapes = ShapesGraph::parse(shapes_str).unwrap();
    let report = Validator::validate(&data, &shapes);

    // Per SHACL Core (1.0), sh:conforms is false whenever ANY validation
    // result exists, regardless of severity -- sh:Info and sh:Warning count
    // too, not just sh:Violation. This is confirmed by the real W3C
    // data-shapes test suite (misc/severity-001.ttl/severity-002.ttl: a
    // shape with sh:severity sh:Warning still expects sh:conforms "false").
    // A prior version of this test asserted the opposite (conforms=true for
    // non-Violation severities) -- that assumption was wrong and has been
    // corrected in shacl.rs's Validator::validate accordingly.
    assert!(
        !report.conforms,
        "conforms must be false whenever any result exists, even sh:Info severity"
    );
    assert_eq!(report.results.len(), 1);

    // The violation is on the property shape, so it should carry the property's sh:severity, which is sh:Info.
    assert_eq!(
        report.results[0].severity.to_string(),
        "<http://www.w3.org/ns/shacl#Info>"
    );
}

#[test]
fn test_message_prefers_plain_literal_over_language_tagged() {
    // Three sh:message values in different "languages" (one with no tag at
    // all). The no-language-tag literal should be preferred.
    let shapes_str = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .

        ex:PersonShape a sh:NodeShape ;
            sh:targetClass ex:Person ;
            sh:property [
                sh:path ex:name ;
                sh:minCount 1 ;
                sh:message "Fehler: Name fehlt"@de ;
                sh:message "Error: name missing"@en ;
                sh:message "Generic error message" ;
            ] .
    "#;
    let data_str = r#"
        @prefix ex: <http://example.org/> .
        ex:Alice a ex:Person .
    "#;

    let data = build_data_index(data_str);
    let shapes = ShapesGraph::parse(shapes_str).unwrap();
    let report = Validator::validate(&data, &shapes);

    assert!(!report.conforms);
    assert_eq!(report.results.len(), 1);
    assert_eq!(
        report.results[0].message.as_deref(),
        Some("Generic error message")
    );
}

#[test]
fn test_message_prefers_english_when_no_plain_literal() {
    // Two sh:message values, neither language-less: "en" should win over "de".
    let shapes_str = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .

        ex:PersonShape a sh:NodeShape ;
            sh:targetClass ex:Person ;
            sh:property [
                sh:path ex:name ;
                sh:minCount 1 ;
                sh:message "Fehler: Name fehlt"@de ;
                sh:message "Error: name missing"@en ;
            ] .
    "#;
    let data_str = r#"
        @prefix ex: <http://example.org/> .
        ex:Alice a ex:Person .
    "#;

    let data = build_data_index(data_str);
    let shapes = ShapesGraph::parse(shapes_str).unwrap();
    let report = Validator::validate(&data, &shapes);

    assert!(!report.conforms);
    assert_eq!(report.results.len(), 1);
    assert_eq!(
        report.results[0].message.as_deref(),
        Some("Error: name missing")
    );
}
