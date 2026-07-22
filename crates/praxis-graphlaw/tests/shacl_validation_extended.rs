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
fn test_recursive_shapes() {
    // Recursive validation using sh:and pointing back to the node shape itself.
    // ex:Alice is a Person, knows ex:Bob.
    // ex:Bob is a Person, knows ex:Alice.
    // ex:Alice has name "Alice".
    // ex:Bob does not have a name (violates minCount 1).
    let shapes_str = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .

        ex:PersonShape a sh:NodeShape ;
            sh:targetClass ex:Person ;
            sh:property [
                sh:path ex:name ;
                sh:minCount 1 ;
            ] ;
            sh:property [
                sh:path ex:knows ;
                sh:and ( ex:PersonShape ) ;
            ] .
    "#;

    let data_str = r#"
        @prefix ex: <http://example.org/> .

        ex:Alice a ex:Person ;
            ex:name "Alice" ;
            ex:knows ex:Bob .

        ex:Bob a ex:Person ;
            ex:knows ex:Alice .
    "#;

    let data = build_data_index(data_str);
    let shapes = ShapesGraph::parse(shapes_str).unwrap();
    let report = Validator::validate(&data, &shapes);

    // Should fail because Bob does not have a name, and Bob is validated recursively because Alice knows Bob.
    // Bob should have a minCount violation for ex:name.
    // Let's check results.
    assert!(!report.conforms);

    // We expect a violation for Bob missing name, and possibly Bob failing the `and` constraint for knows Alice (wait, does Alice fail anything? No, Alice conforms).
    // Let's see what focus nodes failed: Bob should have a minCount violation on name.
    let bob_name_viol: Vec<_> = report
        .results
        .iter()
        .filter(|r| {
            r.focus_node.to_string().contains("Bob")
                && r.source_constraint_component
                    .to_string()
                    .contains("MinCountConstraintComponent")
        })
        .collect();
    assert!(!bob_name_viol.is_empty());
}

#[test]
fn test_property_paths_comprehensive() {
    let shapes_str = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .

        ex:PathShape a sh:NodeShape ;
            sh:targetNode ex:Alice ;

            # 1. Alternative path: ex:father | ex:mother
            sh:property [
                sh:path [ sh:alternativePath (ex:father ex:mother) ] ;
                sh:minCount 2 ;
            ] ;

            # 2. Inverse path: ^ex:child
            sh:property [
                sh:path [ sh:inversePath ex:child ] ;
                sh:minCount 1 ;
            ] ;

            # 3. Zero-or-more path: ex:knows*
            sh:property [
                sh:path [ sh:zeroOrMorePath ex:knows ] ;
                sh:minCount 3 ;
            ] ;

            # 4. One-or-more path: ex:knows+
            sh:property [
                sh:path [ sh:oneOrMorePath ex:knows ] ;
                sh:minCount 2 ;
            ] ;

            # 5. Zero-or-one path: ex:knows?
            sh:property [
                sh:path [ sh:zeroOrOnePath ex:knows ] ;
                sh:minCount 2 ;
            ] ;

            # 6. Sequence path: ex:knows / ex:knows
            sh:property [
                sh:path (ex:knows ex:knows) ;
                sh:minCount 1 ;
            ] .
    "#;

    let data_str = r#"
        @prefix ex: <http://example.org/> .

        # ex:Alice has father ex:Bob and mother ex:Charlie -> alternative path count = 2
        ex:Alice ex:father ex:Bob ;
                 ex:mother ex:Charlie .

        # ex:Diana has child ex:Alice -> inverse path of ex:child on ex:Alice -> value is ex:Diana
        ex:Diana ex:child ex:Alice .

        # ex:Alice knows ex:Bob. ex:Bob knows ex:Charlie.
        # zero-or-more knows: ex:Alice, ex:Bob, ex:Charlie -> count = 3
        # one-or-more knows: ex:Bob, ex:Charlie -> count = 2
        # zero-or-one knows: ex:Alice, ex:Bob -> count = 2
        # sequence knows / knows: ex:Charlie -> count = 1
        ex:Alice ex:knows ex:Bob .
        ex:Bob ex:knows ex:Charlie .
    "#;

    let data = build_data_index(data_str);
    let shapes = ShapesGraph::parse(shapes_str).unwrap();
    let report = Validator::validate(&data, &shapes);

    // Everything is designed to pass
    assert!(
        report.conforms,
        "Expected all property path constraints to pass, but got errors: {:?}",
        report.results
    );

    // Now let's test failing cases by making data insufficient
    let data_str_fail = r#"
        @prefix ex: <http://example.org/> .
        ex:Alice ex:father ex:Bob . # Alternative path count is 1 (fails minCount 2)
        # Missing inverse path ^ex:child (fails minCount 1)
        # Alice knows Bob, no more (zero-or-more knows is Alice, Bob -> count 2, fails minCount 3)
        # one-or-more knows is Bob -> count 1, fails minCount 2
        # zero-or-one knows is Alice, Bob -> count 2, passes minCount 2
        # sequence knows / knows -> empty, fails minCount 1
        ex:Alice ex:knows ex:Bob .
    "#;

    let data_fail = build_data_index(data_str_fail);
    let report_fail = Validator::validate(&data_fail, &shapes);
    assert!(!report_fail.conforms);
    // Let's count how many failures we got
    assert!(report_fail.results.len() >= 4);
}

#[test]
fn test_sparql_ask_constraint() {
    // sh:ask constraint: focus node must have an ex:age >= 18.
    //
    // NOTE: `SHACL_SPARQL_BOUNDARY = "CORE_ONLY"` (`src/shacl/model.rs`,
    // PROJ-407 Step 2 -- a deliberate v26.7.8 threat-model decision, pinned
    // by its own `shacl_test.rs::test_...boundary` unit test) makes
    // `check_sparql_boundary` skip evaluating EVERY `sh:sparql` constraint
    // node entirely (`src/shacl/validate.rs:572`'s `if
    // check_sparql_boundary(...) { ... }` guard around the only call site of
    // `validate_sparql_constraint`). The shape still loads successfully
    // (`ShapesGraph::parse` does not reject it, despite the doc comment's
    // "rejected at shape load time" phrasing -- a real, separately-tracked
    // doc/implementation drift, not something this test changes). With
    // sh:sparql as the shape's ONLY constraint, both Alice (30) and Bob (10)
    // must conform: the age check that would flag Bob never runs.
    let shapes_str = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .

        ex:AdultShape a sh:NodeShape ;
            sh:targetClass ex:Person ;
            sh:sparql [
                sh:ask "ASK { $this <http://example.org/age> ?age . FILTER(?age >= 18) }" ;
                sh:message "Must be an adult" ;
            ] .
    "#;
    let data_str = r#"
        @prefix ex: <http://example.org/> .
        ex:Alice a ex:Person ; ex:age 30 .
        ex:Bob a ex:Person ; ex:age 10 .
    "#;

    let data = build_data_index(data_str);
    let shapes = ShapesGraph::parse(shapes_str).unwrap();
    let report = Validator::validate(&data, &shapes);

    assert!(
        report.conforms,
        "sh:sparql (sh:ask) is inert under SHACL_SPARQL_BOUNDARY = CORE_ONLY, so no violation \
         can be produced regardless of data: {:?}",
        report.results
    );
    assert!(report.results.is_empty());
}

#[test]
fn test_sparql_select_constraint() {
    // sh:select constraint: any solution row is a violation. Here it would
    // flag focus nodes that have two different ex:name values -- IF sh:sparql
    // constraints were evaluated. See `test_sparql_ask_constraint`'s NOTE:
    // `SHACL_SPARQL_BOUNDARY = "CORE_ONLY"` makes every sh:sparql constraint
    // node inert (skipped, not evaluated), so with sh:sparql as the shape's
    // ONLY constraint, both Alice (two names) and Bob (one name) conform.
    let shapes_str = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .

        ex:UniqueNameShape a sh:NodeShape ;
            sh:targetClass ex:Person ;
            sh:sparql [
                sh:select "SELECT ?n1 WHERE { $this <http://example.org/name> ?n1 . $this <http://example.org/name> ?n2 . FILTER(?n1 != ?n2) }" ;
                sh:message "Must have a single name" ;
            ] .
    "#;
    let data_str = r#"
        @prefix ex: <http://example.org/> .
        ex:Alice a ex:Person ; ex:name "Alice" , "Al" .
        ex:Bob a ex:Person ; ex:name "Bob" .
    "#;

    let data = build_data_index(data_str);
    let shapes = ShapesGraph::parse(shapes_str).unwrap();
    let report = Validator::validate(&data, &shapes);

    assert!(
        report.conforms,
        "sh:sparql (sh:select) is inert under SHACL_SPARQL_BOUNDARY = CORE_ONLY, so no \
         violation can be produced regardless of data: {:?}",
        report.results
    );
    assert!(report.results.is_empty());
}

#[test]
fn test_sparql_target() {
    // Focus nodes are selected via a SPARQLTarget (people under 18), rather
    // than sh:targetClass/sh:targetNode/etc. Each selected focus node must
    // then have an ex:guardian.
    let shapes_str = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .

        ex:MinorNeedsGuardianShape a sh:NodeShape ;
            sh:target [
                a sh:SPARQLTarget ;
                sh:select "SELECT ?this WHERE { ?this <http://example.org/age> ?a . FILTER(?a < 18) }" ;
            ] ;
            sh:property [ sh:path ex:guardian ; sh:minCount 1 ] .
    "#;
    let data_str = r#"
        @prefix ex: <http://example.org/> .
        ex:Minor1 ex:age 10 .
        ex:Adult1 ex:age 30 .
        ex:Minor2 ex:age 5 ; ex:guardian ex:Adult1 .
    "#;

    let data = build_data_index(data_str);
    let shapes = ShapesGraph::parse(shapes_str).unwrap();
    let report = Validator::validate(&data, &shapes);

    assert!(!report.conforms);
    assert_eq!(report.results.len(), 1);
    assert!(report.results[0].focus_node.to_string().contains("Minor1"));
}

#[test]
fn test_subclass_closure_cycles_and_multiple_inheritance() {
    let shapes_str = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .

        ex:VehicleShape a sh:NodeShape ;
            sh:targetClass ex:Vehicle ;
            sh:property [
                sh:path ex:cargo ;
                sh:class ex:SafeCargo ;
            ] .
    "#;

    let data_str = r#"
        @prefix ex: <http://example.org/> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

        # Multiple inheritance and multi-level
        ex:Fruit rdfs:subClassOf ex:Food .
        ex:Apple rdfs:subClassOf ex:Fruit .
        ex:Apple rdfs:subClassOf ex:SafeCargo .

        # Subclass Cycle
        ex:A rdfs:subClassOf ex:B .
        ex:B rdfs:subClassOf ex:A .
        ex:A rdfs:subClassOf ex:SafeCargo .

        ex:Truck a ex:Vehicle ;
            ex:cargo ex:MyApple , ex:MyA , ex:MyBox .

        ex:MyApple a ex:Apple .       # is Apple -> Fruit -> Food, and Apple -> SafeCargo (conforms)
        ex:MyA a ex:A .               # is A -> B -> A, and A -> SafeCargo (conforms)
        ex:MyBox a ex:CardboardBox .  # not SafeCargo (violates)
    "#;

    let data = build_data_index(data_str);
    let shapes = ShapesGraph::parse(shapes_str).unwrap();
    let report = Validator::validate(&data, &shapes);

    assert!(!report.conforms);
    let violations: Vec<_> = report
        .results
        .iter()
        .filter(|r| {
            r.value
                .as_ref()
                .map(|v| v.to_string().contains("MyBox"))
                .unwrap_or(false)
        })
        .collect();
    assert_eq!(violations.len(), 1);

    // ensure Apple and A conform (don't generate violations)
    let apple_violations: Vec<_> = report
        .results
        .iter()
        .filter(|r| {
            r.value
                .as_ref()
                .map(|v| v.to_string().contains("MyApple"))
                .unwrap_or(false)
        })
        .collect();
    assert_eq!(apple_violations.len(), 0);

    let a_violations: Vec<_> = report
        .results
        .iter()
        .filter(|r| {
            r.value
                .as_ref()
                .map(|v| v.to_string().contains("MyA"))
                .unwrap_or(false)
        })
        .collect();
    assert_eq!(a_violations.len(), 0);
}
