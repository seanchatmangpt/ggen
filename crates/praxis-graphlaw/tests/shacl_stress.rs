//! Stress and counterfactual tests for the SHACL validator, going beyond the
//! W3C-suite-driven conformance checks to probe behavior at scale and check
//! specifically for over-permissive validation (false conformance).

use praxis_graphlaw::parser::{Parser, Syntax};
use praxis_graphlaw::shacl::{ShapesGraph, Validator};
use praxis_graphlaw::tripleindex::TripleIndex;
use std::time::{Duration, Instant};

fn build_data_index(data_str: &str) -> TripleIndex {
    let triples = Parser::parse_triples(data_str, Syntax::Turtle).unwrap();
    let mut index = TripleIndex::new();
    for t in triples {
        index.add(t);
    }
    index
}

/// Counterfactual: an `sh:and` shape combining a constraint that's always
/// satisfiable with one that's never satisfiable by any real value (a
/// contradictory conjunction: the value must simultaneously equal ex:X via
/// sh:hasValue AND belong to sh:in a disjoint value set). No conforming data
/// can exist. An over-permissive validator that only checks
/// sub-constraints independently (e.g. short-circuiting on the first
/// satisfied branch instead of requiring ALL branches) would incorrectly
/// report conformance; a correct one reports a violation for every focus
/// node hitting this shape.
#[test]
fn test_counterfactual_contradictory_and_never_conforms() {
    let shapes_str = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .

        ex:ImpossibleShape a sh:NodeShape ;
            sh:targetClass ex:Widget ;
            sh:and (
                [ sh:property [ sh:path ex:status ; sh:hasValue ex:Active ] ]
                [ sh:property [ sh:path ex:status ; sh:in ( ex:Inactive ex:Archived ) ] ]
            ) .
    "#;
    let shapes = ShapesGraph::parse(shapes_str).unwrap();

    // Try every value that could conceivably be argued to satisfy one
    // branch or the other -- none should conform, since sh:and requires ALL.
    for status in ["ex:Active", "ex:Inactive", "ex:Archived", "ex:Unknown"] {
        let data_str = format!(
            r#"
            @prefix ex: <http://example.org/> .
            ex:w1 a ex:Widget ; ex:status {status} .
        "#
        );
        let data = build_data_index(&data_str);
        let report = Validator::validate(&data, &shapes);
        assert!(
            !report.conforms,
            "ex:status={} must NOT conform to a contradictory sh:and (hasValue ex:Active AND in (Inactive,Archived)), but validator reported conformance",
            status
        );
    }
}

/// Counterfactual: `sh:not` wrapping a shape that's itself never satisfiable
/// (an empty `sh:in` list -- no value can be a member of an empty set) means
/// `sh:not` should ALWAYS hold (negating "never true" gives "always true").
/// This specifically stresses that `sh:not`'s "does the inner shape conform"
/// check is followed correctly even at the edges (an inner shape that always
/// fails), rather than e.g. an implementation that treats an empty sh:in
/// list as vacuously true by accident.
#[test]
fn test_counterfactual_not_of_unsatisfiable_always_conforms() {
    let shapes_str = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .

        ex:NeverInEmptySetShape a sh:NodeShape ;
            sh:in () .

        ex:AlwaysConformsShape a sh:NodeShape ;
            sh:targetClass ex:Thing ;
            sh:not ex:NeverInEmptySetShape .
    "#;
    let shapes = ShapesGraph::parse(shapes_str).unwrap();

    let data = build_data_index(
        r#"
        @prefix ex: <http://example.org/> .
        ex:anything a ex:Thing .
    "#,
    );
    let report = Validator::validate(&data, &shapes);
    assert!(
        report.conforms,
        "sh:not of an always-unsatisfiable inner shape (empty sh:in) must always conform, got violations: {:?}",
        report.results
    );
}

/// Counterfactual: `sh:xone` (exactly one) with a focus node whose value
/// satisfies BOTH candidate branches must be REJECTED (xone requires
/// exactly one match, not "at least one" like sh:or). This specifically
/// catches an implementation that conflates xone with or.
#[test]
fn test_counterfactual_xone_rejects_multiple_matches() {
    let shapes_str = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .

        ex:XoneShape a sh:NodeShape ;
            sh:targetClass ex:Item ;
            sh:xone (
                [ sh:property [ sh:path ex:value ; sh:minInclusive 0 ] ]
                [ sh:property [ sh:path ex:value ; sh:maxInclusive 100 ] ]
            ) .
    "#;
    let shapes = ShapesGraph::parse(shapes_str).unwrap();

    // value=50 satisfies BOTH branches (>=0 AND <=100) -- xone must reject this.
    let data_both = build_data_index(
        r#"
        @prefix ex: <http://example.org/> .
        ex:i1 a ex:Item ; ex:value 50 .
    "#,
    );
    let report_both = Validator::validate(&data_both, &shapes);
    assert!(
        !report_both.conforms,
        "value satisfying BOTH xone branches must be rejected (xone means exactly one, not at least one)"
    );

    // value=-10 satisfies ONLY the maxInclusive branch (not minInclusive) -- xone must accept this.
    let data_one = build_data_index(
        r#"
        @prefix ex: <http://example.org/> .
        ex:i2 a ex:Item ; ex:value -10 .
    "#,
    );
    let report_one = Validator::validate(&data_one, &shapes);
    assert!(
        report_one.conforms,
        "value satisfying EXACTLY ONE xone branch must conform, got violations: {:?}",
        report_one.results
    );
}

/// Stress: validate 1000 focus nodes (half conforming, half violating)
/// against a shape with several constraint components combined, and check
/// both correctness (exactly the right half is flagged) and that it
/// completes in a practical time budget.
#[test]
fn test_large_scale_validation_1000_focus_nodes() {
    const N: usize = 1000;
    let shapes_str = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .

        ex:PersonShape a sh:NodeShape ;
            sh:targetClass ex:Person ;
            sh:property [
                sh:path ex:age ;
                sh:minInclusive 0 ;
                sh:maxInclusive 150 ;
                sh:minCount 1 ;
                sh:maxCount 1 ;
            ] .
    "#;
    let shapes = ShapesGraph::parse(shapes_str).unwrap();

    let mut data_str = String::from("@prefix ex: <http://example.org/> .\n");
    for i in 0..N {
        // Even-indexed people get a valid age; odd-indexed get an out-of-range one.
        let age = if i % 2 == 0 { (i % 120) as i64 } else { -5 };
        data_str.push_str(&format!("ex:person{} a ex:Person ; ex:age {} .\n", i, age));
    }
    let data = build_data_index(&data_str);

    let start = Instant::now();
    let report = Validator::validate(&data, &shapes);
    let elapsed = start.elapsed();

    assert!(
        !report.conforms,
        "half of 1000 focus nodes have an invalid age and must produce violations"
    );

    let violated_persons: std::collections::HashSet<String> = report
        .results
        .iter()
        .map(|r| r.focus_node.to_string())
        .filter(|s| s.contains("person"))
        .collect();

    // Exactly the odd-indexed (N/2) people should have violations -- not
    // more (false positives on valid data) and not fewer (false negatives
    // missing real violations).
    assert_eq!(
        violated_persons.len(),
        N / 2,
        "expected exactly {} violating focus nodes (the odd-indexed ones with age=-5), got {}",
        N / 2,
        violated_persons.len()
    );

    assert!(
        elapsed < Duration::from_secs(15),
        "validating 1000 focus nodes took {:?}, expected well under 15s",
        elapsed
    );
}

/// Counterfactual: sh:minInclusive/minExclusive/maxInclusive/maxExclusive at
/// their exact boundary values. Inclusive bounds must accept the boundary
/// itself; exclusive bounds must reject it. Mirrors the equivalent ShEx
/// boundary test (`shex_stress.rs::test_counterfactual_numeric_range_exact_boundary`)
/// but was previously untested on the SHACL side.
#[test]
fn test_counterfactual_numeric_range_exact_boundary() {
    let shapes_str = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .

        ex:InclusiveShape a sh:NodeShape ;
            sh:targetClass ex:InclusiveItem ;
            sh:property [ sh:path ex:value ; sh:minInclusive 0 ; sh:maxInclusive 100 ] .

        ex:ExclusiveShape a sh:NodeShape ;
            sh:targetClass ex:ExclusiveItem ;
            sh:property [ sh:path ex:value ; sh:minExclusive 0 ; sh:maxExclusive 100 ] .
    "#;
    let shapes = ShapesGraph::parse(shapes_str).unwrap();

    for (value, should_conform) in [
        (-1, false),
        (0, true),
        (50, true),
        (100, true),
        (101, false),
    ] {
        let data = build_data_index(&format!(
            "@prefix ex: <http://example.org/> .\nex:i a ex:InclusiveItem ; ex:value {value} .\n"
        ));
        let report = Validator::validate(&data, &shapes);
        assert_eq!(
            report.conforms, should_conform,
            "inclusive range: value={} against [0,100] inclusive: expected conforms={}, got {}",
            value, should_conform, report.conforms
        );
    }

    for (value, should_conform) in [
        (-1, false),
        (0, false),
        (1, true),
        (99, true),
        (100, false),
        (101, false),
    ] {
        let data = build_data_index(&format!(
            "@prefix ex: <http://example.org/> .\nex:e a ex:ExclusiveItem ; ex:value {value} .\n"
        ));
        let report = Validator::validate(&data, &shapes);
        assert_eq!(
            report.conforms, should_conform,
            "exclusive range: value={} against (0,100) exclusive: expected conforms={}, got {}",
            value, should_conform, report.conforms
        );
    }
}

/// Type/lexical edge case: comparing an `xsd:dateTime` WITH an explicit
/// timezone against one WITHOUT a timezone is, per XSD's partial order,
/// "indeterminate" (the result depends on which of the +/-14:00 extremes is
/// assumed) -- this crate treats indeterminate comparisons as "constraint
/// not satisfied", matching the real W3C SHACL test suite
/// (minInclusive-002/003, vendored in shacl_conformance/). This test
/// isolates that behavior directly rather than relying only on the
/// vendored suite to exercise it.
#[test]
fn test_datetime_timezone_mismatch_is_indeterminate_and_violates() {
    let shapes_str = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

        ex:TestShape a sh:NodeShape ;
            sh:minInclusive "2002-10-10T12:00:00-05:00"^^xsd:dateTime ;
            sh:targetNode "2002-10-10T12:00:00-05:00"^^xsd:dateTime ,
                          "2002-10-10T12:00:01-05:00"^^xsd:dateTime ,
                          "2002-10-09T12:00:00-05:00"^^xsd:dateTime ,
                          "2002-10-10T12:00:00"^^xsd:dateTime .
    "#;
    let shapes = ShapesGraph::parse(shapes_str).unwrap();
    let data = build_data_index("@prefix ex: <http://example.org/> .\n");
    let report = Validator::validate(&data, &shapes);

    let violated: std::collections::HashSet<String> = report
        .results
        .iter()
        .map(|r| r.focus_node.to_string())
        .collect();

    assert_eq!(
        violated.len(), 2,
        "expected exactly 2 violations (the earlier date, and the timezone-less indeterminate value), got: {:?}", violated
    );
    assert!(
        !violated
            .iter()
            .any(|v| v.contains("2002-10-10T12:00:00-05:00")),
        "same-tz value equal to the bound must conform (no violation), got: {:?}",
        violated
    );
    assert!(
        !violated
            .iter()
            .any(|v| v.contains("2002-10-10T12:00:01-05:00")),
        "same-tz value greater than the bound must conform (no violation), got: {:?}",
        violated
    );
    assert!(
        violated
            .iter()
            .any(|v| v.contains("2002-10-09T12:00:00-05:00")),
        "same-tz value less than the bound must violate, got: {:?}",
        violated
    );
    assert!(
        violated.iter().any(|v| v == "\"2002-10-10T12:00:00\"^^<http://www.w3.org/2001/XMLSchema#dateTime>"),
        "a timezone-less value compared against a timezone-qualified bound is indeterminate and must violate (not silently pass), got: {:?}", violated
    );
}

/// Stress: 3 levels of nested sh:property (a property shape whose value's
/// own property shape has a further nested property shape), confirming the
/// recursive `validate_property_shape` handles depth beyond the 1-level
/// case the W3C suite's `property/property-001.ttl` exercises, and that
/// `sh:resultPath`/`sh:focusNode` stay correctly anchored at each level
/// rather than collapsing to the top-level focus node.
#[test]
fn test_three_level_nested_property_shape() {
    let shapes_str = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .

        ex:CompanyShape a sh:NodeShape ;
            sh:targetClass ex:Company ;
            sh:property [
                sh:path ex:address ;
                sh:property [
                    sh:path ex:city ;
                    sh:property [
                        sh:path ex:country ;
                        sh:class ex:Country ;
                    ] ;
                ] ;
            ] .
    "#;
    let shapes = ShapesGraph::parse(shapes_str).unwrap();

    // Valid chain: Company -> address -> city -> country (of the right class).
    let data_valid = build_data_index(
        r#"
        @prefix ex: <http://example.org/> .
        ex:acme a ex:Company ; ex:address ex:acmeAddr .
        ex:acmeAddr ex:city ex:acmeCity .
        ex:acmeCity ex:country ex:Wakanda .
        ex:Wakanda a ex:Country .
    "#,
    );
    let report_valid = Validator::validate(&data_valid, &shapes);
    assert!(
        report_valid.conforms,
        "a fully valid 3-level chain must conform, got violations: {:?}",
        report_valid.results
    );

    // Break the constraint at the deepest (3rd) level: country is not a ex:Country.
    let data_invalid = build_data_index(
        r#"
        @prefix ex: <http://example.org/> .
        ex:acme a ex:Company ; ex:address ex:acmeAddr .
        ex:acmeAddr ex:city ex:acmeCity .
        ex:acmeCity ex:country ex:NotACountry .
    "#,
    );
    let report_invalid = Validator::validate(&data_invalid, &shapes);
    assert!(
        !report_invalid.conforms,
        "a violation 3 levels deep must still be caught, not silently swallowed by the recursion"
    );
    assert_eq!(
        report_invalid.results.len(),
        1,
        "expected exactly 1 violation from the deepest nesting level, got: {:?}",
        report_invalid.results
    );
    let violation = &report_invalid.results[0];
    assert!(
        violation.focus_node.to_string().contains("acmeCity"),
        "the violation's focus_node must be anchored at the 3rd-level value (ex:acmeCity), not the top-level ex:acme, got: {}",
        violation.focus_node
    );
}
