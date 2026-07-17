//! Catalog items #1-13 (docs/jira/26.7.4 impossible/illogical constructs
//! plan): SHACL shapes that are structurally unsatisfiable or self-
//! contradictory. Where a natural value-range exists to fuzz over, uses
//! `proptest` (via `chicago-tdd-tools`'s `property-testing` feature) to
//! sweep many generated inputs and assert the real `Validator::validate`
//! engine reports non-conformance for *every* generated case -- not just a
//! few hand-picked examples. Items whose contradiction is purely structural
//! (independent of any data value) stay as deterministic tests.

use praxis_graphlaw::parser::{Parser, Syntax};
use praxis_graphlaw::shacl::{ShapesGraph, Validator};
use praxis_graphlaw::tripleindex::TripleIndex;
use proptest::prelude::*;

fn build_data_index(data_str: &str) -> TripleIndex {
    let triples = Parser::parse_triples(data_str, Syntax::Turtle).unwrap();
    let mut index = TripleIndex::new();
    for t in triples {
        index.add(t);
    }
    index
}

// ---------------------------------------------------------------------
// Item #1: sh:minCount 5 + sh:maxCount 2 on the same property -- min
// exceeds max, so no possible number of values can satisfy both.
// ---------------------------------------------------------------------
proptest! {
    #[test]
    fn prop_mincount_exceeds_maxcount_never_conforms(n in 0usize..10) {
        let shapes_str = r#"
            @prefix sh: <http://www.w3.org/ns/shacl#> .
            @prefix ex: <http://example.org/> .
            ex:S a sh:NodeShape ; sh:targetClass ex:Item ;
                sh:property [ sh:path ex:tag ; sh:minCount 5 ; sh:maxCount 2 ] .
        "#;
        let shapes = ShapesGraph::parse(shapes_str).unwrap();
        let mut data_str = String::from("@prefix ex: <http://example.org/> .\nex:i a ex:Item .\n");
        for i in 0..n {
            data_str.push_str(&format!("ex:i ex:tag \"t{}\" .\n", i));
        }
        let data = build_data_index(&data_str);
        let report = Validator::validate(&data, &shapes);
        prop_assert!(!report.conforms, "minCount 5 + maxCount 2 with {} actual values must never conform", n);
    }
}

/// Item #2: sh:hasValue ex:A + sh:in (ex:B ex:C), where ex:A is not a
/// member of the sh:in list -- the value must simultaneously equal ex:A
/// AND be one of {ex:B, ex:C}, impossible since A != B and A != C.
#[test]
fn test_hasvalue_outside_in_list_never_conforms() {
    let shapes_str = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .
        ex:S a sh:NodeShape ; sh:targetClass ex:Item ;
            sh:property [ sh:path ex:status ; sh:hasValue ex:A ; sh:in ( ex:B ex:C ) ] .
    "#;
    let shapes = ShapesGraph::parse(shapes_str).unwrap();
    for candidate in ["ex:A", "ex:B", "ex:C", "ex:D"] {
        let data = build_data_index(&format!(
            "@prefix ex: <http://example.org/> .\nex:i a ex:Item ; ex:status {candidate} .\n"
        ));
        let report = Validator::validate(&data, &shapes);
        assert!(
            !report.conforms,
            "hasValue ex:A + in(ex:B,ex:C) with candidate {candidate} must never conform"
        );
    }
}

// ---------------------------------------------------------------------
// Item #3: sh:minInclusive 10 + sh:maxInclusive 5 -- empty numeric range.
// ---------------------------------------------------------------------
proptest! {
    #[test]
    fn prop_empty_numeric_range_never_conforms(v in -1000i64..1000i64) {
        let shapes_str = r#"
            @prefix sh: <http://www.w3.org/ns/shacl#> .
            @prefix ex: <http://example.org/> .
            ex:S a sh:NodeShape ; sh:targetClass ex:Item ;
                sh:property [ sh:path ex:value ; sh:minInclusive 10 ; sh:maxInclusive 5 ] .
        "#;
        let shapes = ShapesGraph::parse(shapes_str).unwrap();
        let data = build_data_index(&format!(
            "@prefix ex: <http://example.org/> .\nex:i a ex:Item ; ex:value {v} .\n"
        ));
        let report = Validator::validate(&data, &shapes);
        prop_assert!(!report.conforms, "value {} against empty range [10,5] must never conform", v);
    }
}

// ---------------------------------------------------------------------
// Item #4: sh:datatype xsd:integer + sh:datatype xsd:string as two
// sibling constraints -- a literal has exactly one datatype, so both can
// never hold simultaneously regardless of the literal's actual value.
// ---------------------------------------------------------------------
proptest! {
    #[test]
    fn prop_conflicting_sibling_datatypes_never_conforms(v in -1000i64..1000i64) {
        let shapes_str = r#"
            @prefix sh: <http://www.w3.org/ns/shacl#> .
            @prefix ex: <http://example.org/> .
            @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
            ex:S a sh:NodeShape ; sh:targetClass ex:Item ;
                sh:property [ sh:path ex:value ; sh:datatype xsd:integer ; sh:datatype xsd:string ] .
        "#;
        let shapes = ShapesGraph::parse(shapes_str).unwrap();
        let data = build_data_index(&format!(
            "@prefix ex: <http://example.org/> .\nex:i a ex:Item ; ex:value {v} .\n"
        ));
        let report = Validator::validate(&data, &shapes);
        prop_assert!(!report.conforms, "an xsd:integer-typed value {} can never also satisfy sh:datatype xsd:string", v);
    }
}

/// Item #5: sh:class ex:Foo + sh:not [ sh:class ex:Foo ] on the same node
/// shape -- a value must both BE and NOT-BE ex:Foo. Purely structural
/// (independent of data), so a single deterministic test suffices.
#[test]
fn test_class_and_not_class_direct_self_negation_never_conforms() {
    let shapes_str = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .
        ex:S a sh:NodeShape ; sh:targetClass ex:Item ;
            sh:class ex:Foo ;
            sh:not [ sh:class ex:Foo ] .
    "#;
    let shapes = ShapesGraph::parse(shapes_str).unwrap();
    for is_foo in [true, false] {
        let data = build_data_index(&format!(
            "@prefix ex: <http://example.org/> .\nex:i a ex:Item{} .\n",
            if is_foo { " ; a ex:Foo" } else { "" }
        ));
        let report = Validator::validate(&data, &shapes);
        assert!(
            !report.conforms,
            "sh:class ex:Foo + sh:not[sh:class ex:Foo] (is_foo={}) must never conform",
            is_foo
        );
    }
}

// ---------------------------------------------------------------------
// Item #6: sh:in () (empty list) + sh:minCount 1 -- requires at least one
// value, but every value must be a member of an empty set.
// ---------------------------------------------------------------------
proptest! {
    #[test]
    fn prop_empty_in_list_with_mincount_never_conforms(n in 0usize..5) {
        let shapes_str = r#"
            @prefix sh: <http://www.w3.org/ns/shacl#> .
            @prefix ex: <http://example.org/> .
            ex:S a sh:NodeShape ; sh:targetClass ex:Item ;
                sh:property [ sh:path ex:tag ; sh:in () ; sh:minCount 1 ] .
        "#;
        let shapes = ShapesGraph::parse(shapes_str).unwrap();
        let mut data_str = String::from("@prefix ex: <http://example.org/> .\nex:i a ex:Item .\n");
        for i in 0..n {
            data_str.push_str(&format!("ex:i ex:tag \"t{}\" .\n", i));
        }
        let data = build_data_index(&data_str);
        let report = Validator::validate(&data, &shapes);
        prop_assert!(!report.conforms, "sh:in()+minCount 1 with {} values must never conform (0 values fails minCount, >=1 values fails in())", n);
    }
}

// ---------------------------------------------------------------------
// Item #7: sh:equals ex:p1 + sh:disjoint ex:p1 on the same path -- values
// must equal AND be disjoint from ex:p1's values; only possible if both
// sets are empty, which is itself contradicted by any sh:minCount 1.
// ---------------------------------------------------------------------
proptest! {
    #[test]
    fn prop_equals_and_disjoint_same_path_never_conforms(v in 0i64..100) {
        let shapes_str = r#"
            @prefix sh: <http://www.w3.org/ns/shacl#> .
            @prefix ex: <http://example.org/> .
            ex:S a sh:NodeShape ; sh:targetClass ex:Item ;
                sh:property [ sh:path ex:p ; sh:equals ex:p1 ; sh:disjoint ex:p1 ; sh:minCount 1 ] .
        "#;
        let shapes = ShapesGraph::parse(shapes_str).unwrap();
        let data = build_data_index(&format!(
            "@prefix ex: <http://example.org/> .\nex:i a ex:Item ; ex:p {v} ; ex:p1 {v} .\n"
        ));
        let report = Validator::validate(&data, &shapes);
        prop_assert!(!report.conforms, "ex:p={} equal to ex:p1={} (required by sh:equals) directly violates sh:disjoint from the same values", v, v);
    }
}

// ---------------------------------------------------------------------
// Item #8: sh:lessThan ex:p1 + sh:equals ex:p1 on the same path -- value
// must be strictly less than AND equal to the same comparison value.
// ---------------------------------------------------------------------
proptest! {
    #[test]
    fn prop_lessthan_and_equals_same_path_never_conforms(v in 0i64..100) {
        let shapes_str = r#"
            @prefix sh: <http://www.w3.org/ns/shacl#> .
            @prefix ex: <http://example.org/> .
            ex:S a sh:NodeShape ; sh:targetClass ex:Item ;
                sh:property [ sh:path ex:p ; sh:lessThan ex:p1 ; sh:equals ex:p1 ; sh:minCount 1 ] .
        "#;
        let shapes = ShapesGraph::parse(shapes_str).unwrap();
        let data = build_data_index(&format!(
            "@prefix ex: <http://example.org/> .\nex:i a ex:Item ; ex:p {v} ; ex:p1 {v} .\n"
        ));
        let report = Validator::validate(&data, &shapes);
        prop_assert!(!report.conforms, "ex:p={} cannot be both < ex:p1={} and == ex:p1", v, v);
    }
}

/// Item #9: sh:xone over two logically-equivalent (co-extensive)
/// sub-shapes -- since both branches always agree (either both match or
/// neither), "exactly one" can never hold. Purely structural.
#[test]
fn test_xone_over_coextensive_shapes_never_conforms() {
    let shapes_str = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .
        ex:S a sh:NodeShape ; sh:targetClass ex:Item ;
            sh:xone (
                [ sh:property [ sh:path ex:value ; sh:minInclusive 0 ] ]
                [ sh:property [ sh:path ex:value ; sh:minInclusive 0 ] ]
            ) .
    "#;
    let shapes = ShapesGraph::parse(shapes_str).unwrap();
    for v in [-5, 0, 5, 100] {
        let data = build_data_index(&format!(
            "@prefix ex: <http://example.org/> .\nex:i a ex:Item ; ex:value {v} .\n"
        ));
        let report = Validator::validate(&data, &shapes);
        assert!(!report.conforms, "value {} against xone of two identical branches must never conform (both match or neither, never exactly one)", v);
    }
}

// ---------------------------------------------------------------------
// Item #10: sh:minCount 1 + sh:maxCount 0 -- fewer than the minimum
// required, impossible for any actual value count.
// ---------------------------------------------------------------------
proptest! {
    #[test]
    fn prop_mincount_one_maxcount_zero_never_conforms(n in 0usize..5) {
        let shapes_str = r#"
            @prefix sh: <http://www.w3.org/ns/shacl#> .
            @prefix ex: <http://example.org/> .
            ex:S a sh:NodeShape ; sh:targetClass ex:Item ;
                sh:property [ sh:path ex:tag ; sh:minCount 1 ; sh:maxCount 0 ] .
        "#;
        let shapes = ShapesGraph::parse(shapes_str).unwrap();
        let mut data_str = String::from("@prefix ex: <http://example.org/> .\nex:i a ex:Item .\n");
        for i in 0..n {
            data_str.push_str(&format!("ex:i ex:tag \"t{}\" .\n", i));
        }
        let data = build_data_index(&data_str);
        let report = Validator::validate(&data, &shapes);
        prop_assert!(!report.conforms, "minCount 1 + maxCount 0 with {} values must never conform", n);
    }
}

/// Item #11: sh:qualifiedValueShape with sh:qualifiedMinCount set higher
/// than the property's own sh:maxCount -- requires more qualifying values
/// than the total number of values ever allowed to exist.
#[test]
fn test_qualified_mincount_exceeds_maxcount_never_conforms() {
    let shapes_str = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .
        ex:S a sh:NodeShape ; sh:targetClass ex:Item ;
            sh:property [
                sh:path ex:tag ;
                sh:maxCount 2 ;
                sh:qualifiedValueShape [ sh:minInclusive 0 ] ;
                sh:qualifiedMinCount 3 ;
            ] .
    "#;
    let shapes = ShapesGraph::parse(shapes_str).unwrap();
    for n in 0..=3 {
        let mut data_str = String::from("@prefix ex: <http://example.org/> .\nex:i a ex:Item .\n");
        for i in 0..n {
            data_str.push_str(&format!("ex:i ex:tag {} .\n", i));
        }
        let data = build_data_index(&data_str);
        let report = Validator::validate(&data, &shapes);
        assert!(!report.conforms, "{} tag values against maxCount=2 + qualifiedMinCount=3 must never conform (either maxCount or qualifiedMinCount always fails)", n);
    }
}

/// Item #12: sh:languageIn ("en") + sh:datatype xsd:integer on the same
/// property -- a language-tagged literal is always `rdf:langString`
/// typed, never `xsd:integer`; the conjunction is unsatisfiable under
/// RDF's own literal model, not just under this validator's choices.
#[test]
fn test_languagein_and_integer_datatype_never_conforms() {
    let shapes_str = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        ex:S a sh:NodeShape ; sh:targetClass ex:Item ;
            sh:property [ sh:path ex:value ; sh:languageIn ("en") ; sh:datatype xsd:integer ; sh:minCount 1 ] .
    "#;
    let shapes = ShapesGraph::parse(shapes_str).unwrap();
    // Try every RDF-representable attempt: a lang-tagged literal (fails
    // sh:datatype), a plain integer (fails sh:languageIn, no tag at all),
    // and an integer-lexical-form-but-lang-tagged literal (still not
    // xsd:integer-typed under RDF's literal model).
    for data_str in [
        r#"@prefix ex: <http://example.org/> .
           ex:i a ex:Item ; ex:value "hello"@en ."#,
        r#"@prefix ex: <http://example.org/> .
           ex:i a ex:Item ; ex:value 42 ."#,
        r#"@prefix ex: <http://example.org/> .
           ex:i a ex:Item ; ex:value "42"@en ."#,
    ] {
        let data = build_data_index(data_str);
        let report = Validator::validate(&data, &shapes);
        assert!(
            !report.conforms,
            "no real RDF literal can be both language-tagged and xsd:integer-typed, data: {:?}",
            data_str
        );
    }
}

// ---------------------------------------------------------------------
// Item #13: sh:nodeKind sh:IRI + sh:nodeKind sh:Literal as two sibling
// constraints -- a term is exactly one node kind, never both, regardless
// of which actual term (IRI, literal, or blank node) is supplied.
// ---------------------------------------------------------------------
proptest! {
    #[test]
    fn prop_conflicting_sibling_nodekinds_never_conforms(kind in 0u8..3) {
        let shapes_str = r#"
            @prefix sh: <http://www.w3.org/ns/shacl#> .
            @prefix ex: <http://example.org/> .
            ex:S a sh:NodeShape ; sh:targetClass ex:Item ;
                sh:property [ sh:path ex:value ; sh:nodeKind sh:IRI ; sh:nodeKind sh:Literal ; sh:minCount 1 ] .
        "#;
        let shapes = ShapesGraph::parse(shapes_str).unwrap();
        let value_triple = match kind {
            0 => "ex:i ex:value ex:SomeIri .".to_string(),
            1 => "ex:i ex:value \"a literal\" .".to_string(),
            _ => "ex:i ex:value _:someBlank .".to_string(),
        };
        let data = build_data_index(&format!(
            "@prefix ex: <http://example.org/> .\nex:i a ex:Item .\n{}\n", value_triple
        ));
        let report = Validator::validate(&data, &shapes);
        prop_assert!(!report.conforms, "kind variant {} against sh:nodeKind IRI+Literal must never conform (a term is exactly one kind)", kind);
    }
}
