//! Catalog items #14-16 (docs/jira/26.7.4 impossible/illogical constructs
//! plan): ShEx shape expressions that are structurally unsatisfiable
//! (14, 15) or redundant-but-not-contradictory (16). Uses `proptest` (via
//! `chicago-tdd-tools`'s `property-testing` feature) to sweep generated
//! inputs against the real `validate_shex` engine where a natural range
//! exists.

use praxis_graphlaw::parser::{Parser, Syntax};
use praxis_graphlaw::shex::validate_shex;
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
// Item #14: minlength 10 + maxlength 5 -- empty valid string-length range,
// no string of any length can satisfy both.
// ---------------------------------------------------------------------
proptest! {
    #[test]
    fn prop_empty_length_range_never_conforms(len in 0usize..30) {
        let schema_json = r#"{
          "@context": "http://www.w3.org/ns/shex.jsonld",
          "type": "Schema",
          "shapes": [{
            "type": "ShapeDecl", "id": "http://example.org/EmptyRangeShape",
            "shapeExpr": { "type": "Shape", "expression": {
              "type": "TripleConstraint", "predicate": "http://example.org/val",
              "valueExpr": { "type": "NodeConstraint", "minlength": 10, "maxlength": 5 }
            }}
          }]
        }"#;
        let value: String = "a".repeat(len);
        let data = build_data_index(&format!(
            r#"<http://example.org/n> <http://example.org/val> "{value}" ."#
        ));
        let shape_map = vec![("http://example.org/n".to_string(), "http://example.org/EmptyRangeShape".to_string())];
        let report = validate_shex(&data, schema_json, &shape_map).unwrap();
        prop_assert!(!report.conforms, "a string of length {} can never satisfy minlength=10 AND maxlength=5", len);
    }
}

// ---------------------------------------------------------------------
// Item #15: ShapeAnd combining two NodeConstraints with mutually
// exclusive datatypes (xsd:integer vs xsd:string) -- no value can ever
// satisfy both conjuncts, regardless of its actual value.
// ---------------------------------------------------------------------
proptest! {
    #[test]
    fn prop_shapeand_conflicting_datatypes_never_conforms(v in -1000i64..1000i64) {
        let schema_json = r#"{
          "@context": "http://www.w3.org/ns/shex.jsonld",
          "type": "Schema",
          "shapes": [{
            "type": "ShapeDecl", "id": "http://example.org/AndShape",
            "shapeExpr": { "type": "ShapeAnd", "shapeExprs": [
              { "type": "Shape", "expression": { "type": "TripleConstraint", "predicate": "http://example.org/val",
                "valueExpr": { "type": "NodeConstraint", "datatype": "http://www.w3.org/2001/XMLSchema#integer" } } },
              { "type": "Shape", "expression": { "type": "TripleConstraint", "predicate": "http://example.org/val",
                "valueExpr": { "type": "NodeConstraint", "datatype": "http://www.w3.org/2001/XMLSchema#string" } } }
            ] }
          }]
        }"#;
        let data = build_data_index(&format!(
            r#"<http://example.org/n> <http://example.org/val> {v} ."#
        ));
        let shape_map = vec![("http://example.org/n".to_string(), "http://example.org/AndShape".to_string())];
        let report = validate_shex(&data, schema_json, &shape_map).unwrap();
        prop_assert!(!report.conforms, "value {} typed xsd:integer can never also satisfy datatype xsd:string via ShapeAnd", v);
    }
}

/// Item #16: a CLOSED shape whose EXTRA list names a predicate that's ALSO
/// one of the shape's own required TripleConstraint predicates. Unlike
/// items 14-15, this is NOT a genuine unsatisfiability -- EXTRA and a
/// required TripleConstraint on the same predicate are redundant, not
/// contradictory (the predicate is both mandated once and separately
/// exempted from the closed-shape check, which is a no-op given it's
/// already required). This test confirms the engine treats it sanely:
/// data satisfying the required constraint (predicate present exactly
/// once) still conforms, and no double-counting/crash occurs -- verifying
/// "illogical but harmless" rather than assuming it must fail.
#[test]
fn test_closed_extra_naming_required_predicate_is_redundant_not_contradictory() {
    let schema_json = r#"{
      "@context": "http://www.w3.org/ns/shex.jsonld",
      "type": "Schema",
      "shapes": [{
        "type": "ShapeDecl", "id": "http://example.org/RedundantShape",
        "shapeExpr": {
          "type": "Shape",
          "closed": true,
          "extra": ["http://example.org/name"],
          "expression": {
            "type": "TripleConstraint",
            "predicate": "http://example.org/name",
            "valueExpr": { "type": "NodeConstraint", "nodeKind": "literal" }
          }
        }
      }]
    }"#;

    // The required predicate present exactly once must still conform --
    // the redundant EXTRA declaration must not break normal validation.
    let data = build_data_index(r#"<http://example.org/n> <http://example.org/name> "Alice" ."#);
    let shape_map = vec![(
        "http://example.org/n".to_string(),
        "http://example.org/RedundantShape".to_string(),
    )];
    let report = validate_shex(&data, schema_json, &shape_map).unwrap();
    assert!(
        report.conforms,
        "a redundant (required+extra) predicate declaration must not prevent normal conformance"
    );

    // A genuinely disallowed extra predicate (neither required nor listed
    // in EXTRA) must still be rejected by CLOSED -- the redundant
    // declaration doesn't accidentally widen the allowlist to "anything."
    let data_bad = build_data_index(
        r#"<http://example.org/n2> <http://example.org/name> "Bob" .
           <http://example.org/n2> <http://example.org/notAllowed> "x" ."#,
    );
    let shape_map2 = vec![(
        "http://example.org/n2".to_string(),
        "http://example.org/RedundantShape".to_string(),
    )];
    let report2 = validate_shex(&data_bad, schema_json, &shape_map2).unwrap();
    assert!(!report2.conforms, "CLOSED must still reject a genuinely disallowed extra predicate despite the redundant EXTRA declaration");
}
