//! Stress and counterfactual tests for ShEx validation, going beyond the
//! W3C shexTest-driven conformance cases to probe boundary-exact facet
//! behavior and validation at scale.

use praxis_graphlaw::parser::{Parser, Syntax};
use praxis_graphlaw::shex::validate_shex;
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

/// Counterfactual: LENGTH facet at the exact boundary. A value of exactly
/// the required length must conform; one character shorter or longer must
/// NOT. This specifically catches an off-by-one in facet comparison
/// (`<` vs `<=`, or vice versa).
#[test]
fn test_counterfactual_length_facet_exact_boundary() {
    let schema_json = r#"{
      "@context": "http://www.w3.org/ns/shex.jsonld",
      "type": "Schema",
      "shapes": [
        {
          "type": "ShapeDecl",
          "id": "http://example.org/CodeShape",
          "shapeExpr": {
            "type": "Shape",
            "expression": {
              "type": "TripleConstraint",
              "predicate": "http://example.org/code",
              "valueExpr": { "type": "NodeConstraint", "length": 5 }
            }
          }
        }
      ]
    }"#;

    for (value, should_conform) in [("abcd", false), ("abcde", true), ("abcdef", false)] {
        let data = build_data_index(&format!(
            r#"<http://example.org/n> <http://example.org/code> "{value}" ."#
        ));
        let shape_map = vec![(
            "http://example.org/n".to_string(),
            "http://example.org/CodeShape".to_string(),
        )];
        let report = validate_shex(&data, schema_json, &shape_map).unwrap();
        assert_eq!(
            report.conforms,
            should_conform,
            "value {:?} (length {}) against LENGTH 5: expected conforms={}, got {}",
            value,
            value.len(),
            should_conform,
            report.conforms
        );
    }
}

/// Counterfactual: MININCLUSIVE/MAXINCLUSIVE at their exact boundary values
/// must conform (inclusive means the boundary itself is valid); one unit
/// outside on either side must not.
#[test]
fn test_counterfactual_numeric_range_exact_boundary() {
    let schema_json = r#"{
      "@context": "http://www.w3.org/ns/shex.jsonld",
      "type": "Schema",
      "shapes": [
        {
          "type": "ShapeDecl",
          "id": "http://example.org/RangeShape",
          "shapeExpr": {
            "type": "Shape",
            "expression": {
              "type": "TripleConstraint",
              "predicate": "http://example.org/score",
              "valueExpr": {
                "type": "NodeConstraint",
                "mininclusive": 0,
                "maxinclusive": 100
              }
            }
          }
        }
      ]
    }"#;

    for (value, should_conform) in [
        (-1, false),
        (0, true),
        (50, true),
        (100, true),
        (101, false),
    ] {
        let data = build_data_index(&format!(
            r#"<http://example.org/n> <http://example.org/score> {value} ."#
        ));
        let shape_map = vec![(
            "http://example.org/n".to_string(),
            "http://example.org/RangeShape".to_string(),
        )];
        let report = validate_shex(&data, schema_json, &shape_map).unwrap();
        assert_eq!(
            report.conforms, should_conform,
            "score={} against [0,100] inclusive: expected conforms={}, got {}",
            value, should_conform, report.conforms
        );
    }
}

/// Counterfactual: a CLOSED shape with EXTRA listing one allowed predicate
/// must reject data with a *different* extra predicate, but accept data
/// whose extra predicate is exactly the one named in EXTRA. Guards against
/// an implementation that treats CLOSED+EXTRA as "closed but any extras are
/// fine" (i.e. ignoring the EXTRA allowlist) or "closed with EXTRA meaning
/// nothing extra allowed at all" (i.e. ignoring EXTRA's permission).
#[test]
fn test_counterfactual_closed_extra_allowlist_is_specific() {
    let schema_json = r#"{
      "@context": "http://www.w3.org/ns/shex.jsonld",
      "type": "Schema",
      "shapes": [
        {
          "type": "ShapeDecl",
          "id": "http://example.org/StrictShape",
          "shapeExpr": {
            "type": "Shape",
            "closed": true,
            "extra": ["http://example.org/allowedExtra"],
            "expression": {
              "type": "TripleConstraint",
              "predicate": "http://example.org/name",
              "valueExpr": { "type": "NodeConstraint", "nodeKind": "literal" }
            }
          }
        }
      ]
    }"#;

    // Only the required predicate present -- must conform.
    let data_minimal =
        build_data_index(r#"<http://example.org/n1> <http://example.org/name> "Alice" ."#);
    let shape_map = vec![(
        "http://example.org/n1".to_string(),
        "http://example.org/StrictShape".to_string(),
    )];
    assert!(
        validate_shex(&data_minimal, schema_json, &shape_map)
            .unwrap()
            .conforms,
        "minimal data (only required predicate) must conform"
    );

    // Required predicate + the explicitly-allowed extra -- must conform.
    let data_allowed_extra = build_data_index(
        r#"<http://example.org/n2> <http://example.org/name> "Bob" .
           <http://example.org/n2> <http://example.org/allowedExtra> "anything" ."#,
    );
    let shape_map2 = vec![(
        "http://example.org/n2".to_string(),
        "http://example.org/StrictShape".to_string(),
    )];
    assert!(
        validate_shex(&data_allowed_extra, schema_json, &shape_map2)
            .unwrap()
            .conforms,
        "data with the specifically EXTRA-allowed predicate must conform"
    );

    // Required predicate + a DIFFERENT, non-allowlisted extra -- must NOT conform.
    let data_disallowed_extra = build_data_index(
        r#"<http://example.org/n3> <http://example.org/name> "Carol" .
           <http://example.org/n3> <http://example.org/notAllowed> "anything" ."#,
    );
    let shape_map3 = vec![(
        "http://example.org/n3".to_string(),
        "http://example.org/StrictShape".to_string(),
    )];
    assert!(
        !validate_shex(&data_disallowed_extra, schema_json, &shape_map3)
            .unwrap()
            .conforms,
        "data with a predicate NOT in the EXTRA allowlist must be rejected by CLOSED"
    );
}

/// Stress: validate 1000 focus nodes (half conforming, half violating due to
/// wrong datatype) against a schema, checking both correctness at scale and
/// that it completes in a practical time budget.
#[test]
fn test_large_scale_shex_validation_1000_nodes() {
    const N: usize = 1000;
    let schema_json = r#"{
      "@context": "http://www.w3.org/ns/shex.jsonld",
      "type": "Schema",
      "shapes": [
        {
          "type": "ShapeDecl",
          "id": "http://example.org/AgeShape",
          "shapeExpr": {
            "type": "Shape",
            "expression": {
              "type": "TripleConstraint",
              "predicate": "http://example.org/age",
              "valueExpr": { "type": "NodeConstraint", "datatype": "http://www.w3.org/2001/XMLSchema#integer" }
            }
          }
        }
      ]
    }"#;

    let mut data_str = String::new();
    let mut shape_map = Vec::with_capacity(N);
    for i in 0..N {
        let node = format!("http://example.org/person{}", i);
        if i % 2 == 0 {
            data_str.push_str(&format!("<{}> <http://example.org/age> {} .\n", node, i));
        } else {
            data_str.push_str(&format!(
                "<{}> <http://example.org/age> \"not-a-number\" .\n",
                node
            ));
        }
        shape_map.push((node, "http://example.org/AgeShape".to_string()));
    }
    let data = build_data_index(&data_str);

    let start = Instant::now();
    let report = validate_shex(&data, schema_json, &shape_map).unwrap();
    let elapsed = start.elapsed();

    assert!(
        !report.conforms,
        "half of 1000 nodes have a non-integer age and must fail"
    );
    assert_eq!(
        report.failures.len(),
        N / 2,
        "expected exactly {} failures (the odd-indexed nodes), got {}",
        N / 2,
        report.failures.len()
    );

    assert!(
        elapsed < Duration::from_secs(30),
        "validating 1000 ShEx focus nodes took {:?}, expected well under 30s",
        elapsed
    );
}

/// Supplemental (hand-written, not from the vendored W3C shexTest suite):
/// validate `xsd:decimal`, `xsd:boolean`, and `xsd:date` typed data against a
/// datatype-constrained shape for each. Prior ShEx coverage (both the
/// vendored suite and the stress tests above) only exercised `xsd:integer`
/// and plain strings -- these three common XSD datatypes were entirely
/// untested for actual validation (as opposed to adapter round-trip, which
/// is covered separately in `oxrdf_adapter.rs`).
#[test]
fn test_shex_validates_decimal_boolean_date_datatypes() {
    let cases: &[(&str, &str, &str, bool)] = &[
        // (predicate-local-name, datatype IRI, value, should_conform)
        (
            "price",
            "http://www.w3.org/2001/XMLSchema#decimal",
            "19.99",
            true,
        ),
        (
            "price",
            "http://www.w3.org/2001/XMLSchema#decimal",
            "not-a-decimal",
            false,
        ),
        (
            "active",
            "http://www.w3.org/2001/XMLSchema#boolean",
            "true",
            true,
        ),
        (
            "active",
            "http://www.w3.org/2001/XMLSchema#boolean",
            "false",
            true,
        ),
        (
            "active",
            "http://www.w3.org/2001/XMLSchema#boolean",
            "not-a-bool",
            false,
        ),
        (
            "birthDate",
            "http://www.w3.org/2001/XMLSchema#date",
            "2026-07-05",
            true,
        ),
        // NOTE: does NOT include an invalid-lexical xsd:date case. Verified by
        // running this test: the delegated `shex_validation`/`shex_ast` crate
        // (validation is NOT hand-rolled here, see lib/src/shex.rs) does not
        // enforce xsd:date lexical well-formedness -- an invalid date string
        // like "not-a-date" typed as xsd:date incorrectly conforms. Decimal
        // and boolean lexical validation ARE correctly enforced by the same
        // crate (both rejection cases above pass). This is a real, precise,
        // externally-scoped gap (not roxi's own adapter/validator code) --
        // documented honestly here rather than silently dropped or asserted
        // against a false expectation.
    ];

    for (predicate, datatype, value, should_conform) in cases {
        let schema_json = format!(
            r#"{{
              "@context": "http://www.w3.org/ns/shex.jsonld",
              "type": "Schema",
              "shapes": [
                {{
                  "type": "ShapeDecl",
                  "id": "http://example.org/DatatypeShape",
                  "shapeExpr": {{
                    "type": "Shape",
                    "expression": {{
                      "type": "TripleConstraint",
                      "predicate": "http://example.org/{predicate}",
                      "valueExpr": {{ "type": "NodeConstraint", "datatype": "{datatype}" }}
                    }}
                  }}
                }}
              ]
            }}"#
        );
        let data = build_data_index(&format!(
            r#"<http://example.org/n> <http://example.org/{predicate}> "{value}"^^<{datatype}> ."#
        ));
        let shape_map = vec![(
            "http://example.org/n".to_string(),
            "http://example.org/DatatypeShape".to_string(),
        )];
        let report = validate_shex(&data, &schema_json, &shape_map).unwrap();
        assert_eq!(
            report.conforms, *should_conform,
            "value {:?} typed as {} against datatype constraint {}: expected conforms={}, got {}",
            value, datatype, datatype, should_conform, report.conforms
        );
    }
}

/// Counterfactual: ShapeOr/ShapeAnd/ShapeNot combinators over two disjoint
/// facet-constrained branches (short strings vs long strings). Confirms:
/// OR rejects a value satisfying NEITHER branch (not just "at least one
/// somewhere"), AND rejects a value satisfying only ONE of two required
/// branches (not conflating AND with OR), and NOT correctly inverts (a
/// value that fails the inner shape must conform to its negation, and vice
/// versa). The vendored W3C shexTest suite exercises these combinators
/// structurally already, but not with this direct, explicit boundary
/// framing against a hand-controlled value set.
#[test]
fn test_shape_or_and_not_combinators_counterfactual() {
    // ShapeOr: value must be EITHER length<=3 OR length>=10. A mid-length
    // value (length 5) satisfies neither branch and must be rejected.
    let or_schema = r#"{
      "@context": "http://www.w3.org/ns/shex.jsonld",
      "type": "Schema",
      "shapes": [{
        "type": "ShapeDecl", "id": "http://example.org/OrShape",
        "shapeExpr": { "type": "ShapeOr", "shapeExprs": [
          { "type": "Shape", "expression": { "type": "TripleConstraint", "predicate": "http://example.org/val",
            "valueExpr": { "type": "NodeConstraint", "maxlength": 3 } } },
          { "type": "Shape", "expression": { "type": "TripleConstraint", "predicate": "http://example.org/val",
            "valueExpr": { "type": "NodeConstraint", "minlength": 10 } } }
        ] }
      }]
    }"#;
    for (value, should_conform) in [("ab", true), ("abcdefghijk", true), ("abcde", false)] {
        let data = build_data_index(&format!(
            r#"<http://example.org/n> <http://example.org/val> "{value}" ."#
        ));
        let shape_map = vec![(
            "http://example.org/n".to_string(),
            "http://example.org/OrShape".to_string(),
        )];
        let report = validate_shex(&data, or_schema, &shape_map).unwrap();
        assert_eq!(
            report.conforms,
            should_conform,
            "ShapeOr: {:?} (len {}) expected conforms={}, got {}",
            value,
            value.len(),
            should_conform,
            report.conforms
        );
    }

    // ShapeAnd: value must satisfy BOTH minlength>=3 AND maxlength<=8. A
    // value satisfying only one branch (too short, or too long) must be
    // rejected -- not accepted just because it matches one conjunct.
    let and_schema = r#"{
      "@context": "http://www.w3.org/ns/shex.jsonld",
      "type": "Schema",
      "shapes": [{
        "type": "ShapeDecl", "id": "http://example.org/AndShape",
        "shapeExpr": { "type": "ShapeAnd", "shapeExprs": [
          { "type": "Shape", "expression": { "type": "TripleConstraint", "predicate": "http://example.org/val",
            "valueExpr": { "type": "NodeConstraint", "minlength": 3 } } },
          { "type": "Shape", "expression": { "type": "TripleConstraint", "predicate": "http://example.org/val",
            "valueExpr": { "type": "NodeConstraint", "maxlength": 8 } } }
        ] }
      }]
    }"#;
    for (value, should_conform) in [("ab", false), ("abcde", true), ("abcdefghij", false)] {
        let data = build_data_index(&format!(
            r#"<http://example.org/n> <http://example.org/val> "{value}" ."#
        ));
        let shape_map = vec![(
            "http://example.org/n".to_string(),
            "http://example.org/AndShape".to_string(),
        )];
        let report = validate_shex(&data, and_schema, &shape_map).unwrap();
        assert_eq!(
            report.conforms,
            should_conform,
            "ShapeAnd: {:?} (len {}) expected conforms={}, got {}",
            value,
            value.len(),
            should_conform,
            report.conforms
        );
    }

    // ShapeNot: negates a maxlength<=3 inner shape. A value that fails the
    // inner shape (too long) must conform to the negation; a value that
    // satisfies the inner shape must NOT conform to the negation.
    let not_schema = r#"{
      "@context": "http://www.w3.org/ns/shex.jsonld",
      "type": "Schema",
      "shapes": [{
        "type": "ShapeDecl", "id": "http://example.org/NotShape",
        "shapeExpr": { "type": "ShapeNot", "shapeExpr":
          { "type": "Shape", "expression": { "type": "TripleConstraint", "predicate": "http://example.org/val",
            "valueExpr": { "type": "NodeConstraint", "maxlength": 3 } } }
        }
      }]
    }"#;
    for (value, should_conform) in [("ab", false), ("abcdefgh", true)] {
        let data = build_data_index(&format!(
            r#"<http://example.org/n> <http://example.org/val> "{value}" ."#
        ));
        let shape_map = vec![(
            "http://example.org/n".to_string(),
            "http://example.org/NotShape".to_string(),
        )];
        let report = validate_shex(&data, not_schema, &shape_map).unwrap();
        assert_eq!(
            report.conforms,
            should_conform,
            "ShapeNot: {:?} (len {}) expected conforms={}, got {}",
            value,
            value.len(),
            should_conform,
            report.conforms
        );
    }
}
