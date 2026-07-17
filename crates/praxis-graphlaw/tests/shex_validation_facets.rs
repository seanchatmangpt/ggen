use praxis_graphlaw::parser::{Parser, Syntax};
use praxis_graphlaw::shex::validate_shex;
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
fn test_value_set_iris() {
    let schema_json = r#"{
      "@context": "http://www.w3.org/ns/shex.jsonld",
      "type": "Schema",
      "shapes": [
        {
          "type": "ShapeDecl",
          "id": "http://example.org/ColorShape",
          "shapeExpr": {
            "type": "Shape",
            "expression": {
              "type": "TripleConstraint",
              "predicate": "http://example.org/color",
              "valueExpr": {
                "type": "NodeConstraint",
                "values": [
                  "http://example.org/Red",
                  "http://example.org/Green",
                  "http://example.org/Blue"
                ]
              }
            }
          }
        }
      ]
    }"#;

    let data_str = r#"
        <http://example.org/Alice> <http://example.org/color> <http://example.org/Red> .
        <http://example.org/Bob> <http://example.org/color> <http://example.org/Purple> .
    "#;

    let data = build_data_index(data_str);
    let shape_map = vec![
        (
            "http://example.org/Alice".to_string(),
            "http://example.org/ColorShape".to_string(),
        ),
        (
            "http://example.org/Bob".to_string(),
            "http://example.org/ColorShape".to_string(),
        ),
    ];

    let report = validate_shex(&data, schema_json, &shape_map).unwrap();
    assert!(!report.conforms);
    assert_eq!(report.failures.len(), 1);
    assert_eq!(
        report.failures[0].node.to_string(),
        "<http://example.org/Bob>"
    );
}

#[test]
fn test_iri_stem_value_set() {
    let schema_json = r#"{
      "@context": "http://www.w3.org/ns/shex.jsonld",
      "type": "Schema",
      "shapes": [
        {
          "type": "ShapeDecl",
          "id": "http://example.org/ColorRefShape",
          "shapeExpr": {
            "type": "Shape",
            "expression": {
              "type": "TripleConstraint",
              "predicate": "http://example.org/color",
              "valueExpr": {
                "type": "NodeConstraint",
                "values": [
                  { "type": "IriStem", "stem": "http://example.org/colors/" }
                ]
              }
            }
          }
        }
      ]
    }"#;

    let data_str = r#"
        <http://example.org/Alice> <http://example.org/color> <http://example.org/colors/red> .
        <http://example.org/Bob> <http://example.org/color> <http://example.org/other/red> .
    "#;

    let data = build_data_index(data_str);
    let shape_map = vec![
        (
            "http://example.org/Alice".to_string(),
            "http://example.org/ColorRefShape".to_string(),
        ),
        (
            "http://example.org/Bob".to_string(),
            "http://example.org/ColorRefShape".to_string(),
        ),
    ];

    let report = validate_shex(&data, schema_json, &shape_map).unwrap();
    assert!(!report.conforms);
    assert_eq!(report.failures.len(), 1);
    assert_eq!(
        report.failures[0].node.to_string(),
        "<http://example.org/Bob>"
    );
}

#[test]
fn test_string_length_and_pattern_facets() {
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
              "valueExpr": {
                "type": "NodeConstraint",
                "nodeKind": "literal",
                "minlength": 3,
                "maxlength": 10,
                "pattern": "^[A-Za-z]+$"
              }
            }
          }
        }
      ]
    }"#;

    let data_str = r#"
        <http://example.org/Alice> <http://example.org/code> "Hello" .
        <http://example.org/Bob> <http://example.org/code> "H1" .
    "#;

    let data = build_data_index(data_str);
    let shape_map = vec![
        (
            "http://example.org/Alice".to_string(),
            "http://example.org/CodeShape".to_string(),
        ),
        (
            "http://example.org/Bob".to_string(),
            "http://example.org/CodeShape".to_string(),
        ),
    ];

    let report = validate_shex(&data, schema_json, &shape_map).unwrap();
    assert!(!report.conforms);
    assert_eq!(report.failures.len(), 1);
    assert_eq!(
        report.failures[0].node.to_string(),
        "<http://example.org/Bob>"
    );
}

#[test]
fn test_numeric_inclusive_facets() {
    let schema_json = r#"{
      "@context": "http://www.w3.org/ns/shex.jsonld",
      "type": "Schema",
      "shapes": [
        {
          "type": "ShapeDecl",
          "id": "http://example.org/ScoreShape",
          "shapeExpr": {
            "type": "Shape",
            "expression": {
              "type": "TripleConstraint",
              "predicate": "http://example.org/score",
              "valueExpr": {
                "type": "NodeConstraint",
                "datatype": "http://www.w3.org/2001/XMLSchema#integer",
                "mininclusive": 1,
                "maxinclusive": 100
              }
            }
          }
        }
      ]
    }"#;

    let data_str = r#"
        <http://example.org/Alice> <http://example.org/score> 50 .
        <http://example.org/Bob> <http://example.org/score> 200 .
    "#;

    let data = build_data_index(data_str);
    let shape_map = vec![
        (
            "http://example.org/Alice".to_string(),
            "http://example.org/ScoreShape".to_string(),
        ),
        (
            "http://example.org/Bob".to_string(),
            "http://example.org/ScoreShape".to_string(),
        ),
    ];

    let report = validate_shex(&data, schema_json, &shape_map).unwrap();
    assert!(!report.conforms);
    assert_eq!(report.failures.len(), 1);
    assert_eq!(
        report.failures[0].node.to_string(),
        "<http://example.org/Bob>"
    );
}

#[test]
fn test_node_kind_iri() {
    let schema_json = r#"{
      "@context": "http://www.w3.org/ns/shex.jsonld",
      "type": "Schema",
      "shapes": [
        {
          "type": "ShapeDecl",
          "id": "http://example.org/KnowsIriShape",
          "shapeExpr": {
            "type": "Shape",
            "expression": {
              "type": "TripleConstraint",
              "predicate": "http://example.org/knows",
              "valueExpr": { "type": "NodeConstraint", "nodeKind": "iri" }
            }
          }
        }
      ]
    }"#;

    let data_str = r#"
        <http://example.org/Alice> <http://example.org/knows> <http://example.org/Bob> .
        <http://example.org/Carol> <http://example.org/knows> "not an iri" .
    "#;

    let data = build_data_index(data_str);
    let shape_map = vec![
        (
            "http://example.org/Alice".to_string(),
            "http://example.org/KnowsIriShape".to_string(),
        ),
        (
            "http://example.org/Carol".to_string(),
            "http://example.org/KnowsIriShape".to_string(),
        ),
    ];

    let report = validate_shex(&data, schema_json, &shape_map).unwrap();
    assert!(!report.conforms);
    assert_eq!(report.failures.len(), 1);
    assert_eq!(
        report.failures[0].node.to_string(),
        "<http://example.org/Carol>"
    );
}

#[test]
fn test_node_kind_bnode() {
    let schema_json = r#"{
      "@context": "http://www.w3.org/ns/shex.jsonld",
      "type": "Schema",
      "shapes": [
        {
          "type": "ShapeDecl",
          "id": "http://example.org/KnowsBnodeShape",
          "shapeExpr": {
            "type": "Shape",
            "expression": {
              "type": "TripleConstraint",
              "predicate": "http://example.org/knows",
              "valueExpr": { "type": "NodeConstraint", "nodeKind": "bnode" }
            }
          }
        }
      ]
    }"#;

    let data_str = r#"
        <http://example.org/Alice> <http://example.org/knows> _:b1 .
        <http://example.org/Bob> <http://example.org/knows> <http://example.org/SomeIri> .
    "#;

    let data = build_data_index(data_str);
    let shape_map = vec![
        (
            "http://example.org/Alice".to_string(),
            "http://example.org/KnowsBnodeShape".to_string(),
        ),
        (
            "http://example.org/Bob".to_string(),
            "http://example.org/KnowsBnodeShape".to_string(),
        ),
    ];

    let report = validate_shex(&data, schema_json, &shape_map).unwrap();
    assert!(!report.conforms);
    assert_eq!(report.failures.len(), 1);
    assert_eq!(
        report.failures[0].node.to_string(),
        "<http://example.org/Bob>"
    );
}

#[test]
fn test_node_kind_nonliteral() {
    let schema_json = r#"{
      "@context": "http://www.w3.org/ns/shex.jsonld",
      "type": "Schema",
      "shapes": [
        {
          "type": "ShapeDecl",
          "id": "http://example.org/RefShape",
          "shapeExpr": {
            "type": "Shape",
            "expression": {
              "type": "TripleConstraint",
              "predicate": "http://example.org/ref",
              "valueExpr": { "type": "NodeConstraint", "nodeKind": "nonliteral" }
            }
          }
        }
      ]
    }"#;

    let data_str = r#"
        <http://example.org/Alice> <http://example.org/ref> <http://example.org/X> .
        <http://example.org/Bob> <http://example.org/ref> _:b2 .
        <http://example.org/Charlie> <http://example.org/ref> "a literal" .
    "#;

    let data = build_data_index(data_str);
    let shape_map = vec![
        (
            "http://example.org/Alice".to_string(),
            "http://example.org/RefShape".to_string(),
        ),
        (
            "http://example.org/Bob".to_string(),
            "http://example.org/RefShape".to_string(),
        ),
        (
            "http://example.org/Charlie".to_string(),
            "http://example.org/RefShape".to_string(),
        ),
    ];

    let report = validate_shex(&data, schema_json, &shape_map).unwrap();
    assert!(!report.conforms);
    assert_eq!(report.failures.len(), 1);
    assert_eq!(
        report.failures[0].node.to_string(),
        "<http://example.org/Charlie>"
    );
}

#[test]
fn test_blank_node_as_focus_node() {
    let schema_json = r#"{
      "@context": "http://www.w3.org/ns/shex.jsonld",
      "type": "Schema",
      "shapes": [
        {
          "type": "ShapeDecl",
          "id": "http://example.org/NameShape",
          "shapeExpr": {
            "type": "Shape",
            "expression": {
              "type": "TripleConstraint",
              "predicate": "http://example.org/name",
              "valueExpr": { "type": "NodeConstraint", "nodeKind": "literal" }
            }
          }
        }
      ]
    }"#;

    let data_str = r#"
        _:alice <http://example.org/name> "Alice" .
        _:bob <http://example.org/age> 30 .
    "#;

    let data = build_data_index(data_str);

    let report_alice = validate_shex(
        &data,
        schema_json,
        &[(
            "_:alice".to_string(),
            "http://example.org/NameShape".to_string(),
        )],
    )
    .unwrap();
    assert!(report_alice.conforms);
    assert_eq!(report_alice.failures.len(), 0);

    let report_bob = validate_shex(
        &data,
        schema_json,
        &[(
            "_:bob".to_string(),
            "http://example.org/NameShape".to_string(),
        )],
    )
    .unwrap();
    assert!(!report_bob.conforms);
    assert_eq!(report_bob.failures.len(), 1);
    assert_eq!(report_bob.failures[0].node.to_string(), "_:bob");
}

#[test]
fn test_blank_node_as_triple_constraint_value() {
    // Exercises the round trip of a blank node discovered as an *object*
    // value (via the adapter's oxrdf_term_to_roxi_term) being subsequently
    // used as the focus node for a nested (referenced) shape.
    let schema_json = r#"{
      "@context": "http://www.w3.org/ns/shex.jsonld",
      "type": "Schema",
      "shapes": [
        {
          "type": "ShapeDecl",
          "id": "http://example.org/PersonShape",
          "shapeExpr": {
            "type": "Shape",
            "expression": {
              "type": "TripleConstraint",
              "predicate": "http://example.org/friend",
              "valueExpr": "http://example.org/FriendShape"
            }
          }
        },
        {
          "type": "ShapeDecl",
          "id": "http://example.org/FriendShape",
          "shapeExpr": {
            "type": "Shape",
            "expression": {
              "type": "TripleConstraint",
              "predicate": "http://example.org/nick",
              "valueExpr": { "type": "NodeConstraint", "nodeKind": "literal" }
            }
          }
        }
      ]
    }"#;

    let data_str = r#"
        <http://example.org/Alice> <http://example.org/friend> _:b1 .
        _:b1 <http://example.org/nick> "Bobby" .

        <http://example.org/Carol> <http://example.org/friend> _:b2 .
    "#;

    let data = build_data_index(data_str);

    let report_alice = validate_shex(
        &data,
        schema_json,
        &[(
            "http://example.org/Alice".to_string(),
            "http://example.org/PersonShape".to_string(),
        )],
    )
    .unwrap();
    assert!(report_alice.conforms);

    let report_carol = validate_shex(
        &data,
        schema_json,
        &[(
            "http://example.org/Carol".to_string(),
            "http://example.org/PersonShape".to_string(),
        )],
    )
    .unwrap();
    assert!(!report_carol.conforms);
}

#[test]
fn test_language_tagged_literal_values() {
    // Substitutes for a "languageIn" facet (not exposed by the vendored
    // shex_ast AST): a value set entry of type "Language" constrains the
    // literal's language tag irrespective of lexical value.
    let schema_json = r#"{
      "@context": "http://www.w3.org/ns/shex.jsonld",
      "type": "Schema",
      "shapes": [
        {
          "type": "ShapeDecl",
          "id": "http://example.org/EnglishLabelShape",
          "shapeExpr": {
            "type": "Shape",
            "expression": {
              "type": "TripleConstraint",
              "predicate": "http://example.org/label",
              "valueExpr": {
                "type": "NodeConstraint",
                "values": [
                  { "type": "Language", "languageTag": "en" }
                ]
              }
            }
          }
        }
      ]
    }"#;

    let data_str = r#"
        <http://example.org/Alice> <http://example.org/label> "Hello"@en .
        <http://example.org/Bob> <http://example.org/label> "Bonjour"@fr .
    "#;

    let data = build_data_index(data_str);
    let shape_map = vec![
        (
            "http://example.org/Alice".to_string(),
            "http://example.org/EnglishLabelShape".to_string(),
        ),
        (
            "http://example.org/Bob".to_string(),
            "http://example.org/EnglishLabelShape".to_string(),
        ),
    ];

    let report = validate_shex(&data, schema_json, &shape_map).unwrap();
    assert!(!report.conforms);
    assert_eq!(report.failures.len(), 1);
    assert_eq!(
        report.failures[0].node.to_string(),
        "<http://example.org/Bob>"
    );
}

#[test]
fn test_decimal_typed_literal() {
    let schema_json = r#"{
      "@context": "http://www.w3.org/ns/shex.jsonld",
      "type": "Schema",
      "shapes": [
        {
          "type": "ShapeDecl",
          "id": "http://example.org/PriceShape",
          "shapeExpr": {
            "type": "Shape",
            "expression": {
              "type": "TripleConstraint",
              "predicate": "http://example.org/price",
              "valueExpr": {
                "type": "NodeConstraint",
                "datatype": "http://www.w3.org/2001/XMLSchema#decimal"
              }
            }
          }
        }
      ]
    }"#;

    let data_str = r#"
        <http://example.org/Alice> <http://example.org/price> "19.99"^^<http://www.w3.org/2001/XMLSchema#decimal> .
        <http://example.org/Bob> <http://example.org/price> 20 .
    "#;

    let data = build_data_index(data_str);
    let shape_map = vec![
        (
            "http://example.org/Alice".to_string(),
            "http://example.org/PriceShape".to_string(),
        ),
        (
            "http://example.org/Bob".to_string(),
            "http://example.org/PriceShape".to_string(),
        ),
    ];

    let report = validate_shex(&data, schema_json, &shape_map).unwrap();
    assert!(!report.conforms);
    assert_eq!(report.failures.len(), 1);
    assert_eq!(
        report.failures[0].node.to_string(),
        "<http://example.org/Bob>"
    );
}

#[test]
fn test_boolean_typed_literal() {
    let schema_json = r#"{
      "@context": "http://www.w3.org/ns/shex.jsonld",
      "type": "Schema",
      "shapes": [
        {
          "type": "ShapeDecl",
          "id": "http://example.org/ActiveShape",
          "shapeExpr": {
            "type": "Shape",
            "expression": {
              "type": "TripleConstraint",
              "predicate": "http://example.org/active",
              "valueExpr": {
                "type": "NodeConstraint",
                "datatype": "http://www.w3.org/2001/XMLSchema#boolean"
              }
            }
          }
        }
      ]
    }"#;

    let data_str = r#"
        <http://example.org/Alice> <http://example.org/active> "true"^^<http://www.w3.org/2001/XMLSchema#boolean> .
        <http://example.org/Bob> <http://example.org/active> "true" .
    "#;

    let data = build_data_index(data_str);
    let shape_map = vec![
        (
            "http://example.org/Alice".to_string(),
            "http://example.org/ActiveShape".to_string(),
        ),
        (
            "http://example.org/Bob".to_string(),
            "http://example.org/ActiveShape".to_string(),
        ),
    ];

    let report = validate_shex(&data, schema_json, &shape_map).unwrap();
    assert!(!report.conforms);
    assert_eq!(report.failures.len(), 1);
    assert_eq!(
        report.failures[0].node.to_string(),
        "<http://example.org/Bob>"
    );
}
