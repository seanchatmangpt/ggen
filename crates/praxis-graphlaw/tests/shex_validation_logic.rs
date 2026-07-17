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
fn test_one_of_disjunction() {
    let schema_json = r#"{
      "@context": "http://www.w3.org/ns/shex.jsonld",
      "type": "Schema",
      "shapes": [
        {
          "type": "ShapeDecl",
          "id": "http://example.org/ContactShape",
          "shapeExpr": {
            "type": "Shape",
            "expression": {
              "type": "OneOf",
              "expressions": [
                {
                  "type": "TripleConstraint",
                  "predicate": "http://example.org/email",
                  "valueExpr": { "type": "NodeConstraint", "nodeKind": "literal" }
                },
                {
                  "type": "TripleConstraint",
                  "predicate": "http://example.org/phone",
                  "valueExpr": { "type": "NodeConstraint", "nodeKind": "literal" }
                }
              ]
            }
          }
        }
      ]
    }"#;

    let data_str = r#"
        <http://example.org/Alice> <http://example.org/email> "alice@example.org" .
        <http://example.org/Bob> <http://example.org/phone> "555-1234" .
        <http://example.org/Charlie> <http://example.org/name> "Charlie" .
    "#;

    let data = build_data_index(data_str);

    // Pass: has email (first branch).
    let report_alice = validate_shex(
        &data,
        schema_json,
        &[(
            "http://example.org/Alice".to_string(),
            "http://example.org/ContactShape".to_string(),
        )],
    )
    .unwrap();
    assert!(report_alice.conforms);

    // Pass: has phone (second branch).
    let report_bob = validate_shex(
        &data,
        schema_json,
        &[(
            "http://example.org/Bob".to_string(),
            "http://example.org/ContactShape".to_string(),
        )],
    )
    .unwrap();
    assert!(report_bob.conforms);

    // Fail: has neither.
    let report_charlie = validate_shex(
        &data,
        schema_json,
        &[(
            "http://example.org/Charlie".to_string(),
            "http://example.org/ContactShape".to_string(),
        )],
    )
    .unwrap();
    assert!(!report_charlie.conforms);
}

#[test]
fn test_shape_and() {
    let schema_json = r#"{
      "@context": "http://www.w3.org/ns/shex.jsonld",
      "type": "Schema",
      "shapes": [
        {
          "type": "ShapeDecl",
          "id": "http://example.org/PersonShape",
          "shapeExpr": {
            "type": "ShapeAnd",
            "shapeExprs": [
              {
                "type": "Shape",
                "expression": {
                  "type": "TripleConstraint",
                  "predicate": "http://example.org/age",
                  "valueExpr": { "type": "NodeConstraint", "datatype": "http://www.w3.org/2001/XMLSchema#integer" }
                }
              },
              {
                "type": "Shape",
                "expression": {
                  "type": "TripleConstraint",
                  "predicate": "http://example.org/name",
                  "valueExpr": { "type": "NodeConstraint", "nodeKind": "literal" }
                }
              }
            ]
          }
        }
      ]
    }"#;

    let data_str = r#"
        <http://example.org/Alice> <http://example.org/age> 30 ;
                                   <http://example.org/name> "Alice" .
        <http://example.org/Bob> <http://example.org/age> 40 .
    "#;

    let data = build_data_index(data_str);
    let shape_map = vec![
        (
            "http://example.org/Alice".to_string(),
            "http://example.org/PersonShape".to_string(),
        ),
        (
            "http://example.org/Bob".to_string(),
            "http://example.org/PersonShape".to_string(),
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
fn test_shape_or() {
    let schema_json = r#"{
      "@context": "http://www.w3.org/ns/shex.jsonld",
      "type": "Schema",
      "shapes": [
        {
          "type": "ShapeDecl",
          "id": "http://example.org/WorkerShape",
          "shapeExpr": {
            "type": "ShapeOr",
            "shapeExprs": [
              {
                "type": "Shape",
                "expression": {
                  "type": "TripleConstraint",
                  "predicate": "http://example.org/employeeId",
                  "valueExpr": { "type": "NodeConstraint", "nodeKind": "literal" }
                }
              },
              {
                "type": "Shape",
                "expression": {
                  "type": "TripleConstraint",
                  "predicate": "http://example.org/contractId",
                  "valueExpr": { "type": "NodeConstraint", "nodeKind": "literal" }
                }
              }
            ]
          }
        }
      ]
    }"#;

    let data_str = r#"
        <http://example.org/Alice> <http://example.org/employeeId> "E1" .
        <http://example.org/Bob> <http://example.org/contractId> "C1" .
        <http://example.org/Charlie> <http://example.org/name> "Charlie" .
    "#;

    let data = build_data_index(data_str);
    let shape_map = vec![
        (
            "http://example.org/Alice".to_string(),
            "http://example.org/WorkerShape".to_string(),
        ),
        (
            "http://example.org/Bob".to_string(),
            "http://example.org/WorkerShape".to_string(),
        ),
        (
            "http://example.org/Charlie".to_string(),
            "http://example.org/WorkerShape".to_string(),
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
fn test_shape_not() {
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
                "type": "ShapeNot",
                "shapeExpr": {
                  "type": "NodeConstraint",
                  "datatype": "http://www.w3.org/2001/XMLSchema#integer"
                }
              }
            }
          }
        }
      ]
    }"#;

    let data_str = r#"
        <http://example.org/Alice> <http://example.org/code> "ABC123" .
        <http://example.org/Bob> <http://example.org/code> 42 .
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
fn test_closed_shape_with_extra() {
    let schema_json = r#"{
      "@context": "http://www.w3.org/ns/shex.jsonld",
      "type": "Schema",
      "shapes": [
        {
          "type": "ShapeDecl",
          "id": "http://example.org/ClosedShape",
          "shapeExpr": {
            "type": "Shape",
            "closed": true,
            "extra": ["http://example.org/knownExtra"],
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
        <http://example.org/Alice> <http://example.org/name> "Alice" ;
                                   <http://example.org/knownExtra> "allowed" .
        <http://example.org/Bob> <http://example.org/name> "Bob" ;
                                 <http://example.org/disallowedExtra> "not allowed" .
    "#;

    let data = build_data_index(data_str);
    let shape_map = vec![
        (
            "http://example.org/Alice".to_string(),
            "http://example.org/ClosedShape".to_string(),
        ),
        (
            "http://example.org/Bob".to_string(),
            "http://example.org/ClosedShape".to_string(),
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
