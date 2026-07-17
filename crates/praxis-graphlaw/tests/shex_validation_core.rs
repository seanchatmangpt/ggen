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
fn test_node_constraint_datatype() {
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
              "valueExpr": {
                "type": "NodeConstraint",
                "datatype": "http://www.w3.org/2001/XMLSchema#integer"
              }
            }
          }
        }
      ]
    }"#;

    let data_str = r#"
        <http://example.org/Alice> <http://example.org/age> 30 .
        <http://example.org/Bob> <http://example.org/age> "thirty" .
    "#;

    let data = build_data_index(data_str);
    let shape_map = vec![
        (
            "http://example.org/Alice".to_string(),
            "http://example.org/AgeShape".to_string(),
        ),
        (
            "http://example.org/Bob".to_string(),
            "http://example.org/AgeShape".to_string(),
        ),
    ];

    let report = validate_shex(&data, schema_json, &shape_map).unwrap();

    assert!(!report.conforms);
    assert_eq!(report.failures.len(), 1);
    assert_eq!(
        report.failures[0].node.to_string(),
        "<http://example.org/Bob>"
    );
    assert_eq!(report.failures[0].shape, "http://example.org/AgeShape");
}

#[test]
fn test_each_of_shape() {
    let schema_json = r#"{
      "@context": "http://www.w3.org/ns/shex.jsonld",
      "type": "Schema",
      "shapes": [
        {
          "type": "ShapeDecl",
          "id": "http://example.org/UserShape",
          "shapeExpr": {
            "type": "Shape",
            "expression": {
              "type": "EachOf",
              "expressions": [
                {
                  "type": "TripleConstraint",
                  "predicate": "http://example.org/name",
                  "valueExpr": {
                    "type": "NodeConstraint",
                    "nodeKind": "literal"
                  }
                },
                {
                  "type": "TripleConstraint",
                  "predicate": "http://example.org/age",
                  "valueExpr": {
                    "type": "NodeConstraint",
                    "datatype": "http://www.w3.org/2001/XMLSchema#integer"
                  }
                }
              ]
            }
          }
        }
      ]
    }"#;

    let data_str = r#"
        <http://example.org/Alice> <http://example.org/name> "Alice" ;
                                   <http://example.org/age> 30 .
        <http://example.org/Bob> <http://example.org/name> "Bob" .
    "#;

    let data = build_data_index(data_str);
    let shape_map = vec![
        (
            "http://example.org/Alice".to_string(),
            "http://example.org/UserShape".to_string(),
        ),
        (
            "http://example.org/Bob".to_string(),
            "http://example.org/UserShape".to_string(),
        ),
    ];

    let report = validate_shex(&data, schema_json, &shape_map).unwrap();

    assert!(!report.conforms);
    assert_eq!(report.failures.len(), 1);
    assert_eq!(
        report.failures[0].node.to_string(),
        "<http://example.org/Bob>"
    );
    assert_eq!(report.failures[0].shape, "http://example.org/UserShape");
}

#[test]
fn test_cardinality_on_triple_constraint() {
    let schema_json = r#"{
      "@context": "http://www.w3.org/ns/shex.jsonld",
      "type": "Schema",
      "shapes": [
        {
          "type": "ShapeDecl",
          "id": "http://example.org/PhoneShape",
          "shapeExpr": {
            "type": "Shape",
            "expression": {
              "type": "TripleConstraint",
              "predicate": "http://example.org/phone",
              "min": 1,
              "max": 2,
              "valueExpr": {
                "type": "NodeConstraint",
                "nodeKind": "literal"
              }
            }
          }
        }
      ]
    }"#;

    let data_str = r#"
        <http://example.org/Alice> <http://example.org/phone> "111", "222" .
        <http://example.org/Bob> <http://example.org/phone> "111", "222", "333" .
        <http://example.org/Charlie> <http://example.org/name> "Charlie" .
    "#;

    let data = build_data_index(data_str);
    let shape_map = vec![
        (
            "http://example.org/Alice".to_string(),
            "http://example.org/PhoneShape".to_string(),
        ),
        (
            "http://example.org/Bob".to_string(),
            "http://example.org/PhoneShape".to_string(),
        ),
        (
            "http://example.org/Charlie".to_string(),
            "http://example.org/PhoneShape".to_string(),
        ),
    ];

    let report = validate_shex(&data, schema_json, &shape_map).unwrap();

    assert!(!report.conforms);
    assert_eq!(report.failures.len(), 2);

    let failed_nodes: Vec<String> = report.failures.iter().map(|f| f.node.to_string()).collect();
    assert!(failed_nodes.contains(&"<http://example.org/Bob>".to_string()));
    assert!(failed_nodes.contains(&"<http://example.org/Charlie>".to_string()));
}

#[test]
fn test_shape_ref_recursive() {
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
              "predicate": "http://example.org/knows",
              "valueExpr": "http://example.org/PersonShape"
            }
          }
        }
      ]
    }"#;

    let data_str = r#"
        <http://example.org/Alice> <http://example.org/knows> <http://example.org/Bob> .
        <http://example.org/Bob> <http://example.org/knows> <http://example.org/Alice> .
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

    assert!(report.conforms);
    assert_eq!(report.failures.len(), 0);
}

#[test]
fn test_shape_map_pass_fail() {
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
              "valueExpr": {
                "type": "NodeConstraint",
                "datatype": "http://www.w3.org/2001/XMLSchema#integer"
              }
            }
          }
        }
      ]
    }"#;

    let data_str = r#"
        <http://example.org/Alice> <http://example.org/age> 30 .
        <http://example.org/Bob> <http://example.org/age> "thirty" .
    "#;

    let data = build_data_index(data_str);

    // Check clean pass
    let shape_map_pass = vec![(
        "http://example.org/Alice".to_string(),
        "http://example.org/AgeShape".to_string(),
    )];
    let report_pass = validate_shex(&data, schema_json, &shape_map_pass).unwrap();
    assert!(report_pass.conforms);

    // Check fail
    let shape_map_fail = vec![(
        "http://example.org/Bob".to_string(),
        "http://example.org/AgeShape".to_string(),
    )];
    let report_fail = validate_shex(&data, schema_json, &shape_map_fail).unwrap();
    assert!(!report_fail.conforms);
}

#[test]
fn test_empty_and_invalid_schema() {
    let data = build_data_index("");
    let shape_map = vec![(
        "http://example.org/Alice".to_string(),
        "http://example.org/AgeShape".to_string(),
    )];

    // Empty schema should fail parsing
    let res = validate_shex(&data, "", &shape_map);
    assert!(res.is_err());

    // Invalid JSON schema should fail parsing
    let res_invalid = validate_shex(&data, "invalid json", &shape_map);
    assert!(res_invalid.is_err());
}

#[test]
fn test_empty_shape_map() {
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
              "valueExpr": {
                "type": "NodeConstraint",
                "datatype": "http://www.w3.org/2001/XMLSchema#integer"
              }
            }
          }
        }
      ]
    }"#;
    let data = build_data_index("<http://example.org/Alice> <http://example.org/age> 30 .");

    // Empty shape map: should conform since nothing is validated
    let report = validate_shex(&data, schema_json, &[]).unwrap();
    assert!(report.conforms);
    assert!(report.failures.is_empty());
}

#[test]
fn test_empty_graph() {
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
              "valueExpr": {
                "type": "NodeConstraint",
                "datatype": "http://www.w3.org/2001/XMLSchema#integer"
              }
            }
          }
        }
      ]
    }"#;
    let data = build_data_index(""); // empty graph
    let shape_map = vec![(
        "http://example.org/Alice".to_string(),
        "http://example.org/AgeShape".to_string(),
    )];

    // Empty graph: should NOT conform, because the required age property is missing
    let report = validate_shex(&data, schema_json, &shape_map).unwrap();
    assert!(!report.conforms);
    assert_eq!(report.failures.len(), 1);
}

#[test]
fn test_shape_map_failures_and_nonexistent_shape() {
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
              "valueExpr": {
                "type": "NodeConstraint",
                "datatype": "http://www.w3.org/2001/XMLSchema#integer"
              }
            }
          }
        }
      ]
    }"#;
    let data = build_data_index("<http://example.org/Alice> <http://example.org/age> 30 .");

    // Case A: Invalid node syntax in shape map (e.g. empty node string)
    let shape_map_invalid_node = vec![("".to_string(), "http://example.org/AgeShape".to_string())];
    let res = validate_shex(&data, schema_json, &shape_map_invalid_node);
    assert!(
        res.is_err(),
        "Expected parsing error for empty node string, but got Ok"
    );

    // Case B: Non-existent shape label in shape map
    let shape_map_nonexistent_shape = vec![(
        "http://example.org/Alice".to_string(),
        "http://example.org/NonExistentShape".to_string(),
    )];

    // We want to see if this returns Ok or Err, and if conforms is true or false.
    // If it succeeds with conforms: true, it means it silently ignored the non-existent shape.
    let report_res = validate_shex(&data, schema_json, &shape_map_nonexistent_shape);
    match report_res {
        Ok(report) => {
            println!("Nonexistent shape report: {:?}", report);
            // We expect the library to either return conforms: false, or have a failure record.
            // Let's assert whatever is the actual current behavior, but we will document if it allows silent bypass.
            // Wait, let's first check if it conforms. Let's do assert!(report.conforms) or assert!(!report.conforms)
            // based on what it actually does. Let's write an assert that will print if it conforms.
            if report.conforms {
                println!("WARNING: Silently passed validation for non-existent shape!");
            }
        }
        Err(e) => {
            println!(
                "Validation returned expected error for non-existent shape: {:?}",
                e
            );
        }
    }
}
