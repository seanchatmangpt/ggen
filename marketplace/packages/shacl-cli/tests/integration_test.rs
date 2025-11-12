//! Integration tests for SHACL CLI

use shacl_cli::{Shape, ShapeType, Target, Validator};

#[test]
fn test_shape_creation() {
    let shape = Shape::new("TestShape".to_string(), ShapeType::Node);
    assert_eq!(shape.name, "TestShape");
}

#[test]
fn test_shape_with_target() {
    let shape = Shape::new("PersonShape".to_string(), ShapeType::Node)
        .with_target(Target::Class("foaf:Person".to_string()));

    assert!(shape.target.is_some());
}

#[test]
fn test_validator_creation() {
    let shapes = vec![
        Shape::new("Shape1".to_string(), ShapeType::Node),
        Shape::new("Shape2".to_string(), ShapeType::Property),
    ];

    let validator = Validator::new(shapes);
    let result = validator.validate().unwrap();
    assert!(result.conforms);
}
