//! Integration tests for RDF List Validator
//!
//! Tests the validation of RDF list chains for well-formedness and integrity.

use ggen_ai::{RdfListValidator, ValidationError};
use oxigraph::model::{GraphName, Literal, NamedNode};
use oxigraph::store::Store;

// Helper function to add a triple to the store
fn add_triple(store: &Store, subject: &str, predicate: &str, object: &str) {
    let s = NamedNode::new(subject).unwrap();
    let p = NamedNode::new(predicate).unwrap();
    let o = NamedNode::new(object).unwrap();
    let quad = oxigraph::model::Quad::new(s, p, o, GraphName::DefaultGraph);
    store.insert(&quad).expect("Failed to insert triple");
}

// Helper function to add a triple with literal object
fn add_triple_with_literal(store: &Store, subject: &str, predicate: &str, object: &str) {
    let s = NamedNode::new(subject).unwrap();
    let p = NamedNode::new(predicate).unwrap();
    let o = Literal::new_simple_literal(object);
    let quad = oxigraph::model::Quad::new(s, p, o, GraphName::DefaultGraph);
    store.insert(&quad).expect("Failed to insert triple");
}

const RDF_FIRST: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#first";
const RDF_REST: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest";
const RDF_NIL: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil";

#[test]
fn test_integration_validator_empty_list() {
    let store = Store::new().expect("Failed to create store");
    let validator = RdfListValidator::new();

    let result = validator
        .validate_list(&store, RDF_NIL)
        .expect("Validation failed");
    assert!(result.is_empty(), "Empty list should return empty vector");
}

#[test]
fn test_integration_validator_single_element() {
    let store = Store::new().expect("Failed to create store");
    let validator = RdfListValidator::new();

    add_triple_with_literal(&store, "http://example.com/list1", RDF_FIRST, "value1");
    add_triple(&store, "http://example.com/list1", RDF_REST, RDF_NIL);

    let result = validator
        .validate_list(&store, "http://example.com/list1")
        .expect("Validation failed");
    assert_eq!(result.len(), 1);
    assert_eq!(result[0], "value1");
}

#[test]
fn test_integration_validator_three_elements() {
    let store = Store::new().expect("Failed to create store");
    let validator = RdfListValidator::new();

    // Create list: (a b c)
    add_triple_with_literal(&store, "http://example.com/list1", RDF_FIRST, "a");
    add_triple(
        &store,
        "http://example.com/list1",
        RDF_REST,
        "http://example.com/list2",
    );

    add_triple_with_literal(&store, "http://example.com/list2", RDF_FIRST, "b");
    add_triple(
        &store,
        "http://example.com/list2",
        RDF_REST,
        "http://example.com/list3",
    );

    add_triple_with_literal(&store, "http://example.com/list3", RDF_FIRST, "c");
    add_triple(&store, "http://example.com/list3", RDF_REST, RDF_NIL);

    let result = validator
        .validate_list(&store, "http://example.com/list1")
        .expect("Validation failed");
    assert_eq!(result.len(), 3);
    assert_eq!(result[0], "a");
    assert_eq!(result[1], "b");
    assert_eq!(result[2], "c");
}

#[test]
fn test_integration_circular_reference_detection() {
    let store = Store::new().expect("Failed to create store");
    let validator = RdfListValidator::new();

    // Create cycle: list1 -> list2 -> list1
    add_triple_with_literal(&store, "http://example.com/list1", RDF_FIRST, "x");
    add_triple(
        &store,
        "http://example.com/list1",
        RDF_REST,
        "http://example.com/list2",
    );

    add_triple_with_literal(&store, "http://example.com/list2", RDF_FIRST, "y");
    add_triple(
        &store,
        "http://example.com/list2",
        RDF_REST,
        "http://example.com/list1",
    );

    let result = validator.validate_list(&store, "http://example.com/list1");
    assert!(result.is_err(), "Circular reference should be detected");

    match result {
        Err(ValidationError::CircularReference { start, cycle_node }) => {
            assert_eq!(start, "http://example.com/list1");
            assert_eq!(cycle_node, "http://example.com/list1");
        }
        _ => panic!("Expected CircularReference error"),
    }
}

#[test]
fn test_integration_missing_nil_terminator() {
    let store = Store::new().expect("Failed to create store");
    let validator = RdfListValidator::new();

    // Create malformed list without nil terminator
    add_triple_with_literal(&store, "http://example.com/list1", RDF_FIRST, "a");
    add_triple(
        &store,
        "http://example.com/list1",
        RDF_REST,
        "http://example.com/list2",
    );

    add_triple_with_literal(&store, "http://example.com/list2", RDF_FIRST, "b");
    // No rdf:rest property

    let result = validator.validate_list(&store, "http://example.com/list1");
    assert!(result.is_err(), "Missing nil terminator should be detected");

    match result {
        Err(ValidationError::MissingNilTerminator { tail }) => {
            assert_eq!(tail, "http://example.com/list2");
        }
        _ => panic!("Expected MissingNilTerminator error"),
    }
}

#[test]
fn test_integration_max_depth_exceeded() {
    let store = Store::new().expect("Failed to create store");
    let validator = RdfListValidator::with_max_depth(3);

    // Create a chain longer than max depth
    for i in 0..10 {
        let subject = format!("http://example.com/list{}", i);
        let value = format!("item{}", i);
        add_triple_with_literal(&store, &subject, RDF_FIRST, &value);

        if i < 9 {
            let rest = format!("http://example.com/list{}", i + 1);
            add_triple(&store, &subject, RDF_REST, &rest);
        } else {
            add_triple(&store, &subject, RDF_REST, RDF_NIL);
        }
    }

    let result = validator.validate_list(&store, "http://example.com/list0");
    assert!(result.is_err(), "Max depth exceeded should be detected");

    match result {
        Err(ValidationError::MaxDepthExceeded { max, actual }) => {
            assert_eq!(max, 3);
            assert_eq!(actual, 3);
        }
        _ => panic!("Expected MaxDepthExceeded error"),
    }
}

#[test]
fn test_integration_validator_with_uri_values() {
    let store = Store::new().expect("Failed to create store");
    let validator = RdfListValidator::new();

    // Create list of URI values
    add_triple(
        &store,
        "http://example.com/list1",
        RDF_FIRST,
        "http://example.com/resource1",
    );
    add_triple(
        &store,
        "http://example.com/list1",
        RDF_REST,
        "http://example.com/list2",
    );

    add_triple(
        &store,
        "http://example.com/list2",
        RDF_FIRST,
        "http://example.com/resource2",
    );
    add_triple(&store, "http://example.com/list2", RDF_REST, RDF_NIL);

    let result = validator
        .validate_list(&store, "http://example.com/list1")
        .expect("Validation failed");
    assert_eq!(result.len(), 2);
    assert_eq!(result[0], "http://example.com/resource1");
    assert_eq!(result[1], "http://example.com/resource2");
}

#[test]
fn test_integration_validator_self_loop() {
    let store = Store::new().expect("Failed to create store");
    let validator = RdfListValidator::new();

    // Create self-referencing node
    add_triple_with_literal(&store, "http://example.com/list1", RDF_FIRST, "x");
    add_triple(
        &store,
        "http://example.com/list1",
        RDF_REST,
        "http://example.com/list1",
    );

    let result = validator.validate_list(&store, "http://example.com/list1");
    assert!(result.is_err(), "Self-loop should be detected");

    match result {
        Err(ValidationError::CircularReference { .. }) => {
            // Expected
        }
        _ => panic!("Expected CircularReference error"),
    }
}

#[test]
fn test_integration_validator_unicode_values() {
    let store = Store::new().expect("Failed to create store");
    let validator = RdfListValidator::new();

    // Create list with unicode values
    add_triple_with_literal(&store, "http://example.com/list1", RDF_FIRST, "Hello");
    add_triple(
        &store,
        "http://example.com/list1",
        RDF_REST,
        "http://example.com/list2",
    );

    add_triple_with_literal(&store, "http://example.com/list2", RDF_FIRST, "世界");
    add_triple(
        &store,
        "http://example.com/list2",
        RDF_REST,
        "http://example.com/list3",
    );

    add_triple_with_literal(&store, "http://example.com/list3", RDF_FIRST, "🌍");
    add_triple(&store, "http://example.com/list3", RDF_REST, RDF_NIL);

    let result = validator
        .validate_list(&store, "http://example.com/list1")
        .expect("Validation failed");
    assert_eq!(result.len(), 3);
    assert_eq!(result[0], "Hello");
    assert_eq!(result[1], "世界");
    assert_eq!(result[2], "🌍");
}

#[test]
fn test_integration_validator_missing_first_property() {
    let store = Store::new().expect("Failed to create store");
    let validator = RdfListValidator::new();

    // Create malformed node without rdf:first
    add_triple(&store, "http://example.com/list1", RDF_REST, RDF_NIL);

    let result = validator.validate_list(&store, "http://example.com/list1");
    assert!(result.is_err(), "Missing rdf:first should be detected");

    match result {
        Err(ValidationError::MissingFirstProperty { node }) => {
            assert_eq!(node, "http://example.com/list1");
        }
        _ => panic!("Expected MissingFirstProperty error"),
    }
}

#[test]
fn test_integration_validator_duplicate_values() {
    let store = Store::new().expect("Failed to create store");
    let validator = RdfListValidator::new();

    // Create list with duplicate values
    add_triple_with_literal(&store, "http://example.com/list1", RDF_FIRST, "same");
    add_triple(
        &store,
        "http://example.com/list1",
        RDF_REST,
        "http://example.com/list2",
    );

    add_triple_with_literal(&store, "http://example.com/list2", RDF_FIRST, "same");
    add_triple(&store, "http://example.com/list2", RDF_REST, RDF_NIL);

    let result = validator
        .validate_list(&store, "http://example.com/list1")
        .expect("Validation failed");
    assert_eq!(result.len(), 2);
    assert!(result.iter().all(|v| v == "same"));
}

#[test]
fn test_integration_validator_long_list() {
    let store = Store::new().expect("Failed to create store");
    let validator = RdfListValidator::new();

    // Create a 50-element list
    let num_elements = 50;
    for i in 0..num_elements {
        let subject = format!("http://example.com/item{}", i);
        let value = format!("{}", i);
        add_triple_with_literal(&store, &subject, RDF_FIRST, &value);

        if i < num_elements - 1 {
            let rest = format!("http://example.com/item{}", i + 1);
            add_triple(&store, &subject, RDF_REST, &rest);
        } else {
            add_triple(&store, &subject, RDF_REST, RDF_NIL);
        }
    }

    let result = validator
        .validate_list(&store, "http://example.com/item0")
        .expect("Validation failed");
    assert_eq!(result.len(), num_elements);
    for i in 0..num_elements {
        assert_eq!(result[i], format!("{}", i));
    }
}

#[test]
fn test_integration_validator_with_custom_depth_valid() {
    let store = Store::new().expect("Failed to create store");
    let validator = RdfListValidator::with_max_depth(20);

    // Create a 10-element list (within custom max depth)
    for i in 0..10 {
        let subject = format!("http://example.com/node{}", i);
        add_triple_with_literal(&store, &subject, RDF_FIRST, &format!("v{}", i));

        if i < 9 {
            let rest = format!("http://example.com/node{}", i + 1);
            add_triple(&store, &subject, RDF_REST, &rest);
        } else {
            add_triple(&store, &subject, RDF_REST, RDF_NIL);
        }
    }

    let result = validator
        .validate_list(&store, "http://example.com/node0")
        .expect("Validation failed");
    assert_eq!(result.len(), 10);
}
