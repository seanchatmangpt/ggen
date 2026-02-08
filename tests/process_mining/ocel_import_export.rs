//! OCEL Import/Export Tests
//!
//! Tests for reading and writing OCEL (Object-Centric Event Log) format.
//! Validates object-centric process mining capabilities.

use ggen_process_mining::event_log::AttributeValue;
use ggen_process_mining::ocel::{OcelEvent, OcelLog, OcelObject};
use ggen_process_mining::OcelParser;
use std::collections::HashMap;

#[cfg(test)]
mod ocel_tests {
    use super::super::helpers::*;
    use super::*;

    /// Test OCEL parser creation and configuration.
    #[test]
    fn test_ocel_parser_creation() {
        // Arrange & Act
        let parser = OcelParser::new();
        let parser_no_validation = OcelParser::new().with_reference_validation(false);

        // Assert
        assert!(parser.validate_references);
        assert!(!parser_no_validation.validate_references);
    }

    /// Test parsing simple OCEL JSON.
    #[test]
    fn test_parse_simple_ocel() {
        // Arrange
        let ocel_json = create_sample_ocel();
        let parser = OcelParser::new();

        // Act
        let result = parser.parse_str(&ocel_json);

        // Assert
        assert!(result.is_ok());

        let log = result.unwrap();
        assert_eq!(log.events.len(), 4);
        assert_eq!(log.objects.len(), 2);
        assert_eq!(log.object_types.len(), 2);
        assert_eq!(log.event_types.len(), 4);
    }

    /// Test OCEL preserves object type information.
    #[test]
    fn test_ocel_preserves_object_types() {
        // Arrange
        let ocel_json = create_sample_ocel();
        let parser = OcelParser::new();

        // Act
        let log = parser.parse_str(&ocel_json).unwrap();

        // Assert
        let order_types: Vec<_> = log
            .object_types
            .iter()
            .filter(|ot| ot.name == "order")
            .collect();

        assert_eq!(order_types.len(), 1);
        assert_eq!(order_types[0].attributes.len(), 2);
        assert!(order_types[0].attributes.iter().any(|a| a.name == "price"));
        assert!(order_types[0].attributes.iter().any(|a| a.name == "status"));
    }

    /// Test OCEL event-object relationships.
    #[test]
    fn test_ocel_event_object_relationships() {
        // Arrange
        let ocel_json = create_sample_ocel();
        let parser = OcelParser::new();

        // Act
        let log = parser.parse_str(&ocel_json).unwrap();

        // Assert - Event e2 references both order1 and item1
        let e2 = log.events.get("e2").expect("Event e2 should exist");
        assert!(e2.objects.contains_key("order1"));
        assert!(e2.objects.contains_key("item1"));
        assert_eq!(e2.objects.get("order1"), Some(&"order".to_string()));
        assert_eq!(e2.objects.get("item1"), Some(&"item".to_string()));
    }

    /// Test OCEL object attribute parsing.
    #[test]
    fn test_ocel_object_attributes() {
        // Arrange
        let ocel_json = create_sample_ocel();
        let parser = OcelParser::new();

        // Act
        let log = parser.parse_str(&ocel_json).unwrap();

        // Assert
        let order1 = log.objects.get("order1").expect("order1 should exist");
        assert_eq!(order1.type_name, "order");

        assert!(order1.attributes.contains_key("price"));
        assert!(order1.attributes.contains_key("status"));

        if let Some(AttributeValue::Float(price)) = order1.attributes.get("price") {
            assert!((price - 99.99).abs() < 0.01);
        } else {
            panic!("Price should be a float attribute");
        }

        if let Some(AttributeValue::String(status)) = order1.attributes.get("status") {
            assert_eq!(status, "shipped");
        } else {
            panic!("Status should be a string attribute");
        }
    }

    /// Test OCEL unique activities extraction.
    #[test]
    fn test_ocel_unique_activities() {
        // Arrange
        let ocel_json = create_sample_ocel();
        let parser = OcelParser::new();

        // Act
        let log = parser.parse_str(&ocel_json).unwrap();
        let activities = log.unique_activities();

        // Assert
        assert_eq!(activities.len(), 4);
        assert!(activities.contains(&"Create Order".to_string()));
        assert!(activities.contains(&"Add Item".to_string()));
        assert!(activities.contains(&"Pay Order".to_string()));
        assert!(activities.contains(&"Ship Order".to_string()));
    }

    /// Test OCEL objects by type query.
    #[test]
    fn test_ocel_objects_by_type() {
        // Arrange
        let ocel_json = create_sample_ocel();
        let parser = OcelParser::new();

        // Act
        let log = parser.parse_str(&ocel_json).unwrap();

        let orders = log.objects_by_type("order");
        let items = log.objects_by_type("item");
        let non_existent = log.objects_by_type("non_existent");

        // Assert
        assert_eq!(orders.len(), 1);
        assert_eq!(orders[0].id, "order1");

        assert_eq!(items.len(), 1);
        assert_eq!(items[0].id, "item1");

        assert_eq!(non_existent.len(), 0);
    }

    /// Test OCEL events for object query.
    #[test]
    fn test_ocel_events_for_object() {
        // Arrange
        let ocel_json = create_sample_ocel();
        let parser = OcelParser::new();

        // Act
        let log = parser.parse_str(&ocel_json).unwrap();

        let order1_events = log.events_for_object("order1");
        let item1_events = log.events_for_object("item1");
        let non_existent_events = log.events_for_object("non_existent");

        // Assert - order1 appears in 4 events
        assert_eq!(order1_events.len(), 4);

        // Assert - item1 appears in only 1 event (Add Item)
        assert_eq!(item1_events.len(), 1);
        assert_eq!(item1_events[0].activity, "Add Item");

        // Assert - non-existent object has no events
        assert_eq!(non_existent_events.len(), 0);
    }

    /// Test OCEL objects for event query.
    #[test]
    fn test_ocel_objects_for_event() {
        // Arrange
        let ocel_json = create_sample_ocel();
        let parser = OcelParser::new();

        // Act
        let log = parser.parse_str(&ocel_json).unwrap();

        let e1_objects = log.objects_for_event("e1");
        let e2_objects = log.objects_for_event("e2");

        // Assert - e1 (Create Order) references order1
        assert_eq!(e1_objects.len(), 1);
        assert_eq!(e1_objects[0].id, "order1");

        // Assert - e2 (Add Item) references both order1 and item1
        assert_eq!(e2_objects.len(), 2);
        assert!(e2_objects.iter().any(|o| o.id == "order1"));
        assert!(e2_objects.iter().any(|o| o.id == "item1"));
    }

    /// Test OCEL reference validation.
    #[test]
    fn test_ocel_reference_validation() {
        // Arrange - Create OCEL with broken reference
        let broken_ocel = r#"{
  "ocel-version": "2.0",
  "objectTypes": [{"name": "order", "attributes": []}],
  "activities": ["Create Order"],
  "events": [
    {
      "id": "e1",
      "activity": "Create Order",
      "timestamp": "2024-01-01T10:00:00Z",
      "objects": [{"id": "nonexistent", "qualifier": null}]
    }
  ],
  "objects": [
    {"id": "order1", "type": "order", "attributes": {}}
  ]
}"#;

        // Act - With validation enabled
        let parser_with_validation = OcelParser::new().with_reference_validation(true);
        let result_validated = parser_with_validation.parse_str(broken_ocel);

        // Assert - Should fail due to broken reference
        assert!(result_validated.is_err());

        // Act - With validation disabled
        let parser_no_validation = OcelParser::new().with_reference_validation(false);
        let result_no_validation = parser_no_validation.parse_str(broken_ocel);

        // Assert - Should succeed when validation disabled
        assert!(result_no_validation.is_ok());
    }

    /// Test OCEL with multiple object types per event.
    #[test]
    fn test_ocel_multiple_object_types() {
        // Arrange
        let multi_type_ocel = r#"{
  "ocel-version": "2.0",
  "objectTypes": [
    {"name": "customer", "attributes": []},
    {"name": "order", "attributes": []},
    {"name": "product", "attributes": []}
  ],
  "activities": ["Place Order"],
  "events": [
    {
      "id": "e1",
      "activity": "Place Order",
      "timestamp": "2024-01-01T10:00:00Z",
      "objects": [
        {"id": "cust1", "qualifier": "customer"},
        {"id": "order1", "qualifier": "order"},
        {"id": "prod1", "qualifier": "product"}
      ]
    }
  ],
  "objects": [
    {"id": "cust1", "type": "customer", "attributes": {}},
    {"id": "order1", "type": "order", "attributes": {}},
    {"id": "prod1", "type": "product", "attributes": {}}
  ]
}"#;

        let parser = OcelParser::new();

        // Act
        let log = parser.parse_str(multi_type_ocel).unwrap();

        // Assert
        assert_eq!(log.object_types.len(), 3);

        let e1 = log.events.get("e1").unwrap();
        assert_eq!(e1.objects.len(), 3);
        assert!(e1.objects.contains_key("cust1"));
        assert!(e1.objects.contains_key("order1"));
        assert!(e1.objects.contains_key("prod1"));
    }

    /// Test OCEL with qualifiers.
    #[test]
    fn test_ocel_qualifiers() {
        // Arrange
        let ocel_with_qualifiers = r#"{
  "ocel-version": "2.0",
  "objectTypes": [
    {"name": "order", "attributes": []},
    {"name": "item", "attributes": []}
  ],
  "activities": ["Add Item"],
  "events": [
    {
      "id": "e1",
      "activity": "Add Item",
      "timestamp": "2024-01-01T10:00:00Z",
      "objects": [
        {"id": "order1", "qualifier": "order"},
        {"id": "item1", "qualifier": "item"}
      ]
    }
  ],
  "objects": [
    {"id": "order1", "type": "order", "attributes": {}},
    {"id": "item1", "type": "item", "attributes": {}}
  ]
}"#;

        let parser = OcelParser::new();

        // Act
        let log = parser.parse_str(ocel_with_qualifiers).unwrap();

        // Assert
        let e1 = log.events.get("e1").unwrap();
        assert_eq!(e1.objects.get("order1"), Some(&"order".to_string()));
        assert_eq!(e1.objects.get("item1"), Some(&"item".to_string()));
    }

    /// Test OCEL with null qualifier.
    #[test]
    fn test_ocel_null_qualifier() {
        // Arrange
        let ocel_null_qualifier = r#"{
  "ocel-version": "2.0",
  "objectTypes": [{"name": "order", "attributes": []}],
  "activities": ["Pay Order"],
  "events": [
    {
      "id": "e1",
      "activity": "Pay Order",
      "timestamp": "2024-01-01T10:00:00Z",
      "objects": [{"id": "order1", "qualifier": null}]
    }
  ],
  "objects": [{"id": "order1", "type": "order", "attributes": {}}]
}"#;

        let parser = OcelParser::new();

        // Act
        let log = parser.parse_str(ocel_null_qualifier).unwrap();

        // Assert - Null qualifier should be handled
        let e1 = log.events.get("e1").unwrap();
        assert!(e1.objects.contains_key("order1"));
    }

    /// Test OCEL invalid JSON handling.
    #[test]
    fn test_ocel_invalid_json() {
        // Arrange
        let invalid_json = "This is not valid JSON";
        let parser = OcelParser::new();

        // Act
        let result = parser.parse_str(invalid_json);

        // Assert
        assert!(result.is_err());
    }

    /// Test OCEL missing required fields.
    #[test]
    fn test_ocel_missing_required_fields() {
        // Arrange
        let incomplete_ocel = r#"{
  "ocel-version": "2.0",
  "objectTypes": [],
  "activities": []
}"#;

        let parser = OcelParser::new();

        // Act
        let result = parser.parse_str(incomplete_ocel);

        // Assert - Should parse but may have empty collections
        assert!(result.is_ok());
        let log = result.unwrap();
        assert_eq!(log.events.len(), 0);
        assert_eq!(log.objects.len(), 0);
    }

    /// Test OCEL empty log.
    #[test]
    fn test_ocel_empty_log() {
        // Arrange
        let empty_ocel = r#"{
  "ocel-version": "2.0",
  "objectTypes": [],
  "activities": [],
  "events": [],
  "objects": []
}"#;

        let parser = OcelParser::new();

        // Act
        let log = parser.parse_str(empty_ocel).unwrap();

        // Assert
        assert_eq!(log.events.len(), 0);
        assert_eq!(log.objects.len(), 0);
        assert_eq!(log.object_types.len(), 0);
        assert_eq!(log.event_types.len(), 0);
    }

    /// Test OCEL timestamp parsing.
    #[test]
    fn test_ocel_timestamp_parsing() {
        // Arrange
        let ocel_json = create_sample_ocel();
        let parser = OcelParser::new();

        // Act
        let log = parser.parse_str(&ocel_json).unwrap();

        // Assert - All events should have valid timestamps
        for event in log.events.values() {
            use chrono::Datelike;
            assert!(event.timestamp.year() >= 2024);
        }
    }

    /// Test OCEL with various attribute types.
    #[test]
    fn test_ocel_various_attribute_types() {
        // Arrange
        let ocel_attrs = r#"{
  "ocel-version": "2.0",
  "objectTypes": [
    {
      "name": "item",
      "attributes": [
        {"name": "price", "type": "float"},
        {"name": "quantity", "type": "int"},
        {"name": "available", "type": "boolean"}
      ]
    }
  ],
  "activities": ["Create Item"],
  "events": [
    {
      "id": "e1",
      "activity": "Create Item",
      "timestamp": "2024-01-01T10:00:00Z",
      "objects": [{"id": "item1", "qualifier": null}]
    }
  ],
  "objects": [
    {
      "id": "item1",
      "type": "item",
      "attributes": {
        "price": 19.99,
        "quantity": 5,
        "available": true
      }
    }
  ]
}"#;

        let parser = OcelParser::new();

        // Act
        let log = parser.parse_str(ocel_attrs).unwrap();
        let item1 = log.objects.get("item1").unwrap();

        // Assert
        assert!(item1.attributes.contains_key("price"));
        assert!(item1.attributes.contains_key("quantity"));
        assert!(item1.attributes.contains_key("available"));

        if let Some(AttributeValue::Float(price)) = item1.attributes.get("price") {
            assert!((price - 19.99).abs() < 0.01);
        } else {
            panic!("Price should be float");
        }

        if let Some(AttributeValue::Integer(qty)) = item1.attributes.get("quantity") {
            assert_eq!(*qty, 5);
        } else {
            panic!("Quantity should be integer");
        }

        if let Some(AttributeValue::Boolean(avail)) = item1.attributes.get("available") {
            assert!(avail);
        } else {
            panic!("Available should be boolean");
        }
    }

    /// Test OCEL object type definitions.
    #[test]
    fn test_ocel_object_type_definitions() {
        // Arrange
        let ocel_json = create_sample_ocel();
        let parser = OcelParser::new();

        // Act
        let log = parser.parse_str(&ocel_json).unwrap();

        // Assert
        let order_type = log
            .object_types
            .iter()
            .find(|ot| ot.name == "order")
            .expect("Order type should exist");

        assert_eq!(order_type.attributes.len(), 2);
        assert_eq!(order_type.attributes[0].name, "price");
        assert_eq!(order_type.attributes[0].type_name, "float");
        assert_eq!(order_type.attributes[1].name, "status");
        assert_eq!(order_type.attributes[1].type_name, "string");
    }

    /// Test OCEL event type definitions.
    #[test]
    fn test_ocel_event_type_definitions() {
        // Arrange
        let ocel_json = create_sample_ocel();
        let parser = OcelParser::new();

        // Act
        let log = parser.parse_str(&ocel_json).unwrap();

        // Assert
        assert_eq!(log.event_types.len(), 4);
        assert!(log.event_types.contains(&"Create Order".to_string()));
        assert!(log.event_types.contains(&"Add Item".to_string()));
        assert!(log.event_types.contains(&"Pay Order".to_string()));
        assert!(log.event_types.contains(&"Ship Order".to_string()));
    }
}
