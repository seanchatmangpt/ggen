//! OCEL (Object-Centric Event Log) format support.
//!
//! This module provides parsing functionality for the OCEL 2.0 format,
//! which extends XES to support multiple object types per event.

use crate::error::{Error, Result};
use crate::event_log::AttributeValue;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// OCEL event log parser.
///
/// OCEL (Object-Centric Event Log) is a format for event logs that supports
/// multiple object types per event, enabling more complex process mining scenarios.
#[derive(Debug, Clone)]
pub struct OcelParser {
    /// Whether to validate object references.
    pub validate_references: bool,
}

impl Default for OcelParser {
    fn default() -> Self {
        Self::new()
    }
}

impl OcelParser {
    /// Create a new OCEL parser.
    #[must_use]
    pub fn new() -> Self {
        Self {
            validate_references: true,
        }
    }

    /// Set whether to validate object references.
    #[must_use]
    pub fn with_reference_validation(mut self, validate: bool) -> Self {
        self.validate_references = validate;
        self
    }

    /// Parse an OCEL JSON file.
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The file cannot be read
    /// - The JSON format is invalid
    /// - The OCEL structure is invalid
    ///
    /// # Examples
    ///
    /// ```rust,ignore
    /// use ggen_process_mining::OcelParser;
    ///
    /// let parser = OcelParser::new();
    /// let log = parser.parse_file("log.jsonocel")?;
    /// ```
    pub fn parse_file(&self, path: &str) -> Result<OcelLog> {
        let content = std::fs::read_to_string(path).map_err(|e| Error::EventLogRead {
            path: path.into(),
            source: e,
        })?;

        self.parse_str(&content)
    }

    /// Parse an OCEL JSON string.
    ///
    /// # Examples
    ///
    /// ```rust,ignore
    /// use ggen_process_mining::OcelParser;
    ///
    /// let parser = OcelParser::new();
    /// let log = parser.parse_str(jsonocel_string)?;
    /// ```
    pub fn parse_str(&self, json: &str) -> Result<OcelLog> {
        let raw: RawOcelLog = serde_json::from_str(json)
            .map_err(|e| Error::OcelParse(format!("JSON parse error: {e}")))?;

        self.convert_raw_log(raw)
    }

    /// Convert raw OCEL to structured format.
    fn convert_raw_log(&self, raw: RawOcelLog) -> Result<OcelLog> {
        let mut events = HashMap::new();
        let mut objects = HashMap::new();

        // Convert events
        for raw_event in raw.events {
            let attributes = raw_event
                .attributes
                .unwrap_or_default()
                .into_iter()
                .filter_map(|(k, v)| json_to_attribute(v).map(|av| (k, av)))
                .collect();

            let objects_map = raw_event
                .object_refs
                .unwrap_or_default()
                .into_iter()
                .filter_map(|r| r.qualifier.map(|q| (r.object_id, q)))
                .collect();

            let event = OcelEvent {
                id: raw_event.id,
                activity: raw_event.activity,
                timestamp: parse_timestamp(&raw_event.timestamp)?,
                attributes,
                objects: objects_map,
            };
            events.insert(event.id.clone(), event);
        }

        // Convert objects
        for raw_object in raw.objects {
            let attributes = raw_object
                .attributes
                .unwrap_or_default()
                .into_iter()
                .filter_map(|(k, v)| json_to_attribute(v).map(|av| (k, av)))
                .collect();

            let object = OcelObject {
                id: raw_object.id,
                type_name: raw_object.object_type,
                attributes,
            };
            objects.insert(object.id.clone(), object);
        }

        // Validate references if enabled
        if self.validate_references {
            self.validate_references(&events, &objects)?;
        }

        Ok(OcelLog {
            events,
            objects,
            object_types: raw.object_types,
            event_types: raw.activities,
        })
    }

    /// Validate that all object references point to existing objects.
    fn validate_references(
        &self, events: &HashMap<String, OcelEvent>, objects: &HashMap<String, OcelObject>,
    ) -> Result<()> {
        for event in events.values() {
            for object_id in event.objects.keys() {
                if !objects.contains_key(object_id) {
                    return Err(Error::OcelParse(format!(
                        "event {} references non-existent object {}",
                        event.id, object_id
                    )));
                }
            }
        }

        Ok(())
    }
}

/// OCEL event log representation.
#[derive(Debug, Clone)]
pub struct OcelLog {
    /// All events in the log, keyed by event ID.
    pub events: HashMap<String, OcelEvent>,

    /// All objects in the log, keyed by object ID.
    pub objects: HashMap<String, OcelObject>,

    /// Object type definitions.
    pub object_types: Vec<OcelObjectType>,

    /// Event type (activity) definitions.
    pub event_types: Vec<String>,
}

impl OcelLog {
    /// Get events for a specific object.
    ///
    /// # Examples
    ///
    /// ```rust,ignore
    /// use ggen_process_mining::OcelLog;
    ///
    /// let log = OcelLog::from_file("log.jsonocel")?;
    /// let object_events = log.events_for_object("order-123");
    /// ```
    pub fn events_for_object(&self, object_id: &str) -> Vec<&OcelEvent> {
        self.events
            .values()
            .filter(|e| e.objects.contains_key(object_id))
            .collect()
    }

    /// Get objects involved in a specific event.
    pub fn objects_for_event(&self, event_id: &str) -> Vec<&OcelObject> {
        self.events
            .get(event_id)
            .map(|e| {
                e.objects
                    .keys()
                    .filter_map(|oid| self.objects.get(oid))
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Get all objects of a specific type.
    pub fn objects_by_type(&self, type_name: &str) -> Vec<&OcelObject> {
        self.objects
            .values()
            .filter(|o| o.type_name == type_name)
            .collect()
    }

    /// Get all unique activities in the log.
    pub fn unique_activities(&self) -> Vec<String> {
        let mut activities: Vec<_> = self.events.values().map(|e| e.activity.clone()).collect();
        activities.sort();
        activities.dedup();
        activities
    }
}

/// An OCEL event with multiple object references.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OcelEvent {
    /// Unique event identifier.
    pub id: String,

    /// Activity name (event type).
    pub activity: String,

    /// Event timestamp.
    pub timestamp: DateTime<Utc>,

    /// Additional event attributes.
    pub attributes: HashMap<String, AttributeValue>,

    /// Object references (object ID -> qualifier).
    pub objects: HashMap<String, String>,
}

/// An OCEL object.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OcelObject {
    /// Unique object identifier.
    pub id: String,

    /// Object type name.
    pub type_name: String,

    /// Object attributes.
    pub attributes: HashMap<String, AttributeValue>,
}

/// OCEL object type definition.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OcelObjectType {
    /// Object type name.
    pub name: String,

    /// Attribute definitions for this type.
    pub attributes: Vec<OcelAttribute>,
}

/// OCEL attribute definition.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OcelAttribute {
    /// Attribute name.
    pub name: String,

    /// Attribute type.
    pub type_name: String,
}

/// Raw OCEL log structure for JSON deserialization.
#[derive(Debug, Clone, Serialize, Deserialize)]
struct RawOcelLog {
    /// Object type definitions.
    #[serde(rename = "objectTypes")]
    pub object_types: Vec<OcelObjectType>,

    /// Activity definitions.
    pub activities: Vec<String>,

    /// All events.
    pub events: Vec<RawOcelEvent>,

    /// All objects.
    pub objects: Vec<RawOcelObject>,

    /// OCEL version.
    #[serde(rename = "ocel-version")]
    pub ocel_version: String,
}

/// Raw OCEL event.
#[derive(Debug, Clone, Serialize, Deserialize)]
struct RawOcelEvent {
    /// Event ID.
    pub id: String,

    /// Activity name.
    #[serde(rename = "activity")]
    pub activity: String,

    /// Event timestamp.
    pub timestamp: String,

    /// Event attributes.
    pub attributes: Option<HashMap<String, serde_json::Value>>,

    /// Object references.
    #[serde(rename = "objects")]
    pub object_refs: Option<Vec<ObjectRef>>,
}

/// Object reference in an event.
#[derive(Debug, Clone, Serialize, Deserialize)]
struct ObjectRef {
    /// Object ID.
    #[serde(rename = "id")]
    pub object_id: String,

    /// Qualifier for the reference.
    pub qualifier: Option<String>,
}

/// Raw OCEL object.
#[derive(Debug, Clone, Serialize, Deserialize)]
struct RawOcelObject {
    /// Object ID.
    pub id: String,

    /// Object type.
    #[serde(rename = "type")]
    pub object_type: String,

    /// Object attributes.
    pub attributes: Option<HashMap<String, serde_json::Value>>,
}

/// Parse a timestamp string into DateTime.
fn parse_timestamp(s: &str) -> Result<DateTime<Utc>> {
    s.parse::<DateTime<Utc>>()
        .map_err(|_| Error::invalid_timestamp(s.to_string()))
}

/// Convert JSON value to attribute value.
fn json_to_attribute(value: serde_json::Value) -> Option<AttributeValue> {
    match value {
        serde_json::Value::String(s) => Some(AttributeValue::String(s)),
        serde_json::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                Some(AttributeValue::Integer(i))
            } else if let Some(f) = n.as_f64() {
                Some(AttributeValue::Float(f))
            } else {
                None
            }
        }
        serde_json::Value::Bool(b) => Some(AttributeValue::Boolean(b)),
        serde_json::Value::Null => None,
        serde_json::Value::Array(_) => None,
        serde_json::Value::Object(_) => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const SIMPLE_OCEL: &str = r#"{
  "ocel-version": "2.0",
  "objectTypes": [
    {
      "name": "order",
      "attributes": [{"name": "price", "type": "float"}]
    },
    {
      "name": "item",
      "attributes": []
    }
  ],
  "activities": ["Create Order", "Pay Order"],
  "events": [
    {
      "id": "e1",
      "activity": "Create Order",
      "timestamp": "2024-01-01T10:00:00Z",
      "objects": [
        {"id": "o1", "qualifier": "creator"},
        {"id": "i1", "qualifier": "item"}
      ]
    },
    {
      "id": "e2",
      "activity": "Pay Order",
      "timestamp": "2024-01-01T11:00:00Z",
      "objects": [
        {"id": "o1", "qualifier": null}
      ]
    }
  ],
  "objects": [
    {
      "id": "o1",
      "type": "order",
      "attributes": {"price": 99.99}
    },
    {
      "id": "i1",
      "type": "item",
      "attributes": {}
    }
  ]
}"#;

    #[test]
    fn test_ocel_parser_creation() {
        let parser = OcelParser::new();
        assert!(parser.validate_references);
    }

    #[test]
    fn test_ocel_parser_with_config() {
        let parser = OcelParser::new().with_reference_validation(false);
        assert!(!parser.validate_references);
    }

    #[test]
    fn test_parse_simple_ocel() {
        let parser = OcelParser::new();
        let result = parser.parse_str(SIMPLE_OCEL);

        assert!(result.is_ok());

        let log = result.unwrap();
        assert_eq!(log.events.len(), 2);
        assert_eq!(log.objects.len(), 2);
        assert_eq!(log.object_types.len(), 2);
    }

    #[test]
    fn test_ocel_events_for_object() {
        let parser = OcelParser::new();
        let log = parser.parse_str(SIMPLE_OCEL).unwrap();

        let order_events = log.events_for_object("o1");
        assert_eq!(order_events.len(), 2);

        let item_events = log.events_for_object("i1");
        assert_eq!(item_events.len(), 1);
    }

    #[test]
    fn test_ocel_objects_for_event() {
        let parser = OcelParser::new();
        let log = parser.parse_str(SIMPLE_OCEL).unwrap();

        let e1_objects = log.objects_for_event("e1");
        assert_eq!(e1_objects.len(), 2);
    }

    #[test]
    fn test_ocel_objects_by_type() {
        let parser = OcelParser::new();
        let log = parser.parse_str(SIMPLE_OCEL).unwrap();

        let orders = log.objects_by_type("order");
        assert_eq!(orders.len(), 1);

        let items = log.objects_by_type("item");
        assert_eq!(items.len(), 1);
    }

    #[test]
    fn test_ocel_unique_activities() {
        let parser = OcelParser::new();
        let log = parser.parse_str(SIMPLE_OCEL).unwrap();

        let activities = log.unique_activities();
        assert_eq!(activities.len(), 2);
        assert!(activities.contains(&"Create Order".to_string()));
        assert!(activities.contains(&"Pay Order".to_string()));
    }

    #[test]
    fn test_parse_invalid_ocel_json() {
        let parser = OcelParser::new();
        let result = parser.parse_str("not json");

        assert!(result.is_err());
    }

    #[test]
    fn test_parse_timestamp() {
        let result = parse_timestamp("2024-01-01T10:00:00Z");
        assert!(result.is_ok());

        let invalid_result = parse_timestamp("not-a-timestamp");
        assert!(invalid_result.is_err());
    }
}
