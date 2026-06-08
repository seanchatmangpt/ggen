//! OCEL (Object-Centric Event Log) representation for ggen membrane events.
//!
//! Conforming to the OCEL standard JSON format, providing event maps, object maps,
//! and attribute mappings.

use super::core::{BoundaryCrossing, GgenMembrane, InterchangeablePart};
use crate::utils::error::Result;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Value representation in OCEL logs
// f64 fields are not Eq
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(untagged)]
pub enum OcelValue {
    /// String type
    String(String),
    /// Numeric types
    Number(f64),
    /// Boolean type
    Boolean(bool),
    /// List of strings (e.g. interfaces)
    StringArray(Vec<String>),
}

/// Object representation in OCEL
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OCELObject {
    /// The class or type of the object
    #[serde(rename = "ocel:type")]
    pub object_type: String,
    /// Attributes of the object
    #[serde(rename = "ocel:ovmap")]
    pub attributes: HashMap<String, OcelValue>,
}

/// Event representation in OCEL
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OCELEvent {
    /// The activity name
    #[serde(rename = "ocel:activity")]
    pub activity: String,
    /// Timestamp of the event in ISO 8601 format
    #[serde(rename = "ocel:timestamp")]
    pub timestamp: String,
    /// List of object identifiers related to this event
    #[serde(rename = "ocel:omap")]
    pub objects: Vec<String>,
    /// Attributes of the event
    #[serde(rename = "ocel:vmap")]
    pub attributes: HashMap<String, OcelValue>,
}

/// Full Object-Centric Event Log (OCEL)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OCEL {
    /// Objects in the log
    #[serde(rename = "ocel:objects")]
    pub objects: HashMap<String, OCELObject>,
    /// Events in the log
    #[serde(rename = "ocel:events")]
    pub events: HashMap<String, OCELEvent>,
}

impl OCEL {
    /// Generate an OCEL log from the membrane state
    pub fn from_membrane(membrane: &GgenMembrane) -> Self {
        let mut objects = HashMap::new();
        let mut events = HashMap::new();

        // 1. Map interchangeable parts as OCEL objects
        for (part_id, part) in &membrane.core.parts {
            let mut attrs = HashMap::new();
            attrs.insert(
                "version".to_string(),
                OcelValue::String(part.version.clone()),
            );
            attrs.insert(
                "part_type".to_string(),
                OcelValue::String(part.part_type.clone()),
            );
            attrs.insert(
                "payload_hash".to_string(),
                OcelValue::String(part.payload_hash.clone()),
            );
            attrs.insert(
                "payload_size".to_string(),
                OcelValue::Number(part.payload_size as f64),
            );
            attrs.insert(
                "interfaces".to_string(),
                OcelValue::StringArray(part.interfaces.clone()),
            );

            objects.insert(
                part_id.clone(),
                OCELObject {
                    object_type: "InterchangeablePart".to_string(),
                    attributes: attrs,
                },
            );
        }

        // 2. Map adapters as OCEL objects
        for (outer_port, inner_interface) in &membrane.adapters {
            let mut attrs = HashMap::new();
            attrs.insert(
                "inner_interface".to_string(),
                OcelValue::String(inner_interface.clone()),
            );

            objects.insert(
                format!("adapter:{}", outer_port),
                OCELObject {
                    object_type: "AdapterBinding".to_string(),
                    attributes: attrs,
                },
            );
        }

        // 3. Map boundary events as OCEL events
        for crossing in &membrane.event_log {
            let mut attrs = HashMap::new();
            attrs.insert(
                "crossing_type".to_string(),
                OcelValue::String(crossing.crossing_type.clone()),
            );
            attrs.insert(
                "interface_fn".to_string(),
                OcelValue::String(crossing.interface_fn.clone()),
            );
            attrs.insert(
                "input_hash".to_string(),
                OcelValue::String(crossing.input_hash.clone()),
            );
            if let Some(ref oh) = crossing.output_hash {
                attrs.insert("output_hash".to_string(), OcelValue::String(oh.clone()));
            }
            attrs.insert(
                "status_code".to_string(),
                OcelValue::Number(crossing.status_code as f64),
            );
            attrs.insert(
                "duration_us".to_string(),
                OcelValue::Number(crossing.duration_us as f64),
            );

            // Relate event to the caller and callee objects
            let mut related_objects = vec![crossing.callee_id.clone()];
            // If there's an adapter corresponding to the interface, list that object too
            for (outer_port, inner_interface) in &membrane.adapters {
                if inner_interface.contains(&crossing.interface_fn) {
                    related_objects.push(format!("adapter:{}", outer_port));
                }
            }

            events.insert(
                crossing.id.clone(),
                OCELEvent {
                    activity: format!("BoundaryCrossing:{}", crossing.interface_fn),
                    timestamp: crossing.timestamp.to_rfc3339(),
                    objects: related_objects,
                    attributes: attrs,
                },
            );
        }

        Self { objects, events }
    }

    /// Serialize to JSON string
    pub fn to_json(&self) -> Result<String> {
        Ok(serde_json::to_string_pretty(self)?)
    }
}
