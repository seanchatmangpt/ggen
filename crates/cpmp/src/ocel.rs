//! OCEL 2.0 (Object-Centric Event Log) reader and data structures.
//!
//! Implements the OCEL 2.0 standard for multi-object process mining.
//! See: <https://www.ocel-standard.org/2.0/>

use std::collections::HashMap;

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

// ── Error ────────────────────────────────────────────────────────────────────

#[derive(Debug, thiserror::Error)]
pub enum OcelError {
    #[error("parse error: {0}")]
    Parse(#[from] serde_json::Error),
    #[error("io error: {0}")]
    Io(#[from] std::io::Error),
    #[error("missing required field: {0}")]
    MissingField(&'static str),
    #[error("invalid timestamp: {0}")]
    InvalidTimestamp(String),
}

pub type OcelResult<T> = Result<T, OcelError>;

// ── Core OCEL 2.0 Structures ─────────────────────────────────────────────────

/// An attribute key-value pair attached to an event or object.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OcelAttribute {
    pub key: String,
    pub value: serde_json::Value,
    /// Timestamp when this attribute value became valid (for evolving attributes).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub time: Option<DateTime<Utc>>,
}

/// A qualified relationship between an event and an object.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OcelRelationship {
    pub object_id: String,
    pub qualifier: String,
}

/// An OCEL 2.0 event — the atomic unit of process execution.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OcelEvent {
    pub id: String,
    pub activity: String,
    pub timestamp: DateTime<Utc>,
    #[serde(default)]
    pub relationships: Vec<OcelRelationship>,
    #[serde(default)]
    pub attributes: Vec<OcelAttribute>,
}

impl OcelEvent {
    /// Returns the IDs of all objects related to this event.
    pub fn object_ids(&self) -> Vec<&str> {
        self.relationships
            .iter()
            .map(|r| r.object_id.as_str())
            .collect()
    }

    /// Returns the qualifiers for a specific object in this event.
    pub fn qualifiers_for(&self, object_id: &str) -> Vec<&str> {
        self.relationships
            .iter()
            .filter(|r| r.object_id == object_id)
            .map(|r| r.qualifier.as_str())
            .collect()
    }
}

/// An object-to-object relationship.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OcelO2ORelationship {
    pub source_object_id: String,
    pub target_object_id: String,
    pub qualifier: String,
}

/// An OCEL 2.0 object — a business entity that participates in events.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OcelObject {
    pub id: String,
    pub object_type: String,
    #[serde(default)]
    pub attributes: Vec<OcelAttribute>,
    #[serde(default)]
    pub relationships: Vec<OcelO2ORelationship>,
}

/// Attribute specification as declared in the log schema.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OcelAttributeSpec {
    pub name: String,
    #[serde(rename = "type")]
    pub attr_type: String,
}

/// Object-type declaration in the log schema.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OcelObjectTypeSpec {
    pub name: String,
    #[serde(default)]
    pub attributes: Vec<OcelAttributeSpec>,
}

/// Event-type declaration in the log schema.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OcelEventTypeSpec {
    pub name: String,
    #[serde(default)]
    pub attributes: Vec<OcelAttributeSpec>,
}

/// Complete OCEL 2.0 event log — the top-level container.
///
/// Matches the OCEL 2.0 JSON serialisation format exactly.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OcelEventLog {
    #[serde(rename = "objectTypes")]
    pub object_types: Vec<OcelObjectTypeSpec>,
    #[serde(rename = "eventTypes")]
    pub event_types: Vec<OcelEventTypeSpec>,
    pub objects: Vec<OcelObject>,
    pub events: Vec<OcelEvent>,
}

impl OcelEventLog {
    /// Parse an OCEL 2.0 JSON string.
    pub fn from_json(json: &str) -> OcelResult<Self> {
        serde_json::from_str(json).map_err(OcelError::Parse)
    }

    /// Parse an OCEL 2.0 JSON file at the given path.
    pub fn from_json_file(path: &std::path::Path) -> OcelResult<Self> {
        let content = std::fs::read_to_string(path)?;
        Self::from_json(&content)
    }

    /// Number of events in the log.
    pub fn event_count(&self) -> usize {
        self.events.len()
    }

    /// Number of objects in the log.
    pub fn object_count(&self) -> usize {
        self.objects.len()
    }

    /// All distinct activity names, sorted.
    pub fn activity_types(&self) -> Vec<&str> {
        let mut types: Vec<&str> = self
            .events
            .iter()
            .map(|e| e.activity.as_str())
            .collect::<std::collections::HashSet<_>>()
            .into_iter()
            .collect();
        types.sort_unstable();
        types
    }

    /// All declared object-type names, sorted.
    pub fn object_type_names(&self) -> Vec<&str> {
        let mut types: Vec<&str> = self
            .object_types
            .iter()
            .map(|t| t.name.as_str())
            .collect();
        types.sort_unstable();
        types
    }

    /// Events sorted ascending by timestamp.
    pub fn sorted_events(&self) -> Vec<&OcelEvent> {
        let mut events: Vec<&OcelEvent> = self.events.iter().collect();
        events.sort_by_key(|e| e.timestamp);
        events
    }

    /// Events related to a specific object, sorted by timestamp.
    pub fn events_for_object(&self, object_id: &str) -> Vec<&OcelEvent> {
        let mut events: Vec<&OcelEvent> = self
            .events
            .iter()
            .filter(|e| e.object_ids().contains(&object_id))
            .collect();
        events.sort_by_key(|e| e.timestamp);
        events
    }

    /// Directly-follows pairs for a given object: `(predecessor_activity, successor_activity)`.
    pub fn directly_follows_for_object(&self, object_id: &str) -> Vec<(&str, &str)> {
        let events = self.events_for_object(object_id);
        events
            .windows(2)
            .map(|w| (w[0].activity.as_str(), w[1].activity.as_str()))
            .collect()
    }

    /// Build a directly-follows graph (DFG) across all objects.
    ///
    /// Returns a map of `(activity_a, activity_b)` → occurrence count.
    pub fn directly_follows_graph(&self) -> HashMap<(String, String), usize> {
        let mut dfg: HashMap<(String, String), usize> = HashMap::new();
        for obj in &self.objects {
            for (a, b) in self.directly_follows_for_object(&obj.id) {
                *dfg.entry((a.to_owned(), b.to_owned())).or_insert(0) += 1;
            }
        }
        dfg
    }

    /// Compute process variants per object type.
    ///
    /// Returns `{ object_type → { "A -> B -> C" → frequency } }`.
    pub fn variants_per_object_type(&self) -> HashMap<String, HashMap<String, usize>> {
        let mut result: HashMap<String, HashMap<String, usize>> = HashMap::new();
        for obj in &self.objects {
            let trace: Vec<&str> = self
                .events_for_object(&obj.id)
                .iter()
                .map(|e| e.activity.as_str())
                .collect();
            let variant = trace.join(" -> ");
            result
                .entry(obj.object_type.clone())
                .or_default()
                .entry(variant)
                .and_modify(|c| *c += 1)
                .or_insert(1);
        }
        result
    }

    /// Compute summary statistics for the log.
    pub fn stats(&self) -> EventLogStats {
        let dfg = self.directly_follows_graph();
        let variants = self.variants_per_object_type();
        let variant_count: usize = variants.values().map(|v| v.len()).sum();
        EventLogStats {
            event_count: self.events.len(),
            object_count: self.objects.len(),
            activity_type_count: self.activity_types().len(),
            object_type_count: self.object_types.len(),
            dfg_edge_count: dfg.len(),
            variant_count,
        }
    }
}

// ── OcelReader ───────────────────────────────────────────────────────────────

/// Reads OCEL 2.0 event logs from various sources.
///
/// Currently supports the OCEL 2.0 JSON format.  RDF/Turtle and Oxigraph-backed
/// stores can be added here without changing the public `OcelEventLog` API.
pub struct OcelReader;

impl OcelReader {
    /// Parse an OCEL 2.0 event log from a JSON string.
    pub fn from_json(json: &str) -> OcelResult<OcelEventLog> {
        OcelEventLog::from_json(json)
    }

    /// Parse an OCEL 2.0 event log from a JSON file on disk.
    pub fn from_json_file(path: &std::path::Path) -> OcelResult<OcelEventLog> {
        OcelEventLog::from_json_file(path)
    }
}

// ── EventLogStats ─────────────────────────────────────────────────────────────

/// Summary statistics for an OCEL 2.0 event log.
#[derive(Debug, Clone, Serialize)]
pub struct EventLogStats {
    pub event_count: usize,
    pub object_count: usize,
    pub activity_type_count: usize,
    pub object_type_count: usize,
    /// Number of distinct edges in the directly-follows graph.
    pub dfg_edge_count: usize,
    /// Total number of distinct process variants across all object types.
    pub variant_count: usize,
}
