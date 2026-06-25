use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Represents an OCEL (Object-Centric Event Log) data structure.
/// Local working type — converts to/from wasm4pm-compat's authority types.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Default)]
pub struct OcelLog {
    pub objects: Vec<OcelObject>,
    pub events: Vec<OcelEvent>,
}

impl OcelLog {
    pub fn new() -> Self {
        Self::default()
    }
}

/// Represents an object in the OCEL log.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct OcelObject {
    pub id: String,
    pub r#type: String,
    pub attributes: HashMap<String, String>,
}

/// Represents an event in the OCEL log.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct OcelEvent {
    pub id: String,
    pub activity: String,
    pub timestamp: chrono::DateTime<chrono::Utc>,
    pub objects: Vec<OcelObjectRef>,
    pub attributes: HashMap<String, String>,
}

/// A reference to an object from an event, optionally with a qualifier.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct OcelObjectRef {
    pub id: String,
    pub r#type: String,
    pub qualifier: Option<String>,
}
