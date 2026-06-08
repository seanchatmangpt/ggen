use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Represents an OCEL (Object-Centric Event Log) data structure.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Default)]
pub struct OCEL {
    /// The list of objects in the log.
    pub objects: Vec<OCELObject>,
    /// The list of events in the log.
    pub events: Vec<OCELEvent>,
}

impl OCEL {
    /// Creates a new empty `OCEL`.
    pub fn new() -> Self {
        Self::default()
    }
}

/// Represents an object in the OCEL log.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct OCELObject {
    /// The unique identifier of the object.
    pub id: String,
    /// The type classification of the object.
    pub r#type: String,
    /// Custom attributes associated with the object.
    pub attributes: HashMap<String, String>,
}

/// Represents an event in the OCEL log.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct OCELEvent {
    /// The unique identifier of the event.
    pub id: String,
    /// The activity name associated with this event.
    pub activity: String,
    /// The timestamp when the event occurred.
    pub timestamp: chrono::DateTime<chrono::Utc>,
    /// Reference to objects involved in this event.
    pub objects: Vec<OCELObjectRef>,
    /// Custom attributes associated with the event.
    pub attributes: HashMap<String, String>,
}

/// A reference to an object from an event, optionally with a qualifier.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct OCELObjectRef {
    /// The unique identifier of the referenced object.
    pub id: String,
    /// The type of the referenced object.
    pub r#type: String,
    /// An optional qualifier defining the role of the object in the event.
    pub qualifier: Option<String>,
}
