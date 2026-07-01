use chrono::FixedOffset;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use wasm4pm_compat::ocel::{OCELEvent, OCELObject, OCELRelationship, OCELType, OCEL};

/// Represents an OCEL (Object-Centric Event Log) data structure.
/// Local working type — converts to/from wasm4pm-compat's authority types.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Default)]
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
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct OcelObject {
    pub id: String,
    pub r#type: String,
    pub attributes: HashMap<String, String>,
}

/// Represents an event in the OCEL log.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct OcelEvent {
    pub id: String,
    pub activity: String,
    pub timestamp: chrono::DateTime<chrono::Utc>,
    pub objects: Vec<OcelObjectRef>,
    pub attributes: HashMap<String, String>,
}

/// A reference to an object from an event, optionally with a qualifier.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct OcelObjectRef {
    pub id: String,
    pub r#type: String,
    pub qualifier: Option<String>,
}

/// Convert a ggen-graph `OcelLog` to `wasm4pm_compat::ocel::OCEL`.
///
/// Used internally by discovery and conformance modules to delegate
/// process-intelligence work to the authoritative wasm4pm-compat algorithms.
#[allow(clippy::expect_used)]
pub(crate) fn to_compat_ocel(log: &OcelLog) -> OCEL {
    use std::collections::HashSet;

    let event_type_names: HashSet<&str> = log.events.iter().map(|e| e.activity.as_str()).collect();
    let object_type_names: HashSet<&str> = log.objects.iter().map(|o| o.r#type.as_str()).collect();

    let event_types: Vec<OCELType> = event_type_names
        .into_iter()
        .map(|name| OCELType {
            name: name.to_string(),
            attributes: vec![],
        })
        .collect();

    let object_types: Vec<OCELType> = object_type_names
        .into_iter()
        .map(|name| OCELType {
            name: name.to_string(),
            attributes: vec![],
        })
        .collect();

    let objects: Vec<OCELObject> = log
        .objects
        .iter()
        .map(|o| OCELObject {
            id: o.id.clone(),
            object_type: o.r#type.clone(),
            attributes: vec![],
            relationships: vec![],
        })
        .collect();

    let utc: FixedOffset = FixedOffset::east_opt(0).expect("UTC+0 is a valid offset");
    let events: Vec<OCELEvent> = log
        .events
        .iter()
        .map(|e| {
            let time = e.timestamp.with_timezone(&utc);
            let relationships: Vec<OCELRelationship> = e
                .objects
                .iter()
                .map(|r| OCELRelationship {
                    object_id: r.id.clone(),
                    qualifier: r.qualifier.clone().unwrap_or_else(|| r.r#type.clone()),
                })
                .collect();
            OCELEvent {
                id: e.id.clone(),
                event_type: e.activity.clone(),
                time,
                attributes: vec![],
                relationships,
            }
        })
        .collect();

    OCEL {
        event_types,
        object_types,
        events,
        objects,
    }
}
