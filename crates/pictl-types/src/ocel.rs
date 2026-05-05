use crate::event_log::Attributes;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct OCELEventObjectRef {
    pub object_id: String,
    pub object_type: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct OCELEvent {
    pub event_id: String,
    pub event_type: String,
    pub timestamp: String,
    pub attributes: Attributes,
    pub object_refs: Vec<OCELEventObjectRef>,
}

impl OCELEvent {
    pub fn new(event_id: String, event_type: String, timestamp: String) -> Self {
        OCELEvent {
            event_id,
            event_type,
            timestamp,
            attributes: Attributes::new(),
            object_refs: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct OCELObject {
    pub object_id: String,
    pub object_type: String,
    pub ovmap: Attributes,
}

impl OCELObject {
    pub fn new(object_id: String, object_type: String) -> Self {
        OCELObject {
            object_id,
            object_type,
            ovmap: Attributes::new(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct OCEL {
    pub version: String,
    pub ordering: String,
    pub objects: Vec<OCELObject>,
    pub events: Vec<OCELEvent>,
    pub attributes: Attributes,
}

impl OCEL {
    pub fn new() -> Self {
        OCEL {
            version: "2.0".to_string(),
            ordering: "timestamp".to_string(),
            objects: Vec::new(),
            events: Vec::new(),
            attributes: Attributes::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.events.len()
    }

    pub fn is_empty(&self) -> bool {
        self.events.is_empty()
    }
}

impl Default for OCEL {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ocel_creation() {
        let ocel = OCEL::new();
        assert_eq!(ocel.version, "2.0");
        assert!(ocel.is_empty());
    }
}
