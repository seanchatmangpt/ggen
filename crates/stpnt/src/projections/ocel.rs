use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct Ocel2Log {
    pub objects: Vec<OCELObject>,
    pub events: Vec<OCELEvent>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct OCELObject {
    pub id: String,
    pub object_type: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct OCELEvent {
    pub id: String,
    pub activity: String,
    pub timestamp: String,
    pub object_refs: Vec<String>,
}
