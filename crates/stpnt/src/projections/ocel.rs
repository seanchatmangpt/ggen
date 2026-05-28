use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct Ocel2Log {
    pub objects: Vec<OcelObject>,
    pub events: Vec<OcelEvent>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct OcelObject {
    pub id: String,
    pub object_type: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct OcelEvent {
    pub id: String,
    pub activity: String,
    pub timestamp: String,
    pub object_refs: Vec<String>,
}
