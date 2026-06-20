use bp_core::id::Id;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

/// Marker type for the example entity.
#[derive(Debug, Clone, Copy)]
pub struct ItemMarker;

pub type ItemId = Id<ItemMarker>;

/// Example domain entity with typed ID.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Item {
    pub id: ItemId,
    pub name: String,
    pub description: Option<String>,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
}

impl Item {
    pub fn new(name: impl Into<String>) -> Self {
        let now = Utc::now();
        Self {
            id: ItemId::new(),
            name: name.into(),
            description: None,
            created_at: now,
            updated_at: now,
        }
    }
}
