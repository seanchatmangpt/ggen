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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn item_new_has_non_nil_id() {
        let item = Item::new("foo");
        // A nil UUID is all zeros; new() uses Uuid::new_v4() so it must not be nil.
        assert_ne!(
            item.id.inner().as_u128(),
            0u128,
            "Item::new must assign a non-nil UUID"
        );
    }

    #[test]
    fn item_new_stores_name_verbatim() {
        let item = Item::new("foo");
        assert_eq!(item.name, "foo");
    }

    #[test]
    fn item_new_has_no_description() {
        let item = Item::new("foo");
        assert!(
            item.description.is_none(),
            "new Item must have description=None"
        );
    }

    #[test]
    fn item_new_created_at_equals_updated_at() {
        let item = Item::new("bar");
        assert_eq!(
            item.created_at, item.updated_at,
            "created_at and updated_at must be equal for a newly created Item"
        );
    }
}
