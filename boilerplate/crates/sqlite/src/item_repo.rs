use anyhow::anyhow;
use async_trait::async_trait;
use bp_core::{
    pagination::{Page, PagedResult},
    Result,
};
use chrono::{DateTime, Utc};
use domain::{
    entities::{Item, ItemId},
    ports::ItemRepository,
};
use sqlx::{FromRow, SqlitePool};

pub struct SqliteItemRepository {
    pool: SqlitePool,
}

impl SqliteItemRepository {
    pub fn new(pool: SqlitePool) -> Self {
        Self { pool }
    }
}

/// Raw row returned by SQLite — temporal fields stored as RFC3339 text.
#[derive(FromRow)]
struct ItemRow {
    id: String,
    name: String,
    description: Option<String>,
    created_at: String,
    updated_at: String,
}

/// Parse an [`ItemId`] from its UUID string representation.
///
/// `Id<T>` is `#[serde(transparent)]` over `Uuid`, so we reconstruct it via
/// JSON deserialization of a quoted UUID string — no unsafe, no private-field access.
fn parse_item_id(s: &str) -> std::result::Result<ItemId, bp_core::CoreError> {
    let json = format!("\"{}\"", s);
    serde_json::from_str::<ItemId>(&json)
        .map_err(|e| bp_core::CoreError::Internal(anyhow!("invalid item id {s:?}: {e}")))
}

impl TryFrom<ItemRow> for Item {
    type Error = bp_core::CoreError;

    fn try_from(row: ItemRow) -> std::result::Result<Self, Self::Error> {
        let id = parse_item_id(&row.id)?;

        let created_at = DateTime::parse_from_rfc3339(&row.created_at)
            .map_err(|e| bp_core::CoreError::Internal(anyhow!("invalid created_at: {e}")))?
            .with_timezone(&Utc);

        let updated_at = DateTime::parse_from_rfc3339(&row.updated_at)
            .map_err(|e| bp_core::CoreError::Internal(anyhow!("invalid updated_at: {e}")))?
            .with_timezone(&Utc);

        Ok(Item {
            id,
            name: row.name,
            description: row.description,
            created_at,
            updated_at,
        })
    }
}

#[async_trait]
impl ItemRepository for SqliteItemRepository {
    async fn find_by_id(&self, id: &ItemId) -> Result<Item> {
        let id_str = id.to_string();

        let row: Option<ItemRow> = sqlx::query_as(
            "SELECT id, name, description, created_at, updated_at FROM items WHERE id = ?1",
        )
        .bind(&id_str)
        .fetch_optional(&self.pool)
        .await
        .map_err(|e| bp_core::CoreError::Internal(anyhow!("db error in find_by_id: {e}")))?;

        match row {
            Some(r) => Item::try_from(r),
            None => Err(bp_core::CoreError::not_found("Item", id)),
        }
    }

    async fn list(&self, page: Page) -> Result<PagedResult<Item>> {
        let limit = page.limit as i64;
        let offset = page.offset as i64;

        let rows: Vec<ItemRow> = sqlx::query_as(
            "SELECT id, name, description, created_at, updated_at \
             FROM items \
             ORDER BY created_at DESC \
             LIMIT ?1 OFFSET ?2",
        )
        .bind(limit)
        .bind(offset)
        .fetch_all(&self.pool)
        .await
        .map_err(|e| bp_core::CoreError::Internal(anyhow!("db error in list: {e}")))?;

        let (count,): (i64,) = sqlx::query_as("SELECT COUNT(*) FROM items")
            .fetch_one(&self.pool)
            .await
            .map_err(|e| bp_core::CoreError::Internal(anyhow!("db error in count: {e}")))?;

        let total = count as u64;

        let items = rows
            .into_iter()
            .map(Item::try_from)
            .collect::<std::result::Result<Vec<_>, _>>()?;

        Ok(PagedResult::new(items, total, page))
    }

    async fn save(&self, item: &Item) -> Result<()> {
        let id_str = item.id.to_string();
        let created_at = item.created_at.to_rfc3339();
        let updated_at = item.updated_at.to_rfc3339();

        sqlx::query(
            "INSERT OR REPLACE INTO items (id, name, description, created_at, updated_at) \
             VALUES (?1, ?2, ?3, ?4, ?5)",
        )
        .bind(&id_str)
        .bind(&item.name)
        .bind(&item.description)
        .bind(&created_at)
        .bind(&updated_at)
        .execute(&self.pool)
        .await
        .map_err(|e| bp_core::CoreError::Internal(anyhow!("db error in save: {e}")))?;

        Ok(())
    }

    async fn delete(&self, id: &ItemId) -> Result<()> {
        let id_str = id.to_string();

        let result = sqlx::query("DELETE FROM items WHERE id = ?1")
            .bind(&id_str)
            .execute(&self.pool)
            .await
            .map_err(|e| bp_core::CoreError::Internal(anyhow!("db error in delete: {e}")))?;

        if result.rows_affected() == 0 {
            return Err(bp_core::CoreError::not_found("Item", id));
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use bp_core::pagination::Page;
    use sqlx::sqlite::SqlitePoolOptions;

    async fn setup() -> SqliteItemRepository {
        let pool = SqlitePoolOptions::new()
            .connect(":memory:")
            .await
            .unwrap();
        sqlx::query(include_str!("../migrations/0001_items.sql"))
            .execute(&pool)
            .await
            .unwrap();
        SqliteItemRepository::new(pool)
    }

    #[tokio::test]
    async fn save_and_find_roundtrip() {
        let repo = setup().await;
        let mut item = Item::new("test item");
        item.description = Some("a description".to_string());

        repo.save(&item).await.unwrap();

        let found = repo.find_by_id(&item.id).await.unwrap();
        // Compare IDs via their string representation (ItemMarker lacks PartialEq).
        assert_eq!(found.id.to_string(), item.id.to_string());
        assert_eq!(found.name, item.name);
        assert_eq!(found.description, item.description);
        // RFC3339 round-trip is second-level precise; compare timestamps at that granularity.
        assert_eq!(found.created_at.timestamp(), item.created_at.timestamp());
        assert_eq!(found.updated_at.timestamp(), item.updated_at.timestamp());
    }

    #[tokio::test]
    async fn list_returns_saved_items() {
        let repo = setup().await;
        let item_a = Item::new("alpha");
        let item_b = Item::new("beta");

        repo.save(&item_a).await.unwrap();
        repo.save(&item_b).await.unwrap();

        let page = Page::new(0, 10);
        let result = repo.list(page).await.unwrap();

        assert_eq!(result.total, 2);
        assert_eq!(result.items.len(), 2);
        let names: Vec<&str> = result.items.iter().map(|i| i.name.as_str()).collect();
        assert!(names.contains(&"alpha"));
        assert!(names.contains(&"beta"));
    }

    #[tokio::test]
    async fn delete_removes_item() {
        let repo = setup().await;
        let item = Item::new("to delete");
        repo.save(&item).await.unwrap();

        repo.delete(&item.id).await.unwrap();

        let result = repo.find_by_id(&item.id).await;
        assert!(
            matches!(result, Err(bp_core::CoreError::NotFound { .. })),
            "expected NotFound after delete, got: {result:?}",
        );
    }

    #[tokio::test]
    async fn find_missing_returns_not_found() {
        let repo = setup().await;
        let id = ItemId::new();

        let result = repo.find_by_id(&id).await;
        assert!(
            matches!(result, Err(bp_core::CoreError::NotFound { .. })),
            "expected NotFound for unknown id, got: {result:?}",
        );
    }

    #[tokio::test]
    async fn delete_missing_returns_not_found() {
        let repo = setup().await;
        let id = ItemId::new();

        let result = repo.delete(&id).await;
        assert!(
            matches!(result, Err(bp_core::CoreError::NotFound { .. })),
            "expected NotFound when deleting nonexistent item, got: {result:?}",
        );
    }

    #[tokio::test]
    async fn list_pagination_respects_offset_and_limit() {
        let repo = setup().await;
        for i in 0..5_u32 {
            repo.save(&Item::new(format!("item-{i}"))).await.unwrap();
        }

        let page = Page::new(2, 2);
        let result = repo.list(page).await.unwrap();

        assert_eq!(result.total, 5);
        assert_eq!(result.items.len(), 2);
        assert_eq!(result.offset, 2);
        assert_eq!(result.limit, 2);
    }

    #[tokio::test]
    async fn save_replaces_existing_item() {
        let repo = setup().await;
        let mut item = Item::new("original");
        repo.save(&item).await.unwrap();

        item.name = "updated".to_string();
        repo.save(&item).await.unwrap();

        let found = repo.find_by_id(&item.id).await.unwrap();
        assert_eq!(found.name, "updated");

        let result = repo.list(Page::new(0, 10)).await.unwrap();
        assert_eq!(result.total, 1);
    }
}
