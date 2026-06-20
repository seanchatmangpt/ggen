use async_trait::async_trait;
use bp_core::{
    pagination::{Page, PagedResult},
    Result,
};
use domain::{
    entities::{Item, ItemId},
    ports::ItemRepository,
};
use sqlx::SqlitePool;

pub struct SqliteItemRepository {
    pool: SqlitePool,
}

impl SqliteItemRepository {
    pub fn new(pool: SqlitePool) -> Self {
        Self { pool }
    }
}

#[async_trait]
impl ItemRepository for SqliteItemRepository {
    async fn find_by_id(&self, id: &ItemId) -> Result<Item> {
        let _ = (&self.pool, id);
        Err(bp_core::CoreError::not_found("Item", id))
    }

    async fn list(&self, page: Page) -> Result<PagedResult<Item>> {
        let _ = (&self.pool, &page);
        Ok(PagedResult::new(vec![], 0, page))
    }

    async fn save(&self, item: &Item) -> Result<()> {
        let _ = (&self.pool, item);
        Ok(())
    }

    async fn delete(&self, id: &ItemId) -> Result<()> {
        let _ = (&self.pool, id);
        Ok(())
    }
}
