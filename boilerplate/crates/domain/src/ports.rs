use async_trait::async_trait;
use bp_core::{pagination::{Page, PagedResult}, Result};
use crate::entities::{Item, ItemId};

/// Outbound port — repository abstraction for `Item`.
#[async_trait]
pub trait ItemRepository: Send + Sync {
    async fn find_by_id(&self, id: &ItemId) -> Result<Item>;
    async fn list(&self, page: Page) -> Result<PagedResult<Item>>;
    async fn save(&self, item: &Item) -> Result<()>;
    async fn delete(&self, id: &ItemId) -> Result<()>;
}

/// Inbound port — use-case interface for `Item`.
#[async_trait]
pub trait ItemService: Send + Sync {
    async fn create_item(&self, name: String, description: Option<String>) -> Result<Item>;
    async fn get_item(&self, id: &ItemId) -> Result<Item>;
    async fn list_items(&self, page: Page) -> Result<PagedResult<Item>>;
    async fn delete_item(&self, id: &ItemId) -> Result<()>;
}
