use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Page {
    pub offset: u64,
    pub limit: u64,
}

impl Page {
    pub fn new(offset: u64, limit: u64) -> Self {
        Self { offset, limit: limit.min(100) }
    }
}

impl Default for Page {
    fn default() -> Self {
        Self::new(0, 20)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PagedResult<T> {
    pub items: Vec<T>,
    pub total: u64,
    pub offset: u64,
    pub limit: u64,
}

impl<T> PagedResult<T> {
    pub fn new(items: Vec<T>, total: u64, page: Page) -> Self {
        Self { items, total, offset: page.offset, limit: page.limit }
    }

    pub fn has_next(&self) -> bool {
        self.offset + self.limit < self.total
    }
}
