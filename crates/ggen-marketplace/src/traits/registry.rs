use crate::error::Result;
use crate::models::{Package, PackageId, Query};
use async_trait::async_trait;

/// Extended registry trait with additional functionality
#[async_trait]
pub trait RegistryExt: super::Registry {
    /// Search packages with pagination
    async fn search_paginated(
        &self, query: &Query, page: usize, per_page: usize,
    ) -> Result<PaginatedResults>;

    /// Get package statistics
    async fn get_stats(&self, id: &PackageId) -> Result<crate::models::PackageStats>;

    /// Update package statistics
    async fn update_stats(&self, id: &PackageId, stats: crate::models::PackageStats) -> Result<()>;

    /// Get package dependencies recursively
    async fn get_dependencies_recursive(&self, id: &PackageId) -> Result<Vec<Package>>;

    /// Check for package updates
    async fn check_updates(&self, packages: &[PackageId]) -> Result<Vec<UpdateInfo>>;

    /// Get trending packages
    async fn get_trending(&self, limit: usize) -> Result<Vec<Package>>;

    /// Get recently updated packages
    async fn get_recent(&self, limit: usize) -> Result<Vec<Package>>;
}

/// Paginated search results
#[derive(Debug, Clone)]
pub struct PaginatedResults {
    pub packages: Vec<Package>,
    pub total_count: usize,
    pub page: usize,
    pub per_page: usize,
    pub total_pages: usize,
}

impl PaginatedResults {
    pub fn new(packages: Vec<Package>, total_count: usize, page: usize, per_page: usize) -> Self {
        let total_pages = total_count.div_ceil(per_page);
        Self {
            packages,
            total_count,
            page,
            per_page,
            total_pages,
        }
    }

    pub fn has_next_page(&self) -> bool {
        self.page < self.total_pages
    }

    pub fn has_prev_page(&self) -> bool {
        self.page > 1
    }
}

/// Package update information
#[derive(Debug, Clone)]
pub struct UpdateInfo {
    pub package_id: PackageId,
    pub current_version: String,
    pub latest_version: String,
    pub update_available: bool,
}
