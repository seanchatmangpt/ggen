//! V2 Adapter - Legacy adapter code (not used by CLI)
//!
//! NOTE: This module is legacy code from the v1→v2 migration.
//! The CLI (`crates/ggen-cli/src/cmds/marketplace.rs`) uses `ggen-marketplace-v2` directly
//! and bypasses this adapter layer entirely.
//!
//! This code is kept for reference but is not actively used.
//!
//! # Current Architecture
//!
//! ```text
//! CLI Commands (marketplace.rs)
//!      │
//!      └─→ ggen-marketplace-v2 directly
//!           (no adapter layer)
//! ```

use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};

/// Search backend enum - selects which implementation to use
///
/// This enum is the core of the adapter pattern, allowing compile-time
/// selection of the search backend based on feature flags.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SearchBackend {
    /// RDF-backed marketplace search (v2)
    /// NOTE: V1 has been removed. CLI uses ggen-marketplace-v2 directly.
    V2,
}

impl SearchBackend {
    /// Get the active search backend
    ///
    /// Always returns V2 since marketplace v1 has been removed.
    /// The CLI uses ggen-marketplace-v2 directly, bypassing this adapter.
    pub fn active() -> Self {
        Self::V2
    }
}

/// Unified search query input
///
/// This is a common interface that works with both v1 and v2 backends.
/// It's converted to backend-specific types via From implementations.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnifiedSearchQuery {
    /// Search text
    pub query: String,

    /// Filter by category
    pub category: Option<String>,

    /// Filter by author
    pub author: Option<String>,

    /// Maximum results
    pub limit: usize,

    /// Enable fuzzy matching
    pub fuzzy: bool,

    /// Minimum quality score (0-100)
    pub min_quality: Option<u32>,
}

impl UnifiedSearchQuery {
    /// Create a new search query
    pub fn new(query: impl Into<String>) -> Self {
        Self {
            query: query.into(),
            category: None,
            author: None,
            limit: 10,
            fuzzy: false,
            min_quality: None,
        }
    }

    /// Set category filter
    pub fn with_category(mut self, category: impl Into<String>) -> Self {
        self.category = Some(category.into());
        self
    }

    /// Set author filter
    pub fn with_author(mut self, author: impl Into<String>) -> Self {
        self.author = Some(author.into());
        self
    }

    /// Set result limit
    pub fn with_limit(mut self, limit: usize) -> Self {
        self.limit = limit;
        self
    }

    /// Enable fuzzy search
    pub fn with_fuzzy(mut self, fuzzy: bool) -> Self {
        self.fuzzy = fuzzy;
        self
    }

    /// Set minimum quality filter
    pub fn with_min_quality(mut self, score: u32) -> Self {
        self.min_quality = Some(score);
        self
    }
}

/// Unified search result
///
/// This is the common result type returned by both backends.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnifiedSearchResult {
    /// Package name
    pub name: String,

    /// Package version
    pub version: String,

    /// Package description
    pub description: String,

    /// Package author
    pub author: Option<String>,

    /// Download count
    pub downloads: u32,

    /// Star count
    pub stars: u32,

    /// Relevance score (0.0-1.0)
    pub relevance: f64,
}

// ==================== V1 Conversions ====================

// NOTE: v1 conversions removed - marketplace v1 no longer exists
// CLI uses ggen-marketplace-v2 directly, bypassing this adapter

// ==================== V2 Conversions ====================
//
// NOTE: V2 conversions are disabled because ggen-marketplace-v2 currently has
// compilation errors. Once those are fixed, uncomment this section to enable v2.
//
// The architecture is in place, but the implementation is deferred until v2 compiles.

// // NOTE: v2 is now the only option
// mod v2_conversions {
//     use super::*;
//
//     /// Convert UnifiedSearchQuery → v2 SearchQuery
//     impl From<UnifiedSearchQuery> for ggen_marketplace_v2::search::SearchQuery {
//         fn from(query: UnifiedSearchQuery) -> Self {
//             let mut q = Self::new(query.query).with_limit(query.limit);
//
//             if let Some(category) = query.category {
//                 q = q.with_category(category);
//             }
//
//             if let Some(author) = query.author {
//                 q = q.with_author(author);
//             }
//
//             if let Some(min_quality) = query.min_quality {
//                 // Convert u32 to QualityScore
//                 if let Ok(score) = ggen_marketplace_v2::models::QualityScore::new(min_quality) {
//                     q = q.with_min_quality(score);
//                 }
//             }
//
//             q
//         }
//     }
//
//     /// Convert v2 SearchResult → UnifiedSearchResult
//     impl From<ggen_marketplace_v2::models::SearchResult> for UnifiedSearchResult {
//         fn from(result: ggen_marketplace_v2::models::SearchResult) -> Self {
//             Self {
//                 name: result.package.metadata.name,
//                 version: result.package.metadata.version.to_string(),
//                 description: result.package.metadata.description,
//                 author: Some(result.package.metadata.author),
//                 downloads: result.package.usage_stats.downloads as u32,
//                 stars: result.package.usage_stats.stars,
//                 relevance: result.relevance,
//             }
//         }
//     }
//
//     /// Convert v2 Package → UnifiedSearchResult
//     impl From<ggen_marketplace_v2::models::Package> for UnifiedSearchResult {
//         fn from(pkg: ggen_marketplace_v2::models::Package) -> Self {
//             Self {
//                 name: pkg.metadata.name,
//                 version: pkg.metadata.version.to_string(),
//                 description: pkg.metadata.description,
//                 author: Some(pkg.metadata.author),
//                 downloads: pkg.usage_stats.downloads as u32,
//                 stars: pkg.usage_stats.stars,
//                 relevance: 0.8, // Default relevance when converted from Package
//             }
//         }
//     }
// }

/// Execute search using the active backend
///
/// This is the main adapter function that routes to the correct backend.
///
/// # Feature Flag Routing
/// Execute unified search (v2 only)
///
/// NOTE: This function is legacy code and not used by the CLI.
/// The CLI uses ggen-marketplace-v2 directly via `crates/ggen-cli/src/cmds/marketplace.rs`.
///
/// This adapter was part of the migration from v1 to v2, but since v1 has been
/// removed and the CLI bypasses this layer, this function is kept for reference only.
pub async fn execute_unified_search(_query: UnifiedSearchQuery) -> Result<Vec<UnifiedSearchResult>> {
    // CLI uses ggen-marketplace-v2 directly, not this adapter
    Err(ggen_utils::error::Error::new(
        "This adapter is not used. CLI commands use ggen-marketplace-v2 directly.",
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unified_search_query_builder() {
        let query = UnifiedSearchQuery::new("rust web")
            .with_category("web-framework")
            .with_author("test-author")
            .with_limit(20)
            .with_fuzzy(true)
            .with_min_quality(80);

        assert_eq!(query.query, "rust web");
        assert_eq!(query.category, Some("web-framework".to_string()));
        assert_eq!(query.author, Some("test-author".to_string()));
        assert_eq!(query.limit, 20);
        assert!(query.fuzzy);
        assert_eq!(query.min_quality, Some(80));
    }

    // NOTE: v1 feature removed - v2 only
    #[test]
    fn test_v1_query_conversion() {
        let unified = UnifiedSearchQuery::new("test query")
            .with_category("test-cat")
            .with_limit(5);

        let v1_input: crate::marketplace::SearchInput = unified.into();

        assert_eq!(v1_input.query, "test query");
        assert_eq!(v1_input.category, Some("test-cat".to_string()));
        assert_eq!(v1_input.limit, 5);
    }

    // NOTE: v1 feature removed - v2 only
    #[test]
    fn test_v1_result_conversion() {
        let v1_result = crate::marketplace::SearchResult {
            id: "test-id".to_string(),
            name: "Test Package".to_string(),
            version: "1.0.0".to_string(),
            description: "A test package".to_string(),
            author: Some("Test Author".to_string()),
            category: Some("test".to_string()),
            tags: vec![],
            stars: 10,
            downloads: 100,
            is_8020_certified: false,
            sector: None,
            dark_matter_reduction_target: None,
        };

        let unified: UnifiedSearchResult = v1_result.into();

        assert_eq!(unified.name, "Test Package");
        assert_eq!(unified.version, "1.0.0");
        assert_eq!(unified.description, "A test package");
        assert_eq!(unified.author, Some("Test Author".to_string()));
        assert_eq!(unified.downloads, 100);
        assert_eq!(unified.stars, 10);
    }
}
