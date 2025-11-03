//! GraphQL type definitions for the marketplace API

use crate::models::{Category, Package};
use async_graphql::{InputObject, SimpleObject};

/// GraphQL representation of a Package
#[derive(SimpleObject, Clone)]
pub struct PackageGQL {
    /// Full package ID (namespace/name)
    pub id: String,
    /// Package namespace
    pub namespace: String,
    /// Package name
    pub name: String,
    /// Package version
    pub version: String,
    /// Package title
    pub title: String,
    /// Short description
    pub description: String,
    /// License identifier (SPDX)
    pub license: String,
    /// Associated tags
    pub tags: Vec<String>,
    /// Categories
    pub categories: Vec<String>,
    /// Homepage URL
    pub homepage: Option<String>,
    /// Repository URL
    pub repository: Option<String>,
    /// Download count
    pub downloads: i64,
    /// Created timestamp (ISO 8601)
    pub created_at: String,
    /// Updated timestamp (ISO 8601)
    pub updated_at: String,
}

impl From<Package> for PackageGQL {
    fn from(package: Package) -> Self {
        Self {
            id: package.id.to_string(),
            namespace: package.id.namespace.clone(),
            name: package.id.name.clone(),
            version: package.version.to_string(),
            title: package.metadata.title,
            description: package.metadata.description,
            license: package.metadata.license,
            tags: package.metadata.tags,
            categories: package
                .metadata
                .categories
                .iter()
                .map(|c| format!("{:?}", c))
                .collect(),
            homepage: package.metadata.homepage,
            repository: package.metadata.repository,
            downloads: package.stats.downloads as i64,
            created_at: package.created_at.to_rfc3339(),
            updated_at: package.updated_at.to_rfc3339(),
        }
    }
}

/// Input for publishing a new package
#[derive(InputObject)]
pub struct PublishInput {
    /// Package namespace
    pub namespace: String,
    /// Package name
    pub name: String,
    /// Semantic version (e.g., "1.0.0")
    pub version: String,
    /// Display title
    pub title: String,
    /// Short description
    pub description: String,
    /// License (SPDX identifier)
    pub license: String,
    /// Optional tags
    pub tags: Option<Vec<String>>,
    /// Optional categories
    pub categories: Option<Vec<String>>,
    /// Package content (base64 encoded)
    pub content: String,
    /// Optional homepage URL
    pub homepage: Option<String>,
    /// Optional repository URL
    pub repository: Option<String>,
}

/// Search result with metadata
#[derive(SimpleObject, Clone)]
pub struct SearchResultGQL {
    /// Matching packages
    pub packages: Vec<PackageGQL>,
    /// Total result count
    pub total_count: i32,
    /// Search query used
    pub query: String,
}

/// Package statistics
#[derive(SimpleObject, Clone)]
pub struct PackageStatsGQL {
    /// Download count
    pub downloads: i64,
    /// Star/like count
    pub stars: i64,
    /// Dependent package count
    pub dependents: i32,
}

/// Dependency information
#[derive(SimpleObject, Clone)]
pub struct DependencyGQL {
    /// Package namespace
    pub namespace: String,
    /// Package name
    pub name: String,
    /// Version requirement
    pub version_requirement: String,
    /// Whether it's optional
    pub optional: bool,
}

/// Category enumeration for GraphQL
#[derive(async_graphql::Enum, Copy, Clone, Eq, PartialEq)]
pub enum CategoryGQL {
    Development,
    WebFramework,
    CLI,
    Database,
    Testing,
    Networking,
    Security,
    DataScience,
}

impl From<Category> for CategoryGQL {
    fn from(cat: Category) -> Self {
        match cat {
            Category::Development => CategoryGQL::Development,
            Category::WebFramework => CategoryGQL::WebFramework,
            Category::CLI => CategoryGQL::CLI,
            Category::Database => CategoryGQL::Database,
            Category::Testing => CategoryGQL::Testing,
            Category::Networking => CategoryGQL::Networking,
            Category::Security => CategoryGQL::Security,
            Category::DataScience => CategoryGQL::DataScience,
        }
    }
}

impl From<CategoryGQL> for Category {
    fn from(cat: CategoryGQL) -> Self {
        match cat {
            CategoryGQL::Development => Category::Development,
            CategoryGQL::WebFramework => Category::WebFramework,
            CategoryGQL::CLI => Category::CLI,
            CategoryGQL::Database => Category::Database,
            CategoryGQL::Testing => Category::Testing,
            CategoryGQL::Networking => Category::Networking,
            CategoryGQL::Security => Category::Security,
            CategoryGQL::DataScience => Category::DataScience,
        }
    }
}
