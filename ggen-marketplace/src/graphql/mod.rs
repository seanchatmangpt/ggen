//! GraphQL API layer for ggen-marketplace
//!
//! This module provides a GraphQL interface over the existing Registry,
//! PackageStore, and SearchEngine traits, enabling flexible querying
//! and mutations for package management.

use async_graphql::{Context, EmptySubscription, Object, Result as GqlResult, Schema, SimpleObject, InputObject};
use std::sync::Arc;

use crate::error::{MarketplaceError, Result};
use crate::models::{
    Category, ContentId, HashAlgorithm, Package, PackageId, PackageMetadata, PackageStats, Query, Version,
};
use crate::traits::{PackageStore, Registry};

mod types;
pub use types::*;

/// GraphQL Query Root
pub struct QueryRoot;

#[Object]
impl QueryRoot {
    /// Search for packages matching a query string
    async fn search(&self, ctx: &Context<'_>, query: String) -> GqlResult<Vec<PackageGQL>> {
        let registry = ctx.data::<Arc<dyn Registry>>()?;

        let q = Query {
            text: query,
            categories: vec![],
            tags: vec![],
            limit: Some(50),
            offset: 0,
        };

        let packages = registry
            .search(&q)
            .await
            .map_err(|e| format!("Search failed: {}", e))?;

        Ok(packages.into_iter().map(PackageGQL::from).collect())
    }

    /// Get a specific package by namespace and name (latest version)
    async fn package(
        &self,
        ctx: &Context<'_>,
        namespace: String,
        name: String,
    ) -> GqlResult<PackageGQL> {
        let registry = ctx.data::<Arc<dyn Registry>>()?;
        let id = PackageId::new(namespace, name);

        let package = registry
            .get_package(&id)
            .await
            .map_err(|e| format!("Package not found: {}", e))?;

        Ok(PackageGQL::from(package))
    }

    /// List all versions of a package
    async fn list_versions(
        &self,
        ctx: &Context<'_>,
        namespace: String,
        name: String,
    ) -> GqlResult<Vec<PackageGQL>> {
        let registry = ctx.data::<Arc<dyn Registry>>()?;
        let id = PackageId::new(namespace, name);

        let versions = registry
            .list_versions(&id)
            .await
            .map_err(|e| format!("Failed to list versions: {}", e))?;

        Ok(versions.into_iter().map(PackageGQL::from).collect())
    }

    /// Get a specific version of a package
    async fn package_version(
        &self,
        ctx: &Context<'_>,
        namespace: String,
        name: String,
        version: String,
    ) -> GqlResult<PackageGQL> {
        let registry = ctx.data::<Arc<dyn Registry>>()?;
        let id = PackageId::new(namespace, name);

        let package = registry
            .get_package_version(&id, &version)
            .await
            .map_err(|e| format!("Package version not found: {}", e))?;

        Ok(PackageGQL::from(package))
    }

    /// Check if a package exists
    async fn package_exists(
        &self,
        ctx: &Context<'_>,
        namespace: String,
        name: String,
    ) -> GqlResult<bool> {
        let registry = ctx.data::<Arc<dyn Registry>>()?;
        let id = PackageId::new(namespace, name);

        registry
            .exists(&id)
            .await
            .map_err(|e| format!("Failed to check existence: {}", e).into())
    }
}

/// GraphQL Mutation Root
pub struct MutationRoot;

#[Object]
impl MutationRoot {
    /// Publish a new package or version
    async fn publish(&self, ctx: &Context<'_>, input: PublishInput) -> GqlResult<PackageGQL> {
        let registry = ctx.data::<Arc<dyn Registry>>()?;
        let store = ctx.data::<Arc<dyn PackageStore>>()?;

        // Decode base64 content
        let content = base64::decode(&input.content)
            .map_err(|e| format!("Invalid base64 content: {}", e))?;

        // Store content and get content ID
        let content_id = store
            .store(&content)
            .await
            .map_err(|e| format!("Failed to store content: {}", e))?;

        // Build package
        let package_id = PackageId::new(&input.namespace, &input.name);
        let version = Version::parse(&input.version)
            .map_err(|e| format!("Invalid version: {}", e))?;

        let mut metadata = PackageMetadata::default();
        metadata.title = input.title;
        metadata.description = input.description;
        metadata.license = input.license;
        metadata.tags = input.tags.unwrap_or_default();

        if let Some(categories) = input.categories {
            metadata.categories = categories
                .into_iter()
                .map(|c| Category::try_from(c.as_str()).unwrap_or(Category::Other))
                .collect();
        }

        let package = Package {
            id: package_id,
            version,
            metadata,
            content_id,
            dependencies: vec![],
            stats: PackageStats::default(),
            created_at: chrono::Utc::now(),
            updated_at: chrono::Utc::now(),
        };

        // Publish to registry
        registry
            .publish(package.clone())
            .await
            .map_err(|e| format!("Failed to publish: {}", e))?;

        Ok(PackageGQL::from(package))
    }

    /// Delete a specific package version
    async fn delete(
        &self,
        ctx: &Context<'_>,
        namespace: String,
        name: String,
        version: String,
    ) -> GqlResult<bool> {
        let registry = ctx.data::<Arc<dyn Registry>>()?;
        let id = PackageId::new(namespace, name);

        registry
            .delete(&id, &version)
            .await
            .map_err(|e| format!("Failed to delete: {}", e))?;

        Ok(true)
    }
}

/// GraphQL Schema type alias
pub type MarketplaceSchema = Schema<QueryRoot, MutationRoot, EmptySubscription>;

/// Create a new GraphQL schema with the given registry and store
pub fn create_schema(
    registry: Arc<dyn Registry>,
    store: Arc<dyn PackageStore>,
) -> MarketplaceSchema {
    Schema::build(QueryRoot, MutationRoot, EmptySubscription)
        .data(registry)
        .data(store)
        .finish()
}

#[cfg(test)]
mod tests {
    use super::*;
    use async_trait::async_trait;

    struct MockRegistry;

    #[async_trait]
    impl Registry for MockRegistry {
        async fn search(&self, _query: &Query) -> Result<Vec<Package>> {
            Ok(vec![])
        }

        async fn get_package(&self, _id: &PackageId) -> Result<Package> {
            Err(MarketplaceError::package_not_found("test", "mock"))
        }

        async fn get_package_version(&self, _id: &PackageId, _version: &str) -> Result<Package> {
            Err(MarketplaceError::package_not_found("test", "mock"))
        }

        async fn list_versions(&self, _id: &PackageId) -> Result<Vec<Package>> {
            Ok(vec![])
        }

        async fn publish(&self, _package: Package) -> Result<()> {
            Ok(())
        }

        async fn delete(&self, _id: &PackageId, _version: &str) -> Result<()> {
            Ok(())
        }

        async fn exists(&self, _id: &PackageId) -> Result<bool> {
            Ok(false)
        }

        async fn metadata(&self) -> Result<crate::models::RegistryMetadata> {
            Ok(crate::models::RegistryMetadata {
                name: "Test".to_string(),
                url: "http://test".to_string(),
                description: None,
                capabilities: vec![],
            })
        }
    }

    struct MockStore;

    #[async_trait]
    impl PackageStore for MockStore {
        async fn store(&self, _content: &[u8]) -> Result<ContentId> {
            Ok(ContentId::new("test", HashAlgorithm::Sha256))
        }

        async fn retrieve(&self, _id: &ContentId) -> Result<Vec<u8>> {
            Ok(vec![])
        }

        async fn exists(&self, _id: &ContentId) -> Result<bool> {
            Ok(true)
        }

        async fn delete(&self, _id: &ContentId) -> Result<()> {
            Ok(())
        }

        async fn metadata(&self, _id: &ContentId) -> Result<crate::traits::ContentMetadata> {
            Ok(crate::traits::ContentMetadata {
                size: 0,
                content_type: None,
                created_at: chrono::Utc::now(),
            })
        }

        async fn store_stream(
            &self,
            _stream: Box<dyn tokio::io::AsyncRead + Send + Unpin>,
        ) -> Result<ContentId> {
            Ok(ContentId::new("stream", HashAlgorithm::Sha256))
        }

        async fn retrieve_stream(
            &self,
            _id: &ContentId,
        ) -> Result<Box<dyn tokio::io::AsyncRead + Send + Unpin>> {
            unimplemented!()
        }
    }

    #[tokio::test]
    async fn test_create_schema() {
        let registry = Arc::new(MockRegistry) as Arc<dyn Registry>;
        let store = Arc::new(MockStore) as Arc<dyn PackageStore>;

        let schema = create_schema(registry, store);
        assert!(schema.sdl().contains("type Query"));
        assert!(schema.sdl().contains("type Mutation"));
    }
}
