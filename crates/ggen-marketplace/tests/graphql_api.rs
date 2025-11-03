use async_graphql::{EmptySubscription, Schema, Value};
use ggen_marketplace::error::Result;
use ggen_marketplace::models::{
    Category, ContentId, HashAlgorithm, Package, PackageId, PackageMetadata, PackageStats, Query,
    Version,
};
use ggen_marketplace::traits::{PackageStore, Registry};
use std::sync::Arc;
use tokio;

// Mock implementations for testing
use async_trait::async_trait;

/// Mock Registry for testing
struct MockRegistry {
    packages: Vec<Package>,
}

impl MockRegistry {
    fn new(packages: Vec<Package>) -> Self {
        Self { packages }
    }
}

#[async_trait]
impl Registry for MockRegistry {
    async fn search(&self, query: &Query) -> Result<Vec<Package>> {
        // Simple mock: return all packages that match the query string
        let query_text = query.text.to_lowercase();
        Ok(self
            .packages
            .iter()
            .filter(|p| {
                p.metadata.title.to_lowercase().contains(&query_text)
                    || p.metadata.description.to_lowercase().contains(&query_text)
            })
            .cloned()
            .collect())
    }

    async fn get_package(&self, id: &PackageId) -> Result<Package> {
        self.packages
            .iter()
            .find(|p| &p.id == id)
            .cloned()
            .ok_or_else(|| {
                ggen_marketplace::error::MarketplaceError::package_not_found(
                    id.to_string(),
                    "mock registry",
                )
            })
    }

    async fn get_package_version(&self, id: &PackageId, version: &str) -> Result<Package> {
        self.packages
            .iter()
            .find(|p| &p.id == id && p.version.to_string() == version)
            .cloned()
            .ok_or_else(|| {
                ggen_marketplace::error::MarketplaceError::package_not_found(
                    format!("{}@{}", id, version),
                    "mock registry",
                )
            })
    }

    async fn list_versions(&self, id: &PackageId) -> Result<Vec<Package>> {
        Ok(self
            .packages
            .iter()
            .filter(|p| &p.id == id)
            .cloned()
            .collect())
    }

    async fn publish(&self, _package: Package) -> Result<()> {
        Ok(())
    }

    async fn delete(&self, _id: &PackageId, _version: &str) -> Result<()> {
        Ok(())
    }

    async fn exists(&self, id: &PackageId) -> Result<bool> {
        Ok(self.packages.iter().any(|p| &p.id == id))
    }

    async fn metadata(&self) -> Result<ggen_marketplace::models::RegistryMetadata> {
        Ok(ggen_marketplace::models::RegistryMetadata {
            name: "Mock Registry".to_string(),
            url: "http://localhost".to_string(),
            description: Some("Test registry".to_string()),
            capabilities: vec![],
        })
    }
}

/// Mock PackageStore for testing
struct MockPackageStore;

#[async_trait]
impl PackageStore for MockPackageStore {
    async fn store(&self, _content: &[u8]) -> Result<ContentId> {
        Ok(ContentId::new("abc123", HashAlgorithm::Sha256))
    }

    async fn retrieve(&self, _id: &ContentId) -> Result<Vec<u8>> {
        Ok(vec![1, 2, 3, 4])
    }

    async fn exists(&self, _id: &ContentId) -> Result<bool> {
        Ok(true)
    }

    async fn delete(&self, _id: &ContentId) -> Result<()> {
        Ok(())
    }

    async fn metadata(&self, _id: &ContentId) -> Result<ggen_marketplace::traits::ContentMetadata> {
        Ok(ggen_marketplace::traits::ContentMetadata {
            size: 1024,
            content_type: Some("application/octet-stream".to_string()),
            created_at: chrono::Utc::now(),
        })
    }

    async fn store_stream(
        &self, _stream: Box<dyn tokio::io::AsyncRead + Send + Unpin>,
    ) -> Result<ContentId> {
        Ok(ContentId::new("stream123", HashAlgorithm::Sha256))
    }

    async fn retrieve_stream(
        &self, _id: &ContentId,
    ) -> Result<Box<dyn tokio::io::AsyncRead + Send + Unpin>> {
        unimplemented!("Stream retrieval not needed for GraphQL tests")
    }
}

/// Helper function to create test package
fn create_test_package(namespace: &str, name: &str, version: &str) -> Package {
    let id = PackageId::new(namespace, name);
    let version = Version::parse(version).unwrap();
    let content_id = ContentId::new("test_hash", HashAlgorithm::Sha256);

    Package {
        id,
        version,
        metadata: PackageMetadata {
            title: format!("{} Package", name),
            description: format!("Test package for {}", name),
            long_description: None,
            categories: vec![Category::Development],
            tags: vec!["test".to_string()],
            license: "MIT".to_string(),
            authors: vec![],
            homepage: None,
            repository: None,
            documentation: None,
            readme: None,
            changelog: None,
            custom_fields: std::collections::HashMap::new(),
        },
        content_id,
        dependencies: vec![],
        stats: PackageStats::default(),
        created_at: chrono::Utc::now(),
        updated_at: chrono::Utc::now(),
    }
}

#[tokio::test]
async fn test_graphql_search_query() {
    use ggen_marketplace::graphql::{MutationRoot, QueryRoot};

    // Arrange: Create mock registry with test packages
    let packages = vec![
        create_test_package("rust", "web-framework", "1.0.0"),
        create_test_package("rust", "cli-tool", "2.1.0"),
        create_test_package("python", "data-science", "0.5.0"),
    ];
    let registry = Arc::new(MockRegistry::new(packages));
    let store = Arc::new(MockPackageStore);

    // Build GraphQL schema
    let schema = Schema::build(QueryRoot, MutationRoot, EmptySubscription)
        .data(registry as Arc<dyn Registry>)
        .data(store as Arc<dyn PackageStore>)
        .finish();

    // Act: Execute search query
    let query = r#"
        query {
            search(query: "web") {
                id
                name
                version
                title
                description
            }
        }
    "#;

    let result = schema.execute(query).await;

    // Assert: Check that results are returned
    assert!(
        result.errors.is_empty(),
        "GraphQL query should not have errors"
    );

    let data = result.data.into_json().unwrap();
    let search_results = data.get("search").and_then(|v| v.as_array());
    assert!(search_results.is_some(), "Search results should be present");

    let results = search_results.unwrap();
    assert!(!results.is_empty(), "Should find at least one package");

    // Verify the package structure
    let first_package = &results[0];
    assert!(first_package.get("id").is_some());
    assert!(first_package.get("name").is_some());
    assert!(first_package.get("version").is_some());
}

#[tokio::test]
async fn test_graphql_package_query() {
    use ggen_marketplace::graphql::{MutationRoot, QueryRoot};

    // Arrange
    let packages = vec![create_test_package("rust", "awesome-crate", "1.5.0")];
    let registry = Arc::new(MockRegistry::new(packages));
    let store = Arc::new(MockPackageStore);

    let schema = Schema::build(QueryRoot, MutationRoot, EmptySubscription)
        .data(registry as Arc<dyn Registry>)
        .data(store as Arc<dyn PackageStore>)
        .finish();

    // Act: Query specific package
    let query = r#"
        query {
            package(namespace: "rust", name: "awesome-crate") {
                id
                name
                namespace
                version
                title
                description
                license
                tags
            }
        }
    "#;

    let result = schema.execute(query).await;

    // Assert
    assert!(result.errors.is_empty(), "Should execute without errors");

    let data = result.data.into_json().unwrap();
    let package = data.get("package");
    assert!(package.is_some(), "Package should be found");

    let pkg = package.unwrap();
    assert_eq!(
        pkg.get("name").and_then(|v| v.as_str()),
        Some("awesome-crate")
    );
    assert_eq!(pkg.get("namespace").and_then(|v| v.as_str()), Some("rust"));
    assert_eq!(pkg.get("license").and_then(|v| v.as_str()), Some("MIT"));
}

#[tokio::test]
async fn test_graphql_list_versions() {
    use ggen_marketplace::graphql::{MutationRoot, QueryRoot};

    // Arrange: Multiple versions of same package
    let packages = vec![
        create_test_package("rust", "my-lib", "1.0.0"),
        create_test_package("rust", "my-lib", "1.1.0"),
        create_test_package("rust", "my-lib", "2.0.0"),
    ];
    let registry = Arc::new(MockRegistry::new(packages));
    let store = Arc::new(MockPackageStore);

    let schema = Schema::build(QueryRoot, MutationRoot, EmptySubscription)
        .data(registry as Arc<dyn Registry>)
        .data(store as Arc<dyn PackageStore>)
        .finish();

    // Act
    let query = r#"
        query {
            listVersions(namespace: "rust", name: "my-lib") {
                version
            }
        }
    "#;

    let result = schema.execute(query).await;

    // Assert
    assert!(result.errors.is_empty());

    let data = result.data.into_json().unwrap();
    let versions = data.get("listVersions").and_then(|v| v.as_array());
    assert!(versions.is_some());
    assert_eq!(versions.unwrap().len(), 3, "Should return all 3 versions");
}

#[tokio::test]
async fn test_graphql_publish_mutation() {
    use ggen_marketplace::graphql::{MutationRoot, QueryRoot};

    // Arrange
    let registry = Arc::new(MockRegistry::new(vec![]));
    let store = Arc::new(MockPackageStore);

    let schema = Schema::build(QueryRoot, MutationRoot, EmptySubscription)
        .data(registry as Arc<dyn Registry>)
        .data(store as Arc<dyn PackageStore>)
        .finish();

    // Act: Publish new package
    let mutation = r#"
        mutation {
            publish(input: {
                namespace: "rust"
                name: "new-package"
                version: "1.0.0"
                title: "New Package"
                description: "A brand new package"
                license: "MIT"
                tags: ["new", "test"]
                content: "cGFja2FnZSBjb250ZW50"
            }) {
                id
                name
                version
            }
        }
    "#;

    let result = schema.execute(mutation).await;

    // Assert
    assert!(result.errors.is_empty(), "Mutation should succeed");

    let data = result.data.into_json().unwrap();
    let published = data.get("publish");
    assert!(published.is_some(), "Should return published package");
}

#[tokio::test]
async fn test_graphql_error_handling() {
    use ggen_marketplace::graphql::{MutationRoot, QueryRoot};

    // Arrange: Empty registry
    let registry = Arc::new(MockRegistry::new(vec![]));
    let store = Arc::new(MockPackageStore);

    let schema = Schema::build(QueryRoot, MutationRoot, EmptySubscription)
        .data(registry as Arc<dyn Registry>)
        .data(store as Arc<dyn PackageStore>)
        .finish();

    // Act: Query non-existent package
    let query = r#"
        query {
            package(namespace: "nonexistent", name: "package") {
                id
            }
        }
    "#;

    let result = schema.execute(query).await;

    // Assert: Should return error
    assert!(
        !result.errors.is_empty(),
        "Should have errors for non-existent package"
    );

    let error = &result.errors[0];
    assert!(error.message.contains("not found") || error.message.contains("PackageNotFound"));
}

#[tokio::test]
async fn test_graphql_schema_validation() {
    use ggen_marketplace::graphql::{MutationRoot, QueryRoot};

    // Arrange
    let registry = Arc::new(MockRegistry::new(vec![]));
    let store = Arc::new(MockPackageStore);

    let schema = Schema::build(QueryRoot, MutationRoot, EmptySubscription)
        .data(registry as Arc<dyn Registry>)
        .data(store as Arc<dyn PackageStore>)
        .finish();

    // Act: Execute invalid query (missing required field)
    let query = r#"
        query {
            package(name: "test") {
                id
            }
        }
    "#;

    let result = schema.execute(query).await;

    // Assert: Should return validation error
    assert!(!result.errors.is_empty(), "Should have validation errors");
}

#[tokio::test]
async fn test_graphql_delete_mutation() {
    use ggen_marketplace::graphql::{MutationRoot, QueryRoot};

    // Arrange
    let packages = vec![create_test_package("rust", "to-delete", "1.0.0")];
    let registry = Arc::new(MockRegistry::new(packages));
    let store = Arc::new(MockPackageStore);

    let schema = Schema::build(QueryRoot, MutationRoot, EmptySubscription)
        .data(registry as Arc<dyn Registry>)
        .data(store as Arc<dyn PackageStore>)
        .finish();

    // Act: Delete package
    let mutation = r#"
        mutation {
            delete(namespace: "rust", name: "to-delete", version: "1.0.0")
        }
    "#;

    let result = schema.execute(mutation).await;

    // Assert
    assert!(result.errors.is_empty(), "Delete mutation should succeed");

    let data = result.data.into_json().unwrap();
    let deleted = data.get("delete").and_then(|v| v.as_bool());
    assert_eq!(
        deleted,
        Some(true),
        "Should return true on successful delete"
    );
}
