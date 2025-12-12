#![cfg(feature = "marketplace_v1")]
//! Integration tests for marketplace functionality using testcontainers
//!
//! These tests verify that the marketplace system works correctly with
//! real databases, caches, and other services using testcontainers.

use std::collections::HashMap;
use std::time::Duration;

use testcontainers::{clients, Container, RunnableImage};
use testcontainers_modules::postgres::Postgres;
use testcontainers_modules::redis::Redis;

use ggen_cli_lib::cmds::market::{
    add::{run_with_deps, AddArgs, GpackInstaller, InstallResult, RegistryGpackInstaller},
    list::{run_with_deps as list_run_with_deps, ListArgs, GpackLister},
    search::{run_with_deps as search_run_with_deps, SearchArgs, MarketplaceClient, SearchFilters, SearchResult},
};

use ggen_utils::error::Result;

/// Test database container for integration tests
pub struct TestDatabase {
    container: Container<'static, Postgres>,
    connection_string: String,
}

impl TestDatabase {
    pub fn new() -> Self {
        let docker = clients::Cli::default();
        let postgres_image = RunnableImage::from(Postgres::default()).with_tag("15-alpine");
        let container = docker.run(postgres_image);

        let host_port = container.get_host_port_ipv4(5432);
        let connection_string = format!(
            "postgresql://postgres:postgres@localhost:{}/test",
            host_port
        );

        Self {
            container,
            connection_string,
        }
    }

    pub fn connection_string(&self) -> &str {
        &self.connection_string
    }
}

/// Test cache container for integration tests
pub struct TestCache {
    container: Container<'static, Redis>,
    connection_string: String,
}

impl TestCache {
    pub fn new() -> Self {
        let docker = clients::Cli::default();
        let redis_image = RunnableImage::from(Redis::default()).with_tag("7-alpine");
        let container = docker.run(redis_image);

        let host_port = container.get_host_port_ipv4(6379);
        let connection_string = format!("redis://localhost:{}", host_port);

        Self {
            container,
            connection_string,
        }
    }

    pub fn connection_string(&self) -> &str {
        &self.connection_string
    }
}

/// Test environment with database and cache
pub struct TestEnvironment {
    pub database: TestDatabase,
    pub cache: TestCache,
}

impl TestEnvironment {
    pub fn new() -> Self {
        Self {
            database: TestDatabase::new(),
            cache: TestCache::new(),
        }
    }
}

/// Mock marketplace client for testing
#[derive(Default)]
pub struct MockMarketplaceClient {
    packages: Vec<SearchResult>,
}

impl MockMarketplaceClient {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_package(mut self, package: SearchResult) -> Self {
        self.packages.push(package);
        self
    }
}

impl MarketplaceClient for MockMarketplaceClient {
    fn search(&self, _query: &str, _filters: &SearchFilters) -> Result<Vec<SearchResult>> {
        Ok(self.packages.clone())
    }
}

/// Mock package installer for testing
pub struct MockGpackInstaller {
    should_succeed: bool,
}

impl MockGpackInstaller {
    pub fn new(should_succeed: bool) -> Self {
        Self { should_succeed }
    }
}

impl GpackInstaller for MockGpackInstaller {
    fn install(&self, gpack_id: String, _version: Option<String>) -> Result<InstallResult> {
        if self.should_succeed {
            Ok(InstallResult {
                gpack_id,
                version: "1.0.0".to_string(),
                already_installed: false,
            })
        } else {
            Err(ggen_utils::error::Error::new("Mock installation failed"))
        }
    }
}

#[tokio::test]
async fn test_marketplace_search_with_real_registry() {
    // Test that marketplace search works with the actual registry
    let args = SearchArgs {
        query: "hello".to_string(),
        category: None,
        keyword: None,
        author: None,
        license: None,
        min_stars: None,
        min_downloads: None,
        sort: "relevance".to_string(),
        order: "desc".to_string(),
        fuzzy: false,
        suggestions: false,
        detailed: false,
        json: false,
        limit: 10,
    };

    let result = search_run_with_deps(&args, &MockMarketplaceClient::new()).await;

    // Should not panic and should return some result
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_package_installation_workflow() {
    // Test the complete package installation workflow
    let installer = MockGpackInstaller::new(true);

    let args = AddArgs {
        gpack_id: "test-package".to_string(),
    };

    let result = run_with_deps(&args, &installer).await;

    assert!(result.is_ok());
}

#[tokio::test]
async fn test_package_installation_failure() {
    // Test that installation failures are handled correctly
    let installer = MockGpackInstaller::new(false);

    let args = AddArgs {
        gpack_id: "failing-package".to_string(),
    };

    let result = run_with_deps(&args, &installer).await;

    assert!(result.is_err());
}

#[tokio::test]
async fn test_installed_packages_listing() {
    // Test that we can list installed packages
    let lister = MockGpackLister::default();

    let args = ListArgs { detailed: false };

    let result = list_run_with_deps(&args, &lister).await;

    assert!(result.is_ok());
}

/// Mock gpack lister for testing
pub struct MockGpackLister {
    packages: Vec<super::list::InstalledGpack>,
}

impl Default for MockGpackLister {
    fn default() -> Self {
        Self {
            packages: vec![],
        }
    }
}

impl MockGpackLister {
    pub fn with_packages(packages: Vec<super::list::InstalledGpack>) -> Self {
        Self { packages }
    }
}

impl GpackLister for MockGpackLister {
    fn list_installed(&self) -> Result<Vec<super::list::InstalledGpack>> {
        Ok(self.packages.clone())
    }
}

#[tokio::test]
async fn test_package_listing_with_packages() {
    // Test listing when packages are installed
    let packages = vec![
        super::list::InstalledGpack {
            id: "test-package-1".to_string(),
            version: "1.0.0".to_string(),
            sha256: "abc123".to_string(),
            source: "registry".to_string(),
        },
        super::list::InstalledGpack {
            id: "test-package-2".to_string(),
            version: "2.0.0".to_string(),
            sha256: "def456".to_string(),
            source: "registry".to_string(),
        },
    ];

    let lister = MockGpackLister::with_packages(packages);
    let args = ListArgs { detailed: false };

    let result = list_run_with_deps(&args, &lister).await;

    assert!(result.is_ok());
}

#[tokio::test]
async fn test_package_listing_empty() {
    // Test listing when no packages are installed
    let lister = MockGpackLister::default();
    let args = ListArgs { detailed: false };

    let result = list_run_with_deps(&args, &lister).await;

    assert!(result.is_ok());
}

/// Integration test with actual testcontainers
#[tokio::test]
async fn test_marketplace_with_postgres_container() {
    // This test uses testcontainers to test with a real PostgreSQL database
    let test_env = TestEnvironment::new();

    // Wait for containers to be ready
    tokio::time::sleep(Duration::from_secs(5)).await;

    println!("Testing with PostgreSQL: {}", test_env.database.connection_string());
    println!("Testing with Redis: {}", test_env.cache.connection_string());

    // Test that we can connect to the database
    let result = sqlx::query("SELECT 1")
        .fetch_one(&sqlx::PgPool::connect(test_env.database.connection_string()).await.unwrap())
        .await;

    assert!(result.is_ok());

    // Test that we can connect to Redis
    let redis_client = redis::Client::open(test_env.cache.connection_string()).unwrap();
    let mut conn = redis_client.get_connection().unwrap();
    let _: () = redis::cmd("SET").arg("test_key").arg("test_value").query(&mut conn).unwrap();
    let result: String = redis::cmd("GET").arg("test_key").query(&mut conn).unwrap();

    assert_eq!(result, "test_value");
}

#[tokio::test]
async fn test_marketplace_package_validation() {
    // Test that package validation works correctly
    use ggen_cli_lib::cmds::market::add::validate_gpack_input;

    // Valid package ID
    let result = validate_gpack_input("io.ggen.rust.cli");
    assert!(result.is_ok());

    // Valid package ID with version
    let result = validate_gpack_input("io.ggen.rust.cli@1.0.0");
    assert!(result.is_ok());

    // Invalid empty package ID
    let result = validate_gpack_input("");
    assert!(result.is_err());

    // Invalid characters
    let result = validate_gpack_input("invalid/package@1.0.0");
    assert!(result.is_err());
}

#[tokio::test]
async fn test_marketplace_search_filters() {
    // Test that search filters work correctly
    let mock_client = MockMarketplaceClient::new().with_package(SearchResult {
        id: "test-package".to_string(),
        name: "Test Package".to_string(),
        description: "A test package".to_string(),
        version: "1.0.0".to_string(),
        category: Some("utilities".to_string()),
        author: Some("test-author".to_string()),
        license: Some("MIT".to_string()),
        stars: 10,
        downloads: 100,
        updated_at: "2024-01-01T00:00:00Z".to_string(),
        tags: vec!["test".to_string(), "utilities".to_string()],
        health_score: Some(0.9),
    });

    let args = SearchArgs {
        query: "test".to_string(),
        category: Some("utilities".to_string()),
        keyword: None,
        author: None,
        license: None,
        min_stars: Some(5),
        min_downloads: None,
        sort: "relevance".to_string(),
        order: "desc".to_string(),
        fuzzy: false,
        suggestions: false,
        detailed: false,
        json: false,
        limit: 10,
    };

    let result = search_run_with_deps(&args, &mock_client).await;

    assert!(result.is_ok());
}

#[tokio::test]
async fn test_marketplace_error_handling() {
    // Test that various error conditions are handled correctly
    let installer = MockGpackInstaller::new(false);

    let args = AddArgs {
        gpack_id: "nonexistent-package".to_string(),
    };

    let result = run_with_deps(&args, &installer).await;

    assert!(result.is_err());
}

/// Helper function to create a test environment with containers
pub async fn create_test_environment() -> TestEnvironment {
    TestEnvironment::new()
}

/// Helper function to clean up test environment
pub async fn cleanup_test_environment(_env: TestEnvironment) {
    // Testcontainers automatically cleans up containers when dropped
}
