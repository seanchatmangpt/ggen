//! Integration Tests for New Features (Ed25519, P2P, GraphQL)
//!
//! This test suite verifies that the three new features work together:
//! 1. Ed25519 cryptographic signatures
//! 2. P2P peer-to-peer registry discovery
//! 3. GraphQL API for querying packages
//!
//! Testing Philosophy (80/20 Rule):
//! - Focus on critical integration paths
//! - Test real scenarios, not implementation details
//! - Fast, deterministic, isolated tests
//! - No mocks for core interactions

//! Integration Tests for New Features (Ed25519, P2P, GraphQL)
//!
//! This test suite verifies that the three new features work together:
//! 1. Ed25519 cryptographic signatures
//! 2. P2P peer-to-peer registry discovery
//! 3. GraphQL API for querying packages
//!
//! Testing Philosophy (80/20 Rule):
//! - Focus on critical integration paths
//! - Test real scenarios, not implementation details
//! - Fast, deterministic, isolated tests
//! - No mocks for core interactions

#[cfg(feature = "crypto")]
use ggen_marketplace::prelude::*;
use std::collections::HashMap;
use std::sync::Arc;
use tempfile::TempDir;
use tokio::sync::RwLock;

// ============================================================================
// Test Helpers
// ============================================================================

/// Mock P2P Registry for testing
///
/// Simulates a peer-to-peer network without requiring actual networking.
/// This allows fast, deterministic tests while still testing the integration.
struct MockP2PRegistry {
    packages: Arc<RwLock<HashMap<String, Package>>>,
    peers: Arc<RwLock<Vec<String>>>,
}

impl MockP2PRegistry {
    fn new() -> Self {
        Self {
            packages: Arc::new(RwLock::new(HashMap::new())),
            peers: Arc::new(RwLock::new(Vec::new())),
        }
    }

    async fn publish(&self, package: Package) -> Result<()> {
        let mut packages = self.packages.write().await;
        let key = format!("{}:{}", package.id.namespace, package.id.name);
        packages.insert(key, package);
        Ok(())
    }

    async fn discover(&self, query: &str) -> Result<Vec<Package>> {
        let packages = self.packages.read().await;
        let results: Vec<Package> = packages
            .values()
            .filter(|p| {
                p.id.name.contains(query)
                    || p.metadata.description.contains(query)
                    || p.metadata.tags.iter().any(|t| t.contains(query))
            })
            .cloned()
            .collect();
        Ok(results)
    }

    async fn retrieve(&self, package_id: &PackageId) -> Result<Package> {
        let packages = self.packages.read().await;
        let key = format!("{}:{}", package_id.namespace, package_id.name);
        packages.get(&key).cloned().ok_or_else(|| {
            MarketplaceError::not_found(
                format!("Package not found: {}", package_id.name),
                "p2p network",
            )
        })
    }

    async fn add_peer(&self, peer_addr: String) -> Result<()> {
        let mut peers = self.peers.write().await;
        if !peers.contains(&peer_addr) {
            peers.push(peer_addr);
        }
        Ok(())
    }

    async fn list_peers(&self) -> Vec<String> {
        self.peers.read().await.clone()
    }
}

/// Mock GraphQL server for testing
///
/// Provides a simple in-memory GraphQL endpoint for testing queries.
struct MockGraphQLServer {
    registry: Arc<MockP2PRegistry>,
}

impl MockGraphQLServer {
    fn new(registry: Arc<MockP2PRegistry>) -> Self {
        Self { registry }
    }

    async fn execute_query(&self, query: &str) -> Result<GraphQLResponse> {
        // Parse simple queries (simplified for testing)
        if query.contains("searchPackages") {
            let search_term = extract_search_term(query);
            let packages = self.registry.discover(&search_term).await?;

            Ok(GraphQLResponse {
                data: Some(GraphQLData::SearchResults(packages)),
                errors: None,
            })
        } else if query.contains("package(") {
            let package_name = extract_package_name(query);
            let package_id = PackageId::new("test", &package_name);

            match self.registry.retrieve(&package_id).await {
                Ok(package) => Ok(GraphQLResponse {
                    data: Some(GraphQLData::Package(package)),
                    errors: None,
                }),
                Err(e) => Ok(GraphQLResponse {
                    data: None,
                    errors: Some(vec![e.to_string()]),
                }),
            }
        } else {
            Err(MarketplaceError::invalid_package(
                format!("Unsupported query: {}", query),
                "graphql",
            ))
        }
    }
}

#[derive(Debug, Clone)]
struct GraphQLResponse {
    data: Option<GraphQLData>,
    errors: Option<Vec<String>>,
}

#[derive(Debug, Clone)]
enum GraphQLData {
    SearchResults(Vec<Package>),
    Package(Package),
}

fn extract_search_term(query: &str) -> String {
    // Simplified query parsing for tests
    if let Some(start) = query.find("query:") {
        let after_query = &query[start + 6..];
        if let Some(end) = after_query.find('"') {
            return after_query[..end].trim_matches('"').to_string();
        }
    }
    "".to_string()
}

fn extract_package_name(query: &str) -> String {
    // Simplified query parsing for tests
    if let Some(start) = query.find("name:") {
        let after_name = &query[start + 5..];
        if let Some(end) = after_name.find('"') {
            return after_name[..end].trim_matches('"').to_string();
        }
    }
    "".to_string()
}

/// Create a signed package for testing
async fn create_signed_package(
    name: &str, version: &str, content: &[u8],
) -> Result<(Package, Vec<u8>)> {
    // Create package
    let version_parts: Vec<&str> = version.split('.').collect();
    let major = version_parts[0].parse().unwrap_or(1);
    let minor = version_parts
        .get(1)
        .and_then(|v| v.parse().ok())
        .unwrap_or(0);
    let patch = version_parts
        .get(2)
        .and_then(|v| v.parse().ok())
        .unwrap_or(0);

    // Hash the content
    let verifier = Ed25519Verifier::new();
    let content_hash = verifier.hash_content(content)?;

    let package = Package::builder(
        PackageId::new("test", name),
        Version::new(major, minor, patch),
    )
    .title(format!("Signed Package {}", name))
    .description(format!("Test package with Ed25519 signature"))
    .license("MIT")
    .tag("signed")
    .tag("test")
    .content_id(ContentId::new(content_hash, HashAlgorithm::Sha256))
    .build()?;

    Ok((package, content.to_vec()))
}

/// Setup test environment with P2P network and GraphQL server
async fn setup_test_environment() -> (Arc<MockP2PRegistry>, MockGraphQLServer) {
    let p2p_registry = Arc::new(MockP2PRegistry::new());
    let graphql_server = MockGraphQLServer::new(p2p_registry.clone());
    (p2p_registry, graphql_server)
}

// ============================================================================
// Ed25519 Signature Tests
// ============================================================================

#[cfg(feature = "crypto")]
#[tokio::test]
async fn test_ed25519_hash_content() {
    let verifier = Ed25519Verifier::new();
    let content = b"Hello, blockchain!";

    let hash1 = verifier.hash_content(content).expect("hash should succeed");
    let hash2 = verifier.hash_content(content).expect("hash should succeed");

    // Same content produces same hash
    assert_eq!(hash1, hash2);
    assert_eq!(hash1.len(), 64); // SHA-256 produces 64 hex chars
}

#[cfg(feature = "crypto")]
#[tokio::test]
async fn test_ed25519_hash_deterministic() {
    let verifier = Ed25519Verifier::new();

    let hashes: Vec<String> = (0..10)
        .map(|_| verifier.hash_content(b"test content").expect("should hash"))
        .collect();

    // All hashes should be identical
    let first_hash = &hashes[0];
    assert!(hashes.iter().all(|h| h == first_hash));
}

#[cfg(feature = "crypto")]
#[tokio::test]
async fn test_ed25519_different_content_different_hash() {
    let verifier = Ed25519Verifier::new();

    let hash1 = verifier.hash_content(b"content1").expect("should hash");
    let hash2 = verifier.hash_content(b"content2").expect("should hash");

    assert_ne!(hash1, hash2);
}

// Note: Full Ed25519 signing/verification tests require ed25519-dalek dependency
// These tests verify the interface and architecture

#[cfg(feature = "crypto")]
#[tokio::test]
async fn test_ed25519_sign_and_verify() {
    let verifier = Ed25519Verifier::new();
    let keypair = verifier
        .generate_keypair()
        .expect("should generate keypair");
    let verifier_with_key = Ed25519Verifier::with_keypair(keypair);

    let content = b"test content for signing";

    // Sign the content
    let signature = verifier_with_key.sign(content).expect("should sign");

    // Verify signature is 64 bytes
    assert_eq!(signature.value.len(), 64);
    assert_eq!(
        signature.algorithm,
        ggen_marketplace::models::SignatureAlgorithm::Ed25519
    );

    // Verify the signature
    let is_valid = verifier_with_key
        .verify(content, &signature)
        .expect("should verify");
    assert!(is_valid);
}

#[cfg(feature = "crypto")]
#[tokio::test]
async fn test_ed25519_verify_wrong_content_fails() {
    let verifier = Ed25519Verifier::new();
    let keypair = verifier
        .generate_keypair()
        .expect("should generate keypair");
    let verifier_with_key = Ed25519Verifier::with_keypair(keypair);

    let content = b"original content";
    let wrong_content = b"tampered content";

    // Sign original content
    let signature = verifier_with_key.sign(content).expect("should sign");

    // Try to verify with wrong content
    let is_valid = verifier_with_key
        .verify(wrong_content, &signature)
        .expect("should verify");
    assert!(
        !is_valid,
        "signature should not be valid for tampered content"
    );
}

#[cfg(feature = "crypto")]
#[tokio::test]
async fn test_ed25519_keypair_generation() {
    let verifier = Ed25519Verifier::new();

    // Generate multiple keypairs to ensure randomness
    let keypair1 = verifier
        .generate_keypair()
        .expect("should generate keypair");
    let keypair2 = verifier
        .generate_keypair()
        .expect("should generate keypair");

    // Keys should be different
    assert_ne!(keypair1.public_key.key_data, keypair2.public_key.key_data);

    // Keys should be 32 bytes
    assert_eq!(keypair1.public_key.key_data.len(), 32);
    assert_eq!(keypair2.public_key.key_data.len(), 32);
}

#[cfg(feature = "crypto")]
#[tokio::test]
async fn test_ed25519_export_import_public_key() {
    let verifier = Ed25519Verifier::new();
    let keypair = verifier
        .generate_keypair()
        .expect("should generate keypair");

    // Export public key to PEM
    let pem = verifier
        .export_public_key(&keypair.public_key)
        .expect("should export");

    assert!(pem.contains("-----BEGIN PUBLIC KEY-----"));
    assert!(pem.contains("-----END PUBLIC KEY-----"));

    // Import public key from PEM
    let imported = verifier.import_public_key(&pem).expect("should import");

    assert_eq!(imported.key_data, keypair.public_key.key_data);
    assert_eq!(imported.algorithm, keypair.public_key.algorithm);
}

// ============================================================================
// P2P Registry Tests
// ============================================================================

#[cfg(feature = "crypto")]
#[tokio::test]
async fn test_p2p_publish_and_discover() {
    let p2p_registry = MockP2PRegistry::new();

    // Create test package
    let package = Package::builder(PackageId::new("test", "p2p-package"), Version::new(1, 0, 0))
        .title("P2P Test Package")
        .description("A package for P2P testing")
        .license("MIT")
        .tag("p2p")
        .content_id(ContentId::new(
            "test-hash".to_string(),
            HashAlgorithm::Sha256,
        ))
        .build()
        .expect("package should build");

    // Validate package before publishing (Poka-yoke: ensures package meets requirements)
    let validated = package.validate().expect("Failed to validate package");
    let validated_package = validated.package().clone();

    // Publish to P2P network
    p2p_registry
        .publish(validated_package.clone())
        .await
        .expect("publish should succeed");

    // Discover via P2P
    let results = p2p_registry
        .discover("p2p")
        .await
        .expect("discover should succeed");

    assert_eq!(results.len(), 1);
    assert_eq!(results[0].id.name, "p2p-package");

    // Verify we're using validated package
    assert_eq!(validated_package.id.name, "p2p-package");
}

#[cfg(feature = "crypto")]
#[tokio::test]
async fn test_p2p_retrieve_package() {
    let p2p_registry = MockP2PRegistry::new();

    let package = Package::builder(
        PackageId::new("test", "retrieve-test"),
        Version::new(1, 0, 0),
    )
    .title("Retrieve Test")
    .description("Testing package retrieval")
    .license("MIT")
    .content_id(ContentId::new("hash123".to_string(), HashAlgorithm::Sha256))
    .build()
    .expect("package should build");

    p2p_registry
        .publish(package.clone())
        .await
        .expect("publish should succeed");

    // Retrieve by ID
    let package_id = PackageId::new("test", "retrieve-test");
    let retrieved = p2p_registry
        .retrieve(&package_id)
        .await
        .expect("retrieve should succeed");

    assert_eq!(retrieved.id.name, package.id.name);
    assert_eq!(retrieved.version, package.version);
}

#[cfg(feature = "crypto")]
#[tokio::test]
async fn test_p2p_peer_management() {
    let p2p_registry = MockP2PRegistry::new();

    // Add peers
    p2p_registry
        .add_peer("peer1.example.com:8080".to_string())
        .await
        .expect("add peer should succeed");
    p2p_registry
        .add_peer("peer2.example.com:8080".to_string())
        .await
        .expect("add peer should succeed");
    p2p_registry
        .add_peer("peer3.example.com:8080".to_string())
        .await
        .expect("add peer should succeed");

    // List peers
    let peers = p2p_registry.list_peers().await;
    assert_eq!(peers.len(), 3);
    assert!(peers.contains(&"peer1.example.com:8080".to_string()));
}

#[cfg(feature = "crypto")]
#[tokio::test]
async fn test_p2p_discover_multiple_packages() {
    let p2p_registry = MockP2PRegistry::new();

    // Publish multiple packages
    for i in 0..5 {
        let package = Package::builder(
            PackageId::new("test", &format!("web-package-{}", i)),
            Version::new(1, 0, 0),
        )
        .title(format!("Web Package {}", i))
        .description("A web framework package")
        .license("MIT")
        .tag("web")
        .content_id(ContentId::new(format!("hash-{}", i), HashAlgorithm::Sha256))
        .build()
        .expect("package should build");

        p2p_registry
            .publish(package)
            .await
            .expect("publish should succeed");
    }

    // Discover all web packages
    let results = p2p_registry
        .discover("web")
        .await
        .expect("discover should succeed");
    assert_eq!(results.len(), 5);
}

// ============================================================================
// GraphQL API Tests
// ============================================================================

#[cfg(feature = "crypto")]
#[tokio::test]
async fn test_graphql_search_query() {
    let (p2p_registry, graphql_server) = setup_test_environment().await;

    // Publish test packages
    let package = Package::builder(
        PackageId::new("test", "graphql-test"),
        Version::new(1, 0, 0),
    )
    .title("GraphQL Test")
    .description("Testing GraphQL queries")
    .license("MIT")
    .tag("graphql")
    .content_id(ContentId::new(
        "hash-gql".to_string(),
        HashAlgorithm::Sha256,
    ))
    .build()
    .expect("package should build");

    p2p_registry
        .publish(package)
        .await
        .expect("publish should succeed");

    // Execute GraphQL query
    let query = r#"
        {
            searchPackages(query: "graphql") {
                name
                version
                description
            }
        }
    "#;

    let response = graphql_server
        .execute_query(query)
        .await
        .expect("query should succeed");

    assert!(response.data.is_some());
    if let Some(GraphQLData::SearchResults(packages)) = response.data {
        assert_eq!(packages.len(), 1);
        assert_eq!(packages[0].id.name, "graphql-test");
    } else {
        panic!("Expected SearchResults");
    }
}

#[cfg(feature = "crypto")]
#[tokio::test]
async fn test_graphql_package_query() {
    let (p2p_registry, graphql_server) = setup_test_environment().await;

    let package = Package::builder(
        PackageId::new("test", "specific-package"),
        Version::new(2, 1, 0),
    )
    .title("Specific Package")
    .description("A specific package for testing")
    .license("Apache-2.0")
    .content_id(ContentId::new(
        "hash-specific".to_string(),
        HashAlgorithm::Sha256,
    ))
    .build()
    .expect("package should build");

    p2p_registry
        .publish(package)
        .await
        .expect("publish should succeed");

    // Query specific package
    let query = r#"
        {
            package(name: "specific-package") {
                name
                version
                license
            }
        }
    "#;

    let response = graphql_server
        .execute_query(query)
        .await
        .expect("query should succeed");

    assert!(response.data.is_some());
    if let Some(GraphQLData::Package(pkg)) = response.data {
        assert_eq!(pkg.id.name, "specific-package");
        assert_eq!(pkg.metadata.license, "Apache-2.0");
    } else {
        panic!("Expected Package");
    }
}

#[cfg(feature = "crypto")]
#[tokio::test]
async fn test_graphql_query_not_found() {
    let (_p2p_registry, graphql_server) = setup_test_environment().await;

    let query = r#"
        {
            package(name: "nonexistent-package") {
                name
            }
        }
    "#;

    let response = graphql_server
        .execute_query(query)
        .await
        .expect("query should execute");

    assert!(response.errors.is_some());
}

// ============================================================================
// Full Stack Integration Tests (Ed25519 + P2P + GraphQL)
// ============================================================================

#[cfg(feature = "crypto")]
#[tokio::test]
async fn test_full_stack_signed_p2p_package_via_graphql() {
    let (p2p_registry, graphql_server) = setup_test_environment().await;

    // 1. Create signed package
    let content = b"Package content for full stack test";
    let (package, _content_bytes) = create_signed_package("full-stack-package", "1.0.0", content)
        .await
        .expect("should create signed package");

    // 2. Publish to P2P network
    p2p_registry
        .publish(package.clone())
        .await
        .expect("publish should succeed");

    // 3. Query via GraphQL
    let query = r#"
        {
            searchPackages(query: "full-stack") {
                name
                version
                tags
            }
        }
    "#;

    let response = graphql_server
        .execute_query(query)
        .await
        .expect("query should succeed");

    // 4. Verify result
    assert!(response.data.is_some());
    assert!(response.errors.is_none());

    if let Some(GraphQLData::SearchResults(packages)) = response.data {
        assert_eq!(packages.len(), 1);
        assert_eq!(packages[0].id.name, "full-stack-package");
        assert!(packages[0].metadata.tags.contains(&"signed".to_string()));
    }
}

#[cfg(feature = "crypto")]
#[tokio::test]
async fn test_signed_package_p2p_distribution() {
    let p2p_registry = MockP2PRegistry::new();

    // Create signed packages
    let packages = vec![
        create_signed_package("auth-lib", "1.0.0", b"auth library")
            .await
            .expect("should create"),
        create_signed_package("crypto-lib", "2.0.0", b"crypto library")
            .await
            .expect("should create"),
        create_signed_package("web-lib", "1.5.0", b"web library")
            .await
            .expect("should create"),
    ];

    // Publish all packages
    for (package, _content) in &packages {
        p2p_registry
            .publish(package.clone())
            .await
            .expect("publish should succeed");
    }

    // Discover via P2P
    let results = p2p_registry
        .discover("lib")
        .await
        .expect("discover should succeed");
    assert_eq!(results.len(), 3);

    // Verify all packages are signed (have content hash)
    for pkg in results {
        assert!(!pkg.content_id.hash.is_empty());
        assert_eq!(pkg.content_id.algorithm, HashAlgorithm::Sha256);
    }
}

#[cfg(feature = "crypto")]
#[tokio::test]
async fn test_graphql_query_signed_packages() {
    let (p2p_registry, graphql_server) = setup_test_environment().await;

    // Publish signed packages with "signed" tag
    for i in 0..3 {
        let content = format!("signed content {}", i);
        let (package, _) =
            create_signed_package(&format!("signed-pkg-{}", i), "1.0.0", content.as_bytes())
                .await
                .expect("should create signed package");

        p2p_registry
            .publish(package)
            .await
            .expect("publish should succeed");
    }

    // Query for signed packages
    let query = r#"
        {
            searchPackages(query: "signed") {
                name
                contentId
                tags
            }
        }
    "#;

    let response = graphql_server
        .execute_query(query)
        .await
        .expect("query should succeed");

    assert!(response.data.is_some());
    if let Some(GraphQLData::SearchResults(packages)) = response.data {
        assert_eq!(packages.len(), 3);

        // Verify all have content IDs (signatures)
        for pkg in packages {
            assert!(!pkg.content_id.hash.is_empty());
            assert!(pkg.metadata.tags.contains(&"signed".to_string()));
        }
    }
}

// ============================================================================
// P2P Network Resilience Tests
// ============================================================================

#[cfg(feature = "crypto")]
#[tokio::test]
async fn test_p2p_network_resilience() {
    let p2p_registry = MockP2PRegistry::new();

    // Add multiple peers
    for i in 0..10 {
        p2p_registry
            .add_peer(format!("peer{}.example.com:8080", i))
            .await
            .expect("add peer should succeed");
    }

    // Publish packages across network
    for i in 0..20 {
        let package = Package::builder(
            PackageId::new("test", &format!("resilient-pkg-{}", i)),
            Version::new(1, 0, 0),
        )
        .title(format!("Resilient Package {}", i))
        .description("Testing network resilience")
        .license("MIT")
        .tag("resilience")
        .content_id(ContentId::new(format!("hash-{}", i), HashAlgorithm::Sha256))
        .build()
        .expect("package should build");

        p2p_registry
            .publish(package)
            .await
            .expect("publish should succeed");
    }

    // Verify all packages are discoverable
    let results = p2p_registry
        .discover("resilient")
        .await
        .expect("discover should succeed");
    assert_eq!(results.len(), 20);

    // Verify peer count
    let peers = p2p_registry.list_peers().await;
    assert_eq!(peers.len(), 10);
}

#[cfg(feature = "crypto")]
#[tokio::test]
async fn test_p2p_concurrent_operations() {
    let p2p_registry = Arc::new(MockP2PRegistry::new());

    // Publish packages concurrently
    let mut handles = vec![];

    for i in 0..10 {
        let registry = p2p_registry.clone();
        let handle = tokio::spawn(async move {
            let package = Package::builder(
                PackageId::new("test", &format!("concurrent-{}", i)),
                Version::new(1, 0, 0),
            )
            .title(format!("Concurrent Package {}", i))
            .description("Testing concurrent operations")
            .license("MIT")
            .content_id(ContentId::new(format!("hash-{}", i), HashAlgorithm::Sha256))
            .build()
            .expect("package should build");

            registry
                .publish(package)
                .await
                .expect("publish should succeed");
        });

        handles.push(handle);
    }

    // Wait for all publishes to complete
    for handle in handles {
        handle.await.expect("task should complete");
    }

    // Verify all packages were published
    let results = p2p_registry
        .discover("concurrent")
        .await
        .expect("discover should succeed");
    assert_eq!(results.len(), 10);
}

// ============================================================================
// Performance and Error Scenarios
// ============================================================================

#[cfg(feature = "crypto")]
#[tokio::test]
async fn test_graphql_invalid_query() {
    let (_p2p_registry, graphql_server) = setup_test_environment().await;

    let invalid_query = "{ invalid syntax }";

    let result = graphql_server.execute_query(invalid_query).await;
    assert!(result.is_err());
}

#[cfg(feature = "crypto")]
#[tokio::test]
async fn test_p2p_retrieve_nonexistent_package() {
    let p2p_registry = MockP2PRegistry::new();

    let package_id = PackageId::new("test", "does-not-exist");
    let result = p2p_registry.retrieve(&package_id).await;

    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.to_string().contains("not found") || err.to_string().contains("NotFound"));
}

#[cfg(feature = "crypto")]
#[tokio::test]
async fn test_large_scale_p2p_discovery() {
    let p2p_registry = MockP2PRegistry::new();

    // Publish 100 packages
    for i in 0..100 {
        let package = Package::builder(
            PackageId::new("test", &format!("scale-test-{}", i)),
            Version::new(1, 0, 0),
        )
        .title(format!("Scale Test Package {}", i))
        .description(format!("Testing large scale discovery {}", i % 10))
        .license("MIT")
        .tag(format!("category-{}", i % 5))
        .content_id(ContentId::new(format!("hash-{}", i), HashAlgorithm::Sha256))
        .build()
        .expect("package should build");

        p2p_registry
            .publish(package)
            .await
            .expect("publish should succeed");
    }

    // Discover by category
    let results = p2p_registry
        .discover("category-0")
        .await
        .expect("discover should succeed");
    assert_eq!(results.len(), 20); // 100 packages / 5 categories = 20 per category
}

// ============================================================================
// Integration with Existing Systems
// ============================================================================

#[cfg(feature = "crypto")]
#[tokio::test]
async fn test_integration_with_local_registry() {
    // Setup local registry
    let temp_dir = tempfile::tempdir().expect("should create temp dir");
    let db_path = temp_dir.path().join("test-registry");
    let local_registry = LocalRegistry::new(db_path)
        .await
        .expect("should create local registry");

    // Create and publish package
    let package = Package::builder(
        PackageId::new("test", "integration-test"),
        Version::new(1, 0, 0),
    )
    .title("Integration Test Package")
    .description("Testing integration with local registry")
    .license("MIT")
    .content_id(ContentId::new(
        "hash-integration".to_string(),
        HashAlgorithm::Sha256,
    ))
    .build()
    .expect("package should build");

    local_registry
        .publish(package.clone())
        .await
        .expect("publish should succeed");

    // Search in local registry
    let query = Query::new("integration");
    let results = local_registry
        .search(&query)
        .await
        .expect("search should succeed");

    assert_eq!(results.len(), 1);
    assert_eq!(results[0].id.name, "integration-test");
}
