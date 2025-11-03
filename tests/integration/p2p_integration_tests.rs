//! P2P marketplace integration tests
//!
//! Tests for the complete P2P marketplace functionality including:
//! - Node initialization and startup
//! - Package publishing and discovery
//! - Peer reputation tracking
//! - DHT operations

#[cfg(feature = "p2p")]
mod p2p_tests {
    use ggen_marketplace::backend::p2p::{P2PConfig, P2PRegistry};
    use ggen_marketplace::models::{Package, PackageId, PackageMetadata, Version, ContentId};
    use ggen_marketplace::traits::Registry;

    #[tokio::test]
    async fn test_p2p_registry_creation() {
        let config = P2PConfig::default();
        let registry = P2PRegistry::new(config).await;
        assert!(registry.is_ok(), "Should create P2P registry successfully");
    }

    #[tokio::test]
    async fn test_p2p_node_startup() {
        let config = P2PConfig::default();
        let registry = P2PRegistry::new(config).await.unwrap();

        // Start listening
        let result = registry.start_listening().await;
        assert!(result.is_ok(), "Should start listening successfully");

        // Subscribe to packages
        let result = registry.subscribe_to_packages().await;
        assert!(result.is_ok(), "Should subscribe to package topic successfully");
    }

    #[tokio::test]
    async fn test_package_publishing() {
        let config = P2PConfig::default();
        let registry = P2PRegistry::new(config).await.unwrap();

        // Create a test package
        let package = Package {
            id: PackageId::new("test-package"),
            version: Version::new(1, 0, 0),
            metadata: PackageMetadata {
                title: "Test Package".to_string(),
                description: "A test package for P2P".to_string(),
                author: "Test Author".to_string(),
                categories: vec!["testing".to_string()],
                tags: vec!["test".to_string()],
            },
            content_id: ContentId::from_hash("sha256", "test123"),
            dependencies: Vec::new(),
            signature: None,
            created_at: chrono::Utc::now(),
        };

        // Publish the package
        let result = registry.publish(package.clone()).await;
        assert!(result.is_ok(), "Should publish package successfully");

        // Verify package exists locally
        let exists = registry.exists(&package.id).await;
        assert!(exists.is_ok() && exists.unwrap(), "Published package should exist");
    }

    #[tokio::test]
    async fn test_package_retrieval() {
        let config = P2PConfig::default();
        let registry = P2PRegistry::new(config).await.unwrap();

        // Create and publish a package
        let package_id = PackageId::new("retrievable-package");
        let package = Package {
            id: package_id.clone(),
            version: Version::new(1, 0, 0),
            metadata: PackageMetadata {
                title: "Retrievable Package".to_string(),
                description: "Test retrieval".to_string(),
                author: "Test".to_string(),
                categories: Vec::new(),
                tags: Vec::new(),
            },
            content_id: ContentId::from_hash("sha256", "retrieve123"),
            dependencies: Vec::new(),
            signature: None,
            created_at: chrono::Utc::now(),
        };

        registry.publish(package.clone()).await.unwrap();

        // Retrieve the package
        let retrieved = registry.get_package(&package_id).await;
        assert!(retrieved.is_ok(), "Should retrieve published package");

        let retrieved_package = retrieved.unwrap();
        assert_eq!(retrieved_package.id, package_id);
        assert_eq!(retrieved_package.metadata.title, "Retrievable Package");
    }

    #[tokio::test]
    async fn test_package_search() {
        let config = P2PConfig::default();
        let registry = P2PRegistry::new(config).await.unwrap();

        // Publish multiple packages
        for i in 0..3 {
            let package = Package {
                id: PackageId::new(&format!("search-test-{}", i)),
                version: Version::new(1, 0, 0),
                metadata: PackageMetadata {
                    title: format!("Search Test Package {}", i),
                    description: "Test search functionality".to_string(),
                    author: "Test".to_string(),
                    categories: vec!["testing".to_string()],
                    tags: vec!["search".to_string(), "test".to_string()],
                },
                content_id: ContentId::from_hash("sha256", &format!("search{}", i)),
                dependencies: Vec::new(),
                signature: None,
                created_at: chrono::Utc::now(),
            };
            registry.publish(package).await.unwrap();
        }

        // Search for packages
        let query = ggen_marketplace::models::Query {
            text: "search".to_string(),
            categories: vec!["testing".to_string()],
            tags: Vec::new(),
            limit: Some(10),
        };

        let results = registry.search(&query).await;
        assert!(results.is_ok(), "Search should succeed");

        let packages = results.unwrap();
        assert!(packages.len() >= 3, "Should find at least 3 packages");
    }

    #[tokio::test]
    async fn test_peer_reputation_tracking() {
        let config = P2PConfig::default();
        let registry = P2PRegistry::new(config).await.unwrap();

        // Generate a test peer ID
        let keypair = libp2p::identity::Keypair::generate_ed25519();
        let peer_id = libp2p::PeerId::from(keypair.public());

        // Initial reputation should be 1.0 (optimistic)
        let reputation = registry.get_peer_reputation(&peer_id).await;
        assert_eq!(reputation, 1.0, "Initial reputation should be 1.0");
    }

    #[tokio::test]
    async fn test_registry_metadata() {
        let config = P2PConfig::default();
        let registry = P2PRegistry::new(config).await.unwrap();

        let metadata = registry.metadata().await;
        assert!(metadata.is_ok(), "Should get registry metadata");

        let metadata = metadata.unwrap();
        assert_eq!(metadata.name, "Ggen P2P Registry");
        assert!(metadata.supports_publish);
        assert!(!metadata.requires_auth);
    }
}

#[cfg(not(feature = "p2p"))]
#[test]
fn test_p2p_feature_disabled() {
    // This test just ensures the module compiles when p2p feature is disabled
    assert!(true, "P2P feature is disabled, skipping tests");
}
