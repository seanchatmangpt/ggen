//! Integration Tests for P2P Registry

#[cfg(test)]
mod integration_tests {
    use crate::p2p::{
        P2PRegistry, P2PRegistryBuilder, P2PConfig, Registry,
        Package, Query, BootstrapNode,
    };

    #[tokio::test]
    async fn test_full_workflow() {
        // Create registry
        let mut registry = P2PRegistryBuilder::new()
            .with_listen_addresses(vec!["/ip4/127.0.0.1/tcp/0".to_string()])
            .build()
            .unwrap();

        // Start registry
        registry.start().await.unwrap();
        assert!(registry.is_running().await);

        // Publish a package
        let package = Package::new("test-package".to_string(), "1.0.0".to_string());
        registry.publish(package.clone()).await.unwrap();

        // Search for the package
        let query = Query::new(vec!["test".to_string()]);
        let results = registry.search(&query).await.unwrap();
        assert!(!results.is_empty());

        // Get specific package
        let retrieved = registry.get(&package.id).await.unwrap();
        assert!(retrieved.is_some());
        assert_eq!(retrieved.unwrap().id, package.id);

        // Subscribe to updates
        registry.subscribe("test-topic").await.unwrap();

        // Get stats
        let stats = registry.stats().await;
        assert_eq!(stats.cached_packages, 1);

        // Stop registry
        registry.stop().await.unwrap();
        assert!(!registry.is_running().await);
    }

    #[tokio::test]
    async fn test_multiple_packages() {
        let registry = P2PRegistryBuilder::new().build().unwrap();
        registry.start().await.unwrap();

        // Publish multiple packages
        for i in 0..5 {
            let package = Package::new(
                format!("package-{}", i),
                "1.0.0".to_string(),
            );
            registry.publish(package).await.unwrap();
        }

        // Search all packages
        let query = Query::new(vec!["package".to_string()]).with_limit(10);
        let results = registry.search(&query).await.unwrap();
        assert_eq!(results.len(), 5);

        registry.stop().await.unwrap();
    }

    #[tokio::test]
    async fn test_bootstrap_configuration() {
        let nodes = vec![
            BootstrapNode {
                peer_id: "peer1".to_string(),
                address: "/ip4/127.0.0.1/tcp/4001".to_string(),
            },
            BootstrapNode {
                peer_id: "peer2".to_string(),
                address: "/ip4/127.0.0.1/tcp/4002".to_string(),
            },
        ];

        let registry = P2PRegistryBuilder::new()
            .with_bootstrap_nodes(nodes)
            .build()
            .unwrap();

        registry.start().await.unwrap();
        let stats = registry.stats().await;
        assert_eq!(stats.total_peers, 2);

        registry.stop().await.unwrap();
    }

    #[tokio::test]
    async fn test_search_with_filters() {
        let registry = P2PRegistryBuilder::new().build().unwrap();
        registry.start().await.unwrap();

        // Publish packages with different categories
        let mut package1 = Package::new("web-service".to_string(), "1.0.0".to_string());
        package1.category = "web".to_string();
        package1.tags = vec!["rust".to_string(), "web".to_string()];

        let mut package2 = Package::new("cli-tool".to_string(), "1.0.0".to_string());
        package2.category = "cli".to_string();
        package2.tags = vec!["rust".to_string(), "cli".to_string()];

        registry.publish(package1).await.unwrap();
        registry.publish(package2).await.unwrap();

        // Search with category filter
        let query = Query::new(vec!["rust".to_string()])
            .with_category("web".to_string());
        let results = registry.search(&query).await.unwrap();
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].package.category, "web");

        registry.stop().await.unwrap();
    }

    #[tokio::test]
    async fn test_error_handling() {
        let registry = P2PRegistryBuilder::new().build().unwrap();

        // Try operations before starting
        let query = Query::new(vec!["test".to_string()]);
        let result = registry.search(&query).await;
        assert!(result.is_err());

        // Start and test invalid package ID
        registry.start().await.unwrap();
        let result = registry.get("nonexistent").await.unwrap();
        assert!(result.is_none());

        registry.stop().await.unwrap();
    }
}
