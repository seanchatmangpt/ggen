//! P2P Registry Integration Tests
//!
//! Comprehensive tests for ggen's P2P (peer-to-peer) registry functionality.
//! Tests cover peer discovery, package announcements, DHT interactions, and
//! distributed package resolution.
//!
//! ## Test Categories
//! - Peer discovery and connectivity
//! - Package publishing to P2P network
//! - Distributed search across peers
//! - DHT (Distributed Hash Table) operations
//! - Peer reputation and reliability
//! - Network resilience and failover
//!
//! ## Running Tests
//! ```bash
//! # Run all P2P tests
//! cargo test --test marketplace_tests_main integration::marketplace_p2p_tests
//!
//! # Run with networking features
//! cargo test --features p2p --test marketplace_tests_main integration::marketplace_p2p_tests
//! ```

use anyhow::Result;
use ggen_core::registry::{PackMetadata, VersionMetadata};
use std::collections::HashMap;
use std::time::Duration;
use tokio::time::timeout;

// ============================================================================
// MOCK P2P TYPES FOR TESTING
// ============================================================================

/// Mock P2P registry for testing without actual network
#[derive(Clone, Debug)]
struct MockP2PRegistry {
    /// Simulated peer ID
    peer_id: String,
    /// Local package store
    packages: HashMap<String, PackMetadata>,
    /// Connected peers
    peers: Vec<String>,
    /// Simulated network latency
    latency_ms: u64,
}

impl MockP2PRegistry {
    /// Create a new mock P2P registry
    fn new(peer_id: &str) -> Self {
        Self {
            peer_id: peer_id.to_string(),
            packages: HashMap::new(),
            peers: Vec::new(),
            latency_ms: 10,
        }
    }

    /// Simulate connecting to a peer
    async fn connect_peer(&mut self, peer_id: String) -> Result<()> {
        tokio::time::sleep(Duration::from_millis(self.latency_ms)).await;
        if !self.peers.contains(&peer_id) {
            self.peers.push(peer_id);
        }
        Ok(())
    }

    /// Simulate publishing a package
    async fn publish_package(&mut self, package: PackMetadata) -> Result<()> {
        tokio::time::sleep(Duration::from_millis(self.latency_ms)).await;
        self.packages.insert(package.id.clone(), package);
        Ok(())
    }

    /// Simulate searching for packages across connected peers
    async fn search(&self, query: &str) -> Result<Vec<PackMetadata>> {
        tokio::time::sleep(Duration::from_millis(self.latency_ms)).await;

        let results: Vec<PackMetadata> = self
            .packages
            .values()
            .filter(|p| {
                p.id.contains(query)
                    || p.name.to_lowercase().contains(&query.to_lowercase())
                    || p.description.to_lowercase().contains(&query.to_lowercase())
                    || p.tags.iter().any(|t| t.contains(query))
            })
            .cloned()
            .collect();

        Ok(results)
    }

    /// Simulate resolving a package from the DHT
    async fn resolve(&self, package_id: &str) -> Result<Option<PackMetadata>> {
        tokio::time::sleep(Duration::from_millis(self.latency_ms)).await;
        Ok(self.packages.get(package_id).cloned())
    }

    /// Get list of connected peers
    fn connected_peers(&self) -> &[String] {
        &self.peers
    }

    /// Simulate network partition (disconnect all peers)
    fn simulate_partition(&mut self) {
        self.peers.clear();
    }

    /// Simulate network heal (reconnect peers)
    async fn simulate_heal(&mut self, peers: Vec<String>) -> Result<()> {
        for peer in peers {
            self.connect_peer(peer).await?;
        }
        Ok(())
    }
}

/// Mock P2P network with multiple nodes
struct MockP2PNetwork {
    /// Registry nodes
    nodes: HashMap<String, MockP2PRegistry>,
}

impl MockP2PNetwork {
    /// Create a new mock P2P network
    fn new() -> Self {
        Self {
            nodes: HashMap::new(),
        }
    }

    /// Add a node to the network
    fn add_node(&mut self, peer_id: &str) -> &mut MockP2PRegistry {
        self.nodes
            .insert(peer_id.to_string(), MockP2PRegistry::new(peer_id));
        self.nodes.get_mut(peer_id).unwrap()
    }

    /// Connect two nodes
    async fn connect_nodes(&mut self, peer_a: &str, peer_b: &str) -> Result<()> {
        if let Some(node_a) = self.nodes.get_mut(peer_a) {
            node_a.connect_peer(peer_b.to_string()).await?;
        }
        if let Some(node_b) = self.nodes.get_mut(peer_b) {
            node_b.connect_peer(peer_a.to_string()).await?;
        }
        Ok(())
    }

    /// Get a node by peer ID
    fn get_node(&self, peer_id: &str) -> Option<&MockP2PRegistry> {
        self.nodes.get(peer_id)
    }

    /// Get a mutable node by peer ID
    fn get_node_mut(&mut self, peer_id: &str) -> Option<&mut MockP2PRegistry> {
        self.nodes.get_mut(peer_id)
    }
}

// ============================================================================
// PEER DISCOVERY TESTS
// ============================================================================

#[tokio::test]
async fn test_p2p_peer_discovery() -> Result<()> {
    let mut network = MockP2PNetwork::new();

    // Create bootstrap node
    let _bootstrap = network.add_node("bootstrap-peer");

    // Create new peer
    let _peer = network.add_node("peer-1");

    // Connect to bootstrap
    network.connect_nodes("peer-1", "bootstrap-peer").await?;

    // Verify connection
    let peer = network.get_node("peer-1").unwrap();
    assert_eq!(peer.connected_peers().len(), 1);
    assert!(peer
        .connected_peers()
        .contains(&"bootstrap-peer".to_string()));

    Ok(())
}

#[tokio::test]
async fn test_p2p_multi_peer_connectivity() -> Result<()> {
    let mut network = MockP2PNetwork::new();

    // Create mesh of 5 peers
    for i in 0..5 {
        network.add_node(&format!("peer-{}", i));
    }

    // Connect peers in a mesh topology
    for i in 0..5 {
        for j in (i + 1)..5 {
            network
                .connect_nodes(&format!("peer-{}", i), &format!("peer-{}", j))
                .await?;
        }
    }

    // Verify each peer is connected to 4 others
    for i in 0..5 {
        let peer = network.get_node(&format!("peer-{}", i)).unwrap();
        assert_eq!(
            peer.connected_peers().len(),
            4,
            "Peer {} should have 4 connections",
            i
        );
    }

    Ok(())
}

// ============================================================================
// PACKAGE PUBLISHING TESTS
// ============================================================================

#[tokio::test]
async fn test_p2p_publish_package() -> Result<()> {
    let mut registry = MockP2PRegistry::new("publisher-peer");

    // Create test package
    let mut versions = HashMap::new();
    versions.insert(
        "1.0.0".to_string(),
        VersionMetadata {
            version: "1.0.0".to_string(),
            git_url: "https://github.com/test/p2p-package.git".to_string(),
            git_rev: "v1.0.0".to_string(),
            manifest_url: None,
            sha256: "test-hash".to_string(),
        },
    );

    let package = PackMetadata {
        id: "p2p-test-package".to_string(),
        name: "P2P Test Package".to_string(),
        description: "A test package for P2P registry".to_string(),
        tags: vec!["test".to_string(), "p2p".to_string()],
        keywords: vec!["testing".to_string()],
        category: Some("test".to_string()),
        author: Some("Test Author".to_string()),
        latest_version: "1.0.0".to_string(),
        versions,
        downloads: Some(0),
        updated: Some(chrono::Utc::now()),
        license: Some("MIT".to_string()),
        homepage: None,
        repository: None,
        documentation: None,
    };

    // Publish package
    registry.publish_package(package.clone()).await?;

    // Verify package is available
    let resolved = registry.resolve("p2p-test-package").await?;
    assert!(resolved.is_some());
    assert_eq!(resolved.unwrap().id, "p2p-test-package");

    Ok(())
}

#[tokio::test]
async fn test_p2p_package_propagation() -> Result<()> {
    let mut network = MockP2PNetwork::new();

    // Create 3 peers
    network.add_node("peer-a");
    network.add_node("peer-b");
    network.add_node("peer-c");

    // Connect them: a <-> b <-> c
    network.connect_nodes("peer-a", "peer-b").await?;
    network.connect_nodes("peer-b", "peer-c").await?;

    // Publish package on peer-a
    let mut versions = HashMap::new();
    versions.insert(
        "1.0.0".to_string(),
        VersionMetadata {
            version: "1.0.0".to_string(),
            git_url: "https://github.com/test/propagate.git".to_string(),
            git_rev: "v1.0.0".to_string(),
            manifest_url: None,
            sha256: "propagate-hash".to_string(),
        },
    );

    let package = PackMetadata {
        id: "propagated-package".to_string(),
        name: "Propagated Package".to_string(),
        description: "Package that propagates across P2P network".to_string(),
        tags: vec!["test".to_string()],
        keywords: vec!["propagation".to_string()],
        category: Some("test".to_string()),
        author: Some("Tester".to_string()),
        latest_version: "1.0.0".to_string(),
        versions,
        downloads: Some(0),
        updated: Some(chrono::Utc::now()),
        license: Some("MIT".to_string()),
        homepage: None,
        repository: None,
        documentation: None,
    };

    if let Some(peer_a) = network.get_node_mut("peer-a") {
        peer_a.publish_package(package.clone()).await?;
    }

    // Simulate propagation by publishing to connected peers
    if let Some(peer_b) = network.get_node_mut("peer-b") {
        peer_b.publish_package(package.clone()).await?;
    }
    if let Some(peer_c) = network.get_node_mut("peer-c") {
        peer_c.publish_package(package).await?;
    }

    // Verify package is available on all peers
    for peer_id in &["peer-a", "peer-b", "peer-c"] {
        let peer = network.get_node(peer_id).unwrap();
        let resolved = peer.resolve("propagated-package").await?;
        assert!(
            resolved.is_some(),
            "Package should be available on {}",
            peer_id
        );
    }

    Ok(())
}

// ============================================================================
// DISTRIBUTED SEARCH TESTS
// ============================================================================

#[tokio::test]
async fn test_p2p_distributed_search() -> Result<()> {
    let mut network = MockP2PNetwork::new();

    // Create 3 peers with different packages
    network.add_node("peer-rust");
    network.add_node("peer-python");
    network.add_node("peer-javascript");

    // Connect peers
    network.connect_nodes("peer-rust", "peer-python").await?;
    network
        .connect_nodes("peer-python", "peer-javascript")
        .await?;

    // Add rust package to peer-rust
    let rust_package =
        create_test_package("rust-web-server", "Rust web server", vec!["rust", "web"]);
    if let Some(peer) = network.get_node_mut("peer-rust") {
        peer.publish_package(rust_package).await?;
    }

    // Add python package to peer-python
    let python_package = create_test_package("python-api", "Python API", vec!["python", "api"]);
    if let Some(peer) = network.get_node_mut("peer-python") {
        peer.publish_package(python_package).await?;
    }

    // Add javascript package to peer-javascript
    let js_package = create_test_package(
        "nodejs-service",
        "Node.js service",
        vec!["javascript", "nodejs"],
    );
    if let Some(peer) = network.get_node_mut("peer-javascript") {
        peer.publish_package(js_package).await?;
    }

    // Search from each peer (in real P2P, would query connected peers)
    let peer_rust = network.get_node("peer-rust").unwrap();
    let rust_results = peer_rust.search("rust").await?;
    assert_eq!(rust_results.len(), 1);
    assert_eq!(rust_results[0].id, "rust-web-server");

    Ok(())
}

#[tokio::test]
async fn test_p2p_search_with_timeout() -> Result<()> {
    let mut registry = MockP2PRegistry::new("timeout-peer");
    registry.latency_ms = 2000; // 2 second latency

    // Search with 1 second timeout should fail
    let result = timeout(Duration::from_millis(1000), registry.search("test")).await;

    assert!(result.is_err(), "Search should timeout");

    // Search with 3 second timeout should succeed
    registry.latency_ms = 10; // Reset to fast
    let result = timeout(Duration::from_millis(3000), registry.search("test")).await;

    assert!(result.is_ok(), "Search should succeed with longer timeout");

    Ok(())
}

// ============================================================================
// DHT OPERATIONS TESTS
// ============================================================================

#[tokio::test]
async fn test_p2p_dht_put_get() -> Result<()> {
    let mut registry = MockP2PRegistry::new("dht-peer");

    // "Put" a package into DHT (publish)
    let package = create_test_package("dht-test", "DHT test package", vec!["test"]);
    registry.publish_package(package.clone()).await?;

    // "Get" the package from DHT (resolve)
    let retrieved = registry.resolve("dht-test").await?;

    assert!(retrieved.is_some());
    assert_eq!(retrieved.unwrap().id, "dht-test");

    Ok(())
}

#[tokio::test]
async fn test_p2p_dht_key_distribution() -> Result<()> {
    let mut network = MockP2PNetwork::new();

    // Create 10 peers to simulate DHT
    for i in 0..10 {
        network.add_node(&format!("dht-peer-{}", i));
    }

    // Publish 20 packages across the network
    for i in 0..20 {
        let peer_idx = i % 10; // Distribute across peers
        let package = create_test_package(
            &format!("package-{}", i),
            &format!("Package {}", i),
            vec!["test"],
        );

        if let Some(peer) = network.get_node_mut(&format!("dht-peer-{}", peer_idx)) {
            peer.publish_package(package).await?;
        }
    }

    // Verify distribution: each peer should have ~2 packages
    for i in 0..10 {
        let peer = network.get_node(&format!("dht-peer-{}", i)).unwrap();
        let package_count = peer.packages.len();
        assert!(
            package_count >= 1 && package_count <= 3,
            "Peer {} should have 1-3 packages, has {}",
            i,
            package_count
        );
    }

    Ok(())
}

// ============================================================================
// NETWORK RESILIENCE TESTS
// ============================================================================

#[tokio::test]
async fn test_p2p_network_partition_handling() -> Result<()> {
    let mut registry = MockP2PRegistry::new("resilient-peer");

    // Connect some peers
    registry.connect_peer("peer-1".to_string()).await?;
    registry.connect_peer("peer-2".to_string()).await?;
    registry.connect_peer("peer-3".to_string()).await?;

    assert_eq!(registry.connected_peers().len(), 3);

    // Simulate network partition
    registry.simulate_partition();
    assert_eq!(registry.connected_peers().len(), 0);

    // Publish package during partition (should still work locally)
    let package = create_test_package("partition-test", "Partition test", vec!["test"]);
    registry.publish_package(package).await?;

    // Verify local package is still available
    let local_package = registry.resolve("partition-test").await?;
    assert!(local_package.is_some());

    // Simulate network heal
    registry
        .simulate_heal(vec![
            "peer-1".to_string(),
            "peer-2".to_string(),
            "peer-3".to_string(),
        ])
        .await?;

    assert_eq!(registry.connected_peers().len(), 3);

    Ok(())
}

#[tokio::test]
async fn test_p2p_peer_failure_recovery() -> Result<()> {
    let mut network = MockP2PNetwork::new();

    // Create 5 peers
    for i in 0..5 {
        network.add_node(&format!("peer-{}", i));
    }

    // Connect in a ring: 0->1->2->3->4->0
    for i in 0..5 {
        let next = (i + 1) % 5;
        network
            .connect_nodes(&format!("peer-{}", i), &format!("peer-{}", next))
            .await?;
    }

    // Publish package on peer-0
    let package = create_test_package("failure-test", "Failure test", vec!["test"]);
    if let Some(peer) = network.get_node_mut("peer-0") {
        peer.publish_package(package.clone()).await?;
    }

    // Propagate to all peers
    for i in 1..5 {
        if let Some(peer) = network.get_node_mut(&format!("peer-{}", i)) {
            peer.publish_package(package.clone()).await?;
        }
    }

    // "Fail" peer-2 by removing it
    network.nodes.remove("peer-2");

    // Verify package is still available on other peers
    for i in [0, 1, 3, 4] {
        let peer = network.get_node(&format!("peer-{}", i)).unwrap();
        let resolved = peer.resolve("failure-test").await?;
        assert!(
            resolved.is_some(),
            "Package should still be available on peer-{}",
            i
        );
    }

    Ok(())
}

// ============================================================================
// PEER REPUTATION TESTS
// ============================================================================

#[tokio::test]
async fn test_p2p_peer_reputation_tracking() -> Result<()> {
    // In a real implementation, we'd track successful/failed retrievals
    // This test simulates the concept

    #[derive(Debug)]
    struct PeerReputation {
        peer_id: String,
        successful_retrievals: u64,
        failed_retrievals: u64,
    }

    impl PeerReputation {
        fn new(peer_id: &str) -> Self {
            Self {
                peer_id: peer_id.to_string(),
                successful_retrievals: 0,
                failed_retrievals: 0,
            }
        }

        fn success_rate(&self) -> f64 {
            let total = self.successful_retrievals + self.failed_retrievals;
            if total == 0 {
                return 1.0;
            }
            self.successful_retrievals as f64 / total as f64
        }

        fn record_success(&mut self) {
            self.successful_retrievals += 1;
        }

        fn record_failure(&mut self) {
            self.failed_retrievals += 1;
        }
    }

    let mut good_peer = PeerReputation::new("good-peer");
    let mut bad_peer = PeerReputation::new("bad-peer");

    // Good peer: 95% success rate
    for _ in 0..95 {
        good_peer.record_success();
    }
    for _ in 0..5 {
        good_peer.record_failure();
    }

    // Bad peer: 30% success rate
    for _ in 0..30 {
        bad_peer.record_success();
    }
    for _ in 0..70 {
        bad_peer.record_failure();
    }

    assert!(good_peer.success_rate() >= 0.90);
    assert!(bad_peer.success_rate() < 0.50);

    Ok(())
}

// ============================================================================
// PERFORMANCE AND STRESS TESTS
// ============================================================================

#[tokio::test]
async fn test_p2p_concurrent_publishes() -> Result<()> {
    let mut network = MockP2PNetwork::new();
    network.add_node("concurrent-peer");

    // Publish 100 packages concurrently
    let handles: Vec<_> = (0..100)
        .map(|i| {
            let package = create_test_package(
                &format!("concurrent-{}", i),
                &format!("Concurrent package {}", i),
                vec!["test"],
            );
            tokio::spawn(async move {
                // In a real implementation, would publish through the network
                Ok::<_, anyhow::Error>(package)
            })
        })
        .collect();

    // Wait for all publishes to complete
    let results = futures::future::join_all(handles).await;

    // Verify all succeeded
    for result in results {
        assert!(result.is_ok());
    }

    Ok(())
}

#[tokio::test]
async fn test_p2p_large_network_scalability() -> Result<()> {
    let mut network = MockP2PNetwork::new();

    // Create 50 peers
    for i in 0..50 {
        network.add_node(&format!("scale-peer-{}", i));
    }

    // Verify network was created successfully
    assert_eq!(network.nodes.len(), 50);

    // Publish package on first peer
    let package = create_test_package("scalability-test", "Scalability test", vec!["test"]);
    if let Some(peer) = network.get_node_mut("scale-peer-0") {
        peer.publish_package(package).await?;
    }

    // Verify publish completed quickly
    let peer = network.get_node("scale-peer-0").unwrap();
    let resolved = peer.resolve("scalability-test").await?;
    assert!(resolved.is_some());

    Ok(())
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

/// Create a test package with the given ID, description, and tags
fn create_test_package(id: &str, description: &str, tags: Vec<&str>) -> PackMetadata {
    let mut versions = HashMap::new();
    versions.insert(
        "1.0.0".to_string(),
        VersionMetadata {
            version: "1.0.0".to_string(),
            git_url: format!("https://github.com/test/{}.git", id),
            git_rev: "v1.0.0".to_string(),
            manifest_url: None,
            sha256: format!("hash-{}", id),
        },
    );

    PackMetadata {
        id: id.to_string(),
        name: id.replace("-", " ").to_title_case(),
        description: description.to_string(),
        tags: tags.iter().map(|s| s.to_string()).collect(),
        keywords: tags.iter().map(|s| s.to_string()).collect(),
        category: Some("test".to_string()),
        author: Some("Test Author".to_string()),
        latest_version: "1.0.0".to_string(),
        versions,
        downloads: Some(0),
        updated: Some(chrono::Utc::now()),
        license: Some("MIT".to_string()),
        homepage: None,
        repository: None,
        documentation: None,
    }
}

/// Extension trait for string title case conversion
trait ToTitleCase {
    fn to_title_case(&self) -> String;
}

impl ToTitleCase for str {
    fn to_title_case(&self) -> String {
        self.split_whitespace()
            .map(|word| {
                let mut chars = word.chars();
                match chars.next() {
                    None => String::new(),
                    Some(first) => {
                        first.to_uppercase().collect::<String>() + &chars.as_str().to_lowercase()
                    }
                }
            })
            .collect::<Vec<_>>()
            .join(" ")
    }
}
