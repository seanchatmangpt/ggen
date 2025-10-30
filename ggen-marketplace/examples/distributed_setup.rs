//! Example of setting up a distributed marketplace with replication
//!
//! This example demonstrates:
//! - Distributed registry with Raft consensus
//! - Multiple storage nodes
//! - P2P package discovery
//! - Replication and failover

#[cfg(all(feature = "replication", feature = "p2p"))]
use ggen_marketplace::prelude::*;

#[cfg(all(feature = "replication", feature = "p2p"))]
use anyhow::Result;

#[cfg(all(feature = "replication", feature = "p2p"))]
#[tokio::main]
async fn main() -> Result<()> {
    println!("=== Distributed Marketplace Setup ===\n");

    // Node configuration
    let node_id = "node-1";
    let peer_addresses = vec![
        PeerInfo::new("node-1", "10.0.0.1:5000"),
        PeerInfo::new("node-2", "10.0.0.2:5000"),
        PeerInfo::new("node-3", "10.0.0.3:5000"),
    ];

    // Create distributed registry with Raft consensus
    let replication = RaftReplication::new();
    replication.initialize(node_id, peer_addresses.clone()).await?;

    let distributed_registry = DistributedRegistry::builder()
        .database_url("postgresql://localhost/marketplace")
        .replication(replication)
        .cache_size(10_000)
        .build()
        .await?;

    println!("✓ Distributed registry initialized");

    // Wait for leader election
    println!("⏳ Waiting for leader election...");
    loop {
        let role = distributed_registry.replication().role().await?;
        match role {
            NodeRole::Leader => {
                println!("✓ This node is the LEADER");
                break;
            }
            NodeRole::Follower => {
                println!("  This node is a follower");
                tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
            }
            NodeRole::Candidate => {
                println!("  This node is a candidate");
                tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
            }
        }
    }

    // Set up P2P networking for package discovery
    let p2p_network = P2PNetwork::builder()
        .listen_on("/ip4/0.0.0.0/tcp/6000")
        .bootstrap_peers(peer_addresses)
        .enable_mdns() // Local network discovery
        .enable_kad() // Kademlia DHT
        .enable_gossipsub() // Pub/sub for package announcements
        .build()
        .await?;

    println!("✓ P2P network initialized");

    // Create storage with replication
    let primary_store = FilePackageStore::new("./data/packages").await?;
    let backup_store = S3PackageStore::new(
        "marketplace-backup",
        "us-east-1",
        get_aws_credentials(),
    ).await?;

    let replicated_store = ReplicatedStore::new(primary_store, vec![backup_store]);

    println!("✓ Replicated storage initialized");

    // Build the marketplace
    let marketplace = MarketplaceBuilder::new()
        .registry(distributed_registry)
        .storage(replicated_store)
        .search(DistributedSearchEngine::new(peer_addresses))
        .crypto(Ed25519CryptoVerifier::new())
        .p2p(p2p_network)
        .build()
        .await?;

    println!("✓ Distributed marketplace ready\n");

    // Test distributed operations
    println!("=== Testing Distributed Operations ===\n");

    // Publish a package (will be replicated)
    let package_metadata = create_test_package();
    let package_bytes = create_test_package_bytes();

    println!("Publishing package...");
    let package_id = marketplace.publish(&package_metadata, package_bytes).await?;
    println!("✓ Package published: {} v{}", package_id.name, package_id.version);

    // Wait for replication
    println!("⏳ Waiting for replication...");
    tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;

    // Verify replication status
    let cluster_status = marketplace.registry().cluster_status().await?;
    println!("\nCluster Status:");
    println!("  Members: {}", cluster_status.member_count);
    println!("  Healthy: {}", cluster_status.is_healthy);
    println!("  Leader: {}", cluster_status.leader_id.unwrap_or_else(|| "None".to_string()));

    // Check replication lag
    let lag = marketplace.registry().replication_lag().await?;
    println!("  Replication lag: {:?}", lag);

    // Test P2P package discovery
    println!("\n=== Testing P2P Discovery ===\n");

    // Announce package on P2P network
    marketplace.p2p().announce_package(&package_id).await?;
    println!("✓ Package announced on P2P network");

    // Discover peers with the package
    let peers = marketplace.p2p().find_providers(&package_id).await?;
    println!("✓ Found {} peers with the package", peers.len());

    // Test failover
    println!("\n=== Testing Failover ===\n");

    // Simulate primary storage failure
    println!("Simulating primary storage failure...");
    marketplace.storage().set_primary_failed(true).await?;

    // Download should still work via backup
    let downloaded = marketplace.download(&package_id).await?;
    println!("✓ Downloaded from backup storage: {} bytes", downloaded.len());

    // Monitor cluster health
    println!("\n=== Monitoring Cluster ===\n");

    for i in 0..5 {
        let health = marketplace.health_check().await?;

        println!("Health check #{}", i + 1);
        println!("  Registry: {}", if health.registry_healthy { "OK" } else { "FAILED" });
        println!("  Storage: {}", if health.storage_healthy { "OK" } else { "FAILED" });
        println!("  Search: {}", if health.search_healthy { "OK" } else { "FAILED" });
        println!("  P2P: {} peers connected", health.p2p_peer_count);
        println!();

        tokio::time::sleep(tokio::time::Duration::from_secs(5)).await;
    }

    println!("✓ Distributed setup example completed successfully!");

    Ok(())
}

#[cfg(all(feature = "replication", feature = "p2p"))]
fn create_test_package() -> PackageMetadata {
    PackageMetadata {
        id: PackageId {
            name: "distributed-test".to_string(),
            version: semver::Version::parse("1.0.0").unwrap(),
            namespace: None,
        },
        description: "Test package for distributed setup".to_string(),
        authors: vec!["test@example.com".to_string()],
        license: "MIT".to_string(),
        repository: None,
        homepage: None,
        keywords: vec!["test".to_string(), "distributed".to_string()],
        categories: vec!["testing".to_string()],
        dependencies: std::collections::HashMap::new(),
        checksums: Checksums::default(),
        signature: Signature::default(),
        published_at: chrono::Utc::now(),
    }
}

#[cfg(all(feature = "replication", feature = "p2p"))]
fn create_test_package_bytes() -> bytes::Bytes {
    bytes::Bytes::from_static(b"Distributed test package content")
}

#[cfg(all(feature = "replication", feature = "p2p"))]
fn get_aws_credentials() -> aws_config::Credentials {
    // In production, use proper AWS credentials
    todo!()
}

#[cfg(not(all(feature = "replication", feature = "p2p")))]
fn main() {
    println!("This example requires the 'replication' and 'p2p' features.");
    println!("Run with: cargo run --example distributed_setup --features replication,p2p");
}
