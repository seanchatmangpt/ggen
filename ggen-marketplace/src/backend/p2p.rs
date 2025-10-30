//! P2P Registry implementation using libp2p
//!
//! This module provides a decentralized package registry using libp2p for:
//! - Peer discovery via Kademlia DHT
//! - Package announcements via Gossipsub
//! - Content-addressed package storage
//! - Peer reputation tracking

use crate::error::{MarketplaceError, Result};
use crate::models::{Package, PackageId, Query, RegistryMetadata};
use crate::traits::Registry;
use async_trait::async_trait;
use futures::StreamExt;
use libp2p::{
    gossipsub, identify, kad,
    swarm::{NetworkBehaviour, SwarmEvent},
    Multiaddr, PeerId, Swarm,
};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use tokio::sync::RwLock;

/// P2P network behavior combining Kademlia DHT, Gossipsub, and Identify
#[derive(NetworkBehaviour)]
pub struct P2PBehaviour {
    pub kademlia: kad::Behaviour<kad::store::MemoryStore>,
    pub gossipsub: gossipsub::Behaviour,
    pub identify: identify::Behaviour,
}

/// Configuration for P2P registry
#[derive(Debug, Clone)]
pub struct P2PConfig {
    /// Bootstrap nodes for initial peer discovery
    pub bootstrap_nodes: Vec<Multiaddr>,
    /// Gossipsub topic for package announcements
    pub packages_topic: String,
    /// Enable DHT server mode
    pub dht_server_mode: bool,
    /// Local listen addresses
    pub listen_addresses: Vec<Multiaddr>,
}

impl Default for P2PConfig {
    fn default() -> Self {
        Self {
            bootstrap_nodes: Vec::new(),
            packages_topic: "/ggen/packages/v1".to_string(),
            dht_server_mode: true,
            listen_addresses: vec![
                "/ip4/0.0.0.0/tcp/0"
                    .parse()
                    .map_err(|e| anyhow::anyhow!("Failed to parse multiaddr: {}", e))
                    .expect("Failed to parse default listen address"),
            ],
        }
    }
}

/// Peer reputation information
#[derive(Debug, Clone)]
struct PeerReputation {
    peer_id: PeerId,
    successful_retrievals: u64,
    failed_retrievals: u64,
    last_seen: chrono::DateTime<chrono::Utc>,
}

impl PeerReputation {
    fn new(peer_id: PeerId) -> Self {
        Self {
            peer_id,
            successful_retrievals: 0,
            failed_retrievals: 0,
            last_seen: chrono::Utc::now(),
        }
    }

    fn success_rate(&self) -> f64 {
        let total = self.successful_retrievals + self.failed_retrievals;
        if total == 0 {
            return 1.0; // No data yet, assume good
        }
        self.successful_retrievals as f64 / total as f64
    }
}

/// P2P Registry implementation
pub struct P2PRegistry {
    /// libp2p swarm managing network behavior
    swarm: Arc<RwLock<Swarm<P2PBehaviour>>>,
    /// Local peer ID
    peer_id: PeerId,
    /// Locally published packages
    local_packages: Arc<RwLock<HashMap<PackageId, Package>>>,
    /// Discovered packages and their provider peers
    discovered_packages: Arc<RwLock<HashMap<PackageId, HashSet<PeerId>>>>,
    /// Peer reputation tracking
    peer_reputation: Arc<RwLock<HashMap<PeerId, PeerReputation>>>,
    /// Gossipsub topic for package announcements
    packages_topic: gossipsub::IdentTopic,
    /// Configuration
    config: P2PConfig,
}

impl P2PRegistry {
    /// Create a new P2P registry
    pub async fn new(config: P2PConfig) -> Result<Self> {
        // Generate or load local peer keypair
        let local_key = libp2p::identity::Keypair::generate_ed25519();
        let peer_id = PeerId::from(local_key.public());

        // Create Kademlia DHT
        let store = kad::store::MemoryStore::new(peer_id);
        let mut kademlia = kad::Behaviour::new(peer_id, store);

        // Set DHT mode
        if config.dht_server_mode {
            kademlia.set_mode(Some(kad::Mode::Server));
        }

        // Add bootstrap nodes to DHT
        for addr in &config.bootstrap_nodes {
            // Note: In real implementation, we'd need to extract peer ID from multiaddr
            // For now, we just add the address
            let _ = kademlia.add_address(&peer_id, addr.clone());
        }

        // Create Gossipsub
        let gossipsub_config = gossipsub::ConfigBuilder::default()
            .heartbeat_interval(std::time::Duration::from_secs(10))
            .validation_mode(gossipsub::ValidationMode::Strict)
            .build()
            .map_err(|e| MarketplaceError::network_error(format!("Gossipsub config error: {}", e)))?;

        let gossipsub = gossipsub::Behaviour::new(
            gossipsub::MessageAuthenticity::Signed(local_key.clone()),
            gossipsub_config,
        )
        .map_err(|e| MarketplaceError::network_error(format!("Failed to create gossipsub: {}", e)))?;

        // Create Identify protocol
        let identify = identify::Behaviour::new(identify::Config::new(
            "/ggen/1.0.0".to_string(),
            local_key.public(),
        ));

        // Combine behaviors
        let behaviour = P2PBehaviour {
            kademlia,
            gossipsub,
            identify,
        };

        // Create swarm
        let swarm = libp2p::SwarmBuilder::with_existing_identity(local_key)
            .with_tokio()
            .with_tcp(
                libp2p::tcp::Config::default(),
                libp2p::noise::Config::new,
                libp2p::yamux::Config::default,
            )
            .map_err(|e| MarketplaceError::network_error(format!("Failed to configure TCP: {}", e)))?
            .with_behaviour(|_| behaviour)
            .map_err(|e| MarketplaceError::network_error(format!("Failed to create behavior: {}", e)))?
            .build();

        let packages_topic = gossipsub::IdentTopic::new(&config.packages_topic);

        Ok(Self {
            swarm: Arc::new(RwLock::new(swarm)),
            peer_id,
            local_packages: Arc::new(RwLock::new(HashMap::new())),
            discovered_packages: Arc::new(RwLock::new(HashMap::new())),
            peer_reputation: Arc::new(RwLock::new(HashMap::new())),
            packages_topic,
            config,
        })
    }

    /// Start listening on configured addresses
    pub async fn start_listening(&self) -> Result<()> {
        let mut swarm = self.swarm.write().await;
        for addr in &self.config.listen_addresses {
            swarm
                .listen_on(addr.clone())
                .map_err(|e| MarketplaceError::network_error(format!("Failed to listen: {}", e)))?;
        }
        Ok(())
    }

    /// Subscribe to package announcements topic
    pub async fn subscribe_to_packages(&self) -> Result<()> {
        let mut swarm = self.swarm.write().await;
        swarm
            .behaviour_mut()
            .gossipsub
            .subscribe(&self.packages_topic)
            .map_err(|e| MarketplaceError::network_error(format!("Failed to subscribe: {}", e)))?;
        Ok(())
    }

    /// Bootstrap DHT by connecting to bootstrap nodes
    pub async fn bootstrap(&self) -> Result<()> {
        let mut swarm = self.swarm.write().await;
        swarm
            .behaviour_mut()
            .kademlia
            .bootstrap()
            .map_err(|e| MarketplaceError::network_error(format!("Bootstrap failed: {}", e)))?;
        Ok(())
    }

    /// Announce a package to the network
    async fn announce_package(&self, package: &Package) -> Result<()> {
        let announcement = serde_json::to_vec(package)
            .map_err(|e| MarketplaceError::serialization_error(e))?;

        let mut swarm = self.swarm.write().await;
        swarm
            .behaviour_mut()
            .gossipsub
            .publish(self.packages_topic.clone(), announcement)
            .map_err(|e| MarketplaceError::network_error(format!("Failed to publish: {}", e)))?;

        Ok(())
    }

    /// Store package metadata in DHT
    async fn store_in_dht(&self, package_id: &PackageId, package: &Package) -> Result<()> {
        let key = kad::RecordKey::new(&package_id.to_string().as_bytes());
        let value = serde_json::to_vec(package)
            .map_err(|e| MarketplaceError::serialization_error(e))?;

        let record = kad::Record {
            key,
            value,
            publisher: Some(self.peer_id),
            expires: None, // Permanent storage
        };

        let mut swarm = self.swarm.write().await;
        swarm
            .behaviour_mut()
            .kademlia
            .put_record(record, kad::Quorum::One)
            .map_err(|e| MarketplaceError::network_error(format!("DHT put failed: {}", e)))?;

        Ok(())
    }

    /// Query DHT for package metadata
    async fn query_dht(&self, package_id: &PackageId) -> Result<Option<Package>> {
        let key = kad::RecordKey::new(&package_id.to_string().as_bytes());

        let mut swarm = self.swarm.write().await;
        swarm.behaviour_mut().kademlia.get_record(key);

        // Note: In real implementation, we'd need to wait for the query result
        // via swarm events. For now, return None as placeholder.
        Ok(None)
    }

    /// Track successful retrieval from peer
    async fn record_peer_success(&self, peer_id: PeerId) {
        let mut reputation = self.peer_reputation.write().await;
        let entry = reputation
            .entry(peer_id)
            .or_insert_with(|| PeerReputation::new(peer_id));
        entry.successful_retrievals += 1;
        entry.last_seen = chrono::Utc::now();
    }

    /// Track failed retrieval from peer
    async fn record_peer_failure(&self, peer_id: PeerId) {
        let mut reputation = self.peer_reputation.write().await;
        let entry = reputation
            .entry(peer_id)
            .or_insert_with(|| PeerReputation::new(peer_id));
        entry.failed_retrievals += 1;
        entry.last_seen = chrono::Utc::now();
    }

    /// Get peer reputation score (0.0 to 1.0)
    pub async fn get_peer_reputation(&self, peer_id: &PeerId) -> f64 {
        let reputation = self.peer_reputation.read().await;
        reputation
            .get(peer_id)
            .map(|r| r.success_rate())
            .unwrap_or(1.0)
    }

    /// Process network events (should be called in a loop)
    pub async fn process_events(&self) {
        let mut swarm = self.swarm.write().await;

        if let Some(event) = swarm.next().now_or_never() {
            if let Some(event) = event {
                match event {
                    SwarmEvent::Behaviour(event) => {
                        // Handle behavior events
                        // In real implementation, process Kademlia, Gossipsub, and Identify events
                    }
                    SwarmEvent::ConnectionEstablished { peer_id, .. } => {
                        // Track new peer connection
                        let mut reputation = self.peer_reputation.write().await;
                        reputation
                            .entry(peer_id)
                            .or_insert_with(|| PeerReputation::new(peer_id));
                    }
                    _ => {}
                }
            }
        }
    }
}

#[async_trait]
impl Registry for P2PRegistry {
    async fn search(&self, query: &Query) -> Result<Vec<Package>> {
        // Search locally first
        let local_packages = self.local_packages.read().await;
        let mut results: Vec<Package> = local_packages
            .values()
            .filter(|package| {
                // Simple text matching
                let text_match = query.text.is_empty()
                    || package.metadata.title.to_lowercase().contains(&query.text.to_lowercase())
                    || package
                        .metadata
                        .description
                        .to_lowercase()
                        .contains(&query.text.to_lowercase());

                // Category filtering
                let category_match = query.categories.is_empty()
                    || package
                        .metadata
                        .categories
                        .iter()
                        .any(|c| query.categories.contains(c));

                // Tag filtering
                let tag_match = query.tags.is_empty()
                    || package
                        .metadata
                        .tags
                        .iter()
                        .any(|t| query.tags.contains(t));

                text_match && category_match && tag_match
            })
            .cloned()
            .collect();

        // TODO: Query DHT for additional results from remote peers

        // Apply limit if specified
        if let Some(limit) = query.limit {
            results.truncate(limit);
        }

        Ok(results)
    }

    async fn get_package(&self, id: &PackageId) -> Result<Package> {
        // Check local cache first
        {
            let local_packages = self.local_packages.read().await;
            if let Some(package) = local_packages.get(id) {
                return Ok(package.clone());
            }
        }

        // Query DHT for package
        if let Some(package) = self.query_dht(id).await? {
            // Cache locally
            self.local_packages.write().await.insert(id.clone(), package.clone());
            return Ok(package);
        }

        Err(MarketplaceError::package_not_found(
            id.to_string(),
            "p2p registry",
        ))
    }

    async fn get_package_version(&self, id: &PackageId, version: &str) -> Result<Package> {
        let package = self.get_package(id).await?;

        // Check if version matches
        if package.version.to_string() == version {
            Ok(package)
        } else {
            Err(MarketplaceError::package_not_found(
                format!("{}@{}", id, version),
                "p2p registry",
            ))
        }
    }

    async fn list_versions(&self, id: &PackageId) -> Result<Vec<Package>> {
        // In a real P2P implementation, we'd query DHT for all versions
        // For now, return single version if exists
        match self.get_package(id).await {
            Ok(package) => Ok(vec![package]),
            Err(_) => Ok(Vec::new()),
        }
    }

    async fn publish(&self, package: Package) -> Result<()> {
        let package_id = package.id.clone();

        // Store locally
        self.local_packages
            .write()
            .await
            .insert(package_id.clone(), package.clone());

        // Store in DHT
        self.store_in_dht(&package_id, &package).await?;

        // Announce to network via gossipsub
        self.announce_package(&package).await?;

        Ok(())
    }

    async fn delete(&self, id: &PackageId, _version: &str) -> Result<()> {
        // Remove from local cache
        self.local_packages.write().await.remove(id);

        // Note: Cannot truly delete from DHT (immutable records)
        // but we stop serving it locally

        Ok(())
    }

    async fn exists(&self, id: &PackageId) -> Result<bool> {
        // Check local cache
        {
            let local_packages = self.local_packages.read().await;
            if local_packages.contains_key(id) {
                return Ok(true);
            }
        }

        // Check DHT
        Ok(self.query_dht(id).await?.is_some())
    }

    async fn metadata(&self) -> Result<RegistryMetadata> {
        Ok(RegistryMetadata {
            name: "Ggen P2P Registry".to_string(),
            url: format!("/p2p/{}", self.peer_id),
            description: Some("Decentralized P2P package registry using libp2p".to_string()),
            supports_publish: true,
            requires_auth: false,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_p2p_config_default() {
        let config = P2PConfig::default();
        assert_eq!(config.packages_topic, "/ggen/packages/v1");
        assert!(config.dht_server_mode);
        assert_eq!(config.bootstrap_nodes.len(), 0);
    }

    #[tokio::test]
    async fn test_peer_reputation_new() {
        let keypair = libp2p::identity::Keypair::generate_ed25519();
        let peer_id = PeerId::from(keypair.public());
        let reputation = PeerReputation::new(peer_id);

        assert_eq!(reputation.successful_retrievals, 0);
        assert_eq!(reputation.failed_retrievals, 0);
        assert_eq!(reputation.success_rate(), 1.0);
    }

    #[tokio::test]
    async fn test_peer_reputation_success_rate() {
        let keypair = libp2p::identity::Keypair::generate_ed25519();
        let peer_id = PeerId::from(keypair.public());
        let mut reputation = PeerReputation::new(peer_id);

        reputation.successful_retrievals = 8;
        reputation.failed_retrievals = 2;

        assert_eq!(reputation.success_rate(), 0.8);
    }
}
