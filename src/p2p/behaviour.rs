//! P2P Network Behaviour Implementation
//!
//! Combines Kademlia DHT, Gossipsub, Request-Response, and mDNS into a unified behaviour

use anyhow::{Result, anyhow};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::time::{Duration, SystemTime};

use crate::p2p::types::{Package, PackageUpdate, Query, SearchResult};

/// Unified marketplace network behaviour
#[derive(Debug)]
pub struct MarketplaceBehaviour {
    /// DHT state for package discovery
    dht: DHTState,
    /// Gossipsub state for updates
    gossip: GossipState,
    /// Request-response state
    request_response: RequestResponseState,
    /// Local package cache
    package_cache: HashMap<String, Package>,
    /// Peer information
    peers: HashMap<String, PeerState>,
}

/// DHT state management
#[derive(Debug)]
struct DHTState {
    local_packages: HashSet<String>,
    provider_records: HashMap<String, Vec<ProviderRecord>>,
    pending_queries: HashMap<String, Query>,
}

#[derive(Debug, Clone)]
struct ProviderRecord {
    peer_id: String,
    timestamp: SystemTime,
    addresses: Vec<String>,
}

/// Gossipsub state management
#[derive(Debug)]
struct GossipState {
    topics: HashSet<String>,
    subscriptions: HashMap<String, HashSet<String>>, // topic -> peer_ids
    message_cache: Vec<GossipMessage>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct GossipMessage {
    id: String,
    topic: String,
    data: Vec<u8>,
    timestamp: SystemTime,
}

/// Request-response protocol state
#[derive(Debug)]
struct RequestResponseState {
    pending_requests: HashMap<String, PendingRequest>,
}

#[derive(Debug, Clone)]
struct PendingRequest {
    request_type: RequestType,
    target_peer: String,
    timestamp: SystemTime,
}

#[derive(Debug, Clone)]
enum RequestType {
    GetPackage(String),
    SearchPackages(Query),
    GetMetadata(String),
}

/// Peer state tracking
#[derive(Debug, Clone)]
struct PeerState {
    peer_id: String,
    connected: bool,
    addresses: Vec<String>,
    packages: HashSet<String>,
    last_seen: SystemTime,
}

impl MarketplaceBehaviour {
    pub fn new() -> Self {
        Self {
            dht: DHTState {
                local_packages: HashSet::new(),
                provider_records: HashMap::new(),
                pending_queries: HashMap::new(),
            },
            gossip: GossipState {
                topics: HashSet::new(),
                subscriptions: HashMap::new(),
                message_cache: Vec::new(),
            },
            request_response: RequestResponseState {
                pending_requests: HashMap::new(),
            },
            package_cache: HashMap::new(),
            peers: HashMap::new(),
        }
    }

    /// Publish a package to the network
    pub fn publish_package(&mut self, package: Package) -> Result<()> {
        // Add to local cache
        let package_id = package.id.clone();
        self.package_cache.insert(package_id.clone(), package.clone());

        // Register as provider in DHT
        self.dht.local_packages.insert(package_id.clone());

        // Announce via gossipsub
        let update = PackageUpdate {
            package_id: package_id.clone(),
            version: package.version.clone(),
            update_type: crate::p2p::types::UpdateType::NewPackage,
            timestamp: SystemTime::now()
                .duration_since(SystemTime::UNIX_EPOCH)
                .map_err(|e| anyhow!("Time error: {}", e))?
                .as_secs() as i64,
        };

        self.gossip_announce(update)?;

        Ok(())
    }

    /// Search for packages using DHT
    pub fn search_packages(&mut self, query: Query) -> Result<Vec<SearchResult>> {
        let query_id = uuid::Uuid::new_v4().to_string();
        self.dht.pending_queries.insert(query_id.clone(), query.clone());

        // Search local cache first
        let mut results: Vec<SearchResult> = self.package_cache
            .values()
            .filter(|pkg| pkg.matches_query(&query))
            .map(|pkg| SearchResult {
                package: pkg.clone(),
                score: pkg.calculate_score(&query),
                peer_count: self.count_providers(&pkg.id),
            })
            .collect();

        // Sort by score
        results.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap_or(std::cmp::Ordering::Equal));

        // Limit results
        results.truncate(query.limit);

        Ok(results)
    }

    /// Get a specific package by ID
    pub fn get_package(&self, package_id: &str) -> Option<Package> {
        self.package_cache.get(package_id).cloned()
    }

    /// Subscribe to package updates
    pub fn subscribe_to_updates(&mut self, topic: &str) -> Result<()> {
        self.gossip.topics.insert(topic.to_string());
        Ok(())
    }

    /// Handle incoming gossipsub message
    pub fn handle_gossip_message(&mut self, topic: &str, data: Vec<u8>) -> Result<()> {
        // Parse as PackageUpdate
        let update: PackageUpdate = serde_json::from_slice(&data)
            .map_err(|e| anyhow!("Failed to parse update: {}", e))?;

        // Process update based on type
        match update.update_type {
            crate::p2p::types::UpdateType::NewPackage => {
                // Fetch package details from provider
                self.request_package(&update.package_id)?;
            }
            crate::p2p::types::UpdateType::NewVersion => {
                // Update cached package
                if let Some(pkg) = self.package_cache.get_mut(&update.package_id) {
                    pkg.version = update.version;
                    pkg.updated_at = update.timestamp;
                }
            }
            _ => {}
        }

        Ok(())
    }

    /// Add a peer to the network
    pub fn add_peer(&mut self, peer_id: String, addresses: Vec<String>) {
        let peer_state = PeerState {
            peer_id: peer_id.clone(),
            connected: true,
            addresses,
            packages: HashSet::new(),
            last_seen: SystemTime::now(),
        };
        self.peers.insert(peer_id, peer_state);
    }

    /// Remove a peer from the network
    pub fn remove_peer(&mut self, peer_id: &str) {
        if let Some(peer) = self.peers.get_mut(peer_id) {
            peer.connected = false;
        }
    }

    /// Register a package provider
    pub fn register_provider(&mut self, package_id: String, peer_id: String, addresses: Vec<String>) {
        let record = ProviderRecord {
            peer_id: peer_id.clone(),
            timestamp: SystemTime::now(),
            addresses,
        };

        self.dht.provider_records
            .entry(package_id.clone())
            .or_insert_with(Vec::new)
            .push(record);

        // Update peer state
        if let Some(peer) = self.peers.get_mut(&peer_id) {
            peer.packages.insert(package_id);
        }
    }

    /// Get providers for a package
    pub fn get_providers(&self, package_id: &str) -> Vec<ProviderRecord> {
        self.dht.provider_records
            .get(package_id)
            .cloned()
            .unwrap_or_default()
    }

    /// Count providers for a package
    fn count_providers(&self, package_id: &str) -> usize {
        self.dht.provider_records
            .get(package_id)
            .map(|records| records.len())
            .unwrap_or(0)
    }

    /// Announce package update via gossipsub
    fn gossip_announce(&mut self, update: PackageUpdate) -> Result<()> {
        let topic = format!("ggen-marketplace/updates");
        let data = serde_json::to_vec(&update)
            .map_err(|e| anyhow!("Failed to serialize update: {}", e))?;

        let message = GossipMessage {
            id: uuid::Uuid::new_v4().to_string(),
            topic: topic.clone(),
            data,
            timestamp: SystemTime::now(),
        };

        self.gossip.message_cache.push(message);

        Ok(())
    }

    /// Request a package from the network
    fn request_package(&mut self, package_id: &str) -> Result<()> {
        // Find providers
        let providers = self.get_providers(package_id);

        if providers.is_empty() {
            return Err(anyhow!("No providers found for package: {}", package_id));
        }

        // Request from first provider
        let provider = &providers[0];
        let request = PendingRequest {
            request_type: RequestType::GetPackage(package_id.to_string()),
            target_peer: provider.peer_id.clone(),
            timestamp: SystemTime::now(),
        };

        let request_id = uuid::Uuid::new_v4().to_string();
        self.request_response.pending_requests.insert(request_id, request);

        Ok(())
    }

    /// Get network statistics
    pub fn get_stats(&self) -> NetworkStats {
        NetworkStats {
            connected_peers: self.peers.values().filter(|p| p.connected).count(),
            total_peers: self.peers.len(),
            cached_packages: self.package_cache.len(),
            provider_records: self.dht.provider_records.len(),
            gossip_topics: self.gossip.topics.len(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NetworkStats {
    pub connected_peers: usize,
    pub total_peers: usize,
    pub cached_packages: usize,
    pub provider_records: usize,
    pub gossip_topics: usize,
}

impl Default for MarketplaceBehaviour {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::p2p::types::Package;

    #[test]
    fn test_publish_package() {
        let mut behaviour = MarketplaceBehaviour::new();
        let package = Package::new("test-package".to_string(), "1.0.0".to_string());

        let result = behaviour.publish_package(package.clone());
        assert!(result.is_ok());
        assert!(behaviour.package_cache.contains_key(&package.id));
    }

    #[test]
    fn test_search_packages() {
        let mut behaviour = MarketplaceBehaviour::new();
        let package = Package::new("rust-test".to_string(), "1.0.0".to_string());
        behaviour.publish_package(package).unwrap();

        let query = Query::new(vec!["rust".to_string()]);
        let results = behaviour.search_packages(query).unwrap();

        assert_eq!(results.len(), 1);
    }

    #[test]
    fn test_peer_management() {
        let mut behaviour = MarketplaceBehaviour::new();
        behaviour.add_peer(
            "peer1".to_string(),
            vec!["/ip4/127.0.0.1/tcp/4001".to_string()],
        );

        assert_eq!(behaviour.peers.len(), 1);
        assert!(behaviour.peers.get("peer1").unwrap().connected);

        behaviour.remove_peer("peer1");
        assert!(!behaviour.peers.get("peer1").unwrap().connected);
    }
}
