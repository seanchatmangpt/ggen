//! Peer Discovery Implementation

use anyhow::{Result, anyhow};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::time::{Duration, SystemTime};

use crate::p2p::config::BootstrapConfig;

/// Peer discovery service
#[derive(Debug)]
pub struct PeerDiscovery {
    config: BootstrapConfig,
    discovered_peers: HashMap<String, DiscoveredPeer>,
    mdns_enabled: bool,
}

#[derive(Debug, Clone)]
pub struct DiscoveredPeer {
    pub peer_id: String,
    pub addresses: Vec<String>,
    pub discovery_method: DiscoveryMethod,
    pub discovered_at: SystemTime,
    pub last_seen: SystemTime,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum DiscoveryMethod {
    Bootstrap,
    MDNS,
    DHT,
    Manual,
}

impl PeerDiscovery {
    pub fn new(config: BootstrapConfig) -> Self {
        Self {
            mdns_enabled: config.mdns_enabled,
            config,
            discovered_peers: HashMap::new(),
        }
    }

    /// Discover peers using configured methods
    pub async fn discover(&mut self) -> Result<Vec<DiscoveredPeer>> {
        let mut peers = Vec::new();

        // Discover via bootstrap nodes
        if self.config.enabled {
            for node in &self.config.nodes {
                let peer = DiscoveredPeer {
                    peer_id: node.peer_id.clone(),
                    addresses: vec![node.address.clone()],
                    discovery_method: DiscoveryMethod::Bootstrap,
                    discovered_at: SystemTime::now(),
                    last_seen: SystemTime::now(),
                };
                self.discovered_peers.insert(node.peer_id.clone(), peer.clone());
                peers.push(peer);
            }
        }

        // Discover via mDNS (simulated for now)
        if self.mdns_enabled {
            // In a real implementation, this would use libp2p's mDNS
            // For now, we'll return the discovered peers
        }

        Ok(peers)
    }

    /// Add a manually discovered peer
    pub fn add_peer(&mut self, peer_id: String, addresses: Vec<String>) {
        let peer = DiscoveredPeer {
            peer_id: peer_id.clone(),
            addresses,
            discovery_method: DiscoveryMethod::Manual,
            discovered_at: SystemTime::now(),
            last_seen: SystemTime::now(),
        };
        self.discovered_peers.insert(peer_id, peer);
    }

    /// Update last seen time for a peer
    pub fn update_peer(&mut self, peer_id: &str) -> Result<()> {
        if let Some(peer) = self.discovered_peers.get_mut(peer_id) {
            peer.last_seen = SystemTime::now();
            Ok(())
        } else {
            Err(anyhow!("Peer not found: {}", peer_id))
        }
    }

    /// Remove stale peers (not seen for a while)
    pub fn remove_stale_peers(&mut self, max_age: Duration) {
        let now = SystemTime::now();
        self.discovered_peers.retain(|_, peer| {
            now.duration_since(peer.last_seen).unwrap_or(Duration::from_secs(0)) < max_age
        });
    }

    /// Get all discovered peers
    pub fn get_peers(&self) -> Vec<DiscoveredPeer> {
        self.discovered_peers.values().cloned().collect()
    }

    /// Get peer by ID
    pub fn get_peer(&self, peer_id: &str) -> Option<&DiscoveredPeer> {
        self.discovered_peers.get(peer_id)
    }

    /// Get number of discovered peers
    pub fn peer_count(&self) -> usize {
        self.discovered_peers.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::p2p::config::BootstrapNode;

    #[tokio::test]
    async fn test_peer_discovery() {
        let config = BootstrapConfig {
            enabled: true,
            nodes: vec![BootstrapNode {
                peer_id: "peer1".to_string(),
                address: "/ip4/127.0.0.1/tcp/4001".to_string(),
            }],
            mdns_enabled: false,
            retry_interval: 300,
        };

        let mut discovery = PeerDiscovery::new(config);
        let peers = discovery.discover().await.unwrap();

        assert_eq!(peers.len(), 1);
        assert_eq!(peers[0].peer_id, "peer1");
    }

    #[test]
    fn test_add_peer() {
        let config = BootstrapConfig::default();
        let mut discovery = PeerDiscovery::new(config);

        discovery.add_peer(
            "peer2".to_string(),
            vec!["/ip4/127.0.0.1/tcp/4002".to_string()],
        );

        assert_eq!(discovery.peer_count(), 1);
        assert!(discovery.get_peer("peer2").is_some());
    }

    #[test]
    fn test_remove_stale_peers() {
        let config = BootstrapConfig::default();
        let mut discovery = PeerDiscovery::new(config);

        discovery.add_peer("peer1".to_string(), vec![]);

        // Remove peers older than 0 seconds (immediately stale)
        discovery.remove_stale_peers(Duration::from_secs(0));

        assert_eq!(discovery.peer_count(), 0);
    }
}
