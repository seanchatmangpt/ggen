//! P2P Network Configuration

use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use std::time::Duration;

/// P2P Network Configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct P2PConfig {
    pub network: NetworkConfig,
    pub security: SecurityConfig,
    pub bootstrap: BootstrapConfig,
    pub dht: DHTConfig,
    pub gossipsub: GossipsubConfig,
}

/// Network transport configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NetworkConfig {
    pub listen_addresses: Vec<String>,
    pub protocols: Vec<Protocol>,
    pub max_connections: usize,
    pub connection_timeout: u64,
    pub idle_timeout: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Protocol {
    TCP,
    QUIC,
    WebSocket,
}

/// Security configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SecurityConfig {
    pub noise_enabled: bool,
    pub key_type: KeyType,
    pub identity_path: Option<PathBuf>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum KeyType {
    Ed25519,
    Secp256k1,
    RSA,
}

/// Bootstrap node configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BootstrapConfig {
    pub enabled: bool,
    pub nodes: Vec<BootstrapNode>,
    pub mdns_enabled: bool,
    pub retry_interval: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BootstrapNode {
    pub peer_id: String,
    pub address: String,
}

/// Kademlia DHT configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DHTConfig {
    pub protocol_name: String,
    pub replication_factor: usize,
    pub query_timeout: u64,
    pub provider_record_ttl: u64,
    pub cache_size: usize,
}

/// Gossipsub configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GossipsubConfig {
    pub heartbeat_interval: u64,
    pub history_length: usize,
    pub history_gossip: usize,
    pub mesh_n: usize,
    pub mesh_n_low: usize,
    pub mesh_n_high: usize,
    pub topic_prefix: String,
}

impl Default for P2PConfig {
    fn default() -> Self {
        Self {
            network: NetworkConfig::default(),
            security: SecurityConfig::default(),
            bootstrap: BootstrapConfig::default(),
            dht: DHTConfig::default(),
            gossipsub: GossipsubConfig::default(),
        }
    }
}

impl Default for NetworkConfig {
    fn default() -> Self {
        Self {
            listen_addresses: vec![
                "/ip4/0.0.0.0/tcp/0".to_string(),
                "/ip4/0.0.0.0/udp/0/quic-v1".to_string(),
            ],
            protocols: vec![Protocol::TCP, Protocol::QUIC],
            max_connections: 100,
            connection_timeout: 30,
            idle_timeout: 300,
        }
    }
}

impl Default for SecurityConfig {
    fn default() -> Self {
        Self {
            noise_enabled: true,
            key_type: KeyType::Ed25519,
            identity_path: None,
        }
    }
}

impl Default for BootstrapConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            nodes: Vec::new(),
            mdns_enabled: true,
            retry_interval: 300, // 5 minutes
        }
    }
}

impl Default for DHTConfig {
    fn default() -> Self {
        Self {
            protocol_name: "/ggen/marketplace/1.0.0".to_string(),
            replication_factor: 20,
            query_timeout: 60,
            provider_record_ttl: 86400, // 24 hours
            cache_size: 1000,
        }
    }
}

impl Default for GossipsubConfig {
    fn default() -> Self {
        Self {
            heartbeat_interval: 1000, // 1 second
            history_length: 5,
            history_gossip: 3,
            mesh_n: 6,
            mesh_n_low: 4,
            mesh_n_high: 12,
            topic_prefix: "ggen-marketplace".to_string(),
        }
    }
}

impl P2PConfig {
    pub fn with_bootstrap_nodes(mut self, nodes: Vec<BootstrapNode>) -> Self {
        self.bootstrap.nodes = nodes;
        self
    }

    pub fn with_listen_addresses(mut self, addresses: Vec<String>) -> Self {
        self.network.listen_addresses = addresses;
        self
    }

    pub fn with_identity_path(mut self, path: PathBuf) -> Self {
        self.security.identity_path = Some(path);
        self
    }

    pub fn connection_timeout(&self) -> Duration {
        Duration::from_secs(self.network.connection_timeout)
    }

    pub fn idle_timeout(&self) -> Duration {
        Duration::from_secs(self.network.idle_timeout)
    }

    pub fn query_timeout(&self) -> Duration {
        Duration::from_secs(self.dht.query_timeout)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = P2PConfig::default();
        assert!(config.security.noise_enabled);
        assert!(config.bootstrap.mdns_enabled);
        assert_eq!(config.network.protocols.len(), 2);
    }

    #[test]
    fn test_config_builder() {
        let nodes = vec![BootstrapNode {
            peer_id: "12D3KooWTest".to_string(),
            address: "/ip4/127.0.0.1/tcp/4001".to_string(),
        }];

        let config = P2PConfig::default()
            .with_bootstrap_nodes(nodes.clone())
            .with_listen_addresses(vec!["/ip4/0.0.0.0/tcp/4001".to_string()]);

        assert_eq!(config.bootstrap.nodes.len(), 1);
        assert_eq!(config.network.listen_addresses.len(), 1);
    }
}
