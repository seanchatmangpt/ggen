//! P2P Marketplace Registry Implementation

use anyhow::{Result, anyhow};
use async_trait::async_trait;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::RwLock;

use crate::p2p::behaviour::{MarketplaceBehaviour, NetworkStats};
use crate::p2p::config::{P2PConfig, BootstrapNode};
use crate::p2p::types::{Package, Query, SearchResult};

/// Trait for registry operations
#[async_trait]
pub trait Registry: Send + Sync {
    /// Search for packages matching the query
    async fn search(&self, query: &Query) -> Result<Vec<SearchResult>>;

    /// Publish a package to the registry
    async fn publish(&self, package: Package) -> Result<()>;

    /// Get a specific package by ID
    async fn get(&self, package_id: &str) -> Result<Option<Package>>;

    /// Subscribe to package updates
    async fn subscribe(&self, topic: &str) -> Result<()>;
}

/// P2P-based marketplace registry
pub struct P2PRegistry {
    config: P2PConfig,
    behaviour: Arc<RwLock<MarketplaceBehaviour>>,
    running: Arc<RwLock<bool>>,
}

impl P2PRegistry {
    /// Create a new P2P registry with default configuration
    pub fn new(config: P2PConfig) -> Result<Self> {
        let behaviour = Arc::new(RwLock::new(MarketplaceBehaviour::new()));
        let running = Arc::new(RwLock::new(false));

        Ok(Self {
            config,
            behaviour,
            running,
        })
    }

    /// Start the P2P network
    pub async fn start(&self) -> Result<()> {
        let mut running = self.running.write().await;
        if *running {
            return Err(anyhow!("Registry already running"));
        }

        // Initialize network behaviour
        let mut behaviour = self.behaviour.write().await;

        // Subscribe to default topics
        behaviour.subscribe_to_updates("ggen-marketplace/updates")?;

        // Connect to bootstrap nodes
        for node in &self.config.bootstrap.nodes {
            behaviour.add_peer(node.peer_id.clone(), vec![node.address.clone()]);
        }

        *running = true;
        drop(running);

        Ok(())
    }

    /// Stop the P2P network
    pub async fn stop(&self) -> Result<()> {
        let mut running = self.running.write().await;
        if !*running {
            return Err(anyhow!("Registry not running"));
        }

        *running = false;
        Ok(())
    }

    /// Check if the registry is running
    pub async fn is_running(&self) -> bool {
        *self.running.read().await
    }

    /// Get network statistics
    pub async fn stats(&self) -> NetworkStats {
        let behaviour = self.behaviour.read().await;
        behaviour.get_stats()
    }

    /// Add a bootstrap node
    pub async fn add_bootstrap_node(&mut self, peer_id: String, address: String) {
        self.config.bootstrap.nodes.push(BootstrapNode { peer_id, address });
    }
}

#[async_trait]
impl Registry for P2PRegistry {
    async fn search(&self, query: &Query) -> Result<Vec<SearchResult>> {
        if !self.is_running().await {
            return Err(anyhow!("Registry not running"));
        }

        let mut behaviour = self.behaviour.write().await;
        behaviour.search_packages(query.clone())
    }

    async fn publish(&self, package: Package) -> Result<()> {
        if !self.is_running().await {
            return Err(anyhow!("Registry not running"));
        }

        let mut behaviour = self.behaviour.write().await;
        behaviour.publish_package(package)
    }

    async fn get(&self, package_id: &str) -> Result<Option<Package>> {
        if !self.is_running().await {
            return Err(anyhow!("Registry not running"));
        }

        let behaviour = self.behaviour.read().await;
        Ok(behaviour.get_package(package_id))
    }

    async fn subscribe(&self, topic: &str) -> Result<()> {
        if !self.is_running().await {
            return Err(anyhow!("Registry not running"));
        }

        let mut behaviour = self.behaviour.write().await;
        behaviour.subscribe_to_updates(topic)
    }
}

/// Builder for P2PRegistry
pub struct P2PRegistryBuilder {
    config: P2PConfig,
}

impl P2PRegistryBuilder {
    pub fn new() -> Self {
        Self {
            config: P2PConfig::default(),
        }
    }

    pub fn with_config(mut self, config: P2PConfig) -> Self {
        self.config = config;
        self
    }

    pub fn with_bootstrap_nodes(mut self, nodes: Vec<BootstrapNode>) -> Self {
        self.config = self.config.with_bootstrap_nodes(nodes);
        self
    }

    pub fn with_listen_addresses(mut self, addresses: Vec<String>) -> Self {
        self.config = self.config.with_listen_addresses(addresses);
        self
    }

    pub fn with_identity_path(mut self, path: PathBuf) -> Self {
        self.config = self.config.with_identity_path(path);
        self
    }

    pub fn build(self) -> Result<P2PRegistry> {
        P2PRegistry::new(self.config)
    }
}

impl Default for P2PRegistryBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_registry_creation() {
        let registry = P2PRegistryBuilder::new()
            .with_listen_addresses(vec!["/ip4/127.0.0.1/tcp/0".to_string()])
            .build();

        assert!(registry.is_ok());
    }

    #[tokio::test]
    async fn test_registry_start_stop() {
        let registry = P2PRegistryBuilder::new().build().unwrap();

        assert!(!registry.is_running().await);

        let result = registry.start().await;
        assert!(result.is_ok());
        assert!(registry.is_running().await);

        let result = registry.stop().await;
        assert!(result.is_ok());
        assert!(!registry.is_running().await);
    }

    #[tokio::test]
    async fn test_publish_and_search() {
        let registry = P2PRegistryBuilder::new().build().unwrap();
        registry.start().await.unwrap();

        let package = Package::new("test-package".to_string(), "1.0.0".to_string());
        let result = registry.publish(package.clone()).await;
        assert!(result.is_ok());

        let query = Query::new(vec!["test".to_string()]);
        let results = registry.search(&query).await.unwrap();
        assert_eq!(results.len(), 1);

        registry.stop().await.unwrap();
    }
}
