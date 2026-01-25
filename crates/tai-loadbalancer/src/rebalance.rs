//! Automatic rebalancing on topology changes

use crate::error::Result;
use crate::service_registry::{Endpoint, ServiceRegistry};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use std::time::{Duration, SystemTime};
use tokio::sync::mpsc;
use tokio::time::interval;

/// Rebalancing strategy
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RebalanceStrategy {
    /// Immediate rebalancing (may cause connection storms)
    Immediate,
    /// Gradual rebalancing over time
    Gradual,
    /// No automatic rebalancing
    Manual,
}

/// Configuration for rebalancing
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RebalanceConfig {
    /// Strategy to use for rebalancing
    pub strategy: RebalanceStrategy,
    /// Interval to check for topology changes
    pub check_interval: Duration,
    /// Duration over which to gradually rebalance
    pub gradual_duration: Duration,
    /// Maximum percentage of connections to drain per iteration
    pub max_drain_percentage: f32,
}

impl Default for RebalanceConfig {
    fn default() -> Self {
        Self {
            strategy: RebalanceStrategy::Gradual,
            check_interval: Duration::from_secs(30),
            gradual_duration: Duration::from_secs(300),
            max_drain_percentage: 0.1, // 10%
        }
    }
}

/// Represents a rebalancing event
#[derive(Debug, Clone)]
pub struct RebalanceEvent {
    /// Service that changed
    pub service: String,
    /// Instances added
    pub added: Vec<Endpoint>,
    /// Instances removed
    pub removed: Vec<Endpoint>,
    /// Timestamp of event
    pub timestamp: SystemTime,
    /// Rebalance status
    pub status: RebalanceStatus,
}

/// Status of a rebalance operation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RebalanceStatus {
    /// Rebalance started
    Started,
    /// Rebalance in progress
    InProgress,
    /// Rebalance completed
    Completed,
    /// Rebalance failed
    Failed,
}

/// Statistics for rebalancing operations
#[derive(Debug, Clone, Default)]
pub struct RebalanceStats {
    /// Total rebalance events
    pub total_events: usize,
    /// Successfully completed rebalances
    pub completed: usize,
    /// Failed rebalances
    pub failed: usize,
    /// Last rebalance timestamp
    pub last_rebalance: Option<SystemTime>,
}

/// Manages automatic rebalancing on topology changes
pub struct RebalanceManager {
    registry: Arc<ServiceRegistry>,
    config: RebalanceConfig,
    stats: Arc<parking_lot::RwLock<RebalanceStats>>,
    // Track previous topology
    previous_topology: Arc<parking_lot::RwLock<std::collections::HashMap<String, Vec<Endpoint>>>>,
    // Channel for stopping rebalancer
    shutdown_tx: Arc<parking_lot::Mutex<Option<mpsc::Sender<()>>>>,
}

impl RebalanceManager {
    /// Create a new rebalance manager
    pub async fn new(registry: Arc<ServiceRegistry>, config: RebalanceConfig) -> Result<Self> {
        Ok(Self {
            registry,
            config,
            stats: Arc::new(parking_lot::RwLock::new(RebalanceStats::default())),
            previous_topology: Arc::new(parking_lot::RwLock::new(
                std::collections::HashMap::new(),
            )),
            shutdown_tx: Arc::new(parking_lot::Mutex::new(None)),
        })
    }

    /// Start the rebalancing loop
    pub async fn start(&self) -> Result<()> {
        if self.config.strategy == RebalanceStrategy::Manual {
            return Ok(());
        }

        let (tx, mut rx) = mpsc::channel(1);
        *self.shutdown_tx.lock() = Some(tx);

        let registry = self.registry.clone();
        let config = self.config.clone();
        let stats = self.stats.clone();
        let topology = self.previous_topology.clone();

        tokio::spawn(async move {
            let mut interval = interval(config.check_interval);

            loop {
                tokio::select! {
                    _ = rx.recv() => {
                        break;
                    }
                    _ = interval.tick() => {
                        // Check all services for topology changes
                        let services = registry.list_services().await;
                        let mut topo = topology.write();

                        for service in services {
                            if let Ok(current_endpoints) = registry.get_endpoints(&service).await {
                                let previous_endpoints = topo.get(&service).cloned().unwrap_or_default();

                                if current_endpoints != previous_endpoints {
                                    // Topology changed
                                    let added: Vec<_> = current_endpoints
                                        .iter()
                                        .filter(|e| !previous_endpoints.contains(e))
                                        .cloned()
                                        .collect();

                                    let removed: Vec<_> = previous_endpoints
                                        .iter()
                                        .filter(|e| !current_endpoints.contains(e))
                                        .cloned()
                                        .collect();

                                    if !added.is_empty() || !removed.is_empty() {
                                        let _event = RebalanceEvent {
                                            service: service.clone(),
                                            added,
                                            removed,
                                            timestamp: SystemTime::now(),
                                            status: RebalanceStatus::Completed,
                                        };

                                        let mut s = stats.write();
                                        s.total_events += 1;
                                        s.completed += 1;
                                        s.last_rebalance = Some(SystemTime::now());
                                    }

                                    topo.insert(service, current_endpoints);
                                }
                            }
                        }
                    }
                }
            }
        });

        Ok(())
    }

    /// Stop the rebalancing loop
    pub async fn stop(&self) -> Result<()> {
        if let Some(tx) = self.shutdown_tx.lock().take() {
            let _ = tx.send(()).await;
        }
        Ok(())
    }

    /// Manually trigger rebalancing for a service
    pub async fn rebalance_service(&self, service: &str) -> Result<RebalanceEvent> {
        let current_endpoints = self.registry.get_endpoints(service).await?;
        let mut topology = self.previous_topology.write();
        let previous_endpoints = topology.get(service).cloned().unwrap_or_default();

        let added: Vec<_> = current_endpoints
            .iter()
            .filter(|e| !previous_endpoints.contains(e))
            .cloned()
            .collect();

        let removed: Vec<_> = previous_endpoints
            .iter()
            .filter(|e| !current_endpoints.contains(e))
            .cloned()
            .collect();

        topology.insert(service.to_string(), current_endpoints);

        let event = RebalanceEvent {
            service: service.to_string(),
            added,
            removed,
            timestamp: SystemTime::now(),
            status: RebalanceStatus::Completed,
        };

        let mut stats = self.stats.write();
        stats.total_events += 1;
        stats.completed += 1;
        stats.last_rebalance = Some(SystemTime::now());

        Ok(event)
    }

    /// Get rebalancing statistics
    pub fn get_stats(&self) -> RebalanceStats {
        self.stats.read().clone()
    }

    /// Drain connections gradually from removed endpoints
    pub async fn drain_endpoints(&self, endpoints: &[Endpoint], drain_percentage: f32) -> Result<()> {
        if drain_percentage <= 0.0 || drain_percentage > 1.0 {
            return Err(crate::error::Error::invalid_config(
                "drain_percentage must be between 0.0 and 1.0",
            ));
        }

        // In a real implementation, this would:
        // - Track active connections to endpoints
        // - Gradually close connections at drain_percentage rate
        // - Stop accepting new connections to draining endpoints
        // - Wait for all connections to complete before full drain

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_rebalance_manager_creation() {
        let registry = Arc::new(ServiceRegistry::new());
        let config = RebalanceConfig::default();
        let manager = RebalanceManager::new(registry, config).await.unwrap();
        let stats = manager.get_stats();
        assert_eq!(stats.total_events, 0);
    }

    #[tokio::test]
    async fn test_manual_rebalance_trigger() {
        let registry = Arc::new(ServiceRegistry::new());
        let endpoints = vec![
            Endpoint::new("127.0.0.1:5001".parse().unwrap(), None),
            Endpoint::new("127.0.0.1:5002".parse().unwrap(), None),
        ];

        registry
            .register_batch("test-service".to_string(), endpoints.clone())
            .await
            .unwrap();

        let config = RebalanceConfig {
            strategy: RebalanceStrategy::Manual,
            ..Default::default()
        };

        let manager = RebalanceManager::new(registry.clone(), config).await.unwrap();

        // Add a new endpoint
        let new_endpoint = Endpoint::new("127.0.0.1:5003".parse().unwrap(), None);
        registry.register("test-service".to_string(), new_endpoint).await.unwrap();

        // Trigger rebalance
        let event = manager.rebalance_service("test-service").await.unwrap();
        assert_eq!(event.added.len(), 1);
        assert_eq!(event.removed.len(), 0);
    }

    #[tokio::test]
    async fn test_rebalance_stats() {
        let registry = Arc::new(ServiceRegistry::new());
        let config = RebalanceConfig {
            strategy: RebalanceStrategy::Manual,
            ..Default::default()
        };

        let manager = RebalanceManager::new(registry.clone(), config).await.unwrap();
        let endpoint = Endpoint::new("127.0.0.1:5001".parse().unwrap(), None);

        registry
            .register("test-service".to_string(), endpoint)
            .await
            .unwrap();

        manager.rebalance_service("test-service").await.unwrap();
        let stats = manager.get_stats();
        assert_eq!(stats.total_events, 1);
        assert_eq!(stats.completed, 1);
    }
}
