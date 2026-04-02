//! Failover and backup instance management

use crate::error::Result;
use crate::service_registry::Endpoint;
use dashmap::DashMap;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use std::time::SystemTime;

/// Configuration for failover behavior
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FailoverConfig {
    /// Enable automatic failover
    pub enabled: bool,
    /// Maximum attempts before giving up
    pub max_attempts: usize,
    /// Enable failover logging
    pub log_failovers: bool,
}

impl Default for FailoverConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            max_attempts: 3,
            log_failovers: true,
        }
    }
}

/// Failover state for a service/endpoint pair
#[derive(Debug, Clone)]
struct FailoverState {
    primary: Endpoint,
    backups: Vec<Endpoint>,
    current_endpoint: Endpoint,
    failover_count: usize,
    last_failover: Option<SystemTime>,
}

/// Manages failover behavior for critical services
pub struct FailoverManager {
    config: FailoverConfig,
    // Map of service:endpoint → failover state
    states: Arc<DashMap<String, FailoverState>>,
}

impl FailoverManager {
    /// Create a new failover manager
    pub fn new(config: FailoverConfig) -> Self {
        Self {
            config,
            states: Arc::new(DashMap::new()),
        }
    }

    /// Register a failover group (primary + backups)
    pub async fn register(
        &self,
        service: &str,
        primary: Endpoint,
        backups: Vec<Endpoint>,
    ) -> Result<()> {
        let key = format!("{}:{}", service, primary.address);

        let state = FailoverState {
            primary: primary.clone(),
            backups: backups.clone(),
            current_endpoint: primary,
            failover_count: 0,
            last_failover: None,
        };

        self.states.insert(key, state);
        Ok(())
    }

    /// Get the current active endpoint for a service
    pub async fn get_active_endpoint(&self, service: &str, endpoint: &Endpoint) -> Option<Endpoint> {
        let key = format!("{}:{}", service, endpoint.address);
        self.states.get(&key).map(|state| state.current_endpoint.clone())
    }

    /// Trigger failover to next backup
    pub async fn failover(&self, service: &str, endpoint: &Endpoint) -> Result<Option<Endpoint>> {
        if !self.config.enabled {
            return Ok(None);
        }

        let key = format!("{}:{}", service, endpoint.address);

        if let Some(mut state) = self.states.get_mut(&key) {
            if state.failover_count >= self.config.max_attempts {
                if self.config.log_failovers {
                    tracing::error!(
                        "Max failover attempts reached for service: {}, endpoint: {}",
                        service,
                        endpoint.address
                    );
                }
                return Ok(None);
            }

            // Find next available backup
            if !state.backups.is_empty() {
                let next_backup = state.backups[state.failover_count % state.backups.len()].clone();
                state.current_endpoint = next_backup.clone();
                state.failover_count += 1;
                state.last_failover = Some(SystemTime::now());

                if self.config.log_failovers {
                    tracing::warn!(
                        "Failover triggered for service: {}, new endpoint: {}",
                        service,
                        next_backup.address
                    );
                }

                return Ok(Some(next_backup));
            }
        }

        Ok(None)
    }

    /// Reset failover counter for an endpoint
    pub async fn reset_failover(&self, service: &str, endpoint: &Endpoint) -> Result<()> {
        let key = format!("{}:{}", service, endpoint.address);

        if let Some(mut state) = self.states.get_mut(&key) {
            state.failover_count = 0;
            state.current_endpoint = state.primary.clone();

            if self.config.log_failovers {
                tracing::info!(
                    "Failover reset for service: {}, endpoint: {}",
                    service,
                    endpoint.address
                );
            }
        }

        Ok(())
    }

    /// Get failover statistics for an endpoint
    pub async fn get_stats(&self, service: &str, endpoint: &Endpoint) -> Option<FailoverStats> {
        let key = format!("{}:{}", service, endpoint.address);

        self.states.get(&key).map(|state| FailoverStats {
            failover_count: state.failover_count,
            last_failover: state.last_failover,
            current_endpoint: state.current_endpoint.clone(),
            backup_count: state.backups.len(),
        })
    }

    /// Deregister a failover group
    pub async fn deregister(&self, service: &str, endpoint: &Endpoint) -> Result<()> {
        let key = format!("{}:{}", service, endpoint.address);
        self.states.remove(&key);
        Ok(())
    }

    /// Get failover configuration
    pub fn config(&self) -> &FailoverConfig {
        &self.config
    }
}

/// Statistics for failover
#[derive(Debug, Clone)]
pub struct FailoverStats {
    /// Number of failovers performed
    pub failover_count: usize,
    /// Last failover timestamp
    pub last_failover: Option<SystemTime>,
    /// Current active endpoint
    pub current_endpoint: Endpoint,
    /// Number of backup endpoints
    pub backup_count: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_register_failover_group() {
        let config = FailoverConfig::default();
        let manager = FailoverManager::new(config);

        let primary = Endpoint::new("127.0.0.1:5001".parse().unwrap(), None);
        let backups = vec![
            Endpoint::new("127.0.0.1:5002".parse().unwrap(), None),
            Endpoint::new("127.0.0.1:5003".parse().unwrap(), None),
        ];

        manager
            .register("test-service", primary.clone(), backups)
            .await
            .unwrap();

        let active = manager.get_active_endpoint("test-service", &primary).await;
        assert_eq!(active, Some(primary));
    }

    #[tokio::test]
    async fn test_failover_to_backup() {
        let config = FailoverConfig::default();
        let manager = FailoverManager::new(config);

        let primary = Endpoint::new("127.0.0.1:5001".parse().unwrap(), None);
        let backup1 = Endpoint::new("127.0.0.1:5002".parse().unwrap(), None);
        let backup2 = Endpoint::new("127.0.0.1:5003".parse().unwrap(), None);

        manager
            .register("test-service", primary.clone(), vec![backup1.clone(), backup2.clone()])
            .await
            .unwrap();

        // Trigger failover
        let result = manager.failover("test-service", &primary).await.unwrap();
        assert_eq!(result, Some(backup1.clone()));

        // Check current endpoint
        let active = manager.get_active_endpoint("test-service", &primary).await;
        assert_eq!(active, Some(backup1));
    }

    #[tokio::test]
    async fn test_reset_failover() {
        let config = FailoverConfig::default();
        let manager = FailoverManager::new(config);

        let primary = Endpoint::new("127.0.0.1:5001".parse().unwrap(), None);
        let backup = Endpoint::new("127.0.0.1:5002".parse().unwrap(), None);

        manager
            .register("test-service", primary.clone(), vec![backup.clone()])
            .await
            .unwrap();

        // Trigger failover
        manager.failover("test-service", &primary).await.unwrap();

        // Reset
        manager.reset_failover("test-service", &primary).await.unwrap();

        // Should be back to primary
        let active = manager.get_active_endpoint("test-service", &primary).await;
        assert_eq!(active, Some(primary));
    }

    #[tokio::test]
    async fn test_max_failover_attempts() {
        let config = FailoverConfig {
            max_attempts: 2,
            ..Default::default()
        };

        let manager = FailoverManager::new(config);

        let primary = Endpoint::new("127.0.0.1:5001".parse().unwrap(), None);
        let backup = Endpoint::new("127.0.0.1:5002".parse().unwrap(), None);

        manager
            .register("test-service", primary.clone(), vec![backup.clone()])
            .await
            .unwrap();

        // Exceed max attempts
        manager.failover("test-service", &primary).await.unwrap();
        manager.failover("test-service", &primary).await.unwrap();

        // Next failover should fail
        let result = manager.failover("test-service", &primary).await.unwrap();
        assert!(result.is_none());
    }

    #[tokio::test]
    async fn test_failover_stats() {
        let config = FailoverConfig::default();
        let manager = FailoverManager::new(config);

        let primary = Endpoint::new("127.0.0.1:5001".parse().unwrap(), None);
        let backup = Endpoint::new("127.0.0.1:5002".parse().unwrap(), None);

        manager
            .register("test-service", primary.clone(), vec![backup.clone()])
            .await
            .unwrap();

        manager.failover("test-service", &primary).await.unwrap();

        let stats = manager.get_stats("test-service", &primary).await.unwrap();
        assert_eq!(stats.failover_count, 1);
        assert_eq!(stats.backup_count, 1);
    }
}
