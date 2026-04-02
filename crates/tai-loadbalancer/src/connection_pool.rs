//! Connection pooling for efficient resource management

use crate::error::{Error, Result};
use crate::service_registry::Endpoint;
use dashmap::DashMap;
use serde::{Deserialize, Serialize};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::Duration;

/// Configuration for connection pooling
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConnectionPoolConfig {
    /// Minimum connections to maintain per endpoint
    pub min_connections: usize,
    /// Maximum connections per endpoint
    pub max_connections: usize,
    /// Timeout for acquiring a connection
    pub acquire_timeout: Duration,
    /// Timeout for idle connections before being closed
    pub idle_timeout: Duration,
    /// Maximum lifetime of a connection
    pub max_lifetime: Duration,
}

impl Default for ConnectionPoolConfig {
    fn default() -> Self {
        Self {
            min_connections: 2,
            max_connections: 32,
            acquire_timeout: Duration::from_secs(5),
            idle_timeout: Duration::from_secs(300),
            max_lifetime: Duration::from_secs(600),
        }
    }
}

/// Statistics for a connection pool
#[derive(Debug, Clone, Default)]
pub struct PoolStats {
    /// Total connections created
    pub total_created: usize,
    /// Currently active connections
    pub active_connections: usize,
    /// Total connections acquired
    pub total_acquired: usize,
    /// Total connections released
    pub total_released: usize,
    /// Failed acquisition attempts
    pub failed_acquires: usize,
}

/// Connection pool for managing connections to endpoints
pub struct ConnectionPool {
    config: ConnectionPoolConfig,
    // Map of endpoint address → connection count
    connection_counts: Arc<DashMap<String, Arc<AtomicUsize>>>,
    // Stats tracking
    stats: Arc<parking_lot::RwLock<PoolStats>>,
}

impl ConnectionPool {
    /// Create a new connection pool
    pub fn new(config: ConnectionPoolConfig) -> Self {
        // Validate configuration
        assert!(config.min_connections <= config.max_connections);

        Self {
            config,
            connection_counts: Arc::new(DashMap::new()),
            stats: Arc::new(parking_lot::RwLock::new(PoolStats::default())),
        }
    }

    /// Acquire a connection for an endpoint
    pub async fn acquire(&self, endpoint: &Endpoint) -> Result<PooledConnection> {
        let addr_str = endpoint.address.to_string();

        // Get or create counter for this endpoint
        let counter = self
            .connection_counts
            .entry(addr_str.clone())
            .or_insert_with(|| Arc::new(AtomicUsize::new(0)))
            .clone();

        // Try to acquire
        let current = counter.load(Ordering::SeqCst);
        if current >= self.config.max_connections {
            let mut stats = self.stats.write();
            stats.failed_acquires += 1;
            return Err(Error::pool_exhausted("service", self.config.max_connections));
        }

        counter.fetch_add(1, Ordering::SeqCst);

        let mut stats = self.stats.write();
        stats.total_acquired += 1;
        stats.active_connections = counter.load(Ordering::SeqCst);

        Ok(PooledConnection {
            endpoint: endpoint.clone(),
            counter: counter.clone(),
            stats: self.stats.clone(),
        })
    }

    /// Get number of active connections for an endpoint
    pub fn active_connections(&self, endpoint: &Endpoint) -> usize {
        self.connection_counts
            .get(&endpoint.address.to_string())
            .map(|counter| counter.load(Ordering::SeqCst))
            .unwrap_or(0)
    }

    /// Get connection pool statistics
    pub fn get_stats(&self) -> PoolStats {
        self.stats.read().clone()
    }

    /// Clear all connection counts
    pub fn clear(&self) {
        self.connection_counts.clear();
    }

    /// Get total active connections across all endpoints
    pub fn total_active_connections(&self) -> usize {
        self.connection_counts
            .iter()
            .map(|entry| entry.value().load(Ordering::SeqCst))
            .sum()
    }
}

/// A pooled connection that auto-releases on drop
pub struct PooledConnection {
    endpoint: Endpoint,
    counter: Arc<AtomicUsize>,
    stats: Arc<parking_lot::RwLock<PoolStats>>,
}

impl PooledConnection {
    /// Get the endpoint for this connection
    pub fn endpoint(&self) -> &Endpoint {
        &self.endpoint
    }

    /// Get the current active connection count
    pub fn active_count(&self) -> usize {
        self.counter.load(Ordering::SeqCst)
    }
}

impl Drop for PooledConnection {
    fn drop(&mut self) {
        self.counter.fetch_sub(1, Ordering::SeqCst);

        let mut stats = self.stats.write();
        stats.total_released += 1;
        stats.active_connections = self.counter.load(Ordering::SeqCst);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_acquire_and_release_connection() {
        let config = ConnectionPoolConfig {
            min_connections: 1,
            max_connections: 5,
            ..Default::default()
        };

        let pool = ConnectionPool::new(config);
        let endpoint = Endpoint::new("127.0.0.1:5001".parse().unwrap(), None);

        // Acquire a connection
        let conn = pool.acquire(&endpoint).await.unwrap();
        assert_eq!(pool.active_connections(&endpoint), 1);

        // Release by dropping
        drop(conn);
        assert_eq!(pool.active_connections(&endpoint), 0);
    }

    #[tokio::test]
    async fn test_pool_exhaustion() {
        let config = ConnectionPoolConfig {
            min_connections: 1,
            max_connections: 2,
            ..Default::default()
        };

        let pool = ConnectionPool::new(config);
        let endpoint = Endpoint::new("127.0.0.1:5001".parse().unwrap(), None);

        // Acquire all available connections
        let _conn1 = pool.acquire(&endpoint).await.unwrap();
        let _conn2 = pool.acquire(&endpoint).await.unwrap();

        // Should fail on third acquire
        let result = pool.acquire(&endpoint).await;
        assert!(matches!(result, Err(Error::PoolExhausted { .. })));
    }

    #[tokio::test]
    async fn test_multiple_endpoints() {
        let config = ConnectionPoolConfig {
            max_connections: 5,
            ..Default::default()
        };

        let pool = ConnectionPool::new(config);
        let ep1 = Endpoint::new("127.0.0.1:5001".parse().unwrap(), None);
        let ep2 = Endpoint::new("127.0.0.1:5002".parse().unwrap(), None);

        let _conn1 = pool.acquire(&ep1).await.unwrap();
        let _conn2 = pool.acquire(&ep1).await.unwrap();
        let _conn3 = pool.acquire(&ep2).await.unwrap();

        assert_eq!(pool.active_connections(&ep1), 2);
        assert_eq!(pool.active_connections(&ep2), 1);
        assert_eq!(pool.total_active_connections(), 3);
    }

    #[test]
    fn test_pool_stats() {
        let pool = ConnectionPool::new(ConnectionPoolConfig::default());
        let stats = pool.get_stats();
        assert_eq!(stats.total_acquired, 0);
        assert_eq!(stats.failed_acquires, 0);
    }
}
