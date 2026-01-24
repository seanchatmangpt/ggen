//! Secure connection pooling with TLS session management

use super::error::{TlsError, TlsResult};
use std::collections::HashMap;
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::RwLock;

/// Connection pool configuration
#[derive(Debug, Clone)]
pub struct PoolConfig {
    /// Maximum number of connections per host
    pub max_connections_per_host: usize,
    /// Maximum idle time before connection is closed
    pub max_idle_time: Duration,
    /// Connection timeout
    pub connection_timeout: Duration,
    /// Enable TLS session resumption
    pub enable_session_resumption: bool,
    /// Maximum pool size (total connections)
    pub max_pool_size: usize,
}

impl Default for PoolConfig {
    fn default() -> Self {
        Self {
            max_connections_per_host: 10,
            max_idle_time: Duration::from_secs(300), // 5 minutes
            connection_timeout: Duration::from_secs(30),
            enable_session_resumption: true,
            max_pool_size: 100,
        }
    }
}

/// Connection metadata
#[derive(Debug)]
struct ConnectionMetadata {
    /// When the connection was created
    created_at: Instant,
    /// When the connection was last used
    last_used: Instant,
    /// Number of times this connection has been reused
    use_count: usize,
}

impl ConnectionMetadata {
    fn new() -> Self {
        let now = Instant::now();
        Self {
            created_at: now,
            last_used: now,
            use_count: 0,
        }
    }

    fn touch(&mut self) {
        self.last_used = Instant::now();
        self.use_count += 1;
    }

    fn is_expired(&self, max_idle_time: Duration) -> bool {
        self.last_used.elapsed() > max_idle_time
    }
}

/// Secure connection pool
pub struct ConnectionPool {
    config: PoolConfig,
    connections: Arc<RwLock<HashMap<String, Vec<ConnectionHandle>>>>,
    metadata: Arc<RwLock<HashMap<usize, ConnectionMetadata>>>,
    next_id: Arc<RwLock<usize>>,
}

/// Handle to a pooled connection
#[derive(Debug)]
pub struct ConnectionHandle {
    id: usize,
    hostname: String,
}

impl ConnectionHandle {
    fn new(id: usize, hostname: String) -> Self {
        Self { id, hostname }
    }

    /// Get the connection ID
    #[must_use]
    pub fn id(&self) -> usize {
        self.id
    }

    /// Get the hostname
    #[must_use]
    pub fn hostname(&self) -> &str {
        &self.hostname
    }
}

impl ConnectionPool {
    /// Create a new connection pool
    #[must_use]
    pub fn new(config: PoolConfig) -> Self {
        Self {
            config,
            connections: Arc::new(RwLock::new(HashMap::new())),
            metadata: Arc::new(RwLock::new(HashMap::new())),
            next_id: Arc::new(RwLock::new(0)),
        }
    }

    /// Get a connection from the pool or create a new one
    ///
    /// # Errors
    /// Returns `TlsError` if pool is full or connection fails
    pub async fn get_connection(&self, hostname: &str) -> TlsResult<ConnectionHandle> {
        // Try to reuse existing connection
        if let Some(handle) = self.try_reuse_connection(hostname).await? {
            return Ok(handle);
        }

        // Create new connection
        self.create_connection(hostname).await
    }

    /// Try to reuse an existing connection
    async fn try_reuse_connection(&self, hostname: &str) -> TlsResult<Option<ConnectionHandle>> {
        let mut connections = self.connections.write().await;
        let mut metadata = self.metadata.write().await;

        if let Some(host_connections) = connections.get_mut(hostname) {
            // Remove expired connections
            host_connections.retain(|handle| {
                if let Some(meta) = metadata.get(&handle.id) {
                    !meta.is_expired(self.config.max_idle_time)
                } else {
                    false
                }
            });

            // Get the most recently used connection
            if let Some(handle) = host_connections.pop() {
                if let Some(meta) = metadata.get_mut(&handle.id) {
                    meta.touch();
                }
                return Ok(Some(handle));
            }
        }

        Ok(None)
    }

    /// Create a new connection
    async fn create_connection(&self, hostname: &str) -> TlsResult<ConnectionHandle> {
        // Check pool size limits
        let metadata = self.metadata.read().await;
        let total_connections = metadata.len();

        if total_connections >= self.config.max_pool_size {
            return Err(TlsError::PoolError(format!(
                "Pool is full: {total_connections} >= {}",
                self.config.max_pool_size
            )));
        }

        // Check per-host limit
        let connections = self.connections.read().await;
        let host_connections = connections.get(hostname).map_or(0, |v| v.len());

        if host_connections >= self.config.max_connections_per_host {
            return Err(TlsError::PoolError(format!(
                "Too many connections to {hostname}: {host_connections} >= {}",
                self.config.max_connections_per_host
            )));
        }

        drop(connections);
        drop(metadata);

        // Create new connection
        let mut next_id = self.next_id.write().await;
        let id = *next_id;
        *next_id += 1;
        drop(next_id);

        let handle = ConnectionHandle::new(id, hostname.to_string());

        // Store metadata
        let mut metadata = self.metadata.write().await;
        metadata.insert(id, ConnectionMetadata::new());

        Ok(handle)
    }

    /// Return a connection to the pool
    ///
    /// # Errors
    /// Returns `TlsError` if the connection cannot be returned to the pool
    pub async fn return_connection(&self, handle: ConnectionHandle) -> TlsResult<()> {
        let mut connections = self.connections.write().await;
        let mut metadata = self.metadata.write().await;

        // Update last used time
        if let Some(meta) = metadata.get_mut(&handle.id) {
            meta.touch();
        }

        // Return to pool
        connections
            .entry(handle.hostname.clone())
            .or_insert_with(Vec::new)
            .push(handle);

        Ok(())
    }

    /// Close a connection and remove it from the pool
    pub async fn close_connection(&self, handle: ConnectionHandle) {
        let mut metadata = self.metadata.write().await;
        metadata.remove(&handle.id);
        // Connection will be dropped when handle is dropped
    }

    /// Clean up expired connections
    pub async fn cleanup_expired(&self) {
        let mut connections = self.connections.write().await;
        let mut metadata = self.metadata.write().await;

        for (_hostname, host_connections) in connections.iter_mut() {
            host_connections.retain(|handle| {
                if let Some(meta) = metadata.get(&handle.id) {
                    if meta.is_expired(self.config.max_idle_time) {
                        metadata.remove(&handle.id);
                        false
                    } else {
                        true
                    }
                } else {
                    false
                }
            });
        }

        // Remove empty host entries
        connections.retain(|_, v| !v.is_empty());
    }

    /// Get pool statistics
    #[must_use]
    pub async fn stats(&self) -> PoolStats {
        let connections = self.connections.read().await;
        let metadata = self.metadata.read().await;

        let total_connections = metadata.len();
        let connections_by_host = connections
            .iter()
            .map(|(k, v)| (k.clone(), v.len()))
            .collect();

        PoolStats {
            total_connections,
            connections_by_host,
            max_pool_size: self.config.max_pool_size,
        }
    }
}

/// Pool statistics
#[derive(Debug)]
pub struct PoolStats {
    /// Total number of connections
    pub total_connections: usize,
    /// Connections grouped by hostname
    pub connections_by_host: HashMap<String, usize>,
    /// Maximum pool size
    pub max_pool_size: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_pool_config_default() {
        // Arrange & Act
        let config = PoolConfig::default();

        // Assert
        assert_eq!(config.max_connections_per_host, 10);
        assert_eq!(config.max_idle_time, Duration::from_secs(300));
        assert!(config.enable_session_resumption);
    }

    #[tokio::test]
    async fn test_connection_pool_creation() {
        // Arrange
        let config = PoolConfig::default();

        // Act
        let pool = ConnectionPool::new(config);

        // Assert
        let stats = pool.stats().await;
        assert_eq!(stats.total_connections, 0);
    }

    #[tokio::test]
    async fn test_get_connection() {
        // Arrange
        let pool = ConnectionPool::new(PoolConfig::default());

        // Act
        let result = pool.get_connection("example.com").await;

        // Assert
        assert!(result.is_ok(), "Should get connection successfully");
        let handle = result.expect("Connection handle should exist");
        assert_eq!(handle.hostname(), "example.com");
    }

    #[tokio::test]
    async fn test_return_connection() {
        // Arrange
        let pool = ConnectionPool::new(PoolConfig::default());
        let handle = pool.get_connection("example.com").await
            .expect("Failed to get connection");

        // Act
        let result = pool.return_connection(handle).await;

        // Assert
        assert!(result.is_ok(), "Should return connection successfully");
    }

    #[tokio::test]
    async fn test_connection_reuse() {
        // Arrange
        let pool = ConnectionPool::new(PoolConfig::default());
        let handle1 = pool.get_connection("example.com").await
            .expect("Failed to get connection");
        let id1 = handle1.id();
        pool.return_connection(handle1).await
            .expect("Failed to return connection");

        // Act
        let handle2 = pool.get_connection("example.com").await
            .expect("Failed to get second connection");

        // Assert
        assert_eq!(handle2.id(), id1, "Should reuse the same connection");
    }

    #[tokio::test]
    async fn test_max_connections_per_host() {
        // Arrange
        let config = PoolConfig {
            max_connections_per_host: 2,
            ..Default::default()
        };
        let pool = ConnectionPool::new(config);

        // Act
        let handle1 = pool.get_connection("example.com").await;
        let handle2 = pool.get_connection("example.com").await;
        let handle3 = pool.get_connection("example.com").await;

        // Assert
        assert!(handle1.is_ok());
        assert!(handle2.is_ok());
        assert!(handle3.is_err(), "Should fail when exceeding max connections per host");
    }

    #[tokio::test]
    async fn test_max_pool_size() {
        // Arrange
        let config = PoolConfig {
            max_pool_size: 2,
            max_connections_per_host: 10,
            ..Default::default()
        };
        let pool = ConnectionPool::new(config);

        // Act
        let handle1 = pool.get_connection("host1.com").await;
        let handle2 = pool.get_connection("host2.com").await;
        let handle3 = pool.get_connection("host3.com").await;

        // Assert
        assert!(handle1.is_ok());
        assert!(handle2.is_ok());
        assert!(handle3.is_err(), "Should fail when exceeding total pool size");
    }

    #[tokio::test]
    async fn test_close_connection() {
        // Arrange
        let pool = ConnectionPool::new(PoolConfig::default());
        let handle = pool.get_connection("example.com").await
            .expect("Failed to get connection");

        // Act
        pool.close_connection(handle).await;
        let stats = pool.stats().await;

        // Assert
        assert_eq!(stats.total_connections, 0, "Connection should be removed");
    }

    #[tokio::test]
    async fn test_cleanup_expired() {
        // Arrange
        let config = PoolConfig {
            max_idle_time: Duration::from_millis(1),
            ..Default::default()
        };
        let pool = ConnectionPool::new(config);
        let handle = pool.get_connection("example.com").await
            .expect("Failed to get connection");
        pool.return_connection(handle).await
            .expect("Failed to return connection");

        // Act
        tokio::time::sleep(Duration::from_millis(10)).await;
        pool.cleanup_expired().await;
        let stats = pool.stats().await;

        // Assert
        assert_eq!(stats.total_connections, 0, "Expired connections should be removed");
    }

    #[tokio::test]
    async fn test_pool_stats() {
        // Arrange
        let pool = ConnectionPool::new(PoolConfig::default());
        let _h1 = pool.get_connection("host1.com").await.expect("Failed");
        let _h2 = pool.get_connection("host2.com").await.expect("Failed");

        // Act
        let stats = pool.stats().await;

        // Assert
        assert_eq!(stats.total_connections, 2);
    }
}
