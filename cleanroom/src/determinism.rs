//! Deterministic execution manager for cleanroom testing
//!
//! This module provides deterministic execution following core team best practices:
//! - Fixed seed for reproducible tests
//! - Deterministic container startup
//! - Deterministic network configuration
//! - Deterministic file system operations
//! - Deterministic time handling

use crate::error::{Result, CleanroomError};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};
use tokio::sync::RwLock;

/// Deterministic execution manager
#[derive(Debug)]
pub struct DeterministicManager {
    /// Fixed seed for deterministic execution
    seed: u64,
    /// Random number generator
    rng: Arc<RwLock<fastrand::Rng>>,
    /// Deterministic time offset
    time_offset: Duration,
    /// Deterministic port allocation
    port_allocator: Arc<RwLock<DeterministicPortAllocator>>,
    /// Deterministic file system operations
    fs_operations: Arc<RwLock<Vec<DeterministicFsOperation>>>,
    /// Deterministic network configuration
    network_config: Arc<RwLock<DeterministicNetworkConfig>>,
}

/// Deterministic port allocator
#[derive(Debug, Clone)]
pub struct DeterministicPortAllocator {
    /// Base port range
    base_port: u16,
    /// Allocated ports
    allocated_ports: Vec<u16>,
    /// Port allocation counter
    counter: u16,
}

/// Deterministic file system operation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeterministicFsOperation {
    /// Operation timestamp
    pub timestamp: Instant,
    /// Operation type
    pub operation_type: FsOperationType,
    /// Operation path
    pub path: String,
    /// Operation result
    pub result: String,
}

/// File system operation type
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FsOperationType {
    /// Create file
    CreateFile,
    /// Create directory
    CreateDirectory,
    /// Read file
    ReadFile,
    /// Write file
    WriteFile,
    /// Delete file
    DeleteFile,
    /// Delete directory
    DeleteDirectory,
    /// List directory
    ListDirectory,
    /// Get file metadata
    GetMetadata,
}

/// Deterministic network configuration
#[derive(Debug, Clone)]
pub struct DeterministicNetworkConfig {
    /// Network name
    pub network_name: String,
    /// Subnet configuration
    pub subnet: String,
    /// Gateway configuration
    pub gateway: String,
    /// DNS configuration
    pub dns_servers: Vec<String>,
    /// Port mappings
    pub port_mappings: HashMap<u16, u16>,
}

impl DeterministicManager {
    /// Create a new deterministic manager with fixed seed
    pub fn new(seed: u64) -> Self {
        let rng = Arc::new(RwLock::new(fastrand::Rng::with_seed(seed)));
        let time_offset = Duration::from_secs(1640995200); // Fixed epoch: 2022-01-01 00:00:00 UTC
        
        Self {
            seed,
            rng,
            time_offset,
            port_allocator: Arc::new(RwLock::new(DeterministicPortAllocator::new(10000))),
            fs_operations: Arc::new(RwLock::new(Vec::new())),
            network_config: Arc::new(RwLock::new(DeterministicNetworkConfig::new(seed))),
        }
    }
    
    /// Get deterministic random number
    pub async fn random(&self) -> u64 {
        let mut rng = self.rng.write().await;
        rng.u64(..)
    }
    
    /// Get deterministic random number in range
    pub async fn random_range(&self, min: u64, max: u64) -> u64 {
        let mut rng = self.rng.write().await;
        rng.u64(min..max)
    }
    
    /// Get deterministic random float
    pub async fn random_float(&self) -> f64 {
        let mut rng = self.rng.write().await;
        rng.f64()
    }
    
    /// Get deterministic random float in range
    pub async fn random_float_range(&self, min: f64, max: f64) -> f64 {
        let mut rng = self.rng.write().await;
        min + (max - min) * rng.f64()
    }
    
    /// Get deterministic time
    pub async fn deterministic_time(&self) -> SystemTime {
        let elapsed = self.time_offset + Duration::from_millis(self.random().await % 1000000);
        UNIX_EPOCH + elapsed
    }
    
    /// Get deterministic duration
    pub async fn deterministic_duration(&self, min_ms: u64, max_ms: u64) -> Duration {
        let ms = self.random_range(min_ms, max_ms).await;
        Duration::from_millis(ms)
    }
    
    /// Allocate deterministic port
    pub async fn allocate_port(&self) -> Result<u16> {
        let mut allocator = self.port_allocator.write().await;
        allocator.allocate_port()
    }
    
    /// Release deterministic port
    pub async fn release_port(&self, port: u16) -> Result<()> {
        let mut allocator = self.port_allocator.write().await;
        allocator.release_port(port)
    }
    
    /// Get deterministic network configuration
    pub async fn get_network_config(&self) -> DeterministicNetworkConfig {
        let config = self.network_config.read().await;
        config.clone()
    }
    
    /// Record file system operation
    pub async fn record_fs_operation(&self, operation: DeterministicFsOperation) -> Result<()> {
        let mut operations = self.fs_operations.write().await;
        operations.push(operation);
        Ok(())
    }
    
    /// Get file system operations
    pub async fn get_fs_operations(&self) -> Vec<DeterministicFsOperation> {
        let operations = self.fs_operations.read().await;
        operations.clone()
    }
    
    /// Clear file system operations
    pub async fn clear_fs_operations(&self) -> Result<()> {
        let mut operations = self.fs_operations.write().await;
        operations.clear();
        Ok(())
    }
    
    /// Get deterministic container name
    pub async fn get_container_name(&self, base_name: &str) -> String {
        let suffix = self.random_range(1000, 9999).await;
        format!("{}-{}", base_name, suffix)
    }
    
    /// Get deterministic volume name
    pub async fn get_volume_name(&self, base_name: &str) -> String {
        let suffix = self.random_range(1000, 9999).await;
        format!("{}-volume-{}", base_name, suffix)
    }
    
    /// Get deterministic network name
    pub async fn get_network_name(&self, base_name: &str) -> String {
        let suffix = self.random_range(1000, 9999).await;
        format!("{}-network-{}", base_name, suffix)
    }
    
    /// Get seed
    pub fn seed(&self) -> u64 {
        self.seed
    }
    
    /// Reset deterministic state
    pub async fn reset(&self) -> Result<()> {
        // Reset random number generator
        {
            let mut rng = self.rng.write().await;
            *rng = fastrand::Rng::with_seed(self.seed);
        }
        
        // Reset port allocator
        {
            let mut allocator = self.port_allocator.write().await;
            *allocator = DeterministicPortAllocator::new(10000);
        }
        
        // Clear file system operations
        self.clear_fs_operations().await?;
        
        // Reset network configuration
        {
            let mut config = self.network_config.write().await;
            *config = DeterministicNetworkConfig::new(self.seed);
        }
        
        Ok(())
    }
    
    /// Get deterministic state summary
    pub async fn get_state_summary(&self) -> DeterministicStateSummary {
        let fs_operations = self.get_fs_operations().await;
        let network_config = self.get_network_config().await;
        
        DeterministicStateSummary {
            seed: self.seed,
            fs_operations_count: fs_operations.len(),
            network_name: network_config.network_name,
            allocated_ports_count: network_config.port_mappings.len(),
        }
    }
}

impl DeterministicPortAllocator {
    /// Create a new deterministic port allocator
    pub fn new(base_port: u16) -> Self {
        Self {
            base_port,
            allocated_ports: Vec::new(),
            counter: 0,
        }
    }
    
    /// Allocate a port
    pub fn allocate_port(&mut self) -> Result<u16> {
        if self.allocated_ports.len() >= 1000 {
            return Err(CleanroomError::deterministic_error("Too many ports allocated"));
        }
        
        let port = self.base_port + self.counter;
        self.counter += 1;
        self.allocated_ports.push(port);
        
        Ok(port)
    }
    
    /// Release a port
    pub fn release_port(&mut self, port: u16) -> Result<()> {
        if let Some(pos) = self.allocated_ports.iter().position(|&p| p == port) {
            self.allocated_ports.remove(pos);
            Ok(())
        } else {
            Err(CleanroomError::deterministic_error("Port not allocated"))
        }
    }
    
    /// Get allocated ports
    pub fn get_allocated_ports(&self) -> &[u16] {
        &self.allocated_ports
    }
}

impl DeterministicNetworkConfig {
    /// Create a new deterministic network configuration
    pub fn new(seed: u64) -> Self {
        let mut rng = fastrand::Rng::with_seed(seed);
        let network_suffix = rng.u64(1000..9999);
        
        Self {
            network_name: format!("cleanroom-network-{}", network_suffix),
            subnet: "172.20.0.0/16".to_string(),
            gateway: "172.20.0.1".to_string(),
            dns_servers: vec!["8.8.8.8".to_string(), "8.8.4.4".to_string()],
            port_mappings: HashMap::new(),
        }
    }
    
    /// Add port mapping
    pub fn add_port_mapping(&mut self, container_port: u16, host_port: u16) {
        self.port_mappings.insert(container_port, host_port);
    }
    
    /// Get port mapping
    pub fn get_port_mapping(&self, container_port: u16) -> Option<u16> {
        self.port_mappings.get(&container_port).copied()
    }
}

/// Deterministic state summary
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeterministicStateSummary {
    /// Seed used for deterministic execution
    pub seed: u64,
    /// Number of file system operations recorded
    pub fs_operations_count: usize,
    /// Network name
    pub network_name: String,
    /// Number of allocated ports
    pub allocated_ports_count: usize,
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[tokio::test]
    async fn test_deterministic_manager_creation() {
        let manager = DeterministicManager::new(42);
        assert_eq!(manager.seed(), 42);
    }
    
    #[tokio::test]
    async fn test_deterministic_random() {
        let manager = DeterministicManager::new(42);
        let random1 = manager.random().await;
        let random2 = manager.random().await;
        
        // Should be deterministic (same seed produces same sequence)
        assert_ne!(random1, random2); // Different values in sequence
    }
    
    #[tokio::test]
    async fn test_deterministic_port_allocation() {
        let manager = DeterministicManager::new(42);
        let port1 = manager.allocate_port().await.unwrap();
        let port2 = manager.allocate_port().await.unwrap();
        
        assert_eq!(port1, 10000);
        assert_eq!(port2, 10001);
    }
    
    #[tokio::test]
    async fn test_deterministic_port_release() {
        let manager = DeterministicManager::new(42);
        let port = manager.allocate_port().await.unwrap();
        manager.release_port(port).await.unwrap();
        
        // Should be able to allocate again
        let port2 = manager.allocate_port().await.unwrap();
        assert_eq!(port2, 10000);
    }
    
    #[tokio::test]
    async fn test_deterministic_container_name() {
        let manager = DeterministicManager::new(42);
        let name = manager.get_container_name("test").await;
        
        assert!(name.starts_with("test-"));
        assert!(name.len() > 5);
    }
    
    #[tokio::test]
    async fn test_deterministic_network_config() {
        let manager = DeterministicManager::new(42);
        let config = manager.get_network_config().await;
        
        assert!(config.network_name.starts_with("cleanroom-network-"));
        assert_eq!(config.subnet, "172.20.0.0/16");
        assert_eq!(config.gateway, "172.20.0.1");
    }
    
    #[tokio::test]
    async fn test_deterministic_fs_operations() {
        let manager = DeterministicManager::new(42);
        
        let operation = DeterministicFsOperation {
            timestamp: Instant::now(),
            operation_type: FsOperationType::CreateFile,
            path: "/test/file.txt".to_string(),
            result: "success".to_string(),
        };
        
        manager.record_fs_operation(operation.clone()).await.unwrap();
        
        let operations = manager.get_fs_operations().await;
        assert_eq!(operations.len(), 1);
        assert_eq!(operations[0].path, "/test/file.txt");
    }
    
    #[tokio::test]
    async fn test_deterministic_reset() {
        let manager = DeterministicManager::new(42);
        
        // Allocate some ports
        let _port1 = manager.allocate_port().await.unwrap();
        let _port2 = manager.allocate_port().await.unwrap();
        
        // Record some operations
        let operation = DeterministicFsOperation {
            timestamp: Instant::now(),
            operation_type: FsOperationType::CreateFile,
            path: "/test/file.txt".to_string(),
            result: "success".to_string(),
        };
        manager.record_fs_operation(operation).await.unwrap();
        
        // Reset
        manager.reset().await.unwrap();
        
        // Check state is reset
        let operations = manager.get_fs_operations().await;
        assert_eq!(operations.len(), 0);
        
        // Port allocation should start from beginning
        let port = manager.allocate_port().await.unwrap();
        assert_eq!(port, 10000);
    }
    
    #[tokio::test]
    async fn test_deterministic_state_summary() {
        let manager = DeterministicManager::new(42);
        
        // Allocate some ports
        let _port1 = manager.allocate_port().await.unwrap();
        let _port2 = manager.allocate_port().await.unwrap();
        
        // Record some operations
        let operation = DeterministicFsOperation {
            timestamp: Instant::now(),
            operation_type: FsOperationType::CreateFile,
            path: "/test/file.txt".to_string(),
            result: "success".to_string(),
        };
        manager.record_fs_operation(operation).await.unwrap();
        
        let summary = manager.get_state_summary().await;
        assert_eq!(summary.seed, 42);
        assert_eq!(summary.fs_operations_count, 1);
        assert!(summary.network_name.starts_with("cleanroom-network-"));
    }
}