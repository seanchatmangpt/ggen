//! Container-specific ID management
//!
//! This module provides specialized container ID management with lifecycle tracking,
//! health monitoring, and resource association.

use super::{ContainerId, IdRegistry};
use crate::error::Result;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use std::time::Instant;

/// Container metadata associated with an ID
#[derive(Debug, Clone)]
pub struct ContainerMetadata {
    /// Container ID
    pub id: ContainerId,
    /// Container name
    pub name: String,
    /// Container image
    pub image: String,
    /// Creation time
    pub created_at: Instant,
    /// Last health check time
    pub last_health_check: Option<Instant>,
    /// Resource usage
    pub resource_usage: Option<ContainerResourceUsage>,
    /// Status
    pub status: ContainerStatus,
}

/// Container resource usage
#[derive(Debug, Clone)]
pub struct ContainerResourceUsage {
    /// CPU usage percentage
    pub cpu_usage_percent: f64,
    /// Memory usage in bytes
    pub memory_usage_bytes: u64,
    /// Disk usage in bytes
    pub disk_usage_bytes: u64,
    /// Network bytes sent
    pub network_bytes_sent: u64,
    /// Network bytes received
    pub network_bytes_received: u64,
}

/// Container status
#[derive(Debug, Clone, PartialEq)]
pub enum ContainerStatus {
    /// Container is starting
    Starting,
    /// Container is running
    Running,
    /// Container is stopping
    Stopping,
    /// Container is stopped
    Stopped,
    /// Container is unhealthy
    Unhealthy,
    /// Container status is unknown
    Unknown,
}

/// Container ID registry with metadata tracking
#[derive(Debug)]
pub struct ContainerIdRegistry {
    /// ID registry
    registry: IdRegistry,
    /// Container metadata
    metadata: Arc<RwLock<HashMap<ContainerId, ContainerMetadata>>>,
    /// Container name to ID mapping
    name_to_id: Arc<RwLock<HashMap<String, ContainerId>>>,
}

impl ContainerIdRegistry {
    /// Create a new container ID registry
    pub fn new() -> Self {
        Self {
            registry: IdRegistry::new(),
            metadata: Arc::new(RwLock::new(HashMap::new())),
            name_to_id: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Register a new container with metadata
    pub async fn register_container(
        &self,
        name: String,
        image: String,
    ) -> Result<ContainerId> {
        let id = ContainerId::new();
        
        // Register in base registry
        self.registry.register_container(id);
        
        // Create metadata
        let metadata = ContainerMetadata {
            id,
            name: name.clone(),
            image,
            created_at: Instant::now(),
            last_health_check: None,
            resource_usage: None,
            status: ContainerStatus::Starting,
        };
        
        // Store metadata
        {
            let mut meta_map = self.metadata.write().await;
            meta_map.insert(id, metadata);
        }
        
        // Store name mapping
        {
            let mut name_map = self.name_to_id.write().await;
            name_map.insert(name, id);
        }
        
        Ok(id)
    }

    /// Get container metadata
    pub async fn get_metadata(&self, id: &ContainerId) -> Option<ContainerMetadata> {
        let meta_map = self.metadata.read().await;
        meta_map.get(id).cloned()
    }

    /// Update container status
    pub async fn update_status(&self, id: &ContainerId, status: ContainerStatus) -> Result<()> {
        let mut meta_map = self.metadata.write().await;
        if let Some(metadata) = meta_map.get_mut(id) {
            metadata.status = status;
            Ok(())
        } else {
            Err(crate::error::CleanroomError::internal_error("Container not found"))
        }
    }

    /// Update container resource usage
    pub async fn update_resource_usage(
        &self,
        id: &ContainerId,
        usage: ContainerResourceUsage,
    ) -> Result<()> {
        let mut meta_map = self.metadata.write().await;
        if let Some(metadata) = meta_map.get_mut(id) {
            metadata.resource_usage = Some(usage);
            metadata.last_health_check = Some(Instant::now());
            Ok(())
        } else {
            Err(crate::error::CleanroomError::internal_error("Container not found"))
        }
    }

    /// Get container ID by name
    pub async fn get_id_by_name(&self, name: &str) -> Option<ContainerId> {
        let name_map = self.name_to_id.read().await;
        name_map.get(name).copied()
    }

    /// Get container name by ID
    pub async fn get_name_by_id(&self, id: &ContainerId) -> Option<String> {
        let meta_map = self.metadata.read().await;
        meta_map.get(id).map(|m| m.name.clone())
    }

    /// Check if container is registered
    pub fn is_registered(&self, id: &ContainerId) -> bool {
        self.registry.is_container_registered(id)
    }

    /// Unregister a container
    pub async fn unregister_container(&self, id: &ContainerId) -> Result<()> {
        // Get metadata to find name
        let name = {
            let meta_map = self.metadata.read().await;
            meta_map.get(id).map(|m| m.name.clone())
        };
        
        // Remove from base registry
        self.registry.unregister_container(id);
        
        // Remove metadata
        {
            let mut meta_map = self.metadata.write().await;
            meta_map.remove(id);
        }
        
        // Remove name mapping
        if let Some(name) = name {
            let mut name_map = self.name_to_id.write().await;
            name_map.remove(&name);
        }
        
        Ok(())
    }

    /// Get all registered container IDs
    pub fn container_ids(&self) -> Vec<ContainerId> {
        self.registry.container_ids().iter().copied().collect()
    }

    /// Get containers by status
    pub async fn get_containers_by_status(&self, status: ContainerStatus) -> Vec<ContainerId> {
        let meta_map = self.metadata.read().await;
        meta_map
            .iter()
            .filter(|(_, metadata)| metadata.status == status)
            .map(|(id, _)| *id)
            .collect()
    }

    /// Get containers with resource usage above threshold
    pub async fn get_containers_with_high_usage(
        &self,
        cpu_threshold: f64,
        memory_threshold: u64,
    ) -> Vec<ContainerId> {
        let meta_map = self.metadata.read().await;
        meta_map
            .iter()
            .filter(|(_, metadata)| {
                if let Some(usage) = &metadata.resource_usage {
                    usage.cpu_usage_percent > cpu_threshold
                        || usage.memory_usage_bytes > memory_threshold
                } else {
                    false
                }
            })
            .map(|(id, _)| *id)
            .collect()
    }

    /// Get container uptime
    pub async fn get_uptime(&self, id: &ContainerId) -> Option<std::time::Duration> {
        let meta_map = self.metadata.read().await;
        meta_map.get(id).map(|m| m.created_at.elapsed())
    }

    /// Get total number of registered containers
    pub fn total_count(&self) -> usize {
        self.registry.total_count()
    }

    /// Clear all registered containers
    pub async fn clear(&self) {
        self.registry.clear();
        {
            let mut meta_map = self.metadata.write().await;
            meta_map.clear();
        }
        {
            let mut name_map = self.name_to_id.write().await;
            name_map.clear();
        }
    }

    /// Get registry statistics
    pub async fn get_statistics(&self) -> ContainerRegistryStatistics {
        let meta_map = self.metadata.read().await;
        let mut stats = ContainerRegistryStatistics {
            total_containers: meta_map.len(),
            running_containers: 0,
            stopped_containers: 0,
            unhealthy_containers: 0,
            unknown_containers: 0,
            total_cpu_usage: 0.0,
            total_memory_usage: 0,
            average_uptime: std::time::Duration::from_secs(0),
        };
        
        let mut total_uptime = std::time::Duration::from_secs(0);
        
        for metadata in meta_map.values() {
            match metadata.status {
                ContainerStatus::Running => stats.running_containers += 1,
                ContainerStatus::Stopped => stats.stopped_containers += 1,
                ContainerStatus::Unhealthy => stats.unhealthy_containers += 1,
                ContainerStatus::Unknown => stats.unknown_containers += 1,
                _ => {}
            }
            
            if let Some(usage) = &metadata.resource_usage {
                stats.total_cpu_usage += usage.cpu_usage_percent;
                stats.total_memory_usage += usage.memory_usage_bytes;
            }
            
            total_uptime += metadata.created_at.elapsed();
        }
        
        if !meta_map.is_empty() {
            stats.average_uptime = total_uptime / meta_map.len() as u32;
        }
        
        stats
    }
}

/// Container registry statistics
#[derive(Debug, Clone)]
pub struct ContainerRegistryStatistics {
    /// Total number of containers
    pub total_containers: usize,
    /// Number of running containers
    pub running_containers: usize,
    /// Number of stopped containers
    pub stopped_containers: usize,
    /// Number of unhealthy containers
    pub unhealthy_containers: usize,
    /// Number of containers with unknown status
    pub unknown_containers: usize,
    /// Total CPU usage across all containers
    pub total_cpu_usage: f64,
    /// Total memory usage across all containers
    pub total_memory_usage: u64,
    /// Average uptime across all containers
    pub average_uptime: std::time::Duration,
}

impl Default for ContainerIdRegistry {
    fn default() -> Self {
        Self::new()
    }
}

/// Container ID generator with deterministic options
pub struct ContainerIdGenerator {
    /// Counter for deterministic generation
    counter: u64,
    /// Whether to use deterministic generation
    deterministic: bool,
}

impl ContainerIdGenerator {
    /// Create a new container ID generator
    pub fn new() -> Self {
        Self {
            counter: 1,
            deterministic: false,
        }
    }

    /// Create a deterministic container ID generator
    pub fn deterministic() -> Self {
        Self {
            counter: 1,
            deterministic: true,
        }
    }

    /// Generate the next container ID
    pub fn next(&mut self) -> ContainerId {
        if self.deterministic {
            let id = ContainerId::from_value(self.counter).unwrap();
            self.counter += 1;
            id
        } else {
            ContainerId::new()
        }
    }

    /// Reset the counter (for deterministic generation)
    pub fn reset(&mut self) {
        self.counter = 1;
    }

    /// Get the current counter value
    pub fn counter(&self) -> u64 {
        self.counter
    }

    /// Check if generation is deterministic
    pub fn is_deterministic(&self) -> bool {
        self.deterministic
    }
}

impl Default for ContainerIdGenerator {
    fn default() -> Self {
        Self::new()
    }
}

/// Convenience function to create a container ID registry
pub fn container_id_registry() -> ContainerIdRegistry {
    ContainerIdRegistry::new()
}

/// Convenience function to create a container ID generator
pub fn container_id_generator() -> ContainerIdGenerator {
    ContainerIdGenerator::new()
}

/// Convenience function to create a deterministic container ID generator
pub fn deterministic_container_id_generator() -> ContainerIdGenerator {
    ContainerIdGenerator::deterministic()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_container_id_registry() {
        let registry = ContainerIdRegistry::new();
        
        let id = registry.register_container("test-container".to_string(), "alpine:latest".to_string()).await.unwrap();
        
        assert!(registry.is_registered(&id));
        assert_eq!(registry.total_count(), 1);
        
        let metadata = registry.get_metadata(&id).await.unwrap();
        assert_eq!(metadata.name, "test-container");
        assert_eq!(metadata.image, "alpine:latest");
        assert_eq!(metadata.status, ContainerStatus::Starting);
        
        registry.update_status(&id, ContainerStatus::Running).await.unwrap();
        let metadata = registry.get_metadata(&id).await.unwrap();
        assert_eq!(metadata.status, ContainerStatus::Running);
    }

    #[tokio::test]
    async fn test_container_resource_usage() {
        let registry = ContainerIdRegistry::new();
        
        let id = registry.register_container("test-container".to_string(), "alpine:latest".to_string()).await.unwrap();
        
        let usage = ContainerResourceUsage {
            cpu_usage_percent: 25.0,
            memory_usage_bytes: 512 * 1024 * 1024,
            disk_usage_bytes: 2 * 1024 * 1024 * 1024,
            network_bytes_sent: 1000,
            network_bytes_received: 2000,
        };
        
        registry.update_resource_usage(&id, usage.clone()).await.unwrap();
        
        let metadata = registry.get_metadata(&id).await.unwrap();
        assert!(metadata.resource_usage.is_some());
        assert!(metadata.last_health_check.is_some());
        
        let stored_usage = metadata.resource_usage.unwrap();
        assert_eq!(stored_usage.cpu_usage_percent, 25.0);
        assert_eq!(stored_usage.memory_usage_bytes, 512 * 1024 * 1024);
    }

    #[tokio::test]
    async fn test_container_name_mapping() {
        let registry = ContainerIdRegistry::new();
        
        let id = registry.register_container("test-container".to_string(), "alpine:latest".to_string()).await.unwrap();
        
        let found_id = registry.get_id_by_name("test-container").await.unwrap();
        assert_eq!(id, found_id);
        
        let found_name = registry.get_name_by_id(&id).await.unwrap();
        assert_eq!(found_name, "test-container");
    }

    #[tokio::test]
    async fn test_container_status_filtering() {
        let registry = ContainerIdRegistry::new();
        
        let id1 = registry.register_container("container1".to_string(), "alpine:latest".to_string()).await.unwrap();
        let id2 = registry.register_container("container2".to_string(), "alpine:latest".to_string()).await.unwrap();
        
        registry.update_status(&id1, ContainerStatus::Running).await.unwrap();
        registry.update_status(&id2, ContainerStatus::Stopped).await.unwrap();
        
        let running = registry.get_containers_by_status(ContainerStatus::Running).await;
        let stopped = registry.get_containers_by_status(ContainerStatus::Stopped).await;
        
        assert_eq!(running.len(), 1);
        assert_eq!(stopped.len(), 1);
        assert_eq!(running[0], id1);
        assert_eq!(stopped[0], id2);
    }

    #[tokio::test]
    async fn test_container_high_usage_filtering() {
        let registry = ContainerIdRegistry::new();
        
        let id1 = registry.register_container("container1".to_string(), "alpine:latest".to_string()).await.unwrap();
        let id2 = registry.register_container("container2".to_string(), "alpine:latest".to_string()).await.unwrap();
        
        let usage1 = ContainerResourceUsage {
            cpu_usage_percent: 90.0,
            memory_usage_bytes: 512 * 1024 * 1024,
            disk_usage_bytes: 2 * 1024 * 1024 * 1024,
            network_bytes_sent: 1000,
            network_bytes_received: 2000,
        };
        
        let usage2 = ContainerResourceUsage {
            cpu_usage_percent: 10.0,
            memory_usage_bytes: 100 * 1024 * 1024,
            disk_usage_bytes: 1 * 1024 * 1024 * 1024,
            network_bytes_sent: 500,
            network_bytes_received: 1000,
        };
        
        registry.update_resource_usage(&id1, usage1).await.unwrap();
        registry.update_resource_usage(&id2, usage2).await.unwrap();
        
        let high_usage = registry.get_containers_with_high_usage(80.0, 400 * 1024 * 1024).await;
        
        assert_eq!(high_usage.len(), 1);
        assert_eq!(high_usage[0], id1);
    }

    #[tokio::test]
    async fn test_container_statistics() {
        let registry = ContainerIdRegistry::new();
        
        let id1 = registry.register_container("container1".to_string(), "alpine:latest".to_string()).await.unwrap();
        let id2 = registry.register_container("container2".to_string(), "alpine:latest".to_string()).await.unwrap();
        
        registry.update_status(&id1, ContainerStatus::Running).await.unwrap();
        registry.update_status(&id2, ContainerStatus::Stopped).await.unwrap();
        
        let usage = ContainerResourceUsage {
            cpu_usage_percent: 25.0,
            memory_usage_bytes: 512 * 1024 * 1024,
            disk_usage_bytes: 2 * 1024 * 1024 * 1024,
            network_bytes_sent: 1000,
            network_bytes_received: 2000,
        };
        
        registry.update_resource_usage(&id1, usage).await.unwrap();
        
        let stats = registry.get_statistics().await;
        assert_eq!(stats.total_containers, 2);
        assert_eq!(stats.running_containers, 1);
        assert_eq!(stats.stopped_containers, 1);
        assert_eq!(stats.total_cpu_usage, 25.0);
        assert_eq!(stats.total_memory_usage, 512 * 1024 * 1024);
    }

    #[tokio::test]
    async fn test_container_id_generator() {
        let mut generator = ContainerIdGenerator::new();
        
        let id1 = generator.next();
        let id2 = generator.next();
        
        assert_ne!(id1, id2);
        assert!(id1.value() > 0);
        assert!(id2.value() > 0);
    }

    #[tokio::test]
    async fn test_deterministic_container_id_generator() {
        let mut generator = ContainerIdGenerator::deterministic();
        
        let id1 = generator.next();
        let id2 = generator.next();
        let id3 = generator.next();
        
        assert_ne!(id1, id2);
        assert_ne!(id2, id3);
        assert_ne!(id1, id3);
        
        assert_eq!(id1.value(), 1);
        assert_eq!(id2.value(), 2);
        assert_eq!(id3.value(), 3);
        
        generator.reset();
        let id4 = generator.next();
        assert_eq!(id4.value(), 1);
    }

    #[tokio::test]
    async fn test_convenience_functions() {
        let _registry = container_id_registry();
        let _generator = container_id_generator();
        let _deterministic_generator = deterministic_container_id_generator();
        
        // Just verify they compile and create valid instances
        assert_eq!(_registry.total_count(), 0);
        assert!(_generator.counter() >= 1);
        assert!(_deterministic_generator.is_deterministic());
    }
}
