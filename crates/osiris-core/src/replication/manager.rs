//! Multi-Region Manager
//!
//! Orchestrates replication across US-East, US-West, and EU regions.
//! Manages region membership, health status, vector clock coordination, and failover.

use crate::error::{OSIRISError, Result};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use serde::{Deserialize, Serialize};

use super::vector_clock::VectorClock;

/// Health status of a region
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum RegionHealth {
    Healthy,
    Degraded,
    Unhealthy,
    Unknown,
}

/// Replication lag information
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ReplicationLag {
    pub milliseconds: u64,
    pub event_count: u64,
}

impl ReplicationLag {
    pub fn new(milliseconds: u64, event_count: u64) -> Self {
        Self {
            milliseconds,
            event_count,
        }
    }

    pub fn zero() -> Self {
        Self {
            milliseconds: 0,
            event_count: 0,
        }
    }
}

/// A region node in the multi-region cluster
#[derive(Clone, Debug)]
pub struct RegionNode {
    pub region_id: String,
    pub endpoint: String,
    pub health: RegionHealth,
    pub replication_lag: ReplicationLag,
    pub vector_clock: VectorClock,
    pub is_primary: bool,
}

impl RegionNode {
    pub fn new(region_id: String, endpoint: String, is_primary: bool) -> Self {
        Self {
            region_id: region_id.clone(),
            endpoint,
            health: RegionHealth::Unknown,
            replication_lag: ReplicationLag::zero(),
            vector_clock: VectorClock::new(),
            is_primary,
        }
    }

    pub fn set_health(&mut self, health: RegionHealth) {
        self.health = health;
    }

    pub fn set_replication_lag(&mut self, lag: ReplicationLag) {
        self.replication_lag = lag;
    }

    pub fn update_vector_clock(&mut self, vc: &VectorClock) {
        self.vector_clock.merge(vc);
    }
}

/// Configuration for MultiRegionManager
#[derive(Clone, Debug)]
pub struct MultiRegionConfig {
    pub max_replication_lag_ms: u64,
    pub health_check_interval_ms: u64,
    pub failover_threshold_failures: usize,
}

impl Default for MultiRegionConfig {
    fn default() -> Self {
        Self {
            max_replication_lag_ms: 5000,
            health_check_interval_ms: 30000,
            failover_threshold_failures: 3,
        }
    }
}

/// Manages multi-region replication and failover
pub struct MultiRegionManager {
    /// Map of region ID to RegionNode
    regions: Arc<RwLock<HashMap<String, RegionNode>>>,

    /// Shared vector clock for coordinating causality across regions
    vector_clock: Arc<RwLock<VectorClock>>,

    /// Current primary region
    primary_region: Arc<RwLock<String>>,

    /// Configuration
    config: MultiRegionConfig,
}

impl MultiRegionManager {
    /// Create a new MultiRegionManager
    pub fn new(config: MultiRegionConfig) -> Self {
        Self {
            regions: Arc::new(RwLock::new(HashMap::new())),
            vector_clock: Arc::new(RwLock::new(VectorClock::new())),
            primary_region: Arc::new(RwLock::new("us-east".to_string())),
            config,
        }
    }

    /// Create a new MultiRegionManager with default configuration
    pub async fn default_with_regions(region_ids: &[&str]) -> Result<Self> {
        let mut manager = Self::new(MultiRegionConfig::default());
        for (idx, region_id) in region_ids.iter().enumerate() {
            manager.add_region(
                region_id.to_string(),
                format!("https://{}.example.com", region_id),
                idx == 0, // First region is primary
            ).await?;
        }
        Ok(manager)
    }

    /// Add a region to the cluster
    pub async fn add_region(
        &mut self,
        region_id: String,
        endpoint: String,
        is_primary: bool,
    ) -> Result<()> {
        let mut regions = self.regions.write().await;

        if regions.contains_key(&region_id) {
            return Err(OSIRISError::ServiceUnavailable(
                format!("Region {} already exists", region_id),
            ));
        }

        let mut node = RegionNode::new(region_id.clone(), endpoint, is_primary);

        // Initialize vector clock with this region
        let vc = self.vector_clock.read().await.clone();
        node.vector_clock = vc;

        regions.insert(region_id.clone(), node);

        if is_primary {
            let mut primary = self.primary_region.write().await;
            *primary = region_id;
        }

        Ok(())
    }

    /// Get a region by ID (blocking read)
    pub async fn get_region(&self, region_id: &str) -> Result<RegionNode> {
        let regions = self.regions.read().await;
        regions
            .get(region_id)
            .cloned()
            .ok_or_else(|| OSIRISError::ServiceUnavailable(format!("Region {} not found", region_id)))
    }

    /// Get all regions
    pub async fn get_regions(&self) -> Result<Vec<RegionNode>> {
        let regions = self.regions.read().await;
        Ok(regions.values().cloned().collect())
    }

    /// Get primary region ID
    pub async fn get_primary_region(&self) -> Result<String> {
        Ok(self.primary_region.read().await.clone())
    }

    /// Update region health status
    pub async fn update_region_health(
        &self,
        region_id: &str,
        health: RegionHealth,
    ) -> Result<()> {
        let mut regions = self.regions.write().await;
        let region = regions
            .get_mut(region_id)
            .ok_or_else(|| OSIRISError::ServiceUnavailable(format!("Region {} not found", region_id)))?;
        region.set_health(health);
        Ok(())
    }

    /// Update replication lag for a region
    pub async fn update_replication_lag(
        &self,
        region_id: &str,
        lag: ReplicationLag,
    ) -> Result<()> {
        let mut regions = self.regions.write().await;
        let region = regions
            .get_mut(region_id)
            .ok_or_else(|| OSIRISError::ServiceUnavailable(format!("Region {} not found", region_id)))?;
        region.set_replication_lag(lag);
        Ok(())
    }

    /// Get global vector clock
    pub async fn get_vector_clock(&self) -> VectorClock {
        self.vector_clock.read().await.clone()
    }

    /// Update global vector clock
    pub async fn update_vector_clock(&self, vc: &VectorClock) {
        let mut global_vc = self.vector_clock.write().await;
        global_vc.merge(vc);
    }

    /// Increment vector clock for a region (represents a local write)
    pub async fn increment_vector_clock(&self, region_id: &str) -> Result<VectorClock> {
        let mut vc = self.vector_clock.write().await;
        vc.increment(region_id);
        Ok(vc.clone())
    }

    /// Check if a region is primary
    pub async fn is_primary(&self, region_id: &str) -> Result<bool> {
        let primary = self.primary_region.read().await;
        Ok(primary.as_str() == region_id)
    }

    /// Detect if two events are causally ordered
    pub async fn detect_causality(&self, vc1: &VectorClock, vc2: &VectorClock) -> CausalityResult {
        if vc1.happens_before(vc2) {
            CausalityResult::Causality {
                happens_before: true,
            }
        } else if vc2.happens_before(vc1) {
            CausalityResult::Causality {
                happens_before: false,
            }
        } else if vc1.concurrent(vc2) {
            CausalityResult::Concurrent
        } else {
            CausalityResult::Same
        }
    }
}

impl Clone for MultiRegionManager {
    fn clone(&self) -> Self {
        Self {
            regions: Arc::clone(&self.regions),
            vector_clock: Arc::clone(&self.vector_clock),
            primary_region: Arc::clone(&self.primary_region),
            config: self.config.clone(),
        }
    }
}

/// Result of detecting causality between two events
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CausalityResult {
    /// Events are causally related: first event (true) or second (false)
    Causality { happens_before: bool },
    /// Events are concurrent (neither happens before the other)
    Concurrent,
    /// Same event (same vector clock)
    Same,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_create_manager() {
        let manager = MultiRegionManager::new(MultiRegionConfig::default());
        assert!(manager.get_regions().await.is_ok());
    }

    #[tokio::test]
    async fn test_add_region() {
        let mut manager = MultiRegionManager::new(MultiRegionConfig::default());
        let result = manager
            .add_region(
                "us-east".to_string(),
                "https://us-east.example.com".to_string(),
                true,
            )
            .await;
        assert!(result.is_ok());

        let region = manager.get_region("us-east").await;
        assert!(region.is_ok());
        assert_eq!(region.unwrap().region_id, "us-east");
    }

    #[tokio::test]
    async fn test_three_region_cluster() {
        let manager_result = MultiRegionManager::default_with_regions(&["us-east", "us-west", "eu"]).await;
        assert!(manager_result.is_ok());

        let manager = manager_result.unwrap();
        let regions: Vec<_> = manager.get_regions().await.unwrap();
        assert_eq!(regions.len(), 3);

        let primary: String = manager.get_primary_region().await.unwrap();
        assert_eq!(primary, "us-east");
    }

    #[tokio::test]
    async fn test_vector_clock_increment() {
        let manager = MultiRegionManager::default_with_regions(&["us-east", "us-west"])
            .await
            .unwrap();

        let vc1: VectorClock = manager.increment_vector_clock("us-east").await.unwrap();
        let vc2: VectorClock = manager.increment_vector_clock("us-west").await.unwrap();

        // vc2 should have happened after vc1
        assert!(vc1.happens_before(&vc2));
    }

    #[tokio::test]
    async fn test_detect_causality() {
        let manager = MultiRegionManager::default_with_regions(&["us-east", "us-west"])
            .await
            .unwrap();

        let mut vc1 = VectorClock::with_regions(&["us-east", "us-west"]);
        vc1.increment("us-east");

        let mut vc2 = VectorClock::with_regions(&["us-east", "us-west"]);
        vc2.increment("us-east");
        vc2.increment("us-west");

        let result = manager.detect_causality(&vc1, &vc2).await;
        assert_eq!(result, CausalityResult::Causality { happens_before: true });
    }

    #[tokio::test]
    async fn test_detect_concurrent_events() {
        let manager = MultiRegionManager::default_with_regions(&["us-east", "us-west"])
            .await
            .unwrap();

        let mut vc1 = VectorClock::with_regions(&["us-east", "us-west"]);
        vc1.increment("us-east");

        let mut vc2 = VectorClock::with_regions(&["us-east", "us-west"]);
        vc2.increment("us-west");

        let result = manager.detect_causality(&vc1, &vc2).await;
        assert_eq!(result, CausalityResult::Concurrent);
    }
}
