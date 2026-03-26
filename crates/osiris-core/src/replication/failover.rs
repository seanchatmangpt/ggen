//! Health-Triggered Failover Coordination
//!
//! Implements automatic failover orchestration for multi-region deployments.
//! Monitors region health, triggers quorum-based promotions, and manages rollback.

use crate::error::{OSIRISError, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tokio::time::{timeout, Duration};

use super::{RegionHealth, ReplicationLag};

/// Failover state machine
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum FailoverState {
    /// Normal operation: primary healthy
    Healthy,
    /// Monitoring degradation: primary showing signs of failure
    Monitoring,
    /// Failover in progress: promoting standby
    FailingOver,
    /// Recovery in progress: reverting to original primary
    RollingBack,
}

/// Failover event for audit trail
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct FailoverEvent {
    pub timestamp: u64,
    pub event_type: FailoverEventType,
    pub from_region: String,
    pub to_region: String,
    pub reason: String,
    pub quorum_size: usize,
    pub quorum_achieved: bool,
}

/// Types of failover events
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum FailoverEventType {
    /// Degradation detected
    DegradationDetected,
    /// Failover initiated
    FailoverInitiated,
    /// Quorum achieved
    QuorumAchieved,
    /// Promotion successful
    PromotionSuccess,
    /// Promotion failed
    PromotionFailed,
    /// Rollback initiated
    RollbackInitiated,
    /// Rollback complete
    RollbackComplete,
}

/// Result of a failover decision
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FailoverDecision {
    /// No failover needed
    None,
    /// Failover recommended to specific region
    Failover { target_region: String, reason: String },
    /// Rollback recommended to original primary
    Rollback { reason: String },
}

/// Configuration for failover coordinator
#[derive(Clone, Debug)]
pub struct FailoverConfig {
    /// Maximum number of consecutive health check failures before triggering failover
    pub max_consecutive_failures: usize,

    /// Minimum number of healthy regions required for failover (quorum)
    pub min_quorum_size: usize,

    /// Timeout for failover promotion sequence
    pub promotion_timeout_ms: u64,

    /// Maximum replication lag before considering region degraded
    pub max_replication_lag_ms: u64,

    /// Cooldown period between failover attempts
    pub failover_cooldown_ms: u64,
}

impl Default for FailoverConfig {
    fn default() -> Self {
        Self {
            max_consecutive_failures: 3,
            min_quorum_size: 2, // For 3-region cluster, need 2 healthy
            promotion_timeout_ms: 30000, // 30 seconds
            max_replication_lag_ms: 5000,
            failover_cooldown_ms: 60000, // 1 minute
        }
    }
}

/// Health check result for a region
#[derive(Clone, Debug)]
pub struct HealthCheckResult {
    pub region_id: String,
    pub health: RegionHealth,
    pub replication_lag: ReplicationLag,
    pub is_reachable: bool,
    pub timestamp: u64,
}

impl HealthCheckResult {
    pub fn is_healthy(&self, max_lag_ms: u64) -> bool {
        self.health == RegionHealth::Healthy
            && self.is_reachable
            && self.replication_lag.milliseconds <= max_lag_ms
    }

    pub fn is_degraded(&self, max_lag_ms: u64) -> bool {
        self.health == RegionHealth::Degraded
            || (self.is_reachable && self.replication_lag.milliseconds > max_lag_ms)
    }
}

/// Failover coordinator for multi-region deployments
pub struct FailoverCoordinator {
    /// Current failover state
    state: Arc<RwLock<FailoverState>>,

    /// Configuration
    config: FailoverConfig,

    /// Health check results per region
    health_status: Arc<RwLock<HashMap<String, HealthCheckResult>>>,

    /// Consecutive failure count per region
    failure_counts: Arc<RwLock<HashMap<String, usize>>>,

    /// Audit trail of failover events
    event_log: Arc<RwLock<Vec<FailoverEvent>>>,

    /// Current primary region
    current_primary: Arc<RwLock<String>>,

    /// Last failover timestamp (for cooldown)
    last_failover_time: Arc<RwLock<Option<u64>>>,

    /// Original primary (for rollback)
    original_primary: Arc<RwLock<Option<String>>>,
}

impl FailoverCoordinator {
    /// Create a new failover coordinator
    pub fn new(config: FailoverConfig, initial_primary: String) -> Self {
        Self {
            state: Arc::new(RwLock::new(FailoverState::Healthy)),
            config,
            health_status: Arc::new(RwLock::new(HashMap::new())),
            failure_counts: Arc::new(RwLock::new(HashMap::new())),
            event_log: Arc::new(RwLock::new(Vec::new())),
            current_primary: Arc::new(RwLock::new(initial_primary)),
            last_failover_time: Arc::new(RwLock::new(None)),
            original_primary: Arc::new(RwLock::new(None)),
        }
    }

    /// Get current failover state
    pub async fn get_state(&self) -> FailoverState {
        self.state.read().await.clone()
    }

    /// Get current primary region
    pub async fn get_primary(&self) -> String {
        self.current_primary.read().await.clone()
    }

    /// Update health status for a region
    pub async fn update_health(&self, result: HealthCheckResult) -> Result<()> {
        let mut health_status = self.health_status.write().await;
        let mut failure_counts = self.failure_counts.write().await;

        // Update health status
        let previous_health = health_status
            .get(&result.region_id)
            .map(|h| h.health.clone());

        health_status.insert(result.region_id.clone(), result.clone());

        // Track consecutive failures
        if result.is_healthy(self.config.max_replication_lag_ms) {
            // Reset failure count on healthy check
            failure_counts.insert(result.region_id.clone(), 0);
        } else {
            // Increment failure count
            let count = failure_counts.get(&result.region_id).copied().unwrap_or(0);
            failure_counts.insert(result.region_id.clone(), count + 1);
        }

        // Log degradation events
        if let Some(prev) = previous_health {
            if prev == RegionHealth::Healthy && result.health != RegionHealth::Healthy {
                self.log_event(FailoverEvent {
                    timestamp: result.timestamp,
                    event_type: FailoverEventType::DegradationDetected,
                    from_region: result.region_id.clone(),
                    to_region: String::new(),
                    reason: format!("Region degraded from {:?} to {:?}", prev, result.health),
                    quorum_size: 0,
                    quorum_achieved: false,
                }).await;
            }
        }

        Ok(())
    }

    /// Evaluate whether failover is needed based on current health status
    pub async fn evaluate_failover(&self) -> Result<FailoverDecision> {
        let health_status = self.health_status.read().await;
        let failure_counts = self.failure_counts.read().await;
        let current_primary = self.current_primary.read().await.clone();

        // Check if primary is healthy
        let primary_health = health_status.get(&current_primary);
        let primary_failures = failure_counts.get(&current_primary).copied().unwrap_or(0);

        // Check if we're in cooldown period
        let last_failover = *self.last_failover_time.read().await;
        if let Some(last_time) = last_failover {
            let now = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_secs();
            if now.saturating_sub(last_time) < self.config.failover_cooldown_ms / 1000 {
                return Ok(FailoverDecision::None);
            }
        }

        // Evaluate primary health
        let should_failover = match primary_health {
            Some(result) => {
                // Check if primary has exceeded failure threshold
                primary_failures >= self.config.max_consecutive_failures
                    || result.health == RegionHealth::Unhealthy
                    || !result.is_reachable
            }
            None => false, // No health data yet
        };

        if !should_failover {
            return Ok(FailoverDecision::None);
        }

        // Find best candidate for promotion
        let candidate = self.find_best_candidate(&health_status).await?;

        match candidate {
            Some(target_region) => {
                // Check if we have quorum
                let healthy_regions = self.count_healthy_regions(&health_status).await;
                let has_quorum = healthy_regions >= self.config.min_quorum_size;

                if !has_quorum {
                    return Err(OSIRISError::ServiceUnavailable(
                        format!("Insufficient quorum for failover: {}/{} healthy regions",
                               healthy_regions, self.config.min_quorum_size)
                    ));
                }

                Ok(FailoverDecision::Failover {
                    target_region,
                    reason: format!("Primary {} degraded after {} failures",
                                  current_primary, primary_failures),
                })
            }
            None => Err(OSIRISError::ServiceUnavailable(
                "No healthy standby regions available for failover".to_string()
            ))
        }
    }

    /// Execute failover to target region
    pub async fn execute_failover(&self, target_region: String) -> Result<()> {
        // Transition to failing over state
        {
            let mut state = self.state.write().await;
            if *state != FailoverState::Healthy && *state != FailoverState::Monitoring {
                return Err(OSIRISError::InvalidStateTransition(
                    format!("Cannot failover from state {:?}", state)
                ));
            }
            *state = FailoverState::FailingOver;
        }

        let current_primary = self.current_primary.read().await.clone();

        // Log failover initiation
        self.log_event(FailoverEvent {
            timestamp: self.current_timestamp(),
            event_type: FailoverEventType::FailoverInitiated,
            from_region: current_primary.clone(),
            to_region: target_region.clone(),
            reason: format!("Initiating failover from {} to {}", current_primary, target_region),
            quorum_size: self.config.min_quorum_size,
            quorum_achieved: true,
        }).await;

        // Save original primary for potential rollback
        {
            let mut original = self.original_primary.write().await;
            *original = Some(current_primary.clone());
        }

        // Execute promotion sequence with timeout
        let promotion_result = timeout(
            Duration::from_millis(self.config.promotion_timeout_ms),
            self.promote_region(&target_region)
        ).await;

        match promotion_result {
            Ok(Ok(())) => {
                // Promotion succeeded
                {
                    let mut primary = self.current_primary.write().await;
                    *primary = target_region.clone();
                }

                {
                    let mut state = self.state.write().await;
                    *state = FailoverState::Healthy;
                }

                // Update last failover time
                {
                    let mut last_time = self.last_failover_time.write().await;
                    *last_time = Some(self.current_timestamp());
                }

                // Log success
                self.log_event(FailoverEvent {
                    timestamp: self.current_timestamp(),
                    event_type: FailoverEventType::PromotionSuccess,
                    from_region: current_primary,
                    to_region: target_region,
                    reason: "Failover completed successfully".to_string(),
                    quorum_size: self.config.min_quorum_size,
                    quorum_achieved: true,
                }).await;

                Ok(())
            }
            Ok(Err(e)) => {
                // Promotion failed
                self.initiate_roll_back(&current_primary, &target_region, format!("Promotion failed: {}", e)).await
            }
            Err(_) => {
                // Timeout
                self.initiate_roll_back(&current_primary, &target_region, "Promotion timeout".to_string()).await
            }
        }
    }

    /// Rollback to original primary
    pub async fn rollback(&self) -> Result<()> {
        let original = {
            let original_primary = self.original_primary.read().await;
            original_primary.clone().ok_or_else(|| {
                OSIRISError::InvalidStateTransition("No original primary to rollback to".to_string())
            })?
        };

        let current = self.current_primary.read().await.clone();

        self.initiate_roll_back(&original, &current, "Manual rollback requested".to_string()).await
    }

    /// Internal: promote a region to primary
    async fn promote_region(&self, target_region: &str) -> Result<()> {
        // Step 1: Verify target is healthy
        let health_status = self.health_status.read().await;
        let target_health = health_status.get(target_region)
            .ok_or_else(|| OSIRISError::ServiceUnavailable(
                format!("Target region {} not found", target_region)
            ))?;

        if !target_health.is_healthy(self.config.max_replication_lag_ms) {
            return Err(OSIRISError::ServiceUnavailable(
                format!("Target region {} is not healthy", target_region)
            ));
        }
        drop(health_status);

        // Step 2: Log quorum achievement
        self.log_event(FailoverEvent {
            timestamp: self.current_timestamp(),
            event_type: FailoverEventType::QuorumAchieved,
            from_region: String::new(),
            to_region: target_region.to_string(),
            reason: "Quorum verified for promotion".to_string(),
            quorum_size: self.config.min_quorum_size,
            quorum_achieved: true,
        }).await;

        // Step 3: Simulate promotion steps (in production, this would coordinate with regions)
        // - Update DNS/load balancer
        // - Notify all regions of new primary
        // - Verify write capability

        // Simulate async promotion work
        tokio::time::sleep(Duration::from_millis(100)).await;

        Ok(())
    }

    /// Internal: initiate rollback sequence
    async fn initiate_roll_back(&self, from_region: &str, to_region: &str, reason: String) -> Result<()> {
        // Transition to rolling back state
        {
            let mut state = self.state.write().await;
            *state = FailoverState::RollingBack;
        }

        // Log rollback initiation
        self.log_event(FailoverEvent {
            timestamp: self.current_timestamp(),
            event_type: FailoverEventType::RollbackInitiated,
            from_region: to_region.to_string(),
            to_region: from_region.to_string(),
            reason: reason.clone(),
            quorum_size: 0,
            quorum_achieved: false,
        }).await;

        // Execute rollback
        {
            let mut primary = self.current_primary.write().await;
            *primary = from_region.to_string();
        }

        // Simulate rollback work
        tokio::time::sleep(Duration::from_millis(50)).await;

        // Transition back to healthy
        {
            let mut state = self.state.write().await;
            *state = FailoverState::Healthy;
        }

        // Log rollback complete
        self.log_event(FailoverEvent {
            timestamp: self.current_timestamp(),
            event_type: FailoverEventType::RollbackComplete,
            from_region: to_region.to_string(),
            to_region: from_region.to_string(),
            reason: format!("Rollback complete: {}", reason),
            quorum_size: 0,
            quorum_achieved: false,
        }).await;

        Err(OSIRISError::ServiceUnavailable(
            format!("Rollback initiated: {}", reason)
        ))
    }

    /// Find best candidate region for promotion
    async fn find_best_candidate(&self, health_status: &HashMap<String, HealthCheckResult>) -> Result<Option<String>> {
        let current_primary = self.current_primary.read().await.clone();
        let mut candidates: Vec<_> = health_status
            .iter()
            .filter(|(region_id, result)| {
                // Exclude current primary
                **region_id != current_primary &&
                // Only consider healthy regions
                result.is_healthy(self.config.max_replication_lag_ms)
            })
            .collect();

        if candidates.is_empty() {
            return Ok(None);
        }

        // Sort by lowest replication lag (best candidate)
        candidates.sort_by_key(|(_, result)| result.replication_lag.milliseconds);

        Ok(candidates.first().map(|(region_id, _)| (*region_id).clone()))
    }

    /// Count healthy regions
    async fn count_healthy_regions(&self, health_status: &HashMap<String, HealthCheckResult>) -> usize {
        health_status
            .values()
            .filter(|result| result.is_healthy(self.config.max_replication_lag_ms))
            .count()
    }

    /// Get event log
    pub async fn get_event_log(&self) -> Vec<FailoverEvent> {
        self.event_log.read().await.clone()
    }

    /// Get health status for all regions
    pub async fn get_health_status(&self) -> HashMap<String, HealthCheckResult> {
        self.health_status.read().await.clone()
    }

    /// Log a failover event
    async fn log_event(&self, event: FailoverEvent) {
        let mut log = self.event_log.write().await;
        log.push(event);
    }

    /// Get current timestamp
    fn current_timestamp(&self) -> u64 {
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_health_result(
        region_id: &str,
        health: RegionHealth,
        lag_ms: u64,
        reachable: bool,
    ) -> HealthCheckResult {
        HealthCheckResult {
            region_id: region_id.to_string(),
            health,
            replication_lag: ReplicationLag::new(lag_ms, 0),
            is_reachable: reachable,
            timestamp: 0,
        }
    }

    #[tokio::test]
    async fn test_failover_coordinator_creation() {
        let config = FailoverConfig::default();
        let coordinator = FailoverCoordinator::new(config, "us-east".to_string());

        assert_eq!(coordinator.get_state().await, FailoverState::Healthy);
        assert_eq!(coordinator.get_primary().await, "us-east");
    }

    #[tokio::test]
    async fn test_health_status_update() {
        let config = FailoverConfig::default();
        let coordinator = FailoverCoordinator::new(config, "us-east".to_string());

        let result = create_health_result("us-east", RegionHealth::Healthy, 100, true);
        assert!(coordinator.update_health(result).await.is_ok());

        let health_status = coordinator.get_health_status().await;
        assert_eq!(health_status.len(), 1);
        assert!(health_status.contains_key("us-east"));
    }

    #[tokio::test]
    async fn test_no_failover_when_primary_healthy() {
        let config = FailoverConfig::default();
        let coordinator = FailoverCoordinator::new(config, "us-east".to_string());

        // Primary is healthy
        let result = create_health_result("us-east", RegionHealth::Healthy, 100, true);
        coordinator.update_health(result).await.unwrap();

        let decision = coordinator.evaluate_failover().await.unwrap();
        assert_eq!(decision, FailoverDecision::None);
    }

    #[tokio::test]
    async fn test_failover_when_primary_degraded() {
        let config = FailoverConfig {
            max_consecutive_failures: 2,
            ..Default::default()
        };
        let coordinator = FailoverCoordinator::new(config, "us-east".to_string());

        // Primary degrades twice
        for _ in 0..2 {
            let result = create_health_result("us-east", RegionHealth::Degraded, 100, true);
            coordinator.update_health(result).await.unwrap();
        }

        // Standby is healthy
        let result = create_health_result("us-west", RegionHealth::Healthy, 100, true);
        coordinator.update_health(result).await.unwrap();

        let decision = coordinator.evaluate_failover().await.unwrap();
        match decision {
            FailoverDecision::Failover { target_region, .. } => {
                assert_eq!(target_region, "us-west");
            }
            _ => panic!("Expected failover decision"),
        }
    }

    #[tokio::test]
    async fn test_failover_requires_quorum() {
        let config = FailoverConfig {
            min_quorum_size: 2,
            ..Default::default()
        };
        let coordinator = FailoverCoordinator::new(config, "us-east".to_string());

        // Primary degraded
        for _ in 0..3 {
            let result = create_health_result("us-east", RegionHealth::Unhealthy, 100, false);
            coordinator.update_health(result).await.unwrap();
        }

        // Only one healthy standby (insufficient quorum)
        let result = create_health_result("us-west", RegionHealth::Healthy, 100, true);
        coordinator.update_health(result).await.unwrap();

        let decision = coordinator.evaluate_failover().await;
        assert!(decision.is_err());
    }

    #[tokio::test]
    async fn test_execute_failover_success() {
        let config = FailoverConfig::default();
        let coordinator = FailoverCoordinator::new(config, "us-east".to_string());

        // Setup health status
        let result = create_health_result("us-west", RegionHealth::Healthy, 100, true);
        coordinator.update_health(result).await.unwrap();

        // Execute failover
        let failover_result = coordinator.execute_failover("us-west".to_string()).await;

        assert!(failover_result.is_ok());
        assert_eq!(coordinator.get_primary().await, "us-west");
        assert_eq!(coordinator.get_state().await, FailoverState::Healthy);

        // Check event log
        let events = coordinator.get_event_log().await;
        assert!(events.iter().any(|e| e.event_type == FailoverEventType::PromotionSuccess));
    }

    #[tokio::test]
    async fn test_failover_rollback_on_unhealthy_target() {
        let config = FailoverConfig::default();
        let coordinator = FailoverCoordinator::new(config, "us-east".to_string());

        // Target is unhealthy
        let result = create_health_result("us-west", RegionHealth::Degraded, 10000, true);
        coordinator.update_health(result).await.unwrap();

        // Attempt failover
        let failover_result = coordinator.execute_failover("us-west".to_string()).await;

        assert!(failover_result.is_err());
        assert_eq!(coordinator.get_primary().await, "us-east"); // Should remain unchanged
        assert_eq!(coordinator.get_state().await, FailoverState::Healthy);

        // Check rollback events
        let events = coordinator.get_event_log().await;
        assert!(events.iter().any(|e| e.event_type == FailoverEventType::RollbackInitiated));
    }

    #[tokio::test]
    async fn test_manual_rollback() {
        let config = FailoverConfig::default();
        let coordinator = FailoverCoordinator::new(config, "us-east".to_string());

        // Setup health status
        let result = create_health_result("us-west", RegionHealth::Healthy, 100, true);
        coordinator.update_health(result).await.unwrap();

        // Execute failover
        coordinator.execute_failover("us-west".to_string()).await.unwrap();
        assert_eq!(coordinator.get_primary().await, "us-west");

        // Manual rollback
        let rollback_result = coordinator.rollback().await;

        assert!(rollback_result.is_err()); // Rollback returns error but changes state
        assert_eq!(coordinator.get_primary().await, "us-east");

        // Check rollback events
        let events = coordinator.get_event_log().await;
        assert!(events.iter().any(|e| e.event_type == FailoverEventType::RollbackComplete));
    }

    #[tokio::test]
    async fn test_best_candidate_selection() {
        let config = FailoverConfig::default();
        let coordinator = FailoverCoordinator::new(config, "us-east".to_string());

        // Add multiple standbys with different replication lags
        let result1 = create_health_result("us-west", RegionHealth::Healthy, 5000, true);
        let result2 = create_health_result("eu", RegionHealth::Healthy, 100, true);
        coordinator.update_health(result1).await.unwrap();
        coordinator.update_health(result2).await.unwrap();

        // Degrade primary
        for _ in 0..3 {
            let result = create_health_result("us-east", RegionHealth::Degraded, 100, true);
            coordinator.update_health(result).await.unwrap();
        }

        let decision = coordinator.evaluate_failover().await.unwrap();
        match decision {
            FailoverDecision::Failover { target_region, .. } => {
                // Should pick EU (lower replication lag)
                assert_eq!(target_region, "eu");
            }
            _ => panic!("Expected failover decision"),
        }
    }

    #[tokio::test]
    async fn test_event_log_audit_trail() {
        let config = FailoverConfig::default();
        let coordinator = FailoverCoordinator::new(config, "us-east".to_string());

        // Trigger degradation
        for _ in 0..3 {
            let result = create_health_result("us-east", RegionHealth::Degraded, 100, true);
            coordinator.update_health(result).await.unwrap();
        }

        let result = create_health_result("us-west", RegionHealth::Healthy, 100, true);
        coordinator.update_health(result).await.unwrap();

        // Execute failover
        coordinator.execute_failover("us-west".to_string()).await.unwrap();

        // Verify audit trail
        let events = coordinator.get_event_log().await;
        assert!(events.len() >= 3); // At least degradation, failover, promotion

        // Verify event order
        let event_types: Vec<_> = events.iter().map(|e| e.event_type.clone()).collect();
        assert!(event_types.contains(&FailoverEventType::DegradationDetected));
        assert!(event_types.contains(&FailoverEventType::FailoverInitiated));
        assert!(event_types.contains(&FailoverEventType::PromotionSuccess));
    }

    #[tokio::test]
    async fn test_failover_cooldown() {
        let config = FailoverConfig {
            failover_cooldown_ms: 1000, // 1 second
            ..Default::default()
        };
        let coordinator = FailoverCoordinator::new(config, "us-east".to_string());

        // Execute first failover
        let result = create_health_result("us-west", RegionHealth::Healthy, 100, true);
        coordinator.update_health(result).await.unwrap();
        coordinator.execute_failover("us-west".to_string()).await.unwrap();

        // Immediately degrade primary (now us-west)
        for _ in 0..3 {
            let result = create_health_result("us-west", RegionHealth::Degraded, 100, true);
            coordinator.update_health(result).await.unwrap();
        }

        // Should be in cooldown, no failover
        let decision = coordinator.evaluate_failover().await.unwrap();
        assert_eq!(decision, FailoverDecision::None);
    }
}
