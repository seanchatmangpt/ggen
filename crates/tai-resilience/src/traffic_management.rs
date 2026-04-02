//! Traffic management for safe deployments
//!
//! This module provides orchestration for:
//! - Canary deployments (gradual traffic increase)
//! - Blue-green deployments (instant switchover)
//! - A/B testing (traffic splitting)
//! - Traffic mirroring (shadow traffic)

use crate::error::{Error, Result};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::time::Duration;
use tokio::time::sleep;
use tracing::{debug, info, warn};

/// Load balancing algorithm
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum LoadBalancingAlgorithm {
    /// Round-robin distribution
    RoundRobin,
    /// Least connections
    LeastConnection,
    /// Random selection
    Random,
}

impl Default for LoadBalancingAlgorithm {
    fn default() -> Self {
        LoadBalancingAlgorithm::RoundRobin
    }
}

/// Traffic split for routing requests
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrafficSplit {
    /// Primary destination weight (0-100)
    pub primary_weight: u32,
    /// Canary destination weight (0-100)
    pub canary_weight: u32,
    /// Primary destination host
    pub primary_host: String,
    /// Canary destination host
    pub canary_host: String,
    /// Request timeout
    pub timeout: Duration,
}

impl TrafficSplit {
    /// Create a new traffic split configuration
    pub fn new(
        primary_host: impl Into<String>, canary_host: impl Into<String>, primary_weight: u32,
        canary_weight: u32,
    ) -> Result<Self> {
        if primary_weight + canary_weight != 100 {
            return Err(Error::InvalidTrafficSplit {
                reason: "Weights must sum to 100".to_string(),
            });
        }

        Ok(Self {
            primary_weight,
            canary_weight,
            primary_host: primary_host.into(),
            canary_host: canary_host.into(),
            timeout: Duration::from_secs(30),
        })
    }

    /// Get routing decision for a request
    pub fn route_request(&self, request_id: u64) -> String {
        let random_value = (request_id % 100) as u32;
        if random_value < self.primary_weight {
            self.primary_host.clone()
        } else {
            self.canary_host.clone()
        }
    }
}

/// Canary deployment orchestrator
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CanaryDeployment {
    /// Service name
    pub service_name: String,
    /// Current version
    pub current_version: String,
    /// New version to deploy
    pub new_version: String,
    /// Current stage (0-100%)
    pub traffic_percentage: u32,
    /// Stages to deploy through (e.g., [10, 50, 100])
    pub stages: Vec<u32>,
    /// Current stage index
    pub current_stage: usize,
    /// Metrics threshold for rollback
    pub error_rate_threshold: f32,
    /// Start time
    pub started_at: Option<DateTime<Utc>>,
    /// Status
    pub status: DeploymentStatus,
}

/// Deployment status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum DeploymentStatus {
    /// Deployment has started
    InProgress,
    /// Deployment completed successfully
    Completed,
    /// Deployment rolled back
    RolledBack,
    /// Deployment paused
    Paused,
    /// Deployment failed
    Failed,
}

impl CanaryDeployment {
    /// Create a new canary deployment
    pub fn new(
        service_name: impl Into<String>, current_version: impl Into<String>,
        new_version: impl Into<String>,
    ) -> Self {
        Self {
            service_name: service_name.into(),
            current_version: current_version.into(),
            new_version: new_version.into(),
            traffic_percentage: 0,
            stages: vec![10, 50, 100],
            current_stage: 0,
            error_rate_threshold: 0.05,
            started_at: None,
            status: DeploymentStatus::Paused,
        }
    }

    /// Set custom deployment stages
    pub fn with_stages(mut self, stages: Vec<u32>) -> Self {
        self.stages = stages;
        self
    }

    /// Set error rate threshold for rollback
    pub fn with_error_threshold(mut self, threshold: f32) -> Self {
        self.error_rate_threshold = threshold;
        self
    }

    /// Start the canary deployment
    pub async fn start(&mut self, stages: Vec<u32>, duration_per_stage: Duration) -> Result<()> {
        self.stages = stages;
        self.status = DeploymentStatus::InProgress;
        self.started_at = Some(Utc::now());

        info!(
            "Starting canary deployment for {} {} -> {}",
            self.service_name, self.current_version, self.new_version
        );

        for (stage_index, &traffic_pct) in self.stages.iter().enumerate() {
            if self.status != DeploymentStatus::InProgress {
                break;
            }

            self.current_stage = stage_index;
            self.traffic_percentage = traffic_pct;

            info!(
                "Canary: {} - Stage {} ({} traffic)",
                self.service_name,
                stage_index + 1,
                traffic_pct
            );

            // Simulate monitoring and health checks during this stage
            sleep(duration_per_stage).await;

            // Check metrics (in real scenario, would query monitoring system)
            if self.check_health().await? {
                debug!(
                    "Canary health check passed for stage {} ({}%)",
                    stage_index + 1,
                    traffic_pct
                );
            } else {
                warn!("Canary health check failed, rolling back");
                self.rollback().await?;
                return Ok(());
            }
        }

        self.complete().await;
        Ok(())
    }

    /// Advance to next stage
    pub async fn advance_stage(&mut self) -> Result<()> {
        if self.current_stage + 1 < self.stages.len() {
            self.current_stage += 1;
            self.traffic_percentage = self.stages[self.current_stage];
            info!(
                "Advanced canary to stage {} ({}% traffic)",
                self.current_stage + 1,
                self.traffic_percentage
            );
            Ok(())
        } else {
            self.complete().await;
            Ok(())
        }
    }

    /// Pause the deployment
    pub fn pause(&mut self) {
        self.status = DeploymentStatus::Paused;
        info!(
            "Canary deployment for {} paused at {}%",
            self.service_name, self.traffic_percentage
        );
    }

    /// Resume the deployment
    pub fn resume(&mut self) {
        if self.status == DeploymentStatus::Paused {
            self.status = DeploymentStatus::InProgress;
            info!("Canary deployment for {} resumed", self.service_name);
        }
    }

    /// Rollback the deployment
    pub async fn rollback(&mut self) -> Result<()> {
        self.status = DeploymentStatus::RolledBack;
        self.traffic_percentage = 0;
        info!("Rolled back canary deployment for {}", self.service_name);
        Ok(())
    }

    /// Mark deployment as complete
    pub async fn complete(&mut self) {
        self.status = DeploymentStatus::Completed;
        self.traffic_percentage = 100;
        info!(
            "Canary deployment for {} completed successfully",
            self.service_name
        );
    }

    /// Check health of canary deployment
    async fn check_health(&self) -> Result<bool> {
        // In production, this would query metrics/monitoring systems
        // For now, assume healthy
        Ok(true)
    }

    /// Get deployment progress
    pub fn get_progress(&self) -> f32 {
        if self.stages.is_empty() {
            return 0.0;
        }
        (self.current_stage as f32 / self.stages.len() as f32) * 100.0
    }
}

/// Blue-green deployment for instant switchover
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BlueGreenDeployment {
    /// Service name
    pub service_name: String,
    /// Blue environment version
    pub blue_version: String,
    /// Green environment version
    pub green_version: String,
    /// Active environment (Blue or Green)
    pub active_environment: Environment,
    /// Status
    pub status: DeploymentStatus,
    /// Metrics for health assessment
    pub metrics: DeploymentMetrics,
}

/// Environment designation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Environment {
    /// Blue production environment
    Blue,
    /// Green staging/new environment
    Green,
}

/// Deployment metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeploymentMetrics {
    /// Error rate (0-1)
    pub error_rate: f32,
    /// Average response time (ms)
    pub avg_response_time_ms: f32,
    /// Requests per second
    pub requests_per_sec: f32,
    /// Successful deployments count
    pub successful_deployments: u32,
    /// Failed deployments count
    pub failed_deployments: u32,
}

impl BlueGreenDeployment {
    /// Create a new blue-green deployment
    pub fn new(
        service_name: impl Into<String>, blue_version: impl Into<String>,
        green_version: impl Into<String>,
    ) -> Self {
        Self {
            service_name: service_name.into(),
            blue_version: blue_version.into(),
            green_version: green_version.into(),
            active_environment: Environment::Blue,
            status: DeploymentStatus::Paused,
            metrics: DeploymentMetrics {
                error_rate: 0.0,
                avg_response_time_ms: 0.0,
                requests_per_sec: 0.0,
                successful_deployments: 0,
                failed_deployments: 0,
            },
        }
    }

    /// Deploy to green environment and switch traffic
    pub async fn deploy_and_switch(&mut self) -> Result<()> {
        info!(
            "Starting blue-green deployment for {} (Blue: {} -> Green: {})",
            self.service_name, self.blue_version, self.green_version
        );

        // Pre-deployment health check
        if !self.verify_green_health().await? {
            return Err(Error::CanaryDeploymentError {
                reason: "Green environment failed health check".to_string(),
            });
        }

        // Switch traffic
        self.active_environment = Environment::Green;
        self.status = DeploymentStatus::Completed;
        self.metrics.successful_deployments += 1;

        info!(
            "Switched traffic to green environment for {}",
            self.service_name
        );

        Ok(())
    }

    /// Rollback to blue environment
    pub async fn rollback(&mut self) -> Result<()> {
        self.active_environment = Environment::Blue;
        self.status = DeploymentStatus::RolledBack;
        self.metrics.failed_deployments += 1;

        info!("Rolled back to blue environment for {}", self.service_name);

        Ok(())
    }

    /// Verify green environment is healthy
    async fn verify_green_health(&self) -> Result<bool> {
        // In production, check actual health metrics
        Ok(self.metrics.error_rate < 0.01)
    }
}

/// Traffic mirroring for shadow traffic analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrafficMirror {
    /// Source service
    pub source_service: String,
    /// Primary destination (receives real traffic)
    pub primary_destination: String,
    /// Mirror destination (receives shadow traffic)
    pub mirror_destination: String,
    /// Percentage of traffic to mirror (0-100)
    pub mirror_percentage: u32,
    /// Mirror traffic should not affect response to client
    pub mirror_host: String,
    /// Status
    pub is_active: bool,
}

impl TrafficMirror {
    /// Create a new traffic mirror
    pub fn new(
        source_service: impl Into<String>, primary_destination: impl Into<String>,
        mirror_destination: impl Into<String>,
    ) -> Self {
        Self {
            source_service: source_service.into(),
            primary_destination: primary_destination.into(),
            mirror_destination: mirror_destination.into(),
            mirror_percentage: 10,
            mirror_host: format!("{}-mirror", mirror_destination.into::<String>()),
            is_active: false,
        }
    }

    /// Enable traffic mirroring
    pub fn enable(&mut self) {
        self.is_active = true;
        info!(
            "Enabled traffic mirroring for {} -> {} (mirror: {}, {}%)",
            self.source_service,
            self.primary_destination,
            self.mirror_destination,
            self.mirror_percentage
        );
    }

    /// Disable traffic mirroring
    pub fn disable(&mut self) {
        self.is_active = false;
        info!("Disabled traffic mirroring for {}", self.source_service);
    }

    /// Set mirror percentage
    pub fn set_mirror_percentage(&mut self, percentage: u32) -> Result<()> {
        if percentage > 100 {
            return Err(Error::InvalidTrafficSplit {
                reason: "Mirror percentage cannot exceed 100".to_string(),
            });
        }
        self.mirror_percentage = percentage;
        Ok(())
    }

    /// Check if request should be mirrored
    pub fn should_mirror(&self, request_id: u64) -> bool {
        if !self.is_active {
            return false;
        }
        (request_id % 100) < self.mirror_percentage as u64
    }
}

/// A/B testing traffic split
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ABTestSplit {
    /// Service name
    pub service_name: String,
    /// Variant A version
    pub variant_a: String,
    /// Variant B version
    pub variant_b: String,
    /// Traffic percentage for variant A
    pub variant_a_percentage: u32,
    /// Metrics for A
    pub metrics_a: HashMap<String, f32>,
    /// Metrics for B
    pub metrics_b: HashMap<String, f32>,
    /// Is test active
    pub is_active: bool,
}

impl ABTestSplit {
    /// Create a new A/B test
    pub fn new(
        service_name: impl Into<String>, variant_a: impl Into<String>, variant_b: impl Into<String>,
    ) -> Self {
        Self {
            service_name: service_name.into(),
            variant_a: variant_a.into(),
            variant_b: variant_b.into(),
            variant_a_percentage: 50,
            metrics_a: HashMap::new(),
            metrics_b: HashMap::new(),
            is_active: false,
        }
    }

    /// Start the A/B test
    pub fn start(&mut self, variant_a_percentage: u32) -> Result<()> {
        if variant_a_percentage > 100 {
            return Err(Error::InvalidTrafficSplit {
                reason: "Percentage must be 0-100".to_string(),
            });
        }
        self.variant_a_percentage = variant_a_percentage;
        self.is_active = true;
        info!(
            "Started A/B test for {} (A: {}%, B: {}%)",
            self.service_name,
            variant_a_percentage,
            100 - variant_a_percentage
        );
        Ok(())
    }

    /// Stop the A/B test
    pub fn stop(&mut self) {
        self.is_active = false;
        info!("Stopped A/B test for {}", self.service_name);
    }

    /// Route request to variant
    pub fn route_request(&self, request_id: u64) -> &str {
        let random_value = (request_id % 100) as u32;
        if random_value < self.variant_a_percentage {
            &self.variant_a
        } else {
            &self.variant_b
        }
    }

    /// Record metric for variant A
    pub fn record_metric_a(&mut self, metric_name: impl Into<String>, value: f32) {
        self.metrics_a.insert(metric_name.into(), value);
    }

    /// Record metric for variant B
    pub fn record_metric_b(&mut self, metric_name: impl Into<String>, value: f32) {
        self.metrics_b.insert(metric_name.into(), value);
    }

    /// Get statistical winner (simple comparison)
    pub fn get_winner(&self) -> Option<&str> {
        if self.metrics_a.is_empty() || self.metrics_b.is_empty() {
            return None;
        }

        let avg_a: f32 = self.metrics_a.values().sum::<f32>() / self.metrics_a.len() as f32;
        let avg_b: f32 = self.metrics_b.values().sum::<f32>() / self.metrics_b.len() as f32;

        if avg_a > avg_b {
            Some(&self.variant_a)
        } else {
            Some(&self.variant_b)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_traffic_split_creation() {
        let split = TrafficSplit::new("primary", "canary", 90, 10).unwrap();
        assert_eq!(split.primary_weight, 90);
        assert_eq!(split.canary_weight, 10);
    }

    #[test]
    fn test_traffic_split_invalid_weights() {
        let result = TrafficSplit::new("primary", "canary", 50, 60);
        assert!(result.is_err());
    }

    #[test]
    fn test_traffic_split_routing() {
        let split = TrafficSplit::new("primary", "canary", 70, 30).unwrap();
        let route1 = split.route_request(5);
        let route2 = split.route_request(75);

        assert_eq!(route1, "primary");
        assert_eq!(route2, "canary");
    }

    #[test]
    fn test_canary_deployment_creation() {
        let canary = CanaryDeployment::new("my-service", "1.0.0", "1.1.0");
        assert_eq!(canary.service_name, "my-service");
        assert_eq!(canary.current_version, "1.0.0");
        assert_eq!(canary.new_version, "1.1.0");
    }

    #[test]
    fn test_blue_green_deployment_creation() {
        let bg = BlueGreenDeployment::new("my-service", "1.0.0", "1.1.0");
        assert_eq!(bg.active_environment, Environment::Blue);
    }

    #[test]
    fn test_traffic_mirror_should_mirror() {
        let mirror = TrafficMirror::new("service", "primary", "mirror");
        let mut mirror = mirror;
        mirror.enable();
        mirror.set_mirror_percentage(50).unwrap();

        let should_mirror_1 = mirror.should_mirror(25);
        let should_mirror_2 = mirror.should_mirror(75);

        assert_eq!(should_mirror_1, true);
        assert_eq!(should_mirror_2, false);
    }

    #[test]
    fn test_ab_test_routing() {
        let ab = ABTestSplit::new("service", "v1", "v2");
        let mut ab = ab;
        ab.start(60).unwrap();

        let route1 = ab.route_request(30);
        let route2 = ab.route_request(70);

        assert_eq!(route1, "v1");
        assert_eq!(route2, "v2");
    }
}
