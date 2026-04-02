//! Outlier detection for identifying and ejecting unhealthy instances
//!
//! This module monitors service instances and automatically ejects those that exhibit
//! unhealthy behavior such as high error rates or slow responses.

use crate::error::{Error, Result};
use chrono::{DateTime, Duration, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::time::Duration as StdDuration;
use tokio::time::sleep;
use tracing::{debug, info, warn};

/// Outlier detection metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstanceMetrics {
    /// Instance host/address
    pub instance: String,
    /// Total requests to this instance
    pub total_requests: u64,
    /// Successful requests
    pub successful_requests: u64,
    /// Failed requests
    pub failed_requests: u64,
    /// Average response time (milliseconds)
    pub avg_response_time_ms: f32,
    /// Last request time
    pub last_request_time: Option<DateTime<Utc>>,
    /// Error rate (0-1)
    pub error_rate: f32,
    /// Is instance ejected
    pub ejected: bool,
    /// Ejection time
    pub ejected_at: Option<DateTime<Utc>>,
    /// Number of times ejected
    pub ejection_count: u32,
    /// Last health check result
    pub last_health_check: Option<HealthCheckResult>,
}

impl Default for InstanceMetrics {
    fn default() -> Self {
        Self {
            instance: String::new(),
            total_requests: 0,
            successful_requests: 0,
            failed_requests: 0,
            avg_response_time_ms: 0.0,
            last_request_time: None,
            error_rate: 0.0,
            ejected: false,
            ejected_at: None,
            ejection_count: 0,
            last_health_check: None,
        }
    }
}

/// Health check result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HealthCheckResult {
    /// Is healthy
    pub healthy: bool,
    /// Response time in milliseconds
    pub response_time_ms: f32,
    /// Reason
    pub reason: String,
    /// Timestamp
    pub timestamp: DateTime<Utc>,
}

/// Outlier detection state
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum OutlierDetectionState {
    /// Normal operation
    Active,
    /// Detecting outliers
    Detecting,
    /// Processing results
    Processing,
}

/// Configuration for outlier detection
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OutlierDetectionConfig {
    /// Service name
    pub service_name: String,
    /// Minimum request volume to analyze
    pub min_request_volume: u64,
    /// Consecutive error threshold (eject after N consecutive errors)
    pub consecutive_errors_threshold: u32,
    /// Error rate threshold (eject if error rate > this)
    pub error_rate_threshold: f32,
    /// Slow response threshold (milliseconds)
    pub slow_response_threshold_ms: f32,
    /// Slow response percentage threshold (eject if > X% of requests are slow)
    pub slow_response_percentage_threshold: f32,
    /// Base ejection time (how long to keep instance ejected)
    pub base_ejection_time: StdDuration,
    /// Max ejection time (upper limit for exponential backoff)
    pub max_ejection_time: StdDuration,
    /// Re-injection interval (how often to check if ejected instance is healthy)
    pub reinjection_interval: StdDuration,
    /// Analysis interval (how often to run detection)
    pub analysis_interval: StdDuration,
    /// Maximum percentage of instances to eject (0-100)
    pub max_ejection_percentage: u32,
}

impl Default for OutlierDetectionConfig {
    fn default() -> Self {
        Self {
            service_name: "default-service".to_string(),
            min_request_volume: 100,
            consecutive_errors_threshold: 5,
            error_rate_threshold: 0.05,
            slow_response_threshold_ms: 5000.0,
            slow_response_percentage_threshold: 30.0,
            base_ejection_time: StdDuration::from_secs(30),
            max_ejection_time: StdDuration::from_secs(300),
            reinjection_interval: StdDuration::from_secs(60),
            analysis_interval: StdDuration::from_secs(10),
            max_ejection_percentage: 50,
        }
    }
}

/// Outlier detection metrics and state
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OutlierDetectionMetrics {
    /// Current state
    pub state: OutlierDetectionState,
    /// Instance metrics
    pub instances: HashMap<String, InstanceMetrics>,
    /// Instances ejected in current analysis
    pub ejected_instances: Vec<String>,
    /// Instances re-injected in current analysis
    pub reinjected_instances: Vec<String>,
    /// Total analyses run
    pub total_analyses: u64,
    /// Last analysis time
    pub last_analysis_time: Option<DateTime<Utc>>,
    /// Average system error rate
    pub system_error_rate: f32,
}

impl Default for OutlierDetectionMetrics {
    fn default() -> Self {
        Self {
            state: OutlierDetectionState::Active,
            instances: HashMap::new(),
            ejected_instances: Vec::new(),
            reinjected_instances: Vec::new(),
            total_analyses: 0,
            last_analysis_time: None,
            system_error_rate: 0.0,
        }
    }
}

/// Outlier detection engine
pub struct OutlierDetection {
    config: OutlierDetectionConfig,
    metrics: OutlierDetectionMetrics,
    consecutive_error_counts: HashMap<String, u32>,
    slow_response_counts: HashMap<String, u32>,
}

impl OutlierDetection {
    /// Create a new outlier detection engine
    pub fn new(config: OutlierDetectionConfig) -> Self {
        Self {
            config,
            metrics: OutlierDetectionMetrics::default(),
            consecutive_error_counts: HashMap::new(),
            slow_response_counts: HashMap::new(),
        }
    }

    /// Record a request for an instance
    pub fn record_request(
        &mut self, instance: impl Into<String>, success: bool, response_time_ms: f32,
    ) {
        let instance = instance.into();
        let metrics = self
            .metrics
            .instances
            .entry(instance.clone())
            .or_insert_with(|| {
                let mut m = InstanceMetrics::default();
                m.instance = instance.clone();
                m
            });

        metrics.total_requests += 1;
        metrics.last_request_time = Some(Utc::now());

        // Update average response time
        metrics.avg_response_time_ms =
            (metrics.avg_response_time_ms * (metrics.total_requests - 1) as f32 + response_time_ms)
                / metrics.total_requests as f32;

        // Track slow responses
        if response_time_ms > self.config.slow_response_threshold_ms {
            *self
                .slow_response_counts
                .entry(instance.clone())
                .or_insert(0) += 1;
        }

        if success {
            metrics.successful_requests += 1;
            *self.consecutive_error_counts.entry(instance).or_insert(0) = 0;
        } else {
            metrics.failed_requests += 1;
            *self
                .consecutive_error_counts
                .entry(instance.clone())
                .or_insert(0) += 1;
        }

        // Update error rate
        if metrics.total_requests > 0 {
            metrics.error_rate = metrics.failed_requests as f32 / metrics.total_requests as f32;
        }
    }

    /// Run outlier detection analysis
    pub async fn analyze(&mut self) -> Result<()> {
        self.metrics.state = OutlierDetectionState::Detecting;

        debug!(
            "Starting outlier detection analysis for {}",
            self.config.service_name
        );

        // Check each instance for outlier conditions
        let mut instances_to_eject = Vec::new();

        for (instance, metrics) in &self.metrics.instances {
            if metrics.ejected {
                continue; // Skip already ejected instances
            }

            if metrics.total_requests < self.config.min_request_volume {
                continue; // Skip instances with insufficient data
            }

            // Check consecutive errors
            if let Some(&error_count) = self.consecutive_error_counts.get(instance) {
                if error_count >= self.config.consecutive_errors_threshold {
                    info!(
                        "Outlier detected for {}: {} consecutive errors",
                        instance, error_count
                    );
                    instances_to_eject.push(instance.clone());
                    continue;
                }
            }

            // Check error rate
            if metrics.error_rate > self.config.error_rate_threshold {
                info!(
                    "Outlier detected for {}: error rate {} exceeds threshold {}",
                    instance, metrics.error_rate, self.config.error_rate_threshold
                );
                instances_to_eject.push(instance.clone());
                continue;
            }

            // Check slow response percentage
            let slow_count = self.slow_response_counts.get(instance).unwrap_or(&0);
            let slow_percentage = (*slow_count as f32 / metrics.total_requests as f32) * 100.0;

            if slow_percentage > self.config.slow_response_percentage_threshold {
                info!(
                    "Outlier detected for {}: {:.1}% slow responses exceed threshold {:.1}%",
                    instance, slow_percentage, self.config.slow_response_percentage_threshold
                );
                instances_to_eject.push(instance.clone());
            }
        }

        // Eject instances respecting max ejection percentage
        self.eject_instances(instances_to_eject).await?;

        // Check for re-injection
        self.check_reinjection().await?;

        self.metrics.state = OutlierDetectionState::Processing;
        self.metrics.total_analyses += 1;
        self.metrics.last_analysis_time = Some(Utc::now());

        // Calculate system error rate
        let total_requests: u64 = self
            .metrics
            .instances
            .values()
            .map(|m| m.total_requests)
            .sum();
        let total_failures: u64 = self
            .metrics
            .instances
            .values()
            .map(|m| m.failed_requests)
            .sum();

        if total_requests > 0 {
            self.metrics.system_error_rate = total_failures as f32 / total_requests as f32;
        }

        self.metrics.state = OutlierDetectionState::Active;

        debug!(
            "Outlier detection analysis completed. System error rate: {:.2}%",
            self.metrics.system_error_rate * 100.0
        );

        Ok(())
    }

    /// Eject instances
    async fn eject_instances(&mut self, instances: Vec<String>) -> Result<()> {
        // Calculate max instances to eject
        let max_ejectable = (self.metrics.instances.len() as f32
            * (self.config.max_ejection_percentage as f32 / 100.0))
            .ceil() as usize;

        let instances_to_eject = instances
            .into_iter()
            .take(max_ejectable)
            .collect::<Vec<_>>();

        self.metrics.ejected_instances.clear();

        for instance in instances_to_eject {
            if let Some(metrics) = self.metrics.instances.get_mut(&instance) {
                metrics.ejected = true;
                metrics.ejected_at = Some(Utc::now());
                metrics.ejection_count += 1;
                self.metrics.ejected_instances.push(instance.clone());

                warn!(
                    "Ejected instance {} for service {}",
                    instance, self.config.service_name
                );
            }
        }

        Ok(())
    }

    /// Check if ejected instances should be re-injected
    async fn check_reinjection(&mut self) -> Result<()> {
        let now = Utc::now();
        let mut instances_to_reinject = Vec::new();

        for (instance, metrics) in &self.metrics.instances {
            if !metrics.ejected {
                continue;
            }

            if let Some(ejected_at) = metrics.ejected_at {
                let time_since_ejection = now.signed_duration_since(ejected_at);
                let ejection_time = Duration::from_std(self.config.base_ejection_time)
                    .unwrap_or(Duration::seconds(30));

                if time_since_ejection > ejection_time {
                    instances_to_reinject.push(instance.clone());
                }
            }
        }

        self.metrics.reinjected_instances.clear();

        for instance in instances_to_reinject {
            if let Some(metrics) = self.metrics.instances.get_mut(&instance) {
                metrics.ejected = false;
                self.metrics.reinjected_instances.push(instance.clone());

                info!(
                    "Re-injected instance {} for service {}",
                    instance, self.config.service_name
                );
            }
        }

        Ok(())
    }

    /// Get metrics
    pub fn get_metrics(&self) -> OutlierDetectionMetrics {
        self.metrics.clone()
    }

    /// Get instance metrics
    pub fn get_instance_metrics(&self, instance: &str) -> Option<InstanceMetrics> {
        self.metrics.instances.get(instance).cloned()
    }

    /// Get ejected instances
    pub fn get_ejected_instances(&self) -> Vec<String> {
        self.metrics
            .instances
            .iter()
            .filter(|(_, m)| m.ejected)
            .map(|(name, _)| name.clone())
            .collect()
    }

    /// Get healthy instances
    pub fn get_healthy_instances(&self) -> Vec<String> {
        self.metrics
            .instances
            .iter()
            .filter(|(_, m)| !m.ejected)
            .map(|(name, _)| name.clone())
            .collect()
    }

    /// Reset metrics for an instance
    pub fn reset_instance(&mut self, instance: &str) {
        if let Some(metrics) = self.metrics.instances.get_mut(instance) {
            metrics.total_requests = 0;
            metrics.successful_requests = 0;
            metrics.failed_requests = 0;
            metrics.error_rate = 0.0;
        }
        self.consecutive_error_counts.remove(instance);
        self.slow_response_counts.remove(instance);
    }

    /// Start continuous monitoring
    pub async fn start_monitoring(&mut self) -> Result<()> {
        info!(
            "Starting outlier detection monitoring for {}",
            self.config.service_name
        );

        loop {
            self.analyze().await?;
            sleep(self.config.analysis_interval).await;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_outlier_detection_creation() {
        let config = OutlierDetectionConfig {
            service_name: "test-service".to_string(),
            ..Default::default()
        };
        let od = OutlierDetection::new(config);
        assert_eq!(od.config.service_name, "test-service");
    }

    #[test]
    fn test_record_successful_request() {
        let config = OutlierDetectionConfig::default();
        let mut od = OutlierDetection::new(config);

        od.record_request("instance-1", true, 100.0);
        let metrics = od.get_metrics();

        assert_eq!(metrics.instances.len(), 1);
        assert_eq!(metrics.instances["instance-1"].successful_requests, 1);
        assert_eq!(metrics.instances["instance-1"].error_rate, 0.0);
    }

    #[test]
    fn test_record_failed_request() {
        let config = OutlierDetectionConfig::default();
        let mut od = OutlierDetection::new(config);

        od.record_request("instance-1", false, 100.0);
        let metrics = od.get_metrics();

        assert_eq!(metrics.instances["instance-1"].failed_requests, 1);
        assert!(metrics.instances["instance-1"].error_rate > 0.0);
    }

    #[tokio::test]
    async fn test_ejection() {
        let config = OutlierDetectionConfig {
            min_request_volume: 5,
            consecutive_errors_threshold: 3,
            ..Default::default()
        };
        let mut od = OutlierDetection::new(config);

        // Record failing requests
        for _ in 0..5 {
            od.record_request("instance-1", false, 100.0);
        }

        // Run analysis
        od.analyze().await.unwrap();

        let ejected = od.get_ejected_instances();
        assert!(ejected.contains(&"instance-1".to_string()));
    }

    #[test]
    fn test_error_rate_tracking() {
        let config = OutlierDetectionConfig::default();
        let mut od = OutlierDetection::new(config);

        // Record 8 successes and 2 failures = 20% error rate
        for _ in 0..8 {
            od.record_request("instance-1", true, 100.0);
        }
        for _ in 0..2 {
            od.record_request("instance-1", false, 100.0);
        }

        let metrics = od.get_instance_metrics("instance-1").unwrap();
        assert_eq!(metrics.total_requests, 10);
        assert_eq!(metrics.error_rate, 0.2);
    }
}
