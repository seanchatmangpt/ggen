//! Marketplace-MAPE-K Integration
//!
//! Bridges the marketplace domain with the MAPE-K autonomic control system,
//! enabling the marketplace to self-optimize through continuous monitoring,
//! analysis, planning, validation, and knowledge accumulation.

use super::guards::ValidationReceipt;
use std::sync::{Arc, Mutex};
use std::time::SystemTime;

/// Marketplace observation - lightweight telemetry events
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MarketplaceObservation {
    /// Observation ID
    pub id: String,

    /// Type of observation
    pub obs_type: MarketplaceObservationType,

    /// Timestamp (milliseconds since epoch)
    pub timestamp: u64,

    /// Observation data
    pub data: serde_json::Value,

    /// Source component
    pub source: String,
}

/// Types of marketplace observations
#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize, PartialEq, Eq)]
pub enum MarketplaceObservationType {
    /// Validation receipt from guard execution
    Receipt,
    /// Performance metric
    Metric,
    /// Anomaly detection
    Anomaly,
}

/// Converts marketplace receipts to observations
pub struct ReceiptObserver;

impl ReceiptObserver {
    /// Convert validation receipt to observation
    pub fn receipt_to_observation(receipt: &ValidationReceipt) -> MarketplaceObservation {
        let now = get_timestamp();
        let critical_rate = if receipt.critical_total > 0 {
            (receipt.critical_passed as f64 / receipt.critical_total as f64) * 100.0
        } else {
            100.0
        };

        MarketplaceObservation {
            id: format!("receipt-{}-{}", receipt.package_id, now),
            obs_type: MarketplaceObservationType::Receipt,
            timestamp: now,
            data: serde_json::json!({
                "package_id": receipt.package_id,
                "version": receipt.version,
                "overall_score": receipt.overall_score,
                "production_ready": receipt.production_ready,
                "critical_rate": critical_rate,
                "guard_count": receipt.guard_results.len(),
                "validated_at": receipt.validated_at,
            }),
            source: "marketplace-guard-system".to_string(),
        }
    }

    /// Create performance metric observation
    pub fn create_metric_observation(
        component: &str, metric_name: &str, value: f64,
    ) -> MarketplaceObservation {
        let now = get_timestamp();

        MarketplaceObservation {
            id: format!("{}-{}-{}", component, metric_name, now),
            obs_type: MarketplaceObservationType::Metric,
            timestamp: now,
            data: serde_json::json!({
                "component": component,
                "metric": metric_name,
                "value": value,
            }),
            source: "marketplace-metrics".to_string(),
        }
    }

    /// Create anomaly observation
    pub fn create_anomaly_observation(
        component: &str, anomaly_type: &str,
    ) -> MarketplaceObservation {
        let now = get_timestamp();

        MarketplaceObservation {
            id: format!("{}-anomaly-{}", component, now),
            obs_type: MarketplaceObservationType::Anomaly,
            timestamp: now,
            data: serde_json::json!({
                "component": component,
                "anomaly_type": anomaly_type,
            }),
            source: "marketplace-anomaly-detector".to_string(),
        }
    }
}

/// Observation statistics
#[derive(Debug, Clone, Default)]
pub struct ObservationStats {
    /// Total observations ingested
    pub total_observations: usize,

    /// Observations by type
    pub by_type: std::collections::HashMap<String, usize>,

    /// Last observation timestamp
    pub last_observation_timestamp: u64,
}

/// Autonomous marketplace telemetry collector
pub struct AutonomicMarketplace {
    observations: Arc<Mutex<Vec<MarketplaceObservation>>>,
    stats: Arc<Mutex<ObservationStats>>,
    last_loop_timestamp: Arc<Mutex<u64>>,
}

impl AutonomicMarketplace {
    /// Create new autonomic marketplace
    pub fn new() -> Self {
        Self {
            observations: Arc::new(Mutex::new(Vec::new())),
            stats: Arc::new(Mutex::new(ObservationStats::default())),
            last_loop_timestamp: Arc::new(Mutex::new(0)),
        }
    }

    /// Ingest marketplace receipt into observation stream
    pub fn ingest_receipt(&self, receipt: &ValidationReceipt) {
        let obs = ReceiptObserver::receipt_to_observation(receipt);
        self.ingest_observation(obs);
    }

    /// Ingest performance metric
    pub fn ingest_metric(&self, component: &str, metric_name: &str, value: f64) {
        let obs = ReceiptObserver::create_metric_observation(component, metric_name, value);
        self.ingest_observation(obs);
    }

    /// Ingest anomaly detection
    pub fn ingest_anomaly(&self, component: &str, anomaly_type: &str) {
        let obs = ReceiptObserver::create_anomaly_observation(component, anomaly_type);
        self.ingest_observation(obs);
    }

    /// Internal: Record observation and update statistics
    fn ingest_observation(&self, obs: MarketplaceObservation) {
        let mut observations = self.observations.lock().unwrap();
        observations.push(obs.clone());

        let mut stats = self.stats.lock().unwrap();
        stats.total_observations += 1;
        stats.last_observation_timestamp = obs.timestamp;

        let type_name = format!("{:?}", obs.obs_type);
        *stats.by_type.entry(type_name).or_insert(0) += 1;
    }

    /// Get observations for processing by external MAPE-K system
    pub fn get_observations(&self, since_timestamp: u64) -> Vec<MarketplaceObservation> {
        let observations = self.observations.lock().unwrap();
        observations
            .iter()
            .filter(|o| o.timestamp > since_timestamp)
            .cloned()
            .collect()
    }

    /// Clear processed observations
    pub fn clear_observations(&self) {
        let mut observations = self.observations.lock().unwrap();
        observations.clear();
    }

    /// Get current observation statistics
    pub fn observation_stats(&self) -> ObservationStats {
        self.stats.lock().unwrap().clone()
    }

    /// Check health of marketplace
    pub fn health_check(&self) -> MarketplaceHealth {
        let stats = self.stats.lock().unwrap();

        let receipt_count = stats.by_type.get("Receipt").copied().unwrap_or(0);
        let metric_count = stats.by_type.get("Metric").copied().unwrap_or(0);
        let anomaly_count = stats.by_type.get("Anomaly").copied().unwrap_or(0);

        let status = if anomaly_count > 5 {
            AutonomicStatus::Degraded
        } else if anomaly_count > 0 {
            AutonomicStatus::Healthy
        } else {
            AutonomicStatus::Healthy
        };

        MarketplaceHealth {
            status,
            total_observations: stats.total_observations,
            receipt_count,
            metric_count,
            anomaly_count,
            last_observation_at: stats.last_observation_timestamp,
        }
    }

    /// Get last loop execution time
    pub fn last_loop_timestamp(&self) -> u64 {
        *self.last_loop_timestamp.lock().unwrap()
    }

    /// Record that autonomic loop was executed
    pub fn record_loop_execution(&self) {
        *self.last_loop_timestamp.lock().unwrap() = get_timestamp();
    }
}

/// Autonomic marketplace status
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AutonomicStatus {
    /// System operating normally
    Healthy,

    /// System has issues but operational
    Degraded,

    /// System critical
    Critical,
}

/// Health check result for marketplace
#[derive(Debug, Clone)]
pub struct MarketplaceHealth {
    /// Overall autonomic status
    pub status: AutonomicStatus,

    /// Total observations processed
    pub total_observations: usize,

    /// Receipt observations
    pub receipt_count: usize,

    /// Metric observations
    pub metric_count: usize,

    /// Anomaly observations
    pub anomaly_count: usize,

    /// Last observation timestamp
    pub last_observation_at: u64,
}

impl Default for AutonomicMarketplace {
    fn default() -> Self {
        Self::new()
    }
}

/// Get current timestamp in milliseconds
fn get_timestamp() -> u64 {
    SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .map(|d| d.as_millis() as u64)
        .unwrap_or(0)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_autonomic_marketplace_creation() {
        let autonomic = AutonomicMarketplace::new();
        let stats = autonomic.observation_stats();
        assert_eq!(stats.total_observations, 0);
    }

    #[test]
    fn test_receipt_to_observation() {
        let receipt = ValidationReceipt::new(
            "test-package".to_string(),
            "1.0.0".to_string(),
            "ggen-test".to_string(),
        );

        let obs = ReceiptObserver::receipt_to_observation(&receipt);
        assert_eq!(obs.source, "marketplace-guard-system");
        assert_eq!(obs.obs_type, MarketplaceObservationType::Receipt);
    }

    #[test]
    fn test_metric_observation() {
        let obs = ReceiptObserver::create_metric_observation("registry", "latency_ms", 45.5);
        assert_eq!(obs.obs_type, MarketplaceObservationType::Metric);
        assert_eq!(obs.source, "marketplace-metrics");
    }

    #[test]
    fn test_anomaly_observation() {
        let obs =
            ReceiptObserver::create_anomaly_observation("guard-validator", "high_failure_rate");
        assert_eq!(obs.obs_type, MarketplaceObservationType::Anomaly);
        assert_eq!(obs.source, "marketplace-anomaly-detector");
    }

    #[test]
    fn test_ingest_metric() {
        let autonomic = AutonomicMarketplace::new();
        autonomic.ingest_metric("registry", "latency_ms", 45.5);
        autonomic.ingest_metric("registry", "latency_ms", 52.3);
        autonomic.ingest_metric("validator", "latency_ms", 100.0);

        let stats = autonomic.observation_stats();
        assert_eq!(stats.total_observations, 3);
    }

    #[test]
    fn test_ingest_anomaly() {
        let autonomic = AutonomicMarketplace::new();
        autonomic.ingest_anomaly("guard-validator", "high_failure_rate");

        let stats = autonomic.observation_stats();
        assert_eq!(stats.total_observations, 1);
    }

    #[test]
    fn test_ingest_receipt() {
        let autonomic = AutonomicMarketplace::new();
        let receipt = ValidationReceipt::new(
            "pkg".to_string(),
            "1.0.0".to_string(),
            "ggen-test".to_string(),
        );

        autonomic.ingest_receipt(&receipt);

        let stats = autonomic.observation_stats();
        assert_eq!(stats.total_observations, 1);
    }

    #[test]
    fn test_get_observations() {
        let autonomic = AutonomicMarketplace::new();
        let now = get_timestamp();

        autonomic.ingest_metric("comp1", "metric1", 50.0);
        autonomic.ingest_metric("comp2", "metric2", 60.0);

        let obs = autonomic.get_observations(now - 1000);
        assert_eq!(obs.len(), 2);
    }

    #[test]
    fn test_clear_observations() {
        let autonomic = AutonomicMarketplace::new();
        autonomic.ingest_metric("comp1", "metric1", 50.0);

        let stats_before = autonomic.observation_stats();
        assert_eq!(stats_before.total_observations, 1);

        autonomic.clear_observations();

        let obs_after = autonomic.get_observations(0);
        assert_eq!(obs_after.len(), 0);
    }

    #[test]
    fn test_health_check() {
        let autonomic = AutonomicMarketplace::new();
        autonomic.ingest_metric("comp1", "metric1", 50.0);

        let health = autonomic.health_check();
        assert_eq!(health.status, AutonomicStatus::Healthy);
        assert_eq!(health.total_observations, 1);
        assert_eq!(health.metric_count, 1);
    }

    #[test]
    fn test_health_check_degraded() {
        let autonomic = AutonomicMarketplace::new();
        for i in 0..10 {
            autonomic.ingest_anomaly(&format!("comp{}", i), "issue");
        }

        let health = autonomic.health_check();
        assert_eq!(health.status, AutonomicStatus::Degraded);
        assert!(health.anomaly_count > 5);
    }

    #[test]
    fn test_record_loop_execution() {
        let autonomic = AutonomicMarketplace::new();
        assert_eq!(autonomic.last_loop_timestamp(), 0);

        autonomic.record_loop_execution();
        let timestamp = autonomic.last_loop_timestamp();
        assert!(timestamp > 0);
    }

    #[test]
    fn test_observation_stats_by_type() {
        let autonomic = AutonomicMarketplace::new();
        autonomic.ingest_metric("comp", "m", 1.0);
        autonomic.ingest_metric("comp", "m", 2.0);
        autonomic.ingest_anomaly("comp", "anom");

        let stats = autonomic.observation_stats();
        assert_eq!(stats.by_type.get("Metric").copied(), Some(2));
        assert_eq!(stats.by_type.get("Anomaly").copied(), Some(1));
    }
}
