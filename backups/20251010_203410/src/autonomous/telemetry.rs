//! Telemetry feedback loop for continuous improvement

use crate::error::{GgenAiError, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::{debug, info};

/// Telemetry configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TelemetryConfig {
    /// Enable telemetry collection
    pub enabled: bool,
    /// Collection interval in seconds
    pub collection_interval_secs: u64,
    /// Maximum events to store
    pub max_events: usize,
    /// Enable performance metrics
    pub track_performance: bool,
    /// Enable error tracking
    pub track_errors: bool,
    /// Enable resource usage tracking
    pub track_resources: bool,
}

impl Default for TelemetryConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            collection_interval_secs: 60,
            max_events: 10000,
            track_performance: true,
            track_errors: true,
            track_resources: true,
        }
    }
}

/// Types of telemetry events
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum TelemetryEventType {
    /// Regeneration started
    RegenerationStarted,
    /// Regeneration completed
    RegenerationCompleted,
    /// Regeneration failed
    RegenerationFailed,
    /// Deployment started
    DeploymentStarted,
    /// Deployment completed
    DeploymentCompleted,
    /// Deployment failed
    DeploymentFailed,
    /// Validation passed
    ValidationPassed,
    /// Validation failed
    ValidationFailed,
    /// Performance metric
    PerformanceMetric,
    /// Resource usage
    ResourceUsage,
    /// Error occurred
    Error,
}

/// Telemetry event
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TelemetryEvent {
    /// Event ID
    pub id: String,
    /// Event type
    pub event_type: TelemetryEventType,
    /// Timestamp
    pub timestamp: chrono::DateTime<chrono::Utc>,
    /// Component that generated the event
    pub component: String,
    /// Event data
    pub data: HashMap<String, serde_json::Value>,
    /// Tags for filtering
    pub tags: HashMap<String, String>,
}

impl TelemetryEvent {
    /// Create a new telemetry event
    pub fn new(event_type: TelemetryEventType, component: String) -> Self {
        Self {
            id: uuid::Uuid::new_v4().to_string(),
            event_type,
            timestamp: chrono::Utc::now(),
            component,
            data: HashMap::new(),
            tags: HashMap::new(),
        }
    }

    /// Add data to the event
    pub fn with_data(mut self, key: &str, value: serde_json::Value) -> Self {
        self.data.insert(key.to_string(), value);
        self
    }

    /// Add tag to the event
    pub fn with_tag(mut self, key: &str, value: &str) -> Self {
        self.tags.insert(key.to_string(), value.to_string());
        self
    }
}

/// Performance metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceMetrics {
    /// Average regeneration time in milliseconds
    pub avg_regeneration_time_ms: f64,
    /// Total regenerations
    pub total_regenerations: u64,
    /// Success rate (0.0 to 1.0)
    pub success_rate: f64,
    /// Average deployment time in milliseconds
    pub avg_deployment_time_ms: f64,
    /// Total deployments
    pub total_deployments: u64,
    /// Deployment success rate
    pub deployment_success_rate: f64,
    /// Events processed per minute
    pub events_per_minute: f64,
}

impl Default for PerformanceMetrics {
    fn default() -> Self {
        Self {
            avg_regeneration_time_ms: 0.0,
            total_regenerations: 0,
            success_rate: 1.0,
            avg_deployment_time_ms: 0.0,
            total_deployments: 0,
            deployment_success_rate: 1.0,
            events_per_minute: 0.0,
        }
    }
}

/// Telemetry collector
#[derive(Debug)]
pub struct TelemetryCollector {
    config: TelemetryConfig,
    events: Arc<RwLock<Vec<TelemetryEvent>>>,
    metrics: Arc<RwLock<PerformanceMetrics>>,
}

impl TelemetryCollector {
    /// Create a new telemetry collector
    pub fn new(config: TelemetryConfig) -> Self {
        Self {
            config,
            events: Arc::new(RwLock::new(Vec::new())),
            metrics: Arc::new(RwLock::new(PerformanceMetrics::default())),
        }
    }

    /// Record a telemetry event
    pub async fn record(&self, event: TelemetryEvent) {
        if !self.config.enabled {
            return;
        }

        debug!(
            event_id = %event.id,
            event_type = ?event.event_type,
            component = %event.component,
            "Recording telemetry event"
        );

        // Update metrics based on event type
        self.update_metrics(&event).await;

        // Store event
        let mut events = self.events.write().await;
        events.push(event);

        // Trim if exceeds max
        if events.len() > self.config.max_events {
            let excess = events.len() - self.config.max_events;
            events.drain(0..excess);
        }
    }

    /// Update performance metrics
    async fn update_metrics(&self, event: &TelemetryEvent) {
        let mut metrics = self.metrics.write().await;

        match event.event_type {
            TelemetryEventType::RegenerationCompleted => {
                metrics.total_regenerations += 1;
                if let Some(duration) = event.data.get("duration_ms") {
                    if let Some(duration_ms) = duration.as_f64() {
                        // Running average
                        let total = metrics.total_regenerations as f64;
                        metrics.avg_regeneration_time_ms =
                            (metrics.avg_regeneration_time_ms * (total - 1.0) + duration_ms) / total;
                    }
                }
            }
            TelemetryEventType::RegenerationFailed => {
                metrics.total_regenerations += 1;
                let total = metrics.total_regenerations as f64;
                let successful = (total - 1.0) * metrics.success_rate;
                metrics.success_rate = successful / total;
            }
            TelemetryEventType::DeploymentCompleted => {
                metrics.total_deployments += 1;
                if let Some(duration) = event.data.get("duration_ms") {
                    if let Some(duration_ms) = duration.as_f64() {
                        let total = metrics.total_deployments as f64;
                        metrics.avg_deployment_time_ms =
                            (metrics.avg_deployment_time_ms * (total - 1.0) + duration_ms) / total;
                    }
                }
            }
            TelemetryEventType::DeploymentFailed => {
                metrics.total_deployments += 1;
                let total = metrics.total_deployments as f64;
                let successful = (total - 1.0) * metrics.deployment_success_rate;
                metrics.deployment_success_rate = successful / total;
            }
            _ => {}
        }
    }

    /// Get current performance metrics
    pub async fn get_metrics(&self) -> PerformanceMetrics {
        self.metrics.read().await.clone()
    }

    /// Get recent events
    pub async fn get_events(&self, limit: usize, event_type: Option<TelemetryEventType>) -> Vec<TelemetryEvent> {
        let events = self.events.read().await;
        let filtered: Vec<_> = if let Some(et) = event_type {
            events.iter()
                .filter(|e| e.event_type == et)
                .rev()
                .take(limit)
                .cloned()
                .collect()
        } else {
            events.iter()
                .rev()
                .take(limit)
                .cloned()
                .collect()
        };
        filtered
    }

    /// Get events by component
    pub async fn get_events_by_component(&self, component: &str, limit: usize) -> Vec<TelemetryEvent> {
        let events = self.events.read().await;
        events.iter()
            .filter(|e| e.component == component)
            .rev()
            .take(limit)
            .cloned()
            .collect()
    }

    /// Clear all events
    pub async fn clear(&self) {
        let mut events = self.events.write().await;
        events.clear();
        info!("Cleared telemetry events");
    }

    /// Export events to JSON
    pub async fn export_json(&self) -> Result<String> {
        let events = self.events.read().await;
        serde_json::to_string_pretty(&*events)
            .map_err(|e| GgenAiError::telemetry(format!("Failed to serialize events: {}", e)))
    }

    /// Generate summary report
    pub async fn generate_report(&self) -> TelemetryReport {
        let metrics = self.get_metrics().await;
        let events = self.events.read().await;

        let errors = events.iter()
            .filter(|e| e.event_type == TelemetryEventType::Error)
            .count();

        let warnings = events.iter()
            .filter(|e| e.tags.get("severity").map_or(false, |s| s == "warning"))
            .count();

        TelemetryReport {
            metrics,
            total_events: events.len(),
            errors,
            warnings,
            generated_at: chrono::Utc::now(),
        }
    }
}

/// Telemetry report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TelemetryReport {
    pub metrics: PerformanceMetrics,
    pub total_events: usize,
    pub errors: usize,
    pub warnings: usize,
    pub generated_at: chrono::DateTime<chrono::Utc>,
}

/// Feedback loop for continuous improvement
pub struct FeedbackLoop {
    telemetry: Arc<TelemetryCollector>,
}

impl FeedbackLoop {
    /// Create a new feedback loop
    pub fn new(telemetry: Arc<TelemetryCollector>) -> Self {
        Self { telemetry }
    }

    /// Analyze feedback and generate recommendations
    pub async fn analyze(&self) -> Vec<Recommendation> {
        let metrics = self.telemetry.get_metrics().await;
        let mut recommendations = Vec::new();

        // Check regeneration performance
        if metrics.avg_regeneration_time_ms > 5000.0 {
            recommendations.push(Recommendation {
                category: "performance".to_string(),
                severity: "warning".to_string(),
                message: "Average regeneration time exceeds 5 seconds. Consider optimizing template complexity or increasing parallel workers.".to_string(),
                suggested_action: Some("Increase parallel_workers or enable caching".to_string()),
            });
        }

        // Check success rate
        if metrics.success_rate < 0.9 {
            recommendations.push(Recommendation {
                category: "reliability".to_string(),
                severity: "error".to_string(),
                message: format!("Success rate is below 90% ({:.1}%). Investigate common failure patterns.", metrics.success_rate * 100.0),
                suggested_action: Some("Review error logs and improve validation".to_string()),
            });
        }

        // Check deployment success rate
        if metrics.deployment_success_rate < 0.95 {
            recommendations.push(Recommendation {
                category: "deployment".to_string(),
                severity: "warning".to_string(),
                message: "Deployment success rate is below 95%. Consider improving validation or rollback strategies.".to_string(),
                suggested_action: Some("Enable pre-deployment validation".to_string()),
            });
        }

        recommendations
    }

    /// Get telemetry collector
    pub fn collector(&self) -> Arc<TelemetryCollector> {
        self.telemetry.clone()
    }
}

/// Improvement recommendation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Recommendation {
    pub category: String,
    pub severity: String,
    pub message: String,
    pub suggested_action: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_telemetry_collector() {
        let config = TelemetryConfig::default();
        let collector = TelemetryCollector::new(config);

        let event = TelemetryEvent::new(
            TelemetryEventType::RegenerationCompleted,
            "test".to_string(),
        )
        .with_data("duration_ms", serde_json::json!(1500));

        collector.record(event).await;

        let metrics = collector.get_metrics().await;
        assert_eq!(metrics.total_regenerations, 1);
        assert_eq!(metrics.avg_regeneration_time_ms, 1500.0);
    }

    #[tokio::test]
    async fn test_metrics_calculation() {
        let config = TelemetryConfig::default();
        let collector = TelemetryCollector::new(config);

        // Record successful regeneration
        collector.record(
            TelemetryEvent::new(TelemetryEventType::RegenerationCompleted, "test".to_string())
                .with_data("duration_ms", serde_json::json!(1000))
        ).await;

        // Record failed regeneration
        collector.record(
            TelemetryEvent::new(TelemetryEventType::RegenerationFailed, "test".to_string())
        ).await;

        let metrics = collector.get_metrics().await;
        assert_eq!(metrics.total_regenerations, 2);
        assert_eq!(metrics.success_rate, 0.5);
    }

    #[tokio::test]
    async fn test_feedback_loop() {
        let config = TelemetryConfig::default();
        let collector = Arc::new(TelemetryCollector::new(config));
        let feedback = FeedbackLoop::new(collector.clone());

        // Simulate poor performance
        {
            let mut metrics = collector.metrics.write().await;
            metrics.avg_regeneration_time_ms = 6000.0;
            metrics.success_rate = 0.85;
        }

        let recommendations = feedback.analyze().await;
        assert!(recommendations.len() >= 2);
        assert!(recommendations.iter().any(|r| r.category == "performance"));
        assert!(recommendations.iter().any(|r| r.category == "reliability"));
    }
}
