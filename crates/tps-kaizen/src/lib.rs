#![doc = include_str!("../README.md")]
#![warn(missing_docs)]

//! # TPS Kaizen - Continuous Improvement Metrics & Analysis
//!
//! Implements production-grade Kaizen patterns using Prometheus + OpenTelemetry + SPARQL evidence queries.
//!
//! ## Core Philosophy
//!
//! **Kaizen** = "Good change, continuous improvement"
//!
//! In manufacturing, small improvements compound into huge gains. In code:
//! - **Measure everything** - Metrics are facts, not opinions
//! - **Know what "good" means** - SLOs are explicit
//! - **Trends matter more** - Is it getting better or worse?
//! - **Root cause analysis** - Don't just fix symptoms
//! - **Data retention** - Long-term trend analysis
//!
//! ## Architecture
//!
//! ```text
//! Jidoka (Autonomic) → Kanban (Flow) → Andon (Visibility) → Heijunka (Leveling)
//!         ↓                    ↓                ↓                  ↓
//!    Circuit breaker    Queue management   Alert detection    Load balancing
//!    Failure detection   Latency tracking   Visibility         Worker distribution
//!    Recovery time       Throughput         MTTD (Mean Time)   Utilization
//! ```
//!
//! ## Metrics Collection
//!
//! - **Jidoka**: Circuit breaker health (open %, failure rate, recovery time)
//! - **Kanban**: Queue management (depth, latency, throughput, wait time)
//! - **Andon**: Alert system (log volume, alert frequency, MTTD)
//! - **Heijunka**: Load balancing (coefficient, utilization, variance)
//!
//! ## Usage
//!
//! ```rust,ignore
//! use tps_kaizen::{KaizenMetrics, MetricRecorder, MetricAnalyzer};
//!
//! // Initialize metrics collection
//! let metrics = KaizenMetrics::new();
//! let recorder = MetricRecorder::new(metrics.clone());
//!
//! // Record events
//! recorder.record_circuit_open("payment-service").await?;
//! recorder.record_queue_depth("checkout", 42).await?;
//!
//! // Analyze for improvements
//! let analyzer = MetricAnalyzer::new(metrics.clone());
//! let recommendations = analyzer.analyze().await?;
//! ```

pub mod analyzer;
pub mod dashboard;
pub mod metrics;
pub mod recorder;
pub mod sparql;

pub use analyzer::MetricAnalyzer;
pub use dashboard::{Dashboard, DashboardPanel};
pub use metrics::{JidokaMetrics, KaizenMetrics, KanbanMetrics, HeijunkaMetrics, AndonMetrics};
pub use recorder::MetricRecorder;
pub use sparql::SparqlAnalyzer;

use std::sync::Arc;
use thiserror::Error;

/// Kaizen result type
pub type Result<T> = std::result::Result<T, KaizenError>;

/// Kaizen errors
#[derive(Error, Debug)]
pub enum KaizenError {
    /// Metrics error
    #[error("Metrics error: {0}")]
    MetricsError(String),

    /// Recording error
    #[error("Recording error: {0}")]
    RecordingError(String),

    /// Analysis error
    #[error("Analysis error: {0}")]
    AnalysisError(String),

    /// SPARQL query error
    #[error("SPARQL query error: {0}")]
    SparqlError(String),

    /// Serialization error
    #[error("Serialization error: {0}")]
    SerializationError(#[from] serde_json::Error),

    /// Async error
    #[error("Async error: {0}")]
    AsyncError(String),

    /// Time window error
    #[error("Time window error: {0}")]
    TimeWindowError(String),
}

/// Metric value with timestamp
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct TimestampedValue {
    /// Timestamp (ISO 8601)
    pub timestamp: chrono::DateTime<chrono::Utc>,
    /// Metric value
    pub value: f64,
    /// Associated tags
    pub tags: std::collections::HashMap<String, String>,
}

/// Improvement recommendation
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct Recommendation {
    /// Recommendation title
    pub title: String,
    /// Description
    pub description: String,
    /// Affected metric
    pub metric: String,
    /// Current value
    pub current_value: f64,
    /// Target value
    pub target_value: f64,
    /// Confidence (0.0-1.0)
    pub confidence: f64,
    /// Suggested action
    pub action: String,
    /// Priority (1=high, 5=low)
    pub priority: u8,
}

/// SLO (Service Level Objective) definition
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct Slo {
    /// SLO name
    pub name: String,
    /// Metric name
    pub metric: String,
    /// Target value
    pub target: f64,
    /// Time window (seconds)
    pub window_secs: u64,
    /// Is this a minimum or maximum target?
    pub is_maximum: bool,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_display() {
        let err = KaizenError::MetricsError("test error".to_string());
        assert_eq!(err.to_string(), "Metrics error: test error");
    }
}
