// Kanban pull system for ontology evolution
// Work-in-progress limits, flow metrics, and demand-driven generation

pub mod workflow;
pub mod metrics;
pub mod card;
pub mod limits;
pub mod flow_diagram;
pub mod demand;

pub use workflow::{WorkflowState, WorkflowStage, KanbanBoard};
pub use metrics::{FlowMetrics, TransformationMetrics, EfficiencyCalculator};
pub use card::{WorkflowCard, CardStatus, RdfChangeTracker};
pub use limits::{WipLimits, WipPolicy, LimitEnforcer};
pub use flow_diagram::{CumulativeFlowDiagram, FlowData, BottleneckAnalyzer};
pub use demand::{DemandSignal, DemandDrivenGenerator, PullSystem};

use std::collections::HashMap;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

/// Result type for Kanban operations
pub type KanbanResult<T> = Result<T, KanbanError>;

/// Errors that can occur in the Kanban system
#[derive(Debug, thiserror::Error)]
pub enum KanbanError {
    #[error("WIP limit exceeded: {0} items in stage {1}, limit is {2}")]
    WipLimitExceeded(usize, String, usize),

    #[error("Invalid state transition from {0} to {1}")]
    InvalidTransition(String, String),

    #[error("Card not found: {0}")]
    CardNotFound(String),

    #[error("Stage not found: {0}")]
    StageNotFound(String),

    #[error("Demand signal not ready: {0}")]
    DemandNotReady(String),

    #[error("Bottleneck detected in stage {0}: {1}")]
    BottleneckDetected(String, String),

    #[error("RDF schema validation failed: {0}")]
    RdfValidationFailed(String),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("Serialization error: {0}")]
    Serialization(#[from] serde_json::Error),
}

/// Configuration for the Kanban system
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KanbanConfig {
    /// WIP limits per stage
    pub wip_limits: HashMap<String, usize>,

    /// Efficiency thresholds (0.0 to 1.0)
    pub efficiency_threshold: f64,

    /// Enable automatic bottleneck detection
    pub auto_detect_bottlenecks: bool,

    /// Demand signal polling interval (seconds)
    pub demand_poll_interval: u64,

    /// Maximum concurrent template modifications
    pub max_concurrent_modifications: usize,

    /// Flow metrics collection interval (seconds)
    pub metrics_interval: u64,
}

impl Default for KanbanConfig {
    fn default() -> Self {
        let mut wip_limits = HashMap::new();
        wip_limits.insert("backlog".to_string(), 100);
        wip_limits.insert("analysis".to_string(), 5);
        wip_limits.insert("transformation".to_string(), 3);
        wip_limits.insert("validation".to_string(), 5);
        wip_limits.insert("generation".to_string(), 2);
        wip_limits.insert("done".to_string(), 1000);

        Self {
            wip_limits,
            efficiency_threshold: 0.7,
            auto_detect_bottlenecks: true,
            demand_poll_interval: 30,
            max_concurrent_modifications: 3,
            metrics_interval: 60,
        }
    }
}

/// Event in the Kanban system
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KanbanEvent {
    pub timestamp: DateTime<Utc>,
    pub event_type: KanbanEventType,
    pub card_id: String,
    pub stage: String,
    pub metadata: HashMap<String, String>,
}

/// Types of events in the Kanban system
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum KanbanEventType {
    CardCreated,
    CardMoved,
    CardBlocked,
    CardUnblocked,
    CardCompleted,
    WipLimitReached,
    BottleneckDetected,
    DemandSignalReceived,
    GenerationTriggered,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = KanbanConfig::default();
        assert_eq!(config.wip_limits.get("transformation").unwrap(), &3);
        assert_eq!(config.efficiency_threshold, 0.7);
        assert!(config.auto_detect_bottlenecks);
    }

    #[test]
    fn test_kanban_event_serialization() {
        let event = KanbanEvent {
            timestamp: Utc::now(),
            event_type: KanbanEventType::CardCreated,
            card_id: "card-123".to_string(),
            stage: "backlog".to_string(),
            metadata: HashMap::new(),
        };

        let json = serde_json::to_string(&event).unwrap();
        let deserialized: KanbanEvent = serde_json::from_str(&json).unwrap();
        assert_eq!(deserialized.card_id, "card-123");
    }
}
