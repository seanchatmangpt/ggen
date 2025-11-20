//! MAPE-K Autonomic Integration
//!
//! Monitor-Analyze-Plan-Execute-Knowledge feedback loop for self-managing marketplace.
//! Implements the full autonomic control system defined in MAPE-K_AUTONOMIC_INTEGRATION.md

pub mod analyze;
pub mod execute;
pub mod knowledge;
pub mod monitor;
pub mod plan;
pub mod types;

#[cfg(test)]
#[path = "integration_tests.rs"]
mod integration_tests;

// Re-export public API
pub use analyze::{AnalyzeEngine, SLOConfig};
pub use execute::{
    ExecuteEngine, ExecutionResult, PromotionResult, Validator, ValidatorOrchestrator,
};
pub use knowledge::{
    CompactionPolicy, CompactionResult, HistoryQuery, KnowledgeStatistics, KnowledgeStore,
    KnowledgeStore as KStore,
};
pub use monitor::MonitorEngine;
pub use plan::PlanEngine;
pub use types::{
    Finding, FindingKind, MAPEMetrics, Observation, ObservationType, OntologyOverlay, OverlayKind,
    OverlayProposal, OverlayProposer, PolicyAction, PolicyRule, PromotionGate, SnapshotMetadata,
    ValidationResult, ValidationStage, ValidationStatus,
};

#[cfg(test)]
mod integration_tests_inline {
    use super::*;

    fn get_timestamp() -> u64 {
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_millis() as u64)
            .unwrap_or(0)
    }

    #[test]
    fn test_mape_k_knowledge_snapshots() {
        let mut knowledge = KStore::new();
        assert_eq!(knowledge.snapshot_history().len(), 1);

        knowledge.record_promotion("overlay-1".to_string(), "snapshot-1".to_string());
        assert_eq!(knowledge.snapshot_history().len(), 2);
        assert_eq!(knowledge.active_snapshot().unwrap().id, "snapshot-1");
    }

    #[test]
    fn test_mape_k_validators() {
        let executor = ExecuteEngine::new();
        let validators = executor
            .validators()
            .iter()
            .map(|v| v.name())
            .collect::<Vec<_>>();

        assert!(validators.contains(&"SHACL"));
        assert!(validators.contains(&"TDD"));
        assert!(validators.contains(&"Performance"));
        assert!(validators.contains(&"Security"));
    }

    #[test]
    fn test_mape_k_metrics_tracking() {
        let mut knowledge = KStore::new();

        for _ in 0..10 {
            knowledge.record_observation(Observation {
                id: "test".to_string(),
                obs_type: ObservationType::Metric,
                timestamp: get_timestamp(),
                data: serde_json::json!({}),
                source: "test".to_string(),
            });
        }

        knowledge.update_execution_time(500);
        let metrics = knowledge.mape_metrics();
        assert_eq!(metrics.observations_ingested, 10);
        assert_eq!(metrics.total_execution_time_ms, 500);
    }
}
