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
    fn test_mape_k_full_loop() {
        // Monitor: Ingest observations
        let mut monitor = MonitorEngine::new();
        let now = get_timestamp();

        for i in 0..5 {
            monitor.ingest_observation(Observation {
                id: format!("obs-{}", i),
                obs_type: ObservationType::Metric,
                timestamp: now - (5000 - i as u64 * 100),
                data: serde_json::json!({"pattern": "expensive", "ticks": 15}),
                source: "monitor".to_string(),
            });
        }

        let aggregates = monitor.run_aggregations();
        assert!(!aggregates.is_empty());

        // Analyze: Generate findings
        let mut analyzer = AnalyzeEngine::new(SLOConfig::default());
        let findings = analyzer.analyze(&monitor);
        assert!(!findings.is_empty());

        // Plan: Generate proposals
        let mut planner = PlanEngine::new();
        let proposals = planner.plan(&findings);
        assert!(!proposals.is_empty());

        // Execute: Validate overlays
        let mut executor = ExecuteEngine::new();
        let mut proposal = proposals[0].clone();
        let result = executor.execute(&mut proposal);
        assert!(result.success || result.validation_status == ValidationStatus::ReviewNeeded);

        // Knowledge: Store all data
        let mut knowledge = KStore::new();
        for obs in monitor.observations() {
            knowledge.record_observation(obs.clone());
        }
        for finding in &findings {
            knowledge.record_finding(finding.clone());
        }

        let stats = knowledge.statistics();
        assert_eq!(stats.total_observations, 5);
        assert!(stats.total_findings > 0);
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
    fn test_mape_k_monitor_aggregations() {
        let mut monitor = MonitorEngine::new();
        let now = get_timestamp();

        for i in 0..3 {
            monitor.ingest_observation(Observation {
                id: format!("tick-{}", i),
                obs_type: ObservationType::Metric,
                timestamp: now - (3000 - i as u64 * 100),
                data: serde_json::json!({"pattern": format!("p{}", i), "ticks": 10 + i * 2}),
                source: "monitor".to_string(),
            });
        }

        monitor.run_aggregations();
        let violations = monitor.query_tick_budget_violations();
        assert!(!violations.is_empty());
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
