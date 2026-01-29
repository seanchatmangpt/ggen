//! Autonomic computing module for self-managing systems
//!
//! This module provides MAPE-K loop capabilities including:
//! - Failure knowledge base for learning from past failures
//! - Pattern recognition and recovery strategy effectiveness tracking
//! - Historical failure analysis for predictive recovery
//! - Recovery strategy planning and selection
//! - Execute phase for automated recovery with rollback capability

pub mod analyze;
pub mod chaos_orchestrator;
pub mod execute;
pub mod knowledge_base;
pub mod monitor;
pub mod plan;

// Re-export commonly used types
pub use analyze::{
    ClusterState, DetectedFailure, ResilienceAnalyzer, ResourceType, SymptomMatch,
    SymptomMatcher,
};
pub use chaos_orchestrator::{
    ChaosEvent, ChaosOrchestratorConfig, ChaosReport, ChaosScheduler, ContinuousChaosOrchestrator,
};
pub use execute::{
    BackendManager, ExecutionResult, ExecutorConfig, NetworkController, PostRecoveryMetrics,
    RecoveryStatus, ResilienceExecutor, SupervisorController,
};
pub use knowledge_base::{
    FailureKnowledgeBase, HistoricalFailure, KnowledgeBaseConfig,
    StrategyEffectiveness,
};
pub use monitor::{
    BackendService, BackendStatus, ClusterHealthChecker, ClusterMetrics, HealthStatus,
    MonitorConfig, PrometheusCollector, ResilienceMonitor, ResourceUsage,
};
pub use plan::{
    AnalysisResult, BackendType, FailureType, PlannerConfig, Precondition, Priority,
    RecoveryCost, RecoveryPlan, RecoveryStrategy, ResiliencePlanner, RestartType,
    RollbackPlan, RollbackStep,
};
