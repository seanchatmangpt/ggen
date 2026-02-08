// Common Execution Framework for 90% Semantic Convergence
// Based on A2A-RS patterns and unified agent interface

pub mod convergence;
pub mod error;
pub mod framework;
pub mod metrics;
pub mod orchestration;
pub mod pipeline;
pub mod recovery;
pub mod types;

// Re-export commonly used types
// Note: framework::* defines PipelineStage and StageResult
pub use convergence::*;
pub use error::*;
pub use framework::*;
pub use orchestration::*;
pub use types::*;

// Don't use glob import for recovery to avoid HealthStatus ambiguity
pub use recovery::HealthStatus as RecoveryHealthStatus;
pub use recovery::{
    AgentHealthStatus, ErrorRecord, HealingEvent, HealingEventType, HealthChecker, HealthRecord,
    HealthThresholds, RecoveryAttempt, RecoveryCondition, RecoveryManager, RecoveryPolicy,
    RecoveryResult, RecoveryStats, RecoveryStatus, RecoveryStrategy, SelfHealingWorkflow,
    SystemHealthStatus,
};

pub use metrics::*;

// Re-export pipeline items, excluding types already exported from framework
pub use pipeline::{
    ComparisonOperator, ConditionType, EnhancedPipelineExecutor, EnhancedPipelineStage,
    EnhancedStageBuilder, ExecutionMetrics, ParallelPipelineExecutor, PipelineBuilder,
    PipelineValidator, ResourceRequirements, RetryPolicy, StageCondition, StageTaskResult,
    StageType,
};

// Prelude for easier imports
pub mod prelude {
    pub use super::{
        // Types
        AgentId,
        ConvergenceConfig,
        ConvergenceMetrics,
        ConvergenceStatus,
        EnhancedPipelineExecutor,
        // Pipeline (from pipeline module)
        EnhancedPipelineStage,
        ExecutionConfig,
        ExecutionContext,
        // Error handling
        ExecutionError,
        // Core framework
        ExecutionFramework,
        ExecutionGraph,
        // Pipeline (from framework)
        ExecutionPipeline,
        ExecutionResult,
        HealthChecker,
        MessageId,
        MetricsCollector,
        OrchestrationContext,
        ParallelPipelineExecutor,
        // Metrics
        PerformanceMetrics,
        PipelineStage,
        RecoveryPolicy,
        // Recovery
        RecoveryStrategy,
        // Convergence
        SemanticConvergenceEngine,
        StageResult,
        TaskId,
        // Orchestration
        TaskOrchestrator,
    };
}
