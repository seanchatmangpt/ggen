// Common Execution Framework for 90% Semantic Convergence
// Based on A2A-RS patterns and unified agent interface

pub mod framework;
pub mod pipeline;
pub mod orchestration;
pub mod error;
pub mod types;
pub mod convergence;
pub mod recovery;
pub mod metrics;

// Re-export commonly used types
pub use framework::*;
pub use pipeline::*;
pub use orchestration::*;
pub use error::*;
pub use types::*;
pub use convergence::*;
pub use recovery::*;
pub use metrics::*;

// Prelude for easier imports
pub mod prelude {
    pub use super::{
        // Core framework
        ExecutionFramework, ExecutionConfig, ExecutionContext,
        // Pipeline
        ExecutionPipeline, PipelineStage,
        // Orchestration
        TaskOrchestrator, OrchestrationContext, ExecutionGraph,
        // Types
        AgentId, TaskId, MessageId, ExecutionResult, ConvergenceStatus,
        // Error handling
        ExecutionError,
        // Convergence
        SemanticConvergenceEngine, ConvergenceMetrics, ConvergenceConfig,
        // Recovery
        RecoveryStrategy, RecoveryPolicy, HealthChecker,
        // Metrics
        PerformanceMetrics, MetricsCollector,
    };
}