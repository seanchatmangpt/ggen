//! ggen Microframework - Maximum 10-Agent Parallel Execution
//!
//! A lightweight, high-performance microframework for orchestrating up to 10
//! concurrent agents for code generation, testing, review, and more.
//!
//! ## Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────────────────┐
//! │                        ggen Microframework                               │
//! │                                                                          │
//! │  ┌──────────────────────────────────────────────────────────────────┐  │
//! │  │                     AgentOrchestrator                             │  │
//! │  │   ┌─────────────┐  ┌─────────────┐  ┌─────────────────────────┐  │  │
//! │  │   │   Builder   │  │    Task     │  │      Progress           │  │  │
//! │  │   │   Pattern   │──│    Graph    │──│      Tracker            │  │  │
//! │  │   └─────────────┘  └─────────────┘  └─────────────────────────┘  │  │
//! │  └──────────────────────────────────────────────────────────────────┘  │
//! │                                  │                                       │
//! │  ┌──────────────────────────────────────────────────────────────────┐  │
//! │  │                    Pre-built Agent Types                          │  │
//! │  │  ┌──────┐ ┌──────┐ ┌──────┐ ┌──────┐ ┌──────┐ ┌──────┐ ┌──────┐  │  │
//! │  │  │CodeGe│ │Tester│ │Review│ │RdfPro│ │TmplGe│ │Valida│ │Custom│  │  │
//! │  │  │n     │ │      │ │er    │ │cessor│ │n     │ │tor   │ │      │  │  │
//! │  │  └──────┘ └──────┘ └──────┘ └──────┘ └──────┘ └──────┘ └──────┘  │  │
//! │  └──────────────────────────────────────────────────────────────────┘  │
//! │                                  │                                       │
//! │  ┌──────────────────────────────────────────────────────────────────┐  │
//! │  │                  HyperConcurrentExecutor (10 max)                 │  │
//! │  └──────────────────────────────────────────────────────────────────┘  │
//! └─────────────────────────────────────────────────────────────────────────┘
//! ```
//!
//! ## Quick Start
//!
//! ```rust,no_run
//! use ggen_ai::microframework::prelude::*;
//!
//! # async fn example() -> Result<(), Box<dyn std::error::Error>> {
//! // Create orchestrator with max concurrency
//! let orchestrator = AgentOrchestrator::builder()
//!     .max_agents(10)
//!     .enable_circuit_breaker()
//!     .enable_backpressure()
//!     .build()?;
//!
//! // Execute 10 agents in parallel
//! let results = orchestrator
//!     .spawn_batch(vec![
//!         Task::code_gen("Generate Rust struct"),
//!         Task::test("Write unit tests"),
//!         Task::review("Review for safety"),
//!     ])
//!     .await?;
//!
//! # Ok(())
//! # }
//! ```

pub mod agents;
pub mod batch;
pub mod builder;
pub mod orchestrator;
pub mod pipeline;
pub mod progress;
pub mod task_graph;
pub mod tasks;

pub use agents::*;
pub use batch::*;
pub use builder::*;
pub use orchestrator::*;
pub use pipeline::*;
pub use progress::*;
pub use task_graph::*;
pub use tasks::*;

/// Prelude for convenient imports
pub mod prelude {
    pub use super::agents::{
        AgentRole, CodeGenAgent, CustomAgent, ReviewerAgent, RdfProcessorAgent,
        TemplateGenAgent, TesterAgent, ValidatorAgent,
    };
    pub use super::batch::{BatchBuilder, BatchConfig, BatchProcessor, BatchResult};
    pub use super::builder::OrchestratorBuilder;
    pub use super::orchestrator::AgentOrchestrator;
    pub use super::pipeline::{Pipeline, PipelineStage, PipelineBuilder};
    pub use super::progress::{ProgressTracker, TaskProgress};
    pub use super::task_graph::{TaskGraph, GraphStats};
    pub use super::tasks::{Task, TaskConfig, TaskResult, TaskStatus};
    pub use crate::hyper_concurrent::{
        HyperConcurrentConfig, HyperConcurrentExecutor, MAX_CONCURRENT_AGENTS,
    };
}

use serde::{Deserialize, Serialize};

/// Microframework configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MicroframeworkConfig {
    /// Maximum concurrent agents (1-10)
    pub max_agents: usize,
    /// Enable work stealing for load balancing
    pub enable_work_stealing: bool,
    /// Enable circuit breaker for fault tolerance
    pub enable_circuit_breaker: bool,
    /// Enable backpressure handling
    pub enable_backpressure: bool,
    /// Enable progress tracking
    pub enable_progress_tracking: bool,
    /// Default task timeout in seconds
    pub default_timeout_secs: u64,
    /// Enable metrics collection
    pub enable_metrics: bool,
}

impl Default for MicroframeworkConfig {
    fn default() -> Self {
        Self {
            max_agents: 10,
            enable_work_stealing: true,
            enable_circuit_breaker: true,
            enable_backpressure: true,
            enable_progress_tracking: true,
            default_timeout_secs: 60,
            enable_metrics: true,
        }
    }
}

impl MicroframeworkConfig {
    /// Create a high-performance configuration
    pub fn high_performance() -> Self {
        Self {
            max_agents: 10,
            enable_work_stealing: true,
            enable_circuit_breaker: true,
            enable_backpressure: true,
            enable_progress_tracking: false, // Disable for max performance
            default_timeout_secs: 30,
            enable_metrics: false,
        }
    }

    /// Create a development configuration
    pub fn development() -> Self {
        Self {
            max_agents: 3,
            enable_work_stealing: false,
            enable_circuit_breaker: false,
            enable_backpressure: false,
            enable_progress_tracking: true,
            default_timeout_secs: 300,
            enable_metrics: true,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = MicroframeworkConfig::default();
        assert_eq!(config.max_agents, 10);
        assert!(config.enable_work_stealing);
    }

    #[test]
    fn test_high_performance_config() {
        let config = MicroframeworkConfig::high_performance();
        assert_eq!(config.max_agents, 10);
        assert!(!config.enable_progress_tracking);
    }
}
