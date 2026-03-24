//! A2A Tool Use Integration Example
//!
//! Demonstrates autonomous agent tool discovery and use:
//! 1. Goal decomposition (high-level → task sequence)
//! 2. Tool discovery (query MCP for available tools)
//! 3. Planning (convert goals to tool sequences)
//! 4. Execution (run tools in order)
//! 5. Analysis (verify goal completion)
//! 6. Learning (improve from successes/failures)

pub mod goals;
pub mod tool_discovery;
pub mod planning;
pub mod execution;
pub mod analysis;
pub mod agents;

pub use goals::{Goal, GoalType, GoalState};
pub use tool_discovery::{Tool, ToolDiscovery};
pub use planning::{Plan, ExecutionStep};
pub use execution::{Executor, ExecutionResult, ExecutionStatus};
pub use analysis::{Analyzer, AnalysisResult};
pub use agents::{Agent, AgentBase};

#[derive(Debug, Clone)]
pub struct Config {
    pub max_retries: usize,
    pub timeout_ms: u64,
    pub enable_learning: bool,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            max_retries: 3,
            timeout_ms: 30000,
            enable_learning: true,
        }
    }
}
