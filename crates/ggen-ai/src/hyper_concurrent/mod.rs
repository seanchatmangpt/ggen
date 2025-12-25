//! Hyper-Concurrent Agent Execution Framework
//!
//! Maximum 10-agent parallel execution with advanced concurrency patterns:
//! - Work-stealing agent pools for optimal load balancing
//! - Circuit breaker pattern for fault tolerance
//! - Adaptive concurrency based on system resources
//! - Backpressure handling for overwhelm protection
//! - Async streaming for continuous agent output
//! - Synchronization primitives (barriers, channels)
//!
//! ## Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────────┐
//! │                    HyperConcurrentExecutor                      │
//! │  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────────┐ │
//! │  │   Adaptive  │  │   Circuit   │  │     Backpressure        │ │
//! │  │ Concurrency │──│   Breaker   │──│       Handler           │ │
//! │  │ Controller  │  │   Manager   │  │                         │ │
//! │  └─────────────┘  └─────────────┘  └─────────────────────────┘ │
//! │         │                │                     │                │
//! │         └────────────────┼─────────────────────┘                │
//! │                          ▼                                      │
//! │  ┌─────────────────────────────────────────────────────────┐   │
//! │  │              WorkStealingAgentPool (10 agents max)       │   │
//! │  │  ┌───┐ ┌───┐ ┌───┐ ┌───┐ ┌───┐ ┌───┐ ┌───┐ ┌───┐ ┌───┐ ┌───┐ │
//! │  │  │A1 │ │A2 │ │A3 │ │A4 │ │A5 │ │A6 │ │A7 │ │A8 │ │A9 │ │A10│ │
//! │  │  └───┘ └───┘ └───┘ └───┘ └───┘ └───┘ └───┘ └───┘ └───┘ └───┘ │
//! │  └─────────────────────────────────────────────────────────┘   │
//! │                          │                                      │
//! │                          ▼                                      │
//! │  ┌─────────────────────────────────────────────────────────┐   │
//! │  │                ChannelOrchestrator                       │   │
//! │  │     (flume channels for inter-agent communication)       │   │
//! │  └─────────────────────────────────────────────────────────┘   │
//! └─────────────────────────────────────────────────────────────────┘
//! ```

pub mod adaptive_controller;
pub mod backpressure;
pub mod barrier;
pub mod channel_orchestrator;
pub mod circuit_breaker;
pub mod executor;
pub mod metrics;
pub mod streaming;
pub mod work_stealing;

pub use adaptive_controller::*;
pub use backpressure::*;
pub use barrier::*;
pub use channel_orchestrator::*;
pub use circuit_breaker::*;
pub use executor::*;
pub use metrics::*;
pub use streaming::*;
pub use work_stealing::*;

use serde::{Deserialize, Serialize};

/// Maximum concurrent agents (Claude Code's limit)
pub const MAX_CONCURRENT_AGENTS: usize = 10;

/// Default agent execution timeout in seconds
pub const DEFAULT_AGENT_TIMEOUT_SECS: u64 = 60;

/// Hyper-concurrent execution configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HyperConcurrentConfig {
    /// Maximum concurrent agents (1-10)
    pub max_agents: usize,
    /// Enable work stealing
    pub enable_work_stealing: bool,
    /// Enable circuit breaker
    pub enable_circuit_breaker: bool,
    /// Enable adaptive concurrency
    pub enable_adaptive_concurrency: bool,
    /// Enable backpressure handling
    pub enable_backpressure: bool,
    /// Agent timeout in seconds
    pub agent_timeout_secs: u64,
    /// Circuit breaker failure threshold
    pub circuit_breaker_threshold: u32,
    /// Backpressure queue size
    pub backpressure_queue_size: usize,
    /// Metrics collection enabled
    pub metrics_enabled: bool,
}

impl Default for HyperConcurrentConfig {
    fn default() -> Self {
        Self {
            max_agents: MAX_CONCURRENT_AGENTS,
            enable_work_stealing: true,
            enable_circuit_breaker: true,
            enable_adaptive_concurrency: true,
            enable_backpressure: true,
            agent_timeout_secs: DEFAULT_AGENT_TIMEOUT_SECS,
            circuit_breaker_threshold: 5,
            backpressure_queue_size: 100,
            metrics_enabled: true,
        }
    }
}

impl HyperConcurrentConfig {
    /// Create a maximum performance configuration
    pub fn max_performance() -> Self {
        Self {
            max_agents: MAX_CONCURRENT_AGENTS,
            enable_work_stealing: true,
            enable_circuit_breaker: true,
            enable_adaptive_concurrency: true,
            enable_backpressure: true,
            agent_timeout_secs: 30,
            circuit_breaker_threshold: 3,
            backpressure_queue_size: 50,
            metrics_enabled: true,
        }
    }

    /// Create a conservative configuration for stability
    pub fn conservative() -> Self {
        Self {
            max_agents: 5,
            enable_work_stealing: true,
            enable_circuit_breaker: true,
            enable_adaptive_concurrency: false,
            enable_backpressure: true,
            agent_timeout_secs: 120,
            circuit_breaker_threshold: 10,
            backpressure_queue_size: 200,
            metrics_enabled: true,
        }
    }

    /// Create a development/testing configuration
    pub fn development() -> Self {
        Self {
            max_agents: 3,
            enable_work_stealing: false,
            enable_circuit_breaker: false,
            enable_adaptive_concurrency: false,
            enable_backpressure: false,
            agent_timeout_secs: 300,
            circuit_breaker_threshold: 100,
            backpressure_queue_size: 1000,
            metrics_enabled: true,
        }
    }
}

/// Execution priority for agent tasks
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum ExecutionPriority {
    /// Critical priority - immediate execution
    Critical = 0,
    /// High priority - next available slot
    High = 1,
    /// Normal priority - standard queue
    Normal = 2,
    /// Low priority - background execution
    Low = 3,
    /// Idle priority - only when system is idle
    Idle = 4,
}

impl Default for ExecutionPriority {
    fn default() -> Self {
        Self::Normal
    }
}

/// Agent execution state
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum AgentExecutionState {
    /// Agent is idle, ready for work
    Idle,
    /// Agent is executing a task
    Running,
    /// Agent completed successfully
    Completed,
    /// Agent failed with error
    Failed,
    /// Agent was cancelled
    Cancelled,
    /// Agent timed out
    TimedOut,
    /// Agent is in circuit breaker cooldown
    CircuitOpen,
}

/// Agent execution result with detailed metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentExecutionResult<T> {
    /// The result value if successful
    pub value: Option<T>,
    /// Agent ID that produced this result
    pub agent_id: String,
    /// Execution state
    pub state: AgentExecutionState,
    /// Execution duration in milliseconds
    pub duration_ms: u64,
    /// Number of retry attempts
    pub retry_count: u32,
    /// Error message if failed
    pub error: Option<String>,
    /// Execution timestamp
    pub timestamp: String,
    /// Additional metadata
    pub metadata: std::collections::HashMap<String, String>,
}

impl<T> AgentExecutionResult<T> {
    /// Create a successful result
    pub fn success(value: T, agent_id: String, duration_ms: u64) -> Self {
        Self {
            value: Some(value),
            agent_id,
            state: AgentExecutionState::Completed,
            duration_ms,
            retry_count: 0,
            error: None,
            timestamp: chrono::Utc::now().to_rfc3339(),
            metadata: std::collections::HashMap::new(),
        }
    }

    /// Create a failed result
    pub fn failure(agent_id: String, error: String, duration_ms: u64) -> Self {
        Self {
            value: None,
            agent_id,
            state: AgentExecutionState::Failed,
            duration_ms,
            retry_count: 0,
            error: Some(error),
            timestamp: chrono::Utc::now().to_rfc3339(),
            metadata: std::collections::HashMap::new(),
        }
    }

    /// Check if execution was successful
    pub fn is_success(&self) -> bool {
        matches!(self.state, AgentExecutionState::Completed)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = HyperConcurrentConfig::default();
        assert_eq!(config.max_agents, MAX_CONCURRENT_AGENTS);
        assert!(config.enable_work_stealing);
        assert!(config.enable_circuit_breaker);
    }

    #[test]
    fn test_max_performance_config() {
        let config = HyperConcurrentConfig::max_performance();
        assert_eq!(config.max_agents, 10);
        assert_eq!(config.agent_timeout_secs, 30);
    }

    #[test]
    fn test_execution_priority_ordering() {
        assert!(ExecutionPriority::Critical < ExecutionPriority::High);
        assert!(ExecutionPriority::High < ExecutionPriority::Normal);
        assert!(ExecutionPriority::Normal < ExecutionPriority::Low);
    }

    #[test]
    fn test_execution_result_success() {
        let result: AgentExecutionResult<String> =
            AgentExecutionResult::success("test".to_string(), "agent-1".to_string(), 100);
        assert!(result.is_success());
        assert_eq!(result.value, Some("test".to_string()));
    }

    #[test]
    fn test_execution_result_failure() {
        let result: AgentExecutionResult<String> =
            AgentExecutionResult::failure("agent-1".to_string(), "error".to_string(), 50);
        assert!(!result.is_success());
        assert!(result.error.is_some());
    }
}
