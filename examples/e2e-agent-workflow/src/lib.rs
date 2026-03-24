//! End-to-End Agent Workflow Integration Example
//!
//! Demonstrates a complete, integrated system combining:
//! 1. A2A Agents - Agent-to-agent task state machine protocol
//! 2. OSIRIS Domains - Life domain goal management and balancing
//! 3. MCP Tools - Model Context Protocol tool discovery and execution
//! 4. Distributed Consensus - Byzantine fault tolerance across agents
//!
//! System Architecture:
//! - Fault Tolerance: Agent failures detected and recovered
//! - Distributed Coordination: Multi-agent consensus for decisions
//! - Autonomous Reasoning: Agents discover tools, plan actions, improve

#![warn(missing_docs)]
#![forbid(unsafe_code)]

pub mod coordination;
pub mod domain_manager;
pub mod lifecycle;
pub mod tool_integration;
pub mod workflow;

pub use coordination::{
    ConsensusManager, ConsensusResult, AgentVote,
};
pub use domain_manager::{
    DomainManager, LifeDomain, DomainGoal, DomainStatus,
};
pub use lifecycle::{
    AgentPool, AgentInstance, AgentHealth, HealthStatus,
};
pub use tool_integration::{
    ToolManager, ToolSpec, ToolCall, ToolResult,
};
pub use workflow::{
    Workflow, WorkflowStep, WorkflowStatus, WorkflowExecutor,
};

pub const VERSION: &str = "0.1.0";

#[derive(Debug, Clone)]
pub struct SystemConfig {
    pub num_agents: usize,
    pub max_retries: usize,
    pub timeout_ms: u64,
    pub enable_consensus: bool,
    pub enable_fault_tolerance: bool,
    pub enable_learning: bool,
}

impl Default for SystemConfig {
    fn default() -> Self {
        Self {
            num_agents: 6,
            max_retries: 3,
            timeout_ms: 30000,
            enable_consensus: true,
            enable_fault_tolerance: true,
            enable_learning: true,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_version() {
        assert_eq!(VERSION, "0.1.0");
    }

    #[test]
    fn test_default_config() {
        let cfg = SystemConfig::default();
        assert_eq!(cfg.num_agents, 6);
        assert!(cfg.enable_consensus);
    }
}
