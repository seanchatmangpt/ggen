//! Base agent implementation
//!
//! Common agent functionality for goal execution,
//! tool discovery, planning, and learning.

use crate::analysis::{Analyzer, LearningSystem};
use crate::execution::{Executor, ExecutionResult, MockExecutor};
use crate::goals::Goal;
use crate::planning::PlanGenerator;
use crate::tool_discovery::{Tool, ToolRegistry};
use serde::{Deserialize, Serialize};
use uuid::Uuid;
use std::sync::Arc;

/// Agent configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentConfig {
    pub id: String,
    pub name: String,
    pub agent_type: AgentType,
    pub max_retries: usize,
    pub timeout_ms: u64,
    pub enable_learning: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum AgentType {
    Research,
    Code,
    Data,
}

impl std::fmt::Display for AgentType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Research => write!(f, "Research"),
            Self::Code => write!(f, "Code"),
            Self::Data => write!(f, "Data"),
        }
    }
}

impl AgentConfig {
    /// Create new agent config
    pub fn new(name: String, agent_type: AgentType) -> Self {
        Self {
            id: Uuid::new_v4().to_string(),
            name,
            agent_type,
            max_retries: 3,
            timeout_ms: 30000,
            enable_learning: true,
        }
    }
}

/// Base agent trait
pub trait Agent: Send + Sync {
    /// Get agent configuration
    fn config(&self) -> &AgentConfig;

    /// Execute a goal
    fn execute_goal(&self, goal: &Goal) -> impl std::future::Future<Output = ExecutionResult> + Send;

    /// Get learning system
    fn learning_system(&self) -> &LearningSystem;
}

/// Base agent implementation
pub struct AgentBase {
    config: AgentConfig,
    registry: ToolRegistry,
    learning: LearningSystem,
    executor: Arc<MockExecutor>,
}

impl AgentBase {
    /// Create a new agent
    pub fn new(config: AgentConfig, executor: Arc<MockExecutor>) -> Self {
        Self {
            config,
            registry: ToolRegistry::new(),
            learning: LearningSystem::new(),
            executor,
        }
    }

    /// Register a tool
    pub fn register_tool(&mut self, tool: Tool) {
        self.registry.register(tool);
    }

    /// Get registered tools
    pub fn tools(&self) -> &ToolRegistry {
        &self.registry
    }

    /// Get mutable registry
    pub fn tools_mut(&mut self) -> &mut ToolRegistry {
        &mut self.registry
    }

    /// Execute goal and learn from result
    pub async fn execute_and_learn(&self, goal: &Goal) -> ExecutionResult {
        // Discover relevant tools
        let available_tools = self.registry.list_tools()
            .iter()
            .filter_map(|name| self.registry.get(name))
            .cloned()
            .collect::<Vec<_>>();

        if available_tools.is_empty() {
            let mut result = ExecutionResult::new("unknown".to_string());
            result.error_message = Some("No tools available".to_string());
            return result;
        }

        // Generate plan
        let plan = match PlanGenerator::generate_plan(goal, &available_tools) {
            Ok(p) => p,
            Err(e) => {
                let mut result = ExecutionResult::new("unknown".to_string());
                result.error_message = Some(format!("Plan generation failed: {}", e));
                return result;
            }
        };

        // Execute plan
        let executor = Executor::new(Box::new(self.executor.as_ref().clone()));
        let execution = executor.execute(&plan).await;

        // Analyze results
        let analysis = Analyzer::analyze(&execution, goal);

        // Learn from successful execution
        if execution.goal_achieved && self.config.enable_learning {
            for pattern in analysis.learned_patterns {
                // In a real implementation, we would update the learning system
                // For now, we just track that learning occurred
                let _ = pattern;
            }
        }

        execution
    }

    pub fn learning_system(&self) -> &LearningSystem {
        &self.learning
    }
}

impl Agent for AgentBase {
    fn config(&self) -> &AgentConfig {
        &self.config
    }

    async fn execute_goal(&self, goal: &Goal) -> ExecutionResult {
        self.execute_and_learn(goal).await
    }

    fn learning_system(&self) -> &LearningSystem {
        &self.learning
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_agent_config() {
        let config = AgentConfig::new("Test Agent".to_string(), AgentType::Code);
        assert_eq!(config.name, "Test Agent");
        assert_eq!(config.agent_type, AgentType::Code);
    }

    #[tokio::test]
    async fn test_agent_execution() {
        let config = AgentConfig::new("Test Agent".to_string(), AgentType::Code);
        let executor = Arc::new(MockExecutor::new());
        let agent = AgentBase::new(config, executor);

        let goal = Goal::new(
            crate::goals::GoalType::GenerateCode,
            "Test goal".to_string(),
        );

        // Note: This will fail without registered tools, but tests the flow
        let _result = agent.execute_and_learn(&goal).await;
    }
}
