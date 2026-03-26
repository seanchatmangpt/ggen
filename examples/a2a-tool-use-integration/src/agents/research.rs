//! Research Agent
//!
//! Discovers information via tools, aggregates results,
//! and provides summaries and insights.

use crate::agents::base::{Agent, AgentBase, AgentConfig, AgentType};
use crate::execution::{ExecutionResult, MockExecutor};
use crate::goals::Goal;
use std::sync::Arc;

/// Research agent that discovers and aggregates information
pub struct ResearchAgent {
    base: AgentBase,
}

impl ResearchAgent {
    /// Create a new research agent
    pub fn new(base: AgentBase) -> Self {
        assert_eq!(base.config().agent_type, AgentType::Research);
        Self { base }
    }

    /// Create a research agent with configuration
    pub fn with_config(config: AgentConfig, executor: Arc<MockExecutor>) -> Self {
        let base = AgentBase::new(config, executor);
        Self { base }
    }

    /// Investigate a research topic
    pub async fn investigate(&self, topic: &str) -> ResearchResult {
        let goal = Goal::new(
            crate::goals::GoalType::ResearchInformation,
            format!("Research topic: {}", topic),
        );

        let execution = self.base.execute_and_learn(&goal).await;

        ResearchResult {
            topic: topic.to_string(),
            execution_id: execution.id.clone(),
            success: execution.goal_achieved,
            findings: execution.final_output,
            sources_consulted: execution.step_results.len(),
            duration_ms: execution.total_execution_time_ms,
        }
    }
}

impl Agent for ResearchAgent {
    fn config(&self) -> &AgentConfig {
        self.base.config()
    }

    async fn execute_goal(&self, goal: &Goal) -> ExecutionResult {
        self.base.execute_goal(goal).await
    }

    fn learning_system(&self) -> &crate::analysis::LearningSystem {
        self.base.learning_system()
    }
}

/// Result of a research investigation
#[derive(Debug, Clone)]
pub struct ResearchResult {
    pub topic: String,
    pub execution_id: String,
    pub success: bool,
    pub findings: Option<String>,
    pub sources_consulted: usize,
    pub duration_ms: u128,
}

impl std::fmt::Display for ResearchResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Research on '{}': {} sources, {} ms, success: {}",
            self.topic, self.sources_consulted, self.duration_ms, self.success
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::execution::MockExecutor;
    use std::sync::Arc;

    #[tokio::test]
    async fn test_research_agent_creation() {
        let config = AgentConfig::new("Researcher".to_string(), AgentType::Research);
        let executor = Arc::new(MockExecutor::new());
        let agent = ResearchAgent::with_config(config, executor);

        assert_eq!(agent.config().agent_type, AgentType::Research);
    }

    #[tokio::test]
    async fn test_research_investigation() {
        let config = AgentConfig::new("Researcher".to_string(), AgentType::Research);
        let executor = Arc::new(MockExecutor::new());
        let agent = ResearchAgent::with_config(config, executor);

        let _result = agent.investigate("Test topic").await;
    }
}
