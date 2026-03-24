//! Data Agent
//!
//! Analyzes and transforms data using specialized tools.
//! Demonstrates data pipeline construction and analysis.

use crate::agents::base::{Agent, AgentBase, AgentConfig, AgentType};
use crate::execution::{ExecutionResult, MockExecutor};
use crate::goals::Goal;
use std::sync::Arc;

/// Data agent that analyzes and transforms data
pub struct DataAgent {
    base: AgentBase,
}

impl DataAgent {
    /// Create a new data agent
    pub fn new(base: AgentBase) -> Self {
        assert_eq!(base.config().agent_type, AgentType::Data);
        Self { base }
    }

    /// Create a data agent with configuration
    pub fn with_config(config: AgentConfig, executor: Arc<MockExecutor>) -> Self {
        let base = AgentBase::new(config, executor);
        Self { base }
    }

    /// Analyze data and generate insights
    pub async fn analyze(&self, data: &str, analysis_type: &str) -> DataAnalysisResult {
        let mut goal = Goal::new(
            crate::goals::GoalType::AnalyzeData,
            format!("Analyze data: {}", analysis_type),
        );

        let mut params = std::collections::BTreeMap::new();
        params.insert("analysis_type".to_string(), serde_json::Value::String(analysis_type.to_string()));
        params.insert("data_sample".to_string(), serde_json::Value::String(data.to_string()));

        goal = goal.with_context(data.to_string(), params);

        let execution = self.base.execute_and_learn(&goal).await;

        DataAnalysisResult {
            analysis_type: analysis_type.to_string(),
            execution_id: execution.id.clone(),
            success: execution.goal_achieved,
            insights: execution.final_output,
            steps_executed: execution.step_results.len(),
            duration_ms: execution.total_execution_time_ms,
        }
    }

    /// Transform data from one format to another
    pub async fn transform(&self, _data: &str, from_format: &str, to_format: &str) -> DataTransformResult {
        let goal = Goal::new(
            crate::goals::GoalType::TransformData,
            format!("Transform data from {} to {}", from_format, to_format),
        );

        let execution = self.base.execute_and_learn(&goal).await;

        DataTransformResult {
            from_format: from_format.to_string(),
            to_format: to_format.to_string(),
            execution_id: execution.id.clone(),
            success: execution.goal_achieved,
            transformed_data: execution.final_output,
            duration_ms: execution.total_execution_time_ms,
        }
    }
}

impl Agent for DataAgent {
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

/// Result of data analysis
#[derive(Debug, Clone)]
pub struct DataAnalysisResult {
    pub analysis_type: String,
    pub execution_id: String,
    pub success: bool,
    pub insights: Option<String>,
    pub steps_executed: usize,
    pub duration_ms: u128,
}

impl std::fmt::Display for DataAnalysisResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Analysis of {}: {} steps, {} ms, success: {}",
            self.analysis_type, self.steps_executed, self.duration_ms, self.success
        )
    }
}

/// Result of data transformation
#[derive(Debug, Clone)]
pub struct DataTransformResult {
    pub from_format: String,
    pub to_format: String,
    pub execution_id: String,
    pub success: bool,
    pub transformed_data: Option<String>,
    pub duration_ms: u128,
}

impl std::fmt::Display for DataTransformResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Transform {}->{}: success={}, {}ms",
            self.from_format, self.to_format, self.success, self.duration_ms
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::execution::MockExecutor;
    use std::sync::Arc;

    #[tokio::test]
    async fn test_data_agent_creation() {
        let config = AgentConfig::new("DataAnalyzer".to_string(), AgentType::Data);
        let executor = Arc::new(MockExecutor::new());
        let agent = DataAgent::with_config(config, executor);

        assert_eq!(agent.config().agent_type, AgentType::Data);
    }

    #[tokio::test]
    async fn test_data_analysis() {
        let config = AgentConfig::new("DataAnalyzer".to_string(), AgentType::Data);
        let executor = Arc::new(MockExecutor::new());
        let agent = DataAgent::with_config(config, executor);

        let _result = agent.analyze("data_sample", "statistical").await;
    }

    #[tokio::test]
    async fn test_data_transformation() {
        let config = AgentConfig::new("DataAnalyzer".to_string(), AgentType::Data);
        let executor = Arc::new(MockExecutor::new());
        let agent = DataAgent::with_config(config, executor);

        let _result = agent.transform("data", "json", "csv").await;
    }
}
