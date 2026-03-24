//! Code Agent
//!
//! Generates, validates, and formats code using tool sequences.
//! Demonstrates multi-step tool composition for complex goals.

use crate::agents::base::{Agent, AgentBase, AgentConfig, AgentType};
use crate::execution::{ExecutionResult, MockExecutor};
use crate::goals::Goal;
use std::sync::Arc;

/// Code agent that generates, validates, and formats code
pub struct CodeAgent {
    base: AgentBase,
}

impl CodeAgent {
    /// Create a new code agent
    pub fn new(base: AgentBase) -> Self {
        assert_eq!(base.config().agent_type, AgentType::Code);
        Self { base }
    }

    /// Create a code agent with configuration
    pub fn with_config(config: AgentConfig, executor: Arc<MockExecutor>) -> Self {
        let base = AgentBase::new(config, executor);
        Self { base }
    }

    /// Generate code from specification
    pub async fn generate_code(&self, spec: &str, language: &str) -> CodeGenerationResult {
        let mut goal = Goal::new(
            crate::goals::GoalType::GenerateCode,
            format!("Generate {} code", language),
        );

        let mut params = std::collections::BTreeMap::new();
        params.insert("language".to_string(), serde_json::Value::String(language.to_string()));
        params.insert("specification".to_string(), serde_json::Value::String(spec.to_string()));

        goal = goal.with_context(spec.to_string(), params);

        let execution = self.base.execute_and_learn(&goal).await;

        CodeGenerationResult {
            language: language.to_string(),
            execution_id: execution.id.clone(),
            success: execution.goal_achieved,
            generated_code: execution.final_output,
            validation_passed: execution.step_results.iter().any(|s| {
                s.tool_name.contains("validate")
            }),
            formatted: execution.step_results.iter().any(|s| {
                s.tool_name.contains("format")
            }),
            duration_ms: execution.total_execution_time_ms,
        }
    }
}

impl Agent for CodeAgent {
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

/// Result of code generation
#[derive(Debug, Clone)]
pub struct CodeGenerationResult {
    pub language: String,
    pub execution_id: String,
    pub success: bool,
    pub generated_code: Option<String>,
    pub validation_passed: bool,
    pub formatted: bool,
    pub duration_ms: u128,
}

impl CodeGenerationResult {
    /// Get code quality score (0-100)
    pub fn quality_score(&self) -> u32 {
        let mut score = 0u32;
        if self.success { score += 25; }
        if self.validation_passed { score += 35; }
        if self.formatted { score += 25; }
        if self.generated_code.as_ref().map(|c| !c.is_empty()).unwrap_or(false) { score += 15; }
        score
    }
}

impl std::fmt::Display for CodeGenerationResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} generation: quality={}, validated={}, formatted={}, {}ms",
            self.language,
            self.quality_score(),
            self.validation_passed,
            self.formatted,
            self.duration_ms
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::execution::MockExecutor;
    use std::sync::Arc;

    #[tokio::test]
    async fn test_code_agent_creation() {
        let config = AgentConfig::new("CodeGen".to_string(), AgentType::Code);
        let executor = Arc::new(MockExecutor::new());
        let agent = CodeAgent::with_config(config, executor);

        assert_eq!(agent.config().agent_type, AgentType::Code);
    }

    #[tokio::test]
    async fn test_code_generation() {
        let config = AgentConfig::new("CodeGen".to_string(), AgentType::Code);
        let executor = Arc::new(MockExecutor::new());
        let agent = CodeAgent::with_config(config, executor);

        let _result = agent.generate_code("fn hello() {}", "rust").await;
    }

    #[test]
    fn test_code_quality_score() {
        let result = CodeGenerationResult {
            language: "rust".to_string(),
            execution_id: "exec-1".to_string(),
            success: true,
            generated_code: Some("code".to_string()),
            validation_passed: true,
            formatted: true,
            duration_ms: 1000,
        };

        assert!(result.quality_score() > 50);
    }
}
