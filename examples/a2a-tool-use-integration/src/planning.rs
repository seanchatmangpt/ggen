//! Planning engine for converting goals to tool sequences
//!
//! Converts high-level goals into executable plans (sequences of tools)
//! with proper input/output mapping and error handling.

use crate::goals::Goal;
use crate::tool_discovery::Tool;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use uuid::Uuid;

/// A plan is a sequence of tool invocations to achieve a goal
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Plan {
    pub id: String,
    pub goal_id: String,
    pub steps: Vec<ExecutionStep>,
    pub estimated_time_ms: u64,
}

impl Plan {
    /// Create a new plan for a goal
    pub fn new(goal_id: String) -> Self {
        Self {
            id: Uuid::new_v4().to_string(),
            goal_id,
            steps: Vec::new(),
            estimated_time_ms: 0,
        }
    }

    /// Add an execution step to the plan
    pub fn add_step(&mut self, step: ExecutionStep) {
        step.order as usize; // Validate order field exists
        self.steps.push(step);
        self.recalculate_estimate();
    }

    /// Get steps in execution order
    pub fn ordered_steps(&self) -> Vec<&ExecutionStep> {
        let mut steps = self.steps.iter().collect::<Vec<_>>();
        steps.sort_by_key(|s| s.order);
        steps
    }

    /// Recalculate estimated execution time
    fn recalculate_estimate(&mut self) {
        self.estimated_time_ms = self
            .steps
            .iter()
            .map(|s| s.timeout_ms.unwrap_or(5000))
            .sum();
    }

    /// Validate the plan is executable
    pub fn validate(&self) -> Result<(), String> {
        if self.steps.is_empty() {
            return Err("Plan has no steps".to_string());
        }

        let sorted = self.ordered_steps();
        for (i, step) in sorted.iter().enumerate() {
            if step.order != (i + 1) as u32 {
                return Err(format!(
                    "Step order mismatch at position {}: expected {}, got {}",
                    i, i + 1, step.order
                ));
            }
        }

        Ok(())
    }
}

/// Single step in a plan (tool invocation)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExecutionStep {
    pub id: String,
    pub order: u32,
    pub tool: Tool,
    pub input_mapping: BTreeMap<String, String>,
    pub output_binding: String,
    pub timeout_ms: Option<u64>,
    pub retry_policy: Option<RetryPolicy>,
    pub error_handling: ErrorHandling,
    pub depends_on: Vec<String>,
}

impl ExecutionStep {
    /// Create a new execution step for a tool
    pub fn new(order: u32, tool: Tool) -> Self {
        Self {
            id: Uuid::new_v4().to_string(),
            order,
            tool,
            input_mapping: BTreeMap::new(),
            output_binding: String::new(),
            timeout_ms: Some(5000),
            retry_policy: None,
            error_handling: ErrorHandling::Retry,
            depends_on: Vec::new(),
        }
    }

    /// Map input parameter from previous step output
    pub fn with_input_mapping(mut self, param: String, source: String) -> Self {
        self.input_mapping.insert(param, source);
        self
    }

    /// Set output binding for next step
    pub fn with_output_binding(mut self, binding: String) -> Self {
        self.output_binding = binding;
        self
    }

    /// Set timeout for this step
    pub fn with_timeout_ms(mut self, ms: u64) -> Self {
        self.timeout_ms = Some(ms);
        self
    }

    /// Set retry policy
    pub fn with_retry(mut self, policy: RetryPolicy) -> Self {
        self.retry_policy = Some(policy);
        self
    }

    /// Set error handling strategy
    pub fn with_error_handling(mut self, handling: ErrorHandling) -> Self {
        self.error_handling = handling;
        self
    }

    /// Add dependency on another step
    pub fn depends_on(mut self, step_id: String) -> Self {
        self.depends_on.push(step_id);
        self
    }
}

/// Retry configuration for a step
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RetryPolicy {
    pub max_retries: u32,
    pub backoff_ms: u64,
    pub backoff_multiplier: f32,
}

impl RetryPolicy {
    /// Create a new retry policy
    pub fn new(max_retries: u32) -> Self {
        Self {
            max_retries,
            backoff_ms: 100,
            backoff_multiplier: 2.0,
        }
    }

    /// Calculate backoff time for attempt number
    pub fn backoff_for_attempt(&self, attempt: u32) -> u64 {
        let multiplier = self.backoff_multiplier.powi(attempt as i32);
        (self.backoff_ms as f32 * multiplier) as u64
    }
}

/// How to handle step failures
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ErrorHandling {
    Continue,
    Retry,
    Abort,
}

/// Plans generated for different goal types
pub struct PlanGenerator;

impl PlanGenerator {
    /// Generate a plan from a goal and available tools
    pub fn generate_plan(goal: &Goal, tools: &[Tool]) -> Result<Plan, String> {
        let mut plan = Plan::new(goal.id.clone());

        match goal.goal_type {
            crate::goals::GoalType::GenerateCode => {
                Self::plan_code_generation(&mut plan, goal, tools)?;
            }
            crate::goals::GoalType::AnalyzeData => {
                Self::plan_data_analysis(&mut plan, goal, tools)?;
            }
            _ => {
                return Err("Unsupported goal type".to_string());
            }
        }

        plan.validate()?;
        Ok(plan)
    }

    /// Generate plan for code generation goal
    fn plan_code_generation(plan: &mut Plan, _goal: &Goal, tools: &[Tool]) -> Result<(), String> {
        // Find code generation tool
        let code_gen = tools
            .iter()
            .find(|t| t.name.contains("generate"))
            .ok_or("No code generation tool found")?;

        let mut step1 = ExecutionStep::new(1, code_gen.clone());
        step1.output_binding = "generated_code".to_string();
        plan.add_step(step1);

        // Find validation tool
        if let Some(validator) = tools.iter().find(|t| t.name.contains("validate")) {
            let step2 = ExecutionStep::new(2, validator.clone())
                .with_input_mapping("code".to_string(), "generated_code".to_string())
                .with_output_binding("validation_result".to_string())
                .with_error_handling(ErrorHandling::Retry);
            plan.add_step(step2);
        }

        // Find formatter tool
        if let Some(formatter) = tools.iter().find(|t| t.name.contains("format")) {
            let step3 = ExecutionStep::new(3, formatter.clone())
                .with_input_mapping("code".to_string(), "generated_code".to_string())
                .with_output_binding("formatted_code".to_string());
            plan.add_step(step3);
        }

        Ok(())
    }

    /// Generate plan for data analysis goal
    fn plan_data_analysis(plan: &mut Plan, _goal: &Goal, tools: &[Tool]) -> Result<(), String> {
        // Find analyzer tool
        let analyzer = tools
            .iter()
            .find(|t| t.name.contains("analy"))
            .ok_or("No analysis tool found")?;

        let mut step1 = ExecutionStep::new(1, analyzer.clone());
        step1.output_binding = "analysis_result".to_string();
        step1.timeout_ms = Some(10000);
        plan.add_step(step1);

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tool_discovery::ToolCategory;

    fn create_test_tool(name: &str) -> Tool {
        Tool::new(
            name.to_string(),
            format!("Test tool: {}", name),
            ToolCategory::CodeGeneration,
        )
    }

    #[test]
    fn test_plan_creation() {
        let plan = Plan::new("goal-1".to_string());
        assert_eq!(plan.goal_id, "goal-1");
        assert!(plan.steps.is_empty());
    }

    #[test]
    fn test_execution_step() {
        let tool = create_test_tool("test");
        let step = ExecutionStep::new(1, tool);
        assert_eq!(step.order, 1);
    }

    #[test]
    fn test_step_builder() {
        let tool = create_test_tool("code-gen");
        let step = ExecutionStep::new(1, tool)
            .with_input_mapping("input".to_string(), "previous.output".to_string())
            .with_output_binding("generated_code".to_string())
            .with_timeout_ms(5000);

        assert_eq!(step.input_mapping.len(), 1);
        assert_eq!(step.output_binding, "generated_code");
        assert_eq!(step.timeout_ms, Some(5000));
    }

    #[test]
    fn test_plan_validation() {
        let mut plan = Plan::new("goal-1".to_string());
        let tool = create_test_tool("test");
        plan.add_step(ExecutionStep::new(1, tool));

        assert!(plan.validate().is_ok());
    }

    #[test]
    fn test_immutable_plan_with_step() {
        let mut plan = Plan::new("goal-1".to_string());
        let tool = create_test_tool("test");
        plan.add_step(ExecutionStep::new(1, tool));

        assert_eq!(plan.steps.len(), 1);
    }

    #[test]
    fn test_plan_validation_failure() {
        let mut plan = Plan::new("goal-1".to_string());
        // No steps added
        assert!(plan.validate().is_err());
    }

    #[test]
    fn test_retry_policy() {
        let policy = RetryPolicy::new(3);
        assert_eq!(policy.max_retries, 3);
        assert_eq!(policy.backoff_for_attempt(0), 100);
        assert_eq!(policy.backoff_for_attempt(1), 200);
        assert_eq!(policy.backoff_for_attempt(2), 400);
    }
}
