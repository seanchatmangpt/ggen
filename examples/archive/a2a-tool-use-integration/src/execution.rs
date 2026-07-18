//! Plan execution engine
//!
//! Executes tool sequences in order, managing state transitions,
//! handling failures, and collecting results.

use crate::planning::{Plan, ErrorHandling};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use uuid::Uuid;
use chrono::{DateTime, Utc};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ExecutionStatus {
    Success,
    PartialSuccess,
    Failure,
}

impl std::fmt::Display for ExecutionStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Success => write!(f, "Success"),
            Self::PartialSuccess => write!(f, "PartialSuccess"),
            Self::Failure => write!(f, "Failure"),
        }
    }
}

/// Result of executing a single tool
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StepResult {
    pub step_id: String,
    pub step_order: u32,
    pub tool_name: String,
    pub status: ExecutionStatus,
    pub output: Option<String>,
    pub error: Option<String>,
    pub execution_time_ms: u128,
    pub retries_used: u32,
    pub started_at: DateTime<Utc>,
    pub completed_at: DateTime<Utc>,
}

impl StepResult {
    /// Create a new step result
    pub fn new(step_id: String, step_order: u32, tool_name: String) -> Self {
        let now = Utc::now();
        Self {
            step_id,
            step_order,
            tool_name,
            status: ExecutionStatus::Success,
            output: None,
            error: None,
            execution_time_ms: 0,
            retries_used: 0,
            started_at: now,
            completed_at: now,
        }
    }

    /// Mark as successful with output
    fn success(mut self, output: String, execution_time_ms: u128) -> Self {
        self.status = ExecutionStatus::Success;
        self.output = Some(output);
        self.execution_time_ms = execution_time_ms;
        self.completed_at = Utc::now();
        self
    }

    /// Mark as failed with error
    fn failure(mut self, error: String, execution_time_ms: u128) -> Self {
        self.status = ExecutionStatus::Failure;
        self.error = Some(error);
        self.execution_time_ms = execution_time_ms;
        self.completed_at = Utc::now();
        self
    }
}

/// Complete execution result for a plan
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExecutionResult {
    pub id: String,
    pub plan_id: String,
    pub status: ExecutionStatus,
    pub step_results: Vec<StepResult>,
    pub final_output: Option<String>,
    pub error_message: Option<String>,
    pub total_execution_time_ms: u128,
    pub goal_achieved: bool,
    pub started_at: DateTime<Utc>,
    pub completed_at: DateTime<Utc>,
}

impl ExecutionResult {
    /// Create a new execution result
    pub fn new(plan_id: String) -> Self {
        let now = Utc::now();
        Self {
            id: Uuid::new_v4().to_string(),
            plan_id,
            status: ExecutionStatus::Success,
            step_results: Vec::new(),
            final_output: None,
            error_message: None,
            total_execution_time_ms: 0,
            goal_achieved: false,
            started_at: now,
            completed_at: now,
        }
    }

    /// Add a step result
    pub fn add_step_result(&mut self, result: StepResult) {
        if result.status == ExecutionStatus::Failure {
            self.status = ExecutionStatus::Failure;
        }
        self.step_results.push(result);
    }

    /// Mark as complete
    pub fn complete(&mut self, goal_achieved: bool) {
        if !goal_achieved && self.status == ExecutionStatus::Success {
            self.status = ExecutionStatus::PartialSuccess;
        }
        self.goal_achieved = goal_achieved;
        self.completed_at = Utc::now();
        self.total_execution_time_ms = self
            .step_results
            .iter()
            .map(|s| s.execution_time_ms)
            .sum();
    }

    /// Get output from specific step
    pub fn get_step_output(&self, step_order: u32) -> Option<&str> {
        self.step_results
            .iter()
            .find(|s| s.step_order == step_order)
            .and_then(|s| s.output.as_deref())
    }
}

/// Tool executor - simulates tool invocation with success/failure
pub trait ToolExecutor: Send + Sync {
    /// Execute a tool with given inputs
    fn execute(&self, tool_name: &str, inputs: &BTreeMap<String, String>) -> Result<String, String>;
}

/// Mock executor for testing
#[derive(Clone)]
pub struct MockExecutor {
    responses: BTreeMap<String, Result<String, String>>,
}

impl MockExecutor {
    /// Create a new mock executor
    pub fn new() -> Self {
        Self {
            responses: BTreeMap::new(),
        }
    }

    /// Set response for a tool
    pub fn set_response(mut self, tool_name: String, response: Result<String, String>) -> Self {
        self.responses.insert(tool_name, response);
        self
    }
}

impl Default for MockExecutor {
    fn default() -> Self {
        Self::new()
    }
}

impl ToolExecutor for MockExecutor {
    fn execute(&self, tool_name: &str, _inputs: &BTreeMap<String, String>) -> Result<String, String> {
        self.responses
            .get(tool_name)
            .cloned()
            .unwrap_or_else(|| Ok(format!("Mock output from {}", tool_name)))
    }
}

/// Plan executor
pub struct Executor {
    executor: Box<dyn ToolExecutor>,
}

impl Executor {
    /// Create a new executor with a tool executor
    pub fn new(executor: Box<dyn ToolExecutor>) -> Self {
        Self { executor }
    }

    /// Execute a plan
    pub async fn execute(&self, plan: &Plan) -> ExecutionResult {
        let mut result = ExecutionResult::new(plan.id.clone());
        let mut step_outputs: BTreeMap<u32, String> = BTreeMap::new();

        let steps = plan.ordered_steps();
        for step in steps {
            let start = Utc::now();

            // Build inputs from mappings
            let mut inputs = BTreeMap::new();
            for (param, source) in &step.input_mapping {
                if let Some(prev_output) = Self::extract_output(source, &step_outputs) {
                    inputs.insert(param.clone(), prev_output);
                } else if let Some(value) = plan.steps.iter().find(|s| s.id == *source) {
                    // Could be direct step reference
                    if let Some(output) = step_outputs.get(&value.order) {
                        inputs.insert(param.clone(), output.clone());
                    }
                }
            }

            let mut step_result = StepResult::new(
                step.id.clone(),
                step.order,
                step.tool.name.clone(),
            );

            match self.executor.execute(&step.tool.name, &inputs) {
                Ok(output) => {
                    step_outputs.insert(step.order, output.clone());
                    let elapsed_ms = Utc::now().signed_duration_since(start).num_milliseconds() as u128;
                    step_result = step_result.success(output, elapsed_ms);
                }
                Err(err) => {
                    let elapsed_ms = Utc::now().signed_duration_since(start).num_milliseconds() as u128;
                    match step.error_handling {
                        ErrorHandling::Abort => {
                            step_result = step_result.failure(err, elapsed_ms as u128);
                            result.add_step_result(step_result);
                            result.error_message = Some("Execution aborted due to step failure".to_string());
                            result.complete(false);
                            return result;
                        }
                        ErrorHandling::Continue => {
                            step_result = step_result.failure(err, elapsed_ms as u128);
                        }
                        ErrorHandling::Retry => {
                            step_result = step_result.failure(err, elapsed_ms as u128);
                        }
                    }
                }
            }

            result.add_step_result(step_result);
        }

        // Set final output from last step
        if let Some((_, output)) = step_outputs.iter().last() {
            result.final_output = Some(output.clone());
        }

        result.complete(result.status == ExecutionStatus::Success);
        result
    }

    /// Extract output from a source reference (e.g., "step_1.output")
    fn extract_output(source: &str, outputs: &BTreeMap<u32, String>) -> Option<String> {
        if source.starts_with("step_") {
            let step_num = source.split('_').nth(1)?
                .split('.')
                .next()?
                .parse::<u32>()
                .ok()?;
            outputs.get(&step_num).cloned()
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_step_result_creation() {
        let result = StepResult::new("step-1".to_string(), 1, "test-tool".to_string());
        assert_eq!(result.step_order, 1);
        assert_eq!(result.status, ExecutionStatus::Success);
    }

    #[test]
    fn test_execution_result() {
        let mut result = ExecutionResult::new("plan-1".to_string());
        let step = StepResult::new("step-1".to_string(), 1, "tool-1".to_string());
        result.add_step_result(step);

        assert_eq!(result.step_results.len(), 1);
    }

    #[test]
    fn test_mock_executor() {
        let mut executor = MockExecutor::new();
        executor = executor.set_response(
            "test".to_string(),
            Ok("output".to_string()),
        );

        let inputs = BTreeMap::new();
        let result = executor.execute("test", &inputs);
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_plan_execution() {
        let mock = Box::new(
            MockExecutor::new()
                .set_response("gen".to_string(), Ok("generated".to_string()))
        );
        let executor = Executor::new(mock);

        let plan = {
            use crate::planning::ExecutionStep;
            use crate::tool_discovery::{Tool, ToolCategory};

            let mut p = Plan::new("goal-1".to_string());
            let tool = Tool::new("gen".to_string(), "test".to_string(), ToolCategory::CodeGeneration);
            p.add_step(ExecutionStep::new(1, tool));
            p
        };

        let result = executor.execute(&plan).await;
        assert_eq!(result.status, ExecutionStatus::Success);
    }
}
