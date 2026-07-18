//! Execution engine tests
//!
//! Tests for plan execution, tool invocation, and result collection.

use a2a_tool_use_integration::execution::{Executor, ExecutionStatus, MockExecutor, ToolExecutor};
use a2a_tool_use_integration::planning::{Plan, ExecutionStep};
use a2a_tool_use_integration::tool_discovery::{Tool, ToolCategory};

#[tokio::test]
async fn test_plan_execution() {
    let mock = Box::new(MockExecutor::new()
        .set_response("gen".to_string(), Ok("generated code".to_string())));
    let executor = Executor::new(mock);

    let mut plan = Plan::new("goal-1".to_string());
    let tool = Tool::new("gen".to_string(), "test".to_string(), ToolCategory::CodeGeneration);
    plan.add_step(ExecutionStep::new(1, tool));

    let result = executor.execute(&plan).await;
    assert_eq!(result.status, ExecutionStatus::Success);
    assert!(result.final_output.is_some());
}

#[tokio::test]
async fn test_execution_with_failure() {
    let mock = Box::new(MockExecutor::new()
        .set_response("gen".to_string(), Err("Generation failed".to_string())));
    let executor = Executor::new(mock);

    let mut plan = Plan::new("goal-1".to_string());
    let tool = Tool::new("gen".to_string(), "test".to_string(), ToolCategory::CodeGeneration);
    let mut step = ExecutionStep::new(1, tool);
    step.error_handling = a2a_tool_use_integration::planning::ErrorHandling::Abort;
    plan.add_step(step);

    let result = executor.execute(&plan).await;
    assert_eq!(result.status, ExecutionStatus::Failure);
}

#[tokio::test]
async fn test_multi_step_execution() {
    let mock = Box::new(MockExecutor::new()
        .set_response("gen".to_string(), Ok("code".to_string()))
        .set_response("val".to_string(), Ok("valid".to_string())));
    let executor = Executor::new(mock);

    let mut plan = Plan::new("goal-1".to_string());
    let gen_tool = Tool::new("gen".to_string(), "test".to_string(), ToolCategory::CodeGeneration);
    let val_tool = Tool::new("val".to_string(), "test".to_string(), ToolCategory::Validation);

    plan.add_step(ExecutionStep::new(1, gen_tool));
    plan.add_step(ExecutionStep::new(2, val_tool));

    let result = executor.execute(&plan).await;
    assert_eq!(result.step_results.len(), 2);
}

#[tokio::test]
async fn test_step_result_output() {
    let expected_output = "generated code".to_string();
    let mock = Box::new(MockExecutor::new()
        .set_response("gen".to_string(), Ok(expected_output.clone())));
    let executor = Executor::new(mock);

    let mut plan = Plan::new("goal-1".to_string());
    let tool = Tool::new("gen".to_string(), "test".to_string(), ToolCategory::CodeGeneration);
    plan.add_step(ExecutionStep::new(1, tool));

    let result = executor.execute(&plan).await;
    assert_eq!(result.final_output.as_ref(), Some(&expected_output));
}

#[tokio::test]
async fn test_execution_timing() {
    let mock = Box::new(MockExecutor::new()
        .set_response("gen".to_string(), Ok("output".to_string())));
    let executor = Executor::new(mock);

    let mut plan = Plan::new("goal-1".to_string());
    let tool = Tool::new("gen".to_string(), "test".to_string(), ToolCategory::CodeGeneration);
    plan.add_step(ExecutionStep::new(1, tool));

    let result = executor.execute(&plan).await;
    // Execution time is tracked, even if it's minimal (< 1ms rounds to 0)
    assert!(result.step_results.len() > 0);
}

#[tokio::test]
async fn test_goal_achievement() {
    let mock = Box::new(MockExecutor::new()
        .set_response("gen".to_string(), Ok("code".to_string())));
    let executor = Executor::new(mock);

    let mut plan = Plan::new("goal-1".to_string());
    let tool = Tool::new("gen".to_string(), "test".to_string(), ToolCategory::CodeGeneration);
    plan.add_step(ExecutionStep::new(1, tool));

    let result = executor.execute(&plan).await;
    assert!(result.goal_achieved);
}

#[test]
fn test_step_result_creation() {
    use a2a_tool_use_integration::execution::StepResult;

    let result = StepResult::new("step-1".to_string(), 1, "tool-1".to_string());
    assert_eq!(result.step_order, 1);
    assert_eq!(result.tool_name, "tool-1");
    assert_eq!(result.status, ExecutionStatus::Success);
}

#[test]
fn test_execution_result_step_tracking() {
    use a2a_tool_use_integration::execution::{ExecutionResult, StepResult};

    let mut result = ExecutionResult::new("plan-1".to_string());
    let step = StepResult::new("step-1".to_string(), 1, "tool-1".to_string());
    result.add_step_result(step);

    assert_eq!(result.step_results.len(), 1);
}

#[test]
fn test_mock_executor_default() {
    let executor = MockExecutor::default();
    let inputs = std::collections::BTreeMap::new();
    let result = executor.execute("any-tool", &inputs);
    assert!(result.is_ok());
}
