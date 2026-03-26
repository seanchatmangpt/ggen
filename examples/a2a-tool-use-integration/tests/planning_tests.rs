//! Planning engine tests
//!
//! Tests for plan generation, step ordering, and execution validation.

use a2a_tool_use_integration::goals::{Goal, GoalType};
use a2a_tool_use_integration::planning::{Plan, ExecutionStep, PlanGenerator, RetryPolicy, ErrorHandling};
use a2a_tool_use_integration::tool_discovery::{Tool, ToolCategory};

#[test]
fn test_plan_creation() {
    let plan = Plan::new("goal-1".to_string());
    assert_eq!(plan.goal_id, "goal-1");
    assert!(plan.steps.is_empty());
}

#[test]
fn test_plan_step_addition() {
    let mut plan = Plan::new("goal-1".to_string());
    let tool = Tool::new("test".to_string(), "test".to_string(), ToolCategory::CodeGeneration);
    plan.add_step(ExecutionStep::new(1, tool));

    assert_eq!(plan.steps.len(), 1);
}

#[test]
fn test_plan_validation() {
    let mut plan = Plan::new("goal-1".to_string());
    assert!(plan.validate().is_err());

    let tool = Tool::new("test".to_string(), "test".to_string(), ToolCategory::CodeGeneration);
    plan.add_step(ExecutionStep::new(1, tool));
    assert!(plan.validate().is_ok());
}

#[test]
fn test_plan_ordered_steps() {
    let mut plan = Plan::new("goal-1".to_string());
    let tool = Tool::new("test".to_string(), "test".to_string(), ToolCategory::CodeGeneration);

    plan.add_step(ExecutionStep::new(2, tool.clone()));
    plan.add_step(ExecutionStep::new(1, tool.clone()));
    plan.add_step(ExecutionStep::new(3, tool));

    let ordered = plan.ordered_steps();
    assert_eq!(ordered[0].order, 1);
    assert_eq!(ordered[1].order, 2);
    assert_eq!(ordered[2].order, 3);
}

#[test]
fn test_execution_step_builder() {
    let tool = Tool::new("gen".to_string(), "test".to_string(), ToolCategory::CodeGeneration);
    let step = ExecutionStep::new(1, tool)
        .with_input_mapping("description".to_string(), "goal.description".to_string())
        .with_output_binding("generated_code".to_string())
        .with_timeout_ms(5000)
        .with_error_handling(ErrorHandling::Retry);

    assert_eq!(step.input_mapping.len(), 1);
    assert_eq!(step.output_binding, "generated_code");
    assert_eq!(step.timeout_ms, Some(5000));
    assert_eq!(step.error_handling, ErrorHandling::Retry);
}

#[test]
fn test_retry_policy() {
    let policy = RetryPolicy::new(3);
    assert_eq!(policy.max_retries, 3);
    assert_eq!(policy.backoff_for_attempt(0), 100);
    assert_eq!(policy.backoff_for_attempt(1), 200);
    assert_eq!(policy.backoff_for_attempt(2), 400);
    assert_eq!(policy.backoff_for_attempt(3), 800);
}

#[test]
fn test_plan_estimated_time() {
    let mut plan = Plan::new("goal-1".to_string());
    let tool = Tool::new("test".to_string(), "test".to_string(), ToolCategory::CodeGeneration);

    let mut step1 = ExecutionStep::new(1, tool.clone());
    step1.timeout_ms = Some(1000);
    plan.add_step(step1);

    let mut step2 = ExecutionStep::new(2, tool);
    step2.timeout_ms = Some(2000);
    plan.add_step(step2);

    assert_eq!(plan.estimated_time_ms, 3000);
}

#[test]
fn test_plan_step_dependencies() {
    let tool = Tool::new("test".to_string(), "test".to_string(), ToolCategory::CodeGeneration);
    let step1 = ExecutionStep::new(1, tool.clone());
    let step2_id = step1.id.clone();

    let step2 = ExecutionStep::new(2, tool)
        .depends_on(step2_id);

    assert_eq!(step2.depends_on.len(), 1);
}

#[test]
fn test_plan_generator_code_goal() {
    let goal = Goal::new(GoalType::GenerateCode, "Generate Rust module".to_string());
    let tools = vec![
        Tool::new("code-generate".to_string(), "gen".to_string(), ToolCategory::CodeGeneration),
        Tool::new("code-validate".to_string(), "val".to_string(), ToolCategory::Validation),
        Tool::new("code-format".to_string(), "fmt".to_string(), ToolCategory::Transformation),
    ];

    let plan = PlanGenerator::generate_plan(&goal, &tools);
    assert!(plan.is_ok());

    let plan = plan.unwrap();
    assert!(!plan.steps.is_empty());
    assert!(plan.validate().is_ok());
}

#[test]
fn test_plan_generator_data_goal() {
    let goal = Goal::new(GoalType::AnalyzeData, "Analyze CSV data".to_string());
    let tools = vec![
        Tool::new("data-analyzer".to_string(), "test".to_string(), ToolCategory::Analysis),
    ];

    let plan = PlanGenerator::generate_plan(&goal, &tools);
    assert!(plan.is_ok());
}

#[test]
fn test_plan_generator_missing_tools() {
    let goal = Goal::new(GoalType::GenerateCode, "Generate code".to_string());
    let tools = vec![
        Tool::new("wrong-tool".to_string(), "test".to_string(), ToolCategory::Analysis),
    ];

    let plan = PlanGenerator::generate_plan(&goal, &tools);
    assert!(plan.is_err());
}

#[test]
fn test_error_handling_enum() {
    let step = ExecutionStep::new(
        1,
        Tool::new("test".to_string(), "test".to_string(), ToolCategory::CodeGeneration)
    );

    assert_eq!(step.error_handling, ErrorHandling::Retry);
}
