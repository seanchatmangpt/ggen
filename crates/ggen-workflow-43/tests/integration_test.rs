//! Integration tests for workflow patterns

use ggen_workflow_43::*;
use ggen_workflow_43::patterns::*;
use ggen_workflow_43::executor::*;
use async_trait::async_trait;

struct OrderProcessingActivity {
    id: ActivityId,
    step: String,
}

impl OrderProcessingActivity {
    fn new(id: impl Into<String>, step: impl Into<String>) -> Self {
        Self {
            id: ActivityId::new(id),
            step: step.into(),
        }
    }
}

#[async_trait]
impl Activity for OrderProcessingActivity {
    async fn execute(&self, context: &mut ActivityContext) -> Result<()> {
        context.set_output("step", serde_json::json!(self.step));
        context.set_output("completed", serde_json::json!(true));
        Ok(())
    }

    fn id(&self) -> &ActivityId {
        &self.id
    }
}

#[tokio::test]
async fn test_order_processing_workflow() {
    let mut engine = WorkflowEngine::new();

    // Register activities for order processing
    let receive_order = ActivityId::new("receive-order");
    let validate_order = ActivityId::new("validate-order");
    let check_inventory = ActivityId::new("check-inventory");
    let charge_payment = ActivityId::new("charge-payment");
    let ship_order = ActivityId::new("ship-order");
    let send_confirmation = ActivityId::new("send-confirmation");

    engine.register_activity(Box::new(OrderProcessingActivity::new("receive-order", "receive")));
    engine.register_activity(Box::new(OrderProcessingActivity::new("validate-order", "validate")));
    engine.register_activity(Box::new(OrderProcessingActivity::new("check-inventory", "inventory")));
    engine.register_activity(Box::new(OrderProcessingActivity::new("charge-payment", "payment")));
    engine.register_activity(Box::new(OrderProcessingActivity::new("ship-order", "ship")));
    engine.register_activity(Box::new(OrderProcessingActivity::new("send-confirmation", "confirm")));

    // Create workflow: sequence of steps with parallel inventory check and payment
    let sequence = SequencePattern::new(vec![
        receive_order.clone(),
        validate_order.clone(),
    ]);

    let parallel = ParallelSplitPattern::new(vec![
        vec![check_inventory.clone()],
        vec![charge_payment.clone()],
    ]);

    let sync = SynchronizationPattern::new(
        vec![check_inventory, charge_payment],
        ship_order.clone(),
    );

    let final_sequence = SequencePattern::new(vec![
        ship_order,
        send_confirmation.clone(),
    ]);

    // Execute workflow
    let result = sequence.execute(&mut engine).await;
    assert!(result.is_ok());

    let result = parallel.execute(&mut engine).await;
    assert!(result.is_ok());

    let result = sync.execute(&mut engine).await;
    assert!(result.is_ok());

    let result = final_sequence.execute(&mut engine).await;
    assert!(result.is_ok());

    // Verify all steps completed
    assert_eq!(
        engine.get_context(&send_confirmation).ok().map(|c| &c.state),
        Some(&ActivityState::Completed)
    );
}

#[tokio::test]
async fn test_pattern_executor_with_composite() {
    let mut engine = WorkflowEngine::new();

    let act1 = ActivityId::new("comp-1");
    let act2 = ActivityId::new("comp-2");
    let act3 = ActivityId::new("comp-3");

    engine.register_activity(Box::new(OrderProcessingActivity::new("comp-1", "step1")));
    engine.register_activity(Box::new(OrderProcessingActivity::new("comp-2", "step2")));
    engine.register_activity(Box::new(OrderProcessingActivity::new("comp-3", "step3")));

    let composite = CompositePattern::new("test-composite", ExecutionMode::Sequential)
        .add_pattern(Box::new(SequencePattern::new(vec![act1.clone(), act2.clone()])))
        .add_pattern(Box::new(SequencePattern::new(vec![act3.clone()])));

    let result = composite.execute(&mut engine).await;
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_pattern_builder_fluent_api() {
    let mut engine = WorkflowEngine::new();

    let act1 = ActivityId::new("fluent-1");
    let act2 = ActivityId::new("fluent-2");

    engine.register_activity(Box::new(OrderProcessingActivity::new("fluent-1", "step1")));
    engine.register_activity(Box::new(OrderProcessingActivity::new("fluent-2", "step2")));

    let builder = PatternBuilder::new(engine);

    let result = builder
        .with_pattern(Box::new(SequencePattern::new(vec![act1.clone()])))
        .await;

    assert!(result.is_ok());

    let builder = result.ok().unwrap();
    let result = builder
        .with_pattern(Box::new(SequencePattern::new(vec![act2])))
        .await;

    assert!(result.is_ok());
}

#[tokio::test]
async fn test_cancellation_workflow() {
    let mut engine = WorkflowEngine::new();

    let act1 = ActivityId::new("cancel-wf-1");
    let act2 = ActivityId::new("cancel-wf-2");
    let act3 = ActivityId::new("cancel-wf-3");

    engine.register_activity(Box::new(OrderProcessingActivity::new("cancel-wf-1", "step1")));
    engine.register_activity(Box::new(OrderProcessingActivity::new("cancel-wf-2", "step2")));
    engine.register_activity(Box::new(OrderProcessingActivity::new("cancel-wf-3", "step3")));

    // Start workflow
    let sequence = SequencePattern::new(vec![act1.clone()]);
    let result = sequence.execute(&mut engine).await;
    assert!(result.is_ok());

    // Cancel remaining activities
    let cancel_region = CancelRegionPattern::new(vec![act2.clone(), act3.clone()]);
    let result = cancel_region.execute(&mut engine).await;
    assert!(result.is_ok());

    // Verify cancellation
    assert_eq!(
        engine.get_context(&act2).ok().map(|c| &c.state),
        Some(&ActivityState::Cancelled)
    );
    assert_eq!(
        engine.get_context(&act3).ok().map(|c| &c.state),
        Some(&ActivityState::Cancelled)
    );
}

#[tokio::test]
async fn test_multi_instance_workflow() {
    let mut engine = WorkflowEngine::new();

    let template = ActivityId::new("mi-template-wf");
    engine.register_activity(Box::new(OrderProcessingActivity::new("mi-template-wf", "process")));

    let pattern = MultipleInstancesDesignTimePattern::new(template, 5);
    let result = pattern.execute(&mut engine).await;

    assert!(result.is_ok());
    assert!(!engine.execution_history.is_empty());
}

#[tokio::test]
async fn test_loop_pattern_workflow() {
    let mut engine = WorkflowEngine::new();

    let cond = ActivityId::new("loop-cond-wf");
    let body = ActivityId::new("loop-body-wf");

    engine.register_activity(Box::new(OrderProcessingActivity::new("loop-cond-wf", "condition")));
    engine.register_activity(Box::new(OrderProcessingActivity::new("loop-body-wf", "body")));

    // Set condition to stop after first iteration
    if let Ok(context) = engine.get_context_mut(&cond) {
        context.set_output("continue", serde_json::json!(false));
    }

    let pattern = StructuredLoopPattern::new(vec![body], cond, 10);
    let result = pattern.execute(&mut engine).await;

    assert!(result.is_ok());
}

#[tokio::test]
async fn test_milestone_based_workflow() {
    let mut engine = WorkflowEngine::new();

    let milestone = ActivityId::new("approval-milestone");
    let act1 = ActivityId::new("post-approval-1");
    let act2 = ActivityId::new("post-approval-2");

    engine.register_activity(Box::new(OrderProcessingActivity::new("approval-milestone", "approval")));
    engine.register_activity(Box::new(OrderProcessingActivity::new("post-approval-1", "action1")));
    engine.register_activity(Box::new(OrderProcessingActivity::new("post-approval-2", "action2")));

    // Set milestone reached
    if let Ok(context) = engine.get_context_mut(&milestone) {
        context.set_output("milestone_reached", serde_json::json!(true));
    }

    let pattern = MilestonePattern::new(milestone, vec![act1.clone(), act2.clone()]);
    let result = pattern.execute(&mut engine).await;

    assert!(result.is_ok());
    assert_eq!(
        engine.get_context(&act1).ok().map(|c| &c.state),
        Some(&ActivityState::Completed)
    );
}
