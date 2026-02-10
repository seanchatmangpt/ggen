//! Structural Patterns (10-11)

use async_trait::async_trait;
use crate::{ActivityId, Result, WorkflowEngine, WorkflowPattern, ExecutionResult};

/// Pattern 10: Arbitrary Cycles
/// A point in a workflow where one or more activities can be repeated
pub struct ArbitraryCyclesPattern {
    loop_body: Vec<ActivityId>,
    condition_activity: ActivityId,
    max_iterations: usize,
}

impl ArbitraryCyclesPattern {
    /// Create a new arbitrary cycles pattern
    pub fn new(loop_body: Vec<ActivityId>, condition_activity: ActivityId, max_iterations: usize) -> Self {
        Self {
            loop_body,
            condition_activity,
            max_iterations,
        }
    }
}

#[async_trait]
impl WorkflowPattern for ArbitraryCyclesPattern {
    fn name(&self) -> &str {
        "Arbitrary Cycles"
    }

    fn pattern_number(&self) -> u8 {
        10
    }

    async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()> {
        let mut iterations = 0;
        let mut executed = Vec::new();

        loop {
            if iterations >= self.max_iterations {
                break;
            }

            // Execute condition
            engine.execute_activity(&self.condition_activity).await?;
            executed.push(self.condition_activity.clone());

            // Check condition result
            let context = engine.get_context(&self.condition_activity)?;
            let should_continue = context.output_data.get("continue")
                .and_then(|v| v.as_bool())
                .unwrap_or(false);

            if !should_continue {
                break;
            }

            // Execute loop body
            for activity_id in &self.loop_body {
                engine.execute_activity(activity_id).await?;
                executed.push(activity_id.clone());
            }

            iterations += 1;
        }

        engine.record_execution(self, executed, ExecutionResult::Success);
        Ok(())
    }
}

/// Pattern 11: Implicit Termination
/// A workflow instance terminates when there are no remaining activities to execute
pub struct ImplicitTerminationPattern {
    activities: Vec<ActivityId>,
}

impl ImplicitTerminationPattern {
    /// Create a new implicit termination pattern
    pub fn new(activities: Vec<ActivityId>) -> Self {
        Self { activities }
    }
}

#[async_trait]
impl WorkflowPattern for ImplicitTerminationPattern {
    fn name(&self) -> &str {
        "Implicit Termination"
    }

    fn pattern_number(&self) -> u8 {
        11
    }

    async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()> {
        // Execute all activities
        for activity_id in &self.activities {
            engine.execute_activity(activity_id).await?;
        }

        // Check if all activities are completed
        let all_completed = self.activities.iter().all(|id| {
            engine.get_context(id)
                .map(|ctx| ctx.state == crate::ActivityState::Completed)
                .unwrap_or(false)
        });

        if all_completed {
            engine.record_execution(self, self.activities.clone(), ExecutionResult::Success);
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Activity, ActivityContext};

    struct TestActivity {
        id: ActivityId,
    }

    #[async_trait]
    impl Activity for TestActivity {
        async fn execute(&self, context: &mut ActivityContext) -> Result<()> {
            context.set_output("result", serde_json::json!("success"));
            Ok(())
        }

        fn id(&self) -> &ActivityId {
            &self.id
        }
    }

    struct ConditionActivity {
        id: ActivityId,
        iterations: std::sync::Arc<std::sync::Mutex<usize>>,
        max_iterations: usize,
    }

    #[async_trait]
    impl Activity for ConditionActivity {
        async fn execute(&self, context: &mut ActivityContext) -> Result<()> {
            let mut count = self.iterations.lock().map_err(|e|
                crate::WorkflowError::PatternExecutionFailed(e.to_string()))?;
            *count += 1;

            let should_continue = *count < self.max_iterations;
            context.set_output("continue", serde_json::json!(should_continue));
            Ok(())
        }

        fn id(&self) -> &ActivityId {
            &self.id
        }
    }

    #[tokio::test]
    async fn test_arbitrary_cycles_pattern() {
        let mut engine = WorkflowEngine::new();

        let cond = ActivityId::new("cond");
        let body1 = ActivityId::new("body1");

        let iterations = std::sync::Arc::new(std::sync::Mutex::new(0));

        engine.register_activity(Box::new(ConditionActivity {
            id: cond.clone(),
            iterations: iterations.clone(),
            max_iterations: 3,
        }));
        engine.register_activity(Box::new(TestActivity { id: body1.clone() }));

        let pattern = ArbitraryCyclesPattern::new(
            vec![body1],
            cond,
            5,
        );

        let result = pattern.execute(&mut engine).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_implicit_termination_pattern() {
        let mut engine = WorkflowEngine::new();

        let act1 = ActivityId::new("act1");
        let act2 = ActivityId::new("act2");

        engine.register_activity(Box::new(TestActivity { id: act1.clone() }));
        engine.register_activity(Box::new(TestActivity { id: act2.clone() }));

        let pattern = ImplicitTerminationPattern::new(vec![act1, act2]);

        let result = pattern.execute(&mut engine).await;
        assert!(result.is_ok());
    }
}
