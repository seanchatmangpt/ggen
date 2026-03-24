//! Iteration and Advanced Control Flow Patterns (21-43)

use async_trait::async_trait;
use crate::{ActivityId, Result, WorkflowEngine, WorkflowPattern, ExecutionResult, WorkflowError};
use std::sync::Arc;
use tokio::sync::Semaphore;

/// Pattern 21: Structured Loop
/// A structured loop with entry and exit conditions
pub struct StructuredLoopPattern {
    loop_body: Vec<ActivityId>,
    condition_activity: ActivityId,
    max_iterations: usize,
}

impl StructuredLoopPattern {
    /// Create a new structured loop pattern
    pub fn new(loop_body: Vec<ActivityId>, condition_activity: ActivityId, max_iterations: usize) -> Self {
        Self {
            loop_body,
            condition_activity,
            max_iterations,
        }
    }
}

#[async_trait]
impl WorkflowPattern for StructuredLoopPattern {
    fn name(&self) -> &str {
        "Structured Loop"
    }

    fn pattern_number(&self) -> u8 {
        21
    }

    async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()> {
        let mut iterations = 0;
        let mut executed = Vec::new();

        while iterations < self.max_iterations {
            engine.execute_activity(&self.condition_activity).await?;
            executed.push(self.condition_activity.clone());

            let context = engine.get_context(&self.condition_activity)?;
            let should_continue = context.output_data.get("continue")
                .and_then(|v| v.as_bool())
                .unwrap_or(false);

            if !should_continue {
                break;
            }

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

/// Pattern 22: Recursion
/// An activity can invoke itself recursively
pub struct RecursionPattern {
    recursive_activity: ActivityId,
    max_depth: usize,
}

impl RecursionPattern {
    /// Create a new recursion pattern
    pub fn new(recursive_activity: ActivityId, max_depth: usize) -> Self {
        Self {
            recursive_activity,
            max_depth,
        }
    }
}

#[async_trait]
impl WorkflowPattern for RecursionPattern {
    fn name(&self) -> &str {
        "Recursion"
    }

    fn pattern_number(&self) -> u8 {
        22
    }

    async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()> {
        self.recursive_execute(engine, 0).await?;
        engine.record_execution(self, vec![self.recursive_activity.clone()], ExecutionResult::Success);
        Ok(())
    }
}

impl RecursionPattern {
    fn recursive_execute<'a>(&'a self, engine: &'a mut WorkflowEngine, depth: usize) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<()>> + Send + 'a>> {
        Box::pin(async move {
            if depth >= self.max_depth {
                return Ok(());
            }

            engine.execute_activity(&self.recursive_activity).await?;

            let context = engine.get_context(&self.recursive_activity)?;
            let should_recurse = context.output_data.get("recurse")
                .and_then(|v| v.as_bool())
                .unwrap_or(false);

            if should_recurse {
                self.recursive_execute(engine, depth + 1).await?;
            }

            Ok(())
        })
    }
}

/// Pattern 28: Blocking Discriminator
/// Like discriminator but waits for all branches before resetting
pub struct BlockingDiscriminatorPattern {
    branches: Vec<ActivityId>,
    discriminator_activity: ActivityId,
}

impl BlockingDiscriminatorPattern {
    /// Create a new blocking discriminator pattern
    pub fn new(branches: Vec<ActivityId>, discriminator_activity: ActivityId) -> Self {
        Self {
            branches,
            discriminator_activity,
        }
    }
}

#[async_trait]
impl WorkflowPattern for BlockingDiscriminatorPattern {
    fn name(&self) -> &str {
        "Blocking Discriminator"
    }

    fn pattern_number(&self) -> u8 {
        28
    }

    async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()> {
        let semaphore = Arc::new(Semaphore::new(1));
        let mut handles: Vec<tokio::task::JoinHandle<Result<Option<ActivityId>>>> = Vec::new();

        for branch_id in &self.branches {
            let sem = Arc::clone(&semaphore);
            let branch_id_clone = branch_id.clone();

            let handle = tokio::spawn(async move {
                tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;

                if sem.try_acquire().is_ok() {
                    Ok(Some(branch_id_clone))
                } else {
                    Ok(None)
                }
            });

            handles.push(handle);
        }

        // Execute discriminator on first completion
        let mut first_executed = false;
        for handle in handles {
            if let Ok(Ok(Some(_branch_id))) = handle.await {
                if !first_executed {
                    engine.execute_activity(&self.discriminator_activity).await?;
                    first_executed = true;
                }
            }
        }

        let mut all_activities = self.branches.clone();
        all_activities.push(self.discriminator_activity.clone());
        engine.record_execution(self, all_activities, ExecutionResult::Success);
        Ok(())
    }
}

/// Pattern 29: Cancelling Discriminator
/// Discriminator that cancels remaining branches after first completion
pub struct CancellingDiscriminatorPattern {
    branches: Vec<ActivityId>,
    discriminator_activity: ActivityId,
}

impl CancellingDiscriminatorPattern {
    /// Create a new cancelling discriminator pattern
    pub fn new(branches: Vec<ActivityId>, discriminator_activity: ActivityId) -> Self {
        Self {
            branches,
            discriminator_activity,
        }
    }
}

#[async_trait]
impl WorkflowPattern for CancellingDiscriminatorPattern {
    fn name(&self) -> &str {
        "Cancelling Discriminator"
    }

    fn pattern_number(&self) -> u8 {
        29
    }

    fn supports_cancellation(&self) -> bool {
        true
    }

    async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()> {
        use tokio::sync::Mutex;

        let first_completed = Arc::new(Mutex::new(false));
        let mut handles: Vec<tokio::task::JoinHandle<Result<Option<ActivityId>>>> = Vec::new();

        for branch_id in &self.branches {
            let completed = Arc::clone(&first_completed);
            let branch_id_clone = branch_id.clone();

            let handle = tokio::spawn(async move {
                tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;

                let mut completed_guard = completed.lock().await;
                if !*completed_guard {
                    *completed_guard = true;
                    Ok(Some(branch_id_clone))
                } else {
                    Ok(None)
                }
            });

            handles.push(handle);
        }

        // Wait for first and cancel others
        for handle in handles {
            if let Ok(Ok(Some(_branch_id))) = handle.await {
                engine.execute_activity(&self.discriminator_activity).await?;
                break;
            }
        }

        // Cancel remaining branches
        for branch_id in &self.branches {
            let should_cancel = engine.get_context(branch_id)
                .map(|ctx| ctx.state != crate::ActivityState::Completed)
                .unwrap_or(false);

            if should_cancel && engine.activities.contains_key(branch_id) {
                let activity = engine.activities.remove(branch_id);

                if let Some(ref act) = activity {
                    if let Ok(ctx) = engine.get_context_mut(branch_id) {
                        let _cancel_result = act.cancel(ctx).await;
                    }
                }

                if let Some(act) = activity {
                    engine.activities.insert(branch_id.clone(), act);
                }
            }
        }

        let mut all_activities = self.branches.clone();
        all_activities.push(self.discriminator_activity.clone());
        engine.record_execution(self, all_activities, ExecutionResult::Success);
        Ok(())
    }
}

/// Pattern 30: Structured Partial Join
/// N-out-of-M join with structured design
pub struct StructuredPartialJoinPattern {
    branches: Vec<ActivityId>,
    join_activity: ActivityId,
    threshold: usize,
}

impl StructuredPartialJoinPattern {
    /// Create a new structured partial join pattern
    pub fn new(branches: Vec<ActivityId>, join_activity: ActivityId, threshold: usize) -> Self {
        Self {
            branches,
            join_activity,
            threshold,
        }
    }
}

#[async_trait]
impl WorkflowPattern for StructuredPartialJoinPattern {
    fn name(&self) -> &str {
        "Structured Partial Join"
    }

    fn pattern_number(&self) -> u8 {
        30
    }

    async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()> {
        use tokio::sync::Mutex;

        let completed_count = Arc::new(Mutex::new(0usize));
        let mut handles: Vec<tokio::task::JoinHandle<Result<bool>>> = Vec::new();

        for branch_id in &self.branches {
            let count = Arc::clone(&completed_count);
            let _branch_id = branch_id.clone();
            let threshold = self.threshold;

            let handle = tokio::spawn(async move {
                tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;

                let mut count_guard = count.lock().await;
                *count_guard += 1;

                if *count_guard >= threshold {
                    Ok(true)
                } else {
                    Ok(false)
                }
            });

            handles.push(handle);
        }

        // Wait for threshold
        for handle in handles {
            if let Ok(Ok(true)) = handle.await {
                engine.execute_activity(&self.join_activity).await?;
                break;
            }
        }

        let mut all_activities = self.branches.clone();
        all_activities.push(self.join_activity.clone());
        engine.record_execution(self, all_activities, ExecutionResult::Success);
        Ok(())
    }
}

/// Pattern 31: Blocking Partial Join
/// N-out-of-M join that blocks until threshold is met
pub struct BlockingPartialJoinPattern {
    branches: Vec<ActivityId>,
    join_activity: ActivityId,
    threshold: usize,
}

impl BlockingPartialJoinPattern {
    /// Create a new blocking partial join pattern
    pub fn new(branches: Vec<ActivityId>, join_activity: ActivityId, threshold: usize) -> Self {
        Self {
            branches,
            join_activity,
            threshold,
        }
    }
}

#[async_trait]
impl WorkflowPattern for BlockingPartialJoinPattern {
    fn name(&self) -> &str {
        "Blocking Partial Join"
    }

    fn pattern_number(&self) -> u8 {
        31
    }

    async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()> {
        use tokio::sync::Mutex;

        let completed_count = Arc::new(Mutex::new(0usize));
        let mut handles: Vec<tokio::task::JoinHandle<Result<ActivityId>>> = Vec::new();

        for branch_id in &self.branches {
            let count = Arc::clone(&completed_count);
            let branch_id_clone = branch_id.clone();

            let handle = tokio::spawn(async move {
                tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;

                let mut count_guard = count.lock().await;
                *count_guard += 1;
                drop(count_guard);

                Ok::<_, WorkflowError>(branch_id_clone)
            });

            handles.push(handle);
        }

        // Wait for all to complete
        for handle in handles {
            handle.await
                .map_err(|e| WorkflowError::PatternExecutionFailed(e.to_string()))??;
        }

        // Check threshold
        let final_count = *completed_count.lock().await;
        if final_count >= self.threshold {
            engine.execute_activity(&self.join_activity).await?;
        }

        let mut all_activities = self.branches.clone();
        all_activities.push(self.join_activity.clone());
        engine.record_execution(self, all_activities, ExecutionResult::Success);
        Ok(())
    }
}

/// Pattern 32: Cancelling Partial Join
/// N-out-of-M join that cancels remaining branches after threshold
pub struct CancellingPartialJoinPattern {
    branches: Vec<ActivityId>,
    join_activity: ActivityId,
    threshold: usize,
}

impl CancellingPartialJoinPattern {
    /// Create a new cancelling partial join pattern
    pub fn new(branches: Vec<ActivityId>, join_activity: ActivityId, threshold: usize) -> Self {
        Self {
            branches,
            join_activity,
            threshold,
        }
    }
}

#[async_trait]
impl WorkflowPattern for CancellingPartialJoinPattern {
    fn name(&self) -> &str {
        "Cancelling Partial Join"
    }

    fn pattern_number(&self) -> u8 {
        32
    }

    fn supports_cancellation(&self) -> bool {
        true
    }

    async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()> {
        use tokio::sync::Mutex;

        let completed_count = Arc::new(Mutex::new(0usize));
        let threshold_reached = Arc::new(Mutex::new(false));
        let mut handles = Vec::new();

        for branch_id in &self.branches {
            let count = Arc::clone(&completed_count);
            let reached = Arc::clone(&threshold_reached);
            let branch_id_clone = branch_id.clone();
            let threshold = self.threshold;

            let handle = tokio::spawn(async move {
                tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;

                let mut count_guard = count.lock().await;
                *count_guard += 1;

                if *count_guard >= threshold {
                    *reached.lock().await = true;
                }
                drop(count_guard);

                Ok::<_, WorkflowError>(branch_id_clone)
            });

            handles.push(handle);
        }

        // Wait for threshold
        while !*threshold_reached.lock().await {
            tokio::time::sleep(tokio::time::Duration::from_millis(5)).await;
        }

        engine.execute_activity(&self.join_activity).await?;

        // Cancel remaining branches
        for branch_id in &self.branches {
            let should_cancel = engine.get_context(branch_id)
                .map(|ctx| ctx.state != crate::ActivityState::Completed)
                .unwrap_or(false);

            if should_cancel && engine.activities.contains_key(branch_id) {
                let activity = engine.activities.remove(branch_id);

                if let Some(ref act) = activity {
                    if let Ok(ctx) = engine.get_context_mut(branch_id) {
                        let _cancel_result = act.cancel(ctx).await;
                    }
                }

                if let Some(act) = activity {
                    engine.activities.insert(branch_id.clone(), act);
                }
            }
        }

        let mut all_activities = self.branches.clone();
        all_activities.push(self.join_activity.clone());
        engine.record_execution(self, all_activities, ExecutionResult::Success);
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

    #[tokio::test]
    async fn test_structured_loop_pattern() {
        let mut engine = WorkflowEngine::new();

        let cond = ActivityId::new("cond");
        let body = ActivityId::new("body");

        engine.register_activity(Box::new(TestActivity { id: cond.clone() }));
        engine.register_activity(Box::new(TestActivity { id: body.clone() }));

        if let Ok(context) = engine.get_context_mut(&cond) {
            context.set_output("continue", serde_json::json!(false));
        }

        let pattern = StructuredLoopPattern::new(vec![body], cond, 5);
        let result = pattern.execute(&mut engine).await;

        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_blocking_discriminator_pattern() {
        let mut engine = WorkflowEngine::new();

        let branch1 = ActivityId::new("branch1");
        let branch2 = ActivityId::new("branch2");
        let disc = ActivityId::new("disc");

        engine.register_activity(Box::new(TestActivity { id: branch1.clone() }));
        engine.register_activity(Box::new(TestActivity { id: branch2.clone() }));
        engine.register_activity(Box::new(TestActivity { id: disc.clone() }));

        let pattern = BlockingDiscriminatorPattern::new(vec![branch1, branch2], disc);
        let result = pattern.execute(&mut engine).await;

        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_structured_partial_join_pattern() {
        let mut engine = WorkflowEngine::new();

        let branch1 = ActivityId::new("branch1");
        let branch2 = ActivityId::new("branch2");
        let branch3 = ActivityId::new("branch3");
        let join = ActivityId::new("join");

        engine.register_activity(Box::new(TestActivity { id: branch1.clone() }));
        engine.register_activity(Box::new(TestActivity { id: branch2.clone() }));
        engine.register_activity(Box::new(TestActivity { id: branch3.clone() }));
        engine.register_activity(Box::new(TestActivity { id: join.clone() }));

        let pattern = StructuredPartialJoinPattern::new(
            vec![branch1, branch2, branch3],
            join,
            2,
        );

        let result = pattern.execute(&mut engine).await;
        assert!(result.is_ok());
    }
}
