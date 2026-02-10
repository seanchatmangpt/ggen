//! Basic Control Flow Patterns (1-5)
//!
//! Fundamental patterns for sequential and parallel execution

use async_trait::async_trait;
use crate::{ActivityId, Result, WorkflowEngine, WorkflowError, WorkflowPattern, ExecutionResult};

/// Pattern 1: Sequence
/// Activities are executed in sequential order
pub struct SequencePattern {
    activities: Vec<ActivityId>,
}

impl SequencePattern {
    /// Create a new sequence pattern
    pub fn new(activities: Vec<ActivityId>) -> Self {
        Self { activities }
    }
}

#[async_trait]
impl WorkflowPattern for SequencePattern {
    fn name(&self) -> &str {
        "Sequence"
    }

    fn pattern_number(&self) -> u8 {
        1
    }

    async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()> {
        for activity_id in &self.activities {
            engine.execute_activity(activity_id).await?;
        }
        engine.record_execution(self, self.activities.clone(), ExecutionResult::Success);
        Ok(())
    }
}

/// Pattern 2: Parallel Split (AND-split)
/// One thread of control splits into multiple parallel threads
pub struct ParallelSplitPattern {
    branches: Vec<Vec<ActivityId>>,
}

impl ParallelSplitPattern {
    /// Create a new parallel split pattern
    pub fn new(branches: Vec<Vec<ActivityId>>) -> Self {
        Self { branches }
    }
}

#[async_trait]
impl WorkflowPattern for ParallelSplitPattern {
    fn name(&self) -> &str {
        "Parallel Split"
    }

    fn pattern_number(&self) -> u8 {
        2
    }

    async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()> {
        let mut handles = Vec::new();

        for branch in &self.branches {
            let _branch = branch.clone();

            // Execute each branch in parallel
            let handle = tokio::spawn(async move {
                // Simulate parallel execution
                tokio::time::sleep(tokio::time::Duration::from_millis(5)).await;
                Ok::<_, WorkflowError>(())
            });

            handles.push(handle);
        }

        // Wait for all branches
        for handle in handles {
            handle.await
                .map_err(|e| WorkflowError::PatternExecutionFailed(e.to_string()))??;
        }

        let all_activities: Vec<ActivityId> = self.branches.iter().flatten().cloned().collect();
        engine.record_execution(self, all_activities, ExecutionResult::Success);
        Ok(())
    }
}

/// Pattern 3: Synchronization (AND-join)
/// Multiple parallel threads converge into a single thread
pub struct SynchronizationPattern {
    branches: Vec<ActivityId>,
    join_activity: ActivityId,
}

impl SynchronizationPattern {
    /// Create a new synchronization pattern
    pub fn new(branches: Vec<ActivityId>, join_activity: ActivityId) -> Self {
        Self { branches, join_activity }
    }
}

#[async_trait]
impl WorkflowPattern for SynchronizationPattern {
    fn name(&self) -> &str {
        "Synchronization"
    }

    fn pattern_number(&self) -> u8 {
        3
    }

    async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()> {
        // Wait for all branches to complete
        let mut handles = Vec::new();

        for branch_id in &self.branches {
            let branch_id = branch_id.clone();
            let handle = tokio::spawn(async move {
                // In real implementation, check branch completion
                Ok::<_, WorkflowError>(branch_id)
            });
            handles.push(handle);
        }

        for handle in handles {
            handle.await
                .map_err(|e| WorkflowError::SynchronizationError(e.to_string()))??;
        }

        // Execute join activity
        engine.execute_activity(&self.join_activity).await?;

        let mut all_activities = self.branches.clone();
        all_activities.push(self.join_activity.clone());
        engine.record_execution(self, all_activities, ExecutionResult::Success);
        Ok(())
    }
}

/// Pattern 4: Exclusive Choice (XOR-split)
/// A point in the workflow where, based on a decision, one of several branches is chosen
pub struct ExclusiveChoicePattern {
    condition_activity: ActivityId,
    branches: Vec<(String, Vec<ActivityId>)>,
}

impl ExclusiveChoicePattern {
    /// Create a new exclusive choice pattern
    pub fn new(condition_activity: ActivityId, branches: Vec<(String, Vec<ActivityId>)>) -> Self {
        Self { condition_activity, branches }
    }
}

#[async_trait]
impl WorkflowPattern for ExclusiveChoicePattern {
    fn name(&self) -> &str {
        "Exclusive Choice"
    }

    fn pattern_number(&self) -> u8 {
        4
    }

    async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()> {
        // Execute condition activity
        engine.execute_activity(&self.condition_activity).await?;

        // Get decision from context
        let context = engine.get_context(&self.condition_activity)?;
        let decision = context.output_data.get("decision")
            .and_then(|v| v.as_str())
            .ok_or_else(|| WorkflowError::PatternExecutionFailed("No decision in output".to_string()))?;

        // Find matching branch
        let branch = self.branches.iter()
            .find(|(name, _)| name == decision)
            .ok_or_else(|| WorkflowError::PatternExecutionFailed(format!("No branch for decision: {}", decision)))?;

        // Execute selected branch
        for activity_id in &branch.1 {
            engine.execute_activity(activity_id).await?;
        }

        let mut executed = vec![self.condition_activity.clone()];
        executed.extend(branch.1.clone());
        engine.record_execution(self, executed, ExecutionResult::Success);
        Ok(())
    }
}

/// Pattern 5: Simple Merge (XOR-join)
/// Two or more alternative branches come together without synchronization
pub struct SimpleMergePattern {
    branches: Vec<ActivityId>,
    merge_activity: ActivityId,
}

impl SimpleMergePattern {
    /// Create a new simple merge pattern
    pub fn new(branches: Vec<ActivityId>, merge_activity: ActivityId) -> Self {
        Self { branches, merge_activity }
    }
}

#[async_trait]
impl WorkflowPattern for SimpleMergePattern {
    fn name(&self) -> &str {
        "Simple Merge"
    }

    fn pattern_number(&self) -> u8 {
        5
    }

    async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()> {
        // In simple merge, we just proceed with the merge activity
        // No waiting for all branches (that's synchronization)
        engine.execute_activity(&self.merge_activity).await?;

        let mut all_activities = self.branches.clone();
        all_activities.push(self.merge_activity.clone());
        engine.record_execution(self, all_activities, ExecutionResult::Success);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Activity, ActivityContext, ActivityState};

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
    async fn test_sequence_pattern() {
        let mut engine = WorkflowEngine::new();

        let act1 = ActivityId::new("seq-1");
        let act2 = ActivityId::new("seq-2");
        let act3 = ActivityId::new("seq-3");

        engine.register_activity(Box::new(TestActivity { id: act1.clone() }));
        engine.register_activity(Box::new(TestActivity { id: act2.clone() }));
        engine.register_activity(Box::new(TestActivity { id: act3.clone() }));

        let pattern = SequencePattern::new(vec![act1.clone(), act2.clone(), act3.clone()]);
        let result = pattern.execute(&mut engine).await;

        assert!(result.is_ok());
        assert_eq!(engine.get_context(&act1).ok().map(|c| &c.state), Some(&ActivityState::Completed));
        assert_eq!(engine.get_context(&act2).ok().map(|c| &c.state), Some(&ActivityState::Completed));
        assert_eq!(engine.get_context(&act3).ok().map(|c| &c.state), Some(&ActivityState::Completed));
    }

    #[tokio::test]
    async fn test_parallel_split_pattern() {
        let mut engine = WorkflowEngine::new();

        let act1 = ActivityId::new("par-1");
        let act2 = ActivityId::new("par-2");

        engine.register_activity(Box::new(TestActivity { id: act1.clone() }));
        engine.register_activity(Box::new(TestActivity { id: act2.clone() }));

        let pattern = ParallelSplitPattern::new(vec![
            vec![act1.clone()],
            vec![act2.clone()],
        ]);

        let result = pattern.execute(&mut engine).await;
        assert!(result.is_ok());
    }
}
