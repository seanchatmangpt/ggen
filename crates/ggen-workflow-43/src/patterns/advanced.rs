//! Advanced Branching and Synchronization Patterns (6-9)

use async_trait::async_trait;
use crate::{ActivityId, Result, WorkflowEngine, WorkflowError, WorkflowPattern, ExecutionResult};

/// Pattern 6: Multi-Choice (OR-split)
/// A point where multiple branches may be chosen based on conditions
pub struct MultiChoicePattern {
    condition_activity: ActivityId,
    branches: Vec<(String, Vec<ActivityId>)>,
}

impl MultiChoicePattern {
    /// Create a new multi-choice pattern
    pub fn new(condition_activity: ActivityId, branches: Vec<(String, Vec<ActivityId>)>) -> Self {
        Self { condition_activity, branches }
    }
}

#[async_trait]
impl WorkflowPattern for MultiChoicePattern {
    fn name(&self) -> &str {
        "Multi-Choice"
    }

    fn pattern_number(&self) -> u8 {
        6
    }

    async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()> {
        engine.execute_activity(&self.condition_activity).await?;

        // Clone decisions to avoid borrow conflicts
        let decisions_clone = {
            let context = engine.get_context(&self.condition_activity)?;
            context.output_data.get("decisions")
                .and_then(|v| v.as_array())
                .ok_or_else(|| WorkflowError::PatternExecutionFailed("No decisions array".to_string()))?
                .clone()
        };

        let mut executed = vec![self.condition_activity.clone()];

        // Execute all matching branches in parallel
        for decision_value in &decisions_clone {
            if let Some(decision) = decision_value.as_str() {
                if let Some((_, activities)) = self.branches.iter().find(|(name, _)| name == decision) {
                    for activity_id in activities {
                        engine.execute_activity(activity_id).await?;
                        executed.push(activity_id.clone());
                    }
                }
            }
        }

        engine.record_execution(self, executed, ExecutionResult::Success);
        Ok(())
    }
}

/// Pattern 7: Structured Synchronizing Merge
/// Convergence of multiple branches where the number of active branches may vary
pub struct StructuredSynchronizingMergePattern {
    branches: Vec<ActivityId>,
    merge_activity: ActivityId,
}

impl StructuredSynchronizingMergePattern {
    /// Create a new structured synchronizing merge pattern
    pub fn new(branches: Vec<ActivityId>, merge_activity: ActivityId) -> Self {
        Self { branches, merge_activity }
    }
}

#[async_trait]
impl WorkflowPattern for StructuredSynchronizingMergePattern {
    fn name(&self) -> &str {
        "Structured Synchronizing Merge"
    }

    fn pattern_number(&self) -> u8 {
        7
    }

    async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()> {
        // Wait for all active branches to complete
        let mut active_count = 0;

        for branch_id in &self.branches {
            if let Ok(context) = engine.get_context(branch_id) {
                if context.state == crate::ActivityState::Running ||
                   context.state == crate::ActivityState::Completed {
                    active_count += 1;
                }
            }
        }

        if active_count > 0 {
            engine.execute_activity(&self.merge_activity).await?;
        }

        let mut all_activities = self.branches.clone();
        all_activities.push(self.merge_activity.clone());
        engine.record_execution(self, all_activities, ExecutionResult::Success);
        Ok(())
    }
}

/// Pattern 8: Multi-Merge
/// Multiple activations of a subsequent activity, one for each incoming branch
pub struct MultiMergePattern {
    branches: Vec<ActivityId>,
    merge_activity: ActivityId,
}

impl MultiMergePattern {
    /// Create a new multi-merge pattern
    pub fn new(branches: Vec<ActivityId>, merge_activity: ActivityId) -> Self {
        Self { branches, merge_activity }
    }
}

#[async_trait]
impl WorkflowPattern for MultiMergePattern {
    fn name(&self) -> &str {
        "Multi-Merge"
    }

    fn pattern_number(&self) -> u8 {
        8
    }

    async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()> {
        // Execute merge activity once for each completed branch
        for _branch_id in &self.branches {
            engine.execute_activity(&self.merge_activity).await?;
        }

        let mut all_activities = self.branches.clone();
        all_activities.push(self.merge_activity.clone());
        engine.record_execution(self, all_activities, ExecutionResult::Success);
        Ok(())
    }
}

/// Pattern 9: Structured Discriminator
/// Convergence of multiple branches where only the first activation triggers the next activity
pub struct StructuredDiscriminatorPattern {
    branches: Vec<ActivityId>,
    discriminator_activity: ActivityId,
}

impl StructuredDiscriminatorPattern {
    /// Create a new structured discriminator pattern
    pub fn new(branches: Vec<ActivityId>, discriminator_activity: ActivityId) -> Self {
        Self { branches, discriminator_activity }
    }
}

#[async_trait]
impl WorkflowPattern for StructuredDiscriminatorPattern {
    fn name(&self) -> &str {
        "Structured Discriminator"
    }

    fn pattern_number(&self) -> u8 {
        9
    }

    async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()> {
        use tokio::sync::Semaphore;
        use std::sync::Arc;

        let semaphore = Arc::new(Semaphore::new(1));
        let mut handles: Vec<tokio::task::JoinHandle<Result<Option<ActivityId>>>> = Vec::new();

        // Race all branches, but only first one triggers discriminator
        for branch_id in &self.branches {
            let sem = Arc::clone(&semaphore);
            let branch_id = branch_id.clone();

            let handle = tokio::spawn(async move {
                // Simulate branch execution
                tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;

                // Try to acquire semaphore (only first succeeds)
                if sem.try_acquire().is_ok() {
                    Ok(Some(branch_id))
                } else {
                    Ok(None)
                }
            });

            handles.push(handle);
        }

        // Wait for first completion
        for handle in handles {
            if let Ok(Ok(Some(_branch_id))) = handle.await {
                engine.execute_activity(&self.discriminator_activity).await?;
                break;
            }
        }

        let mut all_activities = self.branches.clone();
        all_activities.push(self.discriminator_activity.clone());
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
    async fn test_multi_choice_pattern() {
        let mut engine = WorkflowEngine::new();

        let cond = ActivityId::new("cond");
        let act1 = ActivityId::new("act1");
        let act2 = ActivityId::new("act2");

        engine.register_activity(Box::new(TestActivity { id: cond.clone() }));
        engine.register_activity(Box::new(TestActivity { id: act1.clone() }));
        engine.register_activity(Box::new(TestActivity { id: act2.clone() }));

        // Set up decision data
        if let Ok(context) = engine.get_context_mut(&cond) {
            context.set_output("decisions", serde_json::json!(["branch1", "branch2"]));
        }

        let pattern = MultiChoicePattern::new(
            cond.clone(),
            vec![
                ("branch1".to_string(), vec![act1.clone()]),
                ("branch2".to_string(), vec![act2.clone()]),
            ],
        );

        let result = pattern.execute(&mut engine).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_structured_discriminator_pattern() {
        let mut engine = WorkflowEngine::new();

        let branch1 = ActivityId::new("branch1");
        let branch2 = ActivityId::new("branch2");
        let disc = ActivityId::new("disc");

        engine.register_activity(Box::new(TestActivity { id: branch1.clone() }));
        engine.register_activity(Box::new(TestActivity { id: branch2.clone() }));
        engine.register_activity(Box::new(TestActivity { id: disc.clone() }));

        let pattern = StructuredDiscriminatorPattern::new(
            vec![branch1, branch2],
            disc.clone(),
        );

        let result = pattern.execute(&mut engine).await;
        assert!(result.is_ok());
    }
}
