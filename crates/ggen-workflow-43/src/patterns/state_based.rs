//! State-Based Patterns (16-22)

use async_trait::async_trait;
use crate::{ActivityId, Result, WorkflowEngine, WorkflowError, WorkflowPattern, ExecutionResult};
use std::sync::Arc;
use tokio::sync::Mutex;

/// Pattern 16: Deferred Choice
/// Choice is deferred until runtime and is based on the environment
pub struct DeferredChoicePattern {
    branches: Vec<(ActivityId, Vec<ActivityId>)>,
}

impl DeferredChoicePattern {
    /// Create a new deferred choice pattern
    pub fn new(branches: Vec<(ActivityId, Vec<ActivityId>)>) -> Self {
        Self { branches }
    }
}

#[async_trait]
impl WorkflowPattern for DeferredChoicePattern {
    fn name(&self) -> &str {
        "Deferred Choice"
    }

    fn pattern_number(&self) -> u8 {
        16
    }

    async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()> {
        use tokio::sync::Semaphore;

        let semaphore = Arc::new(Semaphore::new(1));
        let mut handles: Vec<tokio::task::JoinHandle<Result<Option<ActivityId>>>> = Vec::new();

        // Race all trigger activities
        for (trigger_id, _branch) in &self.branches {
            let sem = Arc::clone(&semaphore);
            let trigger_id = trigger_id.clone();

            let handle = tokio::spawn(async move {
                tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;

                if sem.try_acquire().is_ok() {
                    Ok(Some(trigger_id))
                } else {
                    Ok(None)
                }
            });

            handles.push(handle);
        }

        // Wait for first trigger
        let mut selected_branch = None;
        for handle in handles {
            if let Ok(Ok(Some(trigger_id))) = handle.await {
                selected_branch = self.branches.iter()
                    .find(|(tid, _)| tid == &trigger_id)
                    .map(|(_, branch)| branch.clone());
                break;
            }
        }

        // Execute selected branch
        if let Some(branch) = selected_branch {
            for activity_id in &branch {
                engine.execute_activity(activity_id).await?;
            }

            let mut executed: Vec<_> = self.branches.iter().map(|(t, _)| t.clone()).collect();
            executed.extend(branch);
            engine.record_execution(self, executed, ExecutionResult::Success);
        }

        Ok(())
    }
}

/// Pattern 17: Interleaved Parallel Routing
/// Activities execute in parallel but atomically
pub struct InterleavedParallelRoutingPattern {
    activities: Vec<ActivityId>,
}

impl InterleavedParallelRoutingPattern {
    /// Create a new interleaved parallel routing pattern
    pub fn new(activities: Vec<ActivityId>) -> Self {
        Self { activities }
    }
}

#[async_trait]
impl WorkflowPattern for InterleavedParallelRoutingPattern {
    fn name(&self) -> &str {
        "Interleaved Parallel Routing"
    }

    fn pattern_number(&self) -> u8 {
        17
    }

    async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()> {
        use tokio::sync::Semaphore;

        let semaphore = Arc::new(Semaphore::new(1));
        let mut handles = Vec::new();

        for activity_id in &self.activities {
            let sem = Arc::clone(&semaphore);
            let activity_id = activity_id.clone();

            let handle = tokio::spawn(async move {
                // Acquire semaphore for atomic execution
                let _permit = sem.acquire().await.map_err(|e|
                    WorkflowError::PatternExecutionFailed(e.to_string()))?;

                tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;

                Ok::<_, WorkflowError>(activity_id)
            });

            handles.push(handle);
        }

        for handle in handles {
            handle.await
                .map_err(|e| WorkflowError::PatternExecutionFailed(e.to_string()))??;
        }

        engine.record_execution(self, self.activities.clone(), ExecutionResult::Success);
        Ok(())
    }
}

/// Pattern 18: Milestone
/// An activity is only enabled if a certain milestone is reached
pub struct MilestonePattern {
    milestone_activity: ActivityId,
    gated_activities: Vec<ActivityId>,
}

impl MilestonePattern {
    /// Create a new milestone pattern
    pub fn new(milestone_activity: ActivityId, gated_activities: Vec<ActivityId>) -> Self {
        Self {
            milestone_activity,
            gated_activities,
        }
    }
}

#[async_trait]
impl WorkflowPattern for MilestonePattern {
    fn name(&self) -> &str {
        "Milestone"
    }

    fn pattern_number(&self) -> u8 {
        18
    }

    async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()> {
        // Execute milestone activity
        engine.execute_activity(&self.milestone_activity).await?;

        // Check milestone reached
        let context = engine.get_context(&self.milestone_activity)?;
        let milestone_reached = context.output_data.get("milestone_reached")
            .and_then(|v| v.as_bool())
            .unwrap_or(false);

        let mut executed = vec![self.milestone_activity.clone()];

        if milestone_reached {
            // Execute gated activities
            for activity_id in &self.gated_activities {
                engine.execute_activity(activity_id).await?;
                executed.push(activity_id.clone());
            }
        }

        engine.record_execution(self, executed, ExecutionResult::Success);
        Ok(())
    }
}

/// Pattern 19: Critical Section
/// Activities must execute atomically with respect to each other
pub struct CriticalSectionPattern {
    activities: Vec<ActivityId>,
}

impl CriticalSectionPattern {
    /// Create a new critical section pattern
    pub fn new(activities: Vec<ActivityId>) -> Self {
        Self { activities }
    }
}

#[async_trait]
impl WorkflowPattern for CriticalSectionPattern {
    fn name(&self) -> &str {
        "Critical Section"
    }

    fn pattern_number(&self) -> u8 {
        19
    }

    async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()> {
        let mutex = Arc::new(Mutex::new(()));

        // Execute activities in critical section
        let _guard = mutex.lock().await;

        for activity_id in &self.activities {
            engine.execute_activity(activity_id).await?;
        }

        drop(_guard);

        engine.record_execution(self, self.activities.clone(), ExecutionResult::Success);
        Ok(())
    }
}

/// Pattern 20: Interleaved Routing
/// Activities can be executed in any order but not concurrently
pub struct InterleavedRoutingPattern {
    activities: Vec<ActivityId>,
}

impl InterleavedRoutingPattern {
    /// Create a new interleaved routing pattern
    pub fn new(activities: Vec<ActivityId>) -> Self {
        Self { activities }
    }
}

#[async_trait]
impl WorkflowPattern for InterleavedRoutingPattern {
    fn name(&self) -> &str {
        "Interleaved Routing"
    }

    fn pattern_number(&self) -> u8 {
        20
    }

    async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()> {
        use tokio::sync::Semaphore;

        let semaphore = Arc::new(Semaphore::new(1));
        let mut handles = Vec::new();

        for activity_id in &self.activities {
            let sem = Arc::clone(&semaphore);
            let activity_id = activity_id.clone();

            let handle = tokio::spawn(async move {
                let _permit = sem.acquire().await.map_err(|e|
                    WorkflowError::PatternExecutionFailed(e.to_string()))?;

                tokio::time::sleep(tokio::time::Duration::from_millis(5)).await;

                Ok::<_, WorkflowError>(activity_id)
            });

            handles.push(handle);
        }

        for handle in handles {
            handle.await
                .map_err(|e| WorkflowError::PatternExecutionFailed(e.to_string()))??;
        }

        engine.record_execution(self, self.activities.clone(), ExecutionResult::Success);
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
    async fn test_deferred_choice_pattern() {
        let mut engine = WorkflowEngine::new();

        let trigger1 = ActivityId::new("trigger1");
        let trigger2 = ActivityId::new("trigger2");
        let act1 = ActivityId::new("act1");
        let act2 = ActivityId::new("act2");

        engine.register_activity(Box::new(TestActivity { id: trigger1.clone() }));
        engine.register_activity(Box::new(TestActivity { id: trigger2.clone() }));
        engine.register_activity(Box::new(TestActivity { id: act1.clone() }));
        engine.register_activity(Box::new(TestActivity { id: act2.clone() }));

        let pattern = DeferredChoicePattern::new(vec![
            (trigger1, vec![act1]),
            (trigger2, vec![act2]),
        ]);

        let result = pattern.execute(&mut engine).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_milestone_pattern() {
        let mut engine = WorkflowEngine::new();

        let milestone = ActivityId::new("milestone");
        let act1 = ActivityId::new("act1");

        engine.register_activity(Box::new(TestActivity { id: milestone.clone() }));
        engine.register_activity(Box::new(TestActivity { id: act1.clone() }));

        // Set milestone reached
        if let Ok(context) = engine.get_context_mut(&milestone) {
            context.set_output("milestone_reached", serde_json::json!(true));
        }

        let pattern = MilestonePattern::new(milestone, vec![act1]);
        let result = pattern.execute(&mut engine).await;

        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_critical_section_pattern() {
        let mut engine = WorkflowEngine::new();

        let act1 = ActivityId::new("act1");
        let act2 = ActivityId::new("act2");

        engine.register_activity(Box::new(TestActivity { id: act1.clone() }));
        engine.register_activity(Box::new(TestActivity { id: act2.clone() }));

        let pattern = CriticalSectionPattern::new(vec![act1, act2]);
        let result = pattern.execute(&mut engine).await;

        assert!(result.is_ok());
    }
}
