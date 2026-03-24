//! Cancellation and Force Completion Patterns (19-25)

use async_trait::async_trait;
use crate::{ActivityId, Result, WorkflowEngine, WorkflowError, WorkflowPattern, ExecutionResult, ActivityState};

/// Pattern 19: Cancel Activity
/// An enabled activity is disabled (cancelled)
pub struct CancelActivityPattern {
    target_activity: ActivityId,
}

impl CancelActivityPattern {
    /// Create a new cancel activity pattern
    pub fn new(target_activity: ActivityId) -> Self {
        Self { target_activity }
    }
}

#[async_trait]
impl WorkflowPattern for CancelActivityPattern {
    fn name(&self) -> &str {
        "Cancel Activity"
    }

    fn pattern_number(&self) -> u8 {
        19
    }

    fn supports_cancellation(&self) -> bool {
        true
    }

    async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()> {
        // Check if activity can be cancelled
        let can_cancel = engine.activities.get(&self.target_activity)
            .map(|a| a.can_cancel())
            .unwrap_or(false);

        if !can_cancel {
            return Err(WorkflowError::CancellationError("Activity cannot be cancelled".to_string()));
        }

        // Remove activity temporarily
        let activity = engine.activities.remove(&self.target_activity)
            .ok_or_else(|| WorkflowError::ActivityNotFound(self.target_activity.0.clone()))?;

        let context = engine.get_context_mut(&self.target_activity)?;
        activity.cancel(context).await?;

        // Put activity back
        engine.activities.insert(self.target_activity.clone(), activity);

        engine.record_execution(self, vec![self.target_activity.clone()], ExecutionResult::Success);
        Ok(())
    }
}

/// Pattern 20: Cancel Case
/// A complete workflow instance is removed
pub struct CancelCasePattern {
    activities: Vec<ActivityId>,
}

impl CancelCasePattern {
    /// Create a new cancel case pattern
    pub fn new(activities: Vec<ActivityId>) -> Self {
        Self { activities }
    }
}

#[async_trait]
impl WorkflowPattern for CancelCasePattern {
    fn name(&self) -> &str {
        "Cancel Case"
    }

    fn pattern_number(&self) -> u8 {
        20
    }

    fn supports_cancellation(&self) -> bool {
        true
    }

    async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()> {
        // Cancel all activities in the case
        for activity_id in &self.activities {
            if engine.activities.contains_key(activity_id) {
                let activity = engine.activities.remove(activity_id);

                if let Some(ref act) = activity {
                    if let Ok(context) = engine.get_context_mut(activity_id) {
                        let _cancel_result = act.cancel(context).await;
                    }
                }

                if let Some(act) = activity {
                    engine.activities.insert(activity_id.clone(), act);
                }
            }
        }

        engine.record_execution(self, self.activities.clone(), ExecutionResult::Success);
        Ok(())
    }
}

/// Pattern 25: Cancel Region
/// A set of activities is cancelled
pub struct CancelRegionPattern {
    region_activities: Vec<ActivityId>,
}

impl CancelRegionPattern {
    /// Create a new cancel region pattern
    pub fn new(region_activities: Vec<ActivityId>) -> Self {
        Self { region_activities }
    }
}

#[async_trait]
impl WorkflowPattern for CancelRegionPattern {
    fn name(&self) -> &str {
        "Cancel Region"
    }

    fn pattern_number(&self) -> u8 {
        25
    }

    fn supports_cancellation(&self) -> bool {
        true
    }

    async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()> {
        for activity_id in &self.region_activities {
            if engine.activities.contains_key(activity_id) {
                let activity = engine.activities.remove(activity_id);

                if let Some(ref act) = activity {
                    if let Ok(context) = engine.get_context_mut(activity_id) {
                        let _cancel_result = act.cancel(context).await;
                    }
                }

                if let Some(act) = activity {
                    engine.activities.insert(activity_id.clone(), act);
                }
            }
        }

        engine.record_execution(self, self.region_activities.clone(), ExecutionResult::Success);
        Ok(())
    }
}

/// Pattern 26: Cancel Multiple Instance Activity
/// All instances of a multiple instance activity are cancelled
pub struct CancelMultipleInstanceActivityPattern {
    instance_activities: Vec<ActivityId>,
}

impl CancelMultipleInstanceActivityPattern {
    /// Create a new cancel multiple instance activity pattern
    pub fn new(instance_activities: Vec<ActivityId>) -> Self {
        Self { instance_activities }
    }
}

#[async_trait]
impl WorkflowPattern for CancelMultipleInstanceActivityPattern {
    fn name(&self) -> &str {
        "Cancel Multiple Instance Activity"
    }

    fn pattern_number(&self) -> u8 {
        26
    }

    fn supports_cancellation(&self) -> bool {
        true
    }

    async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()> {
        for instance_id in &self.instance_activities {
            if engine.activities.contains_key(instance_id) {
                let activity = engine.activities.remove(instance_id);

                if let Some(ref act) = activity {
                    if let Ok(context) = engine.get_context_mut(instance_id) {
                        let _cancel_result = act.cancel(context).await;
                    }
                }

                if let Some(act) = activity {
                    engine.activities.insert(instance_id.clone(), act);
                }
            }
        }

        engine.record_execution(self, self.instance_activities.clone(), ExecutionResult::Success);
        Ok(())
    }
}

/// Pattern 27: Complete Multiple Instance Activity
/// Force completion of all instances when a threshold is met
pub struct CompleteMultipleInstanceActivityPattern {
    instance_activities: Vec<ActivityId>,
    completion_threshold: usize,
}

impl CompleteMultipleInstanceActivityPattern {
    /// Create a new complete multiple instance activity pattern
    pub fn new(instance_activities: Vec<ActivityId>, completion_threshold: usize) -> Self {
        Self {
            instance_activities,
            completion_threshold,
        }
    }
}

#[async_trait]
impl WorkflowPattern for CompleteMultipleInstanceActivityPattern {
    fn name(&self) -> &str {
        "Complete Multiple Instance Activity"
    }

    fn pattern_number(&self) -> u8 {
        27
    }

    async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()> {
        // Count completed instances
        let completed_count = self.instance_activities.iter()
            .filter(|id| {
                engine.get_context(id)
                    .map(|ctx| ctx.state == ActivityState::Completed)
                    .unwrap_or(false)
            })
            .count();

        if completed_count >= self.completion_threshold {
            // Cancel remaining instances
            for instance_id in &self.instance_activities {
                let should_cancel = engine.get_context(instance_id)
                    .map(|ctx| ctx.state != ActivityState::Completed)
                    .unwrap_or(false);

                if should_cancel && engine.activities.contains_key(instance_id) {
                    let activity = engine.activities.remove(instance_id);

                    if let Some(ref act) = activity {
                        if let Ok(ctx) = engine.get_context_mut(instance_id) {
                            let _cancel_result = act.cancel(ctx).await;
                        }
                    }

                    if let Some(act) = activity {
                        engine.activities.insert(instance_id.clone(), act);
                    }
                }
            }
        }

        engine.record_execution(self, self.instance_activities.clone(), ExecutionResult::Success);
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
    async fn test_cancel_activity_pattern() {
        let mut engine = WorkflowEngine::new();

        let act = ActivityId::new("act");
        engine.register_activity(Box::new(TestActivity { id: act.clone() }));

        let pattern = CancelActivityPattern::new(act.clone());
        let result = pattern.execute(&mut engine).await;

        assert!(result.is_ok());
        assert_eq!(
            engine.get_context(&act).ok().map(|c| &c.state),
            Some(&ActivityState::Cancelled)
        );
    }

    #[tokio::test]
    async fn test_cancel_case_pattern() {
        let mut engine = WorkflowEngine::new();

        let act1 = ActivityId::new("act1");
        let act2 = ActivityId::new("act2");

        engine.register_activity(Box::new(TestActivity { id: act1.clone() }));
        engine.register_activity(Box::new(TestActivity { id: act2.clone() }));

        let pattern = CancelCasePattern::new(vec![act1.clone(), act2.clone()]);
        let result = pattern.execute(&mut engine).await;

        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_cancel_region_pattern() {
        let mut engine = WorkflowEngine::new();

        let act1 = ActivityId::new("act1");
        let act2 = ActivityId::new("act2");

        engine.register_activity(Box::new(TestActivity { id: act1.clone() }));
        engine.register_activity(Box::new(TestActivity { id: act2.clone() }));

        let pattern = CancelRegionPattern::new(vec![act1, act2]);
        let result = pattern.execute(&mut engine).await;

        assert!(result.is_ok());
    }
}
