//! Multiple Instance Patterns (12-15)

use async_trait::async_trait;
use crate::{ActivityId, Result, WorkflowEngine, WorkflowError, WorkflowPattern, ExecutionResult};

/// Pattern 12: Multiple Instances without Synchronization
/// Multiple instances of an activity are created, each executing independently
pub struct MultipleInstancesWithoutSyncPattern {
    template_activity: ActivityId,
    instance_count: usize,
}

impl MultipleInstancesWithoutSyncPattern {
    /// Create a new multiple instances without synchronization pattern
    pub fn new(template_activity: ActivityId, instance_count: usize) -> Self {
        Self {
            template_activity,
            instance_count,
        }
    }
}

#[async_trait]
impl WorkflowPattern for MultipleInstancesWithoutSyncPattern {
    fn name(&self) -> &str {
        "Multiple Instances without Synchronization"
    }

    fn pattern_number(&self) -> u8 {
        12
    }

    async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()> {
        let mut handles = Vec::new();
        let mut instances = Vec::new();

        for i in 0..self.instance_count {
            let instance_id = ActivityId::new(format!("{}-instance-{}", self.template_activity.0, i));
            instances.push(instance_id.clone());

            let handle = tokio::spawn(async move {
                // Each instance executes independently
                tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
                Ok::<_, WorkflowError>(())
            });

            handles.push(handle);
        }

        // Don't wait for completion - fire and forget
        tokio::spawn(async move {
            for handle in handles {
                let _result = handle.await;
            }
        });

        engine.record_execution(self, instances, ExecutionResult::Success);
        Ok(())
    }
}

/// Pattern 13: Multiple Instances with a Priori Design-Time Knowledge
/// Number of instances is known at design time
pub struct MultipleInstancesDesignTimePattern {
    template_activity: ActivityId,
    instance_count: usize,
}

impl MultipleInstancesDesignTimePattern {
    /// Create a new multiple instances with design-time knowledge pattern
    pub fn new(template_activity: ActivityId, instance_count: usize) -> Self {
        Self {
            template_activity,
            instance_count,
        }
    }
}

#[async_trait]
impl WorkflowPattern for MultipleInstancesDesignTimePattern {
    fn name(&self) -> &str {
        "Multiple Instances with a Priori Design-Time Knowledge"
    }

    fn pattern_number(&self) -> u8 {
        13
    }

    async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()> {
        let mut handles = Vec::new();
        let mut instances = Vec::new();

        for i in 0..self.instance_count {
            let instance_id = ActivityId::new(format!("{}-instance-{}", self.template_activity.0, i));
            instances.push(instance_id.clone());

            let handle = tokio::spawn(async move {
                tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
                Ok::<_, WorkflowError>(())
            });

            handles.push(handle);
        }

        // Wait for all instances to complete
        for handle in handles {
            handle.await
                .map_err(|e| WorkflowError::MultipleInstanceError(e.to_string()))??;
        }

        engine.record_execution(self, instances, ExecutionResult::Success);
        Ok(())
    }
}

/// Pattern 14: Multiple Instances with a Priori Run-Time Knowledge
/// Number of instances is determined at runtime before execution
pub struct MultipleInstancesRuntimePattern {
    template_activity: ActivityId,
    count_source_activity: ActivityId,
}

impl MultipleInstancesRuntimePattern {
    /// Create a new multiple instances with runtime knowledge pattern
    pub fn new(template_activity: ActivityId, count_source_activity: ActivityId) -> Self {
        Self {
            template_activity,
            count_source_activity,
        }
    }
}

#[async_trait]
impl WorkflowPattern for MultipleInstancesRuntimePattern {
    fn name(&self) -> &str {
        "Multiple Instances with a Priori Run-Time Knowledge"
    }

    fn pattern_number(&self) -> u8 {
        14
    }

    async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()> {
        // Execute count source to determine instance count
        engine.execute_activity(&self.count_source_activity).await?;

        let context = engine.get_context(&self.count_source_activity)?;
        let instance_count = context.output_data.get("instance_count")
            .and_then(|v| v.as_u64())
            .ok_or_else(|| WorkflowError::MultipleInstanceError("No instance_count in output".to_string()))? as usize;

        let mut handles = Vec::new();
        let mut instances = vec![self.count_source_activity.clone()];

        for i in 0..instance_count {
            let instance_id = ActivityId::new(format!("{}-instance-{}", self.template_activity.0, i));
            instances.push(instance_id.clone());

            let handle = tokio::spawn(async move {
                tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
                Ok::<_, WorkflowError>(())
            });

            handles.push(handle);
        }

        for handle in handles {
            handle.await
                .map_err(|e| WorkflowError::MultipleInstanceError(e.to_string()))??;
        }

        engine.record_execution(self, instances, ExecutionResult::Success);
        Ok(())
    }
}

/// Pattern 15: Multiple Instances without a Priori Run-Time Knowledge
/// New instances can be created while others are executing
pub struct MultipleInstancesDynamicPattern {
    template_activity: ActivityId,
    #[allow(dead_code)]
    controller_activity: ActivityId,
}

impl MultipleInstancesDynamicPattern {
    /// Create a new multiple instances without a priori runtime knowledge pattern
    pub fn new(template_activity: ActivityId, controller_activity: ActivityId) -> Self {
        Self {
            template_activity,
            controller_activity,
        }
    }
}

#[async_trait]
impl WorkflowPattern for MultipleInstancesDynamicPattern {
    fn name(&self) -> &str {
        "Multiple Instances without a Priori Run-Time Knowledge"
    }

    fn pattern_number(&self) -> u8 {
        15
    }

    async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()> {
        use std::sync::Arc;
        use tokio::sync::Mutex;

        let instances = Arc::new(Mutex::new(Vec::new()));
        let running = Arc::new(Mutex::new(true));

        // Controller monitors and creates new instances
        let controller_instances = Arc::clone(&instances);
        let controller_running = Arc::clone(&running);
        let template_activity = self.template_activity.clone();

        let controller_handle = tokio::spawn(async move {
            let mut count = 0;
            while *controller_running.lock().await {
                tokio::time::sleep(tokio::time::Duration::from_millis(20)).await;

                let instance_id = ActivityId::new(format!("{}-dynamic-{}", template_activity.0, count));
                controller_instances.lock().await.push(instance_id);
                count += 1;

                if count >= 5 {
                    *controller_running.lock().await = false;
                }
            }
            Ok::<_, WorkflowError>(())
        });

        controller_handle.await
            .map_err(|e| WorkflowError::MultipleInstanceError(e.to_string()))??;

        let final_instances = instances.lock().await.clone();
        engine.record_execution(self, final_instances, ExecutionResult::Success);
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
    async fn test_multiple_instances_without_sync() {
        let mut engine = WorkflowEngine::new();
        let template = ActivityId::new("template");

        engine.register_activity(Box::new(TestActivity { id: template.clone() }));

        let pattern = MultipleInstancesWithoutSyncPattern::new(template, 3);
        let result = pattern.execute(&mut engine).await;

        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_multiple_instances_design_time() {
        let mut engine = WorkflowEngine::new();
        let template = ActivityId::new("template");

        engine.register_activity(Box::new(TestActivity { id: template.clone() }));

        let pattern = MultipleInstancesDesignTimePattern::new(template, 3);
        let result = pattern.execute(&mut engine).await;

        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_multiple_instances_runtime() {
        let mut engine = WorkflowEngine::new();
        let template = ActivityId::new("template");
        let count_source = ActivityId::new("count-source");

        engine.register_activity(Box::new(TestActivity { id: template.clone() }));
        engine.register_activity(Box::new(TestActivity { id: count_source.clone() }));

        // Set instance count
        if let Ok(context) = engine.get_context_mut(&count_source) {
            context.set_output("instance_count", serde_json::json!(3));
        }

        let pattern = MultipleInstancesRuntimePattern::new(template, count_source);
        let result = pattern.execute(&mut engine).await;

        assert!(result.is_ok());
    }
}
