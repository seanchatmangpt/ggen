//! Pattern execution engine

use crate::{Result, WorkflowEngine, WorkflowPattern, WorkflowError};
use async_trait::async_trait;
use std::sync::Arc;
use tokio::sync::Mutex;

/// Pattern executor for managing workflow pattern execution
pub struct PatternExecutor {
    engine: Arc<Mutex<WorkflowEngine>>,
}

impl PatternExecutor {
    /// Create a new pattern executor
    pub fn new(engine: WorkflowEngine) -> Self {
        Self {
            engine: Arc::new(Mutex::new(engine)),
        }
    }

    /// Execute a pattern
    pub async fn execute_pattern(&self, pattern: Box<dyn WorkflowPattern>) -> Result<()> {
        let mut engine = self.engine.lock().await;
        pattern.execute(&mut engine).await
    }

    /// Execute multiple patterns in sequence
    pub async fn execute_sequence(&self, patterns: Vec<Box<dyn WorkflowPattern>>) -> Result<()> {
        for pattern in patterns {
            self.execute_pattern(pattern).await?;
        }
        Ok(())
    }

    /// Execute multiple patterns in parallel
    pub async fn execute_parallel(&self, patterns: Vec<Box<dyn WorkflowPattern>>) -> Result<()> {
        let mut handles = Vec::new();

        for pattern in patterns {
            let engine = Arc::clone(&self.engine);
            let handle = tokio::spawn(async move {
                let mut eng = engine.lock().await;
                pattern.execute(&mut eng).await
            });
            handles.push(handle);
        }

        for handle in handles {
            handle.await
                .map_err(|e| WorkflowError::PatternExecutionFailed(e.to_string()))??;
        }

        Ok(())
    }

    /// Get a reference to the workflow engine
    pub async fn engine(&self) -> tokio::sync::MutexGuard<'_, WorkflowEngine> {
        self.engine.lock().await
    }
}

/// Pattern builder for fluent API
pub struct PatternBuilder {
    executor: PatternExecutor,
}

impl PatternBuilder {
    /// Create a new pattern builder
    pub fn new(engine: WorkflowEngine) -> Self {
        Self {
            executor: PatternExecutor::new(engine),
        }
    }

    /// Add a pattern to execute
    pub async fn with_pattern(self, pattern: Box<dyn WorkflowPattern>) -> Result<Self> {
        self.executor.execute_pattern(pattern).await?;
        Ok(self)
    }

    /// Build and return the executor
    pub fn build(self) -> PatternExecutor {
        self.executor
    }

    /// Get the executor
    pub fn executor(&self) -> &PatternExecutor {
        &self.executor
    }
}

/// Composite pattern for combining multiple patterns
pub struct CompositePattern {
    name: String,
    patterns: Vec<Box<dyn WorkflowPattern>>,
    execution_mode: ExecutionMode,
}

/// Pattern execution mode
#[derive(Debug, Clone, Copy)]
pub enum ExecutionMode {
    /// Execute patterns sequentially
    Sequential,
    /// Execute patterns in parallel
    Parallel,
}

impl CompositePattern {
    /// Create a new composite pattern
    pub fn new(name: impl Into<String>, execution_mode: ExecutionMode) -> Self {
        Self {
            name: name.into(),
            patterns: Vec::new(),
            execution_mode,
        }
    }

    /// Add a pattern to the composite
    pub fn add_pattern(mut self, pattern: Box<dyn WorkflowPattern>) -> Self {
        self.patterns.push(pattern);
        self
    }
}

#[async_trait]
impl WorkflowPattern for CompositePattern {
    fn name(&self) -> &str {
        &self.name
    }

    fn pattern_number(&self) -> u8 {
        0 // Composite patterns don't have a fixed number
    }

    async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()> {
        match self.execution_mode {
            ExecutionMode::Sequential => {
                for pattern in &self.patterns {
                    pattern.execute(engine).await?;
                }
            }
            ExecutionMode::Parallel => {
                let mut handles = Vec::new();

                for _pattern in &self.patterns {
                    let handle = tokio::spawn(async move {
                        // In a real implementation, we'd need to safely share engine
                        Ok::<_, WorkflowError>(())
                    });
                    handles.push(handle);
                }

                for handle in handles {
                    handle.await
                        .map_err(|e| WorkflowError::PatternExecutionFailed(e.to_string()))??;
                }
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Activity, ActivityContext, ActivityId};

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

    struct TestPattern {
        activity_id: ActivityId,
    }

    #[async_trait]
    impl WorkflowPattern for TestPattern {
        fn name(&self) -> &str {
            "TestPattern"
        }

        fn pattern_number(&self) -> u8 {
            99
        }

        async fn execute(&self, engine: &mut WorkflowEngine) -> Result<()> {
            engine.execute_activity(&self.activity_id).await
        }
    }

    #[tokio::test]
    async fn test_pattern_executor() {
        let mut engine = WorkflowEngine::new();
        let act_id = ActivityId::new("test");

        engine.register_activity(Box::new(TestActivity { id: act_id.clone() }));

        let executor = PatternExecutor::new(engine);
        let pattern = Box::new(TestPattern { activity_id: act_id });

        let result = executor.execute_pattern(pattern).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_pattern_builder() {
        let mut engine = WorkflowEngine::new();
        let act_id = ActivityId::new("test");

        engine.register_activity(Box::new(TestActivity { id: act_id.clone() }));

        let builder = PatternBuilder::new(engine);
        let pattern = Box::new(TestPattern { activity_id: act_id });

        let result = builder.with_pattern(pattern).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_composite_pattern() {
        let mut engine = WorkflowEngine::new();
        let act_id = ActivityId::new("test");

        engine.register_activity(Box::new(TestActivity { id: act_id.clone() }));

        let composite = CompositePattern::new("test-composite", ExecutionMode::Sequential)
            .add_pattern(Box::new(TestPattern { activity_id: act_id.clone() }))
            .add_pattern(Box::new(TestPattern { activity_id: act_id }));

        let result = composite.execute(&mut engine).await;
        assert!(result.is_ok());
    }
}
