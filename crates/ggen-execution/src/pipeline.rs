// Execution pipeline implementation for complex workflows
use crate::error::*;
use crate::framework::*;
use crate::types::*;
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::sync::Arc;
use tokio::sync::{Mutex, RwLock, Semaphore};
use uuid::Uuid;

// ============================================================================
// ENHANCED PIPELINE STAGE DEFINITIONS
// ============================================================================

/// Enhanced pipeline stage with extended capabilities
#[derive(Debug, Clone)]
pub struct EnhancedPipelineStage {
    pub id: String,
    pub name: String,
    pub stage_type: StageType,
    pub tasks: Vec<Task>,
    pub dependencies: Vec<String>,
    pub parallelism: usize,
    pub timeout_seconds: u64,
    pub retry_policy: RetryPolicy,
    pub conditions: Vec<StageCondition>,
    pub resources: ResourceRequirements,
}

impl From<EnhancedPipelineStage> for PipelineStage {
    fn from(enhanced: EnhancedPipelineStage) -> Self {
        Self {
            name: enhanced.name,
            stage_type: enhanced.stage_type.to_string(),
            tasks: enhanced.tasks,
            dependencies: enhanced.dependencies,
            timeout_seconds: enhanced.timeout_seconds,
        }
    }
}

/// Stage types
#[derive(Debug, Clone, PartialEq)]
pub enum StageType {
    DataProcessing,
    Validation,
    Transformation,
    Analysis,
    Communication,
    Custom(String),
}

impl StageType {
    pub fn to_string(&self) -> String {
        match self {
            StageType::DataProcessing => "data_processing".to_string(),
            StageType::Validation => "validation".to_string(),
            StageType::Transformation => "transformation".to_string(),
            StageType::Analysis => "analysis".to_string(),
            StageType::Communication => "communication".to_string(),
            StageType::Custom(s) => s.clone(),
        }
    }
}

/// Retry policy for stage execution
#[derive(Debug, Clone, Default)]
pub struct RetryPolicy {
    pub max_retries: u32,
    pub delay_ms: u64,
    pub backoff_multiplier: f64,
    pub max_delay_ms: u64,
}

/// Resource requirements for stage
#[derive(Debug, Clone, Default)]
pub struct ResourceRequirements {
    pub min_cpu_cores: f64,
    pub min_memory_gb: f64,
    pub max_cpu_cores: Option<f64>,
    pub max_memory_gb: Option<f64>,
}

/// Stage conditions for execution
#[derive(Debug, Clone)]
pub struct StageCondition {
    pub condition_type: ConditionType,
    pub field: String,
    pub operator: ComparisonOperator,
    pub value: serde_json::Value,
}

/// Condition types
#[derive(Debug, Clone, PartialEq)]
pub enum ConditionType {
    Input,
    Output,
    Metadata,
    PreviousStage,
}

/// Comparison operators
#[derive(Debug, Clone, PartialEq)]
pub enum ComparisonOperator {
    Equals,
    NotEquals,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
    Contains,
    NotContains,
}

// ============================================================================
// ENHANCED STAGE BUILDER
// ============================================================================

/// Builder for enhanced pipeline stages
pub struct EnhancedStageBuilder {
    stage: EnhancedPipelineStage,
}

impl EnhancedStageBuilder {
    pub fn new(name: &str, stage_type: StageType) -> Self {
        Self {
            stage: EnhancedPipelineStage {
                id: Uuid::new_v4().to_string(),
                name: name.to_string(),
                stage_type,
                tasks: Vec::new(),
                dependencies: Vec::new(),
                parallelism: 1,
                timeout_seconds: 300,
                retry_policy: RetryPolicy::default(),
                conditions: Vec::new(),
                resources: ResourceRequirements::default(),
            },
        }
    }

    pub fn with_parallelism(mut self, parallelism: usize) -> Self {
        self.stage.parallelism = parallelism.max(1);
        self
    }

    pub fn with_timeout(mut self, timeout_seconds: u64) -> Self {
        self.stage.timeout_seconds = timeout_seconds;
        self
    }

    pub fn with_retry_policy(mut self, policy: RetryPolicy) -> Self {
        self.stage.retry_policy = policy;
        self
    }

    pub fn add_task(mut self, task: Task) -> Self {
        self.stage.tasks.push(task);
        self
    }

    pub fn add_dependency(mut self, stage_id: &str) -> Self {
        self.stage.dependencies.push(stage_id.to_string());
        self
    }

    pub fn add_condition(mut self, condition: StageCondition) -> Self {
        self.stage.conditions.push(condition);
        self
    }

    pub fn with_resources(mut self, resources: ResourceRequirements) -> Self {
        self.stage.resources = resources;
        self
    }

    pub fn build(self) -> EnhancedPipelineStage {
        self.stage
    }

    /// Convert to base PipelineStage for framework compatibility
    pub fn build_base(self) -> PipelineStage {
        PipelineStage::from(self.stage)
    }
}

// ============================================================================
// PIPELINE BUILDER
// ============================================================================

/// Builder for creating pipelines
pub struct PipelineBuilder {
    pipeline: ExecutionPipeline,
}

impl PipelineBuilder {
    pub fn new(id: &str, name: &str) -> Self {
        Self {
            pipeline: ExecutionPipeline {
                id: id.to_string(),
                name: name.to_string(),
                pipeline_type: "custom".to_string(),
                stages: Vec::new(),
                status: PipelineStatus::Created,
            },
        }
    }

    pub fn with_type(mut self, pipeline_type: &str) -> Self {
        self.pipeline.pipeline_type = pipeline_type.to_string();
        self
    }

    pub fn add_stage(mut self, stage: PipelineStage) -> Self {
        self.pipeline.stages.push(stage);
        self
    }

    pub fn with_timeout(mut self, timeout_seconds: u64) -> Self {
        // Apply timeout to all stages
        self.pipeline.stages.iter_mut().for_each(|s| {
            s.timeout_seconds = timeout_seconds;
        });
        self
    }

    pub fn build(self) -> ExecutionPipeline {
        self.pipeline
    }
}

// ============================================================================
// ENHANCED PIPELINE EXECUTION
// ============================================================================

/// Enhanced pipeline executor with concurrency control for base stages
pub struct EnhancedPipelineExecutor {
    framework: Arc<Mutex<ExecutionFramework>>,
    metrics: Arc<RwLock<ExecutionMetrics>>,
}

impl EnhancedPipelineExecutor {
    pub fn new(framework: ExecutionFramework) -> Self {
        Self {
            framework: Arc::new(Mutex::new(framework)),
            metrics: Arc::new(RwLock::new(ExecutionMetrics::new())),
        }
    }

    pub async fn execute_pipeline(
        &self, pipeline: &mut ExecutionPipeline,
    ) -> Result<PipelineResult, ExecutionError> {
        let mut stages = pipeline.stages.clone();
        let mut results = Vec::new();
        let mut success_count = 0;

        // Execute stages in dependency order
        let mut completed_stages = HashSet::new();

        for stage in &mut stages {
            // Check dependencies
            let dependencies_met = stage
                .dependencies
                .iter()
                .all(|dep_id| completed_stages.contains(dep_id));

            if dependencies_met {
                let result = self.execute_stage(stage).await?;
                let success = result.success;
                results.push(result);

                if success {
                    success_count += 1;
                    completed_stages.insert(stage.name.clone());
                } else {
                    // Stop execution on failed stage
                    pipeline.status =
                        PipelineStatus::Failed(format!("Stage {} failed", stage.name));
                    break;
                }
            }
        }

        if success_count == stages.len() {
            pipeline.status = PipelineStatus::Completed;
        }

        Ok(PipelineResult {
            pipeline_id: pipeline.id.clone(),
            success: success_count == stages.len(),
            success_rate: success_count as f64 / stages.len().max(1) as f64,
            total_stages: stages.len(),
            completed_stages: success_count,
            results,
        })
    }

    async fn execute_stage(
        &self, stage: &mut PipelineStage,
    ) -> Result<StageResult, ExecutionError> {
        let mut success_count = 0;
        let mut failed_count = 0;

        for task in &stage.tasks {
            let mut framework = self.framework.lock().await;
            let result = framework.execute_task(task.clone()).await?;

            if result.success {
                success_count += 1;
            } else {
                failed_count += 1;
            }
        }

        Ok(StageResult {
            stage_name: stage.name.clone(),
            success: failed_count == 0,
            success_rate: success_count as f64 / (success_count + failed_count).max(1) as f64,
            total_tasks: success_count + failed_count,
            successful_tasks: success_count,
            failed_tasks: failed_count,
        })
    }

    pub async fn get_metrics(&self) -> ExecutionMetrics {
        self.metrics.read().await.clone()
    }
}

/// Parallel pipeline executor for enhanced stages
pub struct ParallelPipelineExecutor {
    framework: Arc<Mutex<ExecutionFramework>>,
    metrics: Arc<RwLock<ExecutionMetrics>>,
}

impl ParallelPipelineExecutor {
    pub fn new(framework: ExecutionFramework) -> Self {
        Self {
            framework: Arc::new(Mutex::new(framework)),
            metrics: Arc::new(RwLock::new(ExecutionMetrics::new())),
        }
    }

    pub async fn execute_enhanced_pipeline(
        &self, stages: &mut Vec<EnhancedPipelineStage>,
    ) -> Result<EnhancedPipelineResult, ExecutionError> {
        let mut results = Vec::new();
        let mut success_count = 0;
        let total_stages = stages.len();

        // Execute stages in dependency order
        let mut completed_stages = HashSet::new();

        for stage in &mut *stages {
            // Check dependencies
            let dependencies_met = stage
                .dependencies
                .iter()
                .all(|dep_id| completed_stages.contains(dep_id));

            if dependencies_met {
                let result = self.execute_enhanced_stage(stage).await?;
                results.push(result.clone());

                if result.success {
                    success_count += 1;
                    completed_stages.insert(stage.name.clone());
                }
            }
        }

        Ok(EnhancedPipelineResult {
            success: success_count == total_stages,
            success_rate: success_count as f64 / total_stages.max(1) as f64,
            total_stages,
            completed_stages: success_count,
            results,
        })
    }

    async fn execute_enhanced_stage(
        &self, stage: &EnhancedPipelineStage,
    ) -> Result<EnhancedStageResult, ExecutionError> {
        let mut successful_tasks = 0;
        let mut failed_tasks = 0;

        // Group tasks into batches based on parallelism
        let batch_size = stage.parallelism.max(1);
        let semaphore = Arc::new(Semaphore::new(batch_size));

        let mut task_handles = Vec::new();

        for task in &stage.tasks {
            let framework = self.framework.clone();
            let task_clone = task.clone();
            let sem = semaphore.clone();

            let handle = tokio::spawn(async move {
                // Acquire permit
                let _permit = sem.acquire().await.unwrap();

                let mut fw = framework.lock().await;
                fw.execute_task(task_clone).await
            });

            task_handles.push(handle);
        }

        // Wait for all tasks to complete
        for handle in task_handles {
            match handle.await {
                Ok(Ok(task_result)) => {
                    if task_result.success {
                        successful_tasks += 1;
                    } else {
                        failed_tasks += 1;
                    }
                }
                Ok(Err(_)) => {
                    failed_tasks += 1;
                }
                Err(_) => {
                    failed_tasks += 1;
                }
            }
        }

        Ok(EnhancedStageResult {
            stage_name: stage.name.clone(),
            success: failed_tasks == 0,
            success_rate: successful_tasks as f64 / (successful_tasks + failed_tasks).max(1) as f64,
            total_tasks: successful_tasks + failed_tasks,
            successful_tasks,
            failed_tasks,
        })
    }

    pub async fn get_metrics(&self) -> ExecutionMetrics {
        self.metrics.read().await.clone()
    }
}

/// Enhanced pipeline result
#[derive(Debug, Clone)]
pub struct EnhancedPipelineResult {
    pub success: bool,
    pub success_rate: f64,
    pub total_stages: usize,
    pub completed_stages: usize,
    pub results: Vec<EnhancedStageResult>,
}

/// Enhanced stage result
#[derive(Debug, Clone)]
pub struct EnhancedStageResult {
    pub stage_name: String,
    pub success: bool,
    pub success_rate: f64,
    pub total_tasks: usize,
    pub successful_tasks: usize,
    pub failed_tasks: usize,
}

/// Enhanced execution metrics
#[derive(Debug, Clone)]
pub struct ExecutionMetrics {
    pub total_tasks: u64,
    pub successful_tasks: u64,
    pub failed_tasks: u64,
    pub average_execution_time_ms: u64,
    pub total_execution_time_ms: u64,
    pub peak_memory_usage_mb: u64,
    pub current_memory_usage_mb: u64,
    pub throughput_tasks_per_second: f64,
    pub error_rate: f64,
}

impl ExecutionMetrics {
    pub fn new() -> Self {
        Self {
            total_tasks: 0,
            successful_tasks: 0,
            failed_tasks: 0,
            average_execution_time_ms: 0,
            total_execution_time_ms: 0,
            peak_memory_usage_mb: 0,
            current_memory_usage_mb: 0,
            throughput_tasks_per_second: 0.0,
            error_rate: 0.0,
        }
    }

    pub fn record_task(&mut self, success: bool, execution_time_ms: u64) {
        self.total_tasks += 1;
        if success {
            self.successful_tasks += 1;
        } else {
            self.failed_tasks += 1;
        }

        self.total_execution_time_ms += execution_time_ms;
        self.average_execution_time_ms = self.total_execution_time_ms / self.total_tasks;

        // Update error rate
        self.error_rate = self.failed_tasks as f64 / self.total_tasks as f64;
    }
}

/// Stage task result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StageTaskResult {
    pub task_id: TaskId,
    pub success: bool,
    pub execution_time_ms: u64,
}

// ============================================================================
// PIPELINE VALIDATION
// ============================================================================

/// Pipeline validation logic
pub struct PipelineValidator;

impl PipelineValidator {
    pub fn validate_pipeline(pipeline: &ExecutionPipeline) -> Result<(), ExecutionError> {
        // Check for duplicate stage names
        let mut stage_names = std::collections::HashSet::new();
        for stage in &pipeline.stages {
            if stage_names.contains(&stage.name) {
                return Err(ExecutionError::Pipeline(format!(
                    "Duplicate stage name: {}",
                    stage.name
                )));
            }
            stage_names.insert(stage.name.clone());
        }

        // Check for circular dependencies
        if Self::has_circular_dependencies(pipeline)? {
            return Err(ExecutionError::Pipeline(
                "Circular dependencies detected in pipeline".to_string(),
            ));
        }

        // Validate stage configurations
        for stage in &pipeline.stages {
            Self::validate_stage(stage)?;
        }

        Ok(())
    }

    fn has_circular_dependencies(pipeline: &ExecutionPipeline) -> Result<bool, ExecutionError> {
        let mut visited = std::collections::HashSet::new();
        let mut recursion_stack = std::collections::HashSet::new();

        for stage in &pipeline.stages {
            if Self::has_circular_dependencies_recursive(
                stage,
                pipeline,
                &mut visited,
                &mut recursion_stack,
            )? {
                return Ok(true);
            }
        }

        Ok(false)
    }

    fn has_circular_dependencies_recursive(
        stage: &PipelineStage, pipeline: &ExecutionPipeline,
        visited: &mut std::collections::HashSet<String>,
        recursion_stack: &mut std::collections::HashSet<String>,
    ) -> Result<bool, ExecutionError> {
        if visited.contains(&stage.name) {
            return Ok(recursion_stack.contains(&stage.name));
        }

        visited.insert(stage.name.clone());
        recursion_stack.insert(stage.name.clone());

        for dep_id in &stage.dependencies {
            let dep_stage = pipeline
                .stages
                .iter()
                .find(|s| s.name == *dep_id)
                .ok_or_else(|| {
                    ExecutionError::Pipeline(format!("Dependency not found: {}", dep_id))
                })?;

            if Self::has_circular_dependencies_recursive(
                dep_stage,
                pipeline,
                visited,
                recursion_stack,
            )? {
                return Ok(true);
            }
        }

        recursion_stack.remove(&stage.name);
        Ok(false)
    }

    fn validate_stage(stage: &PipelineStage) -> Result<(), ExecutionError> {
        if stage.tasks.is_empty() {
            return Err(ExecutionError::Pipeline(format!(
                "Stage '{}' has no tasks",
                stage.name
            )));
        }

        if stage.timeout_seconds == 0 {
            return Err(ExecutionError::Pipeline(format!(
                "Stage '{}' has invalid timeout: {}",
                stage.name, stage.timeout_seconds
            )));
        }

        Ok(())
    }

    /// Validate enhanced pipeline stage with additional checks
    pub fn validate_enhanced_stage(stage: &EnhancedPipelineStage) -> Result<(), ExecutionError> {
        if stage.tasks.is_empty() {
            return Err(ExecutionError::Pipeline(format!(
                "Stage '{}' has no tasks",
                stage.name
            )));
        }

        if stage.parallelism == 0 {
            return Err(ExecutionError::Pipeline(format!(
                "Stage '{}' has invalid parallelism: {}",
                stage.name, stage.parallelism
            )));
        }

        if stage.timeout_seconds == 0 {
            return Err(ExecutionError::Pipeline(format!(
                "Stage '{}' has invalid timeout: {}",
                stage.name, stage.timeout_seconds
            )));
        }

        Ok(())
    }
}

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_enhanced_stage_builder() {
        let stage = EnhancedStageBuilder::new("Test Stage", StageType::DataProcessing)
            .with_parallelism(2)
            .with_timeout(600)
            .build();

        assert_eq!(stage.name, "Test Stage");
        assert_eq!(stage.parallelism, 2);
        assert_eq!(stage.timeout_seconds, 600);
    }

    #[test]
    fn test_pipeline_builder() {
        let stage = PipelineStage::new("Test Stage", "test");
        let pipeline = PipelineBuilder::new("test-pipeline", "Test Pipeline")
            .with_type("etl")
            .add_stage(stage)
            .build();

        assert_eq!(pipeline.name, "Test Pipeline");
        assert_eq!(pipeline.pipeline_type, "etl");
        assert_eq!(pipeline.stages.len(), 1);
        assert_eq!(pipeline.stages[0].timeout_seconds, 300);
    }

    #[test]
    fn test_pipeline_validation() {
        let mut pipeline = ExecutionPipeline::new("test-pipeline", "Test Pipeline", "test");

        // Add valid stage
        let mut stage = PipelineStage::new("Stage 1", "test");
        let task = Task::new(
            "task-1",
            "Test Task",
            "test",
            TaskPriority::Normal,
            serde_json::json!({}),
        );
        stage.add_task(task);
        pipeline.stages.push(stage.clone());

        // Add stage with dependency
        let mut dependent_stage = PipelineStage::new("Stage 2", "test");
        dependent_stage.dependencies.push(stage.name.clone());
        let task2 = Task::new(
            "task-2",
            "Test Task 2",
            "test",
            TaskPriority::Normal,
            serde_json::json!({}),
        );
        dependent_stage.add_task(task2);
        pipeline.stages.push(dependent_stage);

        // Validation should pass
        assert!(PipelineValidator::validate_pipeline(&pipeline).is_ok());

        // Add duplicate stage name
        let duplicate_stage = PipelineStage::new("Stage 1", "analysis");
        pipeline.stages.push(duplicate_stage);

        // Validation should fail
        assert!(PipelineValidator::validate_pipeline(&pipeline).is_err());
    }

    #[tokio::test]
    async fn test_enhanced_pipeline_executor() {
        let framework = ExecutionFramework::new(ExecutionConfig::default());
        let executor = EnhancedPipelineExecutor::new(framework);

        let mut pipeline = ExecutionPipeline::new("test-pipeline", "Test Pipeline", "test");

        let mut stage = PipelineStage::new("Test Stage", "test");
        let task = Task::new(
            "task-1",
            "Test Task",
            "test",
            TaskPriority::Normal,
            serde_json::json!({}),
        );
        stage.add_task(task);

        pipeline.stages.push(stage);

        let result = executor.execute_pipeline(&mut pipeline).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_parallel_pipeline_executor() {
        let framework = ExecutionFramework::new(ExecutionConfig::default());
        let executor = ParallelPipelineExecutor::new(framework);

        let mut stages = vec![
            EnhancedStageBuilder::new("Stage 1", StageType::DataProcessing)
                .add_task(Task::new(
                    "t1",
                    "Task1",
                    "test",
                    TaskPriority::Normal,
                    serde_json::json!({}),
                ))
                .with_parallelism(2)
                .build(),
        ];

        let result = executor.execute_enhanced_pipeline(&mut stages).await;
        assert!(result.is_ok());
        assert!(result.unwrap().success);
    }

    #[test]
    fn test_execution_metrics() {
        let mut metrics = ExecutionMetrics::new();

        metrics.record_task(true, 100);
        metrics.record_task(false, 200);
        metrics.record_task(true, 150);

        assert_eq!(metrics.total_tasks, 3);
        assert_eq!(metrics.successful_tasks, 2);
        assert_eq!(metrics.failed_tasks, 1);
        assert!((metrics.error_rate - 0.333).abs() < 0.01);
    }

    #[test]
    fn test_enhanced_stage_to_base_conversion() {
        let enhanced = EnhancedStageBuilder::new("Test", StageType::Validation)
            .add_task(Task::new(
                "t1",
                "Task1",
                "test",
                TaskPriority::Normal,
                serde_json::json!({}),
            ))
            .with_timeout(500)
            .build();

        let base: PipelineStage = PipelineStage::from(enhanced.clone());

        assert_eq!(base.name, enhanced.name);
        assert_eq!(base.stage_type, enhanced.stage_type.to_string());
        assert_eq!(base.timeout_seconds, enhanced.timeout_seconds);
    }

    #[test]
    fn test_stage_type_to_string() {
        assert_eq!(StageType::DataProcessing.to_string(), "data_processing");
        assert_eq!(StageType::Validation.to_string(), "validation");
        assert_eq!(
            StageType::Custom("custom".to_string()).to_string(),
            "custom"
        );
    }

    #[test]
    fn test_enhanced_stage_validation() {
        let stage = EnhancedStageBuilder::new("Test", StageType::DataProcessing)
            .add_task(Task::new(
                "t1",
                "Task1",
                "test",
                TaskPriority::Normal,
                serde_json::json!({}),
            ))
            .with_parallelism(1)
            .with_timeout(300)
            .build();

        assert!(PipelineValidator::validate_enhanced_stage(&stage).is_ok());

        // Test invalid parallelism
        let invalid_stage = EnhancedStageBuilder::new("Test", StageType::DataProcessing)
            .add_task(Task::new(
                "t1",
                "Task1",
                "test",
                TaskPriority::Normal,
                serde_json::json!({}),
            ))
            .with_parallelism(0)
            .build();

        assert!(PipelineValidator::validate_enhanced_stage(&invalid_stage).is_err());
    }
}
