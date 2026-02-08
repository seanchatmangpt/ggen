// Unified execution framework for 90% semantic convergence
use crate::types::*;
use crate::error::*;
use crate::metrics::MetricsCollector;
use async_trait::async_trait;
use std::collections::HashMap;
use chrono::{DateTime, Utc};
use uuid::Uuid;
use serde::{Serialize, Deserialize};

// ============================================================================
// EXECUTION FRAMEWORK
// ============================================================================

/// Unified execution framework that works across different agent types
#[derive(Debug, Clone)]
pub struct ExecutionFramework {
    config: ExecutionConfig,
    agents: HashMap<AgentId, Box<dyn UnifiedAgentTrait>>,
    workflows: HashMap<WorkflowId, Workflow>,
    pipelines: HashMap<PipelineId, ExecutionPipeline>,
    metrics: MetricsCollector,
    error_handler: ErrorHandler,
}

/// Configuration for the execution framework
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExecutionConfig {
    pub max_concurrent_agents: usize,
    pub max_concurrent_tasks: usize,
    pub default_timeout_seconds: u64,
    pub retry_attempts: u32,
    pub enable_metrics: bool,
    pub enable_retries: bool,
    pub convergence_threshold: f64,
    pub metadata: HashMap<String, serde_json::Value>,
}

impl Default for ExecutionConfig {
    fn default() -> Self {
        Self {
            max_concurrent_agents: 100,
            max_concurrent_tasks: 1000,
            default_timeout_seconds: 300,
            retry_attempts: 3,
            enable_metrics: true,
            enable_retries: true,
            convergence_threshold: 0.9,
            metadata: HashMap::new(),
        }
    }
}

impl ExecutionFramework {
    /// Create a new execution framework
    pub fn new(config: ExecutionConfig) -> Self {
        Self {
            config,
            agents: HashMap::new(),
            workflows: HashMap::new(),
            pipelines: HashMap::new(),
            metrics: MetricsCollector::new(1000),
            error_handler: ErrorHandler::new(1000),
        }
    }

    /// Register a new agent with the framework
    pub fn register_agent(&mut self, agent: Box<dyn UnifiedAgentTrait>) -> Result<(), ExecutionError> {
        if self.agents.len() >= self.config.max_concurrent_agents {
            return Err(ExecutionError::Agent("Maximum number of agents reached".to_string()));
        }

        let agent_id = agent.get_id().to_string();
        self.agents.insert(agent_id, agent);
        Ok(())
    }

    /// Create and register a workflow
    pub fn create_workflow(&mut self, name: &str, workflow_type: &str) -> Result<WorkflowId, ExecutionError> {
        let workflow_id = Uuid::new_v4().to_string();
        let workflow = Workflow::new(&workflow_id, name, workflow_type);
        self.workflows.insert(workflow_id.clone(), workflow);
        Ok(workflow_id)
    }

    /// Create and register a pipeline
    pub fn create_pipeline(&mut self, name: &str, pipeline_type: &str) -> Result<PipelineId, ExecutionError> {
        let pipeline_id = Uuid::new_v4().to_string();
        let pipeline = ExecutionPipeline::new(&pipeline_id, name, pipeline_type);
        self.pipelines.insert(pipeline_id.clone(), pipeline);
        Ok(pipeline_id)
    }

    /// Execute a task within the framework
    pub async fn execute_task(&mut self, task: Task) -> Result<TaskResult, ExecutionError> {
        // Find an available agent
        let agent_id = self.find_available_agent_index(&task)?;

        // Execute the task
        let result = self.agents.get_mut(&agent_id)
            .ok_or_else(|| ExecutionError::Agent("Agent not found".to_string()))?
            .execute_task(task.clone())
            .await?;

        // Update metrics
        if self.config.enable_metrics {
            self.metrics.record_metrics(PerformanceMetrics {
                timestamp: Utc::now(),
                execution_duration_ms: result.execution_time_ms,
                throughput_per_second: 0.0, // Will be calculated based on overall throughput
                success_rate: result.success as i32 as f64,
                error_rate: (!result.success) as i32 as f64,
                resource_usage: result.resources_used.clone(),
                memory_usage_mb: result.resources_used.memory_mb,
                cpu_usage_percent: result.resources_used.cpu_percent,
                disk_usage_percent: 0.0,
                network_io_mb: 0,
            });
        }

        Ok(result)
    }

    /// Execute a workflow with multiple tasks
    pub async fn execute_workflow(&mut self, workflow_id: &str) -> Result<WorkflowResult, ExecutionError> {
        let workflow = self.workflows.get_mut(workflow_id)
            .ok_or_else(|| ExecutionError::Workflow("Workflow not found".to_string()))?;

        let mut tasks = workflow.tasks.clone();
        let mut results: Vec<TaskExecutionResult> = Vec::new();
        let mut success_count = 0;

        // Execute tasks in dependency order
        while let Some(task) = tasks.pop() {
            // Check dependencies
            let mut dependencies_met = true;
            for dep_id in &task.dependencies {
                let dep_result = results.iter().find(|r| r.task_id == *dep_id);
                if dep_result.is_none() || !dep_result.as_ref().unwrap().result.success {
                    dependencies_met = false;
                    break;
                }
            }

            if dependencies_met {
                let task_id = task.id.clone();
                let result = self.execute_task(task).await?;
                results.push(TaskExecutionResult {
                    task_id,
                    result,
                });

                if result.success {
                    success_count += 1;
                }
            } else {
                // Requeue task for later execution
                tasks.insert(0, task);
            }
        }

        // Calculate overall success rate
        let success_rate = success_count as f64 / results.len().max(1) as f64;

        // Check convergence threshold
        if success_rate < self.config.convergence_threshold {
            return Err(ExecutionError::Convergence(
                format!("Convergence threshold not met: {:.2} < {:.2}",
                    success_rate, self.config.convergence_threshold)
            ));
        }

        // Overall success is true if all tasks completed successfully
        let success = success_count == results.len();

        Ok(WorkflowResult {
            workflow_id: workflow_id.to_string(),
            success,
            success_rate,
            total_tasks: results.len(),
            completed_tasks: success_count,
            execution_time_ms: 0, // Calculate based on actual timing
            results,
        })
    }

    /// Find an available agent for task execution
    fn find_available_agent_index(&self, _task: &Task) -> Result<String, ExecutionError> {
        // Simple round-robin selection for now
        // In production, this would consider agent capabilities, load, etc.
        for (id, agent) in &self.agents {
            if agent.is_available() {
                return Ok(id.clone());
            }
        }

        Err(ExecutionError::Agent("No available agents found".to_string()))
    }

    /// Get framework metrics
    pub fn get_metrics(&self) -> Option<PerformanceMetrics> {
        self.metrics.get_average_metrics()
    }

    /// Get error statistics
    pub fn get_error_stats(&self) -> ErrorStats {
        let errors = self.error_handler.get_errors();
        let error_count = errors.len();
        let error_types: HashMap<String, usize> = errors.iter()
            .map(|e| (e.error_type.clone(), 1))
            .collect();

        ErrorStats {
            total_errors: error_count,
            error_types,
            recent_errors: errors.iter().take(10).cloned().collect(),
        }
    }
}

/// Workflow definition and execution
#[derive(Debug, Clone)]
pub struct Workflow {
    pub id: WorkflowId,
    pub name: String,
    pub workflow_type: String,
    pub tasks: Vec<Task>,
    pub created_at: DateTime<Utc>,
    pub status: WorkflowStatus,
}

#[derive(Debug, Clone, PartialEq)]
pub enum WorkflowStatus {
    Created,
    Running,
    Completed,
    Failed(String),
    Cancelled,
}

impl Workflow {
    pub fn new(id: &str, name: &str, workflow_type: &str) -> Self {
        Self {
            id: id.to_string(),
            name: name.to_string(),
            workflow_type: workflow_type.to_string(),
            tasks: Vec::new(),
            created_at: Utc::now(),
            status: WorkflowStatus::Created,
        }
    }

    pub fn add_task(&mut self, task: Task) {
        self.tasks.push(task);
    }
}

/// Result of workflow execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkflowResult {
    pub workflow_id: WorkflowId,
    pub success: bool,
    pub success_rate: f64,
    pub total_tasks: usize,
    pub completed_tasks: usize,
    pub execution_time_ms: u64,
    pub results: Vec<TaskExecutionResult>,
}

/// Individual task execution result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TaskExecutionResult {
    pub task_id: TaskId,
    pub result: TaskResult,
}

/// Error statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ErrorStats {
    pub total_errors: usize,
    pub error_types: HashMap<String, usize>,
    pub recent_errors: Vec<ErrorMetadata>,
}

// ============================================================================
// UNIFIED AGENT TRAIT
// ============================================================================

/// Unified agent trait that works across different agent types
#[async_trait]
pub trait UnifiedAgentTrait: Send + Sync {
    /// Get agent ID
    fn get_id(&self) -> &str;

    /// Get agent name
    fn get_name(&self) -> &str;

    /// Get agent capabilities
    fn get_capabilities(&self) -> &[String];

    /// Check if agent is available for tasks
    fn is_available(&self) -> bool;

    /// Get agent health status
    fn get_health(&self) -> &AgentHealth;

    /// Execute a single task
    async fn execute_task(&mut self, task: Task) -> Result<TaskResult, ExecutionError>;

    /// Update agent configuration
    async fn update_config(&mut self, config: AgentConfiguration) -> Result<(), ExecutionError>;

    /// Get agent metrics
    fn get_metrics(&self) -> &AgentMetrics;

    /// Start the agent
    async fn start(&mut self) -> Result<(), ExecutionError>;

    /// Stop the agent
    async fn stop(&mut self) -> Result<(), ExecutionError>;

    /// Get agent status
    fn get_status(&self) -> &AgentStatus;
}

/// Default agent implementation
pub struct DefaultAgent {
    id: AgentId,
    name: String,
    capabilities: Vec<String>,
    status: AgentStatus,
    health: AgentHealth,
    metrics: AgentMetrics,
    config: AgentConfiguration,
}

impl DefaultAgent {
    pub fn new(id: &str, name: &str, capabilities: Vec<String>) -> Self {
        Self {
            id: id.to_string(),
            name: name.to_string(),
            capabilities,
            status: AgentStatus::Idle,
            health: AgentHealth::new(),
            metrics: AgentMetrics::new(),
            config: AgentConfiguration::default(),
        }
    }
}

#[async_trait]
impl UnifiedAgentTrait for DefaultAgent {
    fn get_id(&self) -> &str {
        &self.id
    }

    fn get_name(&self) -> &str {
        &self.name
    }

    fn get_capabilities(&self) -> &[String] {
        &self.capabilities
    }

    fn is_available(&self) -> bool {
        matches!(self.status, AgentStatus::Idle | AgentStatus::Starting)
    }

    fn get_health(&self) -> &AgentHealth {
        &self.health
    }

    async fn execute_task(&mut self, task: Task) -> Result<TaskResult, ExecutionError> {
        // Update status
        self.status = AgentStatus::Running;

        // Simulate task execution
        tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

        // Generate result
        let result = TaskResult {
            success: true,
            output: Some(serde_json::json!({"status": "completed", "result": "success"})),
            error: None,
            execution_time_ms: 100,
            resources_used: ResourceUsage {
                cpu_percent: 10.0,
                memory_mb: 50,
                network_in_mb: 1,
                network_out_mb: 1,
            },
        };

        // Update metrics
        self.metrics.tasks_completed += 1;
        self.metrics.average_task_duration_ms = 100;

        // Update status
        self.status = AgentStatus::Idle;

        Ok(result)
    }

    async fn update_config(&mut self, config: AgentConfiguration) -> Result<(), ExecutionError> {
        self.config = config;
        Ok(())
    }

    fn get_metrics(&self) -> &AgentMetrics {
        &self.metrics
    }

    async fn start(&mut self) -> Result<(), ExecutionError> {
        self.status = AgentStatus::Starting;
        // Simulate startup
        tokio::time::sleep(tokio::time::Duration::from_millis(50)).await;
        self.status = AgentStatus::Idle;
        Ok(())
    }

    async fn stop(&mut self) -> Result<(), ExecutionError> {
        self.status = AgentStatus::Stopping;
        // Simulate shutdown
        tokio::time::sleep(tokio::time::Duration::from_millis(50)).await;
        self.status = AgentStatus::Stopped;
        Ok(())
    }

    fn get_status(&self) -> &AgentStatus {
        &self.status
    }
}

// ============================================================================
// PIPELINE DEFINITION
// ============================================================================

/// Execution pipeline for complex workflows
#[derive(Debug, Clone)]
pub struct ExecutionPipeline {
    pub id: PipelineId,
    pub name: String,
    pub pipeline_type: String,
    pub stages: Vec<PipelineStage>,
    pub status: PipelineStatus,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PipelineStatus {
    Created,
    Running,
    Completed,
    Failed(String),
    Cancelled,
}

impl ExecutionPipeline {
    pub fn new(id: &str, name: &str, pipeline_type: &str) -> Self {
        Self {
            id: id.to_string(),
            name: name.to_string(),
            pipeline_type: pipeline_type.to_string(),
            stages: Vec::new(),
            status: PipelineStatus::Created,
        }
    }

    pub fn add_stage(&mut self, stage: PipelineStage) {
        self.stages.push(stage);
    }

    pub async fn execute(&mut self, framework: &mut ExecutionFramework) -> Result<PipelineResult, ExecutionError> {
        self.status = PipelineStatus::Running;

        let mut results = Vec::new();
        let mut success_count = 0;

        for stage in &mut self.stages {
            let result = stage.execute(framework).await?;
            results.push(result);

            if result.success {
                success_count += 1;
            } else {
                // Stop execution on failed stage
                self.status = PipelineStatus::Failed(format!("Stage {} failed", stage.name));
                break;
            }
        }

        if success_count == self.stages.len() {
            self.status = PipelineStatus::Completed;
        }

        Ok(PipelineResult {
            pipeline_id: self.id.clone(),
            success: success_count == self.stages.len(),
            success_rate: success_count as f64 / self.stages.len() as f64,
            total_stages: self.stages.len(),
            completed_stages: success_count,
            results,
        })
    }
}

/// Pipeline stage definition
#[derive(Debug, Clone)]
pub struct PipelineStage {
    pub name: String,
    pub stage_type: String,
    pub tasks: Vec<Task>,
    pub dependencies: Vec<String>,
    pub timeout_seconds: u64,
}

impl PipelineStage {
    pub fn new(name: &str, stage_type: &str) -> Self {
        Self {
            name: name.to_string(),
            stage_type: stage_type.to_string(),
            tasks: Vec::new(),
            dependencies: Vec::new(),
            timeout_seconds: 300,
        }
    }

    pub fn add_task(&mut self, task: Task) {
        self.tasks.push(task);
    }

    async fn execute(&mut self, framework: &mut ExecutionFramework) -> Result<StageResult, ExecutionError> {
        let mut success_count = 0;
        let mut failed_count = 0;

        for task in &self.tasks {
            let result = framework.execute_task(task.clone()).await?;

            if result.success {
                success_count += 1;
            } else {
                failed_count += 1;
            }
        }

        Ok(StageResult {
            stage_name: self.name.clone(),
            success: failed_count == 0,
            success_rate: success_count as f64 / (success_count + failed_count) as f64,
            total_tasks: success_count + failed_count,
            successful_tasks: success_count,
            failed_tasks: failed_count,
        })
    }
}

/// Pipeline execution result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PipelineResult {
    pub pipeline_id: PipelineId,
    pub success: bool,
    pub success_rate: f64,
    pub total_stages: usize,
    pub completed_stages: usize,
    pub results: Vec<StageResult>,
}

/// Stage execution result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StageResult {
    pub stage_name: String,
    pub success: bool,
    pub success_rate: f64,
    pub total_tasks: usize,
    pub successful_tasks: usize,
    pub failed_tasks: usize,
}

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_framework_creation() {
        let config = ExecutionConfig::default();
        let framework = ExecutionFramework::new(config);

        assert_eq!(framework.agents.len(), 0);
        assert_eq!(framework.workflows.len(), 0);
        assert_eq!(framework.pipelines.len(), 0);
    }

    #[tokio::test]
    async fn test_agent_registration() {
        let mut framework = ExecutionFramework::new(ExecutionConfig::default());
        let agent = Box::new(DefaultAgent::new("test-agent", "Test Agent", vec!["test-capability".to_string()]));

        let result = framework.register_agent(agent);
        assert!(result.is_ok());
        assert_eq!(framework.agents.len(), 1);
    }

    #[tokio::test]
    async fn test_task_execution() {
        let mut framework = ExecutionFramework::new(ExecutionConfig::default());
        let agent = Box::new(DefaultAgent::new("test-agent", "Test Agent", vec!["test".to_string()]));

        framework.register_agent(agent).unwrap();

        let task = Task::new("task-1", "Test Task", "test", TaskPriority::Normal, serde_json::json!({}));
        let result = framework.execute_task(task).await;

        assert!(result.is_ok());
        assert!(result.unwrap().success);
    }

    #[tokio::test]
    async fn test_workflow_creation_and_execution() {
        let mut framework = ExecutionFramework::new(ExecutionConfig::default());
        let agent = Box::new(DefaultAgent::new("test-agent", "Test Agent", vec!["test".to_string()]));

        framework.register_agent(agent).unwrap();

        let workflow_id = framework.create_workflow("Test Workflow", "test").unwrap();
        let workflow = framework.workflows.get_mut(&workflow_id).unwrap();

        let task = Task::new("task-1", "Test Task", "test", TaskPriority::Normal, serde_json::json!({}));
        workflow.add_task(task);

        let result = framework.execute_workflow(&workflow_id).await;
        assert!(result.is_ok());
        assert!(result.unwrap().success);
    }

    #[tokio::test]
    async fn test_pipeline_execution() {
        let mut framework = ExecutionFramework::new(ExecutionConfig::default());
        let agent = Box::new(DefaultAgent::new("test-agent", "Test Agent", vec!["test".to_string()]));

        framework.register_agent(agent).unwrap();

        let pipeline = ExecutionPipeline::new("test-pipeline", "Test Pipeline", "test");
        let mut stage = PipelineStage::new("Stage 1", "test");

        let task = Task::new("task-1", "Test Task", "test", TaskPriority::Normal, serde_json::json!({}));
        stage.add_task(task);

        let result = pipeline.execute(&mut framework).await;
        assert!(result.is_ok());
        assert!(result.unwrap().success);
    }
}