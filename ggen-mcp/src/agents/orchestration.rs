//! Orchestration Agent - Agent Coordination and Workflow Management
//!
//! This agent coordinates all other agents in the MCP server, managing workflows,
//! dependencies, and ensuring proper execution order and error handling.
//!
//! # Orchestration Patterns
//!
//! ## Workflow Management
//! - **Sequential Execution** - Execute agents in order
//! - **Parallel Execution** - Execute agents concurrently
//! - **Conditional Execution** - Execute based on conditions
//! - **Error Handling** - Handle failures gracefully
//!
//! ## Agent Coordination
//! - **Dependency Management** - Manage agent dependencies
//! - **Resource Sharing** - Coordinate shared resources
//! - **State Management** - Maintain workflow state
//! - **Communication** - Facilitate inter-agent communication
//!
//! ## Workflow Types
//! - **Linear Workflows** - Simple sequential execution
//! - **Branching Workflows** - Conditional execution paths
//! - **Parallel Workflows** - Concurrent execution
//! - **Loop Workflows** - Repeated execution
//! - **Error Recovery Workflows** - Failure handling

use crate::agents::{Agent, AgentMetadata, AgentStatus, AgentId, AgentCoordinator};
use crate::error::{GgenMcpError, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use uuid::Uuid;
use chrono::{DateTime, Utc};

/// Workflow definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkflowDefinition {
    pub id: Uuid,
    pub name: String,
    pub description: String,
    pub steps: Vec<WorkflowStep>,
    pub dependencies: HashMap<String, Vec<String>>,
    pub error_handling: ErrorHandlingStrategy,
    pub timeout_seconds: u64,
    pub retry_count: u32,
    pub created_at: chrono::DateTime<Utc>,
    pub last_updated: chrono::DateTime<Utc>,
}

/// Workflow step
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkflowStep {
    pub id: String,
    pub name: String,
    pub agent_id: AgentId,
    pub operation: String,
    pub parameters: serde_json::Value,
    pub step_type: StepType,
    pub condition: Option<String>,
    pub timeout_seconds: Option<u64>,
    pub retry_count: Option<u32>,
}

/// Step types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum StepType {
    Sequential,
    Parallel,
    Conditional,
    Loop,
    ErrorRecovery,
}

/// Error handling strategy
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ErrorHandlingStrategy {
    StopOnError,
    ContinueOnError,
    RetryOnError,
    SkipOnError,
}

/// Workflow execution result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkflowExecutionResult {
    pub workflow_id: Uuid,
    pub execution_id: Uuid,
    pub status: WorkflowStatus,
    pub start_time: chrono::DateTime<Utc>,
    pub end_time: Option<chrono::DateTime<Utc>>,
    pub duration_ms: Option<u64>,
    pub step_results: Vec<StepExecutionResult>,
    pub errors: Vec<WorkflowError>,
    pub metadata: HashMap<String, serde_json::Value>,
}

/// Workflow status
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum WorkflowStatus {
    Pending,
    Running,
    Completed,
    Failed,
    Cancelled,
    Timeout,
}

/// Step execution result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StepExecutionResult {
    pub step_id: String,
    pub agent_id: AgentId,
    pub status: StepStatus,
    pub start_time: chrono::DateTime<Utc>,
    pub end_time: Option<chrono::DateTime<Utc>>,
    pub duration_ms: Option<u64>,
    pub result: Option<serde_json::Value>,
    pub error: Option<String>,
    pub retry_count: u32,
}

/// Step status
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum StepStatus {
    Pending,
    Running,
    Completed,
    Failed,
    Skipped,
    Timeout,
}

/// Workflow error
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkflowError {
    pub step_id: String,
    pub agent_id: AgentId,
    pub error_type: String,
    pub message: String,
    pub timestamp: chrono::DateTime<Utc>,
    pub recoverable: bool,
}

/// Orchestration Agent implementation
pub struct OrchestrationAgent {
    id: AgentId,
    coordinator: Arc<AgentCoordinator>,
    workflows: HashMap<Uuid, WorkflowDefinition>,
    executions: Vec<WorkflowExecutionResult>,
    active_executions: HashMap<Uuid, WorkflowExecutionResult>,
}

impl OrchestrationAgent {
    pub fn new(coordinator: Arc<AgentCoordinator>) -> Self {
        let mut agent = Self {
            id: Uuid::new_v4(),
            coordinator,
            workflows: HashMap::new(),
            executions: Vec::new(),
            active_executions: HashMap::new(),
        };

        // Initialize default workflows
        agent.initialize_default_workflows();

        agent
    }

    /// Initialize default workflows
    fn initialize_default_workflows(&mut self) {
        // MCP tool execution workflow
        let mcp_tool_workflow = WorkflowDefinition {
            id: Uuid::new_v4(),
            name: "mcp_tool_execution".to_string(),
            description: "Standard workflow for executing MCP tools".to_string(),
            steps: vec![
                WorkflowStep {
                    id: "validate_input".to_string(),
                    name: "Validate Input".to_string(),
                    agent_id: Uuid::new_v4(), // Will be replaced with actual ValidationAgent ID
                    operation: "validate_schema".to_string(),
                    parameters: serde_json::json!({}),
                    step_type: StepType::Sequential,
                    condition: None,
                    timeout_seconds: Some(5),
                    retry_count: Some(3),
                },
                WorkflowStep {
                    id: "security_check".to_string(),
                    name: "Security Check".to_string(),
                    agent_id: Uuid::new_v4(), // Will be replaced with actual SecurityAgent ID
                    operation: "validate_input".to_string(),
                    parameters: serde_json::json!({}),
                    step_type: StepType::Sequential,
                    condition: None,
                    timeout_seconds: Some(10),
                    retry_count: Some(2),
                },
                WorkflowStep {
                    id: "execute_tool".to_string(),
                    name: "Execute Tool".to_string(),
                    agent_id: Uuid::new_v4(), // Will be replaced with actual IntegrationAgent ID
                    operation: "execute_cli_command".to_string(),
                    parameters: serde_json::json!({}),
                    step_type: StepType::Sequential,
                    condition: None,
                    timeout_seconds: Some(30),
                    retry_count: Some(3),
                },
                WorkflowStep {
                    id: "monitor_performance".to_string(),
                    name: "Monitor Performance".to_string(),
                    agent_id: Uuid::new_v4(), // Will be replaced with actual PerformanceAgent ID
                    operation: "record_metrics".to_string(),
                    parameters: serde_json::json!({}),
                    step_type: StepType::Parallel,
                    condition: None,
                    timeout_seconds: Some(5),
                    retry_count: Some(1),
                },
            ],
            dependencies: HashMap::from([
                ("security_check".to_string(), vec!["validate_input".to_string()]),
                ("execute_tool".to_string(), vec!["security_check".to_string()]),
                ("monitor_performance".to_string(), vec!["execute_tool".to_string()]),
            ]),
            error_handling: ErrorHandlingStrategy::RetryOnError,
            timeout_seconds: 60,
            retry_count: 3,
            created_at: Utc::now(),
            last_updated: Utc::now(),
        };

        self.workflows.insert(mcp_tool_workflow.id, mcp_tool_workflow);
    }

    /// Execute a workflow
    pub async fn execute_workflow(&mut self, workflow_id: Uuid, input: serde_json::Value) -> Result<WorkflowExecutionResult> {
        let workflow = self.workflows.get(&workflow_id)
            .ok_or_else(|| GgenMcpError::InvalidParameter(format!("Unknown workflow: {}", workflow_id)))?;

        let execution_id = Uuid::new_v4();
        let start_time = Utc::now();

        let mut execution = WorkflowExecutionResult {
            workflow_id,
            execution_id,
            status: WorkflowStatus::Running,
            start_time,
            end_time: None,
            duration_ms: None,
            step_results: Vec::new(),
            errors: Vec::new(),
            metadata: HashMap::from([
                ("input".to_string(), input),
                ("workflow_name".to_string(), serde_json::Value::String(workflow.name.clone())),
            ]),
        };

        // Store active execution
        self.active_executions.insert(execution_id, execution.clone());

        // Execute workflow steps
        match self.execute_workflow_steps(workflow, &mut execution).await {
            Ok(_) => {
                execution.status = WorkflowStatus::Completed;
            }
            Err(e) => {
                execution.status = WorkflowStatus::Failed;
                execution.errors.push(WorkflowError {
                    step_id: "workflow".to_string(),
                    agent_id: self.id,
                    error_type: "WorkflowError".to_string(),
                    message: e.to_string(),
                    timestamp: Utc::now(),
                    recoverable: false,
                });
            }
        }

        // Finalize execution
        execution.end_time = Some(Utc::now());
        execution.duration_ms = Some(
            execution.end_time.unwrap()
                .signed_duration_since(execution.start_time)
                .num_milliseconds() as u64
        );

        // Move from active to completed
        self.active_executions.remove(&execution_id);
        self.executions.push(execution.clone());

        // Keep only last 1000 executions
        if self.executions.len() > 1000 {
            self.executions.remove(0);
        }

        Ok(execution)
    }

    /// Execute workflow steps
    async fn execute_workflow_steps(&self, workflow: &WorkflowDefinition, execution: &mut WorkflowExecutionResult) -> Result<()> {
        let mut completed_steps = std::collections::HashSet::new();
        let mut remaining_steps = workflow.steps.clone();

        while !remaining_steps.is_empty() {
            let mut steps_to_execute = Vec::new();

            // Find steps that can be executed (dependencies satisfied)
            for step in &remaining_steps {
                let dependencies_satisfied = workflow.dependencies
                    .get(&step.id)
                    .map(|deps| deps.iter().all(|dep| completed_steps.contains(dep)))
                    .unwrap_or(true);

                if dependencies_satisfied {
                    steps_to_execute.push(step.clone());
                }
            }

            if steps_to_execute.is_empty() {
                return Err(GgenMcpError::ExecutionFailed("Circular dependency detected".to_string()));
            }

            // Execute steps based on their type
            for step in steps_to_execute {
                let step_result = self.execute_workflow_step(&step, execution).await;
                
                execution.step_results.push(step_result.clone());
                completed_steps.insert(step.id.clone());

                // Handle step failure based on error handling strategy
                if step_result.status == StepStatus::Failed {
                    match workflow.error_handling {
                        ErrorHandlingStrategy::StopOnError => {
                            return Err(GgenMcpError::ExecutionFailed("Step failed, stopping workflow".to_string()));
                        }
                        ErrorHandlingStrategy::ContinueOnError => {
                            // Continue with next step
                        }
                        ErrorHandlingStrategy::RetryOnError => {
                            // Retry logic would be implemented here
                        }
                        ErrorHandlingStrategy::SkipOnError => {
                            // Skip this step and continue
                        }
                    }
                }

                // Remove completed step from remaining steps
                remaining_steps.retain(|s| s.id != step.id);
            }
        }

        Ok(())
    }

    /// Execute a single workflow step
    async fn execute_workflow_step(&self, step: &WorkflowStep, execution: &mut WorkflowExecutionResult) -> StepExecutionResult {
        let start_time = Utc::now();
        let mut retry_count = 0;
        let max_retries = step.retry_count.unwrap_or(3);

        loop {
            let step_result = StepExecutionResult {
                step_id: step.id.clone(),
                agent_id: step.agent_id,
                status: StepStatus::Running,
                start_time,
                end_time: None,
                duration_ms: None,
                result: None,
                error: None,
                retry_count,
            };

            // Simulate step execution
            let (success, result, error) = self.simulate_step_execution(step).await;

            let mut final_result = step_result;
            final_result.end_time = Some(Utc::now());
            final_result.duration_ms = Some(
                final_result.end_time.unwrap()
                    .signed_duration_since(final_result.start_time)
                    .num_milliseconds() as u64
            );

            if success {
                final_result.status = StepStatus::Completed;
                final_result.result = Some(result);
                return final_result;
            } else {
                final_result.status = StepStatus::Failed;
                final_result.error = Some(error);

                if retry_count < max_retries {
                    retry_count += 1;
                    // Wait before retry (exponential backoff)
                    tokio::time::sleep(tokio::time::Duration::from_millis(1000 * (retry_count as u64))).await;
                    continue;
                } else {
                    return final_result;
                }
            }
        }
    }

    /// Simulate step execution (placeholder for real agent execution)
    async fn simulate_step_execution(&self, step: &WorkflowStep) -> (bool, serde_json::Value, String) {
        // Simulate 90% success rate
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        
        let mut hasher = DefaultHasher::new();
        step.id.hash(&mut hasher);
        step.operation.hash(&mut hasher);
        let hash = hasher.finish();
        
        if hash % 10 < 9 {
            (true, serde_json::json!({"success": true}), "".to_string())
        } else {
            (false, serde_json::json!({}), "Simulated step failure".to_string())
        }
    }

    /// Get workflow definition
    pub fn get_workflow(&self, workflow_id: &Uuid) -> Option<&WorkflowDefinition> {
        self.workflows.get(workflow_id)
    }

    /// Get all workflows
    pub fn get_all_workflows(&self) -> &HashMap<Uuid, WorkflowDefinition> {
        &self.workflows
    }

    /// Get execution history
    pub fn get_execution_history(&self) -> &Vec<WorkflowExecutionResult> {
        &self.executions
    }

    /// Get active executions
    pub fn get_active_executions(&self) -> &HashMap<Uuid, WorkflowExecutionResult> {
        &self.active_executions
    }

    /// Cancel workflow execution
    pub fn cancel_execution(&mut self, execution_id: Uuid) -> Result<()> {
        if let Some(execution) = self.active_executions.get_mut(&execution_id) {
            execution.status = WorkflowStatus::Cancelled;
            execution.end_time = Some(Utc::now());
            execution.duration_ms = Some(
                execution.end_time.unwrap()
                    .signed_duration_since(execution.start_time)
                    .num_milliseconds() as u64
            );

            // Move to completed executions
            let completed_execution = execution.clone();
            self.active_executions.remove(&execution_id);
            self.executions.push(completed_execution);

            Ok(())
        } else {
            Err(GgenMcpError::InvalidParameter("Execution not found".to_string()))
        }
    }
}

#[async_trait::async_trait]
impl Agent for OrchestrationAgent {
    async fn initialize(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        tracing::info!("Orchestration Agent initialized with ID: {}", self.id);
        tracing::info!("Loaded {} workflows", self.workflows.len());
        Ok(())
    }

    async fn execute(&self, input: serde_json::Value) -> Result<serde_json::Value, Box<dyn std::error::Error>> {
        let operation = input.get("operation")
            .and_then(|v| v.as_str())
            .ok_or("Missing operation")?;

        let mut agent = OrchestrationAgent::new(self.coordinator.clone());
        
        let result = match operation {
            "execute_workflow" => {
                let workflow_id = input.get("workflow_id")
                    .and_then(|v| v.as_str())
                    .and_then(|s| Uuid::parse_str(s).ok())
                    .ok_or("Invalid workflow_id")?;
                let workflow_input = input.get("input")
                    .cloned()
                    .unwrap_or(serde_json::Value::Object(serde_json::Map::new()));
                
                serde_json::to_value(agent.execute_workflow(workflow_id, workflow_input).await?)?
            }
            "get_workflow" => {
                let workflow_id = input.get("workflow_id")
                    .and_then(|v| v.as_str())
                    .and_then(|s| Uuid::parse_str(s).ok())
                    .ok_or("Invalid workflow_id")?;
                serde_json::to_value(agent.get_workflow(&workflow_id))?
            }
            "get_execution_history" => {
                serde_json::to_value(agent.get_execution_history())?
            }
            "get_active_executions" => {
                serde_json::to_value(agent.get_active_executions())?
            }
            _ => return Err("Unknown operation".into()),
        };

        Ok(result)
    }

    fn metadata(&self) -> AgentMetadata {
        AgentMetadata {
            id: self.id,
            name: "OrchestrationAgent".to_string(),
            version: "1.0.0".to_string(),
            status: AgentStatus::Healthy,
            capabilities: vec![
                "workflow_management".to_string(),
                "agent_coordination".to_string(),
                "dependency_management".to_string(),
                "error_handling".to_string(),
            ],
            last_heartbeat: Utc::now(),
        }
    }

    async fn health_check(&self) -> AgentStatus {
        // Orchestration agent is always healthy unless explicitly failed
        AgentStatus::Healthy
    }

    async fn shutdown(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        tracing::info!("Orchestration Agent shutting down");
        tracing::info!("Executed {} workflows", self.executions.len());
        tracing::info!("Active executions: {}", self.active_executions.len());
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[tokio::test]
    async fn test_orchestration_agent_creation() {
        let coordinator = Arc::new(AgentCoordinator::new());
        let agent = OrchestrationAgent::new(coordinator);
        
        assert!(!agent.workflows.is_empty());
        assert_eq!(agent.executions.len(), 0);
    }

    #[tokio::test]
    async fn test_workflow_execution() {
        let coordinator = Arc::new(AgentCoordinator::new());
        let mut agent = OrchestrationAgent::new(coordinator);
        
        let workflow_id = agent.workflows.keys().next().unwrap().clone();
        let input = json!({"test": "data"});
        
        let result = agent.execute_workflow(workflow_id, input).await.unwrap();
        
        assert_eq!(result.status, WorkflowStatus::Completed);
        assert!(!result.step_results.is_empty());
    }

    #[tokio::test]
    async fn test_execution_cancellation() {
        let coordinator = Arc::new(AgentCoordinator::new());
        let mut agent = OrchestrationAgent::new(coordinator);
        
        let workflow_id = agent.workflows.keys().next().unwrap().clone();
        let input = json!({"test": "data"});
        
        let execution = agent.execute_workflow(workflow_id, input).await.unwrap();
        let execution_id = execution.execution_id;
        
        // Try to cancel completed execution (should fail)
        let result = agent.cancel_execution(execution_id);
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_agent_execution() {
        let coordinator = Arc::new(AgentCoordinator::new());
        let mut agent = OrchestrationAgent::new(coordinator);
        agent.initialize().await.unwrap();
        
        let workflow_id = agent.workflows.keys().next().unwrap().clone();
        let input = json!({
            "operation": "execute_workflow",
            "workflow_id": workflow_id.to_string(),
            "input": {"test": "data"}
        });
        
        let result = agent.execute(input).await.unwrap();
        let execution_result: WorkflowExecutionResult = serde_json::from_value(result).unwrap();
        
        assert_eq!(execution_result.status, WorkflowStatus::Completed);
    }
}
