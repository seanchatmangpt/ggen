//! Swarm coordinator - orchestrates multi-agent execution
//!
//! The coordinator manages the execution flow between agents, handles
//! dependencies, and ensures proper sequencing of operations.

use crate::error::{GgenAiError, Result};
use crate::swarm::{
    SwarmAgent, SwarmContext, SwarmInput, SwarmResult, GeneratedArtifact,
    ExecutionMetrics, AgentInput, AgentOutput, SwarmStatus, UltrathinkSwarm,
    SwarmConfig, PerformanceThresholds
};
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::{RwLock, Semaphore};
use tokio::time::{timeout, Duration};
use tracing::debug;

/// Swarm coordinator for managing agent execution
#[derive(Debug)]
pub struct SwarmCoordinator {
    /// Execution semaphore for concurrency control
    execution_semaphore: Arc<Semaphore>,
    /// Execution pipeline configuration
    pipeline_config: PipelineConfig,
}

/// Pipeline configuration for swarm execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PipelineConfig {
    /// Enable parallel execution
    pub enable_parallel: bool,
    /// Maximum parallel operations
    pub max_parallel_operations: usize,
    /// Enable failure recovery
    pub enable_failure_recovery: bool,
    /// Maximum retry attempts for failed operations
    pub max_retry_attempts: u32,
    /// Execution timeout per agent (seconds)
    pub agent_timeout_seconds: u64,
}

/// Execution pipeline for coordinating agent workflows
#[derive(Debug)]
pub struct ExecutionPipeline {
    /// Pipeline stages
    pub stages: Vec<PipelineStage>,
    /// Stage dependencies
    pub dependencies: HashMap<String, Vec<String>>,
    /// Stage configurations
    pub stage_configs: HashMap<String, StageConfig>,
}

/// Pipeline stage definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PipelineStage {
    /// Stage name
    pub name: String,
    /// Agent responsible for this stage
    pub agent: String,
    /// Stage priority
    pub priority: u32,
    /// Stage dependencies
    pub dependencies: Vec<String>,
    /// Stage configuration
    pub config: StageConfig,
}

/// Stage configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StageConfig {
    /// Enable this stage
    pub enabled: bool,
    /// Stage timeout (seconds)
    pub timeout_seconds: u64,
    /// Retry attempts for this stage
    pub retry_attempts: u32,
    /// Stage-specific parameters
    pub parameters: HashMap<String, String>,
}

/// Execution context for a specific pipeline run
#[derive(Debug, Clone)]
pub struct ExecutionContext {
    /// Pipeline being executed
    pub pipeline: ExecutionPipeline,
    /// Current stage being executed
    pub current_stage: Option<String>,
    /// Stage execution results
    pub stage_results: HashMap<String, StageResult>,
    /// Overall execution status
    pub status: ExecutionStatus,
    /// Execution start time
    pub start_time: std::time::Instant,
}

/// Stage execution result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StageResult {
    /// Stage name
    pub stage_name: String,
    /// Execution status
    pub status: StageStatus,
    /// Execution duration (ms)
    pub duration_ms: u64,
    /// Generated artifacts
    pub artifacts: Vec<GeneratedArtifact>,
    /// Error details if failed
    pub error: Option<String>,
    /// Stage metadata
    pub metadata: HashMap<String, String>,
}

/// Stage execution status
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum StageStatus {
    /// Stage not yet executed
    Pending,
    /// Stage currently executing
    Running,
    /// Stage completed successfully
    Completed,
    /// Stage failed
    Failed,
    /// Stage skipped due to dependency failure
    Skipped,
}

/// Overall execution status
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ExecutionStatus {
    /// Execution not started
    NotStarted,
    /// Execution in progress
    InProgress,
    /// Execution completed successfully
    Completed,
    /// Execution failed
    Failed,
    /// Execution cancelled
    Cancelled,
}

/// Execution plan for a swarm operation
#[derive(Debug, Clone)]
pub struct ExecutionPlan {
    /// Ordered stages to execute
    pub stages: Vec<String>,
    /// Parallel execution groups
    pub parallel_groups: Vec<Vec<String>>,
    /// Total estimated execution time
    pub estimated_duration_ms: u64,
    /// Resource requirements
    pub resource_requirements: ResourceRequirements,
}

/// Resource requirements for execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceRequirements {
    /// Estimated memory usage (MB)
    pub memory_mb: u64,
    /// Estimated CPU cores needed
    pub cpu_cores: u32,
    /// Network bandwidth estimate
    pub network_bandwidth_mbps: u32,
}

impl SwarmCoordinator {
    /// Create a new swarm coordinator
    pub fn new() -> Self {
        Self {
            execution_semaphore: Arc::new(Semaphore::new(10)), // Default max concurrent operations
            pipeline_config: PipelineConfig {
                enable_parallel: true,
                max_parallel_operations: 5,
                enable_failure_recovery: true,
                max_retry_attempts: 3,
                agent_timeout_seconds: 30,
            },
        }
    }

    /// Execute swarm with given input
    pub async fn execute_swarm(
        &self,
        agents: &Arc<RwLock<HashMap<String, Box<dyn SwarmAgent>>>>,
        context: &Arc<RwLock<SwarmContext>>,
        input: SwarmInput,
    ) -> Result<SwarmResult> {
        let start_time = std::time::Instant::now();

        // Create default pipeline for autonomous execution
        let pipeline = self.create_autonomous_pipeline(&input).await?;

        // Create execution context
        let mut exec_context = ExecutionContext {
            pipeline,
            current_stage: None,
            stage_results: HashMap::new(),
            status: ExecutionStatus::InProgress,
            start_time,
        };

        // Execute pipeline
        let result = self.execute_pipeline(agents, context, &mut exec_context, &input).await?;

        // Calculate final metrics
        let _execution_time = start_time.elapsed().as_millis() as u64;
        let metrics = ExecutionMetrics {
            total_operations: exec_context.stage_results.len() as u64,
            successful_operations: exec_context.stage_results.values()
                .filter(|r| matches!(r.status, StageStatus::Completed))
                .count() as u64,
            failed_operations: exec_context.stage_results.values()
                .filter(|r| matches!(r.status, StageStatus::Failed))
                .count() as u64,
            avg_execution_time_ms: if exec_context.stage_results.is_empty() {
                0.0
            } else {
                exec_context.stage_results.values()
                    .map(|r| r.duration_ms as f64)
                    .sum::<f64>() / exec_context.stage_results.len() as f64
            },
            memory_usage_mb: 0.0, // Would need actual memory tracking
        };

        Ok(SwarmResult {
            success: matches!(exec_context.status, ExecutionStatus::Completed),
            artifacts: result,
            metrics,
            error: if matches!(exec_context.status, ExecutionStatus::Failed) {
                Some("Pipeline execution failed".to_string())
            } else {
                None
            },
        })
    }

    /// Create autonomous pipeline for given input
    async fn create_autonomous_pipeline(&self, input: &SwarmInput) -> Result<ExecutionPipeline> {
        let mut stages = Vec::new();
        let mut dependencies = HashMap::new();

        // Define standard autonomous pipeline stages
        let pipeline_stages = vec![
            PipelineStage {
                name: "event_monitoring".to_string(),
                agent: "event_monitor".to_string(),
                priority: 1,
                dependencies: vec![],
                config: StageConfig {
                    enabled: true,
                    timeout_seconds: 10,
                    retry_attempts: 2,
                    parameters: HashMap::new(),
                },
            },
            PipelineStage {
                name: "graph_extension".to_string(),
                agent: "graph_extender".to_string(),
                priority: 2,
                dependencies: vec!["event_monitoring".to_string()],
                config: StageConfig {
                    enabled: true,
                    timeout_seconds: 30,
                    retry_attempts: 3,
                    parameters: HashMap::new(),
                },
            },
            PipelineStage {
                name: "validation".to_string(),
                agent: "validator".to_string(),
                priority: 3,
                dependencies: vec!["graph_extension".to_string()],
                config: StageConfig {
                    enabled: true,
                    timeout_seconds: 15,
                    retry_attempts: 2,
                    parameters: HashMap::new(),
                },
            },
            PipelineStage {
                name: "template_generation".to_string(),
                agent: "template_generator".to_string(),
                priority: 4,
                dependencies: vec!["validation".to_string()],
                config: StageConfig {
                    enabled: true,
                    timeout_seconds: 45,
                    retry_attempts: 3,
                    parameters: HashMap::new(),
                },
            },
            PipelineStage {
                name: "code_generation".to_string(),
                agent: "code_generator".to_string(),
                priority: 5,
                dependencies: vec!["template_generation".to_string()],
                config: StageConfig {
                    enabled: true,
                    timeout_seconds: 60,
                    retry_attempts: 3,
                    parameters: HashMap::new(),
                },
            },
            PipelineStage {
                name: "quality_assurance".to_string(),
                agent: "quality_assurance".to_string(),
                priority: 6,
                dependencies: vec!["code_generation".to_string()],
                config: StageConfig {
                    enabled: true,
                    timeout_seconds: 20,
                    retry_attempts: 2,
                    parameters: HashMap::new(),
                },
            },
        ];

        // Build dependency graph
        for stage in &pipeline_stages {
            dependencies.insert(stage.name.clone(), stage.dependencies.clone());
        }

        Ok(ExecutionPipeline {
            stages: pipeline_stages,
            dependencies,
            stage_configs: HashMap::new(), // Would populate with actual configs
        })
    }

    /// Execute the pipeline
    async fn execute_pipeline(
        &self,
        agents: &Arc<RwLock<HashMap<String, Box<dyn SwarmAgent>>>>,
        context: &Arc<RwLock<SwarmContext>>,
        exec_context: &mut ExecutionContext,
        input: &SwarmInput,
    ) -> Result<Vec<GeneratedArtifact>> {
        let mut artifacts = Vec::new();

        // Execute stages in dependency order
        for stage in &exec_context.pipeline.stages {
            if !stage.config.enabled {
                continue;
            }

            // Check if dependencies are satisfied
            if !self.are_dependencies_satisfied(stage, exec_context)? {
                exec_context.stage_results.insert(stage.name.clone(), StageResult {
                    stage_name: stage.name.clone(),
                    status: StageStatus::Skipped,
                    duration_ms: 0,
                    artifacts: vec![],
                    error: Some("Dependencies not satisfied".to_string()),
                    metadata: HashMap::new(),
                });
                continue;
            }

            // Execute stage
            let stage_result = self.execute_stage(agents, context, stage, input).await?;

            exec_context.stage_results.insert(stage.name.clone(), stage_result.clone());

            match stage_result.status {
                StageStatus::Completed => {
                    artifacts.extend(stage_result.artifacts);
                }
                StageStatus::Failed => {
                    if self.pipeline_config.enable_failure_recovery {
                        // Try to recover from failure
                        if let Some(recovered) = self.attempt_recovery(agents, context, stage, input).await? {
                            artifacts.extend(recovered);
                        } else {
                            exec_context.status = ExecutionStatus::Failed;
                            return Ok(artifacts);
                        }
                    } else {
                        exec_context.status = ExecutionStatus::Failed;
                        return Ok(artifacts);
                    }
                }
                _ => {}
            }
        }

        exec_context.status = ExecutionStatus::Completed;
        Ok(artifacts)
    }

    /// Execute a single stage
    async fn execute_stage(
        &self,
        agents: &Arc<RwLock<HashMap<String, Box<dyn SwarmAgent>>>>,
        context: &Arc<RwLock<SwarmContext>>,
        stage: &PipelineStage,
        input: &SwarmInput,
    ) -> Result<StageResult> {
        let start_time = std::time::Instant::now();

        // Get the agent for this stage
        let agents_read = agents.read().await;
        let agent = agents_read.get(&stage.agent)
            .ok_or_else(|| GgenAiError::internal(&format!("Agent {} not found", stage.agent)))?;

        // Prepare agent input
        let agent_input = AgentInput {
            data: serde_json::to_value(input)?,
            input_type: "swarm_execution".to_string(),
            source_agent: None,
            context: input.parameters.clone(),
        };

        // Execute with timeout
        let execution_result = timeout(
            Duration::from_secs(stage.config.timeout_seconds),
            agent.execute(&context.read().await, agent_input),
        ).await;

        let duration_ms = start_time.elapsed().as_millis() as u64;

        match execution_result {
            Ok(Ok(agent_output)) => {
                // Convert agent output to artifacts
                let artifacts = self.convert_agent_output_to_artifacts(&agent_output, &stage.agent)?;

                Ok(StageResult {
                    stage_name: stage.name.clone(),
                    status: StageStatus::Completed,
                    duration_ms,
                    artifacts,
                    error: None,
                    metadata: agent_output.metadata,
                })
            }
            Ok(Err(e)) => {
                Ok(StageResult {
                    stage_name: stage.name.clone(),
                    status: StageStatus::Failed,
                    duration_ms,
                    artifacts: vec![],
                    error: Some(e.to_string()),
                    metadata: HashMap::new(),
                })
            }
            Err(_) => {
                Ok(StageResult {
                    stage_name: stage.name.clone(),
                    status: StageStatus::Failed,
                    duration_ms,
                    artifacts: vec![],
                    error: Some("Stage execution timed out".to_string()),
                    metadata: HashMap::new(),
                })
            }
        }
    }

    /// Check if stage dependencies are satisfied
    fn are_dependencies_satisfied(&self, stage: &PipelineStage, exec_context: &ExecutionContext) -> Result<bool> {
        for dep in &stage.dependencies {
            if let Some(dep_result) = exec_context.stage_results.get(dep) {
                if !matches!(dep_result.status, StageStatus::Completed) {
                    return Ok(false);
                }
            } else {
                return Ok(false);
            }
        }
        Ok(true)
    }

    /// Attempt to recover from stage failure
    async fn attempt_recovery(
        &self,
        agents: &Arc<RwLock<HashMap<String, Box<dyn SwarmAgent>>>>,
        context: &Arc<RwLock<SwarmContext>>,
        stage: &PipelineStage,
        input: &SwarmInput,
    ) -> Result<Option<Vec<GeneratedArtifact>>> {
        // Simple recovery: retry once with modified parameters
        let retry_stage = PipelineStage {
            config: StageConfig {
                retry_attempts: 1,
                ..stage.config.clone()
            },
            ..stage.clone()
        };

        let retry_result = self.execute_stage(agents, context, &retry_stage, input).await?;

        if matches!(retry_result.status, StageStatus::Completed) {
            Ok(Some(retry_result.artifacts))
        } else {
            Ok(None)
        }
    }

    /// Convert agent output to generated artifacts
    fn convert_agent_output_to_artifacts(&self, output: &AgentOutput, agent_name: &str) -> Result<Vec<GeneratedArtifact>> {
        let mut artifacts = Vec::new();

        // Try to extract artifacts from output data
        if let serde_json::Value::Array(array) = &output.data {
            for item in array {
                if let Ok(artifact) = serde_json::from_value::<GeneratedArtifact>(item.clone()) {
                    artifacts.push(artifact);
                }
            }
        } else if let Ok(artifact) = serde_json::from_value::<GeneratedArtifact>(output.data.clone()) {
            artifacts.push(artifact);
        }

        // If no structured artifacts, create one from the output
        if artifacts.is_empty() {
            artifacts.push(GeneratedArtifact {
                artifact_type: output.output_type.clone(),
                content: output.data.to_string(),
                source_agent: agent_name.to_string(),
                timestamp: chrono::Utc::now().to_rfc3339(),
                quality_score: 0.8, // Default quality score
            });
        }

        Ok(artifacts)
    }
}

impl Default for SwarmCoordinator {
    fn default() -> Self {
        Self::new()
    }
}

impl UltrathinkSwarm {
    /// Create a swarm with default autonomous configuration
    pub fn autonomous() -> Self {
        let config = SwarmConfig {
            max_concurrent_agents: 10,
            agent_timeout_seconds: 60,
            learning_enabled: true,
            autonomous_mode: true,
            performance_thresholds: PerformanceThresholds {
                max_execution_time_ms: 10000,
                max_memory_usage_mb: 500,
                min_success_rate: 0.95,
            },
        };

        Self::new(config)
    }

    /// Create a swarm for development/testing
    pub fn development() -> Self {
        let config = SwarmConfig {
            max_concurrent_agents: 3,
            agent_timeout_seconds: 30,
            learning_enabled: false,
            autonomous_mode: false,
            performance_thresholds: PerformanceThresholds {
                max_execution_time_ms: 5000,
                max_memory_usage_mb: 100,
                min_success_rate: 0.8,
            },
        };

        Self::new(config)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::swarm::agents::MockAgent;

    #[tokio::test]
    async fn test_swarm_coordinator_creation() {
        let coordinator = SwarmCoordinator::new();
        assert_eq!(coordinator.execution_semaphore.available_permits(), 10);
    }

    #[tokio::test]
    async fn test_autonomous_swarm_creation() {
        let swarm = UltrathinkSwarm::autonomous();
        assert!(swarm.config.autonomous_mode);
        assert!(swarm.config.learning_enabled);
        assert_eq!(swarm.config.max_concurrent_agents, 10);
    }

    #[tokio::test]
    async fn test_development_swarm_creation() {
        let swarm = UltrathinkSwarm::development();
        assert!(!swarm.config.autonomous_mode);
        assert!(!swarm.config.learning_enabled);
        assert_eq!(swarm.config.max_concurrent_agents, 3);
    }
}
