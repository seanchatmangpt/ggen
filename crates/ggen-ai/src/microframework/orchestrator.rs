//! AgentOrchestrator - High-level API for 10-agent parallel execution
//!
//! The central coordinator for the microframework.

use super::agents::MicroAgent;
use super::progress::{ProgressTracker, TaskProgress};
use super::task_graph::TaskGraph;
use super::tasks::{Task, TaskResult, TaskStatus};
use super::MicroframeworkConfig;
use crate::error::{GgenAiError, Result};
use crate::hyper_concurrent::{
    AgentExecutionResult, CircuitBreaker, ConcurrencyMetrics, HyperConcurrentConfig,
    HyperConcurrentExecutor,
};
use dashmap::DashMap;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::{debug, info, warn};

/// Agent orchestrator for managing parallel agent execution
#[derive(Debug)]
pub struct AgentOrchestrator {
    /// Configuration
    config: MicroframeworkConfig,
    /// Hyper-concurrent executor
    executor: Arc<HyperConcurrentExecutor>,
    /// Registered agents
    agents: DashMap<String, Arc<dyn MicroAgent>>,
    /// Progress tracker
    progress: Arc<ProgressTracker>,
    /// Task graph for dependencies
    task_graph: Arc<RwLock<TaskGraph>>,
    /// Circuit breaker for agents
    circuit_breaker: Arc<CircuitBreaker>,
    /// Metrics
    metrics: Arc<ConcurrencyMetrics>,
    /// Total tasks executed
    total_tasks: AtomicU64,
    /// Successful tasks
    successful_tasks: AtomicU64,
}

impl AgentOrchestrator {
    /// Create a new orchestrator with default configuration
    pub fn new() -> Self {
        Self::with_config(MicroframeworkConfig::default())
    }

    /// Create with custom configuration
    pub fn with_config(config: MicroframeworkConfig) -> Self {
        let hyper_config = HyperConcurrentConfig {
            max_agents: config.max_agents,
            enable_work_stealing: config.enable_work_stealing,
            enable_circuit_breaker: config.enable_circuit_breaker,
            enable_backpressure: config.enable_backpressure,
            agent_timeout_secs: config.default_timeout_secs,
            circuit_breaker_threshold: 5,
            backpressure_queue_size: 100,
            metrics_enabled: config.enable_metrics,
        };

        Self {
            executor: Arc::new(HyperConcurrentExecutor::new(hyper_config)),
            agents: DashMap::new(),
            progress: Arc::new(ProgressTracker::new()),
            task_graph: Arc::new(RwLock::new(TaskGraph::new())),
            circuit_breaker: Arc::new(CircuitBreaker::new(5)),
            metrics: Arc::new(ConcurrencyMetrics::new()),
            total_tasks: AtomicU64::new(0),
            successful_tasks: AtomicU64::new(0),
            config,
        }
    }

    /// Create a builder for the orchestrator
    pub fn builder() -> super::builder::OrchestratorBuilder {
        super::builder::OrchestratorBuilder::new()
    }

    /// Register an agent
    pub fn register_agent<A: MicroAgent + 'static>(&self, agent: A) {
        let name = agent.name().to_string();
        self.agents.insert(name.clone(), Arc::new(agent));
        debug!("Registered agent: {}", name);
    }

    /// Get registered agent count
    pub fn agent_count(&self) -> usize {
        self.agents.len()
    }

    /// Execute a single task
    pub async fn execute(&self, task: Task) -> Result<TaskResult> {
        let results = self.execute_batch(vec![task]).await?;
        results
            .into_iter()
            .next()
            .ok_or_else(|| GgenAiError::internal("No result returned"))
    }

    /// Execute multiple tasks in parallel (up to 10)
    pub async fn execute_batch(&self, tasks: Vec<Task>) -> Result<Vec<TaskResult>> {
        let task_count = tasks.len();
        info!("Executing batch of {} tasks", task_count);

        // Track tasks
        for task in &tasks {
            self.progress.track_task(task);
        }

        // Prepare execution closures
        let mut executions: Vec<(String, _)> = Vec::new();

        for task in tasks {
            let task_id = task.id.clone();
            let agents = self.agents.clone();
            let progress = Arc::clone(&self.progress);
            let metrics = Arc::clone(&self.metrics);

            let execution = move || {
                let task = task;
                let agents = agents;
                let progress = progress;
                let metrics = metrics;

                async move {
                    let start = std::time::Instant::now();

                    // Find an agent that can handle this task
                    let agent = agents
                        .iter()
                        .find(|entry| entry.value().can_handle(&task))
                        .map(|entry| Arc::clone(entry.value()));

                    let result = match agent {
                        Some(agent) => {
                            progress.start_task(&task.id);
                            match agent.execute(&task).await {
                                Ok(result) => {
                                    progress.complete_task(&task.id, result.is_success());
                                    metrics.record_success(start.elapsed().as_millis() as u64);
                                    Ok(result)
                                }
                                Err(e) => {
                                    progress.complete_task(&task.id, false);
                                    metrics.record_failure(start.elapsed().as_millis() as u64);
                                    Ok(TaskResult::failure(
                                        task.id.clone(),
                                        e.to_string(),
                                        start.elapsed().as_millis() as u64,
                                    ))
                                }
                            }
                        }
                        None => {
                            progress.complete_task(&task.id, false);
                            Ok(TaskResult::failure(
                                task.id.clone(),
                                "No agent found for task type".to_string(),
                                0,
                            ))
                        }
                    };

                    result
                }
            };

            executions.push((task_id, execution));
        }

        // Execute in parallel using hyper-concurrent executor
        let execution_results = self.executor.execute_parallel(executions).await;

        // Convert results
        let mut results = Vec::with_capacity(task_count);
        for exec_result in execution_results {
            match exec_result.value {
                Some(task_result) => {
                    if task_result.is_success() {
                        self.successful_tasks.fetch_add(1, Ordering::Relaxed);
                    }
                    results.push(task_result);
                }
                None => {
                    results.push(TaskResult::failure(
                        exec_result.agent_id,
                        exec_result.error.unwrap_or_else(|| "Unknown error".to_string()),
                        exec_result.duration_ms,
                    ));
                }
            }
        }

        self.total_tasks.fetch_add(task_count as u64, Ordering::Relaxed);

        info!(
            "Batch complete: {}/{} successful",
            results.iter().filter(|r| r.is_success()).count(),
            task_count
        );

        Ok(results)
    }

    /// Execute tasks respecting dependencies
    pub async fn execute_with_dependencies(&self, tasks: Vec<Task>) -> Result<Vec<TaskResult>> {
        let mut graph = self.task_graph.write().await;

        // Add all tasks to graph
        for task in &tasks {
            graph.add_task(task.clone());
        }

        // Get execution order
        let execution_order = graph.topological_sort()?;
        drop(graph);

        // Execute in waves based on dependencies
        let mut all_results = Vec::new();
        let mut completed_ids: Vec<String> = Vec::new();

        for wave in execution_order {
            // Filter tasks ready to run
            let ready_tasks: Vec<Task> = tasks
                .iter()
                .filter(|t| wave.contains(&t.id) && t.can_run(&completed_ids))
                .cloned()
                .collect();

            if !ready_tasks.is_empty() {
                let results = self.execute_batch(ready_tasks).await?;

                for result in &results {
                    if result.is_success() {
                        completed_ids.push(result.task_id.clone());
                    }
                }

                all_results.extend(results);
            }
        }

        Ok(all_results)
    }

    /// Spawn tasks using shorthand
    pub async fn spawn_batch(&self, tasks: Vec<Task>) -> Result<Vec<TaskResult>> {
        self.execute_batch(tasks).await
    }

    /// Get current progress
    pub fn progress(&self) -> &ProgressTracker {
        &self.progress
    }

    /// Get orchestrator statistics
    pub fn statistics(&self) -> OrchestratorStats {
        let executor_metrics = self.executor.metrics();

        OrchestratorStats {
            registered_agents: self.agents.len(),
            total_tasks: self.total_tasks.load(Ordering::Relaxed),
            successful_tasks: self.successful_tasks.load(Ordering::Relaxed),
            active_executions: executor_metrics.active_executions,
            success_rate: if self.total_tasks.load(Ordering::Relaxed) > 0 {
                self.successful_tasks.load(Ordering::Relaxed) as f64
                    / self.total_tasks.load(Ordering::Relaxed) as f64
            } else {
                1.0
            },
            avg_execution_time_ms: executor_metrics.avg_execution_time_ms,
        }
    }

    /// Reset statistics
    pub fn reset_stats(&self) {
        self.total_tasks.store(0, Ordering::Relaxed);
        self.successful_tasks.store(0, Ordering::Relaxed);
        self.progress.reset();
    }
}

impl Default for AgentOrchestrator {
    fn default() -> Self {
        Self::new()
    }
}

/// Orchestrator statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OrchestratorStats {
    /// Number of registered agents
    pub registered_agents: usize,
    /// Total tasks executed
    pub total_tasks: u64,
    /// Successful tasks
    pub successful_tasks: u64,
    /// Currently active executions
    pub active_executions: usize,
    /// Success rate (0.0 - 1.0)
    pub success_rate: f64,
    /// Average execution time in milliseconds
    pub avg_execution_time_ms: f64,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::microframework::agents::CodeGenAgent;

    #[tokio::test]
    async fn test_orchestrator_creation() {
        let orchestrator = AgentOrchestrator::new();
        assert_eq!(orchestrator.agent_count(), 0);
    }

    #[tokio::test]
    async fn test_agent_registration() {
        let orchestrator = AgentOrchestrator::new();
        orchestrator.register_agent(CodeGenAgent::new("coder"));
        assert_eq!(orchestrator.agent_count(), 1);
    }

    #[tokio::test]
    async fn test_single_task_execution() {
        let orchestrator = AgentOrchestrator::new();
        orchestrator.register_agent(CodeGenAgent::new("coder"));

        let task = Task::code_gen("Generate a struct");
        let result = orchestrator.execute(task).await.unwrap();
        assert!(result.is_success());
    }

    #[tokio::test]
    async fn test_batch_execution() {
        let orchestrator = AgentOrchestrator::new();
        orchestrator.register_agent(CodeGenAgent::new("coder"));

        let tasks = vec![
            Task::code_gen("Generate struct 1"),
            Task::code_gen("Generate struct 2"),
            Task::code_gen("Generate struct 3"),
        ];

        let results = orchestrator.execute_batch(tasks).await.unwrap();
        assert_eq!(results.len(), 3);
        assert!(results.iter().all(|r| r.is_success()));
    }

    #[tokio::test]
    async fn test_statistics() {
        let orchestrator = AgentOrchestrator::new();
        orchestrator.register_agent(CodeGenAgent::new("coder"));

        let task = Task::code_gen("Generate code");
        orchestrator.execute(task).await.unwrap();

        let stats = orchestrator.statistics();
        assert_eq!(stats.total_tasks, 1);
        assert_eq!(stats.successful_tasks, 1);
        assert_eq!(stats.registered_agents, 1);
    }
}
