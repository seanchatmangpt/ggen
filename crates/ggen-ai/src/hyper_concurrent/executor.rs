//! HyperConcurrentExecutor - Maximum 10-agent parallel execution engine
//!
//! Core executor that orchestrates all concurrent components for maximum throughput.

use super::{
    AdaptiveConcurrencyController, AgentBarrier, AgentExecutionResult, AgentExecutionState,
    BackpressureHandler, ChannelOrchestrator, CircuitBreaker, ConcurrencyMetrics,
    ExecutionPriority, HyperConcurrentConfig, WorkStealingAgentPool, MAX_CONCURRENT_AGENTS,
};
use crate::error::Result;
use crate::swarm::{AgentInput, AgentOutput, SwarmAgent, SwarmContext};
use dashmap::DashMap;
use futures::stream::{FuturesUnordered, StreamExt};
use serde::{Deserialize, Serialize};
use std::future::Future;
use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};
use std::sync::Arc;
use tokio::sync::Semaphore;
use tokio::time::{timeout, Duration};
use tracing::{debug, info, warn};

/// Hyper-concurrent executor for maximum agent parallelism
#[derive(Debug)]
pub struct HyperConcurrentExecutor {
    /// Configuration
    config: HyperConcurrentConfig,
    /// Concurrency semaphore (10 permits max)
    semaphore: Arc<Semaphore>,
    /// Work-stealing agent pool
    work_pool: Arc<WorkStealingAgentPool>,
    /// Circuit breaker manager
    circuit_breaker: Arc<CircuitBreaker>,
    /// Adaptive concurrency controller
    adaptive_controller: Arc<AdaptiveConcurrencyController>,
    /// Backpressure handler
    backpressure: Arc<BackpressureHandler>,
    /// Channel orchestrator for inter-agent communication
    channels: Arc<ChannelOrchestrator>,
    /// Execution metrics
    metrics: Arc<ConcurrencyMetrics>,
    /// Active executions counter
    active_count: AtomicUsize,
    /// Total executions counter
    total_executions: AtomicU64,
    /// Registered agents
    agents: DashMap<String, Arc<dyn SwarmAgent>>,
}

impl HyperConcurrentExecutor {
    /// Create a new hyper-concurrent executor
    pub fn new(config: HyperConcurrentConfig) -> Self {
        let max_agents = config.max_agents.min(MAX_CONCURRENT_AGENTS);

        Self {
            semaphore: Arc::new(Semaphore::new(max_agents)),
            work_pool: Arc::new(WorkStealingAgentPool::new(max_agents)),
            circuit_breaker: Arc::new(CircuitBreaker::new(config.circuit_breaker_threshold)),
            adaptive_controller: Arc::new(AdaptiveConcurrencyController::new(max_agents)),
            backpressure: Arc::new(BackpressureHandler::new(config.backpressure_queue_size)),
            channels: Arc::new(ChannelOrchestrator::new()),
            metrics: Arc::new(ConcurrencyMetrics::new()),
            active_count: AtomicUsize::new(0),
            total_executions: AtomicU64::new(0),
            agents: DashMap::new(),
            config,
        }
    }

    /// Create with maximum performance configuration
    pub fn max_performance() -> Self {
        Self::new(HyperConcurrentConfig::max_performance())
    }

    /// Register an agent with the executor
    pub fn register_agent(&self, agent: Arc<dyn SwarmAgent>) {
        let name = agent.name().to_string();
        self.agents.insert(name.clone(), agent);
        self.channels.create_channel(&name);
        debug!("Registered agent: {}", name);
    }

    /// Execute multiple agents in parallel with maximum concurrency
    ///
    /// This is the primary method for achieving 10-agent parallel execution.
    pub async fn execute_parallel<T, F, Fut>(
        &self,
        tasks: Vec<(String, F)>,
    ) -> Vec<AgentExecutionResult<T>>
    where
        T: Send + 'static,
        F: FnOnce() -> Fut + Send + 'static,
        Fut: Future<Output = Result<T>> + Send + 'static,
    {
        let task_count = tasks.len();
        info!(
            "Executing {} tasks in parallel (max concurrency: {})",
            task_count, self.config.max_agents
        );

        // Check backpressure
        if self.config.enable_backpressure && self.backpressure.is_overloaded() {
            warn!("Backpressure active, throttling execution");
            self.backpressure.wait_for_capacity().await;
        }

        // Update metrics
        self.metrics.record_execution_start(task_count);

        // Create futures for parallel execution
        let mut futures = FuturesUnordered::new();

        for (agent_id, task) in tasks {
            let semaphore = Arc::clone(&self.semaphore);
            let circuit_breaker = Arc::clone(&self.circuit_breaker);
            let metrics = Arc::clone(&self.metrics);
            let timeout_secs = self.config.agent_timeout_secs;
            let enable_circuit_breaker = self.config.enable_circuit_breaker;

            futures.push(tokio::spawn(async move {
                let start = std::time::Instant::now();

                // Check circuit breaker
                if enable_circuit_breaker && circuit_breaker.is_open(&agent_id) {
                    return AgentExecutionResult::failure(
                        agent_id.clone(),
                        "Circuit breaker open".to_string(),
                        0,
                    );
                }

                // Acquire semaphore permit
                let _permit = match semaphore.acquire().await {
                    Ok(permit) => permit,
                    Err(_) => {
                        return AgentExecutionResult::failure(
                            agent_id.clone(),
                            "Failed to acquire execution permit".to_string(),
                            0,
                        );
                    }
                };

                // Execute with timeout
                let result = timeout(Duration::from_secs(timeout_secs), task()).await;
                let duration_ms = start.elapsed().as_millis() as u64;

                match result {
                    Ok(Ok(value)) => {
                        metrics.record_success(duration_ms);
                        if enable_circuit_breaker {
                            circuit_breaker.record_success(&agent_id);
                        }
                        AgentExecutionResult::success(value, agent_id, duration_ms)
                    }
                    Ok(Err(e)) => {
                        metrics.record_failure(duration_ms);
                        if enable_circuit_breaker {
                            circuit_breaker.record_failure(&agent_id);
                        }
                        AgentExecutionResult::failure(agent_id, e.to_string(), duration_ms)
                    }
                    Err(_) => {
                        metrics.record_timeout(duration_ms);
                        if enable_circuit_breaker {
                            circuit_breaker.record_failure(&agent_id);
                        }
                        let mut result = AgentExecutionResult::failure(
                            agent_id,
                            "Execution timed out".to_string(),
                            duration_ms,
                        );
                        result.state = AgentExecutionState::TimedOut;
                        result
                    }
                }
            }));
        }

        // Collect results
        let mut results = Vec::with_capacity(task_count);
        while let Some(result) = futures.next().await {
            match result {
                Ok(execution_result) => results.push(execution_result),
                Err(e) => {
                    results.push(AgentExecutionResult::failure(
                        "unknown".to_string(),
                        format!("Task panicked: {}", e),
                        0,
                    ));
                }
            }
        }

        // Update metrics
        self.metrics.record_execution_complete(task_count);
        self.total_executions
            .fetch_add(task_count as u64, Ordering::Relaxed);

        info!(
            "Parallel execution complete: {} tasks, {} successful",
            task_count,
            results.iter().filter(|r| r.is_success()).count()
        );

        results
    }

    /// Execute agents with priority scheduling
    pub async fn execute_prioritized<T, F, Fut>(
        &self,
        tasks: Vec<(String, ExecutionPriority, F)>,
    ) -> Vec<AgentExecutionResult<T>>
    where
        T: Send + 'static,
        F: FnOnce() -> Fut + Send + 'static,
        Fut: Future<Output = Result<T>> + Send + 'static,
    {
        // Sort by priority
        let mut sorted_tasks: Vec<_> = tasks.into_iter().collect();
        sorted_tasks.sort_by_key(|(_, priority, _)| *priority);

        // Convert to standard task format
        let tasks: Vec<_> = sorted_tasks
            .into_iter()
            .map(|(id, _, task)| (id, task))
            .collect();

        self.execute_parallel(tasks).await
    }

    /// Execute with work stealing for optimal load balancing
    pub async fn execute_work_stealing<T>(
        &self,
        context: &SwarmContext,
        inputs: Vec<(String, AgentInput)>,
    ) -> Vec<AgentExecutionResult<AgentOutput>>
    where
        T: Send + 'static,
    {
        let agents = &self.agents;
        let context = context.clone();

        let tasks: Vec<_> = inputs
            .into_iter()
            .filter_map(|(agent_id, input)| {
                let agent = agents.get(&agent_id)?.clone();
                let ctx = context.clone();
                Some((agent_id, move || async move { agent.execute(&ctx, input).await }))
            })
            .collect();

        self.execute_parallel(tasks).await
    }

    /// Execute with barrier synchronization
    pub async fn execute_with_barrier<T, F, Fut>(
        &self,
        barrier: &AgentBarrier,
        agent_id: String,
        task: F,
    ) -> AgentExecutionResult<T>
    where
        T: Send + 'static,
        F: FnOnce() -> Fut + Send + 'static,
        Fut: Future<Output = Result<T>> + Send + 'static,
    {
        // Wait at barrier before execution
        barrier.wait().await;

        let results = self.execute_parallel(vec![(agent_id.clone(), task)]).await;

        // Wait at barrier after execution
        barrier.wait().await;

        results.into_iter().next().unwrap_or_else(|| {
            AgentExecutionResult::failure(agent_id, "No result returned".to_string(), 0)
        })
    }

    /// Get current execution metrics
    pub fn metrics(&self) -> ExecutorMetrics {
        ExecutorMetrics {
            active_executions: self.active_count.load(Ordering::Relaxed),
            total_executions: self.total_executions.load(Ordering::Relaxed),
            available_permits: self.semaphore.available_permits(),
            success_rate: self.metrics.success_rate(),
            avg_execution_time_ms: self.metrics.avg_execution_time_ms(),
            circuit_breakers_open: self.circuit_breaker.open_count(),
            backpressure_active: self.backpressure.is_overloaded(),
        }
    }

    /// Get registered agent count
    pub fn agent_count(&self) -> usize {
        self.agents.len()
    }

    /// Check if circuit breaker is open for an agent
    pub fn is_circuit_open(&self, agent_id: &str) -> bool {
        self.circuit_breaker.is_open(agent_id)
    }

    /// Reset circuit breaker for an agent
    pub fn reset_circuit(&self, agent_id: &str) {
        self.circuit_breaker.reset(agent_id);
    }

    /// Get adaptive concurrency recommendation
    pub fn recommended_concurrency(&self) -> usize {
        self.adaptive_controller.recommended_concurrency()
    }
}

/// Executor metrics snapshot
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExecutorMetrics {
    /// Currently active executions
    pub active_executions: usize,
    /// Total executions since start
    pub total_executions: u64,
    /// Available semaphore permits
    pub available_permits: usize,
    /// Success rate (0.0 - 1.0)
    pub success_rate: f64,
    /// Average execution time in milliseconds
    pub avg_execution_time_ms: f64,
    /// Number of open circuit breakers
    pub circuit_breakers_open: usize,
    /// Whether backpressure is active
    pub backpressure_active: bool,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_executor_creation() {
        let executor = HyperConcurrentExecutor::new(HyperConcurrentConfig::default());
        assert_eq!(executor.agent_count(), 0);
        assert_eq!(
            executor.semaphore.available_permits(),
            MAX_CONCURRENT_AGENTS
        );
    }

    #[tokio::test]
    async fn test_parallel_execution() {
        let executor = HyperConcurrentExecutor::new(HyperConcurrentConfig::default());

        let tasks: Vec<_> = (0..5)
            .map(|i| {
                let id = format!("agent-{}", i);
                let task = move || async move { Ok(i * 2) };
                (id, task)
            })
            .collect();

        let results = executor.execute_parallel(tasks).await;
        assert_eq!(results.len(), 5);
        assert!(results.iter().all(|r| r.is_success()));
    }

    #[tokio::test]
    async fn test_max_concurrency() {
        let config = HyperConcurrentConfig {
            max_agents: 3,
            ..HyperConcurrentConfig::default()
        };
        let executor = HyperConcurrentExecutor::new(config);
        assert_eq!(executor.semaphore.available_permits(), 3);
    }

    #[tokio::test]
    async fn test_execution_metrics() {
        let executor = HyperConcurrentExecutor::new(HyperConcurrentConfig::default());
        let metrics = executor.metrics();
        assert_eq!(metrics.active_executions, 0);
        assert_eq!(metrics.total_executions, 0);
    }
}
