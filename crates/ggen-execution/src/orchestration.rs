// Task orchestration for complex workflows
use crate::types::*;
use crate::error::*;
use crate::framework::*;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use tokio::sync::{Mutex, RwLock, mpsc};
use uuid::Uuid;
use chrono::{DateTime, Utc};

// ============================================================================
// ORCHESTRATION CONTEXT
// ============================================================================

/// Orchestration context for tracking workflow execution
#[derive(Debug, Clone)]
pub struct OrchestrationContext {
    pub workflow_id: WorkflowId,
    pub current_stage: usize,
    pub total_stages: usize,
    pub active_tasks: HashSet<TaskId>,
    pub completed_tasks: HashSet<TaskId>,
    pub failed_tasks: HashSet<TaskId>,
    pub task_results: HashMap<TaskId, TaskResult>,
    pub started_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
}

impl OrchestrationContext {
    pub fn new(workflow_id: &str, total_stages: usize) -> Self {
        Self {
            workflow_id: workflow_id.to_string(),
            current_stage: 0,
            total_stages,
            active_tasks: HashSet::new(),
            completed_tasks: HashSet::new(),
            failed_tasks: HashSet::new(),
            task_results: HashMap::new(),
            started_at: Utc::now(),
            updated_at: Utc::now(),
        }
    }

    pub fn update_status(&mut self) {
        self.updated_at = Utc::now();
    }

    pub fn can_start_task(&self, task_id: &str) -> bool {
        // Check if task dependencies are completed
        true // Simplified for now
    }

    pub fn mark_task_started(&mut self, task_id: &str) {
        self.active_tasks.insert(task_id.to_string());
        self.update_status();
    }

    pub fn mark_task_completed(&mut self, task_id: &str, result: TaskResult) {
        self.active_tasks.remove(task_id);
        self.completed_tasks.insert(task_id.to_string());
        self.task_results.insert(task_id.to_string(), result);
        self.update_status();
    }

    pub fn mark_task_failed(&mut self, task_id: &str, error: String) {
        self.active_tasks.remove(task_id);
        self.failed_tasks.insert(task_id.to_string());
        self.update_status();
    }

    pub fn get_progress(&self) -> f64 {
        let total = self.completed_tasks.len() + self.failed_tasks.len();
        if total == 0 {
            0.0
        } else {
            total as f64 / (self.completed_tasks.len() + self.failed_tasks.len()).max(1) as f64
        }
    }
}

// ============================================================================
// TASK ORCHESTRATOR
// ============================================================================

/// Task orchestrator for managing complex workflows
pub struct TaskOrchestrator {
    framework: Arc<Mutex<ExecutionFramework>>,
    contexts: HashMap<WorkflowId, Arc<Mutex<OrchestrationContext>>>,
    task_queue: mpsc::Sender<OrchestrationTask>,
    shutdown_signal: Arc<RwLock<bool>>,
    metrics: Arc<RwLock<OrchestrationMetrics>>,
}

/// Orchestration task for processing
#[derive(Debug, Clone)]
pub struct OrchestrationTask {
    pub workflow_id: WorkflowId,
    pub task_id: TaskId,
    pub priority: TaskPriority,
    pub retry_count: u32,
}

/// Orchestration metrics
#[derive(Debug, Clone)]
pub struct OrchestrationMetrics {
    pub total_workflows: u64,
    pub completed_workflows: u64,
    pub failed_workflows: u64,
    pub total_tasks: u64,
    pub successful_tasks: u64,
    pub failed_tasks: u64,
    pub average_workflow_duration_ms: u64,
    pub throughput_tasks_per_second: f64,
}

impl OrchestrationMetrics {
    pub fn new() -> Self {
        Self {
            total_workflows: 0,
            completed_workflows: 0,
            failed_workflows: 0,
            total_tasks: 0,
            successful_tasks: 0,
            failed_tasks: 0,
            average_workflow_duration_ms: 0,
            throughput_tasks_per_second: 0.0,
        }
    }
}

impl TaskOrchestrator {
    pub fn new(framework: ExecutionFramework) -> Self {
        let (task_queue, mut receiver) = mpsc::channel(1000);
        let framework_arc = Arc::new(Mutex::new(framework));
        let contexts = HashMap::new();
        let metrics = Arc::new(RwLock::new(OrchestrationMetrics::new()));
        let shutdown_signal = Arc::new(RwLock::new(false));

        // Start the task processor
        let framework_clone = framework_arc.clone();
        let contexts_clone = contexts.clone();
        let metrics_clone = metrics.clone();

        tokio::spawn(async move {
            while let Some(task) = receiver.recv().await {
                if *shutdown_signal.read().await {
                    break;
                }

                if let Err(e) = Self::process_orchestration_task(
                    &task,
                    &framework_clone,
                    &contexts_clone,
                    &metrics_clone,
                ).await {
                    eprintln!("Error processing orchestration task: {:?}", e);
                }
            }
        });

        Self {
            framework: framework_arc,
            contexts,
            task_queue,
            shutdown_signal,
            metrics,
        }
    }

    /// Start orchestrating a workflow
    pub async fn orchestrate_workflow(
        &mut self,
        workflow: Workflow,
        max_concurrent_tasks: usize,
    ) -> Result<WorkflowResult, ExecutionError> {
        let workflow_id = workflow.id.clone();
        let total_stages = workflow.tasks.len();

        // Create orchestration context
        let context = Arc::new(Mutex::new(OrchestrationContext::new(&workflow_id, total_stages)));
        self.contexts.insert(workflow_id.clone(), context.clone());

        // Update metrics
        let mut metrics = self.metrics.write().await;
        metrics.total_workflows += 1;

        // Queue tasks for orchestration
        for task in workflow.tasks {
            let orchestration_task = OrchestrationTask {
                workflow_id: workflow_id.clone(),
                task_id: task.id.clone(),
                priority: task.priority,
                retry_count: 0,
            };

            self.task_queue.send(orchestration_task).await
                .map_err(|_| ExecutionError::Workflow("Failed to queue task for orchestration".to_string()))?;
        }

        // Wait for workflow completion
        let start_time = Utc::now();
        loop {
            let context_guard = context.lock().await;
            let progress = context_guard.get_progress();

            if progress >= 1.0 {
                // Workflow completed
                let duration = Utc::now().timestamp_millis() - start_time.timestamp_millis();
                let successful_tasks = context_guard.completed_tasks.len();
                let failed_tasks = context_guard.failed_tasks.len();

                metrics.completed_workflows += 1;
                metrics.average_workflow_duration_ms = (metrics.average_workflow_duration_ms * metrics.completed_workflows + duration) / (metrics.completed_workflows + 1);
                metrics.successful_tasks += successful_tasks as u64;
                metrics.failed_tasks += failed_tasks as u64;

                let results: Vec<TaskExecutionResult> = context_guard.task_results.iter()
                    .map(|(task_id, result)| TaskExecutionResult {
                        task_id: task_id.clone(),
                        result: result.clone(),
                    })
                    .collect();

                return Ok(WorkflowResult {
                    workflow_id: workflow_id,
                    success: failed_tasks == 0,
                    success_rate: successful_tasks as f64 / (successful_tasks + failed_tasks).max(1) as f64,
                    total_tasks: successful_tasks + failed_tasks,
                    completed_tasks: successful_tasks,
                    execution_time_ms: duration as u64,
                    results,
                });
            }

            drop(context_guard);

            // Check for timeout
            if Utc::now().timestamp_millis() - start_time.timestamp_millis() > 300_000 {
                return Err(ExecutionError::Workflow("Workflow orchestration timeout".to_string()));
            }

            // Sleep before checking again
            tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;
        }
    }

    /// Process an orchestration task
    async fn process_orchestration_task(
        task: &OrchestrationTask,
        framework: &Arc<Mutex<ExecutionFramework>>,
        contexts: &HashMap<WorkflowId, Arc<Mutex<OrchestrationContext>>>,
        metrics: &Arc<RwLock<OrchestrationMetrics>>,
    ) -> Result<(), ExecutionError> {
        let workflow_id = &task.workflow_id;
        let task_id = &task.task_id;

        let context = contexts.get(workflow_id)
            .ok_or_else(|| ExecutionError::Workflow("Orchestration context not found".to_string()))?;

        let mut context_guard = context.lock().await;

        // Check if task can start
        if !context_guard.can_start_task(task_id) {
            return Ok(());
        }

        // Mark task as started
        context_guard.mark_task_started(task_id);

        // Get the task from the framework and execute it
        // This is simplified - in production, you'd need to track tasks in the context
        let mut framework_guard = framework.lock().await;

        // Execute the task (simplified)
        let task = Task::new(task_id, "Orchestrated Task", "orchestrated", TaskPriority::Normal, serde_json::json!({}));
        let result = framework_guard.execute_task(task).await?;

        // Update context with result
        if result.success {
            context_guard.mark_task_completed(task_id, result);
        } else {
            context_guard.mark_task_failed(task_id, result.error.unwrap_or_else(|| "Unknown error".to_string()));
        }

        Ok(())
    }

    /// Get orchestration metrics
    pub async fn get_metrics(&self) -> OrchestrationMetrics {
        self.metrics.read().await.clone()
    }

    /// Shutdown the orchestrator
    pub async fn shutdown(&mut self) {
        *self.shutdown_signal.write().await = true;
    }
}

// ============================================================================
// EXECUTION GRAPH
// ============================================================================

/// Execution graph for complex task dependencies
#[derive(Debug, Clone)]
pub struct ExecutionGraph {
    nodes: HashMap<TaskId, ExecutionNode>,
    edges: HashMap<TaskId, Vec<TaskId>>, // task_id -> dependent_task_ids
    entry_points: Vec<TaskId>,
    exit_points: Vec<TaskId>,
}

/// Execution node for a task
#[derive(Debug, Clone)]
pub struct ExecutionNode {
    pub task: Task,
    pub dependencies: Vec<TaskId>,
    dependents: Vec<TaskId>,
    pub state: ExecutionNodeState,
    pub retry_count: u32,
}

/// Execution node state
#[derive(Debug, Clone, PartialEq)]
pub enum ExecutionNodeState {
    Pending,
    Ready,
    Running,
    Completed,
    Failed,
    Cancelled,
    Retrying,
}

impl ExecutionGraph {
    pub fn new() -> Self {
        Self {
            nodes: HashMap::new(),
            edges: HashMap::new(),
            entry_points: Vec::new(),
            exit_points: Vec::new(),
        }
    }

    pub fn add_node(&mut self, task: Task) -> Result<(), ExecutionError> {
        if self.nodes.contains_key(&task.id) {
            return Err(ExecutionError::Task(
                format!("Task with ID {} already exists", task.id)
            ));
        }

        let node = ExecutionNode {
            task: task.clone(),
            dependencies: task.dependencies.clone(),
            dependents: Vec::new(),
            state: ExecutionNodeState::Pending,
            retry_count: 0,
        };

        self.nodes.insert(task.id.clone(), node);

        // Update edges
        for dep_id in &task.dependencies {
            self.edges.entry(dep_id.clone()).or_insert_with(Vec::new).push(task.id.clone());
        }

        Ok(())
    }

    pub fn build_execution_order(&self) -> Result<Vec<TaskId>, ExecutionError> {
        let mut order = Vec::new();
        let mut remaining_nodes = self.nodes.clone();
        let mut ready_nodes = Vec::new();

        // Find entry points (nodes with no dependencies)
        for (task_id, node) in &remaining_nodes {
            if node.dependencies.is_empty() {
                ready_nodes.push(task_id.clone());
            }
        }

        while !ready_nodes.is_empty() {
            let current_node_id = ready_nodes.pop()
                .ok_or_else(|| ExecutionError::Workflow("Circular dependency detected".to_string()))?;

            // Add to execution order
            order.push(current_node_id.clone());

            // Remove from remaining nodes
            remaining_nodes.remove(&current_node_id);

            // Find new ready nodes
            for (task_id, node) in &remaining_nodes {
                if node.dependencies.contains(&current_node_id) {
                    // Remove the dependency that was just completed
                    let mut remaining_deps = node.dependencies.clone();
                    remaining_deps.retain(|dep| dep != &current_node_id);

                    if remaining_deps.is_empty() {
                        ready_nodes.push(task_id.clone());
                    }
                }
            }
        }

        if !remaining_nodes.is_empty() {
            return Err(ExecutionError::Workflow("Circular dependency detected".to_string()));
        }

        Ok(order)
    }

    pub fn get_node_state(&self, task_id: &str) -> Option<ExecutionNodeState> {
        self.nodes.get(task_id).map(|n| n.state.clone())
    }

    pub fn update_node_state(&mut self, task_id: &str, new_state: ExecutionNodeState) -> Result<(), ExecutionError> {
        let node = self.nodes.get_mut(task_id)
            .ok_or_else(|| ExecutionError::Task(format!("Task not found: {}", task_id)))?;

        node.state = new_state.clone();

        // Update dependents
        if let Some(dependents) = self.edges.get(task_id) {
            for dependent_id in dependents {
                if let Some(dependent_node) = self.nodes.get_mut(dependent_id) {
                    // Check if all dependencies are completed
                    if new_state == ExecutionNodeState::Completed {
                        let all_deps_completed = dependent_node.dependencies.iter()
                            .all(|dep_id| {
                                if let Some(dep_node) = self.nodes.get(dep_id) {
                                    dep_node.state == ExecutionNodeState::Completed
                                } else {
                                    false
                                }
                            });

                        if all_deps_completed {
                            dependent_node.state = ExecutionNodeState::Ready;
                        }
                    }
                }
            }
        }

        Ok(())
    }
}

// ============================================================================
// ORCHESTRATION STRATEGIES
// ============================================================================

/// Orchestration strategies for different workflow patterns
pub enum OrchestrationStrategy {
    Sequential,
    Parallel,
    DependencyGraph,
    Staged,
    Custom(String),
}

/// Strategy implementation for orchestrating workflows
pub struct OrchestrationStrategyExecutor {
    strategy: OrchestrationStrategy,
}

impl OrchestrationStrategyExecutor {
    pub fn new(strategy: OrchestrationStrategy) -> Self {
        Self { strategy }
    }

    pub async fn execute_workflow(
        &self,
        workflow: &Workflow,
        orchestrator: &TaskOrchestrator,
    ) -> Result<WorkflowResult, ExecutionError> {
        match &self.strategy {
            OrchestrationStrategy::Sequential => {
                self.execute_sequential(workflow, orchestrator).await
            }
            OrchestrationStrategy::Parallel => {
                self.execute_parallel(workflow, orchestrator).await
            }
            OrchestrationStrategy::DependencyGraph => {
                self.execute_dependency_graph(workflow, orchestrator).await
            }
            OrchestrationStrategy::Staged => {
                self.execute_staged(workflow, orchestrator).await
            }
            OrchestrationStrategy::Custom(strategy_name) => {
                self.execute_custom(workflow, orchestrator, strategy_name).await
            }
        }
    }

    async fn execute_sequential(
        &self,
        workflow: &Workflow,
        orchestrator: &TaskOrchestrator,
    ) -> Result<WorkflowResult, ExecutionError> {
        let mut workflow_result = WorkflowResult {
            workflow_id: workflow.id.clone(),
            success: true,
            success_rate: 0.0,
            total_tasks: workflow.tasks.len(),
            completed_tasks: 0,
            execution_time_ms: 0,
            results: Vec::new(),
        };

        for task in &workflow.tasks {
            let task_result = orchestrator.framework.lock().await.execute_task(task.clone()).await?;

            workflow_result.results.push(TaskExecutionResult {
                task_id: task.id.clone(),
                result: task_result,
            });

            if !task_result.success {
                workflow_result.success = false;
            }
        }

        workflow_result.completed_tasks = workflow_result.results.len();
        workflow_result.success_rate = workflow_result.completed_tasks as f64 / workflow_result.total_tasks.max(1) as f64;

        Ok(workflow_result)
    }

    async fn execute_parallel(
        &self,
        workflow: &Workflow,
        orchestrator: &TaskOrchestrator,
    ) -> Result<WorkflowResult, ExecutionError> {
        let tasks = workflow.tasks.clone();
        let mut results = Vec::new();
        let mut success_count = 0;

        let mut futures = Vec::new();
        for task in &tasks {
            let framework = orchestrator.framework.clone();
            let task_clone = task.clone();
            let task_id = task.id.clone();

            futures.push(async move {
                let mut framework = framework.lock().await;
                (task_id, framework.execute_task(task_clone).await)
            });
        }

        // Execute all tasks in parallel
        let task_results = futures::future::join_all(futures).await;

        for result in task_results {
            match result {
                Ok((task_id, Ok(task_result))) => {
                    results.push(TaskExecutionResult {
                        task_id,
                        result: task_result,
                    });

                    if task_result.success {
                        success_count += 1;
                    }
                }
                Ok((task_id, Err(e))) => {
                    results.push(TaskExecutionResult {
                        task_id,
                        result: TaskResult {
                            success: false,
                            output: None,
                            error: Some(e.to_string()),
                            execution_time_ms: 0,
                            resources_used: ResourceUsage::default(),
                        },
                    });
                }
                Err(_) => {
                    results.push(TaskExecutionResult {
                        task_id: "unknown".to_string(),
                        result: TaskResult {
                            success: false,
                            output: None,
                            error: Some("Task execution failed".to_string()),
                            execution_time_ms: 0,
                            resources_used: ResourceUsage::default(),
                        },
                    });
                }
            }
        }

        Ok(WorkflowResult {
            workflow_id: workflow.id.clone(),
            success: success_count == tasks.len(),
            success_rate: success_count as f64 / tasks.len().max(1) as f64,
            total_tasks: tasks.len(),
            completed_tasks: tasks.len(),
            execution_time_ms: 0,
            results,
        })
    }

    async fn execute_dependency_graph(
        &self,
        workflow: &Workflow,
        orchestrator: &TaskOrchestrator,
    ) -> Result<WorkflowResult, ExecutionError> {
        let mut graph = ExecutionGraph::new();

        // Add all tasks to the graph
        for task in &workflow.tasks {
            graph.add_node(task.clone())?;
        }

        // Get execution order
        let execution_order = graph.build_execution_order()?;

        // Execute tasks in dependency order
        let mut results = Vec::new();
        let mut success_count = 0;

        for task_id in execution_order {
            let task_result = orchestrator.framework.lock().await
                .execute_task(Task::new(&task_id, "Graph Task", "graph", TaskPriority::Normal, serde_json::json!({})))
                .await?;

            results.push(TaskExecutionResult {
                task_id: task_id.clone(),
                result: task_result,
            });

            if task_result.success {
                success_count += 1;
            }
        }

        Ok(WorkflowResult {
            workflow_id: workflow.id.clone(),
            success: success_count == workflow.tasks.len(),
            success_rate: success_count as f64 / workflow.tasks.len().max(1) as f64,
            total_tasks: workflow.tasks.len(),
            completed_tasks: workflow.tasks.len(),
            execution_time_ms: 0,
            results,
        })
    }

    async fn execute_staged(
        &self,
        workflow: &Workflow,
        orchestrator: &TaskOrchestrator,
    ) -> Result<WorkflowResult, ExecutionError> {
        // Group tasks by stage (simplified - in production you'd have explicit stages)
        let mut stages = std::collections::HashMap::new();

        for task in &workflow.tasks {
            let stage_id = task.task_type.clone();
            stages.entry(stage_id).or_insert_with(Vec::new).push(task.clone());
        }

        let mut results = Vec::new();
        let mut success_count = 0;

        // Execute each stage sequentially
        for (stage_id, tasks) in stages {
            let stage_success = tasks.len();
            let mut stage_failures = 0;

            for task in tasks {
                let task_id = task.id.clone();
                let task_result = orchestrator.framework.lock().await.execute_task(task.clone()).await?;

                results.push(TaskExecutionResult {
                    task_id,
                    result: task_result,
                });

                if task_result.success {
                    success_count += 1;
                    stage_success += 1;
                } else {
                    stage_failures += 1;
                }
            }
        }

        Ok(WorkflowResult {
            workflow_id: workflow.id.clone(),
            success: success_count == workflow.tasks.len(),
            success_rate: success_count as f64 / workflow.tasks.len().max(1) as f64,
            total_tasks: workflow.tasks.len(),
            completed_tasks: workflow.tasks.len(),
            execution_time_ms: 0,
            results,
        })
    }

    async fn execute_custom(
        &self,
        workflow: &Workflow,
        orchestrator: &TaskOrchestrator,
        strategy_name: &str,
    ) -> Result<WorkflowResult, ExecutionError> {
        // Placeholder for custom strategy implementation
        eprintln!("Custom strategy {} not implemented", strategy_name);
        return Err(ExecutionError::Workflow(
            format!("Custom strategy not implemented: {}", strategy_name)
        ));
    }
}

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_orchestration_context() {
        let mut context = OrchestrationContext::new("test-workflow", 5);

        context.mark_task_started("task-1");
        context.mark_task_completed("task-1", TaskResult::default());

        assert_eq!(context.active_tasks.len(), 0);
        assert_eq!(context.completed_tasks.len(), 1);
        assert!(context.get_progress() > 0.0);
    }

    #[test]
    fn test_execution_graph() {
        let mut graph = ExecutionGraph::new();

        let task1 = Task::new("task-1", "Task 1", "test", TaskPriority::Normal, serde_json::json!({}));
        let task2 = Task::new("task-2", "Task 2", "test", TaskPriority::Normal, serde_json::json!({}));
        let task3 = Task::new("task-3", "Task 3", "test", TaskPriority::Normal, serde_json::json!({
            "dependencies": ["task-1", "task-2"]
        }));

        graph.add_node(task1).unwrap();
        graph.add_node(task2).unwrap();
        graph.add_node(task3).unwrap();

        let order = graph.build_execution_order().unwrap();
        assert_eq!(order.len(), 3);
        assert!(order.contains(&"task-1".to_string()));
        assert!(order.contains(&"task-2".to_string()));
        assert!(order.contains(&"task-3".to_string()));
    }

    #[tokio::test]
    async fn test_task_orchestrator() {
        let framework = ExecutionFramework::new(ExecutionConfig::default());
        let mut orchestrator = TaskOrchestrator::new(framework);

        let workflow = Workflow::new("test-workflow", "Test Workflow", "test");
        let result = orchestrator.orchestrate_workflow(workflow, 5).await;

        assert!(result.is_ok());
        assert!(result.unwrap().success);
    }

    #[test]
    fn test_orchestration_strategies() {
        let workflow = Workflow {
            id: "test-workflow".to_string(),
            name: "Test Workflow".to_string(),
            workflow_type: "test".to_string(),
            tasks: vec![
                Task::new("task-1", "Task 1", "test", TaskPriority::Normal, serde_json::json!({})),
                Task::new("task-2", "Task 2", "test", TaskPriority::Normal, serde_json::json!({})),
            ],
            created_at: Utc::now(),
            status: WorkflowStatus::Created,
        };

        let framework = Arc::new(Mutex::new(ExecutionFramework::new(ExecutionConfig::default())));

        // Test sequential strategy
        let sequential_executor = OrchestrationStrategyExecutor::new(OrchestrationStrategy::Sequential);
        assert!(sequential_executor.execute_workflow(&workflow, &TaskOrchestrator::new((*framework).try_into().unwrap())).await.is_ok());

        // Test parallel strategy
        let parallel_executor = OrchestrationStrategyExecutor::new(OrchestrationStrategy::Parallel);
        assert!(parallel_executor.execute_workflow(&workflow, &TaskOrchestrator::new((*framework).try_into().unwrap())).await.is_ok());
    }
}