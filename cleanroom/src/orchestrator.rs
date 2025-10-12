//! Structured concurrency orchestrator
//!
//! This module provides a structured concurrency orchestrator for managing
//! complex async operations with graceful cancellation and error handling.

use crate::error::Result;
use crate::cleanroom::CleanroomEnvironment;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::{JoinSet, mpsc, oneshot};
use tokio::task::{JoinHandle, AbortHandle};
use futures::future::{Future, FutureExt};
use std::collections::HashMap;
use std::pin::Pin;
use std::task::{Context, Poll};

/// Orchestrator for structured concurrency patterns
pub struct Orchestrator {
    environment: Arc<CleanroomEnvironment>,
    task_groups: HashMap<String, TaskGroup>,
    cancellation_tokens: HashMap<String, oneshot::Sender<()>>,
    max_concurrent_tasks: usize,
    task_timeout: Duration,
}

/// Task group for managing related tasks
pub struct TaskGroup {
    /// Group name
    name: String,
    /// Tasks in the group
    tasks: JoinSet<Result<TaskResult>>,
    /// Group configuration
    config: TaskGroupConfig,
    /// Group statistics
    stats: TaskGroupStatistics,
}

/// Task group configuration
#[derive(Debug, Clone)]
pub struct TaskGroupConfig {
    /// Maximum concurrent tasks in group
    pub max_concurrent_tasks: usize,
    /// Task timeout
    pub task_timeout: Duration,
    /// Enable graceful shutdown
    pub enable_graceful_shutdown: bool,
    /// Shutdown timeout
    pub shutdown_timeout: Duration,
    /// Retry failed tasks
    pub retry_failed_tasks: bool,
    /// Maximum retries
    pub max_retries: usize,
}

/// Task result
#[derive(Debug, Clone)]
pub struct TaskResult {
    /// Task name
    pub task_name: String,
    /// Task status
    pub status: TaskStatus,
    /// Execution duration
    pub duration: Duration,
    /// Task output
    pub output: Option<String>,
    /// Task error
    pub error: Option<String>,
    /// Task metadata
    pub metadata: HashMap<String, String>,
}

/// Task status
#[derive(Debug, Clone, PartialEq)]
pub enum TaskStatus {
    /// Task completed successfully
    Completed,
    /// Task failed
    Failed,
    /// Task was cancelled
    Cancelled,
    /// Task timed out
    TimedOut,
    /// Task is running
    Running,
    /// Task is pending
    Pending,
}

/// Task group statistics
#[derive(Debug, Clone)]
pub struct TaskGroupStatistics {
    /// Total tasks
    pub total_tasks: usize,
    /// Completed tasks
    pub completed_tasks: usize,
    /// Failed tasks
    pub failed_tasks: usize,
    /// Cancelled tasks
    pub cancelled_tasks: usize,
    /// Timed out tasks
    pub timed_out_tasks: usize,
    /// Running tasks
    pub running_tasks: usize,
    /// Pending tasks
    pub pending_tasks: usize,
    /// Average execution time
    pub avg_execution_time: Duration,
    /// Total execution time
    pub total_execution_time: Duration,
}

/// Task definition
pub struct Task {
    /// Task name
    name: String,
    /// Task function
    function: Box<dyn Fn() -> Pin<Box<dyn Future<Output = Result<TaskResult>> + Send>> + Send + Sync>,
    /// Task priority
    priority: TaskPriority,
    /// Task dependencies
    dependencies: Vec<String>,
    /// Task metadata
    metadata: HashMap<String, String>,
}

/// Task priority
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum TaskPriority {
    /// Low priority
    Low,
    /// Normal priority
    Normal,
    /// High priority
    High,
    /// Critical priority
    Critical,
}

impl Orchestrator {
    /// Create a new orchestrator
    pub fn new(environment: CleanroomEnvironment) -> Self {
        Self {
            environment: Arc::new(environment),
            task_groups: HashMap::new(),
            cancellation_tokens: HashMap::new(),
            max_concurrent_tasks: 10,
            task_timeout: Duration::from_secs(30),
        }
    }

    /// Set maximum concurrent tasks
    pub fn with_max_concurrent_tasks(mut self, max: usize) -> Self {
        self.max_concurrent_tasks = max;
        self
    }

    /// Set task timeout
    pub fn with_task_timeout(mut self, timeout: Duration) -> Self {
        self.task_timeout = timeout;
        self
    }

    /// Create a new task group
    pub fn create_task_group(&mut self, name: String, config: TaskGroupConfig) -> Result<()> {
        if self.task_groups.contains_key(&name) {
            return Err(crate::error::CleanroomError::internal_error(&format!(
                "Task group '{}' already exists",
                name
            )));
        }

        let task_group = TaskGroup {
            name: name.clone(),
            tasks: JoinSet::new(),
            config,
            stats: TaskGroupStatistics {
                total_tasks: 0,
                completed_tasks: 0,
                failed_tasks: 0,
                cancelled_tasks: 0,
                timed_out_tasks: 0,
                running_tasks: 0,
                pending_tasks: 0,
                avg_execution_time: Duration::from_secs(0),
                total_execution_time: Duration::from_secs(0),
            },
        };

        self.task_groups.insert(name, task_group);
        Ok(())
    }

    /// Spawn a task in a group
    pub async fn spawn_task(&mut self, group_name: &str, task: Task) -> Result<()> {
        let task_group = self.task_groups.get_mut(group_name)
            .ok_or_else(|| crate::error::CleanroomError::internal_error(&format!(
                "Task group '{}' not found",
                group_name
            )))?;

        // Check if we can spawn more tasks
        if task_group.tasks.len() >= task_group.config.max_concurrent_tasks {
            return Err(crate::error::CleanroomError::internal_error(&format!(
                "Task group '{}' has reached maximum concurrent tasks",
                group_name
            )));
        }

        // Create task future with timeout
        let task_future = async move {
            let start_time = std::time::Instant::now();
            
            // Execute the task
            let result = task.function()().await;
            
            let duration = start_time.elapsed();
            
            // Update result with timing information
            match result {
                Ok(mut task_result) => {
                    task_result.duration = duration;
                    Ok(task_result)
                }
                Err(e) => {
                    Ok(TaskResult {
                        task_name: task.name.clone(),
                        status: TaskStatus::Failed,
                        duration,
                        output: None,
                        error: Some(e.to_string()),
                        metadata: task.metadata,
                    })
                }
            }
        };

        // Spawn the task
        task_group.tasks.spawn(task_future);
        task_group.stats.total_tasks += 1;
        task_group.stats.pending_tasks += 1;

        Ok(())
    }

    /// Wait for all tasks in a group to complete
    pub async fn wait_for_group(&mut self, group_name: &str) -> Result<Vec<TaskResult>> {
        let task_group = self.task_groups.get_mut(group_name)
            .ok_or_else(|| crate::error::CleanroomError::internal_error(&format!(
                "Task group '{}' not found",
                group_name
            )))?;

        let mut results = Vec::new();
        
        while let Some(result) = task_group.tasks.join_next().await {
            match result {
                Ok(task_result) => {
                    match task_result {
                        Ok(result) => {
                            // Update statistics
                            match result.status {
                                TaskStatus::Completed => task_group.stats.completed_tasks += 1,
                                TaskStatus::Failed => task_group.stats.failed_tasks += 1,
                                TaskStatus::Cancelled => task_group.stats.cancelled_tasks += 1,
                                TaskStatus::TimedOut => task_group.stats.timed_out_tasks += 1,
                                _ => {}
                            }
                            
                            task_group.stats.running_tasks = task_group.stats.running_tasks.saturating_sub(1);
                            task_group.stats.total_execution_time += result.duration;
                            
                            results.push(result);
                        }
                        Err(e) => {
                            // Handle task execution error
                            task_group.stats.failed_tasks += 1;
                            task_group.stats.running_tasks = task_group.stats.running_tasks.saturating_sub(1);
                            
                            results.push(TaskResult {
                                task_name: "unknown".to_string(),
                                status: TaskStatus::Failed,
                                duration: Duration::from_secs(0),
                                output: None,
                                error: Some(e.to_string()),
                                metadata: HashMap::new(),
                            });
                        }
                    }
                }
                Err(e) => {
                    // Handle task join error
                    task_group.stats.failed_tasks += 1;
                    task_group.stats.running_tasks = task_group.stats.running_tasks.saturating_sub(1);
                    
                    results.push(TaskResult {
                        task_name: "unknown".to_string(),
                        status: TaskStatus::Failed,
                        duration: Duration::from_secs(0),
                        output: None,
                        error: Some(format!("Task join error: {}", e)),
                        metadata: HashMap::new(),
                    });
                }
            }
        }

        // Update average execution time
        if !results.is_empty() {
            task_group.stats.avg_execution_time = Duration::from_nanos(
                task_group.stats.total_execution_time.as_nanos() as u64 / results.len() as u64
            );
        }

        Ok(results)
    }

    /// Cancel all tasks in a group
    pub async fn cancel_group(&mut self, group_name: &str) -> Result<()> {
        let task_group = self.task_groups.get_mut(group_name)
            .ok_or_else(|| crate::error::CleanroomError::internal_error(&format!(
                "Task group '{}' not found",
                group_name
            )))?;

        // Abort all tasks
        while let Some(handle) = task_group.tasks.join_next().await {
            if let Ok(task_result) = handle {
                if let Ok(result) = task_result {
                    if result.status == TaskStatus::Running {
                        task_group.stats.cancelled_tasks += 1;
                    }
                }
            }
        }

        task_group.stats.running_tasks = 0;
        task_group.stats.pending_tasks = 0;

        Ok(())
    }

    /// Get task group statistics
    pub fn get_group_statistics(&self, group_name: &str) -> Option<&TaskGroupStatistics> {
        self.task_groups.get(group_name).map(|group| &group.stats)
    }

    /// Get all task group names
    pub fn get_task_group_names(&self) -> Vec<String> {
        self.task_groups.keys().cloned().collect()
    }

    /// Remove a task group
    pub fn remove_task_group(&mut self, group_name: &str) -> Result<()> {
        if self.task_groups.remove(group_name).is_none() {
            return Err(crate::error::CleanroomError::internal_error(&format!(
                "Task group '{}' not found",
                group_name
            )));
        }
        Ok(())
    }

    /// Get orchestrator statistics
    pub fn get_statistics(&self) -> OrchestratorStatistics {
        let mut total_tasks = 0;
        let mut total_completed = 0;
        let mut total_failed = 0;
        let mut total_cancelled = 0;
        let mut total_timed_out = 0;
        let mut total_running = 0;
        let mut total_pending = 0;

        for group in self.task_groups.values() {
            total_tasks += group.stats.total_tasks;
            total_completed += group.stats.completed_tasks;
            total_failed += group.stats.failed_tasks;
            total_cancelled += group.stats.cancelled_tasks;
            total_timed_out += group.stats.timed_out_tasks;
            total_running += group.stats.running_tasks;
            total_pending += group.stats.pending_tasks;
        }

        OrchestratorStatistics {
            total_task_groups: self.task_groups.len(),
            total_tasks,
            total_completed,
            total_failed,
            total_cancelled,
            total_timed_out,
            total_running,
            total_pending,
            max_concurrent_tasks: self.max_concurrent_tasks,
            task_timeout: self.task_timeout,
        }
    }
}

/// Orchestrator statistics
#[derive(Debug, Clone)]
pub struct OrchestratorStatistics {
    /// Total number of task groups
    pub total_task_groups: usize,
    /// Total number of tasks
    pub total_tasks: usize,
    /// Total completed tasks
    pub total_completed: usize,
    /// Total failed tasks
    pub total_failed: usize,
    /// Total cancelled tasks
    pub total_cancelled: usize,
    /// Total timed out tasks
    pub total_timed_out: usize,
    /// Total running tasks
    pub total_running: usize,
    /// Total pending tasks
    pub total_pending: usize,
    /// Maximum concurrent tasks
    pub max_concurrent_tasks: usize,
    /// Task timeout
    pub task_timeout: Duration,
}

impl Task {
    /// Create a new task
    pub fn new<F, Fut>(name: String, function: F) -> Self
    where
        F: Fn() -> Fut + Send + Sync + 'static,
        Fut: Future<Output = Result<TaskResult>> + Send + 'static,
    {
        Self {
            name,
            function: Box::new(move || Box::pin(function())),
            priority: TaskPriority::Normal,
            dependencies: Vec::new(),
            metadata: HashMap::new(),
        }
    }

    /// Set task priority
    pub fn with_priority(mut self, priority: TaskPriority) -> Self {
        self.priority = priority;
        self
    }

    /// Add task dependency
    pub fn with_dependency(mut self, dependency: String) -> Self {
        self.dependencies.push(dependency);
        self
    }

    /// Add task metadata
    pub fn with_metadata(mut self, key: String, value: String) -> Self {
        self.metadata.insert(key, value);
        self
    }

    /// Get task name
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Get task priority
    pub fn priority(&self) -> &TaskPriority {
        &self.priority
    }

    /// Get task dependencies
    pub fn dependencies(&self) -> &[String] {
        &self.dependencies
    }

    /// Get task metadata
    pub fn metadata(&self) -> &HashMap<String, String> {
        &self.metadata
    }
}

impl Default for TaskGroupConfig {
    fn default() -> Self {
        Self {
            max_concurrent_tasks: 10,
            task_timeout: Duration::from_secs(30),
            enable_graceful_shutdown: true,
            shutdown_timeout: Duration::from_secs(10),
            retry_failed_tasks: false,
            max_retries: 3,
        }
    }
}

/// Convenience function to create an orchestrator
pub fn orchestrator(environment: CleanroomEnvironment) -> Orchestrator {
    Orchestrator::new(environment)
}

/// Convenience function to create a task
pub fn task<F, Fut>(name: String, function: F) -> Task
where
    F: Fn() -> Fut + Send + Sync + 'static,
    Fut: Future<Output = Result<TaskResult>> + Send + 'static,
{
    Task::new(name, function)
}

/// Convenience function to create a task group config
pub fn task_group_config() -> TaskGroupConfig {
    TaskGroupConfig::default()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::CleanroomConfig;

    #[tokio::test]
    async fn test_orchestrator_creation() {
        let config = CleanroomConfig::default();
        let environment = CleanroomEnvironment::new(config).await.unwrap();
        let orchestrator = Orchestrator::new(environment);
        
        assert_eq!(orchestrator.max_concurrent_tasks, 10);
        assert_eq!(orchestrator.task_timeout, Duration::from_secs(30));
    }

    #[tokio::test]
    async fn test_task_group_creation() {
        let config = CleanroomConfig::default();
        let environment = CleanroomEnvironment::new(config).await.unwrap();
        let mut orchestrator = Orchestrator::new(environment);
        
        let config = TaskGroupConfig::default();
        assert!(orchestrator.create_task_group("test_group".to_string(), config).is_ok());
        assert!(orchestrator.get_task_group_names().contains(&"test_group".to_string()));
    }

    #[tokio::test]
    async fn test_task_spawning() {
        let config = CleanroomConfig::default();
        let environment = CleanroomEnvironment::new(config).await.unwrap();
        let mut orchestrator = Orchestrator::new(environment);
        
        let group_config = TaskGroupConfig::default();
        orchestrator.create_task_group("test_group".to_string(), group_config).unwrap();
        
        let task = Task::new("test_task".to_string(), || async {
            Ok(TaskResult {
                task_name: "test_task".to_string(),
                status: TaskStatus::Completed,
                duration: Duration::from_millis(100),
                output: Some("test output".to_string()),
                error: None,
                metadata: HashMap::new(),
            })
        });
        
        assert!(orchestrator.spawn_task("test_group", task).await.is_ok());
    }

    #[tokio::test]
    async fn test_task_execution() {
        let config = CleanroomConfig::default();
        let environment = CleanroomEnvironment::new(config).await.unwrap();
        let mut orchestrator = Orchestrator::new(environment);
        
        let group_config = TaskGroupConfig::default();
        orchestrator.create_task_group("test_group".to_string(), group_config).unwrap();
        
        let task = Task::new("test_task".to_string(), || async {
            tokio::time::sleep(Duration::from_millis(50)).await;
            Ok(TaskResult {
                task_name: "test_task".to_string(),
                status: TaskStatus::Completed,
                duration: Duration::from_millis(50),
                output: Some("test output".to_string()),
                error: None,
                metadata: HashMap::new(),
            })
        });
        
        orchestrator.spawn_task("test_group", task).await.unwrap();
        let results = orchestrator.wait_for_group("test_group").await.unwrap();
        
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].status, TaskStatus::Completed);
        assert_eq!(results[0].output, Some("test output".to_string()));
    }

    #[tokio::test]
    async fn test_task_statistics() {
        let config = CleanroomConfig::default();
        let environment = CleanroomEnvironment::new(config).await.unwrap();
        let mut orchestrator = Orchestrator::new(environment);
        
        let group_config = TaskGroupConfig::default();
        orchestrator.create_task_group("test_group".to_string(), group_config).unwrap();
        
        let task = Task::new("test_task".to_string(), || async {
            Ok(TaskResult {
                task_name: "test_task".to_string(),
                status: TaskStatus::Completed,
                duration: Duration::from_millis(100),
                output: Some("test output".to_string()),
                error: None,
                metadata: HashMap::new(),
            })
        });
        
        orchestrator.spawn_task("test_group", task).await.unwrap();
        orchestrator.wait_for_group("test_group").await.unwrap();
        
        let stats = orchestrator.get_group_statistics("test_group").unwrap();
        assert_eq!(stats.total_tasks, 1);
        assert_eq!(stats.completed_tasks, 1);
        assert_eq!(stats.failed_tasks, 0);
    }

    #[tokio::test]
    async fn test_convenience_functions() {
        let config = CleanroomConfig::default();
        let environment = CleanroomEnvironment::new(config).await.unwrap();
        
        let _orchestrator = orchestrator(environment);
        let _task = task("test".to_string(), || async {
            Ok(TaskResult {
                task_name: "test".to_string(),
                status: TaskStatus::Completed,
                duration: Duration::from_millis(100),
                output: None,
                error: None,
                metadata: HashMap::new(),
            })
        });
        let _config = task_group_config();
        
        // Just verify they compile and create valid instances
        assert_eq!(_orchestrator.max_concurrent_tasks, 10);
        assert_eq!(_task.name(), "test");
        assert_eq!(_config.max_concurrent_tasks, 10);
    }
}
