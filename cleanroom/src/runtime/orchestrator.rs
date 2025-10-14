//! Structured concurrency orchestrator with graceful cancellation
//!
//! Provides a structured concurrency model for managing concurrent tasks
//! with proper cancellation, timeout handling, and resource cleanup.

use crate::error::{BackendError, Result};
use std::collections::HashMap;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::{broadcast, RwLock};
use tokio::task::JoinSet;
use uuid::Uuid;

/// Task identifier for tracking concurrent operations
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TaskId(Uuid);

impl TaskId {
    /// Create a new task ID
    pub fn new() -> Self {
        Self(Uuid::new_v4())
    }

    /// Get the underlying UUID
    pub fn uuid(&self) -> Uuid {
        self.0
    }
}

impl Default for TaskId {
    fn default() -> Self {
        Self::new()
    }
}

/// Task status enumeration
#[derive(Debug, Clone, PartialEq)]
pub enum TaskStatus {
    /// Task is pending execution
    Pending,
    /// Task is currently running
    Running,
    /// Task completed successfully
    Completed,
    /// Task failed with an error
    Failed,
    /// Task was cancelled
    Cancelled,
    /// Task timed out
    TimedOut,
}

/// Task metadata for tracking and management
#[derive(Debug, Clone)]
pub struct TaskMetadata {
    /// Task identifier
    pub id: TaskId,
    /// Task name/description
    pub name: String,
    /// Task status
    pub status: TaskStatus,
    /// Creation time
    pub created_at: Instant,
    /// Start time
    pub started_at: Option<Instant>,
    /// Completion time
    pub completed_at: Option<Instant>,
    /// Task timeout
    pub timeout: Option<Duration>,
    /// Task priority (higher = more important)
    pub priority: u8,
    /// Task tags for categorization
    pub tags: Vec<String>,
}

impl TaskMetadata {
    /// Create new task metadata
    pub fn new(name: String) -> Self {
        Self {
            id: TaskId::new(),
            name,
            status: TaskStatus::Pending,
            created_at: Instant::now(),
            started_at: None,
            completed_at: None,
            timeout: None,
            priority: 0,
            tags: Vec::new(),
        }
    }

    /// Set task timeout
    pub fn with_timeout(mut self, timeout: Duration) -> Self {
        self.timeout = Some(timeout);
        self
    }

    /// Set task priority
    pub fn with_priority(mut self, priority: u8) -> Self {
        self.priority = priority;
        self
    }

    /// Add task tag
    pub fn with_tag(mut self, tag: String) -> Self {
        self.tags.push(tag);
        self
    }

    /// Mark task as started
    pub fn mark_started(&mut self) {
        self.status = TaskStatus::Running;
        self.started_at = Some(Instant::now());
    }

    /// Mark task as completed
    pub fn mark_completed(&mut self) {
        self.status = TaskStatus::Completed;
        self.completed_at = Some(Instant::now());
    }

    /// Mark task as failed
    pub fn mark_failed(&mut self) {
        self.status = TaskStatus::Failed;
        self.completed_at = Some(Instant::now());
    }

    /// Mark task as cancelled
    pub fn mark_cancelled(&mut self) {
        self.status = TaskStatus::Cancelled;
        self.completed_at = Some(Instant::now());
    }

    /// Mark task as timed out
    pub fn mark_timed_out(&mut self) {
        self.status = TaskStatus::TimedOut;
        self.completed_at = Some(Instant::now());
    }

    /// Get task duration
    pub fn duration(&self) -> Option<Duration> {
        match (self.started_at, self.completed_at) {
            (Some(start), Some(end)) => Some(end.duration_since(start)),
            _ => None,
        }
    }

    /// Check if task is active (running or pending)
    pub fn is_active(&self) -> bool {
        matches!(self.status, TaskStatus::Pending | TaskStatus::Running)
    }

    /// Check if task is finished (completed, failed, cancelled, or timed out)
    pub fn is_finished(&self) -> bool {
        matches!(
            self.status,
            TaskStatus::Completed
                | TaskStatus::Failed
                | TaskStatus::Cancelled
                | TaskStatus::TimedOut
        )
    }
}

/// Task execution context
pub struct TaskContext {
    /// Task metadata
    pub metadata: TaskMetadata,
    /// Cancellation token
    pub cancellation_token: broadcast::Receiver<()>,
    /// Shared state for coordination
    pub shared_state: Arc<RwLock<HashMap<String, serde_json::Value>>>,
}

impl TaskContext {
    /// Create new task context
    pub fn new(metadata: TaskMetadata, cancellation_token: broadcast::Receiver<()>) -> Self {
        Self {
            metadata,
            cancellation_token,
            shared_state: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Check if task should be cancelled
    pub async fn is_cancelled(&mut self) -> bool {
        self.cancellation_token.try_recv().is_ok()
    }

    /// Wait for cancellation signal
    pub async fn wait_for_cancellation(&mut self) {
        let _ = self.cancellation_token.recv().await;
    }

    /// Get shared state value
    pub async fn get_shared_state(&self, key: &str) -> Option<serde_json::Value> {
        self.shared_state.read().await.get(key).cloned()
    }

    /// Set shared state value
    pub async fn set_shared_state(&self, key: String, value: serde_json::Value) {
        self.shared_state.write().await.insert(key, value);
    }
}

/// Task execution result
#[derive(Debug)]
pub enum TaskResult<T> {
    /// Task completed successfully
    Success(T),
    /// Task failed with error
    Failure(crate::error::CleanroomError),
    /// Task was cancelled
    Cancelled,
    /// Task timed out
    TimedOut,
}

impl<T> TaskResult<T> {
    /// Check if task succeeded
    pub fn is_success(&self) -> bool {
        matches!(self, TaskResult::Success(_))
    }

    /// Check if task failed
    pub fn is_failure(&self) -> bool {
        matches!(self, TaskResult::Failure(_))
    }

    /// Check if task was cancelled
    pub fn is_cancelled(&self) -> bool {
        matches!(self, TaskResult::Cancelled)
    }

    /// Check if task timed out
    pub fn is_timed_out(&self) -> bool {
        matches!(self, TaskResult::TimedOut)
    }

    /// Extract the success value
    pub fn success(self) -> Option<T> {
        match self {
            TaskResult::Success(value) => Some(value),
            _ => None,
        }
    }

    /// Extract the error
    pub fn error(self) -> Option<crate::error::CleanroomError> {
        match self {
            TaskResult::Failure(error) => Some(error),
            _ => None,
        }
    }
}

/// Task executor function type
pub type TaskExecutor<T> = Box<
    dyn FnOnce(TaskContext) -> Pin<Box<dyn Future<Output = Result<T>> + Send + 'static>>
        + Send
        + Sync,
>;

/// Structured concurrency orchestrator
#[derive(Debug)]
pub struct ConcurrencyOrchestrator {
    /// Task registry
    tasks: Arc<RwLock<HashMap<TaskId, TaskMetadata>>>,
    /// Join set for managing concurrent tasks
    join_set: JoinSet<TaskResult<()>>,
    /// Cancellation broadcaster
    cancellation_sender: broadcast::Sender<()>,
    /// Global timeout
    global_timeout: Option<Duration>,
    /// Maximum concurrent tasks
    max_concurrent_tasks: usize,
    /// Task execution statistics
    stats: Arc<RwLock<OrchestratorStats>>,
}

/// Orchestrator statistics
#[derive(Debug, Default, Clone)]
pub struct OrchestratorStats {
    /// Total tasks created
    pub total_tasks: u64,
    /// Tasks completed successfully
    pub tasks_completed: u64,
    /// Tasks failed
    pub tasks_failed: u64,
    /// Tasks cancelled
    pub tasks_cancelled: u64,
    /// Tasks timed out
    pub tasks_timed_out: u64,
    /// Average task duration
    pub average_duration_ms: f64,
    /// Peak concurrent tasks
    pub peak_concurrent_tasks: usize,
    /// Current active tasks
    pub current_active_tasks: usize,
}

impl ConcurrencyOrchestrator {
    /// Create a new orchestrator
    pub fn new() -> Self {
        let (cancellation_sender, _) = broadcast::channel(1000);

        Self {
            tasks: Arc::new(RwLock::new(HashMap::new())),
            join_set: JoinSet::new(),
            cancellation_sender,
            global_timeout: None,
            max_concurrent_tasks: 100,
            stats: Arc::new(RwLock::new(OrchestratorStats::default())),
        }
    }

    /// Set global timeout for all tasks
    pub fn with_global_timeout(mut self, timeout: Duration) -> Self {
        self.global_timeout = Some(timeout);
        self
    }

    /// Set maximum concurrent tasks
    pub fn with_max_concurrent_tasks(mut self, max_tasks: usize) -> Self {
        self.max_concurrent_tasks = max_tasks;
        self
    }

    /// Spawn a new task
    pub async fn spawn_task<T, F>(&mut self, name: String, executor: F) -> Result<TaskId>
    where
        T: Send + 'static,
        F: FnOnce(TaskContext) -> Pin<Box<dyn Future<Output = Result<T>> + Send + 'static>>
            + Send
            + Sync
            + 'static,
    {
        let metadata = TaskMetadata::new(name);
        let task_id = metadata.id;

        // Register task
        {
            let mut tasks = self.tasks.write().await;
            tasks.insert(task_id, metadata.clone());
        }

        // Update stats
        {
            let mut stats = self.stats.write().await;
            stats.total_tasks += 1;
            stats.current_active_tasks += 1;
            if stats.current_active_tasks > stats.peak_concurrent_tasks {
                stats.peak_concurrent_tasks = stats.current_active_tasks;
            }
        }

        // Create cancellation receiver
        let cancellation_receiver = self.cancellation_sender.subscribe();
        let context = TaskContext::new(metadata, cancellation_receiver);

        // Spawn task
        let task_handle = tokio::spawn(async move {
            let result = executor(context).await;
            match result {
                Ok(_) => TaskResult::Success(()),
                Err(e) => TaskResult::Failure(e),
            }
        });

        // Add to join set
        self.join_set.spawn(async move {
            match task_handle.await {
                Ok(result) => result,
                Err(e) => TaskResult::Failure(crate::error::CleanroomError::internal_error(
                    format!("Task join error: {}", e),
                )),
            }
        });

        Ok(task_id)
    }

    /// Spawn a task with timeout
    pub async fn spawn_task_with_timeout<T, F>(
        &mut self, name: String, timeout: Duration, executor: F,
    ) -> Result<TaskId>
    where
        T: Send + 'static,
        F: FnOnce(TaskContext) -> Pin<Box<dyn Future<Output = Result<T>> + Send + 'static>>
            + Send
            + Sync
            + 'static,
    {
        let metadata = TaskMetadata::new(name).with_timeout(timeout);
        let task_id = metadata.id;

        // Register task
        {
            let mut tasks = self.tasks.write().await;
            tasks.insert(task_id, metadata.clone());
        }

        // Update stats
        {
            let mut stats = self.stats.write().await;
            stats.total_tasks += 1;
            stats.current_active_tasks += 1;
            if stats.current_active_tasks > stats.peak_concurrent_tasks {
                stats.peak_concurrent_tasks = stats.current_active_tasks;
            }
        }

        // Create cancellation receiver
        let cancellation_receiver = self.cancellation_sender.subscribe();
        let context = TaskContext::new(metadata, cancellation_receiver);

        // Spawn task with timeout
        let task_handle = tokio::spawn(async move {
            let future = executor(context);
            match tokio::time::timeout(timeout, future).await {
                Ok(result) => match result {
                    Ok(_) => TaskResult::Success(()),
                    Err(e) => TaskResult::Failure(e),
                },
                Err(_) => TaskResult::TimedOut,
            }
        });

        // Add to join set
        self.join_set.spawn(async move {
            match task_handle.await {
                Ok(result) => result,
                Err(e) => TaskResult::Failure(crate::error::CleanroomError::internal_error(
                    format!("Task join error: {}", e),
                )),
            }
        });

        Ok(task_id)
    }

    /// Wait for a specific task to complete
    pub async fn wait_for_task(&mut self, task_id: TaskId) -> Result<TaskResult<()>> {
        // Wait for task completion in join set
        if let Some(result) = self.join_set.join_next().await {
            match result {
                Ok(task_result) => {
                    // Update task metadata
                    if let Some(_metadata) = self.get_task_metadata(task_id).await {
                        match &task_result {
                            TaskResult::Success(_) => {
                                let mut tasks = self.tasks.write().await;
                                if let Some(task) = tasks.get_mut(&task_id) {
                                    task.mark_completed();
                                }
                            }
                            TaskResult::Failure(_) => {
                                let mut tasks = self.tasks.write().await;
                                if let Some(task) = tasks.get_mut(&task_id) {
                                    task.mark_failed();
                                }
                            }
                            TaskResult::Cancelled => {
                                let mut tasks = self.tasks.write().await;
                                if let Some(task) = tasks.get_mut(&task_id) {
                                    task.mark_cancelled();
                                }
                            }
                            TaskResult::TimedOut => {
                                let mut tasks = self.tasks.write().await;
                                if let Some(task) = tasks.get_mut(&task_id) {
                                    task.mark_timed_out();
                                }
                            }
                        }
                    }

                    // Update stats
                    {
                        let mut stats = self.stats.write().await;
                        stats.current_active_tasks = stats.current_active_tasks.saturating_sub(1);
                        match &task_result {
                            TaskResult::Success(_) => stats.tasks_completed += 1,
                            TaskResult::Failure(_) => stats.tasks_failed += 1,
                            TaskResult::Cancelled => stats.tasks_cancelled += 1,
                            TaskResult::TimedOut => stats.tasks_timed_out += 1,
                        }
                    }

                    return Ok(task_result);
                }
                Err(e) => {
                    return Err(BackendError::Runtime(format!("Task join error: {}", e)).into());
                }
            }
        }

        Err(BackendError::Runtime("Task not found in join set".to_string()).into())
    }

    /// Wait for all tasks to complete
    pub async fn wait_for_all(&mut self) -> Result<Vec<TaskResult<()>>> {
        let mut results = Vec::new();

        while let Some(result) = self.join_set.join_next().await {
            match result {
                Ok(task_result) => {
                    results.push(task_result);
                }
                Err(e) => {
                    return Err(BackendError::Runtime(format!("Task join error: {}", e)).into());
                }
            }
        }

        Ok(results)
    }

    /// Cancel a specific task
    pub async fn cancel_task(&self, task_id: TaskId) -> Result<()> {
        let mut tasks = self.tasks.write().await;
        if let Some(task) = tasks.get_mut(&task_id) {
            if task.is_active() {
                task.mark_cancelled();
            }
        }
        Ok(())
    }

    /// Cancel all tasks
    pub async fn cancel_all(&self) -> Result<()> {
        // Send cancellation signal
        let _ = self.cancellation_sender.send(());

        // Mark all active tasks as cancelled
        let mut tasks = self.tasks.write().await;
        for task in tasks.values_mut() {
            if task.is_active() {
                task.mark_cancelled();
            }
        }

        Ok(())
    }

    /// Get task metadata
    pub async fn get_task_metadata(&self, task_id: TaskId) -> Option<TaskMetadata> {
        self.tasks.read().await.get(&task_id).cloned()
    }

    /// Get all task metadata
    pub async fn get_all_task_metadata(&self) -> Vec<TaskMetadata> {
        self.tasks.read().await.values().cloned().collect()
    }

    /// Get active tasks
    pub async fn get_active_tasks(&self) -> Vec<TaskMetadata> {
        self.tasks
            .read()
            .await
            .values()
            .filter(|task| task.is_active())
            .cloned()
            .collect()
    }

    /// Get orchestrator statistics
    pub async fn get_stats(&self) -> OrchestratorStats {
        self.stats.read().await.clone()
    }

    /// Check if orchestrator is idle (no active tasks)
    pub async fn is_idle(&self) -> bool {
        self.tasks
            .read()
            .await
            .values()
            .all(|task| task.is_finished())
    }

    /// Get current active task count
    pub async fn active_task_count(&self) -> usize {
        self.tasks
            .read()
            .await
            .values()
            .filter(|task| task.is_active())
            .count()
    }

    /// Cleanup completed tasks
    pub async fn cleanup_completed_tasks(&self) -> Result<()> {
        let mut tasks = self.tasks.write().await;
        tasks.retain(|_, task| !task.is_finished());
        Ok(())
    }
}

impl Default for ConcurrencyOrchestrator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;

    #[tokio::test]
    async fn test_orchestrator_creation() {
        let orchestrator = ConcurrencyOrchestrator::new();
        assert_eq!(orchestrator.active_task_count().await, 0);
        assert!(orchestrator.is_idle().await);
    }

    #[tokio::test]
    async fn test_task_spawning() {
        let mut orchestrator = ConcurrencyOrchestrator::new();

        let task_id = orchestrator
            .spawn_task(
                "test_task".to_string(),
                Box::new(|_context| {
                    Box::pin(async move {
                        tokio::time::sleep(Duration::from_millis(10)).await;
                        Ok::<(), crate::error::CleanroomError>(())
                    })
                        as Pin<Box<dyn Future<Output = Result<()>> + Send + 'static>>
                }),
            )
            .await
            .unwrap();

        assert_eq!(orchestrator.active_task_count().await, 1);

        let result = orchestrator.wait_for_task(task_id).await.unwrap();
        assert!(result.is_success());

        assert_eq!(orchestrator.active_task_count().await, 0);
        assert!(orchestrator.is_idle().await);
    }

    #[tokio::test]
    async fn test_task_with_timeout() {
        let mut orchestrator = ConcurrencyOrchestrator::new();

        let task_id = orchestrator
            .spawn_task_with_timeout(
                "timeout_task".to_string(),
                Duration::from_millis(50),
                Box::new(|_context| {
                    Box::pin(async move {
                        tokio::time::sleep(Duration::from_millis(100)).await;
                        Ok::<(), crate::error::CleanroomError>(())
                    })
                        as Pin<Box<dyn Future<Output = Result<()>> + Send + 'static>>
                }),
            )
            .await
            .unwrap();

        let result = orchestrator.wait_for_task(task_id).await.unwrap();
        assert!(result.is_timed_out());
    }

    #[tokio::test]
    async fn test_multiple_tasks() {
        let mut orchestrator = ConcurrencyOrchestrator::new();

        let mut task_ids = Vec::new();
        for i in 0..5 {
            let task_id = orchestrator
                .spawn_task(
                    format!("task_{}", i),
                    Box::new(|_context| {
                        Box::pin(async move {
                            tokio::time::sleep(Duration::from_millis(10)).await;
                            Ok::<(), crate::error::CleanroomError>(())
                        })
                            as Pin<Box<dyn Future<Output = Result<()>> + Send + 'static>>
                    }),
                )
                .await
                .unwrap();
            task_ids.push(task_id);
        }

        assert_eq!(orchestrator.active_task_count().await, 5);

        let results = orchestrator.wait_for_all().await.unwrap();
        assert_eq!(results.len(), 5);

        for result in results {
            assert!(result.is_success());
        }

        assert!(orchestrator.is_idle().await);
    }

    #[tokio::test]
    async fn test_task_cancellation() {
        let mut orchestrator = ConcurrencyOrchestrator::new();

        let task_id = orchestrator
            .spawn_task(
                "cancellable_task".to_string(),
                Box::new(|mut context: TaskContext| {
                    Box::pin(async move {
                        // Wait for cancellation
                        context.wait_for_cancellation().await;
                        Ok::<(), crate::error::CleanroomError>(())
                    })
                        as Pin<Box<dyn Future<Output = Result<()>> + Send + 'static>>
                }),
            )
            .await
            .unwrap();

        assert_eq!(orchestrator.active_task_count().await, 1);

        // Cancel the task
        orchestrator.cancel_task(task_id).await.unwrap();

        let result = orchestrator.wait_for_task(task_id).await.unwrap();
        assert!(result.is_cancelled());
    }

    #[tokio::test]
    async fn test_orchestrator_stats() {
        let mut orchestrator = ConcurrencyOrchestrator::new();

        // Spawn some tasks
        for i in 0..3 {
            orchestrator
                .spawn_task(
                    format!("stats_task_{}", i),
                    Box::new(|_context| {
                        Box::pin(async move {
                            tokio::time::sleep(Duration::from_millis(10)).await;
                            Ok::<(), crate::error::CleanroomError>(())
                        })
                            as Pin<Box<dyn Future<Output = Result<()>> + Send + 'static>>
                    }),
                )
                .await
                .unwrap();
        }

        orchestrator.wait_for_all().await.unwrap();

        let stats = orchestrator.get_stats().await;
        assert_eq!(stats.total_tasks, 3);
        assert_eq!(stats.tasks_completed, 3);
        assert_eq!(stats.peak_concurrent_tasks, 3);
    }
}
