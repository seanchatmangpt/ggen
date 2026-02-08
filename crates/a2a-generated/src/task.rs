//! Task domain structs and traits
//!
//! This module defines the task abstractions for agent-to-agent
//! task distribution and execution.

use std::collections::HashMap;
use std::time::Duration;

/// Task represents a unit of work to be executed by agents
#[derive(Debug, Clone, PartialEq)]
pub struct Task {
    /// Unique identifier for the task
    pub id: String,
    /// Name/title of the task
    pub name: String,
    /// Type/category of the task
    pub task_type: String,
    /// Input parameters for the task
    pub input: serde_json::Value,
    /// Expected output format
    pub expected_output: serde_json::Value,
    /// Dependencies that must be completed before this task
    pub dependencies: Vec<String>,
    /// Priority of the task
    pub priority: TaskPriority,
    /// Status of the task
    pub status: TaskStatus,
    /// Timeout for task execution
    pub timeout: Duration,
    /// Metadata for the task
    pub metadata: HashMap<String, String>,
}

/// TaskPriority defines the priority levels for tasks
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TaskPriority {
    Low,
    Normal,
    High,
    Critical,
}

/// TaskStatus represents the current state of a task
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TaskStatus {
    Pending,
    Ready,
    Running,
    Completed,
    Failed,
    Cancelled,
}

/// Trait for task execution
pub trait TaskExecutor: Send + Sync {
    /// Execute a task and return the result
    fn execute(&self, task: &Task) -> impl std::future::Future<Output = Result<TaskResult, TaskError>> + Send;

    /// Check if the executor can handle the given task type
    fn can_handle(&self, task_type: &str) -> bool;

    /// Get the maximum parallel tasks this executor can handle
    fn max_parallel_tasks(&self) -> usize;
}

/// Result of task execution
#[derive(Debug, Clone, PartialEq)]
pub struct TaskResult {
    /// The original task ID
    pub task_id: String,
    /// Result output
    pub output: serde_json::Value,
    /// Execution metadata
    pub metadata: HashMap<String, String>,
    /// Execution time
    pub execution_time: Duration,
}

/// Task execution error
#[derive(Debug, Clone, PartialEq)]
pub struct TaskError {
    /// Error message
    pub message: String,
    /// Error type
    pub error_type: TaskErrorType,
    /// Error details
    pub details: Option<serde_json::Value>,
}

/// Types of task errors
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TaskErrorType {
    ExecutionFailed,
    Timeout,
    DependencyError,
    InvalidInput,
    ResourceError,
    Unknown,
}

impl Task {
    pub fn new(
        id: String,
        name: String,
        task_type: String,
        input: serde_json::Value,
    ) -> Self {
        Self {
            id,
            name,
            task_type,
            input,
            expected_output: serde_json::json!({}),
            dependencies: Vec::new(),
            priority: TaskPriority::Normal,
            status: TaskStatus::Pending,
            timeout: Duration::from_secs(300), // 5 minutes default
            metadata: HashMap::new(),
        }
    }

    pub fn with_dependency(mut self, dependency_id: String) -> Self {
        self.dependencies.push(dependency_id);
        self
    }

    pub fn with_priority(mut self, priority: TaskPriority) -> Self {
        self.priority = priority;
        self
    }

    pub fn with_timeout(mut self, timeout: Duration) -> Self {
        self.timeout = timeout;
        self
    }

    pub fn with_metadata(mut self, key: String, value: String) -> Self {
        self.metadata.insert(key, value);
        self
    }

    pub fn is_ready(&self) -> bool {
        self.status == TaskStatus::Ready
    }

    pub fn can_execute(&self) -> bool {
        self.status == TaskStatus::Ready || self.status == TaskStatus::Pending
    }

    pub fn with_status(mut self, status: TaskStatus) -> Self {
        self.status = status;
        self
    }
}

impl TaskResult {
    pub fn new(task_id: String, output: serde_json::Value) -> Self {
        Self {
            task_id,
            output,
            metadata: HashMap::new(),
            execution_time: Duration::from_secs(0),
        }
    }

    pub fn with_execution_time(mut self, time: Duration) -> Self {
        self.execution_time = time;
        self
    }

    pub fn with_metadata(mut self, key: String, value: String) -> Self {
        self.metadata.insert(key, value);
        self
    }
}

impl TaskError {
    pub fn new(message: String, error_type: TaskErrorType) -> Self {
        Self {
            message,
            error_type,
            details: None,
        }
    }

    pub fn with_details(mut self, details: serde_json::Value) -> Self {
        self.details = Some(details);
        self
    }
}

/// Default task executor that simulates task execution
pub struct DefaultTaskExecutor {
    pub max_parallel: usize,
}

impl TaskExecutor for DefaultTaskExecutor {
    async fn execute(&self, task: &Task) -> Result<TaskResult, TaskError> {
        // Simulate task execution
        tokio::time::sleep(Duration::from_millis(100)).await;

        Ok(TaskResult::new(
            task.id.clone(),
            serde_json::json!({
                "status": "completed",
                "task_id": task.id,
                "output": "Task completed successfully"
            }),
        ))
    }

    fn can_handle(&self, _task_type: &str) -> bool {
        // Can handle all task types in default implementation
        true
    }

    fn max_parallel_tasks(&self) -> usize {
        self.max_parallel
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_task_creation() {
        let task = Task::new(
            "task-123".to_string(),
            "Test Task".to_string(),
            "test".to_string(),
            serde_json::json!({"input": "test"}),
        );

        assert_eq!(task.id, "task-123");
        assert_eq!(task.name, "Test Task");
        assert_eq!(task.task_type, "test");
        assert_eq!(task.priority, TaskPriority::Normal);
        assert_eq!(task.status, TaskStatus::Pending);
    }

    #[test]
    fn test_task_with_dependency() {
        let task = Task::new(
            "task-123".to_string(),
            "Test Task".to_string(),
            "test".to_string(),
            serde_json::json!({}),
        )
        .with_dependency("task-456".to_string());

        assert_eq!(task.dependencies, vec!["task-456"]);
    }

    #[test]
    fn test_task_status() {
        let task = Task::new(
            "task-123".to_string(),
            "Test Task".to_string(),
            "test".to_string(),
            serde_json::json!({}),
        );

        assert!(!task.is_ready());
        assert!(task.can_execute());

        let ready_task = task.with_status(TaskStatus::Ready);
        assert!(ready_task.is_ready());
        assert!(ready_task.can_execute());
    }

    #[tokio::test]
    async fn test_task_executor() {
        let executor = DefaultTaskExecutor { max_parallel: 4 };

        let task = Task::new(
            "task-123".to_string(),
            "Test Task".to_string(),
            "test".to_string(),
            serde_json::json!({"input": "test"}),
        );

        let result = executor.execute(&task).await.unwrap();
        assert_eq!(result.task_id, "task-123");
        assert_eq!(result.output["status"].as_str().unwrap(), "completed");
    }
}