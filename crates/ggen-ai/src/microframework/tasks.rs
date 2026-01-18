//! Task definitions for the microframework
//!
//! Tasks represent units of work that can be executed by agents.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::time::Duration;

/// Task status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TaskStatus {
    /// Task is pending execution
    Pending,
    /// Task is currently running
    Running,
    /// Task completed successfully
    Completed,
    /// Task failed
    Failed,
    /// Task was cancelled
    Cancelled,
    /// Task timed out
    TimedOut,
    /// Task is blocked by dependencies
    Blocked,
}

/// Task priority levels
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum TaskPriority {
    /// Critical - immediate execution
    Critical = 0,
    /// High priority
    High = 1,
    /// Normal priority (default)
    Normal = 2,
    /// Low priority
    Low = 3,
    /// Background - execute when idle
    Background = 4,
}

impl Default for TaskPriority {
    fn default() -> Self {
        Self::Normal
    }
}

/// Task type for categorization
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TaskType {
    /// Code generation task
    CodeGen,
    /// Testing task
    Test,
    /// Code review task
    Review,
    /// Validation task
    Validate,
    /// RDF processing task
    RdfProcess,
    /// Template generation task
    TemplateGen,
    /// Custom task
    Custom(String),
}

/// Task configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TaskConfig {
    /// Task timeout
    pub timeout: Duration,
    /// Retry count on failure
    pub retry_count: u32,
    /// Task priority
    pub priority: TaskPriority,
    /// Task dependencies (task IDs that must complete first)
    pub dependencies: Vec<String>,
    /// Task-specific parameters
    pub params: HashMap<String, serde_json::Value>,
    /// Tags for filtering
    pub tags: Vec<String>,
}

impl Default for TaskConfig {
    fn default() -> Self {
        Self {
            timeout: Duration::from_secs(60),
            retry_count: 2,
            priority: TaskPriority::Normal,
            dependencies: Vec::new(),
            params: HashMap::new(),
            tags: Vec::new(),
        }
    }
}

/// A task to be executed by an agent
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Task {
    /// Unique task ID
    pub id: String,
    /// Task type
    pub task_type: TaskType,
    /// Task description/prompt
    pub description: String,
    /// Task configuration
    pub config: TaskConfig,
    /// Input data
    pub input: serde_json::Value,
    /// Task status
    pub status: TaskStatus,
    /// Creation timestamp
    pub created_at: String,
}

impl Task {
    /// Create a new task
    pub fn new(task_type: TaskType, description: &str) -> Self {
        Self {
            id: uuid::Uuid::new_v4().to_string(),
            task_type,
            description: description.to_string(),
            config: TaskConfig::default(),
            input: serde_json::Value::Null,
            status: TaskStatus::Pending,
            created_at: chrono::Utc::now().to_rfc3339(),
        }
    }

    /// Create a code generation task
    pub fn code_gen(description: &str) -> Self {
        Self::new(TaskType::CodeGen, description)
    }

    /// Create a testing task
    pub fn test(description: &str) -> Self {
        Self::new(TaskType::Test, description)
    }

    /// Create a review task
    pub fn review(description: &str) -> Self {
        Self::new(TaskType::Review, description)
    }

    /// Create a validation task
    pub fn validate(description: &str) -> Self {
        Self::new(TaskType::Validate, description)
    }

    /// Create an RDF processing task
    pub fn rdf_process(description: &str) -> Self {
        Self::new(TaskType::RdfProcess, description)
    }

    /// Create a template generation task
    pub fn template_gen(description: &str) -> Self {
        Self::new(TaskType::TemplateGen, description)
    }

    /// Create a custom task
    pub fn custom(name: &str, description: &str) -> Self {
        Self::new(TaskType::Custom(name.to_string()), description)
    }

    /// Set task priority
    pub fn with_priority(mut self, priority: TaskPriority) -> Self {
        self.config.priority = priority;
        self
    }

    /// Set task timeout
    pub fn with_timeout(mut self, timeout: Duration) -> Self {
        self.config.timeout = timeout;
        self
    }

    /// Set retry count
    pub fn with_retries(mut self, count: u32) -> Self {
        self.config.retry_count = count;
        self
    }

    /// Add a dependency
    pub fn depends_on(mut self, task_id: &str) -> Self {
        self.config.dependencies.push(task_id.to_string());
        self
    }

    /// Set input data
    pub fn with_input(mut self, input: serde_json::Value) -> Self {
        self.input = input;
        self
    }

    /// Add a parameter
    pub fn with_param(mut self, key: &str, value: serde_json::Value) -> Self {
        self.config.params.insert(key.to_string(), value);
        self
    }

    /// Add a tag
    pub fn with_tag(mut self, tag: &str) -> Self {
        self.config.tags.push(tag.to_string());
        self
    }

    /// Check if task can run (dependencies satisfied)
    pub fn can_run(&self, completed_tasks: &[String]) -> bool {
        self.config
            .dependencies
            .iter()
            .all(|dep| completed_tasks.contains(dep))
    }
}

/// Result of task execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TaskResult {
    /// Task ID
    pub task_id: String,
    /// Final status
    pub status: TaskStatus,
    /// Output data
    pub output: serde_json::Value,
    /// Execution duration in milliseconds
    pub duration_ms: u64,
    /// Error message if failed
    pub error: Option<String>,
    /// Retry attempts used
    pub retries_used: u32,
    /// Completion timestamp
    pub completed_at: String,
    /// Additional metadata
    pub metadata: HashMap<String, String>,
}

impl TaskResult {
    /// Create a successful result
    pub fn success(task_id: String, output: serde_json::Value, duration_ms: u64) -> Self {
        Self {
            task_id,
            status: TaskStatus::Completed,
            output,
            duration_ms,
            error: None,
            retries_used: 0,
            completed_at: chrono::Utc::now().to_rfc3339(),
            metadata: HashMap::new(),
        }
    }

    /// Create a failure result
    pub fn failure(task_id: String, error: String, duration_ms: u64) -> Self {
        Self {
            task_id,
            status: TaskStatus::Failed,
            output: serde_json::Value::Null,
            duration_ms,
            error: Some(error),
            retries_used: 0,
            completed_at: chrono::Utc::now().to_rfc3339(),
            metadata: HashMap::new(),
        }
    }

    /// Check if successful
    pub fn is_success(&self) -> bool {
        matches!(self.status, TaskStatus::Completed)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_task_creation() {
        let task = Task::code_gen("Generate a Rust struct");
        assert_eq!(task.task_type, TaskType::CodeGen);
        assert_eq!(task.status, TaskStatus::Pending);
    }

    #[test]
    fn test_task_builder() {
        let task = Task::test("Run unit tests")
            .with_priority(TaskPriority::High)
            .with_timeout(Duration::from_secs(120))
            .with_retries(3)
            .with_tag("unit-test");

        assert_eq!(task.config.priority, TaskPriority::High);
        assert_eq!(task.config.timeout, Duration::from_secs(120));
        assert_eq!(task.config.retry_count, 3);
        assert!(task.config.tags.contains(&"unit-test".to_string()));
    }

    #[test]
    fn test_task_dependencies() {
        let task1 = Task::code_gen("Generate code");
        let task2 = Task::test("Run tests").depends_on(&task1.id);

        assert!(!task2.can_run(&[]));
        assert!(task2.can_run(&[task1.id.clone()]));
    }

    #[test]
    fn test_task_result() {
        let result = TaskResult::success(
            "task-1".to_string(),
            serde_json::json!({"code": "fn main() {}"}),
            100,
        );
        assert!(result.is_success());
        assert!(result.error.is_none());
    }
}
