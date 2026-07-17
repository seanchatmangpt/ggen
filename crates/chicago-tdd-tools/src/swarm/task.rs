//! Task Receipt System for Swarm Operations
//!
//! Provides proof of work done by swarm agents. Each task generates a receipt
//! that proves: what was done, by whom, when, and the result.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Status of a task in the swarm
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TaskStatus {
    /// Queued and waiting for execution
    Queued,
    /// Currently being executed
    Executing,
    /// Completed successfully
    Completed,
    /// Failed with error
    Failed,
    /// Cancelled before execution
    Cancelled,
}

impl std::fmt::Display for TaskStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Queued => write!(f, "Queued"),
            Self::Executing => write!(f, "Executing"),
            Self::Completed => write!(f, "Completed"),
            Self::Failed => write!(f, "Failed"),
            Self::Cancelled => write!(f, "Cancelled"),
        }
    }
}

/// A request for a task to be executed by swarm members
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TaskRequest {
    /// Unique task ID
    pub id: String,
    /// Sector(s) to execute in (Academic, Claims, etc.)
    pub sectors: Vec<String>,
    /// Operation to perform
    pub operation: String,
    /// Input data
    pub input: String,
    /// Priority (higher = execute first)
    pub priority: u32,
    /// Deadline for execution
    pub deadline: String,
}

impl TaskRequest {
    /// Create a new task request
    #[must_use]
    pub fn new(id: String, sector: String, operation: String, input: String) -> Self {
        Self {
            id,
            sectors: vec![sector],
            operation,
            input,
            priority: 0,
            deadline: "2099-12-31T23:59:59Z".to_string(),
        }
    }

    /// Set priority for this task
    #[must_use]
    pub const fn with_priority(mut self, priority: u32) -> Self {
        self.priority = priority;
        self
    }

    /// Add a sector to execute in
    #[must_use]
    pub fn add_sector(mut self, sector: String) -> Self {
        if !self.sectors.contains(&sector) {
            self.sectors.push(sector);
        }
        self
    }
}

/// Proof of task completion
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TaskReceipt {
    /// Task ID
    pub task_id: String,
    /// Agent that executed the task
    pub agent_id: String,
    /// Sectors executed in
    pub sectors: Vec<String>,
    /// Status of execution
    pub status: TaskStatus,
    /// Result data
    pub result: String,
    /// Execution time in milliseconds
    pub execution_time_ms: u64,
    /// Timestamp of completion
    pub timestamp: String,
    /// Merkle root of result (for determinism verification)
    pub result_merkle: String,
    /// Metadata from execution
    pub metadata: HashMap<String, String>,
}

impl TaskReceipt {
    /// Create a new task receipt
    #[must_use]
    pub fn new(
        task_id: String,
        agent_id: String,
        sectors: Vec<String>,
        status: TaskStatus,
        result: String,
    ) -> Self {
        Self {
            task_id,
            agent_id,
            sectors,
            status,
            result,
            execution_time_ms: 0,
            timestamp: chrono::Utc::now().to_rfc3339(),
            result_merkle: String::new(),
            metadata: HashMap::new(),
        }
    }

    /// Set execution time
    #[must_use]
    pub const fn with_execution_time(mut self, ms: u64) -> Self {
        self.execution_time_ms = ms;
        self
    }

    /// Set merkle root of result
    #[must_use]
    pub fn with_merkle(mut self, merkle: String) -> Self {
        self.result_merkle = merkle;
        self
    }

    /// Add metadata
    #[must_use]
    pub fn add_metadata(mut self, key: String, value: String) -> Self {
        self.metadata.insert(key, value);
        self
    }

    /// Check if receipt indicates success
    #[must_use]
    pub fn is_success(&self) -> bool {
        self.status == TaskStatus::Completed
    }
}

/// Task queue for swarm operations
#[derive(Debug, Clone)]
pub struct TaskQueue {
    /// Tasks queued for execution
    tasks: Vec<TaskRequest>,
    /// Completed task receipts
    receipts: Vec<TaskReceipt>,
}

impl TaskQueue {
    /// Create a new task queue
    #[must_use]
    pub const fn new() -> Self {
        Self { tasks: Vec::new(), receipts: Vec::new() }
    }

    /// Enqueue a task
    pub fn enqueue(&mut self, task: TaskRequest) {
        self.tasks.push(task);
        // Sort by priority (higher first)
        self.tasks.sort_by(|a, b| b.priority.cmp(&a.priority));
    }

    /// Dequeue the next task
    pub fn dequeue(&mut self) -> Option<TaskRequest> {
        if self.tasks.is_empty() {
            None
        } else {
            Some(self.tasks.remove(0))
        }
    }

    /// Record task completion
    pub fn record_receipt(&mut self, receipt: TaskReceipt) {
        self.receipts.push(receipt);
    }

    /// Get task count
    #[must_use]
    pub const fn task_count(&self) -> usize {
        self.tasks.len()
    }

    /// Get receipt count
    #[must_use]
    pub const fn receipt_count(&self) -> usize {
        self.receipts.len()
    }

    /// Get all receipts
    #[must_use]
    pub fn receipts(&self) -> &[TaskReceipt] {
        &self.receipts
    }
}

impl Default for TaskQueue {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_task_request_creation() {
        let task = TaskRequest::new(
            "task-001".to_string(),
            "Academic".to_string(),
            "desk-review".to_string(),
            "paper data".to_string(),
        );

        assert_eq!(task.id, "task-001");
        assert_eq!(task.sectors, vec!["Academic"]);
        assert_eq!(task.priority, 0);
    }

    #[test]
    fn test_task_with_priority() {
        let task = TaskRequest::new(
            "task-001".to_string(),
            "Academic".to_string(),
            "desk-review".to_string(),
            "paper data".to_string(),
        )
        .with_priority(100);

        assert_eq!(task.priority, 100);
    }

    #[test]
    fn test_task_multi_sector() {
        let task = TaskRequest::new(
            "task-001".to_string(),
            "Academic".to_string(),
            "review".to_string(),
            "data".to_string(),
        )
        .add_sector("Claims".to_string());

        assert_eq!(task.sectors.len(), 2);
        assert!(task.sectors.contains(&"Academic".to_string()));
        assert!(task.sectors.contains(&"Claims".to_string()));
    }

    #[test]
    fn test_task_receipt_creation() {
        let receipt = TaskReceipt::new(
            "task-001".to_string(),
            "agent-1".to_string(),
            vec!["Academic".to_string()],
            TaskStatus::Completed,
            "success".to_string(),
        );

        assert_eq!(receipt.task_id, "task-001");
        assert_eq!(receipt.agent_id, "agent-1");
        assert!(receipt.is_success());
    }

    #[test]
    fn test_task_queue() {
        let mut queue = TaskQueue::new();

        queue.enqueue(TaskRequest::new(
            "t1".to_string(),
            "Academic".to_string(),
            "op".to_string(),
            "data".to_string(),
        ));

        queue.enqueue(
            TaskRequest::new(
                "t2".to_string(),
                "Claims".to_string(),
                "op".to_string(),
                "data".to_string(),
            )
            .with_priority(10),
        );

        // Higher priority should be dequeued first
        let task = queue.dequeue().unwrap(); // Test code: unwrap is acceptable
        assert_eq!(task.id, "t2");
    }

    #[test]
    fn test_task_status_display() {
        assert_eq!(TaskStatus::Queued.to_string(), "Queued");
        assert_eq!(TaskStatus::Completed.to_string(), "Completed");
        assert_eq!(TaskStatus::Failed.to_string(), "Failed");
    }
}
