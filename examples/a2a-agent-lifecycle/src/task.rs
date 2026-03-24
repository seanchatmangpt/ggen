//! Task management with priorities, dependencies, and artifact tracking

use anyhow::Result;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet, VecDeque};
use uuid::Uuid;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum TaskPriority {
    Low = 1,
    Normal = 2,
    High = 3,
    Critical = 4,
}

impl TaskPriority {
    pub fn value(&self) -> u8 {
        *self as u8
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TaskStatus {
    Pending,
    InProgress,
    Completed,
    Failed,
}

impl TaskStatus {
    pub fn code(&self) -> &'static str {
        match self {
            TaskStatus::Pending => "PENDING",
            TaskStatus::InProgress => "IN_PROGRESS",
            TaskStatus::Completed => "COMPLETED",
            TaskStatus::Failed => "FAILED",
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Task {
    pub id: String,
    pub name: String,
    pub description: Option<String>,
    pub priority: TaskPriority,
    status: TaskStatus,
    pub assigned_agent: Option<String>,
    pub depends_on: HashSet<String>,
    pub artifacts: HashMap<String, Artifact>,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
    pub started_at: Option<DateTime<Utc>>,
    pub completed_at: Option<DateTime<Utc>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Artifact {
    pub id: String,
    pub name: String,
    pub artifact_type: String,
    pub data: serde_json::Value,
    pub created_at: DateTime<Utc>,
}

impl Task {
    pub fn new(name: impl Into<String>, priority: TaskPriority) -> Self {
        let id = Uuid::new_v4().to_string();
        let now = Utc::now();

        Task {
            id,
            name: name.into(),
            description: None,
            priority,
            status: TaskStatus::Pending,
            assigned_agent: None,
            depends_on: HashSet::new(),
            artifacts: HashMap::new(),
            created_at: now,
            updated_at: now,
            started_at: None,
            completed_at: None,
        }
    }

    pub fn with_description(mut self, desc: impl Into<String>) -> Self {
        self.description = Some(desc.into());
        self
    }

    pub fn status(&self) -> TaskStatus {
        self.status
    }

    pub fn status_code(&self) -> &'static str {
        self.status.code()
    }

    pub fn is_pending(&self) -> bool {
        self.status == TaskStatus::Pending
    }

    pub fn is_in_progress(&self) -> bool {
        self.status == TaskStatus::InProgress
    }

    pub fn is_completed(&self) -> bool {
        self.status == TaskStatus::Completed
    }

    pub fn is_failed(&self) -> bool {
        self.status == TaskStatus::Failed
    }

    /// Mark task as in progress
    pub fn mark_in_progress(&mut self, agent_id: impl Into<String>) -> Result<()> {
        if self.status != TaskStatus::Pending {
            return Err(anyhow::anyhow!(
                "Cannot mark task as in progress: current status is {}",
                self.status_code()
            ));
        }

        self.status = TaskStatus::InProgress;
        self.assigned_agent = Some(agent_id.into());
        self.started_at = Some(Utc::now());
        self.updated_at = Utc::now();

        Ok(())
    }

    /// Mark task as completed
    pub fn mark_completed(&mut self) -> Result<()> {
        if self.status != TaskStatus::InProgress {
            return Err(anyhow::anyhow!(
                "Cannot mark task as completed: current status is {}",
                self.status_code()
            ));
        }

        self.status = TaskStatus::Completed;
        self.completed_at = Some(Utc::now());
        self.updated_at = Utc::now();

        Ok(())
    }

    /// Mark task as failed
    pub fn mark_failed(&mut self) -> Result<()> {
        if self.status == TaskStatus::Completed {
            return Err(anyhow::anyhow!("Cannot fail a completed task"));
        }

        self.status = TaskStatus::Failed;
        self.completed_at = Some(Utc::now());
        self.updated_at = Utc::now();

        Ok(())
    }

    /// Add a dependency on another task
    pub fn add_dependency(&mut self, task_id: impl Into<String>) {
        self.depends_on.insert(task_id.into());
    }

    /// Add an artifact to the task
    pub fn add_artifact(&mut self, artifact: Artifact) {
        self.artifacts.insert(artifact.id.clone(), artifact);
    }

    /// Create and add an artifact
    pub fn create_artifact(
        &mut self,
        name: impl Into<String>,
        artifact_type: impl Into<String>,
        data: serde_json::Value,
    ) -> String {
        let artifact = Artifact {
            id: Uuid::new_v4().to_string(),
            name: name.into(),
            artifact_type: artifact_type.into(),
            data,
            created_at: Utc::now(),
        };
        let id = artifact.id.clone();
        self.add_artifact(artifact);
        id
    }

    /// Get artifact by ID
    pub fn get_artifact(&self, id: &str) -> Option<&Artifact> {
        self.artifacts.get(id)
    }

    /// Get execution time if task has been started
    pub fn execution_time_ms(&self) -> Option<i64> {
        let start = self.started_at?;
        let end = self.completed_at.unwrap_or_else(Utc::now);
        Some((end - start).num_milliseconds())
    }

    /// Check if all dependencies are satisfied
    pub fn dependencies_satisfied(&self, completed_tasks: &HashSet<String>) -> bool {
        self.depends_on.is_empty() || self.depends_on.iter().all(|id| completed_tasks.contains(id))
    }
}

pub struct TaskManager {
    tasks: HashMap<String, Task>,
    queue: VecDeque<String>, // Task IDs sorted by priority
    completed: HashSet<String>,
}

impl TaskManager {
    pub fn new() -> Self {
        TaskManager {
            tasks: HashMap::new(),
            queue: VecDeque::new(),
            completed: HashSet::new(),
        }
    }

    pub fn add_task(&mut self, task: Task) {
        let id = task.id.clone();
        self.tasks.insert(id.clone(), task);
        self.queue.push_back(id);
        self.sort_queue();
    }

    pub fn get_task(&self, id: &str) -> Option<&Task> {
        self.tasks.get(id)
    }

    pub fn get_task_mut(&mut self, id: &str) -> Option<&mut Task> {
        self.tasks.get_mut(id)
    }

    /// Get next task by priority that has all dependencies satisfied
    pub fn get_next_task(&mut self) -> Option<&mut Task> {
        while let Some(id) = self.queue.pop_front() {
            if let Some(task) = self.tasks.get(&id) {
                if task.is_pending() && task.dependencies_satisfied(&self.completed) {
                    return self.tasks.get_mut(&id);
                }
            }
            // Re-queue if not ready
            self.queue.push_back(id);
        }
        None
    }

    /// Mark task as completed
    pub fn complete_task(&mut self, id: &str) -> Result<()> {
        if let Some(task) = self.tasks.get_mut(id) {
            task.mark_completed()?;
            self.completed.insert(id.to_string());
        }
        Ok(())
    }

    /// Get all tasks
    pub fn tasks(&self) -> impl Iterator<Item = &Task> {
        self.tasks.values()
    }

    /// Get completed tasks count
    pub fn completed_count(&self) -> usize {
        self.completed.len()
    }

    /// Get pending tasks count
    pub fn pending_count(&self) -> usize {
        self.tasks
            .values()
            .filter(|t| t.is_pending())
            .count()
    }

    /// Sort queue by priority (highest first)
    fn sort_queue(&mut self) {
        let mut items: Vec<_> = self.queue.iter().cloned().collect();
        items.sort_by(|a, b| {
            let task_a = self.tasks.get(a);
            let task_b = self.tasks.get(b);

            match (task_a, task_b) {
                (Some(ta), Some(tb)) => tb.priority.cmp(&ta.priority),
                (Some(_), None) => std::cmp::Ordering::Less,
                (None, Some(_)) => std::cmp::Ordering::Greater,
                (None, None) => std::cmp::Ordering::Equal,
            }
        });

        self.queue = items.into_iter().collect();
    }
}

impl Default for TaskManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_task_creation() {
        let task = Task::new("TestTask", TaskPriority::Normal);
        assert_eq!(task.status, TaskStatus::Pending);
        assert_eq!(task.priority, TaskPriority::Normal);
        assert!(task.is_pending());
    }

    #[test]
    fn test_task_lifecycle() {
        let mut task = Task::new("TestTask", TaskPriority::Normal);

        assert!(task.mark_in_progress("agent-1").is_ok());
        assert!(task.is_in_progress());

        assert!(task.mark_completed().is_ok());
        assert!(task.is_completed());
    }

    #[test]
    fn test_invalid_transition() {
        let mut task = Task::new("TestTask", TaskPriority::Normal);
        task.mark_in_progress("agent-1").unwrap();

        // Cannot mark completed task as in progress
        task.mark_completed().unwrap();
        assert!(task.mark_in_progress("agent-2").is_err());
    }

    #[test]
    fn test_task_artifacts() {
        let mut task = Task::new("TestTask", TaskPriority::Normal);
        let artifact_id = task.create_artifact(
            "output.txt",
            "text",
            serde_json::json!({"content": "test"}),
        );

        assert!(task.get_artifact(&artifact_id).is_some());
        assert_eq!(task.artifacts.len(), 1);
    }

    #[test]
    fn test_task_dependencies() {
        let mut task = Task::new("TaskA", TaskPriority::Normal);
        task.add_dependency("task-1");
        task.add_dependency("task-2");

        assert!(!task.depends_on.is_empty());
        assert!(task.depends_on.contains("task-1"));

        let mut completed = HashSet::new();
        assert!(!task.dependencies_satisfied(&completed));

        completed.insert("task-1".to_string());
        assert!(!task.dependencies_satisfied(&completed));

        completed.insert("task-2".to_string());
        assert!(task.dependencies_satisfied(&completed));
    }

    #[test]
    fn test_task_manager() {
        let mut manager = TaskManager::new();

        let task1 = Task::new("Task1", TaskPriority::Normal);
        let task2 = Task::new("Task2", TaskPriority::High);

        manager.add_task(task1);
        manager.add_task(task2);

        assert_eq!(manager.pending_count(), 2);
        assert_eq!(manager.completed_count(), 0);
    }

    #[test]
    fn test_task_priority_ordering() {
        let mut manager = TaskManager::new();

        let task_low = Task::new("LowTask", TaskPriority::Low);
        let task_critical = Task::new("CriticalTask", TaskPriority::Critical);
        let task_normal = Task::new("NormalTask", TaskPriority::Normal);

        manager.add_task(task_low);
        manager.add_task(task_critical);
        manager.add_task(task_normal);

        // Queue should be sorted by priority
        let items: Vec<_> = manager.queue.iter().cloned().collect();
        let critical_idx = items
            .iter()
            .position(|id| {
                manager
                    .tasks
                    .get(id)
                    .map(|t| t.name == "CriticalTask")
                    .unwrap_or(false)
            })
            .unwrap();
        let normal_idx = items
            .iter()
            .position(|id| {
                manager
                    .tasks
                    .get(id)
                    .map(|t| t.name == "NormalTask")
                    .unwrap_or(false)
            })
            .unwrap();

        assert!(critical_idx < normal_idx);
    }

    #[test]
    fn test_task_execution_time() {
        let mut task = Task::new("TestTask", TaskPriority::Normal);
        assert!(task.execution_time_ms().is_none());

        task.mark_in_progress("agent-1").unwrap();
        task.mark_completed().unwrap();

        assert!(task.execution_time_ms().is_some());
    }
}
