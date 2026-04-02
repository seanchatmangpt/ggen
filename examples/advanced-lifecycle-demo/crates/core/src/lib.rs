// Core domain models for the lifecycle demo
// Defines Job, Task, and lifecycle concepts

use async_trait::async_trait;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use thiserror::Error;
use tokio::sync::RwLock;
use uuid::Uuid;

// ============================================================================
// ERRORS
// ============================================================================

#[derive(Debug, Error)]
pub enum CoreError {
    #[error("Job not found: {0}")]
    JobNotFound(Uuid),

    #[error("Invalid job status transition: {current} -> {next}")]
    InvalidStatusTransition { current: String, next: String },

    #[error("Task already completed")]
    TaskCompleted,

    #[error("Job already completed")]
    JobCompleted,

    #[error("Internal error: {0}")]
    Internal(String),
}

pub type CoreResult<T> = Result<T, CoreError>;

// ============================================================================
// DOMAIN TYPES
// ============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum JobStatus {
    Pending,
    Running,
    Paused,
    Completed,
    Failed,
}

impl std::fmt::Display for JobStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JobStatus::Pending => write!(f, "Pending"),
            JobStatus::Running => write!(f, "Running"),
            JobStatus::Paused => write!(f, "Paused"),
            JobStatus::Completed => write!(f, "Completed"),
            JobStatus::Failed => write!(f, "Failed"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum TaskStatus {
    Pending,
    InProgress,
    Completed,
    Failed,
}

impl std::fmt::Display for TaskStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TaskStatus::Pending => write!(f, "Pending"),
            TaskStatus::InProgress => write!(f, "InProgress"),
            TaskStatus::Completed => write!(f, "Completed"),
            TaskStatus::Failed => write!(f, "Failed"),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Task {
    pub id: Uuid,
    pub name: String,
    pub status: TaskStatus,
    pub result: Option<String>,
    pub created_at: DateTime<Utc>,
    pub completed_at: Option<DateTime<Utc>>,
}

impl Task {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            status: TaskStatus::Pending,
            result: None,
            created_at: Utc::now(),
            completed_at: None,
        }
    }

    pub fn mark_in_progress(&mut self) -> CoreResult<()> {
        match self.status {
            TaskStatus::Pending => {
                self.status = TaskStatus::InProgress;
                Ok(())
            }
            _ => Err(CoreError::Internal(
                "Task must be pending to start".to_string(),
            )),
        }
    }

    pub fn complete(&mut self, result: impl Into<String>) -> CoreResult<()> {
        match self.status {
            TaskStatus::InProgress => {
                self.status = TaskStatus::Completed;
                self.result = Some(result.into());
                self.completed_at = Some(Utc::now());
                Ok(())
            }
            _ => Err(CoreError::TaskCompleted),
        }
    }

    pub fn fail(&mut self, error: impl Into<String>) -> CoreResult<()> {
        self.status = TaskStatus::Failed;
        self.result = Some(error.into());
        self.completed_at = Some(Utc::now());
        Ok(())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Job {
    pub id: Uuid,
    pub name: String,
    pub status: JobStatus,
    pub tasks: Vec<Task>,
    pub created_at: DateTime<Utc>,
    pub started_at: Option<DateTime<Utc>>,
    pub completed_at: Option<DateTime<Utc>>,
}

impl Job {
    pub fn new(name: impl Into<String>, tasks: Vec<Task>) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            status: JobStatus::Pending,
            tasks,
            created_at: Utc::now(),
            started_at: None,
            completed_at: None,
        }
    }

    pub fn start(&mut self) -> CoreResult<()> {
        match self.status {
            JobStatus::Pending => {
                self.status = JobStatus::Running;
                self.started_at = Some(Utc::now());
                Ok(())
            }
            _ => Err(CoreError::InvalidStatusTransition {
                current: self.status.to_string(),
                next: "Running".to_string(),
            }),
        }
    }

    pub fn pause(&mut self) -> CoreResult<()> {
        match self.status {
            JobStatus::Running => {
                self.status = JobStatus::Paused;
                Ok(())
            }
            _ => Err(CoreError::InvalidStatusTransition {
                current: self.status.to_string(),
                next: "Paused".to_string(),
            }),
        }
    }

    pub fn resume(&mut self) -> CoreResult<()> {
        match self.status {
            JobStatus::Paused => {
                self.status = JobStatus::Running;
                Ok(())
            }
            _ => Err(CoreError::InvalidStatusTransition {
                current: self.status.to_string(),
                next: "Running".to_string(),
            }),
        }
    }

    pub fn complete(&mut self) -> CoreResult<()> {
        if self.status == JobStatus::Completed {
            return Err(CoreError::JobCompleted);
        }
        self.status = JobStatus::Completed;
        self.completed_at = Some(Utc::now());
        Ok(())
    }

    pub fn fail(&mut self) -> CoreResult<()> {
        self.status = JobStatus::Failed;
        self.completed_at = Some(Utc::now());
        Ok(())
    }

    pub fn all_tasks_completed(&self) -> bool {
        self.tasks
            .iter()
            .all(|t| t.status == TaskStatus::Completed)
    }

    pub fn any_task_failed(&self) -> bool {
        self.tasks.iter().any(|t| t.status == TaskStatus::Failed)
    }
}

// ============================================================================
// REPOSITORY TRAITS
// ============================================================================

#[async_trait]
pub trait JobRepository: Send + Sync {
    async fn create(&self, job: Job) -> CoreResult<Job>;
    async fn get(&self, id: Uuid) -> CoreResult<Job>;
    async fn list(&self) -> CoreResult<Vec<Job>>;
    async fn update(&self, job: Job) -> CoreResult<Job>;
    async fn delete(&self, id: Uuid) -> CoreResult<()>;
}

#[async_trait]
pub trait TaskRepository: Send + Sync {
    async fn create(&self, task: Task) -> CoreResult<Task>;
    async fn get(&self, id: Uuid) -> CoreResult<Task>;
    async fn update(&self, task: Task) -> CoreResult<Task>;
}

// ============================================================================
// IN-MEMORY IMPLEMENTATIONS
// ============================================================================

pub struct InMemoryJobRepository {
    jobs: Arc<RwLock<HashMap<Uuid, Job>>>,
}

impl InMemoryJobRepository {
    pub fn new() -> Self {
        Self {
            jobs: Arc::new(RwLock::new(HashMap::new())),
        }
    }
}

impl Default for InMemoryJobRepository {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl JobRepository for InMemoryJobRepository {
    async fn create(&self, job: Job) -> CoreResult<Job> {
        let mut jobs = self.jobs.write().await;
        jobs.insert(job.id, job.clone());
        Ok(job)
    }

    async fn get(&self, id: Uuid) -> CoreResult<Job> {
        let jobs = self.jobs.read().await;
        jobs.get(&id)
            .cloned()
            .ok_or(CoreError::JobNotFound(id))
    }

    async fn list(&self) -> CoreResult<Vec<Job>> {
        let jobs = self.jobs.read().await;
        Ok(jobs.values().cloned().collect())
    }

    async fn update(&self, job: Job) -> CoreResult<Job> {
        let mut jobs = self.jobs.write().await;
        jobs.insert(job.id, job.clone());
        Ok(job)
    }

    async fn delete(&self, id: Uuid) -> CoreResult<()> {
        let mut jobs = self.jobs.write().await;
        jobs.remove(&id).ok_or(CoreError::JobNotFound(id))?;
        Ok(())
    }
}

pub struct InMemoryTaskRepository {
    tasks: Arc<RwLock<HashMap<Uuid, Task>>>,
}

impl InMemoryTaskRepository {
    pub fn new() -> Self {
        Self {
            tasks: Arc::new(RwLock::new(HashMap::new())),
        }
    }
}

impl Default for InMemoryTaskRepository {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl TaskRepository for InMemoryTaskRepository {
    async fn create(&self, task: Task) -> CoreResult<Task> {
        let mut tasks = self.tasks.write().await;
        tasks.insert(task.id, task.clone());
        Ok(task)
    }

    async fn get(&self, id: Uuid) -> CoreResult<Task> {
        let tasks = self.tasks.read().await;
        tasks.get(&id).cloned().ok_or(CoreError::Internal(
            "Task not found".to_string(),
        ))
    }

    async fn update(&self, task: Task) -> CoreResult<Task> {
        let mut tasks = self.tasks.write().await;
        tasks.insert(task.id, task.clone());
        Ok(task)
    }
}

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_task_creation() {
        let task = Task::new("Sample Task");
        assert_eq!(task.status, TaskStatus::Pending);
        assert_eq!(task.result, None);
    }

    #[test]
    fn test_task_lifecycle() {
        let mut task = Task::new("Task");
        assert!(task.mark_in_progress().is_ok());
        assert_eq!(task.status, TaskStatus::InProgress);
        assert!(task.complete("Success").is_ok());
        assert_eq!(task.status, TaskStatus::Completed);
    }

    #[test]
    fn test_job_creation() {
        let task1 = Task::new("Task 1");
        let task2 = Task::new("Task 2");
        let job = Job::new("Job", vec![task1, task2]);
        assert_eq!(job.status, JobStatus::Pending);
        assert_eq!(job.tasks.len(), 2);
    }

    #[test]
    fn test_job_state_transitions() {
        let job = Job::new("Job", vec![]);
        let mut job = job;
        assert!(job.start().is_ok());
        assert_eq!(job.status, JobStatus::Running);
        assert!(job.pause().is_ok());
        assert_eq!(job.status, JobStatus::Paused);
        assert!(job.resume().is_ok());
        assert_eq!(job.status, JobStatus::Running);
    }

    #[tokio::test]
    async fn test_job_repository() {
        let repo = InMemoryJobRepository::new();
        let job = Job::new("Job", vec![]);
        let created = repo.create(job).await.unwrap();
        let retrieved = repo.get(created.id).await.unwrap();
        assert_eq!(retrieved.id, created.id);
    }

    #[tokio::test]
    async fn test_task_repository() {
        let repo = InMemoryTaskRepository::new();
        let task = Task::new("Task");
        let created = repo.create(task).await.unwrap();
        let retrieved = repo.get(created.id).await.unwrap();
        assert_eq!(retrieved.id, created.id);
    }
}
