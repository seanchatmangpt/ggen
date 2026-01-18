// Scheduler crate - Job orchestration and lifecycle management

use core_domain::{
    Job, JobRepository, JobStatus, TaskRepository, CoreError, CoreResult, Task,
    InMemoryJobRepository, InMemoryTaskRepository,
};
use std::sync::Arc;
use std::time::Duration;
use tokio::time::sleep;
use tracing::{debug, info, warn};

// ============================================================================
// SCHEDULER SERVICE
// ============================================================================

pub struct JobScheduler {
    job_repo: Arc<dyn JobRepository>,
    task_repo: Arc<dyn TaskRepository>,
}

impl JobScheduler {
    pub fn new(job_repo: Arc<dyn JobRepository>, task_repo: Arc<dyn TaskRepository>) -> Self {
        Self { job_repo, task_repo }
    }

    /// Submit a job to the scheduler
    pub async fn submit_job(&self, job: Job) -> CoreResult<Job> {
        debug!("Submitting job: {}", job.name);

        // Create all tasks in the repository
        for task in &job.tasks {
            self.task_repo.create(task.clone()).await?;
        }

        // Create the job
        self.job_repo.create(job).await
    }

    /// Start execution of a job
    pub async fn start_job(&self, id: uuid::Uuid) -> CoreResult<Job> {
        let mut job = self.job_repo.get(id).await?;
        job.start()?;
        info!("Starting job: {}", job.name);
        self.job_repo.update(job).await
    }

    /// Process a single task
    pub async fn process_task(&self, task_id: uuid::Uuid) -> CoreResult<()> {
        let mut task = self.task_repo.get(task_id).await?;

        // Mark as in progress
        task.mark_in_progress()?;
        self.task_repo.update(task.clone()).await?;

        // Simulate work
        debug!("Processing task: {}", task.name);
        sleep(Duration::from_millis(100)).await;

        // Complete the task
        task.complete(format!("Completed: {}", task.name))?;
        self.task_repo.update(task).await?;

        Ok(())
    }

    /// Process all pending tasks in a job
    pub async fn process_job_tasks(&self, job_id: uuid::Uuid) -> CoreResult<()> {
        let job = self.job_repo.get(job_id).await?;

        if job.status != JobStatus::Running {
            return Err(CoreError::Internal(
                "Job must be running to process tasks".to_string(),
            ));
        }

        info!("Processing {} tasks for job: {}", job.tasks.len(), job.name);

        for task in job.tasks {
            self.process_task(task.id).await?;
        }

        Ok(())
    }

    /// Complete a job
    pub async fn complete_job(&self, id: uuid::Uuid) -> CoreResult<Job> {
        let mut job = self.job_repo.get(id).await?;

        // Update job's tasks with latest status from repository
        for i in 0..job.tasks.len() {
            if let Ok(updated_task) = self.task_repo.get(job.tasks[i].id).await {
                job.tasks[i] = updated_task;
            }
        }

        if job.any_task_failed() {
            job.fail()?;
            warn!("Job failed: {} (has failed tasks)", job.name);
        } else {
            job.complete()?;
            info!("Job completed: {}", job.name);
        }

        self.job_repo.update(job).await
    }

    /// Pause a running job
    pub async fn pause_job(&self, id: uuid::Uuid) -> CoreResult<Job> {
        let mut job = self.job_repo.get(id).await?;
        job.pause()?;
        info!("Job paused: {}", job.name);
        self.job_repo.update(job).await
    }

    /// Resume a paused job
    pub async fn resume_job(&self, id: uuid::Uuid) -> CoreResult<Job> {
        let mut job = self.job_repo.get(id).await?;
        job.resume()?;
        info!("Job resumed: {}", job.name);
        self.job_repo.update(job).await
    }

    /// Get job status
    pub async fn get_job_status(&self, id: uuid::Uuid) -> CoreResult<JobStatus> {
        let job = self.job_repo.get(id).await?;
        Ok(job.status)
    }

    /// List all jobs
    pub async fn list_jobs(&self) -> CoreResult<Vec<Job>> {
        self.job_repo.list().await
    }

    /// Execute a complete job workflow
    pub async fn execute_workflow(&self, job_id: uuid::Uuid) -> CoreResult<Job> {
        // Start the job
        self.start_job(job_id).await?;

        // Process all tasks
        self.process_job_tasks(job_id).await?;

        // Complete the job
        self.complete_job(job_id).await
    }
}

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use core_domain::{InMemoryJobRepository, InMemoryTaskRepository};

    #[tokio::test]
    async fn test_scheduler_creation() {
        let job_repo = Arc::new(InMemoryJobRepository::new());
        let task_repo = Arc::new(InMemoryTaskRepository::new());
        let _scheduler = JobScheduler::new(job_repo, task_repo);
    }

    #[tokio::test]
    async fn test_submit_job() {
        let job_repo = Arc::new(InMemoryJobRepository::new());
        let task_repo = Arc::new(InMemoryTaskRepository::new());
        let scheduler = JobScheduler::new(job_repo, task_repo);

        let task = Task::new("Task 1");
        let job = Job::new("Test Job", vec![task]);

        let submitted = scheduler.submit_job(job).await.unwrap();
        assert!(!submitted.id.is_nil());
    }

    #[tokio::test]
    async fn test_start_job() {
        let job_repo = Arc::new(InMemoryJobRepository::new());
        let task_repo = Arc::new(InMemoryTaskRepository::new());
        let scheduler = JobScheduler::new(job_repo, task_repo);

        let job = Job::new("Job", vec![]);
        let submitted = scheduler.submit_job(job).await.unwrap();

        let started = scheduler.start_job(submitted.id).await.unwrap();
        assert_eq!(started.status, JobStatus::Running);
    }

    #[tokio::test]
    async fn test_execute_workflow() {
        let job_repo = Arc::new(InMemoryJobRepository::new());
        let task_repo = Arc::new(InMemoryTaskRepository::new());
        let scheduler = JobScheduler::new(job_repo, task_repo);

        let task1 = Task::new("Task 1");
        let task2 = Task::new("Task 2");
        let job = Job::new("Workflow Job", vec![task1, task2]);

        let submitted = scheduler.submit_job(job).await.unwrap();
        let completed = scheduler.execute_workflow(submitted.id).await.unwrap();

        assert_eq!(completed.status, JobStatus::Completed);
        assert!(completed.all_tasks_completed());
    }

    #[tokio::test]
    async fn test_pause_and_resume() {
        let job_repo = Arc::new(InMemoryJobRepository::new());
        let task_repo = Arc::new(InMemoryTaskRepository::new());
        let scheduler = JobScheduler::new(job_repo, task_repo);

        let job = Job::new("Job", vec![]);
        let submitted = scheduler.submit_job(job).await.unwrap();

        let started = scheduler.start_job(submitted.id).await.unwrap();
        assert_eq!(started.status, JobStatus::Running);

        let paused = scheduler.pause_job(submitted.id).await.unwrap();
        assert_eq!(paused.status, JobStatus::Paused);

        let resumed = scheduler.resume_job(submitted.id).await.unwrap();
        assert_eq!(resumed.status, JobStatus::Running);
    }

    #[tokio::test]
    async fn test_get_job_status() {
        let job_repo = Arc::new(InMemoryJobRepository::new());
        let task_repo = Arc::new(InMemoryTaskRepository::new());
        let scheduler = JobScheduler::new(job_repo, task_repo);

        let job = Job::new("Job", vec![]);
        let submitted = scheduler.submit_job(job).await.unwrap();

        let status = scheduler.get_job_status(submitted.id).await.unwrap();
        assert_eq!(status, JobStatus::Pending);
    }

    #[tokio::test]
    async fn test_list_jobs() {
        let job_repo = Arc::new(InMemoryJobRepository::new());
        let task_repo = Arc::new(InMemoryTaskRepository::new());
        let scheduler = JobScheduler::new(job_repo, task_repo);

        scheduler.submit_job(Job::new("Job 1", vec![])).await.unwrap();
        scheduler.submit_job(Job::new("Job 2", vec![])).await.unwrap();

        let jobs = scheduler.list_jobs().await.unwrap();
        assert_eq!(jobs.len(), 2);
    }
}
