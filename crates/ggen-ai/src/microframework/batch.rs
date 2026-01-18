//! Batch processing utilities for parallel task execution
//!
//! Provides utilities for efficiently processing large batches of tasks
//! with configurable parallelism, chunking, and result aggregation.

use super::orchestrator::AgentOrchestrator;
use super::tasks::{Task, TaskResult, TaskType};
use crate::error::Result;
use serde::{Deserialize, Serialize};
use std::sync::Arc;

/// Batch processing configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BatchConfig {
    /// Maximum tasks per batch (1-10 for agent execution)
    pub max_batch_size: usize,
    /// Continue on individual task failures
    pub continue_on_failure: bool,
    /// Maximum retries per task
    pub max_retries: u32,
    /// Collect partial results on failure
    pub collect_partial: bool,
    /// Enable progress reporting
    pub report_progress: bool,
    /// Delay between batches in milliseconds
    pub batch_delay_ms: u64,
}

impl Default for BatchConfig {
    fn default() -> Self {
        Self {
            max_batch_size: 10, // Maximum Claude Code agent concurrency
            continue_on_failure: true,
            max_retries: 2,
            collect_partial: true,
            report_progress: true,
            batch_delay_ms: 0,
        }
    }
}

impl BatchConfig {
    /// Create a strict configuration that stops on first failure
    pub fn strict() -> Self {
        Self {
            continue_on_failure: false,
            collect_partial: false,
            ..Default::default()
        }
    }

    /// Create a lenient configuration that continues despite failures
    pub fn lenient() -> Self {
        Self {
            continue_on_failure: true,
            max_retries: 5,
            collect_partial: true,
            ..Default::default()
        }
    }
}

/// Batch processor for executing large task sets
pub struct BatchProcessor {
    /// Orchestrator for task execution
    orchestrator: Arc<AgentOrchestrator>,
    /// Configuration
    config: BatchConfig,
}

impl BatchProcessor {
    /// Create a new batch processor
    pub fn new(orchestrator: Arc<AgentOrchestrator>) -> Self {
        Self {
            orchestrator,
            config: BatchConfig::default(),
        }
    }

    /// Create with custom configuration
    pub fn with_config(orchestrator: Arc<AgentOrchestrator>, config: BatchConfig) -> Self {
        Self {
            orchestrator,
            config,
        }
    }

    /// Process a batch of tasks
    pub async fn process(&self, tasks: Vec<Task>) -> Result<BatchResult> {
        let total = tasks.len();
        let mut all_results = Vec::with_capacity(total);
        let mut failed_count = 0;
        let start = std::time::Instant::now();

        // Process in chunks
        for (batch_num, chunk) in tasks.chunks(self.config.max_batch_size).enumerate() {
            if self.config.report_progress {
                let progress = (batch_num * self.config.max_batch_size * 100) / total;
                tracing::info!(
                    "Processing batch {}/{} ({}%)",
                    batch_num + 1,
                    (total + self.config.max_batch_size - 1) / self.config.max_batch_size,
                    progress
                );
            }

            // Execute batch
            let batch_results = self
                .orchestrator
                .execute_batch(chunk.to_vec())
                .await?;

            // Process results
            for result in batch_results {
                if !result.is_success() {
                    failed_count += 1;

                    if !self.config.continue_on_failure {
                        return Ok(BatchResult {
                            total,
                            successful: all_results.iter().filter(|r: &&TaskResult| r.is_success()).count(),
                            failed: failed_count,
                            results: if self.config.collect_partial {
                                all_results
                            } else {
                                Vec::new()
                            },
                            duration_ms: start.elapsed().as_millis() as u64,
                            error: Some("Batch stopped on failure".to_string()),
                        });
                    }
                }
                all_results.push(result);
            }

            // Optional delay between batches
            if self.config.batch_delay_ms > 0 {
                tokio::time::sleep(tokio::time::Duration::from_millis(
                    self.config.batch_delay_ms,
                ))
                .await;
            }
        }

        let successful = all_results.iter().filter(|r| r.is_success()).count();

        Ok(BatchResult {
            total,
            successful,
            failed: failed_count,
            results: all_results,
            duration_ms: start.elapsed().as_millis() as u64,
            error: None,
        })
    }

    /// Process with a transformation function
    pub async fn process_with_transform<F, T>(
        &self,
        items: Vec<T>,
        transform: F,
    ) -> Result<BatchResult>
    where
        F: Fn(T) -> Task,
        T: Send,
    {
        let tasks: Vec<Task> = items.into_iter().map(transform).collect();
        self.process(tasks).await
    }

    /// Process items in parallel, mapping results
    pub async fn map<T, R, F, M>(
        &self,
        items: Vec<T>,
        to_task: F,
        from_result: M,
    ) -> Result<Vec<Option<R>>>
    where
        F: Fn(&T) -> Task,
        M: Fn(&TaskResult) -> Option<R>,
        T: Send,
    {
        let tasks: Vec<Task> = items.iter().map(&to_task).collect();
        let result = self.process(tasks).await?;

        Ok(result.results.iter().map(&from_result).collect())
    }

    /// Filter items based on task success
    pub async fn filter<T, F>(&self, items: Vec<T>, predicate: F) -> Result<Vec<T>>
    where
        F: Fn(&T) -> Task,
        T: Send + Clone,
    {
        let tasks: Vec<Task> = items.iter().map(&predicate).collect();
        let result = self.process(tasks).await?;

        let filtered: Vec<T> = items
            .into_iter()
            .zip(result.results.iter())
            .filter(|(_, r)| r.is_success())
            .map(|(item, _)| item)
            .collect();

        Ok(filtered)
    }
}

/// Result from batch processing
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BatchResult {
    /// Total tasks processed
    pub total: usize,
    /// Successful task count
    pub successful: usize,
    /// Failed task count
    pub failed: usize,
    /// Individual task results
    pub results: Vec<TaskResult>,
    /// Total processing time in milliseconds
    pub duration_ms: u64,
    /// Error message if batch failed
    pub error: Option<String>,
}

impl BatchResult {
    /// Check if all tasks succeeded
    pub fn all_succeeded(&self) -> bool {
        self.failed == 0
    }

    /// Get success rate
    pub fn success_rate(&self) -> f64 {
        if self.total > 0 {
            self.successful as f64 / self.total as f64
        } else {
            1.0
        }
    }

    /// Get failed results
    pub fn failures(&self) -> Vec<&TaskResult> {
        self.results.iter().filter(|r| !r.is_success()).collect()
    }

    /// Get successful results
    pub fn successes(&self) -> Vec<&TaskResult> {
        self.results.iter().filter(|r| r.is_success()).collect()
    }

    /// Average task duration
    pub fn avg_task_duration_ms(&self) -> f64 {
        if self.results.is_empty() {
            0.0
        } else {
            self.results.iter().map(|r| r.duration_ms).sum::<u64>() as f64
                / self.results.len() as f64
        }
    }
}

/// Batch builder for fluent task creation
pub struct BatchBuilder {
    /// Tasks being built
    tasks: Vec<Task>,
    /// Configuration
    config: BatchConfig,
}

impl BatchBuilder {
    /// Create a new batch builder
    pub fn new() -> Self {
        Self {
            tasks: Vec::new(),
            config: BatchConfig::default(),
        }
    }

    /// Add a task
    pub fn add(mut self, task: Task) -> Self {
        self.tasks.push(task);
        self
    }

    /// Add multiple tasks
    pub fn add_all(mut self, tasks: impl IntoIterator<Item = Task>) -> Self {
        self.tasks.extend(tasks);
        self
    }

    /// Add a code generation task
    pub fn code_gen(mut self, description: &str) -> Self {
        self.tasks.push(Task::new(TaskType::CodeGen, description));
        self
    }

    /// Add a test task
    pub fn test(mut self, description: &str) -> Self {
        self.tasks.push(Task::new(TaskType::Test, description));
        self
    }

    /// Add a review task
    pub fn review(mut self, description: &str) -> Self {
        self.tasks.push(Task::new(TaskType::Review, description));
        self
    }

    /// Add a validation task
    pub fn validate(mut self, description: &str) -> Self {
        self.tasks.push(Task::new(TaskType::Validate, description));
        self
    }

    /// Set configuration
    pub fn with_config(mut self, config: BatchConfig) -> Self {
        self.config = config;
        self
    }

    /// Set max batch size
    pub fn max_batch_size(mut self, size: usize) -> Self {
        self.config.max_batch_size = size.min(10).max(1);
        self
    }

    /// Enable strict mode (stop on failure)
    pub fn strict(mut self) -> Self {
        self.config.continue_on_failure = false;
        self
    }

    /// Enable lenient mode (continue on failure)
    pub fn lenient(mut self) -> Self {
        self.config.continue_on_failure = true;
        self
    }

    /// Get the tasks
    pub fn tasks(self) -> Vec<Task> {
        self.tasks
    }

    /// Get task count
    pub fn count(&self) -> usize {
        self.tasks.len()
    }

    /// Process the batch
    pub async fn process(self, orchestrator: Arc<AgentOrchestrator>) -> Result<BatchResult> {
        let processor = BatchProcessor::with_config(orchestrator, self.config);
        processor.process(self.tasks).await
    }
}

impl Default for BatchBuilder {
    fn default() -> Self {
        Self::new()
    }
}

/// Partition tasks into groups for parallel processing
pub fn partition_tasks(tasks: Vec<Task>, group_size: usize) -> Vec<Vec<Task>> {
    let group_size = group_size.min(10).max(1);
    tasks.chunks(group_size).map(|c| c.to_vec()).collect()
}

/// Merge multiple batch results
pub fn merge_results(results: Vec<BatchResult>) -> BatchResult {
    let mut merged = BatchResult {
        total: 0,
        successful: 0,
        failed: 0,
        results: Vec::new(),
        duration_ms: 0,
        error: None,
    };

    for result in results {
        merged.total += result.total;
        merged.successful += result.successful;
        merged.failed += result.failed;
        merged.results.extend(result.results);
        merged.duration_ms = merged.duration_ms.max(result.duration_ms);

        if result.error.is_some() && merged.error.is_none() {
            merged.error = result.error;
        }
    }

    merged
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::microframework::agents::CodeGenAgent;

    #[test]
    fn test_batch_config_default() {
        let config = BatchConfig::default();
        assert_eq!(config.max_batch_size, 10);
        assert!(config.continue_on_failure);
    }

    #[test]
    fn test_batch_config_strict() {
        let config = BatchConfig::strict();
        assert!(!config.continue_on_failure);
        assert!(!config.collect_partial);
    }

    #[test]
    fn test_batch_builder() {
        let batch = BatchBuilder::new()
            .code_gen("Generate struct")
            .test("Run tests")
            .review("Review code")
            .validate("Validate output");

        assert_eq!(batch.count(), 4);
    }

    #[test]
    fn test_partition_tasks() {
        let tasks: Vec<Task> = (0..25)
            .map(|i| Task::new(TaskType::CodeGen, &format!("Task {}", i)))
            .collect();

        let partitions = partition_tasks(tasks, 10);
        assert_eq!(partitions.len(), 3);
        assert_eq!(partitions[0].len(), 10);
        assert_eq!(partitions[1].len(), 10);
        assert_eq!(partitions[2].len(), 5);
    }

    #[test]
    fn test_merge_results() {
        let results = vec![
            BatchResult {
                total: 5,
                successful: 4,
                failed: 1,
                results: Vec::new(),
                duration_ms: 100,
                error: None,
            },
            BatchResult {
                total: 5,
                successful: 5,
                failed: 0,
                results: Vec::new(),
                duration_ms: 200,
                error: None,
            },
        ];

        let merged = merge_results(results);
        assert_eq!(merged.total, 10);
        assert_eq!(merged.successful, 9);
        assert_eq!(merged.failed, 1);
        assert_eq!(merged.duration_ms, 200);
    }

    #[test]
    fn test_batch_result_metrics() {
        let result = BatchResult {
            total: 10,
            successful: 8,
            failed: 2,
            results: Vec::new(),
            duration_ms: 1000,
            error: None,
        };

        assert!(!result.all_succeeded());
        assert_eq!(result.success_rate(), 0.8);
    }

    #[tokio::test]
    async fn test_batch_processor() {
        let orchestrator = Arc::new(AgentOrchestrator::new());
        orchestrator.register_agent(CodeGenAgent::new("coder"));

        let tasks = vec![
            Task::new(TaskType::CodeGen, "Task 1"),
            Task::new(TaskType::CodeGen, "Task 2"),
        ];

        let processor = BatchProcessor::new(orchestrator);
        let result = processor.process(tasks).await.unwrap();

        assert_eq!(result.total, 2);
        assert!(result.all_succeeded());
    }

    #[tokio::test]
    async fn test_batch_builder_process() {
        let orchestrator = Arc::new(AgentOrchestrator::new());
        orchestrator.register_agent(CodeGenAgent::new("coder"));

        let result = BatchBuilder::new()
            .code_gen("Generate code 1")
            .code_gen("Generate code 2")
            .max_batch_size(10)
            .process(orchestrator)
            .await
            .unwrap();

        assert_eq!(result.total, 2);
    }
}
