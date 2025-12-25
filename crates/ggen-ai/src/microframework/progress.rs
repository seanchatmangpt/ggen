//! Progress tracking and reporting for agent execution
//!
//! Provides real-time visibility into task execution progress
//! with support for hierarchical progress and ETA estimation.

use super::tasks::{Task, TaskStatus};
use crate::error::Result;
use dashmap::DashMap;
use serde::{Deserialize, Serialize};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

/// Progress tracker for monitoring task execution
#[derive(Debug)]
pub struct ProgressTracker {
    /// Active task progress
    tasks: DashMap<String, TaskProgress>,
    /// Completed task count
    completed: AtomicU64,
    /// Failed task count
    failed: AtomicU64,
    /// Total task count
    total: AtomicU64,
    /// Start time
    start_time: Instant,
    /// Progress listeners
    listeners: DashMap<String, Arc<dyn ProgressListener>>,
}

/// Progress information for a single task
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TaskProgress {
    /// Task ID
    pub task_id: String,
    /// Task description
    pub description: String,
    /// Current status
    pub status: TaskStatus,
    /// Progress percentage (0-100)
    pub percent: u8,
    /// Current step description
    pub current_step: String,
    /// Start timestamp (ms since epoch)
    pub started_at: u64,
    /// Elapsed time in milliseconds
    pub elapsed_ms: u64,
    /// Estimated remaining time in milliseconds
    pub eta_ms: Option<u64>,
    /// Number of retries
    pub retries: u32,
}

impl TaskProgress {
    /// Create new progress for a task
    pub fn new(task: &Task) -> Self {
        Self {
            task_id: task.id.clone(),
            description: task.description.clone(),
            status: TaskStatus::Pending,
            percent: 0,
            current_step: "Waiting".to_string(),
            started_at: chrono::Utc::now().timestamp_millis() as u64,
            elapsed_ms: 0,
            eta_ms: None,
            retries: 0,
        }
    }

    /// Update progress
    pub fn update(&mut self, percent: u8, step: &str) {
        self.percent = percent.min(100);
        self.current_step = step.to_string();

        // Estimate remaining time based on progress
        if percent > 0 && percent < 100 {
            let rate = self.elapsed_ms as f64 / percent as f64;
            let remaining = (100 - percent) as f64 * rate;
            self.eta_ms = Some(remaining as u64);
        }
    }

    /// Mark as running
    pub fn start(&mut self) {
        self.status = TaskStatus::Running;
        self.current_step = "Running".to_string();
        self.started_at = chrono::Utc::now().timestamp_millis() as u64;
    }

    /// Mark as completed
    pub fn complete(&mut self, success: bool) {
        self.status = if success {
            TaskStatus::Completed
        } else {
            TaskStatus::Failed
        };
        self.percent = 100;
        self.current_step = if success { "Completed" } else { "Failed" }.to_string();
        self.eta_ms = Some(0);
    }
}

impl ProgressTracker {
    /// Create a new progress tracker
    pub fn new() -> Self {
        Self {
            tasks: DashMap::new(),
            completed: AtomicU64::new(0),
            failed: AtomicU64::new(0),
            total: AtomicU64::new(0),
            start_time: Instant::now(),
            listeners: DashMap::new(),
        }
    }

    /// Start tracking a task
    pub fn track_task(&self, task: &Task) {
        let progress = TaskProgress::new(task);
        self.tasks.insert(task.id.clone(), progress);
        self.total.fetch_add(1, Ordering::Relaxed);
        self.notify_listeners(&task.id, ProgressEvent::TaskAdded);
    }

    /// Mark task as started
    pub fn start_task(&self, task_id: &str) {
        if let Some(mut progress) = self.tasks.get_mut(task_id) {
            progress.start();
            self.notify_listeners(task_id, ProgressEvent::TaskStarted);
        }
    }

    /// Update task progress
    pub fn update_task(&self, task_id: &str, percent: u8, step: &str) {
        if let Some(mut progress) = self.tasks.get_mut(task_id) {
            progress.elapsed_ms = self.start_time.elapsed().as_millis() as u64;
            progress.update(percent, step);
            self.notify_listeners(task_id, ProgressEvent::ProgressUpdated);
        }
    }

    /// Mark task as completed
    pub fn complete_task(&self, task_id: &str, success: bool) {
        if let Some(mut progress) = self.tasks.get_mut(task_id) {
            progress.elapsed_ms = self.start_time.elapsed().as_millis() as u64;
            progress.complete(success);

            if success {
                self.completed.fetch_add(1, Ordering::Relaxed);
            } else {
                self.failed.fetch_add(1, Ordering::Relaxed);
            }

            self.notify_listeners(task_id, ProgressEvent::TaskCompleted);
        }
    }

    /// Increment retry count for a task
    pub fn record_retry(&self, task_id: &str) {
        if let Some(mut progress) = self.tasks.get_mut(task_id) {
            progress.retries += 1;
            self.notify_listeners(task_id, ProgressEvent::TaskRetried);
        }
    }

    /// Get progress for a specific task
    pub fn get_task_progress(&self, task_id: &str) -> Option<TaskProgress> {
        self.tasks.get(task_id).map(|p| p.clone())
    }

    /// Get all task progress
    pub fn all_progress(&self) -> Vec<TaskProgress> {
        self.tasks.iter().map(|p| p.clone()).collect()
    }

    /// Get overall progress summary
    pub fn summary(&self) -> ProgressSummary {
        let total = self.total.load(Ordering::Relaxed);
        let completed = self.completed.load(Ordering::Relaxed);
        let failed = self.failed.load(Ordering::Relaxed);
        let elapsed = self.start_time.elapsed();

        let pending = total.saturating_sub(completed + failed);
        let running = self.tasks
            .iter()
            .filter(|p| matches!(p.status, TaskStatus::Running))
            .count() as u64;

        let percent = if total > 0 {
            ((completed + failed) * 100 / total) as u8
        } else {
            0
        };

        // Calculate ETA based on average completion time
        let eta_ms = if completed > 0 && pending > 0 {
            let avg_time = elapsed.as_millis() as u64 / completed;
            Some(avg_time * pending)
        } else {
            None
        };

        ProgressSummary {
            total,
            completed,
            failed,
            running,
            pending,
            percent,
            elapsed_ms: elapsed.as_millis() as u64,
            eta_ms,
        }
    }

    /// Register a progress listener
    pub fn add_listener(&self, id: &str, listener: Arc<dyn ProgressListener>) {
        self.listeners.insert(id.to_string(), listener);
    }

    /// Remove a progress listener
    pub fn remove_listener(&self, id: &str) {
        self.listeners.remove(id);
    }

    /// Reset the tracker
    pub fn reset(&self) {
        self.tasks.clear();
        self.completed.store(0, Ordering::Relaxed);
        self.failed.store(0, Ordering::Relaxed);
        self.total.store(0, Ordering::Relaxed);
    }

    /// Notify all listeners of an event
    fn notify_listeners(&self, task_id: &str, event: ProgressEvent) {
        for listener in self.listeners.iter() {
            listener.on_progress(task_id, &event);
        }
    }
}

impl Default for ProgressTracker {
    fn default() -> Self {
        Self::new()
    }
}

/// Progress summary for all tasks
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProgressSummary {
    /// Total number of tasks
    pub total: u64,
    /// Completed tasks
    pub completed: u64,
    /// Failed tasks
    pub failed: u64,
    /// Currently running tasks
    pub running: u64,
    /// Pending tasks
    pub pending: u64,
    /// Overall completion percentage
    pub percent: u8,
    /// Total elapsed time in milliseconds
    pub elapsed_ms: u64,
    /// Estimated time remaining in milliseconds
    pub eta_ms: Option<u64>,
}

impl ProgressSummary {
    /// Get success rate
    pub fn success_rate(&self) -> f64 {
        if self.completed + self.failed > 0 {
            self.completed as f64 / (self.completed + self.failed) as f64
        } else {
            1.0
        }
    }

    /// Check if all tasks are done
    pub fn is_done(&self) -> bool {
        self.pending == 0 && self.running == 0
    }

    /// Format as human-readable string
    pub fn format(&self) -> String {
        let eta = self.eta_ms
            .map(|ms| format!(" (ETA: {}s)", ms / 1000))
            .unwrap_or_default();

        format!(
            "[{}/{}] {}% complete, {} running, {} failed{}",
            self.completed + self.failed,
            self.total,
            self.percent,
            self.running,
            self.failed,
            eta
        )
    }
}

/// Progress event types
#[derive(Debug, Clone)]
pub enum ProgressEvent {
    /// Task was added
    TaskAdded,
    /// Task started executing
    TaskStarted,
    /// Task progress was updated
    ProgressUpdated,
    /// Task completed (success or failure)
    TaskCompleted,
    /// Task was retried
    TaskRetried,
}

/// Listener for progress events
pub trait ProgressListener: Send + Sync + std::fmt::Debug {
    /// Called when progress changes
    fn on_progress(&self, task_id: &str, event: &ProgressEvent);
}

/// Console progress listener (prints to stdout)
#[derive(Debug)]
pub struct ConsoleProgressListener {
    /// Show individual task updates
    pub verbose: bool,
}

impl ConsoleProgressListener {
    /// Create a new console listener
    pub fn new(verbose: bool) -> Self {
        Self { verbose }
    }
}

impl ProgressListener for ConsoleProgressListener {
    fn on_progress(&self, task_id: &str, event: &ProgressEvent) {
        if self.verbose {
            match event {
                ProgressEvent::TaskStarted => {
                    println!("▶ Started: {}", task_id);
                }
                ProgressEvent::TaskCompleted => {
                    println!("✓ Completed: {}", task_id);
                }
                ProgressEvent::TaskRetried => {
                    println!("⟳ Retrying: {}", task_id);
                }
                _ => {}
            }
        }
    }
}

/// Progress bar for terminal output
#[derive(Debug)]
pub struct ProgressBar {
    /// Bar width in characters
    width: usize,
    /// Fill character
    fill: char,
    /// Empty character
    empty: char,
}

impl ProgressBar {
    /// Create a new progress bar
    pub fn new(width: usize) -> Self {
        Self {
            width,
            fill: '█',
            empty: '░',
        }
    }

    /// Render the progress bar for a percentage
    pub fn render(&self, percent: u8) -> String {
        let filled = (percent as usize * self.width) / 100;
        let empty = self.width - filled;

        format!(
            "[{}{}] {}%",
            self.fill.to_string().repeat(filled),
            self.empty.to_string().repeat(empty),
            percent
        )
    }

    /// Render with summary
    pub fn render_summary(&self, summary: &ProgressSummary) -> String {
        format!(
            "{} {}/{} tasks",
            self.render(summary.percent),
            summary.completed + summary.failed,
            summary.total
        )
    }
}

impl Default for ProgressBar {
    fn default() -> Self {
        Self::new(40)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::microframework::tasks::TaskType;

    fn create_task(id: &str) -> Task {
        let mut task = Task::new(TaskType::CodeGen, id);
        task.id = id.to_string();
        task
    }

    #[test]
    fn test_progress_tracker_creation() {
        let tracker = ProgressTracker::new();
        let summary = tracker.summary();
        assert_eq!(summary.total, 0);
        assert_eq!(summary.completed, 0);
    }

    #[test]
    fn test_track_task() {
        let tracker = ProgressTracker::new();
        let task = create_task("test-1");

        tracker.track_task(&task);

        let summary = tracker.summary();
        assert_eq!(summary.total, 1);
        assert_eq!(summary.pending, 1);
    }

    #[test]
    fn test_task_lifecycle() {
        let tracker = ProgressTracker::new();
        let task = create_task("test-1");

        tracker.track_task(&task);
        tracker.start_task("test-1");

        let progress = tracker.get_task_progress("test-1").unwrap();
        assert!(matches!(progress.status, TaskStatus::Running));

        tracker.complete_task("test-1", true);

        let summary = tracker.summary();
        assert_eq!(summary.completed, 1);
        assert_eq!(summary.failed, 0);
    }

    #[test]
    fn test_failed_task() {
        let tracker = ProgressTracker::new();
        let task = create_task("test-1");

        tracker.track_task(&task);
        tracker.start_task("test-1");
        tracker.complete_task("test-1", false);

        let summary = tracker.summary();
        assert_eq!(summary.completed, 0);
        assert_eq!(summary.failed, 1);
    }

    #[test]
    fn test_progress_update() {
        let tracker = ProgressTracker::new();
        let task = create_task("test-1");

        tracker.track_task(&task);
        tracker.start_task("test-1");
        tracker.update_task("test-1", 50, "Processing");

        let progress = tracker.get_task_progress("test-1").unwrap();
        assert_eq!(progress.percent, 50);
        assert_eq!(progress.current_step, "Processing");
    }

    #[test]
    fn test_progress_bar() {
        let bar = ProgressBar::new(10);

        assert!(bar.render(0).contains("░░░░░░░░░░"));
        assert!(bar.render(50).contains("█████░░░░░"));
        assert!(bar.render(100).contains("██████████"));
    }

    #[test]
    fn test_summary_format() {
        let summary = ProgressSummary {
            total: 10,
            completed: 5,
            failed: 1,
            running: 2,
            pending: 2,
            percent: 60,
            elapsed_ms: 5000,
            eta_ms: Some(3000),
        };

        let formatted = summary.format();
        assert!(formatted.contains("6/10"));
        assert!(formatted.contains("60%"));
    }

    #[test]
    fn test_retry_tracking() {
        let tracker = ProgressTracker::new();
        let task = create_task("test-1");

        tracker.track_task(&task);
        tracker.record_retry("test-1");
        tracker.record_retry("test-1");

        let progress = tracker.get_task_progress("test-1").unwrap();
        assert_eq!(progress.retries, 2);
    }
}
