//! Progress reporting system for pack installation
//!
//! Provides real-time progress feedback with visual indicators,
//! step tracking, and cancellation support.

use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};
use tokio::sync::broadcast;
use tracing::{debug, info, warn};
use uuid::Uuid;

/// Progress reporting system for async operations
#[derive(Clone)]
pub struct ProgressReporter {
    progress: Arc<Mutex<ProgressState>>,
    events: broadcast::Sender<ProgressEvent>,
}

/// Current progress state
#[derive(Debug, Clone)]
pub struct ProgressState {
    pub current_step: String,
    pub step_progress: f64,
    pub total_steps: usize,
    pub completed_steps: usize,
    pub current_operation: String,
    pub start_time: Instant,
    pub estimated_duration: Option<Duration>,
    pub bytes_processed: u64,
    pub total_bytes: u64,
    pub items_processed: usize,
    pub total_items: usize,
    pub is_cancelled: bool,
    pub error: Option<String>,
}

/// Progress events for real-time updates
#[derive(Debug, Clone)]
pub enum ProgressEvent {
    StepStarted { step: String, step_number: usize },
    StepProgress { progress: f64, message: String },
    StepCompleted { step: String, duration_ms: u64 },
    OverallProgress { percent: f64, message: String },
    DataProcessed { bytes: u64, total: u64 },
    ItemProcessed { item: String, current: usize, total: usize },
    Error { message: String, step: String },
    Completed { total_duration_ms: u64 },
    Cancelled,
}

/// Installation plan for user preview
#[derive(Debug, Clone, serde::Serialize)]
pub struct InstallationPlan {
    pub pack_id: String,
    pub total_size_mb: f64,
    pub estimated_duration_seconds: u64,
    pub total_dependencies: usize,
    pub steps: Vec<PlanStep>,
    pub cache_status: CacheStatus,
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct PlanStep {
    pub step_number: usize,
    pub name: String,
    pub description: String,
    pub estimated_duration_ms: u64,
    pub size_mb: f64,
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct CacheStatus {
    pub is_cached: bool,
    pub cached_size_mb: Option<f64>,
    pub cache_hit: bool,
}

impl ProgressReporter {
    /// Create a new progress reporter
    pub fn new() -> Self {
        let (tx, _) = broadcast::channel(100);
        Self {
            progress: Arc::new(Mutex::new(ProgressState::new())),
            events: tx,
        }
    }

    /// Create a progress reporter for a specific operation
    pub fn for_operation(operation_name: &str) -> Self {
        let mut reporter = Self::new();
        reporter.start_operation(operation_name);
        reporter
    }

    /// Start a new operation
    pub fn start_operation(&self, operation_name: &str) {
        let mut state = self.progress.lock().unwrap();
        state.current_operation = operation_name.to_string();
        state.start_time = Instant::now();
        state.current_step = "Initializing".to_string();
        state.step_progress = 0.0;
        state.is_cancelled = false;
        state.error = None;

        debug!("Starting operation: {}", operation_name);
        self.broadcast_event(ProgressEvent::StepStarted {
            step: "Initializing".to_string(),
            step_number: 0,
        });
    }

    /// Start a new step
    pub fn start_step(&self, step_name: &str, step_number: usize) {
        let mut state = self.progress.lock().unwrap();
        state.current_step = step_name.to_string();
        state.step_progress = 0.0;

        info!("Starting step {}: {}", step_number, step_name);
        self.broadcast_event(ProgressEvent::StepStarted {
            step: step_name.to_string(),
            step_number,
        });
    }

    /// Update step progress
    pub fn update_step_progress(&self, progress: f64, message: &str) {
        let mut state = self.progress.lock().unwrap();
        state.step_progress = progress.clamp(0.0, 100.0);

        let overall_progress = if state.total_steps > 0 {
            (state.completed_steps as f64 + progress / 100.0) / state.total_steps as f64 * 100.0
        } else {
            progress
        };

        self.broadcast_event(ProgressEvent::StepProgress {
            progress,
            message: message.to_string(),
        });

        self.broadcast_event(ProgressEvent::OverallProgress {
            percent: overall_progress.clamp(0.0, 100.0),
            message: format!("{}: {}%", state.current_step, overall_progress as u32),
        });
    }

    /// Update data processing progress
    pub fn update_data_progress(&self, bytes_processed: u64, total_bytes: u64) {
        let mut state = self.progress.lock().unwrap();
        state.bytes_processed = bytes_processed;
        state.total_bytes = total_bytes;

        let progress = if total_bytes > 0 {
            (bytes_processed as f64 / total_bytes as f64) * 100.0
        } else {
            0.0;
        };

        self.broadcast_event(ProgressEvent::DataProcessed {
            bytes: bytes_processed,
            total: total_bytes,
        });
    }

    /// Update item processing progress
    pub fn update_item_progress(&self, item: &str, current: usize, total: usize) {
        let mut state = self.progress.lock().unwrap();
        state.items_processed = current;
        state.total_items = total;

        self.broadcast_event(ProgressEvent::ItemProcessed {
            item: item.to_string(),
            current,
            total,
        });
    }

    /// Complete current step
    pub fn complete_step(&self, step_name: &str) {
        let mut state = self.progress.lock().unwrap();
        state.completed_steps += 1;
        state.step_progress = 100.0;

        let duration = state.start_time.elapsed();
        info!("Completed step {}: {}ms", step_name, duration.as_millis());

        self.broadcast_event(ProgressEvent::StepCompleted {
            step: step_name.to_string(),
            duration_ms: duration.as_millis() as u64,
        });
    }

    /// Report error
    pub fn report_error(&self, message: &str, step: &str) {
        let mut state = self.progress.lock().unwrap();
        state.error = Some(message.to_string());
        state.is_cancelled = true;

        warn!("Error in step {}: {}", step, message);
        self.broadcast_event(ProgressEvent::Error {
            message: message.to_string(),
            step: step.to_string(),
        });
    }

    /// Mark operation as completed
    pub fn complete(&self) {
        let state = self.progress.lock().unwrap();
        let total_duration = state.start_time.elapsed();

        info!("Operation completed in {}ms", total_duration.as_millis());
        self.broadcast_event(ProgressEvent::Completed {
            total_duration_ms: total_duration.as_millis() as u64,
        });
    }

    /// Cancel operation
    pub fn cancel(&self) {
        let mut state = self.progress.lock().unwrap();
        state.is_cancelled = true;

        warn!("Operation cancelled");
        self.broadcast_event(ProgressEvent::Cancelled);
    }

    /// Check if operation is cancelled
    pub fn is_cancelled(&self) -> bool {
        let state = self.progress.lock().unwrap();
        state.is_cancelled
    }

    /// Get current progress state
    pub fn get_state(&self) -> ProgressState {
        self.progress.lock().unwrap().clone()
    }

    /// Set total number of steps
    pub fn set_total_steps(&self, total: usize) {
        let mut state = self.progress.lock().unwrap();
        state.total_steps = total;
        info!("Total steps for operation: {}", total);
    }

    /// Set estimated duration
    pub fn set_estimated_duration(&self, duration: Duration) {
        let mut state = self.progress.lock().unwrap();
        state.estimated_duration = Some(duration);
    }

    /// Subscribe to progress events
    pub fn subscribe(&self) -> broadcast::Receiver<ProgressEvent> {
        self.events.subscribe()
    }

    /// Broadcast progress event
    fn broadcast_event(&self, event: ProgressEvent) {
        let _ = self.events.send(event);
    }
}

impl ProgressState {
    /// Create a new progress state
    pub fn new() -> Self {
        Self {
            current_step: "Not started".to_string(),
            step_progress: 0.0,
            total_steps: 0,
            completed_steps: 0,
            current_operation: "Unknown".to_string(),
            start_time: Instant::now(),
            estimated_duration: None,
            bytes_processed: 0,
            total_bytes: 0,
            items_processed: 0,
            total_items: 0,
            is_cancelled: false,
            error: None,
        }
    }

    /// Get overall progress percentage
    pub fn overall_progress(&self) -> f64 {
        if self.total_steps == 0 {
            self.step_progress
        } else {
            ((self.completed_steps as f64 + self.step_progress / 100.0) / self.total_steps as f64) * 100.0
        }
    }

    /// Get elapsed time
    pub fn elapsed(&self) -> Duration {
        self.start_time.elapsed()
    }

    /// Get estimated time remaining
    pub fn estimated_time_remaining(&self) -> Option<Duration> {
        if let Some(estimated) = self.estimated_duration {
            Some(estimated)
        } else if self.total_steps > 0 && self.completed_steps > 0 {
            let elapsed = self.elapsed();
            let avg_step_time = elapsed / self.completed_steps as u32;
            let remaining_steps = self.total_steps - self.completed_steps;
            Some(avg_step_time * remaining_steps as u32)
        } else {
            None
        }
    }

    /// Check if operation is completed
    pub fn is_completed(&self) -> bool {
        self.total_steps > 0 && self.completed_steps >= self.total_steps
    }
}

/// Progress display for console output
pub struct ProgressDisplay {
    reporter: ProgressReporter,
    show_detailed: bool,
}

impl ProgressDisplay {
    pub fn new(reporter: ProgressReporter, show_detailed: bool) -> Self {
        Self {
            reporter,
            show_detailed,
        }
    }

    /// Display progress in a formatted way
    pub fn display(&self) {
        let state = self.reporter.get_state();

        if self.show_detailed {
            println!("📦 {} - {:.1}% complete", state.current_operation, state.overall_progress());
            println!("  Step: {} ({:.1}%)", state.current_step, state.step_progress);
            println!("  Progress: {}/{} steps completed", state.completed_steps, state.total_steps);

            if state.total_bytes > 0 {
                println!("  Data: {}/{} MB ({:.1}%)",
                    state.bytes_processed / 1_048_576,
                    state.total_bytes / 1_048_576,
                    (state.bytes_processed as f64 / state.total_bytes as f64) * 100.0);
            }

            if let Some(remaining) = state.estimated_time_remaining() {
                println!("  Estimated remaining: {:.0}s", remaining.as_secs_f64());
            }
        } else {
            println!("📦 {}: {:.1}% - {} ({}/{})",
                state.current_operation,
                state.overall_progress(),
                state.current_step,
                state.completed_steps,
                state.total_steps);
        }
    }

    /// Display progress bar
    pub fn display_bar(&self) {
        let state = self.reporter.get_state();
        let overall = state.overall_progress();
        let bar_width = 40;

        let filled_width = (overall / 100.0 * bar_width as f64) as usize;
        let empty_width = bar_width - filled_width;

        let filled = "█".repeat(filled_width);
        let empty = "░".repeat(empty_width);

        println!("📦 {} |{}{}| {:.1}% ({}/{})",
            state.current_operation,
            filled, empty,
            overall,
            state.completed_steps,
            state.total_steps);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread;
    use std::time::Duration;

    #[test]
    fn test_progress_state_creation() {
        let state = ProgressState::new();
        assert_eq!(state.current_step, "Not started");
        assert_eq!(state.step_progress, 0.0);
        assert_eq!(state.total_steps, 0);
        assert_eq!(state.completed_steps, 0);
        assert!(!state.is_cancelled);
        assert!(state.error.is_none());
    }

    #[test]
    fn test_progress_calculation() {
        let mut state = ProgressState::new();
        state.total_steps = 5;
        state.completed_steps = 2;
        state.step_progress = 50.0;

        assert_eq!(state.overall_progress(), 50.0); // 2.5/5 = 50%
    }

    #[test]
    fn test_completion_check() {
        let mut state = ProgressState::new();
        assert!(!state.is_completed());

        state.total_steps = 3;
        state.completed_steps = 2;
        assert!(!state.is_completed());

        state.completed_steps = 3;
        assert!(state.is_completed());
    }

    #[tokio::test]
    async fn test_progress_reporter() {
        let reporter = ProgressReporter::new();

        reporter.start_test_operation("test");
        reporter.set_total_steps(3);

        // Test step progress
        reporter.start_step("Step 1", 1);
        reporter.update_step_progress(25.0, "Processing...");
        reporter.complete_step("Step 1");

        reporter.start_step("Step 2", 2);
        reporter.update_step_progress(75.0, "Almost done");
        reporter.complete_step("Step 2");

        reporter.start_step("Step 3", 3);
        reporter.update_step_progress(100.0, "Complete");
        reporter.complete_step("Step 3");

        let state = reporter.get_state();
        assert_eq!(state.completed_steps, 3);
        assert_eq!(state.total_steps, 3);
        assert!(state.is_completed());
    }

    impl ProgressReporter {
        // Helper for testing
        fn start_test_operation(&self, operation_name: &str) {
            self.start_operation(operation_name);
        }
    }
}