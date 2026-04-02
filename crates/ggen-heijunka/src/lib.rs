//! Heijunka load leveling system for smooth workload distribution.
//!
//! This crate implements heijunka (平準化) - a lean manufacturing principle
//! that smooths workload distribution over time to prevent burst arrivals
//! and maintain steady throughput.
//!
//! # Core Concepts
//!
//! - **Load Leveling**: Smooth λ (arrival rate) over time windows
//! - **Buffering**: Absorb spikes and distribute work evenly
//! - **Metrics**: Track throughput variance and system health
//!
//! # Example
//!
//! ```rust
//! use ggen_heijunka::{LoadLeveler, LoadLevelerConfig, WorkItem};
//! use std::time::Duration;
//!
//! # #[tokio::main]
//! # async fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let config = LoadLevelerConfig {
//!     window_duration: Duration::from_secs(60),
//!     max_buffer_size: 1000,
//!     target_throughput: 100.0,
//!     variance_threshold: 0.2,
//! };
//!
//! let mut leveler = LoadLeveler::new(config)?;
//!
//! // Submit work items
//! leveler.submit(WorkItem::new(1, 1.0))?;
//! leveler.submit(WorkItem::new(2, 1.5))?;
//!
//! // Schedule smoothly
//! let scheduled = leveler.schedule_next().await?;
//! # Ok(())
//! # }
//! ```

pub mod buffer;
pub mod metrics;
pub mod scheduler;

use std::time::{Duration, Instant};
use thiserror::Error;

pub use buffer::WorkBuffer;
pub use metrics::{LoadMetrics, ThroughputTracker};
pub use scheduler::{Scheduler, SchedulingStrategy};

/// Errors that can occur in the heijunka system.
#[derive(Debug, Error)]
pub enum HeijunkaError {
    #[error("Buffer overflow: cannot accept more work items (capacity: {capacity})")]
    BufferOverflow { capacity: usize },

    #[error("Buffer underflow: no work items available")]
    BufferUnderflow,

    #[error("Invalid configuration: {0}")]
    InvalidConfig(String),

    #[error("Scheduling error: {0}")]
    SchedulingError(String),

    #[error("Metrics error: {0}")]
    MetricsError(String),

    #[error("Channel error: {0}")]
    ChannelError(String),
}

pub type Result<T> = std::result::Result<T, HeijunkaError>;

/// A work item with associated metadata for scheduling.
#[derive(Debug, Clone, PartialEq)]
pub struct WorkItem {
    pub id: u64,
    pub weight: f64,
    pub priority: u8,
    pub submitted_at: Instant,
}

impl WorkItem {
    /// Create a new work item with default priority.
    pub fn new(id: u64, weight: f64) -> Self {
        Self {
            id,
            weight,
            priority: 0,
            submitted_at: Instant::now(),
        }
    }

    /// Create a new work item with specified priority.
    pub fn with_priority(id: u64, weight: f64, priority: u8) -> Self {
        Self {
            id,
            weight,
            priority,
            submitted_at: Instant::now(),
        }
    }

    /// Get the age of this work item.
    pub fn age(&self) -> Duration {
        self.submitted_at.elapsed()
    }
}

/// Configuration for the load leveler.
#[derive(Debug, Clone)]
pub struct LoadLevelerConfig {
    /// Time window for smoothing the arrival rate.
    pub window_duration: Duration,

    /// Maximum number of items in the buffer.
    pub max_buffer_size: usize,

    /// Target throughput (items per second).
    pub target_throughput: f64,

    /// Maximum acceptable variance (0.0 - 1.0).
    pub variance_threshold: f64,
}

impl Default for LoadLevelerConfig {
    fn default() -> Self {
        Self {
            window_duration: Duration::from_secs(60),
            max_buffer_size: 1000,
            target_throughput: 100.0,
            variance_threshold: 0.2,
        }
    }
}

impl LoadLevelerConfig {
    /// Validate the configuration.
    pub fn validate(&self) -> Result<()> {
        if self.window_duration.is_zero() {
            return Err(HeijunkaError::InvalidConfig(
                "window_duration must be greater than 0".to_string(),
            ));
        }

        if self.max_buffer_size == 0 {
            return Err(HeijunkaError::InvalidConfig(
                "max_buffer_size must be greater than 0".to_string(),
            ));
        }

        if self.target_throughput <= 0.0 {
            return Err(HeijunkaError::InvalidConfig(
                "target_throughput must be greater than 0".to_string(),
            ));
        }

        if !(0.0..=1.0).contains(&self.variance_threshold) {
            return Err(HeijunkaError::InvalidConfig(
                "variance_threshold must be between 0.0 and 1.0".to_string(),
            ));
        }

        Ok(())
    }
}

/// The main load leveler that coordinates buffering, scheduling, and metrics.
pub struct LoadLeveler {
    config: LoadLevelerConfig,
    buffer: WorkBuffer,
    scheduler: Scheduler,
    metrics: LoadMetrics,
    throughput_tracker: ThroughputTracker,
}

impl LoadLeveler {
    /// Create a new load leveler with the given configuration.
    pub fn new(config: LoadLevelerConfig) -> Result<Self> {
        config.validate()?;

        let buffer = WorkBuffer::new(config.max_buffer_size);
        let scheduler = Scheduler::new(
            config.window_duration,
            config.target_throughput,
            SchedulingStrategy::Smoothed,
        );
        let metrics = LoadMetrics::new();
        let throughput_tracker = ThroughputTracker::new(config.window_duration);

        Ok(Self {
            config,
            buffer,
            scheduler,
            metrics,
            throughput_tracker,
        })
    }

    /// Submit a work item to the load leveler.
    pub fn submit(&mut self, item: WorkItem) -> Result<()> {
        self.buffer.push(item)?;
        self.metrics.record_submission();
        Ok(())
    }

    /// Submit multiple work items at once.
    pub fn submit_batch(&mut self, items: Vec<WorkItem>) -> Result<()> {
        for item in items {
            self.submit(item)?;
        }
        Ok(())
    }

    /// Schedule and return the next work item, smoothing the arrival rate.
    pub async fn schedule_next(&mut self) -> Result<Option<WorkItem>> {
        if self.buffer.is_empty() {
            return Ok(None);
        }

        // Calculate delay to smooth arrival rate
        let delay = self
            .scheduler
            .calculate_delay(self.buffer.len(), self.throughput_tracker.current_rate())?;

        if delay > Duration::ZERO {
            tokio::time::sleep(delay).await;
        }

        let item = self.buffer.pop()?;
        self.metrics.record_completion();
        self.throughput_tracker.record_completion();

        Ok(Some(item))
    }

    /// Get current buffer utilization (0.0 - 1.0).
    pub fn buffer_utilization(&self) -> f64 {
        self.buffer.utilization()
    }

    /// Get current throughput variance.
    pub fn throughput_variance(&self) -> f64 {
        self.throughput_tracker.variance()
    }

    /// Check if the system is within acceptable variance threshold.
    pub fn is_stable(&self) -> bool {
        self.throughput_variance() <= self.config.variance_threshold
    }

    /// Get current metrics snapshot.
    pub fn metrics(&self) -> &LoadMetrics {
        &self.metrics
    }

    /// Get the number of items currently buffered.
    pub fn buffered_count(&self) -> usize {
        self.buffer.len()
    }

    /// Check if the buffer is empty.
    pub fn is_empty(&self) -> bool {
        self.buffer.is_empty()
    }

    /// Get the current throughput rate (items per second).
    pub fn current_throughput(&self) -> f64 {
        self.throughput_tracker.current_rate()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_work_item_creation() {
        let item = WorkItem::new(1, 1.0);
        assert_eq!(item.id, 1);
        assert_eq!(item.weight, 1.0);
        assert_eq!(item.priority, 0);
    }

    #[test]
    fn test_work_item_with_priority() {
        let item = WorkItem::with_priority(2, 1.5, 5);
        assert_eq!(item.id, 2);
        assert_eq!(item.weight, 1.5);
        assert_eq!(item.priority, 5);
    }

    #[test]
    fn test_config_validation() {
        let config = LoadLevelerConfig::default();
        assert!(config.validate().is_ok());
    }

    #[test]
    fn test_config_validation_invalid_window() {
        let config = LoadLevelerConfig {
            window_duration: Duration::from_secs(0),
            ..Default::default()
        };
        assert!(config.validate().is_err());
    }

    #[test]
    fn test_config_validation_invalid_buffer_size() {
        let config = LoadLevelerConfig {
            max_buffer_size: 0,
            ..Default::default()
        };
        assert!(config.validate().is_err());
    }

    #[test]
    fn test_config_validation_invalid_throughput() {
        let config = LoadLevelerConfig {
            target_throughput: 0.0,
            ..Default::default()
        };
        assert!(config.validate().is_err());
    }

    #[test]
    fn test_config_validation_invalid_variance() {
        let config = LoadLevelerConfig {
            variance_threshold: 1.5,
            ..Default::default()
        };
        assert!(config.validate().is_err());
    }

    #[test]
    fn test_load_leveler_creation() {
        let config = LoadLevelerConfig::default();
        let leveler = LoadLeveler::new(config).unwrap();
        assert_eq!(leveler.buffered_count(), 0);
        assert!(leveler.is_empty());
    }

    #[test]
    fn test_submit_work_item() {
        let config = LoadLevelerConfig::default();
        let mut leveler = LoadLeveler::new(config).unwrap();

        let item = WorkItem::new(1, 1.0);
        assert!(leveler.submit(item).is_ok());
        assert_eq!(leveler.buffered_count(), 1);
        assert!(!leveler.is_empty());
    }

    #[test]
    fn test_submit_batch() {
        let config = LoadLevelerConfig::default();
        let mut leveler = LoadLeveler::new(config).unwrap();

        let items = vec![
            WorkItem::new(1, 1.0),
            WorkItem::new(2, 1.5),
            WorkItem::new(3, 2.0),
        ];

        assert!(leveler.submit_batch(items).is_ok());
        assert_eq!(leveler.buffered_count(), 3);
    }

    #[test]
    fn test_buffer_utilization() {
        let config = LoadLevelerConfig {
            max_buffer_size: 10,
            ..Default::default()
        };
        let mut leveler = LoadLeveler::new(config).unwrap();

        let items = vec![
            WorkItem::new(1, 1.0),
            WorkItem::new(2, 1.0),
            WorkItem::new(3, 1.0),
            WorkItem::new(4, 1.0),
            WorkItem::new(5, 1.0),
        ];

        leveler.submit_batch(items).ok();
        assert_eq!(leveler.buffer_utilization(), 0.5);
    }

    #[tokio::test]
    async fn test_schedule_empty_buffer() {
        let config = LoadLevelerConfig::default();
        let mut leveler = LoadLeveler::new(config).unwrap();

        let result = leveler.schedule_next().await;
        assert!(result.is_ok());
        assert!(result.unwrap().is_none());
    }

    #[tokio::test]
    async fn test_schedule_next() {
        let config = LoadLevelerConfig {
            target_throughput: 1000.0, // High throughput to minimize delays in tests
            ..Default::default()
        };
        let mut leveler = LoadLeveler::new(config).unwrap();

        let item = WorkItem::new(1, 1.0);
        leveler.submit(item).ok();

        let result = leveler.schedule_next().await;
        assert!(result.is_ok());
        assert!(result.unwrap().is_some());
        assert_eq!(leveler.buffered_count(), 0);
    }
}
