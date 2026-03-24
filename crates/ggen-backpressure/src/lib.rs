//! Backpressure and admission control (λ ≤ μ) for ggen
//!
//! Implements work-in-progress limits, rate limiting, and kanban-style
//! pull-based flow control to prevent system overload.

use async_trait::async_trait;
use std::time::Duration;
use thiserror::Error;

pub mod kanban;
pub mod limiter;
pub mod token;

pub use kanban::{KanbanBoard, KanbanConfig, Stage};
pub use limiter::{RateLimiter, RateLimiterConfig};
pub use token::WIPToken;

/// Errors that can occur during admission control
#[derive(Debug, Error)]
pub enum BackpressureError {
    #[error("Capacity limit exceeded: {0}")]
    CapacityExceeded(String),

    #[error("Rate limit exceeded: {0}")]
    RateLimitExceeded(String),

    #[error("Timeout waiting for admission: {0:?}")]
    Timeout(Duration),

    #[error("Channel closed: {0}")]
    ChannelClosed(String),
}

pub type Result<T> = std::result::Result<T, BackpressureError>;

/// Trait for admission control mechanisms
#[async_trait]
pub trait AdmissionController: Send + Sync {
    /// Attempt to acquire admission, waiting if necessary
    async fn acquire(&self) -> Result<WIPToken>;

    /// Try to acquire admission without blocking
    fn try_acquire(&self) -> Result<Option<WIPToken>>;

    /// Check current utilization (0.0 to 1.0)
    fn utilization(&self) -> f64;

    /// Get current capacity
    fn capacity(&self) -> usize;

    /// Get current in-flight count
    fn in_flight(&self) -> usize;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_backpressure_error_display() {
        let err = BackpressureError::CapacityExceeded("test".to_string());
        assert!(err.to_string().contains("Capacity limit exceeded"));

        let err = BackpressureError::RateLimitExceeded("test".to_string());
        assert!(err.to_string().contains("Rate limit exceeded"));

        let err = BackpressureError::Timeout(Duration::from_secs(1));
        assert!(err.to_string().contains("Timeout"));
    }
}
