//! Backpressure Handler
//!
//! Protects the system from being overwhelmed by too many concurrent
//! requests using rate limiting and queue management.

use governor::{Quota, RateLimiter};
use parking_lot::RwLock;
use serde::{Deserialize, Serialize};
use std::num::NonZeroU32;
use std::sync::atomic::{AtomicBool, AtomicU64, AtomicUsize, Ordering};
use std::time::{Duration, Instant};
use tokio::sync::Semaphore;
use tracing::{debug, warn};

/// Backpressure handler for overwhelm protection
#[derive(Debug)]
pub struct BackpressureHandler {
    /// Maximum queue size before rejecting
    max_queue_size: usize,
    /// Current queue size
    current_queue_size: AtomicUsize,
    /// Whether backpressure is active
    overloaded: AtomicBool,
    /// Capacity semaphore
    capacity: Semaphore,
    /// Rate limiter for request throttling
    rate_limiter: Option<RateLimiter<governor::state::NotKeyed, governor::state::InMemoryState, governor::clock::DefaultClock>>,
    /// Statistics
    stats: BackpressureStats,
    /// Overload threshold (percentage of max queue)
    overload_threshold: f64,
    /// Recovery threshold (percentage of max queue)
    recovery_threshold: f64,
}

/// Backpressure statistics
#[derive(Debug, Default)]
struct BackpressureStats {
    /// Total requests
    total_requests: AtomicU64,
    /// Requests rejected due to backpressure
    rejected_requests: AtomicU64,
    /// Requests delayed due to rate limiting
    delayed_requests: AtomicU64,
    /// Time spent waiting (milliseconds)
    total_wait_ms: AtomicU64,
    /// Peak queue size
    peak_queue_size: AtomicUsize,
    /// Times overload triggered
    overload_events: AtomicU64,
}

impl BackpressureHandler {
    /// Create a new backpressure handler
    pub fn new(max_queue_size: usize) -> Self {
        Self {
            max_queue_size,
            current_queue_size: AtomicUsize::new(0),
            overloaded: AtomicBool::new(false),
            capacity: Semaphore::new(max_queue_size),
            rate_limiter: None,
            stats: BackpressureStats::default(),
            overload_threshold: 0.8,
            recovery_threshold: 0.5,
        }
    }

    /// Create with rate limiting
    pub fn with_rate_limit(max_queue_size: usize, requests_per_second: u32) -> Self {
        let rate_limiter = NonZeroU32::new(requests_per_second).map(|rate| {
            RateLimiter::direct(Quota::per_second(rate))
        });

        Self {
            max_queue_size,
            current_queue_size: AtomicUsize::new(0),
            overloaded: AtomicBool::new(false),
            capacity: Semaphore::new(max_queue_size),
            rate_limiter,
            stats: BackpressureStats::default(),
            overload_threshold: 0.8,
            recovery_threshold: 0.5,
        }
    }

    /// Check if system is overloaded
    pub fn is_overloaded(&self) -> bool {
        self.overloaded.load(Ordering::Relaxed)
    }

    /// Try to acquire capacity
    pub fn try_acquire(&self) -> bool {
        self.stats.total_requests.fetch_add(1, Ordering::Relaxed);

        // Check rate limiter first
        if let Some(ref limiter) = self.rate_limiter {
            if limiter.check().is_err() {
                self.stats.delayed_requests.fetch_add(1, Ordering::Relaxed);
                return false;
            }
        }

        // Check if overloaded
        if self.is_overloaded() {
            self.stats.rejected_requests.fetch_add(1, Ordering::Relaxed);
            return false;
        }

        // Try to get capacity
        match self.capacity.try_acquire() {
            Ok(permit) => {
                std::mem::forget(permit); // We'll manually release
                let current = self.current_queue_size.fetch_add(1, Ordering::Relaxed) + 1;
                self.update_peak(current);
                self.check_overload(current);
                true
            }
            Err(_) => {
                self.stats.rejected_requests.fetch_add(1, Ordering::Relaxed);
                false
            }
        }
    }

    /// Wait for capacity to become available
    pub async fn wait_for_capacity(&self) {
        let start = Instant::now();

        // Wait for rate limiter if present
        if let Some(ref limiter) = self.rate_limiter {
            limiter.until_ready().await;
        }

        // Wait for semaphore capacity
        if let Ok(permit) = self.capacity.acquire().await {
            std::mem::forget(permit);
            let current = self.current_queue_size.fetch_add(1, Ordering::Relaxed) + 1;
            self.update_peak(current);
            self.check_overload(current);

            let wait_ms = start.elapsed().as_millis() as u64;
            if wait_ms > 0 {
                self.stats.total_wait_ms.fetch_add(wait_ms, Ordering::Relaxed);
                self.stats.delayed_requests.fetch_add(1, Ordering::Relaxed);
            }
        }
    }

    /// Release capacity
    pub fn release(&self) {
        let current = self.current_queue_size.fetch_sub(1, Ordering::Relaxed) - 1;
        self.capacity.add_permits(1);
        self.check_recovery(current);
    }

    /// Get current queue size
    pub fn queue_size(&self) -> usize {
        self.current_queue_size.load(Ordering::Relaxed)
    }

    /// Get queue utilization (0.0 - 1.0)
    pub fn utilization(&self) -> f64 {
        self.queue_size() as f64 / self.max_queue_size as f64
    }

    /// Get statistics
    pub fn statistics(&self) -> BackpressureStatistics {
        BackpressureStatistics {
            max_queue_size: self.max_queue_size,
            current_queue_size: self.current_queue_size.load(Ordering::Relaxed),
            is_overloaded: self.is_overloaded(),
            utilization: self.utilization(),
            total_requests: self.stats.total_requests.load(Ordering::Relaxed),
            rejected_requests: self.stats.rejected_requests.load(Ordering::Relaxed),
            delayed_requests: self.stats.delayed_requests.load(Ordering::Relaxed),
            total_wait_ms: self.stats.total_wait_ms.load(Ordering::Relaxed),
            peak_queue_size: self.stats.peak_queue_size.load(Ordering::Relaxed),
            overload_events: self.stats.overload_events.load(Ordering::Relaxed),
        }
    }

    /// Update peak queue size
    fn update_peak(&self, current: usize) {
        let mut peak = self.stats.peak_queue_size.load(Ordering::Relaxed);
        while current > peak {
            match self.stats.peak_queue_size.compare_exchange_weak(
                peak,
                current,
                Ordering::Relaxed,
                Ordering::Relaxed,
            ) {
                Ok(_) => break,
                Err(p) => peak = p,
            }
        }
    }

    /// Check if overload should be triggered
    fn check_overload(&self, current: usize) {
        let threshold = (self.max_queue_size as f64 * self.overload_threshold) as usize;
        if current >= threshold && !self.is_overloaded() {
            self.overloaded.store(true, Ordering::Relaxed);
            self.stats.overload_events.fetch_add(1, Ordering::Relaxed);
            warn!("Backpressure activated: queue size {} >= threshold {}", current, threshold);
        }
    }

    /// Check if recovery should be triggered
    fn check_recovery(&self, current: usize) {
        let threshold = (self.max_queue_size as f64 * self.recovery_threshold) as usize;
        if current <= threshold && self.is_overloaded() {
            self.overloaded.store(false, Ordering::Relaxed);
            debug!("Backpressure deactivated: queue size {} <= threshold {}", current, threshold);
        }
    }
}

/// Backpressure statistics snapshot
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BackpressureStatistics {
    /// Maximum queue size
    pub max_queue_size: usize,
    /// Current queue size
    pub current_queue_size: usize,
    /// Whether currently overloaded
    pub is_overloaded: bool,
    /// Current utilization (0.0 - 1.0)
    pub utilization: f64,
    /// Total requests received
    pub total_requests: u64,
    /// Requests rejected
    pub rejected_requests: u64,
    /// Requests that were delayed
    pub delayed_requests: u64,
    /// Total wait time in milliseconds
    pub total_wait_ms: u64,
    /// Peak queue size observed
    pub peak_queue_size: usize,
    /// Number of overload events
    pub overload_events: u64,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_handler_creation() {
        let handler = BackpressureHandler::new(100);
        assert_eq!(handler.max_queue_size, 100);
        assert!(!handler.is_overloaded());
        assert_eq!(handler.queue_size(), 0);
    }

    #[test]
    fn test_try_acquire() {
        let handler = BackpressureHandler::new(10);

        for _ in 0..5 {
            assert!(handler.try_acquire());
        }
        assert_eq!(handler.queue_size(), 5);

        for _ in 0..5 {
            handler.release();
        }
        assert_eq!(handler.queue_size(), 0);
    }

    #[test]
    fn test_overload_detection() {
        let handler = BackpressureHandler::new(10);

        // Fill to 80% (overload threshold)
        for _ in 0..8 {
            handler.try_acquire();
        }
        assert!(handler.is_overloaded());

        // Release to 50% (recovery threshold)
        for _ in 0..4 {
            handler.release();
        }
        assert!(!handler.is_overloaded());
    }

    #[test]
    fn test_statistics() {
        let handler = BackpressureHandler::new(100);
        handler.try_acquire();
        handler.try_acquire();
        handler.release();

        let stats = handler.statistics();
        assert_eq!(stats.total_requests, 2);
        assert_eq!(stats.current_queue_size, 1);
        assert_eq!(stats.peak_queue_size, 2);
    }

    #[test]
    fn test_utilization() {
        let handler = BackpressureHandler::new(100);
        for _ in 0..25 {
            handler.try_acquire();
        }
        assert!((handler.utilization() - 0.25).abs() < 0.001);
    }
}
