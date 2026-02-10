//! Decision WIP Tracker
//!
//! Tracks work-in-progress decisions to minimize cognitive load.
//! Goal: λ_admitted → 0 by refusing inadmissible at boundary.

use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use thiserror::Error;

/// WIP tracking errors
#[derive(Debug, Error)]
pub enum WipError {
    /// WIP limit exceeded
    #[error("WIP limit exceeded: {current} >= {limit}")]
    LimitExceeded {
        /// Current WIP count
        current: usize,
        /// WIP limit
        limit: usize,
    },

    /// Invalid WIP limit specified
    #[error("Invalid WIP limit: {0}")]
    InvalidLimit(usize),
}

/// Decision WIP tracking with atomic operations
#[derive(Debug, Clone)]
pub struct DecisionWip {
    current: Arc<AtomicUsize>,
    limit: usize,
}

impl DecisionWip {
    /// Create new WIP tracker with specified limit
    pub fn new(limit: usize) -> Result<Self, WipError> {
        if limit == 0 {
            return Err(WipError::InvalidLimit(limit));
        }

        Ok(Self {
            current: Arc::new(AtomicUsize::new(0)),
            limit,
        })
    }

    /// Attempt to acquire WIP slot
    pub fn try_acquire(&self) -> Result<WipGuard, WipError> {
        let current = self.current.fetch_add(1, Ordering::SeqCst);

        if current >= self.limit {
            self.current.fetch_sub(1, Ordering::SeqCst);
            return Err(WipError::LimitExceeded {
                current,
                limit: self.limit,
            });
        }

        Ok(WipGuard {
            wip: self.clone(),
        })
    }

    /// Get current WIP count
    pub fn current(&self) -> usize {
        self.current.load(Ordering::SeqCst)
    }

    /// Get WIP limit
    pub fn limit(&self) -> usize {
        self.limit
    }

    /// Get utilization ratio (0.0 to 1.0)
    pub fn utilization(&self) -> f64 {
        self.current() as f64 / self.limit as f64
    }

    /// Check if at capacity
    pub fn is_full(&self) -> bool {
        self.current() >= self.limit
    }
}

/// RAII guard for WIP tracking
#[derive(Debug)]
pub struct WipGuard {
    wip: DecisionWip,
}

impl Drop for WipGuard {
    fn drop(&mut self) {
        self.wip.current.fetch_sub(1, Ordering::SeqCst);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_wip_creation() {
        let wip = DecisionWip::new(5).unwrap();
        assert_eq!(wip.current(), 0);
        assert_eq!(wip.limit(), 5);
        assert_eq!(wip.utilization(), 0.0);
    }

    #[test]
    fn test_wip_invalid_limit() {
        let result = DecisionWip::new(0);
        assert!(result.is_err());
    }

    #[test]
    fn test_wip_acquire_release() {
        let wip = DecisionWip::new(2).unwrap();

        let guard1 = wip.try_acquire().unwrap();
        assert_eq!(wip.current(), 1);

        let guard2 = wip.try_acquire().unwrap();
        assert_eq!(wip.current(), 2);

        // Should fail at capacity
        let result = wip.try_acquire();
        assert!(result.is_err());

        drop(guard1);
        assert_eq!(wip.current(), 1);

        // Should succeed after release
        let _guard3 = wip.try_acquire().unwrap();
        assert_eq!(wip.current(), 2);

        drop(guard2);
        assert_eq!(wip.current(), 1);
    }

    #[test]
    fn test_wip_utilization() {
        let wip = DecisionWip::new(10).unwrap();
        assert_eq!(wip.utilization(), 0.0);

        let _g1 = wip.try_acquire().unwrap();
        assert_eq!(wip.utilization(), 0.1);

        let _g2 = wip.try_acquire().unwrap();
        assert_eq!(wip.utilization(), 0.2);

        let _g3 = wip.try_acquire().unwrap();
        let _g4 = wip.try_acquire().unwrap();
        let _g5 = wip.try_acquire().unwrap();
        assert_eq!(wip.utilization(), 0.5);
    }

    #[test]
    fn test_wip_is_full() {
        let wip = DecisionWip::new(2).unwrap();
        assert!(!wip.is_full());

        let _g1 = wip.try_acquire().unwrap();
        assert!(!wip.is_full());

        let _g2 = wip.try_acquire().unwrap();
        assert!(wip.is_full());
    }
}
