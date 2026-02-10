//! Admission Control
//!
//! Controls λ_admitted to prevent cognitive overload.
//! Refuses inadmissible requests at the boundary.

use crate::capacity::CapacityEstimator;
use crate::decision_wip::{DecisionWip, WipGuard, WipError};
use std::time::Duration;
use thiserror::Error;

/// Admission control errors
#[derive(Debug, Error)]
pub enum AdmissionError {
    /// System is at capacity
    #[error("Request refused: system at capacity")]
    AtCapacity,

    /// WIP limit exceeded
    #[error("Request refused: WIP limit exceeded")]
    WipLimitExceeded(#[from] WipError),

    /// Insufficient capacity available
    #[error("Request refused: insufficient capacity")]
    InsufficientCapacity {
        /// Required capacity ratio
        required: f64,
        /// Available capacity ratio
        available: f64,
    },

    /// Cooldown period is active
    #[error("Request refused: cooldown period active")]
    CooldownActive,
}

/// Admission control policy
#[derive(Debug, Clone)]
pub struct AdmissionPolicy {
    /// Maximum WIP limit
    pub wip_limit: usize,

    /// Minimum available capacity ratio (0.0 to 1.0)
    pub min_capacity_ratio: f64,

    /// Cooldown duration after refusal
    pub cooldown: Duration,
}

impl Default for AdmissionPolicy {
    fn default() -> Self {
        Self {
            wip_limit: 10,
            min_capacity_ratio: 0.2,
            cooldown: Duration::from_millis(100),
        }
    }
}

/// Admission controller
pub struct AdmissionController {
    wip: DecisionWip,
    capacity: CapacityEstimator,
    policy: AdmissionPolicy,
}

impl AdmissionController {
    /// Create new admission controller
    pub fn new(policy: AdmissionPolicy) -> Result<Self, WipError> {
        let wip = DecisionWip::new(policy.wip_limit)?;
        let capacity = CapacityEstimator::new();

        Ok(Self {
            wip,
            capacity,
            policy,
        })
    }

    /// Attempt to admit request
    pub fn try_admit(&self) -> Result<AdmissionTicket, AdmissionError> {
        // Check WIP limit first (fast path)
        if self.wip.is_full() {
            return Err(AdmissionError::AtCapacity);
        }

        // Check capacity estimation
        let available = self.capacity.available_capacity();
        if available < self.policy.min_capacity_ratio {
            return Err(AdmissionError::InsufficientCapacity {
                required: self.policy.min_capacity_ratio,
                available,
            });
        }

        // Acquire WIP slot
        let guard = self.wip.try_acquire()?;

        Ok(AdmissionTicket { _guard: guard })
    }

    /// Get current admission stats
    pub fn stats(&self) -> AdmissionStats {
        AdmissionStats {
            current_wip: self.wip.current(),
            wip_limit: self.wip.limit(),
            wip_utilization: self.wip.utilization(),
            available_capacity: self.capacity.available_capacity(),
            total_capacity: self.capacity.total_capacity(),
        }
    }

    /// Update capacity estimation
    pub fn record_completion(&mut self, duration: Duration) {
        self.capacity.record_completion(duration);
    }
}

/// Admission ticket (RAII guard)
#[derive(Debug)]
pub struct AdmissionTicket {
    _guard: WipGuard,
}

/// Admission statistics
#[derive(Debug, Clone, Copy)]
pub struct AdmissionStats {
    /// Current work in progress count
    pub current_wip: usize,
    /// Maximum WIP limit
    pub wip_limit: usize,
    /// WIP utilization ratio (0.0 to 1.0)
    pub wip_utilization: f64,
    /// Available capacity ratio (0.0 to 1.0)
    pub available_capacity: f64,
    /// Total capacity
    pub total_capacity: f64,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_admission_policy_default() {
        let policy = AdmissionPolicy::default();
        assert_eq!(policy.wip_limit, 10);
        assert_eq!(policy.min_capacity_ratio, 0.2);
        assert_eq!(policy.cooldown, Duration::from_millis(100));
    }

    #[test]
    fn test_admission_controller_creation() {
        let policy = AdmissionPolicy::default();
        let controller = AdmissionController::new(policy).unwrap();

        let stats = controller.stats();
        assert_eq!(stats.current_wip, 0);
        assert_eq!(stats.wip_limit, 10);
    }

    #[test]
    fn test_admission_accept() {
        let policy = AdmissionPolicy {
            wip_limit: 2,
            min_capacity_ratio: 0.0,
            cooldown: Duration::from_millis(10),
        };
        let controller = AdmissionController::new(policy).unwrap();

        let ticket1 = controller.try_admit().unwrap();
        assert_eq!(controller.stats().current_wip, 1);

        let ticket2 = controller.try_admit().unwrap();
        assert_eq!(controller.stats().current_wip, 2);

        drop(ticket1);
        assert_eq!(controller.stats().current_wip, 1);

        drop(ticket2);
        assert_eq!(controller.stats().current_wip, 0);
    }

    #[test]
    fn test_admission_refuse_at_capacity() {
        let policy = AdmissionPolicy {
            wip_limit: 2,
            min_capacity_ratio: 0.0,
            cooldown: Duration::from_millis(10),
        };
        let controller = AdmissionController::new(policy).unwrap();

        let _t1 = controller.try_admit().unwrap();
        let _t2 = controller.try_admit().unwrap();

        // Should refuse third request
        let result = controller.try_admit();
        assert!(matches!(result, Err(AdmissionError::AtCapacity)));
    }

    #[test]
    fn test_admission_stats() {
        let policy = AdmissionPolicy {
            wip_limit: 5,
            min_capacity_ratio: 0.0,
            cooldown: Duration::from_millis(10),
        };
        let controller = AdmissionController::new(policy).unwrap();

        let _t1 = controller.try_admit().unwrap();
        let _t2 = controller.try_admit().unwrap();

        let stats = controller.stats();
        assert_eq!(stats.current_wip, 2);
        assert_eq!(stats.wip_limit, 5);
        assert_eq!(stats.wip_utilization, 0.4);
    }
}
