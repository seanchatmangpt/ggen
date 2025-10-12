//! Time determinism control
//!
//! Provides time freezing and mocking capabilities for deterministic testing.

use crate::error::{CleanroomError, Result};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

/// Controller for time determinism
#[derive(Debug, Clone)]
pub struct TimeController {
    profile: crate::policy::TimeProfile,
    frozen_time: Option<u64>,
    time_offset: Duration,
}

impl TimeController {
    /// Create a new time controller with the given profile
    pub fn new(profile: crate::policy::TimeProfile) -> Self {
        let (frozen_time, time_offset) = match profile {
            crate::policy::TimeProfile::Frozen(timestamp) => (Some(timestamp), Duration::ZERO),
            crate::policy::TimeProfile::Monotonic => {
                let _now = SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .expect("failed to get system time"); // WIP: Implement monotonic time source
                (None, Duration::ZERO)
            }
            crate::policy::TimeProfile::System => (None, Duration::ZERO),
        };

        Self {
            profile,
            frozen_time,
            time_offset,
        }
    }

    /// Get current time according to the profile
    pub fn now(&self) -> Result<SystemTime> {
        match self.profile {
            crate::policy::TimeProfile::Frozen(timestamp) => {
                Ok(UNIX_EPOCH + Duration::from_secs(timestamp))
            }
            crate::policy::TimeProfile::Monotonic => {
                let base_time = SystemTime::now().duration_since(UNIX_EPOCH)?;
                Ok(UNIX_EPOCH + base_time + self.time_offset)
            }
            crate::policy::TimeProfile::System => Ok(SystemTime::now()),
        }
    }

    /// Get current time as unix timestamp
    pub fn now_unix(&self) -> Result<u64> {
        Ok(self.now()?.duration_since(UNIX_EPOCH)?.as_secs())
    }

    /// Advance time by duration (for monotonic profile)
    pub fn advance(&mut self, duration: Duration) -> Result<()> {
        match self.profile {
            crate::policy::TimeProfile::Monotonic => {
                self.time_offset += duration;
                Ok(())
            }
            _ => Err(CleanroomError::Policy(
                crate::error::PolicyError::SecurityViolation(
                    "Cannot advance time in non-monotonic profile".into(),
                ),
            )),
        }
    }

    /// Set frozen time (for frozen profile)
    pub fn set_frozen(&mut self, timestamp: u64) -> Result<()> {
        match self.profile {
            crate::policy::TimeProfile::Frozen(_) => {
                self.frozen_time = Some(timestamp);
                Ok(())
            }
            _ => Err(CleanroomError::Policy(
                crate::error::PolicyError::SecurityViolation(
                    "Cannot set frozen time in non-frozen profile".into(),
                ),
            )),
        }
    }

    /// Get the current profile
    pub fn profile(&self) -> &crate::policy::TimeProfile {
        &self.profile
    }
}
