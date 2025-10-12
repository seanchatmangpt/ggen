//! RNG determinism control
//!
//! Provides seeded random number generation for reproducible testing.

use crate::error::{CleanroomError, Result};
use rand::rngs::StdRng;
use rand::{Rng, RngCore, SeedableRng};

/// Controller for RNG determinism
#[derive(Debug, Clone)]
pub struct RngController {
    profile: crate::policy::RngProfile,
    rng: Option<StdRng>,
}

impl RngController {
    /// Create a new RNG controller with the given profile
    pub fn new(profile: crate::policy::RngProfile) -> Self {
        let rng = match profile {
            crate::policy::RngProfile::Seed(seed) => Some(StdRng::seed_from_u64(seed)),
            crate::policy::RngProfile::System => None,
        };

        Self { profile, rng }
    }

    /// Generate a random u32
    pub fn next_u32(&mut self) -> Result<u32> {
        match (&self.profile, &mut self.rng) {
            (crate::policy::RngProfile::Seed(_), Some(rng)) => Ok(rng.next_u32()),
            (crate::policy::RngProfile::System, None) => {
                use rand::thread_rng;
                Ok(thread_rng().next_u32())
            }
            _ => Err(CleanroomError::Policy(
                crate::error::PolicyError::SecurityViolation("RNG profile mismatch".into()),
            )),
        }
    }

    /// Generate a random u64
    pub fn next_u64(&mut self) -> Result<u64> {
        match (&self.profile, &mut self.rng) {
            (crate::policy::RngProfile::Seed(_), Some(rng)) => Ok(rng.next_u64()),
            (crate::policy::RngProfile::System, None) => {
                use rand::thread_rng;
                Ok(thread_rng().next_u64())
            }
            _ => Err(CleanroomError::Policy(
                crate::error::PolicyError::SecurityViolation("RNG profile mismatch".into()),
            )),
        }
    }

    /// Fill a byte array with random data
    pub fn fill_bytes(&mut self, dest: &mut [u8]) -> Result<()> {
        match (&self.profile, &mut self.rng) {
            (crate::policy::RngProfile::Seed(_), Some(rng)) => {
                rng.fill_bytes(dest);
                Ok(())
            }
            (crate::policy::RngProfile::System, None) => {
                use rand::thread_rng;
                thread_rng().fill_bytes(dest);
                Ok(())
            }
            _ => Err(CleanroomError::Policy(
                crate::error::PolicyError::SecurityViolation("RNG profile mismatch".into()),
            )),
        }
    }

    /// Generate a random u64 value
    pub fn gen_u64(&mut self) -> Result<u64> {
        match (&self.profile, &mut self.rng) {
            (crate::policy::RngProfile::Seed(_), Some(rng)) => Ok(rng.gen()),
            (crate::policy::RngProfile::System, None) => {
                use rand::thread_rng;
                Ok(thread_rng().gen())
            }
            _ => Err(CleanroomError::Policy(
                crate::error::PolicyError::SecurityViolation("RNG profile mismatch".into()),
            )),
        }
    }

    /// Set the seed for seeded RNG profile
    pub fn set_seed(&mut self, seed: u64) -> Result<()> {
        match &self.profile {
            crate::policy::RngProfile::Seed(_) => {
                self.rng = Some(StdRng::seed_from_u64(seed));
                Ok(())
            }
            _ => Err(CleanroomError::Policy(
                crate::error::PolicyError::SecurityViolation(
                    "Cannot set seed in non-seeded profile".into(),
                ),
            )),
        }
    }

    /// Get the current profile
    pub fn profile(&self) -> &crate::policy::RngProfile {
        &self.profile
    }
}
