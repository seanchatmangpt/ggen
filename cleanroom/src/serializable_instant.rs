//! Serializable Instant wrapper for cleanroom testing
//!
//! This module provides a serializable wrapper around std::time::Instant
//! since Instant doesn't implement Serialize/Deserialize traits.

use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

/// Serializable wrapper around Instant
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct SerializableInstant(Instant);

impl SerializableInstant {
    /// Create a new SerializableInstant from the current time
    pub fn now() -> Self {
        Self(Instant::now())
    }

    /// Get the underlying Instant
    pub fn instant(&self) -> Instant {
        self.0
    }

    /// Create from a Duration since some epoch
    pub fn from_duration_since_epoch(duration: Duration) -> Self {
        // We'll use a fixed epoch for deterministic behavior
        let epoch = Instant::now() - duration;
        Self(epoch)
    }

    /// Get duration since a reference point
    pub fn duration_since(&self, earlier: SerializableInstant) -> Duration {
        self.0.duration_since(earlier.0)
    }

    /// Get elapsed time since creation
    pub fn elapsed(&self) -> Duration {
        self.0.elapsed()
    }
}

impl Serialize for SerializableInstant {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        // Convert to nanoseconds since Unix epoch for serialization
        let system_now = SystemTime::now();
        let unix_duration = system_now.duration_since(UNIX_EPOCH).unwrap_or_default();
        let instant_duration = self.0.elapsed();
        let total_nanos = unix_duration.as_nanos() - instant_duration.as_nanos();
        
        serializer.serialize_u128(total_nanos)
    }
}

impl<'de> Deserialize<'de> for SerializableInstant {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let nanos = u128::deserialize(deserializer)?;
        
        // Convert back from nanoseconds since Unix epoch
        let system_now = SystemTime::now();
        let unix_duration = system_now.duration_since(UNIX_EPOCH).unwrap_or_default();
        let current_nanos = unix_duration.as_nanos();
        
        if nanos > current_nanos {
            return Err(serde::de::Error::custom("Invalid timestamp: future time"));
        }
        
        let elapsed_nanos = current_nanos - nanos;
        let elapsed = Duration::from_nanos(elapsed_nanos as u64);
        
        Ok(Self::from_duration_since_epoch(elapsed))
    }
}

impl From<Instant> for SerializableInstant {
    fn from(instant: Instant) -> Self {
        Self(instant)
    }
}

impl From<SerializableInstant> for Instant {
    fn from(si: SerializableInstant) -> Self {
        si.0
    }
}

impl std::ops::Add<Duration> for SerializableInstant {
    type Output = SerializableInstant;
    
    fn add(self, rhs: Duration) -> Self::Output {
        SerializableInstant(self.0 + rhs)
    }
}

impl std::ops::Sub<Duration> for SerializableInstant {
    type Output = SerializableInstant;
    
    fn sub(self, rhs: Duration) -> Self::Output {
        SerializableInstant(self.0 - rhs)
    }
}