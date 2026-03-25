//! Vector Clock Implementation for Causal Ordering
//!
//! Provides vector clocks for tracking causality across multi-region deployments.
//! Standard VC algorithm: tracks logical timestamps per region to detect concurrent events.

use std::collections::HashMap;
use std::fmt;
use serde::{Deserialize, Serialize};

/// A vector clock for tracking causal dependencies across regions
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct VectorClock {
    /// Map from region ID to logical timestamp
    timestamps: HashMap<String, u64>,
}

impl VectorClock {
    /// Create a new empty vector clock
    pub fn new() -> Self {
        Self {
            timestamps: HashMap::new(),
        }
    }

    /// Create a vector clock from a HashMap of timestamps
    pub fn from_map(timestamps: HashMap<String, u64>) -> Self {
        Self { timestamps }
    }

    /// Initialize a vector clock with specific regions
    pub fn with_regions(regions: &[&str]) -> Self {
        let mut timestamps = HashMap::new();
        for region in regions {
            timestamps.insert(region.to_string(), 0);
        }
        Self { timestamps }
    }

    /// Increment the logical clock for a region
    pub fn increment(&mut self, region: &str) {
        let current = self.timestamps.entry(region.to_string()).or_insert(0);
        *current += 1;
    }

    /// Get the timestamp for a region
    pub fn get(&self, region: &str) -> u64 {
        self.timestamps.get(region).copied().unwrap_or(0)
    }

    /// Merge two vector clocks (take maximum for each region)
    pub fn merge(&mut self, other: &VectorClock) {
        for (region, timestamp) in &other.timestamps {
            let current = self.timestamps.entry(region.clone()).or_insert(0);
            if timestamp > current {
                *current = *timestamp;
            }
        }
    }

    /// Check if this VC happens-before another (partial order)
    pub fn happens_before(&self, other: &VectorClock) -> bool {
        let mut strictly_less = false;

        // Check all regions in self
        for (region, ts) in &self.timestamps {
            let other_ts = other.get(region);
            if ts > &other_ts {
                return false; // Some component is greater
            }
            if ts < &other_ts {
                strictly_less = true;
            }
        }

        // Check for regions only in other (they're implicitly 0 in self)
        for (region, ts) in &other.timestamps {
            if !self.timestamps.contains_key(region) && *ts > 0 {
                strictly_less = true;
            }
        }

        strictly_less
    }

    /// Check if two vector clocks are concurrent (neither happens before the other)
    pub fn concurrent(&self, other: &VectorClock) -> bool {
        !self.happens_before(other) && !other.happens_before(self) && self != other
    }

    /// Get all regions in this vector clock
    pub fn regions(&self) -> Vec<String> {
        let mut regions: Vec<_> = self.timestamps.keys().cloned().collect();
        regions.sort();
        regions
    }

    /// Get all timestamps as a map
    pub fn as_map(&self) -> &HashMap<String, u64> {
        &self.timestamps
    }

    /// Create a new VC that is a clone and then incremented for a region
    pub fn increment_copy(&self, region: &str) -> Self {
        let mut new_vc = self.clone();
        new_vc.increment(region);
        new_vc
    }
}

impl Default for VectorClock {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for VectorClock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut regions: Vec<_> = self.timestamps.iter().collect();
        regions.sort_by_key(|&(k, _)| k);

        write!(f, "[")?;
        for (i, (region, ts)) in regions.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}={}", region, ts)?;
        }
        write!(f, "]")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vector_clock_new() {
        let vc = VectorClock::new();
        assert_eq!(vc.get("us-east"), 0);
    }

    #[test]
    fn test_vector_clock_increment() {
        let mut vc = VectorClock::new();
        vc.increment("us-east");
        assert_eq!(vc.get("us-east"), 1);
        vc.increment("us-east");
        assert_eq!(vc.get("us-east"), 2);
    }

    #[test]
    fn test_vector_clock_with_regions() {
        let vc = VectorClock::with_regions(&["us-east", "us-west", "eu"]);
        assert_eq!(vc.get("us-east"), 0);
        assert_eq!(vc.get("us-west"), 0);
        assert_eq!(vc.get("eu"), 0);
    }

    #[test]
    fn test_vector_clock_merge() {
        let mut vc1 = VectorClock::with_regions(&["us-east", "us-west"]);
        vc1.increment("us-east");
        vc1.increment("us-east");

        let mut vc2 = VectorClock::with_regions(&["us-east", "us-west"]);
        vc2.increment("us-west");
        vc2.increment("us-west");
        vc2.increment("us-west");

        vc1.merge(&vc2);
        assert_eq!(vc1.get("us-east"), 2);
        assert_eq!(vc1.get("us-west"), 3);
    }

    #[test]
    fn test_happens_before() {
        let mut vc1 = VectorClock::with_regions(&["us-east", "us-west"]);
        vc1.increment("us-east");

        let mut vc2 = VectorClock::with_regions(&["us-east", "us-west"]);
        vc2.increment("us-east");
        vc2.increment("us-west");

        assert!(vc1.happens_before(&vc2));
        assert!(!vc2.happens_before(&vc1));
    }

    #[test]
    fn test_concurrent_events() {
        let mut vc1 = VectorClock::with_regions(&["us-east", "us-west"]);
        vc1.increment("us-east");

        let mut vc2 = VectorClock::with_regions(&["us-east", "us-west"]);
        vc2.increment("us-west");

        assert!(vc1.concurrent(&vc2));
        assert!(vc2.concurrent(&vc1));
    }

    #[test]
    fn test_display() {
        let mut vc = VectorClock::with_regions(&["us-east", "us-west"]);
        vc.increment("us-east");
        vc.increment("us-west");
        vc.increment("us-west");
        let display = format!("{}", vc);
        assert!(display.contains("us-east=1"));
        assert!(display.contains("us-west=2"));
    }
}
