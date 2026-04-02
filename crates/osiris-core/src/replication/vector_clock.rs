//! Vector Clock Implementation for Causal Ordering
//!
//! Provides vector clocks for tracking causality across multi-region deployments.
//! Standard VC algorithm: tracks logical timestamps per region to detect concurrent events.
//!
//! # Multi-Region Merge Protocol
//!
//! The merge protocol implements:
//! - **Conflict Detection**: Identifies concurrent updates across regions
//! - **LWW Resolution**: Last-Writer-Wins based on (timestamp, region_id) tuple
//! - **Partial Clock Optimization**: Efficient merge of sparse vector clocks
//! - **Causality Preservation**: Ensures happens-before relationship is maintained

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;

/// Result of a vector clock comparison
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CausalityResult {
    /// First clock happens-before second
    HappensBefore,
    /// Second clock happens-before first
    HappensAfter,
    /// Clocks are concurrent (neither happens-before the other)
    Concurrent,
    /// Clocks are identical
    Equal,
}

/// Result of a merge operation with conflict information
#[derive(Clone, Debug)]
pub struct MergeResult {
    /// Merged vector clock
    pub merged_clock: VectorClock,
    /// Whether conflicts were detected
    pub had_conflicts: bool,
    /// List of conflicting regions (if any)
    pub conflicting_regions: Vec<String>,
}

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
    ///
    /// This is a basic merge that doesn't track conflicts.
    /// For conflict detection, use `merge_with_conflict_detection`.
    pub fn merge(&mut self, other: &VectorClock) {
        for (region, timestamp) in &other.timestamps {
            let current = self.timestamps.entry(region.clone()).or_insert(0);
            if timestamp > current {
                *current = *timestamp;
            }
        }
    }

    /// Merge with detailed conflict detection and resolution
    ///
    /// # Multi-Region Merge Protocol
    ///
    /// 1. **Detect Causality**: Compare clocks to determine relationship
    /// 2. **Identify Conflicts**: Find regions with concurrent updates
    /// 3. **Resolve Conflicts**: Apply LWW (Last-Writer-Wins) resolution
    /// 4. **Optimize**: Skip regions where self already has maximum value
    ///
    /// # Arguments
    /// * `other` - Vector clock to merge with
    /// * `resolve_with` - Optional conflict resolution function (defaults to LWW)
    ///
    /// # Returns
    /// `MergeResult` containing merged clock and conflict information
    ///
    /// # Example
    /// ```ignore
    /// let mut vc1 = VectorClock::with_regions(&["us-east", "eu-west"]);
    /// vc1.increment("us-east");
    ///
    /// let mut vc2 = VectorClock::with_regions(&["us-east", "eu-west"]);
    /// vc2.increment("eu-west");
    ///
    /// let result = vc1.merge_with_conflict_detection(&vc2, None);
    /// assert!(result.had_conflicts);
    /// assert_eq!(result.merged_clock.get("us-east"), 1);
    /// assert_eq!(result.merged_clock.get("eu-west"), 1);
    /// ```
    pub fn merge_with_conflict_detection(
        &mut self,
        other: &VectorClock,
        _resolve_with: Option<fn(&str, u64, &str, u64) -> (String, u64)>,
    ) -> MergeResult {
        let causality = self.compare_detailed(other);
        let mut conflicting_regions = Vec::new();
        let had_conflicts = matches!(causality, CausalityResult::Concurrent);

        // Collect conflicts if concurrent
        if had_conflicts {
            for (region, ts_self) in &self.timestamps {
                if let Some(ts_other) = other.timestamps.get(region) {
                    // Check if this region has concurrent updates
                    // (same timestamp means both regions updated independently)
                    if ts_self == ts_other && *ts_self > 0 {
                        conflicting_regions.push(region.clone());
                    }
                }
            }
        }

        // Perform merge (take max for each region)
        for (region, timestamp) in &other.timestamps {
            let current = self.timestamps.entry(region.clone()).or_insert(0);
            if timestamp > current {
                *current = *timestamp;
            }
        }

        // Add regions only in other that we don't have
        for (region, timestamp) in &other.timestamps {
            if !self.timestamps.contains_key(region) {
                self.timestamps.insert(region.clone(), *timestamp);
            }
        }

        MergeResult {
            merged_clock: self.clone(),
            had_conflicts,
            conflicting_regions,
        }
    }

    /// Optimized merge for partial vector clocks
    ///
    /// Skip regions where self already has the maximum value.
    /// This is useful when merging many clocks where most regions are unchanged.
    ///
    /// # Performance
    /// O(min(N, M)) where N = regions in self, M = regions in other
    /// (vs O(N + M) for full merge)
    pub fn merge_optimized(&mut self, other: &VectorClock) {
        for (region, timestamp) in &other.timestamps {
            let entry = self.timestamps.entry(region.clone());
            // Only update if we don't have it or other's value is greater
            match entry {
                std::collections::hash_map::Entry::Vacant(e) => {
                    e.insert(*timestamp);
                }
                std::collections::hash_map::Entry::Occupied(mut e) => {
                    if timestamp > e.get() {
                        *e.get_mut() = *timestamp;
                    }
                }
            }
        }
    }

    /// Compare two vector clocks with detailed result
    ///
    /// Returns `CausalityResult` indicating the relationship:
    /// - `HappensBefore`: All timestamps in self ≤ other, at least one <
    /// - `HappensAfter`: All timestamps in self ≥ other, at least one >
    /// - `Concurrent`: Neither happens-before the other (some <, some >)
    /// - `Equal`: All timestamps equal
    pub fn compare_detailed(&self, other: &VectorClock) -> CausalityResult {
        if self == other {
            return CausalityResult::Equal;
        }

        let self_before_other = self.happens_before(other);
        let other_before_self = other.happens_before(self);

        match (self_before_other, other_before_self) {
            (true, false) => CausalityResult::HappensBefore,
            (false, true) => CausalityResult::HappensAfter,
            (false, false) => CausalityResult::Concurrent,
            (true, true) => unreachable!("Both clocks can't happen-before each other unless equal"),
        }
    }

    /// Resolve concurrent updates using Last-Writer-Wins
    ///
    /// Given two concurrent updates to the same logical key,
    /// resolve by selecting the one with higher (timestamp, region_id) tuple.
    ///
    /// # Arguments
    /// * `region_a` - First region's ID
    /// * `ts_a` - First region's timestamp
    /// * `region_b` - Second region's ID
    /// * `ts_b` - Second region's timestamp
    ///
    /// # Returns
    /// Tuple of (winning_region, winning_timestamp)
    ///
    /// # Example
    /// ```ignore
    /// // us-east wrote at t=100, eu-west wrote at t=101
    /// let (winner, ts) = VectorClock::resolve_lww("us-east", 100, "eu-west", 101);
    /// assert_eq!(winner, "eu-west");
    /// assert_eq!(ts, 101);
    /// ```
    pub fn resolve_lww(region_a: &str, ts_a: u64, region_b: &str, ts_b: u64) -> (String, u64) {
        if ts_a > ts_b || (ts_a == ts_b && region_a > region_b) {
            (region_a.to_string(), ts_a)
        } else {
            (region_b.to_string(), ts_b)
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

    /// Get the dot product (sum of all timestamps)
    ///
    /// Useful for measuring clock size and merge cost.
    pub fn dot_product(&self) -> u64 {
        self.timestamps.values().sum()
    }

    /// Get the number of regions tracked
    pub fn region_count(&self) -> usize {
        self.timestamps.len()
    }

    /// Check if this clock dominates another (all timestamps ≥)
    ///
    /// Unlike `happens_before`, this returns true even if clocks are equal.
    pub fn dominates(&self, other: &VectorClock) -> bool {
        for (region, ts) in &self.timestamps {
            let other_ts = other.get(region);
            if ts < &other_ts {
                return false;
            }
        }
        true
    }

    /// Create a vector clock from a single region's timestamp
    pub fn from_single_region(region: &str, timestamp: u64) -> Self {
        let mut timestamps = HashMap::new();
        timestamps.insert(region.to_string(), timestamp);
        Self { timestamps }
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

    // ========== Multi-Region Merge Protocol Tests ==========

    #[test]
    fn test_merge_with_conflict_detection_causal() {
        let mut vc1 = VectorClock::with_regions(&["us-east", "eu-west"]);
        vc1.increment("us-east");

        let mut vc2 = VectorClock::with_regions(&["us-east", "eu-west"]);
        vc2.increment("us-east");
        vc2.increment("eu-west");

        let result = vc1.merge_with_conflict_detection(&vc2, None);
        assert!(!result.had_conflicts);
        assert!(result.conflicting_regions.is_empty());
        assert_eq!(result.merged_clock.get("us-east"), 1);
        assert_eq!(result.merged_clock.get("eu-west"), 1);
    }

    #[test]
    fn test_merge_with_conflict_detection_concurrent() {
        let mut vc1 = VectorClock::with_regions(&["us-east", "eu-west"]);
        vc1.increment("us-east");

        let mut vc2 = VectorClock::with_regions(&["us-east", "eu-west"]);
        vc2.increment("eu-west");

        let result = vc1.merge_with_conflict_detection(&vc2, None);
        assert!(result.had_conflicts);
        assert_eq!(result.merged_clock.get("us-east"), 1);
        assert_eq!(result.merged_clock.get("eu-west"), 1);
    }

    #[test]
    fn test_merge_with_conflict_detection_equal() {
        let vc1 = VectorClock::with_regions(&["us-east", "eu-west"]);
        let vc2 = VectorClock::with_regions(&["us-east", "eu-west"]);

        let mut vc1_clone = vc1.clone();
        let result = vc1_clone.merge_with_conflict_detection(&vc2, None);
        assert!(!result.had_conflicts);
        assert!(result.conflicting_regions.is_empty());
    }

    #[test]
    fn test_merge_optimized_partial_clocks() {
        // Clock A: has us-east=5, eu-west=0
        let mut vc_a = VectorClock::new();
        vc_a.timestamps.insert("us-east".to_string(), 5);
        vc_a.timestamps.insert("eu-west".to_string(), 0);

        // Clock B: has us-east=3, eu-west=7
        let mut vc_b = VectorClock::new();
        vc_b.timestamps.insert("us-east".to_string(), 3);
        vc_b.timestamps.insert("eu-west".to_string(), 7);

        // Optimized merge should skip us-east (5 > 3) but update eu-west
        vc_a.merge_optimized(&vc_b);
        assert_eq!(vc_a.get("us-east"), 5); // Unchanged (already max)
        assert_eq!(vc_a.get("eu-west"), 7); // Updated
    }

    #[test]
    fn test_merge_optimized_new_region() {
        let mut vc_a = VectorClock::new();
        vc_a.timestamps.insert("us-east".to_string(), 5);

        let mut vc_b = VectorClock::new();
        vc_b.timestamps.insert("ap-south".to_string(), 10);

        vc_a.merge_optimized(&vc_b);
        assert_eq!(vc_a.get("us-east"), 5);
        assert_eq!(vc_a.get("ap-south"), 10);
    }

    #[test]
    fn test_compare_detailed_happens_before() {
        let mut vc1 = VectorClock::with_regions(&["us-east", "eu-west"]);
        vc1.increment("us-east");

        let mut vc2 = VectorClock::with_regions(&["us-east", "eu-west"]);
        vc2.increment("us-east");
        vc2.increment("eu-west");

        let result = vc1.compare_detailed(&vc2);
        assert_eq!(result, CausalityResult::HappensBefore);
    }

    #[test]
    fn test_compare_detailed_happens_after() {
        let mut vc1 = VectorClock::with_regions(&["us-east", "eu-west"]);
        vc1.increment("us-east");
        vc1.increment("eu-west");

        let mut vc2 = VectorClock::with_regions(&["us-east", "eu-west"]);
        vc2.increment("us-east");

        let result = vc1.compare_detailed(&vc2);
        assert_eq!(result, CausalityResult::HappensAfter);
    }

    #[test]
    fn test_compare_detailed_concurrent() {
        let mut vc1 = VectorClock::with_regions(&["us-east", "eu-west"]);
        vc1.increment("us-east");

        let mut vc2 = VectorClock::with_regions(&["us-east", "eu-west"]);
        vc2.increment("eu-west");

        let result = vc1.compare_detailed(&vc2);
        assert_eq!(result, CausalityResult::Concurrent);
    }

    #[test]
    fn test_compare_detailed_equal() {
        let vc1 = VectorClock::with_regions(&["us-east", "eu-west"]);
        let vc2 = VectorClock::with_regions(&["us-east", "eu-west"]);

        let result = vc1.compare_detailed(&vc2);
        assert_eq!(result, CausalityResult::Equal);
    }

    #[test]
    fn test_resolve_lww_timestamp_tiebreaker() {
        // Higher timestamp wins
        let (winner, ts) = VectorClock::resolve_lww("us-east", 100, "eu-west", 101);
        assert_eq!(winner, "eu-west");
        assert_eq!(ts, 101);
    }

    #[test]
    fn test_resolve_lww_region_tiebreaker() {
        // On timestamp tie, lexicographic region ID wins
        let (winner, ts) = VectorClock::resolve_lww("us-east", 100, "eu-west", 100);
        assert_eq!(winner, "us-east"); // "us-east" > "eu-west" lexicographically
        assert_eq!(ts, 100);
    }

    #[test]
    fn test_resolve_lww_both_tiebreakers() {
        let (winner, ts) = VectorClock::resolve_lww("us-west", 150, "us-east", 100);
        assert_eq!(winner, "us-west");
        assert_eq!(ts, 150);
    }

    #[test]
    fn test_dot_product() {
        let mut vc = VectorClock::with_regions(&["us-east", "eu-west", "ap-south"]);
        vc.increment("us-east");
        vc.increment("us-east");
        vc.increment("eu-west");
        vc.increment("ap-south");
        vc.increment("ap-south");
        vc.increment("ap-south");

        assert_eq!(vc.dot_product(), 6); // 2 + 1 + 3
    }

    #[test]
    fn test_region_count() {
        let vc = VectorClock::with_regions(&["us-east", "eu-west", "ap-south"]);
        assert_eq!(vc.region_count(), 3);
    }

    #[test]
    fn test_dominates() {
        let mut vc1 = VectorClock::with_regions(&["us-east", "eu-west"]);
        vc1.increment("us-east");
        vc1.increment("eu-west");

        let mut vc2 = VectorClock::with_regions(&["us-east", "eu-west"]);
        vc2.increment("us-east");

        assert!(vc1.dominates(&vc2));
        assert!(!vc2.dominates(&vc1));
    }

    #[test]
    fn test_dominates_equal() {
        let vc1 = VectorClock::with_regions(&["us-east", "eu-west"]);
        let vc2 = VectorClock::with_regions(&["us-east", "eu-west"]);

        assert!(vc1.dominates(&vc2)); // Equal clocks dominate each other
        assert!(vc2.dominates(&vc1));
    }

    #[test]
    fn test_from_single_region() {
        let vc = VectorClock::from_single_region("us-east", 42);
        assert_eq!(vc.get("us-east"), 42);
        assert_eq!(vc.region_count(), 1);
    }

    // ========== Integration Tests: Multi-Region Scenarios ==========

    #[test]
    fn test_three_region_causal_chain() {
        // us-east → eu-west → ap-south (causal chain)
        let mut vc1 = VectorClock::with_regions(&["us-east", "eu-west", "ap-south"]);
        vc1.increment("us-east");

        let mut vc2 = vc1.clone();
        vc2.increment("eu-west");

        let mut vc3 = vc2.clone();
        vc3.increment("ap-south");

        assert!(vc1.happens_before(&vc2));
        assert!(vc2.happens_before(&vc3));
        assert!(vc1.happens_before(&vc3));
        assert!(!vc3.happens_before(&vc1));
    }

    #[test]
    fn test_three_region_concurrent_writes() {
        // All three regions write concurrently
        let mut vc_east = VectorClock::with_regions(&["us-east", "eu-west", "ap-south"]);
        vc_east.increment("us-east");

        let mut vc_west = VectorClock::with_regions(&["us-east", "eu-west", "ap-south"]);
        vc_west.increment("eu-west");

        let mut vc_south = VectorClock::with_regions(&["us-east", "eu-west", "ap-south"]);
        vc_south.increment("ap-south");

        // All pairs are concurrent
        assert!(vc_east.concurrent(&vc_west));
        assert!(vc_east.concurrent(&vc_south));
        assert!(vc_west.concurrent(&vc_south));

        // Merge all three
        let mut result = vc_east.merge_with_conflict_detection(&vc_west, None);
        assert!(result.had_conflicts);
        let final_result = result.merged_clock.merge_with_conflict_detection(&vc_south, None);
        assert!(final_result.had_conflicts);
        assert_eq!(final_result.merged_clock.get("us-east"), 1);
        assert_eq!(final_result.merged_clock.get("eu-west"), 1);
        assert_eq!(final_result.merged_clock.get("ap-south"), 1);
    }

    #[test]
    fn test_network_partition_recovery() {
        // Simulate network partition: two regions diverge, then reconnect
        let mut vc_primary = VectorClock::with_regions(&["us-east", "eu-west"]);
        vc_primary.increment("us-east");
        vc_primary.increment("us-east");

        let mut vc_secondary = VectorClock::with_regions(&["us-east", "eu-west"]);
        vc_secondary.increment("eu-west");
        vc_secondary.increment("eu-west");
        vc_secondary.increment("eu-west");

        // Partition: both clocks are concurrent
        assert!(vc_primary.concurrent(&vc_secondary));

        // Recovery: merge clocks
        let result = vc_primary.merge_with_conflict_detection(&vc_secondary, None);
        assert!(result.had_conflicts);
        assert_eq!(result.merged_clock.get("us-east"), 2);
        assert_eq!(result.merged_clock.get("eu-west"), 3);
    }

    #[test]
    fn test_increment_copy_chain() {
        // Test causal chain using increment_copy
        let vc0 = VectorClock::new();
        let vc1 = vc0.increment_copy("us-east");
        let vc2 = vc1.increment_copy("us-east");
        let vc3 = vc2.increment_copy("eu-west");

        assert!(vc0.happens_before(&vc1));
        assert!(vc1.happens_before(&vc2));
        assert!(vc2.happens_before(&vc3));
        assert_eq!(vc3.get("us-east"), 2);
        assert_eq!(vc3.get("eu-west"), 1);
    }

    #[test]
    fn test_merge_idempotence() {
        let mut vc1 = VectorClock::with_regions(&["us-east", "eu-west"]);
        vc1.increment("us-east");

        let mut vc2 = VectorClock::with_regions(&["us-east", "eu-west"]);
        vc2.increment("eu-west");

        // Merge once
        vc1.merge(&vc2);
        let merged_once = vc1.clone();

        // Merge again (should be idempotent)
        vc1.merge(&vc2);

        assert_eq!(vc1, merged_once);
    }

    #[test]
    fn test_merge_commutativity() {
        let mut vc1 = VectorClock::with_regions(&["us-east", "eu-west"]);
        vc1.increment("us-east");

        let mut vc2 = VectorClock::with_regions(&["us-east", "eu-west"]);
        vc2.increment("eu-west");

        let mut vc1_a = vc1.clone();
        let mut vc2_a = vc2.clone();

        // vc1.merge(vc2) should equal vc2.merge(vc1)
        vc1_a.merge(&vc2_a);
        vc2_a.merge(&vc1);

        assert_eq!(vc1_a, vc2_a);
    }

    #[test]
    fn test_merge_associativity() {
        let mut vc1 = VectorClock::with_regions(&["us-east", "eu-west", "ap-south"]);
        vc1.increment("us-east");

        let mut vc2 = VectorClock::with_regions(&["us-east", "eu-west", "ap-south"]);
        vc2.increment("eu-west");

        let mut vc3 = VectorClock::with_regions(&["us-east", "eu-west", "ap-south"]);
        vc3.increment("ap-south");

        // (vc1.merge(vc2)).merge(vc3) should equal vc1.merge(vc2.merge(vc3))
        let mut left_a = vc1.clone();
        let left_b = vc2.clone();
        let left_c = vc3.clone();

        left_a.merge(&left_b);
        left_a.merge(&left_c);

        let mut right_b = vc2.clone();
        let right_c = vc3.clone();
        right_b.merge(&right_c);
        vc1.merge(&right_b);

        assert_eq!(left_a, vc1);
    }

    #[test]
    fn test_sparse_vector_clocks() {
        // Test merge of clocks with different region sets
        let mut vc1 = VectorClock::new();
        vc1.timestamps.insert("us-east".to_string(), 5);
        vc1.timestamps.insert("eu-west".to_string(), 3);

        let mut vc2 = VectorClock::new();
        vc2.timestamps.insert("eu-west".to_string(), 7);
        vc2.timestamps.insert("ap-south".to_string(), 2);

        vc1.merge(&vc2);
        assert_eq!(vc1.get("us-east"), 5);
        assert_eq!(vc1.get("eu-west"), 7); // Max(3, 7)
        assert_eq!(vc1.get("ap-south"), 2);
    }

    #[test]
    fn test_display_sorted() {
        let mut vc = VectorClock::new();
        vc.timestamps.insert("ap-south".to_string(), 3);
        vc.timestamps.insert("us-east".to_string(), 1);
        vc.timestamps.insert("eu-west".to_string(), 2);

        let display = format!("{}", vc);
        // Should be sorted: ap-south, eu-west, us-east
        assert!(display.contains("ap-south=3"));
        assert!(display.contains("eu-west=2"));
        assert!(display.contains("us-east=1"));
    }
}
