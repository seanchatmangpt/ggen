//! Vector clock implementation for distributed causal consistency
//!
//! Vector clocks provide a mechanism to track causality in distributed systems.
//! Each node maintains a vector of logical clocks, one per node in the system.
//!
//! ## Properties
//!
//! - **Happened-before relation**: If VC1 < VC2, then event1 causally precedes event2
//! - **Concurrent events**: If VC1 || VC2, events are concurrent (no causal relationship)
//! - **Total ordering**: Within a single node, events are totally ordered
//!
//! ## Example
//!
//! ```rust
//! use ggen_temporal::vector_clock::{VectorClock, VectorTime};
//!
//! let mut clock_a = VectorClock::new("node-a".to_string());
//! let mut clock_b = VectorClock::new("node-b".to_string());
//!
//! // Node A performs an action
//! clock_a.tick();
//! let time_a1 = clock_a.timestamp();
//!
//! // Node B receives message from A and performs action
//! clock_b.merge(&time_a1);
//! clock_b.tick();
//! let time_b1 = clock_b.timestamp();
//!
//! // Verify causality
//! assert!(time_a1.happened_before(&time_b1));
//! ```

use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::fmt;

/// Unique identifier for a node in the distributed system
pub type NodeId = String;

/// A vector clock timestamp representing a point in causal time
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct VectorTime {
    /// Map from node ID to logical clock value
    clocks: IndexMap<NodeId, u64>,
}

impl VectorTime {
    /// Create a new empty vector time
    #[must_use]
    pub fn new() -> Self {
        Self {
            clocks: IndexMap::new(),
        }
    }

    /// Create a vector time with a single node
    #[must_use]
    pub fn with_node(node_id: NodeId, value: u64) -> Self {
        let mut clocks = IndexMap::new();
        clocks.insert(node_id, value);
        Self { clocks }
    }

    /// Get the clock value for a specific node
    #[must_use]
    pub fn get(&self, node_id: &str) -> u64 {
        self.clocks.get(node_id).copied().unwrap_or(0)
    }

    /// Set the clock value for a specific node
    pub fn set(&mut self, node_id: NodeId, value: u64) {
        self.clocks.insert(node_id, value);
    }

    /// Increment the clock for a specific node
    pub fn increment(&mut self, node_id: &NodeId) {
        let current = self.get(node_id);
        self.clocks.insert(node_id.clone(), current + 1);
    }

    /// Merge another vector time into this one (take max of each component)
    pub fn merge(&mut self, other: &VectorTime) {
        for (node_id, &other_value) in &other.clocks {
            let current = self.get(node_id);
            self.clocks
                .insert(node_id.clone(), current.max(other_value));
        }
    }

    /// Check if this vector time happened before another
    ///
    /// Returns true if self ≤ other AND self ≠ other
    #[must_use]
    pub fn happened_before(&self, other: &VectorTime) -> bool {
        if self == other {
            return false;
        }

        // Check if all components in self are ≤ components in other
        let all_nodes: std::collections::HashSet<_> = self
            .clocks
            .keys()
            .chain(other.clocks.keys())
            .cloned()
            .collect();

        all_nodes
            .iter()
            .all(|node_id| self.get(node_id) <= other.get(node_id))
    }

    /// Check if two vector times are concurrent (no causal relationship)
    #[must_use]
    pub fn is_concurrent(&self, other: &VectorTime) -> bool {
        !self.happened_before(other) && !other.happened_before(self)
    }

    /// Compare two vector times for partial ordering
    #[must_use]
    pub fn partial_cmp_vector(&self, other: &VectorTime) -> Option<Ordering> {
        if self == other {
            return Some(Ordering::Equal);
        }

        if self.happened_before(other) {
            return Some(Ordering::Less);
        }

        if other.happened_before(self) {
            return Some(Ordering::Greater);
        }

        None // Concurrent
    }

    /// Get all node IDs in this vector time
    #[must_use]
    pub fn node_ids(&self) -> Vec<NodeId> {
        self.clocks.keys().cloned().collect()
    }

    /// Get the maximum clock value across all nodes
    #[must_use]
    pub fn max_value(&self) -> u64 {
        self.clocks.values().copied().max().unwrap_or(0)
    }
}

impl Default for VectorTime {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for VectorTime {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{")?;
        let mut first = true;
        for (node_id, value) in &self.clocks {
            if !first {
                write!(f, ", ")?;
            }
            write!(f, "{node_id}:{value}")?;
            first = false;
        }
        write!(f, "}}")
    }
}

/// A vector clock that tracks causal time for a single node
#[derive(Debug, Clone)]
pub struct VectorClock {
    /// The ID of this node
    node_id: NodeId,
    /// The current vector time
    time: VectorTime,
}

impl VectorClock {
    /// Create a new vector clock for a node
    #[must_use]
    pub fn new(node_id: NodeId) -> Self {
        let mut time = VectorTime::new();
        time.set(node_id.clone(), 0);
        Self { node_id, time }
    }

    /// Get the node ID
    #[must_use]
    pub fn node_id(&self) -> &NodeId {
        &self.node_id
    }

    /// Get the current vector time (immutable reference)
    #[must_use]
    pub fn time(&self) -> &VectorTime {
        &self.time
    }

    /// Get the current timestamp (cloned)
    #[must_use]
    pub fn timestamp(&self) -> VectorTime {
        self.time.clone()
    }

    /// Increment the local clock (happens before a local event)
    pub fn tick(&mut self) {
        self.time.increment(&self.node_id);
    }

    /// Merge a received timestamp (happens before receiving a message)
    pub fn merge(&mut self, received: &VectorTime) {
        self.time.merge(received);
        self.tick(); // Increment local clock after merge
    }

    /// Update to a specific vector time (for reconstruction)
    pub fn update(&mut self, new_time: VectorTime) {
        self.time = new_time;
    }

    /// Get the local clock value
    #[must_use]
    pub fn local_time(&self) -> u64 {
        self.time.get(&self.node_id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vector_time_creation() {
        let time = VectorTime::new();
        assert_eq!(time.get("node-a"), 0);
    }

    #[test]
    fn test_vector_time_increment() {
        let mut time = VectorTime::new();
        time.increment(&"node-a".to_string());
        assert_eq!(time.get("node-a"), 1);
        time.increment(&"node-a".to_string());
        assert_eq!(time.get("node-a"), 2);
    }

    #[test]
    fn test_vector_time_merge() {
        let mut time1 = VectorTime::new();
        time1.set("node-a".to_string(), 5);
        time1.set("node-b".to_string(), 3);

        let mut time2 = VectorTime::new();
        time2.set("node-a".to_string(), 3);
        time2.set("node-b".to_string(), 7);

        time1.merge(&time2);

        assert_eq!(time1.get("node-a"), 5); // max(5, 3)
        assert_eq!(time1.get("node-b"), 7); // max(3, 7)
    }

    #[test]
    fn test_happened_before() {
        let mut time1 = VectorTime::new();
        time1.set("node-a".to_string(), 1);
        time1.set("node-b".to_string(), 2);

        let mut time2 = VectorTime::new();
        time2.set("node-a".to_string(), 3);
        time2.set("node-b".to_string(), 4);

        assert!(time1.happened_before(&time2));
        assert!(!time2.happened_before(&time1));
    }

    #[test]
    fn test_concurrent_events() {
        let mut time1 = VectorTime::new();
        time1.set("node-a".to_string(), 5);
        time1.set("node-b".to_string(), 2);

        let mut time2 = VectorTime::new();
        time2.set("node-a".to_string(), 3);
        time2.set("node-b".to_string(), 7);

        assert!(time1.is_concurrent(&time2));
        assert!(time2.is_concurrent(&time1));
    }

    #[test]
    fn test_vector_clock_tick() {
        let mut clock = VectorClock::new("node-a".to_string());
        assert_eq!(clock.local_time(), 0);

        clock.tick();
        assert_eq!(clock.local_time(), 1);

        clock.tick();
        assert_eq!(clock.local_time(), 2);
    }

    #[test]
    fn test_vector_clock_merge() {
        let mut clock_a = VectorClock::new("node-a".to_string());
        let mut clock_b = VectorClock::new("node-b".to_string());

        // Node A does work
        clock_a.tick();
        clock_a.tick();
        let time_a = clock_a.timestamp();

        // Node B receives message from A
        clock_b.merge(&time_a);

        // Verify B's clock shows causality
        assert_eq!(clock_b.time().get("node-a"), 2);
        assert_eq!(clock_b.local_time(), 1); // B ticked once during merge
    }

    #[test]
    fn test_vector_time_display() {
        let mut time = VectorTime::new();
        time.set("node-a".to_string(), 5);
        time.set("node-b".to_string(), 3);

        let display = format!("{time}");
        assert!(display.contains("node-a:5"));
        assert!(display.contains("node-b:3"));
    }

    #[test]
    fn test_partial_ordering() {
        let mut time1 = VectorTime::new();
        time1.set("a".to_string(), 1);
        time1.set("b".to_string(), 2);

        let mut time2 = VectorTime::new();
        time2.set("a".to_string(), 2);
        time2.set("b".to_string(), 3);

        assert_eq!(time1.partial_cmp_vector(&time2), Some(Ordering::Less));
        assert_eq!(time2.partial_cmp_vector(&time1), Some(Ordering::Greater));
        assert_eq!(time1.partial_cmp_vector(&time1), Some(Ordering::Equal));

        let mut time3 = VectorTime::new();
        time3.set("a".to_string(), 3);
        time3.set("b".to_string(), 1);

        assert_eq!(time1.partial_cmp_vector(&time3), None); // Concurrent
    }
}
