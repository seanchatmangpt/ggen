//! Core type definitions for state management

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// Unique identifier for a state document
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct StateId(pub String);

impl StateId {
    pub fn new(id: String) -> Self {
        StateId(id)
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// A snapshot of a state at a point in time
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct StateSnapshot {
    /// The state document ID
    pub id: StateId,

    /// The actual state data as JSON
    pub data: serde_json::Value,

    /// Version for optimistic locking (incremented on each write)
    pub version: u64,

    /// Timestamp of this snapshot (for causality tracking)
    pub timestamp: DateTime<Utc>,

    /// Metadata about the change that created this snapshot
    pub metadata: ChangeMetadata,

    /// Vector clock for distributed causality tracking
    pub causality: VectorClockSnapshot,
}

impl StateSnapshot {
    /// Create a new state snapshot
    pub fn new(
        id: StateId,
        data: serde_json::Value,
        metadata: ChangeMetadata,
    ) -> Self {
        StateSnapshot {
            id,
            data,
            version: 0,
            timestamp: Utc::now(),
            metadata,
            causality: VectorClockSnapshot::new(),
        }
    }

    /// Update timestamp to now
    pub fn touch(&mut self) {
        self.timestamp = Utc::now();
    }

    /// Increment version (called on writes)
    pub fn increment_version(&mut self) {
        self.version += 1;
        self.touch();
    }
}

/// Metadata about a state change
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ChangeMetadata {
    /// Who made the change (e.g., "system", "user-123", "api-client")
    pub changed_by: String,

    /// Why the change was made (e.g., "initialization", "action_performed", "conflict_resolved")
    pub reason: String,

    /// Optional parent version for tracking causality
    pub parent_version: Option<u64>,

    /// Timestamp of the change
    pub timestamp: DateTime<Utc>,

    /// Additional context as key-value pairs
    pub context: BTreeMap<String, serde_json::Value>,
}

impl ChangeMetadata {
    /// Create new change metadata
    pub fn new(changed_by: impl Into<String>, reason: impl Into<String>, parent_version: Option<u64>) -> Self {
        ChangeMetadata {
            changed_by: changed_by.into(),
            reason: reason.into(),
            parent_version,
            timestamp: Utc::now(),
            context: BTreeMap::new(),
        }
    }

    /// Add context to metadata
    pub fn with_context(mut self, key: String, value: serde_json::Value) -> Self {
        self.context.insert(key, value);
        self
    }

    /// Set multiple context values
    pub fn with_contexts(mut self, contexts: BTreeMap<String, serde_json::Value>) -> Self {
        self.context.extend(contexts);
        self
    }
}

/// Vector clock snapshot for distributed causality tracking
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct VectorClockSnapshot {
    /// Node ID to clock value mapping
    pub clocks: BTreeMap<String, u64>,
}

impl VectorClockSnapshot {
    /// Create empty vector clock
    pub fn new() -> Self {
        VectorClockSnapshot {
            clocks: BTreeMap::new(),
        }
    }

    /// Increment clock for a node
    pub fn increment(&mut self, node_id: &str) {
        *self.clocks.entry(node_id.to_string()).or_insert(0) += 1;
    }

    /// Get clock value for a node
    pub fn get(&self, node_id: &str) -> u64 {
        self.clocks.get(node_id).copied().unwrap_or(0)
    }

    /// Check if this clock happened before another (causality check)
    pub fn happened_before(&self, other: &VectorClockSnapshot) -> bool {
        if self.clocks.is_empty() && other.clocks.is_empty() {
            return false;
        }

        let mut has_less = false;
        for (node, &clock) in &self.clocks {
            let other_clock = other.clocks.get(node).copied().unwrap_or(0);
            if clock > other_clock {
                return false;
            }
            if clock < other_clock {
                has_less = true;
            }
        }

        // Check if other has any clocks we don't
        for (node, &other_clock) in &other.clocks {
            if !self.clocks.contains_key(node) && other_clock > 0 {
                has_less = true;
            }
        }

        has_less
    }

    /// Check if clocks are concurrent (neither happened before)
    pub fn concurrent_with(&self, other: &VectorClockSnapshot) -> bool {
        !self.happened_before(other) && !other.happened_before(self)
    }

    /// Merge with another vector clock (take maximum of each node's clock)
    pub fn merge(&mut self, other: &VectorClockSnapshot) {
        for (node, &clock) in &other.clocks {
            let entry = self.clocks.entry(node.clone()).or_insert(0);
            *entry = (*entry).max(clock);
        }
    }
}

impl Default for VectorClockSnapshot {
    fn default() -> Self {
        Self::new()
    }
}

/// Event stored in the event log
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Event {
    /// Unique event ID
    pub id: String,

    /// Action that triggered the state transition
    pub action: String,

    /// State before the action
    pub from_state: Option<StateSnapshot>,

    /// State after the action
    pub to_state: StateSnapshot,

    /// Timestamp of the event
    pub timestamp: DateTime<Utc>,

    /// Vector clock at time of event
    pub causality: VectorClockSnapshot,

    /// Metadata about who performed the action and why
    pub metadata: ChangeMetadata,
}

impl Event {
    /// Create a new event
    pub fn new(
        id: String,
        action: String,
        from_state: Option<StateSnapshot>,
        to_state: StateSnapshot,
        metadata: ChangeMetadata,
    ) -> Self {
        Event {
            id,
            action,
            from_state,
            to_state,
            timestamp: Utc::now(),
            causality: VectorClockSnapshot::new(),
            metadata,
        }
    }
}

/// Transaction record for audit trail
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TransactionRecord {
    /// Transaction ID
    pub transaction_id: String,

    /// Timestamp when transaction started
    pub start_time: DateTime<Utc>,

    /// Timestamp when transaction committed
    pub commit_time: Option<DateTime<Utc>>,

    /// Documents read in transaction
    pub reads: Vec<String>,

    /// Documents written in transaction
    pub writes: Vec<String>,

    /// Transaction status (pending, committed, rolled_back)
    pub status: TransactionStatus,

    /// Reason for rollback if any
    pub rollback_reason: Option<String>,
}

/// Status of a transaction
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TransactionStatus {
    Pending,
    Committed,
    RolledBack,
    Failed,
}

/// Lock held by a client on a resource
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DistributedLock {
    /// Resource being locked
    pub resource: String,

    /// Client holding the lock
    pub client_id: String,

    /// When lock was acquired
    pub acquired_at: DateTime<Utc>,

    /// When lock should expire
    pub expires_at: DateTime<Utc>,

    /// Unique token for this lock instance
    pub lock_token: String,
}

impl DistributedLock {
    /// Create a new lock
    pub fn new(resource: String, client_id: String) -> Self {
        let acquired_at = Utc::now();
        let expires_at = acquired_at + chrono::Duration::seconds(30);
        let lock_token = uuid::Uuid::new_v4().to_string();

        DistributedLock {
            resource,
            client_id,
            acquired_at,
            expires_at,
            lock_token,
        }
    }

    /// Check if lock has expired
    pub fn is_expired(&self) -> bool {
        Utc::now() > self.expires_at
    }

    /// Extend lock expiration
    pub fn extend(&mut self) {
        self.expires_at = Utc::now() + chrono::Duration::seconds(30);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vector_clock_happened_before() {
        let mut clock1 = VectorClockSnapshot::new();
        clock1.increment("node1");
        clock1.increment("node1");

        let mut clock2 = VectorClockSnapshot::new();
        clock2.increment("node1");
        clock2.increment("node1");
        clock2.increment("node1");

        assert!(clock1.happened_before(&clock2));
        assert!(!clock2.happened_before(&clock1));
    }

    #[test]
    fn test_vector_clock_concurrent() {
        let mut clock1 = VectorClockSnapshot::new();
        clock1.increment("node1");

        let mut clock2 = VectorClockSnapshot::new();
        clock2.increment("node2");

        assert!(clock1.concurrent_with(&clock2));
    }

    #[test]
    fn test_vector_clock_merge() {
        let mut clock1 = VectorClockSnapshot::new();
        clock1.increment("node1");
        clock1.increment("node1");

        let mut clock2 = VectorClockSnapshot::new();
        clock2.increment("node2");
        clock2.increment("node2");

        clock1.merge(&clock2);

        assert_eq!(clock1.get("node1"), 2);
        assert_eq!(clock1.get("node2"), 2);
    }
}
